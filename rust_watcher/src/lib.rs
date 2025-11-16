use rustler::{NifResult, ResourceArc, Error, Env, Term};
use notify::{Watcher, RecursiveMode, RecommendedWatcher, Event, EventKind};
use std::sync::{Mutex, Arc};
use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};
use std::collections::HashMap;
use glob::Pattern;

// Resource that holds the file watcher and collected events
pub struct FileWatcherResource {
    watcher: Mutex<Option<RecommendedWatcher>>,
    events: Arc<Mutex<Vec<FileEventInfo>>>,
    deleted_files: Arc<Mutex<HashMap<String, u64>>>,
    patterns: Arc<Vec<Pattern>>,
    path: String,
}

// Structured event info
#[derive(Clone, Debug)]
struct FileEventInfo {
    kind: String,
    path: String,
    timestamp: u64,
}

// Load function that registers the resource
fn on_load(env: Env, _info: Term) -> bool {
    rustler::resource!(FileWatcherResource, env);
    true
}

// Helper functions
fn compile_patterns(patterns: &[String]) -> NifResult<Vec<Pattern>> {
    let mut compiled = Vec::new();

    for pattern_str in patterns {
        let pattern = Pattern::new(pattern_str)
            .map_err(|e| Error::Term(Box::new(format!("Invalid glob pattern '{}': {}", pattern_str, e))))?;
        compiled.push(pattern);
    }

    Ok(compiled)
}

fn matches_patterns(event: &Event, patterns: &[Pattern]) -> bool {
    if patterns.is_empty() {
        return true;
    }

    for path in &event.paths {
        let path_str = path.to_string_lossy();

        for pattern in patterns {
            if pattern.matches(&path_str) {
                return true;
            }

            if let Some(filename) = path.file_name() {
                if pattern.matches(&filename.to_string_lossy()) {
                    return true;
                }
            }

            let relative = path_str.trim_start_matches("./");
            if pattern.matches(relative) {
                return true;
            }
        }
    }

    false
}

fn should_ignore_event(event: &Event) -> bool {
    for path in &event.paths {
        let path_str = path.to_string_lossy();

        if path_str.contains("~") ||
           path_str.contains(".swp") ||
           path_str.contains(".swx") ||
           path_str.contains(".tmp") ||
           path_str.ends_with("#") ||
           path.file_name()
               .map(|n| n.to_string_lossy().starts_with('.'))
               .unwrap_or(false) ||
           path.file_name()
               .and_then(|n| n.to_str())
               .map(|n| n.chars().all(|c| c.is_numeric()))
               .unwrap_or(false)
        {
            return true;
        }

        if matches!(event.kind, EventKind::Access(_)) {
            return true;
        }
    }

    false
}

fn get_timestamp() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as u64
}

fn process_event(
    event: &Event,
    deleted_files: &Arc<Mutex<HashMap<String, u64>>>,
) -> Option<FileEventInfo> {
    let path = event.paths.first()?;
    let path_str = path.to_string_lossy().to_string();
    let timestamp = get_timestamp();

    let kind = match event.kind {
        EventKind::Create(_) => {
            if let Ok(deleted) = deleted_files.lock() {
                if let Some(&delete_time) = deleted.get(&path_str) {
                    if timestamp - delete_time < 2000 {
                        println!("[Rust] Detected edit-save pattern for: {}",
                                 path.file_name().unwrap().to_string_lossy());
                        "MODIFIED"
                    } else {
                        "CREATED"
                    }
                } else {
                    "CREATED"
                }
            } else {
                "CREATED"
            }
        },
        EventKind::Modify(_) => "MODIFIED",
        EventKind::Remove(_) => {
            if let Ok(mut deleted) = deleted_files.lock() {
                deleted.insert(path_str.clone(), timestamp);
            }
            "DELETED"
        },
        _ => return None,
    };

    Some(FileEventInfo {
        kind: kind.to_string(),
        path: path_str,
        timestamp,
    })
}

fn deduplicate_events(events: &[FileEventInfo]) -> Vec<String> {
    let mut grouped: HashMap<String, Vec<&FileEventInfo>> = HashMap::new();

    for event in events {
        grouped.entry(event.path.clone()).or_insert_with(Vec::new).push(event);
    }

    let mut result = Vec::new();
    for (path, events) in grouped {
        let filename = Path::new(&path)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or(&path);

        let has_delete = events.iter().any(|e| e.kind == "DELETED");
        let has_create = events.iter().any(|e| e.kind == "CREATED");
        let has_modify = events.iter().any(|e| e.kind == "MODIFIED");

        let action = if has_delete && !has_create {
            "DELETED"
        } else if has_modify || (has_delete && has_create) {
            "MODIFIED"
        } else if has_create {
            "CREATED"
        } else {
            continue;
        };

        let icon = match action {
            "CREATED" => "➕",
            "MODIFIED" => "📝",
            "DELETED" => "🗑️",
            _ => "📄",
        };

        result.push(format!("{} {} {}", icon, action, filename));
    }

    result
}

// Internal function that does the actual work
fn create_watcher(
    path: String,
    patterns: Vec<String>,
) -> NifResult<ResourceArc<FileWatcherResource>> {
    println!("[Rust] Starting watcher for: {}", path);

    if !patterns.is_empty() {
        println!("[Rust] With patterns: {:?}", patterns);
    }

    if !Path::new(&path).exists() {
        return Err(Error::Term(Box::new(format!("Path does not exist: {}", path))));
    }

    let compiled_patterns = compile_patterns(&patterns)?;
    let patterns_arc = Arc::new(compiled_patterns);

    let events = Arc::new(Mutex::new(Vec::new()));
    let events_clone = events.clone();
    let deleted_files = Arc::new(Mutex::new(HashMap::new()));
    let deleted_files_clone = deleted_files.clone();
    let patterns_clone = patterns_arc.clone();

    let watcher = notify::recommended_watcher(move |res: Result<Event, notify::Error>| {
        match res {
            Ok(event) => {
                if should_ignore_event(&event) {
                    return;
                }

                if !matches_patterns(&event, &patterns_clone) {
                    return;
                }

                let event_info = process_event(&event, &deleted_files_clone);
                if let Some(info) = event_info {
                    if let Ok(mut ev) = events_clone.lock() {
                        ev.push(info);
                    }
                }
            }
            Err(e) => eprintln!("[Rust] Watch error: {:?}", e),
        }
    }).map_err(|e| Error::Term(Box::new(format!("Failed to create watcher: {:?}", e))))?;

    let resource = ResourceArc::new(FileWatcherResource {
        watcher: Mutex::new(Some(watcher)),
        events,
        deleted_files,
        patterns: patterns_arc,
        path: path.clone(),
    });

    if let Ok(mut w) = resource.watcher.lock() {
        if let Some(watcher) = w.as_mut() {
            watcher.watch(Path::new(&path), RecursiveMode::Recursive)
                .map_err(|e| Error::Term(Box::new(format!("Failed to watch path: {:?}", e))))?;
        }
    }

    println!("[Rust] ✓ Watcher started successfully");
    Ok(resource)
}

// NIF functions
#[rustler::nif]
fn start_watching(path: String) -> NifResult<ResourceArc<FileWatcherResource>> {
    create_watcher(path, vec![])
}

#[rustler::nif]
fn start_watching_with_patterns(
    path: String,
    patterns: Vec<String>,
) -> NifResult<ResourceArc<FileWatcherResource>> {
    create_watcher(path, patterns)
}

#[rustler::nif]
fn get_events(watcher: ResourceArc<FileWatcherResource>) -> Vec<String> {
    if let Ok(mut events) = watcher.events.lock() {
        if let Ok(mut deleted) = watcher.deleted_files.lock() {
            let now = get_timestamp();
            deleted.retain(|_, timestamp| now - *timestamp < 5000);
        }

        let formatted = deduplicate_events(&events);
        events.clear();
        formatted
    } else {
        vec![]
    }
}

#[rustler::nif]
fn stop_watching(watcher: ResourceArc<FileWatcherResource>) -> bool {
    println!("[Rust] Stopping watcher for: {}", watcher.path);

    if let Ok(mut w) = watcher.watcher.lock() {
        *w = None;
        println!("[Rust] ✓ Watcher stopped");
        true
    } else {
        false
    }
}

rustler::init!("file_watcher", load = on_load);

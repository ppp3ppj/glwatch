use rustler::{NifResult, ResourceArc, Error, Env, Term};
use notify::{Watcher, RecursiveMode, RecommendedWatcher, Event, EventKind};
use std::sync::{Mutex, Arc};
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};
use std::collections::HashMap;
use std::borrow::Cow;
use glob::Pattern;

pub struct FileWatcherResource {
    watcher: Mutex<Option<RecommendedWatcher>>,
    events: Arc<Mutex<Vec<FileEventInfo>>>,
    deleted_files: Arc<Mutex<HashMap<String, u64>>>,
    patterns: Arc<Vec<Pattern>>,
    paths: Arc<Vec<String>>,
}

#[derive(Clone, Debug)]
struct FileEventInfo {
    kind: Cow<'static, str>,
    path: String,
    timestamp: u64,
}

fn on_load(env: Env, _info: Term) -> bool {
    rustler::resource!(FileWatcherResource, env);
    true
}

fn compile_patterns(patterns: &[String]) -> NifResult<Vec<Pattern>> {
    let mut compiled = Vec::with_capacity(patterns.len());

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

#[inline]
fn should_ignore_event(event: &Event) -> bool {
    for path in &event.paths {
        let path_str = path.to_string_lossy();

        if matches!(event.kind, EventKind::Access(_)) {
            return true;
        }

        if path_str.contains("~")
            || path_str.contains(".swp")
            || path_str.contains(".swx")
            || path_str.contains(".tmp")
            || path_str.ends_with("#")
        {
            return true;
        }

        if let Some(name) = path.file_name() {
            let name_str = name.to_string_lossy();
            if name_str.starts_with('.') || name_str.chars().all(|c| c.is_numeric()) {
                return true;
            }
        }
    }

    false
}

#[inline]
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

    let kind: Cow<'static, str> = match event.kind {
        EventKind::Create(_) => {
            if let Ok(deleted) = deleted_files.lock() {
                if let Some(&delete_time) = deleted.get(&path_str) {
                    if timestamp - delete_time < 2000 {
                        println!("[Rust] Detected edit-save pattern for: {}",
                                 path.file_name().unwrap().to_string_lossy());
                        Cow::Borrowed("MODIFIED")
                    } else {
                        Cow::Borrowed("CREATED")
                    }
                } else {
                    Cow::Borrowed("CREATED")
                }
            } else {
                Cow::Borrowed("CREATED")
            }
        },
        EventKind::Modify(_) => Cow::Borrowed("MODIFIED"),
        EventKind::Remove(_) => {
            if let Ok(mut deleted) = deleted_files.lock() {
                deleted.insert(path_str.clone(), timestamp);
            }
            Cow::Borrowed("DELETED")
        },
        _ => return None,
    };

    Some(FileEventInfo {
        kind,
        path: path_str,
        timestamp,
    })
}

fn deduplicate_events(events: &[FileEventInfo]) -> Vec<String> {
    let mut grouped: HashMap<String, Vec<&FileEventInfo>> = HashMap::with_capacity(events.len());

    for event in events {
        grouped.entry(event.path.clone()).or_insert_with(Vec::new).push(event);
    }

    let mut result = Vec::with_capacity(grouped.len());

    for (path, events) in grouped {
        let filename = Path::new(&path)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or(&path);

        let has_delete = events.iter().any(|e| e.kind == "DELETED");
        let has_create = events.iter().any(|e| e.kind == "CREATED");
        let has_modify = events.iter().any(|e| e.kind == "MODIFIED");

        let (icon, action) = if has_delete && !has_create {
            ("🗑️", "DELETED")
        } else if has_modify || (has_delete && has_create) {
            ("📝", "MODIFIED")
        } else if has_create {
            ("➕", "CREATED")
        } else {
            continue;
        };

        result.push(format!("{} {} {}", icon, action, filename));
    }

    result
}

fn create_watcher(
    paths: Vec<String>,
    patterns: Vec<String>,
) -> NifResult<ResourceArc<FileWatcherResource>> {
    println!("[Rust] Starting watcher for {} path(s)", paths.len());

    for path in &paths {
        println!("[Rust]   - {}", path);
    }

    if !patterns.is_empty() {
        println!("[Rust] With patterns: {:?}", patterns);
    }

    for path in &paths {
        if !Path::new(path).exists() {
            return Err(Error::Term(Box::new(format!("Path does not exist: {}", path))));
        }
    }

    let compiled_patterns = compile_patterns(&patterns)?;
    let patterns_arc = Arc::new(compiled_patterns);
    let paths_arc = Arc::new(paths);

    let events = Arc::new(Mutex::new(Vec::new()));
    let events_clone = events.clone();
    let deleted_files = Arc::new(Mutex::new(HashMap::new()));
    let deleted_files_clone = deleted_files.clone();
    let patterns_clone = patterns_arc.clone();
    let paths_clone = paths_arc.clone();

    let mut watcher = notify::recommended_watcher(move |res: Result<Event, notify::Error>| {
        match res {
            Ok(event) => {
                if should_ignore_event(&event) {
                    return;
                }

                if !matches_patterns(&event, &patterns_clone) {
                    return;
                }

                if let Some(info) = process_event(&event, &deleted_files_clone) {
                    if let Ok(mut ev) = events_clone.lock() {
                        ev.push(info);
                    }
                }
            }
            Err(e) => eprintln!("[Rust] Watch error: {:?}", e),
        }
    }).map_err(|e| Error::Term(Box::new(format!("Failed to create watcher: {:?}", e))))?;

    for path in paths_arc.iter() {
        watcher.watch(Path::new(path), RecursiveMode::Recursive)
            .map_err(|e| Error::Term(Box::new(format!("Failed to watch path {}: {:?}", path, e))))?;
    }

    let resource = ResourceArc::new(FileWatcherResource {
        watcher: Mutex::new(Some(watcher)),
        events,
        deleted_files,
        patterns: patterns_arc,
        paths: paths_clone,
    });

    println!("[Rust] ✓ Watcher started successfully for {} path(s)", paths_arc.len());
    Ok(resource)
}

#[rustler::nif]
fn start_watching(path: String) -> NifResult<ResourceArc<FileWatcherResource>> {
    create_watcher(vec![path], vec![])
}

#[rustler::nif]
fn start_watching_with_patterns(
    path: String,
    patterns: Vec<String>,
) -> NifResult<ResourceArc<FileWatcherResource>> {
    create_watcher(vec![path], patterns)
}

#[rustler::nif]
fn start_watching_multiple(paths: Vec<String>) -> NifResult<ResourceArc<FileWatcherResource>> {
    create_watcher(paths, vec![])
}

#[rustler::nif]
fn start_watching_multiple_with_patterns(
    paths: Vec<String>,
    patterns: Vec<String>,
) -> NifResult<ResourceArc<FileWatcherResource>> {
    create_watcher(paths, patterns)
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
    println!("[Rust] Stopping watcher for {} path(s)", watcher.paths.len());

    if let Ok(mut w) = watcher.watcher.lock() {
        *w = None;
        println!("[Rust] ✓ Watcher stopped");
        true
    } else {
        false
    }
}

rustler::init!("file_watcher", load = on_load);

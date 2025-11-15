use rustler::{NifResult, ResourceArc, Error, Env, Term};
use notify::{Watcher, RecursiveMode, RecommendedWatcher, Event, EventKind};
use std::sync::{Mutex, Arc};
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH, Duration};
use std::collections::HashMap;

// Resource that holds the file watcher and collected events
pub struct FileWatcherResource {
    watcher: Mutex<Option<RecommendedWatcher>>,
    events: Arc<Mutex<Vec<FileEventInfo>>>,
    deleted_files: Arc<Mutex<HashMap<String, u64>>>, // Track recently deleted files
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

#[rustler::nif]
fn start_watching(path: String) -> NifResult<ResourceArc<FileWatcherResource>> {
    println!("[Rust] Starting watcher for: {}", path);

    if !Path::new(&path).exists() {
        return Err(Error::Term(Box::new(format!("Path does not exist: {}", path))));
    }

    let events = Arc::new(Mutex::new(Vec::new()));
    let events_clone = events.clone();
    let deleted_files = Arc::new(Mutex::new(HashMap::new()));
    let deleted_files_clone = deleted_files.clone();

    let watcher = notify::recommended_watcher(move |res: Result<Event, notify::Error>| {
        match res {
            Ok(event) => {
                // Filter out temporary and backup files
                if should_ignore_event(&event) {
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

#[rustler::nif]
fn get_events(watcher: ResourceArc<FileWatcherResource>) -> Vec<String> {
    if let Ok(mut events) = watcher.events.lock() {
        // Clean up old deleted files (older than 5 seconds)
        if let Ok(mut deleted) = watcher.deleted_files.lock() {
            let now = get_timestamp();
            deleted.retain(|_, timestamp| now - *timestamp < 5000);
        }

        // Deduplicate and format events
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

// Get current timestamp in milliseconds
fn get_timestamp() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as u64
}

// Check if we should ignore this event
fn should_ignore_event(event: &Event) -> bool {
    for path in &event.paths {
        let path_str = path.to_string_lossy();

        // Ignore temp files, swap files, and backups
        if path_str.contains("~") ||           // Backup files
           path_str.contains(".swp") ||        // Vim swap files
           path_str.contains(".swx") ||        // Vim swap files
           path_str.contains(".tmp") ||        // Temp files
           path_str.ends_with("#") ||          // Emacs auto-save
           path.file_name()
               .map(|n| n.to_string_lossy().starts_with('.'))
               .unwrap_or(false) ||            // Hidden files starting with .
           path.file_name()
               .and_then(|n| n.to_str())
               .map(|n| n.chars().all(|c| c.is_numeric()))
               .unwrap_or(false)               // Numeric temp files (like 4913)
        {
            return true;
        }

        // Ignore ACCESS events (too noisy)
        if matches!(event.kind, EventKind::Access(_)) {
            return true;
        }
    }

    false
}

// Process event into structured info with smart edit detection
fn process_event(
    event: &Event,
    deleted_files: &Arc<Mutex<HashMap<String, u64>>>,
) -> Option<FileEventInfo> {
    let path = event.paths.first()?;
    let path_str = path.to_string_lossy().to_string();
    let timestamp = get_timestamp();

    let kind = match event.kind {
        EventKind::Create(_) => {
            // Check if this file was recently deleted (within 2 seconds)
            if let Ok(deleted) = deleted_files.lock() {
                if let Some(&delete_time) = deleted.get(&path_str) {
                    if timestamp - delete_time < 2000 {
                        // File was deleted and recreated quickly = MODIFIED
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
            // Track this deletion
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

// Deduplicate events (combine multiple events for same file)
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

        // Determine the most significant event with priority
        let has_delete = events.iter().any(|e| e.kind == "DELETED");
        let has_create = events.iter().any(|e| e.kind == "CREATED");
        let has_modify = events.iter().any(|e| e.kind == "MODIFIED");

        // Priority: DELETED > MODIFIED > CREATED
        // If we have both DELETE and CREATE, it's actually a MODIFY
        let action = if has_delete && !has_create {
            "DELETED"
        } else if has_modify || (has_delete && has_create) {
            "MODIFIED"
        } else if has_create {
            "CREATED"
        } else {
            continue;
        };

        // Add emoji for clarity
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

rustler::init!("file_watcher", load = on_load);

use rustler::{NifResult, ResourceArc, Error, Env, Term};
use notify::{Watcher, RecursiveMode, RecommendedWatcher, Event};
use std::sync::{Mutex, Arc};
use std::path::Path;

// Resource that holds the file watcher and collected events
pub struct FileWatcherResource {
    watcher: Mutex<Option<RecommendedWatcher>>,
    events: Arc<Mutex<Vec<String>>>,
    path: String,
}

// Load function that registers the resource
fn on_load(env: Env, _info: Term) -> bool {
    rustler::resource!(FileWatcherResource, env);
    true
}

#[rustler::nif]
fn start_watching(path: String) -> NifResult<ResourceArc<FileWatcherResource>> {
    println!("[Rust] Starting watcher for: {}", path);

    // Check if path exists
    if !Path::new(&path).exists() {
        return Err(Error::Term(Box::new(format!("Path does not exist: {}", path))));
    }

    let events = Arc::new(Mutex::new(Vec::new()));
    let events_clone = events.clone();

    // Create a watcher that collects events
    let watcher = notify::recommended_watcher(move |res: Result<Event, notify::Error>| {
        match res {
            Ok(event) => {
                let event_str = format_event(&event);
                println!("[Rust] Event detected: {}", event_str);

                if let Ok(mut ev) = events_clone.lock() {
                    ev.push(event_str);
                }
            }
            Err(e) => eprintln!("[Rust] Watch error: {:?}", e),
        }
    }).map_err(|e| Error::Term(Box::new(format!("Failed to create watcher: {:?}", e))))?;

    let resource = ResourceArc::new(FileWatcherResource {
        watcher: Mutex::new(Some(watcher)),
        events,
        path: path.clone(),
    });

    // Start watching the path
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
    // Get and clear collected events
    if let Ok(mut events) = watcher.events.lock() {
        let collected = events.clone();
        events.clear();
        collected
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
        println!("[Rust] ✗ Failed to stop watcher");
        false
    }
}

// Helper function to format events nicely
fn format_event(event: &Event) -> String {
    let kind = match event.kind {
        notify::EventKind::Create(_) => "CREATE",
        notify::EventKind::Modify(_) => "MODIFY",
        notify::EventKind::Remove(_) => "REMOVE",
        notify::EventKind::Access(_) => "ACCESS",
        _ => "OTHER",
    };

    let paths: Vec<String> = event.paths
        .iter()
        .map(|p| p.display().to_string())
        .collect();

    format!("{}: {}", kind, paths.join(", "))
}

// Rustler 0.37 syntax: module name as string, then load function
rustler::init!("file_watcher", load = on_load);

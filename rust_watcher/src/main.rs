use notify::{Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std::io::{self, BufRead, Write, BufReader};
use std::path::Path;
use std::sync::mpsc;
use std::thread;
use std::time::Duration;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Read the directory path from stdin
    let stdin = io::stdin();
    let mut reader = BufReader::new(stdin);
    let mut path_buffer = String::new();

    match reader.read_line(&mut path_buffer) {
        Ok(0) => {
            eprintln!("No input received");
            return Ok(());
        }
        Ok(_) => {
            let watch_path = path_buffer.trim();

            // Verify the path exists
            if !Path::new(watch_path).exists() {
                eprintln!("Error: Path does not exist: {}", watch_path);
                return Ok(());
            }

            let (tx, rx) = mpsc::channel();
            let should_stop = Arc::new(AtomicBool::new(false));
            let should_stop_clone = should_stop.clone();

            // Create file watcher
            let mut watcher = RecommendedWatcher::new(
                move |res: Result<Event, notify::Error>| {
                    if let Ok(event) = res {
                        let _ = tx.send(event);
                    }
                },
                Config::default(),
            )?;

            // Start watching the directory recursively
            watcher.watch(Path::new(watch_path), RecursiveMode::Recursive)?;

            // Spawn thread to handle stdin monitoring (for stop command)
            let _stdin_thread = thread::spawn(move || {
                let mut buffer = String::new();
                let stdin = io::stdin();
                let mut reader = BufReader::new(stdin);

                while !should_stop_clone.load(Ordering::Relaxed) {
                    buffer.clear();
                    match reader.read_line(&mut buffer) {
                        Ok(0) => {
                            // EOF on stdin - parent process died
                            should_stop_clone.store(true, Ordering::Relaxed);
                            break;
                        }
                        Ok(_) => {
                            let line = buffer.trim();
                            if line == "stop" {
                                should_stop_clone.store(true, Ordering::Relaxed);
                                break;
                            }
                        }
                        Err(_) => {
                            should_stop_clone.store(true, Ordering::Relaxed);
                            break;
                        }
                    }
                    thread::sleep(Duration::from_millis(100));
                }
            });

            // Main event processing loop
            while !should_stop.load(Ordering::Relaxed) {
                // Check for events (non-blocking)
                match rx.try_recv() {
                    Ok(event) => {
                        process_file_event(&event);
                    }
                    Err(mpsc::TryRecvError::Empty) => {
                        // No events, continue
                    }
                    Err(mpsc::TryRecvError::Disconnected) => {
                        break;
                    }
                }

                // Small sleep to prevent excessive CPU usage
                thread::sleep(Duration::from_millis(50));
            }
        }
        Err(e) => {
            eprintln!("Error reading directory path: {}", e);
            return Err(Box::new(e));
        }
    }

    Ok(())
}

fn process_file_event(event: &Event) {
    for path in &event.paths {
        // Filter out temporary files
        if is_temp_file(path) {
            continue;
        }

        let event_type = match event.kind {
            EventKind::Create(_) => "created",
            EventKind::Modify(_) => "modified",
            EventKind::Remove(_) => "deleted",
            EventKind::Access(_) => {
                // Skip access events
                continue;
            }
            _ => {
                // Skip other event types
                continue;
            }
        };

        let path_str = path.to_string_lossy().to_string();
        let is_directory = path.is_dir();
        let timestamp = chrono::Utc::now().timestamp_millis();

        // Send event to parent process
        send_file_event(event_type, &path_str, is_directory, timestamp);
    }
}

fn is_temp_file(path: &Path) -> bool {
    if let Some(filename) = path.file_name().and_then(|f| f.to_str()) {
        filename.ends_with('~') ||                           // Editor backups
        filename.starts_with(".#") ||                        // Emacs lock files
        filename.ends_with(".swp") ||                        // Vim swap files
        filename.ends_with(".swo") ||                        // Vim temporary files
        filename.ends_with(".tmp") ||                        // Temporary files
        filename.starts_with("#") && filename.ends_with("#") || // Emacs auto-save
        filename == ".DS_Store" ||                           // macOS system files
        filename == "Thumbs.db" ||                           // Windows thumbnails
        filename.contains(".git") && !path.is_dir() ||       // Git files (allow .git dirs)
        filename.starts_with("._") ||                        // macOS resource forks
        filename.ends_with(".lock") ||                       // Lock files
        filename.contains("~$") ||                           // Office temporary files
        filename.starts_with(".tmp") ||                      // Hidden temporary files
        filename.contains(".temp") ||                        // Temporary variations
        filename.ends_with(".bak") ||                        // Backup files
        filename.ends_with(".orig")                          // Original files
    } else {
        false
    }
}

fn send_file_event(event_type: &str, path: &str, is_directory: bool, timestamp: i64) {
    // Send event in pipe-delimited format for easy parsing
    let output = format!("{}|{}|{}|{}", event_type, path, is_directory, timestamp);
    println!("{}", output);

    // Ensure immediate delivery
    let _ = io::stdout().flush();
}

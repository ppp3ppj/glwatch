use notify::{Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std::io::{self, BufRead, Write, BufReader};
use std::path::Path;
use std::sync::mpsc;
use std::thread;
use std::time::Duration;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    eprintln!("🦀 Halloween Rust File Watcher Starting...");

    // Read the directory path from Erlang via stdin
    let stdin = io::stdin();
    let mut reader = BufReader::new(stdin);
    let mut path_buffer = String::new();

    match reader.read_line(&mut path_buffer) {
        Ok(0) => {
            eprintln!("❌ No input received (EOF)");
            return Ok(());
        }
        Ok(n) => {
            let watch_path = path_buffer.trim();
            eprintln!("👻 Received {} bytes, watching: '{}'", n, watch_path);

            // Verify the path exists
            if !Path::new(watch_path).exists() {
                eprintln!("❌ ERROR: Path does not exist: {}", watch_path);
                return Ok(());
            }

            eprintln!("✅ Path exists, setting up watcher...");

            // Setup file watcher with Halloween magic! 🎃
            let (tx, rx) = mpsc::channel();
            let should_stop = Arc::new(AtomicBool::new(false));
            let should_stop_clone = should_stop.clone();

            let mut watcher = RecommendedWatcher::new(
                move |res: Result<Event, notify::Error>| {
                    eprintln!("🔔 Rust watcher callback triggered!");
                    if let Ok(event) = res {
                        eprintln!("📨 Event received: {:?}", event);
                        let _ = tx.send(event);
                    } else {
                        eprintln!("❌ Watcher error: {:?}", res);
                    }
                },
                Config::default(),
            )?;

            // Start watching the directory recursively
            eprintln!("🔮 Adding watch for: {}", watch_path);
            watcher.watch(Path::new(watch_path), RecursiveMode::Recursive)?;
            eprintln!("✅ Watcher initialized successfully!");

            // Spawn thread to handle stdin monitoring (for stop command)
            let _stdin_thread = thread::spawn(move || {
                let mut buffer = String::new();
                let stdin = io::stdin();
                let mut reader = BufReader::new(stdin);

                while !should_stop_clone.load(Ordering::Relaxed) {
                    buffer.clear();
                    match reader.read_line(&mut buffer) {
                        Ok(0) => {
                            eprintln!("🛑 EOF on stdin - parent died");
                            should_stop_clone.store(true, Ordering::Relaxed);
                            break;
                        }
                        Ok(_) => {
                            let line = buffer.trim();
                            eprintln!("📨 Received command: '{}'", line);
                            if line == "stop" {
                                eprintln!("🛑 Stop command received");
                                should_stop_clone.store(true, Ordering::Relaxed);
                                break;
                            }
                        }
                        Err(e) => {
                            eprintln!("❌ Error reading stdin: {}", e);
                            should_stop_clone.store(true, Ordering::Relaxed);
                            break;
                        }
                    }
                    thread::sleep(Duration::from_millis(100));
                }
                eprintln!("🧵 Stdin thread exiting");
            });

            // Main event processing loop
            let mut event_count = 0;
            eprintln!("🔄 Entering main event loop...");

            while !should_stop.load(Ordering::Relaxed) {
                // Check for events (non-blocking)
                match rx.try_recv() {
                    Ok(event) => {
                        event_count += 1;
                        eprintln!("🎃 Processing event #{}: {:?}", event_count, event);
                        process_spooky_event(&event);
                    }
                    Err(mpsc::TryRecvError::Empty) => {
                        // No events, continue
                    }
                    Err(mpsc::TryRecvError::Disconnected) => {
                        eprintln!("❌ Event channel disconnected");
                        break;
                    }
                }

                // Small sleep to prevent excessive CPU usage
                thread::sleep(Duration::from_millis(50));

                // Periodic heartbeat
                if event_count % 100 == 0 && event_count > 0 {
                    eprintln!("💓 Processed {} events so far", event_count);
                }
            }

            eprintln!("👻 Halloween Rust watcher shutting down gracefully");
            eprintln!("📊 Total events processed: {}", event_count);
        }
        Err(e) => {
            eprintln!("❌ Error reading directory path: {}", e);
            return Err(Box::new(e));
        }
    }

    Ok(())
}

fn process_spooky_event(event: &Event) {
    eprintln!("🔍 Event details: kind={:?}, paths={:?}", event.kind, event.paths);

    for path in &event.paths {
        // Filter out cursed temporary files! 👻
        if is_cursed_temp_file(path) {
            eprintln!("🚫 Ignoring cursed temp file: {:?}", path);
            continue;
        }

        let event_type = match event.kind {
            EventKind::Create(_) => "created",
            EventKind::Modify(_) => "modified",
            EventKind::Remove(_) => "deleted",
            EventKind::Access(_) => {
                eprintln!("⏭️ Skipping access event for: {:?}", path);
                continue;
            }
            other => {
                eprintln!("⏭️ Skipping event type {:?} for: {:?}", other, path);
                continue;
            }
        };

        let path_str = path.to_string_lossy().to_string();
        let is_directory = path.is_dir();
        let timestamp = chrono::Utc::now().timestamp_millis();

        // Send spooky event to Erlang! 🔥
        eprintln!("📤 Sending event: {} | {} | {} | {}", event_type, path_str, is_directory, timestamp);
        send_halloween_event(event_type, &path_str, is_directory, timestamp);
    }
}

fn is_cursed_temp_file(path: &Path) -> bool {
    if let Some(filename) = path.file_name().and_then(|f| f.to_str()) {
        let is_cursed =
            filename.ends_with('~') ||                           // Editor backups: file.txt~
            filename.starts_with(".#") ||                        // Emacs lock files
            filename.ends_with(".swp") ||                        // Vim swap files
            filename.ends_with(".swo") ||                        // More Vim files
            filename.ends_with(".tmp") ||                        // Temporary files
            filename.starts_with("#") && filename.ends_with("#") || // Emacs auto-save
            filename == ".DS_Store" ||                           // macOS cursed files
            filename == "Thumbs.db" ||                           // Windows cursed files
            filename.contains(".git") && !path.is_dir() ||       // Git files (but allow .git dirs)
            filename.starts_with("._") ||                        // macOS resource fork spirits
            filename.ends_with(".lock") ||                       // Lock file ghosts
            filename.contains("~$") ||                           // Office temp files
            filename.starts_with(".tmp") ||                      // Hidden temp files
            filename.contains(".temp") ||                        // Any temp variations
            filename.ends_with(".bak") ||                        // Backup files
            filename.ends_with(".orig");                         // Original files

        if is_cursed {
            eprintln!("👻 Cursed file detected: {}", filename);
        }

        is_cursed
    } else {
        false
    }
}

fn send_halloween_event(event_type: &str, path: &str, is_directory: bool, timestamp: i64) {
    // Send event in pipe-delimited format for easy Erlang parsing
    let output = format!("{}|{}|{}|{}", event_type, path, is_directory, timestamp);
    println!("{}", output);
    eprintln!("✅ Sent to stdout: {}", output);

    // Ensure immediate delivery to Erlang (Halloween magic requires speed!)
    let _ = io::stdout().flush();
}

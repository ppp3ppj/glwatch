import gleam/io
import gleam/list
import gleam/int

pub type WatcherRef

// Single directory (backward compatible)
@external(erlang, "file_watcher", "start_watching")
fn start_watching(directory: String) -> WatcherRef

@external(erlang, "file_watcher", "start_watching_with_patterns")
fn start_watching_with_patterns(
  directory: String,
  patterns: List(String),
) -> WatcherRef

// Multiple directories (NEW)
@external(erlang, "file_watcher", "start_watching_multiple")
fn start_watching_multiple(directories: List(String)) -> WatcherRef

@external(erlang, "file_watcher", "start_watching_multiple_with_patterns")
fn start_watching_multiple_with_patterns(
  directories: List(String),
  patterns: List(String),
) -> WatcherRef

@external(erlang, "file_watcher", "get_events")
fn get_events(watcher: WatcherRef) -> List(String)

@external(erlang, "file_watcher", "stop_watching")
fn stop_watching(watcher: WatcherRef) -> Bool

@external(erlang, "timer", "sleep")
fn sleep(milliseconds: Int) -> Nil

@external(erlang, "erlang", "system_time")
fn system_time(unit: Int) -> Int

pub fn main() {
  print_banner()

  // Choose your mode:
  // start_with_patterns()           // Single directory with patterns
  // start_continuous_watch()        // Single directory, all files
  //start_multiple_directories()    // Multiple directories (NEW!)
  start_multiple_with_patterns()    // Multiple derictories with patterns
}

fn print_banner() {
  io.println("╔════════════════════════════════════════╗")
  io.println("║   GLWATCH v1.2.0                      ║")
  io.println("║   Smart File System Monitor           ║")
  io.println("║   with Multi-Directory Support        ║")
  io.println("╚════════════════════════════════════════╝")
  io.println("")
}

// NEW: Watch multiple directories
fn start_multiple_directories() {
  let watch_paths = ["./watched", "./src", "./test"]

  io.println("🔍 Starting file watcher for multiple directories...")
  io.println("📂 Watching:")
  list.each(watch_paths, fn(p) { io.println("   • " <> p) })
  io.println("")

  let watcher = start_watching_multiple(watch_paths)

  io.println("✅ Watcher started successfully!")
  io.println("⚡ Monitoring file system changes in all directories")
  io.println("🛑 Press Ctrl+C to stop")
  io.println("")
  io.println("💡 Try creating files in any watched directory:")
  io.println("   echo 'test' > watched/test.txt")
  io.println("   echo 'code' > src/new.gleam")
  io.println("   echo 'test' > test/new_test.gleam")
  io.println("")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  io.println("")

  let start_time = system_time(1000)
  watch_loop(watcher, start_time, 0, 0)
}

// NEW: Watch multiple directories with patterns
fn start_multiple_with_patterns() {
  let watch_paths = ["./watched", "./src", "./test"]
  let patterns = ["**/*.gleam", "**/*.js", "**/*.txt"]

  io.println("🔍 Starting file watcher for multiple directories...")
  io.println("📂 Watching:")
  list.each(watch_paths, fn(p) { io.println("   • " <> p) })
  io.println("")
  io.println("🎯 Patterns:")
  list.each(patterns, fn(p) { io.println("   • " <> p) })
  io.println("")

  let watcher = start_watching_multiple_with_patterns(watch_paths, patterns)

  io.println("✅ Watcher started successfully!")
  io.println("⚡ Monitoring file system changes")
  io.println("🛑 Press Ctrl+C to stop")
  io.println("")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  io.println("")

  let start_time = system_time(1000)
  watch_loop(watcher, start_time, 0, 0)
}

// Watch single directory with patterns
fn start_with_patterns() {
  let watch_path = "./watched"
  let patterns = ["**/*.gleam", "**/*.js", "**/*.txt"]

  io.println("🔍 Starting file watcher with patterns...")
  io.println("📂 Target: " <> watch_path)
  io.println("🎯 Patterns:")
  list.each(patterns, fn(p) { io.println("   • " <> p) })
  io.println("")

  let watcher = start_watching_with_patterns(watch_path, patterns)

  io.println("✅ Watcher started successfully!")
  io.println("⚡ Monitoring file system changes")
  io.println("🛑 Press Ctrl+C to stop")
  io.println("")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  io.println("")

  let start_time = system_time(1000)
  watch_loop(watcher, start_time, 0, 0)
}

// Watch single directory (all files)
fn start_continuous_watch() {
  let watch_path = "./watched"

  io.println("🔍 Starting file watcher...")
  io.println("📂 Watching: " <> watch_path)
  io.println("🎯 Mode: All files")
  io.println("")

  let watcher = start_watching(watch_path)

  io.println("✅ Watcher started successfully!")
  io.println("⚡ Monitoring file system changes")
  io.println("🛑 Press Ctrl+C to stop")
  io.println("")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  io.println("")

  let start_time = system_time(1000)
  watch_loop(watcher, start_time, 0, 0)
}

fn watch_loop(
  watcher: WatcherRef,
  start_time: Int,
  tick: Int,
  total_events: Int,
) -> Nil {
  sleep(1000)
  let events = get_events(watcher)
  let event_count = list.length(events)

  case event_count {
    0 -> {
      case tick % 30 {
        0 -> {
          let elapsed = get_elapsed_time(start_time)
          io.println(
            "💚 Watching | Uptime: " <> elapsed <> " | Events: " <> int.to_string(
              total_events,
            ),
          )
        }
        _ -> Nil
      }
      watch_loop(watcher, start_time, tick + 1, total_events)
    }
    _ -> {
      let timestamp = format_time()
      io.println("🔔 [" <> timestamp <> "] Changes detected:")

      list.each(events, fn(event) { io.println("   " <> event) })
      io.println("")

      watch_loop(watcher, start_time, tick + 1, total_events + event_count)
    }
  }
}

fn format_time() -> String {
  let ms = system_time(1000)
  let secs = ms / 1000
  let hours = { secs / 3600 } % 24
  let minutes = { secs / 60 } % 60
  let seconds = secs % 60

  pad_zero(hours) <> ":" <> pad_zero(minutes) <> ":" <> pad_zero(seconds)
}

fn pad_zero(n: Int) -> String {
  case n < 10 {
    True -> "0" <> int.to_string(n)
    False -> int.to_string(n)
  }
}

fn get_elapsed_time(start_time: Int) -> String {
  let now = system_time(1000)
  let elapsed_ms = now - start_time
  let seconds = elapsed_ms / 1000
  let minutes = seconds / 60
  let hours = minutes / 60

  case hours > 0 {
    True -> int.to_string(hours) <> "h " <> int.to_string(minutes % 60) <> "m"
    False ->
      case minutes > 0 {
        True ->
          int.to_string(minutes) <> "m " <> int.to_string(seconds % 60) <> "s"
        False -> int.to_string(seconds) <> "s"
      }
  }
}

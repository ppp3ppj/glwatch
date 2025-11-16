import gleam/io
import gleam/list
import gleam/int

pub type WatcherRef

@external(erlang, "file_watcher", "start_watching")
fn start_watching(directory: String) -> WatcherRef

@external(erlang, "file_watcher", "start_watching_with_patterns")
fn start_watching_with_patterns(
  directory: String,
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

  // Example: Watch with patterns
  start_with_patterns()

  // Or watch everything:
  // start_continuous_watch()
}

fn print_banner() {
  io.println("╔════════════════════════════════════════╗")
  io.println("║   GLWATCH v1.1.0                      ║")
  io.println("║   Smart File System Monitor           ║")
  io.println("║   with Pattern Matching               ║")
  io.println("╚════════════════════════════════════════╝")
  io.println("")
}

// Watch with pattern matching
fn start_with_patterns() {
  let watch_path = "./watched"

  // Define patterns to watch
  let patterns = [
    "**/*.gleam",  // All Gleam files
    "**/*.js",     // All JavaScript files
    "**/*.txt",    // All text files
    "**/*.rs",    // All Rust files
    // "src/**/*",  // Everything in src/
  ]

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
  io.println("💡 Try creating files:")
  io.println("   echo 'test' > watched/test.txt     (✓ will show)")
  io.println("   echo 'test' > watched/test.js      (✓ will show)")
  io.println("   echo 'test' > watched/test.log     (✗ will not show)")
  io.println("")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  io.println("")

  let start_time = system_time(1000)
  watch_loop(watcher, start_time, 0, 0)
}

// Watch everything (no patterns)
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

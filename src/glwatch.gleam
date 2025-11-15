import gleam/io
import gleam/list
import gleam/int
import gleam/string

pub type WatcherRef

@external(erlang, "file_watcher", "start_watching")
fn start_watching(directory: String) -> WatcherRef

@external(erlang, "file_watcher", "get_events")
fn get_events(watcher: WatcherRef) -> List(String)

@external(erlang, "timer", "sleep")
fn sleep(milliseconds: Int) -> Nil

// Get current timestamp in milliseconds
@external(erlang, "erlang", "system_time")
fn system_time(unit: Int) -> Int

pub fn main() {
  print_banner()
  start_continuous_watch()
}

fn print_banner() {
  io.println("╔════════════════════════════════════════╗")
  io.println("║   GLWATCH v1.0.0                      ║")
  io.println("║   Real-time File System Monitor       ║")
  io.println("║   by @ppp3ppj                         ║")
  io.println("╚════════════════════════════════════════╝")
  io.println("")
}

fn start_continuous_watch() {
  let watch_path = "./watched"

  io.println("🚀 Initializing...")
  io.println("📂 Target: " <> watch_path)
  io.println("")

  let watcher = start_watching(watch_path)

  io.println("✅ Watcher is now ACTIVE")
  io.println("⚡ Real-time monitoring enabled")
  io.println("🛑 Press Ctrl+C to stop")
  io.println("")
  io.println("📝 Test commands:")
  io.println("   echo 'test' > watched/file.txt")
  io.println("   mkdir watched/newdir")
  io.println("   rm watched/file.txt")
  io.println("")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  io.println("")

  let start_time = system_time(1000)  // milliseconds
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
      // Heartbeat every 30 seconds
      case tick % 30 {
        0 -> {
          let elapsed = get_elapsed_time(start_time)
          io.println(
            "💚 Active | Uptime: " <> elapsed <> " | Total Events: " <> int.to_string(
              total_events,
            ),
          )
        }
        _ -> Nil
      }
      watch_loop(watcher, start_time, tick + 1, total_events)
    }
    _ -> {
      // Events detected
      let timestamp = get_current_time()
      io.println("🔔 [" <> timestamp <> "] " <> int.to_string(event_count) <> " event(s):")

      list.each(events, fn(event) {
        io.println("   📄 " <> event)
      })
      io.println("")

      watch_loop(watcher, start_time, tick + 1, total_events + event_count)
    }
  }
}

fn get_current_time() -> String {
  let ms = system_time(1000)
  // Simple timestamp format (you can enhance this)
  int.to_string(ms)
}

fn get_elapsed_time(start_time: Int) -> String {
  let now = system_time(1000)
  let elapsed_ms = now - start_time
  let seconds = elapsed_ms / 1000
  let minutes = seconds / 60
  let hours = minutes / 60

  case hours > 0 {
    True -> int.to_string(hours) <> "h " <> int.to_string(minutes % 60) <> "m"
    False -> case minutes > 0 {
      True -> int.to_string(minutes) <> "m " <> int.to_string(seconds % 60) <> "s"
      False -> int.to_string(seconds) <> "s"
    }
  }
}

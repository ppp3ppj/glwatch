import gleam/io
import gleam/list
import gleam/int
import gleam/string
import argv
import config.{type WatchConfig, type ActionConfig}

pub type WatcherRef

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

// Fix: Create Erlang wrapper for os:cmd
@external(erlang, "glwatch_ffi", "exec_command")
fn exec_command(command: String) -> String

pub fn main() {
  print_banner()

  let args = argv.load().arguments
  let config_file = case args {
    ["--config", file, ..] -> file
    ["-c", file, ..] -> file
    _ -> "glwatch.toml"
  }

  io.println("📄 Loading config from: " <> config_file)
  io.println("")

  case config.parse_config(config_file) {
    Ok(watch_config) -> {
      config.print_config(watch_config)
      start_with_config(watch_config)
    }
    Error(err) -> {
      io.println("❌ Error loading config: " <> err)
      io.println("")
      print_usage()
    }
  }
}

fn print_banner() {
  io.println("╔════════════════════════════════════════╗")
  io.println("║   GLWATCH v1.3.0                      ║")
  io.println("║   Smart File System Monitor           ║")
  io.println("║   with Custom Action Triggers         ║")
  io.println("╚════════════════════════════════════════╝")
  io.println("")
}

fn print_usage() {
  io.println("💡 Usage:")
  io.println("   gleam run                           # Uses glwatch.toml")
  io.println("   gleam run -- --config spec.toml     # Uses spec.toml")
  io.println("   gleam run -- -c myconfig.toml       # Uses myconfig.toml")
}

fn start_with_config(watch_config: WatchConfig) {
  let all_patterns =
    watch_config.actions
    |> list.flat_map(fn(action) { action.patterns })

  let watch_paths = ["./watched", "./test"]

  io.println("🔍 Starting file watcher with custom actions...")
  io.println("📂 Watching: " <> list_to_string(watch_paths))
  io.println("🎯 Loaded " <> int.to_string(list.length(watch_config.actions)) <> " action(s)")
  io.println("")
  io.println("✅ Watcher started successfully!")
  io.println("⚡ Monitoring file system changes")
  io.println("🛑 Press Ctrl+C to stop")
  io.println("")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  io.println("")

  let watcher = start_watching_multiple_with_patterns(watch_paths, all_patterns)
  let start_time = system_time(1000)

  watch_loop(watcher, watch_config, start_time, 0, 0)
}

fn watch_loop(
  watcher: WatcherRef,
  config: WatchConfig,
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
      watch_loop(watcher, config, start_time, tick + 1, total_events)
    }
    _ -> {
      let timestamp = format_time()
      io.println("🔔 [" <> timestamp <> "] Changes detected:")

      list.each(events, fn(event) {
        io.println("   " <> event)
        execute_matching_actions(event, config.actions)
      })

      io.println("")

      watch_loop(watcher, config, start_time, tick + 1, total_events + event_count)
    }
  }
}

fn execute_matching_actions(event: String, actions: List(ActionConfig)) {
  let parts = string.split(event, " ")

  case parts {
    [_emoji, event_type, filename, ..] -> {
      list.each(actions, fn(action) {
        let status_match = config.matches_status(event_type, action.status)
        let pattern_match = matches_any_pattern(filename, action.patterns)

        case status_match && pattern_match {
          True -> {
            io.println("")
            io.println("   ▶️  Executing [" <> action.name <> "]: " <> action.command)

            let output = exec_command(action.command)
            let trimmed = string.trim(output)

            case trimmed {
              "" -> io.println("      ✅ Command completed")
              _ -> {
                let lines = string.split(trimmed, "\n")
                list.each(lines, fn(line) {
                  io.println("      " <> line)
                })
                io.println("      ✅ Done")
              }
            }
          }
          False -> Nil
        }
      })
    }
    _ -> Nil
  }
}

fn matches_any_pattern(filename: String, patterns: List(String)) -> Bool {
  list.any(patterns, fn(pattern) {
    case pattern {
      "**/*.js" -> string.ends_with(filename, ".js")
      "**/*.hs" -> string.ends_with(filename, ".hs")
      "**/*.gleam" -> string.ends_with(filename, ".gleam")
      "**/*.css" -> string.ends_with(filename, ".css")
      "**/*.rs" -> string.ends_with(filename, ".rs")
      "**/*.md" -> string.ends_with(filename, ".md")
      "**/*.ts" -> string.ends_with(filename, ".ts")
      "**/*.py" -> string.ends_with(filename, ".py")
      "**/*_test.gleam" -> string.contains(filename, "_test.gleam")
      _ -> False
    }
  })
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

fn list_to_string(items: List(String)) -> String {
  case items {
    [] -> ""
    [first] -> first
    [first, ..rest] -> first <> ", " <> list_to_string(rest)
  }
}

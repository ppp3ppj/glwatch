import gleam/io
import gleam/list
import gleam/string

// Types
pub type WatcherRef {
  WatcherRef(pid: String)
}

pub type FileEvent {
  FileEvent(
    event_type: EventType,
    path: String,
    is_directory: Bool,
    timestamp: Int
  )
}

pub type EventType {
  Created
  Deleted
  Modified
  Unknown
}

pub fn main() {
  io.println("🎃 GLWATCH - Halloween File Monitor 🎃")
  io.println("═══════════════════════════════════════════")
  io.println("")

  start_glwatch()
}

fn start_glwatch() -> Nil {
  io.println("🔮 Starting file watcher...")

  let watcher = erlang_start_watching("watched/")

  io.println("✅ GLWATCH is active!")
  io.println("🎬 Try: echo 'BOO!' > watched/test.txt")
  io.println("")

  glwatch_loop(watcher)
}

fn glwatch_loop(watcher: WatcherRef) -> Nil {
  case get_events_safe(watcher) {
    [] -> {
      io.println("👁️  Monitoring... (create/delete files in watched/ folder)")
      sleep(3000)
      glwatch_loop(watcher)
    }
    events -> {
      io.println("🔥 GLEAM RECEIVED " <> string.inspect(list.length(events)) <> " EVENTS:")

      list.each(events, process_event)
      io.println("")

      glwatch_loop(watcher)
    }
  }
}

fn process_event(event: FileEvent) -> Nil {
  let FileEvent(event_type, path, is_directory, _timestamp) = event

  let emoji = case event_type {
    Created -> "🆕"
    Deleted -> "🗑️"
    Modified -> "📝"
    Unknown -> "❓"
  }

  let item_type = case is_directory {
    True -> "DIR"
    False -> "FILE"
  }

  let filename = get_filename(path)

  io.println("  " <> emoji <> " " <> item_type <> ": " <> filename)
}

// Safe event conversion
fn get_events_safe(watcher: WatcherRef) -> List(FileEvent) {
  case erlang_get_events(watcher) {
    raw_events -> {
      list.filter_map(raw_events, convert_event)
    }
  }
}

// FIXED: Removed unreachable pattern
fn convert_event(raw_event) -> Result(FileEvent, Nil) {
  case raw_event {
    #(type_str, path, is_dir, timestamp) -> {
      let event_type = case type_str {
        "created" -> Created
        "deleted" -> Deleted
        "modified" -> Modified
        _ -> Unknown
      }

      Ok(FileEvent(
        event_type: event_type,
        path: path,
        is_directory: is_dir,
        timestamp: timestamp
      ))
    }
  }
}

fn get_filename(path: String) -> String {
  case string.split(path, "/") {
    [] -> path
    parts -> {
      case list.last(parts) {
        Ok(filename) -> filename
        Error(_) -> path
      }
    }
  }
}

// External calls
@external(erlang, "file_watcher", "start_watching")
fn erlang_start_watching(directory: String) -> WatcherRef

@external(erlang, "file_watcher", "get_events")
fn erlang_get_events(watcher: WatcherRef) -> List(a)

@external(erlang, "timer", "sleep")
fn sleep(milliseconds: Int) -> Nil

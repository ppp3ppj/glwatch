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
}

pub fn main() {
  io.println("GLWATCH - File System Monitor")
  io.println("=============================")
  io.println("Professional file watcher for development")
  io.println("")

  start_glwatch()
}

fn start_glwatch() -> Nil {
  io.println("[INFO] Initializing file watcher...")

  let watcher = erlang_start_watching("watched/")

  io.println("[INFO] GLWATCH is now active and monitoring")
  io.println("[INFO] Test commands:")
  io.println("  echo 'Hello World' > watched/test.txt")
  io.println("  echo 'More content' >> watched/test.txt")
  io.println("  rm watched/test.txt")
  io.println("")

  glwatch_loop(watcher)
}

fn glwatch_loop(watcher: WatcherRef) -> Nil {
  case get_events_safe(watcher) {
    [] -> {
      io.println("[STATUS] Monitoring for file system changes...")
      sleep(2000)
      glwatch_loop(watcher)
    }
    events -> {
      io.println("")
      io.println("[EVENTS] Received " <> string.inspect(list.length(events)) <> " file system events:")

      list.each(events, process_event)
      io.println("")

      glwatch_loop(watcher)
    }
  }
}

fn process_event(event: FileEvent) -> Nil {
  let FileEvent(event_type, path, is_directory, timestamp) = event

  let action = case event_type {
    Created -> "CREATED"
    Deleted -> "DELETED"
    Modified -> "MODIFIED"
  }

  let item_type = case is_directory {
    True -> "DIRECTORY"
    False -> "FILE"
  }

  let filename = get_filename(path)
  let time_str = format_timestamp(timestamp)

  io.println("  [" <> time_str <> "] " <> action <> " " <> item_type <> ": " <> filename)
}

// Safe event conversion - simplified approach
fn get_events_safe(watcher: WatcherRef) -> List(FileEvent) {
  case erlang_get_events(watcher) {
    raw_events -> {
      io.println("[DEBUG] Received " <> string.inspect(list.length(raw_events)) <> " raw events from Erlang")
      list.filter_map(raw_events, convert_event_safe)
    }
  }
}

// Convert event using pattern matching (back to original approach with better error handling)
fn convert_event_safe(raw_event) -> Result(FileEvent, Nil) {
  io.println("[DEBUG] Raw event format: " <> string.inspect(raw_event))

  case raw_event {
    #(type_str, path, is_dir, timestamp) -> {
      io.println("[DEBUG] Successfully matched tuple pattern")

      let event_type = case type_str {
        "created" -> Created
        "deleted" -> Deleted
        "modified" -> Modified
        _ -> {
          io.println("[WARN] Unknown event type: " <> string.inspect(type_str) <> ", defaulting to Modified")
          Modified
        }
      }

      Ok(FileEvent(
        event_type: event_type,
        path: path,
        is_directory: is_dir,
        timestamp: timestamp
      ))
    }
    _ -> {
      io.println("[ERROR] Event format did not match expected tuple pattern")
      io.println("[ERROR] Raw event was: " <> string.inspect(raw_event))
      Error(Nil)
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

fn format_timestamp(timestamp: Int) -> String {
  // Simple timestamp formatting - shows last 6 digits for readability
  let ts_str = string.inspect(timestamp)
  let len = string.length(ts_str)
  case len > 6 {
    True -> {
      let drop_count = len - 6
      string.drop_start(ts_str, drop_count)
    }
    False -> ts_str
  }
}

// External function calls
@external(erlang, "file_watcher", "start_watching")
fn erlang_start_watching(directory: String) -> WatcherRef

@external(erlang, "file_watcher", "get_events")
fn erlang_get_events(watcher: WatcherRef) -> List(a)

@external(erlang, "timer", "sleep")
fn sleep(milliseconds: Int) -> Nil

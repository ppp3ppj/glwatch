import gleam/io
import gleam/list
import gleam/string

// Simple types
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
  io.println("🔍 GLWATCH - Simple File Monitor")
  io.println("================================")
  io.println("")

  start_simple_watcher()
}

fn start_simple_watcher() -> Nil {
  io.println("Starting watcher...")

  let watcher = erlang_start_watching("watched/")

  io.println("✅ Watching 'watched/' directory recursively")
  io.println("Try: echo 'test' > watched/test.txt")
  io.println("Try: mkdir watched/subdir && echo 'nested' > watched/subdir/file.txt")
  io.println("")

  simple_loop(watcher)
}

fn simple_loop(watcher: WatcherRef) -> Nil {
  case get_events(watcher) {
    [] -> {
      io.println("👀 Watching...")
      sleep(2000)
      simple_loop(watcher)
    }
    events -> {
      io.println("")
      io.println("📝 " <> string.inspect(list.length(events)) <> " changes:")
      io.println("PPP: " <> string.inspect(events) <> " changes:")

      list.each(events, show_event)
      io.println("")

      simple_loop(watcher)
    }
  }
}

fn show_event(event: FileEvent) -> Nil {
  let FileEvent(event_type, path, is_directory, _timestamp) = event

  let action = case event_type {
    Created -> "+"
    Deleted -> "-"
    Modified -> "~"
  }

  io.println("Action: "<> action)

  let kind = case is_directory {
    True -> "📁"
    False -> "📄"
  }

  let simple_path = get_simple_path(path)
  let indent = get_simple_indent(simple_path)

  io.println("  " <> indent <> action <> " " <> kind <> " " <> simple_path)
}

fn get_events(watcher: WatcherRef) -> List(FileEvent) {
  case erlang_get_events(watcher) {
    raw_events -> {
      io.println("[DEBUG] Got " <> string.inspect(list.length(raw_events)) <> " raw events")
      list.filter_map(raw_events, convert_simple_event)
    }
  }
}

fn convert_simple_event(raw_event) -> Result(FileEvent, Nil) {
  io.println("[DEBUG] Converting: " <> string.inspect(raw_event))

  case raw_event {
    #(type_atom, path, is_dir, timestamp) -> {
      let event_type = case type_atom {
        created -> Created
        deleted -> Deleted
        modified -> Modified
        _ -> Modified
      }

      // Safe path conversion - handle any data type
      let path_str = safe_to_string(path)

      io.println("[DEBUG] Converted path: " <> path_str)

      Ok(FileEvent(
        event_type: event_type,
        path: path_str,
        is_directory: is_dir,
        timestamp: timestamp
      ))
    }
    _ -> {
      io.println("[ERROR] Unknown event format")
      Error(Nil)
    }
  }
}

// Safe conversion that handles any data type
fn safe_to_string(value) -> String {
  // First, just use string.inspect which works with any data type
  let inspected = string.inspect(value)

  // If it looks like a binary string (starts with <<), try to clean it up
  case inspected {
    "<<\"" <> rest -> {
      // Extract the string content from <<"string">>
      case string.split(rest, "\"") {
        [content, ..] -> content
        [] -> inspected
      }
    }
    // If it's already a regular string (starts with "), clean it up
    "\"" <> rest -> {
      case string.split(rest, "\"") {
      [content, ..] -> content
        [] -> inspected
      }
    }
    // For anything else, just return as-is
    _ -> inspected
  }
}

fn get_simple_path(full_path: String) -> String {
  case string.split(full_path, "/") {
    [] -> full_path
    parts -> {
      case find_watched_part(parts) {
        Ok(remaining) -> {
          case remaining {
            [] -> "/"
            _ -> string.join(remaining, "/")
          }
        }
        Error(_) -> filename_only(full_path)
      }
    }
  }
}

fn find_watched_part(parts: List(String)) -> Result(List(String), Nil) {
  case parts {
    [] -> Error(Nil)
    ["watched", ..rest] -> Ok(rest)
    [_head, ..tail] -> find_watched_part(tail)
  }
}

fn filename_only(path: String) -> String {
  case string.split(path, "/") {
    [] -> path
    parts -> {
      case list.last(parts) {
        Ok(name) -> name
        Error(_) -> path
      }
    }
  }
}

fn get_simple_indent(path: String) -> String {
  let depth = case string.split(path, "/") {
    [] -> 0
    parts -> list.length(parts) - 1
  }

  case depth {
    0 -> ""
    1 -> "  "
    2 -> "    "
    _ -> "      "
  }
}

// External functions
@external(erlang, "file_watcher", "start_watching")
fn erlang_start_watching(directory: String) -> WatcherRef

@external(erlang, "file_watcher", "get_events")
fn erlang_get_events(watcher: WatcherRef) -> List(a)

@external(erlang, "timer", "sleep")
fn sleep(milliseconds: Int) -> Nil

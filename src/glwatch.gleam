import gleam/io
import gleam/list
import gleam/string
import gleam/bit_array

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
  io.println("✨ Now with MODIFICATION DETECTION! ✨")
  io.println("")

  start_glwatch()
}

fn start_glwatch() -> Nil {
  io.println("🔮 Starting enhanced file watcher...")

  let watcher = erlang_start_watching("watched/")

  io.println("✅ GLWATCH is active with modification detection!")
  io.println("🎬 Try these commands:")
  io.println("  echo 'BOO!' > watched/test.txt")
  io.println("  echo 'MORE BOO!' >> watched/test.txt  # This will trigger MODIFIED")
  io.println("  rm watched/test.txt")
  io.println("")

  glwatch_loop(watcher)
}

fn glwatch_loop(watcher: WatcherRef) -> Nil {
  case get_events_safe(watcher) {
    [] -> {
      io.println("👁️  Monitoring... (create/modify/delete files in watched/)")
      sleep(2000)  // Faster polling for better modification detection
      glwatch_loop(watcher)
    }
    events -> {
      io.println("🔥 GLEAM RECEIVED " <> string.inspect(list.length(events)) <> " EVENTS:")

      list.each(events, process_enhanced_event)
      io.println("")

      glwatch_loop(watcher)
    }
  }
}

fn process_enhanced_event(event: FileEvent) -> Nil {
  let FileEvent(event_type, path, is_directory, _timestamp) = event

  let emoji = case event_type {
    Created -> "🆕👻"
    Deleted -> "🗑️💀"
    Modified -> "📝🎃"  // Special Halloween emoji for modifications!
    Unknown -> "❓🎃"
  }

  let action = case event_type {
    Created -> "MANIFESTED"
    Deleted -> "VANISHED"
    Modified -> "TRANSFORMED"  // Spooky modification message
    Unknown -> "MYSTERIOUS"
  }

  let item_type = case is_directory {
    True -> "DIR"
    False -> "FILE"
  }

  let filename = get_filename(path)

  io.println("  " <> emoji <> " " <> action <> " " <> item_type <> ": " <> filename)

  // Special handling for modifications
  case event_type, is_directory {
    Modified, False -> {
      io.println("    🔮 File content or size changed - analyzing...")
      analyze_modified_file(path)
    }
    Created, False -> {
      io.println("    ✨ New file detected - welcome to the haunted directory!")
    }
    Deleted, False -> {
      io.println("    👻 File has been banished to the digital afterlife...")
    }
    _, _ -> Nil
  }
}

fn analyze_modified_file(file_path: String) -> Nil {
  case read_file(file_path) {
    Ok(content) -> {
      let word_count = count_words(content)
      let char_count = string.length(content)

      io.println("    📊 Modified file analysis:")
      io.println("      📝 Words: " <> string.inspect(word_count))
      io.println("      🔤 Characters: " <> string.inspect(char_count))

      case detect_language_simple(file_path) {
        "Rust" -> io.println("      🦀 Rust code modification detected!")
        "Python" -> io.println("      🐍 Python script updated!")
        "Gleam" -> io.println("      ✨ Gleam code enhanced!")
        _ -> io.println("      📄 Text file modified!")
      }
    }
    Error(_) -> io.println("    💀 Cannot analyze modified file")
  }
}

// Safe event conversion
fn get_events_safe(watcher: WatcherRef) -> List(FileEvent) {
  case erlang_get_events(watcher) {
    raw_events -> {
      list.filter_map(raw_events, convert_event)
    }
  }
}

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

// Helper functions
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

fn count_words(content: String) -> Int {
  content
  |> string.split(" ")
  |> list.filter(fn(word) { string.length(string.trim(word)) > 0 })
  |> list.length
}

fn detect_language_simple(file_path: String) -> String {
  case get_extension(file_path) {
    ".rs" -> "Rust"
    ".py" -> "Python"
    ".js" -> "JavaScript"
    ".gleam" -> "Gleam"
    ".erl" -> "Erlang"
    ".txt" -> "Text"
    _ -> "Unknown"
  }
}

fn get_extension(file_path: String) -> String {
  case string.split(file_path, ".") {
    [] -> ""
    [_] -> ""
    parts -> {
      case list.last(parts) {
        Ok(ext) -> "." <> ext
        Error(_) -> ""
      }
    }
  }
}

fn read_file(path: String) -> Result(String, String) {
  case read_file_binary(path) {
    Ok(binary) -> {
      case bit_array.to_string(binary) {
        Ok(content) -> Ok(content)
        Error(_) -> Error("Could not decode file as UTF-8")
      }
    }
    Error(reason) -> Error("File read error: " <> string.inspect(reason))
  }
}

// External calls
@external(erlang, "file_watcher", "start_watching")
fn erlang_start_watching(directory: String) -> WatcherRef

@external(erlang, "file_watcher", "get_events")
fn erlang_get_events(watcher: WatcherRef) -> List(a)

@external(erlang, "file", "read_file")
fn read_file_binary(path: String) -> Result(BitArray, String)

@external(erlang, "timer", "sleep")
fn sleep(milliseconds: Int) -> Nil

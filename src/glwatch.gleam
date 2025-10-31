import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleam/bit_array

// Types that match your Erlang records exactly
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
  io.println("🎃 GLWATCH - Halloween File Monitor 🎃")
  io.println("═══════════════════════════════════════════")
  io.println("🦇 Multi-Language File Analysis System")
  io.println("📡 Erlang: Concurrent file watching")
  io.println("✨ Gleam: Type-safe orchestration")
  io.println("⚡ Rust: Fast content analysis (coming soon)")
  io.println("═══════════════════════════════════════════")
  io.println("")

  start_glwatch()
}

fn start_glwatch() -> Nil {
  io.println("🔮 Starting Erlang file watcher...")

  // External call to your Erlang file_watcher module
  let watcher = erlang_start_watching("watched/")

  io.println("✅ GLWATCH is now active!")
  io.println("")
  io.println("🎯 Enhanced monitoring features:")
  io.println("  📁 Nested directory support")
  io.println("  🆕 File creation detection")
  io.println("  📝 File modification tracking")
  io.println("  🗑️ File deletion monitoring")
  io.println("")
  io.println("🎬 Try these Halloween commands:")
  io.println("  echo 'Spooky content!' > watched/ghost.txt")
  io.println("  mkdir watched/haunted_house")
  io.println("  echo 'fn main() { println!(\"BOO!\"); }' > watched/scary.rs")
  io.println("  echo 'def vampire(): return \"BITE!\"' > watched/vampire.py")
  io.println("")

  // Start the main monitoring loop
  glwatch_loop(watcher)
}

fn glwatch_loop(watcher: WatcherRef) -> Nil {
  // External call to get events from your Erlang watcher
  case erlang_get_events(watcher) {
    [] -> {
      io.println("👁️  GLWATCH monitoring... (drop some spooky files!)")
      sleep(3000)
      glwatch_loop(watcher)
    }
    events -> {
      let event_count = list.length(events)
      io.println("")
      io.println("🔥 GLWATCH DETECTED " <> string.inspect(event_count) <> " EVENTS! 🔥")
      io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")

      // Process each event with Halloween flair
      list.each(events, process_halloween_event)
      io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
      io.println("")

      // Continue the eternal watch
      glwatch_loop(watcher)
    }
  }
}

fn process_halloween_event(event: FileEvent) -> Nil {
  let FileEvent(event_type, path, is_directory, timestamp) = event

  // Spooky event emojis
  let event_emoji = case event_type {
    Created -> "🆕👻"
    Deleted -> "🗑️💀"
    Modified -> "📝🎃"
  }

  let item_type = case is_directory {
    True -> "📁 DIRECTORY"
    False -> "📄 FILE"
  }

  let type_str = case event_type {
    Created -> "MANIFESTED"
    Deleted -> "VANISHED"
    Modified -> "TRANSFORMED"
  }

  io.println(event_emoji <> " " <> type_str <> " " <> item_type)
  io.println("👻 Path: " <> path)
  io.println("⏰ Timestamp: " <> string.inspect(timestamp))

  // Handle different spooky events
  case event_type, is_directory {
    Created, False -> {
      io.println("⚡ Gleam → Analyzing new haunted file...")
      analyze_spooky_file(path)
    }
    Modified, False -> {
      io.println("⚡ Gleam → Re-analyzing transformed file...")
      analyze_spooky_file(path)
    }
    Created, True -> {
      io.println("🏚️ New haunted directory - GLWATCH will monitor its spirits")
    }
    Deleted, False -> {
      io.println("👻 File has vanished into the digital afterlife...")
    }
    Deleted, True -> {
      io.println("🏚️ Directory demolished - no more spirits to watch")
    }
    _, _ -> {
      io.println("📋 Supernatural event logged in the grimoire")
    }
  }
}

fn analyze_spooky_file(file_path: String) -> Nil {
  io.println("🔍 Performing dark magic analysis on: " <> file_path)

  case analyze_file(file_path) {
    Ok(analysis) -> display_spooky_results(analysis)
    Error(err) -> io.println("💀 Dark magic failed: " <> err)
  }
}

fn display_spooky_results(analysis: FileAnalysis) -> Nil {
  io.println("📊 SUPERNATURAL ANALYSIS RESULTS:")
  io.println("  📝 Word spirits: " <> string.inspect(analysis.word_count))
  io.println("  💻 Detected incantation: " <> analysis.language)
  io.println("  🎯 Magic confidence: " <> string.inspect(analysis.confidence) <> "%")
  io.println("  🔮 Mystical features: " <> string.join(analysis.features, ", "))
  io.println("  🏷️ Enchanted keywords: " <> string.join(analysis.keywords, ", "))

  // Halloween language personality
  case analysis.language {
    "Rust" -> io.println("  🦀 Rust spirit: Memory-safe necromancy!")
    "Python" -> io.println("  🐍 Python serpent: Readable dark arts!")
    "JavaScript" -> io.println("  🟨 JavaScript phantom: Haunts every browser!")
    "Gleam" -> io.println("  ✨ Gleam angel: Type-safe holy magic!")
    "Erlang" -> io.println("  📡 Erlang demon: Immortal concurrent power!")
    "Haskell" -> io.println("  🎭 Haskell wizard: Pure functional sorcery!")
    "Java" -> io.println("  ☕ Java golem: Ancient enterprise magic!")
    "Go" -> io.println("  🐹 Go sprite: Simple cloud conjuration!")
    _ -> io.println("  👻 Unknown spirit: " <> analysis.language)
  }
}

// File analysis types
pub type FileAnalysis {
  FileAnalysis(
    word_count: Int,
    language: String,
    confidence: Int,
    features: List(String),
    keywords: List(String)
  )
}

fn analyze_file(file_path: String) -> Result(FileAnalysis, String) {
  case read_file(file_path) {
    Ok(content) -> {
      let word_count = count_words(content)
      let language = detect_language(content, file_path)
      let confidence = calculate_confidence(content, language)
      let features = detect_features(content, language)
      let keywords = extract_keywords(content)

      Ok(FileAnalysis(
        word_count: word_count,
        language: language,
        confidence: confidence,
        features: features,
        keywords: keywords
      ))
    }
    Error(err) -> Error("Failed to read haunted file: " <> err)
  }
}

fn count_words(content: String) -> Int {
  content
  |> string.split(" ")
  |> list.filter(fn(word) { string.length(string.trim(word)) > 0 })
  |> list.length
}

fn detect_language(content: String, file_path: String) -> String {
  let extension = get_extension(file_path)
  let lower_content = string.lowercase(content)

  // Extension-based detection first
  case extension {
    ".rs" -> "Rust"
    ".py" -> "Python"
    ".js" -> "JavaScript"
    ".gleam" -> "Gleam"
    ".erl" -> "Erlang"
    ".ex" -> "Elixir"
    ".hs" -> "Haskell"
    ".java" -> "Java"
    ".go" -> "Go"
    ".c" -> "C"
    ".cpp" -> "C++"
    ".md" -> "Markdown"
    ".json" -> "JSON"
    ".toml" -> "TOML"
    _ -> detect_by_content(lower_content)
  }
}

fn detect_by_content(content: String) -> String {
  // Fixed: Use separate case expressions instead of pattern matching 5 subjects
  let has_fn_main = string.contains(content, "fn main")
  let has_pub_fn = string.contains(content, "pub fn")
  let has_println = string.contains(content, "println!")
  let has_def = string.contains(content, "def ")
  let has_function = string.contains(content, "function ")

  // Check for Gleam first (most specific)
  case has_fn_main && has_pub_fn {
    True -> "Gleam"
    False -> {
      case has_fn_main && has_println {
        True -> "Rust"
        False -> {
          case has_def {
            True -> "Python"
            False -> {
              case has_function {
                True -> "JavaScript"
                False -> "Plain Text"
              }
            }
          }
        }
      }
    }
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

fn calculate_confidence(content: String, _language: String) -> Int {
  let base = 70
  let length_bonus = case string.length(content) {
    len if len > 200 -> 25
    len if len > 100 -> 15
    len if len > 50 -> 10
    _ -> 0
  }
  base + length_bonus
}

fn detect_features(content: String, language: String) -> List(String) {
  let lower = string.lowercase(content)
  case language {
    "Rust" -> detect_rust_features(lower)
    "Python" -> detect_python_features(lower)
    "JavaScript" -> detect_js_features(lower)
    "Gleam" -> detect_gleam_features(lower)
    _ -> ["basic syntax"]
  }
}

fn detect_rust_features(content: String) -> List(String) {
  let features = []

  let features = case string.contains(content, "fn ") {
    True -> ["functions", ..features]
    False -> features
  }

  let features = case string.contains(content, "struct") {
    True -> ["structs", ..features]
    False -> features
  }

  let features = case string.contains(content, "impl") {
    True -> ["implementations", ..features]
    False -> features
  }

  case string.contains(content, "match") {
    True -> ["pattern matching", ..features]
    False -> features
  }
}

fn detect_python_features(content: String) -> List(String) {
  let features = []

  let features = case string.contains(content, "def ") {
    True -> ["functions", ..features]
    False -> features
  }

  case string.contains(content, "class ") {
    True -> ["classes", ..features]
    False -> features
  }
}

fn detect_js_features(content: String) -> List(String) {
  let features = []

  let features = case string.contains(content, "function") {
    True -> ["functions", ..features]
    False -> features
  }

  case string.contains(content, "console.log") {
    True -> ["console output", ..features]
    False -> features
  }
}

fn detect_gleam_features(content: String) -> List(String) {
  let features = []

  let features = case string.contains(content, "pub fn") {
    True -> ["public functions", ..features]
    False -> features
  }

  case string.contains(content, "pub type") {
    True -> ["type definitions", ..features]
    False -> features
  }
}

fn extract_keywords(content: String) -> List(String) {
  content
  |> string.split(" ")
  |> list.filter(fn(word) {
    let clean = string.trim(word)
    string.length(clean) > 3
  })
  |> list.take(5)
  |> list.map(string.lowercase)
}

fn read_file(path: String) -> Result(String, String) {
  case read_file_binary(path) {
    Ok(binary) -> {
      // Fixed: Added proper import and used bit_array module
      case bit_array.to_string(binary) {
        Ok(content) -> Ok(content)
        Error(_) -> Error("Could not decode file as UTF-8")
      }
    }
    Error(reason) -> Error("File read error: " <> string.inspect(reason))
  }
}

// 🎃 External FFI calls to your Erlang file_watcher module
@external(erlang, "file_watcher", "start_watching")
fn erlang_start_watching(directory: String) -> WatcherRef

// External function returns string-based tuples
@external(erlang, "file_watcher", "get_events")
fn erlang_get_events_raw(watcher: WatcherRef) -> List(#(String, String, Bool, Int))

// Convert string events to proper EventType enum
fn erlang_get_events(watcher: WatcherRef) -> List(FileEvent) {
  erlang_get_events_raw(watcher)
  |> list.map(convert_raw_event_to_file_event)
}

fn convert_raw_event_to_file_event(raw_event: #(String, String, Bool, Int)) -> FileEvent {
  let #(type_str, path, is_dir, timestamp) = raw_event

  let event_type = case type_str {
    "created" -> Created
    "deleted" -> Deleted
    "modified" -> Modified
    _ -> {
      io.println("⚠️ Unknown event type: " <> type_str)
      Created
    }
  }

  FileEvent(
    event_type: event_type,
    path: path,
    is_directory: is_dir,
    timestamp: timestamp
  )
}

@external(erlang, "file_watcher", "stop_watching")
fn erlang_stop_watching(watcher: WatcherRef) -> Nil

@external(erlang, "file", "read_file")
fn read_file_binary(path: String) -> Result(BitArray, String)

@external(erlang, "timer", "sleep")
fn sleep(milliseconds: Int) -> Nil

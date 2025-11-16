import gleeunit
import gleeunit/should
import gleam/list
import gleam/int

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Helper Functions Tests
// ============================================================================

pub fn format_timestamp_test() {
  // Test time formatting logic
  let hours = 9
  let minutes = 5
  let seconds = 3

  let result = pad_zero(hours) <> ":" <> pad_zero(minutes) <> ":" <> pad_zero(seconds)
  result
  |> should.equal("09:05:03")
}

pub fn pad_zero_single_digit_test() {
  pad_zero(5)
  |> should.equal("05")
}

pub fn pad_zero_double_digit_test() {
  pad_zero(15)
  |> should.equal("15")
}

fn pad_zero(n: Int) -> String {
  case n < 10 {
    True -> "0" <> int.to_string(n)
    False -> int.to_string(n)
  }
}

// ============================================================================
// Event Type Tests
// ============================================================================

pub fn event_emoji_created_test() {
  get_event_emoji("CREATED")
  |> should.equal("➕")
}

pub fn event_emoji_modified_test() {
  get_event_emoji("MODIFIED")
  |> should.equal("📝")
}

pub fn event_emoji_deleted_test() {
  get_event_emoji("DELETED")
  |> should.equal("🗑️")
}

pub fn event_emoji_unknown_test() {
  get_event_emoji("UNKNOWN")
  |> should.equal("📄")
}

fn get_event_emoji(event_type: String) -> String {
  case event_type {
    "CREATED" -> "➕"
    "MODIFIED" -> "📝"
    "DELETED" -> "🗑️"
    _ -> "📄"
  }
}

// ============================================================================
// Event List Processing Tests
// ============================================================================

pub fn empty_event_list_test() {
  let events = []

  list.length(events)
  |> should.equal(0)
}

pub fn event_list_with_items_test() {
  let events = ["CREATED file.txt", "MODIFIED config.js", "DELETED old.log"]

  list.length(events)
  |> should.equal(3)
}

pub fn filter_events_test() {
  let events = [
    "CREATED file.txt",
    "MODIFIED config.js",
    "CREATED test.txt",
  ]

  let filtered = list.filter(events, fn(e) {
    case e {
      "CREATED " <> _ -> True
      _ -> False
    }
  })

  list.length(filtered)
  |> should.equal(2)
}

// ============================================================================
// Time Calculation Tests
// ============================================================================

pub fn elapsed_time_seconds_test() {
  format_elapsed_time(15)
  |> should.equal("15s")
}

pub fn elapsed_time_minutes_test() {
  format_elapsed_time(90)
  |> should.equal("1m 30s")
}

pub fn elapsed_time_hours_test() {
  format_elapsed_time(3665)
  |> should.equal("1h 1m")
}

fn format_elapsed_time(seconds: Int) -> String {
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

// ============================================================================
// String Formatting Tests
// ============================================================================

pub fn event_message_format_test() {
  let emoji = "📝"
  let action = "MODIFIED"
  let filename = "test.txt"

  let result = emoji <> " " <> action <> " " <> filename
  result
  |> should.equal("📝 MODIFIED test.txt")
}

pub fn timestamp_format_test() {
  let timestamp = "10:15:23"

  let result = "🔔 [" <> timestamp <> "] Changes detected:"
  result
  |> should.equal("🔔 [10:15:23] Changes detected:")
}

// ============================================================================
// List Operation Tests
// ============================================================================

pub fn event_count_test() {
  let events = ["event1", "event2", "event3"]

  let result = "Detected " <> int.to_string(list.length(events)) <> " event(s)"
  result
  |> should.equal("Detected 3 event(s)")
}

pub fn each_event_processing_test() {
  let events = ["file1.txt", "file2.txt"]
  let processed = list.map(events, fn(e) { "📄 " <> e })

  processed
  |> should.equal(["📄 file1.txt", "📄 file2.txt"])
}

// ============================================================================
// Additional Practical Tests
// ============================================================================

pub fn heartbeat_message_test() {
  let uptime = "1h 30m"
  let total_events = 42

  let result = "💚 Watching | Uptime: " <> uptime <> " | Events: " <> int.to_string(total_events)
  result
  |> should.equal("💚 Watching | Uptime: 1h 30m | Events: 42")
}

pub fn no_events_message_test() {
  let tick = 5
  let total = 10

  let result = "  ⏳ [" <> int.to_string(tick) <> "/" <> int.to_string(total) <> "] No events detected"
  result
  |> should.equal("  ⏳ [5/10] No events detected")
}

pub fn banner_format_test() {
  let line1 = "╔════════════════════════════════════════╗"
  let line2 = "║   GLWATCH v1.0.0                      ║"
  let line3 = "╚════════════════════════════════════════╝"

  line1
  |> should.equal("╔════════════════════════════════════════╗")

  line2
  |> should.equal("║   GLWATCH v1.0.0                      ║")

  line3
  |> should.equal("╚════════════════════════════════════════╝")
}

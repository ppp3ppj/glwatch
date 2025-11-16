import gleam/io
import gleam/list
import gleam/string
import gleam/option.{type Option, None, Some}
import simplifile

pub type EventStatus {
  Created
  Modified
  Deleted
  All
}

pub type ActionConfig {
  ActionConfig(
    name: String,
    patterns: List(String),
    status: List(EventStatus),
    command: String,
  )
}

pub type WatchConfig {
  WatchConfig(actions: List(ActionConfig))
}

// Simple TOML parser for our specific format
pub fn parse_config(file_path: String) -> Result(WatchConfig, String) {
  case simplifile.read(file_path) {
    Ok(content) -> parse_config_content(content)
    Error(_) -> Error("Failed to read config file: " <> file_path)
  }
}

fn parse_config_content(content: String) -> Result(WatchConfig, String) {
  let lines = string.split(content, "\n")
  let actions = parse_sections(lines, [], None)
  Ok(WatchConfig(actions: actions))
}

fn parse_sections(
  lines: List(String),
  actions: List(ActionConfig),
  current: Option(#(String, List(String), List(String), String)),
) -> List(ActionConfig) {
  case lines {
    [] -> {
      // Add last action if exists
      case current {
        Some(#(name, patterns, status_strs, command)) -> {
          let status = list.filter_map(status_strs, parse_status)
          let status_final = case status {
            [] -> [All]
            _ -> status
          }
          case patterns, command {
            [], _ | _, "" -> actions
            _, _ -> [
              ActionConfig(
                name: name,
                patterns: patterns,
                status: status_final,
                command: command,
              ),
              ..actions
            ]
          }
        }
        None -> actions
      }
    }
    [line, ..rest] -> {
      let trimmed = string.trim(line)

      case trimmed {
        // Empty line or comment
        "" -> parse_sections(rest, actions, current)
        "#" <> _ -> parse_sections(rest, actions, current)

        // Section header [name]
        "[" <> section_line -> {
          // Save previous action
          let new_actions = case current {
            Some(#(name, patterns, status_strs, command)) -> {
              let status = list.filter_map(status_strs, parse_status)
              let status_final = case status {
                [] -> [All]
                _ -> status
              }
              case patterns, command {
                [], _ | _, "" -> actions
                _, _ -> [
                  ActionConfig(
                    name: name,
                    patterns: patterns,
                    status: status_final,
                    command: command,
                  ),
                  ..actions
                ]
              }
            }
            None -> actions
          }

          // Extract section name
          let section_name =
            section_line
            |> string.replace("]", "")
            |> string.trim()

          parse_sections(rest, new_actions, Some(#(section_name, [], [], "")))
        }

        // Key-value pair
        _ -> {
          case string.split(trimmed, "=") {
            [key, value] -> {
              let key_trimmed = string.trim(key)
              let value_trimmed = string.trim(value)

              case current {
                Some(#(name, patterns, status_strs, command)) -> {
                  case key_trimmed {
                    "patterns" -> {
                      let new_patterns = parse_array(value_trimmed)
                      parse_sections(
                        rest,
                        actions,
                        Some(#(name, new_patterns, status_strs, command)),
                      )
                    }
                    "status" -> {
                      let new_status = parse_array(value_trimmed)
                      parse_sections(
                        rest,
                        actions,
                        Some(#(name, patterns, new_status, command)),
                      )
                    }
                    "command" -> {
                      let new_command = parse_string(value_trimmed)
                      parse_sections(
                        rest,
                        actions,
                        Some(#(name, patterns, status_strs, new_command)),
                      )
                    }
                    _ -> parse_sections(rest, actions, current)
                  }
                }
                None -> parse_sections(rest, actions, None)
              }
            }
            _ -> parse_sections(rest, actions, current)
          }
        }
      }
    }
  }
}

fn parse_array(value: String) -> List(String) {
  value
  |> string.replace("[", "")
  |> string.replace("]", "")
  |> string.split(",")
  |> list.map(fn(item) {
    item
    |> string.trim()
    |> string.replace("\"", "")
  })
  |> list.filter(fn(item) { item != "" })
}

fn parse_string(value: String) -> String {
  value
  |> string.trim()
  |> string.replace("\"", "")
}

fn parse_status(status_str: String) -> Result(EventStatus, Nil) {
  case status_str {
    "*" -> Ok(All)
    "CREATED" -> Ok(Created)
    "MODIFIED" -> Ok(Modified)
    "DELETED" -> Ok(Deleted)
    _ -> Error(Nil)
  }
}

pub fn matches_status(event_type: String, status_list: List(EventStatus)) -> Bool {
  list.any(status_list, fn(status) {
    case status {
      All -> True
      Created -> event_type == "CREATED"
      Modified -> event_type == "MODIFIED"
      Deleted -> event_type == "DELETED"
    }
  })
}

pub fn print_config(config: WatchConfig) {
  io.println("📋 Loaded Configuration:")
  io.println("")

  list.each(config.actions, fn(action) {
    io.println("🔧 [" <> action.name <> "]")
    io.println("   Patterns: " <> list_to_string(action.patterns))
    io.println("   Status: " <> status_list_to_string(action.status))
    io.println("   Command: " <> action.command)
    io.println("")
  })
}

fn list_to_string(items: List(String)) -> String {
  case items {
    [] -> ""
    [first] -> first
    [first, ..rest] -> first <> ", " <> list_to_string(rest)
  }
}

fn status_list_to_string(status_list: List(EventStatus)) -> String {
  status_list
  |> list.map(fn(s) {
    case s {
      All -> "*"
      Created -> "CREATED"
      Modified -> "MODIFIED"
      Deleted -> "DELETED"
    }
  })
  |> list_to_string()
}

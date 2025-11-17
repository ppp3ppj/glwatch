# GLWATCH

> A smart, real-time file system monitor built with Gleam, Rust, and Erlang

[![Gleam](https://img.shields.io/badge/Gleam-FFAFF3?style=flat&logo=gleam&logoColor=black)](https://gleam.run) [![Rust](https://img.shields.io/badge/Rust-000000?style=flat&logo=rust&logoColor=white)](https://www.rust-lang.org) [![Erlang](https://img.shields.io/badge/Erlang-A90533?style=flat&logo=erlang&logoColor=white)](https://www.erlang.org)

**GLWATCH** monitors your files and folders for changes in real-time, with intelligent filtering and clean output. Perfect for development workflows, build automation, and system monitoring.

---

## Features

- **Real-time monitoring** - Instant file change detection
- **Smart filtering** - Ignores temp files, backups, and editor artifacts
- **Intelligent edit detection** - Recognizes editor save patterns (DELETE + CREATE = MODIFIED)
- **Clean output** - Event deduplication with emoji indicators
- **High performance** - Built with Rust, <1% CPU usage
- **Cross-platform** - Works on Linux, macOS, and Windows

---

##  Quick Start

### Prerequisites

- [Gleam](https://gleam.run) (v1.0.0 or higher)
- [Rust](https://www.rust-lang.org) (v1.70.0 or higher)
- [Erlang](https://www.erlang.org) (v25.0 or higher)

### Installation

```bash
# Clone the repository
git clone https://github.com/ppp3ppj/glwatch
cd glwatch

# Setup (builds everything)
make setup

# Run
make run
```

---

## Usage

### Basic Usage

```bash
# Start watching the ./watched directory
make run
```

### Example Output

```
╔════════════════════════════════════════╗
║   GLWATCH v1.2.0                      ║
║   Smart File System Monitor           ║
║   with Multi-Directory Support        ║
╚════════════════════════════════════════╝

🔍 Starting file watcher for multiple directories...
📂 Watching:
   • ./watched
   • ./src
   • ./test

🎯 Patterns:
   • **/*.gleam
   • **/*.js
   • **/*.txt
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

💚 Watching | Uptime: 0s | Events: 0

🔔 [10:15:23] Changes detected:
   ➕ CREATED test.txt

🔔 [10:15:45] Changes detected:
   📝 MODIFIED test.txt

🔔 [10:16:02] Changes detected:
   🗑️ DELETED test.txt
```

### Testing

In another terminal, try these commands:

```bash
# Create a file
echo "Hello World" > watched/test.txt

# Edit with vim (you'll see MODIFIED, not 14 events!)
vim watched/test.txt

# Create multiple files
for i in {1..5}; do echo "File $i" > watched/file$i.txt; done

# Delete files
rm watched/file*.txt
```

---

## Architecture

GLWATCH uses a three-layer architecture combining the strengths of Gleam, Erlang, and Rust:

```
┌──────────────────────────────────┐
│      Gleam (Application)         │  ← Type-safe business logic
│  • Event loop & formatting       │
│  • Display & user interaction    │
└────────────┬─────────────────────┘
             │ @external FFI calls
┌────────────▼─────────────────────┐
│      Erlang (Bridge)             │  ← NIF loading & glue
│  • Loads Rust library            │
│  • Data type conversion          │
└────────────┬─────────────────────┘
             │ NIF interface
┌────────────▼─────────────────────┐
│      Rust (Native Engine)        │  ← High-performance core
│  • File system watching          │
│  • Event filtering               │
│  • Smart edit detection          │
│  • Uses 'notify' crate           │
└──────────────────────────────────┘
```

### Why Three Languages?

| Language | Role | Strength |
|----------|------|----------|
| **Gleam** | Application logic | Type safety, readability, expressiveness |
| **Erlang** | Bridge/Runtime | BEAM VM, concurrency, fault tolerance |
| **Rust** | Native engine | Performance, memory safety, systems programming |

---

##  How It Works

### The Problem: Editor Noise

When you edit a file with Vim or other editors, they generate **many events**:

```
❌ Without GLWATCH (14 events):
CREATE temp_4913
MODIFY temp_4913
ACCESS temp_4913
REMOVE temp_4913
CREATE backup~
MODIFY file.txt
DELETE file.txt
CREATE file.txt
... 6 more events
```

### The Solution: Smart Detection

```
✅ With GLWATCH (1 event):
🔔 [10:15:23] Changes detected:
   📝 MODIFIED file.txt
```

### Event Processing Pipeline

```
Raw Events → Filter → Smart Detection → Deduplication → Clean Output
```

1. **Filter**: Remove temp files (`.swp`, `~`, `4913`, etc.)
2. **Smart Detection**: DELETE + CREATE within 2s = MODIFIED
3. **Deduplication**: Combine multiple events for same file
4. **Format**: Add emoji indicators and timestamps

---

## Project Structure

```
glwatch/
├── src/
│   ├── glwatch.gleam          # Main application (Gleam)
│   └── file_watcher.erl       # Erlang bridge module
│
├── rust_watcher/
│   ├── src/
│   │   └── lib.rs             # Rust NIF implementation
│   ├── Cargo.toml             # Rust dependencies
│   └── target/                # Rust build output
│
├── priv/
│   └── rust_watcher.so        # Compiled NIF library
│
├── watched/                    # Directory being monitored
│
├── Makefile                    # Build automation
├── gleam.toml                 # Gleam project configuration
└── README.md                  # This file
```

---

## Development

### Available Make Commands

```bash
make help          # Show all available commands
make setup         # Initial setup (install + build)
make build         # Build everything
make run           # Run the application
make clean         # Clean all build artifacts
make rtest         # Run rust tests
make gtest         # Run gleam tests
make format        # Format code
make check         # Run linters
```

### Building Components Separately

```bash
# Build only Rust NIF
make build-rust

# Build only Gleam
make build-gleam

# Copy NIF to priv/
make copy-nif
```

### Development Workflow

```bash
# 1. Make changes to code
vim src/glwatch.gleam

# 2. Rebuild
make build

# 3. Run
make run

# 4. Test your changes
echo "test" > watched/test.txt
```

---

## Key Features Explained

### 1. Smart Edit Detection

**Problem**: Editors use atomic saves (delete + create) which generates confusing events.

**Solution**: Track recently deleted files. If a file is created within 2 seconds of being deleted, report it as MODIFIED instead.

```rust
// Simplified logic
if file_deleted_recently && file_created {
    return EventType::Modified;
}
```

### 2. Noise Filtering

Automatically ignores:

- Temporary files: `4913`, `.tmp`
- Backup files: `file.txt~`, `#file#`
- Swap files: `.swp`, `.swx`
- Hidden files: `.DS_Store`
- Access events: Too noisy, not useful

### 3. Event Deduplication

Multiple events for the same file within 1 second are combined:

```
Input:  CREATE, MODIFY, MODIFY → Output: CREATED
Input:  MODIFY, MODIFY, MODIFY → Output: MODIFIED
Input:  DELETE, CREATE         → Output: MODIFIED (smart detection)
```

### 4. Performance Optimization

- Uses Rust's `notify` crate for efficient OS-level file watching
- Event buffering reduces redundant processing
- Smart polling interval (1 second) balances responsiveness and CPU usage
- Lazy cleanup of deleted file tracking (every 5 seconds)

---

## Use Cases

### 1. Development Workflow

```bash
# Watch your project while coding
cd my-project
glwatch

# In another terminal, edit files
vim src/main.gleam
# GLWATCH shows: MODIFIED src/main.gleam
```

### 2. Build Automation

```bash
# Monitor source files and trigger builds
# (Future feature - coming soon!)
glwatch --exec "npm run build" --pattern "src/**/*.js"
```

### 3. System Monitoring

```bash
# Watch configuration files
glwatch /etc/nginx/
# Detect unauthorized changes
```

### 4. Data Processing

```bash
# Monitor upload directory
glwatch /uploads/
# Process new files automatically
```

---

## Technical Details

### How Gleam Calls Rust

```gleam
// Gleam declares external function
@external(erlang, "file_watcher", "start_watching")
fn start_watching(path: String) -> WatcherRef
```

```erlang
% Erlang loads the Rust NIF
-module(file_watcher).
-on_load(init/0).

init() ->
    erlang:load_nif("priv/rust_watcher", 0).
```

```rust
// Rust implements the function
#[rustler::nif]
fn start_watching(path: String) -> NifResult<ResourceArc<FileWatcherResource>> {
    // Implementation
}
```

### Data Flow

```
User Action → File System → OS Notification → notify crate →
Rust NIF → Erlang Bridge → Gleam Application → Terminal Output
```

### Resource Management

- Rust NIF uses `ResourceArc` for safe cross-language resource sharing
- Erlang BEAM VM handles memory management
- Automatic cleanup when watcher is stopped

---

##  Troubleshooting

### NIF Not Loading

```bash
# Error: nif_not_loaded
# Solution: Rebuild and copy NIF

cd rust_watcher
cargo clean
cargo build --release
cd ..
make copy-nif
```

### Permission Denied

```bash
# Error: Failed to watch path: Permission denied
# Solution: Check directory permissions

ls -la watched/
chmod 755 watched/
```

### Too Many Open Files (Linux)

```bash
# Error: Too many open files
# Solution: Increase limit

ulimit -n 4096

# Or permanently in /etc/security/limits.conf
* soft nofile 4096
* hard nofile 8192
```

### Events Not Showing

```bash
# Check if directory exists
ls -la watched/

# Check if watcher started successfully
# Look for "✅ Watcher started successfully" message

# Try creating a test file
echo "test" > watched/test.txt
```

---

## Resources

### Documentation

- [Gleam Language](https://gleam.run)
- [Rustler (Rust NIFs for Erlang)](https://github.com/rusterlium/rustler)
- [notify crate (File watching)](https://docs.rs/notify)
- [Erlang NIFs](https://www.erlang.org/doc/tutorial/nif.html)

### Similar Projects

- [watchman](https://facebook.github.io/watchman/) - Facebook's file watching service (C++)
- [chokidar](https://github.com/paulmillr/chokidar) - Node.js file watcher
- [watchexec](https://github.com/watchexec/watchexec) - Rust file watcher with command execution
- [inotifywait](https://linux.die.net/man/1/inotifywait) - Linux command-line tool

### Learning Resources

- [Building NIFs with Rustler](https://github.com/rusterlium/rustler/tree/master/rustler_tests)
- [Gleam Foreign Function Interface](https://gleam.run/book/tour/external-functions.html)
- [File System Events](https://en.wikipedia.org/wiki/Inotify)

---

## Todos

- [x] Pattern matching (glob patterns: `*.js`, `src/**/*.gleam`)
- [ ] Ignore file support (`.gitignore` integration)
- [x] Multiple directory watching
- [ ] Configuration file support
- [ ] Custom action triggers (run commands on events)
- [ ] Webhook support
- [ ] Event history and statistics
- [ ] Web dashboard UI
- [ ] Distributed monitoring (multiple machines)
- [ ] Plugin system
- [ ] REST API
- [ ] Database logging

---

## Changelog

### v1.0.0 (2025-11-15)

- Real-time file monitoring
- Smart edit detection
- Event filtering and deduplication
- Cross-platform support (Linux, macOS, Windows)
- Makefile build automation
- Comprehensive documentation

### v1.2.0 (2025-11-15)

- Pattern matching (glob patterns: `*.js`, `src/**/*.gleam`)
- Multi-Directory wathing support
- Better Performance

---

**Made with ❤️ using Gleam, Rust, and Erlang**

*Last updated: 2025-11-16*

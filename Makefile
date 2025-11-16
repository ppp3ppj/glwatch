# Makefile for GLWATCH - File System Monitor
# Author: @ppp3ppj
# Date: 2025-11-15

.PHONY: help build clean run rtest gtest install dev watch release all

# Detect OS
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	LIB_EXT := so
	LIB_PREFIX := lib
endif
ifeq ($(UNAME_S),Darwin)
	LIB_EXT := dylib
	LIB_PREFIX := lib
endif
ifeq ($(OS),Windows_NT)
	LIB_EXT := dll
	LIB_PREFIX :=
endif

# Paths
RUST_DIR := rust_watcher
RUST_TARGET := $(RUST_DIR)/target/release/$(LIB_PREFIX)rust_watcher.$(LIB_EXT)
PRIV_DIR := priv
NIF_TARGET := $(PRIV_DIR)/rust_watcher.so
WATCHED_DIR := watched

# Colors for output
GREEN := \033[0;32m
YELLOW := \033[0;33m
RED := \033[0;31m
NC := \033[0m # No Color

# Default target
all: build

## help: Show this help message
help:
	@echo "$(GREEN)GLWATCH Makefile Commands$(NC)"
	@echo "=========================="
	@echo ""
	@echo "$(YELLOW)Building:$(NC)"
	@echo "  make build          - Build Rust NIF and Gleam project"
	@echo "  make build-rust     - Build only Rust NIF"
	@echo "  make build-gleam    - Build only Gleam project"
	@echo "  make release        - Build optimized release version"
	@echo ""
	@echo "$(YELLOW)Running:$(NC)"
	@echo "  make run            - Run the application"
	@echo "  make dev            - Run in development mode with auto-reload"
	@echo "  make watch          - Watch for changes and rebuild"
	@echo ""
	@echo "$(YELLOW)Testing:$(NC)"
	@echo "  make rtest           - Run Rust tests"
	@echo "  make test-create    - Create test files"
	@echo "  make gtest           - Run Gleam tests"
	@echo ""
	@echo "$(YELLOW)Cleaning:$(NC)"
	@echo "  make clean          - Clean all build artifacts"
	@echo "  make clean-rust     - Clean Rust build artifacts"
	@echo "  make clean-gleam    - Clean Gleam build artifacts"
	@echo ""
	@echo "$(YELLOW)Setup:$(NC)"
	@echo "  make install        - Install dependencies"
	@echo "  make setup          - Initial setup (install + build)"
	@echo ""

## build: Build both Rust NIF and Gleam project
build: build-rust copy-nif build-gleam
	@echo "$(GREEN)✓ Build complete!$(NC)"

## build-rust: Build the Rust NIF
build-rust:
	@echo "$(YELLOW)Building Rust NIF...$(NC)"
	cd $(RUST_DIR) && cargo build --release
	@echo "$(GREEN)✓ Rust NIF built$(NC)"

## build-gleam: Build the Gleam project
build-gleam:
	@echo "$(YELLOW)Building Gleam project...$(NC)"
	gleam build
	@echo "$(GREEN)✓ Gleam project built$(NC)"

## copy-nif: Copy the compiled NIF to priv directory
copy-nif: $(PRIV_DIR)
	@echo "$(YELLOW)Copying NIF to priv directory...$(NC)"
	cp $(RUST_TARGET) $(NIF_TARGET)
	@echo "$(GREEN)✓ NIF copied to $(NIF_TARGET)$(NC)"

## setup: Initial setup - create directories and build everything
setup: $(PRIV_DIR) $(WATCHED_DIR) install build
	@echo "$(GREEN)✓ Setup complete!$(NC)"

## install: Install dependencies
install:
	@echo "$(YELLOW)Installing Rust dependencies...$(NC)"
	cd $(RUST_DIR) && cargo fetch
	@echo "$(YELLOW)Installing Gleam dependencies...$(NC)"
	gleam deps download
	@echo "$(GREEN)✓ Dependencies installed$(NC)"

## run: Run the application
run: build
	@echo "$(GREEN)Starting GLWATCH...$(NC)"
	@echo ""
	gleam run

## dev: Run in development mode
dev: build
	@echo "$(GREEN)Starting GLWATCH in dev mode...$(NC)"
	gleam run

## watch: Watch for Rust changes and rebuild (requires cargo-watch)
watch:
	@echo "$(YELLOW)Watching for changes...$(NC)"
	@echo "$(YELLOW)Press Ctrl+C to stop$(NC)"
	@echo ""
	cd $(RUST_DIR) && cargo watch -x 'build --release' -s 'cd .. && make copy-nif'

## release: Build optimized release version
release: clean
	@echo "$(YELLOW)Building release version...$(NC)"
	cd $(RUST_DIR) && cargo build --release --locked
	$(MAKE) copy-nif
	gleam build
	@echo "$(GREEN)✓ Release build complete!$(NC)"

## test: Run all tests
gtest:
	@echo "$(YELLOW)Running Gleam tests...$(NC)"
	gleam test
	@echo "$(GREEN)✓ Tests passed$(NC)"

## test: Run tests
rtest:
	@echo "$(YELLOW)Running Gleam tests...$(NC)"
	gleam test
	@echo "$(YELLOW)Running Rust tests...$(NC)"
	cd $(RUST_DIR) && cargo test

## test-create: Create test files in watched directory
test-create: $(WATCHED_DIR)
	@echo "$(YELLOW)Creating test files...$(NC)"
	echo "Test file 1" > $(WATCHED_DIR)/test1.txt
	echo "Test file 2" > $(WATCHED_DIR)/test2.txt
	mkdir -p $(WATCHED_DIR)/subdir
	echo "Nested file" > $(WATCHED_DIR)/subdir/nested.txt
	@echo "$(GREEN)✓ Test files created in $(WATCHED_DIR)$(NC)"

## clean: Clean all build artifacts
clean: clean-rust clean-gleam clean-nif
	@echo "$(GREEN)✓ All build artifacts cleaned$(NC)"

## clean-rust: Clean Rust build artifacts
clean-rust:
	@echo "$(YELLOW)Cleaning Rust artifacts...$(NC)"
	cd $(RUST_DIR) && cargo clean
	@echo "$(GREEN)✓ Rust artifacts cleaned$(NC)"

## clean-gleam: Clean Gleam build artifacts
clean-gleam:
	@echo "$(YELLOW)Cleaning Gleam artifacts...$(NC)"
	rm -rf build
	@echo "$(GREEN)✓ Gleam artifacts cleaned$(NC)"

## clean-nif: Remove compiled NIF
clean-nif:
	@echo "$(YELLOW)Removing compiled NIF...$(NC)"
	rm -f $(NIF_TARGET)
	@echo "$(GREEN)✓ NIF removed$(NC)"

## format: Format code
format:
	@echo "$(YELLOW)Formatting Rust code...$(NC)"
	cd $(RUST_DIR) && cargo fmt
	@echo "$(YELLOW)Formatting Gleam code...$(NC)"
	gleam format
	@echo "$(GREEN)✓ Code formatted$(NC)"

## check: Run linters and checks
check:
	@echo "$(YELLOW)Checking Rust code...$(NC)"
	cd $(RUST_DIR) && cargo clippy -- -D warnings
	@echo "$(YELLOW)Checking Gleam code...$(NC)"
	gleam check
	@echo "$(GREEN)✓ All checks passed$(NC)"

## info: Show project information
info:
	@echo "$(GREEN)GLWATCH Project Information$(NC)"
	@echo "==========================="
	@echo "Rust version:  $$(rustc --version)"
	@echo "Cargo version: $$(cargo --version)"
	@echo "Gleam version: $$(gleam --version)"
	@echo "OS:            $(UNAME_S)"
	@echo "NIF extension: .$(LIB_EXT)"
	@echo ""
	@echo "Paths:"
	@echo "  Rust target:  $(RUST_TARGET)"
	@echo "  NIF location: $(NIF_TARGET)"
	@echo "  Watch dir:    $(WATCHED_DIR)"

# Create directories if they don't exist
$(PRIV_DIR):
	@echo "$(YELLOW)Creating priv directory...$(NC)"
	mkdir -p $(PRIV_DIR)

$(WATCHED_DIR):
	@echo "$(YELLOW)Creating watched directory...$(NC)"
	mkdir -p $(WATCHED_DIR)

# Rebuild if Rust source changes
$(RUST_TARGET): $(RUST_DIR)/src/*.rs $(RUST_DIR)/Cargo.toml
	$(MAKE) build-rust

# Makefile for Join Patterns Benchmarks
# Run benchmarks using SBT for various join pattern matching algorithms

.PHONY: help clean compile test benchmarks build console rebuild info
.PHONY: bench-size bench-size-noise bench-size-guards bench-size-guards-noise bench-size-guards-non-matching
.PHONY: bench-bounded-buffer bench-simple-smart-house bench-complex-smart-house bench-all
.PHONY: bench-quick-size bench-quick-size-noise bench-quick-size-guards bench-quick-size-guards-noise bench-quick-size-guards-non-matching
.PHONY: bench-quick-bounded-buffer bench-quick-simple-smart-house bench-quick-complex-smart-house bench-quick-all
.PHONY: plot-install plot-all plot-latest plot-custom

# Configuration Variables
SBT := sbt
SELECTED_MATCHERS := "brute,stateful,while-lazy,filtering-while,lazy-parallel,filtering-parallel"
DATA_DIR := benchmarks/benchmark-results/data

# Benchmark Parameters
QUICK_WARMUP ?= 1
QUICK_REPS ?= 2

# Base arguments for benchmark types
COMMON_ARGS := --matchers $(SELECTED_MATCHERS) -p $(DATA_DIR) --suppress-plot --smoothen

# Main benchmark argument sets (include common args)
SIZE_ARGS := $(COMMON_ARGS) -m 5
BOUNDED_BUFFER_ARGS := $(COMMON_ARGS) --bufferBound 1000 --count 100
SMART_HOUSE_ARGS := $(COMMON_ARGS) --param-step 4 -m 250 --max-param 32

# Quick benchmark additional arguments (to be combined with COMMON_ARGS)
QUICK_SIZE_EXTRA := -m 2
QUICK_BOUNDED_BUFFER_EXTRA := --bufferBound 5 --count 5
QUICK_SMART_HOUSE_EXTRA := --param-step 2 -m 10 --max-param 8

# Default target
help:
	@echo "Join Patterns Benchmarks - Available Targets:"
	@echo ""
	@echo "Build Targets:"
	@echo "  build, compile           - Compile the project"
	@echo "  clean                    - Clean build artifacts"
	@echo "  test                     - Run tests"
	@echo "  rebuild                  - Clean and recompile"
	@echo ""
	@echo "Main Benchmark Targets:"
	@echo "  bench-size               - Size benchmarks (normal)"
	@echo "  bench-size-noise         - Size benchmarks (with noise)"
	@echo "  bench-size-guards        - Size benchmarks with guards (normal)"
	@echo "  bench-size-guards-noise  - Size benchmarks with guards (noisy)"
	@echo "  bench-size-guards-non-matching - Size benchmarks with guards (non-matching)"
	@echo "  bench-bounded-buffer     - Bounded buffer benchmarks"
	@echo "  bench-simple-smart-house - Simple smart house benchmarks"
	@echo "  bench-complex-smart-house - Complex smart house benchmarks"
	@echo "  bench-all                - Run all main benchmarks"
	@echo ""
	@echo "Quick Benchmark Targets (reduced repetitions):"
	@echo "  bench-quick-*            - Quick variants of all above benchmarks"
	@echo "  bench-quick-all          - Run all quick benchmarks"
	@echo ""
	@echo "Plotting Targets:"
	@echo "  plot-install             - Install Python dependencies"
	@echo "  plot-all                 - Generate plots for all CSV files"
	@echo "  plot-latest              - Generate plots for latest CSV files"
	@echo "  plot-custom              - Custom plotting"
	@echo ""
	@echo "Configuration Variables:"
	@echo "  SELECTED_MATCHERS='...' - Matcher selection"
	@echo ""

# Build and Test Targets
build: compile

compile:
	$(SBT) compile

clean:
	$(SBT) clean

test:
	$(SBT) test

console:
	$(SBT) console

rebuild: clean compile

info:
	$(SBT) projects

# Benchmark Execution Functions
define run_benchmark
	@echo "Running $(1) benchmarks..."
	$(SBT) "benchmarks/run $(2)"
endef

# Benchmark Argument Builders
define size_benchmark_cmd
$(if $(filter normal,$(1)),size $(SIZE_ARGS),$(if $(filter noisy,$(1)),size $(SIZE_ARGS) -n,size $(SIZE_ARGS)))
endef

define size_guards_benchmark_cmd
$(if $(filter normal,$(1)),size-with-guards $(SIZE_ARGS) -v normal,$(if $(filter noisy,$(1)),size-with-guards $(SIZE_ARGS) -v noisy,$(if $(filter non-matching,$(1)),size-with-guards $(SIZE_ARGS) -v non-satisfying,size-with-guards $(SIZE_ARGS) -v normal)))
endef

# Main Benchmark Targets
bench-size:
	$(call run_benchmark,size,$(call size_benchmark_cmd,normal))

bench-size-noise:
	$(call run_benchmark,size with noise,$(call size_benchmark_cmd,noisy))

bench-size-guards:
	$(call run_benchmark,size with guards,$(call size_guards_benchmark_cmd,normal))

bench-size-guards-noise:
	$(call run_benchmark,size with guards and noise,$(call size_guards_benchmark_cmd,noisy))

bench-size-guards-non-matching:
	$(call run_benchmark,size with guards and non-matching payloads,$(call size_guards_benchmark_cmd,non-matching))

bench-bounded-buffer:
	$(call run_benchmark,bounded buffer,bounded-buffer $(BOUNDED_BUFFER_ARGS))

bench-simple-smart-house:
	$(call run_benchmark,simple smart house,simple-smart-house $(SMART_HOUSE_ARGS))

bench-complex-smart-house:
	$(call run_benchmark,complex smart house,complex-smart-house $(SMART_HOUSE_ARGS))

# Quick Benchmark Variants (reduced matches/parameters)
bench-quick-size:
	$(call run_benchmark,quick size,size $(COMMON_ARGS) $(QUICK_SIZE_EXTRA))

bench-quick-size-noise:
	$(call run_benchmark,quick size with noise,size $(COMMON_ARGS) $(QUICK_SIZE_EXTRA) -n)

bench-quick-size-guards:
	$(call run_benchmark,quick size with guards,size-with-guards $(COMMON_ARGS) $(QUICK_SIZE_EXTRA) -v normal)

bench-quick-size-guards-noise:
	$(call run_benchmark,quick size with guards and noise,size-with-guards $(COMMON_ARGS) $(QUICK_SIZE_EXTRA) -v noisy)

bench-quick-size-guards-non-matching:
	$(call run_benchmark,quick size with guards and non-matching payloads,size-with-guards $(COMMON_ARGS) $(QUICK_SIZE_EXTRA) -v non-satisfying)

bench-quick-bounded-buffer:
	$(call run_benchmark,quick bounded buffer,bounded-buffer $(COMMON_ARGS) $(QUICK_BOUNDED_BUFFER_EXTRA))

bench-quick-simple-smart-house:
	$(call run_benchmark,quick simple smart house,simple-smart-house $(COMMON_ARGS) $(QUICK_SMART_HOUSE_EXTRA))

bench-quick-complex-smart-house:
	$(call run_benchmark,quick complex smart house,complex-smart-house $(COMMON_ARGS) $(QUICK_SMART_HOUSE_EXTRA))

# Benchmark Suites
MAIN_BENCHMARKS := bench-size bench-size-noise bench-size-guards bench-size-guards-noise bench-size-guards-non-matching bench-bounded-buffer bench-simple-smart-house bench-complex-smart-house
QUICK_BENCHMARKS := bench-quick-size bench-quick-size-noise bench-quick-size-guards bench-quick-size-guards-noise bench-quick-size-guards-non-matching bench-quick-bounded-buffer bench-quick-simple-smart-house bench-quick-complex-smart-house

bench-all: $(MAIN_BENCHMARKS)
	@echo "✓ All main benchmarks completed!"

bench-quick-all: $(QUICK_BENCHMARKS)
	@echo "✓ Quick benchmark suite completed!"

# Plotting Targets
PYTHON := python3
PLOT_SCRIPT := benchmarks/benchmark-results/plot_benchmarks.py
PLOT_DATA_DIR := benchmarks/benchmark-results/data
PLOT_OUTPUT_DIR := benchmarks/benchmark-results/plots

plot-install:
	@echo "📦 Installing Python dependencies for plotting..."
	pip3 install -r requirements.txt
	@echo "✓ Python dependencies installed!"

plot-all:
	@echo "📊 Generating plots for all CSV files..."
	$(PYTHON) $(PLOT_SCRIPT) --data-dir $(PLOT_DATA_DIR) --output-dir $(PLOT_OUTPUT_DIR)
	@echo "✓ All plots generated in $(PLOT_OUTPUT_DIR)/"

plot-latest:
	@echo "📊 Generating plots for the most recent CSV files..."
	@ls -t $(PLOT_DATA_DIR)/*.csv 2>/dev/null | head -5 | while IFS= read -r file; do \
		echo "Processing: $$file"; \
		$(PYTHON) $(PLOT_SCRIPT) --files "$$file" --output-dir $(PLOT_OUTPUT_DIR); \
	done
	@echo "✓ Latest plots generated in $(PLOT_OUTPUT_DIR)/"

plot-custom:
	@echo "📊 Generating custom plots..."
	@if [ -z "$(FILES)" ]; then \
		echo "❌ Error: FILES variable not set."; \
		echo "Usage: make plot-custom FILES='file1.csv file2.csv' [LABELS='Label1 Label2']"; \
		exit 1; \
	fi
	$(PYTHON) $(PLOT_SCRIPT) --files $(FILES) $(if $(LABELS),--labels $(LABELS)) --output-dir $(PLOT_OUTPUT_DIR)
	@echo "✓ Custom plots generated in $(PLOT_OUTPUT_DIR)/"
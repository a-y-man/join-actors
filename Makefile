# Makefile for Join Patterns Benchmarks
# Run benchmarks using SBT for various join pattern matching algorithms

.PHONY: help clean compile test benchmarks build console rebuild info
.PHONY: bench-size bench-size-noise bench-size-guards bench-size-guards-noise bench-size-guards-non-matching
.PHONY: bench-bounded-buffer bench-simple-smart-house bench-complex-smart-house bench-all
.PHONY: bench-quick-size bench-quick-size-noise bench-quick-size-guards bench-quick-size-guards-noise bench-quick-size-guards-non-matching
.PHONY: bench-quick-bounded-buffer bench-quick-simple-smart-house bench-quick-complex-smart-house bench-quick-all
.PHONY: bench-and-plot-all bench-quick-and-plot-all bench-smart-house-and-plot bench-quick-smart-house-and-plot
.PHONY: plot-install plot-all plot-latest plot-custom plot-execution-only plot-throughput-only

# Configuration Variables
SBT := sbt
SELECTED_MATCHERS := "brute,stateful,while-lazy,lazy-parallel,filtering-parallel"
DATA_DIR := benchmarks/benchmark-results/data

# Quick Benchmark Parameters
QUICK_WARMUP ?= 2
QUICK_REPS ?= 3

# Main Benchmark Parameters
WARMUP ?= 2
REPS ?= 5

# Base arguments for benchmark types
COMMON_ARGS := --warmup $(WARMUP) --repetitions $(REPS) --matchers $(SELECTED_MATCHERS) -p $(DATA_DIR) --suppress-plot --smoothen

# Warmup and repetitions for quick benchmarks
COMMON_ARGS_QUICK := --warmup $(QUICK_WARMUP) --repetitions $(QUICK_REPS) --matchers $(SELECTED_MATCHERS) -p $(DATA_DIR) --suppress-plot --smoothen

NOISE_MESSAGES_IN_SIZE := 50
NON_MATCHING_MESSAGES_IN_SIZE := 10

# Main benchmark argument sets (include common args)
SIZE_ARGS := $(COMMON_ARGS) --min-param 1 --max-param 5 -m 10
BOUNDED_BUFFER_ARGS := $(COMMON_ARGS) --bufferBound 1000 --count 10
SMART_HOUSE_ARGS := $(COMMON_ARGS) --param-step 4 -m 10 --max-param 24

# Quick benchmark additional arguments (to be combined with COMMON_ARGS)
QUICK_SIZE_EXTRA := --min-param 1 --max-param 5 -m 2 
QUICK_BOUNDED_BUFFER_EXTRA := --bufferBound 5 --count 5
QUICK_SMART_HOUSE_EXTRA := --param-step 1 -m 5 --max-param 5

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
	@echo "  bench-size              		- Size benchmarks (normal)"
	@echo "  bench-size-noise        		- Size benchmarks (with noise)"
	@echo "  bench-size-guards       		- Size benchmarks with guards (normal)"
	@echo "  bench-size-guards-noise 		- Size benchmarks with guards (noisy)"
	@echo "  bench-size-guards-non-matching - Size benchmarks with guards (non-matching)"
	@echo "  bench-bounded-buffer           - Bounded buffer benchmarks"
	@echo "  bench-simple-smart-house       - Simple smart house benchmarks"
	@echo "  bench-complex-smart-house      - Complex smart house benchmarks"
	@echo "  bench-all                      - Run all main benchmarks"
	@echo ""
	@echo "Quick Benchmark Targets (reduced repetitions):"
	@echo "  bench-quick-*            - Quick variants of all above benchmarks"
	@echo "  bench-quick-all          - Run all quick benchmarks"
	@echo ""
	@echo "Integrated Benchmark + Plot Targets:"
	@echo "  bench-and-plot-all              - Run all benchmarks + generate plots"
	@echo "  bench-quick-and-plot-all        - Run quick benchmarks + generate plots"
	@echo "  bench-smart-house-and-plot      - Run smart house benchmark + generate plots"
	@echo "  bench-quick-smart-house-and-plot - Run quick smart house + generate plots"
	@echo ""
	@echo "Plotting Targets:"
	@echo "  plot-install             - Install Python dependencies"
	@echo "  plot-all                 - Generate execution time + throughput plots for all CSV files"
	@echo "  plot-latest              - Generate execution time + throughput plots for latest CSV files"
	@echo "  plot-custom              - Custom plotting (FILES='...' [LABELS='...'] [THROUGHPUT=no])"
	@echo "  plot-execution-only      - Generate only execution time plots (no throughput)"
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
$(if $(filter normal,$(1)),size $(SIZE_ARGS),$(if $(filter noisy,$(1)),size $(SIZE_ARGS) -v -N $(NOISE_MESSAGES_IN_SIZE),size $(SIZE_ARGS)))
endef

define size_guards_benchmark_cmd
$(if $(filter normal,$(1)),size-with-guards $(SIZE_ARGS) -v normal,$(if $(filter noisy,$(1)),size-with-guards $(SIZE_ARGS) -v noisy -N $(NOISE_MESSAGES_IN_SIZE),$(if $(filter non-matching,$(1)),size-with-guards $(SIZE_ARGS) -v non-satisfying -P $(NON_MATCHING_MESSAGES_IN_SIZE),size-with-guards $(SIZE_ARGS) -v normal)))
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
	$(call run_benchmark,quick size,size $(COMMON_ARGS_QUICK) $(QUICK_SIZE_EXTRA))

bench-quick-size-noise:
	$(call run_benchmark,quick size with noise,size $(COMMON_ARGS_QUICK) $(QUICK_SIZE_EXTRA) -v -N $(NOISE_MESSAGES_IN_SIZE))

bench-quick-size-guards:
	$(call run_benchmark,quick size with guards,size-with-guards $(COMMON_ARGS_QUICK) $(QUICK_SIZE_EXTRA) -v normal)

bench-quick-size-guards-noise:
	$(call run_benchmark,quick size with guards and noise,size-with-guards $(COMMON_ARGS_QUICK) $(QUICK_SIZE_EXTRA) -v noisy -N $(NOISE_MESSAGES_IN_SIZE))

bench-quick-size-guards-non-matching:
	$(call run_benchmark,quick size with guards and non-matching payloads,size-with-guards $(COMMON_ARGS_QUICK) $(QUICK_SIZE_EXTRA) -v non-satisfying -P $(NON_MATCHING_MESSAGES_IN_SIZE))

bench-quick-bounded-buffer:
	$(call run_benchmark,quick bounded buffer,bounded-buffer $(COMMON_ARGS_QUICK) $(QUICK_BOUNDED_BUFFER_EXTRA))

bench-quick-simple-smart-house:
	$(call run_benchmark,quick simple smart house,simple-smart-house $(COMMON_ARGS_QUICK) $(QUICK_SMART_HOUSE_EXTRA))

bench-quick-complex-smart-house:
	$(call run_benchmark,quick complex smart house,complex-smart-house $(COMMON_ARGS_QUICK) $(QUICK_SMART_HOUSE_EXTRA))

# Benchmark Suites
MAIN_BENCHMARKS := bench-size bench-size-noise bench-size-guards bench-size-guards-noise bench-size-guards-non-matching bench-bounded-buffer bench-complex-smart-house
QUICK_BENCHMARKS := bench-quick-size bench-quick-size-noise bench-quick-size-guards bench-quick-size-guards-noise bench-quick-size-guards-non-matching bench-quick-bounded-buffer bench-quick-complex-smart-house

bench-all: $(MAIN_BENCHMARKS)
	@echo "✓ All main benchmarks completed!"

bench-quick-all: $(QUICK_BENCHMARKS)
	@echo "✓ Quick benchmark suite completed!"

# Integrated Benchmark + Plot Targets
bench-and-plot-all: bench-all plot-latest
	@echo "🎯 Benchmarks completed and latest plots generated!"

bench-quick-and-plot-all: bench-quick-all plot-latest
	@echo "🎯 Quick benchmarks completed and latest plots generated!"

bench-smart-house-and-plot: bench-simple-smart-house plot-latest
	@echo "🎯 Smart house benchmark completed and plots generated!"

bench-quick-smart-house-and-plot: bench-quick-simple-smart-house plot-latest
	@echo "🎯 Quick smart house benchmark completed and plots generated!"

# Plotting Targets
PYTHON := python3
BENCHMARK_RESULTS_DIR := benchmarks/benchmark-results
PLOT_SCRIPT := benchmarks/benchmark-results/plot_benchmarks.py
PLOT_DATA_DIR := benchmarks/benchmark-results/data
PLOT_OUTPUT_DIR := benchmarks/benchmark-results/plots

plot-install:
	@echo "📦 Installing Python dependencies for plotting..."
	pip3 install -r $(BENCHMARK_RESULTS_DIR)/requirements.txt
	@echo "✓ Python dependencies installed!"

plot-all:
	@echo "📊 Generating execution time and throughput plots for all CSV files..."
	$(PYTHON) $(PLOT_SCRIPT) --data-dir $(PLOT_DATA_DIR) --output-dir $(PLOT_OUTPUT_DIR) --throughput
	@echo "✓ All plots (execution time + throughput) generated in $(PLOT_OUTPUT_DIR)/"

plot-latest:
	@echo "📊 Generating execution time and throughput plots for the most recent CSV files..."
	@ls -t $(PLOT_DATA_DIR)/*.csv 2>/dev/null | head -5 | while IFS= read -r file; do \
		echo "Processing: $$file"; \
		$(PYTHON) $(PLOT_SCRIPT) --files "$$file" --output-dir $(PLOT_OUTPUT_DIR) --throughput; \
	done
	@echo "✓ Latest plots (execution time + throughput) generated in $(PLOT_OUTPUT_DIR)/"

plot-custom:
	@echo "📊 Generating custom execution time and throughput plots..."
	@if [ -z "$(FILES)" ]; then \
		echo "❌ Error: FILES variable not set."; \
		echo "Usage: make plot-custom FILES='file1.csv file2.csv' [LABELS='Label1 Label2'] [THROUGHPUT=no]"; \
		exit 1; \
	fi
	$(PYTHON) $(PLOT_SCRIPT) --files $(FILES) $(if $(LABELS),--labels $(LABELS)) --output-dir $(PLOT_OUTPUT_DIR) $(if $(findstring no,$(THROUGHPUT)),,--throughput)
	@echo "✓ Custom plots generated in $(PLOT_OUTPUT_DIR)/"

plot-execution-only:
	@echo "📊 Generating execution time plots only (no throughput)..."
	$(PYTHON) $(PLOT_SCRIPT) --data-dir $(PLOT_DATA_DIR) --output-dir $(PLOT_OUTPUT_DIR)
	@echo "✓ Execution time plots generated in $(PLOT_OUTPUT_DIR)/"

plot-throughput-only:
	@echo "📊 Generating throughput plots only (no execution time)..."
	@echo "❌ Error: Throughput-only mode not supported. Use plot-all or plot-custom with THROUGHPUT=no for execution-only."

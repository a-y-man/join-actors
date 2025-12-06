# Makefile for Join Patterns Benchmarks
# Run benchmarks using SBT for various join pattern matching algorithms

.PHONY: help clean compile test benchmarks build console rebuild info docker
.PHONY: bench-size bench-size-noise bench-size-guards bench-size-guards-noise bench-size-guards-non-matching
.PHONY: bench-bounded-buffer bench-simple-smart-house bench-complex-smart-house bench-all
.PHONY: bench-quick-size bench-quick-size-noise bench-quick-size-guards bench-quick-size-guards-noise bench-quick-size-guards-non-matching
.PHONY: bench-quick-bounded-buffer bench-quick-simple-smart-house bench-quick-complex-smart-house bench-quick-all
.PHONY: bench-and-plot-all bench-quick-and-plot-all bench-smart-house-and-plot bench-quick-smart-house-and-plot
.PHONY: plot-install plot-all plot-latest plot-custom plot-execution-only plot-throughput-only
.PHONY: figure-4-top figure-4-bottom figure-5-top figure-5-middle figure-5-bottom figure-6 figure-7
.PHONY: bench-figure-4-top bench-figure-4-bottom bench-figure-5-top bench-figure-5-middle bench-figure-5-bottom bench-figure-6 bench-figure-7
.PHONY: table-2 table-3 table-4 table-5 table-6 table-7 table-8
.PHONY: kick-the-tires full-evaluation plot-with-tables
.PHONY: tables-all

# Configuration Variables
SBT := sbt
SELECTED_MATCHERS := "brute,stateful,while-lazy,lazy-parallel,filtering-parallel"
DATA_DIR := benchmarks/benchmark-results/programming_journal25_data

# Docker Configuration
DOCKER_IMAGE := programming_journal25_artifact
USER_DIR := /join-actors

# Quick Benchmark Parameters
QUICK_WARMUP ?= 5
QUICK_REPS ?= 5

# Main Benchmark Parameters
WARMUP ?= 5
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
QUICK_SIZE_ARGS := --min-param 1 --max-param 5 -m 3 
QUICK_BOUNDED_BUFFER_ARGS := --bufferBound 5 --count 5
QUICK_SMART_HOUSE_ARGS := --param-step 1 -m 5 --max-param 5

# Default target
help:
	@echo "Join Patterns Benchmarks - Available Targets:"
	@echo ""
	@echo "Docker Targets:"
	@echo "  docker                   - Build Docker image and run interactive container"
	@echo ""
	@echo "Build Targets:"
	@echo "  build, compile           - Compile the project"
	@echo "  clean                    - Clean build artifacts"
	@echo "  test                     - Run tests"
	@echo "  rebuild                  - Clean and recompile"
	@echo ""
	@echo "Evaluation Targets:"
	@echo "  kick-the-tires          - Quick evaluation (15-20 min): quick benchmarks + plots + tables"
	@echo "  full-evaluation         - Full evaluation (3-4 hours): all benchmarks + plots + tables"
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
	@echo "Figure-Based Benchmark Aliases:"
	@echo "  bench-figure-4-top      - Same as bench-size (Figure 4 top)"
	@echo "  bench-figure-4-bottom   - Same as bench-size-noise (Figure 4 bottom)"
	@echo "  bench-figure-5-top      - Same as bench-size-guards (Figure 5 top)"
	@echo "  bench-figure-5-middle   - Same as bench-size-guards-noise (Figure 5 middle)"
	@echo "  bench-figure-5-bottom   - Same as bench-size-guards-non-matching (Figure 5 bottom)"
	@echo "  bench-figure-6          - Same as bench-complex-smart-house (Figure 6)"
	@echo "  bench-figure-7          - Same as bench-bounded-buffer (Figure 7)"
	@echo ""
	@echo "Table Generation Targets (improvement factor tables only):"
	@echo "  table-2                 - Size benchmark table"
	@echo "  table-3                 - Size with noise table"
	@echo "  table-4                 - Size with guards table"
	@echo "  table-5                 - Size with guards + noise table"
	@echo "  table-6                 - Size with guards + non-matching table"
	@echo "  table-7                 - Complex smart house table"
	@echo "  table-8                 - Bounded buffer table"
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
	@echo "  plot-with-tables         - Generate plots + improvement factor tables for latest CSV files"
	@echo "  plot-custom              - Custom plotting (FILES='...' [LABELS='...'] [THROUGHPUT=no])"
	@echo "  plot-execution-only      - Generate only execution time plots (no throughput)"
	@echo ""
	@echo "Configuration Variables:"
	@echo "  SELECTED_MATCHERS='...' - Matcher selection"
	@echo ""

# Docker Targets
docker:
	@echo "Building Docker image: $(DOCKER_IMAGE)..."
	docker build -t $(DOCKER_IMAGE) .
	@echo "Starting interactive Docker container..."
	docker run -it --rm -v $$(pwd):$(USER_DIR) -w $(USER_DIR) $(DOCKER_IMAGE) /bin/bash

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

# Figure 4 top
bench-size:
	$(call run_benchmark,size,$(call size_benchmark_cmd,normal))

# Figure 4 bottom
bench-size-noise:
	$(call run_benchmark,size with noise,$(call size_benchmark_cmd,noisy))

# Figure 5 top
bench-size-guards:
	$(call run_benchmark,size with guards,$(call size_guards_benchmark_cmd,normal))

# Figure 5 middle
bench-size-guards-noise:
	$(call run_benchmark,size with guards and noise,$(call size_guards_benchmark_cmd,noisy))

# Figure 5 bottom
bench-size-guards-non-matching:
	$(call run_benchmark,size with guards and non-matching payloads,$(call size_guards_benchmark_cmd,non-matching))

# Figure 6
bench-complex-smart-house:
	$(call run_benchmark,complex smart house,complex-smart-house $(SMART_HOUSE_ARGS))

# Figure 7
bench-bounded-buffer:
	$(call run_benchmark,bounded buffer,bounded-buffer $(BOUNDED_BUFFER_ARGS))

bench-simple-smart-house:
	$(call run_benchmark,simple smart house,simple-smart-house $(SMART_HOUSE_ARGS))

# Quick Benchmark Variants (reduced matches/parameters)
bench-quick-size:
	$(call run_benchmark,quick size,size $(COMMON_ARGS_QUICK) $(QUICK_SIZE_ARGS))

bench-quick-size-noise:
	$(call run_benchmark,quick size with noise,size $(COMMON_ARGS_QUICK) $(QUICK_SIZE_ARGS) -v -N $(NOISE_MESSAGES_IN_SIZE))

bench-quick-size-guards:
	$(call run_benchmark,quick size with guards,size-with-guards $(COMMON_ARGS_QUICK) $(QUICK_SIZE_ARGS) -v normal)

bench-quick-size-guards-noise:
	$(call run_benchmark,quick size with guards and noise,size-with-guards $(COMMON_ARGS_QUICK) $(QUICK_SIZE_ARGS) -v noisy -N $(NOISE_MESSAGES_IN_SIZE))

bench-quick-size-guards-non-matching:
	$(call run_benchmark,quick size with guards and non-matching payloads,size-with-guards $(COMMON_ARGS_QUICK) $(QUICK_SIZE_ARGS) -v non-satisfying -P $(NON_MATCHING_MESSAGES_IN_SIZE))

bench-quick-bounded-buffer:
	$(call run_benchmark,quick bounded buffer,bounded-buffer $(COMMON_ARGS_QUICK) $(QUICK_BOUNDED_BUFFER_ARGS))

bench-quick-simple-smart-house:
	$(call run_benchmark,quick simple smart house,simple-smart-house $(COMMON_ARGS_QUICK) $(QUICK_SMART_HOUSE_ARGS))

bench-quick-complex-smart-house:
	$(call run_benchmark,quick complex smart house,complex-smart-house $(COMMON_ARGS_QUICK) $(QUICK_SMART_HOUSE_ARGS))

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
TABLE_OUTPUT_DIR := benchmarks/benchmark-results/tables

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


tables-all: table-2 table-3 table-4 table-5 table-6 table-7 table-8
	@echo "📄 All improvement factor tables regenerated (Tables 2-8)."


# Figure-Based Benchmark Aliases (for paper reference)
figure-4-top bench-figure-4-top: bench-size
	@echo "✓ Figure 4 (top) benchmark completed!"

figure-4-bottom bench-figure-4-bottom: bench-size-noise
	@echo "✓ Figure 4 (bottom) benchmark completed!"

figure-5-top bench-figure-5-top: bench-size-guards
	@echo "✓ Figure 5 (top) benchmark completed!"

figure-5-middle bench-figure-5-middle: bench-size-guards-noise
	@echo "✓ Figure 5 (middle) benchmark completed!"

figure-5-bottom bench-figure-5-bottom: bench-size-guards-non-matching
	@echo "✓ Figure 5 (bottom) benchmark completed!"

figure-6 bench-figure-6: bench-complex-smart-house
	@echo "✓ Figure 6 benchmark completed!"

figure-7 bench-figure-7: bench-bounded-buffer
	@echo "✓ Figure 7 benchmark completed!"

# Table Generation Targets (improvement factor tables only)
table-2:
	@echo "📊 Generating Table 2: Performance of Join Pattern Matching..."
	@mkdir -p $(TABLE_OUTPUT_DIR)
	@latest_file=$$(ls -t $(PLOT_DATA_DIR)/*join_pattern*.csv 2>/dev/null | grep -v noise | grep -v guard | head -1); \
	if [ -z "$$latest_file" ]; then \
		echo "❌ Error: No matching CSV file found for Size Benchmark"; \
		exit 1; \
	fi; \
	$(PYTHON) $(PLOT_SCRIPT) --files "$$latest_file" --table-only --save-tables --table-output $(TABLE_OUTPUT_DIR)/Table_2_Performance_of_Join_Pattern_Matching.md --table-title "Table 2: Performance of Join Pattern Matching"
	@echo "✓ Table saved to $(TABLE_OUTPUT_DIR)/Table_2_Performance_of_Join_Pattern_Matching.md"

table-3:
	@echo "📊 Generating Table 3: Performance of Join Pattern Matching with Noise..."
	@mkdir -p $(TABLE_OUTPUT_DIR)
	@latest_file=$$(ls -t $(PLOT_DATA_DIR)/*join_pattern*noise*.csv 2>/dev/null | grep -v guard | head -1); \
	if [ -z "$$latest_file" ]; then \
		echo "❌ Error: No matching CSV file found for Size with Noise Benchmark"; \
		exit 1; \
	fi; \
	$(PYTHON) $(PLOT_SCRIPT) --files "$$latest_file" --table-only --save-tables --table-output $(TABLE_OUTPUT_DIR)/Table_3_Performance_of_Join_Pattern_Matching_with_Noise.md --table-title "Table 3: Performance of Join Pattern Matching with Noise"
	@echo "✓ Table saved to $(TABLE_OUTPUT_DIR)/Table_3_Performance_of_Join_Pattern_Matching_with_Noise.md"

table-4:
	@echo "📊 Generating Table 4: Performance of Guarded Join Pattern Matching..."
	@mkdir -p $(TABLE_OUTPUT_DIR)
	@latest_file=$$(ls -t $(PLOT_DATA_DIR)/*guard*.csv 2>/dev/null | grep -v noise | grep -v "non" | head -1); \
	if [ -z "$$latest_file" ]; then \
		echo "❌ Error: No matching CSV file found for Size with Guards Benchmark"; \
		exit 1; \
	fi; \
	$(PYTHON) $(PLOT_SCRIPT) --files "$$latest_file" --table-only --save-tables --table-output $(TABLE_OUTPUT_DIR)/Table_4_Performance_of_Guarded_Join_Pattern_Matching.md --table-title "Table 4: Performance of Guarded Join Pattern Matching"
	@echo "✓ Table saved to $(TABLE_OUTPUT_DIR)/Table_4_Performance_of_Guarded_Join_Pattern_Matching.md"

table-5:
	@echo "📊 Generating Table 5: Performance of Guarded Join Pattern Matching with Noise..."
	@mkdir -p $(TABLE_OUTPUT_DIR)
	@latest_file=$$(ls -t $(PLOT_DATA_DIR)/*guard*noise*.csv 2>/dev/null | grep -v "non" | head -1); \
	if [ -z "$$latest_file" ]; then \
		echo "❌ Error: No matching CSV file found for Size with Guards and Noise Benchmark"; \
		exit 1; \
	fi; \
	$(PYTHON) $(PLOT_SCRIPT) --files "$$latest_file" --table-only --save-tables --table-output $(TABLE_OUTPUT_DIR)/Table_5_Performance_of_Guarded_Join_Pattern_Matching_with_Noise.md --table-title "Table 5: Performance of Guarded Join Pattern Matching with Noise"
	@echo "✓ Table saved to $(TABLE_OUTPUT_DIR)/Table_5_Performance_of_Guarded_Join_Pattern_Matching_with_Noise.md"

table-6:
	@echo "📊 Generating Table 6: Performance of Guarded Join Pattern Matching with Non-Satisfying Payloads..."
	@mkdir -p $(TABLE_OUTPUT_DIR)
	@latest_file=$$(ls -t $(PLOT_DATA_DIR)/*non*.csv 2>/dev/null | head -1); \
	if [ -z "$$latest_file" ]; then \
		echo "❌ Error: No matching CSV file found for Size with Guards and Non-Matching Benchmark"; \
		exit 1; \
	fi; \
	$(PYTHON) $(PLOT_SCRIPT) --files "$$latest_file" --table-only --save-tables --table-output $(TABLE_OUTPUT_DIR)/Table_6_Performance_of_Guarded_Join_Pattern_Matching_with_Non-Satisfying_Payloads.md --table-title "Table 6: Performance of Guarded Join Pattern Matching with Non-Satisfying Payloads"
	@echo "✓ Table saved to $(TABLE_OUTPUT_DIR)/Table_6_Performance_of_Guarded_Join_Pattern_Matching_with_Non-Satisfying_Payloads.md"

table-7:
	@echo "📊 Generating Table 7: Smart House Benchmark..."
	@mkdir -p $(TABLE_OUTPUT_DIR)
	@latest_file=$$(ls -t $(PLOT_DATA_DIR)/*Smart*House*.csv 2>/dev/null | head -1); \
	if [ -z "$$latest_file" ]; then \
		echo "❌ Error: No matching CSV file found for Complex Smart House Benchmark"; \
		exit 1; \
	fi; \
	$(PYTHON) $(PLOT_SCRIPT) --files "$$latest_file" --table-only --save-tables --table-output $(TABLE_OUTPUT_DIR)/Table_7_Smart_House_Benchmark.md --table-title "Table 7: Smart House Benchmark"
	@echo "✓ Table saved to $(TABLE_OUTPUT_DIR)/Table_7_Smart_House_Benchmark.md"

table-8:
	@echo "📊 Generating Table 8: Bounded Buffer Benchmark..."
	@mkdir -p $(TABLE_OUTPUT_DIR)
	@latest_file=$$(ls -t $(PLOT_DATA_DIR)/*Bounded*Buffer*.csv 2>/dev/null | head -1); \
	if [ -z "$$latest_file" ]; then \
		echo "❌ Error: No matching CSV file found for Bounded Buffer Benchmark"; \
		exit 1; \
	fi; \
	$(PYTHON) $(PLOT_SCRIPT) --files "$$latest_file" --table-only --save-tables --table-output $(TABLE_OUTPUT_DIR)/Table_8_Bounded_Buffer_Benchmark.md --table-title "Table 8: Bounded Buffer Benchmark"
	@echo "✓ Table saved to $(TABLE_OUTPUT_DIR)/Table_8_Bounded_Buffer_Benchmark.md"

# Evaluation Targets
kick-the-tires:
	@echo "🚀 Starting kick-the-tires evaluation (quick benchmarks)..."
	@echo "This will take approximately X minutes."
	@echo ""
	$(MAKE) bench-quick-all
	@echo ""
	@echo "📊 Generating plots and improvement factor tables..."
	$(MAKE) plot-latest
	@echo ""
	$(MAKE) tables-all
	@echo ""
	@echo "✅ Kick-the-tires evaluation complete!"
	@echo "📁 Plots: $(PLOT_OUTPUT_DIR)/"
	@echo "📄 Tables: $(TABLE_OUTPUT_DIR)/"

full-evaluation:
	@echo "🚀 Starting full evaluation (all benchmarks)..."
	@echo "⚠️  This will take approximately X hours on a Xeon-based system."
	@echo ""
	$(MAKE) bench-all
	@echo ""
	@echo "📊 Generating plots and improvement factor tables..."
	$(MAKE) plot-latest
	@echo ""
	$(MAKE) tables-all
	@echo ""
	@echo "✅ Full evaluation complete!"
	@echo "📁 Plots: $(PLOT_OUTPUT_DIR)/"
	@echo "📄 Tables: $(TABLE_OUTPUT_DIR)/"

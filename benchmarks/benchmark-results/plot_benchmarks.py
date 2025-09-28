#!/usr/bin/env python3
"""
Benchmark Plot Generator

This script generates LaTeX-styled matplotlib plots from CSV benchmark data files.
It processes individual CSV files and creates PDF plots for execution time and 
optionally throughput (matches per second) for each one.

Usage:
    python plot_benchmarks.py [options]

Examples:
    # Plot all CSV files in the data directory (execution time only)
    python plot_benchmarks.py

    # Plot execution time and throughput plots
    python plot_benchmarks.py --throughput

    # Plot specific files with custom labels
    python plot_benchmarks.py --files data/file1.csv data/file2.csv --labels "Benchmark 1" "Benchmark 2"

    # Save plots to a specific directory
    python plot_benchmarks.py --output-dir plots/

    # Customize plot style (smaller size with larger fonts by default)
    python plot_benchmarks.py --figsize 10 6
"""

import argparse
import os
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.style as mplstyle
import numpy as np
from pathlib import Path
import re
from typing import List, Optional, Tuple
import warnings
warnings.filterwarnings('ignore')

# LaTeX-style matplotlib configuration with larger fonts for smaller plots
plt.rcParams.update({
    'text.usetex': True,  # Set to True if LaTeX is installed
    'font.family': 'serif',
    'font.serif': ['Times', 'Computer Modern Roman'],
    'font.size': 16,
    'axes.labelsize': 18,
    'axes.titlesize': 20,
    'xtick.labelsize': 16,
    'ytick.labelsize': 16,
    'legend.fontsize': 14,
    'figure.titlesize': 22,
    'axes.grid': True,
    'grid.alpha': 0.3,
    'axes.spines.top': False,
    'axes.spines.right': False,
    'axes.linewidth': 1.2,
    'xtick.major.width': 1.2,
    'ytick.major.width': 1.2,
})

class BenchmarkPlotter:
    """Generate plots from benchmark CSV data."""
    
    def __init__(self, output_dir: str = "plots", figsize: Tuple[int, int] = (8, 5)):
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(exist_ok=True)
        self.figsize = figsize
        
        # Color palette for algorithms
        self.colors = [
            '#1f77b4',  # Blue
            '#ff7f0e',  # Orange  
            '#2ca02c',  # Green
            '#d62728',  # Red
            '#9467bd',  # Purple
            '#8c564b',  # Brown
            '#e377c2',  # Pink
            '#7f7f7f',  # Gray
            '#bcbd22',  # Olive
            '#17becf'   # Cyan
        ]
        
        # Line styles for variety
        self.line_styles = ['-', '--', '-.', ':', '-', '--', '-.', ':', '-', '--']
        
        # Markers for data points
        self.markers = ['o', 's', '^', 'D', 'v', '<', '>', 'p', '*', 'h']
    
    def extract_title_from_filename(self, filename: str) -> str:
        """Extract a clean title from the CSV filename."""
        # Remove timestamp prefix and file extension
        title = re.sub(r'^\d{4}_\d{2}_\d{2}_\d{2}_\d{2}_\d{2}_', '', filename)
        title = re.sub(r'\.csv$', '', title)
        
        # Clean up the title
        title = title.replace('_', ' ').title()
        return title
    
    def parse_csv_file(self, filepath: str) -> Tuple[pd.DataFrame, str, str, Optional[pd.Series]]:
        """Parse a CSV file and extract data, x-label, title, and matches column."""
        df = pd.read_csv(filepath)

        # Clean column names and capture x-axis label
        df.columns = [col.strip() for col in df.columns]
        x_label = df.columns[0]

        matches_series: Optional[pd.Series] = None
        if len(df.columns) > 1:
            for candidate in df.columns[1:]:
                if candidate.strip().lower() == "matches":
                    matches_series = df[candidate].copy()
                    df = df.drop(columns=[candidate])
                    break

        # Extract title from filename
        filename = Path(filepath).name
        title = self.extract_title_from_filename(filename)

        return df, x_label, title, matches_series
    
    def parse_algorithm_repetitions(self, df: pd.DataFrame) -> Tuple[List[str], pd.DataFrame, pd.DataFrame]:
        """Parse CSV with repetitions and return algorithm names, means, and standard deviations."""
        algorithm_names: List[str] = []
        algorithm_columns: dict[str, List[str]] = {}

        # Check if CSV has repetition columns (after the x-axis column)
        has_repetitions = any('(rep ' in col for col in df.columns[1:])

        if has_repetitions:
            for col in df.columns[1:]:  # Skip the first column (x-axis)
                if '(rep ' in col:
                    algo_name = col.split(' (rep ')[0].strip()
                    algorithm_columns.setdefault(algo_name, []).append(col)
                elif '(avg)' in col:
                    continue

            means_data: dict[str, List[float]] = {}
            stds_data: dict[str, List[float]] = {}
            x_values = df[df.columns[0]].values

            for algo_name, rep_cols in algorithm_columns.items():
                means: List[float] = []
                stds: List[float] = []

                for _, row in df.iterrows():
                    rep_values = [row[col] for col in rep_cols]
                    rep_values = [float(val) for val in rep_values if pd.notna(val)]

                    if rep_values:
                        rep_values_seconds = [val / 1000.0 for val in rep_values]
                        mean_val = float(np.mean(rep_values_seconds))
                        std_val = float(np.std(rep_values_seconds, ddof=1)) if len(rep_values_seconds) > 1 else 0.0
                    else:
                        mean_val = 0.0
                        std_val = 0.0

                    means.append(mean_val)
                    stds.append(std_val)

                means_data[algo_name] = means
                stds_data[algo_name] = stds
                algorithm_names.append(algo_name)

            means_df = pd.DataFrame(means_data)
            means_df.insert(0, df.columns[0], x_values)

            stds_df = pd.DataFrame(stds_data)
            stds_df.insert(0, df.columns[0], x_values)

        else:
            x_values = df[df.columns[0]].values
            algorithm_names = [col.strip() for col in df.columns[1:]]

            means_data = {}
            stds_data = {}

            for algo_name in algorithm_names:
                values = df[algo_name].values
                means_data[algo_name] = [float(val) / 1000.0 for val in values]
                stds_data[algo_name] = [0.0] * len(values)

            means_df = pd.DataFrame(means_data)
            means_df.insert(0, df.columns[0], x_values)

            stds_df = pd.DataFrame(stds_data)
            stds_df.insert(0, df.columns[0], x_values)

        return algorithm_names, means_df, stds_df

    def extract_match_counts(self, matches_series: Optional[pd.Series], length: int) -> List[Optional[float]]:
        """Convert the optional matches column into numeric counts per row."""
        if matches_series is None:
            return [None] * length

        raw_values = matches_series.tolist()
        counts: List[Optional[float]] = []

        for idx, raw in enumerate(raw_values):
            if pd.isna(raw):
                counts.append(None)
                continue

            if isinstance(raw, (int, float, np.integer, np.floating)):
                counts.append(float(raw))
                continue

            parts = re.split(r"[|,]", str(raw))
            numeric_parts = []
            for part in parts:
                part = part.strip()
                if not part:
                    continue
                try:
                    numeric_parts.append(float(part))
                except ValueError:
                    continue

            if numeric_parts:
                if len(set(numeric_parts)) > 1:
                    print(
                        f"Warning: multiple match counts detected in row {idx}: {raw}. "
                        f"Using average {np.mean(numeric_parts):.2f}."
                    )
                counts.append(float(np.mean(numeric_parts)))
            else:
                counts.append(None)

        if len(counts) < length:
            counts.extend([None] * (length - len(counts)))
        elif len(counts) > length:
            counts = counts[:length]

        return counts
    
    def calculate_statistics(self, data: List[float]) -> Tuple[float, float]:
        """Calculate mean and standard deviation."""
        if len(data) == 0:
            return 0.0, 0.0
        
        mean_val = np.mean(data)
        std_val = np.std(data, ddof=1) if len(data) > 1 else 0.0
        return mean_val, std_val
    
    def clean_algorithm_name(self, name: str) -> str:
        """Clean algorithm names for display."""
        # Remove common suffixes like _8, _16, etc.
        name = re.sub(r'_\d+$', '', name)
        
        # Convert camelCase to spaced words
        name = re.sub(r'([a-z])([A-Z])', r'\1 \2', name)
        
        # Clean up specific algorithm names
        replacements = {
            'Algorithm': '',
            'BruteForce': 'Brute Force',
            'StatefulTreeBased': 'Stateful Tree-Based',
            'WhileLazy': 'While Lazy',
            'FilteringParallel': 'Filtering Parallel'
        }
        
        for old, new in replacements.items():
            name = name.replace(old, new)
        
        return name.strip()
    
    def plot_single_file(self, filepath: str, custom_label: Optional[str] = None) -> str:
        """Generate a plot for a single CSV file with error bars."""
        df, x_label, title, _ = self.parse_csv_file(filepath)
        
        if custom_label:
            title = custom_label
        
        # Parse algorithm repetitions and calculate means/stds
        algorithm_names, means_df, stds_df = self.parse_algorithm_repetitions(df)
        
        # Create figure
        fig, ax = plt.subplots(figsize=self.figsize)
        
        # Plot each algorithm with error bars
        for i, algorithm in enumerate(algorithm_names):
            x_values = means_df[means_df.columns[0]].values
            y_means = means_df[algorithm].values
            y_stds = stds_df[algorithm].values
            
            # Clean algorithm name for legend
            clean_name = self.clean_algorithm_name(algorithm)
            
            # Plot line with markers and error bars
            color = self.colors[i % len(self.colors)]
            ax.errorbar(x_values, y_means, yerr=y_stds,
                       color=color,
                       linestyle=self.line_styles[i % len(self.line_styles)],
                       marker=self.markers[i % len(self.markers)],
                       markersize=6,
                       linewidth=2,
                       capsize=4,
                       capthick=1.5,
                       label=clean_name,
                       alpha=0.8)
        
        # Customize plot
        ax.set_xlabel(x_label, fontweight='bold')
        ax.set_ylabel(r'Execution Time (seconds) - Log Scale', fontweight='bold')
        ax.set_title(title, fontweight='bold', pad=20)
        
        # Set y-axis to log scale by default
        ax.set_yscale('log')
        
        # Set x-axis ticks to exact values from the CSV
        ax.set_xticks(x_values)
        ax.set_xticklabels([str(int(x)) for x in x_values])
        
        # Add legend at the bottom in a 2x2 grid
        legend = ax.legend(frameon=True, fancybox=True, shadow=True, 
                          bbox_to_anchor=(0.5, -0.15), loc='upper center',
                          ncol=2, columnspacing=1.5, handletextpad=0.5)
        
        # Adjust layout to accommodate legend at bottom
        plt.tight_layout()
        plt.subplots_adjust(bottom=0.2)
        
        # Save plot
        output_filename = f"{Path(filepath).stem}_plot.pdf"
        output_path = self.output_dir / output_filename
        plt.savefig(output_path, bbox_inches='tight', 
                   facecolor='white', edgecolor='none', format='pdf')
        
        print(f"Plot saved: {output_path}")
        plt.close()
        
        return str(output_path)
    
    def plot_matches_per_second(self, filepath: str, custom_label: Optional[str] = None) -> str:
        """Generate a plot showing matches per second for each algorithm."""
        df, x_label, title, matches_series = self.parse_csv_file(filepath)
        
        if custom_label:
            title = custom_label
        
        # Parse algorithm repetitions and calculate means/stds
        algorithm_names, means_df, stds_df = self.parse_algorithm_repetitions(df)
        
        # Create figure
        fig, ax = plt.subplots(figsize=self.figsize)
        
        # Plot each algorithm with error bars
        for i, algorithm in enumerate(algorithm_names):
            x_values = means_df[means_df.columns[0]].values
            y_means = means_df[algorithm].values
            y_stds = stds_df[algorithm].values
            
            match_counts = self.extract_match_counts(matches_series, len(x_values))
            fallback_matches = next((val for val in match_counts if val is not None), None)
            default_matches = fallback_matches if fallback_matches is not None else 50.0

            matches_per_second = []
            
            for j, (x_val, time_mean, time_std) in enumerate(zip(x_values, y_means, y_stds)):
                matches_value = match_counts[j] if match_counts[j] is not None else default_matches
                if time_mean > 0 and matches_value is not None:
                    mps = matches_value / time_mean
                    matches_per_second.append(mps)
                else:
                    matches_per_second.append(0)
            
            # Clean algorithm name for legend
            clean_name = self.clean_algorithm_name(algorithm)
            
            # Plot line with markers (no error bars for throughput)
            color = self.colors[i % len(self.colors)]
            ax.plot(x_values, matches_per_second,
                   color=color,
                   linestyle=self.line_styles[i % len(self.line_styles)],
                   marker=self.markers[i % len(self.markers)],
                   markersize=6,
                   linewidth=2,
                   label=clean_name,
                   alpha=0.8)
        
        # Customize plot - use identical x-axis setup as execution time plot
        ax.set_xlabel(x_label, fontweight='bold')
        ax.set_ylabel(r'Matches per Second - Log Scale', fontweight='bold')
        ax.set_title(f"{title} - Throughput", fontweight='bold', pad=20)
        
        # Set y-axis to log scale by default
        ax.set_yscale('log')
        
        # Set x-axis ticks to exact values from the CSV (identical to execution time plot)
        ax.set_xticks(x_values)
        ax.set_xticklabels([str(int(x)) for x in x_values])
        
        # Add legend at the bottom in a 2x2 grid
        legend = ax.legend(frameon=True, fancybox=True, shadow=True, 
                          bbox_to_anchor=(0.5, -0.15), loc='upper center',
                          ncol=2, columnspacing=1.5, handletextpad=0.5)
        
        # Adjust layout to accommodate legend at bottom
        plt.tight_layout()
        plt.subplots_adjust(bottom=0.2)
        
        # Save plot
        output_filename = f"{Path(filepath).stem}_throughput_plot.pdf"
        output_path = self.output_dir / output_filename
        plt.savefig(output_path, bbox_inches='tight', 
                   facecolor='white', edgecolor='none', format='pdf')
        
        print(f"Throughput plot saved: {output_path}")
        plt.close()
        
        return str(output_path)


def find_csv_files(data_dir: str) -> List[str]:
    """Find all CSV files in the data directory."""
    data_path = Path(data_dir)
    if not data_path.exists():
        raise FileNotFoundError(f"Data directory not found: {data_dir}")
    
    csv_files = list(data_path.glob("*.csv"))
    return [str(f) for f in csv_files]


def main():
    parser = argparse.ArgumentParser(
        description="Generate LaTeX-styled plots from benchmark CSV files",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    
    parser.add_argument(
        '--data-dir', '-d',
        default='benchmarks/data',
        help='Directory containing CSV files (default: benchmarks/data)'
    )
    
    parser.add_argument(
        '--files', '-f',
        nargs='*',
        help='Specific CSV files to plot'
    )
    
    parser.add_argument(
        '--labels', '-l',
        nargs='*',
        help='Custom labels for the plots (must match number of files)'
    )
    
    parser.add_argument(
        '--output-dir', '-o',
        default='plots',
        help='Output directory for plots (default: plots)'
    )
    
    parser.add_argument(
        '--figsize',
        nargs=2,
        type=int,
        default=[8, 5],
        help='Figure size in inches (width height, default: 8 5)'
    )
    
    parser.add_argument(
        '--latex',
        action='store_true',
        help='Enable LaTeX rendering (requires LaTeX installation)'
    )
    
    parser.add_argument(
        '--throughput',
        action='store_true',
        help='Generate throughput plots (matches per second) in addition to execution time plots'
    )
    
    args = parser.parse_args()
    
    # Enable LaTeX if requested
    if args.latex:
        plt.rcParams['text.usetex'] = True
    
    # Initialize plotter
    plotter = BenchmarkPlotter(
        output_dir=args.output_dir,
        figsize=tuple(args.figsize)
    )
    
    # Determine which files to process
    if args.files:
        csv_files = args.files
    else:
        csv_files = find_csv_files(args.data_dir)
    
    if not csv_files:
        print("No CSV files found to process.")
        return
    
    print(f"Found {len(csv_files)} CSV files to process:")
    for f in csv_files:
        print(f"  - {f}")
    
    # Validate labels
    if args.labels and len(args.labels) != len(csv_files):
        print(f"Warning: Number of labels ({len(args.labels)}) doesn't match number of files ({len(csv_files)})")
        args.labels = None
    
    # Generate individual plots
    print("\nGenerating execution time plots...")
    for i, filepath in enumerate(csv_files):
        try:
            label = args.labels[i] if args.labels else None
            plotter.plot_single_file(filepath, label)
        except Exception as e:
            print(f"Error plotting {filepath}: {e}")
    
    # Generate throughput plots if requested
    if args.throughput:
        print("\nGenerating throughput plots (matches per second)...")
        for i, filepath in enumerate(csv_files):
            try:
                label = args.labels[i] if args.labels else None
                plotter.plot_matches_per_second(filepath, label)
            except Exception as e:
                print(f"Error plotting throughput for {filepath}: {e}")
    
    print(f"\nAll plots saved to: {args.output_dir}/")


if __name__ == '__main__':
    main()

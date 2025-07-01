#!/usr/bin/env python3
"""
Benchmark Plot Generator

This script generates LaTeX-styled matplotlib plots from CSV benchmark data files.
It processes individual CSV files and creates PDF plots for each one.

Usage:
    python plot_benchmarks.py [options]

Examples:
    # Plot all CSV files in the data directory
    python plot_benchmarks.py

    # Plot specific files with custom labels
    python plot_benchmarks.py --files data/file1.csv data/file2.csv --labels "Benchmark 1" "Benchmark 2"

    # Save plots to a specific directory
    python plot_benchmarks.py --output-dir plots/

    # Customize plot style
    python plot_benchmarks.py --figsize 12 8
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

# LaTeX-style matplotlib configuration
plt.rcParams.update({
    'text.usetex': True,  # Set to True if LaTeX is installed
    'font.family': 'serif',
    'font.serif': ['Times', 'Computer Modern Roman'],
    'font.size': 13,
    'axes.labelsize': 15,
    'axes.titlesize': 17,
    'xtick.labelsize': 13,
    'ytick.labelsize': 13,
    'legend.fontsize': 12,
    'figure.titlesize': 19,
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
    
    def __init__(self, output_dir: str = "plots", figsize: Tuple[int, int] = (10, 6)):
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
    
    def parse_csv_file(self, filepath: str) -> Tuple[pd.DataFrame, str, str]:
        """Parse a CSV file and extract data, x-label, and title."""
        df = pd.read_csv(filepath)
        
        # Get column names
        columns = df.columns.tolist()
        x_label = columns[0].strip()
        
        # Clean column names
        df.columns = [col.strip() for col in df.columns]
        
        # Extract title from filename
        filename = Path(filepath).name
        title = self.extract_title_from_filename(filename)
        
        return df, x_label, title
    
    def parse_algorithm_repetitions(self, df: pd.DataFrame) -> Tuple[List[str], pd.DataFrame, pd.DataFrame]:
        """Parse CSV with repetitions and return algorithm names, means, and standard deviations."""
        # Extract unique algorithm names by looking for pattern: "AlgorithmName (rep X)"
        algorithm_names = []
        algorithm_columns = {}
        
        # Check if CSV has repetition columns
        has_repetitions = any('(rep ' in col for col in df.columns[1:])
        
        if has_repetitions:
            # Handle CSV with repetitions
            for col in df.columns[1:]:  # Skip the first column (x-axis)
                if '(rep ' in col:
                    # Extract algorithm name before "(rep X)"
                    algo_name = col.split(' (rep ')[0].strip()
                    if algo_name not in algorithm_columns:
                        algorithm_columns[algo_name] = []
                    algorithm_columns[algo_name].append(col)
                elif '(avg)' in col:
                    # Skip average columns as we'll calculate our own
                    continue
            
            # Create dataframes for means and standard deviations
            means_data = {}
            stds_data = {}
            x_values = df[df.columns[0]].values
            
            for algo_name, rep_cols in algorithm_columns.items():
                means = []
                stds = []
                
                for _, row in df.iterrows():
                    # Get repetition values for this algorithm and row
                    rep_values = [row[col] for col in rep_cols]
                    # Convert to float and filter out any non-numeric values
                    rep_values = [float(val) for val in rep_values if pd.notna(val) and str(val).replace('.','').replace('-','').isdigit()]
                    
                    if rep_values:
                        # Convert milliseconds to seconds
                        rep_values_seconds = [val / 1000.0 for val in rep_values]
                        mean_val = np.mean(rep_values_seconds)
                        std_val = np.std(rep_values_seconds, ddof=1) if len(rep_values_seconds) > 1 else 0.0
                    else:
                        mean_val = 0.0
                        std_val = 0.0
                    
                    means.append(mean_val)
                    stds.append(std_val)
                
                means_data[algo_name] = means
                stds_data[algo_name] = stds
                algorithm_names.append(algo_name)
            
            # Create dataframes
            means_df = pd.DataFrame(means_data)
            means_df.insert(0, df.columns[0], x_values)
            
            stds_df = pd.DataFrame(stds_data)
            stds_df.insert(0, df.columns[0], x_values)
            
        else:
            # Handle CSV without repetitions - treat each column as a single measurement
            x_values = df[df.columns[0]].values
            algorithm_names = [col.strip() for col in df.columns[1:]]
            
            means_data = {}
            stds_data = {}
            
            for algo_name in algorithm_names:
                # Convert milliseconds to seconds and use as means
                values = df[algo_name].values
                means_data[algo_name] = [float(val) / 1000.0 for val in values]
                # No standard deviation for single measurements
                stds_data[algo_name] = [0.0] * len(values)
            
            # Create dataframes
            means_df = pd.DataFrame(means_data)
            means_df.insert(0, df.columns[0], x_values)
            
            stds_df = pd.DataFrame(stds_data)
            stds_df.insert(0, df.columns[0], x_values)
        
        return algorithm_names, means_df, stds_df
    
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
        df, x_label, title = self.parse_csv_file(filepath)
        
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
        default=[10, 6],
        help='Figure size in inches (width height, default: 10 6)'
    )
    
    parser.add_argument(
        '--latex',
        action='store_true',
        help='Enable LaTeX rendering (requires LaTeX installation)'
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
    print("\nGenerating individual plots...")
    for i, filepath in enumerate(csv_files):
        try:
            label = args.labels[i] if args.labels else None
            plotter.plot_single_file(filepath, label)
        except Exception as e:
            print(f"Error plotting {filepath}: {e}")
    
    print(f"\nAll plots saved to: {args.output_dir}/")


if __name__ == '__main__':
    main()

#!/usr/bin/env python3
"""
Concatenates ALMA ANTAB files produced by PolConvert.

Usage:
   combineANTABs.py [-o output.antab] infile1.antab [infile2.antab ...]

Example:
   combineANTABs.py -o c261z_Aa.antab ./POLCONVERT_AMPGAINS/*STATION_AA.ANTAB
"""

import sys
import os

import sys
import os
import argparse


def is_discardable_line(line):
    """
    Return True if the line is a comment or an ANTAB line which isn't Tsys data

    Example input:
      GAIN AA  ELEV DPFU=0.010   FREQ=10,100000
      POLY=1.0000E+00
      /
      TSYS AA  FT=1.0  TIMEOFF=0
      INDEX= 'L1|R1', 'L2|R2', 'L3|R3', 'L4|R4', 'L5|R5', 'L6|R6', 'L7|R7', 'L8|R8'
      /
      ! Column 1 = L1|R1 : DiFX freq 38, 42976.00000 MHz USB bw 64.00 MHz, center 43008.00000 MHz
      ! Column 2 = L2|R2 : DiFX freq 39, 43104.00000 MHz USB bw 64.00 MHz, center 43136.00000 MHz
      ! Column 3 = L3|R3 : DiFX freq 40, 43232.00000 MHz USB bw 64.00 MHz, center 43264.00000 MHz
      ! Column 4 = L4|R4 : DiFX freq 41, 43360.00000 MHz USB bw 64.00 MHz, center 43392.00000 MHz
      ! Column 5 = L5|R5 : DiFX freq 57, 42912.00000 MHz USB bw 64.00 MHz, center 42944.00000 MHz
      ! Column 6 = L6|R6 : DiFX freq 58, 43040.00000 MHz USB bw 64.00 MHz, center 43072.00000 MHz
      ! Column 7 = L7|R7 : DiFX freq 59, 43168.00000 MHz USB bw 64.00 MHz, center 43200.00000 MHz
      ! Column 8 = L8|R8 : DiFX freq 60, 43296.00000 MHz USB bw 64.00 MHz, center 43328.00000 MHz
      114 5:4.6709      0.5581      0.5650      0.5634      0.5708      0.5615      0.5598      0.5647      0.5682
      114 5:4.6795      0.5582      0.5651      0.5635      0.5709      0.5616      0.5599      0.5648      0.5683
      114 5:4.6882      0.5583      0.5652      0.5636      0.5710      0.5617      0.5600      0.5649      0.5684
      114 5:4.6968      0.5584      0.5654      0.5637      0.5711      0.5618      0.5601      0.5650      0.5685
      ...
    for which is_discardable_line() will identify all except the below lines as 'discardable':
      114 5:4.6709      0.5581      0.5650      0.5634      0.5708      0.5615      0.5598      0.5647      0.5682
      114 5:4.6795      0.5582      0.5651      0.5635      0.5709      0.5616      0.5599      0.5648      0.5683
      114 5:4.6882      0.5583      0.5652      0.5636      0.5710      0.5617      0.5600      0.5649      0.5684
      114 5:4.6968      0.5584      0.5654      0.5637      0.5711      0.5618      0.5601      0.5650      0.5685
      ...
    """
    stripped = line.lstrip().upper()
    if stripped.startswith('!') or stripped.startswith('/'):
        return True
    if any([token in stripped for token in ['GAIN','POLY','TSYS','INDEX']]):
        return True
    return False


def process_file(filepath, keep_nonTsys_lines=True):
    """
    Read an ANTAB file and return its content.
    Optionally removes lines that are comments or not actual Tsys data lines.
    """
    if not os.path.isfile(filepath):
        print(f"Error: File '{filepath}' not found.", file=sys.stderr)
        return None

    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            lines = f.readlines()

        if keep_nonTsys_lines:
            # Keep all lines, including comments etc
            return lines
        else:
            # Prune lines down to those with actual Tsys data, drop comments etc
            return [line for line in lines if not is_discardable_line(line)]

    except Exception as e:
        print(f"Error reading file '{filepath}': {e}", file=sys.stderr)
        return None


def concatenate_antab_files(file_paths):
    """
    Returns the content of all files, concatenated, retaining comments and
    metadata such as gain curve info from the first file only.
    """
    concatenated = []

    for i, filepath in enumerate(file_paths):

        lines = process_file(filepath, keep_nonTsys_lines=(i==0))
        if lines is None:
            continue
        concatenated.extend(lines)

    return concatenated


def main():
    """Main entry point for the script."""

    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('files', nargs='+', help='ANTAB text files to concatenate')
    parser.add_argument(
        '-o', '--output',
        help='Output file path (default: stdout)'
    )

    args = parser.parse_args()

    concatenated_lines = concatenate_antab_files(args.files)
    if concatenated_lines is None:
        sys.exit(1)
    # add a final forward slash to complete the assembeld file
    concatenated_lines.extend('/\n')

    if args.output:
        try:
            with open(args.output, 'w') as f:
                for line in concatenated_lines:
                    f.write(line)
        except Exception as e:
            print(f"Error writing to output file '{args.output}': {e}", file=sys.stderr)
            sys.exit(1)
    else:
        for line in concatenated_lines:
            print(line, end='')

if __name__ == "__main__":
    main()


#!/usr/bin/env python3

# =======================================================================
# Copyright (C) 2026 Cormac Reynolds
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the
# Free Software Foundation, Inc.,
# 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
# =======================================================================

''' 
Cormac Reynolds, June 2026: automate fits creation on Pawsey system.
Could easily be made generic by moving some Pawsey features to a config
'''

import argparse
import re
import subprocess
import os
from pathlib import Path


def find_phasecentres_and_bins(passname):
    '''Parse the filenames in each .difx directory to extract bins and phasecentres'''

    phasecentres = set()
    bins = set()

    pattern = re.compile(r"\.s(\d+)\.b(\d+)$")

    # Find all DiFX output directories for this pass
    difxdirs = sorted(Path(".").glob(f"{passname}_*.difx"))

    if not difxdirs:
        raise RuntimeError(f"No .difx directories found for {passname}")

    for difxdir in difxdirs:
        print(f"Scanning {difxdir}")

        for path in difxdir.iterdir():
            match = pattern.search(path.name)

            if match:
                phasecentres.add(int(match.group(1)))
                bins.add(int(match.group(2)))

    return sorted(phasecentres), sorted(bins), difxdirs


def main():
    parser = argparse.ArgumentParser(
        description="Convert DiFX output to FITS-IDI, auto-handling phase centres and pulsar bins."
    )

    parser.add_argument(
        "passname",
        help="Correlator pass name (e.g. v252a or v255a-line)"
    )

    args = parser.parse_args()

    phasecentres, bins, difxdirs = find_phasecentres_and_bins(args.passname)

    difx_image = os.environ["DIFX_IMAGE"]
    difxdir_str = [str(difxdir) for difxdir in difxdirs]
    fitsfiles = []
    pawsey_project = os.environ['PAWSEY_PROJECT']
    jobids = []
    for pc in phasecentres:
        for b in bins:

            # Determine FITS file name, ensuring uniqueness of phase centre and bin number
            fitsfile = f"{args.passname.upper()}"
            if len(phasecentres) > 1:
                fitsfile += f".P{pc:d}" 
            if len(bins) > 1:
                fitsfile += f".B{pc:d}" 
            fitsfile += ".FITS"
            fitsfiles += [fitsfile]

            cmd = [
                f"singularity", "exec", "--pwd", os.getcwd(), difx_image, "difx2fits", "-u",
                "--phasecenter", str(pc),
                "-B", str(b),
                *difxdir_str,
                fitsfile
            ]
            #cmd = ["echo", "$PWD"]

            cmd_str = " ".join(cmd)
            slurm_cmd = [
                    "sbatch",
                    "--export=ALL",
                    f"--account={pawsey_project}",
                    f"--job-name={fitsfile}",
                    "--time=1:00:00",
                    "--parsable",
                    "--qos=high",
                    "--mem=4000M",
                    f'--wrap={cmd_str}'
            ]

            #print("Running:", " ".join(cmd))
            #subprocess.run(cmd, check=True)
            print("Running:", " ".join(slurm_cmd))
            slurm_response = subprocess.run(slurm_cmd, capture_output=True, text=True, check=True)
            #subprocess.run(slurm_cmd, check=True)
            jobids += [slurm_response.stdout.strip()]

    print(f"Found {len(difxdirs)} DiFX job(s)")
    print(f"Phase centres: {len(phasecentres)}")
    print(f"Bins: {len(bins)}")
    print(f"FITS: {fitsfiles}")
    print(f"jobs: {' '.join(jobids)}")


if __name__ == "__main__":
    main()

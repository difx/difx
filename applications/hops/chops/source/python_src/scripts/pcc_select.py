#!/usr/bin/python
#
# Script to take proxy-cable cal .dat files and convert them to
# the cable-cal file format
# 01/25/18 first version jpb

#core imports
from __future__ import print_function
from builtins import str
from builtins import input
from builtins import range
import datetime
import argparse
import re
import string
import sys
import os

#HOPS imports
import vpal

def prompt_yes_no(prompt):
    while True:
        reply = str(input(prompt + ' (y/n):\n')).lower().strip()
        if reply[:1] == 'y':
            return True
        if reply[:1] == 'n':
            return False

################################################################################

def main():

    parser = argparse.ArgumentParser()
    parser._action_groups.pop()
    required = parser.add_argument_group('required arguments')
    optional = parser.add_argument_group('optional arguments')

    required.add_argument("-e", "--experiment", dest='experiment_name', help="Experiment name, for example: vt7226", required=True) #required argument
    required.add_argument("-d", "--dat-dir", dest='dat_directory', help="Directory containing per-band .dat files", required=True) #required argument
    optional.add_argument("-o", "--output-dir", dest='output_directory', default="DAT_DIRECTORY", help="Specify the output directory, default is DAT_DIRECTORY") #optional argument
    optional.add_argument('-y', '--yes', action='store_true', dest='yes_prompt', help='reply yes to all prompts, default=False.', default=False)
    optional.add_argument('-n', '--no', action='store_true', dest='no_prompt', help='reply no to all prompts, default=False.', default=False)

    selection_help_string = 'Space deliminated lists of selected stations, bands, and polarizations. Stations must be specified with single character code. \n' \
    ' allowable values for bands: A, B, C, D ' \
    ' allowable values for polarizations: X, Y. ' \
    ' The following two examples are equivalent ' \
    ' Example 1: (select bands/pols collectively): G:BCD:XY   E:BC:XY   V:BC:X   Y:BCD:Y ' \
    ' Example 2: (list individiual bands-pols separately): G:BX,BY,CX,CY,DX,DY   E:BX,BY,CX,CY   V:BX,CX   Y:BY,CY,DY '
    required.add_argument("-s", "--select", dest='station_bands_pols', nargs='*', help=selection_help_string, required=True) #require argument with variable number
    args = parser.parse_args()

    #construct default output directory
    default_output_directory = os.path.abspath(args.dat_directory)

    if args.output_directory != 'DAT_DIRECTORY':
        output_directory = os.path.abspath(args.output_directory)
    else:
        output_directory = default_output_directory

    if not os.path.exists(output_directory):
        os.makedirs(output_directory)

    print("experiment_name: {}".format(args.experiment_name))
    print("dat_directory: {}".format(args.dat_directory))
    print("station_bands_pols: {}".format(args.station_bands_pols))

    stations = []
    band_pols = dict()
    # bands = []
    # pols = []


    #loop over the station/band/pol arguments and determine the list of files we need
    for x in args.station_bands_pols:
        if x.count(':') == 2 and x.count(',') == 0: #do combined selection of bands/pols
            sbp_list = x.split(':')
            if len(sbp_list) != 3:
                print("Error: could not parse station:bands:polarizations from <", x, ">")
                sys.exit(1)
            elif len(sbp_list[0]) != 1:
                print("Error: could not parse station:bands:polarizations from <", x, ">")
                sys.exit(1)
            elif not all( b in 'ABCDabcd' for b in sbp_list[1]) or len(sbp_list[1]) == 0:
                print("Error: could not parse station:bands:polarizations from <", x, ">")
                sys.exit(1)
            elif not all( p in 'XYxy' for p in sbp_list[2]) or len(sbp_list[2]) == 0:
                print("Error: could not parse station:bands:polarizations from <", x, ">")
                sys.exit(1)
            else:
                stations.append(sbp_list[0])
                tmp_bp = []
                for band in sbp_list[1]:
                    for pol in sbp_list[2]:
                        tmp_bp.append( band.upper() + pol.upper() )
                band_pols[ sbp_list[0] ] = tmp_bp
                # bands.append(sbp_list[1])
                # pols.append(sbp_list[2])
        elif x.count(':') == 1 and x.count(',') != 0: #do individiual selections of bands pols
            tmp_station = x.split(':')[0]
            bp_list = (x.split(':')[1]).split(',')
            if len(bp_list) == 0:
                print("Error: could not parse station:bands:polarizations from <", x, ">")
                sys.exit(1)
            elif not all( len(y) == 2 for y in bp_list):
                print("Error: could not parse station:bands:polarizations from <", x, ">")
                sys.exit(1)
            elif not all( bp in 'AX,AY,BX,BY,CX,CY,DX,DY,ax,ay,bx,by,cx,cy,dx,dy' for bp in bp_list):
                print("Error: could not parse station:bands:polarizations from <", x, ">")
                sys.exit(1)
            else:
                stations.append(tmp_station)
                tmp_bp = []
                for bp in bp_list:
                    tmp_bp.append( bp.upper() )
                band_pols[ tmp_station ] = tmp_bp

    station_files = []
    for n in list(range(0, len(stations))):
        s = stations[n]
        # bd = sorted(set((bands[n].upper()).strip()))
        # pl = sorted(set((pols[n].upper()).strip()))
        print("Preparing to create band delay file for station: ", s, " using bands and polarizations: ", str(band_pols[s]))
        #now generate the names of the files we need to compute the band delay corrections (e.g bandmodel.G.C.X.dat)
        files_needed = []
        for bp in band_pols[s]:
            fname = 'bandmodel.' + args.experiment_name + '.' + s + '.' + bp[0] + '.' + bp[1] + '.dat'
            full_fname = os.path.join( os.path.abspath(args.dat_directory), fname )
            if os.path.exists(full_fname): #check that the file exists
                files_needed.append(full_fname)
            else:
                #now try looking for the same file with the experiment name capitalized
                fname = 'bandmodel.' + args.experiment_name.upper() + '.' + s + '.' + bp[0] + '.' + bp[1] +  '.dat'
                full_fname_upper = os.path.join( os.path.abspath(args.dat_directory), fname )
                if os.path.exists(full_fname_upper): #check that the file exists
                    files_needed.append(full_fname_upper)
                else:
                    print("Error, could not find a data file matching either: ", full_fname, " or ", full_fname_upper, ".")
                    sys.exit(1)
        if len(files_needed) != 0:
            station_files.append(files_needed)

    #now loop over each station, read the file, average and write out to the combined .pcmt file
    for n in list(range(0,len(stations))):
        s = stations[n]
        files_needed = station_files[n]
        file_data_list = []
        ave_calc = vpal.proxy_cable_cal.ExperimentMultibandDelayAverager()
        for f in files_needed:
            #read the file into a experiment_pcc_band_delay object
            file_obj = vpal.proxy_cable_cal.ExperimentPccBandDelay()
            file_obj.read_file(f)
            if len(file_obj.scan_pcc_line_list) >= 1:
                file_data_list.append(file_obj)
                ave_calc.add_band_data(file_obj)

        #now we just need to match the scans and average the bands
        #eventually we should probably implement some cut parameters to exclude
        #bad scans/fits from the average
        ave_calc.average_band_delays()

        outfile_name = args.experiment_name + (ave_calc.station_code).lower() + ".pcmt." + ave_calc.bands + "." + ave_calc.pols + ".dat"
        outfile_name= os.path.join( os.path.abspath(output_directory), outfile_name )

        #if there are pre-existing files, warn the user they are about to be overwritten and give them a chance to bail out (per-station)
        keep_going = True
        if os.path.exists(outfile_name):
            if args.no_prompt == True:
                print("Warning: file for station: " + s + " bands: " + ave_calc.bands + " and pols: " + ave_calc.pols + " will not overwritten, skipping this station." )
                keep_going = False
            elif args.yes_prompt == True:
                print("Warning: file for station: " + s + " bands: " + ave_calc.bands + " and pols: " + ave_calc.pols + " will be overwritten." )
                keep_going = True
            else:
                print("Warning: file for station: " + s + " bands: " + ave_calc.bands + " and pols: " + ave_calc.pols + " is about to be overwritten.")
                keep_going = prompt_yes_no("Do you wish to continue?")

        if keep_going == False:
            print("Skipping...")
        else:
            output_file = open(outfile_name, 'w')

            #put a header at the top of the file
            header_line1 = "#data elements: (scan_start_time) (delay) (source_name) (scan_name)\n"
            header_line2 = "#data format: (YYYY MM DD HH MM SS) (seconds) (N/A) (DOY-HHMMi)\n"
            output_file.write(header_line1)
            output_file.write(header_line2)

            for line in ave_calc.file_lines:
                output_file.write(line + "\n")

            output_file.close()

    print("Done.")

################################################################################

if __name__ == '__main__':          # official entry point
    main()
    sys.exit(0)

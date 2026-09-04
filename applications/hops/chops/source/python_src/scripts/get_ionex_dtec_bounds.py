#!/usr/bin/python

#    get_ionex_dtec_bounds.py - script to retrieve IONEX maps for ionospheric
#    TEC; parse scan calc files for station position, orientation, and time;
#    and calculate line-of-sight TEC per station.
#
#    Adapted from PolConvert by Ivan Marti-Vidal, (C) 2021
#    https://github.com/marti-vidal-i/PolConvert/blob/main/EU-VGOS/EUVGOS_PY3/PY_PHASES.py
#
#    Copyright (C) 2022  D Hoak
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.

#-3.13++#from __future__ import print_function

import os, sys
import argparse
import json

import numpy as np
import datetime as dt

import scipy.interpolate as spint


### script to generate a json file with dTEC bounds for VGOS scans

# inputs are
# - a DiFX experiment directory (for calc files)
# - a directory to find (or put) the IONEX map files
# - a name for the output JSON file
# - an IONEX product (default is the final JPL solution, "jplg")

# steps:
#
# read in calc files, generate dict of scan ra/dec, time, station location
# check for ionex files, download if necessary
# build TEC prediction for each scan/station, save to json


# dict to store two-letter-to-one-letter station codes
# for translating calc files, where the two-letter codes are capitalized




def build_station_code_dict(codes_file=None):
    '''
    Function to read a "codes" or "stations.m" file used for difx2mark4 and build a dict
    matching two-letter station codes (needed for the CALC files) to single-letter
    station codes (needed for the mk4 files).

    Expects the input codes_file to have the format:

    E Wf
    G Gs
    H K2
    V Ws
    Y Yj

    Returns a dict.
    '''

    stations_2to1 = {}
    stations_2to1['WF'] = 'E'
    stations_2to1['GS'] = 'G'
    stations_2to1['MG'] = 'M'
    stations_2to1['K2'] = 'H'
    stations_2to1['KE'] = 'K'
    stations_2to1['YJ'] = 'Y'
    stations_2to1['WS'] = 'V'
    stations_2to1['OE'] = 'S'
    stations_2to1['OW'] = 'T'
    stations_2to1['HB'] = 'O'
    stations_2to1['IS'] = 'I'
    stations_2to1['NN'] = 'N'
    stations_2to1['SA'] = 'A'
    stations_2to1['WN'] = 'W'
    stations_2to1['YG'] = 'P'
    stations_2to1['HV'] = 'J'

    if codes_file is None:
        return stations_2to1
    else:
        codes = open(codes_file)
        lines = codes.readlines()
        codes.close()

        for line in lines:
            if len(line.rstrip('\n').split(' '))>2:
                print('Station codes file has the wrong format!')
                sys.exit()
            else:
                l1, l2 = line.rstrip('\n').split(' ')
                stations_2to1[l2.upper()] = l1

        return stations_2to1


        

    
    



def build_calc_dict(difx_directory, stations_2to1):
    '''
    Function to read *.calc files from a DiFX experiment directory
    
    Returns a dictionary keyed to scan names and single-letter station codes
    The fields in the dict keyed to scan name contain the stations, scan time, RA,dec of the source
    The fields keyed to station contain the X,Y,Z coordinates of the station (in ECEF)
    
    '''
    
    calc_dict = {}
    calc_dict['stations'] = {}
    
    ii=0
    for file in os.listdir(difx_directory):
        if file.endswith('.calc'):

            ii+=1
            station = None
            scan_stations = ''
            IFF = open(os.path.join(difx_directory, file))
            
            # loop through the lines in the calc file and gather the data we need
            for line in IFF.readlines():
                # parse the scan time
                if line.startswith('START MJD'):
                    MJD = float(line.split(':')[1])
                if line.startswith('START YEAR'):
                    YY = int(line.split(':')[1])
                if line.startswith('START MONTH'):
                    MM = int(line.split(':')[1])
                if line.startswith('START DAY'):
                    DD = int(line.split(':')[1])
                if line.startswith('START HOUR'):
                    hh = int(line.split(':')[1])
                if line.startswith('START MINUTE'):
                    mm = int(line.split(':')[1])
                if line.startswith('START SECOND'):
                    ss = int(line.split(':')[1])

                # parse the station coords
                if line.startswith('TELESCOPE'):
                    temp = line.split()
                    if temp[2]=='NAME:':
                        tel_name = stations_2to1[temp[-1].replace(' ','')]
                        scan_stations += tel_name

                        # if we haven't collected the coordinates for this station, initialize a dict
                        # if the station dict is already there, 'station' will not be changed from None, and the 
                        # subsequent lines will be skipped
                        if tel_name not in calc_dict['stations']:
                            calc_dict['stations'][tel_name] = {}
                            station = tel_name
                    
                    if temp[2]=='X' and station is not None:
                        calc_dict['stations'][tel_name]['X'] = float(temp[-1])
                    if temp[2]=='Y' and station is not None:
                        calc_dict['stations'][tel_name]['Y'] = float(temp[-1])
                    if temp[2]=='Z' and station is not None:
                        calc_dict['stations'][tel_name]['Z'] = float(temp[-1])
                        #print(calc_dict['stations'], station, tel_name, temp[2])

                # parse the scan name
                if line.startswith('SCAN'):
                    temp = line.split()
                    if temp[2]=='IDENTIFIER:':
                        SCAN_NAME = temp[-1].replace(' ','')

                # parse the source name & coordinates
                if line.startswith('SOURCE'):
                    temp = line.split()
                    if temp[2]=='NAME:':
                        SOURCE_NAME = temp[-1].replace(' ','')
                    if temp[2]=='RA:':
                        RA = float(temp[-1])
                    if temp[2]=='DEC:':
                        DEC = float(temp[-1])


            #print(SCAN_NAME, scan_stations, SOURCE_NAME, YY, MM, DD, hh, mm)

            # build one scan entry per calc file
            calc_dict[SCAN_NAME] = {}
            calc_dict[SCAN_NAME]['scan_stations'] = scan_stations
            calc_dict[SCAN_NAME]['source_name'] = SOURCE_NAME
            calc_dict[SCAN_NAME]['ra'] = RA
            calc_dict[SCAN_NAME]['dec'] = DEC
            
            calc_dict[SCAN_NAME]['mjd'] = MJD
            calc_dict[SCAN_NAME]['year'] = YY
            calc_dict[SCAN_NAME]['month'] = MM
            calc_dict[SCAN_NAME]['day'] = DD
            calc_dict[SCAN_NAME]['hour'] = hh
            calc_dict[SCAN_NAME]['minute'] = mm
            calc_dict[SCAN_NAME]['sec'] = ss

        
    print('Collected',ii,'scans.')

    return calc_dict

				
				

def download_ionex_map(ionex_data_directory, YYYY, DOY, ion_center='jpl', num='0', email='dhoak@mit.edu'):
    '''
    Function to download an IONEX map file from CDDIS
    
    See https://cddis.nasa.gov/Data_and_Derived_Products/GNSS/atmospheric_products.html
    for descriptions of file names and locations
    
    'directory' is the absolute path to the data directory where the file will be saved
    At present, zipped file is downloaded to working directory, uncompressed, then moved.
    
    'YYYY' and 'DOY' are strings (4-digit year and day-of-year)
    
    'ion_center' is the analysis center name (jpl, esa, igs, etc)
    
    'num' a string, the file number of the day (typically zero)
    
    returns the path to the file
    
    It would be better to do this with the python requests module, but that does not support ftp?
    The FTP_TLS command from ftplib returns an SSL version error...not sure why
    For now we use a system call to curl.
    
    '''

    #ftp_path = 'ftp://gdc.cddis.eosdis.nasa.gov/gps/products/ionex/'
    ftp_path = 'ftp://gdc.cddis.eosdis.nasa.gov/gnss/products/ionex/'
    
    # The CDDIS IONEX files had their filename format changed in late 2023/early 2024
    # As of June 2024, the c1p and c2p predict files are still available with the old
    # filename format

    # Rapid and final solutions from JPL, ESA, IGS, CAS, and COD are available with different
    # latencies with the new filename format.

    # The file format should be the same...
    
    if ion_center=='c1p' or ion_center=='c2p' or int(YYYY)<2024:
        print('Using old filename format for IONEX file.')
        file_name = ion_center + 'g' + DOY+num + '.' + YYYY[2:4] + 'i.Z'
        unzipped_fname = ion_center + 'g' + DOY+num + '.' + YYYY[2:4] + 'i'
        file_path = os.path.join(ftp_path, YYYY, DOY, file_name)
        #print(file_path)
        
    elif ion_center=='jpl' or ion_center=='esa' or ion_center=='igs' or ion_center=='upc' or ion_center=='cod' and int(YYYY)>2023:
        print('Using new filename format for IONEX file.')

        file_name = ion_center.upper() + '0OPSFIN_' + YYYY + DOY + '0000_01D_02H_GIM.INX.gz'
        unzipped_fname = ion_center.upper() + '0OPSFIN_' + YYYY + DOY + '0000_01D_02H_GIM.INX'
        file_path = os.path.join(ftp_path, YYYY, DOY, file_name)
        #print(file_path)

    #elif ion_center=='cas'
        
    else:
        print('Analysis center not supported!  Please choose one of c1p, c2p, jpl, esa, upc, cod, or igs.')
        sys.exit()
        
    # ESA0OPSFIN_20240660000_01D_02H_GIM.INX.gz
    # JPL0OPSFIN_20241430000_01D_02H_GIM.INX.gz
    # JPL0OPSFIN_20240660000_01D_02H_GIM.INX.gz
    # IGS0OPSFIN_20240660000_01D_02H_GIM.INX.gz
    
    # check that the file doesn't already exist
    if not os.path.exists(os.path.join(ionex_data_directory,unzipped_fname)):
        
        print('Downloading IONEX map for', YYYY, DOY)
        #print(file_name[0:-3])
        sys_call = 'curl -u anonymous:'+email+' -O --ftp-ssl '+file_path
        
        os.system(sys_call)
        os.system('gunzip '+file_name)
        os.system('mv '+unzipped_fname+' '+ionex_data_directory)
        
    #else:
    #	print('IONEX map file for '+YYYY+' '+DOY+' already exists, no need to download.')
	
    return os.path.join(ionex_data_directory,unzipped_fname)




def calc_scan_TEC(calc_dict, scan, ionex_file, station, LFACT=1.0):
    """ 
    Compute the TEC content in the line of sight of each antenna for a 
    given scan. The estimates are taken from IONEX maps. 
    
    calc_dict is a dictionary containing parameters from the DiFX calc files
    scan is a scan name (a key for the calc_dict)
    ionex_file is the path to the IONEX map file for this scan
    station is a one-letter station code
    
    LFACT: Sun follow-up interpolation factor.
    
    Returns: TEPATH (float), line-of-sight TEC in TECU for source from station at time of scan
    
    This function is borrowed from PolConvert, Ivan Marti-Vidal, University of Valencia (Spain)
    """

    # Set some constants:
    REARTH = 6371.e3
    SPEED_OF_LIGHT = 2.99458792e8
    

    MJD = calc_dict[scan]['mjd']
    YY = calc_dict[scan]['year']
    MM = calc_dict[scan]['month']
    DD = calc_dict[scan]['day']
    hh = calc_dict[scan]['hour']
    mm = calc_dict[scan]['minute']
    ss = calc_dict[scan]['sec']
    source_name = calc_dict[scan]['source_name']
    RA = calc_dict[scan]['ra']
    DEC = calc_dict[scan]['dec']
    
    scan_time  = dt.datetime(YY, MM, DD, hh, mm, ss)
    
    d1 = dt.date(YY,MM,DD)	
    
    # Parse IONEX file to get the maps:
    EXPO = -1.0
    SCALING = 1.0
    IFF = open(ionex_file,'r')
    lines = IFF.readlines()
    TIMES = []
    for li,line in enumerate(lines):
        if line[60:79]==  '# OF MAPS IN FILE  ':
            NMAP = int(line.split()[0])
        elif line[60:79]=='MAP DIMENSION      ':
            NDIM = int(line.split()[0])
        elif line[60:79]=='HGT1 / HGT2 / DHGT ':
            HGT1,HGT2,DHGT = map(float,line.split()[:3])
            HGT1 *= 1.e3 ; HGT2 *= 1.e3 ; DHGT *= 1.e3 ;
        elif line[60:79]=='LAT1 / LAT2 / DLAT ':
            LAT1,LAT2,DLAT = map(float,line.split()[:3])
        elif line[60:79]=='LON1 / LON2 / DLON ':
            LON1,LON2,DLON = map(float,line.split()[:3])
        elif line[60:79]=='EXPONENT           ':
            EXPO = float(line.split()[0])
        elif line[60:79]=='START OF TEC MAP   ':
            hour = list(map(int,lines[li+1].split()[:6]))
            d2 = dt.date(hour[0],hour[1],hour[2])
            dday = (d2-d1).days
            mhour = hour[3]+hour[4]/60.+hour[5]/3600.+24.*dday
            TIMES.append([int(line.split()[0]),mhour,li+2])
        elif 'COMMENT' in line[60:79] and 'TECU;' in line.split():
            temp = line.split()
            SCALING = float(temp[temp.index('TECU;')-1])
    IFF.close()



    # sort the map times
    TS = np.argsort([ti[1] for ti in TIMES])
    
    # Get the map times bracketing the scan:
    found = False
    for ti in range(len(TIMES)-1):
        #print(ti, TIMES[TS[ti]][1], hh+mm/60.)
        if TIMES[TS[ti]][1]<=hh+mm/60. and TIMES[TS[ti+1]][1]>hh+mm/60.:
            found = True
            break
    if not found: 
        raise Exception("ERROR! IONEX MAP DOES NOT CONTAIN OBSERVING TIME!", scan, ionex_file)

    # Interpolation times:
    DT1 = (hh+mm/60.) - TIMES[TS[ti]][1]
    DT2 = TIMES[TS[ti+1]][1]-(hh+mm/60.)
    
    ## Prepare memory for maps:
    NLAT = int((LAT2-LAT1)/DLAT) ; NLON = int((LON2-LON1)/DLON)
    MAP1 = np.zeros((NLAT,NLON+1),dtype=np.float32)
    MAP2 = np.zeros((NLAT,NLON+1),dtype=np.float32)
    
    LATGRID = np.linspace(LAT2,LAT1,NLAT)
    LONGRID = np.linspace(LON1,LON2,NLON+1)
    
    # Read maps:
    rLat = 0
    lread = TIMES[TS[ti]][2]
    for i in range(NLAT):
        nlonRead = 0
        lread += 1
        while nlonRead<NLON:
            line = list(map(float,lines[lread][:-1].split()))
            nlon = len(line)
            MAP1[i,nlonRead:nlonRead+nlon]=line
            nlonRead += nlon
            lread += 1
            
    rLat = 0
    lread = TIMES[TS[ti+1]][2]
    for i in range(NLAT):
        nlonRead = 0
        lread += 1
        while nlonRead<NLON:
            line = list(map(float,lines[lread][:-1].split()))
            nlon = len(line)
            MAP2[i,nlonRead:nlonRead+nlon]=line
            nlonRead += nlon
            lread += 1

    # Build map Interpolations:
    MAP1 *= SCALING
    MAP2 *= SCALING
    MapInterp1 = spint.RectBivariateSpline(LATGRID,LONGRID,MAP1[::-1,:],kx=1,ky=1)
    MapInterp2 = spint.RectBivariateSpline(LATGRID,LONGRID,MAP2[::-1,:],kx=1,ky=1)
    
    ## Get GMST:
    t = (MJD-51544.0)/36525.
    Hh = (MJD - np.floor(MJD))
    GMsec = 24110.54841 + 8640184.812866*t + 0.093104*t*t - 0.0000062*t*t*t
    GMST = (GMsec/86400. + Hh)*2.*np.pi
    
    CosDec = np.cos(DEC)
    SinDec = np.sin(DEC)
    
    TECORR = {}
    TELCOORDS = {}
    
    
    ## Get Antenna pointing direction and intersection with Ionosphere:
    TNAM = station
    LAT = np.arctan2(calc_dict['stations'][station]['Z'],np.sqrt(calc_dict['stations'][station]['Y']**2.+calc_dict['stations'][station]['X']**2.))
    LON = np.arctan2(calc_dict['stations'][station]['Y'],calc_dict['stations'][station]['X'])
    
    TELCOORDS[TNAM] = [LAT*180./np.pi,LON*180./np.pi]
    HANG = (GMST - RA)%(2.*np.pi) + LON    
    ELEV = np.arcsin(SinDec*np.sin(LAT)+np.cos(LAT)*CosDec*np.cos(HANG))
    ZANG = np.pi/2. - ELEV
    
    if np.cos(ELEV)!=0.0:
        AZIM = np.arctan2(-CosDec*np.sin(HANG),np.cos(LAT)*SinDec - np.sin(LAT)*CosDec*np.cos(HANG))
    else:
        AZIM = 0.0
    if AZIM<0.0:
        AZIM += 2.*np.pi
        
    ZAION = np.arcsin(REARTH*np.sin(ZANG)/(REARTH+HGT1))
    THETA = ZANG - ZAION
    LATION = np.arcsin( np.sin(LAT)*np.cos(THETA)+np.cos(LAT)*np.sin(THETA)*np.cos(AZIM))
    DLATI = LATION - LAT
    
    SAZION = np.sin(AZIM)*np.cos(LAT)/np.cos(LATION)
    if SAZION >= 1.0: AZION = np.pi/2.
    elif SAZION <= -1.0: AZION = -np.pi/2.
    else: AZION = np.arcsin(SAZION)
    
    DLONG = np.arcsin(np.sin(AZIM)*np.sin(THETA)/np.cos(LATION))
    
    if np.abs(AZIM) > np.pi/2.:
        if AZION > 0.0:
            AZION = np.pi - AZION
        else:
            AZION = -np.pi - AZION

    IONLON = LON + DLONG
    IONLAT = LAT + DLATI
    
    ## Apply Ionosphere rotation:
    TLONG1 = IONLON*180./np.pi + 360.0/24.*DT1*LFACT
    TLONG2 = IONLON*180./np.pi - 360.0/24.*DT2*LFACT
    
    TLAT = IONLAT*180./np.pi
    
    if TLONG1 < -180.:  TLONG1 += 360.
    elif TLONG1 > 180.: TLONG1 -= 360.
    
    if TLONG2 < -180.:  TLONG2 += 360.
    elif TLONG2 > 180.: TLONG2 -= 360.
    
    ## Estimate TEC:
    TEC1 = MapInterp1(TLAT,TLONG1)[0][0]
    TEC2 = MapInterp2(TLAT,TLONG2)[0][0]
    
    TEC = (DT2*TEC1 + DT1*TEC2)/(DT1+DT2)
    
    TEPATH = TEC/np.cos(ZAION)
    
    return TEPATH







def main():

    parser = argparse.ArgumentParser(
        prog='get_ionex_dtec_bounds.py', \
        description='''script to generate JSON file of dTEC ranges for a VGOS experiment''')
    
    parser.add_argument('difx_directory', help='DiFX experiment directory containing per-scan calc files')
    parser.add_argument('ionex_directory', help='directory to store the IONEX data files')
    parser.add_argument('outfile', help='output filename for the JSON table')
    parser.add_argument('-a', '--analysis_center', dest='analysis_center', help='IONEX analysis center (c1p, c2p, cor, jpr, jpl, etc; default is jpl)', default='esa')
    parser.add_argument('-c', '--codes_file', dest='codes_file', help='file matching 1-letter to 2-letter station codes; same file used in difx2mark4', default=None)
    
    
    args = parser.parse_args()
    #print('args: ', args)

    stations_2to1 = build_station_code_dict(args.codes_file)

    # build the dictionary of calc parameters (station coordinates, source rad/dec, scan time)
    print('Collecting information from the calc files')
    calc_dict = build_calc_dict(args.difx_directory, stations_2to1)
    
    
    ### build the dict of TEC values for each station and scan
    vgos_TEC_dict = {}
    for scan in sorted(calc_dict.keys()):
        if 'year' in calc_dict[scan]: # need to distinguish between keys for scans and the station keys with coordinates
                
            d0 = dt.date(calc_dict[scan]['year'],1,1)
            d1 = dt.date(calc_dict[scan]['year'],calc_dict[scan]['month'],calc_dict[scan]['day'])
            dayofyear = int((d1-d0).days+1)
            YYYY = str(calc_dict[scan]['year'])
            DOY = "{0:03}".format(dayofyear)
            
            # figure out what days are covered by the experiment and get the ionex files
            ionex_filename =  download_ionex_map(args.ionex_directory, YYYY, DOY, ion_center=args.analysis_center)
            
            if scan not in vgos_TEC_dict:
                vgos_TEC_dict[scan] = {}
                
            print('Calculating station TECs for scan',scan)

            for sta in calc_dict[scan]['scan_stations']:

                if args.analysis_center=='jpl':
                    # the JPL IONEX predicts are not in agreement with VLBI results as of Jan1 2025
                    # it looks like they are too large by 10x...but there are still large errors present
                    sta_TEC = calc_scan_TEC(calc_dict, scan, ionex_filename, sta) / 10.
                else:
                    sta_TEC = calc_scan_TEC(calc_dict, scan, ionex_filename, sta)
                    
                #print(scan, sta, np.round(sta_TEC,3))
                vgos_TEC_dict[scan][sta] = round(np.round(sta_TEC,3),3)
			

            scan_time = (calc_dict[scan]['year'],
                         calc_dict[scan]['month'],
                         calc_dict[scan]['day'],
                         calc_dict[scan]['hour'],
                         calc_dict[scan]['minute'],
                         calc_dict[scan]['sec'])

            vgos_TEC_dict[scan]['scan_time'] = scan_time


                
    # done; save to file
    with open(args.outfile, 'w') as fp:
        json.dump(vgos_TEC_dict, fp)


	
	

if __name__ == '__main__':          # official entry point
    main()
    sys.exit(0)
				

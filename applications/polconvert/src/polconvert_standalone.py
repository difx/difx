# Copyright (c) Ivan Marti-Vidal 2012-2023
#               EU ALMA Regional Center. Nordic node.
#               Universitat de Valencia (Spain)
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>,
# or write to the Free Software Foundation, Inc.,
# 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# a. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# b. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the
#    distribution.
# c. Neither the name of the author nor the names of contributors may
#    be used to endorse or promote products derived from this software
#    without specific prior written permission.
#
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#
#

from __future__ import absolute_import
from __future__ import print_function

__version__ = "2.0.7  "  # 7 characters
date = "Aug 14, 2023"


################
# Import all necessary modules.

########
# Execute twice, to avoid the silly (and harmless)
# error regarding the different API versions of
# numpy between the local system and CASA:
import os, sys

try:
    mypath = os.path.dirname(__file__)
    sys.path.append(mypath)
    import _PolConvert as PC

    goodclib = True
    print("\nC++ shared library loaded successfully (first try)\n")
except:
    goodclib = False
    print("\n There has been an error related to the numpy")
    print(" API version used by CASA. This is related to PolConvert")
    print(" (which uses the API version of your system) and should")
    print(" be *harmless*.\n")


if not goodclib:
    try:
        import _PolConvert as PC

        goodclib = True
        print("\nC++ shared library loaded successfully (2nd try)\n")
    except:
        goodclib = False
        print("\nNo, the shared library did not load successfully\n")
#############

if not goodclib:
    try:
        import _PolConvert as PC

        goodclib = True
        print("\nC++ shared library loaded successfully (3rd try)\n")
    except:
        goodclib = False
        print("\nNo, the shared library still did not load successfully\n")
#############
### ... and we could keep on trying! (with an infinite loop! XD ).


import os, sys, shutil, re
import gc
import time
import struct as stk
import scipy.optimize as spopt
import scipy.interpolate as spint
import numpy as np
import pylab as pl
import datetime as dt
import sys
import pickle as pk


def polconvert(
    IDI="",
    OUTPUTIDI="",
    DiFXinput="",
    DiFXcalc="",
    doIF=[],
    linAntIdx=[],
    Range=[],
    XYadd={},
    XYdel={},
    XYratio={},
    usePcal={},
    swapXY=[],
    swapRL=False,
    feedRotation=[],
    correctParangle=False,
    IDI_conjugated=False,
    plotIF=[],
    plotRange=[],
    plotAnt="",
    excludeAnts=[],
    excludeBaselines=[],
    doSolve=-1,
    solint=[1, 1],
    doTest=True,
    npix=-1,
    solveAmp=True,
    solveMethod="COBYLA",
    calstokes=[1.0, 0.0, 0.0, 0.0],
    calfield=-1,
    plotSuffix="",
    pcalSuffix="",
    ALMAstuff=[],
    saveArgs=False,
    amp_norm=0.0,
    IFoffset=0,
    AC_MedianWindow=0,
    XYpcalMode="bandpass",
    UVTaper=1.e9,
    useRates = False,
    useDelays = False,
    mounts = {}
):

    """POLCONVERT - STANDALONE VERSION 2.0.7

     Similar parameters as the method defined in polconvert_CASA. The parameters specific
     of this task (i.e., polconvert_standalone::polconvert) may be useful to parallelize polconvert:

        plotSuffix: Suffix to add to the names of the plot files.
        pcalSuffix: Suffix to add to the names of the original pcal files in the difx directories.
        ALMAstuff: List of ALMA-specific parameters passed by polconvert_CASA::polconvert.

    Other new parameters are:

       IFoffset: treats the IF with index "i" as the same IF with index "i+IFoffset". This is
                 useful to process correctly the autocorrelations with multi-file has been
                 used in the observations.

       AC_MedianWindow: Size (in channels) of a median window filter to be applied to the
                        autocorrelations. This is useful if we want to filter out phasecal peaks
                        (for a better fringe normalization).

       XYpcalModel: Controls how the X-Y pcal differences are interpolated in frequency.
                    It can be either "bandpass" (default) or "multitone" (similar to fourfit's).
      
       UVTaper:  Apply a UVtaper (in meters) to the X-pol gain estimator (to avoid too high 
                 cross-polarization fringes).

       useRates:  If True, the pol-wise weighted rate average will be use to increase 
                  (hopefully) the coherence time of the data when the x-pol gains are 
                  being estimated.

       useDelays: If True, the pol-wise delay average will be used to increase (hopefully)
                  the SNR over wider frequency averages.
      
       mounts:  Antenna mounts (for SWIN case), given as a dictionary. The keywords are 
                the antenna codes and the elements are the mount types. The default antenna
                mount is alt-azimuth. Recognized mounts are:

                AZ (alt-az), EQ (equatorial), OB (orbit), XY (X-Y), NR (Nasmyth right), NL (Nasmyth left)

                Example: if antenna Yebes-40m (code YB) has Nasmyth left, and all the other antennas
                have alt-az mounts, then: mounts = {'YB':'NL'}

    """

    if saveArgs:

        ARGS = {
            "IDI": IDI,
            "OUTPUTIDI": OUTPUTIDI,
            "DiFXinput": DiFXinput,
            "DiFXcalc": DiFXcalc,
            "doIF": doIF,
            "linAntIdx": linAntIdx,
            "Range": Range,
            "XYadd": XYadd,
            "XYdel": XYdel,
            "XYratio": XYratio,
            "usePcal": usePcal,
            "swapXY": swapXY,
            "swapRL": swapRL,
            "feedRotation": feedRotation,
            "correctParangle": correctParangle,
            "IDI_conjugated": IDI_conjugated,
            "plotIF": plotIF,
            "plotRange": plotRange,
            "plotAnt": plotAnt,
            "excludeAnts": excludeAnts,
            "excludeBaselines": excludeBaselines,
            "doSolve": doSolve,
            "solint": solint,
            "doTest": doTest,
            "npix": npix,
            "amp_norm": amp_norm,
            "solveAmp": solveAmp,
            "solveMethod": solveMethod,
            "calstokes": calstokes,
            "calfield": calfield,
            "ALMAstuff": ALMAstuff,
            "IFoffset": IFoffset,
            "AC_MedianWindow": AC_MedianWindow,
            "XYpcalMode": XYpcalMode,
            "UVTaper": UVTaper,
            "useRates":useRates,
            "useDelays":useDelays,
            "mounts":mounts
        }

        OFF = open("PolConvert_standalone.last", "wb")
        pk.dump(ARGS, OFF)
        OFF.close()

    ## Set to non-ALMA:
    if len(ALMAstuff) == 0:
        PC.setPCMode(0)

    #  amp_norm=0.0

    logName = "PolConvert%s.log" % plotSuffix


## Codes for antenna mounts:
    MntCodes = {'AZ':0, 'EQ':1, 'OB':2, 'XY':3, 'NR':4, 'NL':5}


    ############################################

    # this turns into the verbosity argument of _PolConvert.so
    print("Entered polconvert_standalone::polconvert()")
    DEBUG = False

    if "POLCONVERTDEBUG" in os.environ:
        if os.environ["POLCONVERTDEBUG"] == "True":
            DEBUG = True
        else:
            DEBUG = False
    print("DEBUG setting is " + str(DEBUG))
    print("__name__ is " + __name__)

    # Auxiliary function: print error and raise exception:
    def printError(msg):
        print(msg, "\n")
        lfile = open(logName, "a")
        print("\n" + msg + "\n", file=lfile)
        lfile.close()
        sys.stdout.flush()
        raise Exception(msg)

    # Auxiliary function: print message (terminal and log):
    def printMsg(msg, doterm=True, dolog=True):
        if doterm:
            print(msg)
        if dolog:
            lfile = open(logName, "a")
            print(msg, file=lfile)
            lfile.close()

    # Auxiliary function: derive job label from DiFXinput
    def jobLabel(inputname):
        label = inputname
        try:
            label = re.sub(".input", "", os.path.basename(label))
        except:
            pass
        return label

    tic = time.time()

    greet = """ 
  ##########################################################################'
  # POLCONVERT --  version.                                                #'
  #       Please, add the POLCONVERT reference to your publications:       #'
  #                                                                        #'
  #          Marti-Vidal, Roy, Conway & Zensus 2016, A&A, 587, 143         #'
  #                                                                        #'
  ##########################################################################'
  """

    greetings = re.sub("version", __version__, greet)
    printMsg(greetings, dolog=False)
    printMsg("\n\nPOLCONVERT - VERSION %s" % __version__, doterm=False)

    #########################################
    # DO SOME SANITY CHECKS OF PARAMETERS

    try:
        doSolve = float(doSolve)
    except:
        printError("ERROR! doSolve should be a float!")

    scipyMethods = ["COBYLA", "Nelder-Mead", "Powell", "BFGS", "Newton-CG", "SLSQP"]
    allMethods = scipyMethods + ["gradient", "Levenberg-Marquardt"]

    if solveMethod not in allMethods:
        printError("ERROR! 'solveMethod' must be any of: %s" % (", ".join(allMethods)))

    if type(calstokes) is not list:
        printError("ERROR! Wrong calstokes!")
    elif len(calstokes) != 4 and doSolve > 0.0:
        printError(
            "ERROR! calstokes should have 4 elements, not %d (%s)!"
            % (len(calstokes), str(calstokes))
        )
    for ii, item in enumerate(calstokes):
        try:
            calstokes[ii] = float(item)
        except:
            printError(
                "ERROR! calstokes should only have float elements; got %s!"
                % str(type(item))
            )

    Stokes = list(calstokes)

    if doSolve >= 0.0 and (
        Stokes[0] <= 0.0
        or Stokes[0] < np.sqrt(Stokes[1] ** 2.0 + Stokes[2] ** 2.0 + Stokes[3] ** 2.0)
    ):
        printError("ERROR! Inconsistent Stokes parameters!")

    # Will implement solveQU soon!
    solveQU = False
    #  calfield = -1

    if type(solveQU) is not bool:
        printError("ERROR! Wrong solveQU!")
    if type(calfield) is not int:
        printError("ERROR! Wrong calfield!")

    doConj = True
    if type(IDI_conjugated) is not bool:
        printError("ERROR! IDI_cojugated should be a boolean!")
    else:
        doConj = IDI_conjugated

    if type(swapRL) is not bool:
        printError("ERROR! swapRL should be a boolean!")

    if type(doIF) is not list:
        printError("ERROR! doIF should be a list of integers!")
    else:
        for elem in doIF:
            if type(elem) is not int:
                printError("ERROR! doIF should be a list of integers!")
            if elem == 0:
                printError(
                    "ERROR! IF numbers are given in AIPS (and FITS-IDI) convention\n(i.e., starting from 1; not from 0).\n"
                )

    if type(doTest) is not bool:
        printError("ERROR! doTest should be a boolean!")

    if doTest:
        printMsg("Will only compute, but not update the output file(s)", doterm=False)

    if type(linAntIdx) is not list:
        printError("ERROR! linAntIdx should be a list of antenna indices or names!")
    for elem in linAntIdx:
        if type(elem) not in [int, str]:
            printError("ERROR! linAntIdx should be a list of antenna indices or names!")

    if type(solint) is not list or (len(solint) not in [2, 3]):
        printError(
            "ERROR! solint (%s) must be a list of two/three numbers!" % str(solint)
        )
    else:
        try:
            solint[0] = int(solint[0])
            solint[1] = int(solint[1])
        except:
            printError(
                "ERROR! solint (%s) must be a list of two/three numbers!" % str(solint)
            )

    if len(solint) == 3:
        solint[2] = float(solint[2])
    else:  # Default dt
        solint.append(100.0)

    nALMA = len(linAntIdx)

    if not os.path.exists(IDI):
        printError("ERROR! IDI file (or SWIN folder) does not exist!")

    if type(OUTPUTIDI) is not str:
        printError("ERROR! OUTPUTIDI should be a string!")

    if type(plotIF) is int:
        if plotIF > 0:
            plotIF = [plotIF]
        else:
            plotIF = []
    for pli in plotIF:
        if type(pli) is not int:
            printError("ERROR! plotIF should be an integer or a list of integers!")

    for pli in plotIF:
        if pli not in doIF:
            printError("ERROR! Only converted IFs can be plotted!")

    if type(usePcal) is not dict:
        printError("Invalid format for usePcal! Should be a dictionary!\n")

    pcant = 0
    for k in usePcal.keys():
        if type(usePcal[k]) is not bool:
            printError("The elements of usePcal must be booleans!\n")
        elif usePcal[k]:
            pcant += 1
    isPcalUsed = pcant > 0

    if len(swapXY) == 0:
        swapXY = [False for swp in range(nALMA)]

    if len(swapXY) != nALMA:
        printError(
            "Invalid format for swapXY!\n Should be a list of booleans, as large as the number of linear-polarization VLBI stations!"
        )
    for sxy in swapXY:
        if type(sxy) is not bool:
            printError(
                "Invalid format for swapXY!\n Should be a list of booleans, as large as the number of linear-polarization VLBI stations!"
            )

    if len(Range) not in [0, 8]:
        printError(
            "Invalid format for Range! Should be either an empty list or a list of 8 integers!"
        )
    for rr in Range:
        if type(rr) is not int:
            printError(
                "Invalid format for Range! Should be either an empty list or a list of 8 integers!"
            )

    if len(plotRange) not in [0, 8]:
        printError(
            "Invalid format for Range! Should be either an empty list or a list of 8 integers!"
        )
    for rr in plotRange:
        if type(rr) is not int:
            printError(
                "Invalid format for Range! Should be either an empty list or a list of 8 integers!"
            )

    #########################################

    #######
    # CHECK IF THIS IS A FITS-IDI FILE OR A SWIN DIR.:
    if os.path.isdir(IDI):
        isSWIN = True
        printMsg("\n\nYou have asked to convert a set of SWIN files.")

        if len(DiFXinput) == 0:
            DiFXinput = IDI[:-4] + "input"
        if len(DiFXcalc) == 0:
            DiFXcalc = IDI[:-4] + "calc"

        if not os.path.exists(DiFXinput) or not os.path.isfile(DiFXinput):
            printError("Invalid DiFX input file! %s" % DiFXinput)

        printMsg("Opening calc file... %s" % DiFXcalc)
        try:
            printMsg('Opening "%s"' % (DiFXcalc))
            antcoords = []
            calcsoucoords = [[], []]
            antmounts = []
            antcodes = []
            calc = open(DiFXcalc)
            lines = calc.readlines()
            calc.close()
            printMsg("Read %d lines from %s" % (len(lines), DiFXcalc))
            for ii, line in enumerate(lines):
                if "TELESCOPE" in line and "NAME" in line:
                    antcodes.append(line.split()[-1])
                if "TELESCOPE" in line and "X (m):" in line:
                    antcoords.append(list(map(float, [ll.split()[-1] for ll in lines[ii : ii + 3]])))
                    printMsg(
                        "TELESCOPE %s AT X: %.3f ; Y: %.3f ; Z: %.3f"
                        % tuple([antcodes[-1]] + antcoords[-1])
                    )
                    # DEFAULT MOUNTS ARE ALT-AZ:
                    antmounts.append(0)

                if "SOURCE" in line and " NAME: " in line:
                    SNAM = line.split()[-1]
                    calcsoucoords[0].append(float(lines[ii + 1].split()[-1]))
                    calcsoucoords[1].append(float(lines[ii + 2].split()[-1]))
                    printMsg(
                        "SOURCE %s AT RA: %.8f rad, DEC: %.8f rad"
                        % (SNAM, calcsoucoords[0][-1], calcsoucoords[1][-1])
                    )

            antcoords = np.array(antcoords, dtype=np.float, order="C")
            antmounts = np.array(antmounts, order="C")
            calcsoucoords[0] = np.array(calcsoucoords[0], dtype=np.float, order="C")
            calcsoucoords[1] = np.array(calcsoucoords[1], dtype=np.float, order="C")
            printMsg("done parsing calc")
        except Exception as ex:
            printMsg(str(ex))
            printMsg(
                (
                    "WARNING! Invalid DiFX calc file '%s'!\n"
                    + "PolConvert may not calibrate properly."
                )
                % DiFXcalc
            )
        if len(antmounts) == 0:
            printError("ERROR! NO ANTENNAS FOUND IN CALC FILE!")
        else:
            printMsg("There are %i antennas." % len(antmounts))


## Read antenna mounts from dictionary:
        for antName in mounts.keys():
            if antName in antcodes:
                if mounts[antName] not in MntCodes.keys():
                    printError("ERROR! Mount %s not recognized!"%mounts[antName])
                else:
                    code = mounts[antName]
                    mnt = MntCodes[code]
                    antmounts[antcodes.index(antName)] = int(mnt)
                    printMsg("Setting mount %s (%i) to antenna %s"%(code,mnt,antName))


    elif os.path.isfile(IDI):
        isSWIN = False

        # Currently, PCALs are not supported for FITS-IDI:
        isPcalUsed = False

        printMsg("\n\nYou have asked to convert a FITS-IDI file.")
        printMsg("Reading array geometry...")
        try:
            from astropy.io import fits as pf

            ffile = pf.open(IDI)
            for ii, group in enumerate(ffile):
                if group.name == "ARRAY_GEOMETRY":
                    grarr = ii
                elif group.name == "SOURCE":
                    grsou = ii

            raappUnit = (
                ffile[grsou]
                .data.columns[
                    [coln.name for coln in ffile[grsou].data.columns].index("RAAPP")
                ]
                .unit
            )
            decappUnit = (
                ffile[grsou]
                .data.columns[
                    [coln.name for coln in ffile[grsou].data.columns].index("DECAPP")
                ]
                .unit
            )

            soucoords = [
                np.array(ffile[grsou].data["RAAPP"], dtype=np.float, order="C"),
                np.array(ffile[grsou].data["DECAPP"], dtype=np.float, order="C"),
            ]

            if raappUnit == "DEGREES":
                soucoords[0] *= np.pi / 180.0

            if decappUnit == "DEGREES":
                soucoords[1] *= np.pi / 180.0

            antcodes = [ff[:2] for ff in ffile["ANTENNA"].data["ANNAME"]]

            print("ANTENNA NAMES: ")
            print(",".join(antcodes))

            ffile.close()

            # THESE LINES FAIL IF ORBPARM IS PRESENT IN ARRAY GEOMETRY!
            import _getAntInfo as gA

            success = gA.getAntInfo(IDI)
            if success != 0:
                printError("ERROR GETTING FITS-IDI METADATA! ERR: %i" % success)
            else:
                antcoords = gA.getCoords()
                antmounts = gA.getMounts()
        # FIXME: need a way to find antenna codes if above fails:
        #        antcodes = ['%02i'%i for i in range(1,len(antmounts)+1)]
        except:
            printMsg(
                "WARNING! This FITS-IDI file has missing information!\nPolConvert may not calibrate properly."
            )
    else:
        printError("Invalid input data!")

    ######

    ######
    # IF THIS IS A SWIN DATASET, READ THE INPUT FILE INTO
    # A METADATA LIST:

    if isSWIN:
        printMsg("Reading the DiFX input file\n")
        ifile = open(DiFXinput)
        inputlines = ifile.readlines()
        ifile.close()
        FreqL = [inputlines.index(l) for l in inputlines if "FREQ TABLE" in l]

        # ONLY ONE FREQ TABLE IS ALLOWED:
        try:
            fr = FreqL[0]
            Nfreq = int(
                list(filter(lambda x: "FREQ ENTRIES" in x, inputlines[fr + 1 :]))[
                    0
                ].split()[-1]
            )
            Nr = list(range(Nfreq))
        except Exception as ex:
            printMsg(str(ex))
            printError("BAD input file!")

        FrInfo = {
            "FREQ (MHZ)": [0.0 for i in Nr],
            "BW (MHZ)": [0.0 for i in Nr],
            "SIDEBAND": ["U" for i in Nr],
            "NUM CHANNELS": [1 for i in Nr],
            "CHANS TO AVG": [1 for i in Nr],
            "OVERSAMPLE FAC.": [1 for i in Nr],
            "DECIMATION FAC.": [1 for i in Nr],
            "SIGN": [1.0 for i in Nr],
        }

        # READ METADATA FOR ALL FREQUENCIES:
        for entry in FrInfo.keys():
            for line in inputlines[fr + 1 :]:
                if entry in line:
                    index = int((line.split(":")[0]).split()[-1])
                    FrInfo[entry][index] = type(FrInfo[entry][0])(line.split(":")[-1])

        # SORT OUT THE CHANNEL FREQUENCIES:

        if len(doIF) == 0:
            doIF = list(range(1, len(Nr) + 1))

        metadata = []
        IFchan = 0
        for nu in Nr:
            nu0 = FrInfo["FREQ (MHZ)"][nu]
            bw = FrInfo["BW (MHZ)"][nu]
            nchan = FrInfo["NUM CHANNELS"][nu]
            # MAX. NUMBER OF CHANNELS:
            chav = FrInfo["CHANS TO AVG"][nu]
            if nu in doIF:
                IFchan = max([IFchan, int(nchan / chav)])
            sb = {True: 1.0, False: -1.0}[FrInfo["SIDEBAND"][nu] == "U"]
            FrInfo["SIGN"][nu] = float(sb)
            freqs = (
                nu0 + np.linspace((sb-1.0)/2.0, (sb+1.0)/2.0, nchan//chav, endpoint=False)*bw)*1.0e6
            if float(nchan // chav) != float(nchan / chav):
                printMsg("linspace check chan: %d / %d = %f"%(nchan, chav, float(nchan / chav)))
            freqs = (
                nu0 + np.linspace((sb-1.0)/2.0, (sb+1.0)/2.0, nchan//chav, endpoint=False)*bw)*1.0e6
            metadata.append(freqs)

    #####
    ######
    # IF THIS IS A FITS-IDI FILE, READ THE METADATA LIST:

    else:

        # READ FREQUENCY INFO:
        from astropy.io import fits as pf

        fitsf = pf.open(IDI)
        nu0 = fitsf["FREQUENCY"].header["REF_FREQ"]
        bw = fitsf["FREQUENCY"].header["CHAN_BW"]
        nch = fitsf["FREQUENCY"].header[
            "NO_CHAN"
        ]  # *fitsf['FREQUENCY'].header['NO_BAND']
        IFchan = nch
        Nr = fitsf["FREQUENCY"].header["NO_BAND"]
        sgn = {True: 1.0, False: -1.0}[bw > 0.0]
        FrInfo = {"FREQ (MHZ)": [], "BW (MHZ)": [], "SIGN": [], "NUM CHANNELS": []}
        if sgn:
            FrInfo["SIDEBAND"] = ["U" for i in range(Nr)]
        else:
            FrInfo["SIDEBAND"] = ["L" for i in range(Nr)]

        metadata = []
        for i in range(Nr):
            FrInfo["FREQ (MHZ)"] += [(nu0 + i * bw * nch) / 1.0e6]
            FrInfo["BW (MHZ)"] += [bw * nch / 1.0e6]
            FrInfo["SIGN"] += [sgn]
            FrInfo["NUM CHANNELS"] += [int(nch)]
            freqs = (
                nu0 + np.linspace((sgn - 1.0) / 2.0, (sgn + 1.0) / 2.0, nch, endpoint=False)*bw)
            metadata.append(freqs)

        FrInfo["CHANS TO AVG"] = [1 for i in range(Nr)]
        FrInfo["OVERSAMPLE FAC."] = [1 for i in range(Nr)]
        FrInfo["DECIMATION FAC."] = [1 for i in range(Nr)]

        if len(doIF) == 0:
            doIF = list(range(1, 1 + fitsf["FREQUENCY"].header["NO_BAND"]))

        fitsf.close()

    # ANTENNAS TO PARTICIPATE IN THE GAIN ESTIMATES:
    nTotAnt = len(antcoords)

    calAnts = []
    for exA in antcodes:
        if exA not in excludeAnts:
            calAnts.append(antcodes.index(exA) + 1)
        else:
            printMsg("Excluding antenna %s from the solution." % str(exA))

    ##z following section
    if type(plotAnt) is str and len(plotAnt) == 0:
        plotAnt = 1
    try:
        plotAnt = int(plotAnt)
    except:
        if plotAnt not in antcodes:
            printError("Reference antenna %s is not found in metadata among %s!" % (str(plotAnt), antcodes))
        else:
            plotAnt = antcodes.index(plotAnt) + 1

    if plotAnt in linAntIdx or antcodes[plotAnt - 1] in linAntIdx:
        printMsg(
            "WARNING: Plotting will involve autocorrelations. \nThis has not been fully tested!"
        )

    FlagBas1 = []
    FlagBas2 = []
    for fbi in excludeBaselines:
        printMsg("Excluding baseline %s from solution." % str(fbi))
        if fbi[0] in antcodes and fbi[1] in antcodes:
            FlagBas1.append(
                antcodes.index(fbi[0]) + 1
            )  ### = np.array([int(i[0]+1) for i in excludeBaselines])
            FlagBas2.append(
                antcodes.index(fbi[1]) + 1
            )  ### = np.array([int(i[1]+1) for i in excludeBaselines])
        else:
            printError(
                "Unknown antenna(s) %s and/or %s in excludeBaselines!\n"
                % (fbi[0], fbi[1])
            )

    FlagBas1 = np.array(FlagBas1, order="C", dtype=np.int32)
    FlagBas2 = np.array(FlagBas2, order="C", dtype=np.int32)

    if plotAnt not in calAnts:
        if doSolve >= 0:
            printError(
                "ERROR! plotAnt/Reference antenna is NOT in list of calibratable antennas!"
            )
        else:
            printMsg(
                "plotAnt (%d) is not in antenna list, so plots will be missing"
                % plotAnt
            )

    if type(feedRotation) is not list:
        printError("feedRotation must be a list of numbers")
    elif len(feedRotation) == 0:
        feedRot = np.zeros(nTotAnt, order="C", dtype=np.float)
    elif len(feedRotation) != nTotAnt:
        printError("feedRotation must have %i entries!" % nTotAnt)
    else:
        feedRot = np.pi / 180.0 * np.array(feedRotation, dtype=np.float, order="C")

    # Get the REAL number (and names) of linear-pol antennas in this dataset:
    nALMATrue = 0
    linAntIdxTrue = []
    linAntNamTrue = []
    OrigLinIdx = []
    for i, idd in enumerate(linAntIdx):
        if type(idd) is int and idd <= len(antcodes):
            nALMATrue += 1
            linAntIdxTrue.append(idd)
            linAntNamTrue.append(antcodes[idd - 1])
            OrigLinIdx.append(i)
        elif idd in antcodes:
            nALMATrue += 1
            linAntNamTrue.append(idd)
            linAntIdxTrue.append(antcodes.index(idd) + 1)
            OrigLinIdx.append(i)

    # Setting isLinear list of booleans:
    isLinear = []
    for antc in antcodes:
        isLinear.append(antc in linAntNamTrue)

    # Check that pcals are (or not) set for this antenna:
    for idd in linAntNamTrue:
        if idd not in usePcal.keys():
            printMsg(
                "WARNING: Antenna %s not in usePcal dictionary. Will NOT use Pcals for this antenna"
                % idd
            )
            usePcal[idd] = False

    printMsg("There are %i linear-polarization antennas in THIS dataset" % nALMATrue)

    # COMPUTE TIME RANGES:

    if len(plotRange) == 0:
        plRan = np.array([0.0, 0.0])
        plotFringe = False
    else:
        try:
            plRan = np.array(
                [
                    plotRange[0]
                    + plotRange[1]/24.0
                    + plotRange[2]/1440.0
                    + plotRange[3]/86400.0,
                    plotRange[4]
                    + plotRange[5]/24.0
                    + plotRange[6]/1440.0
                    + plotRange[7]/86400.0,
                ]
            )
            plotFringe = True
            if len(plotIF) == 0:
                plotIF = list(doIF)
        except:
            printError("Bad time range format for plotRange!")

    if len(Range) == 0:
        Ran = np.array([0.0, 1.0e20])
    else:
        try:
            Ran = np.array(
                [
                    Range[0] + Range[1] / 24.0 + Range[2] / 1440.0 + Range[3] / 86400.0,
                    Range[4] + Range[5] / 24.0 + Range[6] / 1440.0 + Range[7] / 86400.0,
                ]
            )
        except:
            printError("Bad time range format for Range!")

    #######
    # WARNING! UNCOMMENT THIS IF NOT DEBUGGING!
    if os.path.exists(OUTPUTIDI) and IDI != OUTPUTIDI:
        printMsg("Will REMOVE the existing OUTPUT file (or directory)!\n")
        printMsg("Copying IDI to OUTPUTIDI!\n")
        os.system("rm -rf %s" % OUTPUTIDI)
        os.system("cp -r %s %s" % (IDI, OUTPUTIDI))
    elif not os.path.exists(OUTPUTIDI):
        printMsg("Copying IDI to OUTPUTIDI!\n")
        os.system("cp -r %s %s" % (IDI, OUTPUTIDI))
    #
    #######

    if isSWIN:
        OUTPUT_UNSORT = []
        PHASECALS_UNSORT = []
        walk = [f for f in os.walk(OUTPUTIDI)]
        for subd in walk:
            ADD2OUTPUT = [
                os.path.join(subd[0], fi)
                for fi in filter(lambda x: x.startswith("DIFX_"), subd[2])
            ]
            OUTPUT_UNSORT += ADD2OUTPUT
            for difxfile in ADD2OUTPUT:
                prefix = (os.path.basename(difxfile).split(".")[0])[5:]
                PHASECALS_UNSORT.append(
                    [
                        os.path.join(subd[0], fi)
                        for fi in filter(
                            lambda x: x.startswith("PCAL_%s" % prefix), subd[2]
                        )
                    ]
                )

        if len(OUTPUT_UNSORT) == 0:
            printError("No *.difx files found in directory!")

        OUTPUT = []
        PHASECALS = []
        TOOUT = np.argsort(
            [
                float(os.path.basename(fi).split(".")[0].split("_")[-1])
                for fi in OUTPUT_UNSORT
            ]
        )
        for i in range(len(OUTPUT_UNSORT)):
            OUTPUT.append(OUTPUT_UNSORT[TOOUT[i]])
            PHASECALS.append(PHASECALS_UNSORT[TOOUT[i]])

        # Derive the days of observation of each difx file:
        mjd = np.zeros(len(OUTPUT))
        mjs = np.zeros(len(OUTPUT))
        for i, fi in enumerate(OUTPUT):
            mjd[i], mjs[i] = list(
                map(float, ((os.path.basename(fi)).split(".")[0]).split("_")[1:3])
            )
        mjd0 = np.min(mjd)
        mjp = Ran + mjd0
        metadata.append(mjd0)

        # Filter out files outside the computing time window:
        t0 = mjd + mjs / 86400.0
        i0 = np.logical_and(t0 <= mjp[1], t0 >= mjp[0])
        OUTPUT = [OUTPUT[i] for i in range(len(i0)) if i0[i]]

        # Get source coordinate for each file (if possible):
        #CALCS = ["%s.calc" % os.path.dirname(ci)[:-5] for ci in OUTPUT]  # NB the [:-5] fails when user suffix used like '.pcdifx' or '.difx_PC' like by EUVGOS_PY3/POLCONVERTER.py
        CALCS = ["%s.calc" % os.path.dirname(ci)[:os.path.dirname(ci).rfind('.')] for ci in OUTPUT]

        ##OBSOLETE! NOW, WE ARE CONCATENATING SWIN METADATA TO ENSURE CONSISTENCY!
        # There will be one source per SWIN file:
        #    soucoords = [np.ones(len(OUTPUT))*calcsoucoords[0][0], np.ones(len(OUTPUT))*calcsoucoords[0][0]]
        #    for sui,Fcalc in enumerate(CALCS):
        #     # print(sui,Fcalc)
        #      if os.path.exists(Fcalc):
        #        FCin = open(Fcalc)
        #        lines = FCin.readlines()
        #        FCin.close()
        #        for ii,line in enumerate(lines):
        #          if 'SOURCE' in line and ' NAME: ' in line:
        #            SNAM = line.split()[-1]
        #            soucoords[0][sui] = float(lines[ii+1].split()[-1])
        #            soucoords[1][sui] = float(lines[ii+2].split()[-1])
        #            printMsg('FOUND SOURCE %s AT RA: %.8f rad, DEC: %.8f rad'%(SNAM,soucoords[0][sui],soucoords[1][sui]))

        CALC2OPEN = {True: CALCS[0], False: DiFXcalc}[len(ALMAstuff)==0]
        FCin = open(CALC2OPEN)
        lines = FCin.readlines()
        FCin.close()
        for line in lines:
            if line.startswith("NUM SOURCES: "):
                NSOU = int(line.split()[-1].replace("\n", ""))
                soucoords = [np.ones(NSOU), np.ones(NSOU)]
                break

        for ii, line in enumerate(lines):
            if "SOURCE" in line and " NAME: " in line:
                tempLine = line.split()
                SID = int(tempLine[1])
                soucoords[0][SID] = float(lines[ii + 1].split()[-1])
                soucoords[1][SID] = float(lines[ii + 2].split()[-1])

    else:
        metadata = []
        OUTPUT = [OUTPUTIDI]

    ##########################################
    #########
    # COMPUTE XY delays:

    XYdelF = [[0.0, 0.0] for i in range(nALMATrue)]
    if type(XYdel) is not dict:  # or len(XYdel) != nALMA:
        printError(
            "Invalid format for XYdel!\n"
        )  # Should be a LIST of numbers, as large as the number of linear-polarization VLBI stations!")

    for i, doant in enumerate(linAntNamTrue):
        if doant in XYdel.keys():
            if type(XYdel[doant]) is list:
                try:
                    XYdelF[i] = list(
                        map(float, XYdel[doant])
                    )  # float(XYdel[i]*np.pi/180.)
                except:
                    printError(
                        "Invalid format for XYdel!\n Should be a dictionary with LISTS of numbers!"
                    )
            else:
                try:
                    XYdelF[i] = [float(XYdel[doant]), 0.0]  # float(XYdel[i]*np.pi/180.)
                except:
                    printError(
                        "Invalid format for XYdel!\n Should be a dictionary with LISTS of numbers!"
                    )

    if isSWIN:
        XYaddF = [[[] for i in range(nALMATrue)] for difxdfile in OUTPUT]
    else:
        XYaddF = [[[] for i in range(nALMATrue)]]

    for i in range(nALMATrue):
        for j in doIF:  # range(len(FrInfo['SIGN'])):
            sgn = FrInfo["SIGN"][j - 1]

            if (
                float(FrInfo["NUM CHANNELS"][j - 1] // FrInfo["CHANS TO AVG"][j - 1])
                != FrInfo["NUM CHANNELS"][j - 1] / FrInfo["CHANS TO AVG"][j - 1]
            ):
                printMsg(
                    "linspace check freq: %d / %d = %f"
                    % (
                        FrInfo["NUM CHANNELS"][j - 1],
                        FrInfo["CHANS TO AVG"][j - 1],
                        FrInfo["NUM CHANNELS"][j - 1] / FrInfo["CHANS TO AVG"][j - 1],
                    )
                )
            if isSWIN:
                NuChan = np.linspace(
                    (sgn - 1.0) / 2.0,
                    (sgn + 1.0) / 2.0,
                    FrInfo["NUM CHANNELS"][j - 1] // FrInfo["CHANS TO AVG"][j - 1],
                    endpoint=False,
                )
            else:
                NuChan = np.linspace(
                    0.0,
                    sgn,
                    FrInfo["NUM CHANNELS"][j - 1] // FrInfo["CHANS TO AVG"][j - 1],
                    endpoint=False,
                )
            Nus = 1.0e6 * np.array(
                FrInfo["FREQ (MHZ)"][j - 1] + FrInfo["BW (MHZ)"][j - 1] * NuChan,
                dtype=np.float,
            )

            for dfile in range(len(XYaddF)):
                XYaddF[dfile][i].append(np.zeros(len(Nus), dtype=np.float, order="C"))
                XYaddF[dfile][i][-1][:] = (
                    2.0 * np.pi * (Nus - XYdelF[i][1]) * XYdelF[i][0]
                )

    #########
    ##########################################

    # Prepare memory of XY amplitude ratios:

    if type(XYratio) is not dict:
        printError("Invalid format for XYratio!")

    if isSWIN:
        XYratioF = [[[] for i in range(nALMATrue)] for difxfile in OUTPUT]
    else:
        XYratioF = [[[] for i in range(nALMATrue)]]

    for i in range(nALMATrue):
        for j in doIF:
            if (
                float(FrInfo["NUM CHANNELS"][j - 1] // FrInfo["CHANS TO AVG"][j - 1])
                != FrInfo["NUM CHANNELS"][j - 1] / FrInfo["CHANS TO AVG"][j - 1]
            ):
                printMsg(
                    "linspace check freq: %d / %d = %f"
                    % (
                        FrInfo["NUM CHANNELS"][j - 1],
                        FrInfo["CHANS TO AVG"][j - 1],
                        FrInfo["NUM CHANNELS"][j - 1] / FrInfo["CHANS TO AVG"][j - 1],
                    )
                )

            for dfile in range(len(XYratioF)):
                XYratioF[dfile][i].append(
                    np.ones(
                        FrInfo["NUM CHANNELS"][j - 1] // FrInfo["CHANS TO AVG"][j - 1],
                        dtype=np.float,
                        order="C",
                    )
                )

    # Set XYadd and XYratio:

    if type(XYadd) is not dict:  # or len(XYadd.keys) != nALMA:
        printError(
            "Invalid format for XYadd!\n"
        )  # Should be a list as large as the number of
        # linear-polarization VLBI stations!
        # The elements of that list shall be either
        # numbers or lists as large as the number of IFs

    if isPcalUsed:
        import _XPCalMF as XP

        # printMsg('keys of XYadd:' + str(XYadd.keys()))
        # printMsg('keys of XYratio:' + str(XYratio.keys())+'\n')

    for i, doant in enumerate(linAntNamTrue):

        #########################
        #### CORRECTIONS BASED ON PHASECAL TONES:

        if isPcalUsed:

            if usePcal[doant]:
                printMsg("Using Pcal for %s" % doant)

                for dfile, phcalscan in enumerate(PHASECALS):

                    PCFile = list(
                        filter(lambda x: x.endswith(doant + pcalSuffix), phcalscan)
                    )

                    if len(PCFile) == 0:
                        ## If antenna has missing PCAL, there is no error anymore (dummy pcals are used instead):
                        pcalScan = "_".join(
                            os.path.basename(phcalscan[0]).split("_")[1:3]
                        )
                        printMsg(
                            "\n\n SANITY-TEST FAILURE! NO PHASECAL FOR %s IN SCAN %s\n"
                            % (doant, pcalScan)
                        )
                        isPcalFile = False
                        ## Dummy values for xpol pcal gains:
                        tempArr = np.zeros((2, 7))
                        tempArr[0, :] = [0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1]
                        tempArr[1, :] = [1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1]
                    else:
                        isPcalFile = True
                        fName = XP.XPCalMF(PCFile[0], [], 0, 0, FrInfo, IFoffset)
                        IFFCP = open(fName)
                        tempArr = []
                        for line in IFFCP.readlines():
                            if line[0] != "#":
                                tempArr.append(list(map(float, line.split())))
                        IFFCP.close()
                        tempArr = np.array(tempArr)

                    # Update pcal files (if not doing a test):
                    if isPcalFile and not doTest:
                        ErrCode = XP.XPCalMF(PCFile[0], [], 1, 0, {}, IFoffset)
                        if ErrCode != 0:
                            printError(
                                "\n\n ERROR Converting phasecal file %s\n"
                                % os.path.basename(PCFile[0])
                            )

                    if len(tempArr[0]) == 0:
                        printError(
                            "\n\n ERROR! No phasecal information for antenna %i\n Will NOT convert!\n"
                            % i
                        )
                    else:

                        for ji, j in enumerate(doIF):
                            mask = tempArr[:, -1] == float(j)
                            if np.sum(mask) > 0:
                                CPhase = spint.interp1d(
                                    tempArr[mask, 0],
                                    -tempArr[mask, 1],
                                    bounds_error=False,
                                    fill_value="extrapolate",
                                )
                                CAmpl = spint.interp1d(
                                    tempArr[mask, 0],
                                    tempArr[mask, 2],
                                    bounds_error=False,
                                    fill_value=1.0,
                                )
                            else:
                                CPhase = lambda x: 0.0
                                CAmpl = lambda x: 1.0

                            sgn = FrInfo["SIGN"][j - 1]
                            if isSWIN:
                                NuChan = np.linspace(
                                    (sgn - 1.0) / 2.0,
                                    (sgn + 1) / 2.0,
                                    int(FrInfo["NUM CHANNELS"][j - 1]/FrInfo["CHANS TO AVG"][j - 1]),
                                    endpoint=False,
                                )
                            else:
                                NuChan = np.linspace(
                                    0.0,
                                    sgn,
                                    int(FrInfo["NUM CHANNELS"][j - 1]/FrInfo["CHANS TO AVG"][j - 1]),
                                    endpoint=False,
                                )
                            Nus = np.array(
                                FrInfo["FREQ (MHZ)"][j - 1]
                                + FrInfo["BW (MHZ)"][j - 1] * NuChan,
                                dtype=np.float,
                            )

                            #### BANDPASS MODE:
                            if XYpcalMode.lower() == "bandpass":

                                XYaddF[dfile][i][ji] += (CPhase(Nus)) * np.pi / 180.0
                                if doant in XYratio.keys() and XYratio[doant] == 0.0:
                                    XYratioF[dfile][i][ji] *= CAmpl(Nus)

                            #### MULTITONE MODE:
                            elif XYpcalMode.lower() == "multitone":
                                NuAv = np.average(Nus)
                                ## Get the closest tone to the center frequency:
                                thisTone = np.argmin(np.abs(j - tempArr[:, -1]))
                                ## Comptue the delay model:
                                XYaddF[dfile][i][ji] -= (
                                    2.0
                                    * np.pi
                                    * tempArr[thisTone, 3]
                                    * (Nus - tempArr[thisTone, 5])
                                    + tempArr[thisTone, 4] * np.pi / 180.0
                                )
                                if doant in XYratio.keys() and XYratio[doant] == 0.0:
                                    XYratioF[dfile][i][ji] *= np.average(CAmpl(Nus))
                            else:

                                printError("Unknown tone mode %s\n" % XYpcalMode)

                            del CPhase, CAmpl
                            Nelem = len(tempArr)

        ##################################

        if doant not in XYadd.keys():
            printMsg("ANTENNA %s DOES NOT HAVE XYadd INFO." % doant)

        else:

            TOADD = {}
            if type(XYadd[doant]) is list:
                for ifi in range(len(XYadd[doant])):
                    TOADD[ifi + 1] = XYadd[doant][ifi]
            elif type(XYadd[doant]) is dict:
                TOADD = XYadd[doant]

            if len(TOADD.keys()) > 0:
                for jid, j in enumerate(sorted(TOADD.keys())):
                    if type(TOADD[j]) in [list, np.ndarray]:
                        arrSize = np.shape(np.array(TOADD[j]))
                        if len(arrSize) != 1 or arrSize[0] != IFchan:
                            printError(
                                "Shape of XYadd array(s) (%s) does not coincide with number of IF channels (%s)\n"
                                % (str(arrSize),str(IFchan))
                            )
                        else:
                            for dfile in range(len(OUTPUT)):
                                #   print(dfile,doant,i,jid, OUTPUT)
                                #   print(TOADD)
                                #   print(XYaddF[dfile][i])
                                if j in doIF:
                                    XYaddF[dfile][i][doIF.index(j)] += (
                                        np.array(TOADD[j]) * np.pi / 180.0
                                    )
                    else:
                        try:
                            for dfile in range(len(OUTPUT)):
                                if j in doIF:
                                    XYaddF[dfile][i][doIF.index(j)] += float(TOADD[j]) * np.pi / 180.0
                        except Exception as ex:
                            printMsg(str(ex))
                            printError(
                                "Invalid format for XYadd!\nShould be a LIST of numbers (or a list of lists of numbers),\nor a list of lists of arrays"
                            )
            else:

                try:
                    for j in range(len(doIF)):
                        for dfile in range(len(OUTPUT)):
                            XYaddF[dfile][i][j] += float(XYadd[doant]) * np.pi / 180.0
                except Exception as ex:
                    printMsg(str(ex))
                    printError(
                        "Invalid format for XYadd!\n Should be a LIST of numbers (or a list of lists),\n as large as the number of linear-polarization VLBI stations!"
                    )

    UseAutoCorrs = np.zeros(nALMATrue, dtype=np.int32, order="C")

    for i, doant in enumerate(linAntNamTrue):

        #    try:

        if doant not in XYratio.keys():
            printMsg("ANTENNA %s DOES NOT HAVE XYratio INFO!" % doant)

        else:

            TOADD = {}
            if type(XYratio[doant]) is list:
                for ifi in range(len(XYratio[doant])):
                    TOADD[ifi + 1] = XYratio[doant][ifi]
            elif type(XYratio[doant]) is dict:
                TOADD = XYratio[doant]

            if len(TOADD.keys()) > 0:

                for jid, j in enumerate(sorted(TOADD.keys())):
                    if type(TOADD[j]) in [list, np.ndarray]:
                        tempArr = np.array(TOADD[j])
                        arrSize = np.shape(tempArr)
                        if len(arrSize) != 1 or arrSize[0] != IFchan:
                            printError(
                                "Shape of XYratio array(s) does not coincide with number of IF channels\n"
                            )
                        else:
                            for dfile in range(len(XYratioF)):
                                if j in doIF:
                                    XYratioF[dfile][i][doIF.index(j)] *= tempArr
                    else:
                        for dfile in range(len(XYratioF)):
                            if j in doIF:
                                XYratioF[dfile][i][doIF.index(j)] *= float(TOADD[j])

            else:
                try:
                    for j in range(len(doIF)):
                        for dfile in range(len(XYratioF)):
                            XYratioF[dfile][i][j] *= float(XYratio[doant])
                except Exception as ex:
                    printMsg(str(ex))
                    printError(
                        "Invalid format for XYadd!\n Should be a LIST of numbers (or a list of lists),\n as large as the number of linear-polarization VLBI stations!"
                    )

    #    except Exception as ex:
    #      printMsg(str(ex))
    #      printError("Invalid format for XYratio!\n Should be a LIST of numbers (or a list of lists),\n as large as the number of linear-polarization VLBI stations!")

    # A-PRIORI GAINS:

    PrioriGains = [[] for dfile in XYaddF]
    for dfile in range(len(XYaddF)):
        for i in range(len(linAntIdxTrue)):
            PrioriGains[dfile].append([])
            for j in range(len(XYaddF[dfile][i])):
                PrioriGains[dfile][i].append(
                    np.array(
                        XYratioF[dfile][i][j] * np.exp(1.0j * XYaddF[dfile][i][j]),
                        dtype=np.complex64,
                        order="C",
                    )
                )
               # print(dfile,i,j,PrioriGains)
    # print(XYaddF)

    # TEMPORARY FILE TO STORE FRINGE FOR PLOTTING:

    if amp_norm > 0.0:
        os.system("rm -rf POLCONVERT.GAINS")

    if len(plotIF) > 0:
        for dirnam in [
            "CONVERSION.MATRIX",
            "FRINGE.PEAKS",
            "FRINGE.PLOTS",
            "POLCONVERT.FRINGE",
        ]:
            if not os.path.exists(dirnam):
                os.system("mkdir %s" % dirnam)

    printMsg("\n###\n### Going to PolConvert\n###")

    doAmpNorm = amp_norm > 0.0
    didit = -10

    if not isSWIN:
        OUTPUT = OUTPUT[0]
    # plotAnt is no longer used by PC.PolConvert(), but is required by doSolve
    # the second argument is "PC:PolConvert::plIF" and controls whether the huge binary fringe files are written.
    try:

        didit = PC.PolConvert(
            nALMATrue,
            plotIF,
            plotAnt,
            doIF,
            IFoffset,
            AC_MedianWindow,
            swapXY,
            OUTPUT,
            linAntIdxTrue,
            plRan,
            Ran,
            doTest,
            doSolve,
            doConj,
            doAmpNorm,
            PrioriGains,
            metadata,
            soucoords,
            antcoords,
            antmounts,
            isLinear,
            calfield,
            UseAutoCorrs,
            bool(correctParangle),
            DEBUG,
            logName,
            ALMAstuff,
        )

    except Exception as ex:
        printMsg(str(ex))
        # printMsg("Continuing despite the exception, just for the fun of it")
        # didit = 0
        printError("\n###\n### Done with PolConvert (status %d).\n###" % (didit))

    # GENERATE ANTAB FILE(s):

    if doAmpNorm:
        printMsg("Generating ANTAB file(s).")
        DPFU = float(amp_norm)
        printMsg("DPFU(amp_norm) is %f\n" % DPFU)

        try:
            gfile = open("POLCONVERT.GAINS")
        except:
            printError("No gain file written!")

        entries = [l.split() for l in gfile.readlines()]
        gfile.close()
        if entries == 0:
            printMsg("Gain file is empty!  ANTAB will be as well.")
        else:
            printMsg("Gain file had %d entries" % len(entries))

        IFs = np.zeros(len(entries), dtype=np.int)
        Data = np.zeros((len(entries), 2))
        AntIdx = np.zeros(len(entries), dtype=np.int)

        for i, entry in enumerate(entries):
            IFs[i] = int(entry[0])
            AntIdx[i] = int(entry[1])
            Data[i, :] = list(map(float, entry[2:]))

        Times = np.unique(Data[:, 0])
        Tsys = np.zeros((len(Times), len(doIF) + 1))
        Tsys[:, 0] = Times

        for j, jnam in enumerate(linAntNamTrue):
            for ii, i in enumerate(doIF):
                mask = np.logical_and(IFs == i, AntIdx == linAntIdxTrue[j])
                for datum in Data[mask]:
                    itime = np.where(Times == datum[0])[0]
                    Tsys[itime, ii + 1] = datum[1] * DPFU

            outf = open("POLCONVERT_STATION_%s.ANTAB" % jnam, "w")
            print("GAIN AA  ELEV DPFU=%.3f   FREQ=10,100000" % DPFU, file=outf)
            print("POLY=1.0000E+00", file=outf)
            print("/", file=outf)
            print("TSYS AA  FT=1.0  TIMEOFF=0", file=outf)
            print(
                "INDEX= "
                + ", ".join(["'L%i|R%i'" % (i + 1, i + 1) for i in range(len(doIF))]),
                file=outf,
            )
            print("/", file=outf)
            fmt0 = "%i %i:%2.4f  "
            # boost field width to retain significant figures
            fmt1 = "%10.4f  " * len(doIF)
            prevT = " "
            for entry in Tsys:
                MJD2000 = 51544
                Tfr, Tin = np.modf(entry[0])
                tobs = (
                    (dt.date(2000, 1, 1) + dt.timedelta(Tin - MJD2000))
                    .timetuple()
                    .tm_yday
                )
                minute, hour = np.modf(Tfr * 24.0)
                minute *= 60.0
                currT = fmt0 % (tobs, hour, minute)
                if currT != prevT:  # Limited time resolution in ANTAB
                    prevT = currT
                    print(currT + fmt1 % tuple(entry[1:]), file=outf)
            print("/", file=outf)
            outf.close()

        printMsg("Finished with ANTAB file(s).")

    tac = time.time()

    printMsg("PolConvert took %.1f seconds.\n\n" % (tac - tic))

    # SOLVE FOR THE CROSS-POLARIZATION GAINS:
    if doSolve >= 0.0:

        tic = time.time()

        CGains = {
            "XYadd": {},
            "XYratio": {},
            "aPrioriXYGain": PrioriGains,
            "Frequency": {},
        }

        #   solveMethod = 'Levenberg-Marquardt'
        #   fitMethod = 'COBYLA'   # 'Newton-CG'  # 'nelder-mead'
        fitMethod = solveMethod

        useCov = False
        if fitMethod == "Levenberg-Marquardt":
            useCov = True

        # Fine-tunning parameters for Levenberg-Marquardt:
        LMLambda = 1.0e-3
        KFacRaise = 5.0  # 2.0
        KFacDecr = 10.0  # 3.0
        maxErr = 1.0e-5
        maxIter = 20  # Per gain.

        # Load the solver library:
        # try:
        if True:
            import _PolGainSolve as PS

            goodclib = True
            print("\nC++ shared library loaded successfully (first try)\n")
        # except:
        else:
            goodclib = False
            print("\n There has been an error related to the numpy")
            print(" API version used by CASA. This is related to PolConvert")
            print(" (which uses the API version of your system) and should")
            print(" be *harmless*.\n")

        if not goodclib:
            try:
                import _PolGainSolve as PS

                goodclib = True
                print("\nC++ shared library loaded successfully (2nd try)\n")
            except:
                goodclib = False

        ## FOR DEBUGGING:
        os.system("rm -f PolConvert.GainSolve.Calls")

        ############################################################
        # Levenberg-Marquardt minimizer of the GCPFF problem:

        ### NOTE: CURRENTLY BROKEN!!

        def LMMin(p0, Ch0, Ch1):

            MAXIT = maxIter * len(fitAnts)
            relchange = 1.0

            i = 0

            # First iteration:
            pini = np.array(p0)

            ptst0 = np.copy(pini)
            ptst1 = np.copy(pini)

            LMTune = LMLambda

            minChi2 = 0.0
            minGains = np.copy(pini)

            currP = np.copy(pini)

            while i < MAXIT:

                ptst0 = np.copy(currP)
                currChi2 = PS.GetChi2(ptst0, LMTune, Ch0, Ch1, 0,useRates,useDelays)
                Chi2_0 = PS.GetChi2(ptst0, -1.0, Ch0, Ch1, 0,useRates,useDelays)
                #  sys.stdout.write('%.3g/1 '%Chi2_0) ; sys.stdout.flush()

                if i == 0 or currChi2 < minChi2:
                    minChi2 = currChi2
                    minGains[:] = currP

                i += 1

                while Chi2_0 >= currChi2:

                    LMTune *= KFacRaise
                    ptst0 = np.copy(currP)

                    Chi2_ini = PS.GetChi2(ptst0, LMTune, Ch0, Ch1, 0,useRates,useDelays)
                    Chi2_0 = PS.GetChi2(ptst0, -1.0, Ch0, Ch1, 0,useRates,useDelays)
                    #    sys.stdout.write('%.3g/2 '%Chi2_0) ; sys.stdout.flush()
                    i += 1

                    if i >= MAXIT:
                        break

                if Chi2_0 > 0.0:
                    relchange = (currChi2 - Chi2_0) / Chi2_0
                else:
                    printError("\n\n  Problem in PolGainSolve.\n")

                # No improvement:
                if currChi2 < minChi2:
                    minChi2 = currChi2
                    minGains[:] = currP

                # Improvement:
                if Chi2_0 < currChi2:
                    currP = np.copy(ptst0)
                    currChi2 = Chi2_0
                    # Absolute improvement:
                    if Chi2_0 < minChi2:
                        minChi2 = Chi2_0
                        minGains[:] = ptst0

                if i >= MAXIT or np.abs(relchange) < maxErr:
                    break

                ptst0 = np.copy(currP)
                Chi2_ini = PS.GetChi2(ptst0, LMTune, Ch0, Ch1, 0, useRates,useDelays)
                Chi2_0 = PS.GetChi2(ptst0, -1.0, Ch0, Ch1, 0, useRates,useDelays)
                #   sys.stdout.write('%.3g/3 '%Chi2_0) ; sys.stdout.flush()

                i += 1

                if Chi2_0 > currChi2:
                    LMTune *= KFacRaise

                else:

                    while True:  # Chi2_0<currChi2:
                        i += 1
                        LMTune /= KFacDecr
                        ptst0 = np.copy(currP)
                        Chi2_ini = PS.GetChi2(ptst0, LMTune, Ch0, Ch1, 0, useRates,useDelays)
                        Chi2_1 = PS.GetChi2(ptst0, -1.0, Ch0, Ch1, 0, useRates,useDelays)
                        #       sys.stdout.write('%.3g/4'%Chi2_1)
                        if Chi2_1 < minChi2:
                            minChi2 = Chi2_1
                            minGains = np.copy(ptst0)
                            currP = np.copy(ptst0)
                            currChi2 = Chi2_1
                        #        sys.stdout.write('a')

                        if Chi2_1 < Chi2_0 and i <= MAXIT:
                            relchange = (currChi2 - Chi2_0) / Chi2_0
                            Chi2_0 = Chi2_1
                            ptst1[:] = ptst0
                            currP = np.copy(ptst0)
                            currChi2 = Chi2_1
                        #        sys.stdout.write('b')
                        else:
                            break
                    #      sys.stdout.write(' ') ; sys.stdout.flush()

                    #   relchange = (currChi2 - Chi2_0)/Chi2_0
                    LMTune *= KFacDecr  # Come back to state of last successful decrease

                    if i >= MAXIT or np.abs(relchange) < maxErr:
                        break

            Chi2_final = PS.GetChi2(minGains, LMTune, Ch0, Ch1, 1, useRates,useDelays)
            FLIP = Chi2_final > 0.0  # Flip gains by 180 degrees.

            # GBC debugging:
            if FLIP:
                sys.stdout.write(" Flipped\n")
            else:
                sys.stdout.write(" NotFlip\n")
            printMsg("    Final error: %.3e in ChSq" % (np.abs(relchange)))
            if i >= MAXIT:
                if np.abs(relchange) > maxErr:
                    printMsg(
                        "    WARNING! Slow cross-pol gain convergence (%d)! | Chan(s): %i-%i !"
                        % (i, Ch0, Ch1 - 1)
                    )
                else:
                    printMsg("    Warning: too many iterations (%d) why is that?" % i)
            # printMsg("\n    Final error: %.3e in ChSq / %.3e in gains\n"%(np.abs(relchange),Gchange))

            return [minGains, FLIP]

        # end of Levenberg-Marquardt minimizer of the GCPFF problem
        #############################################

        ########################################
        ### Gradient Minimizer for the GCPFF problem.

        def GrMin(p0, Ch0, Ch1):

            MAXIT = maxIter * len(fitAnts)
            relchange = 1.0

            Lambda = 1.0

            i = 0

            # First iteration:
            pini = np.array(p0)
            currP = np.copy(pini)
            Chi2ini = PS.GetChi2(currP, Lambda, Ch0, Ch1, 0,useRates,useDelays)
            bestChi = float(Chi2ini)
            bestP = np.copy(pini)
            ptest = np.copy(currP)

            while i < MAXIT:
                i += 1
                currP[:] = ptest
                Chi2 = PS.GetChi2(ptest, Lambda, Ch0, Ch1, 0,useRates,useDelays)
                if Chi2 < bestChi:
                    bestChi = Chi2
                    bestP[:] = currP
                    Lambda *= 2.0
                else:
                    ptest[:] = currP
                    Lambda /= 2.0

            Chi2_final = PS.GetChi2(bestP, -1.0, Ch0, Ch1, 1,useRates,useDelays)
            FLIP = Chi2_final > 0.0  # Flip gains by 180 degrees.

            return [bestP, FLIP]

        if goodclib:

            selAnts = np.array(calAnts, dtype=np.int32)

            doSolveD = float(doSolve)

            if doSolveD > 0.0:
                fitAnts = list(calAnts)
            else:
                fitAnts = [
                    ai for ai in linAntIdxTrue if ai in calAnts
                ]  # list(linAntIdxTrue)

            cAnts = np.array(calAnts, dtype=np.int32)
            lAnts = np.array(linAntIdxTrue, dtype=np.int32)

            printMsg("\n%%% initializing PolGainSolve\n")
            MySolve = PS.PolGainSolve(
                doSolveD,
                UVTaper,
                solint,
                selAnts,
                lAnts,
                [FlagBas1, FlagBas2],
                "PolGainSolve%s.log" % plotSuffix,
            )
            printMsg(PS.__doc__ + ("\nInitialization rv %d\n" % MySolve) + "%%%\n")

            AllFreqs = []

            for pli in doIF:
                printMsg("Reading back IF #%i" % pli)
                file1 = "POLCONVERT.FRINGE/OTHERS.FRINGE_IF%i" % pli
                file2 = "POLCONVERT.FRINGE/POLCONVERT.FRINGE_IF%i" % pli
                success = PS.ReadData(pli, file1, file2, 0.0)
                NScan = PS.GetNScan(pli)
                if success != 0:
                    printError("Failed PolGainSolve: ERROR %i" % success)

                AllFreqs.append(np.zeros(PS.GetNchan(pli), order="C", dtype=np.float))
                ifsofIF = PS.GetIFs(pli, AllFreqs[-1])

            MaxChan = max([np.shape(pp)[0] for pp in AllFreqs])

            printMsg(
                "  length of AllFreqs is %d. MaxChan %d" % (len(AllFreqs), MaxChan)
            )
            printMsg("\nWill now estimate the residual cross-polarization gains.\n")

            # ESTIMATE RATES FOR ALL STATIONS (plotAnt IS THE [plotting] REFERENCE):

            dropAnt = calAnts.index(plotAnt)
            rateAnts = calAnts[:dropAnt] + calAnts[dropAnt + 1 :]
            printMsg("\n Estimate antenna delays & rates\n")
            for nsi in range(NScan):
                PS.DoGFF(rateAnts, npix, True, nsi, 5.0)

            for ci in antcodes:
                CGains["XYadd"][ci] = {}
                CGains["XYratio"][ci] = {}

            for plii, pli in enumerate(doIF):
                CGains["Frequency"][pli] = AllFreqs[plii]

            # BP MODE:

            if solint[0] != 0:
                printMsg("\n Estimate antenna cross-pol gains: BP mode\n")

                ChAv = abs(solint[0])

                for plii, pli in enumerate(doIF):
                    # printMsg("  working %s,%s"%(str(plii),str(pli)))
                    Nchans = np.shape(AllFreqs[plii])[0]

                    temp = [np.zeros(Nchans, dtype=np.complex64) for ci in fitAnts]
                    BPChan = list(range(0, Nchans, ChAv))
                    if BPChan[-1] < Nchans - 1:
                        BPChan.append(Nchans - 1)
                    BPChan = np.array(BPChan, dtype=np.int32)
                    Npar = len(fitAnts) * {True: 2, False: 1}[solveAmp]
                    laux = [pli]
                    rv = PS.SetFit(
                        Npar, laux, fitAnts, solveAmp, solveQU, Stokes, useCov, feedRot
                    )
                    if rv != 0:
                        printMsg("  PS.SetFit rv %d" % rv)

                    ## Auxiliary arrays for x-pol gain interpolation:
                    interpGain = [
                        np.zeros(len(BPChan) - 1, dtype=np.complex64) for ci in fitAnts
                    ]
                    interpChan = np.zeros(len(BPChan) - 1)

                    for chran in range(len(BPChan) - 1):
                        if chran == 0 and plii == 0:
                            p0 = []
                            for ci in fitAnts:
                                if solveAmp:
                                    p0 += [1.0, 0.0]
                                else:
                                    p0 += [0.0]
                        else:
                            p0 = list(myfit)

                        laux = [pli]
                        sys.stdout.write(
                            "\r Apply rates and estimate cross-gains for IF #%i, channels %i to %i   "
                            % (pli, BPChan[chran], BPChan[chran + 1] - 1)
                        )
                        sys.stdout.flush()


                        if fitMethod not in scipyMethods:
                            if fitMethod == "Levenberg-Marquardt":
                                myfit, FLIP = LMMin(
                                    p0, BPChan[chran], BPChan[chran + 1]
                                )
                            if fitMethod == "gradient":
                                myfit, FLIP = GrMin(
                                    p0, BPChan[chran], BPChan[chran + 1]
                                )
                        else:

                            if fitMethod in ["BFGS", "Newton-CG", "SLSQP"]:

                                def Fmini(p):
                                    return PS.GetChi2(
                                        p, -1.0, BPChan[chran], BPChan[chran + 1], 0, useRates, useDelays
                                    )

                                def Fgrad(p):
                                    return spopt.approx_fprime(p, Fmini, 1.0e-8)

                                mymin = spopt.minimize(
                                    Fmini, p0, method=fitMethod, jac=Fgrad
                                )

                            else:

                                mymin = spopt.minimize(
                                    PS.GetChi2,
                                    p0,
                                    args=(-1.0, BPChan[chran], BPChan[chran + 1], 0, useRates, useDelays),
                                    method=fitMethod,
                                )

                            Chi2_final = PS.GetChi2(
                                mymin.x, -1, BPChan[chran], BPChan[chran + 1], 1, useRates, useDelays
                            )
                            FLIP = Chi2_final > 0.0
                            myfit = mymin.x
                        interpChan[chran] = 0.5 * (BPChan[chran] + BPChan[chran + 1])
                        for ci, calant in enumerate(fitAnts):
                            PhasFactor = {True: np.pi, False: 0.0}[
                                FLIP and (calant in linAntIdxTrue)
                            ]
                            if solveAmp:
                                # AMP+PHASE SPACE:
                                temp[ci][BPChan[chran] : BPChan[chran + 1] + 1] = myfit[
                                    2 * ci
                                ] * np.exp(1.0j * (PhasFactor + myfit[2 * ci + 1]))
                                interpGain[ci][chran] = myfit[2 * ci] * np.exp(
                                    1.0j * (PhasFactor + myfit[2 * ci + 1])
                                )
                            else:
                                temp[ci][
                                    BPChan[chran] : BPChan[chran + 1] + 1
                                ] = np.exp(1.0j * (PhasFactor + myfit[ci]))
                                interpGain[ci][chran] = np.exp(
                                    1.0j * (PhasFactor + myfit[ci])
                                )
                    for ci in antcodes:
                        CGains["XYratio"][ci][pli] = np.ones(len(temp[0]))
                        CGains["XYadd"][ci][pli] = np.zeros(len(temp[0]))

#####
## Identify accidental pi jumps:
                    Namb = [0,0]; kj = 0
                    NantHf = int(len(fitAnts)//2)
                    Saltos = [0]
                    for gi in range(len(interpGain[0])-1):
                        NjAnt = 0
                        for ci in range(len(fitAnts)):         
                            if np.abs(np.angle(interpGain[ci][gi+1]/interpGain[ci][gi]))>np.pi/2.:
                                NjAnt += 1
                        if NjAnt > NantHf:
                            kj = 1-kj
                            Saltos.append(gi+1)
                        Namb[kj] += 1
                    Saltos.append(len(interpGain[0]))
       
                    if Namb[1]>0 and Namb[0]>Namb[1]:
                        for kj in range(1,len(Saltos)-1,2):
                            for ci in range(len(fitAnts)):
                                interpGain[ci][Saltos[kj]:Saltos[kj+1]] *= -1.0
                    elif Namb[1]>Namb[0]:
                        for kj in range(0,len(Saltos)-1,2):
                            for ci in range(len(fitAnts)):
                                interpGain[ci][Saltos[kj]:Saltos[kj+1]] *= -1.0
         


                    for ci, calant in enumerate(fitAnts):
                        #####
                        ## Prepare gain interpolation (connect phases across band):

                        Angles = np.angle(interpGain[ci])
                        for gi in range(len(Angles) - 1):
                            if Angles[gi + 1] - Angles[gi] > np.pi:
                                Angles[gi + 1 :] -= 2.0 * np.pi
                            if Angles[gi + 1] - Angles[gi] < -np.pi:
                                Angles[gi + 1 :] += 2.0 * np.pi

                        AmpInterp = spint.interp1d(
                            interpChan,
                            np.abs(interpGain[ci]),
                            kind="linear",
                            bounds_error=False,
                            fill_value="extrapolate",
                        )
                        PhsInterp = spint.interp1d(
                            interpChan,
                            Angles,
                            kind="linear",
                            bounds_error=False,
                            fill_value="extrapolate",
                        )
                        #####
                        CGains["XYratio"][antcodes[calant - 1]][pli][
                            :
                        ] = 1.0 / AmpInterp(range(Nchans))
                        CGains["XYadd"][antcodes[calant - 1]][pli][:] = (
                            -180.0
                            / np.pi
                            * np.angle(np.exp(1.0j * PhsInterp(range(Nchans))))
                        )

                printMsg("Done with BP mode\n")

            # SBD MODE:
            else:
                for plii, pli in enumerate(doIF):
                    Nchans = np.shape(AllFreqs[plii])[0]
                    temp = [np.zeros(Nchans, dtype=np.complex64) for ci in fitAnts]
                    p0 = []
                    for ci in fitAnts:
                        if solveAmp:
                            p0 += [1.0, 0.0]
                        else:
                            p0 += [0.0]
                    for ci in fitAnts:
                        p0 += [0.0]
                    laux = [pli]  # list(doIF)
                    Npar = len(fitAnts) * {True: 3, False: 2}[solveAmp]
                    rv = PS.SetFit(
                        Npar, laux, fitAnts, solveAmp, solveQU, Stokes, useCov, feedRot
                    )
                    if rv != 0:
                        printMsg("  PS.SetFit rv %d" % rv)
                    Nchans = np.shape(AllFreqs[plii])[0]
                    FreqChan = np.linspace(-Nchans / 2.0, Nchans / 2.0, Nchans)
                    if fitMethod not in scipyMethods:  # =='Levenberg-Marquardt':
                        myfit = LMMin(p0, 0, MaxChan - 1)
                        sys.stdout.write(".")
                        sys.stdout.flush()
                    else:

                        # Preliminary fit (with no delays):
                        mymin = spopt.minimize(
                            PS.GetChi2, p0[:-nfitAnt], args=(-1.0, 0, Nchans, 0, useRates,useDelays), method=fitMethod)
                        p0[:-nfitAnt] = mymin.x

                        # Now, fit the delays as well:
                        mymin = spopt.minimize(
                            PS.GetChi2, p0, args=(-1.0, 0, Nchans, 0, useRates,useDelays), method=fitMethod)

                    Chi2_final = PS.GetChi2(mymin.x, -1, 0, Nchans, 1, useRates,useDelays)
                    FLIP = Chi2_final > 0.0
                    myfit = mymin.x

                    for ci, calant in enumerate(fitAnts):
                        PhasFactor = {True: np.pi, False: 0.0}[
                            FLIP and (calant in linAntIdxTrue)
                        ]
                        if solveAmp:
                            # AMP+PHASE SPACE:
                            #    temp[ci][:]= (myfit[2*ci]*np.exp(1.j*(PhasFactor + myfit[2*ci+1])))
                            temp[ci][:] = myfit[2*ci]*np.exp(
                                1.0j*(PhasFactor + myfit[2*ci+1] + myfit[2 * len(fitAnts) + ci]*FreqChan)
                            )
                        else:
                            #    temp[ci][BPChan[chran]:BPChan[chran+1]+1]= np.exp(1.j*(PhasFactor+myfit[ci]))
                            temp[ci][:] = np.exp(
                                1.0j*(PhasFactor + myfit[ci] + myfit[len(fitAnts) + ci]*FreqChan)
                            )
                    for ci in antcodes:
                        CGains["XYratio"][ci][pli] = np.ones(len(temp[0]))
                        CGains["XYadd"][ci][pli] = np.zeros(len(temp[0]))
                    for ci, calant in enumerate(fitAnts):
                        #####
                        ## Prepare gain interpolation (connect phases across band):

                        Angles = np.angle(temp[ci])
                        for gi in range(len(Angles) - 1):
                            if Angles[gi + 1] - Angles[gi] > np.pi:
                                Angles[gi + 1 :] -= 2.0 * np.pi
                            if Angles[gi + 1] - Angles[gi] < -np.pi:
                                Angles[gi + 1 :] += 2.0 * np.pi

                        # AmpInterp = spint.interp1d(interpChan,np.abs(interpGain[ci]),kind='linear',bounds_error=False,fill_value="extrapolate")
                        # PhsInterp = spint.interp1d(interpChan,Angles,kind='linear',bounds_error=False,fill_value="extrapolate")
                        #####
                        CGains["XYratio"][antcodes[calant - 1]][pli][:] = 1.0 / np.abs(
                            temp[ci]
                        )  # AmpInterp(range(Nchans))
                        CGains["XYadd"][antcodes[calant - 1]][pli][:] = (
                            -180.0 / np.pi * np.angle(temp[ci])
                        )  # np.angle(np.exp(1.j*PhsInterp(range(Nchans))))

                printMsg("Done with SBD mode\n")

            ## These arrays can be very large!

            #    try:
            if True:

                fig = pl.figure()
                MaxG = 0.0
                color = ["r", "g", "b", "k", "m", "y", "c"]
                symbol = ["o", "+", "x"]
                Freq2Plot = np.concatenate(AllFreqs) / 1.0e9

                printMsg("Working subplot 1")
                sub1 = fig.add_subplot(211)
                for antii, anti in enumerate(sorted(CGains["XYadd"].keys())):
                    sub1.plot(
                        Freq2Plot,
                        np.concatenate(
                            [
                                np.array(CGains["XYadd"][anti][ll])
                                for ll in sorted(CGains["XYadd"][anti].keys())
                            ]
                        ),
                        symbol[int(((antii) // len(color)) % len(symbol))]
                        + color[int((antii) % len(color))],
                        label="ANT. " + str(anti), ms=2
                    )

                printMsg("Working subplot 2")
                sub2 = fig.add_subplot(212, sharex=sub1)
                for antii, anti in enumerate(sorted(CGains["XYratio"].keys())):
                    toplot = np.concatenate(
                        [
                            np.array(CGains["XYratio"][anti][ll])
                            for ll in sorted(CGains["XYratio"][anti].keys())
                        ]
                    )
                    MaxG = max(MaxG, np.max(toplot))
                    sub2.plot(
                        Freq2Plot,
                        toplot,
                        symbol[int(((antii) // len(color)) % len(symbol))]
                        + color[int((antii) % len(color))],
                        label="ANT. " + str(anti), ms=2
                    )

                printMsg("Labelling and adjusting")
                sub1.set_ylim((-180.0, 180.0))
                sub2.set_ylim((0.0, 1.1 * MaxG))
                pl.setp(sub1.get_xticklabels(), "visible", False)
                Dnu = np.max(Freq2Plot) - np.min(Freq2Plot)
                sub1.set_xlim(
                    (np.min(Freq2Plot) - Dnu * 0.1, np.max(Freq2Plot) + Dnu * 0.45)
                )
                sub2.set_xlim(
                    (np.min(Freq2Plot) - Dnu * 0.1, np.max(Freq2Plot) + Dnu * 0.45)
                )
                sub2.set_ylim((0.0, 2.5))

                sub1.legend(numpoints=1)
                sub1.set_ylabel("Cross-Phase (deg.)")
                sub2.set_ylabel("Cross-Amp (Norm.)")
                sub2.set_xlabel("Frequency (GHz)")
                EXPN = os.path.basename(IDI)
                if "." in EXPN:
                    EXPN = "-".join(EXPN.split(".")[:-1])
                EXPN = EXPN.replace(" ", "_")
                fig.suptitle("XPOL GAINS %s" % EXPN)
                pl.savefig("Cross-Gains_%s%s.png" % (EXPN, plotSuffix))

            PS.FreeData()
            printMsg("PS Data freed\n")

            tac = time.time()
            printMsg("PolGainSolve took %.1f seconds.\n\n" % (tac - tic))

        else:  # if not goodclib!

            printMsg(
                "\n\n  doSolve can ONLY work IF the source was compiled with DO_SOLVE=True\n  PLEASE, RECOMPILE!\n\n"
            )

    else:  # if doSolve >= 0.0:

        printMsg("\n\n  doSolve was not requested\n\n")
        CGains = {"aPrioriXYGain": PrioriGains}

    # end of doSolve if...else

    # PLOT FRINGES:
    if plotAnt not in calAnts:
        didit = 1
        printMsg("plotAnt not in calAnts, so skipping plots")

    # didit is returned from PC, and if nonzero will have terminated above.
    if plotFringe and didit == 0:
        printMsg("proceding to fringe plots with plotAnt %d..." % plotAnt)

        fig = pl.figure(figsize=(12, 6))

        fig2 = pl.figure()

        fringeAmps = {}
        fringeAmpsMix = {}
        ResidGains = {}
        MixedCalib = {}

        # Filter out IFs with no data:
        GoodIFs = []
        for pli in plotIF:
            if os.stat("POLCONVERT.FRINGE/POLCONVERT.FRINGE_IF%i" % pli).st_size > 10:
                GoodIFs.append(pli)
            else:
                printMsg("WARNING! IF %i was NOT polconverted properly\n" % pli)
                printMsg("POLCONVERT.FRINGE/POLCONVERT.FRINGE_IF%i missing/bad\n" % pli)

        # start of GoodIFs pli loop
        for pli in GoodIFs:

            print("\n\n")
            printMsg("Plotting selected fringe for IF #%i" % pli)

            frfile = open("POLCONVERT.FRINGE/POLCONVERT.FRINGE_IF%i" % pli, "rb")

            alldats = frfile.read(5)
            nchPlot, isParang = stk.unpack("ib", alldats)
            dtype = np.dtype(
                [
                    ("FILE", np.int32),
                    ("JDT", np.float64),
                    ("ANT1", np.int32),
                    ("ANT2", np.int32),
                    ("PANG1", np.float64),
                    ("PANG2", np.float64),
                    ("UVDIST", np.float64),
                    ("MATRICES", np.complex64, 12 * nchPlot),
                ]
            )

            # There is a silly bug in Python 2.7, which generates
            # an "integer is required" error in the first try to read:
            try:
                fringe = np.fromfile(frfile, dtype=dtype)
                printMsg("Read fringe data from file POLCONVERT.FRINGE_IF%i" % pli)
            except:
                fringe = np.fromfile(frfile, dtype=dtype)
                printMsg("Exceptional Read of fringe data POLCONVERT.FRINGE_IF%i" % pli)
                printMsg("len(fringe) = %d" % len(fringe))
            frfile.close()

            # start of for ant1 in linAntIdx
            NBinFringes = np.max(fringe["FILE"]) + 1
            for ant1 in linAntIdxTrue:
                if pli == GoodIFs[0]:
                    MixedCalib[ant1] = {}
                    fringeAmps[ant1] = []
                    fringeAmpsMix[ant1] = []
                    ResidGains[ant1] = []

                # start of fringe loop on ant1-ant2 baseline
                for ant2 in [plotAnt]:

                    AntEntry1 = np.logical_and(
                        fringe[:]["ANT1"] == ant1, fringe[:]["ANT2"] == ant2
                    )
                    AntEntry2 = np.logical_and(
                        fringe[:]["ANT2"] == ant1, fringe[:]["ANT1"] == ant2
                    )
                    AntennasIn = np.logical_or(AntEntry1, AntEntry2)
                    for NBF in range(NBinFringes):
                        AntEntry = np.logical_and(
                            fringe[:]["FILE"] == NBF,
                            np.logical_or(AntEntry1, AntEntry2),
                        )
                        if np.sum(AntEntry) > 0:
                            break
                    printMsg("np.sum(AntEntry) > 0: '" + str(np.sum(AntEntry)) + "'")

                    # start of np.sum(AntEntry)
                    if np.sum(AntEntry) > 0:
                        printMsg("Something to plot, apparently")

                        if pli == GoodIFs[0]:
                            MixedCalib[ant1][ant2] = []

                        # This is to store all fringes with linear-feeds involved:

                        if nchPlot > 0 and len(fringe) > 0:

                            uncal = [
                                (fringe[AntEntry]["MATRICES"])[:, i::12]
                                for i in range(4)
                            ]
                            cal = [
                                (fringe[AntEntry]["MATRICES"])[:, i::12]
                                for i in range(4, 8)
                            ]
                            Kmat = [
                                (fringe[AntEntry]["MATRICES"])[:, i::12]
                                for i in range(8, 12)
                            ]

                            rchan = np.shape(uncal[0])[0]

                            # Zoom for the image plots: a square centered on nchPlot and rchan:
                            if npix > 0:
                                ToZoom = min(nchPlot, npix, rchan)
                                print("ToZoom: ", rchan, nchPlot, npix, ToZoom)
                                t0 = (nchPlot - ToZoom) // 2
                                t1 = (nchPlot + ToZoom) // 2
                                Ch0 = (rchan - ToZoom) // 2
                                Ch1 = (rchan + ToZoom) // 2
                            else:
                                t0 = 0
                                t1 = -1
                                Ch0 = 0
                                Ch1 = -1

                            # Fringes in delay-rate space:
                            if ant2 == plotAnt or ant1 == plotAnt:
                                FRRu = np.fft.fftshift(np.fft.fft2(uncal[0]))
                                FRLu = np.fft.fftshift(np.fft.fft2(uncal[1]))
                                FLRu = np.fft.fftshift(np.fft.fft2(uncal[2]))
                                FLLu = np.fft.fftshift(np.fft.fft2(uncal[3]))

                                RRu = np.abs(FRRu)
                                RLu = np.abs(FRLu)
                                LRu = np.abs(FLRu)
                                LLu = np.abs(FLLu)

                                # Calibration matrix (in frequency-time space)

                                MAXK = max(list(map(np.max, list(map(np.abs, Kmat)))))
                                MINK = min(list(map(np.min, list(map(np.abs, Kmat)))))

                                # Peaks to scale plots:

                                RMAXu = np.unravel_index(
                                    np.argmax(RRu + RLu + LRu + LLu), np.shape(RRu)
                                )
                                MAXu = max(
                                    [RRu[RMAXu], RLu[RMAXu], LRu[RMAXu], LLu[RMAXu]]
                                )
                                MAXmix = [
                                    np.max(RRu),
                                    np.max(RLu),
                                    np.max(LRu),
                                    np.max(LLu),
                                ]
                                PEAK = np.unravel_index(
                                    np.argmax(RRu + LLu), np.shape(RRu)
                                )

                            RRVis = np.fft.fftshift(np.fft.fft2(cal[0]))
                            RLVis = np.fft.fftshift(np.fft.fft2(cal[1]))
                            LRVis = np.fft.fftshift(np.fft.fft2(cal[2]))
                            LLVis = np.fft.fftshift(np.fft.fft2(cal[3]))

                            RR = np.abs(RRVis)
                            RL = np.abs(RLVis)
                            LR = np.abs(LRVis)
                            LL = np.abs(LLVis)

                            # Peaks of converted fringes:
                            if DEBUG:
                                print("fringe maximum debugging:")
                            RMAX = np.unravel_index(np.argmax(RR + LL), np.shape(RRVis))
                            if DEBUG:
                                print(" unravel_index:", RMAX)
                            MAXVis = [
                                RRVis[RMAX],
                                RLVis[RMAX],
                                LRVis[RMAX],
                                LLVis[RMAX],
                            ]
                            if DEBUG:
                                print(" MAXVis:", MAXVis)
                            try:
                                MixedCalib[ant1][ant2].append(
                                    [np.array(MM) for MM in MAXVis]
                                )
                            except Exception as ex:
                                print("  MixedCalib np exception", str(ex))

                            MAXl = [RR[RMAX], RL[RMAX], LR[RMAX], LL[RMAX]]
                            if DEBUG:
                                print(" MAXl:", MAXl)
                            MAX = max(MAXl)
                            if DEBUG:
                                print(" Final MAX", MAX, "done")

                            # Plot fringes:
                            if ant2 == plotAnt or ant1 == plotAnt:

                                printMsg(
                                    "making Fringe.plot.ANT%i-%i.IF%i.png"
                                    % (ant1, ant2, pli)
                                )
                                fig.clf()
                                #   ratt = max(1.0,ToZoom/rchan)
                                ratt = "auto"

                                fig.subplots_adjust(
                                    left=0.02, right=0.98, wspace=0.05, hspace=0.2
                                )

                                try:  # to clear out previous plot stuff -- is this necessary?
                                    # answer: when there are MANY baselines and/or scans, yes.
                                    del sub0
                                    del sub1
                                    del sub2
                                    del sub3
                                    del sub4
                                    del sub5
                                    del sub6
                                    del sub7
                                    del sub
                                    del cbar
                                except:
                                    pass

                                sub0 = fig.add_subplot(241)
                                sub0.imshow(
                                    np.abs(RRu[Ch0:Ch1, t0:t1]),
                                    vmin=0.0,
                                    vmax=MAXu,
                                    interpolation="nearest",
                                    aspect=ratt,
                                )
                                sub0.set_title("XR mixed")
                                pl.setp(sub0.get_xticklabels(), visible=False)
                                pl.setp(sub0.get_yticklabels(), visible=False)

                                sub1 = fig.add_subplot(242, sharex=sub0, sharey=sub0)
                                sub1.imshow(
                                    np.abs(RLu[Ch0:Ch1, t0:t1]),
                                    vmin=0.0,
                                    vmax=MAXu,
                                    interpolation="nearest",
                                    aspect=ratt,
                                )
                                sub1.set_title("XL mixed")
                                pl.setp(sub1.get_xticklabels(), visible=False)
                                pl.setp(sub1.get_yticklabels(), visible=False)

                                sub2 = fig.add_subplot(243, sharex=sub0, sharey=sub0)
                                sub2.imshow(
                                    np.abs(RR[Ch0:Ch1, t0:t1]),
                                    vmin=0.0,
                                    vmax=MAX,
                                    interpolation="nearest",
                                    aspect=ratt,
                                )
                                sub2.set_title("RR cal")
                                pl.setp(sub2.get_xticklabels(), visible=False)
                                pl.setp(sub2.get_yticklabels(), visible=False)

                                sub3 = fig.add_subplot(244, sharex=sub0, sharey=sub0)
                                sub3.imshow(
                                    np.abs(RL[Ch0:Ch1, t0:t1]),
                                    vmin=0.0,
                                    vmax=MAX,
                                    interpolation="nearest",
                                    aspect=ratt,
                                )
                                sub3.set_title("RL cal")
                                pl.setp(sub3.get_xticklabels(), visible=False)
                                pl.setp(sub3.get_yticklabels(), visible=False)

                                sub4 = fig.add_subplot(245, sharex=sub0, sharey=sub0)
                                sub4.imshow(
                                    np.abs(LRu[Ch0:Ch1, t0:t1]),
                                    vmin=0.0,
                                    vmax=MAXu,
                                    interpolation="nearest",
                                    aspect=ratt,
                                )
                                sub4.set_title("YR mixed")
                                pl.setp(sub4.get_xticklabels(), visible=False)
                                pl.setp(sub4.get_yticklabels(), visible=False)

                                sub5 = fig.add_subplot(246, sharex=sub0, sharey=sub0)
                                sub5.imshow(
                                    np.abs(LLu[Ch0:Ch1, t0:t1]),
                                    vmin=0.0,
                                    vmax=MAXu,
                                    interpolation="nearest",
                                    aspect=ratt,
                                )
                                sub5.set_title("YL mixed")
                                pl.setp(sub5.get_xticklabels(), visible=False)
                                pl.setp(sub5.get_yticklabels(), visible=False)

                                sub6 = fig.add_subplot(247, sharex=sub0, sharey=sub0)
                                sub6.imshow(
                                    np.abs(LR[Ch0:Ch1, t0:t1]),
                                    vmin=0.0,
                                    vmax=MAX,
                                    interpolation="nearest",
                                    aspect=ratt,
                                )
                                sub6.set_title("LR cal")
                                pl.setp(sub6.get_xticklabels(), visible=False)
                                pl.setp(sub6.get_yticklabels(), visible=False)

                                sub7 = fig.add_subplot(248, sharex=sub0, sharey=sub0)
                                sub7.imshow(
                                    np.abs(LL[Ch0:Ch1, t0:t1]),
                                    vmin=0.0,
                                    vmax=MAX,
                                    interpolation="nearest",
                                    aspect=ratt,
                                )
                                sub7.set_title("LL cal")
                                pl.setp(sub7.get_xticklabels(), visible=False)
                                pl.setp(sub7.get_yticklabels(), visible=False)

                                fig.suptitle(
                                    "DELAY-RATE FRINGE FOR IF %i (BASELINE %s-%s) FROM %i-%02i:%02i:%02i TO %i-%02i:%02i:%02i"
                                    % tuple(
                                        [pli, antcodes[ant1 - 1], antcodes[ant2 - 1]]
                                        + plotRange
                                    )
                                )
                                fig.savefig(
                                    "FRINGE.PLOTS/Fringe.plot.ANT_%s-%s.IF%i.png"
                                    % (antcodes[ant1 - 1], antcodes[ant2 - 1], pli)
                                )

                                # Plot calibration matrix:

                                ratt = float(np.shape(Kmat[0])[1]) / float(
                                    np.shape(Kmat[0])[0]
                                )

                                printMsg(
                                    "creating Kmatrix_AMP_IF%i-ANT%i.png" % (pli, ant1)
                                )
                                fig2.clf()

                                fig2.subplots_adjust(right=0.8)
                                cbar_ax = fig2.add_axes([0.85, 0.15, 0.05, 0.7])

                                sub = fig2.add_subplot(221)
                                im = sub.imshow(
                                    np.abs(Kmat[0]),
                                    vmin=0.0,
                                    vmax=MAXK,
                                    interpolation="nearest",
                                    aspect=ratt,
                                )
                                pl.title(r"$K_{XR}$")
                                pl.setp(sub.get_xticklabels(), visible=False)
                                pl.setp(sub.get_yticklabels(), visible=False)

                                sub = fig2.add_subplot(222)
                                sub.imshow(
                                    np.abs(Kmat[1]),
                                    vmin=0.0,
                                    vmax=MAXK,
                                    interpolation="nearest",
                                    aspect=ratt,
                                )
                                pl.title(r"$K_{XL}$")
                                pl.setp(sub.get_xticklabels(), visible=False)
                                pl.setp(sub.get_yticklabels(), visible=False)

                                sub = fig2.add_subplot(223)
                                sub.imshow(
                                    np.abs(Kmat[2]),
                                    vmin=0.0,
                                    vmax=MAXK,
                                    interpolation="nearest",
                                    aspect=ratt,
                                )
                                pl.title(r"$K_{YR}$")
                                pl.setp(sub.get_xticklabels(), visible=False)
                                pl.setp(sub.get_yticklabels(), visible=False)

                                sub = fig2.add_subplot(224)
                                sub.imshow(
                                    np.abs(Kmat[3]),
                                    vmin=0.0,
                                    vmax=MAXK,
                                    interpolation="nearest",
                                    aspect=ratt,
                                )
                                pl.title(r"$K_{YL}$")
                                pl.setp(sub.get_xticklabels(), visible=False)
                                pl.setp(sub.get_yticklabels(), visible=False)

                                cbar = fig2.colorbar(im, cax=cbar_ax)
                                cbar.set_label("Amplitude (Norm)")
                                pl.suptitle(
                                    "CAL. MATRIX FOR IF %i FROM %i-%02i:%02i:%02i TO %i-%02i:%02i:%02i - FREQ = X ; TIME = Y"
                                    % tuple([pli] + plotRange)
                                )

                                pl.savefig(
                                    "CONVERSION.MATRIX/Kmatrix_AMP_IF%i-ANT%i.png"
                                    % (pli, ant1)
                                )

                                printMsg(
                                    "creating Kmatrix_PHAS_IF%i-ANT%i.png" % (pli, ant1)
                                )
                                fig2.clf()

                                fig2.subplots_adjust(right=0.8)
                                cbar_ax = fig2.add_axes([0.85, 0.15, 0.05, 0.7])

                                sub = fig2.add_subplot(221)
                                im = sub.imshow(
                                    180.0 / np.pi * np.angle(Kmat[0]),
                                    vmin=-180.0,
                                    vmax=180.0,
                                    interpolation="nearest",
                                    aspect=ratt,
                                )
                                pl.title(r"$K_{XR}$")
                                pl.setp(sub.get_xticklabels(), visible=False)
                                pl.setp(sub.get_yticklabels(), visible=False)

                                sub = fig2.add_subplot(222)
                                sub.imshow(
                                    180.0 / np.pi * np.angle(Kmat[1]),
                                    vmin=-180.0,
                                    vmax=180.0,
                                    interpolation="nearest",
                                    aspect=ratt,
                                )
                                pl.title(r"$K_{XL}$")
                                pl.setp(sub.get_xticklabels(), visible=False)
                                pl.setp(sub.get_yticklabels(), visible=False)

                                sub = fig2.add_subplot(223)
                                sub.imshow(
                                    180.0 / np.pi * np.angle(Kmat[2]),
                                    vmin=-180.0,
                                    vmax=180.0,
                                    interpolation="nearest",
                                    aspect=ratt,
                                )
                                pl.title(r"$K_{YR}$")
                                pl.setp(sub.get_xticklabels(), visible=False)
                                pl.setp(sub.get_yticklabels(), visible=False)

                                sub = fig2.add_subplot(224)
                                sub.imshow(
                                    180.0 / np.pi * np.angle(Kmat[3]),
                                    vmin=-180.0,
                                    vmax=180.0,
                                    interpolation="nearest",
                                    aspect=ratt,
                                )
                                pl.title(r"$K_{YL}$")
                                pl.setp(sub.get_xticklabels(), visible=False)
                                pl.setp(sub.get_yticklabels(), visible=False)

                                cbar = fig2.colorbar(im, cax=cbar_ax)
                                cbar.set_label("Phase (deg.)")
                                pl.suptitle(
                                    "CAL. MATRIX FOR IF %i FROM %i-%02i:%02i:%02i TO %i-%02i:%02i:%02i - FREQ = X ; TIME = Y"
                                    % tuple([pli] + plotRange)
                                )

                                pl.savefig(
                                    "CONVERSION.MATRIX/Kmatrix_PHAS_IF%i-ANT%i.png"
                                    % (pli, ant1)
                                )

                                # Estimate the Dynamic range:
                                DLL = np.max(LL) / np.std(
                                    np.sort(LL.flatten())[:-nchPlot]
                                )
                                DRR = np.max(RR) / np.std(
                                    np.sort(RR.flatten())[:-nchPlot]
                                )
                                DRL = np.max(RL) / np.std(
                                    np.sort(RL.flatten())[:-nchPlot]
                                )
                                DLR = np.max(LR) / np.std(
                                    np.sort(LR.flatten())[:-nchPlot]
                                )

                                DLLu = np.max(LLu) / np.std(
                                    np.sort(LLu.flatten())[:-nchPlot]
                                )
                                DRRu = np.max(RRu) / np.std(
                                    np.sort(RRu.flatten())[:-nchPlot]
                                )
                                DRLu = np.max(RLu) / np.std(
                                    np.sort(RLu.flatten())[:-nchPlot]
                                )
                                DLRu = np.max(LRu) / np.std(
                                    np.sort(LRu.flatten())[:-nchPlot]
                                )

                                RLRatio = (MAXl[0] / MAXl[3]) / (MAXl[2] / MAXl[1])

                                toprint = [
                                    antcodes[ant1 - 1],
                                    antcodes[ant2 - 1],
                                    pli,
                                    MAX,
                                    MAXl[0] / MAX,
                                    DRR,
                                    MAXl[3] / MAX,
                                    DLL,
                                    MAXl[1] / MAX,
                                    DRL,
                                    MAXl[2] / MAX,
                                    DLR,
                                    MAX / len(fringe),
                                    RLRatio,
                                ]
                                fringeAmps[ant1].append(
                                    [
                                        pli,
                                        MAXl[0],
                                        MAXl[0] / DRR,
                                        MAXl[3],
                                        MAXl[3] / DLL,
                                        MAXl[1],
                                        MAXl[1] / DRL,
                                        MAXl[2],
                                        MAXl[2] / DLR,
                                        RLRatio,
                                    ]
                                )
                                fringeAmpsMix[ant1].append(
                                    [
                                        pli,
                                        MAXmix[0],
                                        MAXmix[0] / DRRu,
                                        MAXmix[3],
                                        MAXmix[3] / DLLu,
                                        MAXmix[1],
                                        MAXmix[1] / DRLu,
                                        MAXmix[2],
                                        MAXmix[2] / DLRu,
                                    ]
                                )

                                pmsg = """BASELINE %s TO %s
  FOR IF #%i. NORM. FRINGE PEAKS (MAX OF %.3e ):  
     RR: %.2e ; SNR: %.1f
     LL: %.2e ; SNR: %.1f 
     RL: %.2e ; SNR: %.1f   
     LR: %.2e ; SNR: %.1f
     AMPLITUDE: %.2e  RL/LR Norm.: %.2e
  """

                                printMsg(
                                    "writing FRINGE.PEAKS_IF%i_SCAN_%i_%s-%s.dat"
                                    % (pli, NBF, antcodes[ant1 - 1], antcodes[ant2 - 1])
                                )
                                pfile = open(
                                    "FRINGE.PEAKS/FRINGE.PEAKS_IF%i_SCAN_%i_%s-%s.dat"
                                    % (
                                        pli,
                                        NBF,
                                        antcodes[ant1 - 1],
                                        antcodes[ant2 - 1],
                                    ),
                                    "w",
                                )

                                printMsg(pmsg % tuple(toprint))
                                pfile.write(pmsg % tuple(toprint))

                                NUM = np.angle(FRRu[RMAXu] * np.average(Kmat[2]))
                                DEN = np.angle(FLRu[RMAXu] * np.average(Kmat[3]))
                                optph = (180.0 / np.pi * ((NUM - DEN) - np.pi)) % 360.0
                                if optph > 180.0:
                                    optph -= 360.0
                                pmsg = "\n\nFor RL: optimum X/Y phase is %.1f deg." % (
                                    optph
                                )
                                NUM = np.angle(FRLu[RMAXu] * np.average(Kmat[0]))
                                DEN = np.angle(FLLu[RMAXu] * np.average(Kmat[1]))
                                optph = (180.0 / np.pi * ((NUM - DEN) - np.pi)) % 360.0
                                if optph > 180.0:
                                    optph -= 360.0
                                pmsg += "\nFor LR: optimum X/Y phase is %.1f deg.\n" % (
                                    optph
                                )

                                printMsg(pmsg)
                                pfile.write(pmsg)

                                printMsg(
                                    "wrote FRINGE.PEAKS_IF%i-ANT%i.dat" % (pli, ant1)
                                )
                            # end of if for this baseline to be plotted
                        # end of if len(fringe)>0
                        else:
                            printMsg("Fringe length was zero")

                    # end of np.sum(AntEntry)
                    else:
                        printMsg("Nothing to plot, apparently")
            # start of for ant1 in linAntIdx

            # end of fringe loop on ant1-ant2 baseline

            try:
                del uncal[3], uncal[2], uncal[1], uncal[0], uncal
                del cal[3], cal[2], cal[1], cal[0], cal
                del RRVis, RLVis, LRVis, LLVis, RR, LL, RL, LR
            except:
                pass

            if ant2 == plotAnt or ant1 == plotAnt:
                try:
                    del FRRu, FRLu, FLRu, FLLu
                    del RRu, RLu, LRu, LLu
                    del Kmat[3], Kmat[2], Kmat[1], Kmat[0], Kmat
                except:
                    pass
            try:
                del fringe
            except:
                pass
            gc.collect()
            # end of if ant1 in linAntIdx

        #  try:
        for thisAnt in linAntIdxTrue:
            if len(fringeAmps[thisAnt]) > 0:
                fig.clf()
                pl.figure(fig.number)
                fig.subplots_adjust(left=0.1, right=0.95, bottom=0.15, top=0.95)
                sub = fig.add_subplot(121)
                # CIRCULAR-CIRCULAR FRINGE AMPLITUDES (MUST NORMALIZE FROM FFT):
                CONVAMP = np.array(fringeAmps[thisAnt])
                CONVAMP[:, 1:-1] *= 1.0e3 / (rchan * nchPlot)
                # MIXED FRINGE APLITUDES (MUST NORMALIZE FROM FFT):
                MIXAMP = np.array(fringeAmpsMix[thisAnt])
                MIXAMP[:, 1:] *= 1.0e3 / (rchan * nchPlot)

                sub.plot(MIXAMP[:, 0], MIXAMP[:, 1], "sr", label="XR", markersize=15)
                sub.plot(MIXAMP[:, 0], MIXAMP[:, 3], "dg", label="YL", markersize=15)
                sub.plot(MIXAMP[:, 0], MIXAMP[:, 5], "+r", label="XL", markersize=15)
                sub.plot(MIXAMP[:, 0], MIXAMP[:, 7], "xg", label="YR", markersize=15)
                pl.legend(numpoints=1)

                sub.errorbar(
                    MIXAMP[:, 0], MIXAMP[:, 1], MIXAMP[:, 2], linestyle="None", fmt="k"
                )
                sub.errorbar(
                    MIXAMP[:, 0], MIXAMP[:, 3], MIXAMP[:, 4], linestyle="None", fmt="k"
                )
                sub.errorbar(
                    MIXAMP[:, 0], MIXAMP[:, 5], MIXAMP[:, 6], linestyle="None", fmt="k"
                )
                sub.errorbar(
                    MIXAMP[:, 0], MIXAMP[:, 7], MIXAMP[:, 8], linestyle="None", fmt="k"
                )

                pl.xlabel("IF NUMBER")
                pl.ylabel(r"AMP (CORR. $\times 10^3$)")
                sub2 = fig.add_subplot(122, sharex=sub, sharey=sub)
                sub2.plot(CONVAMP[:, 0], CONVAMP[:, 1], "sr", label="RR", markersize=15)
                sub2.plot(CONVAMP[:, 0], CONVAMP[:, 3], "dg", label="LL", markersize=15)
                sub2.plot(CONVAMP[:, 0], CONVAMP[:, 5], "+r", label="RL", markersize=15)
                sub2.plot(CONVAMP[:, 0], CONVAMP[:, 7], "xg", label="LR", markersize=15)
                pl.legend(numpoints=1)

                sub2.errorbar(
                    CONVAMP[:, 0],
                    CONVAMP[:, 1],
                    CONVAMP[:, 2],
                    linestyle="None",
                    fmt="k",
                )
                sub2.errorbar(
                    CONVAMP[:, 0],
                    CONVAMP[:, 3],
                    CONVAMP[:, 4],
                    linestyle="None",
                    fmt="k",
                )
                sub2.errorbar(
                    CONVAMP[:, 0],
                    CONVAMP[:, 5],
                    CONVAMP[:, 6],
                    linestyle="None",
                    fmt="k",
                )
                sub2.errorbar(
                    CONVAMP[:, 0],
                    CONVAMP[:, 7],
                    CONVAMP[:, 8],
                    linestyle="None",
                    fmt="k",
                )
                pl.setp(sub2.get_yticklabels(), visible=False)
                pl.xlabel("IF NUMBER")
                dChan = max(plotIF) - min(plotIF) + 1
                pl.xlim((min(plotIF) - dChan * 0.2, max(plotIF) + 0.4 * dChan))
                pl.ylim((0.0, 1.1 * np.max(CONVAMP[:, [1, 3, 5, 7]])))
                fig.suptitle(jobLabel(DiFXinput) + " ANT: %i v %i" % (thisAnt, plotAnt))
                fig.savefig(
                    "FRINGE.PLOTS/ALL_IFs_ANT_%i_%i%s.png"
                    % (thisAnt, plotAnt, plotSuffix)
                )

                fig3 = pl.figure()
                sub1 = fig3.add_subplot(111)
                sub1.plot(CONVAMP[:, 0], CONVAMP[:, -1], "sk")
                RatioError = CONVAMP[:, -1] * np.sqrt(
                    (CONVAMP[:, 2] / CONVAMP[:, 1]) ** 2.0
                    + (CONVAMP[:, 4] / CONVAMP[:, 3]) ** 2.0
                    + (CONVAMP[:, 6] / CONVAMP[:, 5]) ** 2.0
                    + (CONVAMP[:, 8] / CONVAMP[:, 7]) ** 2.0
                )
                sub1.errorbar(
                    CONVAMP[:, 0], CONVAMP[:, -1], RatioError, linestyle="None", fmt="k"
                )
                sub1.plot([min(CONVAMP[:, 0]) - 1, max(CONVAMP[:, 0]) + 1], [1, 1], "r")

                pl.xlabel("IF NUMBER")
                pl.ylabel("(RR/LL)/(LR/RL)")
                pl.xlim((min(CONVAMP[:, 0]) - 1, max(CONVAMP[:, 0]) + 1))

                uylim = np.max(CONVAMP[:, -1])
                if np.isfinite(uylim):
                    pl.ylim((0, uylim * 1.02))
                else:
                    pl.ylim((0, 2))

                fig3.suptitle(
                    jobLabel(DiFXinput) + " ANT: %i v %i" % (thisAnt, plotAnt)
                )
                fig3.savefig(
                    "FRINGE.PLOTS/RL_LR_RATIOS_ANT_%i_%i%s.png"
                    % (thisAnt, plotAnt, plotSuffix)
                )

    else:

        printMsg("NO DATA TO PLOT!")
    # end of plotting

    printMsg("Please, check the PolConvert.log file for special messages.", dolog=False)

    if doSolve >=0.0:
        if sys.version_info.major < 3:
            ofile = open("PolConvert.XYGains%s.dat" % plotSuffix, "w")
        else:
            ofile = open("PolConvert.XYGains%s.dat" % plotSuffix, "wb")

        cgs = str(CGains)
    # if len(cgs) > 79: printMsg("%s..." % cgs[0:78])
    # else:             printMsg("%s" % cgs)

        try:
            pk.dump(CGains, ofile)
        except Exception as ex:
            printMsg(str(ex))
        ofile.close()
    #  printMsg('PolConvert.XYGains.dat was written with CGains' + str(CGains.keys()))

    printMsg("Task PolConvert is Done\n\n")
    return CGains  # RETURN!


#
# eof
#

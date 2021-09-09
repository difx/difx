# -*- coding: utf-8 -*-
#===========================================================================
# Copyright (C) 2019  Max-Planck-Institut für Radioastronomie, Bonn, Germany
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#===========================================================================
# SVN properties (DO NOT CHANGE)
#
# $Id$
# $HeadURL: $
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#============================================================================

import glob, struct, sys

from .Config import Config
from .Freq import Freq
from .Telescope import Telescope
from .Baseline import Baseline
from .Datastream import Datastream

M_SYNC_WORD = 0xFF00FF00

def parse_output_header(input):
    toreturn = []
    orgbuffer = b''
    buffer = input.read(4)
    if len(buffer) < 4:
        return toreturn
    if struct.unpack("I", buffer)[0] != M_SYNC_WORD:
        if buffer != "BASE": #Some weird stuff.  Return empty
            print(("Non-recognised sync word: ascii " + buffer + ", binary %x" % (struct.unpack("I", buffer)[0])))
            return toreturn
        #Must be the old style file.  Suck it up.
        orgbuffer += buffer
        buffer = input.readline()
        orgbuffer += buffer
        toreturn.append(int((buffer.split(':')[1]).strip())) #baselinenum
        buffer = input.readline()
        orgbuffer += buffer
        toreturn.append(int((buffer.split(':')[1]).strip())) #mjd
        buffer = input.readline()
        orgbuffer += buffer
        toreturn.append(float((buffer.split(':')[1]).strip())) #seconds
        buffer = input.readline()
        orgbuffer += buffer
        toreturn.append(int((buffer.split(':')[1]).strip())) #configindex
        buffer = input.readline()
        orgbuffer += buffer
        toreturn.append(int((buffer.split(':')[1]).strip())) #srcindex
        buffer = input.readline()
        orgbuffer += buffer
        toreturn.append(int((buffer.split(':')[1]).strip())) #freqindex
        buffer = input.readline()
        orgbuffer += buffer
        binarypolpair = buffer.split(':')[1].strip() #polpair
        toreturn.append(chr(binarypolpair[0]) + chr(binarypolpair[1]))
        buffer = input.readline()
        orgbuffer += buffer
        toreturn.append(int((buffer.split(':')[1]).strip())) #pulsarbin
        buffer = input.readline()
        orgbuffer += buffer
        buffer = input.readline() #Skip over flagged
        buffer = input.readline()
        toreturn.append(float((buffer.split(':')[1]).strip())) #dataweight
        buffer = input.readline()
        orgbuffer += buffer
        toreturn.append(float((buffer.split(':')[1]).strip())) #u
        buffer = input.readline()
        orgbuffer += buffer
        toreturn.append(float((buffer.split(':')[1]).strip())) #v
        buffer = input.readline()
        orgbuffer += buffer
        toreturn.append(float((buffer.split(':')[1]).strip())) #w
    else:
        #It is the new style file.  Hooray.
        orgbuffer += buffer
        buffer = input.read(4)
        orgbuffer += buffer
        if struct.unpack("i", buffer)[0] != 1:
            #Don't know how to unpack this version - return empty
            return toreturn
        #Ok, we can deal with this
        buffer = input.read(4)
        orgbuffer += buffer
        toreturn.append(struct.unpack("i", buffer)[0]) #baselinenum
        buffer = input.read(4)
        orgbuffer += buffer
        toreturn.append(struct.unpack("i", buffer)[0]) #mjd
        buffer = input.read(8)
        orgbuffer += buffer
        toreturn.append(struct.unpack("d", buffer)[0]) #seconds
        buffer = input.read(4)
        orgbuffer += buffer
        toreturn.append(struct.unpack("i", buffer)[0]) #configindex
        buffer = input.read(4)
        orgbuffer += buffer
        toreturn.append(struct.unpack("i", buffer)[0]) #srcindex
        buffer = input.read(4)
        orgbuffer += buffer
        toreturn.append(struct.unpack("i", buffer)[0]) #freqindex
        buffer = input.read(2)
        orgbuffer += buffer
        toreturn.append(buffer.decode("utf-8"))        #polpair
        buffer = input.read(4)
        orgbuffer += buffer
        toreturn.append(struct.unpack("i", buffer)[0]) #pulsarbin
        buffer = input.read(8)
        orgbuffer += buffer
        toreturn.append(struct.unpack("d", buffer)[0]) #dataweight
        buffer = input.read(8)
        orgbuffer += buffer
        toreturn.append(struct.unpack("d", buffer)[0]) #u
        buffer = input.read(8)
        orgbuffer += buffer
        toreturn.append(struct.unpack("d", buffer)[0]) #v
        buffer = input.read(8)
        orgbuffer += buffer
        toreturn.append(struct.unpack("d", buffer)[0]) #w
    toreturn.append(orgbuffer)
    return toreturn 

def make_output_header_v1(hdrstruct):
    fmt_part1 = "iidiii" # baselinenum, mjd, seconds, configindex, srcindex, freqindex
    fmt_part2 = "idddd" # pulsarbin, dataweight, u, v, w
    packed = [struct.pack('I', M_SYNC_WORD)]
    packed.append(struct.pack('i', 1))
    for n in range(6):
        packed.append(struct.pack(fmt_part1[n], hdrstruct[n]))
    packed.append(hdrstruct[6].encode('utf-8')) # polpair
    for n in range(7,12):
        packed.append(struct.pack(fmt_part2[n-7], hdrstruct[n]))
    binhdr = b''.join(packed)
    return binhdr

def nextinputline(inputlines):
    while len(inputlines) > 0:
        if len(inputlines[0]) == 0:
            inputlines = inputlines[1:]
            continue
        if inputlines[0][0] == '@':
            inputlines = inputlines[1:]
            continue
        break
    if len(inputlines) == 0:
        print ("Hit end of input file while parsing??")
        sys.exit()
    sepindex = inputlines[0].find(':') + 1
    if sepindex < 20:
        sepindex = 20
    val = inputlines[0][sepindex:]
    return val[:-1], inputlines

def get_common_settings(inputfile):
    input = open(inputfile)
    lines = input.readlines()
    input.close()

    at = 0
    while at < len(lines) and lines[at] != "# COMMON SETTINGS ##!\n":
        at += 1
    at += 1

    settings = {}
    val, lines = nextinputline(lines[at:])
    settings['calcfile'] = val
    val, lines = nextinputline(lines[1:])
    settings['corefile'] = val
    val, lines = nextinputline(lines[1:])
    settings['exectime'] = float(val)
    val, lines = nextinputline(lines[1:])
    settings['startmjd'] = int(val)
    val, lines = nextinputline(lines[1:])
    settings['startsec'] = int(val)
    val, lines = nextinputline(lines[1:])
    settings['datastreams'] = int(val)
    val, lines = nextinputline(lines[1:])
    settings['baselines'] = int(val)
    val, lines = nextinputline(lines[1:])
    settings['visbuflen'] = int(val)
    val, lines = nextinputline(lines[1:])
    settings['outformat'] = val
    val, lines = nextinputline(lines[1:])
    settings['difxfile'] = val
    return settings

def get_telescopetable_info(inputfile):
    input = open(inputfile)
    lines = input.readlines()
    input.close()

    at = 0
    while at < len(lines) and lines[at] != "# TELESCOPE TABLE ##!\n":
        at += 1

    at += 1
    val, lines = nextinputline(lines[at:])
    numtelescopes = int(val)
    telescopes = []
    for i in range(numtelescopes):
        telescopes.append(Telescope())
        val, lines = nextinputline(lines[1:])
        telescopes[-1].name = val
        val, lines = nextinputline(lines[1:])
        telescopes[-1].clockrefmjd = float(val)
        val, lines = nextinputline(lines[1:])
        telescopes[-1].clockorder = int(val)
        telescopes[-1].clockcoeff = []
        for i in range(telescopes[-1].clockorder + 1):
            val, lines = nextinputline(lines[1:])
            telescopes[-1].clockcoeff.append(float(val))

    return (numtelescopes, telescopes)

def get_baselinetable_info(inputfile):
    input = open(inputfile)
    lines = input.readlines()
    input.close()

    at = 0
    while at < len(lines) and lines[at] != "# BASELINE TABLE ###!\n":
        at += 1

    val, lines = nextinputline(lines[at+1:])
    numbaselines = int(val)
    baselines = []
    for i in range(numbaselines):
        baselines.append(Baseline())
        val, lines = nextinputline(lines[1:])
        baselines[-1].dsaindex = int(val)
        val, lines = nextinputline(lines[1:])
        baselines[-1].dsbindex = int(val)
        val, lines = nextinputline(lines[1:])
        baselines[-1].nfreq = int(val)
        baselines[-1].destfreq = []
        baselines[-1].freqpols = []
        baselines[-1].dsabandindex = []
        baselines[-1].dsbbandindex = []
        for j in range(baselines[-1].nfreq):
            baselines[-1].dsabandindex.append([])
            baselines[-1].dsbbandindex.append([])
            val, lines = nextinputline(lines[1:])
            if 'TARGET FREQ' in lines[0]:
                baselines[-1].destfreq.append(int(val))
                baselines[-1].version = 2.7
                val, lines = nextinputline(lines[1:])
            baselines[-1].freqpols.append(int(val))
            for k in range(baselines[-1].freqpols[-1]):
                val, lines = nextinputline(lines[1:])
                baselines[-1].dsabandindex[-1].append(int(val))
                val, lines = nextinputline(lines[1:])
                baselines[-1].dsbbandindex[-1].append(int(val))
    return (numbaselines, baselines)

def put_baselinetable_info(fo,bl):
    fo.write("# BASELINE TABLE ###!\n");
    fo.write("%-20s%d\n" % ("BASELINE ENTRIES:",len(bl)))
    for n in range(len(bl)):
        b = bl[n]
        fo.write("%-20s%d\n" % ("D/STREAM A INDEX %d:"%(n),b.dsaindex))
        fo.write("%-20s%d\n" % ("D/STREAM B INDEX %d:"%(n),b.dsbindex))
        fo.write("%-20s%d\n" % ("NUM FREQS %d:"%(n),len(b.freqpols)))
        for f in range(b.nfreq):
            fo.write("%-20s%d\n" % ("POL PRODUCTS %d/%d:"%(n,f),b.freqpols[f]))
            for k in range(b.freqpols[f]):
                fo.write("%-20s%d\n" % ("D/STREAM A BAND %d:"%(k),b.dsabandindex[f][k]))
                fo.write("%-20s%d\n" % ("D/STREAM B BAND %d:"%(k),b.dsbbandindex[f][k]))
    fo.write("\n")

def get_datastreamtable_info(inputfile):
    input = open(inputfile)
    lines = input.readlines()
    input.close()

    at = 0
    while at < len(lines) and lines[at] != "# DATASTREAM TABLE #!\n":
        at += 1

    val, lines = nextinputline(lines[at+1:])
    numdatastreams = int(val)
    datastreams = []
    lines = lines[2:]
    for i in range(numdatastreams):
        datastreams.append(Datastream())
        val, lines = nextinputline(lines[1:])
        datastreams[-1].telescopeindex = int(val)
        val, lines = nextinputline(lines[1:])
        datastreams[-1].tsys = float(val)
        val, lines = nextinputline(lines[1:])
        datastreams[-1].format = val
        val, lines = nextinputline(lines[1:])
        datastreams[-1].quantbits = int(val)
        val, lines = nextinputline(lines[1:])
        datastreams[-1].framesize = int(val)
        val, lines = nextinputline(lines[1:])
        datastreams[-1].datasampling = val
        val, lines = nextinputline(lines[1:])
        datastreams[-1].datasource = val
        val, lines = nextinputline(lines[1:])
        datastreams[-1].filterbankused = (val == "TRUE")
        if "TCAL" in lines[1]:
            lines = lines[1:]
        val, lines = nextinputline(lines[1:])
        datastreams[-1].phasecalint = int(val)
        val, lines = nextinputline(lines[1:])
        datastreams[-1].nrecfreq = int(val)
        datastreams[-1].recfreqpols = []
        datastreams[-1].recfreqindex = []
        datastreams[-1].recclockoffset = []
        datastreams[-1].recfreqoffset = []
        datastreams[-1].recgainoffset = []
        datastreams[-1].nrecband = 0
        for j in range(datastreams[-1].nrecfreq):
            val, lines = nextinputline(lines[1:])
            datastreams[-1].recfreqindex.append(int(val))
            val, lines = nextinputline(lines[1:])
            if ':' not in val:
                datastreams[-1].recclockoffset.append(float(val))
            else:
                # extended 'f1p1:f1p2delta' syntax, ignore the delta -- TODO: store it somewhere?
                datastreams[-1].recclockoffset.append(float(val.split(':')[0]))
            val, lines = nextinputline(lines[1:])
            datastreams[-1].recfreqoffset.append(float(val))
            val, lines = nextinputline(lines[1:])
            if 'GAIN OFFSET' in lines[0]:
                datastreams[-1].version = 2.7
                datastreams[-1].recgainoffset.append(float(val))
                val, lines = nextinputline(lines[1:])
            datastreams[-1].recfreqpols.append(int(val))
            datastreams[-1].nrecband  += datastreams[-1].recfreqpols[-1]
        datastreams[-1].recbandpol = []
        datastreams[-1].recbandindex = []
        for j in range(datastreams[-1].nrecband):
            val, lines = nextinputline(lines[1:])
            datastreams[-1].recbandpol.append(val)
            val, lines = nextinputline(lines[1:])
            datastreams[-1].recbandindex.append(int(val))
        val, lines = nextinputline(lines[1:])
        datastreams[-1].zoomfreqpols = []
        datastreams[-1].zoomfreqindex = []
        datastreams[-1].nzoomband = 0
        datastreams[-1].nzoomfreq = int(val)
        for j in range(datastreams[-1].nzoomfreq):
            val, lines = nextinputline(lines[1:])
            datastreams[-1].zoomfreqindex.append(int(val))
            val, lines = nextinputline(lines[1:])
            datastreams[-1].zoomfreqpols.append(int(val))
            datastreams[-1].nzoomband  += datastreams[-1].zoomfreqpols[-1]
        datastreams[-1].zoombandpol = []
        datastreams[-1].zoombandindex = []
        for j in range(datastreams[-1].nzoomband):
            val, lines = nextinputline(lines[1:])
            datastreams[-1].zoombandpol.append(val)
            val, lines = nextinputline(lines[1:])
            datastreams[-1].zoombandindex.append(int(val))
    return(numdatastreams, datastreams)


def put_datastreamtable_info(fo,ds):
    fo.write("# DATASTREAM TABLE #!\n")
    fo.write("%-20s%d\n" % ("DATASTREAM ENTRIES:",len(ds)))
    fo.write("DATA BUFFER FACTOR: 32\n")
    fo.write("NUM DATA SEGMENTS:  8\n")
    for d in ds:
        fo.write("%-20s%d\n" % ("TELESCOPE INDEX:",d.telescopeindex))
        fo.write("%-20s%f\n" % ("TSYS:",d.tsys))
        fo.write("%-20s%s\n" % ("DATA FORMAT:",d.format))
        fo.write("%-20s%d\n" % ("QUANTISATION BITS:",d.quantbits))
        fo.write("%-20s%d\n" % ("DATA FRAME SIZE:",d.framesize))
        fo.write("%-20s%s\n" % ("DATA SAMPLING:",d.datasampling))
        fo.write("%-20s%s\n" % ("DATA SOURCE:",d.datasource))
        fo.write("%-20s%s\n" % ("FILTERBANK USED:","FALSE")) # TODO
        fo.write("%-20s%d\n" % ("PHASE CAL INT (MHZ):",d.phasecalint))
        fo.write("%-20s%d\n" % ("NUM RECORDED FREQS:",d.nrecfreq))
        for n in range(d.nrecfreq):
            fo.write("%-20s%d\n" % ("REC FREQ INDEX %d:"%(n),d.recfreqindex[n]))
            fo.write("%-20s%f\n" % ("CLK OFFSET %d (us):"%(n),d.recclockoffset[n]))
            fo.write("%-20s%f\n" % ("FREQ OFFSET %d (Hz):"%(n),d.recfreqoffset[n]))
            fo.write("%-20s%d\n" % ("NUM REC POLS %d:"%(n),d.recfreqpols[n]))
        for n in range(d.nrecband):
            fo.write("%-20s%s\n" % ("REC BAND %d POL:"%(n),d.recbandpol[n]))
            fo.write("%-20s%d\n" % ("REC BAND %d INDEX:"%(n),d.recbandindex[n]))
        fo.write("%-20s%d\n" % ("NUM ZOOM FREQS:",d.nzoomfreq))
        for n in range(d.nzoomfreq):
            fo.write("%-20s%d\n" % ("ZOOM FREQ INDEX %d:"%(n),d.zoomfreqindex[n]))
            fo.write("%-20s%d\n" % ("NUM ZOOM POLS %d:"%(n),d.zoomfreqpols[n]))
        for n in range(d.nzoomband):
            fo.write("%-20s%s\n" % ("ZOOM BAND %d POL:"%(n),d.zoombandpol[n]))
            fo.write("%-20s%d\n" % ("ZOOM BAND %d INDEX:"%(n),d.zoombandindex[n]))
    fo.write("\n")

def get_configtable_info(inputfile):
    input = open(inputfile)
    lines = input.readlines()
    input.close()

    at = 0
    while at < len(lines) and lines[at] != "# CONFIGURATIONS ###!\n":
        at += 1

    if at == len(lines):
        return (0, [])

    numconfigs = int(lines[at+1][20:])
    configs = []
    at += 2
    for i in range(numconfigs):
        configs.append(Config())
        configs[-1].name    = lines[at+0][20:]
        configs[-1].inttime = float(lines[at+1][20:])
        configs[-1].subintns= int(lines[at+2][20:])
        configs[-1].guardns = int(lines[at+3][20:])
        configs[-1].fringerotorder = int(lines[at+4][20:])
        configs[-1].arraystridelen = int(lines[at+5][20:])
        configs[-1].xmacstridelen  = int(lines[at+6][20:])
        configs[-1].numbufferedFFTs= int(lines[at+7][20:])
        if i < numconfigs-1:
            at += 11
            while not "CONFIG NAME:" in lines[at]:
                at += 1

    return (numconfigs, configs)

def get_freqtable_info(inputfile):
    input = open(inputfile)
    lines = input.readlines()
    input.close()

    at = 0
    while at < len(lines) and lines[at] != "# FREQ TABLE #######!\n":
        at += 1

    if at == len(lines):
        return (0, [])

    numfreqs = int(lines[at+1][20:])
    freqs = []
    at += 2
    for i in range(numfreqs):
        freqs.append(Freq())
        freqs[-1].freq = float(lines[at+0][20:])
        freqs[-1].bandwidth = float(lines[at+1][20:])
        if lines[at+2][20:21] == 'L':
            freqs[-1].lsb = True
        freqs[-1].numchan = int(lines[at+3][20:])
        freqs[-1].specavg = int(lines[at+4][20:])
        freqs[-1].oversamplefac = int(lines[at+5][20:])
        freqs[-1].decimfac = int(lines[at+6][20:])
        di = lines[at+7].rfind(':')
        freqs[-1].npcal = int(lines[at+7][di+1:])
        for p in range(freqs[-1].npcal):
            di = lines[at+8+p].rfind(':')
            freqs[-1].pcalindices.append(int(lines[at+8+p][di+1:]))
        at += 8 + freqs[-1].npcal
    return (numfreqs, freqs)

def put_freqtable_info(fo,freqs):
    fo.write("# FREQ TABLE #######!\n")
    fo.write("%-20s%d\n" % ("FREQ ENTRIES:",len(freqs)))
    for i in range(len(freqs)):
        f = freqs[i]
        fo.write("%-20s%.11f\n" % ("FREQ (MHZ) %d:"%(i),f.freq))
        fo.write("%-20s%.11f\n" % ("BW (MHZ) %d:"%(i),f.bandwidth))
        if f.lsb:
            fo.write("%-20sL\n" % ("SIDEBAND %d:"%(i)))
        else:
            fo.write("%-20sU\n" % ("SIDEBAND %d:"%(i)))
        fo.write("%-20s%d\n" % ("NUM CHANNELS %d:"%(i),f.numchan))
        fo.write("%-20s%d\n" % ("CHANS TO AVG %d:"%(i),f.specavg))
        fo.write("%-20s%d\n" % ("OVERSAMPLE FAC. %d:"%(i),f.oversamplefac))
        fo.write("%-20s%d\n" % ("DECIMATION FAC. %d:"%(i),f.decimfac))
        fo.write("%-20s%d\n" % ("PHASE CALS %d OUT:"%(i),f.npcal))
        for p in range(f.npcal):
            fo.write("%-20s%d\n" % ("PHASE CAL %d/%d INDEX:"%(i,p),f.pcalindices[p]))
    fo.write("\n")

import struct, sys

SYNC_WORD = 0xFF00FF00

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
        print "Hit end of input file while parsing??"
        sys.exit()
    sepindex = inputlines[0].find(':') + 1
    if sepindex < 20:
        sepindex = 20
    val = inputlines[0][sepindex:]
    return val[:-1], inputlines

class Config:
    name = ""
    inttime = 0.0
    subintns = 0
    guardns = 0
    fringerotorder = 0
    arraystridelen = 0
    xmacstridelen = 0
    numbufferedffts = 0

class Freq:
    bandwidth = 0.0
    freq = 0.0
    lsb = False
    numchan = 0
    specavg = 0
    oversamplefac = 1
    decimfac = 1
    npcal = 0
    pcalindices = []

class Telescope:
    name = ""
    clockrefmjd = 55000.0
    clockorder = 0
    clockcoeff = [0]

class Datastream:
    telescopeindex = 0
    tsys = 0
    format = ""
    quantbits = 0
    framesize = 0
    datasampling = ""
    datasource = ""
    filterbankused = False
    phasecalint = 0
    nrecfreq = 0
    recfreqpols = []
    recfreqindex = []
    recclockoffset = []
    recfreqoffset = []
    nrecband = 0
    recbandpol = []
    recbandindex = []
    nzoomfreq = 0
    zoomfreqpols = []
    zoomfreqindex = []
    nzoomband = 0
    zoombandpol = []
    zoombandindex = []


class Baseline:
    dsaindex = 0
    dsbindex = 0
    nfreq = 0
    freqpols = []
    dsabandindex = []
    dsbbandindex = []

def parse_output_header(input):
    toreturn = []
    orgbuffer = ""
    buffer = input.read(4)
    if buffer == "":
        return toreturn
    if struct.unpack("I", buffer)[0] != SYNC_WORD:
        if buffer != "BASE": #Some weird stuff.  Return empty
            print "Non-recognised sync word: ascii " + buffer + ", binary %x" % (struct.unpack("I", buffer)[0])
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
        toreturn.append((buffer.split(':')[1].strip())) #polpair
	buffer = input.readline()
        orgbuffer += buffer
        toreturn.append(int((buffer.split(':')[1]).strip())) #pulsarbin
	buffer = input.readline()
        orgbuffer += buffer
        buffer = input.readline() #Skip over flagged
	orgbuffer += buffer
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
        toreturn.append(buffer)                        #polpair
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
    packed = [struct.pack('I', SYNC_WORD)]
    packed.append(struct.pack('i', 1))
    for n in range(6):
        packed.append(struct.pack(fmt_part1[n], hdrstruct[n]))
    packed.append(hdrstruct[6]) # polpair
    for n in range(7,12):
        packed.append(struct.pack(fmt_part2[n-7], hdrstruct[n]))
    binhdr = b''.join(packed)
    return binhdr

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
        baselines[-1].freqpols = []
        baselines[-1].dsabandindex = []
        baselines[-1].dsbbandindex = []
        for j in range(baselines[-1].nfreq):
            baselines[-1].dsabandindex.append([])
            baselines[-1].dsbbandindex.append([])
            val, lines = nextinputline(lines[1:])
            baselines[-1].freqpols.append(int(val))
            for k in range(baselines[-1].freqpols[-1]):
                val, lines = nextinputline(lines[1:])
                baselines[-1].dsabandindex[-1].append(int(val))
                val, lines = nextinputline(lines[1:])
                baselines[-1].dsbbandindex[-1].append(int(val))
    return (numbaselines, baselines)

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
        datastreams[-1].nrecband = 0
        for j in range(datastreams[-1].nrecfreq):
            val, lines = nextinputline(lines[1:])
            datastreams[-1].recfreqindex.append(int(val))
            val, lines = nextinputline(lines[1:])
            datastreams[-1].recclockoffset.append(float(val))
            val, lines = nextinputline(lines[1:])
            datastreams[-1].recfreqoffset.append(float(val))
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
	freqs[-1].npcal = int(lines[at+7][20:])
        for p in range(freqs[-1].npcal):
	    freqs[-1].pcalindices.append(int(lines[at+8+p][20:]))
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


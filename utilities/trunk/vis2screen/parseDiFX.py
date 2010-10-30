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

class Freq:
    bandwidth = 0.0
    freq = 0.0
    lsb = False
    numchan = 0
    specavg = 0

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
    buffer = input.read(4)
    if buffer == "":
        return toreturn
    if struct.unpack("I", buffer)[0] != SYNC_WORD:
        if buffer != "BASE": #Some weird stuff.  Return empty
            print "Non-recognised sync word: ascii " + buffer + ", binary %x" % (struct.unpack("I", buffer)[0])
            return toreturn
        #Must be the old style file.  Suck it up.
        toreturn.append(int((input.readline().split(':')[1]).strip())) #baselinenum
        toreturn.append(int((input.readline().split(':')[1]).strip())) #mjd
        toreturn.append(float((input.readline().split(':')[1]).strip())) #seconds
        toreturn.append(int((input.readline().split(':')[1]).strip())) #configindex
        toreturn.append(int((input.readline().split(':')[1]).strip())) #srcindex
        toreturn.append(int((input.readline().split(':')[1]).strip())) #freqindex
        toreturn.append((input.readline().split(':')[1].strip())) #polpair
        toreturn.append(int((input.readline().split(':')[1]).strip())) #pulsarbin
        input.readline() #Skip over flagged
        toreturn.append(float((input.readline().split(':')[1]).strip())) #dataweight
        toreturn.append(float((input.readline().split(':')[1]).strip())) #u
        toreturn.append(float((input.readline().split(':')[1]).strip())) #v
        toreturn.append(float((input.readline().split(':')[1]).strip())) #w
    else:
        #It is the new style file.  Hooray.
        buffer = input.read(4)
        if struct.unpack("i", buffer)[0] != 1:
            #Don't know how to unpack this version - return empty
            return toreturn
        #Ok, we can deal with this
        buffer = input.read(4)
        toreturn.append(struct.unpack("i", buffer)[0]) #baselinenum
        buffer = input.read(4)
        toreturn.append(struct.unpack("i", buffer)[0]) #mjd
        buffer = input.read(8)
        toreturn.append(struct.unpack("d", buffer)[0]) #seconds
        buffer = input.read(4)
        toreturn.append(struct.unpack("i", buffer)[0]) #configindex
        buffer = input.read(4)
        toreturn.append(struct.unpack("i", buffer)[0]) #srcindex
        buffer = input.read(4)
        toreturn.append(struct.unpack("i", buffer)[0]) #freqindex
        buffer = input.read(2)
        toreturn.append(buffer)                        #polpair
        buffer = input.read(4)
        toreturn.append(struct.unpack("i", buffer)[0]) #pulsarbin
        buffer = input.read(8)
        toreturn.append(struct.unpack("d", buffer)[0]) #dataweight
        buffer = input.read(8)
        toreturn.append(struct.unpack("d", buffer)[0]) #u
        buffer = input.read(8)
        toreturn.append(struct.unpack("d", buffer)[0]) #v
        buffer = input.read(8)
        toreturn.append(struct.unpack("d", buffer)[0]) #w
    return toreturn 

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
    lines = lines[1:]
    for i in range(numbaselines):
        baselines.append(Baseline())
        val, lines = nextinputline(lines[1:])
        baselines[-1].dsaindex = int(val)
        val, lines = nextinputline(lines[1:])
        baselines[-1].dsbindex = int(val)
        val, lines = nextinputline(lines[1:])
        baselines[-1].nfreq = int(val)
        baselines[-1].freqpols = []
        baselines[-1].dsabandindex
        baselines[-1].dsbbandindex
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
        datastreams[-1].datasource = (val == "TRUE")
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
            datastreams[-1].zoonfreqpols.append(int(val))
            datastreams[-1].nzoomband  += datastreams[-1].zoomfreqpols[-1]
        datastreams[-1].zoombandpol = []
        datastreams[-1].zoombandindex = []
        for j in range(datastreams[-1].nzoomband):
            val, lines = nextinputline(lines[1:])
            datastreams[-1].zoombandpol.append(val)
            val, lines = nextinputline(lines[1:])
            datastreams[-1].zoombandindex.append(int(val))
    return(numdatastreams, datastreams)
        
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
    for i in range(numfreqs):
        freqs.append(Freq())
        freqs[-1].freq = float(lines[at+i*8+2][20:])
        freqs[-1].bandwidth = float(lines[at+i*8+3][20:])
        if lines[at+i*7+4][20:21] == 'L':
            freqs[-1].lsb = True
        freqs[-1].numchan = int(lines[at+i*8+5][20:])
        freqs[-1].specavg = int(lines[at+i*8+6][20:])

    return (numfreqs, freqs)

import struct

SYNC_WORD = 0xFF00FF00

class Freq:
    bandwidth = 0.0
    freq = 0.0
    lsb = False
    numchan = 0
    specavg = 0

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
        freqs[-1].freq = float(lines[at+i*7+2][20:])
        freqs[-1].bandwidth = float(lines[at+i*7+3][20:])
        if lines[at+i*7+4][20:21] == 'L':
            freqs[-1].lsb = True
        freqs[-1].numchan = int(lines[at+i*7+5][20:])
        freqs[-1].specavg = int(lines[at+i*7+6][20:])

    return (numfreqs, freqs)

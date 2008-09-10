#!i/usr/bin/python
import sys
import os
import string
import struct

def writeBCD(file, number, ones, zeroes):
  a = number/8;
  b = (number%8)/4;
  c = (number%4)/2;
  d = (number%2);
  if a>0:
    file.write(ones[0])
  else:
    file.write(zeroes[0])
  if b>0:
    file.write(ones[0])
  else:
    file.write(zeroes[0])
  if c>0:
    file.write(ones[0])
  else:
    file.write(zeroes[0])
  if d>0:
    file.write(ones[0])
  else:
    file.write(zeroes[0])

if len(sys.argv) != 10:
  sys.exit("Usage: changemk5filetime <filename> <numtracks> <byte offset> <Y> <DDD> <HH> <MM> <SS> <sss>")

filename = sys.argv[1]
tracks = string.atoi(sys.argv[2])
offset = string.atoi(sys.argv[3])
Y = string.atoi(sys.argv[4])
DDD = string.atoi(sys.argv[5])
HH = string.atoi(sys.argv[6])
MM = string.atoi(sys.argv[7])
SS = string.atoi(sys.argv[8])
sss = string.atoi(sys.argv[9])

if tracks == 8:
  ones = struct.pack('i', 255).rstrip()
  zeros = struct.pack('i', 0).rstrip()
elif tracks == 16:
  ones = struct.pack('ii', 255, 255)
  zeros = struct.pack('ii', 0, 0)
elif tracks == 32:
  ones = struct.pack('iiii', 255, 255, 255, 255)
  zeros = struct.pack('iiii', 0, 0, 0, 0)
else:
  print 'Tracks must be one of 8,16 or 32'
  sys.exit(0)

i = open(filename, 'rb+')
i.seek(offset, 0)
writeBCD(i, Y, ones, zeros)
writeBCD(i, DDD/100, ones, zeros)
writeBCD(i, (DDD%100)/10, ones, zeros)
writeBCD(i, DDD%10, ones, zeros)
writeBCD(i, HH/10, ones, zeros)
writeBCD(i, HH%10, ones, zeros)
writeBCD(i, MM/10, ones, zeros)
writeBCD(i, MM%10, ones, zeros)
writeBCD(i, SS/10, ones, zeros)
writeBCD(i, SS%10, ones, zeros)
writeBCD(i, sss/100, ones, zeros)
writeBCD(i, (sss%100)/10, ones, zeros)
writeBCD(i, sss%10, ones, zeros)
i.close()




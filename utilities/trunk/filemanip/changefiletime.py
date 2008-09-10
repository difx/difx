#!i/usr/bin/python
import sys
import os

if len(sys.argv) != 3:
  sys.exit("Usage: changefiletime <list file> <replacement second>")

listfile = sys.argv[1]
ch = sys.argv[2][0]
offset = 19

f = open(listfile,'r')
for line in f.readlines():
  i = open(line.rstrip(), 'rb+')
  i.seek(offset, 0)
  i.write(ch)
  i.close()
  os.rename(line.rstrip(), line.rstrip()[0:-5] + ch + line.rstrip()[-4:])
f.close()


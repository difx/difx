#!/usr/bin/env python3

import sys, socket, subprocess

sites = ['sc','hn','nl','fd','la','pt','kp','br','ov','mk', 'gb', 'ea']
playbacks = ['01', '02', '03', '04', '05', '06', '07', '08']

def usage():

  print('Usage: remotem6modsmartdata.py [options] <unit code> <slot>\n')
  print('A program to show SMART data for the disks in a module')
  print('  in a given slot a given site or playback mark6 unit')
  print('options: -s - short output, show only serial number and error log')
  print('<unit code> is two letter vlba site code for site mark6 units or')
  print('            01 to 08 for playback mark6 units')
  print('<slot number> must be 1, 2, 3 or 4\n')

arglen = len(sys.argv)
if arglen < 3 or arglen > 4:
  usage()
  exit(1)

shortform = False
if sys.argv[1] == '-s':
  shortform = True

unit = 'zz'
slot = 0

if shortform == True and arglen == 4:
  unit = sys.argv[2].lower()
  slot = sys.argv[3]
else:
  unit = sys.argv[1].lower()
  slot = sys.argv[2]

site = False
playback = False

if unit in sites:
  site = True
elif unit in playbacks:
  playback = True
else:
  print('\nError!', unit, 'not a valid site code or playback number\n')
  usage()
  exit(1)

if slot not in ['1', '2', '3', '4']:
  print('\nError!', slot, 'not a valid slot number\n')
  usage()
  exit(1)

# build host string
hoststring = ''
if site == True:
  hoststring = 'vlbamon@' + unit + '-mark6-1'
elif playback == True:
  hoststring = 'difx@mark6fx' + unit
else:
  print('\nError!', unit, 'not a valid unit\n')
  usage()

# build command string
cmdstring = ''
if shortform == True:
  cmdstring = 'm6modsmartdata.py -s ' + slot
else:
  cmdstring = 'm6modsmartdata.py ' + slot

sshstring = ["ssh", hoststring, cmdstring]

try:
  smartout = subprocess.check_output(sshstring)
except subprocess.CalledProcessError:
  print('Error: could not find project', project, 'on any module in', mark6)
  exit(3)

# output is multiple lines of bytes type delimeted by '\n'
smartlines = smartout.split(b'\n')
for line in smartlines:
  print(line.decode('utf-8'))


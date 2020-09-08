#!/usr/bin/env python2

#**************************************************************************
#   Copyright (C) 2018 by Mark Wainright                                  *
#                                                                         *
#   This program is free software: you can redistribute it and/or modify  *
#   it under the terms of the GNU General Public License as published by  *
#   the Free Software Foundation, either version 3 of the License, or     *
#   (at your option) any later version.                                   *
#                                                                         *
#   This program is distributed in the hope that it will be useful,       *
#   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
#   GNU General Public License for more details.                          *
#                                                                         *
#   You should have received a copy of the GNU General Public License     *
#   along with this program.  If not, see <http://www.gnu.org/licenses/>. *
#**************************************************************************

import sys
import time
import subprocess
import string
import os

program = 'm6modinit.py'
version = '0.2'
author  = 'Mark Wainright <mwainrig@lbo.us>'
verdate = '20180131'

def usage():
        print '\n%s - ver. %s - %s - %s\n' % (program, version, author, verdate)
        print 'A program used to initialize a module in a mark6 unit.\n'
        print 'usage : %s <slot number> <module serial number>\n' % program
        #print 'Module serial number is in ccc%nnnn format.\n'

def unmount(slot):
  # unmount all partitions for slot
  rc = 1
  for i in range(8):
    datastr = '/mnt/disks/' + str(slot) + '/' + str(i)
    datacmd = ['sudo', 'umount', datastr]
    subp = subprocess.Popen(datacmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    out = subp.communicate();
    
    metastr = '/mnt/disks/.meta/' + str(slot) + '/' + str(i)
    metacmd = ['sudo', 'umount', metastr]
    subp = subprocess.Popen(metacmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    out = subp.communicate();

def getdevices(slot):
  sas_dev_num = []
  disks = []

  # find attached scsi devices
  res = subprocess.Popen(["lsscsi", "-H"],  stdout=subprocess.PIPE)
  out = res.communicate()[0]
  all_scsi_dev = string.split(out)
  sl = len(all_scsi_dev)

  # build list of mpt3sas device numbers (should be 2 for 2 host bus adapters)
  for i in range (1,sl,2):
    # --- if the devices uses the mpt3sas driver
    print all_scsi_dev[i]
    if all_scsi_dev[i] == "mpt3sas" or all_scsi_dev[i] == "mpt2sas" :
      val = all_scsi_dev[i-1]
      # --- extract the number from '[1]' string
      num_val = val[val.find('[')+1:val.find(']')]
      sas_dev_num.append(num_val)

  # get block devices for both a and b host bus adapters
  a = b = ""
  num_dev = len(sas_dev_num)
  for i in range(num_dev):
    res = subprocess.Popen(["lsscsi", "-t", sas_dev_num[i]],  stdout=subprocess.PIPE)
    # --- since we should have only 2 sas devices attached to driver at most:
    out = res.communicate()[0]
    if i == 0:
      a = string.split(out)
    else:
      b = string.split(out)

  # --- IF THE LENGHT OF B IS > 0 THEN IT WAS ASSIGNED, CONCATENATE OUTPUT
  if len(b) > 0:
      scsi_list = a + b
  elif len(a) > 0:
      scsi_list = a
  else:
      print "no disk modules powered up"
      return disks

  # --- scsi_list number of items
  sll = len(scsi_list)
  # --- since the format is as above, every 4 is the devices
  num_available_disks = sll / 4

  for i in range(0, sll, 4):
    disk = scsi_list[i+3]
    if "/dev/sd" in disk:
      # get the sas address
      (ty, sas_address) = string.split(scsi_list[i+2],":")
      # --- the device number on the SAS controller, since it can be hex, convert from base 16 string format
      dev_num = int(sas_address[11],16)
      # get the host id
      host_info = scsi_list[i]
      host_id = int(host_info[1:host_info.find(':')])

      # build disk list for slot
      if host_id != 0:
        if dev_num < 8 and slot == 1:
          disks.append({'dev_num': dev_num, 'slot': slot, 'disk': disk})
        elif dev_num >= 8 and slot == 2:
          disks.append({'dev_num': dev_num - 8, 'slot': slot, 'disk': disk})
      if host_id == 0:
        if dev_num < 8 and slot == 3:
          disks.append({'dev_num': dev_num, 'slot': slot, 'disk': disk})
        elif dev_num >= 8 and slot == 4:
          disks.append({'dev_num': dev_num - 8, 'slot': slot, 'disk': disk})

  # get serial number for disk
  if len(disks) > 0:
    for i in range(len(disks)):
      serial_num = ''
      disk_dev = disks[i]['disk']
      # use udisks to get serial number
      #res = subprocess.Popen(["udisks", "--show-info", disk_dev],  stdout=subprocess.PIPE)
      res = subprocess.Popen(["udisksctl", "info", "-b" , disk_dev],  stdout=subprocess.PIPE)
      out = res.communicate()[0]
      # output is split by new line
      disk_info = out.split("\n")
      for j in range(len(disk_info)):
        tmp_disk_info = disk_info[j].split()
        #if "  serial:" in disk_info[j]:
        if "Id:" in disk_info[j]:
          if len(tmp_disk_info) > 1:
            serial_num = tmp_disk_info[1][-8:]
	    print serial_num
            disks[i]['serial_num'] = serial_num
        elif "Size:" in disk_info[j]:
          if len(tmp_disk_info) > 1:
            disk_size = tmp_disk_info[1]
            disks[i]['disk_size'] = disk_size
      if len(serial_num) == 0:
        print "serial number could not be determined for disk", i, "device", disk_dev
        exit(1)
      print disks[i]['serial_num'],disks[i]['disk_size']

  #print "disks", disks
    
  return disks

def checkforerror(partcmd, subp, output):
  if subp.returncode != 0:
    print 'command:', partcmd
    print 'return code:', subp.returncode
    print 'output (stdout, stderr):', output
    exit(1)

#-------main execution---------
if len(sys.argv) < 3:
  usage()
  exit(1)

slot = int(sys.argv[1])

if slot not in [1, 2, 3, 4]:
  usage()
  exit(1)

msn = sys.argv[2]

if len(msn) != 8:
  usage()
  exit(1)

# get a list of disks for slot
disks = getdevices(slot)

if len(disks) == 0:
  print program, "- No disks were found in slot.  Stopping."
  exit(0)

# display disk information and confirm initialization
serial_list = []
size_list = []
print "\n", program, "will initialize module in slot", slot, "with MSN", msn, "\n"
print "The following disks will be initialized:\n"
for i in range(len(disks)):
  serial_list.append(str(i) + ':' + disks[i]['serial_num'] + '\n')
  size_list.append(disks[i]['disk_size'])
  print "slot", str(slot), "disk", disks[i]['dev_num'], "device", disks[i]['disk'], "serial number", disks[i]['serial_num'], "size", disks[i]['disk_size']
print "\n!!! Any information on these disks will be erased!\n"
sys.stdout.write("Continue? (y/n) ")
choice = raw_input().lower()
if choice == "n":
  exit(0)

print "\nInitialization will take a few minutes.\n"

# unmount module in slot
unmount(slot)

# make eMSN string
capacity = 0
for i in range(len(size_list)):
  capacity += int(size_list[i])/1000000000

# eMSN is of form <msn>/<total capacity of pack>/<speed (always 4)>/<number of disks>
eMSN = msn + '/' + str(capacity) + '/4/' + str(len(size_list))

print 'eMSN is', eMSN, '\n'

# partition and configure the disks
meta_data_path_list = []
for i in range(len(disks)):
  # remove existing partitions
  partcmd = ['sudo', 'parted', '-s', disks[i]['disk'], 'rm', '1']
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  
  partcmd = ['sudo', 'parted', '-s', disks[i]['disk'], 'rm', '2']
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  time.sleep(0.1)
    
  # make new partions
  print "Partitioning disk", disks[i]['dev_num'], "device", disks[i]['disk']
  partcmd = ['sudo', 'parted', '-s', disks[i]['disk'], 'mktable', 'gpt']
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

  partcmd = ['sudo', 'parted', '-s', disks[i]['disk'], 'mkpart primary xfs 1 -100M']
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

  partlabel = msn + '_' + str(disks[i]['dev_num'])
  partcmd = ['sudo', 'parted', '-s', disks[i]['disk'], 'name 1 ' + partlabel]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)
  
  partcmd = ['sudo', 'parted', '-s', disks[i]['disk'], 'mkpart primary xfs -100M 100%']
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

  partlabel = msn + '_' + str(disks[i]['dev_num']) + 'm'
  partcmd = ['sudo', 'parted', '-s', disks[i]['disk'], 'name 2 ' + partlabel]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)
  time.sleep(0.1)
  
  # make xfs file systems
  print "Making xfs file systems for disk", disks[i]['dev_num']
  partcmd = ['sudo', 'mkfs.xfs', '-f', disks[i]['disk'] + '1']
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

  partcmd = ['sudo', 'mkfs.xfs', '-f', disks[i]['disk'] + '2']
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)
  time.sleep(0.1)

  # mount and configure .meta partitions
  print "Configuring metadata partition for disk", disks[i]['dev_num']
  meta_data_path = '/mnt/disks/.meta/' + str(slot) + '/' + str(disks[i]['dev_num'])
  meta_data_path_list.append(meta_data_path)
  partcmd = ['sudo', 'mount', '-t', 'xfs', disks[i]['disk'] + '2', meta_data_path]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

  partcmd = ['sudo', 'chmod', '777', meta_data_path]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)
  
  partcmd = ['sudo', 'chown', '1000:1001', meta_data_path]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

  # write disk_sn, eMSN and state files to .meta directories
  sn_file_path = '/mnt/disks/.meta/' + str(slot) + '/' + str(disks[i]['dev_num']) + '/disk_sn'
  sn_file = open(sn_file_path, 'w')
  for j in range(len(serial_list)):
    sn_file.write(serial_list[j])
  sn_file.close()

  partcmd = ['sudo', 'chmod', '666', sn_file_path]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

  emsn_file_path = '/mnt/disks/.meta/' + str(slot) + '/' + str(disks[i]['dev_num']) + '/eMSN'
  emsn_file = open(emsn_file_path, 'w')
  emsn_file.write(eMSN)
  emsn_file.close()

  partcmd = ['sudo', 'chmod', '666', emsn_file_path]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

  state_file_path = '/mnt/disks/.meta/' + str(slot) + '/' + str(disks[i]['dev_num']) + '/state'
  state_file = open(state_file_path, 'w')
  state_file.write('erased\n')
  state_file.close()

  partcmd = ['sudo', 'chmod', '666', state_file_path]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

  # mount and configure data partitions
  print "Configuring data partition for disk", disks[i]['dev_num'], "\n"
  partcmd = ['sudo', 'mount', '-t', 'xfs', disks[i]['disk'] + '1', '/mnt/disks/' + str(slot) + '/' + str(disks[i]['dev_num'])]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

  partcmd = ['sudo', 'chmod', '777', '/mnt/disks/' + str(slot) + '/' + str(disks[i]['dev_num'])]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

  data_dir_path = '/mnt/disks/' + str(slot) + '/' + str(disks[i]['dev_num']) + '/data'
  partcmd = ['sudo', 'mkdir', data_dir_path]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

  partcmd = ['sudo', 'chmod', '777', data_dir_path]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

  partcmd = ['sudo', 'chown', '1000:1001', data_dir_path]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

  cull_dir_path = '/mnt/disks/' + str(slot) + '/' + str(disks[i]['dev_num']) + '/cull'
  partcmd = ['sudo', 'mkdir', cull_dir_path]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

  partcmd = ['sudo', 'chmod', '777', cull_dir_path]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

  partcmd = ['sudo', 'chown', '1000:1001', cull_dir_path]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

# make single module group
print "The next step will put this module a single module group."
print "If you don't put this module in a single module group, you"
print "will need to group this module with the m6modgroup.py script.\n"
sys.stdout.write("Put this module in a single module group? (y/n) ")
choice = raw_input().lower()
if choice == "n":
  print "\nNot putting module in slot", slot, "in a group.  Finished.\n"
  exit(0)

print "\nPutting module in slot", slot, "in a single module group.\n"

for i in range(len(meta_data_path_list)):
  group_file_path = meta_data_path_list[i] + '/group'
  group_string = "1:" + eMSN
  group_file = open(group_file_path, 'w')
  group_file.write(group_string)
  group_file.close()

  partcmd = ['sudo', 'chmod', '666', group_file_path]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

print "Group files written.  Unmounting.\n"

partcmd = ['m6modunmount.py', str(slot)]
subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
out = subp.communicate();
checkforerror(partcmd, subp, out)

print "Initialization of", msn, "in slot", slot, "finished.\n"

exit(0)


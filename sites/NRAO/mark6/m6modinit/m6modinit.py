#!/usr/bin/env python3

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
version = '0.1.1'
author  = 'Mark Wainright <mwainrig@lbo.us>'
verdate = '20251113'

def usage():
        print('\n', program, '- ver.', version, '-', author, '-', verdate, 's\n')
        print('A program used to initialize a module in a mark6 unit.\n')
        print('usage :', program, '<slot number> <module serial number>\n')
        print('Module serial number is in LBO%nnnn format.\n')

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
  res = subprocess.Popen(["lsscsi", "-H"],  stdout=subprocess.PIPE, universal_newlines=True)
  out = res.communicate()[0]
  all_scsi_dev = out.split()
  sl = len(all_scsi_dev)

  mpt3sasdevices = []

  # build list of mpt3sas device numbers (should be 2 for 2 host bus adapters)
  for i in range (1,sl,2):      # only look at second column of lsscsi -H output
    # --- if the devices uses the mpt3sas driver
    if all_scsi_dev[i] == "mpt3sas" or all_scsi_dev[i] == "mpt2sas" :
      val = all_scsi_dev[i-1]
      # --- extract the number from '[1]' string
      num_val = val[val.find('[')+1:val.find(']')]
      sas_dev_num.append(num_val)
      if all_scsi_dev[i] == "mpt3sas":
        mpt3sasdevices.append(num_val)
  sas_dev_num.sort()

  # get block devices for all mptXsas host bus adapters
  scsi_list = []
  num_dev = len(sas_dev_num)
  for i in range(num_dev):
    res = subprocess.Popen(["lsscsi", "-t", sas_dev_num[i]],  stdout=subprocess.PIPE, universal_newlines=True)
    # --- since we should have only 2 sas devices attached to driver at most:
    out = res.communicate()[0]
    devicesinfo = out.split()
    if len(devicesinfo) > 2:
       scsi_list += devicesinfo

  if len(scsi_list) <= 0:
      print("no disk modules powered up")
      return []

  # --- scsi_list number of items
  sll = len(scsi_list)
  # --- since the format is as above, every 4 is the devices
  num_available_disks = sll / 4

  for i in range(0, sll, 4):
# krowe Dec  4 2024: disk is type bytes
    disk = str(scsi_list[i+3])  # disk name, e.g. /dev/sdb
    if "/dev/sd" in disk:
      # get the sas address
      (ty, sas_address) = str(scsi_list[i+2]).split(":")
      controllerID = str(scsi_list[i]).split(":")[0][1:]
      # --- the device number on the SAS controller, since it can be hex, convert from base 16 string format
      # apparently sas3 controller encode the device id differently then sas2

      # krowe Nov  7 2025: RHEL7 and RHEL8 mpt3sas devices encode things
      # differetnly.
      # RHEL7
      # [0:0:11:0]   disk    sas:0x300062b202bf4dc8          /dev/sdo 
      # [0:0:12:0]   disk    sas:0x300062b202bf4dc9          /dev/sdp 
      # [0:0:13:0]   disk    sas:0x300062b202bf4dca          /dev/sdq 
      # RHEL8
      # [0:0:15:0]   disk    sas:0x4433221100000000          /dev/sdb 
      # [0:0:16:0]   disk    sas:0x4433221101000000          /dev/sdc 
      # [0:0:17:0]   disk    sas:0x4433221102000000          /dev/sdd 
      
      # In RHEL8 and mpt3sas, I need int(sas_address[11],16) to get the
      # dev_num.  Maybe there is a better way to do this that works with
      # both RHEL7 and 8. What is dev_num anyway? Actually I can't find
      # anythig better than dev_num.
      if controllerID in mpt3sasdevices:
#        dev_num = int(sas_address[17],16) # doesn't work on RHEL8
        dev_num = int(sas_address[11],16)
      else:
        dev_num = int(sas_address[11],16)
      # get the host id
      host_info = str(scsi_list[i])
      host_id = int(host_info[1:host_info.find(':')])

      # krowe Nov  7 2025: what is this logic?
      # Why does it assume slots 3 and 4 will be on host_id 0?
      # I don't think there is a good technical way to associate a slot
      # with a physical address.  This might be up to human convention.
      # At NRAO we label slots 1 and 2 on the main chassis. Other sites
      # May to this differntly.
      # build disk list for slot
      controller_index = sas_dev_num.index(str(host_id))
      if controller_index == 0:
        # mptsas 0 has slots 1+2
        if dev_num < 8 and slot == 1:
          disks.append({'dev_num': dev_num, 'slot': slot, 'disk': disk})
        elif dev_num >= 8 and slot == 2:
          disks.append({'dev_num': dev_num - 8, 'slot': slot, 'disk': disk})
      elif controller_index == 1:
        # mptsas 1 has slots 3+4
        if dev_num < 8 and slot == 3:
          disks.append({'dev_num': dev_num, 'slot': slot, 'disk': disk})
        elif dev_num >= 8 and slot == 4:
          disks.append({'dev_num': dev_num - 8, 'slot': slot, 'disk': disk})
      elif controller_index == 2:
        # mptsas 2 has slots 5+6
        if dev_num < 8 and slot == 5:
          disks.append({'dev_num': dev_num, 'slot': slot, 'disk': disk})
        elif dev_num >= 8 and slot == 6:
          disks.append({'dev_num': dev_num - 8, 'slot': slot, 'disk': disk})

  # get serial number for disk
  if len(disks) > 0:
    for i in range(len(disks)):
      disk_dev = disks[i]['disk']
      # use udisks to get serial number
      # krowe Dec  5 2024: udisks is gone.  udisksctl is too complicated.
      # Use lsblk instead.
      res = subprocess.Popen(["lsblk", "--nodeps", "--noheadings", "--bytes", "--output", "serial,size", disk_dev], stdout=subprocess.PIPE)
      out = res.communicate()[0].split()
      disks[i]['serial_num'] = out[0].decode('UTF-8')
      disks[i]['disk_size'] = out[1].decode('UTF-8')
      if len(disks[i]['serial_num']) == 0:
        print("serial number could not be determined for disk", i, "device", disk_dev)
        exit(1)

  #print("disks", disks)
    
  return disks

def checkforerror(partcmd, subp, output):
  if subp.returncode != 0:
    print('command:', partcmd)
    print('return code:', subp.returncode)
    print('output (stdout, stderr):', output)
    exit(1)

#-------main execution---------
if len(sys.argv) < 3:
  usage()
  exit(1)

slot = int(sys.argv[1])

if slot not in [1, 2, 3, 4, 5, 6]:
  usage()
  exit(1)

msn = sys.argv[2]

if len(msn) != 8 or '%' not in msn:
  usage()
  exit(1)

# get a list of disks for slot
disks = getdevices(slot)

if len(disks) == 0:
  print(program, "- No disks were found in slot.  Stopping.")
  exit(0)

# display disk information and confirm initialization
serial_list = []
size_list = []
print("\n", program, "will initialize module in slot", slot, "with MSN", msn, "\n")
print("The following disks will be initialized:\n")
for i in range(len(disks)):
  serial_list.append(str(i) + ':' + disks[i]['serial_num'] + '\n')
  size_list.append(disks[i]['disk_size'])
  print("slot", str(slot), "disk", disks[i]['dev_num'], "device", disks[i]['disk'], "serial number", disks[i]['serial_num'], "size", disks[i]['disk_size'])
print("\n!!! Any information on these disks will be erased!\n")
sys.stdout.write("Continue? (y/n) ")
choice = input().lower()
if choice == "n":
  exit(0)

print("\nInitialization will take a few minutes.\n")

# unmount module in slot
unmount(slot)

# make eMSN string
capacity = 0
for i in range(len(size_list)):
  capacity += int(size_list[i])/1000000000

# eMSN is of form <msn>/<total capacity of pack>/<speed (always 4)>/<number of disks>
eMSN = msn + '/' + str(capacity) + '/4/' + str(len(size_list))

print('eMSN is', eMSN, '\n')

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
  print("Partitioning disk", disks[i]['dev_num'], "device", disks[i]['disk'])
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
  print("Making xfs file systems for disk", disks[i]['dev_num'])
  #;krowe Jan 10 2025: XFS in RHEL8 is incompatible with RHEL7 unless reflink=0
  #partcmd = ['sudo', 'mkfs.xfs', '-f', disks[i]['disk'] + '1']
  partcmd = ['sudo', 'mkfs.xfs', '-f', '-m', 'reflink=0', disks[i]['disk'] + '1']
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

  partcmd = ['sudo', 'mkfs.xfs', '-f', '-m', 'reflink=0', disks[i]['disk'] + '2']
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)
  time.sleep(0.1)

  # mount and configure .meta partitions
  print("Configuring metadata partition for disk", disks[i]['dev_num'])
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
  
  partcmd = ['sudo', 'chown', 'root:nmstaff', meta_data_path]
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
  print("Configuring data partition for disk", disks[i]['dev_num'], "\n")
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

  partcmd = ['sudo', 'chown', 'root:nmstaff', data_dir_path]
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

  partcmd = ['sudo', 'chown', 'root:nmstaff', cull_dir_path]
  subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  out = subp.communicate();
  checkforerror(partcmd, subp, out)

# make single module group
print("The next step will put this module a single module group.")
print("If you don't put this module in a single module group, you")
print("will need to group this module with the m6modgroup.py script.\n")
sys.stdout.write("Put this module in a single module group? (y/n) ")
choice = input().lower()
if choice == "n":
  print("\nNot putting module in slot", slot, "in a group.  Finished.\n")
  exit(0)

print("\nPutting module in slot", slot, "in a single module group.\n")

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

print("Group files written.  Unmounting.\n")

partcmd = ['m6modunmount.py', str(slot)]
subp = subprocess.Popen(partcmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
out = subp.communicate();
checkforerror(partcmd, subp, out)

print("Initialization of", msn, "in slot", slot, "finished.\n")

exit(0)


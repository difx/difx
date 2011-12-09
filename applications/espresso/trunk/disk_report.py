#!/usr/bin/python
# Simple program to show disk usage on the main CUPPA data storage areas
# Cormac Reynolds: 2010 June 2
import os, subprocess, sys, time
import espressolib


data_areas = espressolib.get_corrhosts(os.environ.get('CORR_HOSTS'))

diskreport = dict()
for machine in data_areas:
    if data_areas[machine][1]:
        sys.stderr.write(machine + '\n')
        diskreport[machine] = dict()
        for data_area in data_areas[machine][1]:
            diskreport[machine][data_area] = dict()
            #command = ["ssh", machine, "'du -csh " + data_area + "/*'"]
            command = "ssh " + machine +  " 'du -c -B 1G " + data_area + "/*'"
            sys.stderr.write(command + '\n')
            #proc = subprocess.Popen(command, shell=True)
            proc = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE)
            du = proc.communicate()[0]
            diskreport[machine][data_area]['du'] = (du.split('\n'))

            command = "ssh " + machine +  " 'df -h " + data_area + "'"
            sys.stderr.write(command + '\n')
            proc = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE)
            df = proc.communicate()[0]
            diskreport[machine][data_area]['df'] = df


def sizesort(x):
    try:
        sortstring = int(x.split()[0])
    except: 
        sortstring = -1
    return sortstring

print "-" * 50
print "Summary of disk availability at ", time.asctime(), "\n"
for machine in sorted(diskreport):
    print "*" * 5
    print machine
    for data_area in sorted(diskreport[machine]):
        print data_area
        print diskreport[machine][data_area]['df']

for machine in sorted(diskreport):
    print "-" * 50
    print "Disk report for ", machine
    for data_area in sorted(diskreport[machine]):
        print "\nData area:", data_area
        print diskreport[machine][data_area]['df']
        diskreport[machine][data_area]['du'].sort(key=sizesort, reverse=True)
        print "Disk usage by subdirectory (GB)"
        for line in  diskreport[machine][data_area]['du']:
            print line


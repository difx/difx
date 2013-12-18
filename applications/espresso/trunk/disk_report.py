#!/usr/bin/python
# Simple program to show disk usage on the main CUPPA data storage areas
# Cormac Reynolds: 2010 June 2
import os, subprocess, sys, time
import espressolib
import multiprocessing
from multiprocessing import Process, Queue
from Queue import Empty

def remote_command(inputq, outputq):
    while True:
        try:
            disk_query, machine, data_area  = inputq.get(block=False)
            command = str()
            if disk_query == 'du':
                command = "ssh MACHINE 'du -c -B 1G DATA_AREA/*'"
            elif disk_query == 'df':
                command = "ssh MACHINE 'df -P -B 1G DATA_AREA'"

            command = command.replace('MACHINE', machine)
            command = command.replace('DATA_AREA', data_area)
            sys.stderr.write(command + '\n')

            proc = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE)
            output = proc.communicate()[0]
            #print output
            outputq.put([machine, data_area, output])
        except Empty:
            break
    #return output

def sizesort(x):
    try:
        sortval = int(x.split()[0])
    except: 
        sortval = -1

    try:
        # the 'total' line is a special case - always put at bottom.
        if x.split()[1] == 'total':
            sortval = -1
    except:
        pass

    return sortval

#def data_summary():
    #print 'here'

difx_machines = os.environ.get('DIFX_MACHINES')
if not difx_machines:
    difx_machines = os.environ.get('CORR_HOSTS')
    if difx_machines:
        sys.stderr.write('Warning: use of the $CORR_HOSTS variable is deprecated. Please define $DIFX_MACHINES instead\n')
    else: 
        raise Exception('$DIFX_MACHINES not set!')

try:
    data_areas = espressolib.get_corrhosts(difx_machines)
except:
    raise Exception("Problem with file: " + difx_machines)

# run two processes per available cpu (the work is all being done remotely)
nproc = multiprocessing.cpu_count()*2
diskreport = dict()
disk_queries = []
disk_queries = ['du', 'df']
for disk_query in disk_queries:
    # do the disk queries in parallel
    inputq = Queue()
    outputq = Queue()
    for machine in data_areas:
        if data_areas[machine][1]:
            #sys.stderr.write(machine + '\n')
    
            for data_area in data_areas[machine][1]:
                # form a queue of data areas
                inputq.put([disk_query, machine, data_area])
                    
    # nproc is the number of parallel processes to initiate (each process will
    # take jobs from the inputq until it is empty)
    processes = [Process(target=remote_command, args=(inputq,outputq)) for i in range(nproc)]
    for p in processes:
        p.start()
    for p in processes:
        #sys.stderr.write( str(p.pid) + "\n" )
        p.join()
    
    while not outputq.empty():
        machine, data_area, output = outputq.get()
        if not machine in diskreport.keys():
            diskreport[machine] = dict()
        if not data_area in diskreport[machine].keys():
            diskreport[machine][data_area] = dict()
        if disk_query == 'du':
            diskreport[machine][data_area]['du'] = (output.split('\n'))
        elif disk_query == 'df':
            diskreport[machine][data_area]['df'] = output

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


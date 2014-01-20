#!/usr/bin/python
# simple program to parse the .joblist file and return the job length

import optparse, espressolib

usage = '''%prog <joblist> 
    will return the length of the jobs in the <joblist> file.'''

parser = optparse.OptionParser(usage=usage, version='%prog ' + '1.0')

(options, args) = parser.parse_args()
if len(args) < 1:
    parser.print_help()
    parser.error("Give at least one .joblist file")

jobfilenames = args

for jobfilename in jobfilenames:
    jobfile = open(jobfilename).readlines()
    header = jobfile.pop(0)
    print header
    passlen = 0
    pass_size = 0
    pass_stations = 0
    for line in jobfile:
        jobinfo = line.split()
        jobstart = espressolib.convertdate(float(jobinfo[1]), outformat='vex')
        joblen = float(jobinfo[2]) - float(jobinfo[1])
        jobstations = float(jobinfo[3])
        jobsize = float(jobinfo[6])

        pass_stations = (jobstations*joblen + pass_stations*passlen)/(joblen+passlen)
        passlen += joblen
        pass_size += jobsize


        #print jobinfo[0], joblen*24., 'hours'
        print "%s: %s \t %0.3f %s \t %0.2f %s \t %d %s" % (jobinfo[0], jobstart, joblen*24., 'hours', jobsize, 'MB', jobstations, 'stations')

    #print 'Total:', passlen*24., 'hours'
    print "%s: %0.3f %s \t %0.2f %s \t %0.1f %s" % ('Total', passlen*24., 'hours', pass_size, 'MB', pass_stations, 'stations (avg)')


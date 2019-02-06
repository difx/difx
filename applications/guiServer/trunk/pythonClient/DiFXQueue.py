#!/usr/bin/env python
#<!---======================================================================--->
## Script to update the queue file using the DiFX Server
#
#  This script inserts a new item into a difx processing queue using guiServer.  
#  The queue is actually processed by the script DiFXrunQueue.py.   The 
#  required inputs are as follows: 
#  --hostname, -H   :  Specify the host name were difx will run. This is a 
#                      mandatory parameter, to specify the local machine you 
#                      can use localhost as the host name.
#
#  --port, -p       :  TCP port used to communicated with the difxServer.  
#                      This must be the same port number used by guiServer.  
#
#  --priority, -q   :  The priority number assigned to this queue job.  This 
#                      is specified as an integer were lower numbers mean 
#                      higher priority.
#
#  --input_file, -i :  Difx .input file or list of input files for difx to 
#                      correlate.
#
#  --queue, -Q      :  Full path and name of the queue file e.g. /path/to/queue.txt   
#
#
#  --start, -s      :  Start the queue processing? True/False (No by default)
#
#
#   Example:
#
#      python DiFXQueue.py --hostname localhost -port 50400 --queue path/to/queue_file.txt --priority 1 --input_file /path/to/input_file.txt 
#  
#  
#<!---======================================================================--->

import sys
import DiFXControl
import subprocess
import os
import argparse
import glob


#<!------------------------------------------------------------------------>
##  
#  @param parameters Dictionary of input parameters for the queue
#
#  Obtains the input parameters from the user and stores them in 
#  the dictionary "parameters" 
#  
#<!------------------------------------------------------------------------>

def get_parameters():
	# Get input parameters from user
	parser = argparse.ArgumentParser()
	parser.add_argument("--hostname","-H",help="Specify the host name were difx will run. This is a mandatory parameter, to specify the local machine you can use localhost as the host name.",default="node-001")
	parser.add_argument("--port","-p",help="TCP port used to communicated with the difxServer.  This must be the same port number used by guiServer.",default="50200")
	parser.add_argument("--priority","-q",help="The priority number assigned to this queue job.  This is specified as an integer were lower numbers mean higher priority.",default=1)
	parser.add_argument("--input_file","-i",help="Difx .input file or list of input files for difx to correlate.",required=True,type=str,nargs='+')
	parser.add_argument("--queue","-Q",help="Full path and name of the queue file e.g. /path/to/queue.txt",default="/san02/data1/correlator/run/TESTS/python_queue_testing/queue.txt")
	parser.add_argument("--start","-s",help="Start the queue processing? True/False (No by default)",default=False)
	parameters = parser.parse_args()	
	return parameters

#<!------------------------------------------------------------------------>
##  
#  @param difx       Instance of the difx cliente class.  
#  @param parameters Dictionary of input parameters for the queue
#
#  Sets up the difx cliente connection and checks if the provided input 
#  file exists
#  
#<!------------------------------------------------------------------------>
def initialize(difx,parameters):
       
	# Set up difx client class instance named difx
        difx.connect(parameters.hostname,parameters.port)
        difx.monitor()
	
	# If DiFXQueue.py is given notation like file* for a list of files 
	# (eg file1.input,file2.input) it will create a list if the directory is 
	# accesable to DiFXQueue.py, if not (like when DiFXQueue.py is run
	# in a remote setting) we'll need to perform a remote ls command 
	# to create the list of files. 

	# First check to see is parameter input file has multiple items
	#if (parameters.input_file 
	print (parameters.input_file)
	if (len(parameters.input_file) > 1):
		for filename in parameters.input_file:
			check_file_exists(difx,filename)
		write_file_list(difx,parameters.input_file,parameters)
	elif (len(parameters.input_file) == 1):
		remote_filepath = difx.ls(parameters.input_file[0])	
		if (len(remote_filepath) > 1): 
			for filename in remote_filepath:
				check_file_exists(difx,filename)
			write_file_list(difx,remote_filepath,parameters)
		else:
			# check if remote file path exists
			check_file_exists(difx,remote_filepath)
			paramters.input_file = remote_filepath

#	print("paramters.input_file = ")
#	print(parameters.input_file)
#	exit(0)

def check_file_exists(difx,filename):
	remote_filepath = difx.ls(filename,"-l")
        if (remote_filepath == None):
                raise Exception("Input file " + filename + " does not exist aborting.  Note: the full path to input file(s) is required.")

#<!------------------------------------------------------------------------>
##  
#  @param difx       Instance of the difx cliente class.  
#  @param parameters Dictionary of input parameters for the queue
#
#   
#  
#  
#<!------------------------------------------------------------------------>
def write_file_list(difx,filelist,parameters):
	# Grab path to first input file, stick inputFileList there
	pathlist = filelist[0].split('/')
	pathlist = pathlist[:-1]
        path = '/'.join(pathlist)
	print("path = " + path)
	if (path):
		inputFileList = path +"/inputFileList"
	else:
		
		inputFileList = "inputFileList"		

	difx.touch(inputFileList)
	fileContents = '\n'.join(filelist)        
	difx.sendFile(inputFileList,fileContents)	
	parameters.input_file = inputFileList



#<!------------------------------------------------------------------------>
##  
#  @param difx       Instance of the difx cliente class.  
#  @param parameters Dictionary of input parameters for the queue
#
#  Inserts a new item into the queue text file.  The postion within this   
#  text file determines the processing order for DiFXrunQueue.
#  
#<!------------------------------------------------------------------------>
def write_queue(difx,parameters):
	
	# Set up difx client class instance named difx
	# Construct string that will be inserted into the queue, the trailing 0 is the progress of
	# the new item, here it is initialized to 0
	queuedata_new = str(parameters.priority) + " " + parameters.input_file + " 0"	


	
	
	# Read existing queue data 
	queuedata = difx.getFile(parameters.queue)
	# If queue contains no data (or is empty) create a new file 
	if not queuedata:
		print("running touch")
		difx.touch(parameters.queue)
		difx.sendFile(parameters.queue,queuedata_new)
		return
	
	# construct queuedata list
	queuedata = queuedata.split("\n")
	
	# Scrub empty elements
	while '' in queuedata:
		queuedata.remove('')
	
	# Determine the order of the new queue
	idx = 0 
	insert_idx = 0
	end_of_file = True
	for line in queuedata:
		line_split = line.split(" ")
		queuepriority = int(line_split[0])
		print("parameters.priority = " + str(parameters.priority))
		print("queuepriority = " + str(queuepriority))
		parameters.priority = int(parameters.priority)
		queuepriority = int(queuepriority)
		if (parameters.priority < queuepriority):
			print("parameters.priority < queuepriority")
			insert_idx = idx
			end_of_file = False
			break
		print("Incrementing idx")
		idx = idx + 1 

	print("insert idx " + str(insert_idx))	
	print(end_of_file)
	# Check if the new data for the queue just needs to be 
        # appended at the end of the queue file the condition 
        # queuepriority_newjob < queuepriority should never be 
        # satisfied in this case
	if end_of_file:
		# Insert new job at the end of the queue
		queuedata.append(queuedata_new)
	else:
		# Insert the new job data at a specified place in the queue 
		queuedata.insert(insert_idx,queuedata_new)
	
	
	# Push new data to existing queue file 
	difx.cp(parameters.queue,parameters.queue+"~")
	queuedata_string = "\n".join(queuedata)
	difx.sendFile(parameters.queue+"~",queuedata_string)
	difx.mv(parameters.queue+"~",parameters.queue)



#<!------------------------------------------------------------------------>
##   
#  @param parameters Dictionary of input parameters for the queue
#
#  Spawns DiFXrunQueue.py  
#  
#  
#<!------------------------------------------------------------------------>
def spawnRunQueue(parameters):
	DiFXrunQueue_fullpath = os.path.dirname(os.path.realpath("DiFXrunQueue.py")) + "/DiFXrunQueue.py"
	args = [DiFXrunQueue_fullpath,"-H " + parameters.hostname,"-p " + str(parameters.port),"-Q " + parameters.queue]
	print(args)
	#exit(0)
	subprocess.Popen([sys.executable or 'python'] + args)
	

#<!------------------------------------------------------------------------>
##   
#  @param difx       Instance of the difx cliente class.   
#
#  Closes the client connection.  
#  
#<!------------------------------------------------------------------------>
def close(difx):
	difx.close()


#<!------------------------------------------------------------------------>
##   
#   Checks on the existance of temp files to make sure edits to the queue
#   are not made at the same time
#  
#<!------------------------------------------------------------------------>
def check_temp(filename):
	filename = filename.str()
	tmp_filename = filename + "~"
	remote_filepath = difx.ls(tmp_filename,"-l")
        while (remote_filepath != None):
		remote_filepath = difx.ls(tmp_filename,"-l")
		time.sleep(5)
		continue
	return
	



#<!------------------------------------------------------------------------>
##   
#   Runs all funtions and provides error handeling.
#  
#<!------------------------------------------------------------------------>

def main():
	difx = DiFXControl.Client()
	try:
		parameters = get_parameters()
		initialize(difx,parameters)
		write_queue(difx,parameters)
		if (parameters.start):
			spawnRunQueue(parameters)
	finally:
		close(difx)
main()


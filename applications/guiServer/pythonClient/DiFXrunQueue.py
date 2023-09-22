#!/usr/bin/env python
import DiFXControl
import DiFXJobControl
import sys 
import time 
import os.path
import os 
import atexit
import argparse
import math
import signal



# Parses a range of integers
def parseIntSet(nputstr=""):
  selection = set()
  invalid = set()
  # tokens are comma seperated values
  tokens = [x.strip() for x in nputstr.split(',')]
  for i in tokens:
     try:
        # typically tokens are plain old integers
        selection.add(int(i))
     except:
        # if not, then it might be a range
        try:
           token = [int(k.strip()) for k in i.split('-')]
           if len(token) > 1:
              token.sort()
              # we have items seperated by a dash
              # try to build a valid range
              first = token[0]
              last = token[len(token)-1]
              for x in range(first, last+1):
                 selection.add(x)
        except:
           # not an int and not a range...
           invalid.add(i)
  # Report invalid tokens before returning valid selection
  print "Invalid set: " + str(invalid)
  return selection
# end parseIntSet

#  Define a class to respond to messages
class Responder:
    def __init__( self ):
        self.jobProgress = 0
    #  Callback triggered when the monitoring client receives a new DiFX message.
    def difxRelayCallback( self, data ):
        #  Parse the message.
        xmlDat = DiFXControl.parseXML( data )
        #  See if this is a message type we are interested in.
        if xmlDat.typeStr == "DifxStatusMessage":
            #  Make sure the identifier (job name) is one we are interested in.
            #  Compute a progress value.  The data for this might not be there, so we
            #  have some default situations.
            try:
        	   self.jobProgress = 100.0 * ( float( xmlDat.visibilityMJD ) - float( xmlDat.jobstartMJD ) ) / ( float( xmlDat.jobstopMJD ) - float( xmlDat.jobstartMJD ) )
            except:  #  Exception caused by failures of float conversions - because fields are empty
                    if xmlDat.state == "Done" or xmlDat.state == "MpiDone":
                        self.jobProgress = 100
                    elif xmlDat.state == "Ending":
                        #  Ending state is annoying - use the job's known progress
                        pass
                    else:
                        self.jobProgress = 0

## Returns the index of a job within the list queuedata
def job_index(queuedata,job_input_file):
	idx = 0 
	for item in queuedata:
		item_list = item.split(' ')
		job_file = item_list[1]
		#print("job_input_file = " + job_input_file)
		#print("job_file = " + job_file)
		if (job_input_file == job_file):
			break
		idx += 1
	return idx


## Funtion which appends the job completion percentage to the queue file.  It has 
#  2 modes of operation operating with the respoder or just writing a percent
#  to the queue.  In the first mode this function will run until the respoder 
#  class signals that the correlator process is done.  The second mode simply 
#  writes a constant completion percentage to the queuue.
def append_status(difx, job_input_file, parameters,  responder=None, jobProgress=None ):
	#print ("Append Status, job_input_file = " + str(job_input_file))
	time_interval = 5.0
	jobidx = 0 
	job_done = False
	queuedata = [] 
	difx.cp(parameters.queue,parameters.queue+"~")	
	read_list_file(difx,queuedata,parameters.queue)
	jobidx = job_index(queuedata,job_input_file)
	#print("JOB INDEX = " + str(jobidx))
	#quit()
	# Modify the completion percentage of the current queue item 		      
 	while not job_done:
		first_queue_item = queuedata[jobidx].split(' ')
		if (responder is not None):
			#print("responder is not none")
			first_queue_item[-1] = str(responder.jobProgress)
			#print("responder.jobProgress = " + str(responder.jobProgress))
		elif (jobProgress is not None):
			first_queue_item[-1] = str(jobProgress)  
		queuedata[jobidx] = " ".join(first_queue_item)
		queuedata_string = "\n".join(queuedata)	
		difx.sendFile(parameters.queue+"~",queuedata_string)
       	 	difx.mv(parameters.queue+"~",parameters.queue)
		#print("Job Progress = " + str(first_queue_item[-1])) 
		# Break out of loop and just append constant jobProgress if 
		# that was given
		if (jobProgress is not None):  
			break
		# Continue the loop if we're using the responder to update 
		# job progress
		elif (responder is not None):
			if (responder.jobProgress == 100):
				job_done = True
			time.sleep(time_interval)


# returns a boolean determining if the provided item is still the first item in the queue
def check_queue_file(difx, parameters, current_queue_item):
	queuedata = []
	read_list_file(difx,queuedata,parameters.queue)
	first_item_list = queuedata[0].split(" ")
	first_input_file = first_item_list[1]
	first_input_file.strip()
	current_queue_item.strip() 
	#print("first_input_file = " + str(first_input_file))
	#print("current_queue_item = " + str(current_queue_item))
	if first_input_file != current_queue_item:
		return False
	else:
		return True
		
		

## Funtion used for processing input file lists. The number of 
#  input files processed is used to determine a done percentage to 
#  monitor progress.  This assumes most scans are roughly the same size.
def process_input_file_list(difx,parameters,difxJobs,list_file,job_progress):

	scan_list = []
        jobProgress = 0 
 	idx = 0 

 	read_list_file(difx,scan_list,list_file)


	# Determine full path to each "scan" file 
	path_list = list_file.split("/")
	path_list = path_list[:-1]
	full_path = "/".join(path_list)
	full_path = full_path + "/"

	# If job progress in the queue file is not 0, Determine the done status 
	# of each item and advanced to the correct scan
	#print("job_progress = of list file" + str(job_progress))
	if (float(job_progress) != 0):
		scan_number = (float(job_progress) * float(len(scan_list))) / 100	
		# updated list to only include items that havn't been processed
		#print ("scan number = " + str(scan_number))
		scan_number = int(round(scan_number))
		#print ("scan number (rounded) = " + str(scan_number))
		#scan_list = scan_list[scan_number:]
		idx = scan_number
		print("scan_number = ", scan_number)
		print("job_progress = ", job_progress)
		#exit()

        # Loop though the scans and process each one, update progress in 
 	# queue file
	interrupted = False
 	while idx < len(scan_list):
		scan = scan_list[idx]
		scan_name = full_path + scan
		print("scan_name = " + scan_name)
		difxJobs.inputFile(scan_name)
	        difxJobs.waitTime(300.0)
       		currentJob = difxJobs.newJob()		
		# Not using the false option here becasue we don't 
 		# want to monitor progress using the respoder
		print ("Running DIFX")
		currentJob.start()		
		idx += 1 
		jobProgress = (float(idx)/float(len(scan_list))) * 100	
		jobProgress = round(jobProgress,1) 
		append_status(difx,list_file,parameters,None,jobProgress)
		print("Job progress after scan " + scan_name + " jobProgress = " + str(jobProgress))
		# Check the queue file to see if any items have been added
		if not check_queue_file(difx, parameters, list_file):
			queuedata = []
			read_list_file(difx,queuedata,parameters.queue)
			#print("Check queue file returns true.")
			jobidx = job_index(queuedata,list_file)
       	 		#print("JOB INDEX = " + str(jobidx))
			#quit()
 			break			
	return jobProgress


def initialize_responder(difxJobs,responder):
	#  Add the callback in the responder class instance
	difxJobs.addRelayCallback( responder.difxRelayCallback )
	#  Turn on packet "relays" - this causes guiServer to feed DiFX message traffic to the client
	difxJobs.relayPackets()


#  Callback functions for messages, warnings, and errors.
def messageCallback( argstr ):
    print str( argstr )
    
def warningCallback( argstr ):
    print "WARNING: " + str( argstr )
    
def errorCallback( argstr ):
    print "ERROR: " + str( argstr )

## Determine whether a file is a list of .input files or an .input file.
#  returns a type string
def check_file_type(difx,filename):
	filetype = ""
	filedata = []
	read_list_file(difx,filedata,filename)
	# Obtain full path to files using the txt filename
	path_list = filename.split("/")
	path_list = path_list[:-1]
	path = "/".join(path_list)
	path = path + "/"
	for item in filedata:
		item_full_path = path + item
		item_full_path = item_full_path.strip()
		if (item[0] == "#"):
			continue 
                remote_filepath = difx.ls(item_full_path,"-l")
		if (remote_filepath == None):
			filetype = "input"
			return filetype
	filetype = "list"
	return filetype


def removeQueueItem(difx,parameters,job_input_file_finished,queuedata,queuedata_items_removed):
	difx.cp(parameters.queue,parameters.queue+"~")
	# Make a copy of queuedata
	# Parse the queue and remove the finished item from queuedata
	for queue_item in queuedata:
		#print(queue_item)
		jobdata = queue_item.split(" ")
                queue_number = jobdata[0]
                job_input_file = jobdata[1]
		if (job_input_file_finished == job_input_file):
			queuedata_items_removed.remove(queue_item)
			break
	# Write a new version of the queue text file with the 
	# finished item removed 
	difx.cp(parameters.queue,parameters.queue+"~")
        queuedata_string = "\n".join(queuedata_items_removed)
        difx.sendFile(parameters.queue+"~",queuedata_string)
        difx.mv(parameters.queue+"~",parameters.queue)

def check_job_status(job_input_file,difxJobs):

	# The returned value is the string queue_action which can have 
	# the values continue, next or start the action the queue
	# will take with each of these values are as follows
	#
	# continue - continue processing the current queue item
	# next - move on to the next queue item
	# start - start processing the current queue item
	queue_action = ""
        #print("job_input_file = " + job_input_file)
	statusInfo = difxJobs.jobStatus(job_input_file,True,True) 

	# Slice done status out of status info, syntax is a little wonky
	# dealing with list of tuples	
	done_status = statusInfo[1][0][1][1]

	# Remove leading and trailing whitespace
	done_status = done_status.strip()
        #print("Done status = " + done_status)	
	# Determine queue action based on done status
	if (done_status == 'Done' or done_status == 'MpiDone'):
		queue_action = 'next'
	elif (done_status == "No .difxlog"):
		queue_action = "start"
	else:
		queue_action = "continue"

	# Also check the done status in the queue
	return queue_action

def get_parameters():

	parser = argparse.ArgumentParser()
	parser.add_argument("--hostname","-H",help="Specify the host name were difx will run. This is a mandatory parameter, to specify the local machine you can use localhost as the host name.",required=True)
        parser.add_argument("--port","-p",help="TCP port used to communicated with the difxServer.  This must be the same port number used by guiServer.",required=True)
	parser.add_argument("--queue","-Q",help="Full path and name of the queue file e.g. /path/to/queue.txt",default="/san02/data1/correlator/run/TESTS/python_queue_testing/queue.txt")
	parameters = parser.parse_args()
	# The parameters are getting an extra space for some reason 
	# need to remove them...
	for x in parameters.__dict__:
		parameters.__dict__[x] = parameters.__dict__[x].strip()	
	return parameters
	
def initialize(difx,parameters):
	# Set up difx client class instance named difx
        difx.connect(parameters.hostname,parameters.port)
        difx.monitor()

## Reads a text file list and returns a list object with the file's contents 
def read_list_file(difx,queuedata,filename):
	queuedata_string = ''
	# Read existing queue data 
       	queuedata_string = difx.getFile(filename)
        # Construct queuedata list
        queuedata[:] = queuedata_string.split("\n")
        # Scrub empty elements
        while '' in queuedata:
                queuedata.remove('')


def run_queue(difx,difxJobs,parameters):
	responder = Responder()
	queue_number = 0
	job_input_file = ""
	empty_queue = False
	queuedata = []
	
	# Set up JobControl object to run difx jobs
	difxJobs.connect(parameters.hostname,str(parameters.port))
	difxJobs.monitor()

	# Set up responder class to monitor difx job progress
	initialize_responder(difxJobs,responder)
	
	
	# Loop over the jobs in the queue and run each
	while not empty_queue:
	
		# Read the queue file
		read_list_file(difx,queuedata,parameters.queue)				
		queuedata_items_removed = list(queuedata)
		
		# If the queue is empty set empty_queue to true and terminate the loop
		if (0 == len(queuedata)):
			empty_queue = True
			continue
		
		# Parse the first item in the queue
		jobdata = queuedata[0].split(" ")
		queue_number = jobdata[0]
		job_input_file = jobdata[1]
		job_progress = jobdata[2]
		#print("job_progress = " + str(job_progress))
	
		# Check if input file exists if not move on to the next item in the queue
		remote_filepath = difx.ls(job_input_file,"-l")
		if (remote_filepath == None):
			print("Input file " + job_input_file + " does not exist, moving on to the next item in queue.")
			# Write something in the queue file?
			continue
	               
		# Check input file type, if it's an input file list process it separately as
	        # one "job" with a completion percentage based on the number of scans processed
		# by the correlator
		filetype = check_file_type(difx,job_input_file)
		if (filetype == "list"):
			job_progress = process_input_file_list(difx,parameters,difxJobs,job_input_file,job_progress)
			# Check job status and remove only if complete, process_input_file_list 
			# could break and return an unfinished job if a queue item has been added
			# before the list finished processing
			if (job_progress >= 100):
				removeQueueItem(difx,parameters,job_input_file,queuedata,queuedata_items_removed)	
			continue		
	
		# Check job status based on completion percentage in the queue file 
		# and the difxlog file.  If job is not complete simply monitor progress
		# until the job completes 
		if (check_job_status(job_input_file,difxJobs) == "start"):
			difxJobs.inputFile(job_input_file)
			difxJobs.waitTime(300.0)
			currentJob = difxJobs.newJob()
			currentJob.start(False)
			append_status(difx,job_input_file,parameters,responder) 
			removeQueueItem(difx,parameters,job_input_file,queuedata,queuedata_items_removed)				
		elif (check_job_status(job_input_file,difxJobs) == "continue"):
			append_status(difx,job_input_file,parameters,responder)
	                removeQueueItem(difx,parameters,job_input_file,queuedata,queuedata_items_removed)
		elif (check_job_status(job_input_file,difxJobs) == "next"):
			removeQueueItem(difx,parameters,job_input_file,queuedata,queuedata_items_removed)
	

def close(difx):
        difx.close()

def main():
        difx = DiFXControl.Client()
	difxJobs = DiFXJobControl.Client()
	status = 0

	# Attempt to catch keyboard interupt
	def signal_handler(*args):
		print("Key board interrupt clean up and exit.")
		difx.close()
		difxJobs.close()
		sys.exit()

        signal.signal(signal.SIGINT, signal_handler)
	try:
 		parameters = get_parameters()
       		initialize(difx,parameters)
		run_queue(difx,difxJobs,parameters) 
	finally:
		close(difxJobs)
		close(difx)

if __name__=='__main__':
	main()

/************************************************************************/
/*                                                                      */
/* Creates a file in the same directory as the root file which can be   */
/* used to signal other running fourfit processes that a fringe file    */
/* is about to be create. This is necessary to keep simultaneously      */
/* running fourfit processes from clobbering each others files          */
/* the lock file has as a name with the following format:               */
/* PROC_ID.lock, and contains the value of the epoch second at which    */
/* it was generated, this might be somewhat vulnerable to process-id    */
/* recycling, but as long we are not spawning thousands of fourfit      */
/* processes at once that shouldn't be a problem                        */
/*                                                                      */
/*      Inputs:         rootname        Full pathname of the root file  */
/*                                                                      */
/*      Output:         return value    0=OK, else bad                  */
/*                                                                      */
/* Created Jan 24 2017 JPB                                              */
/*                                                                      */
/************************************************************************/

#include <sys/types.h>
#include <unistd.h>
#include <sys/time.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "write_lock_mechanism.h"


void init_lockfile_data(lockfile_data_struct* data)
    {
    data->validity = LOCK_INVALID;
    data->seq_number = 0;
    data->pid = 0;
    data->time_sec = 0;
    data->time_usec = 0;
    int i = 0;
    for(i=0; i<256; i++){data->hostname[i] = '\0';}
    for(i=0; i<MAX_LOCKNAME_LEN; i++){data->lockfile_name[i] = '\0';}
    }

void clear_global_lockfile_data()
    {
    init_lockfile_data(&global_lockfile_data);
    }

void remove_lockfile()
    {
    if(global_lockfile_data.validity == LOCK_VALID)
        {
        remove(global_lockfile_data.lockfile_name);
        clear_global_lockfile_data();
        }
    }
    
int parse_lockfile_name(char* lockfile_name_base, lockfile_data_struct* result)
    {
  
    init_lockfile_data(result);

    strcpy(result->lockfile_name, lockfile_name_base);

    //tokenize the lockfile name base, (note: this modifies the input)
    char* ptr;
    ptr = strtok(lockfile_name_base, ".");

    if(ptr != NULL){sscanf(ptr, "%u", &(result->pid));}
    else{return LOCK_PARSE_ERROR;}
    
    ptr = strtok(NULL, ".");

    if(ptr != NULL){sscanf(ptr, "%u", &(result->seq_number));}
    else{return LOCK_PARSE_ERROR;}
    
    ptr = strtok(NULL, ".");

    if(ptr != NULL){sscanf(ptr, "%x", &(result->time_sec));}
    else{return LOCK_PARSE_ERROR;}
    
    ptr = strtok(NULL, ".");
    
    if(ptr != NULL){sscanf(ptr, "%x", &(result->time_usec));}
    else{return LOCK_PARSE_ERROR;}
    
    ptr = strtok(NULL, ".");
    
    if(ptr != NULL){sscanf(ptr, "%s", &(result->hostname));}
    else{return LOCK_PARSE_ERROR;}

    result->validity = LOCK_VALID;
    return 0;
    }

int check_stale(lockfile_data_struct* other)
    {
    //returns LOCK_STATUS_OK if the other lock is stale 
    //returns LOCK_STALE_ERROR if the lock is not stale

    //check for stale-ness irrespective of priority or host
    struct timeval timevalue;
    gettimeofday(&timevalue, NULL);
    unsigned long int epoch_sec = timevalue.tv_sec;
    unsigned long int micro_sec = timevalue.tv_usec;

    if( other->time_sec < epoch_sec )
        {
        if( (epoch_sec - other->time_sec) > LOCK_STALE_SEC )
            {
            //issue warning, do not have priority
            msg ("Error: stale lock file detected: %s ", 3, other->lockfile_name);
            return LOCK_STALE_ERROR;
            }
        }

    return LOCK_STATUS_OK;
    }


int lock_has_priority(lockfile_data_struct* other)
    {
    //returns LOCK_PROCESS_HAS_PRIORITY if this process has priority over the other lock 
    //returns LOCK_PROCESS_NO_PRIORITY if this process does not have priority
    //returns LOCK_STALE_ERROR if there is a stale lock error

    //no need to check for stale-ness, this function is only called 
    //if the lock is stolen

    //strict temporal ordering between the processes is not totally
    //necessary, however, we do need a robust mechanism for deciding
    //the ordering between processes, for this reason we use the pid
    //in the rare circumstances where the pid is the same, because
    //of different hosts or pid recycling, we defer to using time ordering
    //if that in turn fails, then the lock with be deleted and we try again
    
    if(global_lockfile_data.pid < other->pid)
        {
        return LOCK_PROCESS_HAS_PRIORITY;
        }
    else if( global_lockfile_data.pid == other->pid) //different hosts or pid recycling
        {
        if( global_lockfile_data.time_sec < other->time_sec )
            {
            return LOCK_PROCESS_HAS_PRIORITY;
            }
        else if ( global_lockfile_data.time_sec == other->time_sec)
            {
            if( global_lockfile_data.time_usec < other->time_usec)
                {
                return LOCK_PROCESS_HAS_PRIORITY;
                }
            else
                {
                return LOCK_PROCESS_NO_PRIORITY;
                }
            }
        else
            {
            return LOCK_PROCESS_NO_PRIORITY;
            }
        }
    else
        {
        return LOCK_PROCESS_NO_PRIORITY;
        }

    }

int create_lockfile(char *rootname, char* lockfile_name)
    {
    //max file extent number seen at time of lock file creation 
    //(or rather, the time which fileset() was run)
    extern int max_seq_no;
  
    //get the host name
    char host_name[256] = {'\0'};
    int ret_val = gethostname(host_name, 256);
    if( ret_val != 0)
        {
        msg("Fatal error retrieving host name: create_lockfile.", 3);
        exit(1);
        };
  
    //get the process id,
    pid_t this_pid = getpid();
    unsigned int pid = this_pid;

    //then get the epoch second 
    struct timeval timevalue;
    gettimeofday(&timevalue, NULL);
    unsigned long int epoch_sec = timevalue.tv_sec;
    unsigned long int micro_sec = timevalue.tv_usec;
    unsigned int sequence_to_reserve = max_seq_no + 1;

    //dump the process id into a string to create the filename
    unsigned int i = 0;
    for(i=0; i<MAX_LOCKNAME_LEN; i++){lockfile_name[i] = '\0';}
    
    //copy in the scan directory and append the filename
    strcpy(lockfile_name, rootname);
    char* end_ptr = strrchr(lockfile_name, '/');
    end_ptr++;
    sprintf(end_ptr, "%u.%u.%x.%x.%s.lock", pid, sequence_to_reserve, epoch_sec, micro_sec, host_name);
    
    FILE* lockfile = fopen(lockfile_name, "w+" );
    if (lockfile!=NULL)
        {
        char time_buffer[100] = {'\0'};
        sprintf(time_buffer, "%lu\n", epoch_sec);
        fputs(time_buffer, lockfile);
        char temp_buffer[100] = {'\0'};
        sprintf(temp_buffer, "%d\n", pid);
        fputs(temp_buffer, lockfile);
        fclose(lockfile);
        
        //variables so that the signal handler can remove the lock
        //file if an interrupt is caught
        global_lockfile_data.validity = LOCK_VALID;
        global_lockfile_data.seq_number = sequence_to_reserve;
        global_lockfile_data.pid = this_pid;
        global_lockfile_data.time_sec = epoch_sec;
        global_lockfile_data.time_usec = micro_sec;
        strcpy(global_lockfile_data.hostname, host_name);
        strcpy(global_lockfile_data.lockfile_name, lockfile_name);

        }
    else
        {
        return LOCK_FILE_ERROR;
        }
    
    return LOCK_STATUS_OK;
    }

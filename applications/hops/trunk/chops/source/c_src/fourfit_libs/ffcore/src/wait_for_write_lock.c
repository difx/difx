/************************************************************************/
/*                                                                      */
/* Scan's the root directory for any lock files, if it finds any other  */
/* than the one related to the current process, then it reads them and  */
/* then determines the current processes order in the queue, it then   */
/* waits until the all preceeding lock files have been removed before   */
/* returning and allowing this process to write a file                  */
/*                                                                      */
/*      Inputs:         rootname        Full pathname of the root file  */
/*                      lock file name  name of the lock file in dir    */
/*                      fileset struct to store directory information   */
/*                                                                      */
/*      Output:         return value    0=OK, else bad                  */
/*                                                                      */
/* Created Jan 25 2017 JPB                                              */
/*                                                                      */
/************************************************************************/

#include <sys/types.h>
#include <sys/time.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <unistd.h>
#include "fileset.h"
#include "write_lock_mechanism.h"

//returns LOCK_PROCESS_HAS_PRIORITY if this process is at the front of the queue
//returns LOCK_PROCESS_NO_PRIORITY if otherwise
//returns and error code (various, see write_lock_mechanism.h) if an error
int at_front(char* rootname, char* lockfile_name)
    {

    //figure out root directory
    char root_dir[MAX_LOCKNAME_LEN] = {'\0'};
    strcpy(root_dir, rootname);
    char* end_ptr_a = strrchr(root_dir, '/');
    end_ptr_a++;
    *end_ptr_a = '\0';
    
    int process_priority = LOCK_PROCESS_HAS_PRIORITY;
    char temp_lock_name[MAX_LOCKNAME_LEN] = {'\0'};
    lockfile_data_struct temp_lock_struct;
    
    //scan the list of all files in the directory 
    //for ones with the ".lock" extension
    DIR* d;
    struct dirent* dir;
    d = opendir(root_dir);
    if(d)
        {
        while( (dir = readdir(d)) != NULL)
            {
            if(strstr(dir->d_name, ".lock") != NULL)
                {
                //found a lock file already in the directory
                //so this process cannot have priority
                process_priority = LOCK_PROCESS_NO_PRIORITY;
                //check if the other file is stale (this is an error)
                strcpy(temp_lock_name, dir->d_name);
                init_lockfile_data(&temp_lock_struct);
                int error_code = parse_lockfile_name(temp_lock_name, &temp_lock_struct);
                if(error_code != LOCK_STATUS_OK)
                    {
                    msg ("Error: un-parsable lock file name: %s ", 3, dir->d_name);
                    return LOCK_PARSE_ERROR;
                    }
                int stale_lock = check_stale(&temp_lock_struct);
                if(stale_lock != LOCK_STATUS_OK)
                    {
                    closedir(d);
                    return LOCK_STALE_ERROR;
                    }
                }
            }
        closedir(d);
        }
    else
        {
        msg ("Error: can't access directory: %s ", 3, root_dir);
        return LOCK_FILE_ERROR;
        }

    //don't have priority, bail out
    if(process_priority != LOCK_PROCESS_HAS_PRIORITY){return process_priority;};
    
    //no other locks present, so go ahead and try to create a lock file
    int lock_retval = create_lockfile(rootname, lockfile_name);

    if(lock_retval == LOCK_STATUS_OK)
        {
        //created the lock file OK, but now we need to make sure
        //that no other process stole it out from under us

        //strip out the lockfile base name
        char lockfile_base[MAX_LOCKNAME_LEN] = {'\0'};
        char* end_ptr_b = strrchr(lockfile_name, '/');
        end_ptr_b++;
        strcpy(lockfile_base, end_ptr_b);

        //look for other lock files that may have snuck in
        d = opendir(root_dir);
        if(d)
            {
            while( (dir = readdir(d)) != NULL)
                {
                if(strstr(dir->d_name, ".lock") != NULL)
                    {
                    strcpy(temp_lock_name, dir->d_name);
                    if(strcmp(lockfile_base, temp_lock_name) != 0) //not our own lock file
                        {
                        init_lockfile_data(&temp_lock_struct);
                        int error_code = parse_lockfile_name(temp_lock_name, &temp_lock_struct);
                        if(error_code != 0)
                            {
                            msg ("Error: un-parsable lock file name: %s ", 3, dir->d_name);
                            return LOCK_PARSE_ERROR;
                            }
                        process_priority = lock_has_priority(&temp_lock_struct);
                        if(process_priority != LOCK_PROCESS_HAS_PRIORITY)
                            {
                            //either we don't have write priority or an error occured    
                            break;
                            }
                        }
                    }
                }
                closedir(d);
            }
        else
            {
            //something went wrong reading the directory, clean up
            remove_lockfile();
            lockfile_name[0] = '\0';
            msg ("Error: can't access directory: %s ", 3, root_dir);
            return LOCK_FILE_ERROR;
            }

        
        if(process_priority != LOCK_PROCESS_HAS_PRIORITY)
            {
            //some other process created a lock file at almost the same time
            //and we don't have priority, so defer to the other process, delete our lock
            //and return 0 (we don't have priority) 
            remove_lockfile();
            lockfile_name[0] = '\0';
            return process_priority;
            }
        
        //made it here so this process has write priority
        return process_priority;
        
        }
    else
        {
        msg ("Error: could not create write lock on directory: %s ", 3, root_dir);
        return LOCK_FILE_ERROR;
        }
    
    }

int wait_for_write_lock(char* rootname, char* lockfile_name, struct fileset *fset)
    {
    //this function gets the max file extent number seen at time of lock file creation 
    //(or rather, the time which fileset() was run)
    extern int fileset(char*, struct fileset* );
    extern int max_seq_no;
    
    int ret_val;
    
    //wait until this process is at the front of the write queue
    int is_at_front = 0;
    int n_checks = 0;
    do
        {
        //check for max sequence number
        ret_val = fileset(rootname, fset);
        if(ret_val != 0)
            {
            return LOCK_FILESET_FAIL;
            }
        else
            {
                max_seq_no = fset->maxfile;
            }
          
        is_at_front = at_front(rootname, lockfile_name);
        n_checks++;
        if(is_at_front == LOCK_PROCESS_NO_PRIORITY){usleep(100000);}
        }
    while( is_at_front == LOCK_PROCESS_NO_PRIORITY && n_checks < LOCK_TIMEOUT );

    //couldn't get a write lock because of time out
    if(n_checks >= LOCK_TIMEOUT)
        {
        msg ("Error: lock file time-out error associated with: %s ", 3, rootname);
        return LOCK_TIMEOUT_ERROR;
        }

    if(is_at_front != LOCK_PROCESS_HAS_PRIORITY)
        {
        //some other error has occurred
        return is_at_front;
        }

    //made it here, so we have write priority now, just need to
    //check/update the extent number for type-2 files and return it
    ret_val = fileset(rootname, fset);
    if(ret_val == 0)
    {
        return LOCK_STATUS_OK;
    }
    else
    {
        return LOCK_FILESET_FAIL;
    }

    }

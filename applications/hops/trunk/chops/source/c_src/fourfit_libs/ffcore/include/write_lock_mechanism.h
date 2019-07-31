#ifndef WRITE_LOCK_MECHANISM_H__
#define WRITE_LOCK_MECHANISM_H__

#include "fileset.h"

//number of chars in lock file name
#define MAX_LOCKNAME_LEN 512

//wait time allowed before a lock file is declared stale
//this is 5 minutes...probably much longer than needed
#define LOCK_STALE_SEC 300

//number of lock attempts before time-out error
//this is roughly 15 minutes...probably much longer than needed
#define LOCK_TIMEOUT 9000

//struct validity
#define LOCK_VALID 0
#define LOCK_INVALID -1

//return error codes
#define LOCK_FILESET_FAIL -6
#define LOCK_TIMEOUT_ERROR -5
#define LOCK_FILE_ERROR -4
#define LOCK_STALE_ERROR -3
#define LOCK_PARSE_ERROR -2
#define LOCK_PROCESS_NO_PRIORITY -1
#define LOCK_STATUS_OK 0
#define LOCK_PROCESS_HAS_PRIORITY 1


//struct of holding data about the lock file's creation
struct lockfile_data
{
    int validity;
    unsigned int seq_number;
    unsigned int pid;
    unsigned long int time_sec;
    unsigned long int time_usec;
    char hostname[256];
    char lockfile_name[MAX_LOCKNAME_LEN];
};

typedef struct lockfile_data lockfile_data_struct;

//global variables provided for signal handling and clean up
extern lockfile_data_struct global_lockfile_data;

void init_lockfile_data(lockfile_data_struct* data);

void clear_global_lockfile_data();

void remove_lockfile(); //must go through global variables

int parse_lockfile_name(char* lockfile_name_base, lockfile_data_struct* result);

int create_lockfile(char *rootname, char* lockfile_name);

int check_stale(lockfile_data_struct* other);

int lock_has_priority(lockfile_data_struct* other);

int at_front(char* rootname, char* lockfile_name);

int wait_for_write_lock(char* rootname, char* lockfile_name, struct fileset *fset);

#endif /* end of include guard: WRITE_LOCK_MECHANISM_H__ */

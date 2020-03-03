#ifndef D2M4_PCAL_RECORD_H__
#define D2M4_PCAL_RECORD_H__

#include <stdio.h>

#define AP_TOL 0.001
#define D2M4_NPC_TONES 64
#define D2M4_NPC_FREQS 64
#define D2M4_LBUFF_SIZE 40 * D2M4_NPC_TONES * D2M4_NPC_FREQS + 256

//#define D2M4_PCAL_DEBUG

//stores an individual pcal phasor
struct d2m4_pcal_phasor
{
    char polarization; //polarization code
    int frequency; //frequency in MHz
    int dstr; //keep track of the original datastream this phasor came from
    double real;
    double imag;
};

//stores all the pcal data from a single line of PCAL file
//thisshould be data from single mjd...but for a given mjd,
//there may be multiple records from different data streams
struct d2m4_pcal_record
{
    char antenna[8]; //antenna code
    double mjd; //time
    double tint; //integration time?
    int nchannels; //number of channels
    int ntones; //max number of tones per channel
    struct d2m4_pcal_phasor* phasors;
};

//node for a linked list of pcal data
//so we can load and iterate over the whole PCAL file data in memory
struct d2m4_pcal_list_node
{
    struct d2m4_pcal_record* pcal_record; //node data
    struct d2m4_pcal_list_node* next; //next node in the list
    struct d2m4_pcal_list_node* previous; //previous node in the list -- do we need bidirectional traversal?
};

//initialize a pcal record
extern void d2m4_pcal_init_record(struct d2m4_pcal_record* rec);

//destroy a record
extern void d2m4_pcal_free_record(struct d2m4_pcal_record* rec);

//populate a record from a PCAL file data line
extern void d2m4_pcal_populate_record(struct d2m4_pcal_record* rec, char* line_buffer);

//dump a pcal record into a line buffer, returns the lenght of the dumped data (in chars)
extern int d2m4_pcal_dump_record(struct d2m4_pcal_record* rec, char* line_buffer, int buffer_size);

//copy the data from record2 into record1
extern void d2m4_pcal_copy_record(struct d2m4_pcal_record* record1, struct d2m4_pcal_record* record2);

//return 1 if two pcal_records can be merged (same AP), and 0 if otherwise
extern int d2m4_pcal_can_merge_records(struct d2m4_pcal_record* record1, struct d2m4_pcal_record* record2);

//returns the number of unique polaization in a pcal record
extern int d2m4_pcal_count_unique_polarizations(struct d2m4_pcal_record* record);

//allocates space for a new pcal_record which is the result of merging two
//individual pcal_records (of a single mjd, but different data streams)
//note that the merged records should be deleted afterwards, but this function does not take care of this
extern struct d2m4_pcal_record* d2m4_pcal_merge_record(struct d2m4_pcal_record* record1, struct d2m4_pcal_record* record2);

//run through a whole PCAL file and read it into memory
//return a the pcal records as a linked list
extern struct d2m4_pcal_list_node* d2m4_pcal_create_list(FILE* fin);

struct d2m4_pcal_list_node* d2m4_pcal_merge_datastreams_in_list(struct d2m4_pcal_list_node* input_pcal_list);

//destroy all the data in the linked-list
extern void d2m4_pcal_free_list(struct d2m4_pcal_list_node* pcal_list);

#endif

#include <stdlib.h>
#include <stdio.h>

#include "d2m4_pcal_record.h"

struct d2m4_pcal_list_node* d2m4_pcal_merge_datastreams_in_list(struct d2m4_pcal_list_node* input_pcal_list)
{
    int in_count = 0;
    int out_count = 0;
    //need a pointer to the head and tail node on input list
    struct d2m4_pcal_list_node* input_tail = input_pcal_list;

    //need a pointer to the head and tail node on output list
    struct d2m4_pcal_list_node* output_tail = NULL;
    struct d2m4_pcal_list_node* output_head = NULL;

    //temporary pointer to the merged record
    struct d2m4_pcal_record* merged_record = NULL;

    //intialize the output linked list
    output_head = (struct d2m4_pcal_list_node*) malloc( sizeof( struct d2m4_pcal_list_node) );
    output_tail = output_head;
    output_tail->pcal_record = NULL;
    output_tail->next = NULL; //nothing here yet
    output_tail->previous = NULL; //head has no previous node

    //initialize the first output tail record
    output_tail->pcal_record = malloc( sizeof( struct d2m4_pcal_record ) );
    d2m4_pcal_init_record(output_tail->pcal_record);
    d2m4_pcal_copy_record(output_tail->pcal_record, input_tail->pcal_record);

    //iterate the input tail
    input_tail = input_tail->next;

    //printf("begin ptr to output_pcal_list %p\n", output_pcal_list);

    //we are asumming that the records in the list are in time-order
    while(input_tail->next != NULL && input_tail->pcal_record != NULL)
    {
        output_tail->next = NULL;
        //printf("ptr to input_tail %p\n", input_tail);
        if( d2m4_pcal_can_merge_records(output_tail->pcal_record, input_tail->pcal_record) )
        {
            merged_record = NULL;
            merged_record = d2m4_pcal_merge_record( output_tail->pcal_record, input_tail->pcal_record );
            if(merged_record != NULL)
            {
                #ifdef D2M4_PCAL_DEBUG
                printf("successfully merged pcal records from same APs %lf, %lf\n", output_tail->pcal_record->mjd, merged_record->mjd);
                #endif
                d2m4_pcal_free_record(output_tail->pcal_record);
                output_tail->pcal_record = merged_record;
            }
            else
            {
                //ERROR
                printf("Error: d2m4_pcal_merge_datastreams_in_list could not merge pcal records!\n");
            }
        }
        else //can't merge records, so just bump the output tail and copy in the input_tail's record
        {
            //create the next node in the linked output list
            output_tail->next = (struct d2m4_pcal_list_node*) malloc( sizeof( struct d2m4_pcal_list_node) );
            //output_tail->next->previous = output_tail;
            output_tail = output_tail->next;
            output_tail->next = NULL;
            output_tail->pcal_record = malloc( sizeof( struct d2m4_pcal_record ) );
            d2m4_pcal_init_record(output_tail->pcal_record);
            d2m4_pcal_copy_record(output_tail->pcal_record, input_tail->pcal_record);
            #ifdef D2M4_PCAL_DEBUG
            printf("copying pcal record from next AP %lf \n", output_tail->pcal_record->mjd);
            #endif
            out_count++;
        }

        //Now we move to the next input node
        input_tail = input_tail->next;
        in_count++;
    };

    //now make sure the output linked list is terminated
    //output_tail->next = NULL;

    #ifdef D2M4_PCAL_DEBUG
    printf("d2m4_pcal_merge_datastreams_in_list: merged list of length %d into list of length %d\n", in_count, out_count);
    #endif

    return output_head;

}

#include <stdlib.h>
#include <stdio.h>

#include "d2m4_pcal_record.h"

void d2m4_pcal_free_list(struct d2m4_pcal_list_node* pcal_list)
{
    //first run down the list and destroy the pcal record data
    struct d2m4_pcal_list_node* prev_tail = NULL;
    struct d2m4_pcal_list_node* tail = pcal_list;

    do
    {
        //printf("ptr to tail %p\n", tail);
        if(tail->pcal_record != NULL)
        {
            d2m4_pcal_free_record(tail->pcal_record);
            tail->pcal_record = NULL;
        }
        prev_tail = tail;
        tail = tail->next;
        //now destroy the previous tail
        free(prev_tail);
    }
    while(tail != NULL && tail->next != NULL);
}

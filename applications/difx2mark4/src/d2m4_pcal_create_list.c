#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "d2m4_pcal_record.h"


struct d2m4_pcal_list_node* d2m4_pcal_create_list(FILE* fin)
{
    int version = 0;
    char* pc = NULL;
    char* line = NULL;
    //buffer for a single line of an input pcal file
    char lbuff[D2M4_LBUFF_SIZE];
    //need a pointer to the head and tail head node
    struct d2m4_pcal_list_node* head = NULL;
    struct d2m4_pcal_list_node* tail = NULL;

    head = (struct d2m4_pcal_list_node*) malloc( sizeof( struct d2m4_pcal_list_node) );
    head->pcal_record = NULL;
    head->next = NULL; //nothing here yet
    head->previous = NULL; //head has no previous node
    tail = head;

    do
    {
        line = fgets(lbuff, D2M4_LBUFF_SIZE, fin);
        if(line == NULL)
        {
            break;
        }
        else if (*line == '#')
        {
            pc = strstr (line, "File version =");
            if(pc)         // get version, if present
            {
                sscanf (pc + 14, "%d", &version);
            }
            continue;       // skip over comment lines
        }

        if( version == 0)   // legacy is version 0
        {
            //roll over and die right now
            printf("Fatal error! PCAL version 0 not supported!\n");
            exit(1);
        }
        else  // by elimination, version must be 1
        {
            //make some space of the pcal data record, then initialize and populate the record
            tail->pcal_record = malloc( sizeof( struct d2m4_pcal_record ) );
            d2m4_pcal_init_record(tail->pcal_record);
            d2m4_pcal_populate_record(tail->pcal_record, line);

            //create the next node in the linked list
            tail->next = (struct d2m4_pcal_list_node*) malloc( sizeof( struct d2m4_pcal_list_node) );
            tail->next->previous = tail;
            tail = tail->next;
            tail->next = NULL;
            tail->pcal_record = NULL;
        }
    }
    while(line != NULL);

    //done here
    return head;

}

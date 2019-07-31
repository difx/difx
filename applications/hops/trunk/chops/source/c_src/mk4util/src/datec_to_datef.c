#include "general.h"
#include "mk4_util.h"

void
datec_to_datef(struct datec *input, struct datef *output)
    {
    output->year = input->year;
    output->month = input->month;
    output->day = input->day_of_month;
    output->hour = input->hour;
    output->minute = input->minute;
    output->second = input->second;
    }

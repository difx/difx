#include "general.h"
#include "mk4_util.h"

void
datef_to_datec (struct datef *input, struct datec *output)
    {
    int i, doy;
    static int nday[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

    output->year = input->year;
    doy = 0;
    for (i=0; i<(input->month - 1); i++) doy += nday[i];
    if (((input->year % 4) == 0) && (input->month > 2)) doy++; 
    output->day_of_year = doy + input->day;
    output->month = input->month;
    output->day_of_month = input->day;
    output->hour = input->hour;
    output->minute = input->minute;
    output->second = input->second;
    }

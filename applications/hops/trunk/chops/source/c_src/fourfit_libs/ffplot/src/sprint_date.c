#include "general.h"
#include <stdlib.h>

char *
sprint_date (struct datec date)
    {
    char *temp;
    char *result;

    result = (char *) malloc(15);
    temp = (char *) malloc(10);
    sprintf(temp, "%hd", date.day_of_month);
    switch(date.month)
	{
	case 1:
	    strcat(temp, "JAN");
	    break;
	case 2:
	    strcat(temp, "FEB");
	    break;
	case 3:
	    strcat(temp, "MAR");
	    break;
	case 4:
	    strcat(temp, "APR");
	    break;
	case 5:
	    strcat(temp, "MAY");
	    break;
	case 6:
	    strcat(temp, "JUN");
	    break;
	case 7:
	    strcat(temp, "JUL");
	    break;
	case 8:
	    strcat(temp, "AUG");
	    break;
	case 9:
	    strcat(temp, "SEP");
	    break;
	case 10:
	    strcat(temp, "OCT");
	    break;
	case 11:
	    strcat(temp, "NOV");
	    break;
	case 12:
	    strcat(temp, "DEC");
	    break;
	} /* switch */
    sprintf(result, "%s.%hd", temp, date.year);
    free(temp);
    return(result);
    } /* sprint_date */		

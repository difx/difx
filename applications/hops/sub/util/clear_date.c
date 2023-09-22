#include "mk4_typedefs.h"

void
clear_date (date_struct *timetag)
    {
    timetag->year = 0;
    timetag->day = 0;
    timetag->hour = 0;
    timetag->minute = 0;
    timetag->second = 0.0;
    }

#include <stdio.h>
#include <string.h>

/*++*********************************************************************/
int             ingdec (decdeg, decstr)
    double         decdeg;             /* dec in decimal degrees   */
    char           *decstr;             /* VLBA dec(angle) format string*/  
/**********************************************************************/
/*
%% converts Ingres declination (decimal deg.) into VLBA format string
   The angle convention for the DBMS is decimal degrees. The ingdec 
function converts decimal degrees into the VLBA standard for script
declinations, eg., 21d15'35.44". Ingdec returns two decimal places
in arc-seconds.
---
LANGUAGE:C
ENVIRONMENT: Sun Unix
++$ AUDIT TRAIL
1.0  90Jan03  J. Benson Initial submission
--$
-*/
{
	char  decword[20];
        double secs;
        int   deg,mins,isign;


        *decstr = '\0';
        deg  = decdeg;
        isign = 1;
        if (decdeg < 0.0) 
           isign = -1;
        mins = (decdeg - deg)*60.0;
        secs = (decdeg - deg - mins/60.0)*3600.0;
        mins = isign*mins;
        secs = isign*secs;

        if (deg == 0 && isign == -1)
           sprintf(decword,"-%1dd%1d\'%7.6f\"",deg,mins,secs);
        else
           sprintf(decword,"%3dd%1d\'%7.6f\"",deg,mins,secs);
        strcpy(decstr,decword);
        return (0);
};


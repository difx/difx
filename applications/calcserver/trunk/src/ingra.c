#include <stdio.h>
#include <string.h>


/*++*********************************************************************/
int             ingra (radeg, rastr)
    double         radeg;              /* ra in decimal degrees   */
    char           *rastr;             /* VLBA ra (time) format string*/  
/**********************************************************************/
/*
%% converts Ingres right ascention (decimal deg.) into VLBA format string
   The ingra function converts the right ascention angle retrieved from
Ingres into the VLBA standard string format for time (23h12m35.441s).
Ingra returns three decimal places in the fractional part of seconds.

---
LANGUAGE:C
ENVIRONMENT: Sun Unix
++$ AUDIT TRAIL
1.0  90Jan03  J. Benson Initial submission
--$
-*/
{
	char  raword[20];
        double secs;
        int   hrs,mins;


        *rastr = '\0';
        hrs = radeg / 15.0;
        mins = (radeg - 15.0*hrs)*4.0;
        secs = (radeg - 15.0*hrs - mins/4.0)*4.0*60.0;

        sprintf(raword,"%2dh%1dm%7.6fs",hrs,mins,secs);
        strcpy(rastr,raword);
        return (0);
};

/********************************************************************************/
/*                                                                              */
/* This routine prints the contents of the "inp" structure on the screen when   */
/* in ascii terminal mode.  These inputs determine the behaviour of the "action"*/
/* commands, such as "plot", "read", "edit", and the like.                      */
/*                                                                              */
/* Created March 30 1989 by CJL                                                 */
/* Added options to print out only parts of inputs, April 19 1990, CJL          */
/*                                                                              */
/********************************************************************************/
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include "aedit.h"
#include "pstruct.h"
#include "usearray.h"

#define ALL 1
#define FILTER 2
#define PLOT 3

pr_inputs(string)
char *string;
    {
    extern struct inputs inp;
    int syear,sday,shour,smin,ssec,fyear,fday,fhour,fmin,fsec;
    int i, n, option, p0, pind;
    char c, dummy[30], pname[30], llim[20], ulim[20];
    extern struct usearray user_param;
    extern int output_version;

    n = strlen(string);
    for(i=0;i<n;i++) 
        {
        c = string[i];
        if(isupper(c)) c = tolower(c);
        string[i] = c;
        }
    if(strncmp(string,"all",n) == 0) option = ALL;
    else if(strncmp(string,"filter",n) == 0) option = FILTER;
    else if(strncmp(string,"plot",n) == 0) option = PLOT;
    else 
        {
        msg("Unrecognized inputs option (all, filter, or plot)",2);
        return(-1);
        }

    msg ("", 2);
    msg ("\t****************", 2);
    msg ("\t| AEDIT INPUTS |", 2);
    msg ("\t****************", 2);

    if(option == ALL || option == FILTER) 
        {
        msg ("", 2);
        msg ("", 2);
        msg ("DATA FILTER PARAMETERS", 2);
        msg ("----------------------");

        if(inp.begin != 0 && inp.end != 0) 
            {
            int_to_time(inp.begin,&syear,&sday,&shour,&smin,&ssec);
            int_to_time(inp.end,&fyear,&fday,&fhour,&fmin,&fsec);
            msg ("Timerange:     %2d%03d-%02d%02d%02d to  %2d%03d-%02d%02d%02d", 2,
                    syear,sday,shour,smin,ssec,fyear,fday,fhour,fmin,fsec); 
            }
        else msg ("Timerange:     No limits specified", 2);

        if(inp.proc_begin != 0 && inp.proc_end != 0) 
            {
            int_to_time(inp.proc_begin,&syear,&sday,&shour,&smin,&ssec);
            int_to_time(inp.proc_end,&fyear,&fday,&fhour,&fmin,&fsec);
            msg ("Procrange:     %2d%03d-%02d%02d to  %2d%03d-%02d%02d", 2,
                    syear,sday,shour,smin,fyear,fday,fhour,fmin); 
            }
        else msg ("Procrange:     No limits specified", 2);

        if(strlen(inp.stations) > 0) msg ("Stations:      %s", 2,inp.stations);
        else msg ("Stations:      No restriction specified", 2);

        if(strlen(inp.baselines) > 0) msg ("Baselines:     %s", 2,inp.baselines);
        else msg ("Baselines:     No restriction specified", 2);

        if(strlen(inp.triangles) > 0) msg ("Triangles:     %s", 2,inp.triangles);
        else msg ("Triangles:     No restriction specified", 2);

        if(strlen(inp.quads) > 0) msg ("Quads:         %s", 2,inp.quads);
        else msg ("Quads:         No restriction specified", 2);

        if(strlen(inp.frequencies) > 0) msg ("Frequencies:   %s", 2,inp.frequencies);
        else msg ("Frequencies:   No restriction specified", 2);

        if(strlen(inp.polarizations) > 0) msg ("Polarizations: %s"
                                                , 2,inp.polarizations);
        else msg ("Polarizations: No restriction specified", 2);

        if(inp.experiment > 0) msg ("Experiment:    %d", 2,inp.experiment);
        else msg ("Experiment:    None specified", 2);

        if(strlen(inp.qcodes) > 0) msg ("Qcodes:        %s", 2,inp.qcodes);
        else msg ("Qcodes:        No restriction specified", 2);

        if(strcmp(inp.type, "0,1,2,3,4") != NULL) msg ("Type:          %s", 2, inp.type);
        else msg ("Type:          No restriction specified", 2);

        if(inp.snr[0] > 0.0) msg ("Snrmin:        %5.4g", 2,inp.snr[0]);
        else msg ("Snrmin:        None specified", 2);

        if(inp.snr[1] < 100000.0) msg ("Snrmax:        %5.4g", 2,inp.snr[1]);
        else msg ("Snrmax:        None specified", 2);

        if(inp.bsnr[0] > 0.0) msg ("Bis_snrmin:    %5.4g", 2,inp.bsnr[0]);
        else msg ("Bis_snrmin:    None specified", 2);

        if(inp.bsnr[1] < 1000000.0) msg ("Bis_snrmax:    %5.4g", 2,inp.bsnr[1]);
        else msg ("Bis_snrmax:    None specified", 2);

        if(strlen(inp.sources) > 0) msg ("Sources:       %s", 2,inp.sources);
        else msg ("Sources:       None specified", 2);

        if(inp.length > 0) msg ("Length:        %d", 2,inp.length);
        else msg ("Length:        None specified", 2);

        if(inp.fraction > 0) 
            msg ("Fraction:      >=%d%% of data processed", 2, inp.fraction*10);
        else if(inp.fraction < 0) 
            msg ("Fraction:      <=%d%% of data processed", 2, inp.fraction*(-10));
        else msg ("Fraction:      No restriction specified", 2);

        if((inp.nfreq[0] > 1) || (inp.nfreq[1] < MAXFREQ))
                msg ("Nfreq:         %d to %d", 2,inp.nfreq[0], inp.nfreq[1]);
        else msg ("Nfreq:         No restriction specified", 2);

                                        /* Stored 1-relative in inp, 0-relative */
                                        /* in parameter array */
        p0 = (int)(inp.parameter[0] + .01) - 1;
                                        /* Parameter filter must be out of date */
        if (p0 >= user_param.nparms)
            {
            inp.parameter[0] = 0.0;
            p0 = -1;
            }
        if (p0 >= 0)
            {
            pind = user_param.type[p0].parameter_index;
            if (pind > 0) sprintf (pname, "%s(%d)",
                        user_param.type[p0].parameter_name, pind);
            else sprintf (pname, "%s", user_param.type[p0].parameter_name);
            if (inp.parameter[1] < -0.99e30) sprintf (llim, "-infinity");
            else sprintf (llim, "%g", inp.parameter[1]);
            if (inp.parameter[2] > 0.99e30) sprintf (ulim, "+infinity");
            else sprintf (ulim, "%g", inp.parameter[2]);
            msg  ("User param:  %s  %s to %s", 2, pname, llim, ulim);
            }

        if (output_version == 0)
            msg ("Outversion:    0 (i.e. same as that read in)", 2);
        else
            msg ("Outversion:    %d (overrides whatever was read in)",
                2, output_version);
        }

    if(option == ALL || option == PLOT) 
        {
        msg ("", 2);
        msg ("", 2);
        msg ("PLOTTING PARAMETERS", 2);
        msg ("-------------------", 2);

        msg ("Axis:        Plot %s against %s", 2,inp.y_axis, inp.x_axis);

        if (inp.refrem == REFERENCE) strcpy (dummy, "reference antenna");
        else strcpy (dummy, "remote antenna");
        if (inp.plotby == BASELINE_PLOT)
            msg ("              (station-based quantities use %s)", 2, dummy);

        msg ("Grid:        plot with %d horizontal and %d vertical subplots", 2,
                inp.grid[0], inp.grid[1]); 

        if(inp.yscale[0] == 0.0 && inp.yscale[1] == 0.0)
                msg ("Y-scale:     Plot between extrema of data", 2);
        else msg ("Y-scale:     Plot between %s = %f and %f", 2,
                inp.y_axis,inp.yscale[0],inp.yscale[1]);

        if(inp.xscale[0] == 0.0 && inp.xscale[1] == 0.0)
                msg ("X-scale:     Plot between extrema of data", 2);
        else msg ("X-scale:     Plot between %s = %f and %f", 2,
                inp.x_axis,inp.xscale[0],inp.xscale[1]);

        if(inp.mode == 1) 
                msg ("Mode:        Split (split into 1 plot per source)", 2);
        else if(inp.mode == 0)
                msg ("Mode:        Nosplit (multiple sources per plot)", 2);
        else msg ("Mode:  CRAZY MODE VALUE = %d !!!", 2,inp.mode);

        if (inp.dev_auto == 0)
            msg ("Device:      Device for graphics output = %s", 2,inp.device);
        else if (inp.dev_auto == 1)
            msg ("Device:      Device for graphics output = xwindow", 2);
        else if (inp.dev_auto == 2)
            msg ("Device:      Device for graphics output = ppostscript", 2);
        else if (inp.dev_auto == 3)
            msg ("Device:      Device for graphics output = lpostscript", 2);
        else if (inp.dev_auto == 4)
            msg ("Device:      Device for graphics output = hpgl", 2);
        msg ("", 2);
        }
    }

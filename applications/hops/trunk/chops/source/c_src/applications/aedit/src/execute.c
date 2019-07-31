/****************************************************************************/
/*                                                                          */
/* This routine is where the user's commands are processed.  The "command"  */
/* structure contains the command identification, and all the arguments to  */
/* that command.  Adding commands requires modification only of this        */
/* routine and command_id.c, together with the code to actually do the work */
/* of the new command.  For X-window commands, of course, you also need to  */
/* modify the main control panel.                                           */
/*                                                                          */
/*      Input:          command         Structure containing parsed         */
/*                                      command with arguments              */
/*                                                                          */
/*      Output:         return value    0 for success                       */
/*                                                                          */
/* Created March 30 by CJL                                                  */
/*                                                                          */
/****************************************************************************/
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "aedata.h"
#include "aedit.h"
#include "pstruct.h"
#include "summary.h"

int force_closure;

int execute (esum *data, struct com *command)
                        /* This indirection needed because dynamic memory */
{                       /* allocation in read_data changes array address */
        extern struct inputs inp;
        extern int fscan, batch, output_version;
        extern int rsortstat[], csortstat[], fsortstat[], 
                                        tsortstat[], qsortstat[];
        int i, ret, comno, oldvrs, command_id(), comax;
        float snr;

        ret = 0;                        /* Assume success */

/***************************************************************************/
/* Identify the command by number and feed to the switch *******************/
/***************************************************************************/

        comno = command_id (command->cmnd);
        switch (comno) {

            case 0:             /* EXIT */
            case 100:           /* QUIT */
                if (confirm ("Are you sure?"))
                    {
                    msg ("    aedit session ends -- Goodbye!\n\n", 2);
                    ret = cleanup();
                    exit (ret);
                    }
                break;

            case 1:             /* INPUTS */
                if (command->narg > 1) 
                    {
                    msg ("\tOptions:  \tINPUTS (ALL)", 2);
                    msg ("\t\t\tINPUTS FILTER", 2);
                    msg ("\t\t\tINPUTS PLOT", 2);
                    ret = -1;
                    }
                else 
                    {
                    if (command->narg == 0) sprintf (command->arg1, "all");
                    pr_inputs (command->arg1);
                    }
                break;

            case 2:             /* PLOT */
                if (command->narg > 2)
                    {
                    msg ("\tOptions:  \tPLOT", 2);
                    msg ("\t\t\tPLOT YAXIS_NAME", 2);
                    msg ("\t\t\tPLOT YAXIS_NAME XAXIS_NAME", 2);
                    ret = -1;
                    }
                else ret = plot (data, command->arg1, command->arg2);
                break;

            case 3:             /* CLEAR */
                if (command->narg != 1) 
                    {
                    msg ("\tOptions:  \tCLEAR DATA", 2);
                    msg ("\t\t\tCLEAR CLOSE", 2);
                    msg ("\t\t\tCLEAR INPUTS", 2);
                    msg ("\t\t\tCLEAR PLOT", 2);
                    msg ("\t\t\tCLEAR ALL", 2);
                    ret = -1;
                    }
                else ret = clear (data, command->arg1);
                break;

            case 4:             /* EDIT */
                if (command->narg == 0) 
                    {
                    msg ("\tOptions:  \tEDIT CURSOR", 2);
                    msg ("\t\t\tEDIT INPUTS", 2);
                    msg ("\t\t\tEDIT DUPLICATES SNR", 2);
                    msg ("\t\t\t                QCODES", 2);
                    msg ("\t\t\t                PROCDATE", 2);
                    msg ("\t\t\tEDIT PARENTS", 2);
                    msg ("\t\t\tEDIT CHILDREN", 2);
                    msg ("\t\t\tEDIT CLOSE BASELINES", 2);
                    msg ("\t\t\tEDIT CLOSE TRIANGLES", 2);
                    msg ("\t\t\tEDIT CLOSE", 2);
                    ret = -1;
                    }
                else ret = edit (data, command->arg1, command->arg2);
                break;

            case 5:             /* ZOOM */
                if (batch) 
                    {
                    msg ("ZOOM disabled in batch mode .. non-fatal error", 2);
                    ret = 0;
                    }
                else if (command->narg != 0) 
                    {
                    msg ("ZOOM does not take arguments", 2);
                    ret = -1;
                    }
                else ret = zoom (data);
                break;

            case 6:             /* READ */
                if (command->narg != 1) {
                    msg ("READ takes a single filename argument", 2);
                    ret = -1;
                }
                else ret = read_data (data, command->arg1);
                break;

            case 7:             /* WRITE */
                if (command->narg != 1) {
                    msg ("WRITE takes a single filename argument", 2);
                    ret = -1;
                }
                else ret = write_data (data, command->arg1);
                break;

            case 8:             /* HELP */
                help (command->arg1);
                ret = 0;        /* Failure is never harmful */
                break;

            case 9:             /* UNFLAG */
                if (command->narg != 1) {
                    msg ("\tOptions:  \tUNFLAG ALL", 2);
                    msg ("\t\t\tUNFLAG DUPLICATES", 2);
                    msg ("\t\t\tUNFLAG CURSOR", 2);
                    msg ("\t\t\tUNFLAG QCODES", 2);
                    msg ("\t\t\tUNFLAG SNR", 2);
                    msg ("\t\t\tUNFLAG BSNR", 2);
                    msg ("\t\t\tUNFLAG TIMERANGE", 2);
                    msg ("\t\t\tUNFLAG PROCRANGE", 2);
                    msg ("\t\t\tUNFLAG STATIONS", 2);
                    msg ("\t\t\tUNFLAG BASELINES", 2);
                    msg ("\t\t\tUNFLAG TRIANGLES", 2);
                    msg ("\t\t\tUNFLAG EXPERIMENT", 2);
                    msg ("\t\t\tUNFLAG FREQUENCIES", 2);
                    msg ("\t\t\tUNFLAG TYPE", 2);
                    msg ("\t\t\tUNFLAG SOURCES", 2);
                    msg ("\t\t\tUNFLAG LENGTH", 2);
                    msg ("\t\t\tUNFLAG FRACTION", 2);
                    msg ("\t\t\tUNFLAG NFREQ", 2);
                    msg ("\t\t\tUNFLAG PARAMETER", 2);
                    msg ("\t\t\tUNFLAG PARENTS", 2);
                    msg ("\t\t\tUNFLAG CHILDREN", 2);
                    msg ("\t\t\tUNFLAG NOBASELINES", 2);
                    msg ("\t\t\tUNFLAG NOTRIANGLES", 2);
                    ret = -1;
                }
                else ret = unflag (data, command->arg1);
                break;

            case 10:            /* SUMMARY */
                ret = pr_summary (data, command->arg1);
                break;

            case 11:            /* SETYEAR */
                if (command->narg != 1) {
                    msg ("SETYEAR has one integer argument", 2);
                    ret = -1;
                }
                else ret = set_year (data->fdata, command->arg1);
                break;

            case 12:            /* BATCH */
                if (command->narg != 0) {
                    msg ("BATCH has no arguments", 2);
                    ret = -1;
                }
                else {
                    batch = TRUE;
                    confirm ("OFF");
                    ret = 0;
                }
                break;

            case 13:            /* NOBATCH */
                if (command->narg != 0) {
                    msg ("NOBATCH has no arguments", 2);
                    ret = -1;
                }
                else {
                    batch = FALSE;
                    confirm ("ON");
                    ret = 0;
                }
                break;

            case 14:            /* SORT */
                if (command->narg != 1) {
                    msg ("\tOptions:  \tSORT TIMETAG", 2);
                    msg ("\t\t\tSORT PROCDATE", 2);
                    msg ("\t\t\tSORT SNR", 2);
                    msg ("\t\t\tSORT LENGTH", 2);
                    msg ("\t\t\tSORT BASELINE", 2);
                    msg ("\t\t\tSORT TRIANGLE", 2);
                    msg ("\t\t\tSORT FREQUENCY", 2);
                    msg ("\t\t\tSORT SOURCENAME", 2);
                    msg ("\t\t\tSORT QCODE", 2);
                    msg ("\t\t\tSORT EXPERIMENT", 2);
                    msg ("\t\t\tSORT ROOTCODE", 2);
                    msg ("\t\t\tSORT FAMILY", 2);
                    ret = -1;
                }
                else 
                    {
                    ret = summ_data (data, VERSION);
                    ret += sorter (data->rdata, command->arg1, 0);
                    ret += sorter (data->cdata, command->arg1, 1);
                    ret += sorter (data->fdata, command->arg1, 2);
                    ret += sorter (data->tdata, command->arg1, 3);
                    }
                break;

            case 15:            /* UNSORT */
                if (command->narg != 0) {
                    msg ("UNSORT has no arguments", 2);
                    ret = -1;
                }
                else {
                    fsortstat[0] = csortstat[0] = rsortstat[0] = 
                                        tsortstat[0] = qsortstat[0] = 0;
                    msg ("Original (as read in) sort order restored", 2);
                    ret = 0;
                }
                break;

            case 16:            /* RUN */
                if (command->narg != 1) {
                    msg ("Usage: RUN filename", 2);
                    msg ("       where filename contains aedit commands", 2);
                    ret = -1;
                }               /* Note use of data pointer, allowing recursive */
                                /* calls of execute() */
                else ret = run_com_file (data, command->arg1);
                break;

            case 17:            /* FPLOT */
                if (batch) {
                    msg ("FPLOT disabled in batch mode .. non-fatal error", 2);
                    ret = 0;
                }
                else if (command->narg != 0) {
                    msg ("FPLOT does not take arguments", 2);
                    ret = -1;
                }
                else ret = fplot (data);
                break;

            case 18:            /* PARAMETER */
                ret = get_param (data, command->arg1, command->arg2, command->remarg);
                break;

            case 19:            /* PWRITE */
                if ((command->narg != 1) && (command->narg != 2)) 
                    {
                    msg ("PWRITE has one or two arguments", 2);
                    ret = -1;
                    }
                else ret = write_param (data, command->arg1, command->arg2);
                break;

            case 20:            /* PLIST */
                if (command->narg != 0) 
                    {
                    msg ("PLIST has no arguments", 2);
                    ret = -1;
                    }
                ret = param_list (data);
                break;

            case 21:            /* TWRITE */
                if (command->narg != 1) {
                    msg ("TWRITE takes a single filename argument", 2);
                    ret = -1;
                }
                else ret = write_tdata (data, command->arg1);
                break;

            case 22:            /* CLOSE */
                force_closure = FALSE;
                if (command->narg > 1)
                    {
                    msg ("CLOSE has at most one argument", 2);
                    ret = -1;
                    }
                else if (command->narg == 1) 
                    {
                    if (strncmp (command->arg1, "force", strlen(command->arg1)) == 0)
                        force_closure = TRUE;
                    else msg ("Unrecognized close argument '%s'", 2, command->arg1);
                    }
                if (ret == 0) summ_data (data, CLOSURE);
                force_closure = FALSE;
                break;


            case 30:            /* GRID */
                ret = -1;
                if (command->narg != 2)
                    msg ("GRID has two integer arguments", 2);
                else if (sscanf (command->arg1, "%d", &(inp.grid[0])) != 1)
                    msg ("GRID bad argument", 2);
                else if (sscanf (command->arg2, "%d", &(inp.grid[1])) != 1)
                    msg ("GRID bad argument", 2);
                else if (inp.grid[0] < 1 || inp.grid[1] < 1 || inp.grid[0] > 2 ||
                                inp.grid[1] > 10)
                    msg ("GRID specifications out of range (1-2, 1-10)", 2);
                else ret = 0;
                break;

            case 31:            /* YSCALE */
                ret = -1;
                if (command->narg == 0) 
                    {
                    inp.yscale[0] = 0.0;
                    inp.yscale[1] = 0.0;
                    ret = 0;
                    }
                else if (command->narg != 2)
                    msg ("YSCALE has two floating arguments", 2);
                else if (sscanf (command->arg1, "%f", &(inp.yscale[0])) != 1)
                    msg ("YSCALE bad argument", 2);
                else if (sscanf (command->arg2, "%f", &(inp.yscale[1])) != 1)
                    msg ("YSCALE bad argument", 2);
                else if (inp.yscale[0] >= inp.yscale[1])
                    msg ("Ymin must be less than ymax", 2);
                else ret = 0;
                break;

            case 32:            /* AXIS */
                ret = axis (command->arg1, command->arg2);
                break;

            case 33:            /* MODE */
                if (command->narg != 1) ret = -1;
                else ret = set_mode (command->arg1);
                if (ret != 0) msg ("Syntax: MODE SPLIT or MODE NOSPLIT", 2);
                break;

            case 34:            /* XSCALE */
                ret = -1;
                if (command->narg == 0) 
                    {
                    inp.xscale[0] = 0.0;
                    inp.xscale[1] = 0.0;
                    ret = 0;
                    }
                else if (command->narg != 2)
                    msg ("XSCALE has two floating arguments", 2);
                else if (sscanf (command->arg1, "%f", &(inp.xscale[0])) != 1)
                    msg ("XSCALE bad argument", 2);
                else if (sscanf (command->arg2, "%f", &(inp.xscale[1])) != 1)
                    msg ("XSCALE bad argument", 2);
                else if (inp.xscale[0] >= inp.xscale[1])
                    msg ("Xmin must be less than xmax", 2);
                else ret = 0;
                break;

            case 35:            /* REMOTE */
                ret = 0;
                if (command->narg != 0)
                    {
                    msg ("REMOTE has no arguments", 2);
                    ret = -1;
                    }
                else inp.refrem = REMOTE;
                break;

            case 36:            /* REFERENCE */
                ret = 0;
                if (command->narg != 0)
                    {
                    msg ("REFERENCE has no arguments", 2);
                    ret = -1;
                    }
                else inp.refrem = REFERENCE;
                break;

            case 40:            /* TIMERANGE */
                if (command->narg > 2 || command->narg == 1) {
                    msg ("Syntax error.  TIMERANGE has either 2 or 0 args", 2);
                    ret = -1;
                }
                else ret = set_timerange (command->arg1, command->arg2);
                break;

            case 41:            /* STATIONS */
                ret = set_stations (command->arg1, command->arg2, command->remarg);
                break;

            case 42:            /* BASELINES */
                ret = set_baselines (command->arg1, command->arg2, command->remarg);
                break;

            case 43:            /* FREQUENCIES */
                ret = set_frequencies (command->arg1, command->arg2, command->remarg);
                break;

            case 44:            /* EXPERIMENT */
                ret = -1;
                if (command->narg > 1)
                    msg ("EXPERIMENT has only one integer argument", 2);
                else if (command->narg == 0) {
                    inp.experiment = 0;
                    ret = 0;
                }
                else if (sscanf (command->arg1, "%d", &(inp.experiment)) != 1)
                    msg ("EXPERIMENT bad argument", 2);
                else ret = 0;
                break;

            case 45:            /* QCODES */
                ret = set_qcodes (command->arg1, command->arg2, command->remarg);
                break;

            case 46:            /* TYPE */
                ret = set_type (command->arg1, command->arg2, command->remarg);
                break;

            case 47:            /* SNRMIN */
                ret = -1;
                if (command->narg > 1)
                    msg ("SNRMIN has only one numeric argument", 2);
                else if (command->narg == 0) {
                    inp.snr[0] = 0.0;
                    ret = 0;
                }
                else if (sscanf (command->arg1, "%f", &snr) != 1)
                    msg ("SNRMIN bad argument", 2);
                else if (snr >= inp.snr[1])
                    msg ("SNRMIN bad value ... greater than SNRMAX.  Ignored", 2);
                else 
                    {
                    inp.snr[0] = snr;
                    ret = 0;
                    }
                break;

            case 48:            /* SOURCES */
                ret = set_sources (command->arg1, command->arg2, command->remarg);
                break;

            case 49:            /* LENGTH */
                ret = -1;
                if (command->narg > 1)
                    msg ("LENGTH has only one integer argument", 2);
                else if (command->narg == 0) {
                    inp.length = 0;
                    ret = 0;
                }
                else if (sscanf (command->arg1, "%d", &(inp.length)) != 1)
                    msg ("LENGTH bad argument", 2);
                else ret = 0;
                break;

            case 50:            /* FRACTION */
                ret = set_fraction (command->arg1, command->arg2, command->remarg);
                break;

            case 51:            /* NFREQ */
                ret = set_nfreq (command->arg1, command->arg2, command->remarg);
                break;

            case 52:            /* SNRMAX */
                ret = -1;
                if (command->narg > 1)
                    msg ("SNRMAX has only one integer argument", 2);
                else if (command->narg == 0) {
                    inp.snr[1] = 100000.0;
                    ret = 0;
                }
                else if (sscanf (command->arg1, "%f", &snr) != 1)
                    msg ("SNRMAX bad argument", 2);
                else if (snr <= inp.snr[0])
                    msg ("SNRMAX bad value ... less than SNRMIN.  Ignored", 2);
                else 
                    {
                    inp.snr[1] = snr;
                    ret = 0;
                    }
                break;

            case 53:            /* PRANGE */
                ret = set_prange (command->arg1, command->arg2, command->remarg);
                break;

            case 54:            /* PROCRANGE */
                if (command->narg > 2 || command->narg == 1) {
                    msg ("Syntax error.  PROCRANGE has either 2 or 0 args", 2);
                    ret = -1;
                }
                else ret = set_procrange (command->arg1, command->arg2);
                break;

            case 55:            /* TRIANGLES */
                ret = set_triangles (command->arg1, command->arg2, command->remarg);
                break;

            case 56:            /* QUADS */
                ret = set_quads (command->arg1, command->arg2, command->remarg);
                break;

            case 57:            /* BSNRMIN */
                ret = -1;
                if (command->narg > 1)
                    msg ("BSNRMIN has only one numeric argument", 2);
                else if (command->narg == 0) {
                    inp.bsnr[0] = 0.0;
                    ret = 0;
                }
                else if (sscanf (command->arg1, "%f", &snr) != 1)
                    msg ("BSNRMIN bad argument", 2);
                else if (snr >= inp.bsnr[1])
                    msg ("BSNRMIN bad value ... greater than SNRMAX.  Ignored", 2);
                else 
                    {
                    inp.bsnr[0] = snr;
                    ret = 0;
                    }
                break;

            case 58:            /* BSNRMAX */
                ret = -1;
                if (command->narg > 1)
                    msg ("BSNRMAX has only one integer argument", 2);
                else if (command->narg == 0) {
                    inp.bsnr[1] = 1000000.0;
                    ret = 0;
                }
                else if (sscanf (command->arg1, "%f", &snr) != 1)
                    msg ("BSNRMAX bad argument", 2);
                else if (snr <= inp.bsnr[0])
                    msg ("BSNRMAX bad value ... less than SNRMIN.  Ignored", 2);
                else 
                    {
                    inp.bsnr[1] = snr;
                    ret = 0;
                    }
                break;

            case 59:            /* POLARIZATIONS */
                ret = set_pols (command->arg1, command->arg2, command->remarg);
                break;

            case 70:            /* CCREAD */
                msg ("ccread no longer supported!", 2);
                break;

            case 71:            /* PSPLOT */
                if (command->narg != 0) 
                    {
                    msg ("PSPLOT has no arguments", 2);
                    ret = -1;
                    }
                else ret = psplot (data);
                break;

            case 72:            /* PSFILE */
                if (command->narg != 1) 
                    {
                    msg ("PSFILE has one argument", 2);
                    ret = -1;
                    }
                else ret = psfile4 (data, command->arg1, 0);
                break;

            case 73:            /* REPROC */
                if (command->narg != 1) 
                    {
                    msg ("REPROC has one argument", 2);
                    ret = -1;
                    }
                else ret = psfile4 (data, command->arg1, 1);
                break;

            case 81:            /* DEVICE */
                ret = -1;
                if (command->narg > 1) msg ("DEVICE has only one argument", 2);
                else if (strlen (command->arg1) > 29) msg ("Device name too long", 2);
                else if (strlen (command->arg1) == 0) 
                    msg ("Command DEVICE: You must specify a device name/type", 2);
                else ret = set_device (command->arg1);
                break;

            case 82:            /* OUTVERSION */
                ret = -1;
                oldvrs = output_version;
                if (command->narg != 1) 
                    msg ("OUTVERSION has one argument", 2);
                else if (sscanf (command->arg1, "%d", &output_version) != 1)
                    msg ("Command OUTVERSION: Please supply a single integer", 2);
                else if ((output_version < 0) || (output_version > MAXVERSION))
                    {
                    output_version = oldvrs;
                    msg ("Supported version numbers are 0-%d (0=same as input version)",
                        2, MAXVERSION);
                    msg ("Try again.", 2);
                    }
                else ret = 0;

            case 99:            /* NOOP */
                break;

            case 101:           /* TEST1 */
                test1 (data, command->arg1, command->arg2, command->remarg);
                break;

            case 102:           /* COCLIP */
                ret = -1;
                if (command->narg > 1)
                    msg ("COCLIP has one integer argument", 2);
                else if (sscanf (command->arg1, "%d", &comax) != 1)
                    msg ("COCLIP bad argument", 2);
                else if (comax <= 0)
                    msg ("COCLIP bad value ... less than 1.  Ignored", 2);
                else 
                    {
                    for (i=0; i<fscan; i++)
                        {
                        if (data->fdata[i].flag != 0) continue;
                        if (data->fdata[i].data.srch_cotime > comax)
                            data->fdata[i].data.srch_cotime = comax;
                        if (data->fdata[i].data.noloss_cotime > comax)
                            data->fdata[i].data.noloss_cotime = comax;
                        }
                    ret = 0;
                    }
                break;

            default:
                msg ("Unrecognized command: %s", 3, command->cmnd);
                ret = -1;
        }
        return (ret);                   /* 0 = success, !=0 = failure */
}

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "mk4_data.h"

char progname[] = "test_mk4fringe";
int msglev = 1;
int verb = 0;

#define FRINGE_SAMPLE \
    "../../data/ff_testdata/testdata/2836/scan001/AE.X.13.olomfh"

static int sample_missing(char *ff)
    {
    struct stat buf;
    int stst = stat(ff, &buf);
    if (verb>0) fprintf(stderr, "Checking on %s\n", ff);
    if (!stst && buf.st_size>0) return(0);   /* got it */
    if (errno == ENOENT)
	{
	if (verb>0) fprintf(stderr, "Pass: file %s missing\n", ff);
	return(1);
	}
    fprintf(stderr, "Stat() returned %d, errno %d\n", stst, errno);
    return(stst);
    }

void
pr_date (string, date)
char *string;
struct date date;
    {
    printf ("%s: %04d yr, %03d day, %02d hour, %02d min, %g sec\n", string,
                date.year, date.day, date.hour, date.minute, date.second);
    fflush (stdout);
    }

main (argc, argv)
int argc;
char *argv[];
    {
    struct mk4_fringe fringe;
    struct type_200 *t200;
    struct type_201 *t201;
    struct type_202 *t202;
    struct type_203 *t203;
    struct type_204 *t204;
    struct type_208 *t208;
    int i;
    char *src = getenv("srcdir");
    char *ff, *tv = getenv("testverb");

    verb = tv ? atoi(tv) : 0;

    /* at the moment there is at least one file to try */
    if (!src) return(0);
    ff = malloc(strlen(src) + 300);
    if (!ff) return(perror("malloc"),1);
    sprintf(ff, "%s/%s", src, FRINGE_SAMPLE);
    if (argc>1) ff = argv[1];
    if ((i = sample_missing(ff))) return(i>0 ? 0 : 1);	/* punt */

    fringe.nalloc = 0;
    if (verb>0) fprintf(stderr, "Reading %s\n", ff);
    if (read_mk4fringe (ff, &fringe) != 0)
        {
        printf ("Error return from read_mk4fringe on file:\n  %s\n", ff);
        exit (1);
        }
    if (verb>0) fprintf(stderr, "Successful read.\n");
    
    if (verb<2) return(0);	/* success */

    t200 = fringe.t200;
    t201 = fringe.t201;
    t202 = fringe.t202;
    t203 = fringe.t203;
    t204 = fringe.t204;
    t208 = fringe.t208;
    
    printf ("t200 software_rev =");
    for (i=0; i<10; i++) printf (" %d", t200->software_rev[i]);
    printf ("\n");
    printf ("t200 expt no = %d\n", t200->expt_no);
    printf ("t200 expname = '%s'\n", t200->exper_name);
    printf ("t200 scan_name = '%s'\n", t200->scan_name);
    printf ("t200 correlator = '%s'\n", t200->correlator);
    pr_date ("t200 scantime", t200->scantime);
    pr_date ("t200 corr_date", t200->corr_date);
    pr_date ("t200 fourfit_date", t200->fourfit_date);
    pr_date ("t200 frt", t200->frt);

    printf ("t201 source = '%s'\n", t201->source);
    printf ("t201 coord = %d %d %g %d %d %g\n", t201->coord.ra_hrs,
                                                t201->coord.ra_mins,
                                                t201->coord.ra_secs,
                                                t201->coord.dec_degs,
                                                t201->coord.dec_mins,
                                                t201->coord.dec_secs);
    printf ("t201 epoch = %d\n", t201->epoch);
    pr_date ("t201 coord_date", t201->coord_date);
    printf ("t201 ra_rate = %g\n", t201->ra_rate);
    printf ("t201 ra_rate = %g\n", t201->ra_rate);

    printf ("t202 baseline = '%.2s'\n", t202->baseline);
    printf ("t202 ref id = '%.2s'\n", t202->ref_intl_id);
    printf ("t202 rem id = '%.2s'\n", t202->rem_intl_id);
    printf ("t202 ref_name = '%.8s'\n", t202->ref_name);
    printf ("t202 rem_name = '%.8s'\n", t202->rem_name);
    printf ("t202 ref_tape = '%.8s'\n", t202->ref_tape);
    printf ("t202 ref_tape = '%.8s'\n", t202->ref_tape);
    printf ("t202 ref_xpos = %g\n", t202->ref_xpos);
    printf ("t202 rem_xpos = %g\n", t202->rem_xpos);
    printf ("t202 ref_ypos = %g\n", t202->ref_ypos);
    printf ("t202 rem_ypos = %g\n", t202->rem_ypos);
    printf ("t202 ref_zpos = %g\n", t202->ref_zpos);
    printf ("t202 rem_zpos = %g\n", t202->rem_zpos);
    printf ("t202 ref_clock = %g\n", t202->ref_clock);
    printf ("t202 rem_clock = %g\n", t202->rem_clock);
    printf ("t202 ref_clockrate = %g\n", t202->ref_clockrate);
    printf ("t202 rem_clockrate = %g\n", t202->rem_clockrate);
    printf ("t202 ref_idelay = %g\n", t202->ref_idelay);
    printf ("t202 rem_idelay = %g\n", t202->rem_idelay);
    printf ("t202 ref_zdelay = %g\n", t202->ref_zdelay);
    printf ("t202 rem_zdelay = %g\n", t202->rem_zdelay);

    for (i=0; i<32; i++)
        printf ("t203 channel %d, %d %d '%c%c' '%c%c' %g %g '%.8s' '%.8s'\n", i,
                    t203->channels[i].index,
                    t203->channels[i].sample_rate,
                    t203->channels[i].refsb,
                    t203->channels[i].remsb,
                    t203->channels[i].refpol,
                    t203->channels[i].rempol,
                    t203->channels[i].ref_freq,
                    t203->channels[i].rem_freq,
                    t203->channels[i].ref_chan_id,
                    t203->channels[i].rem_chan_id);

    printf ("t204 ff_version = '%.2s'\n", t204->ff_version);
    printf ("t204 platform = '%.8s'\n", t204->platform);
    printf ("t204 control_file = '%s'\n", t204->control_file);
    pr_date ("t204 ffcf_date", t204->ffcf_date);
    printf ("t204 override = '%s'\n", t204->override);

    printf ("t208 quality = '%c'\n", t208->quality);
    printf ("t208 errcode = '%c'\n", t208->errcode);
    printf ("t208 tape_qcode = '%.6s'\n", t208->tape_qcode);
    printf ("t208 adelay = %g\n", t208->adelay);
    printf ("t208 arate = %g\n", t208->arate);
    printf ("t208 aaccel = %g\n", t208->aaccel);
    printf ("t208 tot_mbd = %g\n", t208->tot_mbd);
    printf ("t208 tot_sbd = %g\n", t208->tot_sbd);
    printf ("t208 tot_rate = %g\n", t208->tot_rate);
    printf ("t208 resid_mbd = %g\n", t208->resid_mbd);
    printf ("t208 resid_sbd = %g\n", t208->resid_sbd);
    printf ("t208 resid_rate = %g\n", t208->resid_rate);
    printf ("t208 mbd_error = %g\n", t208->mbd_error);
    printf ("t208 sbd_error = %g\n", t208->sbd_error);
    printf ("t208 rate_error = %g\n", t208->rate_error);
    printf ("t208 ambiguity = %g\n", t208->ambiguity);
    printf ("t208 amplitude = %g\n", t208->amplitude);
    printf ("t208 inc_seg_ampl = %g\n", t208->inc_seg_ampl);
    printf ("t208 inc_chan_ampl = %g\n", t208->inc_chan_ampl);
    printf ("t208 snr = %g\n", t208->snr);
    printf ("t208 prob_false = %g\n", t208->prob_false);
    printf ("t208 totphase = %g\n", t208->totphase);
    printf ("t208 resphase = %g\n", t208->resphase);

    if (verb<3) return(0);
    display_221 (fringe.t221, 0);

    return (0);
    }


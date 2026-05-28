/*
 * Function to skip plots that do not have a polarization match
 *
 * type_203 has channels[8*MAXFREQ] with refpol and rempol for each
 * (2 sidebands * 4 pols ^ for each freq)
 * see parse_cmdline.c for interpretation of pols...
 *
 * returns 1 to skip, 0 to show
 */
#include "mk4_sizes.h"
#include "type_203.h"
#include "type_205.h"
#include "msg.h"

int skip_poln(char *name,
    struct type_203 *t203, struct type_205 *t205, int poln)
{
    int ch, fc, cc, jj, polc, hits, miss;
    char refpol, rempol;
    hits = miss = 0;
    for (ch=0; ch<MAXFREQ; ch++) {
        fc = t205->ffit_chan[ch].ffit_chan_id;
        if (fc == ' ') continue;    /* what unassigned is supposed to be */
        if (! ((fc >= 'a' && fc <= 'z') || (fc >= 'A' && fc <= 'Z') ||
            (fc >= '0' && fc <= '9') || (fc == '$') || (fc == '%')) )
                continue;   /* not a legal code, thus garbage */
        /* channels[4] are indices into type 203 per type 205 */
        for (cc = 0; cc < 4; cc++) {
            jj = t205->ffit_chan[ch].channels[cc];
            if (jj < 0) continue;
            polc = 0x1111;  /* hex read as binary */
            refpol = t203->channels[jj].refpol;
            if (refpol == 'X' || refpol == 'L') polc &= 0x1010;
            if (refpol == 'Y' || refpol == 'R') polc &= 0x1011;
            rempol = t203->channels[jj].rempol;
            if (rempol == 'X' || rempol == 'L') polc &= 0x0101;
            if (rempol == 'Y' || rempol == 'R') polc &= 0x0111;
            if (polc == poln) hits++;
            else miss++;
        }
    }
    msg("%s with %d hits %d miss poln is b%x", 1, name, hits, miss, poln);
    return (hits > 0) ? 0 : 1;
}

/*
 * eof
 */

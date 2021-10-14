/*
 * Make adjustments to the SNR, Cf. TMS3rdEd Eq 8.14, 8.32-35, 8.50-52
 *
 * The caller, make_plotdata(struct type_pass *pass) will have made most
 * of the calculations necessary to make the plot.  A few more are made
 * in make_postplot() and even in generate_text().  And the caller will
 * have estimated the SNR of the fringe based on theoretical factors.
 *
 * status.snr = status.delres_max * param.inv_sigma
 *          * sqrt((double)status.total_ap_frac * eff_npol)
 *                      / (1.0E4 * status.amp_corr_fact);
 *
 * SNR = (status.delres_max/1.0E4)  # AMP without Whitney multiplier
 *     * param.inv_sigma            # sqrt(samples / channel)
 *     * sqrt(num-AP pol channels)  # multiplier on inv_sigma
 *
 * The visibilities calculated by DiFX are multiplied by SCALE (10000)
 *      #define SCALE 10000.0       // amplitude factor to normalize for fourfit 
 * in difx2mark4 and in the same place (new_type1.c), the Van Vleck factors
 * are applied via a sqrt() of the normal factor on a per station basis
 *      double factor[2] = {1.25331, 1.06448}; 
 * Thus 1v1 and 2v2 end up with 1.5707859561 (1/0.6366239755), and
 * 1.1331176704 (1/0.8825208768) and 1v2 is 1.3341234288 (1/0.7495558345)
 * which explains the puzzling numbers in fill_param.c, which are only
 * correct at the sub-percent level.  Thus inv_sigma has factors to undo
 * what difx2mark4 did when computing the SNR.  Note that inv_sigma is
 * calculated where it is to allow ionospheric search windows to branch
 * on snr_approx (computed from inv_sigma, which is not otherwise used
 * until this point).
 *
 * For backward compatibility with hardware correlation (fill_param.c):
 * case 2: fact2 = (param->corr_type==DIFX) ? 0.6366239755 : 0.637;
 * case 3: fact2 = (param->corr_type==DIFX) ? 0.7495558345 : 0.749;
 * case 4: fact2 = (param->corr_type==DIFX) ? 0.8825208768 : 0.881;
 * the point being that there ARE issues at the 0.1% level certainly,
 * but all that is really happening here is undo-ing the spectral function
 * multiplier that was applied in difx2mark4.  Being more specific, Fig 8.6
 * of TMS shows that these factors are dependent on v0 (the threshold for bit
 * state definition, which is at best only approximately set and possibly
 * variable due to changes in the receiver or atmospheric background, &c.)
 * The discussion after TMS Eq 8.53 (also Table 8.2) is the source of the 0.881.
 * DiFX itself uses OPTIMAL_2BIT_HIGH = 3.3359 (defined in mark5_stream.h of
 * the mark5access library, appearing in format_codif.c format_vdif.c,
 * mark5_format_d2k.c, mark5_format_kvn5b.c, mark5_format_mark4.c
 * mark5_format_mark5b.c, mark5_format_vlba.c,  mark5_format_vlba_nomod.c
 * and mark5_stream.c....  The calculus for \eta_4 corresponding to 8.53
 * is coded in misc/bits/b4thresh (worked out 2011 - 2012 to understand
 * all of this for the ALMA situation); ./b4thresh
 * eta4(3.33590000,0.98160000) = 0.88251815 phi(0.98160000/sqrt(2)) = 0.67370305
 * (RJC may have worked this out independently when difx2mark4 was written;
 * the code was refactored by May 2012 and new_type1.c appears at that time.
 * GBC recalls discussions about all of this and the "more precise" value may
 * have been adopted in the process of efforts to validate Mark4 correlation
 * against correlation with DiFX.).
 *
 * Note that apply_video_bp() and apply_cmplxbp() should be (at least
 * approximately) SNR neutral, but notches and passband are more likely
 * to break things (if the bandwidth discarded is large).
 *
 * However, the "apply_functions" break those assumptions and thus the
 * true SNR may be wildly different from the theoretical value.  E.g. if
 * half of the bandwidth is discarded, one will have a narrower channel,
 * fewer samples and lower SNR.  This function adjusts the theoretical
 * value based on the information stored in the status structure at the
 * time the adjustments are made.  Temporary adjustments will have been
 * stored in MAXFREQ +0 (passband) or +1 (notches) and later incremented to
 * status->sb_bw_fracs (0 or [fractional correction]) and
 * status->sb_bw_origs (the value of datum->?sb_frac prior to adjustment).
 * (That is, an addition to sb_bw_fracs and sb_bw_count has been made for
 * every ap/sb/fr datum that survived any data selection.)  The counter
 * status->sb_bw_apcnt counts the number of APs that have been accumulated
 * into sb_bw_fracs and sb_bw_origs so that we can try to make adjustments.
 *
 * Now a problem is that passband corrections may remove frac AP increments
 * and the integration timing calculation uses the number of channels whether
 * any data survives from them or not.  sb_bw_aperr accumulates the error.
 * This results in corrections to fill_206() and in search() once norm_fx
 * has completed.
 *
 * norm_xf was missing the logic to skip things after the apply_* edits
 * to the passband (or notches) and the corrections are indicated, but not
 * made.
 *
 * So in summary: param.inv_sigma carries the number of bits which is then
 * multiplied by the sqrt(the number of channels and perhaps pols co-added).
 * When the effective bandwidth has been reduced, the SNR must be as well.
 */
#include <math.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include "apply_funcs.h"
#include "ff_misc_if.h"

double adjust_snr(struct type_pass *pass, struct type_status *status)
{
    double theoretical_snr = status->snr, corrected_snr;
    double bw_corr, frac_bw, ap_bw;
    int sb, fr, chcnt;

    /* first consider if a bandwidth correction needs to be made */
    for (sb = 0, chcnt = 0, frac_bw = 0, ap_bw = 0; sb < 2; sb++)
        for(fr = 0; fr < pass->nfreq; fr++) {
            /* ignore channels that were not part of the accumulation */
            if (status->sb_bw_apcnt[fr][sb] <= 0.0) {
                    continue;
            }
            chcnt ++;   /* this fr/sb was included */
            frac_bw += status->sb_bw_fracs[fr][sb];
            ap_bw += status->sb_bw_apcnt[fr][sb];
            msg ("adjust_snr "
                 "sb %d fr %d chcnt %d frac_bw %lf ap_bw %lf [%lf %lf %lf]",
                 1, sb, fr, chcnt, frac_bw, ap_bw, status->sb_bw_fracs[fr][sb],
                status->sb_bw_origs[fr][sb], status->sb_bw_apcnt[fr][sb]);
        }

    /* if chcnt is 0 we have no bw_corr to make */
    bw_corr = (chcnt > 0) ? sqrt(frac_bw / ap_bw) : 1.0;
    corrected_snr = bw_corr * theoretical_snr;

    /* for now, always report if SNR was diddled */
    if (bw_corr != 1.0)
        msg ("adjust_snr %lf -> %lf (%lf; %lf %lf)", 1,
            theoretical_snr, corrected_snr, bw_corr,
            status->total_ap_frac, status->tot_sb_bw_aperr);
    return(corrected_snr);
}

/*
 * eof
 */

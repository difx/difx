/*
 * Fit the SNR versus segmentation time to fine the max SNR.
 *
 * fit_snr() does a 3pt parabolic interpolation about the max SNR
 *  but with noise, it can easily find a bump.
 * fit_cbs() does a cubic spline fit to the entire shape allowing
 *  a more likely SNR maximum using the GSL 2.8 splines
 * fit_2p7() does a cubic spline fit to the entire shape allowing
 *  a more likely SNR maximum using the GSL 2.7 splines
 *
 * the 2.7 spline code is deprecated (but present) in 2.8 which
 * introduced new code.  Thus in
 *  2.7) only fit_2p7() can be used
 *  2.8) both can be used (presumably with identical results)
 *  2.9) (likely) only fit_cbs() can be used
 *
 * Note that both use SNRs as renormalized in normalize_snr()
 * return an integer peak-SNR time and populate fitsnr[].
 *
 * Created GBC May 1 2026.
 */
#include "fit_gsl.h"

int fit_msnr(cosumary *codatum, int npt)
{
    double pk3pt, pk2p8, pk2p7;
    int    ik3pt, ik2p8, ik2p7, isnrmax;

    /* mark as totally invalid */
    codatum->redchisq[FITOPT_NDX_3PT] =
    codatum->redchisq[FITOPT_NDX_2P8] =
    codatum->redchisq[FITOPT_NDX_2P7] = -17.0;

    if (codatum->fitmask & FITOPT_SNR_3PT) pk3pt = fit_snr(codatum, npt);
    else pk3pt = -3.0;
    ik3pt = round(pk3pt);
    msg("fit_snr() = %lf -> %d", 1, pk3pt, ik3pt);
#if HAVEGSL2P8
    if (codatum->fitmask & FITOPT_SNR_2P8) pk2p8 = fit_cbs2p8(codatum, npt);
    else pk2p8 = -3.0;
    ik2p8 = round(pk2p8);
    msg("fit_cbs2p8() = %lf -> %d", 1, pk2p8, ik2p8);
#endif /* HAVEGSL2P8 */
#if HAVEGSL2P7
    if (codatum->fitmask & FITOPT_SNR_2P7) pk2p7 = fit_cbs2p7(codatum, npt);
    else pk2p7 = -3.0;
    ik2p7 = round(pk2p7);
    msg("fit_cbs2p7() = %lf -> %d", 1, pk2p7, ik2p7);
#endif /* HAVEGSL2P7 */

    /* expect that the cbs 2.8/2.7 (equivalent) should always be better */
    msg("", 1);
#if HAVEGSL2P8
    if (pk2p8 > 0) {
        codatum->snr_cotime = pk2p8;
        codatum->bestsnr = FITOPT_SNR_2P8;
        isnrmax = ik2p8;
        msg("Using CBS2.8 result for the position of SNR maximum", 2);
    } else
#endif /* HAVEGSL2P8 */
#if HAVEGSL2P7
    if (pk2p7 > 0) {
        codatum->snr_cotime = pk2p7;
        codatum->bestsnr = FITOPT_SNR_2P7;
        isnrmax = ik2p7;
        msg("Using CBS2.7 result for the position of SNR maximum", 2);
    } else
#endif /* HAVEGSL2P7 */
    {
        codatum->snr_cotime = pk3pt;
        codatum->bestsnr = FITOPT_SNR_3PT;
        isnrmax = ik3pt;
        msg("Using 3PT result for the position of SNR maximum", 2);
    }

    return(round(codatum->snr_cotime));
}

/*
 * eof vim:nospell
 */

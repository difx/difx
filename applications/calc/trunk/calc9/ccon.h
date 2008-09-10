/*
 * ccon.h: A c++ header file correspong to ccon.i, for use in c/c++ programs
 *         Defines a global struct equivalent to the Fortran common block in ccon.i
 */
 #ifdef _cplusplus
extern"C" {
#endif

 struct {
    int ILUOUT, KATMC, KATMD, KAXOC, KAXOD, KPTDC, KPTDD, KDNPC, KDNPD,
        KETDC, KETDD, KIONC, KIOND, KNUTC, KNUTD, KPREC, KPRED,
        KRELC, KRELD, KSITC, KSITD, KSTRC, KSTRD, KUT1C, KUT1D,
        KWOBC, KWOBD, KUTCC, KUTCD, KATIC, KATID, KCTIC, KCTID,
        KPEPC, KPEPD, KDIUC, KDIUD, KM20C, KM20D, KROSC, KROSD,
        KSTEC, KSTED, KSUNC, KSUND, KSARC, KSARD, KTHEC, KTHED,
        KMATC, KMATD, KVECC, KVECD, KOCEC, KOCED, KASTC, KASTD,
        KSTAC, KSTAD, KPLXC, KPLXD, KPANC, KPAND, Kspace;
} con_;

//  Apparently Kspace is for alignment purposes only

#ifdef _cplusplus
}
#endif  

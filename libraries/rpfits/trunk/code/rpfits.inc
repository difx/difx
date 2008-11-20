C--------------------- COMMONS FOR RPFITS ROUTINES ---------------------
C
C                  SEE RPFITS.DEFN FOR AN EXPLANATION
C
C     The FORTRAN standard requires that CHARACTER variables not be
C     mixed with other types in COMMON and hence they have a separate
C     COMMON block.  However, they are declared in context.
C
C     DOUBLE PRECISION variables were also segregated into a separate
C     COMMON block for reasons long forgotten.
C
C $Id: rpfits.inc,v 1.18 2008/09/11 06:38:53 cal103 Exp $
C-----------------------------------------------------------------------

      INTEGER   ant_max, max_card, max_cu, max_fg, max_if, max_mt,
     :          max_nx, max_sc, max_su, pol_max
      PARAMETER (ant_max  = 15,
     :           max_card = 650,
     :           max_cu   = 32,
     :           max_fg   = 32,
     :           max_if   = 16,
     :           max_mt   = 256,
     :           max_nx   = 256,
     :           max_sc   = 16,
     :           max_su   = 500,
     :           pol_max  = 8)


C     Error handling.
      INTEGER   errlun, rp_iostat
      CHARACTER errmsg*80
      COMMON /iostat/ rp_iostat, errlun

C     -------- Parameter values obtained from the RPFITS header --------
C                (except intbase which is a random parameter)

C     Miscellaneous parameters from RPFITS header cards (ncount, nfreq,
C     nstok, freq, dfreq, ra, dec, and coord are defunct; use if_nfreq,
C     if_nstok, if_freq, if_bw, if_nfreq su_ra, and su_dec, instead and
C     ignore ncount and coord - now always J2000).
      LOGICAL   write_wt
      INTEGER   data_format, intime, ncard, ncount, nfreq, nscan, nstok
      REAL      intbase
      DOUBLE PRECISION dec, dfreq, freq, ra
      CHARACTER bunit*16, cal*16, card(max_card)*80, coord*8, datobs*12,
     :          datsys*8, datwrit*12, file*256, instrument*16,
     :          object*16, obstype*16, rp_observer*16, rpfitsversion*20,
     :          version*20
      COMMON /param/ nstok, nfreq, ncount, intime, nscan, write_wt,
     :          ncard, intbase, data_format

C     Ephemeris parameters from RPFITS header cards.
      INTEGER   rp_defeat
      DOUBLE PRECISION rp_c(12), rp_djmrefp, rp_djmreft, rp_utcmtai
      COMMON /ephem/ rp_defeat

C     Proper motion parameters from RPFITS header cards.
      DOUBLE PRECISION pm_dec, pm_epoch, pm_ra
      COMMON /proper/ pm_ra, pm_dec, pm_epoch

C     Spectral line information from RPFITS header cards.
      INTEGER   ivelref
      DOUBLE PRECISION rfreq, vel1
      COMMON /spect/ ivelref

C     Antenna table (embedded in the RPFITS header).
      LOGICAL   an_found
      INTEGER   ant_mount(ant_max), ant_num(ant_max), nant
      DOUBLE PRECISION axis_offset(ant_max),
     :          feed_cal(ant_max,max_if,pol_max), feed_pa(2,ant_max),
     :          x(ant_max), x_array, y(ant_max), y_array, z(ant_max),
     :          z_array
      CHARACTER feed_type(2,ant_max)*2, sta(ant_max)*8
      COMMON /anten/ nant, ant_num, ant_mount, an_found

C     Uncalibration table (embedded in the RPFITS header).
      LOGICAL   cu_found
      INTEGER   cu_ant(max_cu), cu_ch1(max_cu), cu_ch2(max_cu),
     :          cu_if(max_cu), n_cu
      DOUBLE PRECISION cu_cal1(max_cu), cu_cal2(max_cu), cu_ut(max_cu)
      COMMON /cu/ n_cu, cu_ant, cu_if, cu_ch1, cu_ch2, cu_found

C     Flag table (embedded in the RPFITS header).
      LOGICAL   fg_found
      INTEGER   fg_ant(2,max_fg), fg_chan(2,max_fg),
     :          fg_if(2,max_fg), fg_stok(2,max_fg), n_fg
      DOUBLE PRECISION fg_ut(2,max_fg)
      CHARACTER fg_reason(max_fg)*24
      COMMON /fg/ n_fg, fg_ant, fg_if, fg_chan, fg_stok, fg_found

C     IF table (embedded in the RPFITS header).
      LOGICAL   if_found
      INTEGER   if_chain(max_if), if_invert(max_if), if_nfreq(max_if),
     :          if_nstok(max_if), if_num(max_if), if_sampl(max_if),
     :          if_simul(max_if), n_if
      DOUBLE PRECISION if_bw(max_if), if_freq(max_if), if_ref(max_if)
      CHARACTER if_cstok(4,max_if)*2
      COMMON /if/ n_if, if_invert, if_nfreq, if_nstok, if_sampl,
     :          if_found, if_num, if_simul, if_chain

C     Meteorological table (embedded in the RPFITS header).
      LOGICAL   mt_found
      INTEGER   mt_ant(max_mt), n_mt
      DOUBLE PRECISION mt_humid(max_mt), mt_press(max_mt),
     :          mt_temp(max_mt), mt_ut(max_mt)
      COMMON /mt/ n_mt, mt_ant, mt_found

C     Index table (embedded in the RPFITS header).
      LOGICAL   nx_found
      INTEGER   n_nx, nx_rec(max_nx)
      DOUBLE PRECISION nx_ut(max_nx)
      CHARACTER nx_date(max_nx)*12, nx_source(max_nx)*16
      COMMON /nx/ n_nx, nx_rec, nx_found

C     Source table (embedded in the RPFITS header).
      LOGICAL   su_found
      INTEGER   n_su, su_num(max_su)
      DOUBLE PRECISION su_dec(max_su), su_decd(max_su), su_pdec(max_su),
     :          su_pdecd(max_su), su_pra(max_su), su_prad(max_su),
     :          su_ra(max_su), su_rad(max_su)
      CHARACTER su_cal(max_su)*4, su_name(max_su)*16
      COMMON /su/ n_su, su_found, su_num


C     DOUBLE PRECISION variables (declared in context above).
      COMMON /doubles/ axis_offset, dec, dfreq, cu_cal1, cu_cal2, cu_ut,
     :          feed_cal, feed_pa, fg_ut, freq, if_bw, if_ref, if_freq,
     :          mt_humid, mt_press, mt_temp, mt_ut, nx_ut, ra, rfreq,
     :          rp_c, rp_djmrefp, rp_djmreft, rp_utcmtai, su_dec, su_ra,
     :          su_rad, su_decd, su_pra, su_pdec, su_prad, su_pdecd,
     :          vel1, x, x_array, y, y_array, z, z_array

C     CHARACTER variables (declared in context above).
      COMMON /names/ object, instrument, cal, rp_observer, datobs,
     :          datwrit, file, datsys, version, coord, sta, feed_type,
     :          card, if_cstok, su_name, su_cal, fg_reason, nx_source,
     :          nx_date, rpfitsversion, bunit, obstype, errmsg

C     -------------- Values obtained from the RPFITS data --------------

C     Syscal data.
      INTEGER   sc_ant, sc_if, sc_q, sc_srcno
      REAL      sc_cal(max_sc,max_if,ant_max), sc_ut
      COMMON /sc/ sc_ut, sc_ant, sc_if, sc_q, sc_cal, sc_srcno

C     ------------------------------------------------------------------

C     The following is for compatibility with early versions:
      DOUBLE PRECISION ant_humid(ant_max), ant_pressure(ant_max),
     :          ant_temp(ant_max), rp_humid(ant_max),
     :          rp_pressure(ant_max), rp_temp(ant_max)
      EQUIVALENCE (mt_humid, ant_humid,    rp_humid)
      EQUIVALENCE (mt_press, ant_pressure, rp_pressure)
      EQUIVALENCE (mt_temp,  ant_temp,     rp_temp)

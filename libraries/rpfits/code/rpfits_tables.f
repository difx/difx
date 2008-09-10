      subroutine RPFITS_READ_TABLE(lun, tcards, ii, endhdr)
*-----------------------------------------------------------------------
*     Read all RPFITS tables to the end of the header.
*
*     Given:
*          LUN      int   Logical unit number of the RPFITS file.
*          TCARDS(32)*80
*                   char  Array of header cards containing tables.
*          II       int   Current index in array TCARDS, or -1 if only
*                         the flag table (at the end of the data) is to
*                         be read.
*
*     Returned:
*          ENDHDR   log   TRUE if the end of header was encountered.
*
*     Original: Ray Norris 1988/09/29
*     $Id: rpfits_tables.f,v 1.9 2006/06/20 07:02:46 cal103 Exp $
*-----------------------------------------------------------------------
      logical   endhdr, fg_only
      integer   AT_READ, idx, ierr, ii, ichr(640), j, lun
      character keywrd*8, tcards(32)*80

      include 'rpfits.inc'

      idx = ABS(ii)
      fg_only = (ii.eq.-1)
      do while (.not. endhdr)
         if (ncard.lt.0) then
            card(-ncard) = tcards(idx)
            ncard = ncard - 1
         end if

         keywrd = tcards(idx)(1:8)
         if (keywrd.eq.'TABLE IF') then
            if_found = .true.
            call READIF (lun, tcards, idx)
         else if (keywrd.eq.'TABLE SU') then
            su_found = .true.
            call READSU (lun, tcards, idx)
         else if (keywrd.eq.'TABLE FG') then
            fg_found = .true.
            call READFG (lun, tcards, idx)
         else if (keywrd.eq.'TABLE AN') then
            an_found = .true.
            call READAN (lun, tcards, idx)
         else if (keywrd.eq.'TABLE MT') then
            mt_found = .true.
            call READMT (lun, tcards, idx)
         else if (keywrd.eq.'TABLE CU') then
            cu_found = .true.
            call READCU (lun, tcards, idx)
         else if (keywrd.eq.'END     ') then
            endhdr = .true.
            return
         end if

         if (fg_only) then
            endhdr = .false.
            return
         end if

         idx = idx + 1
         if (idx.gt.32) then
            ierr = AT_READ (lun, ichr)
            write (tcards, '(32(20a4,:,/))') (ichr(j), j=1,640)
            idx = 1
         end if
      end do

      return
      end



      subroutine READIF (lun, tcards, idx)
*-----------------------------------------------------------------------
*     Read an IF (intermediate frequency, i.e. spectral window) table
*     from an RPFITS file.
*
*     Original: Ray Norris 1988/09/29
*-----------------------------------------------------------------------
      integer   AT_READ, ichr(640), idx, ierr, j, jdx, l, lun
      character keywrd*8, tcards(32)*80, temp*5

      include 'rpfits.inc'

      n_if = 0
      do while (.true.)
         do jdx = idx+1, 32
            if (ncard.lt.0) then
               card(-ncard) = tcards(jdx)
               ncard = ncard - 1
            end if

            keywrd = tcards(jdx)(1:8)
            if (keywrd.eq.'ENDTABLE') then
               idx = jdx
               go to 999
            else if (keywrd.eq.'HEADER') then
            else if (keywrd.eq.'COMMENT') then
            else
               n_if = n_if + 1
               if (n_if.gt.max_if) then
                  stop ' IF TABLE CONTAINS TOO MANY ENTRIES'
               end if

               read (tcards(jdx), 100, iostat=ierr) if_num(n_if),
     :            if_freq(n_if), if_invert(n_if), if_bw(n_if),
     :            if_nfreq(n_if), if_nstok(n_if),
     :            (if_cstok(l,n_if), l=1,4), if_sampl(n_if),
     :            if_ref(n_if), temp
 100           format (bn,i3,f16.3,i3,f17.3,i5,i3,1x,4a2,i2,f7.1,1x,a5)
               if (ierr.ne.0) stop ' ERROR READING IF TABLE'

               if (temp.eq.' ') then
                  if_simul(n_if) = 1
                  if_chain(n_if) = 1
               else
                  read (temp, *, iostat=ierr) if_simul(n_if),
     :               if_chain(n_if)
                  if (ierr.ne.0) stop ' ERROR 2 READING IF TABLE'

                  if (if_simul(n_if) .eq. 0) if_simul(n_if) = 1
                  if (if_chain(n_if) .eq. 0) if_chain(n_if) = 1
               end if
            end if
         end do

         ierr = AT_READ (lun, ichr)
         write (tcards, '(32(20a4,:,/))') (ichr(j), j=1,640)
         idx = 0
      end do

  999 return
      end



      subroutine WRITE_IF_TABLE (idx, tcards)
*-----------------------------------------------------------------------
*     Write an IF (intermediate frequency, i.e. spectral window) table
*     from an RPFITS file.
*
*     Original: Ray Norris 1988/09/29
*-----------------------------------------------------------------------
      integer   idx, iif, l
      character tcards(*)*80
      include 'rpfits.inc'

      idx = idx + 1
      tcards(idx) = 'TABLE IF'
      idx = idx + 1
      tcards(idx) = 'HEADER     FREQ    INVERT   BW         NCHAN ' //
     :              'NSTOK TYPE SAM REF SIM CHAIN'

      do iif = 1, n_if
         idx = idx + 1
         write (tcards(idx), 100) if_num(iif), if_freq(iif),
     :      if_invert(iif), if_bw(iif), if_nfreq(iif), if_nstok(iif),
     :      (if_cstok(l,iif), l=1,4), if_sampl(iif), if_ref(iif),
     :      if_simul(iif), if_chain(iif)
 100     format (i3,f16.3,i3,f17.3,i5,i3,1x,4a2,i2,f7.1,2i3)
      end do

      idx = idx + 1
      tcards(idx) = 'ENDTABLE'

      return
      end



      subroutine READSU(lun, tcards, idx)
*-----------------------------------------------------------------------
*     Read an SU (source) table from an RPFITS file.
*
*     Original: Ray Norris 1988/11/08
*-----------------------------------------------------------------------
      integer   AT_READ, ichr(640), idx, ierr, j, jdx, k, lun
      character keywrd*8, tcards(32)*80
      include 'rpfits.inc'

      n_su = 0
      do while (.true.)
         do jdx = idx+1, 32
            if (ncard.lt.0) then
               card(-ncard) = tcards(jdx)
               ncard = ncard-1
            end if

            keywrd = tcards(jdx)(1:8)
            if (keywrd.eq.'ENDTABLE') then
               idx = jdx
               go to 999
            else if (keywrd.eq.'HEADER') then
            else if (keywrd.eq.'COMMENT') then
            else
               k = n_su + 1
               if (k.gt.max_su) then
                  stop ' SU TABLE CONTAINS TOO MANY ENTRIES'
               end if

               read (tcards(jdx), 100, iostat=ierr) su_num(k),
     :            su_name(k), su_ra(k), su_dec(k), su_cal(k), su_rad(k),
     :            su_decd(k)
 100           format (bn,i3,a16,2f13.9,1x,a4,2f12.9)
               if (ierr.ne.0) stop ' ERROR READING SU TABLE'

               su_pra(k)  = su_ra(k)
               su_pdec(k) = su_dec(k)
               n_su = n_su + 1
            end if
         end do

         ierr = AT_READ (lun, ichr)
         write (tcards, '(32(20a4,:,/))') (ichr(j), j=1,640)
         idx = 0
      end do

  999 return
      end



      subroutine WRITE_SU_TABLE (idx, tcards)
*-----------------------------------------------------------------------
*     Write an SU (source) table from an RPFITS file.
*
*     Original: Ray Norris 1988/11/08
*-----------------------------------------------------------------------
      integer   idx, isu
      character tcards(*)*80
      include 'rpfits.inc'

      idx = idx + 1
      tcards(idx) = 'TABLE SU'
      idx = idx + 1
      tcards(idx) = 'HEADER  NAME            RA2000   DEC2000' //
     :              '  CALCODE  RA_DATE    DEC_DATE          '

      do isu = 1, n_su
         idx = idx + 1
         write (tcards(idx), 100) su_num(isu), su_name(isu), su_ra(isu),
     :      su_dec(isu), su_cal(isu), su_rad(isu), su_decd(isu)
 100     format (i3,a16,2f13.9,1x,a4,2f12.9)
      end do

      idx = idx + 1
      tcards(idx) = 'ENDTABLE'

      return
      end



      subroutine READFG (lun, tcards, idx)
*-----------------------------------------------------------------------
*     Read a FG (flag) table from an RPFITS file.
*
*     Original: Ray Norris 1988/11/08
*-----------------------------------------------------------------------
      integer   AT_READ, ichr(640), idx, ierr, j, jdx, k, lun
      character keywrd*8, tcards(32)*80
      include 'rpfits.inc'

      n_fg = 0
      do while (.true.)
         do jdx = idx+1, 32
            if (ncard.lt.0) then
               card(-ncard) = tcards(jdx)
               ncard = ncard - 1
            end if

            keywrd = tcards(jdx)(1:8)
            if (keywrd.eq.'ENDTABLE') then
               idx = jdx
               go to 999
            else if (keywrd.eq.'HEADER' ) then
            else if (keywrd.eq.'COMMENT') then
            else
               read (tcards(jdx), 100, iostat=ierr) k, fg_ant(1,k),
     :            fg_ant(2,k), fg_ut(1,k), fg_ut(2,k), fg_if(1,k),
     :            fg_if(2,k), fg_chan(1,k), fg_chan(2,k), fg_stok(1,k),
     :            fg_stok(2,k), fg_reason
 100           format (bn,i3,i2,i3,2f9.1,1x,2i3,i4,i5,2i2,a24)
               if (ierr.ne.0) stop ' ERROR READING FG TABLE'

               n_fg = n_fg + 1
            end if
         end do

         ierr = AT_READ (lun, ichr)
         write (tcards, '(32(20a4,:,/))') (ichr(j), j=1,640)
         idx = 0
      end do

  999 return
      end



      subroutine WRITE_FG_TABLE (idx, tcards)
*-----------------------------------------------------------------------
*     Write a FG (flag) table to an RPFITS file.
*
*     Original: Ray Norris 1988/11/08
*-----------------------------------------------------------------------
      integer   idx, ifg
      character tcards(*)*80
      include 'rpfits.inc'

      idx = idx + 1
      tcards(idx) = 'TABLE FG'
      idx = idx + 1
      tcards(idx) = 'HEADER  ANT   UT    IF     CHAN     STOK       ' //
     :              'REASON'

      do ifg = 1, n_fg
         idx = idx + 1
         write (tcards(idx), 100) ifg, fg_ant(1,ifg), fg_ant(2,ifg),
     :      fg_ut(1,ifg), fg_ut(2,ifg), fg_if(1,ifg), fg_if(2,ifg),
     :      fg_chan(1,ifg), fg_chan(2,ifg), fg_stok(1,ifg),
     :      fg_stok(2,ifg), fg_reason(ifg)
 100     format (i3,i2,i3,2f9.1,1x,2i3,i4,i5,2i2,a24)
      end do

      idx = idx + 1
      tcards(idx) = 'ENDTABLE'

      return
      end



      subroutine READAN (lun, tcards, idx)
*-----------------------------------------------------------------------
*     Read an AN (antenna) table from an RPFITS file.
*
*     Original: Ray Norris 1989/07/17
*-----------------------------------------------------------------------
      integer   AT_READ, iaxis_offset, ichr(640), idx, ierr, j, jdx, lun
      character keywrd*8, tcards(32)*80
      include 'rpfits.inc'

      nant = 0
      do while (.true.)
         do jdx = idx+1, 32
            if (ncard.lt.0) then
               card(-ncard) = tcards(jdx)
               ncard = ncard - 1
            end if

            keywrd = tcards(jdx)(1:8)
            if (keywrd.eq.'ENDTABLE') then
               idx = jdx
               go to 999
            else if (keywrd.eq.'HEADER' ) then
            else if (keywrd.eq.'COMMENT') then
            else
               nant = nant + 1
               if (nant.gt.ant_max) then
                  stop ' AN TABLE CONTAINS TOO MANY ENTRIES'
               end if

               read (tcards(jdx), 100, iostat=ierr) ant_num(nant),
     :            sta(nant), ant_mount(nant), x(nant), y(nant), z(nant),
     :            iaxis_offset
                  if (ierr.ne.0) stop 'ERROR READING AN TABLE'
 100           format (i2,1x,a8,i2,3f14.3,i5)

               axis_offset(nant) = iaxis_offset/1000.0
            end if
         end do

         ierr = AT_READ (lun, ichr)
         write (tcards, '(32(20a4,:,/))') (ichr(j), j=1,640)
         idx = 0
      end do

 999  return
      end



      subroutine WRITE_AN_TABLE (idx, tcards)
*-----------------------------------------------------------------------
*     Write an AN (antenna) table to an RPFITS file.
*
*     Original: Ray Norris 1989/09/29
*-----------------------------------------------------------------------
      integer   iant, idx
      character tcards(*)*80
      include 'rpfits.inc'

      idx = idx + 1
      tcards(idx) = 'TABLE AN'
      idx = idx + 1
      tcards(idx) = 'HEADER      M       X             ' //
     :              'Y             Z       AXIS'

      do iant = 1, nant
         idx = idx + 1
         write (tcards(idx),100) ant_num(iant), sta(iant),
     :      ant_mount(iant), x(iant), y(iant), z(iant),
     :      nint(axis_offset(iant)*1000.0)
 100     format (i2,1x,a8,i2,3f14.3,i5)
      end do

      idx = idx + 1
      tcards(idx) = 'ENDTABLE'

      return
      end



      subroutine READMT (lun, tcards, idx)
*-----------------------------------------------------------------------
*     Read an MT (meteorological) table to an RPFITS file.
*
*     Original: Ray Norris 1989/10/11
*-----------------------------------------------------------------------
      integer   AT_READ, ichr(640), idx, ierr, j, jdx, lun
      character keywrd*8, tcards(32)*80
      include 'rpfits.inc'

      n_mt = 0
      do while (.true.)
         do jdx = idx+1, 32
            if (ncard.lt.0) then
               card(-ncard) = tcards(jdx)
               ncard = ncard - 1
            end if

            keywrd = tcards(jdx)(1:8)
            if (keywrd.eq.'ENDTABLE') then
               idx = jdx
               go to 999
            else if (keywrd.eq.'HEADER' ) then
            else if (keywrd.eq.'COMMENT') then
            else
               n_mt = n_mt + 1
               if (n_mt.gt.max_mt) then
                  stop ' MT TABLE CONTAINS TOO MANY ENTRIES'
               end if

               read (tcards(jdx), 100, iostat=ierr) mt_ant(n_mt),
     :            mt_ut(n_mt), mt_press(n_mt), mt_temp(n_mt),
     :            mt_humid(n_mt)
 100           format (i2,f9.1,f7.1,2f6.1)
               if (ierr.ne.0) stop ' ERROR READING MT TABLE'
            end if
         end do

         ierr = AT_READ (lun, ichr)
         write (tcards, '(32(20a4,:,/))') (ichr(j), j=1,640)
         idx = 0
      end do

 999  return
      end



      subroutine WRITE_MT_TABLE (idx, tcards)
*-----------------------------------------------------------------------
*     Write an MT (meteorological) table to an RPFITS file.
*
*     Original: Ray Norris 1989/10/11
*-----------------------------------------------------------------------
      integer idx, imt
      character tcards(32)*80
      include 'rpfits.inc'

      idx = idx + 1
      tcards(idx) = 'TABLE MT'
      idx = idx + 1
      tcards(idx) = 'HEADER UT PRESS  TEMP  HUMID'

      do imt = 1, n_mt
         idx = idx + 1
         write (tcards(idx), 100) mt_ant(imt), mt_ut(imt),
     :      mt_press(imt), mt_temp(imt), mt_humid(imt)
 100     format (i2,f9.1,f7.1,2f6.1)
      end do

      idx = idx + 1
      tcards(idx) = 'ENDTABLE'

      return
      end



      subroutine READCU (lun, tcards, idx)
*-----------------------------------------------------------------------
*     Read a CU (uncalibration) table from an RPFITS file.
*
*     Original: Ray Norris 1990/03/22
*-----------------------------------------------------------------------
      integer AT_READ, ichr(640), idx, ierr, j, jdx, lun
      character keywrd*8, tcards(32)*80
      include 'rpfits.inc'

      n_cu = 0
      do while (.true.)
         do jdx = idx+1, 32
            if (ncard.lt.0) then
               card(-ncard) = tcards(jdx)
               ncard = ncard - 1
            end if

            keywrd = tcards(jdx)(1:8)
            if (keywrd.eq.'ENDTABLE') then
               idx = jdx
               go to 999
            else if (keywrd.eq.'HEADER' ) then
            else if (keywrd.eq.'COMMENT') then
            else
               n_cu = n_cu + 1
               if (n_cu.gt.max_cu) then
                  stop ' CU TABLE CONTAINS TOO MANY ENTRIES'
               end if

               read (tcards(jdx), 100, iostat=ierr) cu_ut(n_cu),
     :            cu_ant(n_cu), cu_if(n_cu), cu_cal1(n_cu),
     :            cu_cal2(n_cu), cu_ch1(n_cu), cu_ch2(n_cu)
 100           format (bn,f8.1,i3,i4,f6.1,f7.1,2i5)
               if (ierr.ne.0) stop ' ERROR READING CU TABLE'
            end if
         end do

         ierr = AT_READ (lun, ichr)
         write (tcards, '(32(20a4,:,/))') (ichr(j), j=1,640)
         idx = 0
      end do

 999  return
      end



      subroutine WRITE_CU_TABLE (idx, tcards)
*-----------------------------------------------------------------------
*     Write a CU (uncalibration) table to an RPFITS file.
*
*     Original: Ray Norris 1989/10/11
*-----------------------------------------------------------------------
      integer icu, idx
      character tcards(*)*80
      include 'rpfits.inc'

      idx = idx + 1
      tcards(idx) = 'TABLE CU'
      idx = idx + 1
      tcards(idx) = 'HEADER  ANT IF CALSTART  CALSTOP   CH1  CH2'

      do icu = 1, n_cu
         idx = idx + 1
         write (tcards(idx), 100) cu_ut(n_cu), cu_ant(n_cu),
     :      cu_if(n_cu), cu_cal1(n_cu), cu_cal2(n_cu), cu_ch1(n_cu),
     :      cu_ch2(n_cu)
 100     format (f8.1,i3,i4,f6.1,f7.1,2i5)
      end do

      idx = idx + 1
      tcards(idx) = 'ENDTABLE'

      return
      end

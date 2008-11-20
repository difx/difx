C-----------------------------------------------------------------------
C     SUBROUTINE RPFITSIN
C-----------------------------------------------------------------------
C
C     For information on the use of this software, and on the RPFITS
C     format, see the file RPFITS.DEFN.
C
C     Programmer: Ray Norris
C     Date: 25 April 1985
C
C     $Id: rpfitsin.f,v 1.45 2008/10/27 05:37:12 cal103 Exp $
C-----------------------------------------------------------------------

      subroutine RPFITSIN (jstat, vis, weight, baseline, ut, u, v, w,
     :   flag, bin, if_no, sourceno)

      integer baseline, flag, bin, if_no, sourceno
      real    weight(*), ut, u, v, w
      complex vis(*)


      include 'rpfits.inc'

      logical   async, endhdr, endscan, isopen, new_antenna, open_only,
     :          starthdr
      integer   AT_CLOSE, AT_OPEN_READ, AT_READ, AT_SKIP_EOF, AT_UNREAD,
     :          bufleft, bufleft3, bufptr, grplength, grpptr, i, i1, i2,
     :          i3, i_buff(640), i_grphdr(11), icard, ierr, illegal, j,
     :          jstat, k, lun, pcount, SIMPLE
      real      buffer(640), crpix4, grphdr(11), r1, r2, revis,
     :          sc_buf(max_sc*max_if*ant_max), velref, pra, pdec
      double precision d2pi
      character keyvalue*20, keyword*8, m(32)*80, terr*2

      equivalence (i_buff(1), buffer(1))
      equivalence (i_grphdr(1), grphdr(1))
      equivalence (sc_buf(1), sc_cal(1,1,1))

      parameter (d2pi = 2d0 * 3.14159265358979323846d0)

      data isopen  /.false./
      data async   /.false./
      data new_antenna /.false./
      data illegal /32768/

      save

C-------------------------- DECIDE ON ACTION ---------------------------

      rp_iostat = 0
      errmsg = ''

      open_only = jstat.eq.-3

      if (jstat.eq.-3) go to 1000
      if (jstat.eq.-2) go to 1000
      if (jstat.eq.-1) go to 2000
      if (jstat.eq.0) go to 3000
      if (jstat.eq.1) go to 5000
      if (jstat.eq.2) go to 6000

      write (errmsg, '(a,i3)') 'Illegal value of jstat =', jstat
      call RPFERR (errmsg)
      jstat = -1
      RETURN

C--------------------------- OPEN FITS FILE ----------------------------

 1000 if (isopen) then
         call RPFERR ('File is already open.')
         jstat = -1
         RETURN
      end if

      rp_iostat =  AT_OPEN_READ (file, async, lun)
      if (rp_iostat.ne.0) then
         call RPFERR ('File open error')
         jstat = -1
         RETURN
      end if
      isopen = .true.

      if (open_only) then
         jstat = 0
         RETURN
      end if

C----------------------------- READ HEADER -----------------------------

 2000 if (.not.isopen) then
         call RPFERR ('File is not open.')
         jstat = -1
         RETURN
      end if

      bufptr = 0
      n_if = 0
      icard = 1
      if (ncard.lt.0) ncard = -1
      an_found = .false.
      if_found = .false.
      su_found = .false.
      fg_found = .false.
      nx_found = .false.
      mt_found = .false.
      cu_found = .false.
      pra = 0.0
      pdec = 0.0

C     Look for start of next header.
      starthdr = .false.
      do while (.not.starthdr)
         rp_iostat = AT_READ (lun, buffer)
         if (rp_iostat.ne.0) then
            if (rp_iostat.eq.-1) then
               jstat = 3
               RETURN
            end if

            call RPFERR ('I/O error reading header')
            jstat = -1
            RETURN
         end if

         jstat = SIMPLE (buffer, lun)
         if (jstat.eq.1) then
C           Start of header.
            starthdr = .true.
         else if (jstat.eq.3) then
C           End-of-file while reading flag table.
            RETURN
         else if (jstat.eq.4) then
C           Encountered flag table.
            RETURN
         else if (jstat.ne.0) then
C           Fortran I/O error status.
            jstat = -1
            RETURN
         end if

         write (m, '(32(20a4,:,/))') (buffer(j),j=1,640)
      end do

C     Scan through header, getting the interesting bits.
      endhdr = .false.
      do 2500 while (.not.endhdr)
         if (.not.starthdr) then
            rp_iostat = AT_READ (lun, buffer)
            write (m,'(32(20a4,:,/))') (buffer(j),j=1,640)
            if (rp_iostat.ne.0) then
               if (rp_iostat.eq.-1) then
                  jstat = 3
                  RETURN
               end if

               call RPFERR ('I/O error reading header')
               jstat = -1
               RETURN
            end if
         end if

         starthdr = .false.
         version = ' '
         do 2400 i = 1, 32
C           Parse the PFITS keyword and keyvalue.
            keyword  = m(i)(1:8)

            if (m(i)(11:11).eq.'''') then
C              Must be a character value.
               keyvalue = m(i)(12:31)
               do j = 1, 20
                  if (keyvalue(j:j).eq.'''') then
C                    Strip off the trailing apostrophe.
                     keyvalue(j:) = ' '
                  end if
               end do
            else
               keyvalue = m(i)(11:30)
            end if

C           Lexical chop based on the first letter of the keyword name.
            if (keyword(:1).le.'C') then
C              Keyword names beginning with A to C.
               if (keyword.eq.'ALTRVAL ') then
                  read (keyvalue, '(g20.12)') vel1
               else if (keyword.eq.'BUNIT') then
                  bunit = keyvalue
               else if (keyword.eq.'CAL') then
                  cal = keyvalue
               else if (keyword.eq.'CDELT4') then
                  read (keyvalue, '(g20.12)') dfreq
               else if (keyword.eq.'CRPIX4') then
                  read (keyvalue, '(g20.12)') crpix4
               else if (keyword.eq.'CRVAL4') then
                  read (keyvalue, '(g20.12)') freq
               else if (keyword.eq.'CRVAL5') then
                  read (keyvalue, '(g20.12)') ra
                  if (ra.lt.0d0) ra = ra + d2pi
               else if (keyword.eq.'CRVAL6') then
                  read (keyvalue, '(g20.12)') dec
               end if

            else if (keyword(:1).le.'E') then
C              Keyword names beginning with D or E.
               if (keyword.eq.'DATE') then
C                 Fix old-format dates.
                  call datfit(keyvalue(:10), datwrit, ierr)
               else if (keyword.eq.'DATE-OBS') then
C                 Fix old-format dates.
                  call datfit(keyvalue(:10), datobs, ierr)
                  datsys = m(i)(35:36)
                  if (datsys.eq.'UT D') datsys = 'UT'
               else if (keyword.eq.'DEFEAT  ') then
                  read (keyvalue, '(i20)') rp_defeat
               else if (keyword.eq.'DJMREFP ') then
                  read (keyvalue, '(g20.12)') rp_djmrefp
               else if (keyword.eq.'DJMREFT ') then
                  read (keyvalue, '(g20.12)') rp_djmreft
               else if (keyword.eq.'END') then
C                 END card.
                  endhdr = .true.
               else if (keyword(1:5).eq.'EPHEM') then
                  read (keyword(6:7), '(i2)') k
                  read (keyvalue, '(g20.12)') rp_c(k)
               else if (keyword.eq.'EPOCH') then
                  coord = keyvalue
               end if

            else if (keyword(:1).le.'N') then
C              Keyword names beginning with F to N.
               if (keyword.eq.'GCOUNT') then
                  read (keyvalue, '(i20)') ncount
               else if (keyword(1:5).eq.'HUMID') then
                  read (keyword(6:7), '(i2)') k
                  read (keyvalue, '(g20.12)') rp_humid(k)
               else if (keyword.eq.'INSTRUME') then
                  instrument = keyvalue
               else if (keyword.eq.'INTIME') then
                  read (keyvalue, '(i20)') intime
               else if (keyword.eq.'NAXIS2') then
                  read (keyvalue, '(i20)') data_format
                  write_wt = data_format.eq.3
               else if (keyword.eq.'NAXIS3') then
                  read (keyvalue, '(i20)') nstok
               else if (keyword.eq.'NAXIS4') then
                  read (keyvalue, '(i20)') nfreq
               else if (keyword.eq.'NAXIS7') then
C                 Note fudge for intermediate format PTI data.
                  read (keyvalue, '(i20)') nstok
               end if

            else if (keyword(:1).le.'P') then
C              Keyword names beginning with M to P.
               if (keyword.eq.'OBJECT') then
                  object = keyvalue
               else if (keyword.eq.'OBSERVER') then
                  rp_observer = keyvalue
               else if (keyword.eq.'OBSTYPE') then
                  obstype = keyvalue
               else if (keyword.eq.'PCOUNT') then
                  read (keyvalue, '(i20)') pcount
               else if (keyword.eq.'PMDEC') then
                  read (keyvalue, '(g20.12)') pm_dec
               else if (keyword.eq.'PMEPOCH') then
                  read (keyvalue, '(g20.12)') pm_epoch
               else if (keyword.eq.'PMRA') then
                  read (keyvalue, '(g20.12)') pm_ra
               else if (keyword.eq.'PNTCENTR') then
                  read (m(i)(11:35),'(g12.9,1x,g12.9)') pra,pdec
               else if (keyword(1:5).eq.'PRESS') then
                  read (keyword(6:7), '(i2)') k
                  read (keyvalue, '(g20.12)') rp_pressure(k)
               end if

            else
C              Keyword names beginning with Q to Z.
               if (keyword.eq.'RESTFREQ') then
                  read (keyvalue, '(g20.12)') rfreq
               else if (keyword.eq.'RPFITS  ') then
                  rpfitsversion = keyvalue
               else if (keyword.eq.'SCANS ') then
                  read (keyvalue, '(i20)') nscan
               else if (keyword(1:6).eq.'TABLE ') then
C                 Sort out tables.
                  call RPFITS_READ_TABLE (lun, m, i, endhdr, terr, ierr)
                  if (ierr.ne.0) then
                     if (ierr.eq.1) then
                        jstat = -1
                        call RPFERR (terr // ' table contains too ' //
     :                               'many entries.')
                     else if (rp_iostat.lt.0) then
                        jstat = 3
                     else
                        jstat = -1
                        call RPFERR ('I/O error reading ' // terr //
     :                               ' table')
                     end if

                     RETURN
                  end if

               else if (keyword(1:5).eq.'TEMPE') then
                  read (keyword(6:7), '(i2)') k
                  read (keyvalue, '(g20.12)') rp_temp(k)
               else if (keyword.eq.'UTCMTAI ') then
                  read (keyvalue, '(g20.12)') rp_utcmtai
               else if (keyword.eq.'VELREF  ') then
                  read (keyvalue, '(g20.12)') velref
               else if (keyword.eq.'VERSION ') then
                  version = keyvalue
               end if
            end if

C           Write into "cards" array if necessary.
            if (ncard.gt.0) then
               do j = 1, ncard
                  if (keyword.eq.card(j)(1:8)) then
                     card(j) = m(i)
                  end if
               end do
            else if (ncard.lt.0) then
               if (icard.le.max_card .and. .not.endhdr) then
                  card(-ncard) = m(i)
                  icard = icard + 1
                  ncard = ncard - 1
               end if
            end if

C           Antenna parameters.
            if (keyword(:7).eq.'ANTENNA') then
               if (.not.new_antenna) then
                  nant = 0
                  new_antenna = .true.
               end if

               if (keyword.eq.'ANTENNA') then
                  read (m(i)(11:80), 2200) k, sta(k), x(k), y(k), z(k)
 2200             format (i1,1x,a3,3x,g17.10,3x,g17.10,3x,g17.10)
               else
C                 Old format ('ANTENNA:').
                  read (m(i)(12:71), 2300) k, x(k), y(k), z(k), sta(k)
 2300             format (i1,4x,g13.6,3x,g13.6,3x,g13.6,5x,a3)
               end if

               nant = nant + 1
            end if

            if (ENDHDR) go to 2500
 2400    continue
 2500 continue
      ncard = ABS(ncard)

C     Set up for reading data.
      if (data_format.lt.1 .or. data_format.gt.3) then
         call RPFERR ('NAXIS2 must be 1, 2, or 3.')
         jstat = -1
         RETURN
      end if

C     Insert default values into table commons if tables weren't found.
      if (.not.if_found) then
         n_if = 1
         if_freq(1) = freq
         if_invert(1) = 1
         if_bw(1) = nfreq*dfreq
         if_nfreq(1) = nfreq
         if_nstok(1) = nstok
         if_ref(1) = crpix4
         do i = 1, 4
            if_cstok(i,1) = ' '
         end do
         if_simul(1) = 1
         if_chain(1) = 1
      else
         freq  = if_freq(1)
         nfreq = if_nfreq(1)
         if (if_nfreq(1).gt.1) then
            dfreq = if_bw(1)/(if_nfreq(1) - 1)
         else
            dfreq = if_bw(1)/if_nfreq(1)
         end if
         nstok = if_nstok(1)
      end if
      if (.not. su_found) then
         n_su = 1
         su_name(1) = object
         su_ra(1)  = ra
         su_dec(1) = dec
      else
         object = su_name(1)
         ra  = su_ra(1)
         dec = su_dec(1)
C        For single source, record possible pointing centre offset
         if (n_su.eq.1 .and. (pra.ne.0.0 .or. pdec.ne.0.0)) then
           su_pra(1) = pra
           su_pdec(1) = pdec
         end if
      end if

C     Tidy up.
      n_if = max(n_if, 1)
      ivelref = velref + 0.5
      new_antenna = .false.
      bufptr = 0

      jstat = 0
      RETURN

C----------------------- READ DATA GROUP HEADER ------------------------
 3000 if (.not.isopen) then
         call RPFERR ('File is not open.')
         jstat = -1
         RETURN
      end if

C     THE FOLLOWING POINTERS AND COUNTERS ARE USED HERE:
C     GRPLENGTH      No. of visibilities in group
C     GRPPTR         Pointer to next visibility in group to be read
C     BUFPTR         Pointer to next word to be read in current buffer
C     BUFLEFT        No. of words still to be read from current buffer
C
C     Note that data are read in blocks of 5 records = 640 (4byte)
C     words.

      grpptr = 1
      if_no = 1

      if (bufptr.eq.0 .or. bufptr.eq.641) then
         rp_iostat = AT_READ (lun, buffer)
         if (rp_iostat.ne.0) then
            if (rp_iostat.eq.-1) then
               jstat = 3
               RETURN
            end if

            call RPFERR ('I/O error reading data')
            jstat = -1
            RETURN
         end if

         jstat = SIMPLE (buffer, lun)
         if (jstat.ne.0) then
            rp_iostat = AT_UNREAD (lun, buffer)
            RETURN
         end if

         bufptr = 1

      end if


C     READ PARAMETERS FROM FITS FILE
C     FORMAT FROM RPFITS IS:
C      ------ VIS data -------------      ----------- SYSCAL data ----
C      (baseline > 0)                         (baseline = -1)
C      param 1=u in m                         0.0
C      param 2=v in m                         0.0
C      param 3=w in m                         0.0
C      param 4=baseline number                -1.0
C      param 5=UT in seconds                  sc_ut: UT in seconds
C      param 6= flag (if present)             sc_ant
C      param 7= bin  (if present)             sc_if
C      param 8=if_no (if present)             sc_q
C      param 9=sourceno (if present)          sc_srcno
C      param 10=intbase (if present)          intbase (if present)

 3100 bufleft = 641 - bufptr

C     End of scan?
      call VAXI4 (i_buff(bufptr), i1)
      endscan = i1.eq.illegal

      if (.not.endscan .and. bufleft.ge.pcount) then
C        Old rpfits files may be padded with zeros, so check for u,
C        baseline no and UT all zero.  Assume that if next vis
C        incomplete at end of buffer, next buffer will be all zeros.

         call VAXI4 (i_buff(bufptr+3), i2)
         call VAXI4 (i_buff(bufptr+4), i3)
         endscan = i1.eq.0 .and. i2.eq.0 .and. i3.eq.0
      end if

      if (endscan) then
         rp_iostat = AT_READ (lun, buffer)
         if (rp_iostat.ne.0) then
            if (rp_iostat.eq.-1) then
               jstat = 3
               RETURN
            end if

            call RPFERR ('I/O error reading header')
            jstat = -1
            RETURN
         end if

         jstat = SIMPLE (buffer, lun)
         if (jstat.ne.0) then
            rp_iostat = AT_UNREAD (lun, buffer)
            RETURN
         end if

         bufptr = 1
         jstat = 5
         RETURN
      end if

C     ------------NOW READ DATA -------------

      if (bufleft.ge.pcount) then

C        If it will all fit in current buffer, then things are easy.
         call GETPARM (jstat, buffer, i_buff, bufptr, bufptr, buffer,
     :      pcount, u, v, w, baseline, lun, ut, flag, bin, if_no,
     :      sourceno)
         if (jstat.eq.-2) goto 3100
         if (jstat.ne.0) RETURN
         bufptr = bufptr+pcount

      else
C        We can recover only part of the group header.  Dispose of what
C        we have, then read the remainder from the next batch of data
C        (pcount blocks).

         do i = 1,bufleft
            i_grphdr(i) = i_buff(bufptr+i-1)
         end do
         rp_iostat = AT_READ (lun, buffer)
         if (rp_iostat.ne.0) then
            if (rp_iostat.eq.-1) then
               jstat = 3
               RETURN
            end if

            call RPFERR ('I/O error reading data')
            jstat = -1
            RETURN
         end if

         jstat = SIMPLE (buffer, lun)
         if (jstat.ne.0) then
            rp_iostat = AT_UNREAD (lun, buffer)
            RETURN
         end if

         bufptr = pcount-bufleft

C        Extract bufptr items from the next buffer.
         do i = 1, bufptr
            i_grphdr(i+bufleft) = i_buff(i)
         end do

         call GETPARM (jstat, grphdr, i_grphdr, 1, bufptr, buffer,
     :      pcount, u, v, w, baseline, lun, ut, flag, bin, if_no,
     :      sourceno)
         if (jstat.eq.-2) goto 3100
         if (jstat.ne.0) RETURN

C        Set bufptr to the first visibility in the new buffer.
         bufptr = bufptr + 1

      end if


C     Determine GRPLENGTH.
      if (baseline.eq.-1) then
         grplength = sc_q*sc_if*sc_ant
      else if (if_no.gt.1) then
         grplength = if_nfreq(if_no)*if_nstok(if_no)
      else
         grplength = nstok*nfreq
      end if

      if (baseline.eq.-1) go to 4000

C--------------------- READ VISIBILITY DATA GROUP ----------------------

C     The RPFITS data format is determined by the value of NAXIS2:
C
C        NAXIS2      word 1    word 2    word 3
C        ------     --------  --------  --------
C           1       Real(vis)     -         -
C           2       Real(vis) Imag(vis)     -
C           3       Real(vis) Imag(vis)  Weight

      if (data_format.lt.1 .or. data_format.gt.3) then
         call RPFERR ('NAXIS2 in file must be 1, 2, or 3.')
         jstat = -1
         RETURN
      end if

 3500 bufleft = 641 - bufptr
         if (bufleft.ge.(data_format*(grplength-grpptr+1))) then
C           Entire group can be filled from existing buffer.
            do i = grpptr, grplength
               if (data_format.eq.1) then
                  call VAXR4 (buffer(bufptr), vis(i))
               else
                  call VAXR4 (buffer(bufptr),   r1)
                  call VAXR4 (buffer(bufptr+1), r2)
                  vis(i) = CMPLX(r1, r2)

                  if (data_format.eq.3) then
                     call VAXR4 (buffer(bufptr+2), weight(i))
                  end if
               end if
               bufptr = bufptr + data_format
            end do

            jstat = 0
            RETURN

         else
C           Otherwise things are a bit more complicated, first read
C           complete visibilities in old buffer.
            bufleft3 = bufleft/data_format
            do i = 1, bufleft3
               if (data_format.eq.1) then
                  call VAXR4 (buffer(bufptr), vis(grpptr+i-1))
               else
                  call VAXR4 (buffer(bufptr), r1)
                  call VAXR4 (buffer(bufptr+1), r2)
                  vis(grpptr+i-1) = CMPLX(r1, r2)

                  if (data_format.eq.3) then
                     call VAXR4 (buffer(bufptr+2), weight(grpptr+i-1))
                  end if
               end if
               bufptr = bufptr + data_format
            end do
            grpptr = grpptr + bufleft3

C           Read the fraction of a visibility left in old buffer.
C           Should not happen for data_format = 1.
            bufleft = bufleft - data_format*bufleft3
            if (bufleft.eq.1) then
               call VAXR4 (buffer(640), revis)
            else if (bufleft.eq.2 .and. data_format.eq.3) then
               call VAXR4 (buffer(639), r1)
               call VAXR4 (buffer(640), r2)
               vis(grpptr) = CMPLX(r1, r2)
            end if

C           Now read in a new buffer.
            rp_iostat = AT_READ (lun, buffer)
            if (rp_iostat.ne.0) then
               if (rp_iostat.eq.-1) then
                  jstat = 3
                  RETURN
               end if

               call RPFERR ('I/O error reading data')
               jstat = -1
               RETURN
            end if

            jstat = SIMPLE (buffer, lun)
            if (jstat.ne.0) then
               rp_iostat = AT_UNREAD (lun, buffer)
               RETURN
            end if

C           Fill any incomplete visibility (data_format = 2 or 3 only).
            if (bufleft.eq.0) then
               bufptr = 1

            else if (bufleft.eq.1) then
               call VAXR4 (buffer(1), r1)
               vis(grpptr) = CMPLX(revis, r1)
               if (data_format.eq.3) then
                  call VAXR4 (buffer(2), weight(grpptr))
               end if
               grpptr = grpptr + 1
               bufptr = data_format

            else if (bufleft.eq.2 .and. data_format.eq.3) then
               call VAXR4 (buffer(1), weight(grpptr))
               grpptr = grpptr + 1
               bufptr = 2
            end if
         end if

C        Return to pick up the rest of the group.
      go to 3500

C----------------------- READ SYSCAL DATA GROUP ------------------------

C     Note that in this context GRPLENGTH is in units of words, not
C     visibilities.

 4000 bufleft = 641 - bufptr
         if (bufleft.ge.(grplength-grpptr+1)) then

C           Entire group can be filled from existing buffer.
            do i = grpptr, grplength
               call VAXR4 (buffer(bufptr), sc_buf(i))
               bufptr = bufptr + 1
            end do

            jstat = 0
            RETURN

         else
C           Otherwise read complete visibilities in old buffer.
            do i = 1, bufleft
               call VAXR4 (buffer(bufptr), sc_buf(grpptr+i-1))
               bufptr = bufptr + 1
            end do
            grpptr = grpptr + bufleft

C           Then read in a new buffer.
            rp_iostat = AT_READ (lun, buffer)
            if (rp_iostat.ne.0) then
               if (rp_iostat.eq.-1) then
                  jstat = 3
                  RETURN
               end if

               call RPFERR ('I/O error reading data')
               jstat = -1
               RETURN
            end if

            jstat = SIMPLE (buffer, lun)
            if (jstat.ne.0) then
               rp_iostat = AT_UNREAD (lun, buffer)
               RETURN
            end if
            bufptr = 1
         end if

C        Go back to pick up the rest of the group.
      go to 4000

C--------------------------- CLOSE FITS FILE ---------------------------

 5000 if (isopen) then
         rp_iostat = AT_CLOSE (lun)
         if (rp_iostat.ne.0) then
            call RPFERR ('I/O error closing file')
            jstat = -1
            RETURN
         end if
         isopen = .false.
      end if

      jstat = 0
      RETURN

C------------------------- SKIP TO END OF FILE -------------------------

 6000 if (.not.isopen) then
         call RPFERR ('File is not open.')
         jstat = -1
         RETURN
      end if

      rp_iostat = AT_SKIP_EOF (lun)
      if (rp_iostat.eq.-1) then
         jstat = 3
      else
         call RPFERR ('I/O error skipping to EOF')
         jstat = -1
         RETURN
      end if

      return
      end

C-----------------------------------------------------------------------

      integer function SIMPLE (buffer, lun)

C-----------------------------------------------------------------------
C     SIMPLE tests for the start of a new header or FG (flag) table.
C     Reads the FG table if encountered.
C-----------------------------------------------------------------------

      include 'rpfits.inc'

      logical   endhdr
      integer   ierr, j, lun
      character m(80)*32, terr*2
      real buffer(640)

C     Assume not.
      SIMPLE = 0

C     Write first 8 characters from buffer into character string.
      write (m(1)(1:8),'(2a4)') (buffer(j),j=1,2)

      if (m(1)(1:6).eq.'SIMPLE') then
C        Start of header.
         SIMPLE = 1

      else if (m(1)(1:8).eq.'FG TABLE') then
C        Start of FG (flag) table.
         SIMPLE = 4

         write (m, '(32(20a4,:,/))') (buffer(j),j=1,640)
         call RPFITS_READ_TABLE (lun, m, -1, endhdr, terr, ierr)
         if (ierr.ne.0) then
            if (ierr.eq.1) then
               call RPFERR ('FG table contains too many entries.')
               SIMPLE = -1
            else if (rp_iostat.lt.0) then
               SIMPLE = 3
            else
               SIMPLE = -1
               call RPFERR ('I/O error reading FG table')
            end if
         end if
      end if

      return
      end

C-----------------------------------------------------------------------

      subroutine GETPARM (jstat, grphdr, i_grphdr, grpptr, bufptr,
     :                    buffer, pcount, u, v, w, baseline, lun, ut,
     :                    flag, bin, if_no, sourceno)

C-----------------------------------------------------------------------
C     Read group header parameters from grphdr and check validity.
C     If invalid, scan through the data until valid data are found and
C     return the new buffer and bufptr.
C
C     jstat is 0 on exit for immediate success, or -2 if success was
C     achieved after skipping data, or -1 for a total lack of success.
C-----------------------------------------------------------------------

      include 'rpfits.inc'

      logical   ILLPARM
      integer   baseline, bin, bufptr, flag, grpptr, i_grphdr(640),
     :          iant, if_no, iif, iq, jstat, lun, pcount, sourceno
      real      grphdr(640), buffer(640), rbase, u, v, w, ut

C     First 5 parameters are always there - you hope!
      call VAXR4 (grphdr(grpptr),   u)
      call VAXR4 (grphdr(grpptr+1), v)
      call VAXR4 (grphdr(grpptr+2), w)
      call VAXR4 (grphdr(grpptr+3), rbase)
      call VAXR4 (grphdr(grpptr+4), ut)

      if (rbase.lt.0.0) then
C        Syscal parameters.
         call VAXI4 (i_grphdr(grpptr+5), iant)
         call VAXI4 (i_grphdr(grpptr+6), iif)
         call VAXI4 (i_grphdr(grpptr+7), iq)
      else
C        IF number.
         call VAXI4 (i_grphdr(grpptr+7), iif)

         if (pcount.ge.11) then
C           Otherwise, data_format comes from NAXIS2.
            call VAXI4 (i_grphdr(grpptr+10), data_format)
         end if
      end if

C     Check for illegal parameters.
      if (ILLPARM(u, v, w, rbase, ut, iant, iif, iq)) then
C        This can be caused by a bad block, so look for more data.
         call RPFERR ('Corrupted data encountered, skipping...')
         call SKIPTHRU (jstat, bufptr, buffer, lun, pcount)
         RETURN
      end if

C     Looks ok, pick up remaining parameters.
      baseline = NINT(rbase)
      if (baseline.eq.-1) then
C        Syscal parameters.
         sc_ut  = ut
         sc_ant = iant
         sc_if  = iif
         sc_q   = iq
         call VAXI4 (i_grphdr(grpptr+8), sc_srcno)
         if (pcount.gt.9) then
            call VAXR4 (REAL(i_grphdr(grpptr+9)), intbase)
         else
            intbase = 0.0
         end if

      else if (pcount.gt.5) then
         call VAXI4 (i_grphdr(grpptr+5), flag)
         call VAXI4 (i_grphdr(grpptr+6), bin)
         call VAXI4 (i_grphdr(grpptr+7), if_no)
         call VAXI4 (i_grphdr(grpptr+8), sourceno)

         if (pcount.gt.9) then
            call VAXR4 (grphdr(grpptr+9), intbase)
         else
            intbase = intime
         end if
      end if

      jstat = 0
      return
      end

*-----------------------------------------------------------------------

      logical function ILLPARM (u, v, w, rbase, ut, iant, iif, iq)

*-----------------------------------------------------------------------
*     Check for any illegal parameters; return true if so.
*-----------------------------------------------------------------------

      include 'rpfits.inc'

      integer  baseline, iant, iant1, iant2, iif, iq
      real     u, ut, v, w, rbase

      if (data_format.lt.1 .or. data_format.gt.3) then
*        Invalid data format.
         ILLPARM = .true.

      else if (abs(u).gt.1e10 .or.
     :         abs(v).gt.1e10 .or.
     :         abs(w).gt.1e10) then
*        Invalid visibility coordinate.
         ILLPARM = .true.

      else if (rbase.lt.-1.1 .or. rbase.gt.(257*nant+0.1)) then
*        Invalid baseline number.
         ILLPARM = .true.

      else if (ut.lt.0.0 .or. ut.gt.172800.0) then
*        Invalid time.
         ILLPARM = .true.

      else
*        Baseline can now safely be converted to integer.
         baseline = NINT(rbase)

         if (ABS(rbase - baseline).gt.0.001) then
*           This value is not close enough to an integer to be valid.
            ILLPARM = .true.

         else
            if (baseline.eq.-1) then
*              Syscal record.
               ILLPARM = iant.lt.1 .or. iant.gt.ant_max .or.
     :                    iif.lt.1 .or.  iif.gt.max_if  .or.
     :                     iq.lt.1 .or.   iq.gt.100

            else
*              Data record.
               iant1 = baseline/256
               iant2 = MOD(baseline,256)
               ILLPARM = iant1.lt.1 .or. iant1.gt.nant .or.
     :                   iant2.lt.1 .or. iant2.gt.nant .or.
     :                   iif.lt.0   .or. iif.gt.max_if
            end if
         end if
      end if

      return
      end

C-----------------------------------------------------------------------

      subroutine SKIPTHRU (jstat, bufptr, buffer, lun, pcount)

C-----------------------------------------------------------------------
C     Skip through data looking for recognisable data or header.
C
C     Returns jstat = -2 if successful.
C
C     rpn 17/11/90
C-----------------------------------------------------------------------

      include 'rpfits.inc'

      logical   ILLPARM
      integer   AT_READ, AT_UNREAD, bufptr, i, iant, iif, iq, j, jstat,
     :          lun, pcount, SIMPLE
      real      buffer(640), rbase, u, ut, v, w

      do 10 j = 1, 1000
C        Read a new block; the remainder of the old one is unlikely to
C        contain anything useful (and at most one integration).
         rp_iostat = AT_READ (lun, buffer)
         if (rp_iostat.ne.0) then
            if (rp_iostat.eq.-1) then
               jstat = 3
               RETURN
            end if

            call RPFERR ('Read error')
            jstat = -1
            RETURN
         end if

C        Check to see if it's a header block.
         jstat = SIMPLE (buffer, lun)
         if (jstat.ne.0) then
            rp_iostat = AT_UNREAD (lun, buffer)
            RETURN
         end if
         bufptr = 1

C        Scan through the block looking for something legal.
         do i = 1, 640
            call VAXR4 (buffer(bufptr),   u)
            call VAXR4 (buffer(bufptr+1), v)
            call VAXR4 (buffer(bufptr+2), w)
            call VAXR4 (buffer(bufptr+3), rbase)
            call VAXR4 (buffer(bufptr+4), ut)

            if (rbase.lt.0.0) then
C              Syscal parameters.
               call VAXI4 (buffer(bufptr+5), iant)
               call VAXI4 (buffer(bufptr+6), iif)
               call VAXI4 (buffer(bufptr+7), iq)
            else
C              IF number.
               call VAXI4 (buffer(bufptr+7), iif)

               if (pcount.ge.11) then
C                 Otherwise, data_format comes from NAXIS2.
                  call VAXI4 (buffer(bufptr+10), data_format)
               end if
            end if

            if (.not.ILLPARM(u, v, w, rbase, ut, iant, iif, iq)) then
               goto 999
            end if

            bufptr = bufptr + 1
            if (bufptr.gt.632) goto 10
         end do
 10   continue

C     Success!
 999  jstat = -2
      return
      end

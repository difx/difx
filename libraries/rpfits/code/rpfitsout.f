C-----------------------------------------------------------------------
C
C                     SUBROUTINE RPFITSOUT
C
C-----------------------------------------------------------------------
      subroutine RPFITSOUT (jstat, vis, weight, baseline, ut, u, v,
     +   w, flag, bin, if_no, sourceno)
C
C     This routine is for writing an RPFITS file.
C     Its function when called depends primarily on the value of
C     JSTAT, as follows:
C           JSTAT=-3......Open file
C           JSTAT=-2......Open file and write a header
C           JSTAT=-1......Write a header
C           JSTAT=0.......Write a data group to the file
C           JSTAT=1.......Close the file.
C           JSTAT=2.......Write an FG table to the end of the data
C           JSTAT=3.......Flush buffer, close file, reopen file
C     When JSTAT=1 or -1, all the other arguments are dummy, and are
C     left unchanged.
C     When JSTAT=0, all the parameters and the contents of common blocks
C     should be set before entry. They are all unchanged on exit, except
C     DATOBS which, if blank on entry, will contain the current UT
C     date on exit.
C
C     On exit, the value of JSTAT indicates the success of the operation:
C           JSTAT=-1.........Operation unsuccessful
C           JSTAT=0..........Operation successful
C
C-----------------DUMMY ARGUMENTS---------------------------------------

      integer baseline, flag, bin, if_no, sourceno
      real    weight(*), ut, u, v, w
      complex vis(*)

C-----------------OTHER BITS & PIECES-----------------------------------

      include 'rpfits.inc'

      logical   async, open, scan_head
      integer   actualdim, AT_CLOSE, AT_CREATE, AT_REOPEN_WRITE,
     +          AT_WRITE, bufleft, bufleft3, bufptr, cdelt3, crval3,
     +          grplength, grpptr, i, i_buff(640), i_grphdr(11), icard,
     +          illegal, instat, jstat, k, length, lun, max_header,
     +          n_words, newdim, pcount
      real      buffer(640), crpix4, grphdr(11),
     +          sc_buf(max_sc*max_if*ant_max)
      double precision d2pi
      character key*8, utdate*12

      equivalence (i_buff(1), buffer(1))
      equivalence (i_grphdr(1), grphdr(1))
      equivalence (sc_buf(1), sc_cal(1,1,1))

      parameter (d2pi = 2d0 * 3.14159265358979323846d0)

C-------------------TEMPLATE FOR FILE HEADERS---------------------------

      parameter (MAX_HEADER = 650)
      parameter (ACTUALDIM = 70)
      character m(MAX_HEADER)*80
      integer   ichr(640), j
      character mout(32)*80
      real      chr(640)
      equivalence (ichr, chr)

      data illegal /32768/
      data bufptr  /1/
      data async   /.false./
      data length  /2560/
      data open    /.false./
      data pcount  /11/
      data rpfitsversion /'2.11                '/

      data (m(i), i = 1,15) /
     + 'SIMPLE  =                    F  / NONCONFORMIST',
     + 'FORMAT  =             ''RPFITS''  / RPFITS',
     + 'SCANS   =                   -1  / No. of scans in file',
     + 'BITPIX  =                  -32  / Values are real',
     + 'NAXIS   =                    6  /',
     + 'NAXIS1  =                    0  / Required for grouped data',
     + 'NAXIS2  =                    3  / Complex=real,imag.,weight',
     + 'NAXIS3  =                    ?  / No. of Stokes parameters',
     + 'NAXIS4  =                    ?  / No. of frequencies',
     + 'NAXIS5  =                    1  / Right Ascension (EPOCH)',
     + 'NAXIS6  =                    1  / Declination (EPOCH)',
     + 'RPFITS  = ''0.0                 '' / RPFITS version number',
     + 'GROUPS  =                    T  / Data structured in groups',
     + 'PCOUNT  =                   11  / No. of random parameters',
     + 'GCOUNT  =                    ?  / No. of groups'/

      data (m(i), i = 16,25) /
     + 'BUNIT   = ''JY                ''  / Unit of flux',
     + 'BLANK   =               -32768  / Value of blank pixel',
     + 'OBJECT  = ''                  ''  / Source Name',
     + 'INSTRUME= ''DUMMY             ''  / Instrument',
     + 'CAL     = ''NONE              ''  / Cal applied',
     + 'EPOCH   = ''J2000             ''  / Epoch of RA & Dec.',
     + 'OBSERVER= ''                  ''  /',
     + 'DATE    = ''                  ''  / UT Date data written',
     + 'DATE-OBS= ''                  ''  / UT Date data generated',
     + 'HISTORY AT''                  ''  /'/


      data (m(i), i = 26,37) /
     + 'CTYPE2  = ''COMPLEX''             / Fringe visibilities',
     + 'CRPIX2  =                  1.0  /',
     + 'CRVAL2  =                  1.0  /',
     + 'CDELT2  =                  1.0  /',

     + 'CTYPE3  = ''STOKES''              /',
     + 'CRPIX3  =                    1  /',
     + 'CRVAL3  =                    1  /',
     + 'CDELT3  =                    1  /',

     + 'CTYPE4  = ''FREQ     ''           / Frequency in Hz',
     + 'CRPIX4  =                    ?  / Ref. pixel= centre channel',
     + 'CRVAL4  =                    ?  / Freq. (Hz) of ref. pixel',
     + 'CDELT4  =                  1.0  / Frequency interval (Hz)'/

      data (m(i), i = 38,45) /
     + 'CTYPE5  = ''RA''                  / Right Ascension in radians',
     + 'CRPIX5  =                    1  /',
     + 'CRVAL5  =                  0.0  /',
     + 'CDELT5  =                  1.0  /',

     + 'CTYPE6  = ''DEC''                 / Declination in radians',
     + 'CRPIX6  =                    1  /',
     + 'CRVAL6  =                    ?  /',
     + 'CDELT6  =                  1.0  /'/

      data (m(i), i = 46,56) /
     + 'PTYPE1  = ''UU  ''                / U coordinate in metres',
     + 'PTYPE2  = ''VV  ''                / V coordinate in metres',
     + 'PTYPE3  = ''WW  ''                / W coordinate in metres',
     + 'PTYPE4  = ''BASELINE''            / =aerial(j)*256+aerial(i)',
     + 'PTYPE5  = ''UT   ''               / UT in seconds',
     + 'PTYPE6  = ''FLAG ''               / Data flag',
     + 'PTYPE7  = ''BIN  ''               / Pulsar bin no.',
     + 'PTYPE8  = ''IF_NO''               / IF no (index to IF table)',
     + 'PTYPE9  = ''SOURCENO''            / SRC no (index to SU table)',
     + 'PTYPE10 = ''INTBAS''              / Integration time (secs)',
     + 'VELREF  =                    0  / Velocity reference system'/

      data (m(i), i = 57,66) /
     + 'RESTFREQ=                  0.0  / Line rest frequency in Hz',
     + 'ALTRVAL =                  0.0  / Alternate reference value',
     + 'ALTRPIX =                  1.0  / Alternate reference pixel',
     + 'INTIME  =                    0  / Integration time in seconds',
     + 'HISTORY = ''?                   '' /',
     + 'TABLES  =                    1  / May be tables',
     + 'DEFEAT  =                    0  / Ephemeris defeat switch',
     + 'UTCMTAI =                    0  / UTC-TAI in seconds',
     + 'DJMREFP =                    0  / Param from USNO circular',
     + 'DJMREFT =                    0  / Param from USNO circular'/
      data (m(i), i = 67,70) /
     + 'VERSION = ''                    '' / Set by calling program ',
     + 'PMRA    =                    0  / Proper motion RA (sect/day)',
     + 'PMDEC   =                    0  / Proper motion DEC (asec/day)',
     + 'PMEPOCH =                    0  / Ref epoch for proper motion'/

C---------------------DECIDE ON FUNCTION--------------------------------

      instat=jstat
      if (jstat.eq.-3) go to 100
      if (jstat.eq.-2) go to 100
      if (jstat.eq.-1) go to 100
      if (jstat.eq.0) go to 2000
      if (jstat.eq.1) go to 6000
      if (jstat.eq.2) go to 1000
      if (jstat.eq.3) go to 6000
      write (6, *)
     +   ' Illegal value of jstat in routine RPFITSOUT is ', jstat
      RETURN

C------------------------WRITE HEADER ----------------------------------

C     Only this first bit differs for file or scan headers
  100 if (jstat.eq.-1) then
         scan_head = .true.
         jstat = 0


C        If an incomplete buffer remains fill it with reserved operands
C        for easy detection then write it out.
         if (bufptr.gt.1) then
            do i = bufptr, 640
               call I4VAX (illegal, i_buff(i))
            end do

            rp_iostat = AT_WRITE (lun, buffer, length)
            if (rp_iostat.ne.0) then
               jstat = -1
               write (6, *) 'Cannot empty buffer'
               RETURN
            end if
         end if
      else
         if (open) then
             write (6, *) 'File is already open'
             jstat = -1
             RETURN
         end if
         scan_head = .false.
         jstat = 0
         rp_iostat = AT_CREATE (file, async, 0, lun)
         if (rp_iostat.ne.0) then
             jstat = -1
             write (6, *) 'Cannot open file'
             RETURN
         end if

         nscan = 0

      end if

      if (instat.eq.-3) RETURN

C-------------------set up stokes parameters----------------------------

C     First see if how many words are to be written
      if (data_format .eq. 1) then
         n_words = 1
         write_wt = .false.
      else if (data_format .eq. 2 ) then
         n_words = 2
         write_wt = .false.
      else if (data_format .eq. 3 ) then
         n_words = 3
         write_wt = .true.
      else
         write (6, *) 'rpfitsout:data_format must be 1, 2 or 3'
         jstat = -1
         RETURN
      end if
c
c This section really needs to be re-done sometime to handle data with
c non-standard stokes order: JER.
c
      if (feed_type(1,1)(1:1).eq.'I' .or. feed_type(1,1).eq.' ') then
         crval3 = 1
         cdelt3 = 1
      else if (feed_type(1,1).eq.'R' .or. feed_type(1,1).eq.'L') then
         crval3 = -1
         cdelt3 = -1
      else if (feed_type(1,1).eq.'X' .or. feed_type(1,1).eq.'Y') then
         crval3 = -5
         cdelt3 = -1
      else
         crval3 = 5
         cdelt3 = 1
      end if


C-------------------SET UP HEADER PARAMETERS----------------------------

C     Get UTC date.
      call datfit (' ', utdate, jstat)

C     Create current UTC date in the form yyyy-mm-dd
      if (datobs(1:2).eq.'  ') then
         if (utdate.eq.' ') then
            jstat = -1
            datobs = ' '
            write (6, *) 'Failed to get current date for header.'
            RETURN
         else
            datobs = utdate
         endif
      end if

C------------ SORT OUT OBSOLETE HEADER QUANTITIES IF NECESSARY ---------
      n_if = MAX (1, n_if)
      if (if_freq(1).eq.0.) if_freq(1)  = freq
      if (if_bw(1).eq.0.)   if_bw(1)    = nfreq*dfreq
      if (if_nfreq(1).eq.0) if_nfreq(1) = nfreq
      if (if_nstok(1).eq.0) if_nstok(1) = nstok
      if (if_ref(1).eq.0) if_ref(1) = (nfreq+1)/2
      if (if_simul(1) .eq.0) if_simul(1) = 1
      if (if_chain(1) .eq.0) if_chain(1) = 1
      freq  = if_freq(1)
      nfreq = if_nfreq(1)
      if (dfreq.eq.0.) dfreq = if_bw(1)/if_nfreq(1)
         nstok = if_nstok(1)
         crpix4 = if_ref(1)

         n_su = MAX (1, n_su)
         if (object.eq.' ') then
           object = su_name(1)
         else if (su_name(1).eq.' ') then
           su_name(1) = object
         end if

         if (ra.eq.0d0 .and. dec.eq.0d0) then
           ra  = su_ra(1)
           dec = su_dec(1)
         else if (su_ra(1).eq.0d0 .and. su_dec(1).eq.0d0) then
           su_ra(1)  = ra
           su_dec(1) = dec
         end if

C---------------------WRITE FILE HEADER---------------------------------


         do i = 1, actualdim
            key = m(i)(1:8)
            if (key.EQ.'SCANS ') then
               if (scan_head) then
                  write (m(i)(11:30), '(i20)') 0
               else
                  write (m(i)(11:30), '(i20)') -1
               end if
            end if

            if (key(1:5).eq.'NAXIS') then
               if (key.EQ.'NAXIS2') then
                  write (m(i)(11:30), '(i20)') n_words
               else if (key.EQ.'NAXIS3') then
                  write (m(i)(11:30), '(i20)') nstok
               else if (key.EQ.'NAXIS4') then
                  write (m(i)(11:30), '(i20)') nfreq
               end if
            else if (key(1:5).eq.'CRVAL') then
               if (key.EQ.'CRVAL3') then
                  write (m(i)(11:30), '(i20)') crval3
               else if (key.EQ.'CRVAL4') then
                  write (m(i)(11:30), '(g20.12)') freq
                  call RJUSTY(m(i)(11:30))
               else if (key.EQ.'CRVAL5') then
                  if (ra.lt.0d0) ra = ra + d2pi
                  write (m(i)(11:30), '(g20.12)') ra
                  call RJUSTY(m(i)(11:30))
               else if (key.EQ.'CRVAL6') then
                  write (m(i)(11:30), '(g20.12)') dec
                  call RJUSTY(m(i)(11:30))
               end if
            else if (key(1:5).eq.'CDELT') then
               if (key.EQ.'CDELT3') then
                  write (m(i)(11:30), '(i20)') cdelt3
               else if (key.EQ.'CDELT4') then
                  write (m(i)(11:30), '(g20.12)') dfreq
                  call RJUSTY(m(i)(11:30))
               end if
            else if (key.EQ.'CRPIX4') then
               write (m(i)(11:30), '(g20.5)') crpix4
            else if (key.EQ.'GCOUNT') then
               write (m(i)(11:30), '(i20)') ncount
            else if (key.EQ.'INTIME') then
               write (m(i)(11:30), '(i20)') intime
            else if (key.EQ.'BUNIT   ') then
               call LJUSTY(bunit)
               write(m(i)(12:29),'(a16,2x)') bunit
            else if (key.EQ.'OBJECT  ') then
               call LJUSTY(object)
               write(m(i)(12:29),'(a8,10x)') object
            else if (key.EQ.'INSTRUME') then
               call LJUSTY(instrument)
               write(m(i)(12:29),'(a16,2x)') instrument
            else if (key.EQ.'CAL     ') then
               call LJUSTY(cal)
               write(m(i)(12:29),'(a16,2x)') cal
            else if (key.EQ.'OBSERVER') then
               call LJUSTY(rp_observer)
               write(m(i)(12:29),'(a16,2x)') rp_observer
            else if (key.EQ.'RPFITS  ') then
               call LJUSTY(rpfitsversion)
               write(m(i)(12:31),'(a20)') rpfitsversion
            else if (key.EQ.'VERSION ') then
               call LJUSTY(version)
               write(m(i)(12:31),'(a20)') version
            else if (key.EQ.'DATE    ') then
               m(i)(12:23) = utdate
            else if (key.EQ.'DATE-OBS') then
               m(i)(12:23) = datobs
            else if (m(i)(1:9).EQ.'HISTORY =') then
               m(i)(12:31) = 'RPFITSOUT ' // datobs
            else if (key.EQ.'VELREF  ') then
               write (m(i)(11:30), '(i20)') ivelref
            else if (key.EQ.'RESTFREQ') then
               write (m(i)(11:30), '(g20.12)') rfreq
               call RJUSTY(m(i)(11:30))
            else if (key.EQ.'ALTRVAL ') then
               write (m(i)(11:30), '(g20.12)') vel1
               call RJUSTY(m(i)(11:30))
            else if (key.EQ.'EPOCH   ') then
               m(i)(12:19) = coord
            else if (key.EQ.'DEFEAT  ') then
               write (m(i)(11:30), '(i20)') rp_defeat
            else if (key.EQ.'UTCMTAI ') then
               write (m(i)(11:30), '(g20.12)') rp_utcmtai
               call RJUSTY(m(i)(11:30))
            else if (key.EQ.'DJMREFP ') then
               write (m(i)(11:30), '(g20.12)') rp_djmrefp
               call RJUSTY(m(i)(11:30))
            else if (key.EQ.'DJMREFT ') then
               write (m(i)(11:30), '(g20.12)') rp_djmreft
               call RJUSTY(m(i)(11:30))
            else if (key.EQ.'PMRA    ') then
               write (m(i)(11:30), '(g20.12)') pm_ra
               call RJUSTY(m(i)(11:30))
            else if (key.EQ.'PMDEC   ') then
               write (m(i)(11:30), '(g20.12)') pm_dec
               call RJUSTY(m(i)(11:30))
            else if (key.EQ.'PMEPOCH ') then
               write (m(i)(11:30), '(g20.12)') pm_epoch
               call RJUSTY(m(i)(11:30))
            end if
         end do
         i = actualdim

C        Write antenna cards (coordinates in metres), and met stuff
C        for PTI data.

         if((Index(instrument,'PTI').gt.0) .and. (x(1).ne.0.0)) then
             do k = 1, nant
                 i = 4*(k-1) + 1 + actualdim
                 write (m(i), 900) k, sta(k)(1:3), x(k), y(k), z(k)
900              format ('ANTENNA N=',I1,1x,a3,
     +             ' X=',g17.10,' Y=',g17.10,' Z=',g17.10)
                 write (m(i+1), 910) 'PRESS', k, ' =  ', rp_pressure(k)
                 write (m(i+2), 910) 'TEMPE', k, ' =  ', rp_temp(k)
                 write (m(i+3), 910) 'HUMID', k, ' =  ', rp_humid(k)
 910             format (a5,i2,a3,g20.6)
             end do
             i = actualdim + 4*nant
         end if

C        Write ephemeris parameters
         do k = 1, 12
C        Added = sign                             hm 19/6/90
            write (m(i+k), 910) 'EPHEM', k, ' = ', rp_c(k)
            call RJUSTY(m(i+k)(11:30))
         end do
         i = i + 12

C        Write user-defined cards
         do icard = 1, ncard
            write (m(i+icard), '(a)') card(icard)
         end do
         i = i + ncard

C        Write out tables
         if (n_if.ge.1) call WRITE_IF_TABLE (i, m)
         if (n_su.ge.1) call WRITE_SU_TABLE (i, m)
         if (n_fg.ge.1) call WRITE_FG_TABLE (i, m)
         if (n_mt.ge.1) call WRITE_MT_TABLE (i, m)
         if (n_cu.ge.1) call WRITE_CU_TABLE (i, m)
         if (nant.ge.1) call WRITE_AN_TABLE (i, m)

C        Write it all out
         newdim = i + 1
         write (m(newdim), '(a)') 'END'

         do i = 1, (newdim-1)/32+1
            do j = 1, 32
               mout(j) = m(j + 32*(i-1))
            end do
            read (mout, '(32(20a4,:,/))') (ichr(j), j=1,640)
            rp_iostat = AT_WRITE (lun, chr, length)
            if (rp_iostat.ne.0) then
               jstat = -1
               write (6, *) 'Cannot write header'
               RETURN
            end if
         end do

C        tidy up ready for next time
         do icard = actualdim + 1, newdim
            m(icard) = ' '
         end do


         jstat = 0
         bufptr = 1
         nscan = nscan + 1
         n_if = MAX (n_if, 1)
         RETURN

C--------------------------- WRITE FG TABLE ----------------------------

 1000 continue

C     Fill the remainder of the buffer with reserved operands for easy
C     detection
      if (bufptr.gt.1) then
         do i = bufptr, 640
            call I4VAX (illegal, i_buff(i))
         end do

C     Flush buffer
         rp_iostat = AT_WRITE (lun, buffer, length)
         if (rp_iostat.ne.0) then
            jstat = -1
            write (6, *) 'Cannot empty buffer'
            RETURN
         end if
      end if
      bufptr = 1

C     Write table into buffer
      newdim = 0
      if (n_fg.ge.1) call write_fg_table (newdim, m)

C     flush buffer
      do i = 1, (newdim-1)/32+1
         do j = 1, 32
            mout(j) = m(j + 32*(i-1))
         end do
         read (mout, '(32(20a4,:,/))') (ichr(j), j=1,640)
         rp_iostat = AT_WRITE (lun, chr, length)
         if (rp_iostat.ne.0) then
            jstat = -1
            write (6, *) 'Cannot write FG table'
            RETURN
         end if
      end do
      jstat = 0
      RETURN

C------------------------WRITE DATA TO FITS FILE------------------------

 2000 continue

C     THE FOLLOWING POINTERS AND COUNTERS ARE USED HERE:
C     GRPLENGTH     No. of visibilities in group
C     GRPPTR        Pointer to next visibility in group to be written
C     BUFPTR        Pointer to next word to be written to current buffer
C     BUFLEFT       No. of words still to be written to current buffer

C-----------------------------------------------------------------------
C     Note: data are written in blocks of 5 records = 640(4byte) words

C     determine grplength
      if (baseline.eq.-1) then
         grplength = sc_ant*sc_if*sc_q
      else if (if_no.gt.1) then
         grplength = if_nfreq(if_no)*if_nstok(if_no)
      else
         grplength = nstok*nfreq
      end if

      grpptr = 1


C     WRITE PARAMETERS TO FILE, FORMAT OF RPFITS IS:
C----------- VIS data -------------      ----------- SYSCAL data -------
C     (baseline > 0)                         (baseline = -1)
C     param 1=u in m                         0.0
C     param 2=v in m                         0.0
C     param 3=w in m                         0.0
C     param 4=baseline number                -1.0
C     param 5=UT in seconds                  sc_ut: UT in seconds
C     param 6= flag (if present)             sc_ant
C     param 7= bin  (if present)             sc_if
C     param 8=if_no (if present)             sc_q
C     param 9=sourceno (if present)          sc_srcno
C     param 10=intbase integration time      0.0
C     param 11=data_format                   0

      bufleft = 641-bufptr
      if (baseline.eq.-1) then
         if (bufleft.ge.pcount) then
            call R4VAX (0.0, buffer(bufptr))
            call R4VAX (0.0, buffer(bufptr+1))
            call R4VAX (0.0, buffer(bufptr+2))
            call R4VAX (FLOAT(baseline), buffer(bufptr+3))
            call R4VAX (sc_ut, buffer(bufptr+4))
            call I4VAX (sc_ant, i_buff(bufptr+5))
            call I4VAX (sc_if, i_buff(bufptr+6))
            call I4VAX (sc_q, i_buff(bufptr+7))
            call I4VAX (sc_srcno, i_buff(bufptr+8))
            call R4VAX (0.0, buffer(bufptr+9))
            call I4VAX (0, i_buff(bufptr+10))
            bufptr = bufptr + pcount
            ut = sc_ut
         else
            call R4VAX (0.0, grphdr(1))
            call R4VAX (0.0, grphdr(2))
            call R4VAX (0.0, grphdr(3))
            call R4VAX (FLOAT(baseline), grphdr(4))
            call R4VAX (sc_ut, grphdr(5))
            call I4VAX (sc_ant, i_grphdr(6))
            call I4VAX (sc_if, i_grphdr(7))
            call I4VAX (sc_q, i_grphdr(8))
            call I4VAX (sc_srcno, i_grphdr(9))
            call R4VAX (0.0, grphdr(10))
            call I4VAX (0, i_grphdr(11))
            ut = sc_ut

            do i = 1, bufleft
               i_buff(bufptr+i-1) = i_grphdr(i)
            end do

            rp_iostat = AT_WRITE (lun, buffer, length)
            if (rp_iostat.ne.0) then
               jstat = -1
               write (6, *) 'Cannot write data (1)'
               RETURN
            end if
            bufptr = pcount - bufleft
            do i = 1, bufptr
               i_buff(i) = i_grphdr(i+bufleft)
            end do
            bufptr = bufptr+1
         end if
      else
         if (bufleft.ge.pcount) then
            call R4VAX (u, buffer(bufptr))
            call R4VAX (v, buffer(bufptr+1))
            call R4VAX (w, buffer(bufptr+2))
            call R4VAX (FLOAT(baseline), buffer(bufptr+3))
            call R4VAX (ut, buffer(bufptr+4))
            call I4VAX (flag, i_buff(bufptr+5))
            call I4VAX (bin, i_buff(bufptr+6))
            call I4VAX (if_no, i_buff(bufptr+7))
            call I4VAX (sourceno, i_buff(bufptr+8))
            call R4VAX (intbase, buffer(bufptr+9))
            call I4VAX (data_format, i_buff(bufptr+10))

            bufptr = bufptr + pcount
         else
            call R4VAX (u, grphdr(1))
            call R4VAX (v, grphdr(2))
            call R4VAX (w, grphdr(3))
            call R4VAX (FLOAT(baseline), grphdr(4))
            call R4VAX (ut, grphdr(5))
            call I4VAX (flag, i_grphdr(6))
            call I4VAX (bin, i_grphdr(7))
            call I4VAX (if_no, i_grphdr(8))
            call I4VAX (sourceno, i_grphdr(9))
            call R4VAX (intbase, grphdr(10))
            call I4VAX (data_format, i_grphdr(11))

            do i = 1, bufleft
               i_buff(bufptr+i-1) = i_grphdr(i)
            end do
            rp_iostat = AT_WRITE (lun, buffer, length)
            if (rp_iostat.ne.0) then
               jstat = -1
               write (6, *) 'Cannot write data (1)'
               RETURN
            end if
            bufptr = pcount - bufleft
            do i = 1, bufptr
               i_buff(i) = i_grphdr(i+bufleft)
            end do
            bufptr = bufptr + 1
         end if
      end if

      if (bufptr.eq.0.or.bufptr.eq.641) then
         rp_iostat = AT_WRITE (lun, buffer, length)
         if (rp_iostat.ne.0) then
            jstat = -1
            write (6, *) 'Cannot write data (2)'
            RETURN
         end if
         bufptr = 1
      end if

C ---------------------- WRITE VIS DATA TO FILE ------------------------
      if (baseline.eq.-1) go to 4000
C        FORMAT FROM RPFITS IS:
C        data_format  3        2        1
C        word 1 =   Re(vis)   Re(vis)   Real(vis)
C        word 2 =   Imag(vis) Imag(vis) -
C        word 3 =   weight    -         -

3500  continue
C     First see if how many words are to be written
      if (data_format .eq. 1) then
         n_words = 1
         write_wt = .false.
      else if (data_format .eq. 2 ) then
         n_words = 2
         write_wt = .false.
      else if (data_format .eq. 3 ) then
         n_words = 3
         write_wt = .true.
      else
         write (6, *) 'rpfitsout:data_format must be 1, 2 or 3'
         jstat = -1
         RETURN
      end if

C      type *,'data_format is ',data_format
C      type *,'n_words is ',n_words
C      type *,'write_wt is ',write_wt
      bufleft = 641-bufptr
      if (bufleft.ge.(n_words*(grplength-grpptr+1))) then

C        If entire group can be put in existing buffer then do so
            do i = grpptr, grplength
               if (n_words.eq.1) then
                  call R4VAX (REAL(vis(i)), buffer(bufptr))
               else
                  call R4VAX (REAL(vis(i)), buffer(bufptr))
                  call R4VAX (AIMAG(vis(i)), buffer(bufptr+1))
               end if
               if (write_wt) call R4VAX (weight(i), buffer(bufptr+2))
               bufptr = bufptr+n_words
            end do
            jstat = 0
            RETURN

         else

C        Otherwise things are a bit more complicated, first write
C        complete visibilities to old buffer.
            bufleft3 = bufleft/n_words
            do i = 1, bufleft3
               call R4VAX (REAL(vis(grpptr+i-1)), buffer(bufptr))
               if (n_words.gt.1)
     +            call R4VAX (AIMAG(vis(grpptr+i-1)), buffer(bufptr+1))

               if (write_wt)
     +            call R4VAX (weight(grpptr+i-1), buffer(bufptr+2))
               bufptr = bufptr+n_words
            end do
            grpptr = grpptr+bufleft3

C        Now write the fraction of a visibility left into old buffer
            bufleft = bufleft-n_words*bufleft3

            if (bufleft.eq.1) then
               call R4VAX (REAL(vis(grpptr)), buffer(640))
            else if (bufleft.eq.2 .and. n_words.eq.3) then
               call R4VAX (REAL(vis(grpptr)), buffer(639))
               call R4VAX (AIMAG(vis(grpptr)), buffer(640))
            end if

C        Start a new buffer
            rp_iostat = AT_WRITE (lun, buffer, length)
            if (rp_iostat.ne.0) then
               jstat = -1
               write (6, *) 'Cannot write data (3)'
               RETURN
            end if

C        Fill any incomplete visibility, and reset BUFPTR
            if (bufleft.eq.0) then
               bufptr = 1
            else if (bufleft.eq.1) then
               call R4VAX (AIMAG(vis(grpptr)), buffer(1))
               if (write_wt) call R4VAX (weight(grpptr), buffer(2))
               grpptr = grpptr+1
               bufptr = n_words
            else if (bufleft.eq.2 .and. write_wt) then
               call R4VAX (weight(grpptr), buffer(1))
               grpptr = grpptr+1
               bufptr = n_words-1
            end if

C        Return to write out the rest of the group
         end if
      go to 3500


C---------------------- WRITE SYSCAL DATA TO FILE ----------------------
4000  continue

      bufleft = 641 - bufptr
      if (bufleft.ge.(grplength-grpptr+1)) then
C        Easy, the group fits into buffer.
         do i = grpptr, grplength
            call R4VAX (sc_buf(i), buffer(bufptr))
            bufptr = bufptr + 1
         end do

         jstat = 0
         RETURN

      else
C        Fill the buffer.
         do i = 1, bufleft
            call R4VAX (sc_buf(grpptr+i-1), buffer(bufptr))
            bufptr = bufptr + 1
         end do

         grpptr = grpptr + bufleft

C        Write the buffer out.
         rp_iostat = AT_WRITE (lun, buffer, length)
         if (rp_iostat.ne.0) then
            jstat = -1
            write (6, *) 'Cannot write data (3)'
            RETURN
         end if

C        Start a new buffer.
         bufptr = 1
      end if

C     Return to write out the rest of the group.
      go to 4000


C--------------------------- CLOSE FILE --------------------------------

6000  continue

      if (bufptr.gt.1) then
C        Fill buffer with reserved operands.
         do i = bufptr, 640
            call I4VAX (illegal, i_buff(i))
         end do

C        Write out buffer.
         rp_iostat = AT_WRITE (lun, buffer, length)
         if (rp_iostat.ne.0) then
            jstat = -1
            write (6, *) 'Cannot empty buffer'
            RETURN
         end if
      end if

C     Reset buffer pointer.
      bufptr = 1

C     Close file
      rp_iostat = AT_CLOSE (lun)
      if (rp_iostat.ne.0) then
         jstat = -1
         write (6, *) 'Cannot close file'
         RETURN
      end if

      if (jstat.eq.1) then
          jstat = 0
          open  = .false.
      else
C        REOPEN file.
         rp_iostat = AT_REOPEN_WRITE (file, lun)
         if (rp_iostat.ne.0) then
            jstat = -1
            write (6, *) 'Cannot reopen file'
         else
            jstat = 0
         end if
      end if
      return

      end

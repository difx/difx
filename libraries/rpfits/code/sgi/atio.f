C-----------------------------------------------------------------------
C   atio.f: Input/output routines for RPFITS on an SGI machine.
C-----------------------------------------------------------------------
C
C   Notes:
C     1) The RPFITS routines are no longer directly supported for SGI
C        and the SGI version of atio.f is simply a copy of the sun4sol
C        routines.
C
C        Refer to the notes in the prologue of the sun4sol version of
C        atio.f for more information.
C
C   $Id: atio.f,v 1.13 2007/07/16 01:11:50 cal103 Exp $
C-----------------------------------------------------------------------



      integer function AT_CREATE (fname, async, initsz, lun)
C-----------------------------------------------------------------------
      byte      bufsav(2560)
      logical   async, islabelled, reread
      integer   initsz, irec(10:99), lenrec(10:99), lun, lunsav
      integer   TOPEN, istat, GETLUN, lfname
      character fname*(*)

      common /atio/ lenrec, irec, reread, lunsav, bufsav
      save /atio/
C-----------------------------------------------------------------------
      AT_CREATE = 0
      if (fname(1:5).eq.'/dev/') then
         lfname = index(fname,' ')
         if (lfname.eq.0) lfname = len(fname)
         istat = GETLUN(.true., lun)
         islabelled  = .true.
         lenrec(lun) = 2560
         AT_CREATE = TOPEN(lun, fname(:lfname), islabelled)

      else
         istat = GETLUN(.false., lun)
         lenrec(lun) = 2560
         open (lun, file=fname, status='new', access='direct',
     +      form='unformatted', recl=lenrec(lun), iostat=AT_CREATE)

         irec(lun) = 1
      endif
      reread = .false.

      return
      end



      integer function AT_REOPEN_WRITE (fname, lun)
C-----------------------------------------------------------------------
C     REOPEN file - on disk only.
C-----------------------------------------------------------------------
      byte      bufsav(2560)
      logical   reread
      integer   irec(10:99), lenrec(10:99), lun, lunsav
      character fname*(*)

      common /atio/ lenrec, irec, reread, lunsav, bufsav
      save /atio/
C-----------------------------------------------------------------------
      AT_REOPEN_WRITE = 0

      open (lun, file=fname, status='old', access='direct',
     +      form='unformatted', recl=lenrec(lun),
     +      iostat=AT_REOPEN_WRITE)

      return
      end



      integer function AT_OPEN_READ (fname, async, lun)
C-----------------------------------------------------------------------
C     "READONLY" is non-standard. Had to remove it.
C-----------------------------------------------------------------------
      byte      bufsav(2560)
      logical   async, islabelled, reread
      integer   irec(10:99), lenrec(10:99), lun, lunsav, TOPEN
      integer   TSKIPF, istat, GETLUN, lfname
      character fname*(*)

      common /atio/ lenrec, irec, reread, lunsav, bufsav
      save /atio/
C-----------------------------------------------------------------------
      AT_OPEN_READ = 0
      if (fname(1:5).eq.'/dev/') then
         istat = GETLUN(.true., lun)
         islabelled  = .true.
         lenrec(lun) = 2560
         lfname = index(fname,' ')
         if (lfname.eq.0) lfname = len(fname)
         AT_OPEN_READ = TOPEN(lun, fname(:lfname), islabelled)
         if (AT_OPEN_READ.eq.0) then
C           Skip header file.
            AT_OPEN_READ = TSKIPF(lun, 1, 0)
            if (AT_OPEN_READ.lt.0) then
               type *,'AT_OPEN_READ:Error skipping header: ',
     +            AT_OPEN_READ
            endif
         endif
      else
         istat = GETLUN(.false., lun)
         lenrec(lun) = 2560
         open (lun, file=fname, status='old', access='direct',
     +      form='unformatted', recl=lenrec(lun), iostat=AT_OPEN_READ)
         if (AT_OPEN_READ.ne.0) then
            lenrec(lun) = 512
            open (lun, file=fname, status='old', access='direct',
     +         form='unformatted', recl=lenrec(lun),
     +         iostat=AT_OPEN_READ)
         end if

         irec(lun) = 1
      end if
      reread = .false.

      return
      end



      integer function AT_WRITE (lun, buffer, nbytes)
C-----------------------------------------------------------------------
      byte      buffer(2560), bufsav(2560), bbuffer(2560)
      logical   ISTAPE, reread
      integer   irec(10:99), lenrec(10:99), lun, lunsav, i, TWRITE
      integer   nbytes
      character cbuffer*2560
      equivalence (bbuffer, cbuffer)

      common /atio/ lenrec, irec, reread, lunsav, bufsav
      save /atio/
C-----------------------------------------------------------------------
      AT_WRITE = 0

      if (ISTAPE(lun)) then
         do i = 1, nbytes
            bbuffer(i) = buffer(i)
         end do
         AT_WRITE = TWRITE(lun, cbuffer(1:nbytes))
      else
         write (lun, rec=irec(lun), iostat=AT_WRITE) buffer
         irec(lun) = irec(lun) + 1
      endif

      return
      end



      integer function AT_READ (lun, buffer)
C-----------------------------------------------------------------------
      byte      buffer(2560), bufsav(2560), bbuffer(2560)
      logical   ISTAPE, reread
      integer   irec(10:99), j, lenrec(10:99), lun, lunsav, i, TREAD
      integer   TSKIPF
      character cbuffer*2560
      equivalence (cbuffer, bbuffer)

      common /atio/ lenrec, irec, reread, lunsav, bufsav
      save /atio/
C-----------------------------------------------------------------------
C     Read the next record or restore the last.
      if (reread) then
C        Check consistency of the input files.
         if (lun.ne.lunsav) then
            AT_READ = 999
            go to 999
         end if

C        Copy the buffer saved by AT_UNREAD to the input buffer.
         do 10 j = 1, 2560
            buffer(j) = bufsav(j)
 10      continue
         reread = .false.
         AT_READ = 0

      else if (lenrec(lun).eq.2560) then
C        Get the next 2560-byte record.
         if (ISTAPE(lun)) then
            cbuffer = ' '
            AT_READ = TREAD(lun, cbuffer)
C           returns 0 if EOF
            if (AT_READ.eq.0) then
               AT_READ = TSKIPF(lun, 1, 0)
               AT_READ = TSKIPF(lun, 1, 0)
               if (AT_READ.ne.0) then
                  type *,
     +            'AT_READ:Failed to skip EOF+trailer. Err ', AT_READ
               endif
               AT_READ = -1
C           returns byte count if OK
            else if (AT_READ.gt.0) then
               AT_READ = 0
               do i = 1,2560
                  buffer(i) = bbuffer(i)
               end do
            endif
         else
            read (lun, rec=irec(lun), iostat=AT_READ) buffer
C           Increment record number only if read OK
            if (AT_READ.eq.0) then
               irec(lun) = irec(lun) + 1
            else
C              Not sure about this!
               AT_READ = -1
            end if
         end if

      else
C        Get the next five 512-byte records.
         if (ISTAPE(lun)) then
            AT_READ = TREAD(lun, cbuffer(1:512))
            AT_READ = TREAD(lun, cbuffer(513:1024))
            AT_READ = TREAD(lun, cbuffer(1025:1536))
            AT_READ = TREAD(lun, cbuffer(1537:2048))
            AT_READ = TREAD(lun, cbuffer(2049:2560))
            if (AT_READ.eq.0) then
C              clear EOF 'flag' and skip over the EOF+trailer
               AT_READ = TSKIPF(lun, 1, 0)
               AT_READ = TSKIPF(lun, 1, 0)
               if (AT_READ.ne.0) then
                  type *,
     +            'AT_READ:Failed to skip EOF+trailer.Err ', AT_READ
               endif
               AT_READ = -1
            else if (AT_READ.gt.0) then
               AT_READ = 0
               do i = 1,2560
                  buffer(i) = bbuffer(i)
               end do
            endif
         else
            read (lun, rec=irec(lun), iostat=AT_READ)
     +         (buffer(j), j=1,512)
            if (AT_READ.eq.0) read (lun, rec=irec(lun)+1,
     +         iostat=AT_READ) (buffer(j), j=513,1024)
            if (AT_READ.eq.0) read (lun, rec=irec(lun)+2,
     +         iostat=AT_READ) (buffer(j), j=1025,1536)
            if (AT_READ.eq.0) read (lun, rec=irec(lun)+3,
     +         iostat=AT_READ) (buffer(j), j=1537,2048)
            if (AT_READ.eq.0) read (lun, rec=irec(lun)+4,
     +         iostat=AT_READ) (buffer(j), j=2049,2560)
            irec(lun) = irec(lun) + 5
         end if
      end if

 999  continue
      return
      end



      integer function AT_SKIP_EOF (lun)
C-----------------------------------------------------------------------
C     Returns -1 if successfully skipped to EOF, otherwise error.
C-----------------------------------------------------------------------
      byte      buffer(2560), bufsav(2560)
      logical   ISTAPE, reread
      integer   irec(10:99), lenrec(10:99), lun, lunsav, TSKIPF

      common /atio/ lenrec, irec, reread, lunsav, bufsav
      save /atio/
C-----------------------------------------------------------------------
      AT_SKIP_EOF = 0
      if (ISTAPE(lun)) then
         AT_SKIP_EOF = TSKIPF(lun, 1, 0)
         AT_SKIP_EOF = TSKIPF(lun, 1, 0)
         if (AT_SKIP_EOF.ne.0) then
            type *,
     +      'AT_SKIP_EOF:Failed to skip EOF+trailer. Err ', AT_SKIP_EOF
            AT_SKIP_EOF = -2
         else
            AT_SKIP_EOF = -1
         endif
      else
         do while (AT_SKIP_EOF.ne.-1)
            read (lun, rec=irec(lun), iostat=AT_SKIP_EOF) buffer
            irec(lun) = irec(lun) + 1
         end do
      end if
      reread = .false.

 999  continue
      return
      end



      integer function AT_UNREAD (lun, buffer)
C-----------------------------------------------------------------------
      byte      buffer(2560), bufsav(2560)
      logical   reread
      integer   irec(10:99), j, lenrec(10:99), lun, lunsav

      common /atio/ lenrec, irec, reread, lunsav, bufsav
      save /atio/
C-----------------------------------------------------------------------
C     Save the buffer for "rereading".
      reread = .true.
      lunsav = lun
      do 10 j = 1, 2560
         bufsav(j) = buffer(j)
 10   continue

      AT_UNREAD = 0

      return
      end



      integer function AT_CLOSE (lun)
C-----------------------------------------------------------------------
      byte      bufsav(2560)
      logical   ISTAPE, reread
      integer   FREELUN, irec(10:99), istat, lenrec(10:99), lun,
     +          lunsav, TCLOSE

      common /atio/ lenrec, irec, reread, lunsav, bufsav
      save /atio/
C-----------------------------------------------------------------------
      AT_CLOSE = 0
      if (ISTAPE(lun)) then
         AT_CLOSE = TCLOSE(lun)
      else
         close (lun, iostat=AT_CLOSE)
      end if

      istat = FREELUN(lun)

      return
      end



      integer function GETLUN (istape, lun)
C-----------------------------------------------------------------------
C     Get a logical unit number.
C
C     Tape LUNs are in the range 0 to 7 and bear no relationship to
C     FORTRAN logical unit numbers.
C
C     FORTRAN logical unit numbers are returned in the range 10 to 99.
C-----------------------------------------------------------------------
      logical   isopen, istape
      integer   j, fluns(10:99), lun, tluns(0:7)

      common /lunlst/ fluns, tluns
      save /lunlst/
C-----------------------------------------------------------------------
      GETLUN = -1
      lun = -1

      if (istape) then
         do 10 j = 7, 0, -1
            if (tluns(j).eq.0) then
               lun = j
               tluns(j) = -1
               GETLUN = 0
               goto 999
            end if
 10      continue

      else
         do 20 j = 99, 10, -1
            if (fluns(j).eq.0) then
C              Has it already been opened outside RPFITS.
               inquire (unit=j, opened=isopen)
               if (isopen) go to 20

               lun = j
               fluns(j) = -1
               GETLUN = 0
               goto 999
            end if
 20      continue
      end if

 999  return
      end



      integer function FREELUN (lun)
C-----------------------------------------------------------------------
C     Free a logical unit number allocated by GETLUN.
C-----------------------------------------------------------------------
      logical   ISTAPE
      integer   lun, fluns(10:99), tluns(0:7)

      common /lunlst/ fluns, tluns
      save /lunlst/
C-----------------------------------------------------------------------
      FREELUN = 0

      if (ISTAPE(lun)) then
         tluns(lun) = 0
      else if (lun.ge.10 .and. lun.le.99) then
         fluns(lun) = 0
      else
         FREELUN = -1
      end if

      return
      end



      logical function ISTAPE (lun)
C-----------------------------------------------------------------------
C     Does a logical unit number allocated by GETLUN correspond to a
C     tape unit?
C-----------------------------------------------------------------------
      integer   lun
C-----------------------------------------------------------------------
      ISTAPE = lun.ge.0 .and. lun.le.7

      return
      end



      block data
C-----------------------------------------------------------------------
C     Initialise logical unit number lists.
C-----------------------------------------------------------------------
      integer   fluns(10:99), tluns(0:7)

      common /lunlst/ fluns, tluns
      data  fluns, tluns /90*0, 8*0/
      save /lunlst/
C-----------------------------------------------------------------------
      end

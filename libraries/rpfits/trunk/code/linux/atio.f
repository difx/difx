C-----------------------------------------------------------------------
C   atio.f: Input/output routines for RPFITS under linux.
C-----------------------------------------------------------------------
C
C   Notes:
C     1) Cloned from the sun4sol version with tape handling stripped
C        out.
C
C   $Id: atio.f,v 1.6 2007/07/16 01:11:50 cal103 Exp $
C-----------------------------------------------------------------------



      integer function AT_CREATE (fname, async, initsz, lun)
C-----------------------------------------------------------------------
      byte      bufsav(2560)
      logical   async, reread
      integer   initsz, irec(10:99), lenrec(10:99), lun, lunsav
      integer   istat, GETLUN
      character fname*(*)

      common /atio/ lenrec, irec, reread, lunsav, bufsav
      save /atio/
C-----------------------------------------------------------------------
      AT_CREATE = 0
      if (fname(1:5).eq.'/dev/') then
         AT_CREATE = 1

      else
         istat = GETLUN(lun)
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
      logical   async, reread
      integer   irec(10:99), lenrec(10:99), lun, lunsav
      integer   istat, GETLUN
      character fname*(*)

      common /atio/ lenrec, irec, reread, lunsav, bufsav
      save /atio/
C-----------------------------------------------------------------------
      AT_OPEN_READ = 0
      if (fname(1:5).eq.'/dev/') then
         AT_OPEN_READ = 1
      else
         istat = GETLUN(lun)
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
      byte      buffer(2560), bufsav(2560)
      logical   reread
      integer   irec(10:99), lenrec(10:99), lun, lunsav
      integer   nbytes

      common /atio/ lenrec, irec, reread, lunsav, bufsav
      save /atio/
C-----------------------------------------------------------------------
      AT_WRITE = 0

      write (lun, rec=irec(lun), iostat=AT_WRITE) buffer
      irec(lun) = irec(lun) + 1

      return
      end



      integer function AT_READ (lun, buffer)
C-----------------------------------------------------------------------
      byte      buffer(2560), bufsav(2560)
      logical   reread
      integer   irec(10:99), j, lenrec(10:99), lun, lunsav

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
         read (lun, rec=irec(lun), iostat=AT_READ) buffer
C        Increment record number only if read OK
         if (AT_READ.eq.0) then
            irec(lun) = irec(lun) + 1
         else
C           Not sure about this!
            AT_READ = -1
         end if

      else
C        Get the next five 512-byte records.
         read (lun, rec=irec(lun), iostat=AT_READ)
     +      (buffer(j), j=1,512)
         if (AT_READ.eq.0) read (lun, rec=irec(lun)+1,
     +      iostat=AT_READ) (buffer(j), j=513,1024)
         if (AT_READ.eq.0) read (lun, rec=irec(lun)+2,
     +      iostat=AT_READ) (buffer(j), j=1025,1536)
         if (AT_READ.eq.0) read (lun, rec=irec(lun)+3,
     +      iostat=AT_READ) (buffer(j), j=1537,2048)
         if (AT_READ.eq.0) read (lun, rec=irec(lun)+4,
     +      iostat=AT_READ) (buffer(j), j=2049,2560)
         irec(lun) = irec(lun) + 5
      end if

 999  continue
      return
      end



      integer function AT_SKIP_EOF (lun)
C-----------------------------------------------------------------------
C     Returns -1 if successfully skipped to EOF, otherwise error.
C-----------------------------------------------------------------------
      byte      buffer(2560), bufsav(2560)
      logical   reread
      integer   irec(10:99), lenrec(10:99), lun, lunsav

      common /atio/ lenrec, irec, reread, lunsav, bufsav
      save /atio/
C-----------------------------------------------------------------------
      AT_SKIP_EOF = 0
      do while (AT_SKIP_EOF.ne.-1)
         read (lun, rec=irec(lun), iostat=AT_SKIP_EOF) buffer
         irec(lun) = irec(lun) + 1
      end do
      reread = .false.

      continue
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
      logical   reread
      integer   FREELUN, irec(10:99), istat, lenrec(10:99), lun,
     +          lunsav

      common /atio/ lenrec, irec, reread, lunsav, bufsav
      save /atio/
C-----------------------------------------------------------------------
      close (lun, iostat=AT_CLOSE)
      istat = FREELUN(lun)

      return
      end



      integer function GETLUN (lun)
C-----------------------------------------------------------------------
C     Get a logical unit number.
C
C     FORTRAN logical unit numbers are returned in the range 10 to 99.
C-----------------------------------------------------------------------
      logical   isopen
      integer   j, fluns(10:99), lun

      common /lunlst/ fluns
      save /lunlst/
C-----------------------------------------------------------------------
      GETLUN = -1
      lun = -1

      do 10 j = 99, 10, -1
         if (fluns(j).eq.0) then
C           Has it already been opened outside RPFITS.
            inquire (unit=j, opened=isopen)
            if (isopen) go to 10

            lun = j
            fluns(j) = -1
            GETLUN = 0
            goto 999
         end if
 10   continue

 999  return
      end



      integer function FREELUN (lun)
C-----------------------------------------------------------------------
C     Free a logical unit number allocated by GETLUN.
C-----------------------------------------------------------------------
      integer   lun, fluns(10:99)

      common /lunlst/ fluns
      save /lunlst/
C-----------------------------------------------------------------------
      FREELUN = 0

      if (lun.ge.10 .and. lun.le.99) then
         fluns(lun) = 0
      else
         FREELUN = -1
      end if

      return
      end



      block data
C-----------------------------------------------------------------------
C     Initialise logical unit number lists.
C-----------------------------------------------------------------------
      integer   fluns(10:99)

      common /lunlst/ fluns
      data  fluns /90*0/
      save /lunlst/
C-----------------------------------------------------------------------
      end

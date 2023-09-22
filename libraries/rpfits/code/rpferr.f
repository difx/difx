      subroutine RPFERR (text)

*-----------------------------------------------------------------------
*     Report an error.
*
*     Given:
*          TEXT*(*) char  The basic error message used to construct
*                         ERRMSG.
*
*     Given via the IOSTAT common:
*          ERRLUN   int   If non-negative, write the message on ERRLUN.
*                         The message written is ' RPFITSIN: ' followed
*                         by ERRMSG (without trailing blanks).
*
*     Returned via the NAMES common:
*          ERRMSG*80
*                   char  TEXT which, if rp_iostat is non-zero, will be
*                         suffixed with ', iostat =' and the number.
*
*     Original: 2008/09/11 Mark Calabretta
*     $Id: rpferr.f,v 1.1 2008/09/11 06:36:57 cal103 Exp $
*-----------------------------------------------------------------------

      character text*(*)

      include 'rpfits.inc'

      integer   idx, NCHAR

      errmsg = text
      if (rp_iostat.ne.0) then
*        Find last non-blank.
         idx = NCHAR(errmsg)
         if (errmsg(idx:idx).ne.'.') idx = idx + 1

         if (idx.lt.80) then
           write (errmsg(idx:), 10) rp_iostat
 10        format (', iostat =', i4, '.')
         end if
      end if

      if (errlun.ge.0) then
         write (errlun, *) ' RPFITSIN: ', errmsg(:NCHAR(errmsg))
      end if

      return
      end

*     ------------------------------------------------------------------

      block data bd_iostat
      include 'rpfits.inc'
      data rp_iostat /0/
      data errlun /6/
      end

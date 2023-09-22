      subroutine ftfix( ng, gis, gib, gip, gft, gyr1, gyr2, 
     *        npt, time, pol, tscal, tcal, tsys, trec, tatm, gain, 
     *        eff, sta, band, station, bandname )
C
C     Subroutine for PLOTTS and PLOTSUM that fixes data for FT value
C     changes between what was used in the PTANAL's and what is in
C     the gain file.
C
C     PLOTTS keeps LCP and RCP gains separately and will need separate
C     calls.
C
      integer   ng, gis(*), gib(*), gip, i, ig
      real      gyr1(*), gyr2(*), gft(*)
      integer   npt, pol(*) 
      real      time(*), tscal(*), tcal(*), tsys(*), trec(*)
      real      tatm(*), gain(*), eff(*), rescale
      character sta(*)*2, band(*)*6
      character station(*)*2, bandname(*)*6
C -------------------------------------------------------------------
      do i = 1, npt
         do ig = 1, ng
            if( station(gis(ig)) .eq. sta(i) .and.
     *           bandname(gib(ig)) .eq. band(i) ) then
               if( pol(i) .eq. gip ) then
                  if( time(i) .gt. gyr1(ig) .and. 
     *                time(i) .le. gyr2(ig) ) then
                     if( abs( tscal(i) - gft(ig) ) .gt. 0.01 ) then
                        rescale = gft(ig) / tscal(i)
                        tcal(i) = tcal(i) * rescale
                        tsys(i) = tsys(i) * rescale
                        trec(i) = trec(i) * rescale
                        tatm(i) = tatm(i) * rescale
                        gain(i) = gain(i) * rescale
                        eff(i)  = eff(i)  * rescale
                        write(*,*) ' Adjusting for FT change at ', 
     *                     sta(i), ' ', band(i), ' pol', pol(i), 
     *                     ' old:', tscal(i), ' gainfile:', gft(ig) 
                        tscal(i) = gft(ig)
                     end if
                  end if
               end if
            end if
         end do
      end do
C
      return
      end

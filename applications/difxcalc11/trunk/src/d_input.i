! d_input.i
!  Include file for inputs from a difx *.calc file.
!
!   Set MaxStat to the maximum number of stations that will ever be 
!    correlated plus 1 for the geocenter. (Also see Max_Stat in cmxst.i)
      Integer*4 MaxStat
      Parameter (MaxStat = 41)
!
      CHARACTER*128 calc_file_name, Jobname, IM_file_name, calc_out_file
      CHARACTER*10  Base_mode, L_time, Atmdr, Atmwt, Near_Far
      Real*8        d_interval
      Character*6   UVW 
      Character*4   overwrite
      Integer*4     Icalc, Ijob, Verbose, I_out, IM_out, epoch2m,       &
     &              Numjobs
!
!   Interval between Calc runs (24 seconds for VLBA DiFX correlator).
!      Parameter ( d_interval = 24.D0)
      COMMON/Contrl/d_interval, Icalc, Ijob, I_out, IM_out, Verbose,    &
     &              epoch2m, calc_file_name, IM_file_name, Jobname,     &
     &              calc_out_file, Base_mode, L_time, Atmdr, Atmwt,     &
     &              Numjobs, Near_Far, overwrite, UVW
!
      COMMON/Calc_input/ Xleap_sec,                                     &
     &       JobID, NumScans, Numsrc, NumEpochs, NumPhCntr,PointingSrc, &
     &       PhCntrNum, PhCntr, ScanNum, Intrvls2min,                   &
     &       StartYr, StartMo, StartDay, StartHr, StartMin, StartSec,   &
     &       ScanStrt, ScanDur, Sites, Axis, ScanID, ci6
      Real*8 Xleap_sec
      Character*8 Sites(Maxstat)
      Integer*4 JobID, NumScans, NumEpochs, Numsrc
      Integer*4 StartYr, StartMo, StartDay, StartHr, StartMin, StartSec
      Integer*4 ScanStrt, ScanDur, ScanNum, NumPhCntr, PointingSrc,     &
     &          PhCntrNum, PhCntr(500), Intrvls2min
      Character*4 Axis(MaxStat)
      Character*10 ScanID
      Character*6  ci6 
!
!  Maximum number of lines in the near-field object ephemeris
      Integer*4 NF_row 
      Parameter (NF_row = 200)
      COMMON/NFO/ SpTag, SpPos, SpVel, SpcIF, NumSpace, NumRows,        &
     &            SpName, SpFrame, SpOffset, t_offset, NF_model,        &
     &            NF_flag, dum1
      Real*8 SpTag(NF_row,10), SpPos(NF_row,3,10), SpVel(NF_row,3,10),  &
     &       SpcIF(3), t_offset
      Integer*4 NumSpace, Numrows(10)
      Character*20 SpName(10)
      Character*8 SpOffset, NF_model
      Character*4 SpFrame
! Set default spacecraft frame to Earth Centered (geocentric) J2000.
!     Parameter (SpFrame = 'ECJ2')
      Character*2 NF_flag, dum1
!
      COMMON /NFOspline/ ySPxp, ySPyp, ySPzp, ySPxv, ySPyv, ySPzv,      &
     &        y2SPxp, y2SPyp, y2SPzp, y2SPxv, y2SPyv, y2SPzv, xc,       &
              S_spline
!
      Real*8 ySPxp(NF_row), ySPyp(NF_row), ySPzp(NF_row),               &
     &       ySPxv(NF_row), ySPyv(NF_row), ySPzv(NF_row), xc(NF_row)
      Real*8 y2SPxp(NF_row), y2SPyp(NF_row), y2SPzp(NF_row),            &
     &       y2SPxv(NF_row), y2SPyv(NF_row), y2SPzv(NF_row)
      Integer*4  S_spline 
!
      COMMON /NFOsource/ SPpxyz, SPvxyz, SPv2xyz, SPaxyz
      Real*8 SPpxyz(3), SPvxyz(3), SPv2xyz(3), SPaxyz(3)

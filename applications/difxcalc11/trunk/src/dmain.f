      PROGRAM difxcalc
      IMPLICIT None
!
!     Main Program for difxcalc, correlator version of Calc 11.
!
!     The difxcalc main serves as an executive which calls the subroutines
!     which do the real work. These routines are:
!
!     GETCL  - Get the command line inputs.
!     dSTART - Gets the site, source, and EOP apriori's.
!     dINITL - Obtains the math and physical constants, obtains additional
!              apriori's (ocean loading coefficients, ocean pole tide
!              loading coefficients, antenna tilts) initializes the model
!              modules and the necessary utility routines.
!     dDRIVR - Performs the program calculations.
!     IM_OUT - Will write an IM file. Does not exist yet.
!
!     COMMON BLOCKS USED -
      INCLUDE 'cuser11.i'
!          Variables 'from':
!            1. C_mode -  Character*6 variable denoting the user type:
!                          'mark3 ' => Program calc11, Calc/Solve database user.
!                          'difx  ' => Program dcalc, difx correlator user.
!                          '      ' => Other future users.
! obselete?  1. Calc_user - Character denoting the user type:
! obselete?                   'A' = Analysis center, using the old Mark3 
! obselete?                         database handler.
! obselete?                   'N' = Future analysis mode, to work with nuSolve
! obselete?                         and NetCDF files. -Not yet implemented-
! obselete?                   'C' = Correlator user. Interfaces with some
! obselete?                         type of calc server.
! obselete?                   'D' = Difx correlator user. Will get input from
! obselete?                         a '.calc' correlator file.
!
!
      INCLUDE 'd_input.i'
!       Variables From:
!         1. Intrvls2min - Number of 2-minute intervals for the current scan. 
!
      Character*132 rmIM
      CHARACTER*128 Calcfiles(2000), IMfiles(2000)
      Integer*4 ILU, I, J, Num_scans, K, ierr, Kjob
      Integer*4 lu_out, fd_out
      Logical  exist
!
! 1.2.8 PROGRAM VARIABLES -
!           ILU  - The message LU, set to 6. [Not used anywhere?]
!
! 1.2.9 PROGRAMMER - David Gordon 01/15/2013
!      Jan. 2013  DG - dmain.f created for the difx version of calc11. 
!      Mar. 2014  DG - Moved C_mode definition to dmain.f and cmain.f.
!      Jan. 2015  DG - Modified for multiple scans and multiple phase centers.
!
!     MAIN Program Structure
!
!  Define the calc mode:
      C_mode = 'difx  '
!
      ILU = 6
!
! Get command line arguments
      Call GETCL(Calcfiles, IMfiles)
!       Write (6,*) 'dmain  '
!         Do I = 1, Numjobs
!            Write (6,*) I,Calcfiles(I),IMfiles(I)
!         Enddo
!
!  Process all the jobs
      DO K = 1, Numjobs         ! Job Loop
          Kjob = K
        calc_file_name = Calcfiles(K)
        IM_file_name   = IMfiles(K)
!       write(6,*) 'dmain: ', K, calc_file_name, IM_file_name
        Inquire(File=calc_file_name, Exist=exist)
!        write(6,*) ' calc_file_name exists? ', exist
        If (.Not. exist ) Then
          Write(6,*) '                    '             
          Write(6,*) ' File does not exist: ', calc_file_name
          Write(6,*) ' Skipping this job. '             
          Write(6,*) '                    '             
          Go to 100
        Endif
        If(Verbose .ge. 1) Write(6,*) '           '
        If(Verbose .ge. 1) Write(6,*) 'Processing ', calc_file_name
!
        Inquire(File=IM_file_name, Exist=exist)
        If (exist) Then
         If (overwrite .eq. 'yes ') Then
          rmIM(1:4) = 'rm  '
          rmIM(5:132) = IM_file_name
          If (Verbose .ge. 1) Write(6,*) 'Overwriting .im file: ',      &
     &                                    IM_file_name 
          ierr = SYSTEM(rmIM)
         Else
!         Write(6,*) '                    '             
          Write(6,*) ' .im file already exists: ', IM_file_name 
          Write(6,*) ' Skipping this job. ' 
          Write(6,*) '                    '             
          Go to 100
         Endif
        Endif
!
!   Initialize dCALC 
      CALL dSTART(Num_scans, Kjob)
!
!   Initialize the model modules and the necessary utilities.
      CALL dINITL(Kjob)
!
!  Create and begin writing the IM file:
      CALL d_out1(lu_out,fd_out, Kjob)
!
!
!  Begin the scan processing loop
      DO I = 1, Num_scans       ! Scan loop
!
!  Get scan information for the I'th scan
       CALL dSCAN(I, Kjob)
!
!   Begin the 2-minute interval processing loop
       DO J = 1, Intrvls2min    ! 2-minute interval loop
!
!   Create the observations and compute the delays for the I'th scan.
         CALL dDRIVR(I,J)
!
!  Output computations to the IM table for this 2-minute interval.
         Call d_out2(I,J,lu_out,fd_out)
!
       ENDDO                    ! 2-minute interval loop
!
      ENDDO                     ! Scan loop
!
 100  Continue
      ENDDO                     ! Job Loop
!
      Close (lu_out)
      If (Verbose .ge. 1) write(6,*) '  '
!
      STOP
      END

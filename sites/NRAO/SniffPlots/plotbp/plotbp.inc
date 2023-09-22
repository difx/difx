C     Include file for PLOTBP.
C
      INTEGER    MCHAN, INUNIT, MAXIF
      PARAMETER  (INUNIT=8)
      PARAMETER  (MAXIF=32)
      PARAMETER  (MCHAN=32768)
C
      REAL       AMP(MCHAN), PHASE(MCHAN)
      REAL       AMAX, AMPAVG, ALMAX, PMIN, PMAX, FREQ(MAXIF), BW
      INTEGER    NCHAN, NCHIF, NIF, JDAY1
      LOGICAL    XWIN, NEWFMT
      CHARACTER  NAME1*8, NAME2*8, TYPE*2, EXPNAM*8, DOSTA*8, DOSTA2*8
      CHARACTER  CTIME(2)*11, SOURCE*11, STOKES(MAXIF)*2, SBD(MAXIF)*1
      CHARACTER  PLTFILE*256, TF*256
C
C     BPDAT split in efforts to understand some size issues - July 2009
C     Putting the COMMON for BPDAT after the other 2 seems to have 
C     fixed the problem.  g77 bug?
C     Running into problems with "relocation truncated to fit" errors
C     when I tried to increase the number of channels in Mar. 2013.  
C     The problem can crop up even with the tiny (one integer) common
C     in VLBOPE.  I sense that all the commons are getting lumped 
C     together.  Some GOOGLE work indicates that this is simply because 
C     of wanting too much memory (>2GB?) and it is a linker issue.  
C     They suggest compiling with -mcmodel=medium  (gcc option).  
C     That looks like it might work, but libcit will also need it. 
C     that risks having other programs compiled without it break.
C     I have to back off the space in the commons.
C
      COMMON / BPDAT / AMP, PHASE
      COMMON / BPDAT2 / NCHAN, NCHIF, NIF, JDAY1, XWIN,
     1                 AMAX, AMPAVG, ALMAX, PMIN, PMAX, 
     2                 NEWFMT, FREQ, BW
      COMMON / BPCHR / NAME1, NAME2, TYPE, EXPNAM, DOSTA, DOSTA2, CTIME, 
     1                 SOURCE, STOKES, SBD, PLTFILE, TF
C
C     Variables for reading lines of data.  Put in common to
C     avoid compiler complaints about unused variables.
C     July 2011 increase MWORDS for some of the big files I've seen.
C
      INTEGER    MWORDS
      PARAMETER  (MWORDS=200)
      INTEGER    WLEN(MWORDS), NWORDS
      CHARACTER  WORD(MWORDS)*20
      COMMON / RLNDAT / WLEN, NWORDS
      COMMON / RLNCHR / WORD
C
C     Keep summary.  Note MSTA refers to station pairs and can be
C     larger than the number of stations if a joint data set from 
C     more than one sniff is used.  But it won't likely be the number
C     of baselines total.  A typical global situation is that two
C     antennas are used for reference - one European and one US.  For
C     20 station observations, there would then be 2*19 pairs.  Try
C     that for a while.  Note the 3D arrays.  They can get big fast
C     if not careful.  With MCHAN=32768, MSTA=40, MFR=80 are near
C     the limit.  Small increases in either overflow the memory the
C     compiler can handle.
C
      INTEGER    MSTA, MFR, NSTA, NFR
      PARAMETER  (MSTA=40)
      PARAMETER  (MFR=80)
      INTEGER    NORMAC(MFR,MSTA)
      REAL       LOWDP(MFR,MSTA)
      REAL       HIAMP(MCHAN,MFR,MSTA), HIPH(MCHAN,MFR,MSTA)
      REAL       SUMAC(MCHAN,MFR,MSTA)
      REAL       SUMAC1(MCHAN,MFR,MSTA), SUMAC2(MCHAN,MFR,MSTA)
      REAL       SFREQ(MAXIF,MFR)
      REAL       SBW(MFR)
      INTEGER    SNCHAN(MFR), SNCHIF(MFR)
      INTEGER    SNIF(MFR)
      CHARACTER  STA1(MSTA)*8, STA2(MSTA)*8
      CHARACTER  SSTOKE(MAXIF,MFR)*2
      CHARACTER  SSBD(MAXIF,MFR)*1
C
      CHARACTER  SSRC(MFR,MSTA)*11
      INTEGER    SJDAY(MFR,MSTA)
      CHARACTER  SCTIME(2,MFR,MSTA)*11
      CHARACTER  SEXPNAM(MFR,MSTA)*8
      
C
      COMMON / BPSUM / NSTA, NFR, NORMAC, LOWDP, HIAMP,  
     1                 HIPH, SUMAC, SUMAC1, SUMAC2, 
     2                 SFREQ, SBW, SNCHAN, SNCHIF, SNIF, SJDAY
      COMMON / RBPSUM / STA1, STA2, SSTOKE, SSBD, SSRC, SCTIME,
     1                  SEXPNAM
C ----------------------------------------------------------------------


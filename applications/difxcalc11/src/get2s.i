!  Include file get2s.i
!   Include to hold all the database GET's used in the
!    'geometry' subroutines. Those GET's are now moved
!     into Subroutine G_GET. 2012 Dec., D. Gordon 
!
      Real*8    TAGSEC, REF_FREQ
      Integer*2 LNBASE(4,2), ITAG(5), LSTRNM(4) 
!
      COMMON / GGETS / TAGSEC, REF_FREQ, LNBASE, ITAG, LSTRNM
!
      Real*8    SurPR(2,2), SurTP(2,2), SurHM(2,2)
      Integer*4 metPR, metTP, metHM
!
      COMMON / PGETS / SurPR, SurTP, SurHM, metPR, metTP, metHM

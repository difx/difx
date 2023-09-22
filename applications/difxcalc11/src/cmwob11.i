!  Include file cmwob.i
!   Separated from cwobm.f, 98.04.29, D. Gordon
!
      Integer*4 N_spline
      Logical*4 Use_cubic, Use_linear, Use_spline
      Integer*2 KERASK, NEPOCH, LEN_WOB_TABLE, i2dum
      Real*8           DWOBP(2,2), RWOBX(3,3), RWOBY(3,3), WOBIF(3),    &
     &                 WOBX, WOBY, XYWOB(2,20), DWOBX, DWOBY, RWX(3,3), &
     &                 RWY(3,3), XA(20), YAX(20), YAY(20), Y2SX(20),    &
     &                 Y2SY(20), RWOBZ(3,3), RWZ(3,3), WOBXli, WOBYli,  &
     &                 dWOBXli, dWOBYli, WOBXt, WOBYt, dWOBXt, dWOBYt,  &
     &                 WOBXti, WOBYti  
!
      COMMON / WOBCM / DWOBP, RWOBX, RWOBY, WOBIF, WOBX, WOBY, XYWOB,   &
     &                 DWOBX, DWOBY, WOBXli, WOBYli, dWOBXli, dWOBYli,  &
     &                 WOBXt, WOBYt, dWOBXt, dWOBYt, RWX, RWY,          &
     &                 XA, YAX, YAY, Y2SX, WOBXti, WOBYti,              &
     &                 Y2SY, RWOBZ, RWZ, N_spline, Use_cubic,           &
     &                 Use_linear, Use_spline, KERASK, NEPOCH,          &
     &                 LEN_WOB_TABLE, i2dum
!

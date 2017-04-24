000010*****************************
000020*    ´×° DISPLAY (Ò²Ý)      *
000030*****************************
000040 ERR-ENT.
000050     MOVE    ERR-STAT  TO  ERR-FLG.
000060     PERFORM CLSE-ENT THRU CLSE-EXT.
000070     DISPLAY  DISP-MSG-SPACE.
000080 ERR-010.
000090     DISPLAY ERR-DIS.
000100     DISPLAY DISP-BUZ-B.
000110     DISPLAY DISP-MSG-SPACE.
000120     GO TO ERR-010.
000130 ERR-EXT.
000140     EXIT.
000150*****************************
000160*    ŠY“–ŒŽŽæž‚Ýˆ—       *
000170*****************************
000180 Z-RTN.
000190     MOVE    1         TO  ZI.
000200 Z-010.
000210     IF  ZI  >  15
000220         MOVE  99      TO  ZI
000230         GO    TO      Z-EXT.
000240     IF  Z-TOUF(ZI)  >  ZYMD
000250         ADD   1       TO  ZI
000260         GO    TO      Z-010.
000270     IF  Z-TOUT(ZI)  <  ZYMD
000280         ADD   1       TO  ZI
000290         GO    TO      Z-010.
000300 Z-EXT.
000310     EXIT.
000320*
000330*

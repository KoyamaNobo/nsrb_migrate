000010*****************************
000020*    �װ DISPLAY (Ҳ�)      *
000030*****************************
000040 ERR-RTN.
000050     MOVE    ERR-STAT  TO  ERR-FLG.
000060     PERFORM END-RTN THRU END-EX.
000070     DISPLAY  DISP-MSG-SPACE.
000080 ERR-010.
000090     DISPLAY ERR-DIS.
000100     DISPLAY DISP-BUZ-B.
000110     DISPLAY DISP-MSG-SPACE.
000120     GO TO ERR-010.
000130 ERR-EX.
000140     EXIT.
000150*
000160*

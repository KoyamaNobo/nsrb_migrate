000010*****************************
000020*    月数計算ルーチン　     *                                     90/11/20
000030*****************************
000040 NISU-RTN.
000050     MOVE ZERO     TO NISU-NET.
000060     IF NISU-FROMR > NISU-TOR     GO TO NISU-EXT.
000070     COMPUTE NISU-NEN = NISU-TOY - NISU-FROMY.
000080     IF NISU-TOM < NISU-FROMM
000090        COMPUTE NISU-NEN = NISU-NEN - 1
000100        COMPUTE NISU-TUK = 12 - ( NISU-FROMM - NISU-TOM ).
000110     IF NISU-TOM > NISU-FROMM
000120        COMPUTE NISU-TUK = NISU-TOM - NISU-FROMM.
000130     IF NISU-TOM = NISU-FROMM
000140        IF NISU-TOD < NISU-FROMD
000150           COMPUTE NISU-NEN = NISU-NEN - 1
000160           MOVE 11     TO NISU-TUK.
000170 NISU-EXT.
000180     EXIT.
000190*

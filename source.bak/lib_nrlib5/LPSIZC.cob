000010*******************************************************************
000020*    サイズチェック及びサイズ名取得                               *
000030*******************************************************************
000040 SIZE-RTN.
000050     MOVE ZERO          TO SIZE-WK-SW SIZE-WK-KB.
000060     MOVE SPACE         TO SIZE-WK-NM.
000070 SIZE-010.
000080     IF  SIZE-WK-CD          = ZERO
000090         MOVE 1             TO SIZE-WK-SW
000100         GO TO SIZE-EX.
000110 SIZE-020.
000120     MOVE SIZE-WK-HIN   TO  HI-MHCD HI-HCD.
000130     READ HI2-M UNLOCK INVALID
000140          MOVE 9             TO  SIZE-WK-SW
000150          GO TO SIZE-EX.
000160 SIZE-030.
000170     MOVE 1             TO  SIZE-WK-II.
000180 SIZE-040.
000190     MOVE 1             TO  SIZE-WK-JJ.
000200 SIZE-050.
000210     IF SIZE-CD(SIZE-WK-II SIZE-WK-JJ) = SIZE-WK-CD
000220         IF HI-S(SIZE-WK-II SIZE-WK-JJ) NOT = ZERO
000230            GO TO SIZE-100.
000240 SIZE-060.
000250     IF  SIZE-WK-JJ NOT = 10
000260         ADD  1             TO SIZE-WK-JJ
000270         GO TO SIZE-050.
000280 SIZE-070.
000290     IF  SIZE-WK-II NOT = 4
000300         ADD  1             TO SIZE-WK-II
000310         GO TO SIZE-040.
000320*
000330     MOVE 2             TO SIZE-WK-SW.
000340     GO TO SIZE-EX.
000350 SIZE-100.
000360     MOVE SIZE-WK-II    TO SIZE-WK-KB.
000370*
000380*****IF  ZERO = HI-SS2 AND HI-SS3 AND HI-SS4                      D.040323
000390     IF  SIZE-WK-II   =  1                                        I.040928
000400         IF  ZERO NOT = HI-SS2 OR HI-SS3 OR HI-SS4                I.040323
000410             IF  HI-SS1 NOT = ZERO
000420                 MOVE 5             TO SIZE-WK-II.
000430     IF  SIZE-WK-II   =  1                                        I.050301
000440         IF  HI-BC3  = 30                                         I.050301
000450             MOVE 5             TO SIZE-WK-II.                    I.050301
000460*
000470     MOVE SIZE-NM(SIZE-WK-II SIZE-WK-JJ) TO SIZE-WK-NM.
000480*
000490     IF  HI-HKB = 1
000500         MOVE NC"．５"     TO SIZE-WK-NM(3:2).
000510 SIZE-EX.
000520     EXIT.

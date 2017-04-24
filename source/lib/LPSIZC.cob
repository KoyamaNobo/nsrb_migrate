      *******************************************************************
      *    サイズチェック及びサイズ名取得                               *
      *******************************************************************
       SIZE-RTN.
           MOVE ZERO          TO SIZE-WK-SW SIZE-WK-KB.
           MOVE SPACE         TO SIZE-WK-NM.
       SIZE-010.
           IF  SIZE-WK-CD          = ZERO
               MOVE 1             TO SIZE-WK-SW
               GO TO SIZE-EX
           END-IF.
       SIZE-020.
           MOVE SIZE-WK-HIN   TO  HI-MHCD HI-HCD.
      *           READ HI2-M UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 9             TO  SIZE-WK-SW
               GO TO SIZE-EX
           END-IF.
       SIZE-030.
           MOVE 1             TO  SIZE-WK-II.
       SIZE-040.
           MOVE 1             TO  SIZE-WK-JJ.
       SIZE-050.
           IF  SIZE-CD(SIZE-WK-II SIZE-WK-JJ) = SIZE-WK-CD
               IF  HI-S(SIZE-WK-II SIZE-WK-JJ) NOT = ZERO
                   GO TO SIZE-100
               END-IF
           END-IF.
       SIZE-060.
           IF  SIZE-WK-JJ NOT = 10
               ADD  1             TO SIZE-WK-JJ
               GO TO SIZE-050
           END-IF.
       SIZE-070.
           IF  SIZE-WK-II NOT = 4
               ADD  1             TO SIZE-WK-II
               GO TO SIZE-040
           END-IF
      *
           MOVE 2             TO SIZE-WK-SW.
           GO TO SIZE-EX.
       SIZE-100.
           MOVE SIZE-WK-II    TO SIZE-WK-KB.
      *
           IF  SIZE-WK-II   =  1
               IF  ZERO NOT = HI-SS2 OR HI-SS3 OR HI-SS4
                   IF  HI-SS1 NOT = ZERO
                       MOVE 5             TO SIZE-WK-II
                   END-IF
               END-IF
           END-IF
           IF  SIZE-WK-II   =  1
               IF  HI-BC3  = 30
                   MOVE 5             TO SIZE-WK-II
               END-IF
           END-IF
      *
           MOVE SIZE-NM(SIZE-WK-II SIZE-WK-JJ) TO SIZE-WK-NM.
      *
           IF  HI-HKB = 1
               MOVE "．５"     TO SIZE-WK-NM(3:2)
           END-IF.
       SIZE-EX.
           EXIT.

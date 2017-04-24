       NAM-RTN.
           MOVE 0 TO WN-DCHK.
       NAM-010.
           MOVE SPACE TO WN-ONAME WN-UNAME.
           MOVE 27 TO WN-CNT1.
       NAM-020.
           SUBTRACT 1 FROM WN-CNT1.
           IF  WN-CNT1 = 16
               MOVE SPACE TO WN-ONAME
               MOVE WN-NAME TO WN-UNAME
               GO TO NAM-EX
           END-IF
           IF  WN-NA(WN-CNT1) = SPACE
               GO TO NAM-020
           END-IF
           IF  WN-DCHK = 1
               GO TO NAM-310
           END-IF
           IF  WN-DCHK = 2
               GO TO NAM-410
           END-IF.
       NAM-100.
           MOVE 17 TO WN-CNT2.
       NAM-110.
           IF  WN-NA(WN-CNT1) = SPACE
               GO TO NAM-120
           END-IF
           SUBTRACT 1 FROM WN-CNT2.
           IF  WN-CNT2 NOT = ZERO
               MOVE WN-NA(WN-CNT1) TO WN-UNA(WN-CNT2)
               SUBTRACT 1 FROM WN-CNT1
               GO TO NAM-110
           END-IF
           GO TO NAM-210.
       NAM-120.
           IF  WN-CNT1 > 17
               SUBTRACT 1 FROM WN-CNT1
               GO TO NAM-110
           END-IF
      *
           MOVE ZERO TO WN-CNT3.
       NAM-130.
           ADD 1 TO WN-CNT3.
           IF  WN-CNT1 = WN-CNT3
               GO TO NAM-EX
           END-IF
           IF  WN-CNT3 < 17
               MOVE WN-NA(WN-CNT3) TO WN-ONA(WN-CNT3)
               GO TO NAM-130
           END-IF.
       NAM-210.
           MOVE WN-NAME TO WN-WNAME.
           MOVE SPACE TO WN-NAME.
           MOVE ZERO TO WN-CNT1 WN-CNT2.
       NAM-220.
           ADD 1 TO WN-CNT1.
           IF  WN-CNT1 > 26
               MOVE 1 TO WN-DCHK
               GO TO NAM-010
           END-IF
           IF  WN-WNA(WN-CNT1) NOT = SPACE
               ADD 1 TO WN-CNT2
               MOVE WN-WNA(WN-CNT1) TO WN-NA(WN-CNT2)
           END-IF 
           GO TO NAM-220.
       NAM-310.
           MOVE WN-NAME TO WN-WNAME.
           MOVE SPACE TO WN-NAME.
           MOVE ZERO TO WN-CNT1.
       NAM-320.
           ADD 1 TO WN-CNT1.
           IF  WN-CNT1 > 23
               MOVE SPACE TO WN-NAME
               MOVE WN-WNAME TO WN-NAME
               GO TO NAM-410
           END-IF
           MOVE WN-WNA(WN-CNT1) TO WN-NA(WN-CNT1).
           MOVE SPACE TO WN-KUM.
           MOVE WN-CNT1 TO WN-CNT2.
           MOVE ZERO TO WN-CNT3.
       NAM-330.
           ADD 1 TO WN-CNT3.
           IF  WN-CNT3 NOT = 5
               MOVE WN-WNA(WN-CNT2) TO WN-KU(WN-CNT3)
               ADD 1 TO WN-CNT2
               GO TO NAM-330
           END-IF
           IF  WN-KUM NOT = "Š”Ž®‰ïŽÐ" AND "—LŒÀ‰ïŽÐ"
               GO TO NAM-320
           END-IF
      *
           IF  WN-KUM = "Š”Ž®‰ïŽÐ"
               MOVE "‡Š" TO WN-NA(WN-CNT1)
           END-IF
           IF  WN-KUM = "—LŒÀ‰ïŽÐ"
               MOVE "‡‹" TO WN-NA(WN-CNT1)
           END-IF
           MOVE WN-CNT1 TO WN-CNT2.
           ADD 3 TO WN-CNT2.
       NAM-340.
           ADD 1 TO WN-CNT1 WN-CNT2.
           IF  WN-CNT2 < 27
               MOVE WN-WNA(WN-CNT2) TO WN-NA(WN-CNT1)
               GO TO NAM-340
           END-IF
           MOVE 2 TO WN-DCHK.
           GO TO NAM-010.
       NAM-410.
           MOVE SPACE TO WN-ONAME WN-UNAME.
           MOVE ZERO TO WN-CNT1.
       NAM-420.
           ADD 1 TO WN-CNT1.
           IF  WN-CNT1 > 26
               GO TO NAM-EX
           END-IF
           IF  WN-CNT1 < 17
               MOVE WN-NA(WN-CNT1) TO WN-ONA(WN-CNT1)
               GO TO NAM-420
           END-IF
           IF  WN-CNT1 = 17
               MOVE 6 TO WN-CNT2
           END-IF
           ADD 1 TO WN-CNT2.
           MOVE WN-NA(WN-CNT1) TO WN-UNA(WN-CNT2)
           GO TO NAM-420.
       NAM-EX.

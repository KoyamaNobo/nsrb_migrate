       DAI-RTN.
           ADD 1 TO W-DAI2.
           IF  W-DAI2 NOT = ZERO
               GO TO DAI-EX
           END-IF
           MOVE 4 TO W-DNOC.
       DAI-010.
           SUBTRACT 1 FROM W-DNOC.
           IF  W-DNOC = 0
               MOVE "999" TO W-DAI1
               GO TO DAI-EX
           END-IF
           IF  W-DAID(W-DNOC) = "Z"
               GO TO DAI-010
           END-IF
           MOVE ZERO TO W-DNOT.
       DAI-020.
           ADD 1 TO W-DNOT.
           IF  W-DNOT > 26
               MOVE "Z" TO W-DAID(W-DNOC)
               GO TO DAI-EX
           END-IF
           IF  W-DAID(W-DNOC) NOT = W-TBLA(W-DNOT)
               GO TO DAI-020
           END-IF
           ADD 1 TO W-DNOT.
           IF  W-DNOT > 27
               MOVE "Z" TO W-DAID(W-DNOC)
           ELSE
               MOVE W-TBLA(W-DNOT) TO W-DAID(W-DNOC)
           END-IF.
       DAI-EX.

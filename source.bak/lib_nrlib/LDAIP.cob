000010 DAI-RTN.
000020     ADD 1 TO W-DAI2.
000030     IF W-DAI2 NOT = ZERO
000040         GO TO DAI-EX.
000050     MOVE 4 TO W-DNOC.
000060 DAI-010.
000070     SUBTRACT 1 FROM W-DNOC.
000080     IF W-DNOC = 0
000090         MOVE "999" TO W-DAI1
000100         GO TO DAI-EX.
000110     IF W-DAID(W-DNOC) = "Z"
000120         GO TO DAI-010.
000130     MOVE ZERO TO W-DNOT.
000140 DAI-020.
000150     ADD 1 TO W-DNOT.
000160     IF W-DNOT > 26
000170         MOVE "Z" TO W-DAID(W-DNOC)
000180         GO TO DAI-EX.
000190     IF W-DAID(W-DNOC) NOT = W-TBLA(W-DNOT)
000200         GO TO DAI-020.
000210     ADD 1 TO W-DNOT.
000220     IF W-DNOT > 27
000230         MOVE "Z" TO W-DAID(W-DNOC)
000240       ELSE
000250         MOVE W-TBLA(W-DNOT) TO W-DAID(W-DNOC).
000260 DAI-EX.

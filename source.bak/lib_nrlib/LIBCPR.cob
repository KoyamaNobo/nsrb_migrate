000010     OPEN INPUT  M-DATE.
000020     MOVE  "01" TO DATE-KEY.
000030*AA.                                                              D.000225
000040 AAAA.                                                            I.000225
000050     READ  M-DATE   WITH UNLOCK  INVALID
000060           DISPLAY ERR-DATE
000070                   ERR-BUZ
000080                   GO TO  AAAA.                                   I.000225
000090*****              GO TO  AA.                                     D.000225
000100     CLOSE  M-DATE.

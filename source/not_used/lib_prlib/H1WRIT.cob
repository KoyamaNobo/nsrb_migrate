000010     MOVE       4            TO     DATAWK2.
000020     MOVE       8            TO     DATAWK4.
000030     ADD        1            TO     HPAGE.
000040     MOVE       HPAGE        TO     HPAGER.
000050     CALL       "CBLNMOVE"   USING  HPAGER   DATAWK2
000060                                    HPAGEOUT DATAWK4.
000070     MOVE       HPAGEOUT     TO     HPAGEMV1.
000080     MOVE       HPAGEMV      TO     H1-PAGE.

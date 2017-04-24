000010     ACCEPT     HYMDIN       FROM   DATE.
000020     MOVE       HYYIN        TO     HYYIN-W.
000030     MOVE       HMMIN        TO     HMMIN-W.
000040     MOVE       HDDIN        TO     HDDIN-W.
000050     CALL       @CBLNMOVE@   USING  HYMDIN-W DATAWK2
000060                                    HYMDOUT  DATAWK4.
000070     MOVE       HYYOUT       TO     HYYMOVE.
000080     MOVE       HMMOUT       TO     HMMMOVE.
000090     MOVE       HDDOUT       TO     HDDMOVE.
000100     MOVE       HYMDMOVE     TO     H1-DATE.

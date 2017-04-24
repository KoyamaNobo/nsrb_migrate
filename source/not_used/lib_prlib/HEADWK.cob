000010 01  HYMDIN.
000020     02  HYYIN           PIC 99.
000030     02  HMMIN           PIC 99.
000040     02  HDDIN           PIC 99.
000050 01  HYMDIN-W.
000060     02  HYYIN-W         PIC ZZ.
000070     02  HMMIN-W         PIC ZZ.
000080     02  HDDIN-W         PIC ZZ.
000090  01  HYMDOUT.
000100     02  HYYOUT          PIC X(4).
000110     02  HMMOUT          PIC X(4).
000120     02  HDDOUT          PIC X(4).
000130 01  HYMDMOVE.
000140     02  HYYMOVE         PIC BBX(4).
000150     02  FILLER          PIC X(2)    VALUE ""K472F"".
000160     02  HMMMOVE         PIC X(4).
000170     02  FILLER          PIC X(2)    VALUE ""K376E"".
000180     02  HDDMOVE         PIC X(4).
000190     02  FILLER          PIC X(6)    VALUE ""K467C3A6E402E"".
000200 01  HPAGE               PIC 9(4)    VALUE 0.
000210 01  HPAGER              PIC ZZZ9.
000220 01  HPAGEOUT            PIC X(8).
000230 01  HPAGEMV.
000240     02  HPAGEMV1        PIC X(8).
000250     02  FILLER          PIC X(2)    VALUE ""K4A47"".
000260 01  DATAWK2             PIC 9(4)    VALUE 6.
000270 01  DATAWK4             PIC 9(4)    VALUE 12.

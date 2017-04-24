000010***  仕訳ワーク　     (256/1)
000020 FD  SDW
000030*****BLOCK      3     RECORDS                                     D.970530
000040     BLOCK      1     RECORDS                                     I.970530
000050     LABEL  RECORD  STANDARD
000060     VALUE  OF  IDENTIFICATION  WK0256ID.                         I.970530
000070*****VALUE  OF  IDENTIFICATION  "SIWAKE-WK".                      D.960509
000080*****VALUE  OF  IDENTIFICATION  WK0170ID.                         D.970530
000090*
000100 01  SW-REC.
000110*****02  WHTRDATE         PIC 9(6).                               D.971111
000120     02  WHKACD1.                                                 I.980224
000130       03  WHACCNTCD      PIC 9(4).                               I.980224
000140       03  WHHOACCNT      PIC 9(4).                               I.980224
000150     02  WHTRDATE         PIC 9(8).                               I.971111
000160     02  WHJUNLNO         PIC 9(6).
000170     02  WHLINENO         PIC 9(2).
000180     02  WHDR-CR          PIC 9(1).
000190*****02  WHKACD1.                                                 D.980224
000200*****  03  WHACCNTCD      PIC 9(4).                               D.980224
000210*****  03  WHHOACCNT      PIC 9(4).                               D.980224
000220     02  WHSECTCD         PIC 9(4).
000230     02  WHSKINCD         PIC 9(3).
000240     02  WHTAXKB          PIC X(1).
000250     02  WHAMOUNT         PIC S9(10).
000260     02  WHTEG-BAN        PIC 9(2).
000270*****02  WHTEG-NO         PIC X(8).                               D.980224
000280     02  WHKACD2.
000290       03  WHOPPCD        PIC 9(4).
000300       03  WHHOOPPCD      PIC 9(4).
000310     02  WHCUSTCD         PIC 9(5).
000320     02  WHTEKICD         PIC 9(3).
000330     02  WHTEKIYO         PIC N(20).
000340     02  WHKEIHIKB        PIC 9(1).
000350     02  WHNAMEN          PIC N(10).
000360     02  WHACCNTCD2       PIC 9(4).
000370*****02  WHTRDATE2        PIC 9(6).                               D.971111
000380     02  WHTRDATE2        PIC 9(8).                               I.971111
000390     02  WHJUNLNO2        PIC 9(6).
000400     02  WHLINENO2        PIC 9(2).
000410     02  WHDR-CR2         PIC 9(1).
000420*****02  WHKACD3.                                                 D.980224
000430*****  03  WHACCNT        PIC 9(4).                               D.980224
000440*****  03  WHHOACCNT3     PIC 9(4).                               D.980224
000450*****02  WHTRDATE3        PIC 9(6).                               D.971111
000460*****02  WHTRDATE3        PIC 9(8).                               D.980224
000470*****02  WHJUNLNO3        PIC 9(6).                               D.980224
000480*****02  WHLINENO3        PIC 9(2).                               D.980224
000490*****02  WHDR-CR3         PIC 9(1).                               D.980224
000500*****02  FILLER           PIC X(19).                              D.971111
000510*****02  FILLER           PIC X(13).                              D.980217
000520*****02  WHNAMEN          PIC N(10).                              D.980224
000530*****02  F                PIC X(15).                              D.980224
000540     02  F                PIC X(26).                              I.980224
000550     02  WHCOM            PIC 9(1).
000560*****02  FILLER           PIC X(64).                              D.980224
000570     02  FILLER           PIC X(86).                              I.980224

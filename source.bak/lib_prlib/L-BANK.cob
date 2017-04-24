000010 FD  BM
000020     BLOCK       CONTAINS    1       RECORDS
000030     LABEL   RECORD  STANDARD
000040     VALUE   OF  IDENTIFICATION  IS  "BANK-K".
000050 01  BM-REC.
000060     02  BM-KEY.
000070       03  BANKCD        PIC 9(4).                                H 90.12
000080     02  BANKNM          PIC X(20).
000090     02  BANKNMN  REDEFINES  BANKNM  PIC N(10).                   A 90.11
000100     02  KOMOK       OCCURS  12.
000110       03  RATE          PIC 9(4).
000120       03  DBFOR         PIC S9(10).
000130       03  DCTERM        PIC S9(10).
000140       03  MCTZN         PIC S9(12).
000150     02  WARIRATE        PIC 9(4).
000160     02  WARIKIN         PIC S9(10).
000170     02  WARIKINH        PIC S9(10).
000180     02  WARIKINS        PIC S9(12).
000190     02  FILLER          PIC X(20).                               H 90.12
000200 END

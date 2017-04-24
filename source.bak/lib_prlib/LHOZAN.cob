000010******************************************************************
000020*                  補助残高マスタ                                *
000030*                   <  256 REC  1 BLOCK  >                       *
000040*                   <       INDEXED      >                       *
000050******************************************************************
000060*
000070 FD  HZM-F
000080     LABEL   RECORD   IS   STANDARD
000090     BLOCK   CONTAINS   1   RECORDS
000100     VALUE   OF   IDENTIFICATION   "HOZAN-K".
000110*
000120 01  HZM-R.
000130     02  HZM-KEY.
000140         03  HZM-KMCD         PIC   9(04).
000150         03  HZM-HOCD         PIC   9(04).
000160     02  HZM-ZAN              PIC  S9(11)  COMP-3.
000170     02  HZM-TJ.
000180         03  HZM-TJIS      OCCURS  15.
000190             04  HZM-TJKR     PIC  S9(11)  COMP-3.
000200             04  HZM-TJKS     PIC  S9(11)  COMP-3.
000210     02  FILLER               PIC   X(62).

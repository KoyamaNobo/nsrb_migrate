000010******************************************************************
000020*                  科目残高マスタ                                *
000030*                   <  341 REC  3 BLOCK  >                       *
000040*                   <       INDEXED      >                       *
000050******************************************************************
000060*
000070 FD  KZM-F
000080     LABEL   RECORD   IS   STANDARD
000090     BLOCK   CONTAINS   3   RECORDS
000100     VALUE   OF   IDENTIFICATION   "KAZAN-K".
000110*
000120 01  KZM-R.
000130     02  KZM-KEY.
000140         03  KZM-KMCD         PIC   9(04).
000150     02  KZM-ZAN              PIC  S9(11)  COMP-3.
000160     02  KZM-TJ.
000170         03  KZM-TJIS      OCCURS  15.
000180             04  KZM-TJKR     PIC  S9(11)  COMP-3.
000190             04  KZM-TJKS     PIC  S9(11)  COMP-3.
000200     02  KZM-ZJ.
000210         03  KZM-ZJIS      OCCURS  12.
000220             04  KZM-ZJKR     PIC  S9(11)  COMP-3.
000230             04  KZM-ZJKS     PIC  S9(11)  COMP-3.
000240     02  FILLER               PIC   X(07).

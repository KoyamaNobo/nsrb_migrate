000010******************************************************************
000020*                   < лч ╩ч щ   о  ╫  ю  >                       *
000030*                   <  341 REC  3 BLOCK  >                       *
000040*                   <       INDEXED      >                       *
000050******************************************************************
000060*
000070 FD  BZM-F
000080     LABEL   RECORD   IS   STANDARD
000090     BLOCK   CONTAINS   3   RECORDS
000100     VALUE   OF   IDENTIFICATION   "BUZAN-K".
000110*
000120 01  BZM-REC.
000130     02  BZM-KEY.
000140         03  BZM-BMON         PIC   9(04).
000150         03  BZM-BMONR        REDEFINES    BZM-BMON.
000160             04  BZM-BMCD     PIC   9(02).
000170             04  BZM-YOBI     PIC   9(02).
000180         03  BZM-KMCD         PIC   9(04).
000190     02  BZM-TJ.
000200         03  BZM-TJIS      OCCURS  15.                            H 90.12
000210             04  BZM-TJKR     PIC  S9(11)  COMP-3.
000220             04  BZM-TJKS     PIC  S9(11)  COMP-3.
000230     02  BZM-ZJ.
000240         03  BZM-ZJIS      OCCURS  12.
000250             04  BZM-ZJKR     PIC  S9(11)  COMP-3.
000260             04  BZM-ZJKS     PIC  S9(11)  COMP-3.
000270***  02  BZM-YJ.                                                  D 90.12
000280***      03  BZM-YJIS      OCCURS  3.                             D 90.12
000290***          04  BZM-YJKR     PIC  S9(11)  COMP-3.                D 90.12
000300***          04  BZM-YJKS     PIC  S9(11)  COMP-3.                D 90.12
000310     02  FILLER               PIC   X(09).

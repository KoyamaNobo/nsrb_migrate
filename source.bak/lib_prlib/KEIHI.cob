000010 FD  HH-F
000020*****BLOCK       CONTAINS    1      RECORDS                       D.990701
000030     BLOCK       CONTAINS    2      RECORDS                       I.990701
000040     LABEL       RECORD     STANDARD
000050     VALUE       OF          IDENTIFICATION  "KEIHI-K".
000060*****¹²Ë  Ï½À°***************  (128/2)
000070 01  HH-R.
000080     02  HH-KEY.
000090         03  HH-BUCD.
000100             04  HH-BU1  PIC 99.
000110             04  HH-BU2  PIC 99.
000120         03  HH-KACD     PIC 9(4).
000130         03  HH-HOCD     PIC 9(4).                                H 90.12
000140     02  HH-GOCD         PIC 9(3).
000150     02  HH-GYO          PIC 9.
000160     02  HH-TOUKI.                                                H 90.12
000170         03  HH-GEL      PIC S9(11)     COMP-3  OCCURS 15.        H 90.12
000180     02  FILLER          PIC X(22).                               I.990701
000190*****02  HH-YOSAN.                                                D.990701
000200*****    03  HH-YOS      PIC S9(11)     COMP-3  OCCURS 15.        D.990701
000210*****02  FILLER          PIC X(60).                               D.990701

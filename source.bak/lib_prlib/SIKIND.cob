000010 FD  SK-D
000020     BLOCK       CONTAINS    1       RECORDS
000030     LABEL       RECORD      STANDARD
000040     VALUE       OF          IDENTIFICATION  "SIKIN-M".
000050*****¼·Ý  Ï½À°***************
000060 01  SKD-R.
000070     02  SKD-KEY.
000080         03  SKD-SCD     PIC 9(3).
000090     02  SKD-NM          PIC X(25).
000100     02  SKD-GYO         PIC 9.
000110     02  HASTU.
000120         03  SK-KIL      PIC S9(11).
000130         03  SK-GEL      PIC S9(11).
000140         03  SK-NIL      PIC S9(11).
000150     02  KAKO.
000160         03  SK-KAL      PIC S9(11)  OCCURS  12  TIMES.
000170     02  FILLER          PIC X(62).

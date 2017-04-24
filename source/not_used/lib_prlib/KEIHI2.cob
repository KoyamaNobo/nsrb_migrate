000010 FD  HW2-F
000020     BLOCK       CONTAINS    1       RECORDS
000030     LABEL       RECORD      STANDARD
000040     VALUE       OF          IDENTIFICATION  "KEIHI-WK2".
000050*****¹²Ë  Ï½À° (WK2)*********
000060 01  HW2-R.
000070     02  HW2-KEY.
000080         03  HW2-BUCD.
000090             04  HW2-BU1 PIC 99.
000100             04  HW2-BU2 PIC 99.
000110         03  HW2-KACD    PIC 9(4).
000120         03  HW2-HOCD    PIC 9(4).
000130     02  HW2-KOUMO.
000140         03  HW2-GOCD    PIC 9(3).
000150         03  HW2-GYO     PIC 9.
000160         03  HW2-GISTU.
000170             04  HW2-KIL PIC S9(11).
000180             04  HW2-MOL PIC S9(11).
000190         03  HW2-YOSAN.
000200             04  HW2-YOS PIC S9(11)  OCCURS  12  TIMES.
000210         03  FILLER      PIC X(86).

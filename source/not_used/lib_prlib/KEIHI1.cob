000010 FD  HW1-F
000020     BLOCK       CONTAINS    1       RECORDS
000030     LABEL       RECORD      STANDARD
000040     VALUE       OF          IDENTIFICATION  "KEIHI-WK1".
000050*****���  Ͻ�� (WK1)*********
000060 01  HW1-R.
000070     02  HW1-KEY.
000080         03  HW1-BUCD.
000090             04  HW1-BU1 PIC 99.
000100             04  HW1-BU2 PIC 99.
000110         03  HW1-KACD    PIC 9(4).
000120         03  HW1-HOCD    PIC 9(4).
000130     02  HW1-KOUMO.
000140         03  HW1-GOCD    PIC 9(3).
000150         03  HW1-GYO     PIC 9.
000160         03  HW1-GISTU.
000170             04  HW1-KIL PIC S9(11).
000180             04  HW1-MOL PIC S9(11).
000190         03  HW1-YOSAN.
000200             04  HW1-YOS PIC S9(11)  OCCURS  12  TIMES.
000210         03  FILLER      PIC X(86).

000010 FD  KNG
000020     BLOCK       CONTAINS    8       RECORDS
000030     LABEL       RECORD      STANDARD
000040     VALUE       OF          IDENTIFICATION    "KAMOKU-KNG".
000050 01  KNG-R.
000060     02  KNG-KEY.
000070       03  K-ACCD          PIC 9(4).
000080       03  K-HOCD          PIC 9(4).                              H 90.12
000090     02  KNGNM             PIC X(20).
000100     02  KNGNMN  REDEFINES  KNGNM  PIC N(10).
000110     02  KNGTAX            PIC X(01).                             �ېŋ敪
000120***  02  KNGHIFU           PIC X(01).                             D 90.12
000130     02  FILLER            PIC X(3).

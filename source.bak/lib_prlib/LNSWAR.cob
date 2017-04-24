000010*****************************************************************
000020*    割引情報ファイル        ( 64/4 )  INDEXED                  *
000030*****************************************************************
000040 FD  NS-WAR
000050     BLOCK       CONTAINS    4       RECORDS
000060     LABEL       RECORD      STANDARD
000070     VALUE       OF          IDENTIFICATION    "NS-WAR".
000080 01  WAR-R.
000090     02  WAR-KEY.
000100       03  WAR-01          PIC 9(04).
000110     02  WAR-02            PIC 9(04).
000120     02  WAR-03            PIC S9(10).
000130     02  WAR-04            PIC 9(06).
000140     02  WAR-04R  REDEFINES  WAR-04.
000150       03  WAR-041         PIC 9(02).
000160       03  WAR-042         PIC 9(02).
000170       03  WAR-043         PIC 9(02).
000180     02  WAR-05            PIC 9(04).
000190     02  WAR-06            PIC S9(10).
000200     02  WAR-07            PIC 9(04).
000210     02  WAR-08            PIC 9(03).
000220     02  WAR-09            PIC 9(06).
000230     02  WAR-09R REDEFINES  WAR-09.
000240       03  WAR-091         PIC 9(02).
000250       03  WAR-092         PIC 9(02).
000260       03  WAR-093         PIC 9(02).
000270     02  WAR-10            PIC 9(02).
000280     02  WAR-11            PIC 9(02).
000290     02  FILLER            PIC X(09).

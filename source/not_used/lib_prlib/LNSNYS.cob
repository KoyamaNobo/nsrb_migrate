000010*****************************************************************
000020*    ì¸ã‡ê⁄ë±ÉtÉ@ÉCÉã        ( 384/2 )  INDEXED                 *
000030*****************************************************************
000040 FD  NS-NYS
000050     BLOCK       CONTAINS    2       RECORDS
000060     LABEL       RECORD      STANDARD
000070     VALUE       OF          IDENTIFICATION    "NS-NYS".
000080 01  NYS-R.
000090     02  NYS-KEY.
000100       03  NYS-01          PIC 9(06).
000110       03  NYS-02          PIC 9(02).
000120     02  NYS-03            PIC X(01).
000130     02  NYS-04            PIC 9(06).
000140     02  NYS-04R  REDEFINES  NYS-04.
000150       03  NYS-041         PIC 9(02).
000160       03  NYS-042         PIC 9(02).
000170       03  NYS-043         PIC 9(02).
000180     02  NYS-OST.
000190       03  NYS-05     OCCURS 10.
000200         04  NYS-051         PIC 9(04).
000210         04  NYS-052         PIC 9(01).
000220         04  NYS-053         PIC S9(07).
000230         04  NYS-054         PIC S9(08).
000240         04  NYS-055         PIC 9(04).
000250         04  NYS-055R  REDEFINES  NYS-055.
000260           05  NYS-0551      PIC 9(02).
000270           05  NYS-0552      PIC 9(02).
000280         04  NYS-056         PIC 9(06).
000290         04  NYS-056R  REDEFINES  NYS-056.
000300           05  NYS-0561      PIC 9(02).
000310           05  NYS-0562      PIC 9(02).
000320           05  NYS-0563      PIC 9(02).
000330         04  NYS-057         PIC 9(01).
000340         04  NYS-058         PIC 9(02).
000350     02  FILLER            PIC X(39).

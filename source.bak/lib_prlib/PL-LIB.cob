000010***  ‘¹‰vƒ}ƒXƒ^     (85/3)
000020 FD  PL  LABEL   RECORD  STANDARD
000030         BLOCK   3   RECORDS
000040         VALUE   OF  IDENTIFICATION  IS  "PL-K".
000050 01  PL-REC.
000060     02  PL-KEY          PIC X(3).
000070     02  PL-LIN          PIC 9.
000080     02  PL-GKB          PIC 9.
000090     02  PL-NAM          PIC X(20).
000100     02  PL-NAMN     REDEFINES   PL-NAM   PIC N(10).
000110     02  PL-YY.
000120       03  PL-ZENKI      PIC S9(11).
000130       03  PL-TOUKI      PIC S9(11).
000140     02  PL-MM.
000150       03  PL-ZENMM      PIC S9(11).
000160       03  PL-TOUMM      PIC S9(11).
000170     02  PL-URIKB        PIC X.
000180     02  PL-PKB          PIC 9.
000190     02  PL-TANA         PIC 9.
000200***  02  PL-TONEN1.                                               D 90.12
000210***    03  PL-TONEN      PIC S9(11)   OCCURS  12.                 D 90.12
000220***  02  PL-ZENNEN1.                                              D 90.12
000230***    03  PL-ZENNEN     PIC S9(11)   OCCURS  12.                 D 90.12
000240     02  PL-YM.
000250       03  PL-YYWK       PIC 99.
000260       03  PL-MMWK       PIC 99.
000270     02  FILLER          PIC X(9).                                H 90.12

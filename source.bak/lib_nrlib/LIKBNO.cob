000010***********************************
000020*    購買コントロールマスター     *
000030*        KBNOM   (64/4)           *
000040*        IS     KEY:1-5           *
000050***********************************
000060 FD  KBNO-M
000070     BLOCK  4 RECORDS
000080     LABEL RECORD IS STANDARD
000090     VALUE OF IDENTIFICATION "KBNOM".                             C 62/06
000100 01  KBNO-R.
000110     02  BNO-KEY.                                                 KEY
000120       03  BNO-KEYD     PIC  X(002).
000130       03  F            PIC  X(003).
000140     02  BNO-RD1.                                                 01
000150       03  BNO-DATE     PIC  9(002).                              台帳日付
000160       03  BNO-SNG      PIC  9(006).                              I.990519
000170       03  BNO-ENG      PIC  9(006).                              I.990519
000180       03  F            PIC  X(045).                              I.990519
000190*****  03  F            PIC  X(057).                              D.990519
000200     02  BNO-RD2   REDEFINES BNO-RD1.                             02
000210       03  BNO-DNO1     PIC  9(006).                              仕入伝票
000220       03  F            PIC  X(053).
000230     02  BNO-RD3   REDEFINES BNO-RD1.
000240       03  BNO-DNO2     PIC  9(006).                              出庫伝票
000250       03  F            PIC  X(053).

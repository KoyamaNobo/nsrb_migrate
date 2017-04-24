000010************************************************************
000020*    部門別経費配列マスタ         16 REC  / 16 BLK         *
000030************************************************************
000040 FD  BKHHAI-K
000050     BLOCK      CONTAINS     16      RECORDS
000060     LABEL      RECORD       STANDARD
000070     VALUE      OF           IDENTIFICATION      "BKHHAI-K".
000080 01  BKHHAI-REC.
000090     02  BKHHAI-KEY.
000100       03  BKHHAI-PG       PIC 9(02).
000110       03  BKHHAI-LN       PIC 9.
000120     02  BKHHAI-BUCD       PIC 9(04).
000130     02  FILLER            PIC X(09).

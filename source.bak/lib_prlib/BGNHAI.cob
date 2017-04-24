000010************************************************************
000020*    部門別製造配列マスタ         16 REC  / 16 BLK         *
000030************************************************************
000040 FD  BGNHAI
000050     BLOCK      CONTAINS     16      RECORDS
000060     LABEL      RECORD       STANDARD
000070     VALUE      OF           IDENTIFICATION      "BGNHAI-K".
000080 01  BGNHAI-REC.
000090     02  BGNHAI-KEY.
000100       03  BGNHAI-PG       PIC 9(02).
000110       03  BGNHAI-LN       PIC 9.
000120     02  BGNHAI-BUCD       PIC 9(04).
000130     02  FILLER            PIC X(09).

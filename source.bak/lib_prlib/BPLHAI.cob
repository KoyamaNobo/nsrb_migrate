000010****************************************************************
000020*                                                              *
000030*      < ÌÞÓÝÍÞÂ¿Ý´·Ê²ÚÂ Ì§²Ù >           * 16 REC / 16 B *    *
000040*                                         * INDEXED *          *
000050****************************************************************
000060 FD  BPLHAI
000070     BLOCK      CONTAINS     16     RECORDS
000080     LABEL      RECORD       STANDARD
000090     VALUE      OF           IDENTIFICATION      "BPLHAI-K".
000100 01  BPLHAI-REC.
000110     02  BPLHAI-KEY.
000120         03  BPLHAI-PG       PIC 9(02).
000130         03  BPLHAI-LN       PIC 9(01).
000140     02  BPLHAI-BUCD         PIC 9(04).
000150     02  FILLER              PIC X(09).                           H 90.12

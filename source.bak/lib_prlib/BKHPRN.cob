000010****************************************************************
000020*                                                              *
000030*      < ÌÞÓÝÍÞÂ¹²Ë »¸Ë®³Ì§²Ù >           *102 REC / 05 B *    *
000040*                                         * INDEXED *          *
000050****************************************************************
000060 FD  BKH-PRN
000070     BLOCK      CONTAINS     5      RECORDS
000080     LABEL      RECORD       STANDARD
000090     VALUE      OF           IDENTIFICATION      "BKH-PRN".
000100 01  BKHPRN-REC.
000110     02  BKHPRN-KEY.
000120         03  BKHPRN-PG       PIC 9(02).
000130         03  BKHPRN-ACCD.
000140           04  BKHPRN-ACM    PIC 9(04).
000150           04  BKHPRN-ACS    PIC 9(04).                           H 90.12
000160     02  BKHPRN-GCD          PIC 9(03).
000170     02  BKHPRN-GYO          PIC 9(01).
000180     02  BKHPRN-ITEM.
000190         03  BKHPRN-IT  OCCURS   7   TIMES.
000200             04  BKHPRN-GI   PIC S9(11).
000210     02  FILLER              PIC X(11).                           H 90.12

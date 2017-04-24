000010****************************************************************
000020*                                                              *
000030*      < ÌÞÓÝÍÞÂ¿Ý´·»¸Ë®³Ì§²Ù >           *102 REC / 05 B *    *
000040*                                         * INDEXED *          *
000050****************************************************************
000060 FD  BPLPRN
000070     BLOCK      CONTAINS     5      RECORDS
000080     LABEL      RECORD       STANDARD
000090     VALUE      OF           IDENTIFICATION      "BPL-PRN".
000100 01  BPLPRN-REC.
000110     02  BPLPRN-KEY.
000120         03  BPLPRN-PG       PIC 9(02).
000130         03  BPLPRN-LNO      PIC 9(03).
000140     02  BPLPRN-GYO          PIC 9(01).
           02  BPLPRN-GKB          PIC 9(01).
           02  BPLPRN-NM           PIC N(10).
           02  BPLPRN-UKB          PIC X(01).
           02  BPLPRN-IKB          PIC 9(01).
           02  BPLPRN-ITEM.
             03  BPLPRN-AM         PIC S9(11)
                 OCCURS            5   TIMES.
           02  FILLER              PIC X(18).

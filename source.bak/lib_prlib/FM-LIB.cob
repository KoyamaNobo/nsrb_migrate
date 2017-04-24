000010*                                                 *
000020*        FMENUE  FILE        512 / 1  IS KEY=05   *
000030*                                                 *
000040 FD  FM
000050     BLOCK   CONTAINS        1       RECORDS
000060     LABEL   RECORD                  STANDARD
000070     VALUE   OF  IDENTIFICATION  IS  "FMENUE".
000080 01  FM-REC.
000090     02  FM-KEY.
000100       03  FM-LEBEL.
000110         04  FM-L1       PIC X(1).
000120         04  FM-L2       PIC X(1).
000130         04  FM-L3       PIC X(1).
000140         04  FM-L4       PIC X(1).
000150       03  FM-SEQ        PIC 9(1).
000160     02  FM-ITEM.
000170       03  FM-NOITEM OCCURS  13  TIMES.
000180         04  FM-NO       PIC X(2).
000190         04  FM-NAME.
                 05  FM-NM     PIC N(10).
000200         04  FM-PRIRNK   PIC 9(1).
000210         04  FM-KAKUNIN  PIC X(1).
000220         04  FM-PROG.
000230           05  FM-PRO1   PIC X(2).
000240           05  FM-PRO2   PIC X(4).
000250         04  FM-REVCD    PIC 9(1).
000260         04  FM-CHECK.
000270           05  FM-REVLEB PIC 9(1).
000280           05  FM-CRELEB PIC 9(1).
000290           05  FM-DOBUL  PIC 9(1).
000300         04  FM-FIL      PIC X(4).
000310     02  FM-SCRCD        PIC X(4).
000320     02  FM-SCRNO        PIC 9(2).
000330     02  FM-DKB          PIC X(1).
000340     02  FILLER          PIC X(6).

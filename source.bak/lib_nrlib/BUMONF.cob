000010*    ÌÞÓÝÒ² Ì§²Ù        *
000020 FD  BNM
000030     BLOCK      CONTAINS     4      RECORDS
000040     LABEL      RECORD       STANDARD
000050     VALUE      OF           IDENTIFICATION      "BUMON-K ".
000060 01  BNM-REC.
000070     02  BNM-KEY.
000080         03  BNM-BU      PIC 9(02).
000090         03  BNM-KA      PIC 9(02).
000100     02  BNMNM           PIC X(20).
000110     02  BNMNMN  REDEFINES  BNMNM  PIC N(10).                     A 90.11
000120     02  BNM-PL.
000130         03  BNMPLLB  OCCURS 3   TIMES.
000140             04  BNM-PLPG PIC 9(02).
000150             04  BNM-PLLN PIC 9(01).
000160     02  BNM-KH.
000170         03  BNM-KHLB OCCURS 6   TIMES.
000180             04  BNM-KHPG PIC 9(02).
000190             04  BNM-KHLN PIC 9(01).
000200     02  BNM-BUMONKBN     PIC 9(01).
000210     02  BNM-GN.                                                  A 90.12
000220         03  BNM-GNLB OCCURS 3  TIMES.                            A 90.12
000230             04  BNM-GNPG PIC 9(2).                               A 90.12
000240             04  BNM-GNLN PIC 9.                                  A 90.12
000250     02  FILLER          PIC X(03).                               H 90.12

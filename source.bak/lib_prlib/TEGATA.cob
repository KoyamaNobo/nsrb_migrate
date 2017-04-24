000010*    Ã¶ÞÀ  Ï½À°         *    (102/5)
000020 FD  TM
000030     BLOCK      CONTAINS     5      RECORDS
000040     LABEL      RECORD       STANDARD
000050     VALUE      OF           IDENTIFICATION      "TEGATA-K".
000060 01  TM-REC.
000070     02  TM-KEY          PIC X(8).
000080     02  BANKCD          PIC 9(4).                                H 90.12
000090     02  CUSTCD          PIC 9(5).
000100     02  FURYMD          PIC 9(6).
000110     02  KESYMD          PIC 9(6).
000120     02  AMOUNT          PIC S9(10).
000130     02  TEGTKB          PIC 9.
000140     02  TORYMD.
000150       03  TYY           PIC 99.
000160       03  TMM           PIC 99.
000170       03  TDD           PIC 99.
000180     02  MAWASI          PIC 9(5).
000190     02  HONSHICD        PIC 9(3).
000200     02  FURNM           PIC X(20).
000210     02  FURNMN  REDEFINES  FURNM     PIC N(10).                  A 90.11
000220     02  FURBNK          PIC X(20).
000230     02  FURBNKN  REDEFINES  FURBNK   PIC N(10).                  A 90.11
000240     02  FILLER          PIC X(08).                               H 90.12

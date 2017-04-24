000010*    ÄØË·»· Ï½À°        *
000020 FD  TK
000030*****BLOCK      CONTAINS     8      RECORDS                       D.990602
000040     BLOCK      CONTAINS     6      RECORDS                       I.990602
000050     LABEL      RECORD       STANDARD
000060     VALUE      OF           IDENTIFICATION      "NFTORI-M".
000070 01  TK-REC.
000080     02  TK-KEY.                                                  H 90.12
000090         03  TK-CD       PIC 9(5).                                H 90.12
000100         03  TK-CDD  REDEFINES  TK-CD.                            I.000316
000110           04  TK-CD1    PIC 9(01).                               I.000316
000120           04  TK-CD2    PIC 9(04).                               I.000316
000130     02  TK-NAME         PIC X(20).
000140     02  TK-NAMEN  REDEFINES  TK-NAME      PIC N(10).
000150     02  TK-PRC          PIC 9(2).                                I.971105
000160*****02  FILLER          PIC X(1).                                D.990602
000170     02  TK-BKC          PIC 9(2).                                I.990602
000180     02  TK-TCD          PIC 9(4).                                I.991019
000190     02  TK-WNK          PIC 9(1).                                I.991207
000200     02  TK-SS           PIC 9(2).                                I.000819
000210     02  FILLER          PIC X(2).                                I.000819
000220*****02  FILLER          PIC X(4).                                D.000819
000230*****02  FILLER          PIC X(5).                                D.991207
000240*****02  FILLER          PIC X(9).                                D.991019
000250     02  TK-NG           PIC 9(4).                                I.971105
000260*****02  FILLER          PIC X(7).                                D.971105

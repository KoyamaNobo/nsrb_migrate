000010****************************************************************
000020*                                                              *
000030*               < ÌÞÍÞÂ  ¿Ý´·  Ì§²Ù >     * 85 REC / 3 B *     *
000040*                                                              *
000050****************************************************************
000060 FD  BU-F
000070     BLOCK      CONTAINS     3      RECORDS
000080     LABEL      RECORD       STANDARD
000090     VALUE      OF           IDENTIFICATION      "BUPL-K".
000100 01  BU-REC.
000110     02  BU-KEY.
000120         03  BU-BUMN.
000130             04  BU-BUCD     PIC 9(02).                           ÌÞÓÝº°ÄÞ
000140             04  BU-YOBI     PIC 9(02).                           ÖËÞ
000150         03  BU-LINNO        PIC 9(03).                           ×²ÝNO
000160     02      BU-KAIP         PIC 9(01).
000170     02      BU-GOKBN        PIC 9(01).
000180     02      BU-KMKNM        PIC N(10).
000190     02  BU-ZEN.
000200         03  BU-ZENKI        PIC S9(11).
000210         03  BU-TOUKI        PIC S9(11).
000220     02  BU-DOG.
000230         03  BU-DOGET        PIC S9(11).
000240         03  BU-TOGET        PIC S9(11).
000250     02      BU-URKBN        PIC X(01).
000260     02      BU-PRKBN        PIC 9(01).
000270     02      BU-TBKBN        PIC 9(01).
000280     02      F               PIC X(09).

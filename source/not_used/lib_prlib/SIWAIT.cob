000010*    Ã¶ÞÀ ²ÝÌß¯Ä        *     (102/5)
000020 FD  TGI
000030     BLOCK      CONTAINS     5      RECORDS                       H 90.12
000040     LABEL      RECORD       STANDARD
000050     VALUE      OF           IDENTIFICATION      "TEGATA-I".
000060 01  TGI-REC.
000070     02  TGIYMD          PIC 9(6).                                H 90.12
000080     02  TGIJNO          PIC 9(6).                                H 90.12
000090     02  TGILNO          PIC 99.                                  H 90.12
000100     02  TGIDR-CR        PIC 9.                                   H 90.12
000110     02  TGI-KEY.                                                 H 90.12
000120       03  TGI-NO        PIC X(8).                                H 90.12
000130     02  TGIFYMD         PIC 9(6).
000140     02  TGIKYMD         PIC 9(6).
000150***  02  TGIHSCD         PIC 9(3).                                D 90.12
000160     02  FURNIN.
000170       03  TGIFCD        PIC 9(5).
000180       03  TGIFNM        PIC N(10).
000190     02  FBAN.
000200       03  TGIFBCD       PIC 9(5).
000210       03  TGIFBNM       PIC N(10).
000220     02  FILLER          PIC X(16).                               A 90.12
000230     02  TGIDEL          PIC X(01).                               A 90.12

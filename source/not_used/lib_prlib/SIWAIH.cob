000010*    �ܹ INPUT HEADER   *
000020 FD  SDH
000030     BLOCK      CONTAINS     12     RECORDS
000040     LABEL      RECORD       STANDARD
000050     VALUE      OF           IDENTIFICATION      "SIWAKE-IH".
000060 01  SDH-REC.
000070     02  SDH-KEY.
000080       03  SDHYMD        PIC 9(6).
000090       03  SDHJNO        PIC 9(6).
000100     02  SDHTKCD         PIC 9(5).
000110     02  FILLER          PIC X(3).

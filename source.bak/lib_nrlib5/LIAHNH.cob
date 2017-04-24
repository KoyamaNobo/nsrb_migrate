000010************************************************
000020*****    赤ちゃん本舗納品先ファイル        *****
000030*****           ( AHNHF )   64/4           *****
000040************************************************
000050 FD  AHNHF
000060     BLOCK  4 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "AHNHF".
000090 01  AHNH-R.
000100     02  AHNH-KEY.
000110       03  AHNH-STC     PIC  9(007).
000120     02  AHNH-NHSN      PIC  N(016).
000130     02  AHNH-CTC       PIC  9(007).
000140     02  AHNH-CSC.
000150       03  AHNH-TCD     PIC  9(004).
000160       03  AHNH-CCD     PIC  9(003).
000170     02  AHNH-HP        PIC  X(001).                              I.091026
000180     02  F              PIC  X(010).                              I.091026
000190*****02  F              PIC  X(011).                              D.091026

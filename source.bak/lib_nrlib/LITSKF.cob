000010**************************************
000020*****     得意先請求ファイル     *****
000030*****      (  TSKF 256/1  )      *****
000040**************************************
000050 FD  TSKF
000060     BLOCK  1 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "TSKF".
000090 01  TSK-R.
000100     02  TSK-KEY.                                                 KEY
000110       03  TSK-TCD      PIC  9(004).                              得意先C
000120     02  TSK-ZSDD.                                                前回請求
000130       03  TSK-ZSD   OCCURS   5.
000140         04  TSK-HTS    PIC S9(009).
000150         04  TSK-SZS    PIC S9(007).
000160         04  TSK-ZNGP   PIC  9(008).
000170     02  TSK-KKD.                                                 今回請求
000180       03  TSK-HTN      PIC S9(009).
000190       03  TSK-SZN      PIC S9(007).
000200       03  TSK-HTC      PIC S9(007).
000210       03  TSK-SZC      PIC S9(005).
000220       03  TSK-HTU      PIC S9(009).
000230       03  TSK-SZU      PIC S9(007).
000240       03  TSK-KNGP     PIC  9(008).
000250     02  F              PIC  X(008).
000260     02  TSK-TNC        PIC  9(002).                              担当Ｃ
000270     02  TSK-BMC        PIC  9(001).                              部門C
000280     02  TSK-DCC        PIC  9(001).                              I.010220
000290     02  F              PIC  X(068).                              I.010220
000300*****02  TSK-DCN        PIC  9(003).                              D.010220
000310*****02  F              PIC  X(066).                              D.010220

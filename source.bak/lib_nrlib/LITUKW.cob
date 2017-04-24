000010********************************************
000020*****     得意先別売掛管理ファイル     *****
000030*****         (  TUKFW 64/4  )         *****
000040********************************************
000050 FD  TUKF
000060     BLOCK  4 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "TUKFW".
000090 01  TUK-R.
000100     02  TUK-KEY.                                                 KEY
000110       03  TUK-TCD      PIC  9(004).                              得意先C
000120       03  TUK-DAI.                                               台帳№
000130         04  TUK-DN1    PIC  X(003).
000140         04  TUK-DN2    PIC  9(006).
000150         04  TUK-DN3    PIC  9(001).
000160     02  F              PIC  9(004).                              I.030916
000170     02  TUK-DATE       PIC  9(008).                              日付
000180     02  TUK-NGP   REDEFINES TUK-DATE.
000190       03  TUK-NG.
000200         04  TUK-NEN    PIC  9(004).
000210         04  TUK-GET    PIC  9(002).
000220       03  TUK-PEY      PIC  9(002).
000230     02  TUK-DC         PIC  9(001).                              区分
000240     02  TUK-DNO        PIC  X(006).                              I.030916
000250     02  TUK-GNO        PIC  9(001).                              I.030916
000260     02  TUK-KIN        PIC S9(009).                              金額
000270     02  TUK-SHZ        PIC S9(007).                              消費税
000280*****02  TUK-DNO        PIC  X(006).                              D.030916
000290     02  TUK-SKD        PIC  9(008).                              I.000725
000300     02  TUK-SKDD  REDEFINES TUK-SKD.                             I.001201
000310       03  TUK-SNEN     PIC  9(004).                              I.001201
000320       03  TUK-SGET     PIC  9(002).                              I.001201
000330       03  TUK-SPEY     PIC  9(002).                              I.001201
000340*****02  TUK-DCN        PIC  9(003).                              D.030916
000350     02  TUK-DCC        PIC  9(001).                              I.030916
000360     02  TUK-TNC        PIC  9(002).                              I.000725
000370     02  TUK-BMC        PIC  9(001).                              部門C
000380     02  F              PIC  X(002).                              I.030916
000390*****02  F              PIC  X(005).                              D.030916

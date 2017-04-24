000010********************************************
000020*****     ìæà”êÊï îÑä|ä«óùÉtÉ@ÉCÉã     *****
000030*****         (  TUKF  64/4  )         *****
000040********************************************
000050 FD  TUKF
000060     BLOCK  4 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "TUKF".
000090 01  TUK-R.
000100     02  TUK-KEY.                                                 KEY
000110       03  TUK-TCD      PIC  9(004).                              ìæà”êÊC
000120       03  TUK-DAI.                                               ë‰í†áÇ
000130         04  TUK-DN1    PIC  X(003).
000140         04  TUK-DN2    PIC  9(006).
000150         04  TUK-DN3    PIC  9(001).
000160     02  TUK-DATE       PIC  9(008).                              ì˙ït
000170     02  TUK-NGP   REDEFINES TUK-DATE.
000180       03  TUK-NG.
000190         04  TUK-NEN    PIC  9(004).
000200         04  TUK-GET    PIC  9(002).
000210       03  TUK-PEY      PIC  9(002).
000220     02  TUK-DC         PIC  9(001).                              ãÊï™
000230     02  TUK-KIN        PIC S9(009).                              ã‡äz
000240     02  TUK-SHZ        PIC S9(007).                              è¡îÔê≈
000250     02  TUK-DNO        PIC  X(006).                              ì`ï[áÇ
000260     02  TUK-SKD        PIC  9(008).                              I.000725
000270     02  TUK-SKDD  REDEFINES TUK-SKD.                             I.001201
000280       03  TUK-SNEN     PIC  9(004).                              I.001201
000290       03  TUK-SGET     PIC  9(002).                              I.001201
000300       03  TUK-SPEY     PIC  9(002).                              I.001201
000310*****02  TUK-DCN        PIC  9(003).                              D.010220
000320     02  TUK-DCC        PIC  9(001).                              I.010220
000330     02  F              PIC  X(002).                              I.010220
000340     02  TUK-TNC        PIC  9(002).                              I.000725
000350     02  TUK-BMC        PIC  9(001).                              ïîñÂC
000360     02  F              PIC  X(005).

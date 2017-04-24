000010********************************************
000020*****     ìæà”êÊï îÑä|ä«óùÉtÉ@ÉCÉã     *****
000030*****         (  TUKF  64/4  )         *****
000040********************************************
000050 FD  TUKF
000060     BLOCK  4 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "TUKF1"
000090     ALTERNATE IDENTIFICATION "TUKF2".
000100 01  TUK-R.
000110     02  TUK-KEY.                                                 KEY
000120       03  TUK-TCD      PIC  9(004).                              ìæà”êÊC
000130       03  TUK-DAI.                                               ë‰í†áÇ
000140         04  TUK-DN1    PIC  X(003).
000150         04  TUK-DN2    PIC  9(006).
000160         04  TUK-DN3    PIC  9(001).
000170     02  TUK-KEY2.                                                I.030912
000180       03  TUK-TCD2     PIC  9(004).                              I.030912
000190       03  TUK-DATE     PIC  9(008).                              I.030912
000200       03  TUK-NGP   REDEFINES TUK-DATE.                          I.030912
000210         04  TUK-NG.                                              I.030912
000220           05  TUK-NEN  PIC  9(004).                              I.030912
000230           05  TUK-GET  PIC  9(002).                              I.030912
000240         04  TUK-PEY    PIC  9(002).                              I.030912
000250       03  TUK-DC       PIC  9(001).                              I.030912
000260       03  TUK-DNO      PIC  X(006).                              I.030912
000270       03  TUK-GNO      PIC  9(001).                              I.030912
000280*****02  TUK-DATE       PIC  9(008).                              D.030912
000290*****02  TUK-NGP   REDEFINES TUK-DATE.                            D.030912
000300*****  03  TUK-NG.                                                D.030912
000310*****    04  TUK-NEN    PIC  9(004).                              D.030912
000320*****    04  TUK-GET    PIC  9(002).                              D.030912
000330*****  03  TUK-PEY      PIC  9(002).                              D.030912
000340*****02  TUK-DC         PIC  9(001).                              D.030912
000350     02  TUK-KIN        PIC S9(009).                              ã‡äz
000360     02  TUK-SHZ        PIC S9(007).                              è¡îÔê≈
000370*****02  TUK-DNO        PIC  X(006).                              D.030912
000380     02  TUK-SKD        PIC  9(008).                              I.000725
000390     02  TUK-SKDD  REDEFINES TUK-SKD.                             I.001201
000400       03  TUK-SNEN     PIC  9(004).                              I.001201
000410       03  TUK-SGET     PIC  9(002).                              I.001201
000420       03  TUK-SPEY     PIC  9(002).                              I.001201
000430*****02  TUK-DCN        PIC  9(003).                              D.010220
000440     02  TUK-DCC        PIC  9(001).                              I.010220
000450*****02  F              PIC  X(002).                              D.030912
000460     02  TUK-TNC        PIC  9(002).                              I.000725
000470     02  TUK-BMC        PIC  9(001).                              ïîñÂC
000480     02  F              PIC  X(002).                              I.030912
000490*****02  F              PIC  X(005).                              D.030912

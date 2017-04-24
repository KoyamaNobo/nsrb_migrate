000010****************************************
000020*****     非請求データファイル     *****
000030*****       (  SKDKF 192/4  )      *****
000040****************************************
000050 FD  SKDKF
000060     BLOCK  4 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "SKDKF".
000090 01  SKDK-R.
000100     02  SKDK-KEY.                                                KEY
000110       03  SKDK-TCD     PIC  9(004).                              得意先C
000120       03  SKDK-DATE    PIC  9(008).                              日付
000130       03  SKDK-NGP   REDEFINES SKDK-DATE.
000140         04  SKDK-NG.
000150           05  SKDK-NEN PIC  9(004).
000160           05  SKDK-GET PIC  9(002).
000170         04  SKDK-PEY   PIC  9(002).
000180       03  SKDK-NGPL  REDEFINES SKDK-DATE.
000190         04  F          PIC  9(002).
000200         04  SKDK-NGPS  PIC  9(006).
000210       03  SKDK-NGPD  REDEFINES SKDK-DATE.
000220         04  F          PIC  9(004).
000230         04  SKDK-GP    PIC  9(004).
000240       03  SKDK-DTC     PIC  9(001).                              区分
000250       03  SKDK-DNO     PIC  9(006).                              伝票№
000260       03  SKDK-GNO     PIC  9(001).                              　行№
000270     02  SKDK-HCD       PIC  9(006).                              品名Ｃ
000280     02  SKDK-HCDD  REDEFINES SKDK-HCD.
000290       03  SKDK-KCD     PIC  X(005).
000300       03  F            PIC  X(001).
000310     02  SKDK-SU        PIC S9(006)V9(02).                        数量
000320     02  SKDK-T         PIC S9(006)V9(02).                        単価
000330     02  SKDK-KIN       PIC S9(009).                              金額
000340     02  SKDK-DC        PIC  9(001).                              伝区
000350     02  SKDK-CSC       PIC  9(001).
000360     02  SKDK-SKD       PIC  9(008).                              請求日
000370     02  SKDK-SKDD  REDEFINES SKDK-SKD.
000380       03  SKDK-SNEN    PIC  9(004).
000390       03  SKDK-SGET    PIC  9(002).
000400       03  SKDK-SPEY    PIC  9(002).
000410     02  SKDK-TNC       PIC  9(002).                              担当Ｃ
000420     02  SKDK-BMC       PIC  9(001).                              部門C
000430     02  SKDK-DCC       PIC  9(001).
000440     02  F              PIC  X(002).
000450     02  SKDK-TCD2      PIC  9(004).
000460     02  SKDK-CCD       PIC  9(003).                              直送№
000470     02  SKDK-BI        PIC  N(024).                              備考
000480     02  SKDK-HNO       PIC  9(006).                              I.090105
000490     02  F              PIC  X(030).                              I.090105
000500*****02  SKDK-HNO.                                                D.090105
000510*****  03  SKDK-HNO1    PIC  9(006).                              D.090105
000520*****  03  SKDK-HNO2    PIC  9(006).                              D.090105
000530*****  03  SKDK-HNO3    PIC  9(006).                              D.090105
000540*****  03  SKDK-HNO4    PIC  9(006).                              D.090105
000550*****  03  SKDK-HNO5    PIC  9(006).                              D.090105
000560*****  03  SKDK-HNO6    PIC  9(006).                              D.090105
000570     02  SKDK-SHZ       PIC S9(007).                              消費税
000580     02  SKDK-KSU       PIC  9(003).                              個数
000590     02  SKDK-JCD       PIC  9(006).
000600     02  F              PIC  X(012).
000610     02  SKDK-SNO       PIC  9(006).

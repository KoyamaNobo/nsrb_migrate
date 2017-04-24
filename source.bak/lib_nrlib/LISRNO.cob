000010***************************************************
000020*****     êªë¢ÉçÉbÉgáÇÅ@É}ÉXÉ^Å[Å@            *****
000030*****     (  SRNOM 768/1 , KEY 1-10  )        *****
000040***************************************************
000050 FD  SRNOM
000060     BLOCK  1 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "SRNOM".
000090 01  SRNO-R.
000100     02  SRNO-KEY.
000110       03  SRNO-RNO.
000120         04  SRNO-NEN   PIC  9(004).                              îN
000130         04  SRNO-GET   PIC  9(002).                              åé
000140         04  SRNO-NO    PIC  9(003).                              áÇ
000150         04  SRNO-SUB   PIC  9(001).                              é}î‘
000160     02  SRNO-HCD       PIC  9(006).                              ∫∞ƒﬁ
000170     02  SRNO-ASYS.                                               ê¨å^ó\íË
000180       03  SRNO-SYSD  OCCURS   4.
000190         04  SRNO-SYD   OCCURS  10.
000200           05  SRNO-SY  PIC S9(003).
000210     02  SRNO-ASGS.                                               ê¨å^é¿ê—
000220       03  SRNO-SGSD  OCCURS   4.
000230         04  SRNO-SGD   OCCURS  10.
000240           05  SRNO-SG  PIC S9(003).
000250     02  SRNO-AKSS.                                               åüç∏é¿ê—
000260       03  SRNO-KSSD  OCCURS   4.
000270         04  SRNO-KSD   OCCURS  10.
000280           05  SRNO-KS  PIC S9(003).
000290     02  SRNO-AKFS.                                               ïsó«é¿ê—
000300       03  SRNO-KFSD  OCCURS   4.
000310         04  SRNO-KFD   OCCURS  10.
000320           05  SRNO-KF  PIC S9(003).
000330     02  SRNO-ACSS.                                               í≤êÆ
000340       03  SRNO-CSSD  OCCURS   4.
000350         04  SRNO-CSD   OCCURS  10.
000360           05  SRNO-CS  PIC S9(003).
000370     02  SRNO-HNGP      PIC  9(008).                              ê¨å^äJén
000380     02  SRNO-HNGPD REDEFINES SRNO-HNGP.
000390       03  SRNO-HNEN    PIC  9(004).
000400       03  SRNO-HGP.
000410         04  SRNO-HGET  PIC  9(002).
000420         04  SRNO-HPEY  PIC  9(002).
000430     02  SRNO-ONGP      PIC  9(008).                              ê¨å^èIóπ
000440     02  SRNO-ONGPD REDEFINES SRNO-ONGP.
000450       03  SRNO-ONEN    PIC  9(004).
000460       03  SRNO-OGP.
000470         04  SRNO-OGET  PIC  9(002).
000480         04  SRNO-OPEY  PIC  9(002).
000490     02  SRNO-YNGP      PIC  9(008).                              èoâ◊ì˙
000500     02  SRNO-YNGPD REDEFINES SRNO-YNGP.
000510       03  SRNO-YNEN    PIC  9(004).
000520       03  SRNO-YGP.
000530         04  SRNO-YGET  PIC  9(002).
000540         04  SRNO-YPEY  PIC  9(002).
000550     02  SRNO-SNGP      PIC  9(008).                              ê¨å^äÆóπ
000560     02  SRNO-SNGPD REDEFINES SRNO-SNGP.
000570       03  SRNO-SNEN    PIC  9(004).
000580       03  SRNO-SGP.
000590         04  SRNO-SGET  PIC  9(002).
000600         04  SRNO-SPEY  PIC  9(002).
000610     02  SRNO-KNGP      PIC  9(008).                              åüç∏äÆóπ
000620     02  SRNO-KNGPD REDEFINES SRNO-KNGP.
000630       03  SRNO-KNEN    PIC  9(004).
000640       03  SRNO-KGP.
000650         04  SRNO-KGET  PIC  9(002).
000660         04  SRNO-KPEY  PIC  9(002).
000670     02  SRNO-BI        PIC  N(014).                              I.050317
000680*****02  SRNO-HNA       PIC  N(018).                              D.040202
000690*****02  SRNO-COR       PIC  N(018).                              D.040202
000700*****02  SRNO-CORD  REDEFINES SRNO-COR.                           D.040202
000710*****  03  SRNO-COR1    PIC  N(011).                              D.040202
000720*****  03  SRNO-COR2    PIC  N(007).                              D.040202
000730*****02  F              PIC  X(023).                              D.040202
000740*****02  F              PIC  X(095).                              D.050317
000750     02  F              PIC  X(067).                              I.050317
000760     02  SRNO-ANG.                                                ìoò^îNåé
000770       03  SRNO-ANEN    PIC  9(004).
000780       03  SRNO-AGET    PIC  9(002).
000790     02  SRNO-HSC       PIC  9(001).                              ñDêªãÊï™
000800     02  SRNO-DC        PIC  9(001).                              í«â¡ãÊï™
000810     02  SRNO-PNO.                                                çÏï\áÇ
000820       03  SRNO-PNEN    PIC  9(004).
000830       03  SRNO-PGET    PIC  9(002).
000840       03  SRNO-PSNO    PIC  9(003).

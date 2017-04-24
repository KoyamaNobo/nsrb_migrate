000010***************************************************
000020*****     ê∂éYó\íËì¸óÕÉtÉ@ÉCÉã  Å@            *****
000030*****     (  HSYIF 256/1 , KEY 1-20  )        *****
000040***************************************************
000050 FD  HSYIF
000060     BLOCK  1 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "HSYIF".
000090 01  HSYI-R.
000100     02  HSYI-KEY.
000110       03  HSYI-RNO.
000120         04  HSYI-NEN   PIC  9(004).                              îN
000130         04  HSYI-GET   PIC  9(002).                              åé
000140         04  HSYI-NO    PIC  9(003).                              áÇ
000150         04  HSYI-SUB   PIC  9(001).                              é}î‘
000160       03  HSYI-DATE    PIC  9(008).                              ì˙ït
000170       03  HSYI-NGPD  REDEFINES HSYI-DATE.
000180         04  F          PIC  9(002).
000190         04  HSYI-NGPS  PIC  9(006).
000200       03  HSYI-SEQ     PIC  9(002).                              SEQ
000210     02  HSYI-HCD       PIC  9(006).                              ∫∞ƒﬁ
000220     02  HSYI-ASUD.                                               ê¨å^ó\íË
000230       03  HSYI-ASU   OCCURS   4.
000240         04  HSYI-SUD   OCCURS  10.
000250           05  HSYI-SU  PIC S9(003).
000260     02  HSYI-HNGP      PIC  9(008).                              ê¨å^äJén
000270     02  HSYI-HNGPD REDEFINES HSYI-HNGP.
000280       03  HSYI-HNEN    PIC  9(004).
000290       03  HSYI-HGP.
000300         04  HSYI-HGET  PIC  9(002).
000310         04  HSYI-HPEY  PIC  9(002).
000320     02  HSYI-ONGP      PIC  9(008).                              ê¨å^èIóπ
000330     02  HSYI-ONGPD REDEFINES HSYI-ONGP.
000340       03  HSYI-ONEN    PIC  9(004).
000350       03  HSYI-OGP.
000360         04  HSYI-OGET  PIC  9(002).
000370         04  HSYI-OPEY  PIC  9(002).
000380     02  HSYI-YNGP      PIC  9(008).                              èoâ◊ì˙
000390     02  HSYI-YNGPD REDEFINES HSYI-YNGP.
000400       03  HSYI-YNEN    PIC  9(004).
000410       03  HSYI-YGP.
000420         04  HSYI-YGET  PIC  9(002).
000430         04  HSYI-YPEY  PIC  9(002).
000440     02  HSYI-SNGP      PIC  9(008).                              ê¨å^äÆóπ
000450     02  HSYI-SNGPD REDEFINES HSYI-SNGP.
000460       03  HSYI-SNEN    PIC  9(004).
000470       03  HSYI-SGP.
000480         04  HSYI-SGET  PIC  9(002).
000490         04  HSYI-SPEY  PIC  9(002).
000500     02  HSYI-KNGP      PIC  9(008).                              åüç∏äÆóπ
000510     02  HSYI-KNGPD REDEFINES HSYI-KNGP.
000520       03  HSYI-KNEN    PIC  9(004).
000530       03  HSYI-KGP.
000540         04  HSYI-KGET  PIC  9(002).
000550         04  HSYI-KPEY  PIC  9(002).
000560     02  HSYI-BI        PIC  N(014).                              îıçl
000570     02  F              PIC  X(023).
000580     02  HSYI-ANG.                                                ìoò^îNåé
000590       03  HSYI-ANEN    PIC  9(004).
000600       03  HSYI-AGET    PIC  9(002).
000610     02  HSYI-HSC       PIC  9(001).                              ñDêªãÊï™
000620     02  HSYI-DC        PIC  9(001).                              í«â¡ãÊï™
000630     02  HSYI-PNO.                                                çÏï\áÇ
000640       03  HSYI-PNEN    PIC  9(004).
000650       03  HSYI-PGET    PIC  9(002).
000660       03  HSYI-PSNO    PIC  9(003).
000670     02  HSYI-ACT       PIC  9(001).                              ACT
000680     02  HSYI-PC        PIC  9(001).                              àÛéö

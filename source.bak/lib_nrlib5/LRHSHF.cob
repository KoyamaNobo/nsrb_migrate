000010****************************************
000020*****     履物製品発注ファイル     *****
000030*****   (  HSHF-RDB    768/1   )   *****
000040****************************************
000050 FD  HSHF
000060     BLOCK  1 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "HSHF-RDB".
000090 01  HSH-R.
000100     02  HSH-KEY3.
000110       03  HSH-SCD        PIC  9(004).
000120       03  HSH-KEY2.
000130         04  HSH-HCD      PIC  9(006).
000140         04  HSH-KEY      PIC  X(008).
000150         04  HSH-RNO   REDEFINES HSH-KEY.
000160           05  HSH-RSN    PIC  9(002).
000170           05  HSH-RNG    PIC  9(004).
000180           05  HSH-RND    PIC  9(002).
000190     02  HSH-HDD          PIC  9(008).
000200     02  HSH-HDDD  REDEFINES HSH-HDD.
000210       03  HSH-HNG.
000220         04  HSH-HNEN     PIC  9(004).
000230         04  HSH-HNENL REDEFINES HSH-HNEN.
000240           05  HSH-HNEN1  PIC  9(002).
000250           05  HSH-HNEN2  PIC  9(002).
000260         04  HSH-HGET     PIC  9(002).
000270       03  HSH-HPEY       PIC  9(002).
000280     02  HSH-HDDL  REDEFINES HSH-HDD.
000290       03  F              PIC  9(002).
000300       03  HSH-HNGPS      PIC  9(006).
000310       03  HSH-HNGPL REDEFINES HSH-HNGPS.                         I.030228
000320         04  F            PIC  9(002).                            I.030228
000330         04  HSH-HGP      PIC  9(004).                            I.030228
000340     02  HSH-AHSUD.
000350       03  HSH-HSUD  OCCURS   4.                                  ｻｲｽﾞ
000360         04  HSH-AHSU  OCCURS  10.
000370           05  HSH-HSU    PIC S9(004).                            数量
000380     02  HSH-ANSUD.
000390       03  HSH-NSUD  OCCURS   4.                                  ｻｲｽﾞ
000400         04  HSH-ANSU  OCCURS  10.
000410           05  HSH-NSU    PIC S9(004).                            数量
000420     02  HSH-AISUD.
000430       03  HSH-ISUD  OCCURS   4.                                  ｻｲｽﾞ
000440         04  HSH-AISU  OCCURS  10.
000450           05  HSH-ISU    PIC S9(004).                            数量
000460     02  HSH-T            PIC  9(005).
000470     02  HSH-NDD          PIC  9(008).
000480     02  HSH-NDDD  REDEFINES HSH-NDD.
000490       03  HSH-NNG.
000500         04  HSH-NNEN     PIC  9(004).
000510         04  HSH-NNENL REDEFINES HSH-NNEN.
000520           05  HSH-NNEN1  PIC  9(002).
000530           05  HSH-NNEN2  PIC  9(002).
000540         04  HSH-NGET     PIC  9(002).
000550       03  HSH-NPEY       PIC  9(002).
000560     02  HSH-NDDL  REDEFINES HSH-NDD.
000570       03  F              PIC  9(002).
000580       03  HSH-NNGPS      PIC  9(006).
000590     02  HSH-ENGP         PIC  9(006).
000600     02  HSH-ICHK         PIC  9(001).                            I.040520
000610     02  F                PIC  X(242).                            I.040520
000620*****02  F                PIC  X(243).                            D.040520

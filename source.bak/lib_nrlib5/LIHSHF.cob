000010****************************************
000020*****     履物製品発注ファイル     *****
000030*****   (  DATA  HSHFD  768/1  )   *****
000040*****   (  KEY1  HSHF1  11-10  )   *****
000050*****   (  KEY2  HSHF2   4-16  )   *****
000060*****   (  KEY3  HSHF3   1-20  )   *****
000070****************************************
000080 FD  HSHF
000090     BLOCK  1 RECORDS
000100     LABEL RECORD IS STANDARD
000110     VALUE OF IDENTIFICATION "HSHF1"
000120     ALTERNATE IDENTIFICATION "HSHF2"
000130     ALTERNATE IDENTIFICATION "HSHF3".
000140 01  HSH-R.
000150     02  HSH-KEY3.
000160       03  HSH-SCD        PIC  9(004).
000170       03  HSH-KEY2.
000180         04  HSH-HCD      PIC  9(006).
000190         04  HSH-KEY      PIC  X(008).
000200         04  HSH-RNO   REDEFINES HSH-KEY.
000210           05  HSH-RSN    PIC  9(002).
000220           05  HSH-RNG    PIC  9(004).
000230           05  HSH-RND    PIC  9(002).
000240     02  HSH-HDD          PIC  9(008).
000250     02  HSH-HDDD  REDEFINES HSH-HDD.
000260       03  HSH-HNG.
000270         04  HSH-HNEN     PIC  9(004).
000280         04  HSH-HNENL REDEFINES HSH-HNEN.
000290           05  HSH-HNEN1  PIC  9(002).
000300           05  HSH-HNEN2  PIC  9(002).
000310         04  HSH-HGET     PIC  9(002).
000320       03  HSH-HPEY       PIC  9(002).
000330     02  HSH-HDDL  REDEFINES HSH-HDD.
000340       03  F              PIC  9(002).
000350       03  HSH-HNGPS      PIC  9(006).
000360     02  HSH-AHSUD.
000370       03  HSH-HSUD  OCCURS   4.                                  ｻｲｽﾞ
000380         04  HSH-AHSU  OCCURS  10.
000390           05  HSH-HSU    PIC S9(004).                            数量
000400     02  HSH-ANSUD.
000410       03  HSH-NSUD  OCCURS   4.                                  ｻｲｽﾞ
000420         04  HSH-ANSU  OCCURS  10.
000430           05  HSH-NSU    PIC S9(004).                            数量
000440     02  HSH-AISUD.
000450       03  HSH-ISUD  OCCURS   4.                                  ｻｲｽﾞ
000460         04  HSH-AISU  OCCURS  10.
000470           05  HSH-ISU    PIC S9(004).                            数量
000480     02  HSH-T            PIC  9(005).
000490     02  HSH-NDD          PIC  9(008).
000500     02  HSH-NDDD  REDEFINES HSH-NDD.
000510       03  HSH-NNG.
000520         04  HSH-NNEN     PIC  9(004).
000530         04  HSH-NNENL REDEFINES HSH-NNEN.
000540           05  HSH-NNEN1  PIC  9(002).
000550           05  HSH-NNEN2  PIC  9(002).
000560         04  HSH-NGET     PIC  9(002).
000570       03  HSH-NPEY       PIC  9(002).
000580     02  HSH-NDDL  REDEFINES HSH-NDD.
000590       03  F              PIC  9(002).
000600       03  HSH-NNGPS      PIC  9(006).
000610     02  HSH-ENGP         PIC  9(006).
000620     02  F                PIC  X(243).

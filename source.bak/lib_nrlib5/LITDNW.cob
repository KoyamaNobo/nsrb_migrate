000010************************************************
000020*****    統一伝票ファイル（ワークマン）    *****
000030*****           ( TDNWF )  256/1           *****
000040************************************************
000050 FD  TDNWF
000060     BLOCK  1 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "TDNWF".
000090 01  TDNW-R1.                                                     ﾍｯﾀﾞｰ
000100     02  TDNW1-KEY.
000110       03  TDNW1-STC.
000120         04  TDNW1-STC1 PIC  9(004).                              I.150317
000130         04  TDNW1-SCDD REDEFINES TDNW1-STC1.                     I.150317
000140           05  F          PIC  9(002).                            I.150317
000150           05  TDNW1-SCD  PIC  9(002).                            I.150317
000160         04  TDNW1-STC2 PIC  9(005).                              I.150317
000170         04  TDNW1-TCDD REDEFINES TDNW1-STC2.                     I.150317
000180           05  F          PIC  9(001).                            I.150317
000190           05  TDNW1-TCD  PIC  9(004).                            I.150317
000200*****    04  F          PIC  9(002).                              D.150317
000210*****    04  TDNW1-SCD  PIC  9(002).                              D.150317
000220*****    04  F          PIC  9(002).                              D.100906
000230*****    04  TDNW1-TCD  PIC  9(003).                              D.100906
000240*****    04  F          PIC  9(001).                              D.150317
000250*****    04  TDNW1-TCD  PIC  9(004).                              D.150317
000260       03  TDNW1-DNO.
000270         04  F          PIC  9(002).
000280         04  TDNW1-DNOD PIC  9(007).
000290       03  TDNW1-DGN    PIC  9(002).
000300     02  TDNW1-BC.
000310       03  TDNW1-BCD    PIC  9(002).
000320       03  F            PIC  9(001).
000330     02  TDNW1-SHC      PIC  9(001).
000340     02  TDNW1-DPC      PIC  X(002).
000350     02  TDNW1-HNGP     PIC  9(006).
000360     02  TDNW1-HNGPD REDEFINES TDNW1-HNGP.
000370       03  TDNW1-HNEN   PIC  9(002).
000380       03  TDNW1-HGET   PIC  9(002).
000390       03  TDNW1-HPEY   PIC  9(002).
000400     02  TDNW1-NNGP     PIC  9(006).
000410     02  TDNW1-NNGPD REDEFINES TDNW1-NNGP.
000420       03  TDNW1-NNEN   PIC  9(002).
000430       03  TDNW1-NGET   PIC  9(002).
000440       03  TDNW1-NPEY   PIC  9(002).
000450     02  TDNW1-THC      PIC  9(006).
000460     02  TDNW1-MHC      PIC  X(001).
000470     02  F              PIC  X(001).
000480     02  TDNW1-SNA      PIC  X(020).
000490     02  TDNW1-TNA      PIC  X(020).
000500     02  TDNW1-HCC      PIC  9(001).
000510     02  TDNW1-HSP      PIC  X(001).
000520     02  TDNW1-DHC      PIC  X(001).
000530     02  TDNW1-KHC      PIC  X(001).
000540     02  TDNW1-KCC      PIC  X(001).
000550     02  TDNW1-UBC      PIC  X(001).
000560     02  TDNW1-NCC      PIC  9(001).
000570     02  TDNW1-EDI      PIC  9(001).
000580     02  TDNW1-NKC      PIC  9(001).
000590     02  TDNW1-ZAC      PIC  9(001).
000600*****02  F              PIC  X(159).                              D.070919
000610     02  F              PIC  X(150).                              I.070919
000620     02  TDNW1-HC       PIC  9(001).                              I.070919
000630     02  F              PIC  X(008).                              I.070919
000640     02  TDNW1-PC       PIC  9(001).
000650 01  TDNW-R2.                                                     ﾒｲｻｲ
000660     02  TDNW2-KEY.
000670       03  TDNW2-STC.
000680         04  TDNW2-STC1 PIC  9(004).                              I.150317
000690         04  TDNW2-SCDD REDEFINES TDNW2-STC1.                     I.150317
000700           05  F          PIC  9(002).                            I.150317
000710           05  TDNW2-SCD  PIC  9(002).                            I.150317
000720         04  TDNW2-STC2 PIC  9(005).                              I.150317
000730         04  TDNW2-TCDD REDEFINES TDNW2-STC2.                     I.150317
000740           05  F          PIC  9(001).                            I.150317
000750           05  TDNW2-TCD  PIC  9(004).                            I.150317
000760*****    04  F          PIC  9(002).                              D.150317
000770*****    04  TDNW2-SCD  PIC  9(002).                              D.150317
000780*****    04  F          PIC  9(002).                              D.100906
000790*****    04  TDNW2-TCD  PIC  9(003).                              D.100906
000800*****    04  F          PIC  9(001).                              D.150317
000810*****    04  TDNW2-TCD  PIC  9(004).                              D.150317
000820       03  TDNW2-DNO.
000830         04  F          PIC  9(002).
000840         04  TDNW2-DNOD PIC  9(007).
000850       03  TDNW2-DGN    PIC  9(002).
000860     02  TDNW2-HCD      PIC  X(013).
000870     02  TDNW2-HCDD  REDEFINES  TDNW2-HCD.                        I.100105
000880       03  TDNW2-WCO    PIC  9(007).                              I.100105
000890       03  F            PIC  X(006).                              I.100105
000900     02  TDNW2-ISU      PIC  9(003)V9(01).
000910     02  TDNW2-KSU      PIC  9(004).
000920     02  TDNW2-HTC      PIC  X(002).
000930     02  TDNW2-SU       PIC  9(005)V9(01).
000940     02  TDNW2-GTN      PIC  9(007)V9(02).
000950     02  TDNW2-UTN      PIC  9(007).
000960     02  TDNW2-GKIN     PIC  9(010).
000970     02  TDNW2-UKIN     PIC  9(010).
000980     02  TDNW2-GCN      PIC  9(006).
000990     02  TDNW2-CCD      PIC  X(003).
001000     02  TDNW2-SHN      PIC  X(025).
001010     02  TDNW2-JAN      PIC  X(013).
001020     02  F              PIC  X(004).
001030     02  TDNW2-TSH      PIC  9(005).
001040     02  TDNW2-TKC      PIC  X(001).
001050     02  F              PIC  X(001).
001060*****02  F              PIC  X(112).                              D.070919
001070*****02  F              PIC  X(103).                              D.090309
001080     02  F              PIC  X(090).                              I.090309
001090     02  TDNW2-TSU      PIC  9(005)V9(01).                        I.090309
001100     02  TDNW2-TSC      PIC  9(001).                              I.090309
001110     02  TDNW2-HCO      PIC  9(006).                              I.090309
001120     02  TDNW2-HC       PIC  9(001).                              I.070919
001130     02  F              PIC  X(008).                              I.070919
001140     02  TDNW2-PC       PIC  9(001).

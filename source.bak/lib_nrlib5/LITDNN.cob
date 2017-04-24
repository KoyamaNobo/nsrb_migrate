000010************************************************
000020*****    統一伝票ファイル（ナ　フ　コ）    *****
000030*****           ( TDNNF )  256/1           *****
000040************************************************
000050 FD  TDNNF
000060     BLOCK  1 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "TDNNF".
000090 01  TDNN-R1.                                                     ﾍｯﾀﾞｰ
000100     02  TDNN1-KEY.
000110       03  TDNN1-STC.
000120         04  TDNN1-SCD  PIC  9(002).
000130         04  TDNN1-TCD  PIC  9(003).
000140         04  F          PIC  X(004).
000150       03  TDNN1-DNO.
000160         04  F          PIC  X(002).
000170         04  TDNN1-DNOD PIC  9(007).
000180       03  TDNN1-DGN    PIC  9(002).
000190     02  F              PIC  X(002).
000200     02  TDNN1-BCD      PIC  9(002).
000210     02  TDNN1-DPC      PIC  9(002).
000220     02  TDNN1-HNGP     PIC  9(006).
000230     02  TDNN1-HNGPD REDEFINES TDNN1-HNGP.
000240       03  TDNN1-HNEN   PIC  9(002).
000250       03  TDNN1-HGET   PIC  9(002).
000260       03  TDNN1-HPEY   PIC  9(002).
000270     02  TDNN1-NNGP     PIC  9(006).
000280     02  TDNN1-NNGPD REDEFINES TDNN1-NNGP.
000290       03  TDNN1-NNEN   PIC  9(002).
000300       03  TDNN1-NGET   PIC  9(002).
000310       03  TDNN1-NPEY   PIC  9(002).
000320     02  TDNN1-THC      PIC  9(006).
000330     02  TDNN1-STA      PIC  X(002).
000340     02  TDNN1-SNA      PIC  X(015).
000350     02  TDNN1-TNA      PIC  X(015).
000360     02  TDNN1-TSN      PIC  X(015).
000370     02  TDNN1-TST      PIC  X(012).
000380     02  TDNN1-HCC      PIC  9(001).
000390*****02  F              PIC  X(024).                              D.140905
000400     02  TDNN1-F1.                                                I.140905
000410       03  TDNN1-NHB    PIC  X(001).                              I.140905
000420       03  F            PIC  X(021).                              I.140905
000430     02  F              PIC  X(002).                              I.140905
000440     02  TDNN1-AR       PIC  X(007).
000450     02  TDNN1-DUR      PIC  X(026).
000460     02  TDNN1-DSHR     PIC  X(014).
000470     02  TDNN1-DSMR     PIC  X(007).
000480     02  TDNN1-ER       PIC  X(005).
000490     02  TDNN1-FSR      PIC  X(015).
000500     02  TDNN1-FUR      PIC  X(007).
000510     02  TDNN1-LCR      PIC  X(016).
000520     02  TDNN1-LUR      PIC  X(020).
000530     02  TDNN1-LSR      PIC  X(007).
000540*****02  F              PIC  X(003).                              D.070911
000550     02  F              PIC  X(002).                              I.070911
000560     02  TDNN1-HC       PIC  9(001).                              I.070911
000570     02  TDNN1-PC       PIC  9(001).
000580 01  TDNN-R2.                                                     ﾒｲｻｲ
000590     02  TDNN2-KEY.
000600       03  TDNN2-STC.
000610         04  TDNN2-SCD  PIC  9(002).
000620         04  TDNN2-TCD  PIC  9(003).
000630         04  F          PIC  X(004).
000640       03  TDNN2-DNO.
000650         04  F          PIC  X(002).
000660         04  TDNN2-DNOD PIC  9(007).
000670       03  TDNN2-DGN    PIC  9(002).
000680     02  TDNN2-JAN      PIC  X(013).
000690     02  TDNN2-GAR      PIC  X(006).
000700     02  F              PIC  X(001).
000710     02  TDNN2-TNI      PIC  X(003).
000720     02  TDNN2-SU       PIC  9(005).
000730     02  F              PIC  X(001).
000740     02  TDNN2-GTN      PIC  9(007).
000750     02  TDNN2-GTND  REDEFINES TDNN2-GTN.                         I.050317
000760       03  TDNN2-GTN1   PIC  9(001).                              I.050317
000770       03  TDNN2-GTN2   PIC  9(006).                              I.050317
000780     02  F              PIC  X(002).
000790     02  TDNN2-UTN      PIC  9(007).
000800     02  TDNN2-UTND  REDEFINES TDNN2-UTN.                         I.050317
000810       03  TDNN2-UTN1   PIC  9(001).                              I.050317
000820       03  TDNN2-UTN2   PIC  9(006).                              I.050317
000830     02  TDNN2-GKIN     PIC  9(010).
000840     02  TDNN2-UKIN     PIC  9(010).
000850     02  F              PIC  X(009).
000860     02  TDNN2-SHN      PIC  X(025).
000870     02  TDNN2-HSC      PIC  X(008).
000880     02  TDNN2-COR      PIC  X(006).
000890     02  TDNN2-SIZ      PIC  X(005).
000900     02  F              PIC  X(004).
000910     02  TDNN2-KKK      PIC  X(025).
000920     02  TDNN2-PCH      PIC  X(001).
000930     02  TDNN2-PSI      PIC  X(001).
000940     02  TDNN2-PBM      PIC  9(002).
000950     02  TDNN2-PJAN     PIC  X(013).
000960     02  TDNN2-PSHN     PIC  X(020).
000970     02  TDNN2-PKKK     PIC  X(020).
000980     02  TDNN2-PUTN     PIC  9(007).
000990     02  TDNN2-PMS      PIC  9(005).
001000*****02  F              PIC  X(019).                              D.070911
001010*****02  F              PIC  X(018).                              D.090309
001020     02  F              PIC  X(006).                              I.090309
001030     02  TDNN2-TSU      PIC  9(005).                              I.090309
001040     02  TDNN2-TSC      PIC  9(001).                              I.090309
001050     02  TDNN2-HCO      PIC  9(006).                              I.090309
001060     02  TDNN2-HC       PIC  9(001).                              I.070911
001070     02  TDNN2-PC       PIC  9(001).

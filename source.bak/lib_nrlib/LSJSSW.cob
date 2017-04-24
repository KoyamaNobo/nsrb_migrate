000010**********************************************
000020*****     材料仕入・支払累積ファイル     *****
000030**********************************************
000040 FD  JSSR-F
000050*****BLOCK  3 RECORDS                                             D.970423
000060*****BLOCK  5 RECORDS                                             D.951120
000070     BLOCK  2 RECORDS                                             I.970423
000080     LABEL RECORD IS STANDARD
000090     VALUE OF IDENTIFICATION WK0128ID.                            I.970423
000100*****VALUE OF IDENTIFICATION "BA85".                              D.950418
000110*****VALUE OF IDENTIFICATION "BA102".                             D.951120
000120*****VALUE OF IDENTIFICATION WK0170ID.                            D.970423
000130 01  JSSR-R.
000140     02  JR-DC.                                                   伝区
000150       03  JR-DC1       PIC  9(001).
000160       03  JR-DC2       PIC  9(001).
000170*****02  JR-DATE        PIC  9(006).                              D.970825
000180     02  JR-DATE        PIC  9(008).                              I.970825
000190     02  JR-NGP   REDEFINES JR-DATE.                              日付
000200       03  JR-NG.
000210*****    04  JR-NEN     PIC  9(002).                              D.970825
000220         04  JR-NEN     PIC  9(004).                              I.970825
000230         04  JR-NENL  REDEFINES JR-NEN.                           I.970826
000240           05  JR-NEN1  PIC  9(002).                              I.970826
000250           05  JR-NEN2  PIC  9(002).                              I.970826
000260         04  JR-GET     PIC  9(002).
000270       03  JR-PEY       PIC  9(002).
000280     02  JR-NGPD   REDEFINES JR-DATE.
000290*****  03  JR-NEND      PIC  9(002).                              D.970825
000300       03  JR-NEND      PIC  9(004).                              I.970825
000310       03  JR-GP        PIC  9(004).
000320       03  JR-GPD  REDEFINES JR-GP.
000330         04  JR-GETD    PIC  9(002).
000340         04  JR-PEYD    PIC  9(002).
000350     02  JR-NGPL  REDEFINES JR-DATE.                              I.970825
000360       03  F            PIC  9(002).                              I.970825
000370       03  JR-NGPS      PIC  9(006).                              I.970825
000380     02  JR-SCDD.                                                 仕入先C
000390       03  JR-SCD1      PIC  9(001).
000400       03  JR-SCD2      PIC  9(003).
000410     02  JR-SCD    REDEFINES JR-SCDD  PIC  9(004).
000420     02  JR-JCDD.                                                 材料C
000430       03  JR-JCD12.
000440         04  JR-JCD1    PIC  9(001).
000450         04  JR-JCD2    PIC  9(002).
000460       03  JR-JCD3      PIC  9(003).
000470     02  JR-JCD    REDEFINES JR-JCDD  PIC  9(006).
000480     02  JR-SU          PIC S9(007)V9(02).                        数量
000490     02  JR-SUD    REDEFINES JR-SU    PIC S9(009).
000500     02  JR-T           PIC S9(006)V9(02).                        単価
000510     02  JR-TD     REDEFINES JR-T     PIC S9(008).
000520     02  JR-KIN         PIC S9(008).                              I.990518
000530     02  JR-SHZ         PIC S9(007).                              I.990531
000540*****02  JR-SHZ         PIC S9(006).                              D.990531
000550*****02  JR-KIN         PIC S9(009).                              D.990518
000560     02  JR-SNGP.                                                 修正日
000570       03  JR-SNG.
000580         04  JR-SNEN    PIC  9(002).
000590         04  JR-SGET    PIC  9(002).
000600       03  JR-SPEY      PIC  9(002).
000610     02  JR-SNGPD  REDEFINES JR-SNGP.
000620       03  JR-SNEND     PIC  9(002).
000630       03  JR-SGPD      PIC  9(004).
000640       03  JR-SGP  REDEFINES JR-SGPD.
000650         04  JR-SGETD   PIC  9(002).
000660         04  JR-SPEYD   PIC  9(002).
000670     02  JR-SDAT   REDEFINES JR-SNGP  PIC  9(006).                I.120205
000680     02  JR-SJCD        PIC  9(006).                              I.990518
000690     02  JR-NHN         PIC  9(006).                              I.990518
000700     02  JR-FC          PIC  9(001).                              I.990518
000710     02  JR-YC          PIC  9(001).                              用途C
000720     02  JR-TC          PIC  9(001).                              単位C
000730     02  JR-HC          PIC  9(001).                              製品C
000740     02  JR-SC          PIC  9(001).                              支払C
000750*****02  JR-SJCD        PIC  9(006).                              D.990518
000760*****02  JR-NHN         PIC  9(006).                              D.990518
000770*****02  JR-FC          PIC  9(001).                              D.990518
000780*****02  JR-SHZ         PIC S9(007).                              D.990518
000790     02  JR-BSC         PIC  9(001).                              I.980417
000800*****02  F              PIC  X(008).                              D.950418
000810*****02  F              PIC  X(012).                              D.950906
000820*****02  F              PIC  X(010).                              D.970825
000830*****02  F              PIC  X(008).                              D.980417
000840*****02  F              PIC  X(007).                              D.990518
000850     02  JR-BKC         PIC  9(002).                              I.950906
000860     02  F              PIC  X(016).                              I.990531
000870*****02  F              PIC  X(017).                              D.990531
000880*****02  F              PIC  X(006).                              D.990518
000890*****02  JR-DP.                                                   D.980417
000900*****  03  JR-DPP       PIC  9(004).                              D.980417
000910*****  03  JR-DPG       PIC  9(002).                              D.980417
000920     02  JR-KEY         PIC  X(007).                              I.950418
000930     02  JR-KEYD  REDEFINES JR-KEY.                               I.120302
000940       03  JR-DNO       PIC  9(006).                              I.120302
000950       03  JR-GNO       PIC  9(001).                              I.120302
000960     02  JR-CR          PIC  9(001).                              ﾁｪﾂｸﾘｽﾄC
000970     02  F              PIC  X(026).                              I.990518
000980*****02  F              PIC  X(028).                              D.990518
000990*****02  JR-PC          PIC  9(002).                              D.980417
001000*****02  F              PIC  X(026).                              D.980417
001010*****02  F              PIC  X(068).                              D.970423

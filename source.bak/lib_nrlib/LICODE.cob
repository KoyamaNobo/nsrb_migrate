000010************************************
000020*****    コード変換ファイル    *****
000030*****      ( CODEF ) 102/5     *****
000040************************************
000050 FD  CODEF
000060*****BLOCK  8 RECORDS
000070*****BLOCK  4 RECORDS                                             D.100709
000080     BLOCK  5 RECORDS                                             I.100709
000090     LABEL RECORD IS STANDARD
000100     VALUE OF IDENTIFICATION "CODEF"                              I.100715
000110     ALTERNATE IDENTIFICATION "CODEF2".                           I.100715
000120*****VALUE OF IDENTIFICATION "CODEF".                             D.100715
000130 01  CODE-R.
000140*****02  CODE-KEY.                                                D.100709
000150*****  03  CODE-TCD     PIC  9(004).                              D.100709
000160*****  03  CODE-JAN     PIC  X(013).                              D.100709
000170*****  03  CODE-CO    REDEFINES CODE-JAN.                         D.100709
000180*****    04  CODE-WCO   PIC  9(007).                              D.100709
000190*****    04  F          PIC  X(006).                              D.100709
000200*****  03  CODE-HCD     PIC  9(006).                              D.100709
000210*****  03  CODE-HCDD  REDEFINES CODE-HCD.                         D.100709
000220*****    04  CODE-HCD1  PIC  9(004).                              D.100709
000230*****    04  CODE-HCD2  PIC  9(002).                              D.100709
000240*****  03  CODE-SIZ     PIC  9(001).                              D.100709
000250*****  03  CODE-SNO     PIC  9(002).                              D.100709
000260     02  CODE-D1.                                                 I.100709
000270       03  CODE-KEY.                                              I.100709
000280         04  CODE-TCD     PIC  9(004).                            I.100709
000290         04  CODE-JAN     PIC  X(013).                            I.100709
000300         04  CODE-CO    REDEFINES CODE-JAN.                       I.100709
000310           05  CODE-WCO   PIC  9(007).                            I.100709
000320           05  F          PIC  X(006).                            I.100709
000330         04  CODE-HCD     PIC  9(006).                            I.100709
000340         04  CODE-HCDD  REDEFINES CODE-HCD.                       I.100709
000350           05  CODE-HCD1  PIC  9(004).                            I.100709
000360           05  CODE-HCD2  PIC  9(002).                            I.100709
000370         04  CODE-SIZ     PIC  9(001).                            I.100709
000380         04  CODE-SNO     PIC  9(002).                            I.100709
000390       03  F              PIC  X(013).                            I.100709
000400     02  CODE-D2    REDEFINES CODE-D1.                            I.100709
000410       03  CODE-TCD2      PIC  9(004).                            I.100709
000420       03  CODE-JAN3      PIC  X(013).                            I.100709
000430       03  CODE-CO2   REDEFINES CODE-JAN3.                        I.100709
000440         04  CODE-WCO2    PIC  9(007).                            I.100709
000450         04  F            PIC  X(006).                            I.100709
000460       03  CODE-KEY2.                                             I.100709
000470         04  CODE-HCD20   PIC  9(006).                            I.100709
000480         04  CODE-HCDD2 REDEFINES CODE-HCD20.                     I.100709
000490           05  CODE-HCD21 PIC  9(004).                            I.100709
000500           05  CODE-HCD22 PIC  9(002).                            I.100709
000510         04  CODE-SIZ2    PIC  9(001).                            I.100709
000520         04  CODE-SNO2    PIC  9(002).                            I.100709
000530         04  CODE-JAN2    PIC  X(013).                            I.100709
000540     02  CODE-ITF         PIC  X(016).                            I.100629
000550     02  CODE-ITFD    REDEFINES CODE-ITF.                         I.100629
000560       03  CODE-ISU       PIC  9(003).                            I.100629
000570       03  CODE-JAND      PIC  X(013).                            I.100629
000580     02  CODE-NAME        PIC  X(020).                            I.071122
000590     02  F                PIC  X(027).                            I.100709
000600*****02  F              PIC  X(002).                              D.100709
000610*****02  F              PIC  X(018).                              D.100629
000620*****02  F              PIC  X(006).                              D.071122

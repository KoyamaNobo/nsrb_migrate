000010********************************************
000020*****     仕上受入トラン　ファイル     *****
000030*****      ( UTRAN )   128/2           *****
000040********************************************
000050 FD  UTRAN
000060*****BLOCK  5 RECORDS                                             D.020516
000070     BLOCK  2 RECORDS                                             I.020516
000080     LABEL RECORD IS STANDARD
000090     VALUE OF IDENTIFICATION "UTRAN".
000100 01  UTRAN-R.
000110     02  UTRAN-NO       PIC  9(007).                              受入№
000120     02  UTRAN-NOD   REDEFINES UTRAN-NO.
000130       03  UTRAN-UNO    PIC  9(006).
000140       03  UTRAN-GYO    PIC  9(001).
000150*****02  UTRAN-DATE     PIC  9(006).                              D.970709
000160     02  UTRAN-DATE     PIC  9(008).                              I.970709
000170     02  UTRAN-NGPD  REDEFINES UTRAN-DATE.                        I.000211
000180       03  UTRAN-NG     PIC  9(006).                              I.000211
000190       03  F            PIC  9(002).                              I.000211
000200     02  UTRAN-NGP   REDEFINES UTRAN-DATE.                        I.981215
000210       03  F            PIC  9(002).                              I.981215
000220       03  UTRAN-NGPS   PIC  9(006).                              I.981215
000230     02  UTRAN-HCD      PIC  9(006).                              品名C
000240     02  UTRAN-SIZ      PIC  9(001).                              ｻｲｽﾞ区分
000250     02  UTRAN-SUD.                                               数量
000260       03  UTRAN-SU     PIC S9(004)  OCCURS  10.
000270     02  UTRAN-SUT      PIC S9(005).                              合計数量
000280     02  UTRAN-BKIN     PIC S9(008).                              売価金額
000290     02  UTRAN-FKIN     PIC S9(008).                              振替金額
000300     02  UTRAN-NRC      PIC  9(001).                              入力C
000310     02  UTRAN-SSC      PIC  9(001).                              生産C
000320     02  UTRAN-HPC      PIC  9(001).                              返品C
000330     02  UTRAN-SKC      PIC  9(001).                              倉庫C
000340     02  UTRAN-BC.                                                分類C
000350       03  UTRAN-BC1    PIC  9(002).                                   1
000360       03  UTRAN-BC2    PIC  9(002).                                   2
000370       03  UTRAN-BC3    PIC  9(002).                                   3
000380     02  UTRAN-BCD   REDEFINES UTRAN-BC.                          I.040427
000390       03  UTRAN-BC1D   PIC  9(003).                              I.040427
000400       03  F            PIC  9(003).                              I.040427
000410     02  UTRAN-BMC      PIC  9(002).                              I.020516
000420     02  UTRAN-BMNO     PIC  9(001).                              I.020516
000430*****02  F              PIC  X(008).                              D.010108
000440*****02  F              PIC  X(002).                              D.020516
000450     02  UTRAN-KBN      PIC  9(006).                              I.010108
000460     02  F              PIC  X(025).                              I.020516
000470     02  UTRAN-PRC      PIC  9(001).                              I.981215
000480*****02  F              PIC  X(009).                              D.981215
000490*****02  F              PIC  X(011).                              D.970709

000010********************************************
000020*****     仕上受入トラン　ファイル     *****
000030*****      ( UTRAN )   102/5           *****
000040********************************************
000050 FD  UTRAN
000060     BLOCK  5 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "UTRAN".
000090 01  UTRAN-R.
000100     02  UTRAN-NO       PIC  9(007).                              受入№
000110     02  UTRAN-NOD   REDEFINES UTRAN-NO.
000120       03  UTRAN-UNO    PIC  9(006).
000130       03  UTRAN-GYO    PIC  9(001).
000140*****02  UTRAN-DATE     PIC  9(006).                              D.970709
000150     02  UTRAN-DATE     PIC  9(008).                              I.970709
000160     02  UTRAN-HCD      PIC  9(006).                              品名C
000170     02  UTRAN-SIZ      PIC  9(001).                              ｻｲｽﾞ区分
000180     02  UTRAN-SUD.                                               数量
000190       03  UTRAN-SU     PIC S9(004)  OCCURS  10.
000200     02  UTRAN-SUT      PIC S9(005).                              合計数量
000210     02  UTRAN-BKIN     PIC S9(008).                              売価金額
000220     02  UTRAN-FKIN     PIC S9(008).                              振替金額
000230     02  UTRAN-NRC      PIC  9(001).                              入力C
000240     02  UTRAN-SSC      PIC  9(001).                              生産C
000250     02  UTRAN-HPC      PIC  9(001).                              返品C
000260     02  UTRAN-SKC      PIC  9(001).                              倉庫C
000270     02  UTRAN-BC.                                                分類C
000280       03  UTRAN-BC1    PIC  9(002).                                   1
000290       03  UTRAN-BC2    PIC  9(002).                                   2
000300       03  UTRAN-BC3    PIC  9(002).                                   3
000310     02  F              PIC  X(009).                              I.970709
000320*****02  F              PIC  X(011).                              D.970709

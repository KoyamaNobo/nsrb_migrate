000010*********************************************
000020*****     履物品名統計マスター　　      *****
000030*****      ( HHTFD   256/1 )            *****
000040*****      ( HHTF1   7-7   )            *****
000050*****      ( HHTF2   1-13  )            *****
000060*********************************************
000070 FD  HHTF
000080     BLOCK  1 RECORDS
000090     LABEL RECORD IS STANDARD
000100     VALUE OF IDENTIFICATION "HHTF1"                              I.010726
000110     ALTERNATE IDENTIFICATION "HHTF2".                            I.010726
000120*****VALUE OF IDENTIFICATION "HHTF".                              D.010726
000130 01  HHT-R.
000140*    ----- ＫＥＹ --------------------------------------------------------
000150     02  HHT-KEY2.                                                I.010726
000160       03  HHT-MHCD     PIC  9(006).                              I.010726
000170       03  HHT-KEY.                                               I.010726
000180         04  HHT-HCD    PIC  9(006).                              I.010726
000190         04  HHT-HCDD  REDEFINES HHT-HCD.                         I.010726
000200           05  HHT-HCD1 PIC  9(004).                              I.010726
000210           05  HHT-HCD2 PIC  9(002).                              I.010726
000220         04  HHT-SIZ    PIC  9(001).                              I.010726
000230*****02  HHT-KEY.                                                 D.010726
000240*****  03  HHT-HCD      PIC  9(006).                              D.010726
000250*****  03  HHT-HCDD  REDEFINES HHT-HCD.                           D.010726
000260*****    04  HHT-HCD1   PIC  9(004).                              D.010726
000270*****    04  HHT-HCD2   PIC  9(002).                              D.010726
000280*****  03  HHT-SIZ      PIC  9(001).                              D.010726
000290*    ----- 統計項目 ------------------------------------------------------
000300     02  HHT-AZSU.                                                前月残数
000310       03  HHT-ZSUD  OCCURS  10.
000320         04  HHT-ZSU    PIC S9(006) COMP-3.
000330     02  HHT-ANSU.                                                入庫数
000340       03  HHT-NSUD  OCCURS  10.
000350         04  HHT-NSU    PIC S9(006) COMP-3.
000360     02  HHT-AUSU.                                                出庫数
000370       03  HHT-USUD  OCCURS  10.
000380         04  HHT-USU    PIC S9(006) COMP-3.
000390     02  HHT-AASS.                                                預り出荷
000400       03  HHT-ASSD  OCCURS  10.
000410         04  HHT-ASS    PIC S9(004) COMP-3.                       I.010726
000420*****    04  HHT-ASS    PIC S9(006) COMP-3.                       D.010726
000430     02  HHT-ATZS.                                                棚卸帳簿
000440       03  HHT-TSZD  OCCURS  10.
000450         04  HHT-TZS    PIC S9(006) COMP-3.
000460     02  HHT-ATSU.                                                棚卸数
000470       03  HHT-TSUD  OCCURS  10.
000480         04  HHT-TSU    PIC S9(006) COMP-3.
000490*    ----- 分　類 --------------------------------------------------------
000500     02  HHT-BCD12.                                               I.030514
000510       03  HHT-BCD1     PIC  9(003).                              I.030514
000520       03  HHT-BCW1  REDEFINES HHT-BCD1.                          I.030514
000530         04  HHT-BC1    PIC  9(002).                              I.030514
000540         04  HHT-BC21   PIC  9(001).                              I.030514
000550*****02  HHT-BC1        PIC  9(002).                              D.030514
000560*****02  HHT-BC2        PIC  9(002).                              D.030514
000570*****02  HHT-BCD2  REDEFINES HHT-BC2.                             D.030514
000580*****  03  HHT-BC21     PIC  9(001).                              D.030514
000590       03  HHT-BC22     PIC  9(001).
000600     02  HHT-BCW12 REDEFINES HHT-BCD12.                           I.030514
000610       03  F            PIC  9(002).                              I.030514
000620       03  HHT-BC2      PIC  9(002).                              I.030514
000630     02  HHT-BC3        PIC  9(002).                              分類CD3
000640     02  HHT-BCD3  REDEFINES HHT-BC3.
000650       03  HHT-BC31     PIC  9(001).
000660       03  HHT-BC32     PIC  9(001).
000670     02  HHT-BMNO       PIC  9(001).                              I.020425
000680     02  HHT-BC4        PIC  9(001).                              I.090121
000690*
000700     02  F              PIC  X(005).                              I.090121
000710*****02  F              PIC  X(006).                              D.090121
000720*****02  F              PIC  X(007).                              D.020425
000730*****02  F              PIC  X(003).                              D.010726

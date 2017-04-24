000010**************************************************
000020*****     受　取　手　形　フ　ァ　イ　ル     *****
000030**************************************************
000040 FD  UKET-F
000050*****BLOCK  2 RECORDS                                             D.970828
000060*****BLOCK  3 RECORDS                                             D.970602
000070     BLOCK  1 RECORDS                                             I.970602
000080     LABEL RECORD IS STANDARD
000090     VALUE OF IDENTIFICATION WK0256ID.                            I.970828
000100*****VALUE OF IDENTIFICATION WK0128ID.                            D.970828
000110*****VALUE OF IDENTIFICATION "TK128".                             D.970210
000120*****VALUE OF IDENTIFICATION WK0170ID.                            D.970602
000130 01  UKET-R.
000140     02  UT-KEY         PIC  9(004).                              ｳｹﾃNO
000150     02  UT-TSC         PIC  9(002).                              ﾃｶﾞﾀｼｭﾙｲ
000160     02  UT-TSCD  REDEFINES UT-TSC.                               ﾃｶﾞﾀｼｭﾙｲ
000170       03  UT-TC1       PIC  9(001).
000180       03  UT-TC2       PIC  9(001).
000190     02  UT-SKC         PIC  9(002).                              ｼｮﾘｸﾌﾞﾝ
000200     02  UT-BCD         PIC  9(004).                              BKｺｰﾄﾞ
000210     02  UT-TTC         PIC  9(002).                              ﾀﾝﾄｳｺｰﾄﾞ
000220     02  UT-BC          PIC  9(001).                              ﾌﾞﾓﾝｺｰﾄﾞ
000230     02  UT-FKC         PIC  9(002).                              ﾌｹﾝｺｰﾄﾞ
000240     02  UT-TCD         PIC  9(004).                              ﾄﾘﾋｷｺｰﾄﾞ
000250     02  UT-KIN         PIC  9(010).                              ｷﾝｶﾞｸ
000260     02  UT-UTD         PIC  9(006).
000270     02  UT-UTDD  REDEFINES UT-UTD.                               ｳｹﾄﾘﾋﾞ
000280       03  UT-UNG.
000290         04  UT-UTN     PIC  9(002).
000300         04  UT-UTG     PIC  9(002).
000310       03  UT-UTP       PIC  9(002).
000320     02  UT-FDD         PIC  9(006).
000330     02  UT-FDDD  REDEFINES UT-FDD.                               ﾌﾘﾀﾞｼﾋﾞ
000340       03  UT-FDN       PIC  9(002).
000350       03  UT-FDG       PIC  9(002).
000360       03  UT-FDP       PIC  9(002).
000370     02  UT-HKD         PIC  9(006).
000380     02  UT-HKDD  REDEFINES UT-HKD.                               ﾋｷｳｹﾋﾞ
000390       03  UT-HKN       PIC  9(002).
000400       03  UT-HKG       PIC  9(002).
000410       03  UT-HKP       PIC  9(002).
000420     02  UT-MKD         PIC  9(006).
000430     02  UT-MKDD  REDEFINES UT-MKD.                               ﾏﾝｷﾋﾞ
000440       03  UT-MNG.
000450         04  UT-MKN     PIC  9(002).
000460         04  UT-MKG     PIC  9(002).
000470       03  UT-MKP       PIC  9(002).
000480     02  UT-IDD         PIC  9(006).
000490     02  UT-IDDD  REDEFINES UT-IDD.                               ｲﾄﾞｳﾋﾞ
000500       03  UT-IDN       PIC  9(002).
000510       03  UT-IDG       PIC  9(002).
000520       03  UT-IDP       PIC  9(002).
000530     02  UT-SBC         PIC  9(004).                              ｼｮﾘBK
000540     02  UT-FDM         PIC  N(024).                              ﾌﾘﾀﾞｼﾆﾝ
000550*****02  F              PIC  X(003).                              D.970828
000560     02  F              PIC  X(039).                              I.970828
000570     02  UT-OKD         PIC  9(006).                              ｵﾁｺﾐﾋﾞ
000580     02  UT-OKDD  REDEFINES UT-OKD.
000590       03  UT-ONG.
000600         04  UT-OKN     PIC  9(002).
000610         04  UT-OKG     PIC  9(002).
000620       03  UT-OKP       PIC  9(002).
000630     02  UT-SNU         PIC  9(004).                              I.970828
000640     02  UT-SNM         PIC  9(004).                              I.970828
000650     02  UT-SNI         PIC  9(004).                              I.970828
000660     02  F              PIC  X(086).                              I.970602
000670*****02  UT-SNU         PIC  9(002).                              D.970828
000680*****02  UT-SNM         PIC  9(002).                              D.970828
000690*****02  UT-SNI         PIC  9(002).                              D.970828
000700*****02  F              PIC  X(042).                              D.970602

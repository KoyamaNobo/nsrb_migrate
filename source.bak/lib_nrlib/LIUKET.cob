000010**************************************************
000020*****     受　取　手　形　マ　ス　タ　ー     *****
000030*****         ( U K E T M )    170/3         *****
000040**************************************************
000050 FD  UKET-M
000060*****BLOCK  2 RECORDS                                             D.970828
000070     BLOCK  3 RECORDS                                             I.970828
000080     LABEL RECORD IS STANDARD
000090     VALUE OF IDENTIFICATION "UKETM".
000100 01  UKET-R.
000110     02  UT-KEY         PIC  X(004).                              ｳｹﾃNO
000120     02  UT-TSC         PIC  9(002).                              ﾃｶﾞﾀｼｭﾙｲ
000130     02  UT-TSCD  REDEFINES UT-TSC.                               ﾃｶﾞﾀｼｭﾙｲ
000140       03  UT-TC1       PIC  9(001).
000150       03  UT-TC2       PIC  9(001).
000160     02  UT-SKC         PIC  9(002).                              ｼｮﾘｸﾌﾞﾝ
000170     02  UT-BCD         PIC  9(004).                              BKｺｰﾄﾞ
000180     02  UT-TTC         PIC  9(002).                              ﾀﾝﾄｳｺｰﾄﾞ
000190     02  UT-BC          PIC  9(001).                              ﾌﾞﾓﾝｺｰﾄﾞ
000200     02  UT-FKC         PIC  9(002).                              ﾌｹﾝｺｰﾄﾞ
000210     02  UT-TCD         PIC  9(004).                              ﾄﾘﾋｷｺｰﾄﾞ
000220     02  UT-KIN         PIC  9(010).                              ｷﾝｶﾞｸ
000230     02  UT-UTD         PIC  9(006).
000240     02  UT-UTDD  REDEFINES UT-UTD.                               ｳｹﾄﾘﾋﾞ
000250       03  UT-UNG.
000260         04  UT-UTN     PIC  9(002).
000270         04  UT-UTG     PIC  9(002).
000280       03  UT-UTP       PIC  9(002).
000290     02  UT-FDD         PIC  9(006).
000300     02  UT-FDDD  REDEFINES UT-FDD.                               ﾌﾘﾀﾞｼﾋﾞ
000310       03  UT-FDN       PIC  9(002).
000320       03  UT-FDG       PIC  9(002).
000330       03  UT-FDP       PIC  9(002).
000340     02  UT-HKD         PIC  9(006).
000350     02  UT-HKDD  REDEFINES UT-HKD.                               ﾋｷｳｹﾋﾞ
000360       03  UT-HKN       PIC  9(002).
000370       03  UT-HKG       PIC  9(002).
000380       03  UT-HKP       PIC  9(002).
000390     02  UT-MKD         PIC  9(006).
000400     02  UT-MKDD  REDEFINES UT-MKD.                               ﾏﾝｷﾋﾞ
000410       03  UT-MNG.
000420         04  UT-MKN     PIC  9(002).
000430         04  UT-MKG     PIC  9(002).
000440       03  UT-MKP       PIC  9(002).
000450     02  UT-IDD         PIC  9(006).
000460     02  UT-IDDD  REDEFINES UT-IDD.                               ｲﾄﾞｳﾋﾞ
000470       03  UT-ING.
000480         04  UT-IDN     PIC  9(002).
000490         04  UT-IDG     PIC  9(002).
000500       03  UT-IDP       PIC  9(002).
000510     02  UT-SBC         PIC  9(004).                              ｼｮﾘBK
000520     02  UT-FDM         PIC  N(024).                              ﾌﾘﾀﾞｼﾆﾝ
000530     02  F              PIC  X(039).                              I.970828
000540*****02  F              PIC  X(003).                              D.970828
000550     02  UT-OKD         PIC  9(006).                              ｵﾁｺﾐﾋﾞ
000560     02  UT-OKDD  REDEFINES UT-OKD.
000570       03  UT-ONG.
000580         04  UT-OKN     PIC  9(002).
000590         04  UT-OKG     PIC  9(002).
000600       03  UT-OKP       PIC  9(002).
000610     02  UT-SND.                                                  ｾｲﾚｷ ﾈﾝ
000620       03  UT-SNU       PIC  9(004).                              I.970828
000630       03  UT-SNM       PIC  9(004).                              I.970828
000640       03  UT-SNI       PIC  9(004).                              I.970828
000650*****  03  UT-SNU       PIC  9(002).                              D.970828
000660*****  03  UT-SNM       PIC  9(002).                              D.970828
000670*****  03  UT-SNI       PIC  9(002).                              D.970828

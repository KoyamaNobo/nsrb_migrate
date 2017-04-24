000010**************************************************
000020*****     支　払　手　形　マ　ス　タ　ー     *****
000030*****         ( S H I T M )    128/1         *****
000040**************************************************
000050 FD  SHIT-M
000060     BLOCK  2 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "SHITM".
000090 01  SHIT-R.
000100     02  ST-KEY         PIC  X(004).                              ｳｹﾃNO
000110     02  ST-TSC         PIC  9(002).                              ﾃｶﾞﾀｼｭﾙｲ
000120     02  ST-TSCD REDEFINES ST-TSC.                                ﾃｶﾞﾀｼｭﾙｲ
000130       03  ST-TC1       PIC  9(001).
000140       03  ST-TC2       PIC  9(001).
000150     02  ST-SKC         PIC  9(002).                              ｼｮﾘｸﾌﾞﾝ
000160     02  ST-BCD         PIC  9(004).                              BKｺｰﾄﾞ
000170     02  ST-FKC         PIC  9(002).                              ﾌｹﾝｺｰﾄﾞ
000180     02  ST-TCD         PIC  9(004).                              ﾄﾘﾋｷｺｰﾄﾞ
000190     02  ST-KIN         PIC  9(010).                              ｷﾝｶﾞｸ
000200     02  ST-FDD         PIC  9(006).
000210     02  ST-FDDD REDEFINES ST-FDD.                                ﾌﾘﾀﾞｼﾋﾞ
000220       03  ST-FNG.
000230         04  ST-FDN     PIC  9(002).
000240         04  ST-FDG     PIC  9(002).
000250       03  ST-FDP       PIC  9(002).
000260     02  ST-MKD         PIC  9(006).
000270     02  ST-MKDD REDEFINES ST-MKD.                                ﾏﾝｷﾋﾞ
000280       03  ST-MNG.
000290         04  ST-MKN     PIC  9(002).
000300         04  ST-MKG     PIC  9(002).
000310       03  ST-MKP       PIC  9(002).
000320*    [   ｱｲﾃｶﾓｸ  ｳﾁﾜｹ ｷﾝｶﾞｸ   ]
000330     02  ST-UKD.
000340       03  ST-UK    OCCURS  7  PIC  9(008).
000350     02  ST-AUK  REDEFINES ST-UKD.
000360       03  ST-ZR        PIC  9(008).                              ｻﾞｲﾘｮｳ
000370       03  ST-SS        PIC  9(008).                              ｼｲﾚｼｮｳﾋﾝ
000380       03  ST-SB        PIC  9(008).                              ｾﾂﾋﾞ
000390       03  ST-GC        PIC  9(008).                              ｶﾞｲﾁｭｳ
000400       03  ST-SZ        PIC  9(008).                              ｾｲｿﾞｳｹｲﾋ
000410       03  ST-EG        PIC  9(008).                              ｴｲｷﾞｮｳｹｲ
000420       03  ST-ST        PIC  9(008).                              ｿﾉﾀ
000430*****02  F              PIC  X(028).                              D.970828
000440     02  F              PIC  X(024).                              I.970828
000450     02  ST-SND.                                                  ｾｲﾚｷ ﾈﾝ
000460       03  ST-SNF       PIC  9(004).                              I.970828
000470       03  ST-SNM       PIC  9(004).                              I.970828
000480*****  03  ST-SNF       PIC  9(002).                              D.970828
000490*****  03  ST-SNM       PIC  9(002).                              D.970828

000010******************************************
000020*****     工品　品名統計ファイル     *****
000030*****     (  WK0256NNN  256/1  )     *****
000040******************************************
000050 FD  KHT-M
000060*****BLOCK 3 RECORDS                                              D.970522
000070     BLOCK 1 RECORDS                                              I.970522
000080     LABEL RECORD IS STANDARD
000090     VALUE OF IDENTIFICATION WK0256ID.                            I.970522
000100*****VALUE OF IDENTIFICATION "KO170".                             D.950831
000110*****VALUE OF IDENTIFICATION WK0170ID.                            D.970522
000120 01  KHT-R.
000130     02  KHT-KEYD.                                                ｺｰﾄﾞ
000140       03  KHT-YC       PIC  9(002).                              ﾖｳﾄｸﾌﾞﾝ
000150       03  KHT-NC       PIC  9(001).                              I.021004
000160       03  KHT-KEY.                                               ｺｰﾄﾞ
000170         04  KHT-KEY1   PIC  X(002).
000180         04  KHT-KEY2   PIC  9(003).
000190*    [   ﾄ ｳ ｹﾞ ﾂ   ｼﾞ ｯ ｾ  ｷ   ]
000200     02  KHT-KSU        PIC S9(006)V9(02).                        ｶﾘｭｳｽｳ
000210     02  KHT-HSU        PIC S9(006)V9(02).                        ﾊｲｷｬｸｽｳ
000220     02  KHT-ISU        PIC S9(006)V9(02).                        I.021004
000230     02  KHT-KKIN       PIC S9(008).                              ｾｲｻﾝｶﾞｸ
000240     02  KHT-SSU        PIC S9(006)V9(02).                        ｼｭｯｶｽｳ
000250     02  KHT-UKIN       PIC S9(008).                              ｳﾘｱｹﾞｶﾞｸ
000260     02  KHT-NKIN       PIC S9(007).                              ﾈﾋﾞｷｶﾞｸ
000270     02  KHT-GKIN       PIC S9(008).                              I.020822
000280     02  KHT-ZSU        PIC S9(006)V9(02).                        ｾﾞﾝｸﾘｽｳ
000290     02  KHT-ZKIN       PIC S9(008).                              ｾﾞﾝｸﾘｶﾞｸ
000300*    [   ｱｽﾞｶﾘ ﾄｳｹｲ   ]
000310     02  KHT-AZS        PIC S9(006).                              ｾﾞﾝｸﾘｽｳ
000320     02  KHT-AAS        PIC S9(006).                              ｱｽﾞｶﾘｽｳ
000330     02  KHT-AUS        PIC S9(006).                              ｳﾘｱｹﾞｽｳ
000340     02  KHT-ASS        PIC S9(006).                              ｼｭｯｶｽｳ
000350     02  KHT-AC         PIC  9(001).                              ｸﾌﾞﾝ
000360     02  F              PIC  X(012).
000370*    [   ﾄｳｹﾞﾂ ｼｭｯｶ ﾖﾃｲ   ]
000380*****02  KHT-TSY.                                                 D.090429
000390*****  03  KHT-TTG      PIC S9(006).                              D.090429
000400*****  03  KHT-TYG      PIC S9(006).                              D.090429
000410*****  03  KHT-TYY      PIC S9(006).                              D.020822
000420*****  03  KHT-TZO      PIC S9(006).                              D.020822
000430*    [   ﾖｸｹﾞﾂ ｼｭｯｶ ﾖﾃｲ   ]
000440*****02  KHT-YSY.                                                 D.021004
000450*****  03  KHT-YTG      PIC S9(006).                              D.021004
000460*****  03  KHT-YYG      PIC S9(006).                              D.021004
000470*****  03  KHT-YYY      PIC S9(006).                              D.020822
000480*****  03  KHT-YZO      PIC S9(006).                              D.020822
000490*    [   ｸﾌﾞﾝ･ｺｰﾄﾞ  ]
000500     02  KHT-KIS        PIC  9(001).                              ｷｼｭｸﾌﾞﾝ
000510     02  KHT-KCO        PIC  X(005).                              ｶﾘｭｳｺｰﾄﾞ
000520*    [   ﾀﾅｵﾛｼ   ]
000530     02  KHT-JTS        PIC S9(006)V9(02).                        ｼﾞﾂﾀﾅ
000540     02  KHT-TTS        PIC S9(006)V9(02).                        ﾁｮｳﾎﾞﾀﾅ
000550*
000560     02  F              PIC  X(110).                              I.021004
000570*****02  KHT-NC         PIC  9(001).                              D.021004
000580*****02  F              PIC  X(106).                              D.021004
000590*****02  F              PIC  X(107).                              D.020906
000600*****02  F              PIC  X(091).                              D.020822
000610*****02  F              PIC  X(005).                              D.970522

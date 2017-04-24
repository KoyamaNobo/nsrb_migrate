000010********************************************
000020*****     履物製品仕入入力ファイル     *****
000030*****         (  HSSF  256/1  )        *****
000040********************************************
000050 FD  HSS-F
000060     BLOCK  1 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "HSSF".
000090 01  HSS-R.
000100     02  HSS-KEY.
000110       03  HSS-DNO      PIC  9(006).                              伝票№
000120       03  HSS-SNO      PIC  9(001).
000130     02  HSS-DATE       PIC  9(008).                              日付
000140     02  HSS-NGP   REDEFINES HSS-DATE.
000150       03  HSS-NG.
000160         04  HSS-NEN    PIC  9(004).
000170         04  HSS-GET    PIC  9(002).
000180       03  HSS-PEY      PIC  9(002).
000190     02  HSS-NGPD  REDEFINES HSS-DATE.                            I.030213
000200       03  F            PIC  9(004).                              I.030213
000210       03  HSS-GP       PIC  9(004).                              I.030213
000220     02  HSS-NGPL  REDEFINES HSS-DATE.
000230       03  F            PIC  9(002).
000240       03  HSS-NGPS     PIC  9(006).
000250     02  HSS-SCD        PIC  9(004).                              仕入先
000260     02  HSS-JCD        PIC  9(006).                              材料
000270     02  HSS-SUT        PIC S9(006).                              数量
000280     02  HSS-CD         PIC  9(006).                              修正日
000290     02  HSS-HCD        PIC  9(006).                              品名
000300     02  HSS-ASUD.
000310       03  HSS-ASU   OCCURS   4.                                  ｻｲｽﾞ
000320         04  HSS-SUD   OCCURS  10.
000330           05  HSS-SU   PIC S9(004).                              数量
000340     02  HSS-SKC        PIC  9(001).                              倉庫
000350     02  HSS-RNO        PIC  9(008).                              I.030128
000360     02  HSS-RNOD  REDEFINES HSS-RNO.                             I.030128
000370       03  HSS-RSN      PIC  9(002).                              I.030128
000380       03  HSS-RNG      PIC  9(004).                              I.030128
000390       03  HSS-RND      PIC  9(002).                              I.030128
000400     02  HSS-KRC        PIC  9(001).                              I.030128
000410     02  HSS-HPC        PIC  9(001).                              I.030128
000420     02  HSS-UNO        PIC  9(006).                              I.030128
000430     02  F              PIC  X(032).                              I.030128
000440*****02  F              PIC  X(048).                              D.030128
000450     02  HSS-BHC        PIC  9(001).                              購買変換
000460     02  HSS-HHC        PIC  9(001).                              販売変換
000470     02  HSS-HKC        PIC  9(001).                              営業変換
000480     02  HSS-PRC        PIC  9(001).                              印字

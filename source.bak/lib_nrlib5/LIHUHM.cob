000010*********************************************
000020*****     履物製品受払マスター　　      *****
000030*****      ( HUHM )    102/5            *****
000040*********************************************
000050 FD  HUH-M
000060     BLOCK  5 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "HUHM".
000090 01  HUH-R.
000100     02  HUH-KEY.
000110       03  HUH-HCD      PIC  9(006).                              品名ｺｰﾄﾞ
000120       03  HUH-HCDD  REDEFINES HUH-HCD.                           I.930209
000130         04  HUH-HCD1   PIC  9(004).                              I.930209
000140         04  HUH-HCD2   PIC  9(002).                              I.930209
000150*
000160     02  HUH-NGD.
000170*****  03  HUH-NEN      PIC  9(002).                              D.970709
000180       03  HUH-NEN      PIC  9(004).                              I.970709
000190       03  HUH-GET      PIC  9(002).
000200     02  HUH-NG    REDEFINES HUH-NGD  PIC 9(006).                 I.970709
000210*****02  HUH-NG    REDEFINES HUH-NGD  PIC 9(004).                 D.970709
000220*
000230     02  HUH-D.
000240       03  HUH-ZS       PIC S9(006).                              前繰数
000250       03  HUH-ZK       PIC S9(009).                              前繰額
000260       03  HUH-NS       PIC S9(007).                              入庫数
000270       03  HUH-NK       PIC S9(010).                              入庫額
000280       03  HUH-SS       PIC S9(008).                              出荷数
000290       03  HUH-SK       PIC S9(010).                              出荷額
000300       03  HUH-YS       PIC S9(006).                              翌繰数
000310       03  HUH-YK       PIC S9(009).                              翌繰額
000320       03  HUH-UG       PIC S9(010).                              売上原価
000330*
000340     02  HUH-BC1        PIC  9(002).                              分類CD1
000350*****02  HUH-BCD1  REDEFINES HUH-BC1.                             D.940622
000360*****  03  HUH-BC11     PIC  9(001).                              D.940622
000370*****  03  HUH-BC12     PIC  9(001).                              D.940622
000380     02  HUH-BC2        PIC  9(002).                              分類CD2
000390     02  HUH-BCD2  REDEFINES HUH-BC2.
000400       03  HUH-BC21     PIC  9(001).
000410       03  HUH-BC22     PIC  9(001).
000420     02  HUH-BC3        PIC  9(002).                              分類CD3
000430     02  HUH-BCD3  REDEFINES HUH-BC3.
000440       03  HUH-BC31     PIC  9(001).
000450       03  HUH-BC32     PIC  9(001).
000460*
000470*****02  HUH-PBC        PIC  9(001).                              D.960905
000480     02  F              PIC  X(001).                              I.960905
000490     02  HUH-SOC        PIC  9(001).                              倉庫区分
000500*
000510     02  F              PIC  X(007).                              I.970709
000520*****02  F              PIC  X(009).                              D.970709

000010**************************************
000020*****     çHïiÅ@ãÊï™É}ÉXÉ^Å[     *****
000030*****      (  KKBM  64/4  )       *****
000040**************************************
000050 FD  KKB-M
000060     BLOCK  4 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "KKBM".
000090 01  KKB-R.
000100*    * * *   K E Y  ∫ ≥ ” ∏   * * *
000110     02  KKB-KEY.                                                 ∫∞ƒﬁ
000120       03  KKB-NO       PIC  9(002).                              áÇ
000130       03  KKB-BC       PIC  X(005).
000140       03  KKB-BC1  REDEFINES KKB-BC.
000150         04  KKB-YC     PIC  9(002).                              ópìr
000160         04  F          PIC  X(003).
000170*****  03  KKB-BC2  REDEFINES KKB-BC.                             D.141126
000180*****    04  KKB-KS1    PIC  9(001).                              D.141126
000190*****    04  F          PIC  X(004).                              D.141126
000200*****  03  KKB-BC3  REDEFINES KKB-BC.                             D.141126
000210*****    04  KKB-SNO    PIC  X(005).                              D.141126
000220       03  KKB-BC4  REDEFINES KKB-BC.
000230         04  KKB-KS2    PIC  9(002).                              ã@éÌÇQ
000240         04  F          PIC  X(003).
000250       03  KKB-BC5  REDEFINES KKB-BC.
000260         04  KKB-FRC    PIC  9(002).                              îpãpïsó«
000270         04  F          PIC  X(003).
000280*****  03  KKB-BC6  REDEFINES KKB-BC.                             D.141126
000290*****    04  KKB-SGC    PIC  9(002).                              D.141126
000300*****    04  F          PIC  X(003).                              D.141126
000310       03  KKB-BC7  REDEFINES KKB-BC.
000320         04  KKB-JSC    PIC  9(001).                              édì¸êÊáÇ
000330         04  F          PIC  X(004).
000340*****  03  KKB-BC8  REDEFINES KKB-BC.                             D.141126
000350*****    04  F          PIC  X(005).                              D.141126
000360       03  KKB-BC9  REDEFINES KKB-BC.                             I.980330
000370         04  F          PIC  X(005).                              I.980330
000380*    * * *   N A M E  ∫ ≥ ” ∏   * * *
000390     02  KKB-NAME1.
000400       03  KKB-YCN      PIC  N(016).                              ópìrãÊï™
000410       03  F            PIC  X(025).
000420*****02  KKB-NAME2  REDEFINES KKB-NAME1.                          D.141126
000430*****  03  KKB-KSN1     PIC  X(006).                              D.141126
000440*****  03  F            PIC  X(051).                              D.960314
000450*****  03  KKB-KSS      PIC  9(002).                              D.141126
000460*****  03  F            PIC  X(049).                              D.961014
000470*****  03  KKB-YSR      PIC  9(001)V9(02).                        D.141126
000480*****  03  F            PIC  X(046).                              D.141126
000490*****02  KKB-NAME3  REDEFINES KKB-NAME1.                          D.141126
000500*****  03  KKB-SNN      PIC  N(008).                              D.141126
000510*****  03  F            PIC  X(041).                              D.141126
000520     02  KKB-NAME4  REDEFINES KKB-NAME1.
000530       03  KKB-KSN2     PIC  X(006).                              ã@éÌÇQ
000540       03  F            PIC  X(051).
000550     02  KKB-NAME5  REDEFINES KKB-NAME1.
000560       03  KKB-FRN      PIC  N(006).                              îpãpïsó«
000570       03  F            PIC  X(045).
000580*****02  KKB-NAME6  REDEFINES KKB-NAME1.                          D.141126
000590*****  03  KKB-SGN      PIC  N(008).                              D.141126
000600*****  03  F            PIC  X(041).                              D.141126
000610     02  KKB-NAME7  REDEFINES KKB-NAME1.
000620       03  KKB-JSN      PIC  N(008).                              édì¸êÊñº
000630       03  KKB-SCO      PIC  9(004).                              I.980330
000640       03  F            PIC  X(037).                              I.980330
000650*****  03  F            PIC  X(041).                              D.980330
000660*****02  KKB-NAME8  REDEFINES KKB-NAME1.                          D.141126
000670*****  03  KKB-FRR      PIC  9(001)V9(02).                        D.141126
000680*****  03  F            PIC  X(054).                              D.141126
000690*****  03  KKB-YKR      PIC  9(001)V9(02).                        D.961118
000700*****  03  F            PIC  X(051).                              D.961118
000710     02  KKB-NAME9  REDEFINES KKB-NAME1.                          I.980330
000720       03  KKB-JRCA.                                              I.980330
000730         04  KKB-JRCD  OCCURS  4.                                 I.980330
000740           05  KKB-JRC  PIC  9(006).                              I.980330
000750       03  F            PIC  X(033).                              I.980330
000760     02  KKB-NAME90 REDEFINES KKB-NAME1.                          çÏã∆ãÊï™
000770       03  KKB-SC.
000780         04  KKB-SC01   PIC  9(001).                              îÑè„
000790         04  KKB-SC02   PIC  9(001).                              îÑè„ïœä∑
000800         04  KKB-SC03   PIC  9(001).                              ílà¯
000810         04  KKB-SC04   PIC  9(001).                              ì¸ã‡
000820         04  KKB-SC05   PIC  9(001).                              â¡ó∞
000830         04  KKB-SC06   PIC  9(001).                              îpãp
000840         04  KKB-SC07   PIC  9(001).                              ï•èoâÒé˚
000850         04  KKB-SC08   PIC  9(001).                              ìåäCédì¸
000860         04  KKB-SC09   PIC  9(001).                              --------
000870         04  KKB-SC10   PIC  9(001).                              --------
000880         04  KKB-SC11   PIC  9(001).                              --------
000890         04  KKB-SC12   PIC  9(001).                              --------
000900         04  KKB-SC13   PIC  9(001).                              ì˙éüçXêV
000910         04  KKB-SC14   PIC  9(001).                              ó\íËïœä∑
000920         04  KKB-SC15   PIC  9(001).                              åééüçXêV
000930       03  F            PIC  X(042).
000940     02  KKB-NAME95 REDEFINES KKB-NAME1.                          I.970219
000950       03  KKB-DNO      PIC  9(006).                              I.001013
000960*****  03  KKB-DNO      PIC  X(006).                              D.001013
000970*****  03  KKB-DNOD   REDEFINES KKB-DNO.                          D.001013
000980*****    04  KKB-DNO1   PIC  X(001).                              D.001013
000990*****    04  KKB-DNO2   PIC  9(005).                              D.001013
001000       03  F            PIC  X(051).

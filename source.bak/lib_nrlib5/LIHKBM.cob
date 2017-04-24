000010**************************************
000020*****     óöï®Å@ãÊï™É}ÉXÉ^Å[     *****
000030*****      (  HKBM  64/4  )      *****
000040**************************************
000050 FD  HKBM
000060     BLOCK  4 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "HKBM".
000090 01  HKB-R.
000100*    * * *   K E Y  ∫ ≥ ” ∏   * * *
000110     02  HKB-KEY.                                                 ∫∞ƒﬁ
000120       03  HKB-NO       PIC  9(002).                              áÇ
000130       03  HKB-BC       PIC  X(005).
000140       03  HKB-BC01  REDEFINES HKB-BC.
000150         04  HKB-TDFK   PIC  9(002).                              ìsìπï{åß
000160         04  F          PIC  X(003).
000170       03  HKB-BC02  REDEFINES HKB-BC.
000180         04  HKB-BM     PIC  9(001).                              ïîñÂ
000190         04  F          PIC  X(004).
000200*****  03  HKB-BC03  REDEFINES HKB-BC.                            D.050303
000210*****    04  HKB-TKC    PIC  9(002).                              D.050303
000220*****    04  F          PIC  X(003).                              D.050303
000230       03  HKB-BC04  REDEFINES HKB-BC.
000240         04  HKB-TNC    PIC  9(002).                              íSìñ
000250         04  F          PIC  X(003).
000260       03  HKB-BC05  REDEFINES HKB-BC.                            I.990528
000270         04  F          PIC  X(005).                              I.990528
000280       03  HKB-BC08  REDEFINES HKB-BC.
000290         04  HKB-KTKC   PIC  9(001).                              ã≥àÁínãÊ
000300         04  F          PIC  X(004).
000310       03  HKB-BC11  REDEFINES HKB-BC.
000320         04  HKB-BR1    PIC  9(002).                              ï™óﬁÇP
000330         04  F          PIC  X(003).
000340*****  03  HKB-BC12  REDEFINES HKB-BC.                            D.030514
000350*****    04  HKB-BR21   PIC  9(001).                              D.030514
000360*****    04  F          PIC  X(004).                              D.030514
000370       03  HKB-BC13  REDEFINES HKB-BC.
000380         04  HKB-BR22   PIC  9(001).                              ï™óﬁÇQ
000390         04  F          PIC  X(004).
000400       03  HKB-BC14  REDEFINES HKB-BC.                            I.980403
000410         04  HKB-BR3    PIC  9(002).                              ï™óﬁÇR
000420         04  F          PIC  X(003).
000430       03  HKB-BC16  REDEFINES HKB-BC.                            I.020507
000440         04  HKB-BMC    PIC  9(002).                              I.020507
000450         04  F          PIC  X(003).                              I.020507
000460*****  03  HKB-BC18  REDEFINES HKB-BC.                            D.141111
000470*****    04  HKB-KNC    PIC  9(001).                              D.141111
000480*****    04  F          PIC  X(004).                              D.141111
000490*****  03  HKB-BC19  REDEFINES HKB-BC.                            D.141111
000500*****    04  HKB-KHR    PIC  X(005).                              D.141111
000510       03  HKB-BC31  REDEFINES HKB-BC.
000520         04  HKB-NKC1   PIC  9(001).                              ì¸ã‡
000530         04  F          PIC  X(004).
000540       03  HKB-BC32  REDEFINES HKB-BC.                            I.991213
000550         04  HKB-NSC    PIC  9(001).                              I.991213
000560         04  F          PIC  X(004).                              I.991213
000570       03  HKB-BC41  REDEFINES HKB-BC.
000580         04  HKB-SUC    PIC  9(001).                              édè„éÛì¸
000590         04  F          PIC  X(004).
000600       03  HKB-BC42  REDEFINES HKB-BC.
000610         04  HKB-SSC    PIC  9(001).                              ê∂éY
000620         04  F          PIC  X(004).
000630*    * * *   N A M E  ∫ ≥ ” ∏   * * *
000640     02  HKB-NAME       PIC  X(057).
000650     02  HKB-NA01  REDEFINES HKB-NAME.
000660       03  HKB-FKNA     PIC  N(004).                              ï{åßñº
000670       03  HKB-SU       PIC  9(005).                              êlå˚
000680       03  HKB-KIN      PIC S9(010).                              îÑè„ã‡äz
000690       03  HKB-KTKCD    PIC  9(001).                              ã≥àÁínãÊ
000700       03  F            PIC  X(033).
000710     02  HKB-NA02  REDEFINES HKB-NAME.
000720       03  HKB-BMNA     PIC  N(006).                              ïîñÂñº
000730       03  F            PIC  X(045).
000740*****02  HKB-NA03  REDEFINES HKB-NAME.                            D.050303
000750*****  03  HKB-TKNA     PIC  N(012).                              D.050303
000760*****  03  F            PIC  X(033).                              D.050303
000770     02  HKB-NA04  REDEFINES HKB-NAME.
000780       03  HKB-TNNA     PIC  N(014).                              íSìññº
000790       03  F            PIC  X(029).
000800     02  HKB-NA05  REDEFINES HKB-NAME.                            I.990528
000810       03  HKB-UNN      PIC  9(006).                              I.990528
000820       03  HKB-SKN      PIC  9(006).                              I.990528
000830       03  HKB-NKN      PIC  9(006).                              I.990528
000840       03  HKB-DAI      PIC  X(010).                              I.990927
000850       03  F            PIC  X(029).                              I.990927
000860*****  03  F            PIC  X(039).                              D.990927
000870     02  HKB-NA08  REDEFINES HKB-NAME.
000880       03  HKB-KTNA     PIC  N(003).                              ã≥àÁínãÊ
000890       03  F            PIC  X(051).
000900     02  HKB-NA11  REDEFINES HKB-NAME.
000910       03  HKB-BRN1     PIC  N(008).                              ï™óﬁñºÇP
000920       03  F            PIC  X(041).
000930*****02  HKB-NA12  REDEFINES HKB-NAME.                            D.030514
000940*****  03  HKB-BRN21F   PIC  N(008).                              D.030514
000950*****  03  HKB-BRN21R   PIC  N(008).                              D.030514
000960*****  03  HKB-BRN21S   PIC  N(002).                              D.030514
000970*****  03  F            PIC  X(021).                              D.030514
000980     02  HKB-NA13  REDEFINES HKB-NAME.
000990       03  HKB-BRN22    PIC  N(003).                              ï™óﬁñºÇQ
001000       03  F            PIC  X(051).
001010     02  HKB-NA14  REDEFINES HKB-NAME.                            I.980403
001020       03  HKB-BRN3     PIC  N(003).                              ï™óﬁñºÇR
001030       03  F            PIC  X(051).
001040     02  HKB-NA16  REDEFINES HKB-NAME.                            I.020507
001050       03  HKB-BMN      PIC  N(003).                              I.020507
001060       03  F            PIC  X(051).                              I.020507
001070*****02  HKB-NA18  REDEFINES HKB-NAME.                            D.141111
001080*****  03  HKB-KNKH     PIC  9(004).                              D.141111
001090*****  03  F            PIC  X(053).                              D.141111
001100*****02  HKB-NA19  REDEFINES HKB-NAME.                            D.141111
001110*****  03  HKB-CKR      PIC  9(001)V9(03).                        D.141111
001120*****  03  HKB-KKR      PIC  9(001)V9(03).                        D.141111
001130*****  03  F            PIC  X(049).                              D.141111
001140     02  HKB-NA31  REDEFINES HKB-NAME.
001150       03  HKB-NKNA     PIC  N(006).                              ì¸ã‡ñº
001160       03  F            PIC  X(045).
001170     02  HKB-NA32  REDEFINES HKB-NAME.                            I.991213
001180       03  HKB-NSNA     PIC  N(006).                              I.991213
001190       03  F            PIC  X(045).                              I.991213
001200     02  HKB-NA41  REDEFINES HKB-NAME.
001210       03  HKB-SUNA     PIC  N(005).                              édè„éÛì¸
001220       03  F            PIC  X(047).
001230     02  HKB-NA42  REDEFINES HKB-NAME.
001240       03  HKB-SSNA     PIC  N(004).                              ê∂éY
001250       03  F            PIC  X(049).
001260*-----------------------------------------------------------------I.050226
001270     02  HKB-NA90  REDEFINES HKB-NAME.                            çÏã∆ãÊï™
001280       03  HKB-SC.
001290         04  HKB-SC01   PIC  9(001).                              îÑè„ì¸óÕ
001300         04  HKB-SC02   PIC  9(001).                              îÑè„ì`ï[
001310         04  HKB-SC03   PIC  9(001).                              édè„ì¸óÕ
001320         04  HKB-SC04   PIC  9(001).                              édè„ïœä∑
001330         04  HKB-SC05   PIC  9(001).                              ì¸ã‡ì¸óÕ
001340         04  HKB-SC06   PIC  9(001).                              ì¸ã‡ïœä∑
001350         04  HKB-SC07   PIC  9(001).                              ì¸ã‡ï[
001360         04  HKB-SC08   PIC  9(001).                              --------
001370         04  HKB-SC09   PIC  9(001).                              --------
001380         04  HKB-SC10   PIC  9(001).                              --------
001390         04  HKB-SC11   PIC  9(001).                              --------
001400         04  HKB-SC12   PIC  9(001).                              --------
001410         04  HKB-SC13   PIC  9(001).                              ì˙éüçXêV
001420         04  HKB-SC14   PIC  9(001).                              êøãÅèë
001430         04  HKB-SC15   PIC  9(001).                              åééüçXêV
001440       03  F            PIC  X(042).

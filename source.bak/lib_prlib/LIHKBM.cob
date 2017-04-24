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
000200       03  HKB-BC03  REDEFINES HKB-BC.
000210         04  HKB-TKC    PIC  9(002).                              ínãÊ
000220         04  F          PIC  X(003).
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
000340       03  HKB-BC12  REDEFINES HKB-BC.
000350         04  HKB-BR21   PIC  9(001).                              ï™óﬁÇQÇP
000360         04  F          PIC  X(004).
000370       03  HKB-BC13  REDEFINES HKB-BC.
000380         04  HKB-BR22   PIC  9(001).                              ï™óﬁÇQÇQ
000390         04  F          PIC  X(004).
000400       03  HKB-BC14  REDEFINES HKB-BC.                            I.980403
000410         04  HKB-BR3    PIC  9(002).                              ï™óﬁÇR
000420         04  F          PIC  X(003).
000430       03  HKB-BC18  REDEFINES HKB-BC.
000440         04  HKB-KNC    PIC  9(001).                              ä‘ê⁄
000450         04  F          PIC  X(004).
000460       03  HKB-BC19  REDEFINES HKB-BC.                            åoîÔó¶
000470         04  HKB-KHR    PIC  X(005).
000480       03  HKB-BC31  REDEFINES HKB-BC.
000490         04  HKB-NKC1   PIC  9(001).                              ì¸ã‡
000500         04  F          PIC  X(004).
000510       03  HKB-BC32  REDEFINES HKB-BC.                            I.991213
000520         04  HKB-NSC    PIC  9(001).                              I.991213
000530         04  F          PIC  X(004).                              I.991213
000540       03  HKB-BC41  REDEFINES HKB-BC.
000550         04  HKB-SUC    PIC  9(001).                              édè„éÛì¸
000560         04  F          PIC  X(004).
000570       03  HKB-BC42  REDEFINES HKB-BC.
000580         04  HKB-SSC    PIC  9(001).                              ê∂éY
000590         04  F          PIC  X(004).
000600*    * * *   N A M E  ∫ ≥ ” ∏   * * *
000610     02  HKB-NAME       PIC  X(057).
000620     02  HKB-NA01  REDEFINES HKB-NAME.
000630       03  HKB-FKNA     PIC  N(004).                              ï{åßñº
000640       03  HKB-SU       PIC  9(005).                              êlå˚
000650       03  HKB-KIN      PIC S9(010).                              îÑè„ã‡äz
000660       03  HKB-KTKCD    PIC  9(001).                              ã≥àÁínãÊ
000670       03  F            PIC  X(033).
000680     02  HKB-NA02  REDEFINES HKB-NAME.
000690       03  HKB-BMNA     PIC  N(006).                              ïîñÂñº
000700       03  F            PIC  X(045).
000710     02  HKB-NA03  REDEFINES HKB-NAME.
000720       03  HKB-TKNA     PIC  N(012).                              ínãÊñº
000730       03  F            PIC  X(033).
000740     02  HKB-NA04  REDEFINES HKB-NAME.
000750       03  HKB-TNNA     PIC  N(014).                              íSìññº
000760       03  F            PIC  X(029).
000770     02  HKB-NA05  REDEFINES HKB-NAME.                            I.990528
000780       03  HKB-UNN      PIC  9(006).                              I.990528
000790       03  HKB-SKN      PIC  9(006).                              I.990528
000800       03  HKB-NKN      PIC  9(006).                              I.990528
000810       03  HKB-DAI      PIC  X(010).                              I.990927
000820       03  F            PIC  X(029).                              I.990927
000830*****  03  F            PIC  X(039).                              D.990927
000840     02  HKB-NA08  REDEFINES HKB-NAME.
000850       03  HKB-KTNA     PIC  N(003).                              ã≥àÁínãÊ
000860       03  F            PIC  X(051).
000870     02  HKB-NA11  REDEFINES HKB-NAME.
000880       03  HKB-BRN1     PIC  N(008).                              ï™óﬁñºÇP
000890       03  F            PIC  X(041).
000900     02  HKB-NA12  REDEFINES HKB-NAME.
000910       03  HKB-BRN21F   PIC  N(008).                              ï™óﬁñºÇQ
000920       03  HKB-BRN21R   PIC  N(008).                              ï™óﬁñºÇQ
000930       03  HKB-BRN21S   PIC  N(002).                              ï™óﬁñºÇQ
000940       03  F            PIC  X(021).
000950     02  HKB-NA13  REDEFINES HKB-NAME.
000960       03  HKB-BRN22    PIC  N(003).                              ï™óﬁñºÇQ
000970       03  F            PIC  X(051).
000980     02  HKB-NA14  REDEFINES HKB-NAME.                            I.980403
000990       03  HKB-BRN3     PIC  N(003).                              ï™óﬁñºÇR
001000       03  F            PIC  X(051).
001010     02  HKB-NA18  REDEFINES HKB-NAME.
001020       03  HKB-KNKH     PIC  9(004).                              ä‘ê⁄îÔ
001030       03  F            PIC  X(053).
001040     02  HKB-NA19  REDEFINES HKB-NAME.
001050       03  HKB-CKR      PIC  9(001)V9(03).                        íºê⁄ó¶
001060       03  HKB-KKR      PIC  9(001)V9(03).                        ä‘ê⁄ó¶
001070       03  F            PIC  X(049).
001080     02  HKB-NA31  REDEFINES HKB-NAME.
001090       03  HKB-NKNA     PIC  N(006).                              ì¸ã‡ñº
001100       03  F            PIC  X(045).
001110     02  HKB-NA32  REDEFINES HKB-NAME.                            I.991213
001120       03  HKB-NSNA     PIC  N(006).                              I.991213
001130       03  F            PIC  X(045).                              I.991213
001140     02  HKB-NA41  REDEFINES HKB-NAME.
001150       03  HKB-SUNA     PIC  N(005).                              édè„éÛì¸
001160       03  F            PIC  X(047).
001170     02  HKB-NA42  REDEFINES HKB-NAME.
001180       03  HKB-SSNA     PIC  N(004).                              ê∂éY
001190       03  F            PIC  X(049).

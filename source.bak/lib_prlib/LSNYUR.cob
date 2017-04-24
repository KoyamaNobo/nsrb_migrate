000010**************************************************
000020*****                                        *****
000030*****     入　金　累　積　フ　ァ　イ　ル     *****
000040*****            ( NYURF )  64/4             *****
000050**************************************************
000060 FD  NYUR-F
000070*****BLOCK  6 RECORDS                                             D.970709
000080*****BLOCK  5 RECORDS                                             D.991202
000090*****BLOCK  4 RECORDS                                             D.000824
000100     BLOCK  3 RECORDS                                             I.000824
000110     LABEL RECORD IS STANDARD
000120     VALUE OF IDENTIFICATION "NYURF".
000130 01  NYUR-R.
000140     02  NUR-DATE       PIC  9(008).                              I.970709
000150     02  NUR-NGP   REDEFINES NUR-DATE.                            日付
000160       03  NUR-NG.
000170*****    04  NUR-NEN    PIC  9(002).                              D.970709
000180         04  NUR-NEN    PIC  9(004).                              I.970709
000190         04  NUR-NEND  REDEFINES NUR-NEN.                         I.970714
000200           05  NUR-NEN1 PIC  9(002).                              I.970714
000210           05  NUR-NEN2 PIC  9(002).                              I.970714
000220         04  NUR-GET    PIC  9(002).
000230       03  NUR-NGD   REDEFINES NUR-NG.                            I.970809
000240         04  F          PIC  9(002).                              I.970809
000250         04  NUR-NGS    PIC  9(004).                              I.970809
000260       03  NUR-PEY      PIC  9(002).
000270     02  NUR-NGPD  REDEFINES NUR-DATE.                            I.970714
000280       03  F            PIC  9(002).                              I.970714
000290       03  NUR-NGPS     PIC  9(006).                              I.970714
000300*****02  NUR-DATE  REDEFINES NUR-NGP  PIC  9(006).                D.970709
000310     02  NUR-TCD        PIC  9(004).                              得意先C
000320     02  NUR-KIN        PIC S9(008).                              入金金額
000330     02  NUR-NC.                                                  入金区分
000340       03  NUR-NC1      PIC  9(001).
000350       03  NUR-NC2      PIC  9(001).
000360     02  NUR-NSC        PIC  9(001).                              I.991213
000370*****02  NUR-TGK        PIC  9(006).                              D.970711
000380     02  NUR-TGK        PIC  9(008).                              I.970711
000390     02  NUR-TGKD  REDEFINES NUR-TGK.                             I.950227
000400*****  03  NUR-TGN      PIC  9(002).                              D.970711
000410       03  NUR-TGN      PIC  9(004).                              I.970711
000420       03  NUR-TGND  REDEFINES NUR-TGN.                           I.970714
000430         04  NUR-TGN1   PIC  9(002).                              I.970714
000440         04  NUR-TGN2   PIC  9(002).                              I.970714
000450       03  NUR-TGGP     PIC  9(004).                              I.950227
000460       03  NUR-TGGPD REDEFINES NUR-TGGP.                          I.970714
000470         04  NUR-TGG    PIC  9(002).                              I.970714
000480         04  NUR-TGP    PIC  9(002).                              I.970714
000490     02  NUR-TGKL  REDEFINES NUR-TGK.                             I.970714
000500       03  F            PIC  9(002).                              I.970714
000510       03  NUR-TGKS     PIC  9(006).                              I.970714
000520     02  NUR-SS.                                                  請求締日
000530*****  03  NUR-SSN      PIC  9(002).                              D.970714
000540       03  NUR-SSN      PIC  9(004).                              I.970714
000550       03  NUR-SSND  REDEFINES NUR-SSN.                           I.970714
000560         04  NUR-SSN1   PIC  9(002).                              I.970714
000570         04  NUR-SSN2   PIC  9(002).                              I.970714
000580       03  NUR-SSG      PIC  9(002).
000590     02  NUR-SSD   REDEFINES NUR-SS.                              I.970714
000600       03  F            PIC  9(002).                              I.970714
000610       03  NUR-SSS      PIC  9(004).                              I.970714
000620     02  NUR-BC         PIC  9(001).                              部門C
000630     02  NUR-TC         PIC  9(002).                              担当C
000640     02  NUR-SP         PIC  9(002).                              I.950227
000650     02  NUR-SK         PIC  9(001).                              I.950227
000660     02  NUR-KEY.                                                 I.991202
000670       03  NUR-NO       PIC  9(006).                              I.991202
000680       03  NUR-GNO      PIC  9(001).                              I.991202
000690     02  NUR-FDNO.                                                I.991202
000700       03  NUR-FNO      PIC  9(006).                              I.991202
000710       03  NUR-FGNO     PIC  9(002).                              I.991202
000720     02  NUR-SKD        PIC  9(008).                              I.000824
000730     02  F              PIC  X(019).                              I.000824
000740*****02  F              PIC  X(006).                              I.000824
000750*****02  F              PIC  X(007).                              D.991213
000760*****02  NUR-NO         PIC  9(006).                              D.991202
000770*****02  F              PIC  X(003).                              D.991202
000780*****02  F              PIC  X(013).                              D.970711
000790*****02  F              PIC  X(006).                              D.970709

000010**************************************************
000020*****                                        *****
000030*****     入　金　累　積　フ　ァ　イ　ル     *****
000040*****            ( NYURF )  51/5             *****
000050**************************************************
000060 FD  NYUR-F
000070*****BLOCK  6 RECORDS                                             D.970709
000080     BLOCK  5 RECORDS                                             I.970709
000090     LABEL RECORD IS STANDARD
000100     VALUE OF IDENTIFICATION "NYURF".
000110 01  NYUR-R.
000120     02  NUR-DATE       PIC  9(008).                              I.970709
000130     02  NUR-NGP   REDEFINES NUR-DATE.                            日付
000140       03  NUR-NG.
000150*****    04  NUR-NEN    PIC  9(002).                              D.970709
000160         04  NUR-NEN    PIC  9(004).                              I.970709
000170         04  NUR-NEND  REDEFINES NUR-NEN.                         I.970714
000180           05  NUR-NEN1 PIC  9(002).                              I.970714
000190           05  NUR-NEN2 PIC  9(002).                              I.970714
000200         04  NUR-GET    PIC  9(002).
000210       03  NUR-NGD   REDEFINES NUR-NG.                            I.970809
000220         04  F          PIC  9(002).                              I.970809
000230         04  NUR-NGS    PIC  9(004).                              I.970809
000240       03  NUR-PEY      PIC  9(002).
000250     02  NUR-NGPD  REDEFINES NUR-DATE.                            I.970714
000260       03  F            PIC  9(002).                              I.970714
000270       03  NUR-NGPS     PIC  9(006).                              I.970714
000280*****02  NUR-DATE  REDEFINES NUR-NGP  PIC  9(006).                D.970709
000290     02  NUR-TCD        PIC  9(004).                              得意先C
000300     02  NUR-KIN        PIC S9(008).                              入金金額
000310     02  NUR-NC.                                                  入金区分
000320       03  NUR-NC1      PIC  9(001).
000330       03  NUR-NC2      PIC  9(001).
000340*****02  NUR-TGK        PIC  9(006).                              D.970711
000350     02  NUR-TGK        PIC  9(008).                              I.970711
000360     02  NUR-TGKD  REDEFINES NUR-TGK.                             I.950227
000370*****  03  NUR-TGN      PIC  9(002).                              D.970711
000380       03  NUR-TGN      PIC  9(004).                              I.970711
000390       03  NUR-TGND  REDEFINES NUR-TGN.                           I.970714
000400         04  NUR-TGN1   PIC  9(002).                              I.970714
000410         04  NUR-TGN2   PIC  9(002).                              I.970714
000420       03  NUR-TGGP     PIC  9(004).                              I.950227
000430       03  NUR-TGGPD REDEFINES NUR-TGGP.                          I.970714
000440         04  NUR-TGG    PIC  9(002).                              I.970714
000450         04  NUR-TGP    PIC  9(002).                              I.970714
000460     02  NUR-TGKL  REDEFINES NUR-TGK.                             I.970714
000470       03  F            PIC  9(002).                              I.970714
000480       03  NUR-TGKS     PIC  9(006).                              I.970714
000490     02  NUR-SS.                                                  請求締日
000500*****  03  NUR-SSN      PIC  9(002).                              D.970714
000510       03  NUR-SSN      PIC  9(004).                              I.970714
000520       03  NUR-SSND  REDEFINES NUR-SSN.                           I.970714
000530         04  NUR-SSN1   PIC  9(002).                              I.970714
000540         04  NUR-SSN2   PIC  9(002).                              I.970714
000550       03  NUR-SSG      PIC  9(002).
000560     02  NUR-SSD   REDEFINES NUR-SS.                              I.970714
000570       03  F            PIC  9(002).                              I.970714
000580       03  NUR-SSS      PIC  9(004).                              I.970714
000590     02  NUR-BC         PIC  9(001).                              部門C
000600     02  NUR-TC         PIC  9(002).                              担当C
000610     02  NUR-SP         PIC  9(002).                              I.950227
000620     02  NUR-SK         PIC  9(001).                              I.950227
000630     02  NUR-NO         PIC  9(006).                              I.950227
000640     02  F              PIC  X(003).                              I.970711
000650*****02  F              PIC  X(013).                              D.970711
000660*****02  F              PIC  X(006).                              D.970709

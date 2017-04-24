000010****************************
000020**                        **
000030**    日付　マスター      **
000040**   256/1 (DATEM)        **
000050****************************
000060 FD  M-DATE
000070     BLOCK  1 RECORDS
000080     LABEL RECORD IS STANDARD
000090     VALUE OF IDENTIFICATION  "DATEM".
000100 01  DATE-R.
000110     02  DATE-KEY        PIC  X(002).
000120     02  DATE-02R        PIC  9(006).
000130     02  DATE-02   REDEFINES DATE-02R.                            履物
000140       03  DATE-021      PIC  9(002).
000150       03  DATE-022      PIC  9(002).
000160       03  DATE-023      PIC  9(002).
000170     02  DATE-03R        PIC  9(006).
000180     02  DATE-03   REDEFINES DATE-03R.                            工品
000190       03  DATE-031      PIC  9(002).
000200       03  DATE-032      PIC  9(002).
000210       03  DATE-033      PIC  9(002).
000220     02  DATE-04R        PIC  9(006).
000230     02  DATE-04   REDEFINES DATE-04R.                            手形
000240       03  DATE-041      PIC  9(002).
000250       03  DATE-042      PIC  9(002).
000260       03  DATE-043      PIC  9(002).
000270     02  DATE-05R        PIC  9(006).
000280     02  DATE-05   REDEFINES DATE-05R.                            購買
000290       03  DATE-051      PIC  9(002).
000300       03  DATE-052      PIC  9(002).
000310       03  DATE-053      PIC  9(002).
000320     02  DATE-06R        PIC  9(006).
000330     02  DATE-06   REDEFINES DATE-06R.                            その他１
000340       03  DATE-061      PIC  9(002).
000350       03  DATE-062      PIC  9(002).
000360       03  DATE-063      PIC  9(002).
000370     02  DATE-07R        PIC  9(006).
000380     02  DATE-07   REDEFINES DATE-07R.                            その他２
000390       03  DATE-071      PIC  9(002).
000400       03  DATE-072      PIC  9(002).
000410       03  DATE-073      PIC  9(002).
000420*    [    最　終　日　付　　]
000430*       (   履物   )
000440     02  D-HSD           PIC  9(006).                             出荷
000450     02  D-HSDD    REDEFINES D-HSD.
000460       03  D-HSN         PIC  9(002).
000470       03  D-HSG         PIC  9(002).
000480       03  D-HSP         PIC  9(002).
000490     02  D-HND           PIC  9(006).                             入庫
000500     02  D-HNDD    REDEFINES D-HND.
000510       03  D-HNN         PIC  9(002).
000520       03  D-HNG         PIC  9(002).
000530       03  D-HNP         PIC  9(002).
000540*       (   工品   )
000550     02  D-KUD           PIC  9(006).                             売上
000560     02  D-KUDD    REDEFINES D-KUD.
000570       03  D-KUN         PIC  9(002).
000580       03  D-KUG         PIC  9(002).
000590       03  D-KUP         PIC  9(002).
000600     02  D-KKD           PIC  9(006).                             加硫
000610     02  D-KKDD    REDEFINES D-KKD.
000620       03  D-KKN         PIC  9(002).
000630       03  D-KKG         PIC  9(002).
000640       03  D-KKP         PIC  9(002).
000650     02  D-KSD           PIC  9(006).                             仕入支払
000660     02  D-KSDD    REDEFINES D-KSD.
000670       03  D-KSN         PIC  9(002).
000680       03  D-KSG         PIC  9(002).
000690       03  D-KSP         PIC  9(002).
000700     02  D-KRD           PIC  9(006).                             労働時間
000710     02  D-KRDD    REDEFINES D-KRD.
000720       03  D-KRN         PIC  9(002).
000730       03  D-KRG         PIC  9(002).
000740       03  D-KRP         PIC  9(002).
000750*
000760     02  D-TGD           PIC  9(006).                             手形
000770     02  D-TGDD    REDEFINES D-TGD.
000780       03  D-TGN         PIC  9(002).
000790       03  D-TGG         PIC  9(002).
000800       03  D-TGP         PIC  9(002).
000810*
000820     02  D-KBD           PIC  9(006).                             購買
000830     02  D-KBDD    REDEFINES D-KBD.
000840       03  D-KBN         PIC  9(002).
000850       03  D-KBG         PIC  9(002).
000860       03  D-KBP         PIC  9(002).
000870*
000880*****02  F               PIC  X(022).                             D.950713
000890     02  F               PIC  X(014).                             D.950713
000900*
000910*    [    年　間　年　月　　]
000920*         (  作表　年月  )                                        I.950713
000930     02  D-NPDATE.
000940       03  D-SPNG        PIC  9(004).                             開始年月
000950       03  D-SPDATE REDEFINES D-SPNG.
000960         04  D-SPNEN     PIC  9(002).
000970         04  D-SPGET     PIC  9(002).
000980       03  D-EPNG        PIC  9(004).                             最終年月
000990       03  D-EPDATE REDEFINES D-EPNG.
001000         04  D-EPNEN     PIC  9(002).
001010         04  D-EPGET     PIC  9(002).
001020*
001030*         (  教育　年月  )
001040     02  D-NKDATE.
001050       03  D-SKNG        PIC  9(004).                             開始年月
001060       03  D-SKDATE REDEFINES D-SKNG.
001070         04  D-SKNEN     PIC  9(002).
001080         04  D-SKGET     PIC  9(002).
001090       03  D-EKNG        PIC  9(004).                             最終年月
001100       03  D-EKDATE REDEFINES D-EKNG.
001110         04  D-EKNEN     PIC  9(002).
001120         04  D-EKGET     PIC  9(002).
001130*
001140*         (  その他　年月  )
001150     02  D-NDATE.
001160       03  D-SNG         PIC  9(004).                             開始年月
001170       03  D-SDATE REDEFINES D-SNG.
001180         04  D-SNEN      PIC  9(002).
001190         04  D-SGET      PIC  9(002).
001200       03  D-ENG         PIC  9(004).                             最終年月
001210       03  D-EDATE REDEFINES D-ENG.
001220         04  D-ENEN      PIC  9(002).
001230         04  D-EGET      PIC  9(002).
001240*
001250*    [    実　行　年　月　　]
001260     02  D-NNG.
001270       03  D-NHNG        PIC  9(004).                             履物
001280       03  D-NHNGD REDEFINES D-NHNG.
001290         04  D-NHN       PIC  9(002).
001300         04  D-NHG       PIC  9(002).
001310       03  D-NKNG        PIC  9(004).                             工品
001320       03  D-NKNGD REDEFINES D-NKNG.
001330         04  D-NKN       PIC  9(002).
001340         04  D-NKG       PIC  9(002).
001350       03  D-NTNG        PIC  9(004).                             手形
001360       03  D-NTNGD REDEFINES D-NTNG.
001370         04  D-NTN       PIC  9(002).
001380         04  D-NTG       PIC  9(002).
001390       03  D-NBNG        PIC  9(004).                             購買
001400       03  D-NBNGD REDEFINES D-NBNG.
001410         04  D-NBN       PIC  9(002).
001420         04  D-NBG       PIC  9(002).
001430       03  D-NJNG        PIC  9(004).                             給与
001440       03  D-NJNGD REDEFINES D-NJNG.
001450         04  D-NJN       PIC  9(002).
001460         04  D-NJG       PIC  9(002).
001470       03  D-NGNG        PIC  9(004).                             内職
001480       03  D-NGNGD REDEFINES D-NGNG.
001490         04  D-NGN       PIC  9(002).
001500         04  D-NGG       PIC  9(002).
001510       03  D-NRNG        PIC  9(004).                             生産№
001520       03  D-NRNGD REDEFINES D-NRNG.
001530         04  D-NRN       PIC  9(002).
001540         04  D-NRG       PIC  9(002).
001550       03  D-NING        PIC  9(004).                             教育
001560       03  D-NINGD REDEFINES D-NING.
001570         04  D-NIN       PIC  9(002).
001580         04  D-NIG       PIC  9(002).
001590       03  D-NANG        PIC  9(004).                             全体
001600       03  D-NANGD REDEFINES D-NANG.
001610         04  D-NAN       PIC  9(002).
001620         04  D-NAG       PIC  9(002).
001630*
001640     02  F               PIC  X(020).
001650*
001660*    [  年間Ｆ削除年月  ]                                         I.970402
001670     02  D-NFDD          PIC  9(004).                             年月
001680*
001690*    [  年間サイズ別年月  ]                                       I.950808
001700     02  D-SSNG          PIC  9(004).                             年月(1)
001710     02  D-SSNGD  REDEFINES D-SSNG.
001720       03  D-SSN         PIC  9(002).
001730       03  D-SSG         PIC  9(002).
001740     02  D-ESNG          PIC  9(004).                             年月(2)
001750     02  D-ESNGD  REDEFINES D-ESNG.
001760       03  D-ESN         PIC  9(002).
001770       03  D-ESG         PIC  9(002).
001780*    [  工品手配年月  ]
001790     02  D-KTNG1         PIC  9(004).                             年月(1)
001800     02  D-KTNG2         PIC  9(004).                             年月(2)
001810*
001820*    [  履物日計更新(HMD550) チェック  ]
001830     02  D-HKC           PIC  9(001).                             更新C
001840*
001850*    [  履物売上目標　年  ]
001860     02  DATE-HMN        PIC  9(002).                             目標年
001870*
001880*    [  履物年間累積チェック(ﾌﾛｯﾋﾟｰ)  ]
001890     02  DATE-NRC        PIC  9(001).                             累積C
001900*
001910*    [  履物月間区分変更  (0.変更なし , 1.変更あり)  ]
001920     02  DATE-TM.                                                 得意先
001930       03  DATE-HBC      PIC  9(001).                              部門
001940       03  F             PIC  9(001).                             I.050303
001950*****  03  DATE-HTKC     PIC  9(001).                             D.050303
001960       03  DATE-HTNC     PIC  9(001).                              担当
001970       03  F             PIC  9(001).
001980     02  DATE-HM.                                                 品名
001990       03  DATE-HBC1     PIC  9(001).                              分類1
002000       03  DATE-HBC2     PIC  9(001).                              分類2
002010       03  DATE-HBC3     PIC  9(001).                              分類3
002020       03  F             PIC  9(001).
002030*
002040*    [  製造変換チェック  ]
002050     02  DATE-SHC        PIC  9(001).
002060*    [  品名売上自動振替チェック  ]
002070     02  DATE-HFC        PIC  9(001).                             I.020208
002080*
002090     02  F               PIC  X(010).                             I.020208
002100*****02  F               PIC  X(011).                             D.020208
002110*****02  F               PIC  X(012).                             D.010123
002120*****02  F               PIC  X(028).                             D.970902
002130*****02  F               PIC  X(052).                             D.970303
002140*****02  F               PIC  X(036).                             D.970409
002150     02  DATE-WC.                                                 和暦C
002160       03  DATE-WC1.                                              平成年
002170         04  DATE-YF1    PIC  9(002).                             開始年
002180         04  DATE-YT1    PIC  9(002).                             終了年
002190         04  DATE-YC1    PIC  9(004).                             +1988
002200       03  DATE-WC2.                                              昭和年
002210         04  DATE-YF2    PIC  9(002).                             開始年
002220         04  DATE-YT2    PIC  9(002).                             終了年
002230         04  DATE-YC2    PIC  9(004).                             +1925
002240*
002250     02  DATE-SC.                                                 西暦C
002260       03  DATE-SC1.                                              1900年
002270         04  DATE-NF1    PIC  9(002).                             開始年
002280         04  DATE-NT1    PIC  9(002).                             終了年
002290         04  DATE-NC1    PIC  9(004).                             +1900
002300       03  DATE-SC2.                                              2000年
002310         04  DATE-NF2    PIC  9(002).                             開始年
002320         04  DATE-NT2    PIC  9(002).                             終了年
002330         04  DATE-NC2    PIC  9(004).                             +2000
002340*

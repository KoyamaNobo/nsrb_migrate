      ****************************
      **                        **
      **    日付　マスター      **
      **   256/1 (DATEM)        **
      ****************************
       01  M-DATE.
           02  M-DATE_PNAME1   PIC  X(005) VALUE "DATEM".
           02  F               PIC  X(001).
           02  M-DATE_LNAME    PIC  X(006) VALUE "M-DATE".
           02  F               PIC  X(001).
           02  M-DATE_KEY1     PIC  X(100) VALUE SPACE.
           02  M-DATE_SORT     PIC  X(100) VALUE SPACE.
           02  M-DATE_IDLST    PIC  X(100) VALUE SPACE.
           02  M-DATE_RES      USAGE  POINTER.
      *
       01  DATE-R.
           02  DATE-KEY        PIC  X(002).
           02  DATE-02R        PIC  9(006).
           02  DATE-02   REDEFINES DATE-02R.                            履物
             03  DATE-021      PIC  9(002).
             03  DATE-022      PIC  9(002).
             03  DATE-023      PIC  9(002).
           02  DATE-03R        PIC  9(006).
           02  DATE-03   REDEFINES DATE-03R.                            工品
             03  DATE-031      PIC  9(002).
             03  DATE-032      PIC  9(002).
             03  DATE-033      PIC  9(002).
           02  DATE-04R        PIC  9(006).
           02  DATE-04   REDEFINES DATE-04R.                            手形
             03  DATE-041      PIC  9(002).
             03  DATE-042      PIC  9(002).
             03  DATE-043      PIC  9(002).
           02  DATE-05R        PIC  9(006).
           02  DATE-05   REDEFINES DATE-05R.                            購買
             03  DATE-051      PIC  9(002).
             03  DATE-052      PIC  9(002).
             03  DATE-053      PIC  9(002).
           02  DATE-06R        PIC  9(006).
           02  DATE-06   REDEFINES DATE-06R.                            その他１
             03  DATE-061      PIC  9(002).
             03  DATE-062      PIC  9(002).
             03  DATE-063      PIC  9(002).
           02  DATE-07R        PIC  9(006).
           02  DATE-07   REDEFINES DATE-07R.                            その他２
             03  DATE-071      PIC  9(002).
             03  DATE-072      PIC  9(002).
             03  DATE-073      PIC  9(002).
      *    [    最　終　日　付　　]
      *       (   履物   )
           02  D-HSD           PIC  9(006).                             出荷
           02  D-HSDD    REDEFINES D-HSD.
             03  D-HSN         PIC  9(002).
             03  D-HSG         PIC  9(002).
             03  D-HSP         PIC  9(002).
           02  D-HND           PIC  9(006).                             入庫
           02  D-HNDD    REDEFINES D-HND.
             03  D-HNN         PIC  9(002).
             03  D-HNG         PIC  9(002).
             03  D-HNP         PIC  9(002).
      *       (   工品   )
           02  D-KUD           PIC  9(006).                             売上
           02  D-KUDD    REDEFINES D-KUD.
             03  D-KUN         PIC  9(002).
             03  D-KUG         PIC  9(002).
             03  D-KUP         PIC  9(002).
           02  D-KKD           PIC  9(006).                             加硫
           02  D-KKDD    REDEFINES D-KKD.
             03  D-KKN         PIC  9(002).
             03  D-KKG         PIC  9(002).
             03  D-KKP         PIC  9(002).
           02  D-KSD           PIC  9(006).                             仕入支払
           02  D-KSDD    REDEFINES D-KSD.
             03  D-KSN         PIC  9(002).
             03  D-KSG         PIC  9(002).
             03  D-KSP         PIC  9(002).
           02  D-KRD           PIC  9(006).                             労働時間
           02  D-KRDD    REDEFINES D-KRD.
             03  D-KRN         PIC  9(002).
             03  D-KRG         PIC  9(002).
             03  D-KRP         PIC  9(002).
      *
           02  D-TGD           PIC  9(006).                             手形
           02  D-TGDD    REDEFINES D-TGD.
             03  D-TGN         PIC  9(002).
             03  D-TGG         PIC  9(002).
             03  D-TGP         PIC  9(002).
      *
           02  D-KBD           PIC  9(006).                             購買
           02  D-KBDD    REDEFINES D-KBD.
             03  D-KBN         PIC  9(002).
             03  D-KBG         PIC  9(002).
             03  D-KBP         PIC  9(002).
      *
           02  F               PIC  X(014).
      *
      *    [    年　間　年　月　　]
      *         (  作表　年月  )
           02  D-NPDATE.
             03  D-SPNG        PIC  9(004).                             開始年月
             03  D-SPDATE REDEFINES D-SPNG.
               04  D-SPNEN     PIC  9(002).
               04  D-SPGET     PIC  9(002).
             03  D-EPNG        PIC  9(004).                             最終年月
             03  D-EPDATE REDEFINES D-EPNG.
               04  D-EPNEN     PIC  9(002).
               04  D-EPGET     PIC  9(002).
      *
      *         (  教育　年月  )
           02  D-NKDATE.
             03  D-SKNG        PIC  9(004).                             開始年月
             03  D-SKDATE REDEFINES D-SKNG.
               04  D-SKNEN     PIC  9(002).
               04  D-SKGET     PIC  9(002).
             03  D-EKNG        PIC  9(004).                             最終年月
             03  D-EKDATE REDEFINES D-EKNG.
               04  D-EKNEN     PIC  9(002).
               04  D-EKGET     PIC  9(002).
      *
      *         (  その他　年月  )
           02  D-NDATE.
             03  D-SNG         PIC  9(004).                             開始年月
             03  D-SDATE REDEFINES D-SNG.
               04  D-SNEN      PIC  9(002).
               04  D-SGET      PIC  9(002).
             03  D-ENG         PIC  9(004).                             最終年月
             03  D-EDATE REDEFINES D-ENG.
               04  D-ENEN      PIC  9(002).
               04  D-EGET      PIC  9(002).
      *
      *    [    実　行　年　月　　]
           02  D-NNG.
             03  D-NHNG        PIC  9(004).                             履物
             03  D-NHNGD REDEFINES D-NHNG.
               04  D-NHN       PIC  9(002).
               04  D-NHG       PIC  9(002).
             03  D-NKNG        PIC  9(004).                             工品
             03  D-NKNGD REDEFINES D-NKNG.
               04  D-NKN       PIC  9(002).
               04  D-NKG       PIC  9(002).
             03  D-NTNG        PIC  9(004).                             手形
             03  D-NTNGD REDEFINES D-NTNG.
               04  D-NTN       PIC  9(002).
               04  D-NTG       PIC  9(002).
             03  D-NBNG        PIC  9(004).                             購買
             03  D-NBNGD REDEFINES D-NBNG.
               04  D-NBN       PIC  9(002).
               04  D-NBG       PIC  9(002).
             03  D-NJNG        PIC  9(004).                             給与
             03  D-NJNGD REDEFINES D-NJNG.
               04  D-NJN       PIC  9(002).
               04  D-NJG       PIC  9(002).
             03  D-NGNG        PIC  9(004).                             内職
             03  D-NGNGD REDEFINES D-NGNG.
               04  D-NGN       PIC  9(002).
               04  D-NGG       PIC  9(002).
             03  D-NRNG        PIC  9(004).                             生産№
             03  D-NRNGD REDEFINES D-NRNG.
               04  D-NRN       PIC  9(002).
               04  D-NRG       PIC  9(002).
             03  D-NING        PIC  9(004).                             教育
             03  D-NINGD REDEFINES D-NING.
               04  D-NIN       PIC  9(002).
               04  D-NIG       PIC  9(002).
             03  D-NANG        PIC  9(004).                             全体
             03  D-NANGD REDEFINES D-NANG.
               04  D-NAN       PIC  9(002).
               04  D-NAG       PIC  9(002).
      *
           02  F               PIC  X(020).
      *
      *    [  年間Ｆ削除年月  ]
           02  D-NFDD          PIC  9(004).                             年月
      *
      *    [  年間サイズ別年月  ]
           02  D-SSNG          PIC  9(004).                             年月(1)
           02  D-SSNGD  REDEFINES D-SSNG.
             03  D-SSN         PIC  9(002).
             03  D-SSG         PIC  9(002).
           02  D-ESNG          PIC  9(004).                             年月(2)
           02  D-ESNGD  REDEFINES D-ESNG.
             03  D-ESN         PIC  9(002).
             03  D-ESG         PIC  9(002).
      *    [  工品手配年月  ]
           02  D-KTNG1         PIC  9(004).                             年月(1)
           02  D-KTNG2         PIC  9(004).                             年月(2)
      *
      *    [  履物日計更新(HMD550) チェック  ]
           02  D-HKC           PIC  9(001).                             更新C
      *
      *    [  履物売上目標　年  ]
           02  DATE-HMN        PIC  9(002).                             目標年
      *
      *    [  履物年間累積チェック(ﾌﾛｯﾋﾟｰ)  ]
           02  DATE-NRC        PIC  9(001).                             累積C
      *
      *    [  履物月間区分変更  (0.変更なし , 1.変更あり)  ]
           02  DATE-TM.                                                 得意先
             03  DATE-HBC      PIC  9(001).                              部門
             03  F             PIC  9(001).
             03  DATE-HTNC     PIC  9(001).                              担当
             03  F             PIC  9(001).
           02  DATE-HM.                                                 品名
             03  DATE-HBC1     PIC  9(001).                              分類1
             03  DATE-HBC2     PIC  9(001).                              分類2
             03  DATE-HBC3     PIC  9(001).                              分類3
             03  F             PIC  9(001).
      *
      *    [  製造変換チェック  ]
           02  DATE-SHC        PIC  9(001).
      *    [  品名売上自動振替チェック  ]
           02  DATE-HFC        PIC  9(001).
      *
           02  F               PIC  X(010).
           02  DATE-WC.                                                 和暦C
             03  DATE-WC1.                                              平成年
               04  DATE-YF1    PIC  9(002).                             開始年
               04  DATE-YT1    PIC  9(002).                             終了年
               04  DATE-YC1    PIC  9(004).                             +1988
             03  DATE-WC2.                                              昭和年
               04  DATE-YF2    PIC  9(002).                             開始年
               04  DATE-YT2    PIC  9(002).                             終了年
               04  DATE-YC2    PIC  9(004).                             +1925
      *
           02  DATE-SC.                                                 西暦C
             03  DATE-SC1.                                              1900年
               04  DATE-NF1    PIC  9(002).                             開始年
               04  DATE-NT1    PIC  9(002).                             終了年
               04  DATE-NC1    PIC  9(004).                             +1900
             03  DATE-SC2.                                              2000年
               04  DATE-NF2    PIC  9(002).                             開始年
               04  DATE-NT2    PIC  9(002).                             終了年
               04  DATE-NC2    PIC  9(004).                             +2000
       77  F                   PIC X(1).
      *

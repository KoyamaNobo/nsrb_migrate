      ******************************************
      *****     人　事　マ　ス　タ　ー     *****
      *****      (  JINJIM  512/1  )       *****
      ******************************************
       01  JINJIM.
           02  JINJIM_PNAME1      PIC  X(006) VALUE "JINJIM".
           02  F                  PIC  X(001).
           02  JINJIM_LNAME       PIC  X(006) VALUE "JINJIM".
           02  F                  PIC  X(001).
           02  JINJIM_KEY1        PIC  X(100) VALUE SPACE.
           02  JINJIM_SORT        PIC  X(100) VALUE SPACE.
           02  JINJIM_IDLST       PIC  X(100) VALUE SPACE.
           02  JINJIM_RES         USAGE  POINTER.
       01  JINJI-R.
      *    * * *   ｺ ﾃ ｲ  ｺ ｳ ﾓ ｸ   * * *
           02  J-CODE.                                                  ｺｰﾄﾞ
             03  J-SCD            PIC  9(004).                          ｼｮｿﾞｸC
             03  J-SCDD  REDEFINES J-SCD.                               ｼｮｿﾞｸC
               04  J-SCD1         PIC  9(002).
               04  J-SCD2         PIC  9(002).
             03  J-SZC   REDEFINES J-SCD.                               ｼｮｿﾞｸC
               04  J-SZC1         PIC  9(003).
               04  J-SZC2         PIC  9(001).
             03  J-KEY.                                                 ｼｬｲﾝC
               04  J-SIC          PIC  9(004).
           02  J-NAME             PIC  N(008).                          ｼﾒｲ
           02  J-ANA              PIC  X(016).                          ｼﾒｲ ANK
           02  J-ADR              PIC  N(030).                          ｼﾞｭｳｼｮ
           02  J-UNO              PIC  X(008).
           02  J-TEL              PIC  X(014).
      *
           02  J-SNGP             PIC  9(008).
           02  J-SNGPD REDEFINES J-SNGP.
             03  J-SNEN           PIC  9(004).
             03  J-SGP.
               04  J-SGET         PIC  9(002).
               04  J-SPEY         PIC  9(002).
           02  J-SNGPDS REDEFINES J-SNGP.
             03  F                PIC  9(002).
             03  J-SNGPS          PIC  9(006).
           02  J-NNGP             PIC  9(008).
           02  J-NNGPD REDEFINES J-NNGP.
             03  J-NNG.
               04  J-NNEN         PIC  9(004).
               04  J-NGET         PIC  9(002).
             03  J-NPEY           PIC  9(002).
           02  J-NNGPDS REDEFINES J-NNGP.
             03  F                PIC  9(002).
             03  J-NNGPS          PIC  9(006).
           02  J-TNGP             PIC  9(008).
           02  J-TNGPD REDEFINES J-TNGP.
             03  J-TNG.
               04  J-TNEN         PIC  9(004).
               04  J-TGET         PIC  9(002).
             03  J-TPEY           PIC  9(002).
           02  J-TNGPDS REDEFINES J-TNGP.
             03  F                PIC  9(002).
             03  J-TNGPS          PIC  9(006).
      *
           02  J-KHN              PIC  X(010).                          ﾎｹﾝNO
           02  J-KNN              PIC  X(010).                          ﾈﾝｷﾝNO
           02  J-SHN              PIC  X(011).                          ｺﾖｳNO
      *
           02  J-SB               PIC  9(001).                          ｾｲﾍﾞﾂC
           02  J-HSC              PIC  9(001).                          ﾊｹﾝｶﾞｲｼｬ
           02  J-PNO              PIC  9(002).                          ｻｸﾋｮｳNO
           02  J-GRC              PIC  9(001).                          ｶﾞｸﾚｷC
           02  J-SSC              PIC  9(001).                          ｼｮｸｼｭC
           02  J-TJC              PIC  9(001).                          ﾀﾝｷｭｳC
           02  J-KJC              PIC  9(001).                          ｷｭｳｼﾞﾂC
           02  J-KBC              PIC  9(002).                          ﾌﾞﾓﾝｶﾝﾘC
           02  F                  PIC  9(003).
           02  J-HNC              PIC  9(002).                          ﾎﾝﾆﾝC
           02  J-HNCD  REDEFINES J-HNC.
             03  J-HNC1           PIC  9(001).
             03  J-HNC2           PIC  9(001).
           02  J-HGC              PIC  9(002).                          ﾊｲｸﾞｳｼｬC
           02  J-HGCD  REDEFINES J-HGC.
             03  J-HGC1           PIC  9(001).
             03  J-HGC2           PIC  9(001).
           02  J-FYS              PIC  9(001).                          ﾌﾖｳﾆﾝｽﾞｳ
           02  J-DTS              PIC  9(001).                          ﾄﾞｳﾄｸｼｮｳ
           02  J-SGS              PIC  9(001).                          ｼｮｳｶﾞｲｼｬ
           02  J-TSS              PIC  9(001).                          ﾄｸｼｮｳｶﾞｲ
           02  J-RGS              PIC  9(001).                          ﾛｳｼﾞﾝ
           02  J-DRS              PIC  9(001).                          ﾄﾞｳﾛｳｼﾞﾝ
           02  J-TFS              PIC  9(001).
           02  J-MNC              PIC  9(001).                          ﾐｾｲﾈﾝ
           02  J-NCR              PIC  9(001).                          ﾈﾝﾁｮｳﾛｯｸ
           02  F                  PIC  9(001).
           02  J-KZC              PIC  9(001).                          ｶｿﾞｸｸﾌﾞﾝ
           02  J-YSC              PIC  9(002).                          ﾔｸｼｮｸC
           02  J-YSCD  REDEFINES J-YSC.
             03  J-YSC1           PIC  9(001).
             03  J-YSC2           PIC  9(001).
           02  J-SMC1             PIC  9(001).
           02  J-SMC2             PIC  9(001).
           02  J-SHC              PIC  9(002).                          ｼﾊﾗｲC
           02  J-SHCD  REDEFINES J-SHC.
             03  J-SHC1           PIC  9(001).
             03  J-SHC2           PIC  9(001).
           02  J-KMC              PIC  9(001).                          ｸﾐｱｲC
           02  J-SGC              PIC  9(001).                          ｼﾂｷﾞｮｳC
           02  J-KKC              PIC  9(001).                          ｶｲｷﾝC
           02  J-TKC              PIC  9(001).                          ﾂｳｷﾝC
           02  F                  PIC  9(001).
      *    [  ｷ ｿ ｷ ｭ ｳ ﾖ  ]
           02  J-KHK              PIC  9(006).                          ｷﾎﾝｷｭｳ
           02  J-KMK              PIC  9(006).                          ｷﾝﾑｷｭｳ
           02  J-JTT              PIC  9(005).                          ｼﾞｭｳﾀｸ
      *    [  ｼｭｯｺｳ･ﾁｲｷ･ﾔﾁﾝﾎｼﾞｮﾎｶ  ]
           02  J-STT              PIC S9(006).
           02  J-SCT              PIC S9(006).
      *
           02  J-YHS              PIC  9(007).
      *    [  ｺｳｼﾞｮ ｺｳﾓｸ  ]
      *        (  ｼｬｶｲﾎｹﾝ  )
           02  J-HSG              PIC  9(003).                          ﾂｷｶﾞｸ
      *        (  ｼﾞｭｳﾐﾝｾﾞｲ  )
           02  J-JSC              PIC  9(002).                          ｼﾞｭｳｼｮC
           02  J-JZ1              PIC  9(006).
           02  J-JZ2              PIC  9(006).
      *        (  ｻﾞｲｹｲﾖｷﾝ  )
           02  J-ZK1              PIC  9(006).
           02  J-YB1              PIC  9(001).
           02  J-ZK2              PIC  9(006).
           02  J-YB2              PIC  9(001).
      *        (  ｻﾞｲｹｲﾈﾝｷﾝ  )
           02  J-ZKN              PIC  9(005).
           02  J-NBC              PIC  9(001).                          BCｸﾌﾞﾝ
      *        (  ﾔﾁﾝ･ｺｳｻﾞﾎｶ  )
      *    [  ﾛｳｷﾝｼﾊﾗｲ  ]
           02  J-ZRS              PIC  9(005).                          ｾﾞﾝﾛｳｻｲ
           02  J-RKK              PIC  9(006).                          ｶﾘｲﾚﾍﾝｻｲ
           02  J-RMP              PIC  9(006).                          ﾏｲﾌﾟﾗﾝ
      *        (  ﾀﾞﾝﾀｲﾎｹﾝ  )
           02  J-DHR1             PIC  9(005).
           02  J-DHR2             PIC  9(005).
           02  J-DHR3             PIC  9(005).
           02  J-DHR4             PIC  9(005).
           02  J-DHR5             PIC  9(005).
           02  J-DHR6             PIC  9(005).
      *    [  ｷ ｭ ｳ ﾌ ﾘ  ]
      *        (  NO1ｺｳｻﾞ  )
           02  J-BKD1.
             03  J-FBK1.
               04  J-BKC1         PIC  9(004).                          ｷﾞﾝｺｳCD
               04  J-HSC1         PIC  9(003).                          ﾎﾝｼﾃﾝCD
             03  J-FKN1           PIC  9(007).                          ｺｳｻﾞNO
             03  J-FNA1           PIC  X(016).                          ｼﾒｲ ANK
             03  J-FKG1           PIC  9(007).                          ﾌﾘｺﾐｶﾞｸ
      *        (  NO2ｺｳｻﾞ  )
           02  J-BKD2.
             03  J-FBK2.
               04  J-BKC2         PIC  9(004).                          ｷﾞﾝｺｳCD
               04  J-HSC2         PIC  9(003).                          ﾎﾝｼﾃﾝCD
             03  J-FKN2           PIC  9(007).                          ｺｳｻﾞNO
             03  J-FNA2           PIC  X(016).                          ｼﾒｲ ANK
             03  J-FKG2           PIC  9(007).                          ﾌﾘｺﾐｶﾞｸ
      *    * * *   ﾄ ｳ ｹ ｲ  ｺ ｳ ﾓ ｸ   * * *
           02  J-ZKT              PIC S9(007).                          ｶｾﾞｲｶﾞｸ
      *    [  ﾕｳｷｭｳ ﾆｯｽｳ  ]
           02  J-YKZ              PIC S9(002).                          ﾕｳｷｭｳｻﾞﾝ
           02  J-YK1              PIC S9(002).                          ｾﾞﾝﾈﾝ
           02  J-YK2              PIC S9(002).                          ﾄｳﾈﾝ
      *
      *    [  ﾀｲｼｮｸｷｭｳﾖ ﾋｷｱﾃ ｶﾝｹｲ  ]
           02  JIN-YKYMD          PIC  9(008).
           02  JIN-YKYMDR REDEFINES JIN-YKYMD.                          役員就任
             03  JIN-YKY          PIC  9(004).
             03  JIN-YKM          PIC  9(002).                          月
             03  JIN-YKD          PIC  9(002).                          日
           02  JIN-YKYMDRS REDEFINES  JIN-YKYMD.
             03  F                PIC  9(002).
             03  JIN-YKYMDS       PIC  9(006).
           02  JIN-TAK.                                                 退職給与
             03  JIN-TAKZ         PIC  9(010)  COMP-3.                  前期末
             03  JIN-TAKT         PIC  9(010)  COMP-3.                  当期末
           02  JIN-KYK.                                                 共済掛金
             03  JIN-KYKZ         PIC  9(010)  COMP-3.                  前期末
             03  JIN-KYKM         PIC  9(010)  COMP-3.                  当期末
           02  JIN-SSYMD          PIC  9(008).
           02  JIN-SSYMDR REDEFINES  JIN-SSYMD.                         正社員
             03  JIN-SSYM.
               04  JIN-SSY        PIC  9(004).
               04  JIN-SSM        PIC  9(002).                          月
             03  JIN-SSD          PIC  9(002).                          日
           02  JIN-SSYMDRS REDEFINES  JIN-SSYMD.
             03  F                PIC  9(002).
             03  JIN-SSYMDS       PIC  9(006).
      *    [  ｼｮｳｷｭｳ･ｼｮｳﾖ ｹｲｻﾝﾖｳ  ]
           02  J-LNK              PIC  9(001).                          ｻﾃｲﾗﾝｸC
           02  J-PCOK             PIC  9(001).
           02  J-KCOK             PIC  9(001).
           02  J-YSSK             PIC  9(002).
           02  J-PCOS             PIC  9(001).
           02  J-KCOS             PIC  9(001).
           02  J-YSSS             PIC  9(002).
           02  J-KKS              PIC  9(006).                          ｷｿｷｭｳﾖ
           02  J-SNS              PIC  9(003).                          ｼｮｳﾖﾆｯｽｳ
           02  J-KNS              PIC  9(003).                          ｹｯｷﾝｹｲ
           02  J-CNS              PIC  9(003).                          ﾁｿｶﾞｹｲ
           02  J-YKS              PIC  9(003).
           02  J-NENS             PIC  9(002).
           02  J-KSS              PIC  9(001).
      *
           02  F                  PIC  X(005).
      *    [  ｷｭｳｷﾞｮｳ  ]
           02  J-KGK.
             03  J-KGC            PIC  9(001).
             03  J-KGT            PIC  9(002).
      *    [  ｿｳﾀﾝ ﾆｯｽｳ  ]
           02  J-STN.
             03  J-STN1           PIC  9(001).                          ﾄｳｹﾞﾂ
             03  J-STN2           PIC  9(001).                          ﾖｸｹﾞﾂ
             03  J-STN3           PIC  9(001).                          ﾖｸﾖｸｹﾞﾂ
      *
           02  J-OK               PIC  9(004).                          ｷｭｳKEY
      *
      *    [  ｺｳｼﾝ ﾈﾝｹﾞﾂ  ]
           02  J-NG.
             03  J-NEN            PIC  9(002).                          ﾈﾝ
             03  J-GET            PIC  9(002).                          ﾂｷ
       77  F                      PIC  X(001).
      *

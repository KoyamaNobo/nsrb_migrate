000010******************************************
000020*****     人　事　マ　ス　タ　ー     *****
000030*****      (  JINJIM  512/1  )       *****
000040******************************************
000050 FD  JINJIM
000060     BLOCK  1 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "JINJIM".
000090 01  JINJI-R.
000100*    * * *   ｺ ﾃ ｲ  ｺ ｳ ﾓ ｸ   * * *
000110     02  J-CODE.                                                  ｺｰﾄﾞ
000120       03  J-SCD            PIC  9(004).                          ｼｮｿﾞｸC
000130       03  J-SCDD  REDEFINES J-SCD.                               ｼｮｿﾞｸC
000140         04  J-SCD1         PIC  9(002).
000150         04  J-SCD2         PIC  9(002).
000160       03  J-SZC   REDEFINES J-SCD.                               ｼｮｿﾞｸC
000170         04  J-SZC1         PIC  9(003).
000180         04  J-SZC2         PIC  9(001).
000190       03  J-KEY.                                                 ｼｬｲﾝC
000200         04  J-SIC          PIC  9(004).
000210     02  J-NAME             PIC  N(008).                          ｼﾒｲ
000220     02  J-ANA              PIC  X(016).                          ｼﾒｲ ANK
000230     02  J-ADR              PIC  N(030).                          ｼﾞｭｳｼｮ
000240*****02  J-UNO              PIC  X(006).                          D.970618
000250*****02  J-TEL              PIC  X(013).                          D.970618
000260     02  J-UNO              PIC  X(008).                          I.970618
000270     02  J-TEL              PIC  X(014).                          I.970618
000280*
000290*****02  J-SNGP             PIC  9(006).                          D.970618
000300     02  J-SNGP             PIC  9(008).                          I.970618
000310     02  J-SNGPD REDEFINES J-SNGP.
000320*****  03  J-SNEN           PIC  9(002).                          D.970618
000330       03  J-SNEN           PIC  9(004).                          I.970618
000340       03  J-SGP.
000350         04  J-SGET         PIC  9(002).
000360         04  J-SPEY         PIC  9(002).
000370     02  J-SNGPDS REDEFINES J-SNGP.                               I.970704
000380       03  F                PIC  9(002).                          I.970704
000390       03  J-SNGPS          PIC  9(006).                          I.970704
000400*****02  J-NNGP             PIC  9(006).                          D.970618
000410     02  J-NNGP             PIC  9(008).                          I.970618
000420     02  J-NNGPD REDEFINES J-NNGP.
000430       03  J-NNG.
000440*****    04  J-NNEN         PIC  9(002).                          D.970618
000450         04  J-NNEN         PIC  9(004).                          I.970618
000460         04  J-NGET         PIC  9(002).
000470       03  J-NPEY           PIC  9(002).
000480     02  J-NNGPDS REDEFINES J-NNGP.                               I.970704
000490       03  F                PIC  9(002).                          I.970704
000500       03  J-NNGPS          PIC  9(006).                          I.970704
000510*****02  J-TNGP             PIC  9(006).                          D.970618
000520     02  J-TNGP             PIC  9(008).                          I.970618
000530     02  J-TNGPD REDEFINES J-TNGP.
000540       03  J-TNG.
000550*****    04  J-TNEN         PIC  9(002).                          D.970618
000560         04  J-TNEN         PIC  9(004).                          I.970618
000570         04  J-TGET         PIC  9(002).
000580       03  J-TPEY           PIC  9(002).
000590     02  J-TNGPDS REDEFINES J-TNGP.                               I.970704
000600       03  F                PIC  9(002).                          I.970704
000610       03  J-TNGPS          PIC  9(006).                          I.970704
000620*
000630     02  J-KHN              PIC  X(010).                          ﾎｹﾝNO
000640     02  J-KNN              PIC  X(010).                          ﾈﾝｷﾝNO
000650     02  J-SHN              PIC  X(011).                          ｺﾖｳNO
000660*
000670     02  J-SB               PIC  9(001).                          ｾｲﾍﾞﾂC
000680     02  J-HSC              PIC  9(001).                          ﾊｹﾝｶﾞｲｼｬ
000690     02  J-PNO              PIC  9(002).                          ｻｸﾋｮｳNO
000700*****02  J-BMC              PIC  9(001).                          D.021021
000710     02  J-GRC              PIC  9(001).                          ｶﾞｸﾚｷC
000720     02  J-SSC              PIC  9(001).                          ｼｮｸｼｭC
000730     02  J-TJC              PIC  9(001).                          ﾀﾝｷｭｳC
000740     02  J-KJC              PIC  9(001).                          ｷｭｳｼﾞﾂC
000750     02  J-KBC              PIC  9(002).                          ﾌﾞﾓﾝｶﾝﾘC
000760     02  F                  PIC  9(003).                          I.990414
000770*****02  F                  PIC  9(002).                          D.990414
000780*****02  J-ZIC              PIC  9(001).                          D.990414
000790     02  J-HNC              PIC  9(002).                          ﾎﾝﾆﾝC
000800     02  J-HNCD  REDEFINES J-HNC.
000810       03  J-HNC1           PIC  9(001).
000820       03  J-HNC2           PIC  9(001).
000830     02  J-HGC              PIC  9(002).                          ﾊｲｸﾞｳｼｬC
000840     02  J-HGCD  REDEFINES J-HGC.
000850       03  J-HGC1           PIC  9(001).
000860       03  J-HGC2           PIC  9(001).
000870     02  J-FYS              PIC  9(001).                          ﾌﾖｳﾆﾝｽﾞｳ
000880     02  J-DTS              PIC  9(001).                          ﾄﾞｳﾄｸｼｮｳ
000890     02  J-SGS              PIC  9(001).                          ｼｮｳｶﾞｲｼｬ
000900     02  J-TSS              PIC  9(001).                          ﾄｸｼｮｳｶﾞｲ
000910     02  J-RGS              PIC  9(001).                          ﾛｳｼﾞﾝ
000920     02  J-DRS              PIC  9(001).                          ﾄﾞｳﾛｳｼﾞﾝ
000930     02  J-TFS              PIC  9(001).                          I.971125
000940     02  J-MNC              PIC  9(001).                          ﾐｾｲﾈﾝ
000950     02  J-NCR              PIC  9(001).                          ﾈﾝﾁｮｳﾛｯｸ
000960     02  F                  PIC  9(001).                          I.971125
000970*****02  F                  PIC  9(002).                          D.971125
000980     02  J-KZC              PIC  9(001).                          ｶｿﾞｸｸﾌﾞﾝ
000990     02  J-YSC              PIC  9(002).                          ﾔｸｼｮｸC
001000     02  J-YSCD  REDEFINES J-YSC.
001010       03  J-YSC1           PIC  9(001).
001020       03  J-YSC2           PIC  9(001).
001030*****02  J-SMC              PIC  9(001).                          D.021021
001040     02  J-SMC1             PIC  9(001).                          I.021021
001050     02  J-SMC2             PIC  9(001).                          I.021021
001060     02  J-SHC              PIC  9(002).                          ｼﾊﾗｲC
001070     02  J-SHCD  REDEFINES J-SHC.
001080       03  J-SHC1           PIC  9(001).
001090       03  J-SHC2           PIC  9(001).
001100     02  J-KMC              PIC  9(001).                          ｸﾐｱｲC
001110     02  J-SGC              PIC  9(001).                          ｼﾂｷﾞｮｳC
001120     02  J-KKC              PIC  9(001).                          ｶｲｷﾝC
001130     02  J-TKC              PIC  9(001).                          ﾂｳｷﾝC
001140     02  F                  PIC  9(001).                          I.021021
001150*****02  F                  PIC  9(002).                          D.021021
001160*    [  ｷ ｿ ｷ ｭ ｳ ﾖ  ]
001170     02  J-KHK              PIC  9(006).                          ｷﾎﾝｷｭｳ
001180     02  J-KMK              PIC  9(006).                          ｷﾝﾑｷｭｳ
001190     02  J-JTT              PIC  9(005).                          ｼﾞｭｳﾀｸ
001200*    [  ｼｭｯｺｳ･ﾁｲｷ･ﾔﾁﾝﾎｼﾞｮﾎｶ  ]
001210*****02  J-STT              PIC S9(005).                          D.951206
001220     02  J-STT              PIC S9(006).                          I.951206
001230     02  J-SCT              PIC S9(006).                          I.990413
001240*
001250     02  J-YHS              PIC  9(007).                          I.021021
001260*****02  J-YHS              PIC  9(006).                          D.021021
001270*    [  ｺｳｼﾞｮ ｺｳﾓｸ  ]
001280*        (  ｼｬｶｲﾎｹﾝ  )
001290     02  J-HSG              PIC  9(003).                          ﾂｷｶﾞｸ
001300*        (  ｼﾞｭｳﾐﾝｾﾞｲ  )
001310     02  J-JSC              PIC  9(002).                          ｼﾞｭｳｼｮC
001320     02  J-JZ1              PIC  9(006).                          I.000609
001330     02  J-JZ2              PIC  9(006).                          I.000609
001340*****02  J-JZ1              PIC  9(005).                          D.000609
001350*****02  J-JZ2              PIC  9(005).                          D.000609
001360*        (  ｻﾞｲｹｲﾖｷﾝ  )
001370*****02  J-ZKY              PIC  9(006).                          D.951006
001380*****02  J-YBC              PIC  9(001).                          D.951006
001390     02  J-ZK1              PIC  9(006).                          I.951006
001400     02  J-YB1              PIC  9(001).                          I.951006
001410     02  J-ZK2              PIC  9(006).                          I.951006
001420     02  J-YB2              PIC  9(001).                          I.951006
001430*        (  ｻﾞｲｹｲﾈﾝｷﾝ  )
001440*****02  J-ZKN              PIC  9(006).                          D.951006
001450     02  J-ZKN              PIC  9(005).                          I.951006
001460     02  J-NBC              PIC  9(001).                          BCｸﾌﾞﾝ
001470*        (  ﾔﾁﾝ･ｺｳｻﾞﾎｶ  )
001480*****02  J-YKD              PIC S9(006).                          D.951006
001490*    [  ﾛｳｷﾝｼﾊﾗｲ  ]
001500     02  J-ZRS              PIC  9(005).                          ｾﾞﾝﾛｳｻｲ
001510     02  J-RKK              PIC  9(006).                          ｶﾘｲﾚﾍﾝｻｲ
001520     02  J-RMP              PIC  9(006).                          ﾏｲﾌﾟﾗﾝ
001530*        (  ﾀﾞﾝﾀｲﾎｹﾝ  )
001540     02  J-DHR1             PIC  9(005).
001550     02  J-DHR2             PIC  9(005).
001560     02  J-DHR3             PIC  9(005).
001570     02  J-DHR4             PIC  9(005).
001580     02  J-DHR5             PIC  9(005).
001590     02  J-DHR6             PIC  9(005).
001600*****02  J-DHR7             PIC  9(005).                          D.990413
001610*    [  ｷ ｭ ｳ ﾌ ﾘ  ]
001620*        (  NO1ｺｳｻﾞ  )
001630     02  J-BKD1.
001640       03  J-FBK1.
001650         04  J-BKC1         PIC  9(004).                          ｷﾞﾝｺｳCD
001660         04  J-HSC1         PIC  9(003).                          ﾎﾝｼﾃﾝCD
001670       03  J-FKN1           PIC  9(007).                          ｺｳｻﾞNO
001680       03  J-FNA1           PIC  X(016).                          ｼﾒｲ ANK
001690       03  J-FKG1           PIC  9(007).                          ﾌﾘｺﾐｶﾞｸ
001700*        (  NO2ｺｳｻﾞ  )
001710     02  J-BKD2.
001720       03  J-FBK2.
001730         04  J-BKC2         PIC  9(004).                          ｷﾞﾝｺｳCD
001740         04  J-HSC2         PIC  9(003).                          ﾎﾝｼﾃﾝCD
001750       03  J-FKN2           PIC  9(007).                          ｺｳｻﾞNO
001760       03  J-FNA2           PIC  X(016).                          ｼﾒｲ ANK
001770       03  J-FKG2           PIC  9(007).                          ﾌﾘｺﾐｶﾞｸ
001780*    * * *   ﾄ ｳ ｹ ｲ  ｺ ｳ ﾓ ｸ   * * *
001790     02  J-ZKT              PIC S9(007).                          ｶｾﾞｲｶﾞｸ
001800*    [  ﾕｳｷｭｳ ﾆｯｽｳ  ]
001810     02  J-YKZ              PIC S9(002).                          ﾕｳｷｭｳｻﾞﾝ
001820     02  J-YK1              PIC S9(002).                          ｾﾞﾝﾈﾝ
001830     02  J-YK2              PIC S9(002).                          ﾄｳﾈﾝ
001840*
001850*    [  ﾀｲｼｮｸｷｭｳﾖ ﾋｷｱﾃ ｶﾝｹｲ  ]
001860     02  JIN-YKYMD          PIC  9(008).                          I.970618
001870     02  JIN-YKYMDR REDEFINES JIN-YKYMD.                          役員就任
001880*****  03  JIN-YKY          PIC  9(002).                          D.970618
001890       03  JIN-YKY          PIC  9(004).                          I.970618
001900       03  JIN-YKM          PIC  9(002).                          月
001910       03  JIN-YKD          PIC  9(002).                          日
001920     02  JIN-YKYMDRS REDEFINES  JIN-YKYMD.                        I.970704
001930       03  F                PIC  9(002).                          I.970704
001940       03  JIN-YKYMDS       PIC  9(006).                          I.970704
001950*****02  JIN-YKYMD  REDEFINES JIN-YKYMDR  PIC  9(006).            D.970618
001960     02  JIN-TAK.                                                 退職給与
001970       03  JIN-TAKZ         PIC  9(010)  COMP-3.                  前期末
001980       03  JIN-TAKT         PIC  9(010)  COMP-3.                  当期末
001990     02  JIN-KYK.                                                 共済掛金
002000       03  JIN-KYKZ         PIC  9(010)  COMP-3.                  前期末
002010       03  JIN-KYKM         PIC  9(010)  COMP-3.                  当期末
002020     02  JIN-SSYMD          PIC  9(008).                          I.970618
002030     02  JIN-SSYMDR REDEFINES  JIN-SSYMD.                         正社員
002040       03  JIN-SSYM.
002050*****    04  JIN-SSY        PIC  9(002).                          D.970618
002060         04  JIN-SSY        PIC  9(004).                          I.970618
002070         04  JIN-SSM        PIC  9(002).                          月
002080       03  JIN-SSD          PIC  9(002).                          日
002090     02  JIN-SSYMDRS REDEFINES  JIN-SSYMD.                        I.970704
002100       03  F                PIC  9(002).                          I.970704
002110       03  JIN-SSYMDS       PIC  9(006).                          I.970704
002120*****02  JIN-SSYMD  REDEFINES  JIN-SSYMDR  PIC  9(006).           D.970618
002130*****
002140*    [  ｼｮｳｷｭｳ･ｼｮｳﾖ ｹｲｻﾝﾖｳ  ]
002150     02  J-LNK              PIC  9(001).                          ｻﾃｲﾗﾝｸC
002160*****02  J-PCO              PIC  9(001).                          D.950530
002170*****02  J-KCO              PIC  9(001).                          D.950530
002180*****02  J-YSS              PIC  9(002).                          D.950530
002190     02  J-PCOK             PIC  9(001).                          I.950530
002200     02  J-KCOK             PIC  9(001).                          I.950530
002210     02  J-YSSK             PIC  9(002).                          I.950530
002220     02  J-PCOS             PIC  9(001).                          I.950530
002230     02  J-KCOS             PIC  9(001).                          I.950530
002240     02  J-YSSS             PIC  9(002).                          I.950530
002250     02  J-KKS              PIC  9(006).                          ｷｿｷｭｳﾖ
002260     02  J-SNS              PIC  9(003).                          ｼｮｳﾖﾆｯｽｳ
002270     02  J-KNS              PIC  9(003).                          ｹｯｷﾝｹｲ
002280     02  J-CNS              PIC  9(003).                          ﾁｿｶﾞｹｲ
002290     02  J-YKS              PIC  9(003).                          I.020312
002300     02  J-NENS             PIC  9(002).                          I.010122
002310     02  J-KSS              PIC  9(001).                          I.010122
002320*
002330     02  F                  PIC  X(005).                          I.020312
002340*****02  F                  PIC  X(008).                          D.020312
002350*****02  F                  PIC  X(031).                          D.950530
002360*****02  F                  PIC  X(027).                          D.951206
002370*****02  F                  PIC  X(026).                          D.951216
002380*****02  F                  PIC  X(023).                          D.970618
002390*****02  F                  PIC  X(010).                          D.990413
002400*****02  F                  PIC  X(013).                          D.000609
002410*****02  F                  PIC  X(011).                          D.010122
002420*    [  ｷｭｳｷﾞｮｳ  ]
002430     02  J-KGK.
002440       03  J-KGC            PIC  9(001).                          I.951216
002450       03  J-KGT            PIC  9(002).                          I.951216
002460*    [  ｿｳﾀﾝ ﾆｯｽｳ  ]
002470     02  J-STN.
002480       03  J-STN1           PIC  9(001).                          ﾄｳｹﾞﾂ
002490       03  J-STN2           PIC  9(001).                          ﾖｸｹﾞﾂ
002500       03  J-STN3           PIC  9(001).                          ﾖｸﾖｸｹﾞﾂ
002510*
002520     02  J-OK               PIC  9(004).                          ｷｭｳKEY
002530*
002540*****02  F                  PIC  X(004).                          D.990413
002550*****02  J-QC               PIC  9(004).                          D.970619
002560*    [  ｺｳｼﾝ ﾈﾝｹﾞﾂ  ]
002570     02  J-NG.
002580       03  J-NEN            PIC  9(002).                          ﾈﾝ
002590       03  J-GET            PIC  9(002).                          ﾂｷ
002600*

      ******************************************
      *****     �l�@���@�}�@�X�@�^�@�[     *****
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
      *    * * *   � � �  � � � �   * * *
           02  J-CODE.                                                  ����
             03  J-SCD            PIC  9(004).                          ���޸C
             03  J-SCDD  REDEFINES J-SCD.                               ���޸C
               04  J-SCD1         PIC  9(002).
               04  J-SCD2         PIC  9(002).
             03  J-SZC   REDEFINES J-SCD.                               ���޸C
               04  J-SZC1         PIC  9(003).
               04  J-SZC2         PIC  9(001).
             03  J-KEY.                                                 ����C
               04  J-SIC          PIC  9(004).
           02  J-NAME             PIC  N(008).                          �Ҳ
           02  J-ANA              PIC  X(016).                          �Ҳ ANK
           02  J-ADR              PIC  N(030).                          �ޭ���
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
           02  J-KHN              PIC  X(010).                          ι�NO
           02  J-KNN              PIC  X(010).                          �ݷ�NO
           02  J-SHN              PIC  X(011).                          �ֳNO
      *
           02  J-SB               PIC  9(001).                          �����C
           02  J-HSC              PIC  9(001).                          ʹݶ޲��
           02  J-PNO              PIC  9(002).                          ��ˮ�NO
           02  J-GRC              PIC  9(001).                          �޸ڷC
           02  J-SSC              PIC  9(001).                          �����C
           02  J-TJC              PIC  9(001).                          �ݷ��C
           02  J-KJC              PIC  9(001).                          ������C
           02  J-KBC              PIC  9(002).                          ���ݶ��C
           02  F                  PIC  9(003).
           02  J-HNC              PIC  9(002).                          ����C
           02  J-HNCD  REDEFINES J-HNC.
             03  J-HNC1           PIC  9(001).
             03  J-HNC2           PIC  9(001).
           02  J-HGC              PIC  9(002).                          ʲ�޳��C
           02  J-HGCD  REDEFINES J-HGC.
             03  J-HGC1           PIC  9(001).
             03  J-HGC2           PIC  9(001).
           02  J-FYS              PIC  9(001).                          �ֳ�ݽ޳
           02  J-DTS              PIC  9(001).                          �޳ĸ���
           02  J-SGS              PIC  9(001).                          ����޲��
           02  J-TSS              PIC  9(001).                          ĸ����޲
           02  J-RGS              PIC  9(001).                          ۳���
           02  J-DRS              PIC  9(001).                          �޳۳���
           02  J-TFS              PIC  9(001).
           02  J-MNC              PIC  9(001).                          о���
           02  J-NCR              PIC  9(001).                          �����ۯ�
           02  F                  PIC  9(001).
           02  J-KZC              PIC  9(001).                          ��޸����
           02  J-YSC              PIC  9(002).                          Ը���C
           02  J-YSCD  REDEFINES J-YSC.
             03  J-YSC1           PIC  9(001).
             03  J-YSC2           PIC  9(001).
           02  J-SMC1             PIC  9(001).
           02  J-SMC2             PIC  9(001).
           02  J-SHC              PIC  9(002).                          ��ײC
           02  J-SHCD  REDEFINES J-SHC.
             03  J-SHC1           PIC  9(001).
             03  J-SHC2           PIC  9(001).
           02  J-KMC              PIC  9(001).                          �б�C
           02  J-SGC              PIC  9(001).                          �·ޮ�C
           02  J-KKC              PIC  9(001).                          ����C
           02  J-TKC              PIC  9(001).                          ³��C
           02  F                  PIC  9(001).
      *    [  � � � � � �  ]
           02  J-KHK              PIC  9(006).                          ��ݷ��
           02  J-KMK              PIC  9(006).                          ��ѷ��
           02  J-JTT              PIC  9(005).                          �ޭ���
      *    [  �������������μޮζ  ]
           02  J-STT              PIC S9(006).
           02  J-SCT              PIC S9(006).
      *
           02  J-YHS              PIC  9(007).
      *    [  ���ޮ ��Ӹ  ]
      *        (  ����ι�  )
           02  J-HSG              PIC  9(003).                          ·�޸
      *        (  �ޭ��ݾ޲  )
           02  J-JSC              PIC  9(002).                          �ޭ���C
           02  J-JZ1              PIC  9(006).
           02  J-JZ2              PIC  9(006).
      *        (  �޲��ַ�  )
           02  J-ZK1              PIC  9(006).
           02  J-YB1              PIC  9(001).
           02  J-ZK2              PIC  9(006).
           02  J-YB2              PIC  9(001).
      *        (  �޲���ݷ�  )
           02  J-ZKN              PIC  9(005).
           02  J-NBC              PIC  9(001).                          BC����
      *        (  ��ݥ����ζ  )
      *    [  ۳�ݼ�ײ  ]
           02  J-ZRS              PIC  9(005).                          ���۳��
           02  J-RKK              PIC  9(006).                          �ز��ݻ�
           02  J-RMP              PIC  9(006).                          ϲ����
      *        (  �����ι�  )
           02  J-DHR1             PIC  9(005).
           02  J-DHR2             PIC  9(005).
           02  J-DHR3             PIC  9(005).
           02  J-DHR4             PIC  9(005).
           02  J-DHR5             PIC  9(005).
           02  J-DHR6             PIC  9(005).
      *    [  � � � � �  ]
      *        (  NO1����  )
           02  J-BKD1.
             03  J-FBK1.
               04  J-BKC1         PIC  9(004).                          ��ݺ�CD
               04  J-HSC1         PIC  9(003).                          �ݼ��CD
             03  J-FKN1           PIC  9(007).                          ����NO
             03  J-FNA1           PIC  X(016).                          �Ҳ ANK
             03  J-FKG1           PIC  9(007).                          �غж޸
      *        (  NO2����  )
           02  J-BKD2.
             03  J-FBK2.
               04  J-BKC2         PIC  9(004).                          ��ݺ�CD
               04  J-HSC2         PIC  9(003).                          �ݼ��CD
             03  J-FKN2           PIC  9(007).                          ����NO
             03  J-FNA2           PIC  X(016).                          �Ҳ ANK
             03  J-FKG2           PIC  9(007).                          �غж޸
      *    * * *   � � � �  � � � �   * * *
           02  J-ZKT              PIC S9(007).                          ��޲�޸
      *    [  ճ��� Ư��  ]
           02  J-YKZ              PIC S9(002).                          ճ������
           02  J-YK1              PIC S9(002).                          �����
           02  J-YK2              PIC S9(002).                          ĳ��
      *
      *    [  ��������� ˷�� �ݹ�  ]
           02  JIN-YKYMD          PIC  9(008).
           02  JIN-YKYMDR REDEFINES JIN-YKYMD.                          �����A�C
             03  JIN-YKY          PIC  9(004).
             03  JIN-YKM          PIC  9(002).                          ��
             03  JIN-YKD          PIC  9(002).                          ��
           02  JIN-YKYMDRS REDEFINES  JIN-YKYMD.
             03  F                PIC  9(002).
             03  JIN-YKYMDS       PIC  9(006).
           02  JIN-TAK.                                                 �ސE���^
             03  JIN-TAKZ         PIC  9(010)  COMP-3.                  �O����
             03  JIN-TAKT         PIC  9(010)  COMP-3.                  ������
           02  JIN-KYK.                                                 ���ϊ|��
             03  JIN-KYKZ         PIC  9(010)  COMP-3.                  �O����
             03  JIN-KYKM         PIC  9(010)  COMP-3.                  ������
           02  JIN-SSYMD          PIC  9(008).
           02  JIN-SSYMDR REDEFINES  JIN-SSYMD.                         ���Ј�
             03  JIN-SSYM.
               04  JIN-SSY        PIC  9(004).
               04  JIN-SSM        PIC  9(002).                          ��
             03  JIN-SSD          PIC  9(002).                          ��
           02  JIN-SSYMDRS REDEFINES  JIN-SSYMD.
             03  F                PIC  9(002).
             03  JIN-SSYMDS       PIC  9(006).
      *    [  ����������� ����ֳ  ]
           02  J-LNK              PIC  9(001).                          �ò�ݸC
           02  J-PCOK             PIC  9(001).
           02  J-KCOK             PIC  9(001).
           02  J-YSSK             PIC  9(002).
           02  J-PCOS             PIC  9(001).
           02  J-KCOS             PIC  9(001).
           02  J-YSSS             PIC  9(002).
           02  J-KKS              PIC  9(006).                          ������
           02  J-SNS              PIC  9(003).                          ����Ư��
           02  J-KNS              PIC  9(003).                          ���ݹ�
           02  J-CNS              PIC  9(003).                          ���޹�
           02  J-YKS              PIC  9(003).
           02  J-NENS             PIC  9(002).
           02  J-KSS              PIC  9(001).
      *
           02  F                  PIC  X(005).
      *    [  ����ޮ�  ]
           02  J-KGK.
             03  J-KGC            PIC  9(001).
             03  J-KGT            PIC  9(002).
      *    [  ���� Ư��  ]
           02  J-STN.
             03  J-STN1           PIC  9(001).                          ĳ���
             03  J-STN2           PIC  9(001).                          ָ���
             03  J-STN3           PIC  9(001).                          ָָ���
      *
           02  J-OK               PIC  9(004).                          ���KEY
      *
      *    [  ���� �ݹ��  ]
           02  J-NG.
             03  J-NEN            PIC  9(002).                          ��
             03  J-GET            PIC  9(002).                          ·
       77  F                      PIC  X(001).
      *

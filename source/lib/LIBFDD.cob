      ****************************
      **                        **
      **    ���t�@�}�X�^�[      **
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
           02  DATE-02   REDEFINES DATE-02R.                            ����
             03  DATE-021      PIC  9(002).
             03  DATE-022      PIC  9(002).
             03  DATE-023      PIC  9(002).
           02  DATE-03R        PIC  9(006).
           02  DATE-03   REDEFINES DATE-03R.                            �H�i
             03  DATE-031      PIC  9(002).
             03  DATE-032      PIC  9(002).
             03  DATE-033      PIC  9(002).
           02  DATE-04R        PIC  9(006).
           02  DATE-04   REDEFINES DATE-04R.                            ��`
             03  DATE-041      PIC  9(002).
             03  DATE-042      PIC  9(002).
             03  DATE-043      PIC  9(002).
           02  DATE-05R        PIC  9(006).
           02  DATE-05   REDEFINES DATE-05R.                            �w��
             03  DATE-051      PIC  9(002).
             03  DATE-052      PIC  9(002).
             03  DATE-053      PIC  9(002).
           02  DATE-06R        PIC  9(006).
           02  DATE-06   REDEFINES DATE-06R.                            ���̑��P
             03  DATE-061      PIC  9(002).
             03  DATE-062      PIC  9(002).
             03  DATE-063      PIC  9(002).
           02  DATE-07R        PIC  9(006).
           02  DATE-07   REDEFINES DATE-07R.                            ���̑��Q
             03  DATE-071      PIC  9(002).
             03  DATE-072      PIC  9(002).
             03  DATE-073      PIC  9(002).
      *    [    �Ł@�I�@���@�t�@�@]
      *       (   ����   )
           02  D-HSD           PIC  9(006).                             �o��
           02  D-HSDD    REDEFINES D-HSD.
             03  D-HSN         PIC  9(002).
             03  D-HSG         PIC  9(002).
             03  D-HSP         PIC  9(002).
           02  D-HND           PIC  9(006).                             ����
           02  D-HNDD    REDEFINES D-HND.
             03  D-HNN         PIC  9(002).
             03  D-HNG         PIC  9(002).
             03  D-HNP         PIC  9(002).
      *       (   �H�i   )
           02  D-KUD           PIC  9(006).                             ����
           02  D-KUDD    REDEFINES D-KUD.
             03  D-KUN         PIC  9(002).
             03  D-KUG         PIC  9(002).
             03  D-KUP         PIC  9(002).
           02  D-KKD           PIC  9(006).                             ����
           02  D-KKDD    REDEFINES D-KKD.
             03  D-KKN         PIC  9(002).
             03  D-KKG         PIC  9(002).
             03  D-KKP         PIC  9(002).
           02  D-KSD           PIC  9(006).                             �d���x��
           02  D-KSDD    REDEFINES D-KSD.
             03  D-KSN         PIC  9(002).
             03  D-KSG         PIC  9(002).
             03  D-KSP         PIC  9(002).
           02  D-KRD           PIC  9(006).                             �J������
           02  D-KRDD    REDEFINES D-KRD.
             03  D-KRN         PIC  9(002).
             03  D-KRG         PIC  9(002).
             03  D-KRP         PIC  9(002).
      *
           02  D-TGD           PIC  9(006).                             ��`
           02  D-TGDD    REDEFINES D-TGD.
             03  D-TGN         PIC  9(002).
             03  D-TGG         PIC  9(002).
             03  D-TGP         PIC  9(002).
      *
           02  D-KBD           PIC  9(006).                             �w��
           02  D-KBDD    REDEFINES D-KBD.
             03  D-KBN         PIC  9(002).
             03  D-KBG         PIC  9(002).
             03  D-KBP         PIC  9(002).
      *
           02  F               PIC  X(014).
      *
      *    [    �N�@�ԁ@�N�@���@�@]
      *         (  ��\�@�N��  )
           02  D-NPDATE.
             03  D-SPNG        PIC  9(004).                             �J�n�N��
             03  D-SPDATE REDEFINES D-SPNG.
               04  D-SPNEN     PIC  9(002).
               04  D-SPGET     PIC  9(002).
             03  D-EPNG        PIC  9(004).                             �ŏI�N��
             03  D-EPDATE REDEFINES D-EPNG.
               04  D-EPNEN     PIC  9(002).
               04  D-EPGET     PIC  9(002).
      *
      *         (  ����@�N��  )
           02  D-NKDATE.
             03  D-SKNG        PIC  9(004).                             �J�n�N��
             03  D-SKDATE REDEFINES D-SKNG.
               04  D-SKNEN     PIC  9(002).
               04  D-SKGET     PIC  9(002).
             03  D-EKNG        PIC  9(004).                             �ŏI�N��
             03  D-EKDATE REDEFINES D-EKNG.
               04  D-EKNEN     PIC  9(002).
               04  D-EKGET     PIC  9(002).
      *
      *         (  ���̑��@�N��  )
           02  D-NDATE.
             03  D-SNG         PIC  9(004).                             �J�n�N��
             03  D-SDATE REDEFINES D-SNG.
               04  D-SNEN      PIC  9(002).
               04  D-SGET      PIC  9(002).
             03  D-ENG         PIC  9(004).                             �ŏI�N��
             03  D-EDATE REDEFINES D-ENG.
               04  D-ENEN      PIC  9(002).
               04  D-EGET      PIC  9(002).
      *
      *    [    ���@�s�@�N�@���@�@]
           02  D-NNG.
             03  D-NHNG        PIC  9(004).                             ����
             03  D-NHNGD REDEFINES D-NHNG.
               04  D-NHN       PIC  9(002).
               04  D-NHG       PIC  9(002).
             03  D-NKNG        PIC  9(004).                             �H�i
             03  D-NKNGD REDEFINES D-NKNG.
               04  D-NKN       PIC  9(002).
               04  D-NKG       PIC  9(002).
             03  D-NTNG        PIC  9(004).                             ��`
             03  D-NTNGD REDEFINES D-NTNG.
               04  D-NTN       PIC  9(002).
               04  D-NTG       PIC  9(002).
             03  D-NBNG        PIC  9(004).                             �w��
             03  D-NBNGD REDEFINES D-NBNG.
               04  D-NBN       PIC  9(002).
               04  D-NBG       PIC  9(002).
             03  D-NJNG        PIC  9(004).                             ���^
             03  D-NJNGD REDEFINES D-NJNG.
               04  D-NJN       PIC  9(002).
               04  D-NJG       PIC  9(002).
             03  D-NGNG        PIC  9(004).                             ���E
             03  D-NGNGD REDEFINES D-NGNG.
               04  D-NGN       PIC  9(002).
               04  D-NGG       PIC  9(002).
             03  D-NRNG        PIC  9(004).                             ���Y��
             03  D-NRNGD REDEFINES D-NRNG.
               04  D-NRN       PIC  9(002).
               04  D-NRG       PIC  9(002).
             03  D-NING        PIC  9(004).                             ����
             03  D-NINGD REDEFINES D-NING.
               04  D-NIN       PIC  9(002).
               04  D-NIG       PIC  9(002).
             03  D-NANG        PIC  9(004).                             �S��
             03  D-NANGD REDEFINES D-NANG.
               04  D-NAN       PIC  9(002).
               04  D-NAG       PIC  9(002).
      *
           02  F               PIC  X(020).
      *
      *    [  �N�Ԃe�폜�N��  ]
           02  D-NFDD          PIC  9(004).                             �N��
      *
      *    [  �N�ԃT�C�Y�ʔN��  ]
           02  D-SSNG          PIC  9(004).                             �N��(1)
           02  D-SSNGD  REDEFINES D-SSNG.
             03  D-SSN         PIC  9(002).
             03  D-SSG         PIC  9(002).
           02  D-ESNG          PIC  9(004).                             �N��(2)
           02  D-ESNGD  REDEFINES D-ESNG.
             03  D-ESN         PIC  9(002).
             03  D-ESG         PIC  9(002).
      *    [  �H�i��z�N��  ]
           02  D-KTNG1         PIC  9(004).                             �N��(1)
           02  D-KTNG2         PIC  9(004).                             �N��(2)
      *
      *    [  �������v�X�V(HMD550) �`�F�b�N  ]
           02  D-HKC           PIC  9(001).                             �X�VC
      *
      *    [  ��������ڕW�@�N  ]
           02  DATE-HMN        PIC  9(002).                             �ڕW�N
      *
      *    [  �����N�ԗݐσ`�F�b�N(�ۯ�߰)  ]
           02  DATE-NRC        PIC  9(001).                             �ݐ�C
      *
      *    [  �������ԋ敪�ύX  (0.�ύX�Ȃ� , 1.�ύX����)  ]
           02  DATE-TM.                                                 ���Ӑ�
             03  DATE-HBC      PIC  9(001).                              ����
             03  F             PIC  9(001).
             03  DATE-HTNC     PIC  9(001).                              �S��
             03  F             PIC  9(001).
           02  DATE-HM.                                                 �i��
             03  DATE-HBC1     PIC  9(001).                              ����1
             03  DATE-HBC2     PIC  9(001).                              ����2
             03  DATE-HBC3     PIC  9(001).                              ����3
             03  F             PIC  9(001).
      *
      *    [  �����ϊ��`�F�b�N  ]
           02  DATE-SHC        PIC  9(001).
      *    [  �i�����㎩���U�փ`�F�b�N  ]
           02  DATE-HFC        PIC  9(001).
      *
           02  F               PIC  X(010).
           02  DATE-WC.                                                 �a��C
             03  DATE-WC1.                                              �����N
               04  DATE-YF1    PIC  9(002).                             �J�n�N
               04  DATE-YT1    PIC  9(002).                             �I���N
               04  DATE-YC1    PIC  9(004).                             +1988
             03  DATE-WC2.                                              ���a�N
               04  DATE-YF2    PIC  9(002).                             �J�n�N
               04  DATE-YT2    PIC  9(002).                             �I���N
               04  DATE-YC2    PIC  9(004).                             +1925
      *
           02  DATE-SC.                                                 ����C
             03  DATE-SC1.                                              1900�N
               04  DATE-NF1    PIC  9(002).                             �J�n�N
               04  DATE-NT1    PIC  9(002).                             �I���N
               04  DATE-NC1    PIC  9(004).                             +1900
             03  DATE-SC2.                                              2000�N
               04  DATE-NF2    PIC  9(002).                             �J�n�N
               04  DATE-NT2    PIC  9(002).                             �I���N
               04  DATE-NC2    PIC  9(004).                             +2000
       77  F                   PIC X(1).
      *

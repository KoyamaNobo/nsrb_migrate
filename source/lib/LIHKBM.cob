      **************************************
      *****     �����@�敪�}�X�^�[     *****
      *****      (  HKBM  64/4  )      *****
      **************************************
       01  HKBM.
           02  HKBM_PNAME1    PIC  X(004) VALUE "HKBM".
           02  F              PIC  X(001).
           02  HKBM_LNAME     PIC  X(004) VALUE "HKBM".
           02  F              PIC  X(001).
           02  HKBM_KEY1      PIC  X(100) VALUE SPACE.
           02  HKBM_KEY2      PIC  X(100) VALUE SPACE.
           02  HKBM_SORT      PIC  X(100) VALUE SPACE.
           02  HKBM_IDLST     PIC  X(100) VALUE SPACE.
           02  HKBM_RES       USAGE  POINTER.
      *
       01  HKB-R.
      *    * * *   K E Y  � � � �   * * *
           02  HKB-KEY.                                                 ����
             03  HKB-NO       PIC  9(002).                              ��
             03  HKB-BC       PIC  X(005).
             03  HKB-BC01  REDEFINES HKB-BC.
               04  HKB-TDFK   PIC  9(002).                              �s���{��
               04  F          PIC  X(003).
             03  HKB-BC02  REDEFINES HKB-BC.
               04  HKB-BM     PIC  9(001).                              ����
               04  F          PIC  X(004).
             03  HKB-BC04  REDEFINES HKB-BC.
               04  HKB-TNC    PIC  9(002).                              �S��
               04  F          PIC  X(003).
             03  HKB-BC05  REDEFINES HKB-BC.
               04  F          PIC  X(005).
             03  HKB-BC08  REDEFINES HKB-BC.
               04  HKB-KTKC   PIC  9(001).                              ����n��
               04  F          PIC  X(004).
             03  HKB-BC11  REDEFINES HKB-BC.
               04  HKB-BR1    PIC  9(002).                              ���ނP
               04  F          PIC  X(003).
             03  HKB-BC13  REDEFINES HKB-BC.
               04  HKB-BR22   PIC  9(001).                              ���ނQ
               04  F          PIC  X(004).
             03  HKB-BC14  REDEFINES HKB-BC.
               04  HKB-BR3    PIC  9(002).                              ���ނR
               04  F          PIC  X(003).
             03  HKB-BC16  REDEFINES HKB-BC.
               04  HKB-BMC    PIC  9(002).
               04  F          PIC  X(003).
             03  HKB-BC31  REDEFINES HKB-BC.
               04  HKB-NKC1   PIC  9(001).                              ����
               04  F          PIC  X(004).
             03  HKB-BC32  REDEFINES HKB-BC.
               04  HKB-NSC    PIC  9(001).
               04  F          PIC  X(004).
             03  HKB-BC41  REDEFINES HKB-BC.
               04  HKB-SUC    PIC  9(001).                              �d����
               04  F          PIC  X(004).
             03  HKB-BC42  REDEFINES HKB-BC.
               04  HKB-SSC    PIC  9(001).                              ���Y
               04  F          PIC  X(004).
      *    * * *   N A M E  � � � �   * * *
           02  HKB-NAME       PIC  X(057).
           02  HKB-NA01  REDEFINES HKB-NAME.
             03  HKB-FKNA     PIC  N(004).                              �{����
             03  HKB-SU       PIC  9(005).                              �l��
             03  HKB-KIN      PIC S9(010).                              ������z
             03  HKB-KTKCD    PIC  9(001).                              ����n��
             03  F            PIC  X(033).
           02  HKB-NA02  REDEFINES HKB-NAME.
             03  HKB-BMNA     PIC  N(006).                              ���喼
             03  F            PIC  X(045).
           02  HKB-NA04  REDEFINES HKB-NAME.
             03  HKB-TNNA     PIC  N(014).                              �S����
             03  F            PIC  X(029).
           02  HKB-NA05  REDEFINES HKB-NAME.
             03  HKB-UNN      PIC  9(006).
             03  HKB-SKN      PIC  9(006).
             03  HKB-NKN      PIC  9(006).
             03  HKB-DAI      PIC  X(010).
             03  F            PIC  X(029).
           02  HKB-NA08  REDEFINES HKB-NAME.
             03  HKB-KTNA     PIC  N(003).                              ����n��
             03  F            PIC  X(051).
           02  HKB-NA11  REDEFINES HKB-NAME.
             03  HKB-BRN1     PIC  N(008).                              ���ޖ��P
             03  F            PIC  X(041).
           02  HKB-NA13  REDEFINES HKB-NAME.
             03  HKB-BRN22    PIC  N(003).                              ���ޖ��Q
             03  F            PIC  X(051).
           02  HKB-NA14  REDEFINES HKB-NAME.
             03  HKB-BRN3     PIC  N(003).                              ���ޖ��R
             03  F            PIC  X(051).
           02  HKB-NA16  REDEFINES HKB-NAME.
             03  HKB-BMN      PIC  N(003).
             03  F            PIC  X(051).
           02  HKB-NA31  REDEFINES HKB-NAME.
             03  HKB-NKNA     PIC  N(006).                              ������
             03  F            PIC  X(045).
           02  HKB-NA32  REDEFINES HKB-NAME.
             03  HKB-NSNA     PIC  N(006).
             03  F            PIC  X(045).
           02  HKB-NA41  REDEFINES HKB-NAME.
             03  HKB-SUNA     PIC  N(005).                              �d����
             03  F            PIC  X(047).
           02  HKB-NA42  REDEFINES HKB-NAME.
             03  HKB-SSNA     PIC  N(004).                              ���Y
             03  F            PIC  X(049).
      *-----------------------------------------------------------------
           02  HKB-NA90  REDEFINES HKB-NAME.                            ��Ƌ敪
             03  HKB-SC.
               04  HKB-SC01   PIC  9(001).                              �������
               04  HKB-SC02   PIC  9(001).                              ����`�[
               04  HKB-SC03   PIC  9(001).                              �d�����
               04  HKB-SC04   PIC  9(001).                              �d��ϊ�
               04  HKB-SC05   PIC  9(001).                              ��������
               04  HKB-SC06   PIC  9(001).                              �����ϊ�
               04  HKB-SC07   PIC  9(001).                              �����[
               04  HKB-SC08   PIC  9(001).                              --------
               04  HKB-SC09   PIC  9(001).                              --------
               04  HKB-SC10   PIC  9(001).                              --------
               04  HKB-SC11   PIC  9(001).                              --------
               04  HKB-SC12   PIC  9(001).                              --------
               04  HKB-SC13   PIC  9(001).                              �����X�V
               04  HKB-SC14   PIC  9(001).                              ������
               04  HKB-SC15   PIC  9(001).                              �����X�V
             03  F            PIC  X(042).
       77  F                  PIC  X(001).

      *******************************************
      *****     �H�i�@�i�����v�}�X�^�[      *****
      *****  (  KHTD : KEY=KHTM1  170/3  )  *****
      *******************************************
       01  KHT-M.
           02  KHT-M_PNAME1   PIC  X(005) VALUE "KHTM1".
           02  F              PIC  X(001).
           02  KHT-M_LNAME    PIC  X(005) VALUE "KHT-M".
           02  F              PIC  X(001).
           02  KHT-M_KEY1     PIC  X(100) VALUE SPACE.
           02  KHT-M_SORT     PIC  X(100) VALUE SPACE.
           02  KHT-M_IDLST    PIC  X(100) VALUE SPACE.
           02  KHT-M_RES      USAGE  POINTER.
       01  KHT-R.
           02  KHT-KEYD.                                                ����
             03  KHT-YC       PIC  9(002).                              ֳĸ���
             03  KHT-NC       PIC  9(001).
             03  KHT-KEY.                                               ����
               04  KHT-KEY1   PIC  X(002).
               04  KHT-KEY2   PIC  9(003).
      *    [   � � �� �   �� � � �   ]
           02  KHT-KSU        PIC S9(006)V9(02).                        �ح���
           02  KHT-HSU        PIC S9(006)V9(02).                        ʲ�����
           02  KHT-ISU        PIC S9(006)V9(02).
           02  KHT-KKIN       PIC S9(008).                              ���ݶ޸
           02  KHT-SSU        PIC S9(006)V9(02).                        ������
           02  KHT-UKIN       PIC S9(008).                              �ر�޶޸
           02  KHT-NKIN       PIC S9(007).                              ��޷�޸
           02  KHT-GKIN       PIC S9(008).
           02  KHT-ZSU        PIC S9(006)V9(02).                        ��ݸؽ�
           02  KHT-ZKIN       PIC S9(008).                              ��ݸض޸
      *    [   ��޶� ĳ��   ]
           02  KHT-AZS        PIC S9(006).                              ��ݸؽ�
           02  KHT-AAS        PIC S9(006).                              ��޶ؽ�
           02  KHT-AUS        PIC S9(006).                              �ر�޽�
           02  KHT-ASS        PIC S9(006).                              ������
           02  KHT-AC         PIC  9(001).                              ����
           02  F              PIC  X(012).
      *    [   ���ݥ����  ]
           02  KHT-KIS        PIC  9(001).                              �������
           02  KHT-KCO        PIC  X(005).                              �ح�����
      *    [   �ŵۼ   ]
           02  KHT-JTS        PIC S9(006)V9(02).                        �����
           02  KHT-TTS        PIC S9(006)V9(02).                        �������
           02  KHT-HKS        PIC S9(006)V9(02).
      *
           02  F              PIC  X(016).
       77  F                  PIC  X(001).

      ***********************************************
      *****                                     *****
      **   �@�@�n�^�k�󋵁@�t�@�C���@�@�@�@�@�@�@�@**
      *****         ( J O J F )  512/1          *****
      ***********************************************
       01  JOJF.
           02  JOJF_PNAME1       PIC  X(004) VALUE "JOJF".
           02  F                 PIC  X(001).
           02  JOJF_LNAME        PIC  X(004) VALUE "JOJF".
           02  F                 PIC  X(001).
           02  JOJF_KEY1         PIC  X(100) VALUE SPACE.
           02  JOJF_SORT         PIC  X(100) VALUE SPACE.
           02  JOJF_IDLST        PIC  X(100) VALUE SPACE.
           02  JOJF_RES          USAGE  POINTER.
       01  JOJF-REC.
           02  JOJF-KEY.                                                KEY
               03  JOJF-01       PIC 9(04).                             SEQ.NO
           02  JOJF-02           PIC 9(04).                             ���t
           02  JOJF-02R          REDEFINES  JOJF-02.
               03  JOJF-021      PIC 9(02).                               �N
               03  JOJF-022      PIC 9(02).                               ��
           02  JOJF-03           PIC 9(04).                             �J�n����
           02  JOJF-03R          REDEFINES  JOJF-03.
               03  JOJF-031      PIC 9(02).                               ��
               03  JOJF-032      PIC 9(02).                               ��
           02  JOJF-04           PIC 9(04).                             �I������
           02  JOJF-04R          REDEFINES  JOJF-04.
               03  JOJF-041      PIC 9(02).                               ��
               03  JOJF-042      PIC 9(02).                               ��
           02  JOJF-05           PIC 9(01).                             �����敪
           02  JOJF-06.                                                 �I����
               03  JOJF-061      PIC 9(01).                               �敪
               03  JOJF-062      PIC X(01).                               STS 1
               03  JOJF-063      PIC 9(02).                               STS 2
           02  JOJF-07           PIC 9(03).                             �����
           02  JOJF-TBL.
               03  JOJF-TBL1     OCCURS  12.
                   04  JOJF-08   PIC 9(02).                             �ް��敪
                   04  JOJF-09   PIC 9(06).                             �ް�����
                   04  JOJF-10   PIC 9(06).                             ���󌏐�
                   04  JOJF-11   PIC X(10).                             ���� KEY
                   04  JOJF-12   PIC X(10).                             END  KEY
                   04  F         PIC X(06).
           02  F                 PIC X(04).
           02  JOJF-90           PIC 9(04).                             NEXT NO
       77  F                     PIC  X(001).

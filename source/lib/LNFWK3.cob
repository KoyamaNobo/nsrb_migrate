      ***********************************
      ******    �׎D���[�N�R            *
      ******                ISAM        *
      ******                 32/8       *
      ***********************************
       01  NF-WK3.
           02  NF-WK3_PNAME1           PIC  X(017) VALUE SPACE.
           02  F                       PIC  X(001).
           02  NF-WK3_LNAME            PIC  X(006) VALUE "NF-WK3".
           02  F                       PIC  X(001).
           02  NF-WK3_KEY1             PIC  X(100) VALUE SPACE.
           02  NF-WK3_KEY2             PIC  X(100) VALUE SPACE.
           02  NF-WK3_KEY3             PIC  X(100) VALUE SPACE.
           02  NF-WK3_SORT             PIC  X(100) VALUE SPACE.
           02  NF-WK3_IDLST            PIC  X(100) VALUE SPACE.
           02  NF-WK3_RES              USAGE  POINTER.
      *
       01  WK3-R.
           02   WK3-KEY.                                                KEY
                03   WK3-01            PIC 9(06).                       ����
                03   WK3-02            PIC 9(02).                       �}��(��)
                03   WK3-03            PIC 9(01).                       �ꑫ��KB
           02   WK3-04                 PIC 9(05).                       ���v����
           02   WK3-05                 PIC 9(03).                       ���ݓ���
           02   WK3-06                 PIC S9(04).                      ��������
           02   WK3-07                 PIC S9(04).                      �[��
           02   WK3-08                 PIC S9(04).                      ���ޕʌ�
           02   F                      PIC X(03).
       77  F                           PIC X(01).

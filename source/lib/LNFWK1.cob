      ***********************************
      ******    �׎D���[�N�P�@�@�@�@�@�@*
      ******                ISAM        *
      ******                 51/5       *
      ***********************************
       01  NF-WK1.
           02  NF-WK1_PNAME1           PIC  X(017) VALUE SPACE.
           02  F                       PIC  X(001).
           02  NF-WK1_LNAME            PIC  X(006) VALUE "NF-WK1".
           02  F                       PIC  X(001).
           02  NF-WK1_KEY1             PIC  X(100) VALUE SPACE.
           02  NF-WK1_KEY2             PIC  X(100) VALUE SPACE.
           02  NF-WK1_KEY3             PIC  X(100) VALUE SPACE.
           02  NF-WK1_KEY4             PIC  X(100) VALUE SPACE.
           02  NF-WK1_KEY5             PIC  X(100) VALUE SPACE.
           02  NF-WK1_KEY6             PIC  X(100) VALUE SPACE.
           02  NF-WK1_SORT             PIC  X(100) VALUE SPACE.
           02  NF-WK1_IDLST            PIC  X(100) VALUE SPACE.
           02  NF-WK1_RES              USAGE  POINTER.
      *
       01  WK1-R.
           02   WK1-KEY.                                                KEY
                03   WK1-01            PIC 9(06).                       ����
                03   WK1-02.                                            �}��
                     04   WK1-021      PIC 9(02).                       �@����
                     04   WK1-022      PIC 9(03).                       �@�A��
                03   WK1-03            PIC 9(01).                       �ꑫ��KB
                03   WK1-04            PIC 9(06).                       �i������
                03   WK1-05            PIC 9(03).                       ���޺���
           02   WK1-06                 PIC 9(03).                       �Z�b�g��
           02   WK1-07                 PIC S9(04).                      �w�}����
           02   WK1-08                 PIC 9(06).                       �w�}�`NO
           02   WK1-09                 PIC 9(03).                       ���ݓ���
           02   WK1-10                 PIC S9(04).                      ��������
           02   WK1-11                 PIC S9(04).                      �c��
           02   F                      PIC X(06).
       77  F                           PIC X(01).

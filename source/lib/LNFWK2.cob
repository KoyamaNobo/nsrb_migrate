      ***********************************
      ******    �׎D���[�N�Q�@�@�@�@�@�@*
      ******                ISAM        *
      ******                 42/6       *
      ***********************************
       01  NF-WK2.
           02  NF-WK2_PNAME1           PIC  X(017) VALUE SPACE.
           02  F                       PIC  X(001).
           02  NF-WK2_LNAME            PIC  X(006) VALUE "NF-WK2".
           02  F                       PIC  X(001).
           02  NF-WK2_KEY1             PIC  X(100) VALUE SPACE.
           02  NF-WK2_SORT             PIC  X(100) VALUE SPACE.
           02  NF-WK2_IDLST            PIC  X(100) VALUE SPACE.
           02  NF-WK2_RES              USAGE  POINTER.
      *
       01  WK2-R.
           02   WK2-KEY.                                                KEY
                03   WK2-01            PIC 9(06).                       ����
                03   WK2-02.                                            �}��
                     04   WK2-021      PIC 9(02).                       �@����
                     04   WK2-022      PIC 9(03).                       �@�A��
                03   WK2-03            PIC 9(01).                       �ꑫ��KB
                03   WK2-04            PIC 9(06).                       �i������
                03   WK2-05            PIC 9(03).                       ���޺���
           02   WK2-06                 PIC 9(03).                       �Z�b�g��
           02   WK2-07                 PIC 9(06).                       �w�}�`NO
           02   WK2-08                 PIC 9(03).                       ���ݓ���
           02   WK2-09                 PIC S9(04).                      �c��
           02   F                      PIC X(04).
           02   WK2-70                 PIC 9(01).
       77  F                           PIC X(01).

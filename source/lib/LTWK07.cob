      *****�@�󒍐����v���[�N�i�����Ӑ�i���ʁj�@*******************************
       01  JT-WK07.
           02  JT-WK07_PNAME1     PIC  X(009) VALUE SPACE.
           02  F                  PIC  X(001).
           02  JT-WK07_LNAME      PIC  X(007) VALUE "JT-WK07".
           02  F                  PIC  X(001).
           02  JT-WK07_KEY1       PIC  X(100) VALUE SPACE.
           02  JT-WK07_SORT       PIC  X(100) VALUE SPACE.
           02  JT-WK07_IDLST      PIC  X(100) VALUE SPACE.
           02  JT-WK07_RES        USAGE  POINTER.
       01  WK07-R.
           02  WK07-01.                                                 �N��
             03  WK07-011         PIC 9(02).                            �N
             03  WK07-012         PIC 9(02).                            ��
           02  WK07-02            PIC 9(04).                            ���Ӑ�
           02  WK07-03            PIC 9(06).                            �i��
           02  WK07-04            PIC 9(01).                            ����
           02  WK07-05.                                                 �󒍐�
             03  WK07-051  OCCURS 10.
               04  WK07-0511      PIC S9(06).
           02  FILLER             PIC X(53).                            FILLER
       77  F                      PIC X(01).

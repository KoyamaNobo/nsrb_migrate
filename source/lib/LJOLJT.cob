      ***********************************************
      *****                                     *****
      **             �n�^�k��M�g����              **
      *****         ( JOLJT   )  256/1          *****
      ***********************************************
       01  JOLJT.
           02  JOLJT_PNAME1      PIC  X(005) VALUE "JOLJT".
           02  F                 PIC  X(001).
           02  JOLJT_LNAME       PIC  X(005) VALUE "JOLJT".
           02  F                 PIC  X(001).
           02  JOLJT_KEY1        PIC  X(100) VALUE SPACE.
           02  JOLJT_SORT        PIC  X(100) VALUE SPACE.
           02  JOLJT_IDLST       PIC  X(100) VALUE SPACE.
           02  JOLJT_RES         USAGE  POINTER.
      *----�o�׎w���g����        �i�敪���P�P�j
       01  JOLJT11-REC.
           02  JOLJT11-01        PIC 9(02).                             �q�b�敪
           02  JOLJT11-KEYW.
             03  JOLJT11-02      PIC 9(06).                                �����
             03  JOLJT11-03      PIC 9(01).                                �ޮ�
           02  JOLJT11-04        PIC 9(01).                                ��ݸ
           02  JOLJT11-05.                                                 �����
               03  JOLJT11-051   PIC 9(04).
               03  JOLJT11-052   PIC 9(02).                                ·
               03  JOLJT11-053   PIC 9(02).                                �
           02  JOLJT11-06.                                                 �����
               03  JOLJT11-061   PIC 9(04).
               03  JOLJT11-062   PIC 9(02).                                ·
               03  JOLJT11-063   PIC 9(02).                                �
           02  JOLJT11-07.                                                 �����
               03  JOLJT11-071   PIC 9(04).                                ĸ���
               03  JOLJT11-072   PIC 9(03).                                ��� N
           02  JOLJT11-08        PIC 9(01).                                �� ��
           02  JOLJT11-09.                                                 �ޭ��
               03  JOLJT11-091   PIC 9(06).                                �ޭ��
               03  JOLJT11-092   PIC 9(01).                                �ޮ�
           02  JOLJT11-10        PIC 9(06).                                �ݺ��
           02  JOLJT11-11        PIC 9(01).                                ���޸
           02  JOLJT11-12.                                                 �����
               03  JOLJT11-121   OCCURS  10.                               �����
                   04  JOLJT11-1211      PIC  9(04).
                   04  JOLJT11-1211S     PIC  9(01).                    0:+,1:-
               03  JOLJT11-122   PIC  9(05).
               03  JOLJT11-122S  PIC  9(01).                            0:+,1:-
           02  JOLJT11-13.                                                 �����
               03  JOLJT11-131   OCCURS  10.                               �����
                   04  JOLJT11-1311      PIC  9(04).
                   04  JOLJT11-1311S     PIC  9(01).                    0:+,1:-
               03  JOLJT11-132   PIC  9(05).
               03  JOLJT11-132S  PIC  9(01).                            0:+,1:-
           02  JOLJT11-14        PIC 9(01).                                ��޶�
           02  JOLJT11-15        PIC 9(01).                                �^�� 
           02  JOLJT11-15A       PIC 9(03).                                �Z�b 
           02  JOLJT11-15B       PIC 9(06).                                ���� 
           02  JOLJT11-15C       PIC 9(02).                                �}��
           02  JOLJT11-15D       PIC N(09).                                �z�B
           02  JOLJT11-16        PIC N(23).                             �E�v
           02  JOLJT11-20        PIC X(10).
           02  JOLJT11-16A       PIC  9(03).                            ��
           02  JOLJT11-16AS      PIC  9(01).                            0:+,1:-
           02  FILLER            PIC X(01).
           02  JOLJT11-19        PIC X(01).                             ��������
           02  JOLJT11-168       PIC 9(01).                                A-890
           02  JOLJT11-17        PIC 9(01).                             ��� 
           02  JOLJT11-18        PIC 9(01).                             �X�V�
       77  F                     PIC  X(001).

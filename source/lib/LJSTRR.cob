       01  JSTRRF.                                                      �����������
           02  JSTRRF_PNAME1          PIC  X(006) VALUE "JSTRRF".
           02  F                      PIC  X(001).
           02  JSTRRF_LNAME           PIC  X(006) VALUE "JSTRRF".
           02  F                      PIC  X(001).
           02  JSTRRF_KEY1            PIC  X(100) VALUE SPACE.
           02  JSTRRF_SORT            PIC  X(100) VALUE SPACE.
           02  JSTRRF_IDLST           PIC  X(100) VALUE SPACE.
           02  JSTRRF_RES             USAGE  POINTER.
      *
       01  JSTRR-R.
           02   JSTRR-KEY.                                              KEY
                03   JSTRR-01         PIC 9(6).                         �������
                03   JSTRR-02         PIC 9(1).                         �ޮ�
           02   JSTRR-03              PIC 9(1).                         ��ݸ
           02   JSTRR-04.                                               ������ �
                03  JSTRR-041         PIC 9(4).
                03  JSTRR-042         PIC 9(2).                         ·
                03  JSTRR-043         PIC 9(2).                         �
           02   JSTRR-05.                                               �����޼�
                03  JSTRR-051         PIC 9(4).
                03  JSTRR-052         PIC 9(2).                         ·
                03  JSTRR-053         PIC 9(2).                         �
           02   JSTRR-05L   REDEFINES  JSTRR-05.
                03  F                 PIC 9(2).
                03  JSTRR-05S         PIC 9(6).
           02   JSTRR-06.                                               ����� CD
                03  JSTRR-061         PIC 9(4).                         ĸ�����
                03  JSTRR-062         PIC 9(3).                         ��� NO
           02   JSTRR-07              PIC 9(1).                         �� ����
           02   JSTRR-08.                                               �ޭ���
                03  JSTRR-081         PIC 9(6).                         �ޭ���NO
                03  JSTRR-082         PIC 9(1).                         �ޮ�
           02   JSTRR-09              PIC 9(6).                         �ݺ���
           02   JSTRR-10              PIC 9(1).                         ���޸���
           02   JSTRR-11.                                               ��������
                03  JSTRR-111    OCCURS  10.                            �������
                    04  JSTRR-1111    PIC S9(4).
                03  JSTRR-112         PIC S9(5).
           02   JSTRR-12.                                               �������
                03  JSTRR-121    OCCURS  10.                            �������
                    04  JSTRR-1211    PIC S9(4).
                03  JSTRR-122         PIC S9(5).
           02  JSTRR-13               PIC 9(1).                         ��޶� KB
           02  JSTRR-14               PIC 9(1).                         �^���b�c
           02  JSTRR-14A              PIC 9(3).                         �Z�b�g��
           02  JSTRR-14B              PIC 9(6).                         �����
           02  JSTRR-14C              PIC 9(2).                         �}��
           02  JSTRR-14D              PIC N(9).                         �z�B
           02  JSTRR-15               PIC N(23).                        �E�v
           02  JSTRR-20               PIC X(10).
           02  JSTRR-15A              PIC S9(03).                       ��
           02  JSTRR-30               PIC 9(1).
           02  JSTRR-40               PIC X(9).
           02  FILLER                 PIC X(08).
           02  JSTRR-90               PIC 9(08).
           02  JSTRR-19               PIC X(01).
           02  JSTRR-158              PIC 9(01).
           02  JSTRR-16               PIC 9(01).                        ��ʋ���
           02  JSTRR-17               PIC 9(01).                        �X�V���
       77  F                          PIC X(01).

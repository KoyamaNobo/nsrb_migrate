       01  JT-WK03.                                                     �����������
           02  JT-WK03_PNAME1        PIC  X(009) VALUE SPACE.
           02  F                     PIC  X(001).
           02  JT-WK03_LNAME         PIC  X(007) VALUE "JT-WK03".
           02  F                     PIC  X(001).
           02  JT-WK03_KEY1          PIC  X(100) VALUE SPACE.
           02  JT-WK03_SORT          PIC  X(100) VALUE SPACE.
           02  JT-WK03_IDLST         PIC  X(100) VALUE SPACE.
           02  JT-WK03_RES           USAGE  POINTER.
      *
       01  W03-R.
           02   W03-KEY.                                                KEY
                03   W03-01          PIC 9(6).                          �������
                03   W03-02          PIC 9(1).                          �ޮ�
           02   W03-03               PIC 9(1).                          ��ݸ
           02   W03-04.                                                 ������ �
                03  W03-041          PIC 9(4).
                03  W03-041L  REDEFINES  W03-041.
                    04  W03-0411     PIC 9(2).
                    04  W03-0412     PIC 9(2).
                03  W03-042          PIC 9(2).                          ·
                03  W03-043          PIC 9(2).                          �
           02   W03-05.                                                 �����޼�
                03  W03-051          PIC 9(4).
                03  W03-051L  REDEFINES  W03-051.
                    04  W03-0511     PIC 9(2).
                    04  W03-0512     PIC 9(2).
                03  W03-052          PIC 9(2).                          ·
                03  W03-053          PIC 9(2).                          �
           02   W03-06.                                                 ����� CD
                03  W03-061          PIC 9(4).                          ĸ�����
                03  W03-062          PIC 9(3).                          ��� NO
           02   W03-07               PIC 9(1).                          �� ����
           02   W03-08.                                                 �ޭ���
                03  W03-081          PIC 9(6).                          �ޭ���NO
                03  W03-082          PIC 9(1).                          �ޮ�
           02   W03-09               PIC 9(6).                          �ݺ���
           02   W03-10               PIC 9(1).                          ���޸���
           02   W03-11.                                                 ��������
                03  W03-111    OCCURS  10.                              �������
                    04  W03-1111     PIC S9(4).
                03  W03-112          PIC S9(5).
           02   W03-12.                                                 �������
                03  W03-121    OCCURS  10.                              �������
                    04  W03-1211     PIC S9(4).
                03  W03-122          PIC S9(5).
           02  W03-13                PIC 9(1).                          ��޶� KB
           02  W03-14                PIC 9(1).                          �^���b�c
           02  W03-14A               PIC 9(3).                          �Z�b�g��
           02  W03-14B               PIC 9(6).                          �����
           02  W03-14C               PIC 9(2).                          �}��
           02  W03-14D               PIC N(9).                          �z�B
           02  W03-15                PIC N(23).                         �E�v
           02  W03-20                PIC X(10).
           02  W03-15A               PIC S9(03).                        ��
           02  FILLER                PIC X(27).
           02  W03-158               PIC 9(01).
           02  W03-16                PIC 9(01).                         ��ʋ���
           02  W03-17                PIC 9(01).                         �X�V���
       77  F                         PIC X(01).

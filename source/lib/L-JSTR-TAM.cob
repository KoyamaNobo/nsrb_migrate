       01  JSTR.                                                        ��������
           02  JSTR_PNAME1            PIC  X(008) VALUE "JSTR-TAM".
           02  F                      PIC  X(001).
           02  JSTR_LNAME             PIC  X(004) VALUE "JSTR".
           02  F                      PIC  X(001).
           02  JSTR_KEY1              PIC  X(100) VALUE SPACE.
           02  JSTR_KEY2              PIC  X(100) VALUE SPACE.
           02  JSTR_SORT              PIC  X(100) VALUE SPACE.
           02  JSTR_IDLST             PIC  X(100) VALUE SPACE.
           02  JSTR_RES               USAGE  POINTER.                                       ���
      *
       01  JSTR-R.
           02   JSTR-KEY.                                               KEY
                03   JSTR-01          PIC 9(6).                         �������
                03   JSTR-02          PIC 9(1).                         �ޮ�
           02   JSTR-03               PIC 9(1).                         ��ݸ
           02   JSTR-04.                                                ������ �
                03  JSTR-041          PIC 9(4).
                03  JSTR-041L  REDEFINES  JSTR-041.
                    04  JSTR-0411     PIC 9(2).
                    04  JSTR-0412     PIC 9(2).
                03  JSTR-042          PIC 9(2).                         ·
                03  JSTR-043          PIC 9(2).                         �
           02   JSTR-04L   REDEFINES  JSTR-04.
                03  F                 PIC 9(2).
                03  JSTR-04S          PIC 9(6).
           02   JSTR-05.                                                �����޼�
                03  JSTR-051          PIC 9(4).
                03  JSTR-051L  REDEFINES  JSTR-051.
                    04  JSTR-0511     PIC 9(2).
                    04  JSTR-0512     PIC 9(2).
                03  JSTR-052          PIC 9(2).                         ·
                03  JSTR-053          PIC 9(2).                         �
           02   JSTR-05L   REDEFINES  JSTR-05.
                03  F                 PIC 9(2).
                03  JSTR-05S          PIC 9(6).
           02   JSTR-06.                                                ����� CD
                03  JSTR-061          PIC 9(4).                         ĸ�����
                03  JSTR-062          PIC 9(3).                         ��� NO
           02   JSTR-07               PIC 9(1).                         �� ����
           02   JSTR-08.                                                �ޭ���
                03  JSTR-081          PIC 9(6).                         �ޭ���NO
                03  JSTR-082          PIC 9(1).                         �ޮ�
           02   JSTR-09               PIC 9(6).                         �ݺ���
           02   JSTR-10               PIC 9(1).                         ���޸���
           02   JSTR-11.                                                ��������
                03  JSTR-111    OCCURS  10.                             �������
                    04  JSTR-1111     PIC S9(4).
                03  JSTR-112          PIC S9(5).
           02   JSTR-12.                                                �������
                03  JSTR-121    OCCURS  10.                             �������
                    04  JSTR-1211     PIC S9(4).
                03  JSTR-122          PIC S9(5).
           02  JSTR-13                PIC 9(1).                         ��޶� KB
           02  JSTR-14                PIC 9(1).                         �^���b�c
           02  JSTR-14A               PIC 9(3).                         �Z�b�g��
           02  JSTR-14B               PIC 9(6).                         �����
           02  JSTR-14C               PIC 9(2).                         �}��
           02  JSTR-14D               PIC N(9).                         �z�B
           02  JSTR-15                PIC N(23).                        �E�v
           02  JSTR-20                PIC X(10).                        �E�v
           02  JSTR-15A               PIC S9(03).                       ��
           02  JSTR-30                PIC 9(01).
           02  JSTR-40.
               03  JSTR-401.
                   04  JSTR-4011      PIC X(03).
                   04  JSTR-4012      PIC 9(01).
               03  JSTR-402.
                   04  JSTR-4021      PIC X(03).
                   04  JSTR-4022      PIC 9(01).
                   04  JSTR-4023      PIC 9(01).
           02  FILLER                 PIC X(16).
           02  JSTR-19                PIC X(01).
           02  JSTR-158               PIC 9(01).
           02  JSTR-16                PIC 9(01).                        ��ʋ���
           02  JSTR-17                PIC 9(01).                        �X�V���
       77  F                          PIC  X(001).

      *
      ***  �󒍃}�X�^  �i�j�d�x�Q�j
      *
       01  JMST2.                                                        �����
           02  JMST2_PNAME1               PIC  X(005) VALUE "JMST1".
           02  F                          PIC  X(001).
           02  JMST2_LNAME                PIC  X(005) VALUE "JMST2".
           02  F                          PIC  X(001).
           02  JMST2_KEY1                 PIC  X(100) VALUE SPACE.
           02  JMST2_KEY2                 PIC  X(100) VALUE SPACE.
           02  JMST2_KEY3                 PIC  X(100) VALUE SPACE.
           02  JMST2_KEY4                 PIC  X(100) VALUE SPACE.
           02  JMST2_KEY5                 PIC  X(100) VALUE SPACE.
           02  JMST2_KEY6                 PIC  X(100) VALUE SPACE.
           02  JMST2_SORT                 PIC  X(100) VALUE SPACE.
           02  JMST2_IDLST                PIC  X(100) VALUE SPACE.
           02  JMST2_RES                  USAGE  POINTER.
      *
       01  JMST2-R.
           02   JMST2-01                  PIC 9(1).                     ��޶�
           02   JMST2-02.                                               �ޭ�����
                03   JMST2-021            PIC 9(4).
                03   JMST2-021L   REDEFINES  JMST2-021.
                     04   JMST2-0211      PIC 9(2).
                     04   JMST2-0212      PIC 9(2).
                03   JMST2-022            PIC 9(2).                     ·
                03   JMST2-023            PIC 9(2).                     �
           02   JMST2-02L    REDEFINES  JMST2-02.
                03   F                    PIC 9(2).
                03   JMST2-02S            PIC 9(6).
           02   JMST2-03                  PIC 9(6).                     �ݺ���
           02   JMST2-KEY.
                03   JMST2-04             PIC 9(4).                     ĸ�����
                03   JMST2-05             PIC 9(6).                     �ݺ���
                03   JMST2-06.                                          ɳ�
                     04   JMST2-061       PIC 9(4).
                     04   JMST2-062       PIC 9(2).                     ·
                     04   JMST2-063       PIC 9(2).                     �
                03   JMST2-06L    REDEFINES  JMST2-06.
                     04   F               PIC 9(2).
                     04   JMST2-06S       PIC 9(6).
                03   JMST2-07             PIC 9(6).                     �ޭ���NO
                03   JMST2-08             PIC 9(1).                     �ޮ� NO
           02   JMST2-09                  PIC 9(1).                     ���޸���
           02   JMST2-10                  PIC 9(3).                     ����� NO
           02   JMST2-11.                                               �ޭ�����
                03  JMST2-111            OCCURS  10.                    �������
                    04  JMST2-1111       PIC S9(6)   COMP-3.
           02   JMST2-12.                                               ������
                03  JMST2-121            OCCURS  10.                    �������
                    04  JMST2-1211       PIC S9(6)   COMP-3.
           02   JMST2-14.                                               ��ݹ����
                03  JMST2-141  OCCURS 10 PIC S9(06)  COMP-3.            �������
           02   JMST2-15.                                               ��������
                03  JMST2-151  OCCURS 10 PIC S9(06)  COMP-3.            �������
           02   JMST2-16                 PIC S9(03).                    ��Ľ�
           02   JMST2-23                 PIC 9(04).
           02   F                        PIC X(08).
           02   JMST2-20                 PIC 9(03).                     �ݺ�NO.
           02   JMST2-13                 PIC N(32).                     ÷ֳ
           02   JMST2-21                 PIC 9(01).                     �����敪
           02   JMST2-17                 PIC 9(05).
           02   JMST2-22                 PIC X(10).
           02   JMST2-51                 PIC 9(03).
           02   FILLER                   PIC X(24).
           02   JMST2-89.
                03  JMST2-891            PIC 9(08).
                03  JMST2-892            PIC 9(01).
           02   JMST2-90                 PIC 9(01).                     ������
           02   JMST2-91                 PIC 9(02).                     �S���敪
       77  F                             PIC X(01).

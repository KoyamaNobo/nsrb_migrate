      *
      ***  �󒍃}�X�^  �i�j�d�x�P�j
      *
       01  JMST1.                                                        �����
           02  JMST1_PNAME1               PIC  X(005) VALUE "JMST1".
           02  F                          PIC  X(001).
           02  JMST1_LNAME                PIC  X(005) VALUE "JMST1".
           02  F                          PIC  X(001).
           02  JMST1_KEY1                 PIC  X(100) VALUE SPACE.
           02  JMST1_KEY2                 PIC  X(100) VALUE SPACE.
           02  JMST1_SORT                 PIC  X(100) VALUE SPACE.
           02  JMST1_IDLST                PIC  X(100) VALUE SPACE.
           02  JMST1_RES                  USAGE  POINTER.
      *
       01  JMST1-R.
           02   JMST1-01                  PIC 9(1).                     ��޶�
           02   JMST1-02.                                               �ޭ�����
                03   JMST1-021            PIC 9(4).
                03   JMST1-021L   REDEFINES  JMST1-021.
                     04  JMST1-0211       PIC 9(2).
                     04  JMST1-0212       PIC 9(2).
                03   JMST1-022            PIC 9(2).                     ·
                03   JMST1-023            PIC 9(2).                     �
           02   JMST1-02L   REDEFINES  JMST1-02.
                03   F                    PIC 9(2).
                03   JMST1-02S            PIC 9(6).
           02   JMST1-03                  PIC 9(6).                     �ݺ���
           02   JMST1-04                  PIC 9(4).                     ĸ�����
           02   JMST1-05                  PIC 9(6).                     �ݺ���
           02   JMST1-06.                                               ɳ�
                03   JMST1-061            PIC 9(4).
                03   JMST1-062            PIC 9(2).                     ·
                03   JMST1-063            PIC 9(2).                     �
           02   JMST1-06L   REDEFINES  JMST1-06.
                03   F                    PIC 9(2).
                03   JMST1-06S            PIC 9(6).
           02   JMST1-KEY1.
                03   JMST1-07             PIC 9(6).                     �ޭ���NO
                03   JMST1-08             PIC 9(1).                     �ޮ� NO
           02   JMST1-09                  PIC 9(1).                     ���޸���
           02   JMST1-10                  PIC 9(3).                     ����� NO
           02   JMST1-11.                                               �ޭ�����
                03  JMST1-111            OCCURS  10.                    �������
                    04  JMST1-1111       PIC S9(6)   COMP-3.
           02   JMST1-12.                                               ������
                03  JMST1-121            OCCURS  10.                    �������
                    04  JMST1-1211       PIC S9(6)   COMP-3.
           02   JMST1-14.                                               ��ݹ����
                03  JMST1-141  OCCURS 10 PIC S9(06)  COMP-3.            �������
           02   JMST1-15.                                               ��������
                03  JMST1-151  OCCURS 10 PIC S9(06)  COMP-3.            �������
           02   JMST1-16                 PIC S9(03).                    ��Ľ�
           02   JMST1-23                 PIC 9(04).
           02   F                        PIC X(08).
           02   JMST1-20                 PIC 9(03).                     �ݺ�NO.
           02   JMST1-13                 PIC N(32).                     ÷ֳ
           02   JMST1-21                 PIC 9(01).                     �����敪
           02   JMST1-17                 PIC 9(05).
           02   JMST1-22                 PIC X(10).
           02   JMST1-51                 PIC 9(03).
           02   FILLER                   PIC X(24).
           02   JMST1-89.
                03  JMST1-891            PIC 9(08).
                03  JMST1-892            PIC 9(01).
           02   JMST1-90                 PIC 9(01).                     ������
           02   JMST1-91                 PIC 9(02).                     �S���敪
       77  F                             PIC X(01).

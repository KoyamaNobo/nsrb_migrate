       01  HSMSF.
           02  HSMSF_PNAME1           PIC  X(005) VALUE "HSMSF".
           02  F                      PIC  X(001).
           02  HSMSF_LNAME            PIC  X(005) VALUE "HSMSF".
           02  F                      PIC  X(001).
           02  HSMSF_KEY1             PIC  X(100) VALUE SPACE.
           02  HSMSF_KEY2             PIC  X(100) VALUE SPACE.
           02  HSMSF_SORT             PIC  X(100) VALUE SPACE.
           02  HSMSF_IDLST            PIC  X(100) VALUE SPACE.
           02  HSMSF_RES              USAGE  POINTER.
      *
       01  HSMS-R.
           02  HSMS-R1.
               03  HSMS-KEY.                                            KEY
                   04   HSMS-01           PIC 9(6).                      �������
                   04   HSMS-02           PIC 9(1).                      �ޮ�
               03  HSMS-03                PIC 9(1).                      ��ݸ
               03  HSMS-05.                                              �����޼�
                   04  HSMS-051           PIC 9(4).
                   04  HSMS-052           PIC 9(2).                      ·
                   04  HSMS-053           PIC 9(2).                      �
               03  HSMS-06.                                              ����� CD
                   04  HSMS-061           PIC 9(4).                      ĸ�����
                   04  HSMS-062           PIC 9(3).                      ��� NO
               03  HSMS-07                PIC 9(1).                      �� ����
               03  HSMS-09                PIC 9(6).                      �ݺ���
               03  HSMS-10                PIC 9(1).                      ���޸���
               03  HSMS-12.                                             �������
                   04  HSMS-121    OCCURS  10.                          �������
                       05  HSMS-1211      PIC S9(4).
                   04  HSMS-122           PIC S9(6).
               03  HSMS-13                PIC 9(1).                      ��޶� KB
               03  HSMS-14                PIC S9(03).                    ��
               03  HSMS-21                PIC 9(01).                     �ݼ޸���
               03  HSMS-20                PIC 9(02).                     ��ĳ
               03  HSMS-16                PIC 9(02).                     ���ٲ2
               03  HSMS-17                PIC 9(05).                     �ݶ
               03  HSMS-18                PIC 9(08).
               03  HSMS-22                PIC X(10).
               03  HSMS-23                PIC 9(01).                     �ݶݸ���
               03  HSMS-24                PIC 9(01).
               03  FILLER                 PIC X(14).
               03  HSMS-26                PIC 9(01).
               03  HSMS-25                PIC 9(01).
               03  HSMS-19                PIC 9(01).                     �ֳ����
           02  HSMS-R2    REDEFINES  HSMS-R1.
               03  HSMS-KEYB.                                            KEY
                   04   HSMS-01B          PIC 9(6).                      �������
                   04   HSMS-02B          PIC 9(1).                      �ޮ�
               03  HSMS-03B               PIC 9(1).
               03  HSMS-05B.
                   04  HSMS-051B          PIC 9(4).
                   04  HSMS-052B          PIC 9(2).
                   04  HSMS-053B          PIC 9(2).
               03  HSMS-06B.
                   04  HSMS-061B          PIC 9(4).
                   04  HSMS-062B          PIC 9(3).
               03  HSMS-07B               PIC 9(1).
               03  HSMS-15                PIC N(24).                     �E�v
               03  FILLER                 PIC X(37).
               03  HSMS-23B               PIC 9(01).
               03  HSMS-24B               PIC 9(01).
               03  FILLER                 PIC X(14).
               03  HSMS-26B               PIC 9(01).
               03  HSMS-25B               PIC 9(01).
               03  HSMS-19B               PIC 9(01).                     �ֳ����
       77  F                              PIC X(01).

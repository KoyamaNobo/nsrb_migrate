      ***  �d��q�X�g��     (170/3)
       01  SH-REC.
           02  SH-KEY3.
             03  HKACD1.
               04  HACCNTCD     PIC 9(4).
               04  HHOACCNT     PIC 9(4).
             03  SH-KEY1.
               04  HTRDATE      PIC 9(8).
               04  HJUNLNO      PIC 9(6).
               04  HLINENO      PIC 9(2).
               04  HDR-CR       PIC 9(1).
           02  HSECTCD          PIC 9(4).                               ����C
           02  HSKINCD          PIC 9(3).                               ��ݸ��C
           02  HTAXKB           PIC X(1).                               ��޲C
           02  HAMOUNT          PIC S9(10).                             �ݶ޸
           02  HTEG-BAN         PIC 9(2).                               ö��BKC
           02  HKACD2.
             03  HOPPCD         PIC 9(4).                               ��öӸC
             03  HHOOPPCD       PIC 9(4).                                μޮ�Ӹ
           02  HCUSTCD          PIC 9(5).                               ��˷��C
           02  HTEKICD          PIC 9(3).                               ÷ֳC
           02  HTEKIYO          PIC N(20).                              ÷ֳ
           02  HKEIHIKB         PIC 9(1).                               ���C
           02  HNAMEN           PIC N(10).
           02  SH-KEY2.                                                 KEY2
             03  HACCNTCD2      PIC 9(4).                               �ӸC
             03  HTRDATE2       PIC 9(8).
             03  HJUNLNO2       PIC 9(6).                               ����߮�
             03  HLINENO2       PIC 9(2).                                �ޮ�
             03  HDR-CR2        PIC 9(1).                               �����C
           02  F                PIC X(25).
           02  HETAX            PIC X(01).
           02  HCOM             PIC 9(1).                               �޲����C

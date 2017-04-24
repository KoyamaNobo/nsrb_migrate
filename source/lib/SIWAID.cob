      *    ¼Ü¹ ²ÝÌß¯Ä         *     (170/3)
       01  SDI.
           02  SDI_PNAME1      PIC  X(008) VALUE "SIWAKE-I".
           02  F               PIC  X(001).
           02  SDI_LNAME       PIC  X(003) VALUE "SDI".
           02  F               PIC  X(001).
           02  SDI_KEY1        PIC  X(100) VALUE SPACE.
           02  SDI_KEY2        PIC  X(100) VALUE SPACE.
           02  SDI_KEY3        PIC  X(100) VALUE SPACE.
           02  SDI_SORT        PIC  X(100) VALUE SPACE.
           02  SDI_IDLST       PIC  X(100) VALUE SPACE.
           02  SDI_RES         USAGE  POINTER.
       01  SDI-REC.
           02  SDI-KEY.
             03  SDIYMD        PIC 9(8).
             03  SDIJNO        PIC 9(6).
             03  SDILNO        PIC 9(2).
           02  SDIKARI.
             03  KRCD.
               04  KRCDM       PIC 9(4).
               04  KRCDS       PIC 9(4).
             03  KRSECT        PIC 9(4).
             03  KRSKN         PIC 9(3).
             03  KRTAX         PIC X(1).
             03  KRKIN         PIC S9(10).
             03  KR-TB         PIC 9(2).
           02  SDIKASI.
             03  KSCD.
               04  KSCDM       PIC 9(4).
               04  KSCDS       PIC 9(4).
             03  KSSECT        PIC 9(4).
             03  KSSKN         PIC 9(3).
             03  KSTAX         PIC X(1).
             03  KSKIN         PIC S9(10).
             03  KS-TB         PIC 9(2).
           02  SDICUST         PIC 9(5).
           02  SDITEKICD       PIC 9(3).
           02  SDITEKI         PIC N(20).
           02  SDISIN          PIC X(1).
           02  SDINAMEN        PIC N(10).
           02  F               PIC X(10).
           02  SDIETAX         PIC X(1).
           02  FILLER          PIC X(17).
           02  SDIDEL          PIC X(1).
       77  F                   PIC X(1).

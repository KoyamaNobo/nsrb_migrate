      ****************************************************************
      *                                                              *
      *               < ÌÞÍÞÂ  ¾²¿Þ³¹ÞÝ¶ F >    * 85 REC / 3 B *     *
      *                                                              *
      ****************************************************************
       01  BUGEN-F.
           02  BUGEN-F_PNAME1      PIC  X(007) VALUE "BUGEN-K".
           02  F                   PIC  X(001).
           02  BUGEN-F_LNAME       PIC  X(007) VALUE "BUGEN-F".
           02  F                   PIC  X(001).
           02  BUGEN-F_KEY1        PIC  X(100) VALUE SPACE.
           02  BUGEN-F_KEY2        PIC  X(100) VALUE SPACE.
           02  BUGEN-F_SORT        PIC  X(100) VALUE SPACE.
           02  BUGEN-F_IDLST       PIC  X(100) VALUE SPACE.
           02  BUGEN-F_RES         USAGE  POINTER.
       01  BU-REC.
           02  BU-KEY.
               03  BU-BUMN.
                   04  BU-BUCD     PIC 9(02).                           ÌÞÓÝº°ÄÞ
                   04  BU-YOBI     PIC 9(02).                           ÖËÞ
               03  BU-LINNO        PIC 9(03).                           ×²ÝNO
           02      BU-KAIP         PIC 9(01).
           02      BU-GOKBN        PIC 9(01).
           02      BU-KMKNM        PIC N(10).
           02  BU-ZEN.
               03  BU-ZENKI        PIC S9(11).
               03  BU-TOUKI        PIC S9(11).
           02  BU-DOG.
               03  BU-DOGET        PIC S9(11).
               03  BU-TOGET        PIC S9(11).
           02      BU-URKBN        PIC X(01).
           02      BU-PRKBN        PIC 9(01).
           02      BU-TBKBN        PIC 9(01).
           02      F               PIC X(09).
       77  F                       PIC X(1).

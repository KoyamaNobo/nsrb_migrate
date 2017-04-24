      *     ¼Ü¹ ÃÞ°À          *     (128/2)
       01  SSD.
           02  SSD_PNAME1      PIC  X(008) VALUE "SIWAKE-D".
           02  F               PIC  X(001).
           02  SSD_LNAME       PIC  X(003) VALUE "SSD".
           02  F               PIC  X(001).
           02  SSD_KEY1        PIC  X(100) VALUE SPACE.
           02  SSD_SORT        PIC  X(100) VALUE SPACE.
           02  SSD_IDLST       PIC  X(100) VALUE SPACE.
           02  SSD_RES         USAGE  POINTER.
       01  SD-REC.
           02  KACD1.
             03  ACCNTCD       PIC 9(4).
             03  HOACCNT       PIC 9(4).
           02  TRDATE          PIC 9(8).
           02  JUNLNO          PIC 9(6).
           02  LINENO          PIC 9(2).
           02  DR-CR           PIC 9(1).
           02  SECTCD          PIC 9(4).
           02  SKINCD          PIC 9(3).
           02  TAXKB           PIC X(1).
           02  AMOUNT          PIC S9(10).
           02  TEG-BAN         PIC 9(2).
           02  KACD2.
             03  OPPCD         PIC 9(4).
             03  HOOPPCD       PIC 9(4).
           02  CUSTCD          PIC 9(5).
           02  TEKICD          PIC 9(3).
           02  TEKIYO          PIC N(20).
           02  KEIHIKB         PIC 9(1).
           02  NAMEN           PIC N(10).
           02  F               PIC X(4).
           02  ETAX            PIC X(1).
           02  DELKB           PIC X(1).
       77  F                   PIC X(1).

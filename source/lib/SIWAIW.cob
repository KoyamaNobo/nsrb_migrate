      *    仕訳インプット　ワーク   (192/4)
       01  SDW.
           02  SDW_PNAME1      PIC  X(009) VALUE "SIWAKE-IW".
           02  F               PIC  X(001).
           02  SDW_LNAME       PIC  X(003) VALUE "SDW".
           02  F               PIC  X(001).
           02  SDW_KEY1        PIC  X(100) VALUE SPACE.
           02  SDW_KEY2        PIC  X(100) VALUE SPACE.
           02  SDW_SORT        PIC  X(100) VALUE SPACE.
           02  SDW_IDLST       PIC  X(100) VALUE SPACE.
           02  SDW_RES         USAGE  POINTER.
       01  SDW-REC.
           02  SDWYMD          PIC 9(8).
           02  SDWYMD1  REDEFINES SDWYMD.
             03  SDWYY         PIC 9(4).
             03  SDWMD         PIC 9(4).
           02  SDWYMD2  REDEFINES SDWYMD.
             03  SDWYM         PIC 9(6).
             03  SDWDD         PIC 9(2).
           02  SDW-KEY.
             03  SDWJNO        PIC 9(6).
             03  SDWLNO        PIC 9(2).
           02  SDWKARI.
             03  KRCDW.
               04  KRCDMW      PIC 9(4).
               04  KRCDSW      PIC 9(4).
             03  KRSECTW       PIC 9(4).
             03  KRSKNW        PIC 9(3).
             03  KRTAXW        PIC X(1).
             03  KRKINW        PIC S9(10).
             03  KR-TBW        PIC 9(2).
           02  SDWKASI.
             03  KSCDW.
               04  KSCDMW      PIC 9(4).
               04  KSCDSW      PIC 9(4).
             03  KSSECTW       PIC 9(4).
             03  KSSKNW        PIC 9(3).
             03  KSTAXW        PIC X(1).
             03  KSKINW        PIC S9(10).
             03  KS-TBW        PIC 9(2).
           02  SDWCUST         PIC 9(5).
           02  SDWTEKICD       PIC 9(3).
           02  SDWTEKI         PIC N(20).
           02  SDWSIN          PIC X(1).
           02  SDWNAMEN        PIC N(10).
           02  SDWETAX         PIC X(01).
           02  SDWTENO         PIC 9(06).
           02  SDWTKD          PIC 9(08).
           02  SDWNKCD         PIC 9(02).
           02  SDWNSC          PIC 9(01).
           02  SDWSKNG         PIC 9(04).
           02  SDWSKNGR  REDEFINES SDWSKNG.
             03  SDWSKNR       PIC 9(02).
             03  SDWSKGR       PIC 9(02).
           02  SDWTCD          PIC 9(04).
           02  SDWSKD          PIC 9(08).
           02  FILLER          PIC X(15).
           02  SDWHHC          PIC 9(01).
           02  SDWZHC          PIC 9(01).
       77  F                   PIC X(01).

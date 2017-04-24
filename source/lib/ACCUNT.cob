      *    ¶Ó¸ Ï½À°           *
       01  AM.
           02  AM_PNAME1       PIC  X(008) VALUE "KAMOKU-K".
           02  F               PIC  X(001).
           02  AM_LNAME        PIC  X(002) VALUE "AM".
           02  F               PIC  X(001).
           02  AM_KEY1         PIC  X(100) VALUE SPACE.
           02  AM_SORT         PIC  X(100) VALUE SPACE.
           02  AM_IDLST        PIC  X(100) VALUE SPACE.
           02  AM_RES          USAGE  POINTER.
       01  AM-REC.
           02  AM-KEY.
             03  ACCTCD1       PIC 9.
             03  ACCTCD2       PIC 9.
             03  ACCTCD3       PIC 9.
             03  ACCTCD4       PIC 9.
           02  DR-CR           PIC 9.
           02  DDRCR.
             03  BFDZN         PIC S9(11).
             03  DDR           PIC S9(11).
             03  DCR           PIC S9(11).
           02  TEG-BAN         PIC 99.
           02  TANA            PIC 9.
           02  HOJYO           PIC 9.
           02  BS-PL           PIC 9.
           02  BSKOU  OCCURS   6   TIMES.
             03  BSGOU.
               04  BSKEY       PIC 9(3).
               04  BSDR-CR     PIC 9.
               04  BSCOM       PIC 9.
           02  PLKOU  OCCURS   12  TIMES.
             03  PLGOU.
               04  PLKEY       PIC 9(3).
               04  PLCOM       PIC 9.
           02  KEIHI           PIC 9.
           02  MOTKB           PIC 9.
           02  GNKOU  OCCURS   12  TIMES.
             03  GNGOU.
               04  GNKEY       PIC 9(3).
               04  GNCOM       PIC 9.
           02  SKNKOU.
             03  SKNKEY        PIC 9(3).
             03  SKNCOM        PIC 9(1).
             03  SKNHAT        PIC 9(1).
           02  FILLER          PIC X(71).
           02  AM-ZAN          PIC 9(1).
           02  AM-KAF          PIC 9(4).
           02  AM-KAR          PIC 9(4).
       77  F                   PIC X(1).

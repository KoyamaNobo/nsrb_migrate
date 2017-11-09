       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHD530.
      *********************************************************
      *    PROGRAM         :  日計更新　　累計　　　　　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  NO                              *
      *        変更　　　  :  62/04/01                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  WM-DATE.
             03  WM-KUD       PIC  9(008).
             03  WM-KUDD  REDEFINES WM-KUD.
               04  WM-KUDN    PIC  9(004).
               04  WM-KUDNL REDEFINES WM-KUDN.
                 05  WM-KUDN1 PIC  9(002).
                 05  WM-KUDN2 PIC  9(002).
               04  WM-KUDGP   PIC  9(004).
             03  WM-KUDL  REDEFINES WM-KUD.
               04  F          PIC  9(002).
               04  WM-KUDS    PIC  9(006).
             03  WM-KKD       PIC  9(008).
             03  WM-KKDD  REDEFINES WM-KKD.
               04  WM-KKDN    PIC  9(004).
               04  WM-KKDNL REDEFINES WM-KKDN.
                 05  WM-KKDN1 PIC  9(002).
                 05  WM-KKDN2 PIC  9(002).
               04  WM-KKDGP   PIC  9(004).
             03  WM-KKDL  REDEFINES WM-KKD.
               04  F          PIC  9(002).
               04  WM-KKDS    PIC  9(006).
             03  WM-KSD       PIC  9(008).
             03  WM-KSDD  REDEFINES WM-KSD.
               04  WM-KSDN    PIC  9(004).
               04  WM-KSDNL REDEFINES WM-KSDN.
                 05  WM-KSDN1 PIC  9(002).
                 05  WM-KSDN2 PIC  9(002).
               04  WM-KSDGP   PIC  9(004).
             03  WM-KSDL  REDEFINES WM-KSD.
               04  F          PIC  9(002).
               04  WM-KSDS    PIC  9(006).
           02  W-DATE.
             03  W-KUD        PIC  9(008).
             03  W-KUDL  REDEFINES W-KUD.
               04  F          PIC  9(002).
               04  W-KUDS     PIC  9(006).
             03  W-KKD        PIC  9(008).
             03  W-KKDL  REDEFINES W-KKD.
               04  F          PIC  9(002).
               04  W-KKDS     PIC  9(006).
             03  W-KSD        PIC  9(008).
             03  W-KSDL  REDEFINES W-KSD.
               04  F          PIC  9(002).
               04  W-KSDS     PIC  9(006).
           02  W-OC           PIC  9(001) VALUE 0.
           02  W-FILE         PIC  X(013).
           02  W-DC           PIC  9(001).
           02  W-KEY.
             03  F            PIC  X(013).
             03  W-DNO        PIC  9(006).
             03  F            PIC  X(001).
       01  ERR-STAT           PIC  X(002).
           COPY LDAIW.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHKBM.
           COPY LITUKF.
           COPY LISKDF.
      *FD  URI-F
       01  URI-F_KHD530.
           02  URI-F_PNAME1   PIC  X(004) VALUE "URIF".
           02  F              PIC  X(001).
           02  URI-F_LNAME    PIC  X(012) VALUE "URI-F_KHD530".
           02  F              PIC  X(001).
           02  URI-F_KEY1     PIC  X(100) VALUE SPACE.
           02  URI-F_SORT     PIC  X(100) VALUE SPACE.
           02  URI-F_IDLST    PIC  X(100) VALUE SPACE.
           02  URI-F_RES      USAGE  POINTER.
       01  URI-R.
           02  U-DC           PIC  9(001).
           02  U-DATE         PIC  9(008).
           02  U-NGP   REDEFINES U-DATE.
             03  U-NG         PIC  9(006).
             03  F            PIC  9(002).
           02  U-TCD          PIC  9(004).
           02  U-HCD          PIC  X(005).
           02  U-SU           PIC S9(006)V9(02).
           02  U-T            PIC S9(006)V9(02).
           02  U-KIN          PIC S9(008).
           02  U-YC           PIC  9(002).
           02  U-SD           PIC  9(004).
           02  U-NNO          PIC  X(006).
           02  U-HYC          PIC  9(001).
           02  U-CSC          PIC  9(001).
           02  U-BKC          PIC  9(002).
           02  U-SKD          PIC  9(008).
           02  U-DNO          PIC  9(006).
           02  U-GNO          PIC  9(001).
           02  U-JCD          PIC  9(006).
           02  U-GT           PIC S9(006)V9(02).
           02  U-TEK          PIC  N(018).
           02  F              PIC  X(005).
       77  F                  PIC  X(001).
      *FD  KNH-F
       01  KNH-F_KHD530.
           02  KNH-F_PNAME1   PIC  X(004) VALUE "KNHF".
           02  F              PIC  X(001).
           02  KNH-F_LNAME    PIC  X(012) VALUE "KNH-F_KHD530".
           02  F              PIC  X(001).
           02  KNH-F_KEY1     PIC  X(100) VALUE SPACE.
           02  KNH-F_SORT     PIC  X(100) VALUE SPACE.
           02  KNH-F_IDLST    PIC  X(100) VALUE SPACE.
           02  KNH-F_RES      USAGE  POINTER.
       01  KNH-R.
           02  NH-NHC         PIC  9(002).
           02  NH-DATE        PIC  9(008).
           02  NH-HCD         PIC  X(005).
           02  F              PIC  X(007).
           02  NH-SU          PIC S9(006)V9(02).
           02  F              PIC  X(016).
           02  NH-YC          PIC  9(002).
           02  F              PIC  X(016).
       77  F                  PIC  X(001).
      *FD  URIR-F
       01  URIR-F_KHD530.
           02  URIR-F_PNAME1  PIC  X(005) VALUE "URIRF".
           02  F              PIC  X(001).
           02  URIR-F_LNAME   PIC  X(013) VALUE "URIR-F_KHD530".
           02  F              PIC  X(001).
           02  URIR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  URIR-F_SORT    PIC  X(100) VALUE SPACE.
           02  URIR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  URIR-F_RES     USAGE  POINTER.
       01  URIR-R             PIC  X(128).
       77  F                  PIC  X(001).
      *FD  KNHR-F
       01  KNHR-F_KHD530.
           02  KNHR-F_PNAME1  PIC  X(005) VALUE "KNHRF".
           02  F              PIC  X(001).
           02  KNHR-F_LNAME   PIC  X(013) VALUE "KNHR-F_KHD530".
           02  F              PIC  X(001).
           02  KNHR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  KNHR-F_SORT    PIC  X(100) VALUE SPACE.
           02  KNHR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  KNHR-F_RES     USAGE  POINTER.
       01  KNHR-R             PIC X(064).
       77  F                  PIC  X(001).
      *FD  URIRYR
       01  URIRYR_KHD530.
           02  URIRYR_PNAME1  PIC  X(006) VALUE "URIRYR".
           02  F              PIC  X(001).
           02  URIRYR_LNAME   PIC  X(013) VALUE "URIRYR_KHD530".
           02  F              PIC  X(001).
           02  URIRYR_KEY1    PIC  X(100) VALUE SPACE.
           02  URIRYR_SORT    PIC  X(100) VALUE SPACE.
           02  URIRYR_IDLST   PIC  X(100) VALUE SPACE.
           02  URIRYR_RES     USAGE  POINTER.
       01  URIRY-R            PIC  X(128).
       77  F                  PIC  X(001).
      *FD  KNHRYR
       01  KNHRYR_KHD530.
           02  KNHRYR_PNAME1  PIC  X(006) VALUE "KNHRYR".
           02  F              PIC  X(001).
           02  KNHRYR_LNAME   PIC  X(013) VALUE "KNHRYR_KHD530".
           02  F              PIC  X(001).
           02  KNHRYR_KEY1    PIC  X(100) VALUE SPACE.
           02  KNHRYR_SORT    PIC  X(100) VALUE SPACE.
           02  KNHRYR_IDLST   PIC  X(100) VALUE SPACE.
           02  KNHRYR_RES     USAGE  POINTER.
       01  KNHRY-R            PIC  X(064).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-AMID.
           02  C-MID.
             03  FILLER  PIC  N(016) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
             03  FILLER  PIC  N(016) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
             03  FILLER  PIC  N(016) VALUE
                  "＊＊＊　　　　　　　　　　＊＊＊".
             03  FILLER  PIC  N(016) VALUE
                  "＊＊＊　　　　　　　　　　＊＊＊".
             03  FILLER  PIC  N(016) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
             03  FILLER  PIC  N(016) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER.
             03  C-MID2  PIC  N(016) VALUE
                  "＊＊＊　売上・値引　累積　＊＊＊".
             03  C-MID4  PIC  N(016) VALUE
                  "＊＊＊　加硫・廃却　累積　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME3   PIC  X(018) VALUE
                  "***  DATEM ﾅｼ  ***".
             03  E-ME4   PIC  X(027) VALUE
                  "***  DATEM REWRITE ｴﾗｰ  ***".
             03  E-ME5   PIC  X(030) VALUE
                  "***  URIR-F WRITE ｴﾗｰ  ***    ".
             03  E-ME6   PIC  X(030) VALUE
                  "***  NYUR-F WRITE ｴﾗｰ  ***    ".
             03  E-ME8   PIC  X(030) VALUE
                  "***  KNHR-F WRITE ｴﾗｰ  ***    ".
             03  E-ME11  PIC  X(030) VALUE
                  "***  TM REWRITE ｴﾗｰ  ***      ".
             03  E-ME15  PIC  X(030) VALUE
                  "***  NYUF REWRITE ｴﾗｰ  ***    ".
             03  E-ME21  PIC  X(030) VALUE
                  "***  URIRYR WRITE ｴﾗｰ  ***    ".
             03  E-ME22  PIC  X(030) VALUE
                  "***  KNHRYR WRITE ｴﾗｰ  ***    ".
             03  E-ME25  PIC  X(026) VALUE
                  "***  NYURYR WRITE ｴﾗｰ  ***".
             03  E-ME26  PIC  X(024) VALUE
                  "***  SKDF WRITE ｴﾗｰ  ***".
             03  E-ME27  PIC  X(026) VALUE
                  "***  SKDF REWRITE ｴﾗｰ  ***".
             03  E-ME28.
               04  FILLER    PIC  X(039) VALUE
                    "***  ﾃﾞﾝﾋﾟｮｳNO ｴﾗｰ (      ･      )  ***".
               04  02E-ME28  PIC  9(006).
               04  03E-ME28  PIC  9(006).
             03  E-ME31  PIC X(17) VALUE
                  "***  HKBM ﾅｼ  ***".
             03  E-ME32  PIC X(26) VALUE
                  "***  HKBM REWRITE ｴﾗｰ  ***".
             03  E-ME33  PIC X(24) VALUE
                  "***  HKBM WRITE ｴﾗｰ  ***".
             03  E-ME35  PIC  X(026) VALUE
                  "***   TUKF WRITE ｴﾗｰ   ***".
             03  E-ME50  PIC  X(026) VALUE
                  "***   ﾀﾞｲﾁｮｳNO ｵｰﾊﾞｰ   ***".
           COPY LSSEM.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-AMID
       CALL "SD_Init" USING 
            "C-AMID" " " "0" "0" "256" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "192" " " "C-AMID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "32" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "32" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "32" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "7" "10" "32" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "8" "10" "32" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "9" "10" "32" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-AMID" " " "6" "0" "64" "C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "C-MID2" "N" "6" "10" "32" " " "02C-AMID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "C-MID4" "N" "6" "10" "32" "C-MID2" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "415" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "415" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "27" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "30" "E-ME4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "30" "E-ME5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "30" "E-ME8" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME21" "X" "24" "15" "30" "E-ME11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME22" "X" "24" "15" "30" "E-ME21" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME26" "X" "24" "15" "24" "E-ME22" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME27" "X" "24" "15" "26" "E-ME26" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME28" " " "24" "0" "51" "E-ME27" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME28" "X" "0" "15" "39" " " "E-ME28"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME28" "9" "0" "35" "6" "01E-ME28" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME28" BY REFERENCE U-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03E-ME28" "9" "0" "42" "6" "02E-ME28" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03E-ME28" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME31" "X" "24" "15" "17" "E-ME28" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME32" "X" "24" "15" "26" "E-ME31" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME33" "X" "24" "15" "24" "E-ME32" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME35" "X" "24" "15" "26" "E-ME33" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME50" "X" "24" "15" "26" "E-ME35" " "  RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATE.
           CALL "DB_F_Open" USING
            "INPUT" URI-F_PNAME1 " " BY REFERENCE URI-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" KNH-F_PNAME1 " " BY REFERENCE KNH-F_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" URIR-F_PNAME1 " " BY REFERENCE URIR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" KNHR-F_PNAME1 " " BY REFERENCE KNHR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" URIRYR_PNAME1 " " BY REFERENCE URIRYR_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" KNHRYR_PNAME1 " " BY REFERENCE KNHRYR_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "I-O" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
      *
           CALL "SD_Output" USING "C-MID2" C-MID2 "p" RETURNING RESU.
           MOVE 1 TO W-DC.
       M-320.
      *           READ URI-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URI-F_PNAME1 BY REFERENCE URI-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-400
           END-IF
           IF  W-OC = 0
               MOVE 1 TO W-OC
               CALL "DB_F_Open" USING
                "I-O" TUKF_PNAME1 "SHARED" BY REFERENCE TUKF_IDLST "2"
                "TUK-KEY" BY REFERENCE TUK-KEY "TUK-KEY2" BY REFERENCE
                TUK-KEY2
           END-IF.
       M-340.
           MOVE ZERO TO URIR-R.
           MOVE URI-R TO URIR-R.
      *           WRITE URIR-R.
      *//////////////
           CALL "DB_Insert" USING
            URIR-F_PNAME1 URIR-F_LNAME URIR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-360
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME5" E-ME5 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE TUKF_IDLST TUKF_PNAME1
               GO TO M-980
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE URIR-F_IDLST URIR-F_PNAME1.
           MOVE "URIRF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" URIR-F_PNAME1 " " BY REFERENCE URIR-F_IDLST "0".
           GO TO M-340.
       M-360.
           IF  U-DATE > W-KUD
               MOVE U-DATE TO W-KUD
           END-IF
      *
           MOVE U-TCD TO T-KEY.
      *           READ T-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R " " RETURNING RET.
           IF  RET = 1
               MOVE 1 TO T-BC
               MOVE 90 TO T-TNC
               MOVE ZERO TO T-DCC
               GO TO M-380
           END-IF
           IF  U-NG NOT > T-DNG
               GO TO M-380
           END-IF
           MOVE U-NG TO T-DNG.
           IF  T-ENG NOT = ZERO
               MOVE ZERO TO T-ENG
           END-IF
      *           REWRITE T-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            T-M_PNAME1 T-M_LNAME T-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       M-380.
           PERFORM URI-RTN THRU URI-EX.
           IF  COMPLETION_CODE = 255
               CALL "DB_F_Close" USING
                BY REFERENCE TUKF_IDLST TUKF_PNAME1
               GO TO M-980
           END-IF
           PERFORM HKB-RTN THRU HKB-EX.
           IF  COMPLETION_CODE = 255
               CALL "DB_F_Close" USING
                BY REFERENCE TUKF_IDLST TUKF_PNAME1
               GO TO M-980
           END-IF
           PERFORM TUK-RTN THRU TUK-EX.
           IF  COMPLETION_CODE = 255
               CALL "DB_F_Close" USING
                BY REFERENCE TUKF_IDLST TUKF_PNAME1
               GO TO M-980
           END-IF
           IF  U-GNO = 9
               GO TO M-390
           END-IF
           PERFORM SKDW-RTN THRU SKDW-EX.
           IF  COMPLETION_CODE = 255
               CALL "DB_F_Close" USING
                BY REFERENCE TUKF_IDLST TUKF_PNAME1
               GO TO M-980
           END-IF
           GO TO M-320.
       M-390.
           IF  U-DNO NOT = W-DNO
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME28" E-ME28 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE TUKF_IDLST TUKF_PNAME1
               GO TO M-980
           END-IF
           MOVE W-KEY TO SKD-KEY.
      *           READ SKDF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" SKDF_PNAME1 BY REFERENCE SKD-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME28" E-ME28 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE TUKF_IDLST TUKF_PNAME1
               GO TO M-980
           END-IF.
       M-395.
           PERFORM SKDR-RTN THRU SKDR-EX.
           IF  COMPLETION_CODE = 255
               CALL "DB_F_Close" USING
                BY REFERENCE TUKF_IDLST TUKF_PNAME1
               GO TO M-980
           END-IF
      *
      *           READ SKDF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-320
           END-IF
           IF  SKD-DNO = W-DNO
               GO TO M-395
           END-IF
           GO TO M-320.
       M-400.
           IF  W-OC = 1
               MOVE 0 TO W-OC
               CALL "DB_F_Close" USING
                BY REFERENCE TUKF_IDLST TUKF_PNAME1
           END-IF
           CALL "SD_Output" USING "C-MID4" C-MID4 "p" RETURNING RESU.
       M-420.
      *           READ KNH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KNH-F_PNAME1 BY REFERENCE KNH-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-500
           END-IF.
       M-440.
           MOVE ZERO TO KNHR-R.
           MOVE KNH-R TO KNHR-R.
      *           WRITE KNHR-R.
      *//////////////
           CALL "DB_Insert" USING
            KNHR-F_PNAME1 KNHR-F_LNAME KNHR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-460
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME8" E-ME8 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-980
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE KNHR-F_IDLST KNHR-F_PNAME1.
           MOVE "KNHRF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" KNHR-F_PNAME1 " " BY REFERENCE KNHR-F_IDLST "0".
           GO TO M-440.
       M-460.
           IF  NH-DATE > W-KKD
               MOVE NH-DATE TO W-KKD
           END-IF
           PERFORM KNH-RTN THRU KNH-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-980
           END-IF
           GO TO M-420.
       M-500.
       M-800.
           CALL "DB_F_Open" USING
            "I-O" M-DATE_PNAME1 " " BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
           MOVE "01" TO DATE-KEY.
      *           READ M-DATE INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" M-DATE_PNAME1 BY REFERENCE DATE-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE M-DATE_IDLST M-DATE_PNAME1
               GO TO M-980
           END-IF
           MOVE ZERO TO WM-DATE.
           MOVE D-KUD TO WM-KUDS.
           MOVE D-KKD TO WM-KKDS.
           MOVE D-KSD TO WM-KSDS.
           IF  WM-KUDS NOT = ZERO
               IF  WM-KUDN2 >= DATE-NF1 AND <= DATE-NT1
                   ADD DATE-NC1 TO WM-KUDN
               END-IF
           END-IF
           IF  WM-KUDS NOT = ZERO
               IF  WM-KUDN2 >= DATE-NF2 AND <= DATE-NT2
                   ADD DATE-NC2 TO WM-KUDN
               END-IF
           END-IF
           IF  WM-KKDS NOT = ZERO
               IF  WM-KKDN2 >= DATE-NF1 AND <= DATE-NT1
                   ADD DATE-NC1 TO WM-KKDN
               END-IF
           END-IF
           IF  WM-KKDS NOT = ZERO
               IF  WM-KKDN2 >= DATE-NF2 AND <= DATE-NT2
                   ADD DATE-NC2 TO WM-KKDN
               END-IF
           END-IF
           IF  WM-KSDS NOT = ZERO
               IF  WM-KSDN2 >= DATE-NF1 AND <= DATE-NT1
                   ADD DATE-NC1 TO WM-KSDN
               END-IF
           END-IF
           IF  WM-KSDS NOT = ZERO
               IF  WM-KSDN2 >= DATE-NF2 AND <= DATE-NT2
                   ADD DATE-NC2 TO WM-KSDN
               END-IF
           END-IF
           IF  W-KUD > WM-KUD
               MOVE W-KUDS TO D-KUD
           END-IF
           IF  W-KKD > WM-KKD
               MOVE W-KKDS TO D-KKD
           END-IF
           IF  W-KSD > WM-KSD
               MOVE W-KSDS TO D-KSD
           END-IF
      *           REWRITE DATE-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            M-DATE_PNAME1 M-DATE_LNAME DATE-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
       M-980.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE URI-F_IDLST URI-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KNH-F_IDLST KNH-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE URIR-F_IDLST URIR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KNHR-F_IDLST KNHR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE URIRYR_IDLST URIRYR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KNHRYR_IDLST KNHRYR_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       URI-RTN.
           MOVE ZERO TO URIRY-R.
           MOVE URI-R TO URIRY-R.
      *           WRITE URIRY-R.
      *//////////////
           CALL "DB_Insert" USING
            URIRYR_PNAME1 URIRYR_LNAME URIRY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO URI-EX
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME21" E-ME21 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO URI-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE URIRYR_IDLST URIRYR_PNAME1.
           MOVE "URIRYR       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" URIRYR_PNAME1 " " BY REFERENCE URIRYR_IDLST "0".
           GO TO URI-RTN.
       URI-EX.
           EXIT.
       KNH-RTN.
           MOVE ZERO TO KNHRY-R.
           MOVE KNH-R TO KNHRY-R.
      *           WRITE KNHRY-R.
      *//////////////
           CALL "DB_Insert" USING
            KNHRYR_PNAME1 KNHRYR_LNAME KNHRY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO KNH-EX
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME22" E-ME22 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO KNH-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE KNHRYR_IDLST KNHRYR_PNAME1.
           MOVE "KNHRYR       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" KNHRYR_PNAME1 " " BY REFERENCE KNHRYR_IDLST "0".
           GO TO KNH-RTN.
       KNH-EX.
           EXIT.
       HKB-RTN.
           CALL "DB_F_Open" USING
            "I-O" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
      *           READ HKBM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME31" E-ME31 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO HKB-010
           END-IF
           MOVE HKB-DAI TO W-DAI.
           GO TO HKB-EX.
       HKB-010.
           INITIALIZE HKB-R.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
      *           WRITE HKB-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME33" E-ME33 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO HKB-EX
           END-IF
           MOVE HKB-DAI TO W-DAI.
       HKB-EX.
           EXIT.
       TUK-RTN.
           COPY LDAIP.
           IF  W-DAI1 = "999"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME50" E-ME50 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           MOVE W-DAI TO HKB-DAI.
      *           REWRITE HKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME32" E-ME32 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
       TUK-010.
           INITIALIZE TUK-R.
           MOVE U-TCD TO TUK-TCD TUK-TCD2.
           MOVE W-DAI TO TUK-DAI.
           MOVE U-DATE TO TUK-DATE.
           IF  U-DC < 8
               MOVE 1 TO TUK-DC
           ELSE
               MOVE 2 TO TUK-DC
           END-IF
           IF  U-DC NOT = 5 AND 9
               MOVE U-KIN TO TUK-KIN
           ELSE
               MOVE U-KIN TO TUK-SHZ
           END-IF
           MOVE U-DNO TO TUK-DNO.
           MOVE U-GNO TO TUK-GNO.
           MOVE TUK-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO T-TNC T-DCC
           END-IF
           MOVE T-TNC TO TUK-TNC.
           MOVE T-DCC TO TUK-DCC.
           MOVE 1 TO TUK-BMC.
      *           WRITE TUK-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TUKF_PNAME1 TUKF_LNAME TUK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME35" E-ME35 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TUK-040
           END-IF
           GO TO TUK-EX.
       TUK-040.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TUK-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE TUKF_IDLST TUKF_PNAME1.
           MOVE "TUKF         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TUKF_PNAME1 "SHARED" BY REFERENCE TUKF_IDLST "2"
            "TUK-KEY" BY REFERENCE TUK-KEY "TUK-KEY2" BY REFERENCE
            TUK-KEY2.
           GO TO TUK-010.
       TUK-EX.
           EXIT.
       SKDW-RTN.
           INITIALIZE SKD-R.
           MOVE ZERO TO SKD-HNO.
           MOVE SPACE TO SKD-BI SKD-HCDD.
           MOVE U-TCD TO SKD-TCD.
           MOVE U-DATE TO SKD-DATE.
           IF  U-DC = 0 OR 1 OR 2 OR 3 OR 4
               MOVE 0 TO SKD-DTC
               MOVE U-KIN TO SKD-KIN
           END-IF
           IF  U-DC = 5
               MOVE 0 TO SKD-DTC
               MOVE U-KIN TO SKD-SHZ
           END-IF
           IF  U-DC = 8
               MOVE 1 TO SKD-DTC
               MOVE U-KIN TO SKD-KIN
           END-IF
           IF  U-DC = 9
               MOVE 1 TO SKD-DTC
               MOVE U-KIN TO SKD-SHZ
           END-IF
           MOVE U-DNO TO SKD-DNO.
           MOVE U-GNO TO SKD-GNO.
           MOVE U-HCD TO SKD-KCD.
           MOVE U-SU TO SKD-SU.
           MOVE U-T TO SKD-T.
           MOVE U-DC TO SKD-DC.
           IF  U-DC = 8
               MOVE 0 TO SKD-DC
           END-IF
           MOVE U-CSC TO SKD-CSC.
           MOVE U-SKD TO SKD-SKD.
           MOVE U-JCD TO SKD-JCD.
           MOVE U-TEK TO SKD-BI.
           MOVE T-TNC TO SKD-TNC.
           MOVE T-BC TO SKD-BMC.
           MOVE T-DCC TO SKD-DCC.
       SKDW-010.
      *           WRITE SKD-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            SKDF_PNAME1 SKDF_LNAME SKD-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME26" E-ME26 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SKDW-020
           END-IF
           IF  SKD-DNO NOT = W-DNO
               MOVE SKD-KEY TO W-KEY.
           GO TO SKDW-EX.
       SKDW-020.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SKDW-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           MOVE "SKDF         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
           GO TO SKDW-RTN.
       SKDW-EX.
           EXIT.
       SKDR-RTN.
           MOVE U-KIN TO SKD-SHZ.
      *           REWRITE SKD-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SKDF_PNAME1 SKDF_LNAME SKD-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME27" E-ME27 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       SKDR-EX.
           EXIT.

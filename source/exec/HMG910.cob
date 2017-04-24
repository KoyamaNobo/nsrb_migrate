       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG910.
      *********************************************************
      *    PROGRAM         :  月次更新　ＣＨＥＣＫ            *
      *    PRINTER TYPE    :  *****                           *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  00/03/22                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-NDC          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-NGD.
             03  W-NEND       PIC  9(002).
             03  W-GETD       PIC  9(002).
           02  W-NG.
             03  W-NG1        PIC  9(006).
             03  W-NG2        PIC  9(006).
             03  W-NG3        PIC  9(006).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKKBM.
      *FD  STRAN
       01  STRAN_HMG910.
           02  STRAN_PNAME1   PIC  X(005) VALUE "STRAN".
           02  F              PIC  X(001).
           02  STRAN_LNAME    PIC  X(012) VALUE "STRAN_HMG910".
           02  F              PIC  X(001).
           02  STRAN_KEY1     PIC  X(100) VALUE SPACE.
           02  STRAN_KEY2     PIC  X(100) VALUE SPACE.
           02  STRAN_SORT     PIC  X(100) VALUE SPACE.
           02  STRAN_IDLST    PIC  X(100) VALUE SPACE.
           02  STRAN_RES      USAGE  POINTER.
       01  STRAN-R            PIC  X(128).
       77  F                  PIC  X(001).
      *FD  NYU-F
       01  NYU-F_HMG910.
           02  NYU-F_PNAME1   PIC  X(004) VALUE "NYUF".
           02  F              PIC  X(001).
           02  NYU-F_LNAME    PIC  X(012) VALUE "NYU-F_HMG910".
           02  F              PIC  X(001).
           02  NYU-F_KEY1     PIC  X(100) VALUE SPACE.
           02  NYU-F_SORT     PIC  X(100) VALUE SPACE.
           02  NYU-F_IDLST    PIC  X(100) VALUE SPACE.
           02  NYU-F_RES      USAGE  POINTER.
       01  NYU-R.
           02  F              PIC  X(043).
           02  NYU-KEY        PIC  X(007).
           02  F              PIC  X(052).
       77  F                  PIC  X(001).
      *FD  UTRAN
       01  UTRAN_HMG910.
           02  UTRAN_PNAME1   PIC  X(005) VALUE "UTRAN".
           02  F              PIC  X(001).
           02  UTRAN_LNAME    PIC  X(012) VALUE "UTRAN_HMG910".
           02  F              PIC  X(001).
           02  UTRAN_KEY1     PIC  X(100) VALUE SPACE.
           02  UTRAN_KEY2     PIC  X(100) VALUE SPACE.
           02  UTRAN_SORT     PIC  X(100) VALUE SPACE.
           02  UTRAN_IDLST    PIC  X(100) VALUE SPACE.
           02  UTRAN_RES      USAGE  POINTER.
       01  UTRAN-R            PIC  X(128).
       77  F                  PIC  X(001).
      *FD  URI-F
       01  URI-F_HMG910.
           02  URI-F_PNAME1   PIC  X(004) VALUE "URIF".
           02  F              PIC  X(001).
           02  URI-F_LNAME    PIC  X(012) VALUE "URI-F_HMG910".
           02  F              PIC  X(001).
           02  URI-F_KEY1     PIC  X(100) VALUE SPACE.
           02  URI-F_SORT     PIC  X(100) VALUE SPACE.
           02  URI-F_IDLST    PIC  X(100) VALUE SPACE.
           02  URI-F_RES      USAGE  POINTER.
       01  URI-R              PIC  X(128).
       77  F                  PIC  X(001).
      *FD  KNH-F
       01  KNH-F_HMG910.
           02  KNH-F_PNAME1   PIC  X(004) VALUE "KNHF".
           02  F              PIC  X(001).
           02  KNH-F_LNAME    PIC  X(012) VALUE "KNH-F_HMG910".
           02  F              PIC  X(001).
           02  KNH-F_KEY1     PIC  X(100) VALUE SPACE.
           02  KNH-F_SORT     PIC  X(100) VALUE SPACE.
           02  KNH-F_IDLST    PIC  X(100) VALUE SPACE.
           02  KNH-F_RES      USAGE  POINTER.
       01  KNH-R              PIC  X(064).
       77  F                  PIC  X(001).
      *FD  HKSR-F
       01  HKSR-F_HMG910.
           02  HKSR-F_PNAME1  PIC  X(005) VALUE "HKSRF".
           02  F              PIC  X(001).
           02  HKSR-F_LNAME   PIC  X(013) VALUE "HKSR-F_HMG910".
           02  F              PIC  X(001).
           02  HKSR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  HKSR-F_SORT    PIC  X(100) VALUE SPACE.
           02  HKSR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  HKSR-F_RES     USAGE  POINTER.
       01  HKSR-R.
           02  F              PIC  X(026).
           02  HKSR-NG        PIC  9(006).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　履物・工品他　月次更新　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(024) VALUE
                "【    '  年   月分    】".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NGD.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
           02  FILLER.
             03  D-SME01 PIC  N(017) VALUE
                  "＊＊＊　　他で売上入力中　　＊＊＊".
             03  D-SME02 PIC  N(017) VALUE
                  "＊＊＊　　他で売上変換中　　＊＊＊".
             03  D-SME03 PIC  N(017) VALUE
                  "＊＊＊　　他で値引入力中　　＊＊＊".
             03  D-SME04 PIC  N(017) VALUE
                  "＊＊＊　　他で入金入力中　　＊＊＊".
             03  D-SME05 PIC  N(017) VALUE
                  "＊＊＊　　他で加硫入力中　　＊＊＊".
             03  D-SME06 PIC  N(017) VALUE
                  "＊＊＊　　他で廃却入力中　　＊＊＊".
             03  D-SME13 PIC  N(017) VALUE
                  "＊＊＊　　他で日次更新中　　＊＊＊".
             03  D-SME14 PIC  N(017) VALUE
                  "＊＊＊　　他で手配変換中　　＊＊＊".
             03  D-SME15 PIC  N(017) VALUE
                  "＊＊＊　　他で月次更新中　　＊＊＊".
             03  D-SME99 PIC  N(015) VALUE
                  "＊＊＊　　他で使用中　　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  N(016) VALUE
                  "入力データ有り　処理を中止します".
             03  E-ME2   PIC  N(016) VALUE
                  "教育振興会をクリアして下さい".
             03  E-ME5   PIC  X(017) VALUE
                  "***  KKBM ﾅｼ  ***".
             03  E-ME6   PIC  X(026) VALUE
                  "***  KKBM REWRITE ｴﾗ-  ***".
           COPY LIBSCR.
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
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "340" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "42" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "12" "17" "24" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "20" "18" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "35" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "340" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NGD" " " "12" "0" "4" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NGD" "9" "12" "24" "2" " " "D-NGD" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NGD" BY REFERENCE W-NEND "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NGD" "9" "12" "29" "2" "01D-NGD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NGD" BY REFERENCE W-GETD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "15" "0" "336" "D-NGD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME01" "bN" "15" "18" "34" " " "02C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME02" "bN" "15" "18" "34" "D-SME01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME03" "bN" "15" "18" "34" "D-SME02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME04" "bN" "15" "18" "34" "D-SME03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME05" "bN" "15" "18" "34" "D-SME04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME06" "bN" "15" "18" "34" "D-SME05" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME13" "bN" "15" "18" "34" "D-SME06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME14" "bN" "15" "18" "34" "D-SME13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME15" "bN" "15" "18" "34" "D-SME14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME99" "bN" "15" "18" "30" "D-SME15" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "107" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "107" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "N" "24" "15" "32" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "N" "24" "15" "32" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME5" "X" "24" "15" "17" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME6" "X" "24" "15" "26" "E-ME5" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
      *
           COPY LIBCPR.
           MOVE D-NHNG TO W-NGD.
           CALL "SD_Output" USING "D-NGD" D-NGD "p" RETURNING RESU.
      *
           PERFORM CHK-RTN THRU CHK-EX.
           IF  W-NDC = 1
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  W-NDC = 2
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           MOVE SPACE TO KKB-KEY.
           MOVE 90 TO KKB-NO.
      *           READ KKB-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  KKB-SC15 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME15" D-SME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  KKB-SC14 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME14" D-SME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  1 = KKB-SC13 OR KKB-SC12
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME13" D-SME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  1 = KKB-SC11 OR KKB-SC10 OR KKB-SC09
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME99" D-SME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  KKB-SC06 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME06" D-SME06 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  KKB-SC05 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME05" D-SME05 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  KKB-SC04 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME04" D-SME04 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  KKB-SC03 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME03" D-SME03 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  KKB-SC02 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME02" D-SME02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  KKB-SC01 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME01" D-SME01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
      *
           MOVE 1 TO KKB-SC15.
      *           REWRITE KKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KKB-M_PNAME1 KKB-M_LNAME KKB-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       CHK-RTN.
           MOVE 0 TO W-NDC.
      *
           CALL "DB_F_Open" USING
            "INPUT" STRAN_PNAME1 " " BY REFERENCE STRAN_IDLST "0".
      *           READ STRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" STRAN_PNAME1 BY REFERENCE STRAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO CHK-010
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE STRAN_IDLST STRAN_PNAME1.
           MOVE 1 TO W-NDC.
           GO TO CHK-EX.
       CHK-010.
           CALL "DB_F_Close" USING
            BY REFERENCE STRAN_IDLST STRAN_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" NYU-F_PNAME1 " " BY REFERENCE NYU-F_IDLST "1"
            "NYU-KEY" BY REFERENCE NYU-KEY.
      *           READ NYU-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NYU-F_PNAME1 BY REFERENCE NYU-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO CHK-030
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
           MOVE 1 TO W-NDC.
           GO TO CHK-EX.
       CHK-030.
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" UTRAN_PNAME1 " " BY REFERENCE UTRAN_IDLST "0".
      *           READ UTRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" UTRAN_PNAME1 BY REFERENCE UTRAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO CHK-040
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE UTRAN_IDLST UTRAN_PNAME1.
           MOVE 1 TO W-NDC.
           GO TO CHK-EX.
       CHK-040.
           CALL "DB_F_Close" USING
            BY REFERENCE UTRAN_IDLST UTRAN_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" URI-F_PNAME1 " " BY REFERENCE URI-F_IDLST "0".
      *           READ URI-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" URI-F_PNAME1 BY REFERENCE URI-R " " RETURNING RET.
           IF  RET = 1
               GO TO CHK-050
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE URI-F_IDLST URI-F_PNAME1.
           MOVE 1 TO W-NDC.
           GO TO CHK-EX.
       CHK-050.
           CALL "DB_F_Close" USING
            BY REFERENCE URI-F_IDLST URI-F_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" KNH-F_PNAME1 " " BY REFERENCE KNH-F_IDLST "0".
      *           READ KNH-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" KNH-F_PNAME1 BY REFERENCE KNH-R " " RETURNING RET.
           IF  RET = 1
               GO TO CHK-060
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE KNH-F_IDLST KNH-F_PNAME1.
           MOVE 1 TO W-NDC.
           GO TO CHK-EX.
       CHK-060.
           CALL "DB_F_Close" USING
            BY REFERENCE KNH-F_IDLST KNH-F_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" HKSR-F_PNAME1 " " BY REFERENCE HKSR-F_IDLST "0".
       CHK-090.
      *           READ HKSR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HKSR-F_PNAME1 BY REFERENCE HKSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO CHK-100
           END-IF
           IF  HKSR-NG = W-NG1 OR W-NG2 OR W-NG3
               GO TO CHK-090
           END-IF
           IF  W-NG1 = ZERO
               MOVE HKSR-NG TO W-NG1
               GO TO CHK-090
           END-IF
           IF  W-NG2 = ZERO
               MOVE HKSR-NG TO W-NG2
               GO TO CHK-090
           END-IF
           IF  W-NG3 = ZERO
               MOVE HKSR-NG TO W-NG3
               GO TO CHK-090
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE HKSR-F_IDLST HKSR-F_PNAME1.
           MOVE 2 TO W-NDC.
           GO TO CHK-EX.
       CHK-100.
           CALL "DB_F_Close" USING
            BY REFERENCE HKSR-F_IDLST HKSR-F_PNAME1.
       CHK-EX.
           EXIT.

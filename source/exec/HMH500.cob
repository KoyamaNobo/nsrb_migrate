       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMH500.
      ************************************************************
      *    PROGRAM         :  履物製品　廃棄更新            　　 *
      ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       77  I                  PIC  9(002).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-D.
             03  W-HCD        PIC  9(006).
             03  W-SU         PIC S9(006).
             03  W-KIN        PIC S9(009).
           02  CNT            PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LWMSG.
      *
           COPY LIHHTF.
           COPY LIHIM.
           COPY LIHUHM.
      *FD  HAIKI
       01  HAIKI_HMH500.
           02  HAIKI_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HAIKI_LNAME    PIC  X(012) VALUE "HAIKI_HMH500".
           02  F              PIC  X(001).
           02  HAIKI_KEY1     PIC  X(100) VALUE SPACE.
           02  HAIKI_SORT     PIC  X(100) VALUE SPACE.
           02  HAIKI_IDLST    PIC  X(100) VALUE SPACE.
           02  HAIKI_RES      USAGE  POINTER.
       01  HAI-R.
           02  HAI-KEY.
             03  HAI-HCD      PIC  9(006).
             03  HAI-SIZ      PIC  9(001).
           02  HAI-ASU.
             03  HAI-SUD   OCCURS  10.
               04  HAI-SU     PIC S9(005).
           02  F              PIC  X(007).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　履物　廃棄　セット　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME5   PIC  X(026) VALUE
                  "***  HUHM REWRITE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(018) VALUE
                  "***  ｻﾞｲｺ ｴﾗｰ  ***".
             03  E-ME7   PIC  X(017) VALUE
                  "***  HUHM ﾅｼ  ***".
             03  E-ME8   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME10  PIC  X(017) VALUE
                  "***  HHTF ﾅｼ  ***".
             03  E-ME11  PIC  X(026) VALUE
                  "***  HHTF REWRITE ｴﾗ-  ***".
             03  E-HHTF  PIC  X(007).
             03  E-HUHM  PIC  X(006).
           COPY LSSEM.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "350" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "50" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "50" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "50" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "50" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "50" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "50" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "133" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "133" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME5" "X" "24" "15" "26" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME6" "X" "24" "15" "18" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME7" "X" "24" "15" "17" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME8" "X" "24" "15" "16" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME10" "X" "24" "15" "17" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME11" "X" "24" "15" "26" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-HHTF" "X" "24" "50" "7" "E-ME11" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-HHTF" BY REFERENCE HHT-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-HUHM" "X" "24" "50" "6" "E-HHTF" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-HUHM" BY REFERENCE HUH-KEY "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO HAIKI_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HAIKI_PNAME1 " " BY REFERENCE HAIKI_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" HHTF_PNAME1 " " BY REFERENCE HHTF_IDLST "2"
            "HHT-KEY" BY REFERENCE HHT-KEY "HHT-KEY2" BY REFERENCE
            HHT-KEY2.
           CALL "DB_F_Open" USING
            "I-O" HUH-M_PNAME1 " " BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-15.
      *           READ HAIKI AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HAIKI_PNAME1 BY REFERENCE HAI-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           MOVE HAI-KEY TO HHT-KEY.
      *           READ HHTF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HHTF_PNAME1 BY REFERENCE HHT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HHTF" E-HHTF "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-30
           END-IF
           MOVE ZERO TO CNT.
       M-20.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-25
           END-IF
           IF  HAI-SU(CNT) = ZERO
               GO TO M-20
           END-IF
           COMPUTE HHT-ZSU(CNT) = HHT-ZSU(CNT) - HAI-SU(CNT).
           MOVE HHT-ZSU(CNT) TO HHT-TZS(CNT).
           GO TO M-20.
       M-25.
      *           REWRITE HHT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HHTF_PNAME1 HHTF_LNAME HHT-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HHTF" E-HHTF "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-30.
           MOVE HAI-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HHTF" E-HHTF "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO HI-FT
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           COMPUTE W-SU = HAI-SU(01) + HAI-SU(02) + HAI-SU(03)
                        + HAI-SU(04) + HAI-SU(05) + HAI-SU(06)
                        + HAI-SU(07) + HAI-SU(08) + HAI-SU(09)
                        + HAI-SU(10).
      *
           MOVE HAI-HCD TO HUH-KEY.
      *           READ HUH-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HUHM" E-HUHM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-15
           END-IF
           COMPUTE HUH-YS = HUH-YS - W-SU.
           COMPUTE HUH-YK = HUH-YS * HI-FT.
           MOVE HUH-YS TO HUH-ZS.
           MOVE HUH-YK TO HUH-ZK.
      *           REWRITE HUH-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HHTF_PNAME1 HHTF_LNAME HHT-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HUHM" E-HUHM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-15.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-M_IDLST HUH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HAIKI_IDLST HAIKI_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

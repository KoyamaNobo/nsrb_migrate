       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG940.
       DATE-WRITTEN. 1997-01-07.
      *********************************************************
      *    PROGRAM         :  教育出荷集計累積ファイル作成　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  __/__/__                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-BCD.
             03  W-BCD1       PIC  9(001).
             03  W-BCD2       PIC  9(001).
           02  W-TAN          PIC S9(005).
      *
           COPY LIHKBM.
           COPY LITM.
           COPY LIHIM.
           COPY LITHTM.
      *FD  SNTR-F
       01  SNTR-F_HMG940.
           02  SNTR-F_PNAME1  PIC  X(005) VALUE "SNTRF".
           02  F              PIC  X(001).
           02  SNTR-F_LNAME   PIC  X(013) VALUE "SNTR-F_HMG940".
           02  F              PIC  X(001).
           02  SNTR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  SNTR-F_SORT    PIC  X(100) VALUE SPACE.
           02  SNTR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  SNTR-F_RES     USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DNO       PIC  9(006).
           02  SNTR-GNO       PIC  9(001).
           02  SNTR-KEY.
             03  SNTR-DATE.
               04  SNTR-NG    PIC  9(006).
               04  SNTR-PEY   PIC  9(002).
             03  SNTR-TCD     PIC  9(004).
             03  SNTR-HCD.
               04  SNTR-HCD1  PIC  9(004).
               04  SNTR-HCD2  PIC  9(002).
           02  SNTR-SIZ       PIC  9(001).
           02  SNTR-SUD.
             03  SNTR-SU    OCCURS  10  PIC S9(004)  COMP-3.
           02  SNTR-SUT       PIC S9(005).
           02  SNTR-T         PIC  9(005).
           02  SNTR-KIN       PIC S9(008).
           02  F              PIC  X(001).
           02  SNTR-DK        PIC  9(001).
           02  SNTR-FT        PIC  9(005).
           02  F              PIC  X(003).
           02  SNTR-BC1       PIC  9(002).
           02  SNTR-BC2.
             03  SNTR-BC21    PIC  9(001).
             03  SNTR-BC22    PIC  9(001).
           02  SNTR-BC3       PIC  9(002).
           02  SNTR-SKC       PIC  9(001).
           02  SNTR-TNC       PIC  9(002).
           02  SNTR-FKC       PIC  9(002).
           02  F              PIC  X(032).
           02  SNTR-SNC       PIC  9(001).
       77  F                  PIC  X(001).
      *FD  HKS-F
       01  HKS-F_HMG940.
           02  HKS-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HKS-F_LNAME    PIC  X(012) VALUE "HKS-F_HMG940".
           02  F              PIC  X(001).
           02  HKS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HKS-F_SORT     PIC  X(100) VALUE SPACE.
           02  HKS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HKS-F_RES      USAGE  POINTER.
       01  HKS-R.
           02  HKS-TCD        PIC  9(004).
           02  HKS-HCD1       PIC  9(004).
           02  HKS-SU         PIC S9(006).
           02  HKS-KIN        PIC S9(008).
           02  HKS-CC         PIC  9(001).
           02  HKS-BC         PIC  9(001).
           02  F              PIC  X(002).
           02  HKS-NG         PIC  9(006).
           02  F              PIC  X(032).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　教育出荷集計ファイル　作成　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME2   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME3   PIC  X(026) VALUE
                  "***  WK0064 WRITE ｴﾗｰ  ***".
             03  E-HCD   PIC  9(006).
             03  E-TCD   PIC  9(004).
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "322" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "46" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "67" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "67" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "16" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "15" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "26" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HCD" "9" "24" "35" "6" "E-ME3" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HCD" BY REFERENCE HI-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TCD" "9" "24" "35" "4" "E-HCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE T-TCD "4" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO HKS-F_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" HKS-F_PNAME1 " " BY REFERENCE HKS-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" SNTR-F_PNAME1 " " BY REFERENCE SNTR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
       M-10.
      *           READ SNTR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  SNTR-GNO = 9
               GO TO M-10
           END-IF
           IF  SNTR-DK = 3 OR 6 OR 8 OR 9
               GO TO M-10
           END-IF
           IF  SNTR-BC3 NOT = 30
               GO TO M-10
           END-IF
           IF  SNTR-HCD1 = 9999
               GO TO M-10
           END-IF
      *
           MOVE SNTR-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO HI-R
               MOVE SNTR-BC2 TO HI-BC2
           END-IF
           IF  HI-OL = 1
               GO TO M-10
           END-IF
      *
           MOVE SNTR-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO T-FKC
           END-IF
           IF  T-KSC NOT = 1
               GO TO M-10
           END-IF
      *
           MOVE SPACE TO HKB-KEY.
           MOVE 01 TO HKB-NO.
           MOVE T-FKC TO HKB-TDFK.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 0 TO HKB-KTKCD
           END-IF.
       M-15.
           MOVE ZERO TO HKS-R.
           MOVE SNTR-NG TO HKS-NG.
           MOVE SNTR-TCD TO HKS-TCD.
           MOVE SNTR-HCD1 TO HKS-HCD1.
           MOVE HI-BC2 TO W-BCD.
           MOVE W-BCD2 TO HKS-BC.
           MOVE HKB-KTKCD TO HKS-CC.
           IF  SNTR-SNC = 1
               COMPUTE HKS-KIN = -1 * SNTR-KIN
               GO TO M-50
           END-IF
           IF  SNTR-DK = 1 OR 5 OR 2
               COMPUTE HKS-SU = -1 * SNTR-SUT
               COMPUTE HKS-KIN = -1 * SNTR-KIN
               GO TO M-50
           END-IF
           MOVE SNTR-SUT TO HKS-SU.
           IF  SNTR-DK NOT = 4
               MOVE SNTR-KIN TO HKS-KIN
               GO TO M-50
           END-IF
           PERFORM TAN-RTN THRU TAN-EX.
           COMPUTE HKS-KIN = W-TAN * SNTR-SUT.
       M-50.
      *           WRITE HKS-R.
      *//////////////
           CALL "DB_Insert" USING
            HKS-F_PNAME1 HKS-F_LNAME HKS-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-10
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME3" E-ME3 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE HKS-F_IDLST HKS-F_PNAME1.
           MOVE "WK0064       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" HKS-F_PNAME1 " " BY REFERENCE HKS-F_IDLST "0".
           GO TO M-15.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HKS-F_IDLST HKS-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       TAN-RTN.
           MOVE ZERO TO W-TAN.
           MOVE SNTR-TCD TO THT-TCD.
           MOVE SNTR-HCD TO THT-HCD.
           MOVE SNTR-SIZ TO THT-SIZ.
      *           READ THTM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO TAN-010
           END-IF
           MOVE THT-T TO W-TAN.
       TAN-010.
           IF  W-TAN NOT = ZERO
               GO TO TAN-EX
           END-IF
           MOVE SNTR-TCD TO THT-TCD.
           MOVE SNTR-HCD TO THT-HCD.
           MOVE 9 TO THT-SIZ.
      *           READ THTM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO TAN-110
           END-IF
           MOVE THT-T TO W-TAN.
       TAN-110.
           IF  W-TAN = ZERO
               MOVE HI-SB TO W-TAN
           END-IF.
       TAN-EX.
           EXIT.

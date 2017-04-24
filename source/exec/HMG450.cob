       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG450.
      *********************************************************
      *    PROGRAM         :  預り受払データ　生成　　　　　　*
      *    PRINTER TYPE    :  *****                           *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/14                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM100.
       OBJECT-COMPUTER. SYSTEM100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-SEN          PIC  9(001).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *FD  TAZ-M
       01  TAZ-M_HMG450.
           02  TAZ-M_PNAME1   PIC  X(004) VALUE "TAZM".
           02  F              PIC  X(001).
           02  TAZ-M_LNAME    PIC  X(012) VALUE "TAZ-M_HMG450".
           02  F              PIC  X(001).
           02  TAZ-M_KEY1     PIC  X(100) VALUE SPACE.
           02  TAZ-M_SORT     PIC  X(100) VALUE SPACE.
           02  TAZ-M_IDLST    PIC  X(100) VALUE SPACE.
           02  TAZ-M_RES      USAGE  POINTER.
       01  TAZ-R.
           02  TAZ-KEY.
             03  TAZ-TCD      PIC  9(004).
             03  TAZ-HCD      PIC  9(006).
           02  TAZ-ZA         PIC S9(005).
           02  TAZ-AS         PIC S9(005).
           02  TAZ-ZAS        PIC S9(005).
           02  TAZ-ASS        PIC S9(005).
           02  F              PIC  X(006).
           02  TAZ-NG         PIC  9(006).
       77  F                  PIC  X(001).
      *FD  SNTR-F
       01  SNTR-F_HMG450.
           02  SNTR-F_PNAME1  PIC  X(005) VALUE "SNTRF".
           02  F              PIC  X(001).
           02  SNTR-F_LNAME   PIC  X(013) VALUE "SNTR-F_HMG450".
           02  F              PIC  X(001).
           02  SNTR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  SNTR-F_SORT    PIC  X(100) VALUE SPACE.
           02  SNTR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  SNTR-F_RES     USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DNO       PIC  9(006).
           02  SNTR-GNO       PIC  9(001).
           02  F              PIC  9(002).
           02  SNTR-DATE      PIC  9(006).
           02  SNTR-TCD       PIC  9(004).
           02  SNTR-HCD       PIC  9(006).
           02  F              PIC  X(031).
           02  SNTR-SU        PIC S9(005).
           02  F              PIC  X(014).
           02  SNTR-DC        PIC  9(001).
           02  F              PIC  X(038).
           02  SNTR-SKD       PIC  9(008).
           02  F              PIC  X(005).
           02  SNTR-SNC       PIC  9(001).
       77  F                  PIC  X(001).
      *****＊＊＊   預り受払ファイル　　＊＊＊*******
      *FD  AUH-F
       01  AUH-F_HMG450.
           02  AUH-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  AUH-F_LNAME    PIC  X(012) VALUE "AUH-F_HMG450".
           02  F              PIC  X(001).
           02  AUH-F_KEY1     PIC  X(100) VALUE SPACE.
           02  AUH-F_SORT     PIC  X(100) VALUE SPACE.
           02  AUH-F_IDLST    PIC  X(100) VALUE SPACE.
           02  AUH-F_RES      USAGE  POINTER.
       01  AUH-R.
           02  UH-TCD         PIC  9(004).
           02  UH-HCD         PIC  9(006).
           02  UH-DATE        PIC  9(006).
           02  UH-AS          PIC S9(005).
           02  UH-SS          PIC S9(005).
           02  UH-KS          PIC S9(005).
           02  F              PIC  X(033).
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
           02  FILLER  PIC  N(020) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
              "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
              "＊＊＊　預り　受払ファイル　作成　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
              "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(025) VALUE
                "全体=0  請求分=1  .....  ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  A-DMM   PIC  9(001).
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
           "C-MID" " " "0" "0" "327" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "40" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "40" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "40" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "40" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "40" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "40" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "15" "17" "25" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "20" "19" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SEN" "9" "15" "41" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "36" "1" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SEN > 1
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-DMM = 9
               GO TO M-05
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-15
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SEN" A-SEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DMM" A-DMM "p" RETURNING RESU.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO AUH-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TAZ-M_PNAME1 "SHARED" BY REFERENCE TAZ-M_IDLST "1"
            "TAZ-KEY" BY REFERENCE TAZ-KEY.
           CALL "DB_F_Open" USING
            "INPUT" SNTR-F_PNAME1 "SHARED" BY REFERENCE
            SNTR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" AUH-F_PNAME1 "EXCLUSIVE" BY REFERENCE
            AUH-F_IDLST "0".
       M-20.
      *           READ TAZ-M NEXT AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT AT END" TAZ-M_PNAME1 BY REFERENCE TAZ-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
           IF  W-SEN = 0
               IF  TAZ-ZA = ZERO
                   GO TO M-20
               END-IF
           END-IF
           IF  W-SEN = 1
               IF  TAZ-ZAS = ZERO
                   GO TO M-20
               END-IF
           END-IF
           MOVE ZERO TO AUH-R.
           MOVE TAZ-TCD TO UH-TCD.
           MOVE TAZ-HCD TO UH-HCD.
           IF  W-SEN = 0
               MOVE TAZ-ZA TO UH-KS
           ELSE
               MOVE TAZ-ZAS TO UH-KS
           END-IF
      *           WRITE AUH-R.
      *//////////////////////
           CALL "DB_Insert" USING
            AUH-F_PNAME1 AUH-F_LNAME AUH-R RETURNING RET.
           GO TO M-20.
       M-30.
      *           READ SNTR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SNTR-GNO = 9
               GO TO M-30
           END-IF
           IF  SNTR-DC NOT = 3 AND 4 AND 9
               GO TO M-30
           END-IF
           IF  SNTR-DC = 3 OR 9
               IF  W-SEN = 1
                   IF  SNTR-SKD = 99999999
                       GO TO M-30
                   END-IF
               END-IF
           END-IF
           MOVE ZERO TO AUH-R.
           MOVE SNTR-TCD TO UH-TCD.
           MOVE SNTR-HCD TO UH-HCD.
           MOVE SNTR-DATE TO UH-DATE.
           IF  SNTR-DC = 3 OR 9
               IF  W-SEN = 0
                   MOVE SNTR-SU TO UH-AS
               ELSE
                   IF  SNTR-SKD NOT = 99999999
                       MOVE SNTR-SU TO UH-AS
                   END-IF
               END-IF
           END-IF
           IF  SNTR-DC = 4
               MOVE SNTR-SU TO UH-SS
           END-IF
      *           WRITE AUH-R.
      *//////////////////////
           CALL "DB_Insert" USING
            AUH-F_PNAME1 AUH-F_LNAME AUH-R RETURNING RET.
           GO TO M-30.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE TAZ-M_IDLST TAZ-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE AUH-F_IDLST AUH-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

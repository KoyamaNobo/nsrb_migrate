       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         HKT840.
      **************************************************************
      *    PROGRAM         :  担当別売上･粗利前年対比ワーク作成    *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  ______                               *
      *    COMPILE TYPE    :  COBOL                                *
      **************************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0512".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-TC1          PIC  9(001).
           02  CNT            PIC  9(002).
       01  ERR-STAT           PIC  X(002).
      *
      *FD  THY-M
       01  THY-M_HKT840.
           02  THY-M_PNAME1   PIC  X(009) VALUE "WK0512".
           02  F              PIC  X(001).
           02  THY-M_LNAME    PIC  X(012) VALUE "THY-M_HKT840".
           02  F              PIC  X(001).
           02  THY-M_KEY1     PIC  X(100) VALUE SPACE.
           02  THY-M_SORT     PIC  X(100) VALUE SPACE.
           02  THY-M_IDLST    PIC  X(100) VALUE SPACE.
           02  THY-M_RES      USAGE  POINTER.
       01  TH-R.
           02  TH-KEY         PIC  9(004).
           02  TH-IKC         PIC  9(001).
           02  TH-UD.
             03  TH-U     OCCURS  24.
               04  TH-UKD     PIC S9(009).
             03  TH-TU.
               04  TH-AOTU    PIC S9(010).
               04  TH-ANTU    PIC S9(010).
           02  TH-AD.
             03  TH-A     OCCURS  24.
               04  TH-AKD     PIC S9(009).
             03  TH-TA.
               04  TH-AOTA    PIC S9(010).
               04  TH-ANTA    PIC S9(010).
           02  TH-NG.
             03  TH-N         PIC  9(004).
             03  TH-G         PIC  9(002).
           02  TH-TC.
             03  TH-TC1       PIC  9(001).
             03  TH-TC2       PIC  9(001).
           02  TH-BC          PIC  9(001).
           02  F              PIC  X(025).
           02  TH-SEN         PIC  9(001).
       77  F                  PIC  X(001).
      *FD  WTHY-M
       01  WTHY-M_HKT840.
           02  WTHY-M_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  WTHY-M_LNAME   PIC  X(013) VALUE "WTHY-M_HKT840".
           02  F              PIC  X(001).
           02  WTHY-M_KEY1    PIC  X(100) VALUE SPACE.
           02  WTHY-M_SORT    PIC  X(100) VALUE SPACE.
           02  WTHY-M_IDLST   PIC  X(100) VALUE SPACE.
           02  WTHY-M_RES     USAGE  POINTER.
       01  WTH-R.
           02  WTH-KEY        PIC  9(004).
           02  WTH-IKC        PIC  9(001).
           02  WTH-UD.
             03  WTH-U     OCCURS  24.
               04  WTH-UKD    PIC S9(009).
             03  WTH-TU.
               04  WTH-AOTU   PIC S9(010).
               04  WTH-ANTU   PIC S9(010).
           02  WTH-AD.
             03  WTH-A     OCCURS  24.
               04  WTH-AKD    PIC S9(009).
             03  WTH-TA.
               04  WTH-AOTA   PIC S9(010).
               04  WTH-ANTA   PIC S9(010).
           02  WTH-NG.
             03  WTH-N        PIC  9(004).
             03  WTH-G        PIC  9(002).
           02  WTH-TC.
             03  WTH-TC1      PIC  9(001).
             03  WTH-TC2      PIC  9(001).
           02  WTH-BC         PIC  9(001).
           02  F              PIC  X(025).
           02  WTH-SEN        PIC  9(001).
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
                "＊＊＊　　担当売上粗利前年対比ワーク作成　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                   "***  ﾃﾞｰﾀ ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
       PROCEDURE           DIVISION.
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
           "C-ERR" " " "0" "0" "77" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "77" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0512ID.
           MOVE WK0512ID TO WTHY-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" THY-M_PNAME1 " " BY REFERENCE THY-M_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" WTHY-M_PNAME1 " " BY REFERENCE WTHY-M_IDLST "0".
      *
      *           READ THY-M AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" THY-M_PNAME1 BY REFERENCE TH-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-10.
           MOVE TH-TC1 TO W-TC1.
           INITIALIZE WTH-R.
           MOVE TH-R TO WTH-R.
       M-25.
      *           READ THY-M AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" THY-M_PNAME1 BY REFERENCE TH-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  TH-TC1 = W-TC1
               GO TO M-30
           END-IF
      *           WRITE WTH-R.
      *//////////////
           CALL "DB_Insert" USING
            WTHY-M_PNAME1 WTHY-M_LNAME WTH-R RETURNING RET.
           GO TO M-10.
       M-30.
           ADD TH-AOTU TO WTH-AOTU.
           ADD TH-ANTU TO WTH-ANTU.
           ADD TH-AOTA TO WTH-AOTA.
           ADD TH-ANTA TO WTH-ANTA.
           MOVE ZERO TO CNT.
       M-35.
           ADD 1 TO CNT.
           IF  CNT < 25
               ADD TH-UKD(CNT) TO WTH-UKD(CNT)
               ADD TH-AKD(CNT) TO WTH-AKD(CNT)
               GO TO M-35
           END-IF
           GO TO M-25.
       M-85.
      *           WRITE WTH-R.
      *//////////////
           CALL "DB_Insert" USING
            WTHY-M_PNAME1 WTHY-M_LNAME WTH-R RETURNING RET.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE THY-M_IDLST THY-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WTHY-M_IDLST WTHY-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

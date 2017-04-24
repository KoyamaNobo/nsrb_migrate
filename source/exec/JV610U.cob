       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JV610U.
      ************************************************************
      *    PROGRAM         :  玉島棚卸変換(WK0128→HTIM)         *
      *    PRINTER TYPE    :  JIPS                               *
      *    SCREEN          :  ______                             *
      *    COMPILE TYPE    :  COBOL                              *
      *    JS-SIGN         :  バーコードあり=0 , なし=1          *
      ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  JS-SIGN            PIC  9(001).
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0128".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  W-SOKD         PIC  9(01).
           02  W-DMM          PIC  X(01).
           02  W-DNO          PIC  9(05).
           02  W-KEY.
             03  W-HHT        PIC  X(06).
             03  W-SOK        PIC  9(01).
             03  W-BAS        PIC  9(06).
             03  W-HCD        PIC  9(06).
           COPY LSTAT.
      *
           COPY LIHTIM.
           COPY L-JCON.
      *FD  TANAO                                                        ﾆﾌﾀﾞﾄﾗﾝ
       01  TANAO_JV610U.
           02  TANAO_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TANAO_LNAME    PIC  X(012) VALUE "TANAO_JV610U".
           02  F              PIC  X(001).
           02  TANAO_KEY1     PIC  X(100) VALUE SPACE.
           02  TANAO_SORT     PIC  X(100) VALUE SPACE.
           02  TANAO_IDLST    PIC  X(100) VALUE SPACE.
           02  TANAO_RES      USAGE  POINTER.
       01  TANAO-R.
           02  TANAO-KEY.
             03  TANAO-HHT    PIC X(6).                                 HHT
             03  TANAO-SOK    PIC 9(1).                                 倉庫C
             03  TANAO-BAS    PIC 9(6).                                 場所NO
             03  TANAO-HCD    PIC 9(6).                                 品名C
           02  TANAO-SC       PIC 9(1).                                 ｻｲｽﾞ
           02  TANAO-ASU.                                               数量
             03  TANAO-SUD   OCCURS  10.                                数量
               04  TANAO-SU   PIC S9(6).                                数量
           02  TANAO-GC       PIC 9(1).                                 行C
           02  TANAO-ISU      PIC 9(3).                                 入数
           02  TANAO-BC       PIC 9(06).
           02  F              PIC X(38).
       77  F                  PIC X(01).
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
           02  FILLER.
               03  FILLER  PIC  N(003) VALUE  "確認（".
               03  FILLER  PIC  X(009) VALUE  "OK=1,NO=9".
               03  FILLER  PIC  N(001) VALUE  "）".
               03  FILLER  PIC  X(009) VALUE  "---> ﾘﾀｰﾝ".
       01  C-MID0.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　玉島棚卸変換（バーコード分）　＊＊＊".
       01  C-MID1.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　棚卸変換（バーコードなし分）　＊＊＊".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SOM   PIC  N(006).
           02  FILLER.
             03  D-DEL   PIC  N(009) VALUE
                         "「　削　除　中　」".
             03  D-WRI   PIC  N(009) VALUE
                         "「　書き込み中　」".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME3   PIC  X(025) VALUE
                  "***  HTIM DELETE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(024) VALUE
                  "***  HTIM WRITE ｴﾗｰ  ***".
             03  E-ME5   PIC  X(026) VALUE
                  "***  HTIM 2ｼﾞｭｳｷｰ ｴﾗｰ  ***".
             03  E-ME6   PIC  X(025) VALUE
                  "***  ﾃﾞﾝﾋﾟｮｳNO ｵｰﾊﾞｰ  ***".
             03  E-ME7.
               04  FILLER  PIC  X(016) VALUE
                    "***  ｿｳｺ ﾅｼ  ***".
               04  02E-ME7 PIC  9(001).
             03  E-KEY   PIC  X(007).
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
           "C-MID" " " "0" "0" "26" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" " " "23" "0" "26" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "23" "41" "6" " " "01C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "9" "23" "47" "9" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "23" "56" "2" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "9" "23" "58" "9" "04C-MID" " " RETURNING RESU.
      *C-MID0
       CALL "SD_Init" USING
           "C-MID0" " " "0" "0" "46" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID0" "N" "1" "13" "46" " " "C-MID0" RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING
           "C-MID1" " " "0" "0" "46" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID1" "N" "1" "13" "46" " " "C-MID1" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "62" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "48" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-SOM" "N" "11" "30" "12" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING
           "D-SOM" BY REFERENCE JCON3-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "01C-DSP" " " "15" "0" "36" "D-SOM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-DEL" "bN" "15" "27" "18" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "D-WRI" "bN" "15" "27" "18" "D-DEL" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "159" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "159" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" "X" "24" "15" "25" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME4" "X" "24" "15" "24" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME5" "X" "24" "15" "26" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME6" "X" "24" "15" "25" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME7" " " "24" "0" "17" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01E-ME7" "X" "24" "15" "16" " " "E-ME7" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-ME7" "9" "24" "36" "1" "01E-ME7" " " RETURNING RESU.
       CALL "SD_From" USING
           "02E-ME7" BY REFERENCE TANAO-SOK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "X" "24" "45" "7" "E-ME7" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE HTI-KEY "7" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Output" USING
                "C-MID0" C-MID0 "p" RETURNING RESU
           END-IF
           IF  JS-SIGN = 1
               CALL "SD_Output" USING
                "C-MID1" C-MID1 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 0
               GO TO M-10
           END-IF
           PERFORM S-05 THRU S-15.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
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
           CALL "SD_Output" USING "D-DEL" D-DEL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" HTI-M_PNAME1 "SHARED" BY REFERENCE HTI-M_IDLST "1"
            "HTI-KEY" BY REFERENCE HTI-KEY.
           MOVE SPACE TO HTI-KEY.
           IF  JS-SIGN = 0
               MOVE 70000 TO HTI-DNO1
           END-IF
           IF  JS-SIGN = 1
               IF  W-SOKD = 1
                   MOVE 60000 TO HTI-DNO1
               ELSE
                   IF  W-SOKD = 7
                       MOVE 65000 TO HTI-DNO1
                   ELSE
                       IF  W-SOKD = 6
                           MOVE 80000 TO HTI-DNO1
                       END-IF
                   END-IF
               END-IF
           END-IF
      *           START HTI-M KEY NOT < HTI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HTI-M_PNAME1 "HTI-KEY" "NOT < " HTI-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF.
       M-15.
      *           READ HTI-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HTI-M_PNAME1 BY REFERENCE HTI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF
           IF  JS-SIGN = 0
               IF  HTI-DNO1 > 79999
                   GO TO M-20
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  W-SOKD = 1
                   IF  HTI-DNO1 > 64999
                       GO TO M-20
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  W-SOKD = 7
                   IF  HTI-DNO1 > 69999
                       GO TO M-20
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  W-SOKD = 6
                   IF  HTI-DNO1 > 84999
                       GO TO M-20
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  HTI-NC NOT = 1
                   GO TO M-15
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  HTI-NC NOT = 2
                   GO TO M-15
               END-IF
           END-IF
      *           DELETE HTI-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HTI-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HTI-M_IDLST HTI-M_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-15.
       M-20.
           CALL "DB_F_Close" USING
            BY REFERENCE HTI-M_IDLST HTI-M_PNAME1.
           CALL "SD_Output" USING "D-WRI" D-WRI "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID22.
           MOVE W-FID2 TO WK0128ID.
           MOVE WK0128ID TO TANAO_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TANAO_PNAME1 " " BY REFERENCE TANAO_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" HTI-M_PNAME1 "SHARED" BY REFERENCE HTI-M_IDLST "1"
            "HTI-KEY" BY REFERENCE HTI-KEY.
      *
      *           READ TANAO AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TANAO_PNAME1 BY REFERENCE TANAO-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  JS-SIGN = 0
               MOVE 70000 TO W-DNO
           END-IF
           IF  JS-SIGN = 1
               IF  W-SOKD = 1
                   MOVE 60000 TO W-DNO
               ELSE
                   IF  W-SOKD = 7
                       MOVE 65000 TO W-DNO
                   ELSE
                       IF  W-SOKD = 6
                           MOVE 80000 TO W-DNO
                       END-IF
                   END-IF
               END-IF
           END-IF.
       M-25.
           MOVE TANAO-KEY TO W-KEY.
           ADD 1 TO W-DNO.
           IF  JS-SIGN = 0
               IF  W-DNO > 79999
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "E-ME6" E-ME6 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-90
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  W-SOKD = 1
                   IF  W-DNO > 64999
                       CALL "C3_Set_Jrcode" USING 
                        USER_ID BY REFERENCE COMPLETION_CODE  255
                       CALL "SD_Output" USING
                        "E-ME6" E-ME6 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME99" E-ME99 "p" RETURNING RESU
                       GO TO M-90
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  W-SOKD = 7
                   IF  W-DNO > 69999
                       CALL "C3_Set_Jrcode" USING 
                        USER_ID BY REFERENCE COMPLETION_CODE  255
                       CALL "SD_Output" USING
                        "E-ME6" E-ME6 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME99" E-ME99 "p" RETURNING RESU
                       GO TO M-90
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  W-SOKD = 6
                   IF  W-DNO > 84999
                       CALL "C3_Set_Jrcode" USING 
                        USER_ID BY REFERENCE COMPLETION_CODE  255
                       CALL "SD_Output" USING
                        "E-ME6" E-ME6 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME99" E-ME99 "p" RETURNING RESU
                       GO TO M-90
                   END-IF
               END-IF
           END-IF
      *
           MOVE SPACE TO HTI-KEY.
           MOVE W-DNO TO HTI-DNO1.
      *           START HTI-M KEY NOT < HTI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HTI-M_PNAME1 "HTI-KEY" "NOT < " HTI-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
      *           READ HTI-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HTI-M_PNAME1 BY REFERENCE HTI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
           IF  HTI-DNO1 = W-DNO
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-25
           END-IF.
       M-30.
           MOVE ZERO TO HTI-R.
           MOVE W-DNO TO HTI-DNO1.
           MOVE SPACE TO HTI-DNO2.
           MOVE TANAO-GC TO HTI-GNO.
           MOVE TANAO-SOK TO HTI-SNO.
           MOVE TANAO-HCD TO HTI-HCD.
           MOVE TANAO-SC TO HTI-SIZ.
           MOVE TANAO-ASU TO HTI-SUD.
           MOVE TANAO-BC TO HTI-BC.
           MOVE TANAO-ISU TO HTI-ISU.
           IF  JS-SIGN = 0
               MOVE 1 TO HTI-NC
           END-IF
           IF  JS-SIGN = 1
               MOVE 2 TO HTI-NC
           END-IF
      *           WRITE HTI-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HTI-M_PNAME1 HTI-M_LNAME HTI-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-35
           END-IF
           GO TO M-40.
       M-35.
           IF  ERR-STAT = 22
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-CL" E-CL "p" RETURNING RESU
               GO TO M-25
           END-IF
           IF  ERR-STAT NOT = 24
               GO TO M-90
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE HTI-M_IDLST HTI-M_PNAME1.
           MOVE "HTIM         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" HTI-M_PNAME1 "SHARED" BY REFERENCE HTI-M_IDLST "1"
            "HTI-KEY" BY REFERENCE HTI-KEY.
           GO TO M-30.
       M-40.
      *           READ TANAO AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TANAO_PNAME1 BY REFERENCE TANAO-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  TANAO-KEY = W-KEY
               GO TO M-30
           END-IF
           GO TO M-25.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE TANAO_IDLST TANAO_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HTI-M_IDLST HTI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID22.
           MOVE W-FID2 TO WK0128ID.
           MOVE WK0128ID TO TANAO_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TANAO_PNAME1 " " BY REFERENCE TANAO_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON3-KEY" BY REFERENCE JCON3-KEY.
      *           READ TANAO AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TANAO_PNAME1 BY REFERENCE TANAO-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-10
           END-IF
           MOVE "3" TO JCON3-01.
           MOVE TANAO-SOK TO W-SOKD JCON3-02.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-10
           END-IF
           CALL "SD_Output" USING "D-SOM" D-SOM "p" RETURNING RESU.
       S-10.
           CALL "DB_F_Close" USING
            BY REFERENCE TANAO_IDLST TANAO_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
       S-15.
           EXIT.

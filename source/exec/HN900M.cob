       IDENTIFICATION DIVISION.
       PROGRAM-ID. HN900M.
      ******************************************************
      *    得意先品名単価マスタ　一括削除（得意先・品名毎）*
      ******************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       77  W-EC               PIC  9(001) VALUE 0.
       77  CHK                PIC  9(001) VALUE 0.
       77  W-INV              PIC  9(001) VALUE 0.
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-SEN          PIC  9(001).
           02  W-SCO          PIC  9(004).
           02  W-ECO          PIC  9(004).
           02  W-DMM          PIC  9(001).
           COPY LSTAT.
      *
           COPY LITHTM.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　得意先品名単価マスタ　削除　　＊＊＊".
           02  FILLER  PIC  X(035) VALUE
                  "得意先別 = 1  ,  品名別 = 2    ﾘﾀｰﾝ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  FILLER.
             03  A-SCO   PIC  9(004).
             03  A-ECO   PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-M1    PIC  X(024) VALUE
                  "得意先ｺｰﾄﾞ       ～     ".
             03  D-M2    PIC  X(024) VALUE
                  "  品名ｺｰﾄﾞ       ～     ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  THTM ﾅｼ  ***".
             03  E-ME2   PIC  X(026) VALUE
                  "***  THTM DELETE ｴﾗｰ  ***".
             03  E-KEY   PIC  X(011).
           COPY LSSEM.
           COPY LIBSCR.
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
           "C-MID" " " "0" "0" "103" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "1" "15" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "X" "10" "20" "35" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "X" "20" "26" "22" "02C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SEN" "9" "10" "49" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "14" "0" "8" "A-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SCO" "9" "14" "37" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SCO" BY REFERENCE W-SCO "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-ECO" "9" "14" "45" "4" "A-SCO" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-ECO" BY REFERENCE W-ECO "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "43" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "48" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-DSP" " " "14" "0" "48" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "D-M1" "X" "14" "25" "24" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "D-M2" "X" "14" "25" "24" "D-M1" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "54" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "54" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "26" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "X" "24" "45" "11" "E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE THT-KEY "11" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
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
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SEN NOT = 1 AND 2
               GO TO M-10
           END-IF
      *
           IF  W-SEN = 1
               CALL "SD_Output" USING "D-M1" D-M1 "p" RETURNING RESU
           ELSE
               IF  W-SEN = 2
                   CALL "SD_Output" USING
                    "D-M2" D-M2 "p" RETURNING RESU
               END-IF
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SCO "A-SCO" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-ECO "A-ECO" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-SCO > W-ECO
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-25
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
      *
           MOVE 0 TO CHK.
           IF W-SEN NOT = 1
               GO TO M-40
           END-IF
           MOVE SPACE TO THT-KEY.
           MOVE W-SCO TO THT-TCD.
      *           START THTM KEY NOT < THT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            THTM_PNAME1 "THT-KEY" " NOT < " THT-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF.
       M-35.
      *           READ THTM NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" THTM_PNAME1 BY REFERENCE THT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  THT-TCD > W-ECO
               GO TO M-50
           END-IF
      *           DELETE THTM INVALID KEY
      *///////////////
           CALL "DB_Delete" USING THTM_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-50
           END-IF
           IF  CHK = 0
               MOVE 1 TO CHK
           END-IF
           IF  W-EC = 0
               MOVE 1 TO W-EC
           END-IF
           GO TO M-35.
       M-40.
           MOVE SPACE TO THT-KEY2.
           MOVE W-SCO TO THT-HCDF.
      *           START THTM KEY NOT < THT-KEY2 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            THTM_PNAME1 "THT-KEY" " NOT < " THT-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF.
       M-45.
      *           READ THTM NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" THTM_PNAME1 BY REFERENCE THT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  THT-HCDF > W-ECO
               GO TO M-50
           END-IF
      *           DELETE THTM INVALID KEY
      *///////////////
           CALL "DB_Delete" USING THTM_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-50
           END-IF
           IF  CHK = 0
               MOVE 1 TO CHK
           END-IF
           IF  W-EC = 0
               MOVE 1 TO W-EC
           END-IF
           GO TO M-45.
       M-50.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           IF  CHK = 0
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU
           END-IF
           GO TO M-15.
       M-95.
           IF  W-EC = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

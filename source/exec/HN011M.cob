       IDENTIFICATION DIVISION.
       PROGRAM-ID. HN011M.
      *********************************************************
      *    PROGRAM         :  得意先品名単価変換 EXCELより    *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERR-STAT             PIC  X(002).
       01  W-FILE               PIC  X(013).
       01  W-DATA.
           02  W-DMM            PIC  9(001).
           02  W-UC             PIC  9(001).
           02  W-DC             PIC  9(001).
           02  W-SU             PIC  9(006).
           COPY    LSTAT.
      *
           COPY    LITM.
           COPY    LIHUHM.
           COPY    LITHTM.
      *FD  HENKAN
       01  HENKAN_HN011M.
           02  HENKAN_PNAME1    PIC  X(009) VALUE "WK0064000".
           02  F                PIC  X(001).
           02  HENKAN_LNAME     PIC  X(013) VALUE "HENKAN_HN011M".
           02  F                PIC  X(001).
           02  HENKAN_KEY1      PIC  X(100) VALUE SPACE.
           02  HENKAN_SORT      PIC  X(100) VALUE SPACE.
           02  HENKAN_IDLST     PIC  X(100) VALUE SPACE.
           02  HENKAN_RES       USAGE  POINTER.
       01  HENKAN-R.
           02  HENKAN-KEY.
             03  HENKAN-TCD     PIC  X(004).
             03  HENKAN-HCD     PIC  X(006).
             03  HENKAN-HCDD  REDEFINES HENKAN-HCD.
               04  HENKAN-HCD1  PIC  X(004).
               04  HENKAN-HCD2  PIC  X(002).
             03  HENKAN-SIZ     PIC  X(001).
           02  HENKAN-NEWT      PIC  9(005).
           02  F                PIC  X(048).
       77  F                    PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  ACP-AREA.
           02  A-DMM   PIC  9(01).
       01  C-MID.
           02  FILLER.
             03  D-TITLE     PIC  N(21)  VALUE
                 "＊＊＊　　得意先品名別単価　変換　　＊＊＊".
           02  FILLER        PIC  X(21)  VALUE
                 "確認 OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-DSP.
           02  D-KEN.
             03  FILLER      PIC  N(003) VALUE "行　数".
             03  02D-KEN     PIC  Z(006).
             03  FILLER      PIC  N(001) VALUE "件".
           02  D-SKEN.
             03  FILLER.
               04  FILLER      PIC  N(003) VALUE "処　理".
               04  03D-SKEN    PIC  Z(006).
               04  FILLER      PIC  N(001) VALUE "件".
             03  FILLER      PIC  X(21)  VALUE
                 "確認             ﾘﾀｰﾝ".
       01  DSP-ERR.
           02  FILLER.
             03  E-ME1.
               04  FILLER      PIC  X(11)  VALUE
                    "REWRITE ｴﾗｰ".
               04  02E-ME1     PIC  X(11).
             03  E-ME2.
               04  FILLER      PIC  X(09)  VALUE
                    "WRITE ｴﾗｰ".
               04  02E-ME2     PIC  X(11).
             03  E-ME3.
               04  FILLER      PIC  X(10)  VALUE
                    "DELETE ｴﾗｰ".
               04  02E-ME3     PIC  X(11).
             03  E-ME4.
               04  FILLER      PIC  X(07)  VALUE
                    "ﾋﾝﾒｲ ﾅｼ".
               04  02E-ME4     PIC  X(11).
             03  E-ME5.
               04  FILLER      PIC  X(08)  VALUE
                    "ﾄｸｲｻｷ ﾅｼ".
               04  02E-ME5     PIC  X(11).
           COPY    LSSEM.
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
      *ACP-AREA
       CALL "SD_Init" USING
           "ACP-AREA" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "66" "1" "ACP-AREA" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "63" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" " " "1" "0" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "D-TITLE" "N" "1" "16" "42" " " "01C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "X" "23" "50" "21" "01C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "49" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-KEN" " " "12" "0" "14" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "01D-KEN" "N" "12" "31" "6" " " "D-KEN" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-KEN" "Z" "12" "38" "6" "01D-KEN" " " RETURNING RESU.
       CALL "SD_From" USING
           "02D-KEN" BY REFERENCE W-SU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-KEN" "N" "12" "44" "2" "02D-KEN" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-SKEN" " " "0" "0" "35" "D-KEN" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-SKEN" " " "14" "0" "14" " " "D-SKEN" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-SKEN" "N" "14" "31" "6" " " "01D-SKEN" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-SKEN" "Z" "14" "38" "6" "02D-SKEN" " " RETURNING RESU.
       CALL "SD_From" USING
           "03D-SKEN" BY REFERENCE W-SU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "04D-SKEN" "N" "14" "44" "2" "03D-SKEN" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05D-SKEN" "X" "23" "50" "21" "01D-SKEN" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "100" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "100" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" " " "24" "0" "22" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "01E-ME1" "X" "24" "15" "11" " " "E-ME1" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-ME1" "X" "24" "29" "11" "01E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING
           "02E-ME1" BY REFERENCE THT-KEY "11" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" " " "24" "0" "20" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01E-ME2" "X" "24" "15" "9" " " "E-ME2" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-ME2" "X" "24" "29" "11" "01E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING
           "02E-ME2" BY REFERENCE THT-KEY "11" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" " " "24" "0" "21" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01E-ME3" "X" "24" "15" "10" " " "E-ME3" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-ME3" "X" "24" "29" "11" "01E-ME3" " " RETURNING RESU.
       CALL "SD_From" USING
           "02E-ME3" BY REFERENCE THT-KEY "11" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME4" " " "24" "0" "18" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01E-ME4" "X" "24" "15" "7" " " "E-ME4" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-ME4" "X" "24" "29" "11" "01E-ME4" " " RETURNING RESU.
       CALL "SD_From" USING
           "02E-ME4" BY REFERENCE HENKAN-KEY "11" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME5" " " "24" "0" "19" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01E-ME5" "X" "24" "15" "8" " " "E-ME5" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-ME5" "X" "24" "29" "11" "01E-ME5" " " RETURNING RESU.
       CALL "SD_From" USING
           "02E-ME5" BY REFERENCE HENKAN-KEY "11" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           PERFORM S-05 THRU S-20.
           CALL "SD_Output" USING "D-KEN" D-KEN "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           MOVE 0 TO W-UC.
           CALL "DB_F_Open" USING
            "INPUT" HENKAN_PNAME1 " " BY REFERENCE HENKAN_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
       M-15.
      *           READ HENKAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HENKAN_PNAME1 BY REFERENCE HENKAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           IF  HENKAN-NEWT = ZERO
               GO TO M-15
           END-IF
           MOVE SPACE TO THT-KEY.
           MOVE HENKAN-TCD TO THT-TCD.
           IF  HENKAN-HCD2 = SPACE
               MOVE HENKAN-HCD1 TO THT-HCDF
           ELSE
               MOVE HENKAN-HCD TO THT-HCD
           END-IF.
      *           START THTM KEY NOT < THT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            THTM_PNAME1 "THT-KEY" " NOT < " THT-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF.
       M-20.
      *           READ THTM NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" THTM_PNAME1 BY REFERENCE THT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           IF  HENKAN-TCD NOT = THT-TCD
               GO TO M-15
           END-IF
           IF  HENKAN-HCD2 = SPACE
               IF  HENKAN-HCD1 NOT = THT-HCDF
                   GO TO M-15
               END-IF
           END-IF
           IF  HENKAN-HCD2 NOT = SPACE
               IF  HENKAN-HCD NOT = THT-HCD
                   GO TO M-15
               END-IF
           END-IF
      *
      *           DELETE THTM INVALID KEY
      *///////////////
           CALL "DB_Delete" USING THTM_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-20.
       M-25.
           CALL "DB_F_Close" USING
            BY REFERENCE HENKAN_IDLST HENKAN_PNAME1.
           MOVE 1 TO W-UC.
           MOVE ZERO TO W-SU.
           CALL "DB_F_Open" USING
            "INPUT" HENKAN_PNAME1 " " BY REFERENCE HENKAN_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HUH-M_PNAME1 "SHARED" BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
       M-30.
      *           READ HENKAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HENKAN_PNAME1 BY REFERENCE HENKAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HENKAN-NEWT = ZERO
               GO TO M-30
           END-IF
           MOVE HENKAN-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-30
           END-IF
           IF  HENKAN-HCD2 = SPACE
               GO TO M-35
           END-IF
           MOVE HENKAN-HCD TO HUH-KEY.
      *           READ HUH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-30
           END-IF
           GO TO M-45.
       M-35.
           MOVE SPACE TO HUH-KEY.
           MOVE HENKAN-HCD1 TO HUH-HCD1.
      *           START HUH-M KEY NOT < HUH-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HUH-M_PNAME1 "HUH-KEY" " NOT < " HUH-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-30
           END-IF
           MOVE 0 TO W-DC.
       M-40.
      *           READ HUH-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HUH-M_PNAME1 BY REFERENCE HUH-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-30
           END-IF
           IF  HUH-HCD1 NOT = HENKAN-HCD1
               IF  W-DC = 0
                   CALL "SD_Output" USING
                    "E-ME4" E-ME4 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-CL" E-CL "p" RETURNING RESU
                   GO TO M-30
               ELSE
                   GO TO M-30
               END-IF
           END-IF.
       M-45.
           INITIALIZE THT-R.
           MOVE HENKAN-TCD TO THT-TCD THT-TCD1.
           MOVE HUH-HCD TO THT-HCD.
           MOVE HENKAN-SIZ TO THT-SIZ.
           MOVE HENKAN-NEWT TO THT-T.
           MOVE T-TNC TO THT-TNC.
      *           WRITE THT-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            THTM_PNAME1 THTM_LNAME THT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           ADD 1 TO W-SU.
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF
           IF  HENKAN-HCD2 = SPACE
               GO TO M-40
           END-IF
           GO TO M-30.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE HENKAN_IDLST HENKAN_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           IF  W-UC = 1
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HUH-M_IDLST HUH-M_PNAME1
           END-IF
           CALL "SD_Output" USING "D-SKEN" D-SKEN "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE ZERO TO W-SU.
           CALL "DB_F_Open" USING
            "INPUT" HENKAN_PNAME1 " " BY REFERENCE HENKAN_IDLST "0".
       S-10.
      *           READ HENKAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HENKAN_PNAME1 BY REFERENCE HENKAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-15
           END-IF
           IF  HENKAN-NEWT NOT = ZERO
               ADD 1 TO W-SU
           END-IF
           GO TO S-10.
       S-15.
           CALL "DB_F_Close" USING
            BY REFERENCE HENKAN_IDLST HENKAN_PNAME1.
       S-20.
           EXIT.

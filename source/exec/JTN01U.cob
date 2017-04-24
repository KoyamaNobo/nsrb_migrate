       IDENTIFICATION DIVISION.
       PROGRAM-ID. JTN01U.
      *****************************************************************
      *    PROGRAM         :  受注マスタ変換 JMSTD → JMSTW           *
      *    PRINTER TYPE    :  JIPS                                    *
      *    SCREEN          :  ******                                  *
      *    COMPILE TYPE    :  COBOL                                   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-FILE         PIC  X(013).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LJMSTD.
           COPY LIHIM.
           COPY LITM.
           COPY LITCM.
      *FD  JMSTW
       01  JMSTW_JTN01U.
           02  JMSTW_PNAME1   PIC  X(009) VALUE "JMSTW-RDB".
           02  F              PIC  X(001).
           02  JMSTW_LNAME    PIC  X(012) VALUE "JMSTW_JTN01U".
           02  F              PIC  X(001).
           02  JMSTW_KEY1     PIC  X(100) VALUE SPACE.
           02  JMSTW_SORT     PIC  X(100) VALUE SPACE.
           02  JMSTW_IDLST    PIC  X(100) VALUE SPACE.
           02  JMSTW_RES      USAGE  POINTER.
       01  JMSTW-R.
           02  JMSTW-KEY1.
             03  JMSTW-07     PIC 9(6).
             03  JMSTW-08     PIC 9(1).
           02  JMSTW-01       PIC 9(1).
           02  JMSTW-02       PIC 9(8).
           02  JMSTW-04       PIC 9(4).
           02  JMSTW-10       PIC 9(3).
           02  JMSTW-05       PIC 9(6).
           02  JMSTW-09       PIC 9(1).
           02  JMSTW-11.
             03  JMSTW-111    OCCURS  10.
               04  JMSTW-1111 PIC S9(6).
             03  JMSTW-112    PIC S9(6).
           02  JMSTW-16       PIC S9(03).
           02  JMSTW-06       PIC 9(8).
           02  JMSTW-17       PIC 9(05).
           02  JMSTW-22       PIC X(10).
           02  JMSTW-13       PIC N(32).
           02  JMSTW-31       PIC N(26).
           02  JMSTW-32       PIC N(26).
           02  JMSTW-33       PIC N(24).
           02  F              PIC X(03).
       77  F                  PIC X(01).
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
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　受注マスタ　変換　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  HUH-M ﾅｼ  ***".
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
            "C-MID" " " "0" "0" "274" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "36" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "36" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "36" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "36" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "36" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "36" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "36" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "20" "21" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "38" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "18" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "18" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" JMSTD_PNAME1 "SHARED" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2"
            BY REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE
            JMSTD-KEY3.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" JMSTW_PNAME1 " " BY REFERENCE JMSTW_IDLST "0".
       M-15.
      *           READ JMSTD NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JMSTD-07 > 200000
               GO TO M-15
           END-IF
           MOVE JMSTD-03 TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
           END-IF
           MOVE JMSTD-04 TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
           END-IF
           MOVE JMSTD-04 TO TC-TCD.
           MOVE JMSTD-10 TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
           END-IF
           INITIALIZE JMSTW-R.
           MOVE JMSTD-07       TO JMSTW-07.
           MOVE JMSTD-08       TO JMSTW-08.
           MOVE JMSTD-01       TO JMSTW-01.
           MOVE JMSTD-02       TO JMSTW-02.
           MOVE JMSTD-04       TO JMSTW-04.
           MOVE JMSTD-10       TO JMSTW-10.
           MOVE JMSTD-05       TO JMSTW-05.
           MOVE JMSTD-09       TO JMSTW-09.
           MOVE JMSTD-1111(01) TO JMSTW-1111(01).
           MOVE JMSTD-1111(02) TO JMSTW-1111(02).
           MOVE JMSTD-1111(03) TO JMSTW-1111(03).
           MOVE JMSTD-1111(04) TO JMSTW-1111(04).
           MOVE JMSTD-1111(05) TO JMSTW-1111(05).
           MOVE JMSTD-1111(06) TO JMSTW-1111(06).
           MOVE JMSTD-1111(07) TO JMSTW-1111(07).
           MOVE JMSTD-1111(08) TO JMSTW-1111(08).
           MOVE JMSTD-1111(09) TO JMSTW-1111(09).
           MOVE JMSTD-1111(10) TO JMSTW-1111(10).
           COMPUTE JMSTW-112 = JMSTD-1111(01) + JMSTD-1111(02)
                              + JMSTD-1111(03) + JMSTD-1111(04)
                              + JMSTD-1111(05) + JMSTD-1111(06)
                              + JMSTD-1111(07) + JMSTD-1111(08)
                              + JMSTD-1111(09) + JMSTD-1111(10).
           MOVE JMSTD-16       TO JMSTW-16.
           MOVE JMSTD-06       TO JMSTW-06.
           MOVE JMSTD-17       TO JMSTW-17.
           MOVE JMSTD-22       TO JMSTW-22.
           MOVE JMSTD-13       TO JMSTW-13.
           MOVE T-NAME         TO JMSTW-31.
           MOVE TC-NAME        TO JMSTW-32.
           MOVE HI-NAME        TO JMSTW-33.
      *           WRITE JMSTW-R.
      *//////////////
           CALL "DB_Insert" USING
            JMSTW_PNAME1 JMSTW_LNAME JMSTW-R RETURNING RET.
           GO TO M-15.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTW_IDLST JMSTW_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

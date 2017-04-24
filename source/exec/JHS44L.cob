       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS44L.
      *********************************************************
      *    PROGRAM         :  赤ちゃん本舗出荷指図参考リスト  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-ERR              PIC  9(001) VALUE 0.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  F                  PIC  X(05)  VALUE X"1A24212474".
           02  F                  PIC  X(34)  VALUE  SPACE.
           02  F                  PIC  N(26)  VALUE
               "＊＊＊　　赤ちゃん本舗　出荷指図参考リスト　　＊＊＊".
           02  F                  PIC  X(27)  VALUE  SPACE.
           02  F                  PIC  X(05)  VALUE  "DATE.".
           02  H-DATE             PIC  99B99B99.
           02  F                  PIC  X(05)  VALUE  SPACE.
           02  F                  PIC  X(02)  VALUE  "P.".
           02  H-PAGE             PIC ZZ9.
       01  HEAD2.
           02  F                  PIC  X(05)  VALUE X"1A24212078".
           02  F                  PIC  X(05)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "ｺｰﾄﾞ".
           02  F                  PIC  N(10)  VALUE
                                              "直　　送　　先　　名".
           02  F                  PIC  X(27)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "ｺｰﾄﾞ".
           02  F                  PIC  X(03)  VALUE  SPACE.
           02  F                  PIC  N(06)  VALUE "社　店　名　".
           02  F                  PIC  X(69)  VALUE  SPACE.
       01  HEAD3.
           02  F                  PIC  X(18)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "ｺｰﾄﾞ".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  N(06)  VALUE "品　　　名　".
           02  F                  PIC  X(28)  VALUE  SPACE.
           02  F                  PIC  X(01)  VALUE  "1".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "    ".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "    ".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "  SS".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "   S".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "   M".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "   L".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "  LL".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "28.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "29.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "30.0".
           02  F                  PIC  X(14)  VALUE  SPACE.
       01  HEAD4.
           02  F                  PIC  X(61)  VALUE  SPACE.
           02  F                  PIC  X(01)  VALUE  "2".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "12.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "13.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "13.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "14.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "15.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "16.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "17.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "18.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "19.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "20.0".
           02  F                  PIC  X(14)  VALUE  SPACE.
       01  HEAD5.
           02  F                  PIC  X(61)  VALUE  SPACE.
           02  F                  PIC  X(01)  VALUE  "3".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "21.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "21.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "22.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "22.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "23.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "23.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "24.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "24.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "25.0".
           02  F                  PIC  X(20)  VALUE  SPACE.
       01  HEAD6.
           02  F                  PIC  X(61)  VALUE  SPACE.
           02  F                  PIC  X(01)  VALUE  "4".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "24.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "24.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "25.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "25.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "26.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "26.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "27.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "27.5".
           02  F                  PIC  X(16)  VALUE  SPACE.
           02  F                  PIC  N(02)  VALUE "　計".
           02  F                  PIC  X(04)  VALUE  SPACE.
           02  F                  PIC  N(02)  VALUE "合計".
       01  HEAD7.
           02  F                  PIC  X(50)  VALUE
                "･･････････････････････････････････････････････････".
           02  F                  PIC  X(50)  VALUE
                "･･････････････････････････････････････････････････".
           02  F                  PIC  X(36)  VALUE
                "････････････････････････････････････".
       01  HEAD8.
           02  F                  PIC  X(50)  VALUE
                "--------------------------------------------------".
           02  F                  PIC  X(50)  VALUE
                "--------------------------------------------------".
           02  F                  PIC  X(36)  VALUE
                "------------------------------------".
       01  HEAD9.
           02  F                  PIC  X(50)  VALUE
                "==================================================".
           02  F                  PIC  X(50)  VALUE
                "==================================================".
           02  F                  PIC  X(36)  VALUE
                "====================================".
       01  W-P1.
           02  P-TCD              PIC 9(04).
           02  P-V                PIC X(01).
           02  P-CCD              PIC 9(03).
           02  F                  PIC X(01).
           02  P-CNA              PIC N(26).
           02  F                  PIC X(02).
           02  P-STC              PIC 9(07).
           02  F                  PIC X(01).
           02  P-NHSN             PIC N(16).
           02  F                  PIC X(59).
       01  W-P2.
           02  F                  PIC X(17).
           02  P-HCD              PIC 9(06).
           02  F                  PIC X(01).
           02  P-HNA              PIC N(24).
           02  F                  PIC X(01).
           02  P-SIZ              PIC 9(01).
           02  P-ASUD.
             03  P-SUD   OCCURS  10.
               04  P-SU           PIC --,---.
           02  P-SUT              PIC ---,---.
           02  P-TSU              PIC ---,---.
       01  W-DATA.
           02  W-PAGE         PIC  9(003).
           02  CNT            PIC  9(002).
           02  W-CC           PIC  9(001).
           02  W-GC           PIC  9(001).
           02  W-HC           PIC  9(001).
           02  W-CCD          PIC  9(003).
           02  W-STC          PIC  9(007).
           02  W-ISU          PIC  9(003).
           02  W-HCD          PIC  9(006).
           02  W-SUT          PIC  9(006).
           02  W-TSU          PIC  9(006).
      *
           COPY LIHIM2.
           COPY LITCM.
           COPY LIAHNH.
           COPY LSPF.
      *FD  SHAW
       01  SHAW_JHS44L.
           02  SHAW_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SHAW_LNAME     PIC  X(011) VALUE "SHAW_JHS44L".
           02  F              PIC  X(001).
           02  SHAW_KEY1      PIC  X(100) VALUE SPACE.
           02  SHAW_SORT      PIC  X(100) VALUE SPACE.
           02  SHAW_IDLST     PIC  X(100) VALUE SPACE.
           02  SHAW_RES       USAGE  POINTER.
       01  SHAW-R.
           02  SHAW-DNGP      PIC  9(008).
           02  SHAW-CSC.
             03  SHAW-TCD     PIC  9(004).
             03  SHAW-CCD     PIC  9(003).
           02  SHAW-STC       PIC  9(007).
           02  SHAW-ISU       PIC  9(003).
           02  SHAW-HCD       PIC  9(006).
           02  SHAW-SIZ       PIC  9(001).
           02  SHAW-ASU.
             03  SHAW-SUD   OCCURS  10.
               04  SHAW-SU    PIC S9(004).
           02  SHAW-SNG       PIC  9(006).
           02  F              PIC  X(050).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  X(034) VALUE
               "赤ちゃん本舗　出荷指図　参考リスト".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
           COPY LSSEM.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "34" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "RX" "1" "10" "34" " " "C-MID" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "35" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "35" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
       M-10.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO SHAW_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SHAW_PNAME1 " " BY REFERENCE SHAW_IDLST "0".
       M-15.
      *           READ SHAW AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SHAW_PNAME1 BY REFERENCE SHAW-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SHAW_IDLST SHAW_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  ZERO = SHAW-SU(01) AND SHAW-SU(02) AND SHAW-SU(03) AND
                     SHAW-SU(04) AND SHAW-SU(05) AND SHAW-SU(06) AND
                     SHAW-SU(07) AND SHAW-SU(08) AND SHAW-SU(09) AND
                     SHAW-SU(10)
               GO TO M-15
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" AHNHF_PNAME1 "SHARED" BY REFERENCE AHNHF_IDLST "1"
            "AHNH-KEY" BY REFERENCE AHNH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           ACCEPT H-DATE FROM DATE.
           PERFORM S-10 THRU S-15.
       M-17.
           MOVE SHAW-CCD TO W-CCD.
           MOVE 0077 TO TC-TCD.
           MOVE W-CCD TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
               MOVE "＊＊　直送先なし　＊＊" TO TC-NAME
           END-IF
           MOVE 0 TO W-CC.
       M-20.
           MOVE SHAW-STC TO W-STC.
           MOVE W-STC TO AHNH-KEY.
      *           READ AHNHF UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" AHNHF_PNAME1 BY REFERENCE AHNH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO AHNH-NHSN
               MOVE "＊＊　店名なし　＊＊" TO AHNH-NHSN
           END-IF.
       M-25.
           MOVE SHAW-ISU TO W-ISU.
           MOVE ZERO TO W-TSU W-GC.
      *
           MOVE SPACE TO W-P1.
           MOVE 0077 TO P-TCD.
           MOVE "-" TO P-V.
           MOVE W-CCD TO P-CCD.
           MOVE TC-NAME TO P-CNA.
           MOVE W-STC TO P-STC.
           MOVE AHNH-NHSN TO P-NHSN.
           PERFORM S-17 THRU S-18.
       M-30.
           MOVE SHAW-HCD TO W-HCD.
           MOVE W-HCD TO HI-MHCD HI-HCD.
      *           READ HI2-M UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "＊＊　品名なし　＊＊" TO HI-NAME
           END-IF
           MOVE 0 TO W-HC.
       M-32.
           PERFORM S-20 THRU S-30.
       M-35.
      *           READ SHAW AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SHAW_PNAME1 BY REFERENCE SHAW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  ZERO = SHAW-SU(01) AND SHAW-SU(02) AND SHAW-SU(03) AND
                     SHAW-SU(04) AND SHAW-SU(05) AND SHAW-SU(06) AND
                     SHAW-SU(07) AND SHAW-SU(08) AND SHAW-SU(09) AND
                     SHAW-SU(10)
               GO TO M-35
           END-IF
           IF  SHAW-CCD NOT = W-CCD
               GO TO M-50
           END-IF
           IF  SHAW-STC NOT = W-STC
               GO TO M-45
           END-IF
           IF  SHAW-ISU NOT = W-ISU
               GO TO M-40
           END-IF
           IF  W-GC = 6
               GO TO M-40
           END-IF
           PERFORM S-35 THRU S-40.
           IF  SHAW-HCD NOT = W-HCD
               GO TO M-30
           ELSE
               GO TO M-32
           END-IF.
       M-40.
           MOVE W-TSU TO P-TSU.
           PERFORM S-35 THRU S-40.
           MOVE SPACE TO SP-R.
           MOVE HEAD7 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO M-25.
       M-45.
           MOVE W-TSU TO P-TSU.
           PERFORM S-35 THRU S-40.
           MOVE SPACE TO SP-R.
           MOVE HEAD8 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO M-20.
       M-50.
           MOVE W-TSU TO P-TSU.
           PERFORM S-35 THRU S-40.
           MOVE SPACE TO SP-R.
           MOVE HEAD9 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO M-17.
       M-90.
           MOVE W-TSU TO P-TSU.
           PERFORM S-35 THRU S-40.
           MOVE SPACE TO SP-R.
           MOVE HEAD9 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "DB_F_Close" USING
            BY REFERENCE SHAW_IDLST SHAW_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE AHNHF_IDLST AHNHF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD4 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD6 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-17.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-18.
           EXIT.
       S-20.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA.
           IF  W-HC = 0
               MOVE 1 TO W-HC
               MOVE W-HCD TO P-HCD
               MOVE HI-NAME TO P-HNA
           END-IF
           MOVE SHAW-SIZ TO P-SIZ.
           MOVE ZERO TO CNT W-SUT.
       S-25.
           ADD 1 TO CNT.
           IF  CNT < 11
               MOVE SHAW-SU(CNT) TO P-SU(CNT)
               ADD SHAW-SU(CNT) TO W-SUT
               GO TO S-25
           END-IF
           MOVE W-SUT TO P-SUT.
           ADD W-SUT TO W-TSU.
           ADD 1 TO W-GC.
       S-30.
           EXIT.
       S-35.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-HCD TO P-HCD
               MOVE HI-NAME TO P-HNA
               PERFORM S-05 THRU S-15
               PERFORM S-17 THRU S-18
           END-IF
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-40.
           EXIT.

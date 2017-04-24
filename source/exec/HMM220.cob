       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMM220.
      *********************************************************
      *    PROGRAM         :  直送先　名簿　　　　　　　　    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :                                  *
      *        変更　　　  :  62/03/27                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       01  HEAD11.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(038) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  H-PM11A        PIC  N(008).
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  X(036) VALUE SPACE.
           02  H-PM11B        PIC  X(025).
       01  HEAD12.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(034) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  H-PM12A        PIC  N(008).
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  X(030) VALUE SPACE.
           02  H-PM12B        PIC  X(025).
       01  HEAD21.
           02  F              PIC  X(015) VALUE "  コード       ".
           02  H-PM21         PIC  X(117).
       01  HEAD22.
           02  F              PIC  X(005) VALUE SPACE.
           02  H-PM22         PIC  X(117).
       01  W-P1.
           02  P-15K1         PIC  X(005).
           02  F              PIC  X(001).
           02  P-TCD1         PIC  9(004).
           02  F              PIC  X(001).
           02  P-CCD1         PIC  9(003).
           02  F              PIC  X(002).
           02  P-PM1          PIC  X(154).
       01  W-P2.
           02  P-15K2         PIC  X(005).
           02  F              PIC  X(001).
           02  P-PM2          PIC  X(154).
       01  W-PM.
           02  W-PMH1A        PIC  N(008) VALUE "履物直送先　名簿".
           02  W-PMH1B.
             03  F            PIC  N(003) VALUE "作成日".
             03  H-DATE       PIC B99B99B99.
             03  F            PIC  X(007) VALUE "     P.".
             03  H-PAGE       PIC ZZ9.
           02  W-PMH2.
             03  F            PIC  N(010) VALUE
                  "直　　送　　先　　名".
             03  F            PIC  X(014) VALUE SPACE.
             03  F            PIC  N(004) VALUE "郵便番号".
             03  F            PIC  X(015) VALUE SPACE.
             03  F            PIC  N(010) VALUE
                  "住　　　　　　　　所".
             03  F            PIC  X(024) VALUE SPACE.
             03  F            PIC  X(014) VALUE " 電 話 番 号  ".
             03  F            PIC  X(002) VALUE SPACE.
           02  W-PMD.
             03  P-15K2D        PIC  X(005).
             03  P-CNA        PIC  N(024).
             03  F            PIC  X(002).
             03  P-UNO        PIC  X(008).
             03  F            PIC  X(002).
             03  P-ZU         PIC  N(024).
             03  F            PIC  X(002).
             03  P-ZS         PIC  N(012).
             03  F            PIC  X(002).
             03  P-TEL        PIC  X(014).
       01  W-D.
           02  W-DMM          PIC  9(001).
           02  W-PC           PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-SEKEY.
             03  W-SKEY       PIC  9(004).
             03  W-EKEY       PIC  9(004).
           02  W-PAGE         PIC  9(003).
           02  CNT            PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LITCM.
      *FD  SP-F1
       77  SP-R1              PIC  X(206).
      *FD  SP-F2
       77  SP-R2              PIC  X(206).
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊　　直送先　名簿　　＊＊＊".
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(034) VALUE
                "得意先ｺｰﾄﾞ      より      迄打出し".
           02  FILLER  PIC  X(036) VALUE
                "直送先ｺｰﾄﾞ  PRINT  ｽﾙ=1 ｼﾅｲ=5   ﾘﾀｰﾝ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SKEY  PIC  9(004).
             03  A-EKEY  PIC  9(004).
           02  A-PC    PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  TC-M ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "316" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "32" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "32" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "32" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "32" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "32" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "32" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "32" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "9" "34" "07C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "15" "8" "36" "08C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "20" "15" "22" "09C-MID" " "
            RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "10" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "12" "0" "8" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SKEY" "9" "12" "20" "4" " " "01C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SKEY" BY REFERENCE W-SKEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EKEY" "9" "12" "30" "4" "A-SKEY" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EKEY" BY REFERENCE W-EKEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PC" "9" "15" "39" "1" "01C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PC" BY REFERENCE W-PC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "32" "1" "A-PC" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "79" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "79" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-STAT" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-SEKEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SKEY "A-SKEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-EKEY "A-EKEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-SKEY > W-EKEY
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-PC "A-PC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-PC NOT = 1 AND 5
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
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-25
           END-IF.
       M-30.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           MOVE ZERO TO TC-KEY.
           MOVE W-SKEY TO TC-TCD.
      *           START TC-M KEY NOT < TC-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TC-M_PNAME1 "TC-KEY" " NOT < " TC-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *           READ TC-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  TC-TCD > W-EKEY
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  TC-TCD = 9999
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           ACCEPT H-DATE FROM DATE.
           IF  W-PC = 1
               CALL "PR_Open_Assign" USING
                "P1-999-FHM220" RETURNING RESP
               MOVE W-PMH1A TO H-PM11A
               MOVE W-PMH2 TO H-PM21
           ELSE
               CALL "PR_Open_Assign" USING
                "P2-999-FHM230" RETURNING RESP
               MOVE W-PMH1A TO H-PM12A
               MOVE W-PMH2 TO H-PM22
           END-IF
           MOVE ZERO TO W-PAGE W-DMM.
           PERFORM S-10 THRU S-15.
       M-35.
           ADD 1 TO CNT.
           IF  CNT < 60
               GO TO M-40
           END-IF
           PERFORM S-05 THRU S-15.
           GO TO M-35.
       M-40.
           MOVE TC-TCD TO W-TCD.
           MOVE SPACE TO W-PMD.
           MOVE W-15K TO P-15K2D.
           MOVE TC-NAME TO P-CNA.
           MOVE TC-UNO TO P-UNO.
           MOVE TC-JSU TO P-ZU.
           MOVE TC-JSS TO P-ZS.
           MOVE TC-TEL TO P-TEL.
           IF  W-PC NOT = 1
               MOVE SPACE TO W-P2
               MOVE W-15K TO P-15K2
               MOVE W-PMD TO P-PM2
               MOVE W-P2 TO SP-R2
               CALL "PR_Write" USING SP-R2 RETURNING RESP
               MOVE SPACE TO SP-R2
               GO TO M-45
           END-IF
           MOVE SPACE TO W-P1.
           MOVE W-15K TO P-15K1.
           IF  W-DMM = 0
               MOVE 5 TO W-DMM
               MOVE TC-TCD TO P-TCD1
           END-IF
           MOVE TC-CCD TO P-CCD1.
           MOVE W-PMD TO P-PM1.
           MOVE W-P1 TO SP-R1.
           CALL "PR_Write" USING SP-R1 RETURNING RESP.
           MOVE SPACE TO SP-R1.
       M-45.
      *           READ TC-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  TC-TCD > W-EKEY
               GO TO M-90
           END-IF
           IF  TC-TCD = 9999
               GO TO M-90
           END-IF
           IF  TC-TCD NOT = W-TCD
               MOVE 0 TO W-DMM
           END-IF
           GO TO M-35.
       M-90.
           IF  W-PC = 1
               CALL "PR_Close" RETURNING RESP
           ELSE
               CALL "PR_Close" RETURNING RESP
           END-IF.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           IF  W-PC = 1
               MOVE SPACE TO SP-R1
               CALL "PR_Write" USING SP-R1 RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           ELSE
               MOVE SPACE TO SP-R2
               CALL "PR_Write" USING SP-R2 RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF.
       S-10.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           IF  W-PC = 1
               MOVE W-PMH1B TO H-PM11B
               MOVE SPACE TO SP-R1
               MOVE HEAD11 TO SP-R1
               CALL "PR_Write" USING SP-R1 RETURNING RESP
               MOVE SPACE TO SP-R1
               MOVE HEAD21 TO SP-R1
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R1 RETURNING RESP
               MOVE SPACE TO SP-R1
           ELSE
               MOVE W-PMH1B TO H-PM12B
               MOVE SPACE TO SP-R2
               MOVE HEAD12 TO SP-R2
               CALL "PR_Write" USING SP-R2 RETURNING RESP
               MOVE SPACE TO SP-R2
               MOVE HEAD22 TO SP-R2
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R2 RETURNING RESP
               MOVE SPACE TO SP-R2
           END-IF
           MOVE ZERO TO CNT W-DMM.
       S-15.
           EXIT.

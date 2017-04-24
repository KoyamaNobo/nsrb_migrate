       IDENTIFICATION DIVISION.
       PROGRAM-ID. JTN99L.
      *********************************************************
      *    PROGRAM         :  ＳＴＮ№日付別　指図入力枚数表  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  W-END              PIC  9(001) VALUE 0.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0064".
           02  W-FID12        PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  H-NEN          PIC  9(004).
           02  F              PIC  N(001) VALUE "年".
           02  H-GET          PIC  Z(002).
           02  F              PIC  N(002) VALUE "月分".
           02  F              PIC  X(024) VALUE SPACE.
           02  F              PIC  N(023) VALUE
                "＊＊＊　　画面日付別　指図入力枚数表　　＊＊＊".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(004) VALUE "画面№".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "分類".
           02  F              PIC  X(006) VALUE SPACE.
           02  H-APD.
             03  H-PD    OCCURS  25.
               04  F          PIC  X(002).
               04  H-PEY      PIC  Z(002).
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "合計".
       01  W-P.
           02  P-STN          PIC  N(004).
           02  F              PIC  X(001).
           02  P-ME           PIC  N(006).
           02  P-KSU.
             03  P-ASU   OCCURS  25.
               04  P-SU       PIC  Z(004).
           02  P-SUT          PIC ZZZ,ZZZ.
       01  W-DATA.
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-GET        PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-STN          PIC  X(003).
           02  W-APD.
             03  W-PD    OCCURS  25.
               04  W-PEY      PIC  9(002).
           02  W-ADD.
             03  W-AD    OCCURS  31.
               04  W-IP       PIC  9(004).
               04  W-WA       PIC  9(004).
               04  W-KY       PIC  9(004).
               04  W-GO       PIC  9(004).
           02  CNT            PIC  9(002).
           02  W-C            PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-SUT          PIC  9(006).
           02  W-ADDT.
             03  W-ADT   OCCURS  31.
               04  W-TIP      PIC  9(004).
               04  W-TWA      PIC  9(004).
               04  W-TKY      PIC  9(004).
               04  W-TGO      PIC  9(004).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LSPF.
      *FD  SHSSF
       01  SHSSF_JTN99L.
           02  SHSSF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SHSSF_LNAME    PIC  X(012) VALUE "SHSSF_JTN99L".
           02  F              PIC  X(001).
           02  SHSSF_KEY1     PIC  X(100) VALUE SPACE.
           02  SHSSF_SORT     PIC  X(100) VALUE SPACE.
           02  SHSSF_IDLST    PIC  X(100) VALUE SPACE.
           02  SHSSF_RES      USAGE  POINTER.
       01  SHSS-R.
           02  SHSS-DNO       PIC  9(06).
           02  SHSS-DC        PIC  9(01).
           02  SHSS-NG        PIC  9(06).
           02  SHSS-PEY       PIC  9(02).
           02  SHSS-TCD       PIC  9(04).
           02  SHSS-NC        PIC  9(01).
           02  SHSS-STN       PIC  X(03).
           02  F              PIC  X(41).
       77  F                  PIC  X(01).
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
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　ＳＴＮ№日付別　指図入力枚数表　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(021) VALUE
                  "【      年   月分　】".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(01).
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC  9(04).
             03  FILLER  PIC  Z(02).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "**  DATA ﾅｼ  **".
             03  E-ME9   PIC  X(016) VALUE
                  "**  DATA ｴﾗｰ  **".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "393" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "13" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "13" "50" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "13" "50" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "13" "50" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "13" "50" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "13" "50" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "13" "50" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "28" "21" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "22" "30" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "47" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NG" " " "12" "0" "6" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NG" "9" "12" "32" "4" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-NEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NG" "Z" "12" "39" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-GET "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "41" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "41" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "16" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12.
           MOVE W-FID1 TO WK0064ID.
           MOVE WK0064ID TO SHSSF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SHSSF_PNAME1 " " BY REFERENCE SHSSF_IDLST "0".
      *           READ SHSSF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SHSSF_PNAME1 BY REFERENCE SHSS-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SHSSF_IDLST SHSSF_PNAME1
               GO TO M-95
           END-IF
           MOVE SHSS-NG TO W-NG.
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "DB_F_Close" USING
                BY REFERENCE SHSSF_IDLST SHSSF_PNAME1
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF.
       M-15.
           IF  SHSS-DNO < 100000
               ADD 1 TO W-TKY(SHSS-PEY)
           ELSE
               IF  SHSS-NC = 0
                   ADD 1 TO W-TIP(SHSS-PEY)
               ELSE
                   ADD 1 TO W-TWA(SHSS-PEY)
               END-IF
           END-IF
           ADD 1 TO W-TGO(SHSS-PEY).
      *
      *           READ SHSSF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SHSSF_PNAME1 BY REFERENCE SHSS-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF
           GO TO M-15.
       M-20.
           CALL "DB_F_Close" USING
            BY REFERENCE SHSSF_IDLST SHSSF_PNAME1.
      *
           MOVE SPACE TO H-APD.
           MOVE ZERO TO W-APD CNT W-C.
       M-25.
           ADD 1 TO CNT.
           IF  CNT > 31
               GO TO M-30
           END-IF
           IF  W-TGO(CNT) = ZERO
               GO TO M-25
           END-IF
           ADD 1 TO W-C.
           IF  W-C > 25
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE CNT TO H-PEY(W-C) W-PEY(W-C).
           GO TO M-25.
       M-30.
           MOVE W-NEN TO H-NEN.
           MOVE W-GET TO H-GET.
           ACCEPT H-DATE FROM DATE.
           CALL "PR_Open" RETURNING RESP.
      *
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           CALL "DB_F_Open" USING
            "INPUT" SHSSF_PNAME1 " " BY REFERENCE SHSSF_IDLST "0".
      *           READ SHSSF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SHSSF_PNAME1 BY REFERENCE SHSS-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SHSSF_IDLST SHSSF_PNAME1
               GO TO M-95
           END-IF.
       M-35.
           MOVE SHSS-STN TO W-STN.
           MOVE ZERO TO W-ADD CNT W-C.
       M-40.
           IF  SHSS-DNO < 100000
               ADD 1 TO W-KY(SHSS-PEY)
           ELSE
               IF  SHSS-NC = 0
                   ADD 1 TO W-IP(SHSS-PEY)
               ELSE
                   ADD 1 TO W-WA(SHSS-PEY)
               END-IF
           END-IF
           ADD 1 TO W-GO(SHSS-PEY).
      *           READ SHSSF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SHSSF_PNAME1 BY REFERENCE SHSS-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF
           IF  SHSS-STN = W-STN
               GO TO M-40
           END-IF
           PERFORM S-05 THRU S-50.
           GO TO M-35.
       M-45.
           PERFORM S-05 THRU S-50.
           MOVE ZERO TO W-ADD.
           MOVE W-ADDT TO W-ADD.
           MOVE 1 TO W-END.
           PERFORM S-05 THRU S-50.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE SHSSF_IDLST SHSSF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE ZERO TO W-SUT CNT.
       S-06.
           ADD 1 TO CNT.
           IF  CNT < 32
               ADD W-IP(CNT) TO W-SUT
               GO TO S-06
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-STN.
           IF  W-END = 0
               MOVE W-STN TO P-STN
           ELSE
               MOVE "総合計　" TO P-STN
           END-IF
           MOVE "一般・ワーク" TO P-ME.
           MOVE W-SUT TO P-SUT.
           MOVE ZERO TO CNT W-C.
       S-10.
           ADD 1 TO CNT.
           IF  CNT > 31
               GO TO S-15
           END-IF
           IF  W-TGO(CNT) = ZERO
               GO TO S-10
           END-IF
           ADD 1 TO W-C.
           IF  W-C > 25
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE W-IP(CNT) TO P-SU(W-C).
           GO TO S-10.
       S-15.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE ZERO TO W-SUT CNT.
       S-16.
           ADD 1 TO CNT.
           IF  CNT < 32
               ADD W-WA(CNT) TO W-SUT
               GO TO S-16
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-STN.
           MOVE "ＥＯＳ他　　" TO P-ME.
           MOVE W-SUT TO P-SUT.
           MOVE ZERO TO CNT W-C.
       S-20.
           ADD 1 TO CNT.
           IF  CNT > 31
               GO TO S-25
           END-IF
           IF  W-TGO(CNT) = ZERO
               GO TO S-20
           END-IF
           ADD 1 TO W-C.
           IF  W-C > 25
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE W-WA(CNT) TO P-SU(W-C).
           GO TO S-20.
       S-25.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE ZERO TO W-SUT CNT.
       S-26.
           ADD 1 TO CNT.
           IF  CNT < 32
               ADD W-KY(CNT) TO W-SUT
               GO TO S-26
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-STN.
           MOVE "教　　育　　" TO P-ME.
           MOVE W-SUT TO P-SUT.
           MOVE ZERO TO CNT W-C.
       S-30.
           ADD 1 TO CNT.
           IF  CNT > 31
               GO TO S-35
           END-IF
           IF  W-TGO(CNT) = ZERO
               GO TO S-30
           END-IF
           ADD 1 TO W-C.
           IF  W-C > 25
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE W-KY(CNT) TO P-SU(W-C).
           GO TO S-30.
       S-35.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE ZERO TO W-SUT CNT.
       S-36.
           ADD 1 TO CNT.
           IF  CNT < 32
               ADD W-GO(CNT) TO W-SUT
               GO TO S-36
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-STN.
           MOVE "合　　計　　" TO P-ME.
           MOVE W-SUT TO P-SUT.
           MOVE ZERO TO CNT W-C.
       S-40.
           ADD 1 TO CNT.
           IF  CNT > 31
               GO TO S-45
           END-IF
           IF  W-TGO(CNT) = ZERO
               GO TO S-40
           END-IF
           ADD 1 TO W-C.
           IF  W-C > 25
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE W-GO(CNT) TO P-SU(W-C).
           GO TO S-40.
       S-45.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-50.
           EXIT.

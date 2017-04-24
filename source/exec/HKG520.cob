       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG520.
      *******************************
      *    日付別　入金予定表   　  *
      *******************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-PAGE             PIC  9(002) VALUE ZERO.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  ERR-STAT           PIC  X(002).
       01  W-FILE             PIC  X(013).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(038) VALUE SPACE.
           02  F              PIC  N(019) VALUE
                "＊＊＊　　日付別　入金予定表　　＊＊＊".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(004) VALUE "入金予定".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　商品代".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "合計金額".
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  N(004) VALUE "手形期日".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　請求日".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "担当".
       01  W-P.
           02  F              PIC  X(001).
           02  P-GET          PIC Z9.
           02  P-S1           PIC  X(001).
           02  P-PEY          PIC Z9.
           02  F              PIC  X(002).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  P-KIN          PIC ZZZZ,ZZZ,ZZ9.
           02  P-SHZ          PIC ZZ,ZZZ,ZZ9.
           02  P-KEI          PIC ZZZZ,ZZZ,ZZ9.
           02  F              PIC  X(002).
           02  P-NS1          PIC  N(004).
           02  P-NS2          PIC  N(004).
           02  F              PIC  X(002).
           02  P-TD           PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-SG           PIC Z9.
           02  P-S2           PIC  X(001).
           02  P-SP           PIC Z9.
           02  F              PIC  X(002).
           02  P-TNC          PIC  9(002).
       01  W-DATA.
           02  W-SNGP.
             03  W-SNEN       PIC  9(004).
             03  W-SNENL REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-ENGP.
             03  W-ENEN       PIC  9(004).
             03  W-ENENL REDEFINES W-ENEN.
               04  W-ENEN1    PIC  9(002).
               04  W-ENEN2    PIC  9(002).
             03  W-EGET       PIC  9(002).
             03  W-EPEY       PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-MC           PIC  9(001).
           02  W-KC           PIC  9(001).
           02  W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  WN-D.
             03  WN-KIN       PIC S9(009).
             03  WN-SHZ       PIC S9(007).
             03  WN-KEI       PIC S9(009).
           02  WS-D.
             03  WS-KIN       PIC S9(009).
             03  WS-SHZ       PIC S9(007).
             03  WS-KEI       PIC S9(009).
           02  WA-D.
             03  WA-KIN       PIC S9(009).
             03  WA-SHZ       PIC S9(007).
             03  WA-KEI       PIC S9(009).
           COPY LSTAT.
      *
           COPY LITM.
           COPY LSPF.
      *FD  NYWF
       01  NYWF_HKG520.
           02  NYWF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  NYWF_LNAME     PIC  X(011) VALUE "NYWF_HKG520".
           02  F              PIC  X(001).
           02  NYWF_KEY1      PIC  X(100) VALUE SPACE.
           02  NYWF_SORT      PIC  X(100) VALUE SPACE.
           02  NYWF_IDLST     PIC  X(100) VALUE SPACE.
           02  NYWF_RES       USAGE  POINTER.
       01  NYW-R.
           02  NYW-DATE       PIC  9(008).
           02  NYW-NGP   REDEFINES NYW-DATE.
             03  NYW-NEN      PIC  9(004).
             03  NYW-GET      PIC  9(002).
             03  NYW-PEY      PIC  9(002).
           02  NYW-TCD        PIC  9(004).
           02  NYW-KIN        PIC S9(009).
           02  NYW-SHZ        PIC S9(007).
           02  NYW-KEI        PIC S9(009).
           02  NYW-NC.
             03  NYW-NC1      PIC  9(001).
             03  NYW-NC2      PIC  9(001).
           02  NYW-TGK        PIC  9(008).
           02  NYW-TGKD  REDEFINES NYW-TGK.
             03  F            PIC  9(002).
             03  NYW-TGKS     PIC  9(006).
           02  NYW-SS         PIC  9(008).
           02  NYW-SSD   REDEFINES NYW-SS.
             03  NYW-SNEN     PIC  9(004).
             03  NYW-SGET     PIC  9(002).
             03  NYW-SPEY     PIC  9(002).
           02  NYW-TNC        PIC  9(002).
           02  F              PIC  X(007).
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　日付別　入金予定表　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(026) VALUE
                "'0000.00.00 ～ '9999.99.99".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SNEN  PIC  9(004).
             03  A-SGET  PIC  9(002).
             03  A-SPEY  PIC  9(002).
             03  A-ENEN  PIC  9(004).
             03  A-EGET  PIC  9(002).
             03  A-EPEY  PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "314" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "38" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "38" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "38" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "38" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "38" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "38" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "38" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "14" "16" "26" "07C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "18" "22" "08C-MID" " "  RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "17" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "14" "0" "16" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNEN" "9" "14" "17" "4" " " "01C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNEN" BY REFERENCE W-SNEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SGET" "9" "14" "22" "2" "A-SNEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SPEY" "9" "14" "25" "2" "A-SGET" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENEN" "9" "14" "32" "4" "A-SPEY" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENEN" BY REFERENCE W-ENEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EGET" "9" "14" "37" "2" "A-ENEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EPEY" "9" "14" "40" "2" "A-EGET" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "35" "1" "01C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "17" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "17" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           MOVE 99999999 TO W-ENGP.
           GO TO M-40.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-SNEN = ZERO
               IF  W-SGET NOT = ZERO
                   GO TO M-15
               END-IF
           END-IF
           IF  W-SNEN NOT = ZERO
               IF  W-SGET < 1 OR > 12
                   GO TO M-15
               END-IF
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-SPEY "A-SPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-SGET = ZERO
               IF  W-SPEY NOT = ZERO
                   GO TO M-20
               END-IF
           END-IF
           IF  W-SGET NOT = ZERO
               IF  W-SPEY < 1 OR > 31
                   GO TO M-20
               END-IF
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-ENEN < W-SNEN
               GO TO M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-ENEN = ZERO
               IF  W-EGET NOT = ZERO
                   GO TO M-30
               END-IF
           END-IF
           IF  W-ENEN NOT = ZERO
               IF  W-EGET < 1 OR > 12
                   GO TO M-30
               END-IF
           END-IF.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-EPEY "A-EPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-30
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF
           IF  W-EGET = ZERO
               IF  W-EPEY NOT = ZERO
                  GO TO M-35
               END-IF
           END-IF
           IF  W-EGET NOT = ZERO
               IF  W-EPEY < 1 OR > 31
                   GO TO M-35
               END-IF
           END-IF
           IF  W-SNGP > W-ENGP
               GO TO M-25
           END-IF.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-35
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-40
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-40
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO NYWF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NYWF_PNAME1 " " BY REFERENCE NYWF_IDLST "0".
       M-45.
      *           READ NYWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NYWF_PNAME1 BY REFERENCE NYW-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE NYWF_IDLST NYWF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  NYW-DATE < W-SNGP
               GO TO M-45
           END-IF
           IF  NYW-DATE > W-ENGP
               CALL "DB_F_Close" USING
                BY REFERENCE NYWF_IDLST NYWF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *
           ACCEPT H-DATE FROM DATE.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
       M-50.
           MOVE ZERO TO WS-D.
           MOVE NYW-DATE TO W-NGP.
       M-55.
           MOVE ZERO TO WN-D W-MC W-KC.
           MOVE NYW-PEY TO W-PEY.
       M-60.
           MOVE NYW-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "＊＊　得意先なし　＊＊" TO T-NAME
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA P-NS1 P-NS2.
           IF  W-MC = 0
               MOVE 1 TO W-MC
               MOVE NYW-GET TO P-GET
               MOVE "/" TO P-S1
               MOVE NYW-PEY TO P-PEY
           END-IF
           MOVE NYW-TCD TO P-TCD.
           MOVE T-NAME TO P-TNA.
           MOVE NYW-KIN TO P-KIN.
           MOVE NYW-SHZ TO P-SHZ.
           MOVE NYW-KEI TO P-KEI.
           IF  NYW-NC1 = 1
               MOVE "現　金　" TO P-NS1
           END-IF
           IF  NYW-NC1 = 2
               MOVE "小切手　" TO P-NS1
           END-IF
           IF  NYW-NC1 = 3
               MOVE "手　形　" TO P-NS1
           END-IF
           IF  NYW-NC2 = 1
               MOVE "現　金　" TO P-NS2
           END-IF
           IF  NYW-NC2 = 2
               MOVE "小切手　" TO P-NS2
           END-IF
           IF  NYW-NC2 = 3
               MOVE "手　形　" TO P-NS2
           END-IF
           IF  NYW-TGK NOT = ZERO
               MOVE NYW-TGKS TO P-TD
           END-IF
           MOVE NYW-SGET TO P-SG.
           MOVE "/" TO P-S2.
           MOVE NYW-SPEY TO P-SP.
           MOVE NYW-TNC TO P-TNC.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE NYW-GET TO P-GET
               MOVE "/" TO P-S1
               MOVE NYW-PEY TO P-PEY
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD NYW-KIN TO WN-KIN.
           ADD NYW-SHZ TO WN-SHZ.
           ADD NYW-KEI TO WN-KEI.
           IF  W-KC = 1
               MOVE 2 TO W-KC
           END-IF
           IF  W-KC = 0
               MOVE 1 TO W-KC
           END-IF.
       M-65.
      *           READ NYWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NYWF_PNAME1 BY REFERENCE NYW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  NYW-DATE > W-ENGP
               GO TO M-85
           END-IF
           IF  NYW-GET NOT = W-GET
               GO TO M-70
           END-IF
           IF  NYW-PEY = W-PEY
               GO TO M-60
           END-IF
           PERFORM S-20 THRU S-30.
           GO TO M-55.
       M-70.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-40.
           GO TO M-50.
       M-85.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-40.
           PERFORM S-45 THRU S-50.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE NYWF_IDLST NYWF_PNAME1.
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
           MOVE W-PAGE  TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           IF  W-KC < 2
               GO TO S-25
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA P-NS1 P-NS2.
           MOVE "　　　　　　　　　　　　　　　（　計　）" TO P-TNA.
           MOVE WN-KIN TO P-KIN.
           MOVE WN-SHZ TO P-SHZ.
           MOVE WN-KEI TO P-KEI.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE NYW-GET TO P-GET
               MOVE "/" TO P-S1
               MOVE NYW-PEY TO P-PEY
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-25.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WN-KIN TO WS-KIN.
           ADD WN-SHZ TO WS-SHZ.
           ADD WN-KEI TO WS-KEI.
       S-30.
           EXIT.
       S-35.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA P-NS1 P-NS2.
           MOVE "　　　　　　　　　　［　小　計　］　　　" TO P-TNA.
           MOVE WS-KIN TO P-KIN.
           MOVE WS-SHZ TO P-SHZ.
           MOVE WS-KEI TO P-KEI.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-KIN TO WA-KIN.
           ADD WS-SHZ TO WA-SHZ.
           ADD WS-KEI TO WA-KEI.
       S-40.
           EXIT.
       S-45.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA P-NS1 P-NS2.
           MOVE "　　　　　【　総　合　計　】　　　　　　" TO P-TNA.
           MOVE WA-KIN TO P-KIN.
           MOVE WA-SHZ TO P-SHZ.
           MOVE WA-KEI TO P-KEI.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-50.
           EXIT.

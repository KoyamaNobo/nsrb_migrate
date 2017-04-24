       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD950.
      ***************************************************************
      *    PROGRAM         :  履物　販売日計表                　　  *
      *    PRINTER TYPE    :  JIPS                                  *
      *    SCREEN          :  ******                                *
      *        変更　　　  :  62/04/15                              *
      *    COMPILE TYPE    :  COBOL                                 *
      ***************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(039) VALUE SPACE.
           02  F              PIC  N(018) VALUE
                "＊＊＊　　履物　販売日計表　　＊＊＊".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  X(008) VALUE "I-----  ".
           02  F              PIC  N(004) VALUE "通常売上".
           02  F              PIC  X(009) VALUE "  ------I".
           02  F              PIC  X(015) VALUE SPACE.
           02  F              PIC  N(002) VALUE "預り".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(008) VALUE "I-----  ".
           02  F              PIC  N(002) VALUE "返品".
           02  F              PIC  X(009) VALUE "  ------I".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  X(008) VALUE "I-----  ".
           02  F              PIC  N(004) VALUE "預り保管".
           02  F              PIC  X(008) VALUE "  -----I".
           02  F              PIC  X(013) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　日付　".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "担当".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　足　数".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "値引金額".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　出荷数".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　足　数".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(006) VALUE "出荷足数合計".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　足　数".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(006) VALUE "売上金額合計".
           02  F              PIC  X(005) VALUE X"1A24212474".
       01  W-P.
           02  P-ATM          PIC  N(004).
           02  P-MD    REDEFINES P-ATM.
             03  F            PIC  X(001).
             03  P-GP         PIC 99/99.
             03  F            PIC  X(002).
           02  P-TM           PIC  N(004).
           02  P-D     REDEFINES P-TM.
             03  F            PIC  X(003).
             03  P-TNC        PIC  9(001).
             03  F            PIC  X(004).
           02  P-URS          PIC --,---,--9.
           02  P-URK          PIC -----,---,--9.
           02  P-NBK          PIC ----,---,--9.
           02  P-AZS          PIC -----,--9.
           02  P-HPS          PIC ----,--9.
           02  P-HPK          PIC ----,---,--9.
           02  P-SUT          PIC --,---,--9.
           02  P-AHS          PIC --,---,--9.
           02  P-AHK          PIC ----,---,--9.
           02  P-KNT          PIC -----,---,--9.
       01  W-DATA.
           02  W-PAGE         PIC  9(002).
           02  W-DATE.
             03  W-SNGP.
               04  W-SNEN     PIC  9(002).
               04  W-SGP.
                 05  W-SGET   PIC  9(002).
                 05  W-SPEY   PIC  9(002).
             03  W-EPEY       PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-C            PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-GP           PIC  9(004).
           02  W-TNC          PIC  9(001).
           02  W-TNCD.
             03  W-TNC1       PIC  9(001).
             03  W-TNC2       PIC  9(001).
           02  W-CC           PIC  9(001).
           02  W-ATM          PIC  N(004).
           02  W-TND   REDEFINES W-ATM.
             03  W-TN         PIC  N(001)  OCCURS  4.
           02  W-TNNA         PIC  N(014).
           02  W-TNA   REDEFINES W-TNNA.
             03  W-NA         PIC  N(001)  OCCURS 14.
           02  W-D.
             03  W-URS        PIC S9(007).
             03  W-URK        PIC S9(010).
             03  W-NBK        PIC S9(009).
             03  W-AZS        PIC S9(006).
             03  W-HPS        PIC S9(006).
             03  W-HPK        PIC S9(009).
             03  W-SUT        PIC S9(007).
             03  W-AHS        PIC S9(007).
             03  W-AHK        PIC S9(009).
             03  W-KNT        PIC S9(010).
       01  WT-AD.
           02  WT-D     OCCURS  10.
             03  WT-URS       PIC S9(007).
             03  WT-URK       PIC S9(010).
             03  WT-NBK       PIC S9(009).
             03  WT-AZS       PIC S9(006).
             03  WT-HPS       PIC S9(006).
             03  WT-HPK       PIC S9(009).
             03  WT-SUT       PIC S9(007).
             03  WT-AHS       PIC S9(007).
             03  WT-AHK       PIC S9(009).
             03  WT-KNT       PIC S9(010).
       01  WS-D.
           02  WS-URS         PIC S9(007).
           02  WS-URK         PIC S9(010).
           02  WS-NBK         PIC S9(009).
           02  WS-AZS         PIC S9(006).
           02  WS-HPS         PIC S9(006).
           02  WS-HPK         PIC S9(009).
           02  WS-SUT         PIC S9(007).
           02  WS-AHS         PIC S9(007).
           02  WS-AHK         PIC S9(009).
           02  WS-KNT         PIC S9(010).
       01  WA-D.
           02  WA-URS         PIC S9(007).
           02  WA-URK         PIC S9(010).
           02  WA-NBK         PIC S9(009).
           02  WA-AZS         PIC S9(006).
           02  WA-HPS         PIC S9(006).
           02  WA-HPK         PIC S9(009).
           02  WA-SUT         PIC S9(007).
           02  WA-AHS         PIC S9(007).
           02  WA-AHK         PIC S9(009).
           02  WA-KNT         PIC S9(010).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHKBM.
           COPY LSSNTW.
           COPY LSPF.
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
                "＊＊＊　　履物　販売　日計表　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(032) VALUE
                "ＰＲＩＮＴ  範囲      日 ～   日".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SPEY  PIC  9(002).
             03  A-EPEY  PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
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
            "C-MID" " " "0" "0" "320" " " " "  RETURNING RESU.
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
            "08C-MID" "X" "12" "13" "32" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "18" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "5" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "12" "0" "4" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SPEY" "9" "12" "33" "2" " " "01C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EPEY" "9" "12" "41" "2" "A-SPEY" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "35" "1" "01C-ACP" " "  RETURNING RESU.
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
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE W-SNGP.
           MOVE W-SPEY TO W-EPEY.
           MOVE 1 TO W-SPEY.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SPEY" A-SPEY "p" RETURNING RESU.
           CALL "SD_Output" USING "A-EPEY" A-EPEY "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SPEY "A-SPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ERR-STAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SPEY > 31
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-EPEY "A-EPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-SPEY > W-EPEY
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SNTRF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SNTRF_PNAME1 " " BY REFERENCE SNTRF_IDLST "0".
       M-25.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  SNTR-DC = 8
               GO TO M-25
           END-IF
           IF  ZERO = SNTR-SU AND SNTR-KIN
               GO TO M-25
           END-IF
           IF  SNTR-PEY > W-EPEY
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO WA-D WT-AD W-PAGE.
           PERFORM S-10 THRU S-15.
       M-30.
           MOVE SNTR-GP TO W-GP.
           MOVE ZERO TO WS-D CHK W-C.
       M-35.
           MOVE SNTR-TNC1 TO W-TNC.
           MOVE ZERO TO W-D.
       M-40.
           IF  SNTR-SNC = 1
               SUBTRACT SNTR-KIN FROM W-NBK
               GO TO M-45
           END-IF
           IF  SNTR-DC = 0
               ADD SNTR-KIN TO W-URK
               IF  SNTR-HCD < 999900
                   ADD SNTR-SU TO W-URS
               END-IF
           END-IF
           IF  SNTR-DC = 1
               SUBTRACT SNTR-KIN FROM W-HPK
               IF  SNTR-HCD < 999900
                   SUBTRACT SNTR-SU FROM W-HPS
               END-IF
           END-IF
           IF  SNTR-DC = 2
               SUBTRACT SNTR-KIN FROM W-HPK
           END-IF
           IF  SNTR-DC = 3 OR 9
               ADD SNTR-KIN TO W-AHK
               IF  SNTR-HCD < 999900
                   ADD SNTR-SU TO W-AHS
               END-IF
           END-IF
           IF  SNTR-DC = 4
               IF  SNTR-HCD < 999900
                   ADD SNTR-SU TO W-AZS
               END-IF
           END-IF
           IF  SNTR-DC = 5
               SUBTRACT SNTR-KIN FROM W-URK
               IF  SNTR-HCD < 999900
                   SUBTRACT SNTR-SU FROM W-URS
               END-IF
           END-IF
           IF  SNTR-DC = 6
               ADD SNTR-KIN TO W-URK
           END-IF
           IF  SNTR-DC = 7
               IF  SNTR-HCD < 999900
                   ADD SNTR-SU TO W-URS
               END-IF
           END-IF.
       M-45.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SNTR-DC = 8
               GO TO M-45
           END-IF
           IF  ZERO = SNTR-SU AND SNTR-KIN
               GO TO M-45
           END-IF
           IF  SNTR-PEY > W-EPEY
               GO TO M-90
           END-IF
           IF  W-GP NOT = SNTR-GP
               GO TO M-50
           END-IF
           IF  SNTR-TNC1 = W-TNC
               GO TO M-40
           END-IF
           PERFORM S-20 THRU S-30.
           GO TO M-35.
       M-50.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-45.
           GO TO M-30.
       M-90.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-45.
           PERFORM S-50 THRU S-75.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
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
       S-15.
           EXIT.
       S-20.
           IF  ZERO = W-URS AND W-URK AND W-NBK AND W-AZS AND
                     W-HPS AND W-HPK AND W-AHS AND W-AHK
               GO TO S-30
           END-IF
           COMPUTE W-SUT = W-URS + W-AZS + W-HPS.
           COMPUTE W-KNT = W-URK + W-NBK + W-HPK + W-AHK.
           IF  W-GP < W-SGP
               GO TO S-25
           END-IF
           MOVE SPACE TO W-P.
           IF  CHK = 0
               MOVE 5 TO CHK
               MOVE W-GP TO P-GP
           END-IF
           MOVE W-TNC TO P-TNC.
           MOVE W-URS TO P-URS.
           MOVE W-URK TO P-URK.
           MOVE W-NBK TO P-NBK.
           MOVE W-AZS TO P-AZS.
           MOVE W-HPS TO P-HPS.
           MOVE W-HPK TO P-HPK.
           MOVE W-SUT TO P-SUT.
           MOVE W-AHS TO P-AHS.
           MOVE W-AHK TO P-AHK.
           MOVE W-KNT TO P-KNT.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-GP TO P-GP
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  W-C = 0
               MOVE 1 TO W-C
           END-IF.
       S-25.
           COMPUTE CNT = W-TNC + 1.
           ADD W-URS TO WS-URS WT-URS(CNT).
           ADD W-URK TO WS-URK WT-URK(CNT).
           ADD W-NBK TO WS-NBK WT-NBK(CNT).
           ADD W-AZS TO WS-AZS WT-AZS(CNT).
           ADD W-HPS TO WS-HPS WT-HPS(CNT).
           ADD W-HPK TO WS-HPK WT-HPK(CNT).
           ADD W-SUT TO WS-SUT WT-SUT(CNT).
           ADD W-AHS TO WS-AHS WT-AHS(CNT).
           ADD W-AHK TO WS-AHK WT-AHK(CNT).
           ADD W-KNT TO WS-KNT WT-KNT(CNT).
       S-30.
           EXIT.
       S-35.
           IF  W-GP < W-SGP
               GO TO S-40
           END-IF
           IF  W-C = 0
               GO TO S-40
           END-IF
           MOVE SPACE TO W-P.
           MOVE "（小計）" TO P-TM.
           MOVE WS-URS TO P-URS.
           MOVE WS-URK TO P-URK.
           MOVE WS-NBK TO P-NBK.
           MOVE WS-AZS TO P-AZS.
           MOVE WS-HPS TO P-HPS.
           MOVE WS-HPK TO P-HPK.
           MOVE WS-SUT TO P-SUT.
           MOVE WS-AHS TO P-AHS.
           MOVE WS-AHK TO P-AHK.
           MOVE WS-KNT TO P-KNT.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-GP TO P-GP
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-40.
           ADD WS-URS TO WA-URS.
           ADD WS-URK TO WA-URK.
           ADD WS-NBK TO WA-NBK.
           ADD WS-AZS TO WA-AZS.
           ADD WS-HPS TO WA-HPS.
           ADD WS-HPK TO WA-HPK.
           ADD WS-SUT TO WA-SUT.
           ADD WS-AHS TO WA-AHS.
           ADD WS-AHK TO WA-AHK.
           ADD WS-KNT TO WA-KNT.
       S-45.
           EXIT.
       S-50.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           MOVE ZERO TO CNT CHK.
       S-55.
           ADD 1 TO CNT.
           IF  CNT > 10
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               GO TO S-70
           END-IF
           IF  ZERO = WT-URS(CNT) AND WT-URK(CNT) AND WT-NBK(CNT) AND
                     WT-AZS(CNT) AND WT-HPS(CNT) AND WT-HPK(CNT) AND
                     WT-AHS(CNT) AND WT-AHK(CNT)
               GO TO S-55
           END-IF
           IF  CHK = 0
               MOVE 5 TO CHK
               MOVE SPACE TO W-P
               MOVE "［　累　" TO P-ATM
               MOVE "計　］　" TO P-TM
               MOVE W-P TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           COMPUTE W-TNC = CNT - 1.
           MOVE SPACE TO W-ATM.
           MOVE ZERO TO W-TNCD.
           MOVE W-TNC TO W-TNC1.
           MOVE SPACE TO HKB-KEY.
           MOVE "04" TO HKB-NO.
           MOVE W-TNCD TO HKB-TNC.
      *           START HKBM KEY NOT < HKB-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HKBM_PNAME1 "HKB-KEY" " NOT < " HKB-KEY RETURNING RET.
           IF  RET = 1
               GO TO S-65
           END-IF
      *           READ HKBM NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO S-65
           END-IF
           MOVE HKB-TNC TO W-TNCD.
           IF  W-TNC NOT = W-TNC1
               GO TO S-65
           END-IF
           MOVE HKB-TNNA TO W-TNNA.
           MOVE ZERO TO W-CC.
       S-60.
           ADD 1 TO W-CC.
           IF  W-CC < 5
               IF  W-NA(W-CC) NOT = "（"
                   MOVE W-NA(W-CC) TO W-TN(W-CC)
                   GO TO S-60
               END-IF
           END-IF.
       S-65.
           MOVE SPACE TO W-P.
           MOVE W-ATM TO P-ATM.
           MOVE W-TNC TO P-TNC.
           MOVE WT-URS(CNT) TO P-URS.
           MOVE WT-URK(CNT) TO P-URK.
           MOVE WT-NBK(CNT) TO P-NBK.
           MOVE WT-AZS(CNT) TO P-AZS.
           MOVE WT-HPS(CNT) TO P-HPS.
           MOVE WT-HPK(CNT) TO P-HPK.
           MOVE WT-SUT(CNT) TO P-SUT.
           MOVE WT-AHS(CNT) TO P-AHS.
           MOVE WT-AHK(CNT) TO P-AHK.
           MOVE WT-KNT(CNT) TO P-KNT.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO S-55.
       S-70.
           MOVE SPACE TO W-P.
           MOVE "【　合　" TO P-ATM.
           MOVE "計　】　" TO P-TM.
           MOVE WA-URS TO P-URS.
           MOVE WA-URK TO P-URK.
           MOVE WA-NBK TO P-NBK.
           MOVE WA-AZS TO P-AZS.
           MOVE WA-HPS TO P-HPS.
           MOVE WA-HPK TO P-HPK.
           MOVE WA-SUT TO P-SUT.
           MOVE WA-AHS TO P-AHS.
           MOVE WA-AHK TO P-AHK.
           MOVE WA-KNT TO P-KNT.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-75.
           EXIT.

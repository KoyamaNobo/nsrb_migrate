       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG120.
       AUTHOR. S-NAKAO.
      *********************************************************
      *    PROGRAM         :  請求用売掛残高明細表　　　      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/26                        *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=販売用 , 1=経理用             *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  H-MID          PIC  N(007) VALUE SPACE.
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "＊＊＊　　請求用　売掛残高　明細表　　＊＊＊".
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(001) VALUE "(".
           02  F              PIC  N(002) VALUE "　△".
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  N(004) VALUE "２０締　".
           02  F              PIC  X(004) VALUE " **:".
           02  F              PIC  N(002) VALUE "末締".
           02  F              PIC  X(004) VALUE "  *:".
           02  F              PIC  N(002) VALUE "他締".
           02  F              PIC  X(002) VALUE " )".
           02  F              PIC  X(054) VALUE SPACE.
           02  F              PIC  X(014) VALUE "I-----------  ".
           02  F              PIC  N(004) VALUE "締　　後".
           02  F              PIC  X(014) VALUE "  -----------I".
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  N(002) VALUE "入金".
           02  F              PIC  X(002) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(011) VALUE "      ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　前回請求額".
           02  F              PIC  X(002) VALUE "( ".
           02  F              PIC  N(002) VALUE "日付".
           02  F              PIC  X(002) VALUE " )".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　入金額".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　売上額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "消費税額".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　合　計".
           02  F              PIC  N(008) VALUE "　次回請求予定額".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "予定".
       01  W-P.
           02  P-TNC          PIC  9(002).
           02  F              PIC  X(001).
           02  P-X            PIC  X(002).
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TN           PIC  N(026).
           02  P-NC           PIC  N(002).
           02  P-SZZ          PIC ----,---,--9.
           02  P-F            PIC  X(001).
           02  P-GET          PIC Z9.
           02  P-V            PIC  X(001).
           02  P-PEY          PIC Z9.
           02  P-R            PIC  X(001).
           02  P-SNK          PIC ----,---,--9.
           02  P-SUA          PIC ----,---,--9.
           02  P-SUAZ         PIC --,---,--9.
           02  P-UT           PIC ----,---,--9.
           02  P-TS           PIC ----,---,--9.
           02  F              PIC  X(001).
           02  P-NGP          PIC 99/99.
       01  W-DATA.
           02  W-SS           PIC  9(001).
           02  W-C            PIC  9(001).
           02  W-BMC          PIC  9(001).
           02  W-TNC.
             03  W-TNC1       PIC  9(001).
             03  W-TNC2       PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-UT           PIC S9(010).
           02  W-TS           PIC S9(010).
           02  W-SNG.
             03  W-SN         PIC  9(002).
             03  W-SG         PIC  9(002).
           02  W-NGP          PIC  9(004).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NGD        PIC  9(002).
             03  W-NPD        PIC  9(002).
           02  W-SNGP.
             03  W-SNEN       PIC  9(004).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  CNT.
             03  CNT1         PIC  9(002).
             03  CNT2         PIC  9(002).
           02  W-PAGE         PIC  9(002).
           02  WN-D.
             03  WN-SZZ       PIC S9(010).
             03  WN-SNK       PIC S9(010).
             03  WN-SUA       PIC S9(010).
             03  WN-SUAZ      PIC S9(008).
             03  WN-UT        PIC S9(010).
             03  WN-TS        PIC S9(010).
           02  WS-D.
             03  WS-SZZ       PIC S9(010).
             03  WS-SNK       PIC S9(010).
             03  WS-SUA       PIC S9(010).
             03  WS-SUAZ      PIC S9(008).
             03  WS-UT        PIC S9(010).
             03  WS-TS        PIC S9(010).
           02  WA-D.
             03  WA-SZZ       PIC S9(010).
             03  WA-SNK       PIC S9(010).
             03  WA-SUA       PIC S9(010).
             03  WA-SUAZ      PIC S9(008).
             03  WA-UT        PIC S9(010).
             03  WA-TS        PIC S9(010).
           02  WT-D     OCCURS   4.
             03  WT-SZZ       PIC S9(010).
             03  WT-SNK       PIC S9(010).
             03  WT-SUA       PIC S9(010).
             03  WT-SUAZ      PIC S9(008).
             03  WT-UT        PIC S9(010).
             03  WT-TS        PIC S9(010).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LSPF.
      *FD  TSKF
       01  TSKF_HKG120.
           02  TSKF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TSKF_LNAME     PIC  X(011) VALUE "TSKF_HKG120".
           02  F              PIC  X(001).
           02  TSKF_KEY1      PIC  X(100) VALUE SPACE.
           02  TSKF_SORT      PIC  X(100) VALUE SPACE.
           02  TSKF_IDLST     PIC  X(100) VALUE SPACE.
           02  TSKF_RES       USAGE  POINTER.
       01  TSK-R.
           02  TSK-KEY.
             03  TSK-TCD      PIC  9(004).
           02  TSK-SZZ        PIC S9(009).
           02  TSK-ZNGP       PIC  9(008).
           02  TSK-ZNGPD REDEFINES TSK-ZNGP.
             03  F            PIC  9(002).
             03  TSK-ZNGS     PIC  9(004).
             03  F            PIC  9(002).
           02  TSK-SNK        PIC S9(009).
           02  TSK-SUA        PIC S9(009).
           02  TSK-SUAZ       PIC S9(007).
           02  TSK-TNC.
             03  TSK-TNC1     PIC  9(001).
             03  TSK-TNC2     PIC  9(001).
           02  TSK-BMC        PIC  9(001).
           02  F              PIC  X(015).
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　請求用　売掛残高　明細表　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "280" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "40" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "40" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "40" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "40" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "40" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "40" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "27" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "27" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO TSKF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TSKF_PNAME1 " " BY REFERENCE TSKF_IDLST "0".
       M-25.
      *           READ TSKF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TSKF_PNAME1 BY REFERENCE TSK-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TSKF_IDLST TSKF_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ZERO = TSK-SZZ AND TSK-SNK AND TSK-SUA AND TSK-SUAZ
               GO TO M-25
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO WA-D W-C W-PAGE.
           PERFORM S-10 THRU S-15.
       M-30.
           IF  JS-SIGN = 1
               GO TO M-35
           END-IF
           MOVE ZERO TO WS-D.
           MOVE TSK-TNC1 TO W-TNC1.
       M-35.
           IF  JS-SIGN = 0
               MOVE 0 TO CHK
               MOVE ZERO TO WN-D
               MOVE TSK-TNC2 TO W-TNC2
           END-IF
           IF  JS-SIGN = 1
               MOVE ZERO TO WS-D
               MOVE TSK-BMC TO W-BMC
           END-IF.
       M-40.
           MOVE TSK-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO T-SS T-SHC1
               MOVE "　＊＊　マスター　なし　＊＊" TO T-NAME
           END-IF
           MOVE TSK-ZNGP TO W-SNGP
           COMPUTE W-UT = TSK-SUA + TSK-SUAZ.
           COMPUTE W-TS = TSK-SZZ - TSK-SNK + W-UT.
           PERFORM S-40 THRU S-45.
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NC.
           IF  JS-SIGN = 0
               IF  CHK = 0
                   MOVE 1 TO CHK
                   MOVE W-TNC TO P-TNC
               END-IF
           END-IF
           IF (T-SS = 99) OR (T-ENG NOT = ZERO)
               MOVE "X " TO P-X
               MOVE 4 TO W-SS
               GO TO M-45
           END-IF
           IF  T-SS = 20
               MOVE 1 TO W-SS
               GO TO M-45
           END-IF
           IF  T-SS > 29
               MOVE "**" TO P-X
               MOVE 2 TO W-SS
           ELSE
               MOVE " *" TO P-X
               MOVE 3 TO W-SS
           END-IF.
       M-45.
           MOVE TSK-TCD TO P-TCD.
           MOVE T-NAME TO P-TN.
           IF  T-SHC1 = 1
               MOVE "　現" TO P-NC
           END-IF
           IF  T-SHC1 = 2
               MOVE "　小" TO P-NC
           END-IF
           IF  T-SHC1 = 3
               MOVE "　手" TO P-NC
           END-IF
           MOVE TSK-SZZ TO P-SZZ.
           IF  T-SS NOT = 99
               IF  W-SNGP NOT = ZERO
                   MOVE "(" TO P-F
                   MOVE W-SGET TO P-GET
                   MOVE "/" TO P-V
                   MOVE W-SPEY TO P-PEY
                   MOVE ")" TO P-R
               END-IF
           END-IF
           MOVE TSK-SNK TO P-SNK.
           MOVE TSK-SUA TO P-SUA.
           MOVE TSK-SUAZ TO P-SUAZ.
           MOVE W-UT TO P-UT.
           MOVE W-TS TO P-TS.
           IF  W-NGP NOT = ZERO
               MOVE W-NGP TO P-NGP
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
               IF  JS-SIGN = 0
                   MOVE W-TNC TO P-TNC
               END-IF
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           IF  JS-SIGN = 1
               ADD TSK-SZZ TO WS-SZZ WT-SZZ(W-SS)
               ADD TSK-SNK TO WS-SNK WT-SNK(W-SS)
               ADD TSK-SUA TO WS-SUA WT-SUA(W-SS)
               ADD TSK-SUAZ TO WS-SUAZ WT-SUAZ(W-SS)
               ADD W-UT TO WS-UT WT-UT(W-SS)
               ADD W-TS TO WS-TS WT-TS(W-SS)
           ELSE
               ADD TSK-SZZ TO WN-SZZ WT-SZZ(W-SS)
               ADD TSK-SNK TO WN-SNK WT-SNK(W-SS)
               ADD TSK-SUA TO WN-SUA WT-SUA(W-SS)
               ADD TSK-SUAZ TO WN-SUAZ WT-SUAZ(W-SS)
               ADD W-UT TO WN-UT WT-UT(W-SS)
               ADD W-TS TO WN-TS WT-TS(W-SS)
           END-IF.
       M-50.
      *           READ TSKF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TSKF_PNAME1 BY REFERENCE TSK-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  ZERO = TSK-SZZ AND TSK-SNK AND TSK-SUA AND TSK-SUAZ
               GO TO M-50
           END-IF
           IF  JS-SIGN = 1
               IF  TSK-BMC = W-BMC
                   GO TO M-40
               ELSE
                   GO TO M-55
               END-IF
           END-IF
           IF  TSK-TNC1 NOT = W-TNC1
               GO TO M-55
           END-IF
           IF  TSK-TNC2 = W-TNC2
               GO TO M-40
           END-IF
           PERFORM S-20 THRU S-25.
           GO TO M-35.
       M-55.
           IF  JS-SIGN = 0
               PERFORM S-20 THRU S-25
           END-IF
           PERFORM S-30 THRU S-35.
           GO TO M-30.
       M-85.
           IF  JS-SIGN = 0
               PERFORM S-20 THRU S-25
           END-IF
           PERFORM S-30 THRU S-35.
       M-90.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NC.
           MOVE "　［　　ＡＬＬ　ＴＯＴＡＬ　　］" TO P-TN.
           MOVE WA-SZZ TO P-SZZ.
           MOVE WA-SNK TO P-SNK.
           MOVE WA-SUA TO P-SUA.
           MOVE WA-SUAZ TO P-SUAZ.
           MOVE WA-UT TO P-UT.
           MOVE WA-TS TO P-TS.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           PERFORM S-50 THRU S-60.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
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
           MOVE SPACE TO SP-R.
           ADD 1 TO W-PAGE.
           IF  JS-SIGN = 1
               MOVE "【　経　理　】" TO H-MID
           END-IF
           MOVE W-PAGE TO H-PAGE.
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
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NC.
           MOVE "　　　　　　　　　　　　（　ＴＯＴＡＬ　）" TO P-TN.
           MOVE WN-SZZ TO P-SZZ.
           MOVE WN-SNK TO P-SNK.
           MOVE WN-SUA TO P-SUA.
           MOVE WN-SUAZ TO P-SUAZ.
           MOVE WN-UT TO P-UT.
           MOVE WN-TS TO P-TS.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
               IF  JS-SIGN = 0
                   MOVE W-TNC TO P-TNC
               END-IF
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WN-SZZ TO WS-SZZ.
           ADD WN-SNK TO WS-SNK.
           ADD WN-SUA TO WS-SUA.
           ADD WN-SUAZ TO WS-SUAZ.
           ADD WN-UT TO WS-UT.
           ADD WN-TS TO WS-TS.
       S-25.
           EXIT.
       S-30.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NC.
           MOVE "　　　　　＜　　ＳＵＢ　ＴＯＴＡＬ　　＞" TO P-TN.
           MOVE WS-SZZ TO P-SZZ.
           MOVE WS-SNK TO P-SNK.
           MOVE WS-SUA TO P-SUA.
           MOVE WS-SUAZ TO P-SUAZ.
           MOVE WS-UT TO P-UT.
           MOVE WS-TS TO P-TS.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-SZZ TO WA-SZZ.
           ADD WS-SNK TO WA-SNK.
           ADD WS-SUA TO WA-SUA.
           ADD WS-SUAZ TO WA-SUAZ.
           ADD WS-UT TO WA-UT.
           ADD WS-TS TO WA-TS.
       S-35.
           EXIT.
       S-40.
           MOVE ZERO TO W-SNG W-NGP CNT.
           IF  TSK-SZZ = ZERO
               GO TO S-45
           END-IF
           IF (T-NKY = ZERO) OR (T-SS = 99)
               GO TO S-45
           END-IF
           MOVE TSK-ZNGS TO W-SNG.
           IF  W-SNG = ZERO
               GO TO S-45
           END-IF
           MOVE W-SG TO W-NGD.
           MOVE T-SS TO W-NPD.
           IF  T-SS = ZERO
               MOVE 30 TO W-NPD
           END-IF
           DIVIDE 30 INTO T-NKY GIVING CNT1 REMAINDER CNT2.
           ADD CNT1 TO W-NGD.
           ADD CNT2 TO W-NPD.
           IF  W-NPD > 30
               ADD 1 TO W-NGD
               SUBTRACT 30 FROM W-NPD
           END-IF
           IF  W-NGD > 12
               SUBTRACT 12 FROM W-NGD
           END-IF
           IF  W-NPD NOT = 30
               GO TO S-45
           END-IF
           IF  W-NGD = 2
               MOVE 28 TO W-NPD
           END-IF
           IF  W-NGD = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
               MOVE 31 TO W-NPD
           END-IF.
       S-45.
           EXIT.
       S-50.
           MOVE 0 TO W-SS.
       S-55.
           ADD 1 TO W-SS.
           IF  W-SS = 5
               GO TO S-60
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NC.
           IF  W-SS = 1
               MOVE "　　　　　　　　　　（　２０日締計　）" TO P-TN
           END-IF
           IF  W-SS = 2
               MOVE "　　　　　　　　　　（　末日締計　）　" TO P-TN
           END-IF
           IF  W-SS = 3
               MOVE "　　　　　　　　　　（　その他締計　）" TO P-TN
           END-IF
           IF  W-SS = 4
               MOVE "　　　　　　　　　　（　　×　計　　）" TO P-TN
           END-IF
           MOVE WT-SZZ(W-SS) TO P-SZZ.
           MOVE WT-SNK(W-SS) TO P-SNK.
           MOVE WT-SUA(W-SS) TO P-SUA.
           MOVE WT-SUAZ(W-SS) TO P-SUAZ.
           MOVE WT-UT(W-SS) TO P-UT.
           MOVE WT-TS(W-SS) TO P-TS.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO S-55.
       S-60.
           EXIT.

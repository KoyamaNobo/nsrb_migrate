       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG610.
       AUTHOR. T-FUJII.
       DATE-WRITTEN. 1974-07-17.
      *********************************************************
      *    PROGRAM         :  担当得意先別　販売実績表        *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *        変更　　　  :  62/05/12                        *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=作表 , 1=ＥＸＣＥＬ変換       *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-15K              PIC  X(005) VALUE X"1A24212078".
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(036) VALUE SPACE.
           02  F              PIC  N(023) VALUE
                "＊＊＊　　担当者得意先別　販売実績表　　＊＊＊".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  N(002) VALUE "担当".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(007) VALUE "得　意　先　名".
           02  F              PIC  X(108) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(021) VALUE "               I---  ".
           02  F              PIC  N(006) VALUE "前月売掛残高".
           02  F              PIC  X(015) VALUE "  ---I I-----  ".
           02  F              PIC  N(004) VALUE "当月売上".
           02  F              PIC  X(014) VALUE "  ----I I---  ".
           02  F              PIC  N(004) VALUE "当月値引".
           02  F              PIC  X(014) VALUE "  --I I-----  ".
           02  F              PIC  N(004) VALUE "当月入金".
           02  F              PIC  X(016) VALUE "  ----I I-----  ".
           02  F              PIC  N(004) VALUE "売掛残高".
           02  F              PIC  X(008) VALUE "  -----I".
       01  HEAD4.
           02  F              PIC  X(022) VALUE SPACE.
           02  F              PIC  N(003) VALUE "売　上".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(003) VALUE "売　上".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "売　上".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(003) VALUE "売　上".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "売　上".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
       01  W-P1.
           02  P-15K          PIC  X(005).
           02  F              PIC  X(001).
           02  P-TC           PIC  9(002).
           02  F              PIC  X(002).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(026).
           02  F              PIC  X(083).
       01  W-P2.
           02  P-20K          PIC  X(005).
           02  P-TM           PIC  N(007).
           02  P-TZZ          PIC --,---,---,--9.
           02  P-TZZZ         PIC ---,---,--9.
           02  P-TUA          PIC -----,---,--9.
           02  P-TUAZ         PIC ---,---,--9.
           02  P-TNB          PIC ---,---,--9.
           02  P-TNBZ         PIC -----,--9.
           02  P-TNK          PIC -----,---,--9.
           02  P-TNKZ         PIC ---,---,--9.
           02  P-TUZ          PIC --,---,---,--9.
           02  P-TUZZ         PIC ---,---,--9.
       01  W-D.
           02  W-TC           PIC  9(002).
           02  W-PAGE         PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-ZC           PIC  9(001).
       01  WS-D.
           02  WS-TZZ         PIC S9(010).                              前月残高
           02  WS-TZZZ        PIC S9(008).                              　消費税
           02  WS-TUA         PIC S9(010).                              売上金額
           02  WS-TUAZ        PIC S9(008).                              　消費税
           02  WS-TNB         PIC S9(009).                              値引金額
           02  WS-TNBZ        PIC S9(007).                              　消費税
           02  WS-TNK         PIC S9(010).                              入金金額
           02  WS-TNKZ        PIC S9(008).                              　消費税
           02  WS-TUZ         PIC S9(010).                              売掛残高
           02  WS-TUZZ        PIC S9(008).                              　消費税
       01  WA-D.
           02  WA-TZZ         PIC S9(010).                              前月残高
           02  WA-TZZZ        PIC S9(008).                              　消費税
           02  WA-TUA         PIC S9(010).                              売上金額
           02  WA-TUAZ        PIC S9(008).                              　消費税
           02  WA-TNB         PIC S9(009).                              値引金額
           02  WA-TNBZ        PIC S9(007).                              　消費税
           02  WA-TNK         PIC S9(010).                              入金金額
           02  WA-TNKZ        PIC S9(008).                              　消費税
           02  WA-TUZ         PIC S9(010).                              売掛残高
           02  WA-TUZZ        PIC S9(008).                              　消費税
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LSTTM.
           COPY LSPF.
      *FD  EXL-F
       01  EXL-F_HKG610.
           02  EXL-F_PNAME1   PIC  X(009) VALUE "WK0256000".
           02  F              PIC  X(001).
           02  EXL-F_LNAME    PIC  X(012) VALUE "EXL-F_HKG610".
           02  F              PIC  X(001).
           02  EXL-F_KEY1     PIC  X(100) VALUE SPACE.
           02  EXL-F_SORT     PIC  X(100) VALUE SPACE.
           02  EXL-F_IDLST    PIC  X(100) VALUE SPACE.
           02  EXL-F_RES      USAGE  POINTER.
       01  EXL-R.
           02  EXL-TNC        PIC  X(002).
           02  EXL-TCD        PIC  X(004).
           02  EXL-NAME       PIC  N(026).
           02  EXL-TZZ        PIC S9(010).
           02  EXL-TZZZ       PIC S9(008).
           02  EXL-TUA        PIC S9(010).
           02  EXL-TUAZ       PIC S9(008).
           02  EXL-TNB        PIC S9(009).
           02  EXL-TNBZ       PIC S9(007).
           02  EXL-TNK        PIC S9(010).
           02  EXL-TNKZ       PIC S9(008).
           02  EXL-TUZ        PIC S9(010).
           02  EXL-TUZZ       PIC S9(008).
           02  F              PIC  X(110).
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　担当得意先別　販売実績表　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
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
           "C-MID" " " "0" "0" "308" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
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
               CALL "DB_Close"
               STOP RUN
           END-IF
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO TT-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TT-M_PNAME1 " " BY REFERENCE TT-M_IDLST "0".
       M-10.
      *           READ TT-M AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TT-M_PNAME1 BY REFERENCE TT-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE TT-M_IDLST TT-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ZERO = TT-TZZ AND TT-TZZZ AND TT-TUA AND TT-TUAZ
                 AND TT-TNB AND TT-TNBZ AND TT-TNK AND TT-TNKZ
                 AND TT-TUZ AND TT-TUZZ
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           IF  JS-SIGN = 1
               CALL "DB_F_Open" USING
                "OUTPUT" EXL-F_PNAME1 " " BY REFERENCE EXL-F_IDLST "0"
               MOVE ZERO TO WA-D
               GO TO M-20
           END-IF
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO WA-D W-PAGE.
           PERFORM S-10 THRU S-15.
       M-20.
           MOVE TT-TNC TO W-TC.
           MOVE ZERO TO WS-D CHK.
       M-25.
           MOVE TT-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊" TO T-NAME
           END-IF
      *
           IF  JS-SIGN = 1
               INITIALIZE EXL-R
               MOVE TT-TCD TO EXL-TCD
               MOVE T-NAME TO EXL-NAME
               MOVE TT-TZZ TO EXL-TZZ
               MOVE TT-TZZZ TO EXL-TZZZ
               MOVE TT-TUA TO EXL-TUA
               MOVE TT-TUAZ TO EXL-TUAZ
               MOVE TT-TNB TO EXL-TNB
               MOVE TT-TNBZ TO EXL-TNBZ
               MOVE TT-TNK TO EXL-TNK
               MOVE TT-TNKZ TO EXL-TNKZ
               MOVE TT-TUZ TO EXL-TUZ
               MOVE TT-TUZZ TO EXL-TUZZ
               IF  CHK = 0
                   MOVE W-TC TO EXL-TNC
      *                   WRITE EXL-R
      *//////////////
                   CALL "DB_Insert" USING
                    EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
                   GO TO M-27
               ELSE
      *                   WRITE EXL-R
      *//////////////
                   CALL "DB_Insert" USING
                    EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
                   GO TO M-27
               END-IF
           END-IF
           MOVE SPACE TO W-P1 W-P2.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           IF  CHK = 0
               MOVE W-TC TO P-TC
           END-IF
           MOVE TT-TCD TO P-TCD.
           MOVE T-NAME TO P-NAME.
           MOVE TT-TZZ TO P-TZZ.
           MOVE TT-TZZZ TO P-TZZZ.
           MOVE TT-TUA TO P-TUA.
           MOVE TT-TUAZ TO P-TUAZ.
           MOVE TT-TNB TO P-TNB.
           MOVE TT-TNBZ TO P-TNBZ.
           MOVE TT-TNK TO P-TNK.
           MOVE TT-TNKZ TO P-TNKZ.
           MOVE TT-TUZ TO P-TUZ.
           MOVE TT-TUZZ TO P-TUZZ.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE W-TC TO P-TC
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       M-27.
           ADD TT-TZZ TO WS-TZZ.
           ADD TT-TZZZ TO WS-TZZZ.
           ADD TT-TUA TO WS-TUA.
           ADD TT-TUAZ TO WS-TUAZ.
           ADD TT-TNB TO WS-TNB.
           ADD TT-TNBZ TO WS-TNBZ.
           ADD TT-TNK TO WS-TNK.
           ADD TT-TNKZ TO WS-TNKZ.
           ADD TT-TUZ TO WS-TUZ.
           ADD TT-TUZZ TO WS-TUZZ.
           IF  CHK = 5
               MOVE 9 TO CHK
           END-IF
           IF  CHK = 0
               MOVE 5 TO CHK
           END-IF.
       M-30.
      *           READ TT-M AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TT-M_PNAME1 BY REFERENCE TT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  ZERO = TT-TZZ AND TT-TZZZ AND TT-TUA AND TT-TUAZ
                 AND TT-TNB AND TT-TNBZ AND TT-TNK AND TT-TNKZ
                 AND TT-TUZ AND TT-TUZZ
               GO TO M-30
           END-IF
           IF  W-TC = TT-TNC
               GO TO M-25
           END-IF
           PERFORM S-20 THRU S-30.
           GO TO M-20.
       M-90.
           PERFORM S-20 THRU S-30.
           IF  JS-SIGN = 1
               INITIALIZE EXL-R
               MOVE "　　　　　【　総合計　】" TO EXL-NAME
               MOVE WA-TZZ TO EXL-TZZ
               MOVE WA-TZZZ TO EXL-TZZZ
               MOVE WA-TUA TO EXL-TUA
               MOVE WA-TUAZ TO EXL-TUAZ
               MOVE WA-TNB TO EXL-TNB
               MOVE WA-TNBZ TO EXL-TNBZ
               MOVE WA-TNK TO EXL-TNK
               MOVE WA-TNKZ TO EXL-TNKZ
               MOVE WA-TUZ TO EXL-TUZ
               MOVE WA-TUZZ TO EXL-TUZZ
      *               WRITE EXL-R
      *//////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO M-95
           END-IF
           MOVE SPACE TO W-P2.
           MOVE W-20K TO P-20K.
           MOVE "【　総合計　】" TO P-TM.
           MOVE WA-TZZ TO P-TZZ.
           MOVE WA-TZZZ TO P-TZZZ.
           MOVE WA-TUA TO P-TUA.
           MOVE WA-TUAZ TO P-TUAZ.
           MOVE WA-TNB TO P-TNB.
           MOVE WA-TNBZ TO P-TNBZ.
           MOVE WA-TNK TO P-TNK.
           MOVE WA-TNKZ TO P-TNKZ.
           MOVE WA-TUZ TO P-TUZ.
           MOVE WA-TUZZ TO P-TUZZ.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
           IF  JS-SIGN NOT = 0
               CALL "DB_F_Close" USING
                BY REFERENCE EXL-F_IDLST EXL-F_PNAME1
           ELSE
               CALL "PR_Close" RETURNING RESP
           END-IF
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
       S-15.
           EXIT.
       S-20.
           IF  JS-SIGN = 1
               INITIALIZE EXL-R
               MOVE "　　　　　　　　　　　［　小計　］" TO EXL-NAME
               MOVE WS-TZZ TO EXL-TZZ
               MOVE WS-TZZZ TO EXL-TZZZ
               MOVE WS-TUA TO EXL-TUA
               MOVE WS-TUAZ TO EXL-TUAZ
               MOVE WS-TNB TO EXL-TNB
               MOVE WS-TNBZ TO EXL-TNBZ
               MOVE WS-TNK TO EXL-TNK
               MOVE WS-TNKZ TO EXL-TNKZ
               MOVE WS-TUZ TO EXL-TUZ
               MOVE WS-TUZZ TO EXL-TUZZ
      *               WRITE EXL-R
      *//////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO S-27
           END-IF
           IF  CHK NOT = 9
               GO TO S-25
           END-IF
           MOVE SPACE TO W-P2.
           MOVE W-20K TO P-20K.
           MOVE "　［　小計　］" TO P-TM.
           MOVE WS-TZZ TO P-TZZ.
           MOVE WS-TZZZ TO P-TZZZ.
           MOVE WS-TUA TO P-TUA.
           MOVE WS-TUAZ TO P-TUAZ.
           MOVE WS-TNB TO P-TNB.
           MOVE WS-TNBZ TO P-TNBZ.
           MOVE WS-TNK TO P-TNK.
           MOVE WS-TNKZ TO P-TNKZ.
           MOVE WS-TUZ TO P-TUZ.
           MOVE WS-TUZZ TO P-TUZZ.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-25.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-27.
           ADD WS-TZZ TO WA-TZZ.
           ADD WS-TZZZ TO WA-TZZZ.
           ADD WS-TUA TO WA-TUA.
           ADD WS-TUAZ TO WA-TUAZ.
           ADD WS-TNB TO WA-TNB.
           ADD WS-TNBZ TO WA-TNBZ.
           ADD WS-TNK TO WA-TNK.
           ADD WS-TNKZ TO WA-TNKZ.
           ADD WS-TUZ TO WA-TUZ.
           ADD WS-TUZZ TO WA-TUZZ.
       S-30.
           EXIT.

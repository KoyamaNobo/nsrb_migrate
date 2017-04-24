       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD750.
      *********************************************************
      *    PROGRAM         :  販売状況表　　　　　　　　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/18                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(035) VALUE SPACE.
           02  F              PIC  N(019) VALUE
                "＊＊＊　　販　売　状　況　表　　＊＊＊".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(006) VALUE "前月売掛残高".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(006) VALUE "当月売上金額".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(006) VALUE "当月値引金額".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(006) VALUE "当月消費税額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(006) VALUE "当月入金金額".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売掛残高".
       01  W-P.
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-NAME         PIC  N(026).
           02  P-TZZ          PIC -----,---,--9.
           02  P-TUA          PIC -----,---,--9.
           02  P-TNB          PIC ----,---,--9.
           02  P-SHZ          PIC ---,---,--9.
           02  P-TNK          PIC -----,---,--9.
           02  P-TUZ          PIC -----,---,--9.
       01  W-D.
           02  W-TZZ          PIC S9(009).
           02  W-SHZ          PIC S9(007).
           02  W-TNK          PIC S9(009).
           02  W-TUZ          PIC S9(009).
       01  WT-D.
           02  WT-TZZ         PIC S9(010).
           02  WT-TUA         PIC S9(010).
           02  WT-TNB         PIC S9(009).
           02  WT-SHZ         PIC S9(008).
           02  WT-TNK         PIC S9(010).
           02  WT-TUZ         PIC S9(010).
       01  W-PAGE             PIC  9(002) VALUE ZERO.
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LITTM.
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
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　販売状況表　作表　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-KEY   PIC  9(004).
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "252" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "36" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "36" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "36" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "36" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "36" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "36" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "36" "06C-MID" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "29" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "29" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "9" "24" "35" "4" "E-ME1" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE TT-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-KEY" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" 
            TT-M_PNAME1 "SHARED" BY REFERENCE TT-M_IDLST "1"
            "TT-KEY" BY REFERENCE TT-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO WT-D.
       M-10.
      *           READ TT-M WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" TT-M_PNAME1 BY REFERENCE TT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           MOVE ZERO TO W-D.
           COMPUTE W-TZZ = TT-TZZ + TT-TZZZ.
           COMPUTE W-SHZ = TT-TUAZ + TT-TNBZ.
           COMPUTE W-TNK = TT-TNK + TT-TNKZ.
           COMPUTE W-TUZ = TT-TUZ + TT-TUZZ.
           IF  ZERO = W-TZZ AND TT-TUA AND TT-TNB AND W-SHZ
                                      AND W-TNK AND W-TUZ
               GO TO M-10
           END-IF
           MOVE TT-KEY TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-25.
           MOVE SPACE TO W-P.
           MOVE TT-TCD TO P-TCD.
           MOVE T-NAME TO P-NAME.
           MOVE W-TZZ TO P-TZZ.
           MOVE TT-TUA TO P-TUA.
           MOVE TT-TNB TO P-TNB.
           MOVE W-SHZ TO P-SHZ.
           MOVE W-TNK TO P-TNK.
           MOVE W-TUZ TO P-TUZ.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-TZZ TO WT-TZZ.
           ADD TT-TUA TO WT-TUA.
           ADD TT-TNB TO WT-TNB.
           ADD W-SHZ TO WT-SHZ.
           ADD W-TNK TO WT-TNK.
           ADD W-TUZ TO WT-TUZ.
           GO TO M-10.
       M-90.
           MOVE SPACE TO W-P.
           MOVE "　　　　　　　　【　　総　合　計　　】　" TO P-NAME.
           MOVE WT-TZZ TO P-TZZ.
           MOVE WT-TUA TO P-TUA.
           MOVE WT-TNB TO P-TNB.
           MOVE WT-SHZ TO P-SHZ.
           MOVE WT-TNK TO P-TNK.
           MOVE WT-TUZ TO P-TUZ.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
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
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMK120.
       DATE-WRITTEN. 1974-07-27.
      *********************************************************
      *    PROGRAM       　:  教育振興会　会費請求用集計表    *
      *    PRINTER TYPE    :  JIPS*                           *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/16                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
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
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　教育振興会　会費用　出荷集計表　＊＊＊".
           02  F              PIC  X(005) VALUE SPACE.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(001) VALUE "'".
           02  H-NEN          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-GET          PIC Z9.
           02  F              PIC  N(002) VALUE "月分".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(007) VALUE "品　　　　　名".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  N(016) VALUE
                "数　量　売　価　　　売上金額　　".
           02  F              PIC  N(004) VALUE "　振興会".
       01  HEAD3.
           02  F              PIC  X(078) VALUE SPACE.
           02  F              PIC  X(009) VALUE "(金額X3%)".
       01  W-P.
           02  P-15K          PIC  X(005).
           02  P-HCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-NAME         PIC  N(024).
           02  P-SU           PIC ---,---,--9.
           02  P-T            PIC ----,--9.
           02  P-KIN          PIC --,---,---,--9.
           02  P-TR           PIC ----,---,--9.
       01  W-D.
           02  W-NC           PIC  9(001).
           02  CNT            PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-NAMED.
             03  W-NM    OCCURS  24  PIC  N(001).
           02  W-NMD   REDEFINES W-NAMED PIC  N(024).
           02  W-NAME.
             03  W-NA    OCCURS  24  PIC  N(001).
           02  W-NAD   REDEFINES W-NAME  PIC  N(024).
           02  W-NG.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
       01  WS-D.
           02  WS-SU          PIC S9(007).
           02  WS-KIN         PIC S9(009).
           02  WS-TR          PIC S9(009).
       01  WA-D.
           02  WA-SU          PIC S9(007).
           02  WA-KIN         PIC S9(009).
           02  WA-TR          PIC S9(009).
       01  W-PAGE             PIC  9(002) VALUE ZERO.
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LSPF.
      *FD  HKS-F
       01  HKS-F_HMK120.
           02  HKS-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HKS-F_LNAME    PIC  X(012) VALUE "HKS-F_HMK120".
           02  F              PIC  X(001).
           02  HKS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HKS-F_SORT     PIC  X(100) VALUE SPACE.
           02  HKS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HKS-F_RES      USAGE  POINTER.
       01  HKS-R.
           02  HK-NC          PIC  9(001).
           02  HK-HCD1        PIC  9(004).
           02  HK-SU          PIC S9(006).
           02  HK-T           PIC S9(005).
           02  HK-KIN         PIC S9(008).
           02  F              PIC  X(009).
           02  HK-TR          PIC S9(007).
           02  F              PIC  X(024).
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
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　教育協議会用　品種別売上集計表　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
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
            "C-MID" " " "0" "0" "350" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "50" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "50" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "50" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "50" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "50" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "50" "06C-MID" " " RETURNING RESU.
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
      *
       M-05.
           COPY LIBCPR.
           MOVE D-NING TO W-NG.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO HKS-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HKS-F_PNAME1 " " BY REFERENCE HKS-F_IDLST "0".
       M-10.
      *           READ HKS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKS-F_PNAME1 BY REFERENCE HKS-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HKS-F_IDLST HKS-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           MOVE W-NEN TO H-NEN.
           MOVE W-GET TO H-GET.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO WA-D.
       M-15.
           MOVE HK-NC TO W-NC.
           MOVE ZERO TO WS-D.
       M-20.
           PERFORM S-20 THRU S-35.
       M-30.
      *           READ HKS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HKS-F_PNAME1 BY REFERENCE HKS-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HK-NC NOT = W-NC
               GO TO M-35
           END-IF
           GO TO M-20.
       M-35.
           PERFORM S-40 THRU S-45.
           GO TO M-15.
       M-90.
           PERFORM S-40 THRU S-45.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 58
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R W-P.
           MOVE W-15K TO P-15K.
           MOVE "　　　［　　総　合　計　　］" TO P-NAME.
           MOVE WA-SU TO P-SU.
           MOVE WA-KIN TO P-KIN.
           MOVE WA-TR TO P-TR.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HKS-F_IDLST HKS-F_PNAME1.
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
           MOVE SPACE TO W-P SP-R.
           MOVE W-15K TO P-15K.
           MOVE HK-HCD1 TO P-HCD.
           MOVE ZERO TO HI-KEY.
           MOVE HK-HCD1 TO HI-HCD1.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　" TO P-NAME
               GO TO S-30
           END-IF
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　" TO P-NAME
               GO TO S-30
           END-IF
           IF  HI-HCD1 NOT = HK-HCD1
               MOVE "　＊＊　マスター　なし　＊＊　" TO P-NAME
               GO TO S-30
           END-IF
           MOVE SPACE TO W-NMD W-NAD.
           MOVE ZERO TO CNT.
           MOVE HI-NAME TO W-NMD.
       S-25.
           ADD 1 TO CNT.
           IF  CNT = 25
               MOVE W-NAD TO P-NAME
               GO TO S-30
           END-IF
           MOVE W-NM(CNT) TO W-NA(CNT).
           IF  W-NM(CNT) NOT = SPACE
               GO TO S-25
           END-IF
           ADD 1 TO CNT.
           IF  CNT = 25
               MOVE W-NAD TO P-NAME
               GO TO S-30
           END-IF
           MOVE W-NM(CNT) TO W-NA(CNT).
           IF  W-NM(CNT) NOT = SPACE
               GO TO S-25
           END-IF
           MOVE W-NAD TO P-NAME.
       S-30.
           MOVE HK-SU TO P-SU.
           MOVE HK-T TO P-T.
           MOVE HK-KIN TO P-KIN.
           MOVE HK-TR TO P-TR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD HK-SU TO WS-SU.
           ADD HK-KIN TO WS-KIN.
           ADD HK-TR TO WS-TR.
       S-35.
           EXIT.
       S-40.
           MOVE SPACE TO W-P SP-R.
           MOVE W-15K TO P-15K.
           MOVE "　　　　　　　　＜　　小　　計　　＞" TO P-NAME.
           MOVE WS-SU TO P-SU.
           MOVE WS-KIN TO P-KIN.
           MOVE WS-TR TO P-TR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD WS-SU TO WA-SU.
           ADD WS-KIN TO WA-KIN.
           ADD WS-TR TO WA-TR.
       S-45.
           EXIT.

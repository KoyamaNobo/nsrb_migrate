       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHG210.
      *********************************************************
      *    PROGRAM         :  加硫　廃却数　明細表            *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    DATE WRITTN     :  57/06/14                        *
      *        変更　　　  :  62/04/07                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  N(018) VALUE
                "＊＊＊　　加硫・廃却明細表　　＊＊＊".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(006) VALUE "品　　　名　".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  N(004) VALUE "加硫数量".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "加硫金額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "廃却数量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "廃却金額".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　廃却率".
           02  F              PIC  X(001) VALUE "%".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(002) VALUE "特性".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "不良数量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "不良金額".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　不良率".
           02  F              PIC  X(001) VALUE "%".
           02  F              PIC  X(005) VALUE X"1A24212474".
       01  W-P.
           02  P-KEY          PIC  X(005).
           02  F              PIC  X(002).
           02  P-NAME         PIC  X(020).
           02  P-KSU          PIC ---,---,---.
           02  P-KKIN         PIC -----,---,---.
           02  P-HSU          PIC --,---,---.
           02  P-HKIN         PIC ---,---,---.
           02  P-HRR          PIC -----9.99.
           02  P-TSU          PIC --,---,---.
           02  P-FSU          PIC --,---,---.
           02  P-FKIN         PIC ---,---,---.
           02  P-FRR          PIC -----9.99.
       01  W-D.
           02  W-YC           PIC  9(002).
           02  W-KEY          PIC  X(005).
           02  W-HRR          PIC  9(003)V9(03).
           02  W-FRR          PIC  9(003)V9(03).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
       01  WN-D.
           02  W-KSU          PIC S9(007).
           02  W-KKIN         PIC S9(008).
           02  W-HSU          PIC S9(007).
           02  W-HKIN         PIC S9(008).
           02  W-TSU          PIC S9(007).
           02  W-FSU          PIC S9(007).
           02  W-FKIN         PIC S9(008).
       01  WS-D.
           02  WS-KSU         PIC S9(008).
           02  WS-KKIN        PIC S9(009).
           02  WS-HSU         PIC S9(007).
           02  WS-HKIN        PIC S9(008).
           02  WS-TSU         PIC S9(007).
           02  WS-FSU         PIC S9(007).
           02  WS-FKIN        PIC S9(008).
       01  WA-D.
           02  WA-KSU         PIC S9(008).
           02  WA-KKIN        PIC S9(009).
           02  WA-HSU         PIC S9(007).
           02  WA-HKIN        PIC S9(008).
      *
           COPY LIBFDD.
           COPY LIKHM.
           COPY LSPF.
      *FD  KNHR-F
       01  KNHR-F_KHG210.
           02  KNHR-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  KNHR-F_LNAME   PIC  X(013) VALUE "KNHR-F_KHG210".
           02  F              PIC  X(001).
           02  KNHR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  KNHR-F_SORT    PIC  X(100) VALUE SPACE.
           02  KNHR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  KNHR-F_RES     USAGE  POINTER.
       01  KNHR-R.
           02  KNH-NHC        PIC  9(002).
           02  KNH-DATE       PIC  9(008).
           02  KNH-KEY        PIC  X(005).
           02  F              PIC  X(007).
           02  KNH-SU         PIC S9(006)V9(02).
           02  F              PIC  X(008).
           02  KNH-KIN        PIC S9(008).
           02  KNH-YC         PIC  9(002).
           02  F              PIC  X(001).
           02  KNH-KI         PIC  9(002).
           02  F              PIC  X(002).
           02  KNH-NC         PIC  9(001).
           02  F              PIC  X(010).
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
                "＊＊＊　　加硫・廃却数　明細表　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(016) VALUE
                  "***  KHM ﾅｼ  ***".
             03  E-KEY   PIC  X(005).
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
            "C-MID" " " "0" "0" "280" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "40" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "40" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "40" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "40" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "40" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "40" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "40" "06C-MID" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "31" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "31" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "16" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "35" "5" "E-ME1" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE KNH-KEY "5" "0" RETURNING RESU.
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
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO KNHR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KNHR-F_PNAME1 " " BY REFERENCE KNHR-F_IDLST "0".
       M-10.
      *           READ KNHR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KNHR-F_PNAME1 BY REFERENCE KNHR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KNHR-F_IDLST KNHR-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  KNH-NC = 1
               GO TO M-10
           END-IF
           IF  KNH-SU = ZERO
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-03R TO H-DATE.
           MOVE ZERO TO WA-D.
           PERFORM S-10 THRU S-15.
       M-15.
           MOVE KNH-YC TO W-YC.
           MOVE ZERO TO WS-D.
       M-20.
           MOVE KNH-KEY TO W-KEY.
           MOVE KNH-KEY TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE ZERO TO WN-D.
       M-25.
           IF  KNH-NHC = ZERO
               ADD KNH-SU TO W-KSU
               ADD KNH-KIN TO W-KKIN
           ELSE
               ADD KNH-SU TO W-HSU
               ADD KNH-KIN TO W-HKIN
               IF  KNH-NHC = 25 OR 26
                   ADD KNH-SU TO W-TSU
               ELSE
                   ADD KNH-SU TO W-FSU
                   ADD KNH-KIN TO W-FKIN
               END-IF
           END-IF.
       M-30.
      *           READ KNHR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KNHR-F_PNAME1 BY REFERENCE KNHR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  KNH-NC = 1
               GO TO M-30
           END-IF
           IF  KNH-SU = ZERO
               GO TO M-30
           END-IF
           IF  KNH-YC NOT = W-YC
               GO TO M-35
           END-IF
           IF  KNH-KEY = W-KEY
               GO TO M-25
           END-IF
           PERFORM S-20 THRU S-25.
           GO TO M-20.
       M-35.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-35.
           GO TO M-15.
       M-90.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-35.
           IF  WA-KKIN = ZERO
               MOVE 100 TO W-HRR
           ELSE
               COMPUTE W-HRR ROUNDED = (WA-HKIN / WA-KKIN) * 100
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE "【　総　合  計  】  " TO P-NAME.
           MOVE WA-KSU TO P-KSU.
           MOVE WA-KKIN TO P-KKIN.
           MOVE WA-HSU TO P-HSU.
           MOVE WA-HKIN TO P-HKIN.
           IF  W-HRR NOT = ZERO
               MOVE W-HRR TO P-HRR
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KNHR-F_IDLST KNHR-F_PNAME1.
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
           IF  W-KKIN = ZERO
               MOVE 100 TO W-HRR
           ELSE
               COMPUTE W-HRR ROUNDED = (W-HKIN / W-KKIN) * 100
           END-IF
           IF  W-KKIN = ZERO
               MOVE 100 TO W-FRR
           ELSE
               COMPUTE W-FRR ROUNDED = (W-FKIN / W-KKIN) * 100
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE W-KEY TO P-KEY.
           MOVE KH-NAME TO P-NAME.
           MOVE W-KSU TO P-KSU.
           MOVE W-KKIN TO P-KKIN.
           MOVE W-HSU TO P-HSU.
           MOVE W-HKIN TO P-HKIN.
           IF  W-HRR NOT = ZERO
               MOVE W-HRR TO P-HRR
           END-IF
           MOVE W-TSU TO P-TSU.
           MOVE W-FSU TO P-FSU.
           MOVE W-FKIN TO P-FKIN.
           IF  W-FRR NOT = ZERO
               MOVE W-FRR TO P-FRR
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-KSU TO WS-KSU.
           ADD W-KKIN TO WS-KKIN.
           ADD W-HSU TO WS-HSU.
           ADD W-HKIN TO WS-HKIN.
           ADD W-TSU TO WS-TSU.
           ADD W-FSU TO WS-FSU.
           ADD W-FKIN TO WS-FKIN.
       S-25.
           EXIT.
       S-30.
           IF  WS-KKIN = ZERO
               MOVE 100 TO W-HRR
           ELSE
               COMPUTE W-HRR ROUNDED = (WS-HKIN / WS-KKIN) * 100
           END-IF
           IF  WS-KKIN = ZERO
               MOVE 100 TO W-FRR
           ELSE
               COMPUTE W-FRR ROUNDED = (WS-FKIN / WS-KKIN) * 100
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE "    ［  小  計  ］  " TO P-NAME.
           MOVE WS-KSU TO P-KSU.
           MOVE WS-KKIN TO P-KKIN.
           MOVE WS-HSU TO P-HSU.
           MOVE WS-HKIN TO P-HKIN.
           IF  W-HRR NOT = ZERO
               MOVE W-HRR TO P-HRR
           END-IF
           MOVE WS-TSU TO P-TSU.
           MOVE WS-FSU TO P-FSU.
           MOVE WS-FKIN TO P-FKIN.
           IF  W-FRR NOT = ZERO
               MOVE W-FRR TO P-FRR
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WS-KSU TO WA-KSU.
           ADD WS-KKIN TO WA-KKIN.
           ADD WS-HSU TO WA-HSU.
           ADD WS-HKIN TO WA-HKIN.
       S-35.
           EXIT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN910.
      *********************************************************
      *    PROGRAM         :  決算用倉庫別棚卸明細表          *
      *    PRINTER TYPE    :  *****                           *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/15                        *
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
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(020) VALUE SPACE.
           02  F              PIC  N(023) VALUE
                "＊＊＊　　履物　倉庫別　棚卸　明細表　　＊＊＊".
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  N(016) VALUE
                "倉　庫　名　　コード　品　　　名".
           02  F              PIC  X(042) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単価".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
       01  W-P.
           02  P-SNA          PIC  N(006).
           02  F              PIC  X(003).
           02  P-HCD          PIC  9(004).
           02  F              PIC  X(003).
           02  P-HNA          PIC  N(024).
           02  P-SU           PIC --,---,--9.
           02  P-T            PIC ---,--9.
           02  P-KIN          PIC --,---,---,--9.
       01  W-DATA.
           02  W-SC           PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-NCD          PIC  9(001).
           02  W-NC           PIC  9(001).
           02  W-SNA          PIC  N(006).
           02  W-NAD.
             03  W-NA    OCCURS  24  PIC  N(001).
           02  W-HNAD.
             03  W-HNA   OCCURS  24  PIC  N(001).
           02  W-C            PIC  9(002).
           02  W-PAGE         PIC  9(002).
       01  W-D.
           02  W-SU           PIC S9(007).
           02  W-KIN          PIC S9(010).
       01  WS-D.
           02  WS-SU          PIC S9(007).
           02  WS-KIN         PIC S9(010).
       01  WA-D.
           02  WA-SU          PIC S9(007).
           02  WA-KIN         PIC S9(010).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY L-JCON.
           COPY LIHIM.
           COPY LSPF.
      *FD  HKTWF
       01  HKTWF_HMN910.
           02  HKTWF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HKTWF_LNAME    PIC  X(012) VALUE "HKTWF_HMN910".
           02  F              PIC  X(001).
           02  HKTWF_KEY1     PIC  X(100) VALUE SPACE.
           02  HKTWF_SORT     PIC  X(100) VALUE SPACE.
           02  HKTWF_IDLST    PIC  X(100) VALUE SPACE.
           02  HKTWF_RES      USAGE  POINTER.
       01  HKTW-R.
           02  HKTW-HCD.
             03  HKTW-HCD1    PIC  9(004).
             03  F            PIC  9(002).
           02  HKTW-SNO       PIC  9(001).
           02  HKTW-SU        PIC S9(007).
           02  HKTW-KT        PIC  9(005).
           02  HKTW-KKIN      PIC S9(009).
           02  HKTW-FT        PIC  9(005).
           02  HKTW-FKIN      PIC S9(009).
           02  HKTW-BC1       PIC  9(002).
           02  HKTW-BC2.
             03  HKTW-BC21    PIC  9(001).
             03  HKTW-BC22    PIC  9(001).
           02  HKTW-BC3       PIC  9(002).
           02  HKTW-BMC       PIC  9(002).
           02  HKTW-BMNO      PIC  9(001).
           02  F              PIC  X(013).
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
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　倉庫別　棚卸　明細表　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
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
           "C-ERR" " " "0" "0" "62" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "62" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO HKTWF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HKTWF_PNAME1 " " BY REFERENCE HKTWF_IDLST "0".
           MOVE 0 TO W-DC.
       M-10.
      *           READ HKTWF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HKTWF_PNAME1 BY REFERENCE HKTW-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HKTWF_IDLST HKTWF_PNAME1
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  HKTW-SNO = 9
               MOVE 1 TO W-DC
               GO TO M-50
           END-IF
           IF  HKTW-SU = ZERO
               GO TO M-10
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO WA-D W-DATA.
           PERFORM S-10 THRU S-15.
       M-15.
           MOVE ZERO TO WS-D.
           MOVE 0 TO CHK.
           MOVE HKTW-SNO TO W-SC.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           MOVE 3 TO JCON3-01.
           MOVE W-SC TO JCON3-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　　　　　　" TO JCON3-03
           END-IF
           MOVE JCON3-03 TO W-SNA.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
       M-20.
           MOVE ZERO TO W-D.
           MOVE HKTW-BMNO TO W-NC.
       M-25.
           PERFORM S-20 THRU S-25.
       M-30.
      *           READ HKTWF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HKTWF_PNAME1 BY REFERENCE HKTW-R " " RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-DC
               GO TO M-40
           END-IF
           IF  HKTW-SNO = 9
               GO TO M-40
           END-IF
           IF  HKTW-SU = ZERO
               GO TO M-30
           END-IF
           MOVE HKTW-BMNO TO W-NCD.
           IF  HKTW-SNO NOT = W-SC
               GO TO M-35
           END-IF
           IF  W-NC = W-NCD
               GO TO M-25
           END-IF
           PERFORM S-30 THRU S-35.
           GO TO M-20.
       M-35.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
           GO TO M-15.
       M-40.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
           PERFORM S-50 THRU S-55.
           IF  W-DC = 9
               GO TO M-95
           END-IF.
       M-50.
           IF  W-DC = 1
               CALL "DB_F_Open" USING
                "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
                "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE
                HI-KEY2
               CALL "PR_Open" RETURNING RESP
               PERFORM S-10 THRU S-15
           END-IF
           MOVE ZERO TO WA-D CHK.
           MOVE "預　り　分　" TO W-SNA.
       M-55.
           MOVE ZERO TO W-D.
           COMPUTE W-SU = HKTW-SU * -1.
           COMPUTE W-KIN = HKTW-KKIN * -1.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-SNA P-HNA.
           IF  CHK = ZERO
               MOVE 1 TO CHK
               MOVE W-SNA TO P-SNA
           END-IF
           PERFORM S-60 THRU S-70.
           MOVE HKTW-HCD1 TO P-HCD.
           MOVE W-HNAD TO P-HNA.
           MOVE W-SU TO P-SU.
           MOVE HKTW-KT TO P-T.
           MOVE W-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-SNA TO P-SNA
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD W-SU TO WA-SU.
           ADD W-KIN TO WA-KIN.
       M-60.
      *           READ HKTWF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HKTWF_PNAME1 BY REFERENCE HKTW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-65
           END-IF
           IF  HKTW-SU = ZERO
               GO TO M-60
           END-IF
           GO TO M-55.
       M-65.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-SNA P-HNA.
           MOVE "　　　　　＜　　ＴＯＴＡＬ　　＞　　　" TO P-HNA.
           MOVE WA-SU TO P-SU.
           MOVE WA-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-SNA TO P-SNA
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HKTWF_IDLST HKTWF_PNAME1.
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
       S-15.
           EXIT.
       S-20.
           ADD HKTW-SU TO W-SU.
           ADD HKTW-KKIN TO W-KIN.
           PERFORM S-60 THRU S-70.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-SNA P-HNA.
           IF  CHK = 0
               MOVE 1 TO CHK
               MOVE W-SNA TO P-SNA
           END-IF
           MOVE HKTW-HCD1 TO P-HCD.
           MOVE W-HNAD TO P-HNA.
           MOVE HKTW-SU TO P-SU.
           MOVE HKTW-KT TO P-T.
           MOVE HKTW-KKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-SNA TO P-SNA
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
       S-25.
           EXIT.
       S-30.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-SNA P-HNA.
           IF  W-NC = 1
               MOVE "　　　（　国内計　）　" TO P-HNA
           END-IF
           IF  W-NC = 2
               MOVE "　　　（　上海計　）　" TO P-HNA
           END-IF
           IF  W-NC = 3
               MOVE "　　　（　仕入計　）　" TO P-HNA
           END-IF
           IF  W-NC = 4
               MOVE "　　　（　ワーク計　）" TO P-HNA
           END-IF
           IF  W-NC = 5
               MOVE "　　　（　教育計　）　" TO P-HNA
           END-IF
           MOVE W-SU TO P-SU.
           MOVE W-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-SNA TO P-SNA
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD W-SU TO WS-SU.
           ADD W-KIN TO WS-KIN.
       S-35.
           EXIT.
       S-40.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-SNA P-HNA.
           MOVE "　　　　　＜　ＳＵＢ　ＴＯＴＡＬ　＞　" TO P-HNA.
           MOVE WS-SU TO P-SU.
           MOVE WS-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-SNA TO P-SNA
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-SU TO WA-SU.
           ADD WS-KIN TO WA-KIN.
       S-45.
           EXIT.
       S-50.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-SNA P-HNA.
           MOVE "　［　ＡＬＬ　ＴＯＴＡＬ　］　" TO P-HNA.
           MOVE WA-SU TO P-SU.
           MOVE WA-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-55.
           EXIT.
       S-60.
           MOVE HKTW-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "＊＊　ＨＩ－Ｍ　無し　＊＊　　" TO HI-NAME
           END-IF
           MOVE SPACE TO W-NAD W-HNAD.
           MOVE HI-NAME TO W-NAD.
           MOVE ZERO TO W-C.
       S-65.
           ADD 1 TO W-C.
           IF  W-C = 25
               GO TO S-70
           END-IF
           MOVE W-NA(W-C) TO W-HNA(W-C).
           IF  W-NA(W-C) NOT = SPACE
               GO TO S-65
           END-IF
           ADD 1 TO W-C.
           IF  W-C = 25
               GO TO S-70
           END-IF
           MOVE W-NA(W-C) TO W-HNA(W-C).
           IF  W-NA(W-C) NOT = SPACE
               GO TO S-65
           END-IF.
       S-70.
           EXIT.

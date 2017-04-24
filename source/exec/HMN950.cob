       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN950.
      *********************************************************
      *    PROGRAM         :  決算用　棚卸差額　明細表        *
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
           02  F              PIC  X(035) VALUE SPACE.
           02  F              PIC  N(021) VALUE
                "＊＊＊　　決算　棚卸差額　明細表　　＊＊＊".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(022) VALUE
                " ｺｰﾄﾞ   品　　　　　名".
           02  F              PIC  X(034) VALUE SPACE.
           02  F              PIC  X(034) VALUE
                "　　数　量  見積単価        金  額".
           02  F              PIC  X(037) VALUE
                "      原価        金  額       差　額".
       01  W-P.
           02  F              PIC  X(001).
           02  P-HCD          PIC  9(004).
           02  F              PIC  X(003).
           02  P-HNA          PIC  N(024).
           02  P-HNAD  REDEFINES P-HNA.
             03  F            PIC  N(015).
             03  P-BRN        PIC  N(003).
             03  F            PIC  N(006).
           02  P-SU           PIC --,---,--9.
           02  P-FT           PIC --,---,--9.
           02  P-FK           PIC --,---,---,--9.
           02  P-KT           PIC --,---,--9.
           02  P-KK           PIC --,---,---,--9.
           02  P-SG           PIC -----,---,--9.
       01  W-D.
           02  W-BC1          PIC  9(002).
           02  W-BMNO         PIC  9(001).
           02  W-BC3          PIC  9(002).
           02  W-SG           PIC S9(009).
           02  CNT            PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-NAD.
             03  W-NA    OCCURS  24  PIC  N(001).
           02  W-HNAD.
             03  W-HNA   OCCURS  24  PIC  N(001).
           02  W-NAME  REDEFINES  W-HNAD  PIC  N(024).
           02  W-C            PIC  9(002).
       01  WN-D.
           02  WN-DD    OCCURS   5.
             03  WN-SU        PIC S9(007).
             03  WN-FK        PIC S9(010).
             03  WN-KK        PIC S9(010).
             03  WN-SG        PIC S9(009).
       01  WT-D.
           02  WT-SU          PIC S9(007).
           02  WT-FK          PIC S9(010).
           02  WT-KK          PIC S9(010).
           02  WT-SG          PIC S9(009).
       01  WG-D.
           02  WG-SU          PIC S9(007).
           02  WG-FK          PIC S9(010).
           02  WG-KK          PIC S9(010).
           02  WG-SG          PIC S9(009).
       01  WS-D.
           02  WS-SU          PIC S9(007).
           02  WS-FK          PIC S9(010).
           02  WS-KK          PIC S9(010).
           02  WS-SG          PIC S9(009).
       01  WA-D.
           02  WA-SU          PIC S9(007).
           02  WA-FK          PIC S9(010).
           02  WA-KK          PIC S9(010).
           02  WA-SG          PIC S9(009).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LIHKBM.
           COPY LSPF.
      *FD  HKTWF
       01  HKTWF_HMN950.
           02  HKTWF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HKTWF_LNAME    PIC  X(012) VALUE "HKTWF_HMN950".
           02  F              PIC  X(001).
           02  HKTWF_KEY1     PIC  X(100) VALUE SPACE.
           02  HKTWF_SORT     PIC  X(100) VALUE SPACE.
           02  HKTWF_IDLST    PIC  X(100) VALUE SPACE.
           02  HKTWF_RES      USAGE  POINTER.
       01  HKTW-R.
           02  HKTW-HCD.
             03  HKTW-HCD1    PIC  9(004).
             03  HKTW-HCD2    PIC  9(002).
           02  F              PIC  X(001).
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
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　決算棚卸差額明細表　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1.
               04  FILLER  PIC  X(027) VALUE
                    "***  HKBM ﾅｼ (       )  ***".
               04  02E-ME1 PIC  X(007).
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "266" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "38" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "38" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "38" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "38" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "38" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "38" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "38" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "96" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "96" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" " " "24" "0" "34" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "01E-ME1" "X" "24" "15" "27" " " "E-ME1" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-ME1" "X" "24" "29" "7" "01E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING
           "02E-ME1" BY REFERENCE HKB-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
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
       M-10.
      *           READ HKTWF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HKTWF_PNAME1 BY REFERENCE HKTW-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HKTWF_IDLST HKTWF_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  HKTW-SU = ZERO
               GO TO M-10
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO WA-D WN-D.
           PERFORM S-10 THRU S-15.
       M-15.
           MOVE HKTW-BC3 TO W-BC3.
           MOVE ZERO TO WS-D.
       M-20.
           MOVE HKTW-BMNO TO W-BMNO.
           MOVE ZERO TO WG-D.
       M-25.
           MOVE HKTW-BC1 TO W-BC1.
           MOVE ZERO TO WT-D.
       M-30.
           MOVE HKTW-HCD TO HI-HCD.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊＊　マスター　ナシ　＊＊＊" TO HI-NAME
           END-IF
           MOVE SPACE TO W-NAD W-HNAD.
           MOVE HI-NAME TO W-NAD.
           MOVE ZERO TO W-C.
       M-35.
           ADD 1 TO W-C.
           IF  W-C = 25
               GO TO M-40
           END-IF
           MOVE W-NA(W-C) TO W-HNA(W-C).
           IF  W-NA(W-C) NOT = SPACE
               GO TO M-35
           END-IF
           ADD 1 TO W-C.
           IF  W-C = 25
               GO TO M-40
           END-IF
           MOVE W-NA(W-C) TO W-HNA(W-C).
           IF  W-NA(W-C) NOT = SPACE
               GO TO M-35
           END-IF.
       M-40.
           PERFORM S-20 THRU S-25.
       M-45.
      *           READ HKTWF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HKTWF_PNAME1 BY REFERENCE HKTW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  HKTW-SU = ZERO
               GO TO M-45
           END-IF
           IF  HKTW-BC3 NOT = W-BC3
               GO TO M-60
           END-IF
           IF  HKTW-BMNO NOT = W-BMNO
               GO TO M-55
           END-IF
           IF  HKTW-BC1 NOT = W-BC1
               GO TO M-50
           END-IF
           GO TO M-30.
       M-50.
           PERFORM S-30 THRU S-35.
           GO TO M-25.
       M-55.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
           GO TO M-20.
       M-60.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
           PERFORM S-50 THRU S-55.
           GO TO M-15.
       M-80.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
           PERFORM S-50 THRU S-55.
       M-85.
           MOVE SPACE TO W-P P-HNA.
           MOVE "　【　総　合　計　】　　　　　　" TO P-HNA.
           MOVE WA-SU TO P-SU.
           MOVE WA-FK TO P-FK.
           MOVE WA-KK TO P-KK.
           MOVE WA-SG TO P-SG.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           MOVE ZERO TO CNT.
       M-90.
           ADD 1 TO CNT.
           IF  CNT = 6
               GO TO M-95
           END-IF
           MOVE SPACE TO W-P P-HNA.
           MOVE SPACE TO HKB-KEY.
           MOVE "16" TO HKB-NO.
           IF  CNT = 1
               MOVE 22 TO HKB-BMC
           END-IF
           IF  CNT = 2
               MOVE 26 TO HKB-BMC
           END-IF
           IF  CNT = 3
               MOVE 29 TO HKB-BMC
           END-IF
           IF  CNT = 4
               MOVE 23 TO HKB-BMC
           END-IF
           IF  CNT = 5
               MOVE 24 TO HKB-BMC
           END-IF
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE SPACE TO HKB-BMN
           END-IF
           MOVE HKB-BMN TO P-BRN.
           MOVE WN-SU(CNT) TO P-SU.
           MOVE WN-FK(CNT) TO P-FK.
           MOVE WN-KK(CNT) TO P-KK.
           MOVE WN-SG(CNT) TO P-SG.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO M-90.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
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
           COMPUTE W-SG = HKTW-KKIN - HKTW-FKIN.
      *
           MOVE SPACE TO W-P.
           MOVE HKTW-HCD1 TO P-HCD.
           MOVE W-NAME TO P-HNA.
           MOVE HKTW-SU TO P-SU.
           MOVE HKTW-FT TO P-FT.
           MOVE HKTW-FKIN TO P-FK.
           MOVE HKTW-KT TO P-KT.
           MOVE HKTW-KKIN TO P-KK.
           MOVE W-SG TO P-SG.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD HKTW-SU TO WT-SU WN-SU(W-BMNO).
           ADD HKTW-FKIN TO WT-FK WN-FK(W-BMNO).
           ADD HKTW-KKIN TO WT-KK WN-KK(W-BMNO).
           ADD W-SG TO WT-SG WN-SG(W-BMNO).
       S-25.
           EXIT.
       S-30.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-HNA.
           MOVE "　　　　　　　　　　　　　　　　（　計　）" TO P-HNA.
           MOVE WT-SU TO P-SU.
           MOVE WT-FK TO P-FK.
           MOVE WT-KK TO P-KK.
           MOVE WT-SG TO P-SG.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WT-SU TO WG-SU.
           ADD WT-FK TO WG-FK.
           ADD WT-KK TO WG-KK.
           ADD WT-SG TO WG-SG.
       S-35.
           EXIT.
       S-40.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-HNA.
           MOVE "　　　　　　　　　　　　＜　小　計　＞　" TO P-HNA.
           MOVE WG-SU TO P-SU.
           MOVE WG-FK TO P-FK.
           MOVE WG-KK TO P-KK.
           MOVE WG-SG TO P-SG.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WG-SU TO WS-SU.
           ADD WG-FK TO WS-FK.
           ADD WG-KK TO WS-KK.
           ADD WG-SG TO WS-SG.
       S-45.
           EXIT.
       S-50.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-HNA.
           MOVE "　　　　　　　［　合　計　］　　　　　　" TO P-HNA.
           MOVE WS-SU TO P-SU.
           MOVE WS-FK TO P-FK.
           MOVE WS-KK TO P-KK.
           MOVE WS-SG TO P-SG.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WS-SU TO WA-SU.
           ADD WS-FK TO WA-FK.
           ADD WS-KK TO WA-KK.
           ADD WS-SG TO WA-SG.
       S-55.
           EXIT.

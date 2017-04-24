       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN960.
      *********************************************************
      *    PROGRAM         :  分類　決算棚卸差額　集計表　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  X(018) VALUE SPACE.
           02  F              PIC  N(019) VALUE
                "　＊＊＊　　分類　決算棚卸差額　集計表".
           02  F              PIC  N(005) VALUE  "　　＊＊＊".
           02  F              PIC  X(019) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "見積単価".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(003) VALUE "原　価".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(003) VALUE "差　額".
       01  W-P.
           02  P-M0           PIC  N(003).
           02  F              PIC  X(004).
           02  P-TM           PIC  N(013).
           02  P-MD    REDEFINES P-TM.
             03  P-M1         PIC  N(003).
             03  F            PIC  X(004).
             03  P-M2         PIC  N(008).
           02  P-SU           PIC ---,---,--9.
           02  P-KT           PIC --,---,--9.
           02  P-KKIN         PIC --,---,---,--9.
           02  P-FT           PIC --,---,--9.
           02  P-FKIN         PIC --,---,---,--9.
           02  P-SKIN         PIC -----,---,--9.
       01  W-DATA.
           02  W-DC           PIC  9(001) VALUE 0.
           02  W-KEY.
             03  W-BC3        PIC  9(002).
             03  W-BMC        PIC  9(002).
           02  W-BN.
             03  W-BN3        PIC  N(003).
             03  W-BMN        PIC  N(003).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  CNT            PIC  9(001).
           02  W-UD.
             03  W-KT         PIC S9(005).
             03  W-FT         PIC S9(005).
             03  W-SKIN       PIC S9(009).
           02  WP-D.
             03  WP-SU        PIC S9(007).
             03  WP-KKIN      PIC S9(010).
             03  WP-FKIN      PIC S9(010).
       01  WT-D.
           02  WT-SU          PIC S9(007).
           02  WT-KKIN        PIC S9(010).
           02  WT-FKIN        PIC S9(010).
       01  WS-D.
           02  WS-SU          PIC S9(007).
           02  WS-KKIN        PIC S9(010).
           02  WS-FKIN        PIC S9(010).
       01  WA-D.
           02  WA-SU          PIC S9(007).
           02  WA-KKIN        PIC S9(010).
           02  WA-FKIN        PIC S9(010).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIHKBM.
           COPY LSPF.
      *FD  HKTWF
       01  HKTWF_HMN960.
           02  HKTWF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HKTWF_LNAME    PIC  X(012) VALUE "HKTWF_HMN960".
           02  F              PIC  X(001).
           02  HKTWF_KEY1     PIC  X(100) VALUE SPACE.
           02  HKTWF_SORT     PIC  X(100) VALUE SPACE.
           02  HKTWF_IDLST    PIC  X(100) VALUE SPACE.
           02  HKTWF_RES      USAGE  POINTER.
       01  HKTW-R.
           02  F              PIC  X(007).
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
           02  F              PIC  X(077).
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　分類　決算棚卸差額　集計表　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1.
               04  FILLER  PIC  X(027) VALUE
                    "***  HKBM ﾅｼ (       )  ***".
               04  02E-ME1 PIC  X(007).
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
           "C-MID" " " "0" "0" "322" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "46" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "44" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "44" " " "C-ERR" RETURNING RESU.
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
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO HKTWF_PNAME1.
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
      *
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           CALL "PR_Open" RETURNING RESP.
      *
           MOVE DATE-02R TO H-DATE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE ZERO TO WA-D.
       M-15.
           MOVE HKTW-BC3 TO W-BC3.
           MOVE ZERO TO WS-D CHK.
           MOVE SPACE TO HKB-KEY.
           MOVE "14" TO HKB-NO.
           MOVE W-BC3 TO HKB-BR3.
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
               MOVE SPACE TO HKB-BRN3.
           MOVE HKB-BRN3 TO W-BN3.
       M-20.
           MOVE HKTW-BMC TO W-BMC.
           MOVE ZERO TO WT-D CHK2 CNT.
           MOVE SPACE TO HKB-KEY.
           MOVE "16" TO HKB-NO.
           MOVE W-BMC TO HKB-BMC.
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
           MOVE HKB-BMN TO W-BMN.
       M-25.
           MOVE SPACE TO HKB-KEY.
           MOVE "11" TO HKB-NO.
           MOVE HKTW-BC1 TO HKB-BR1.
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
               MOVE SPACE TO HKB-BRN1
           END-IF
      *
           MOVE ZERO TO WP-D.
           MOVE HKTW-SU TO WP-SU.
           MOVE HKTW-KKIN TO WP-KKIN.
           MOVE HKTW-FKIN TO WP-FKIN.
      *
           MOVE 0 TO W-DC.
           PERFORM S-05 THRU S-15.
      *
           ADD HKTW-SU TO WT-SU.
           ADD HKTW-KKIN TO WT-KKIN.
           ADD HKTW-FKIN TO WT-FKIN.
           IF  CNT < 2
               ADD 1 TO CNT
           END-IF.
       M-30.
      *           READ HKTWF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HKTWF_PNAME1 BY REFERENCE HKTW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HKTW-SU = ZERO
               GO TO M-30
           END-IF
           IF  HKTW-BC3 NOT = W-BC3
               GO TO M-35
           END-IF
           IF  HKTW-BMC = W-BMC
               GO TO M-25
           END-IF
           PERFORM S-20 THRU S-30.
           GO TO M-20.
       M-35.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-40.
           GO TO M-15.
       M-90.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-40.
           PERFORM S-45 THRU S-50.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE HKTWF_IDLST HKTWF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE ZERO TO W-UD.
           IF  WP-SU NOT = ZERO
               IF  WP-KKIN NOT = ZERO
                   COMPUTE W-KT ROUNDED = WP-KKIN / WP-SU
               END-IF
           END-IF
           IF  WP-SU NOT = ZERO
               IF  WP-FKIN NOT = ZERO
                   COMPUTE W-FT ROUNDED = WP-FKIN / WP-SU
               END-IF
           END-IF
           COMPUTE W-SKIN = WP-KKIN - WP-FKIN.
      *
           MOVE SPACE TO W-P.
           IF  W-DC = 1
               MOVE "　　　　　　（　小　計　）" TO P-TM
               GO TO S-10
           END-IF
           IF  W-DC = 2
               MOVE "　　　　［　合　計　］　　" TO P-TM
               GO TO S-10
           END-IF
           IF  W-DC = 3
               MOVE "　【　総　合　計　】　　　" TO P-TM
               GO TO S-10
           END-IF
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               MOVE 0 TO CHK2
               MOVE W-BN3 TO P-M0
           END-IF
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               MOVE W-BMN TO P-M1
           END-IF
           MOVE HKB-BRN1 TO P-M2.
       S-10.
           MOVE WP-SU TO P-SU.
           MOVE W-KT TO P-FT.
           MOVE WP-KKIN TO P-FKIN.
           MOVE W-FT TO P-KT.
           MOVE WP-FKIN TO P-KKIN.
           MOVE W-SKIN TO P-SKIN.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           IF  CNT NOT = 2
               GO TO S-25
           END-IF
      *
           MOVE ZERO TO WP-D.
           MOVE WT-D TO WP-D.
      *
           MOVE 1 TO W-DC.
           PERFORM S-05 THRU S-15.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-25.
           ADD WT-SU TO WS-SU.
           ADD WT-KKIN TO WS-KKIN.
           ADD WT-FKIN TO WS-FKIN.
       S-30.
           EXIT.
       S-35.
      *
           MOVE ZERO TO WP-D.
           MOVE WS-D TO WP-D.
      *
           MOVE 2 TO W-DC.
           PERFORM S-05 THRU S-15.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WS-SU TO WA-SU.
           ADD WS-KKIN TO WA-KKIN.
           ADD WS-FKIN TO WA-FKIN.
       S-40.
           EXIT.
       S-45.
      *
           MOVE ZERO TO WP-D.
           MOVE WA-D TO WP-D.
      *
           MOVE 3 TO W-DC.
           PERFORM S-05 THRU S-15.
       S-50.
           EXIT.

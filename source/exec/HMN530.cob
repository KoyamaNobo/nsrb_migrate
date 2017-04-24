       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN530.
      *********************************************************
      *    PROGRAM         :  履物製品棚卸表　　　　　　　　　*
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
           02  F              PIC  N(018) VALUE
                "＊＊＊　　履物　製品棚卸表　　＊＊＊".
           02  F              PIC  X(024) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(038) VALUE SPACE.
           02  F              PIC  X(007) VALUE "I----  ".
           02  F              PIC  N(004) VALUE "帳簿残高".
           02  F              PIC  X(018) VALUE "  -----I  I-----  ".
           02  F              PIC  N(003) VALUE "棚　卸".
           02  F              PIC  X(017) VALUE "  ------I  I---  ".
           02  F              PIC  N(004) VALUE "棚卸誤差".
           02  F              PIC  X(006) VALUE "  ---I".
       01  HEAD3.
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(014) VALUE
                "金　額　　数　量　　　金　額".
       01  W-P.
           02  P-TM           PIC  N(018).
           02  P-MD    REDEFINES P-TM.
             03  P-M0         PIC  N(003).
             03  F            PIC  X(004).
             03  P-M1         PIC  N(003).
             03  F            PIC  X(004).
             03  P-M2         PIC  N(008).
           02  P-ZKS          PIC ---,---,--9.
           02  P-ZKK          PIC --,---,---,--9.
           02  P-YKS          PIC --,---,--9.
           02  P-YKK          PIC --,---,---,--9.
           02  P-ZGS          PIC ---,---,--9.
           02  P-ZGK          PIC ----,---,--9.
       01  W-DATA.
           02  W-DC           PIC  9(001) VALUE 0.
           02  W-EDC          PIC  9(001) VALUE 0.
           02  W-KEY.
             03  W-BC3        PIC  9(002).
             03  W-BMC        PIC  9(002).
             03  W-BC1        PIC  9(002).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  W-BC           PIC  9(002).
           02  W-NC           PIC  9(002).
           02  CNT            PIC  9(001).
           02  W-D.
             03  W-ZKK        PIC S9(009).
             03  W-YKK        PIC S9(009).
           02  WN-D.
             03  WN-ZKS       PIC S9(006).
             03  WN-ZKK       PIC S9(009).
             03  WN-YKS       PIC S9(006).
             03  WN-YKK       PIC S9(009).
           02  WP-D.
             03  WP-ZKS       PIC S9(007).
             03  WP-ZKK       PIC S9(010).
             03  WP-YKS       PIC S9(007).
             03  WP-YKK       PIC S9(010).
             03  WP-ZGS       PIC S9(007).
             03  WP-ZGK       PIC S9(009).
       01  WT-D.
           02  WT-ZKS         PIC S9(007).
           02  WT-ZKK         PIC S9(010).
           02  WT-YKS         PIC S9(007).
           02  WT-YKK         PIC S9(010).
       01  WS-D.
           02  WS-ZKS         PIC S9(007).
           02  WS-ZKK         PIC S9(010).
           02  WS-YKS         PIC S9(007).
           02  WS-YKK         PIC S9(010).
       01  WA-D.
           02  WA-ZKS         PIC S9(007).
           02  WA-ZKK         PIC S9(010).
           02  WA-YKS         PIC S9(007).
           02  WA-YKK         PIC S9(010).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIHKBM.
           COPY LSPF.
      *FD  HB-F
       01  HB-F_HMN530.
           02  HB-F_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HB-F_LNAME     PIC  X(011) VALUE "HB-F_HMN530".
           02  F              PIC  X(001).
           02  HB-F_KEY1      PIC  X(100) VALUE SPACE.
           02  HB-F_SORT      PIC  X(100) VALUE SPACE.
           02  HB-F_IDLST     PIC  X(100) VALUE SPACE.
           02  HB-F_RES       USAGE  POINTER.
       01  HB-R.
           02  HB-HCD         PIC  9(006).
           02  HB-ZKS         PIC S9(006).
           02  HB-YKS         PIC S9(006).
           02  HB-FT          PIC  9(005).
           02  HB-BC1         PIC  9(002).
           02  HB-BC2.
             03  HB-BC21      PIC  9(001).
             03  HB-BC22      PIC  9(001).
           02  HB-BC3         PIC  9(002).
           02  HB-BMC         PIC  9(002).
           02  HB-BMNO        PIC  9(001).
           02  F              PIC  X(032).
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
                "＊＊＊　　　履物　製品棚卸表　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
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
           "C-ERR" " " "0" "0" "44" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "44" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" " " "24" "0" "34" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "01E-ME1" "X" "24" "15" "27" "E-ME1" " " RETURNING RESU.
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
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO HB-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HB-F_PNAME1 " " BY REFERENCE HB-F_IDLST "0".
       M-10.
      *           READ HB-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HB-F_PNAME1 BY REFERENCE HB-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HB-F_IDLST HB-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
       M-15.
           PERFORM S-55 THRU S-75.
           IF  W-EDC = 1
               IF  W-DC = 0
                   CALL "DB_F_Close" USING
                    BY REFERENCE HB-F_IDLST HB-F_PNAME1
                   CALL "SD_Output" USING
                    "C-CLEAR" C-CLEAR "p" RETURNING RESU
                   CALL "DB_Close"
                   STOP RUN
               END-IF
           END-IF
           IF  W-DC = 0
               GO TO M-15
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
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
           MOVE ZERO TO WA-D.
       M-20.
           MOVE W-BC3 TO W-BC.
           MOVE ZERO TO WS-D CHK.
       M-25.
           MOVE W-BMC TO W-NC.
           MOVE ZERO TO WT-D CHK2 CNT.
       M-30.
           MOVE SPACE TO W-P.
           IF CHK1 NOT = 0
               GO TO M-35
           END-IF
           MOVE 1 TO CHK1.
           MOVE SPACE TO HKB-KEY.
           MOVE "14" TO HKB-NO.
           MOVE W-BC TO HKB-BR3.
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
               MOVE SPACE TO HKB-BRN3
           END-IF
           MOVE HKB-BRN3 TO P-M0.
       M-35.
           IF CHK2 NOT = 0
               GO TO M-40
           END-IF
           MOVE 1 TO CHK2.
           MOVE SPACE TO HKB-KEY.
           MOVE "16" TO HKB-NO.
           MOVE W-NC TO HKB-BMC.
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
               MOVE SPACE TO HKB-BMN.
           MOVE HKB-BRN22 TO P-M1.
       M-40.
           MOVE SPACE TO HKB-KEY.
           MOVE "11" TO HKB-NO.
           MOVE W-BC1 TO HKB-BR1.
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
           MOVE HKB-BRN1 TO P-M2.
      *
           MOVE ZERO TO WP-D.
           MOVE WN-ZKS TO WP-ZKS.
           MOVE WN-ZKK TO WP-ZKK.
           MOVE WN-YKS TO WP-YKS.
           MOVE WN-YKK TO WP-YKK.
      *
           PERFORM S-05 THRU S-10.
      *
           ADD WN-ZKS TO WT-ZKS.
           ADD WN-ZKK TO WT-ZKK.
           ADD WN-YKS TO WT-YKS.
           ADD WN-YKK TO WT-YKK.
           IF  CNT < 2
               ADD 1 TO CNT
           END-IF
           IF  W-EDC = 1
               GO TO M-90
           END-IF.
       M-45.
           PERFORM S-55 THRU S-75.
           IF  W-BC3 NOT = W-BC
               GO TO M-50
           END-IF
           IF  W-BMC = W-NC
               GO TO M-30
           END-IF
           PERFORM S-15 THRU S-25.
           GO TO M-25.
       M-50.
           PERFORM S-15 THRU S-25.
           PERFORM S-30 THRU S-35.
           GO TO M-20.
       M-90.
           PERFORM S-15 THRU S-25.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HB-F_IDLST HB-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           COMPUTE WP-ZGS = WP-YKS - WP-ZKS.
           COMPUTE WP-ZGK = WP-YKK - WP-ZKK.
           MOVE WP-ZKS TO P-ZKS.
           MOVE WP-ZKK TO P-ZKK.
           MOVE WP-YKS TO P-YKS.
           MOVE WP-YKK TO P-YKK.
           MOVE WP-ZGS TO P-ZGS.
           MOVE WP-ZGK TO P-ZGK.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-10.
           EXIT.
       S-15.
           IF  CNT NOT = 2
               GO TO S-20
           END-IF
           MOVE SPACE TO W-P.
           MOVE "　　　　　　　　　（　計　）　　　　" TO P-TM.
      *
           MOVE WT-D TO WP-D.
      *
           PERFORM S-05 THRU S-10.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-20.
           ADD WT-ZKS TO WS-ZKS.
           ADD WT-ZKK TO WS-ZKK.
           ADD WT-YKS TO WS-YKS.
           ADD WT-YKK TO WS-YKK.
       S-25.
           EXIT.
       S-30.
           MOVE SPACE TO W-P.
           MOVE "　　　　　［　小　計　］　　　　　　" TO P-TM.
      *
           MOVE WS-D TO WP-D.
      *
           PERFORM S-05 THRU S-10.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WS-ZKS TO WA-ZKS.
           ADD WS-ZKK TO WA-ZKK.
           ADD WS-YKS TO WA-YKS.
           ADD WS-YKK TO WA-YKK.
       S-35.
           EXIT.
       S-40.
           MOVE SPACE TO W-P.
           MOVE "　　【　合　計　】　　　　　　　　　" TO P-TM.
      *
           MOVE WA-D TO WP-D.
      *
           PERFORM S-05 THRU S-10.
       S-45.
           EXIT.
       S-55.
           MOVE ZERO TO WN-D.
           MOVE HB-BC3 TO W-BC3.
           MOVE HB-BMC TO W-BMC.
           MOVE HB-BC1 TO W-BC1.
       S-60.
           MOVE ZERO TO W-D.
           COMPUTE W-ZKK = HB-ZKS * HB-FT.
           COMPUTE W-YKK = HB-YKS * HB-FT.
           ADD HB-ZKS TO WN-ZKS.
           ADD W-ZKK TO WN-ZKK.
           ADD HB-YKS TO WN-YKS.
           ADD W-YKK TO WN-YKK.
       S-65.
      *           READ HB-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HB-F_PNAME1 BY REFERENCE HB-R " " RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-EDC
               GO TO S-70
           END-IF
           IF  ZERO = HB-ZKS AND HB-YKS
               GO TO S-65
           END-IF
           IF (HB-BC3 = W-BC3) AND
              (HB-BMC = W-BMC) AND (HB-BC1 = W-BC1)
               GO TO S-60
           END-IF.
       S-70.
           IF  ZERO = WN-ZKS AND WN-ZKK AND WN-YKS AND WN-YKK
               IF  W-EDC = 0
                   GO TO S-55
               ELSE
                   GO TO S-75
               END-IF
           END-IF
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF.
       S-75.
           EXIT.

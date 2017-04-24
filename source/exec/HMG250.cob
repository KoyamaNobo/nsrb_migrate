       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG250.
      *********************************************************
      *    PROGRAM         :  履物分類別　製品受払表      　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=作表 , 1=ＥＸＣＥＬ 2=作表参  *
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
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0128".
           02  W-FID12        PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  H-SAN          PIC  N(001) VALUE SPACE.
           02  F              PIC  X(042) VALUE SPACE.
           02  F              PIC  N(023) VALUE
                "＊＊＊　　履　物　製　品　受　払　表　　＊＊＊".
           02  F              PIC  X(032) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(045) VALUE
                "I----  前月繰越  -----I I---  当月受入  ---I ".
           02  F              PIC  X(045) VALUE
                "I---  当月払出  ---I I----  翌月繰越  -----I ".
           02  F              PIC  X(020) VALUE "I----　増　減　----I".
       01  HEAD3.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
       01  W-P.
           02  P-TM           PIC  N(016).
           02  P-DM    REDEFINES P-TM.
             03  P-M0         PIC  N(004).
             03  P-M1         PIC  N(004).
             03  P-M2         PIC  N(008).
           02  P-ZKS          PIC --,---,--9.
           02  P-ZKK          PIC --,---,---,--9.
           02  P-SSU          PIC -----,--9.
           02  P-SKN          PIC ----,---,--9.
           02  P-USU          PIC -----,--9.
           02  P-UG           PIC ----,---,--9.
           02  P-YKS          PIC --,---,--9.
           02  P-YKK          PIC --,---,---,--9.
           02  P-ZGS          PIC -----,--9.
           02  P-ZGK          PIC ----,---,--9.
       01  W-DATA.
           02  W-BCD.
             03  W-CC         PIC  9(002).
             03  W-NC         PIC  9(002).
             03  W-BC         PIC  9(002).
           02  W-D.
             03  W-ZGS        PIC S9(007).
             03  W-ZGK        PIC S9(010).
           02  WK-D.
             03  WK-ZKS       PIC S9(007).
             03  WK-ZKK       PIC S9(010).
             03  WK-SSU       PIC S9(007).
             03  WK-SKN       PIC S9(010).
             03  WK-USU       PIC S9(007).
             03  WK-UG        PIC S9(010).
             03  WK-YKS       PIC S9(007).
             03  WK-YKK       PIC S9(010).
           02  WT-D.
             03  WT-ZKS       PIC S9(007).
             03  WT-ZKK       PIC S9(010).
             03  WT-SSU       PIC S9(007).
             03  WT-SKN       PIC S9(010).
             03  WT-USU       PIC S9(007).
             03  WT-UG        PIC S9(010).
             03  WT-YKS       PIC S9(007).
             03  WT-YKK       PIC S9(010).
           02  WS-D.
             03  WS-ZKS       PIC S9(007).
             03  WS-ZKK       PIC S9(010).
             03  WS-SSU       PIC S9(007).
             03  WS-SKN       PIC S9(010).
             03  WS-USU       PIC S9(007).
             03  WS-UG        PIC S9(010).
             03  WS-YKS       PIC S9(007).
             03  WS-YKK       PIC S9(010).
           02  WA-D.
             03  WA-ZKS       PIC S9(007).
             03  WA-ZKK       PIC S9(010).
             03  WA-SSU       PIC S9(007).
             03  WA-SKN       PIC S9(010).
             03  WA-USU       PIC S9(007).
             03  WA-UG        PIC S9(010).
             03  WA-YKS       PIC S9(007).
             03  WA-YKK       PIC S9(010).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  CNT            PIC  9(001).
           02  W-BRN3         PIC  N(003).
           02  W-BMN          PIC  N(003).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIHKBM.
           COPY LSPF.
      *FD  HC-F
       01  HC-F_HMG250.
           02  HC-F_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HC-F_LNAME     PIC  X(011) VALUE "HC-F_HMG250".
           02  F              PIC  X(001).
           02  HC-F_KEY1      PIC  X(100) VALUE SPACE.
           02  HC-F_SORT      PIC  X(100) VALUE SPACE.
           02  HC-F_IDLST     PIC  X(100) VALUE SPACE.
           02  HC-F_RES       USAGE  POINTER.
       01  HC-R.
           02  HC-BC.
             03  HC-BC1       PIC  9(002).
             03  HC-BC2.
               04  HC-BC21    PIC  9(001).
               04  HC-BC22    PIC  9(001).
             03  HC-BC3       PIC  9(002).
           02  HC-BMC         PIC  9(002).
           02  HC-BMNO        PIC  9(001).
           02  HC-ZKS         PIC S9(007).
           02  HC-ZKK         PIC S9(010).
           02  HC-NS          PIC S9(007).
           02  HC-SKN         PIC S9(010).
           02  HC-SS          PIC S9(007).
           02  HC-UKN         PIC S9(010).
           02  HC-YKS         PIC S9(007).
           02  HC-YKK         PIC S9(010).
           02  HC-UG          PIC S9(010).
           02  F              PIC  X(041).
       77  F                  PIC  X(001).
      *FD  EXLF
       01  EXLF_HMG250.
           02  EXLF_PNAME1    PIC  X(009) VALUE "WK0256000".
           02  F              PIC  X(001).
           02  EXLF_LNAME     PIC  X(011) VALUE "EXLF_HMG250".
           02  F              PIC  X(001).
           02  EXLF_KEY1      PIC  X(100) VALUE SPACE.
           02  EXLF_SORT      PIC  X(100) VALUE SPACE.
           02  EXLF_IDLST     PIC  X(100) VALUE SPACE.
           02  EXLF_RES       USAGE  POINTER.
       01  EXL-R.
           02  EXL-M0         PIC  N(003).
           02  EXL-M1         PIC  N(003).
           02  EXL-M2         PIC  N(008).
           02  EXL-ZKS        PIC S9(007).
           02  EXL-ZKK        PIC S9(009).
           02  EXL-SSU        PIC S9(007).
           02  EXL-SKN        PIC S9(009).
           02  EXL-USU        PIC S9(007).
           02  EXL-UG         PIC S9(009).
           02  EXL-YKS        PIC S9(007).
           02  EXL-YKK        PIC S9(009).
           02  EXL-ZGS        PIC S9(007).
           02  EXL-ZGK        PIC S9(009).
           02  F              PIC  X(148).
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
                "＊＊＊　　製　品　受　払　表　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
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
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 2
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12.
           MOVE W-FID1 TO WK0128ID.
           MOVE WK0128ID TO HC-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HC-F_PNAME1 " " BY REFERENCE HC-F_IDLST "0".
       M-10.
      *           READ HC-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HC-F_PNAME1 BY REFERENCE HC-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HC-F_IDLST HC-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ZERO = HC-ZKS AND HC-ZKK AND HC-NS AND HC-SKN AND
                     HC-SS AND HC-UKN AND HC-YKS AND HC-YKK AND HC-UG
               GO TO M-10
           END-IF
      *
           MOVE ZERO TO WA-D.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           IF  JS-SIGN = 1
               CALL "DB_F_Open" USING
                "OUTPUT" EXLF_PNAME1 " " BY REFERENCE EXLF_IDLST "0"
               GO TO M-15
           END-IF
           IF  JS-SIGN = 2
               MOVE "参" TO H-SAN
           END-IF
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
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       M-15.
           MOVE HC-BC3 TO W-CC.
           MOVE ZERO TO WS-D CHK.
           MOVE SPACE TO HKB-KEY.
           MOVE "14" TO HKB-NO.
           MOVE W-CC TO HKB-BR3.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BRN3
           END-IF
           MOVE HKB-BRN3 TO W-BRN3.
       M-20.
           MOVE HC-BMC TO W-NC.
           MOVE ZERO TO WT-D CHK2 CNT.
           MOVE SPACE TO HKB-KEY.
           MOVE "16" TO HKB-NO.
           MOVE W-NC TO HKB-BMC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BMN
           END-IF
           MOVE HKB-BMN TO W-BMN.
       M-25.
           IF  JS-SIGN = 1
               INITIALIZE EXL-R
           ELSE
               MOVE SPACE TO W-P
           END-IF
           MOVE SPACE TO P-TM.
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               IF  JS-SIGN = 1
                   MOVE W-BRN3 TO EXL-M0
               ELSE
                   MOVE W-BRN3 TO P-M0
               END-IF
           END-IF
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               IF  JS-SIGN = 1
                   MOVE W-BMN TO EXL-M1
               ELSE
                   MOVE W-BMN TO P-M1
               END-IF
           END-IF
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "11" TO HKB-NO.
           MOVE HC-BC1 TO HKB-BR1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BRN1
           END-IF
           IF  JS-SIGN = 1
               MOVE HKB-BRN1 TO EXL-M2
           ELSE
               MOVE HKB-BRN1 TO P-M2
           END-IF
           MOVE HC-ZKS TO WK-ZKS.
           MOVE HC-ZKK TO WK-ZKK.
           MOVE HC-NS  TO WK-SSU.
           MOVE HC-SKN TO WK-SKN.
           MOVE HC-SS  TO WK-USU.
           MOVE HC-UG  TO WK-UG.
           MOVE HC-YKS TO WK-YKS.
           MOVE HC-YKK TO WK-YKK.
           PERFORM S-05 THRU S-10.
      *
           ADD HC-ZKS TO WT-ZKS.
           ADD HC-ZKK TO WT-ZKK.
           ADD HC-NS TO WT-SSU.
           ADD HC-SKN TO WT-SKN.
           ADD HC-SS TO WT-USU.
           ADD HC-UG TO WT-UG.
           ADD HC-YKS TO WT-YKS.
           ADD HC-YKK TO WT-YKK.
           IF  CNT < 2
               ADD 1 TO CNT
           END-IF.
       M-35.
      *           READ HC-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HC-F_PNAME1 BY REFERENCE HC-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  ZERO = HC-ZKS AND HC-ZKK AND HC-NS AND HC-SKN AND
                     HC-SS AND HC-UKN AND HC-YKS AND HC-YKK AND HC-UG
               GO TO M-35
           END-IF
           IF  HC-BC3 NOT = W-CC
               GO TO M-40
           END-IF
           IF  HC-BMC = W-NC
               GO TO M-25
           END-IF
           PERFORM S-15 THRU S-25.
           GO TO M-20.
       M-40.
           PERFORM S-15 THRU S-25.
           PERFORM S-30 THRU S-35.
           GO TO M-15.
       M-80.
           PERFORM S-15 THRU S-25.
           PERFORM S-30 THRU S-35.
           IF  JS-SIGN = 1
               INITIALIZE EXL-R
               MOVE "【　合　計　】　" TO EXL-M2
           ELSE
               MOVE SPACE TO W-P
               MOVE "　【　合　計　】　　　　" TO P-TM
           END-IF
           MOVE WA-D TO WK-D.
           PERFORM S-05 THRU S-10.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HC-F_IDLST HC-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           IF  JS-SIGN = 1
               CALL "DB_F_Close" USING
                BY REFERENCE EXLF_IDLST EXLF_PNAME1
           ELSE
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           COMPUTE W-ZGS = WK-YKS - WK-ZKS.
           COMPUTE W-ZGK = WK-YKK - WK-ZKK.
           IF  JS-SIGN = 1
               MOVE WK-ZKS TO EXL-ZKS
               MOVE WK-ZKK TO EXL-ZKK
               MOVE WK-SSU TO EXL-SSU
               MOVE WK-SKN TO EXL-SKN
               MOVE WK-USU TO EXL-USU
               MOVE WK-UG TO EXL-UG
               MOVE WK-YKS TO EXL-YKS
               MOVE WK-YKK TO EXL-YKK
               MOVE W-ZGS TO EXL-ZGS
               MOVE W-ZGK TO EXL-ZGK
      *               WRITE EXL-R
      *//////////////
               CALL "DB_Insert" USING
                EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET
               GO TO S-10
           END-IF
           MOVE WK-ZKS TO P-ZKS.
           MOVE WK-ZKK TO P-ZKK.
           MOVE WK-SSU TO P-SSU.
           MOVE WK-SKN TO P-SKN.
           MOVE WK-USU TO P-USU.
           MOVE WK-UG TO P-UG.
           MOVE WK-YKS TO P-YKS.
           MOVE WK-YKK TO P-YKK.
           MOVE W-ZGS TO P-ZGS.
           MOVE W-ZGK TO P-ZGK.
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
           IF  JS-SIGN = 1
               INITIALIZE EXL-R
               MOVE "　　　（　計　）" TO EXL-M2
           ELSE
               MOVE SPACE TO W-P
               MOVE "　　　　　　　（　計　）" TO P-TM
           END-IF
           MOVE WT-D TO WK-D.
           PERFORM S-05 THRU S-10.
       S-20.
           IF  JS-SIGN NOT = 1
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
      *
           ADD WT-ZKS TO WS-ZKS.
           ADD WT-ZKK TO WS-ZKK.
           ADD WT-SSU TO WS-SSU.
           ADD WT-SKN TO WS-SKN.
           ADD WT-USU TO WS-USU.
           ADD WT-UG TO WS-UG.
           ADD WT-YKS TO WS-YKS.
           ADD WT-YKK TO WS-YKK.
       S-25.
           EXIT.
       S-30.
           IF  JS-SIGN = 1
               INITIALIZE EXL-R
               MOVE "　［　小　計　］" TO EXL-M2
           ELSE
               MOVE SPACE TO W-P
               MOVE "　　　　［　小　計　］　" TO P-TM
           END-IF
           MOVE WS-D TO WK-D.
           PERFORM S-05 THRU S-10.
           IF  JS-SIGN NOT = 1
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
      *
           ADD WS-ZKS TO WA-ZKS.
           ADD WS-ZKK TO WA-ZKK.
           ADD WS-SSU TO WA-SSU.
           ADD WS-SKN TO WA-SKN.
           ADD WS-USU TO WA-USU.
           ADD WS-UG TO WA-UG.
           ADD WS-YKS TO WA-YKS.
           ADD WS-YKK TO WA-YKK.
       S-35.
           EXIT.

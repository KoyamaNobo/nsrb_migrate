       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN620.
      *********************************************************
      *    PROGRAM         :  履物分類別棚卸誤差表            *
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
       77  JS-SIGN            PIC  9(001).
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0064".
           02  W-FID12        PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  N(021) VALUE
                "＊＊＊　　履物　分類　棚卸誤差表　　＊＊＊".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(038) VALUE SPACE.
           02  F              PIC  X(008) VALUE "I-----  ".
           02  F              PIC  N(003) VALUE "帳　簿".
           02  F              PIC  X(019) VALUE "  ------I  I-----  ".
           02  F              PIC  N(003) VALUE "棚　卸".
           02  F              PIC  X(018) VALUE "  ------I  I----  ".
           02  F              PIC  N(003) VALUE "誤　差".
           02  F              PIC  X(007) VALUE "  ----I".
       01  HEAD3.
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
       01  W-P.
           02  P-M0           PIC  N(003).
           02  F              PIC  X(004).
           02  P-TM           PIC  N(013).
           02  P-MD    REDEFINES P-TM.
             03  P-M1         PIC  N(003).
             03  F            PIC  X(004).
             03  P-M2         PIC  N(008).
           02  P-CSU          PIC ---,---,--9.
           02  P-CKIN         PIC --,---,---,--9.
           02  P-JSU          PIC ---,---,--9.
           02  P-JKIN         PIC --,---,---,--9.
           02  P-KSU          PIC --,---,--9.
           02  P-KKIN         PIC ----,---,--9.
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-BC3          PIC  9(002).
           02  W-BMC          PIC  9(002).
           02  W-BC1          PIC  9(002).
           02  W-BN3          PIC  N(003).
           02  W-BMN          PIC  N(003).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  CNT            PIC  9(001).
       01  W-TD.
           02  WT-CSU         PIC S9(007).
           02  WT-CKIN        PIC S9(010).
           02  WT-JSU         PIC S9(007).
           02  WT-JKIN        PIC S9(010).
           02  WT-KSU         PIC S9(007).
           02  WT-KKIN        PIC S9(009).
       01  W-GD.
           02  WG-CSU         PIC S9(007).
           02  WG-CKIN        PIC S9(010).
           02  WG-JSU         PIC S9(007).
           02  WG-JKIN        PIC S9(010).
           02  WG-KSU         PIC S9(007).
           02  WG-KKIN        PIC S9(009).
       01  W-SD.
           02  WS-CSU         PIC S9(007).
           02  WS-CKIN        PIC S9(010).
           02  WS-JSU         PIC S9(007).
           02  WS-JKIN        PIC S9(010).
           02  WS-KSU         PIC S9(007).
           02  WS-KKIN        PIC S9(009).
       01  W-AD.
           02  WA-CSU         PIC S9(007).
           02  WA-CKIN        PIC S9(010).
           02  WA-JSU         PIC S9(007).
           02  WA-JKIN        PIC S9(010).
           02  WA-KSU         PIC S9(007).
           02  WA-KKIN        PIC S9(009).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHKBM.
           COPY LSPF.
      *FD  TSW-F
       01  TSW-F_HMN620.
           02  TSW-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TSW-F_LNAME    PIC  X(012) VALUE "TSW-F_HMN620".
           02  F              PIC  X(001).
           02  TSW-F_KEY1     PIC  X(100) VALUE SPACE.
           02  TSW-F_SORT     PIC  X(100) VALUE SPACE.
           02  TSW-F_IDLST    PIC  X(100) VALUE SPACE.
           02  TSW-F_RES      USAGE  POINTER.
       01  TSW-R.
           02  TSW-HCD        PIC  9(006).
           02  TSW-CSU        PIC S9(006).
           02  TSW-JSU        PIC S9(006).
           02  TSW-FT         PIC  9(005).
           02  TSW-BC1        PIC  9(002).
           02  TSW-BC2        PIC  9(002).
           02  TSW-BC3        PIC  9(002).
           02  TSW-BMC        PIC  9(002).
           02  TSW-BMNO       PIC  9(001).
           02  F              PIC  X(032).
       77  F                  PIC  X(001).
      *FD  EXLF
       01  EXLF_HMN620.
           02  EXLF_PNAME1    PIC  X(009) VALUE "WK0128000".
           02  F              PIC  X(001).
           02  EXLF_LNAME     PIC  X(011) VALUE "EXLF_HMN620".
           02  F              PIC  X(001).
           02  EXLF_KEY1      PIC  X(100) VALUE SPACE.
           02  EXLF_SORT      PIC  X(100) VALUE SPACE.
           02  EXLF_IDLST     PIC  X(100) VALUE SPACE.
           02  EXLF_RES       USAGE  POINTER.
       01  EXL-R.
           02  EXL-M0         PIC  N(003).
           02  EXL-M1         PIC  N(003).
           02  EXL-M2         PIC  N(008).
           02  EXL-CSU        PIC S9(007).
           02  EXL-CKIN       PIC S9(009).
           02  EXL-JSU        PIC S9(007).
           02  EXL-JKIN       PIC S9(009).
           02  EXL-KSU        PIC S9(007).
           02  EXL-KKIN       PIC S9(009).
           02  F              PIC  X(052).
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
                "＊＊＊　　　履物　分類　棚卸誤差表　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-EXL   PIC  N(012) VALUE
                "（　Ｅｘｃｅｌ　変換　）".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  HKBM ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
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
           "C-MID" " " "0" "0" "344" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING
           "08C-MID" "X" "22" "22" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "22" "39" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "24" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-EXL" "N" "12" "21" "24" " " "C-DSP" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "46" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "46" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
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
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "D-EXL" D-EXL "p" RETURNING RESU
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "D-EXL" D-EXL "p" RETURNING RESU
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12.
           MOVE W-FID1 TO WK0064ID.
           MOVE WK0064ID TO TSW-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TSW-F_PNAME1 " " BY REFERENCE TSW-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           IF  JS-SIGN = 1
               CALL "DB_F_Open" USING
                "OUTPUT" EXLF_PNAME1 " " BY REFERENCE EXLF_IDLST "0"
               GO TO M-15
           END-IF
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO W-AD.
       M-15.
      *           READ TSW-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TSW-F_PNAME1 BY REFERENCE TSW-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  ZERO = TSW-CSU AND TSW-JSU
               GO TO M-15
           END-IF
           IF  JS-SIGN = 0
               PERFORM MID-020 THRU MID-EX
           END-IF.
       M-20.
           MOVE TSW-BC3 TO W-BC3.
           MOVE ZERO TO W-SD CHK.
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
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE SPACE TO HKB-BRN3
           END-IF
           MOVE HKB-BRN3 TO W-BN3.
       M-25.
           MOVE TSW-BMC TO W-BMC.
           MOVE ZERO TO W-GD CHK2 CNT.
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
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE SPACE TO HKB-BMN
           END-IF
           MOVE HKB-BMN TO W-BMN.
       M-30.
           MOVE TSW-BC1 TO W-BC1.
           MOVE ZERO TO W-TD.
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
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE SPACE TO HKB-BRN1
           END-IF.
       M-35.
           ADD TSW-CSU TO WT-CSU.
           COMPUTE WT-CKIN = WT-CKIN + (TSW-CSU * TSW-FT).
           ADD TSW-JSU TO WT-JSU.
           COMPUTE WT-JKIN = WT-JKIN + (TSW-JSU * TSW-FT).
           COMPUTE WT-KSU = WT-JSU - WT-CSU.
           COMPUTE WT-KKIN = WT-JKIN - WT-CKIN.
       M-40.
      *           READ TSW-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TSW-F_PNAME1 BY REFERENCE TSW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  ZERO = TSW-CSU AND TSW-JSU
               GO TO M-40
           END-IF
           IF  TSW-BC3 NOT = W-BC3
               GO TO M-50
           END-IF
           IF  TSW-BMC NOT = W-BMC
               GO TO M-45
           END-IF
           IF  TSW-BC1 = W-BC1
               GO TO M-35
           END-IF
      *
           PERFORM MEI-RTN THRU MEI-EX.
           GO TO M-30.
       M-45.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM KEI1-RTN THRU KEI1-EX.
           GO TO M-25.
       M-50.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM KEI1-RTN THRU KEI1-EX.
           PERFORM KEI2-RTN THRU KEI2-EX.
           GO TO M-20.
       M-85.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM KEI1-RTN THRU KEI1-EX.
           PERFORM KEI2-RTN THRU KEI2-EX.
           PERFORM KEI3-RTN THRU KEI3-EX.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TSW-F_IDLST TSW-F_PNAME1.
           IF  JS-SIGN = 1
               CALL "DB_F_Close" USING
                BY REFERENCE EXLF_IDLST EXLF_PNAME1
           ELSE
               CALL "PR_Close" RETURNING RESP
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-020.
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
       MID-EX.
           EXIT.
       MEI-RTN.
           IF  ZERO = WT-CSU AND WT-CKIN AND WT-JSU AND WT-JKIN AND
                     WT-KSU AND WT-KKIN
               GO TO MEI-EX
           END-IF
           IF  JS-SIGN = 1
               GO TO MEI-010
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE ALL "　" TO P-M0 P-TM.
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               MOVE W-BN3 TO P-M0
               MOVE W-BMN TO P-M1
           END-IF
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               MOVE W-BMN TO P-M1
           END-IF
           MOVE HKB-BRN1 TO P-M2.
           MOVE WT-CSU TO P-CSU.
           MOVE WT-CKIN TO P-CKIN.
           MOVE WT-JSU TO P-JSU.
           MOVE WT-JKIN TO P-JKIN.
           MOVE WT-KSU TO P-KSU.
           MOVE WT-KKIN TO P-KKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO MEI-050.
       MEI-010.
           INITIALIZE EXL-R.
           MOVE ALL "　" TO EXL-M0 EXL-M1 EXL-M2.
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               MOVE W-BN3 TO EXL-M0
               MOVE W-BMN TO EXL-M1
           END-IF
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               MOVE W-BMN TO EXL-M1
           END-IF
           MOVE HKB-BRN1 TO EXL-M2.
           MOVE WT-CSU TO EXL-CSU.
           MOVE WT-CKIN TO EXL-CKIN.
           MOVE WT-JSU TO EXL-JSU.
           MOVE WT-JKIN TO EXL-JKIN.
           MOVE WT-KSU TO EXL-KSU.
           MOVE WT-KKIN TO EXL-KKIN.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET.
       MEI-050.
           ADD WT-CSU TO WG-CSU.
           ADD WT-CKIN TO WG-CKIN.
           ADD WT-JSU TO WG-JSU.
           ADD WT-JKIN TO WG-JKIN.
           ADD WT-KSU TO WG-KSU.
           ADD WT-KKIN TO WG-KKIN.
           IF  CNT = 5
               MOVE 9 TO CNT
           END-IF
           IF  CNT = 0
               MOVE 5 TO CNT
           END-IF.
       MEI-EX.
           EXIT.
       KEI1-RTN.
           IF  JS-SIGN = 1
               GO TO KEI1-030
           END-IF
           IF  CNT NOT = 9
               MOVE SPACE TO SP-R
               GO TO KEI1-020
           END-IF
           MOVE SPACE TO W-P.
           MOVE ALL "　" TO P-M0 P-TM.
           MOVE "　　　　　　　　（　計　）" TO P-TM.
           MOVE WG-CSU TO P-CSU.
           MOVE WG-CKIN TO P-CKIN.
           MOVE WG-JSU TO P-JSU.
           MOVE WG-JKIN TO P-JKIN.
           MOVE WG-KSU TO P-KSU.
           MOVE WG-KKIN TO P-KKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       KEI1-020.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO KEI1-050.
       KEI1-030.
           INITIALIZE EXL-R.
           MOVE ALL "　" TO EXL-M0 EXL-M1 EXL-M2.
           MOVE "　　　（　計　）" TO EXL-M2.
           MOVE WG-CSU TO EXL-CSU.
           MOVE WG-CKIN TO EXL-CKIN.
           MOVE WG-JSU TO EXL-JSU.
           MOVE WG-JKIN TO EXL-JKIN.
           MOVE WG-KSU TO EXL-KSU.
           MOVE WG-KKIN TO EXL-KKIN.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET.
       KEI1-050.
           ADD WG-CSU TO WS-CSU.
           ADD WG-CKIN TO WS-CKIN.
           ADD WG-JSU TO WS-JSU.
           ADD WG-JKIN TO WS-JKIN.
           ADD WG-KSU TO WS-KSU.
           ADD WG-KKIN TO WS-KKIN.
       KEI1-EX.
           EXIT.
       KEI2-RTN.
           IF  JS-SIGN = 1
               GO TO KEI2-030
           END-IF
           MOVE SPACE TO W-P.
           MOVE ALL "　" TO P-M0 P-TM.
           MOVE "　　　［　小　計　］　　　" TO P-TM.
           MOVE WS-CSU TO P-CSU.
           MOVE WS-CKIN TO P-CKIN.
           MOVE WS-JSU TO P-JSU.
           MOVE WS-JKIN TO P-JKIN.
           MOVE WS-KSU TO P-KSU.
           MOVE WS-KKIN TO P-KKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO KEI2-050.
       KEI2-030.
           INITIALIZE EXL-R.
           MOVE ALL "　" TO EXL-M0 EXL-M1 EXL-M2.
           MOVE "　［　小　計　］" TO EXL-M2.
           MOVE WS-CSU TO EXL-CSU.
           MOVE WS-CKIN TO EXL-CKIN.
           MOVE WS-JSU TO EXL-JSU.
           MOVE WS-JKIN TO EXL-JKIN.
           MOVE WS-KSU TO EXL-KSU.
           MOVE WS-KKIN TO EXL-KKIN.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET.
       KEI2-050.
           ADD WS-CSU TO WA-CSU.
           ADD WS-CKIN TO WA-CKIN.
           ADD WS-JSU TO WA-JSU.
           ADD WS-JKIN TO WA-JKIN.
           ADD WS-KSU TO WA-KSU.
           ADD WS-KKIN TO WA-KKIN.
       KEI2-EX.
           EXIT.
       KEI3-RTN.
           IF  JS-SIGN = 1
               GO TO KEI3-030
           END-IF
           MOVE SPACE TO W-P.
           MOVE ALL "　" TO P-M0 P-TM.
           MOVE "【　　合　計　　】　　　　" TO P-TM.
           MOVE WA-CSU TO P-CSU.
           MOVE WA-CKIN TO P-CKIN.
           MOVE WA-JSU TO P-JSU.
           MOVE WA-JKIN TO P-JKIN.
           MOVE WA-KSU TO P-KSU.
           MOVE WA-KKIN TO P-KKIN.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO KEI3-EX.
       KEI3-030.
           INITIALIZE EXL-R.
           MOVE ALL "　" TO EXL-M0 EXL-M1 EXL-M2.
           MOVE "【　合　計　】　" TO EXL-M2.
           MOVE WA-CSU TO EXL-CSU.
           MOVE WA-CKIN TO EXL-CKIN.
           MOVE WA-JSU TO EXL-JSU.
           MOVE WA-JKIN TO EXL-JKIN.
           MOVE WA-KSU TO EXL-KSU.
           MOVE WA-KKIN TO EXL-KKIN.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET.
       KEI3-EX.
           EXIT.

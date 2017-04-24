       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSA100.
      **************************************
      *****     送　金　御　案　内     *****
      **************************************
       AUTHOR. S-NAKAO.
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
       01  W-CK.
           02  W-15K        PIC  X(005) VALUE X"1A24212078".
           02  W-20K        PIC  X(005) VALUE X"1A24212474".
       01  W-P1.
           02  P-20K1       PIC  X(005).
           02  F            PIC  X(006).
           02  P-UBM        PIC  N(001).
           02  F            PIC  X(001).
           02  P-UNO        PIC  X(008).
           02  F            PIC  X(059).
       01  W-P2.
           02  P-15K2       PIC  X(005).
           02  F            PIC  X(007).
           02  P-JSU        PIC  N(024).
           02  F            PIC  X(033).
       01  W-P3.
           02  P-15K3       PIC  X(005).
           02  F            PIC  X(025).
           02  P-JSS        PIC  N(012).
           02  F            PIC  X(033).
       01  W-P4.
           02  P-15K4       PIC  X(005).
           02  F            PIC  X(009).
           02  P-NAME1      PIC  N(026).
           02  F            PIC  X(028).
       01  W-P5.
           02  P-15K5       PIC  X(005).
           02  F            PIC  X(060).
           02  P-NGM        PIC  N(002).
           02  F            PIC  X(001).
           02  P-NEN        PIC  Z(002).
           02  F            PIC  X(003).
           02  P-GET        PIC  Z(002).
           02  F            PIC  X(003).
           02  P-PEY        PIC  Z(002).
       01  W-P6.
           02  P-15K6       PIC  X(005).
           02  F            PIC  X(005).
           02  P-NAME2      PIC  N(026).
           02  F            PIC  X(032).
       01  W-P7.
           02  P-20K7       PIC  X(005).
           02  F            PIC  X(020).
           02  P-KIN        PIC  N(012).
           02  P-KEM        PIC  N(001).
           02  F            PIC  X(030).
       01  W-P8.
           02  F            PIC  X(031).
           02  P-KS         PIC  Z(002).
           02  F            PIC  X(006).
           02  P-TS         PIC  Z(002).
           02  F            PIC  X(035).
       01  W-P9.
           02  F            PIC  X(025).
           02  P-SR         PIC  Z(003).
           02  F            PIC  X(048).
       01  W-DATA.
           02  W-KEY.
             03  W-DATE.
               04  W-NEN    PIC  9(002).
               04  W-GET    PIC  9(002).
               04  W-PEY    PIC  9(002).
             03  W-SCD      PIC  9(004).
           02  W-D.
             03  W-KS       PIC  9(002).
             03  W-TS       PIC  9(002).
             03  W-KIN      PIC  9(010).
           02  W-KIND       PIC \\\\,\\\,\\\.
           02  W-NAMED      PIC  N(026).
           02  W-ANAME REDEFINES W-NAMED.
             03  W-NAME  OCCURS 26  PIC  N(001).
           02  CNT          PIC  9(002).
           02  W-TPC        PIC  9(001).
           02  W-SCDD       PIC  9(004).
           02  W-DMM        PIC  9(001).
       01  ERR-STAT         PIC  X(002).
           COPY LSTAT.
      *
           COPY LISM.
      *FD  TSA-F
       01  TSA-F_TSA100.
           02  TSA-F_PNAME1 PIC  X(009) VALUE SPACE.
           02  F            PIC  X(001).
           02  TSA-F_LNAME  PIC  X(012) VALUE "TSA-F_TSA100".
           02  F            PIC  X(001).
           02  TSA-F_KEY1   PIC  X(100) VALUE SPACE.
           02  TSA-F_SORT   PIC  X(100) VALUE SPACE.
           02  TSA-F_IDLST  PIC  X(100) VALUE SPACE.
           02  TSA-F_RES    USAGE  POINTER.
       01  TSA-R.
           02  SA-KEY.
             03  SA-DATE    PIC  9(006).
             03  SA-SCD     PIC  9(004).
           02  SA-SC        PIC  9(001).
           02  SA-KIN       PIC  9(009).
           02  F            PIC  X(044).
       77  F                PIC  X(001).
      *FD  SP-F
       77  SP-R             PIC  X(206).
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
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(019) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                  "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                  "＊＊＊　　送　金　御　案　内　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                  "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(041) VALUE
                "テストプリント　する＝5  しない＝0   ".
           02  FILLER  PIC  X(035) VALUE
                "仕入先ｺｰﾄﾞ 0000   全部=0000 終了=Ⅱ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-TPC   PIC  9(001).
           02  A-SCD   PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NAME  PIC  N(024).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(015) VALUE
                  "***  SM ﾅｼ  ***".
             03  E-KEY   PIC  9(004).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "364" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "11" "41" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "15" "13" "35" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "20" "18" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TPC" "9" "12" "47" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TPC" BY REFERENCE W-TPC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SCD" "9" "15" "24" "4" "A-TPC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SCD" BY REFERENCE W-SCDD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "35" "1" "A-SCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "48" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "16" "24" "48" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE S-NAME "48" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "98" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "98" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "15" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "9" "24" "40" "4" "E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE W-SCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-KEY" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE SPACE TO W-P1 W-P2 W-P3 W-P4 W-P5 W-P6 W-P7 W-P8 W-P9.
           MOVE W-15K TO P-15K2 P-15K3 P-15K4 P-15K5 P-15K6.
           MOVE W-20K TO P-20K1 P-20K7.
           MOVE ALL   "Ｎ" TO P-UBM P-JSU P-JSS P-NAME1 P-NGM P-NAME2
                                                          P-KIN P-KEM.
           MOVE ALL "9" TO P-UNO P-NEN P-GET P-PEY P-KS P-TS P-SR.
           CALL "PR_Open" RETURNING RESP.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-TPC "A-TPC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-TPC = ZERO
               GO TO M-15
           END-IF
           IF  W-TPC = 5
               PERFORM S-10 THRU S-15
           END-IF
           GO TO M-10.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SCD "A-SCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1
               CALL "PR_Close" RETURNING RESP
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-SCDD = ZERO
               MOVE SPACE TO S-NAME
               CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU
               GO TO M-20
           END-IF
           MOVE W-SCDD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-DMM = 9
               CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1
               CALL "PR_Close" RETURNING RESP
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO TSA-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TSA-F_PNAME1 " " BY REFERENCE TSA-F_IDLST "0".
       M-25.
      *           READ TSA-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TSA-F_PNAME1 BY REFERENCE TSA-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  SA-SC = 3
               GO TO M-25
           END-IF
           IF  W-SCDD NOT = ZERO
               IF  W-SCDD NOT = SA-SCD
                   GO TO M-25
               END-IF
           END-IF.
       M-30.
           MOVE SA-KEY TO W-KEY.
           MOVE ZERO TO W-D.
           MOVE W-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE SPACE TO W-NAMED.
           MOVE S-NAME TO W-NAMED.
           MOVE 25 TO CNT.
       M-35.
           SUBTRACT 1 FROM CNT.
           IF  CNT = ZERO
               GO TO M-40
           END-IF
           IF  W-NAME(CNT) = SPACE
               GO TO M-35
           END-IF
           ADD 3 TO CNT.
           IF  CNT > 26
               MOVE 26 TO CNT
           END-IF
           MOVE   "殿" TO W-NAME(CNT).
       M-40.
           ADD SA-KIN TO W-KIN.
           IF  SA-SC = 1
               ADD 1 TO W-KS
           END-IF
           IF  SA-SC = 2
               ADD 1 TO W-TS
           END-IF.
       M-45.
      *           READ TSA-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TSA-F_PNAME1 BY REFERENCE TSA-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SA-SC = 3
               GO TO M-45
           END-IF
           IF  W-SCDD NOT = ZERO
               IF  W-SCDD NOT = SA-SCD
                   GO TO M-45
               END-IF
           END-IF
           IF  SA-KEY = W-KEY
               GO TO M-40
           END-IF
           PERFORM S-05 THRU S-15.
           GO TO M-30.
       M-90.
           PERFORM S-05 THRU S-15.
           IF  W-SCDD NOT = ZERO
               CALL "DB_F_Close" USING
                BY REFERENCE TSA-F_IDLST TSA-F_PNAME1
               GO TO M-15
           END-IF.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TSA-F_IDLST TSA-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO W-P1 W-P2 W-P3 W-P4 W-P5 W-P6 W-P7 W-P8 W-P9.
           MOVE W-15K TO P-15K2 P-15K3 P-15K4 P-15K5 P-15K6.
           MOVE W-20K TO P-20K1 P-20K7.
           MOVE   "〒" TO P-UBM.
           MOVE S-UNO TO P-UNO.
           MOVE S-JSU TO P-JSU.
           MOVE S-JSS TO P-JSS.
           MOVE W-NAMED TO P-NAME1 P-NAME2.
           MOVE   "平成" TO P-NGM.
           MOVE W-NEN TO P-NEN.
           MOVE W-GET TO P-GET.
           MOVE W-PEY TO P-PEY.
           MOVE W-KIN TO W-KIND.
           MOVE W-KIND TO P-KIN.
           MOVE   "※" TO P-KEM.
           MOVE W-KS TO P-KS.
           MOVE W-TS TO P-TS.
           IF  S-SKR = 1
               MOVE 000 TO P-SR
           ELSE
               MOVE 380 TO P-SR
           END-IF.
       S-10.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P4 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P5 TO SP-R.
           CALL "PR_LineFeed" USING "21" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P6 TO SP-R.
           CALL "PR_LineFeed" USING "5" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P7 TO SP-R.
           CALL "PR_LineFeed" USING "6" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P8 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P9 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-15.
           EXIT.

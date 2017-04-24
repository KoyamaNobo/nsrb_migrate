       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBY410.
      *********************************************************
      *    PROGRAM         :  �N�Ԏd����ޗ��ʎd�����ו\      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
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
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  N(005) VALUE "�������@�@".
           02  H-HN           PIC  9(002).
           02  F              PIC  X(001) VALUE "/".
           02  H-HG           PIC  9(002).
           02  F              PIC  X(003) VALUE " - ".
           02  H-ON           PIC  9(002).
           02  F              PIC  X(001) VALUE "/".
           02  H-OG           PIC  9(002).
           02  F              PIC  N(020) VALUE
                "�@�N�Ԏd����ޗ��ʁ@�d�����ו\�@�@������".
           02  F              PIC  X(014) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(005) VALUE "���� ".
           02  F              PIC  N(010) VALUE
                "�d�@�@���@�@��@�@��".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(006) VALUE "����  ".
           02  F              PIC  N(014) VALUE
                "�ށ@���@���@�i���@�i�@���j�@".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  N(004) VALUE "���@�@��".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "�P�@�@��".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "���@�@�z".
       01  W-P.
           02  P-SCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-SNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-JCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-JNA          PIC  N(024).
           02  P-SU           PIC ---,---,--9.99.
           02  P-T            PIC --,---,--9.99.
           02  P-KIN          PIC ---,---,---,--9.
       01  W-DATA.
           02  W-ASCD.
             03  W-FTSCD OCCURS  10.
               04  W-FSCD     PIC  9(004).
               04  W-TSCD     PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  CNTA           PIC  9(002).
           02  CNTB           PIC  9(002).
           02  W-L            PIC  9(002).
           02  W-KAKU         PIC  9(001).
           02  W-PAGE         PIC  9(003).
           02  W-SCD          PIC  9(004).
           02  W-JCD          PIC  9(006).
           02  W-ND.
             03  W-SU         PIC S9(008)V9(02).
             03  W-T          PIC S9(007)V9(02).
             03  W-KIN        PIC S9(011).
           02  W-SKIN         PIC S9(011).
           02  W-AKIN         PIC S9(011).
           02  W-NG.
             03  F            PIC  9(002).
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
           02  CHK            PIC  9(001).
           02  CNT            PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKBNO.
           COPY LISM.
           COPY LIJM.
           COPY LSPF.
      *FD  JSSRYR
       01  JSSRYR_KBY410.
           02  JSSRYR_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  JSSRYR_LNAME   PIC  X(013) VALUE "JSSRYR_KBY410".
           02  F              PIC  X(001).
           02  JSSRYR_KEY1    PIC  X(100) VALUE SPACE.
           02  JSSRYR_SORT    PIC  X(100) VALUE SPACE.
           02  JSSRYR_IDLST   PIC  X(100) VALUE SPACE.
           02  JSSRYR_RES     USAGE  POINTER.
       01  JSSRY-R.
           02  JRY-DC.
             03  JRY-DC1      PIC  9(001).
             03  JRY-DC2      PIC  9(001).
           02  JRY-NGP.
             03  JRY-NG.
               04  JRY-NEN    PIC  9(004).
               04  JRY-GET    PIC  9(002).
             03  JRY-PEY      PIC  9(002).
           02  JRY-SCD        PIC  9(004).
           02  JRY-JCD.
             03  JRY-JCD12.
               04  JRY-JCD1   PIC  9(001).
               04  JRY-JCD2   PIC  9(002).
             03  JRY-JCD3     PIC  9(003).
           02  JRY-SU         PIC S9(007)V9(02).
           02  JRY-T          PIC S9(006)V9(02).
           02  JRY-KIN        PIC S9(008).
           02  JRY-SHZ        PIC S9(007).
           02  JRY-SNGP.
             03  JRY-SNG.
               04  JRY-SNEN   PIC  9(002).
               04  JRY-SGET   PIC  9(002).
             03  JRY-SPEY     PIC  9(002).
           02  JRY-SJCD       PIC  9(006).
           02  JRY-NHN        PIC  9(006).
           02  JRY-FC         PIC  9(001).
           02  JRY-YC         PIC  9(001).
           02  JRY-TC         PIC  9(001).
           02  JRY-HC         PIC  9(001).
           02  JRY-SC         PIC  9(001).
           02  JRY-BSC        PIC  9(001).
           02  JRY-BKC        PIC  9(002).
           02  F              PIC  X(016).
           02  JRY-KEY        PIC  X(007).
           02  JRY-CR         PIC  9(001).
           02  F              PIC  X(026).
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
           02  FILLER  PIC  N(024) VALUE
                "�������@�@�N�Ԏd����ޗ��ʁ@�d�����ו\�@�@������".
           02  FILLER  PIC  X(040) VALUE
                "�d���溰��       �`             �m�F=F10".
           02  FILLER  PIC  X(024) VALUE
                "                 �`     ".
           02  FILLER  PIC  X(024) VALUE
                "                 �`     ".
           02  FILLER  PIC  X(024) VALUE
                "                 �`     ".
           02  FILLER  PIC  X(024) VALUE
                "                 �`     ".
           02  FILLER  PIC  X(024) VALUE
                "                 �`     ".
           02  FILLER  PIC  X(024) VALUE
                "                 �`     ".
           02  FILLER  PIC  X(024) VALUE
                "                 �`     ".
           02  FILLER  PIC  X(024) VALUE
                "                 �`     ".
           02  FILLER  PIC  X(024) VALUE
                "                 �`     ".
           02  FILLER  PIC  X(022) VALUE
                "�m�F  OK=1 NO=9   ����".
       01  C-ACP.
           02  FILLER.
             03  A-FSCD  PIC  9(004).
             03  A-TSCD  PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SCD.
             03  FILLER  PIC  X(004) VALUE "    ".
             03  FILLER  PIC  X(004) VALUE "    ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ż  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  KBNOM ż  ***".
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
            "C-MID" " " "0" "0" "326" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "5" "22" "40" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "6" "22" "24" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "7" "22" "24" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "X" "8" "22" "24" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "X" "9" "22" "24" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "X" "10" "22" "24" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "11" "22" "24" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "12" "22" "24" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "13" "22" "24" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-MID" "X" "14" "22" "24" "10C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "12C-MID" "X" "22" "30" "22" "11C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "W-L" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FSCD" "9" "W-L" "34" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FSCD" BY REFERENCE W-FSCD(1) "4" "1" BY REFERENCE CNTA 8
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TSCD" "9" "W-L" "42" "4" "A-FSCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TSCD" BY REFERENCE W-TSCD(1) "4" "1" BY REFERENCE CNTA 8
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "47" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SCD" " " "W-L" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SCD" "X" "W-L" "34" "4" " " "D-SCD" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SCD" "X" "W-L" "42" "4" "01D-SCD" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "185" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "185" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "75" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "75" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           PERFORM S-55 THRU S-85.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           COPY LIBCPR.
           MOVE DATE-05R TO H-DATE.
           CALL "DB_F_Open" USING
            "INPUT" KBNO-M_PNAME1 "SHARED" BY REFERENCE KBNO-M_IDLST "1"
            "BNO-KEY" BY REFERENCE BNO-KEY.
           MOVE SPACE TO BNO-KEY.
           MOVE "01" TO BNO-KEYD.
      *           READ KBNO-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KBNO-M_PNAME1 BY REFERENCE KBNO-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO BNO-SNG BNO-ENG
           END-IF
           MOVE BNO-SNG TO W-NG.
           MOVE W-NEN TO H-HN.
           MOVE W-GET TO H-HG.
           MOVE BNO-ENG TO W-NG.
           MOVE W-NEN TO H-ON.
           MOVE W-GET TO H-OG.
           CALL "DB_F_Close" USING
            BY REFERENCE KBNO-M_IDLST KBNO-M_PNAME1.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JSSRYR_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSSRYR_PNAME1 " " BY REFERENCE JSSRYR_IDLST "0".
       M-10.
      *           READ JSSRYR AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSRYR_PNAME1 BY REFERENCE JSSRY-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JSSRYR_IDLST JSSRYR_PNAME1
               GO TO M-95
           END-IF
           MOVE ZERO TO CNTA.
       M-12.
           ADD 1 TO CNTA.
           IF  CNT = 11
               GO TO M-10
           END-IF
           IF  W-TSCD(CNTA) = ZERO
               GO TO M-10
           END-IF
           IF  JRY-SCD < W-FSCD(CNTA) OR > W-TSCD(CNTA)
               GO TO M-12
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO W-AKIN W-PAGE.
           PERFORM S-10 THRU S-15.
       M-15.
           MOVE ZERO TO W-SKIN CHK CNT.
           MOVE JRY-SCD TO W-SCD.
           MOVE W-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "�@�������@�}�X�^�[�@�Ȃ��@�������@�@" TO S-NAME
           END-IF.
       M-20.
           MOVE ZERO TO W-ND.
           MOVE JRY-JCD TO W-JCD.
           MOVE W-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "�@�������@�}�X�^�[�@�Ȃ��@�������@�@" TO J-NAME
           END-IF.
       M-25.
           ADD JRY-SU TO W-SU.
           ADD JRY-KIN TO W-KIN.
       M-30.
      *           READ JSSRYR AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSRYR_PNAME1 BY REFERENCE JSSRY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           MOVE ZERO TO CNTA.
       M-32.
           ADD 1 TO CNTA.
           IF  CNTA = 37
               GO TO M-30
           END-IF
           IF  W-TSCD(CNTA) = ZERO
               GO TO M-30
           END-IF
           IF  JRY-SCD < W-FSCD(CNTA) OR > W-TSCD(CNTA)
               GO TO M-32
           END-IF
      *
           IF  W-SCD NOT = JRY-SCD
               GO TO M-40
           END-IF
           IF  W-JCD NOT = JRY-JCD
               GO TO M-35
           END-IF
           GO TO M-25.
       M-35.
           PERFORM S-20 THRU S-25.
           GO TO M-20.
       M-40.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-40.
           GO TO M-15.
       M-90.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-40.
           PERFORM S-45 THRU S-50.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSRYR_IDLST JSSRYR_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
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
           IF  ZERO = W-SU AND W-KIN
               GO TO S-25
           END-IF
           MOVE ZERO TO W-T.
           IF  W-SU NOT = ZERO
               IF  W-KIN NOT = ZERO
                   COMPUTE W-T ROUNDED = W-KIN / W-SU
               END-IF
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-SNA P-JNA.
           IF  CHK = 0
               MOVE 1 TO CHK
               MOVE W-SCD TO P-SCD
               MOVE S-NAME TO P-SNA
           END-IF
           MOVE W-JCD TO P-JCD.
           MOVE J-NAME TO P-JNA.
           IF  W-SU NOT = ZERO
               MOVE W-SU TO P-SU
           END-IF
           IF  W-T NOT = ZERO
               MOVE W-T TO P-T
           END-IF
           MOVE W-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE W-SCD TO P-SCD
               MOVE S-NAME TO P-SNA
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD W-KIN TO W-SKIN.
           IF  CNT = 1
               MOVE 2 TO CNT
           END-IF
           IF  CNT = 0
               MOVE 1 TO CNT
           END-IF.
       S-25.
           EXIT.
       S-30.
           IF  CNT NOT = 2
               GO TO S-35
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-SNA P-JNA.
           MOVE "�@�@�@�@�@�@�@�@�@�@�@�@�i�@���@�v�@�j�@" TO P-JNA.
           MOVE W-SKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE W-SCD TO P-SCD
               MOVE S-NAME TO P-SNA
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-35.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD W-SKIN TO W-AKIN.
       S-40.
           EXIT.
       S-45.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-SNA P-JNA.
           MOVE "�@�@�m�@���@���@�v�@�n�@" TO P-JNA.
           MOVE W-AKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-50.
           EXIT.
       S-55.
           MOVE ZERO TO W-ASCD CNTA CNTB W-KAKU.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       S-60.
           ADD 1 TO CNTA W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNTA = 11
               GO TO S-80
           END-IF
           IF  W-KAKU = 1
               MOVE ZERO TO W-FSCD(CNTA) W-TSCD(CNTA)
               CALL "SD_Output" USING "D-SCD" D-SCD "p" RETURNING RESU
               GO TO S-60
           END-IF.
       S-65.
           CALL "SD_Accept" USING BY REFERENCE A-FSCD "A-FSCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               IF  CNT = 1
                   GO TO S-85
               END-IF
           END-IF
           IF  ESTAT = ADV
               IF  CNTA NOT = 1
                   MOVE 1 TO W-KAKU
                   MOVE ZERO TO W-FSCD(CNTA) W-TSCD(CNTA)
                   CALL "SD_Output" USING
                    "D-SCD" D-SCD "p" RETURNING RESU
                   GO TO S-60
               END-IF
           END-IF
           IF  ESTAT = BTB
               GO TO S-75
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-65
           END-IF
           IF  CNTB NOT = ZERO
               IF  W-TSCD(CNTB) > W-FSCD(CNTA)
                   GO TO S-65
               END-IF
           END-IF.
       S-70.
           CALL "SD_Accept" USING BY REFERENCE A-TSCD "A-TSCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO S-65
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-70
           END-IF
           IF  W-FSCD(CNTA) > W-TSCD(CNTA)
               GO TO S-70
           END-IF
           MOVE CNTA TO CNTB.
           GO TO S-60.
       S-75.
           SUBTRACT 1 FROM CNTA W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNTA = ZERO
               GO TO S-55
           END-IF
           IF  W-TSCD(CNTA) = ZERO
               GO TO S-75
           END-IF
           GO TO S-70.
       S-80.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 15 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               GO TO S-75
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-80
           END-IF
           IF  W-DMM = 9
               GO TO S-55
           END-IF
           IF  W-DMM NOT = 1
               GO TO S-80
           END-IF.
       S-85.
           EXIT.

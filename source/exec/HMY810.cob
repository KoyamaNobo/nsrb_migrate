       IDENTIFICATION   DIVISION.
       PROGRAM-ID. HMY810.
      *********************************************************
      *    PROGRAM         :  年間履物返品明細表　　　　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/13                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  N(007) VALUE "＊　＊　＊　　".
           02  H-SNG          PIC 99/99.
           02  F              PIC  X(003) VALUE " - ".
           02  H-ENG          PIC 99/99.
           02  F              PIC  N(018) VALUE
                "　年間履物　返品明細表　　＊　＊　＊".
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  N(003) VALUE "担当　".
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(007) VALUE "得　意　先　名".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  N(009) VALUE "コード　品　　　名".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  N(014) VALUE
                "数　量　単　価　　　金　　額".
       01  W-P.
           02  P-15K          PIC  X(005).
           02  F              PIC  X(001).
           02  P-TC           PIC  9(002).
           02  F              PIC  X(003).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(002).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(002).
           02  P-HNA          PIC  N(024).
           02  P-SU           PIC -----,--9.
           02  P-T            PIC ----,---.
           02  P-KIN          PIC --,---,---,--9.
           02  P-20K          PIC  X(005).
       01  WC-D.
           02  W-HCD          PIC  9(006).
           02  WC-SU          PIC S9(007).
           02  WC-T           PIC S9(005).
           02  WC-KIN         PIC S9(010).
       01  W-D.
           02  W-TCD          PIC  9(004).
           02  W-SU           PIC S9(007).
           02  W-KIN          PIC S9(010).
       01  WS-D.
           02  W-TC           PIC  9(002).
           02  WS-SU          PIC S9(007).
           02  WS-KIN         PIC S9(010).
       01  WA-D.
           02  WA-SU          PIC S9(007).
           02  WA-KIN         PIC S9(010).
       01  W-DATA.
           02  W-TNA          PIC  N(024).
           02  W-HNA          PIC  N(024).
           02  W-TCB          PIC  9(002).
           02  W-TCDB         PIC  9(004).
           02  W-HCDB         PIC  9(006).
           02  W-SBC3         PIC  9(002) VALUE 00.
           02  W-EBC3         PIC  9(002) VALUE 99.
           02  W-DMM          PIC  9(001).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-SNG          PIC  9(006).
           02  W-SNGD  REDEFINES W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SND   REDEFINES W-SNEN.
               04  W-SN1      PIC  9(002).
               04  W-SN2      PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-ENG          PIC  9(006).
           02  W-ENGD  REDEFINES W-ENG.
             03  W-ENEN       PIC  9(004).
             03  W-END   REDEFINES W-ENEN.
               04  W-EN1      PIC  9(002).
               04  W-EN2      PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-ENGL  REDEFINES W-ENG.
             03  F            PIC  9(002).
             03  W-ENGS       PIC  9(004).
           02  CHK            PIC  9(003).
           02  CNT            PIC  9(003).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LIHIM.
           COPY LITM.
           COPY LSPF.
       01  HPR-F_HMY810.
           02  HPR-F_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  HPR-F_LNAME    PIC  X(012)  VALUE "HPR-F_HMY810".
           02  F              PIC  X(001).
           02  HPR-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  HPR-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  HPR-F_SORT     PIC  X(100)  VALUE SPACE.
           02  HPR-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  HPR-F_RES      USAGE  POINTER.
       01  HPR-R.
           02  HP-TCD         PIC  9(004).
           02  HP-HCD         PIC  9(006).
           02  HP-SU          PIC S9(007).
           02  HP-KIN         PIC S9(010).
           02  HP-TC          PIC  9(002).
           02  HP-BC1         PIC  9(002).
           02  HP-BC2         PIC  9(002).
           02  HP-BC3         PIC  9(002).
           02  F              PIC  X(001).
           02  HP-NG          PIC  9(006).
           02  F              PIC  X(022).
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
           02  C-CL  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　年間履物　返品明細表　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(026) VALUE
                "'  年   月  ～  '  年   月".
           02  FILLER  PIC  X(046) VALUE
                "分類3   00 ～ 99     一般=10,ワーク=20,教育=30".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SBC3    PIC  9(002).
             03  A-EBC3    PIC  9(002).
           02  A-DMM     PIC  9(001).
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC 99.
             03  FILLER  PIC Z9.
             03  FILLER  PIC 99.
             03  FILLER  PIC Z9.
       01  C-ERR.
           02  FILLER.
             03  E-ME98     PIC  X(005) VALUE X"1B4A05".
             03  E-ME99     PIC  X(005) VALUE X"1B4205".
             03  E-STAT     PIC  X(002).
             03  E-CL       PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "374" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING
            "08C-MID" "X" "14" "17" "26" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "16" "10" "46" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "20" "19" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "16" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SBC3" "9" "16" "18" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SBC3" BY REFERENCE W-SBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EBC3" "9" "16" "24" "2" "A-SBC3" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EBC3" BY REFERENCE W-EBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "36" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "14" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "99" "14" "18" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-SN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "Z9" "14" "23" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NG" "99" "14" "34" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE W-EN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NG" "Z9" "14" "39" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NG" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
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
      *
       M-05.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO W-SNG W-ENG.
           MOVE D-SPNG TO W-SNGS.
           MOVE D-EPNG TO W-ENGS.
           IF  W-SN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF.
           IF  W-SN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
           IF  W-EN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF.
           IF  W-EN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" 
                                         RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SBC3 "A-SBC3" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
           IF  W-SBC3 NOT = 00 AND 10 AND 20 AND 30
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-EBC3 "A-EBC3" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
           IF  W-EBC3 NOT = 10 AND 20 AND 30 AND 99
               GO TO M-15
           END-IF.
           IF  W-SBC3 > W-EBC3
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
           IF  W-DMM = 9
               GO TO M-10
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO HPR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HPR-F_PNAME1 " " BY REFERENCE HPR-F_IDLST "0".
       M-25.
      *           READ HPR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HPR-F_PNAME1 BY REFERENCE HPR-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HPR-F_IDLST HPR-F_PNAME1
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  HP-NG < W-SNG OR > W-ENG
               GO TO M-25
           END-IF.
           IF  HP-BC3 < W-SBC3 OR > W-EBC3
               GO TO M-25
           END-IF.
           IF  ZERO = HP-SU AND HP-KIN
               GO TO M-25
           END-IF.
           MOVE W-SNGS TO H-SNG.
           MOVE W-ENGS TO H-ENG.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO WA-D.
       M-30.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA P-HNA.
           MOVE W-15K TO P-15K.
           MOVE ZERO TO WS-D.
           MOVE HP-TC TO W-TC P-TC.
       M-35.
           MOVE ZERO TO W-D.
           MOVE HP-TCD TO W-TCD P-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　得意先マスター　無し　＊＊" TO T-NAME
           END-IF.
           MOVE T-NAME TO P-TNA.
       M-40.
           MOVE ZERO TO WC-D.
           MOVE HP-HCD TO W-HCD.
       M-45.
           ADD HP-SU TO WC-SU.
           ADD HP-KIN TO WC-KIN.
       M-50.
      *           READ HPR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HPR-F_PNAME1 BY REFERENCE HPR-R " " RETURNING RET.
           IF  RET = 1
                GO TO M-90
           END-IF.
           IF  HP-NG < W-SNG OR > W-ENG
               GO TO M-50
           END-IF.
           IF  HP-BC3 < W-SBC3 OR > W-EBC3
               GO TO M-50
           END-IF.
           IF  ZERO = HP-SU AND HP-KIN
               GO TO M-50
           END-IF.
           IF  HP-TC NOT = W-TC
               GO TO M-65
           END-IF.
           IF  HP-TCD NOT = W-TCD
               GO TO M-60
           END-IF.
           IF  HP-HCD NOT = W-HCD
               GO TO M-55
           END-IF.
           GO TO M-45.
       M-55.
           PERFORM S-20 THRU S-25.
           GO TO M-40.
       M-60.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-40.
           GO TO M-35.
       M-65.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-40.
           PERFORM S-45 THRU S-50.
           GO TO M-30.
       M-90.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-40.
           PERFORM S-45 THRU S-50.
           PERFORM S-55 THRU S-60.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HPR-F_IDLST HPR-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
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
           IF ZERO = WC-SU AND WC-KIN
               GO TO S-25
           END-IF.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　ＨＩＭ　無し　＊＊" TO HI-NAME
           END-IF.
           MOVE W-HCD TO P-HCD.
           MOVE HI-NAME TO P-HNA.
           MOVE WC-SU TO P-SU.
           IF  ZERO = WC-KIN OR WC-SU
               MOVE ZERO TO WC-T
           ELSE
               COMPUTE WC-T ROUNDED = WC-KIN / WC-SU
           END-IF.
           MOVE WC-T TO P-T.
           MOVE WC-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TC TO P-TC
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE W-20K TO P-20K.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
           MOVE SPACE TO P-TNA P-HNA.
           MOVE W-15K TO P-15K.
           ADD WC-SU TO W-SU.
           ADD WC-KIN TO W-KIN.
           ADD 1 TO CHK.
       S-25.
           EXIT.
       S-30.
           MOVE SPACE TO SP-R W-P.
           MOVE SPACE TO P-TNA P-HNA.
           MOVE W-15K TO P-15K.
           IF  CHK = 01
               GO TO S-35
           END-IF.
           MOVE "　　　　　　　　　　（　ＴＯＴＡＬ　）" TO P-HNA.
           MOVE W-SU TO P-SU.
           MOVE W-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TC TO P-TC
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE W-20K TO P-20K.
           MOVE W-P TO SP-R.
           MOVE ZERO TO CHK.
       S-35.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
           MOVE SPACE TO P-TNA P-HNA.
           MOVE W-15K TO P-15K.
           ADD W-SU TO WS-SU.
           ADD W-KIN TO WS-KIN.
           MOVE ZERO TO CHK.
       S-40.
           EXIT.
       S-45.
           MOVE SPACE TO SP-R W-P.
           MOVE SPACE TO P-TNA P-HNA.
           MOVE W-15K TO P-15K.
           MOVE "　　　　　　［　　ＳＵＢ　ＴＯＴＡＬ　　］" TO P-TNA.
           MOVE WS-SU TO P-SU.
           MOVE WS-KIN TO P-KIN.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  LINAGECOUNTER > 62
               MOVE W-TC TO P-TC
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE W-20K TO P-20K.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
           MOVE SPACE TO P-TNA P-HNA.
           MOVE W-15K TO P-15K.
           ADD WS-SU TO WA-SU.
           ADD WS-KIN TO WA-KIN.
       S-50.
           EXIT.
       S-55.
           MOVE SPACE TO SP-R W-P.
           MOVE SPACE TO P-TNA P-HNA.
           MOVE W-15K TO P-15K.
           MOVE "　［　　ＡＬＬ　ＴＯＴＡＬ　　］　　" TO P-TNA.
           MOVE WA-SU TO P-SU.
           MOVE WA-KIN TO P-KIN.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE W-20K TO P-20K.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
           MOVE SPACE TO P-TNA P-HNA.
           MOVE W-15K TO P-15K.
       S-60.
           EXIT.

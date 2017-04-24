       IDENTIFICATION   DIVISION.
       PROGRAM-ID. HMY820.
      *********************************************************
      *    PROGRAM         :  年間履物品種別返品明細表　　　　*
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
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(006) VALUE "　＊＊＊　　".
           02  H-SNG          PIC 99/99.
           02  F              PIC  X(003) VALUE " - ".
           02  H-ENG          PIC 99/99.
           02  F              PIC  N(018) VALUE
                "　年間品種別　返品明細表　　＊＊＊　".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(006) VALUE "品　　　名　".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単価".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
       01  W-P.
           02  P-HCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-HNA          PIC  N(024).
           02  P-SU           PIC ------,--9.
           02  P-T            PIC ----,---.
           02  P-KIN          PIC ---,---,---,--9.
       01  W-DATA.
           02  W-SBC3         PIC  9(002).
           02  W-EBC3         PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-PAGE         PIC  9(002).
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
           02  W-BC3          PIC  9(002).
           02  W-HCD          PIC  9(004).
           02  CNT            PIC  9(002).
           02  W-C            PIC  9(002).
           02  W-NAME         PIC  N(024).
           02  W-ANA   REDEFINES W-NAME.
             03  W-NA         PIC  N(001) OCCURS  24.
           02  W-NAMED        PIC  N(024).
           02  W-ANAD  REDEFINES W-NAMED.
             03  W-NAD        PIC  N(001) OCCURS  24.
           02  W-T            PIC S9(006).
           02  W-D.
             03  W-SU         PIC S9(007).
             03  W-KIN        PIC S9(010).
           02  WS-D.
             03  WS-SU        PIC S9(007).
             03  WS-KIN       PIC S9(010).
           02  WA-D.
             03  WA-SU        PIC S9(007).
             03  WA-KIN       PIC S9(010).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LIHIM.
           COPY LSPF.
       01  HPR-F_HMY820.
           02  HPR-F_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  HPR-F_LNAME    PIC  X(012)  VALUE "HPR-F_HMY820".
           02  F              PIC  X(001).
           02  HPR-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  HPR-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  HPR-F_SORT     PIC  X(100)  VALUE SPACE.
           02  HPR-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  HPR-F_RES      USAGE  POINTER.
       01  HPR-R.
           02  HP-TCD         PIC  9(004).
           02  HP-HCD         PIC  9(004).
           02  F              PIC  9(002).
           02  HP-SU          PIC S9(007).
           02  HP-KIN         PIC S9(010).
           02  HP-TC          PIC  9(002).
           02  HP-BC1         PIC  9(002).
           02  HP-BC2.
             03  HP-BC21      PIC  9(001).
             03  HP-BC22      PIC  9(001).
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　年間履物品種別　返品明細表　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
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
             03  E-ME98     PIC X(005) VALUE X"1B4A05".
             03  E-ME99     PIC X(005) VALUE X"1B4205".
             03  E-STAT     PIC X(002).
             03  E-CL       PIC X(050) VALUE
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
            "C-MID" " " "0" "0" "416" " " " " RETURNING RESU.
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
            "08C-MID" "X" "14" "20" "26" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "16" "10" "46" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "20" "22" "22" "09C-MID" " " RETURNING RESU.
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
            "A-DMM" "9" "20" "39" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "14" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "99" "14" "21" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-SN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "Z9" "14" "26" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NG" "99" "14" "37" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE W-EN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NG" "Z9" "14" "42" "2" "03D-NG" " " RETURNING RESU.
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
           INITIALIZE W-DATA.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
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
           MOVE 00 TO W-SBC3.
           MOVE 99 TO W-EBC3.
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
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO WA-D.
       M-30.
           MOVE HP-BC3 TO W-BC3.
           MOVE ZERO TO WS-D.
       M-35.
           MOVE HP-HCD TO W-HCD.
           MOVE ZERO TO W-D W-C.
           MOVE ZERO TO HI-KEY.
           MOVE W-HCD TO HI-HCD1.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               GO TO M-40
           END-IF.
           IF  W-HCD NOT = HI-HCD1
               MOVE SPACE TO HI-NAME
           END-IF.
       M-40.
           MOVE SPACE TO W-NAME W-NAMED.
           MOVE HI-NAME TO W-NAMED.
           MOVE ZERO TO CNT.
       M-45.
           ADD 1 TO CNT.
           IF  CNT > 24
               GO TO M-50
           END-IF.
           MOVE W-NAD(CNT) TO W-NA(CNT).
           IF  W-NAD(CNT) NOT = SPACE
               GO TO M-45
           END-IF.
           ADD 1 TO CNT.
           IF  CNT > 24
               GO TO M-50
           END-IF.
           MOVE W-NAD(CNT) TO W-NA(CNT).
           IF  W-NAD(CNT) NOT = SPACE
               GO TO M-45
           END-IF.
       M-50.
           ADD HP-SU TO W-SU.
           ADD HP-KIN TO W-KIN.
       M-55.
      *           READ HPR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HPR-F_PNAME1 BY REFERENCE HPR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
           IF  HP-NG < W-SNG OR > W-ENG
               GO TO M-55
           END-IF.
           IF  HP-BC3 < W-SBC3 OR > W-EBC3
               GO TO M-55
           END-IF.
           IF  ZERO = HP-SU AND HP-KIN
               GO TO M-55
           END-IF.
           IF  W-BC3 NOT = HP-BC3
               GO TO M-60
           END-IF.
           IF  W-HCD = HP-HCD
               GO TO M-50
           END-IF.
           PERFORM S-20 THRU S-25
           GO TO M-35.
       M-60.
           PERFORM S-20 THRU S-25
           PERFORM S-40 THRU S-45
           GO TO M-30.
       M-90.
           PERFORM S-20 THRU S-25
           PERFORM S-40 THRU S-45
           PERFORM S-50 THRU S-55.
       M-95.
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
           IF  ZERO = W-SU AND W-KIN
               GO TO S-25
           END-IF.
           IF  ZERO = W-KIN OR W-SU
               MOVE ZERO TO W-T
           ELSE
               COMPUTE W-T ROUNDED = W-KIN / W-SU
           END-IF.
           MOVE SPACE TO W-P.
           MOVE W-HCD TO P-HCD.
           MOVE W-NAME TO P-HNA.
           MOVE W-SU TO P-SU.
           MOVE W-T TO P-T.
           MOVE W-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD W-SU TO WS-SU.
           ADD W-KIN TO WS-KIN.
       S-25.
           EXIT.
       S-40.
           MOVE SPACE TO W-P.
           MOVE "　　　　　　［　小　計　］　　　　　　　" TO P-HNA.
           MOVE WS-SU TO P-SU.
           MOVE WS-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-SU TO WA-SU.
           ADD WS-KIN TO WA-KIN.
       S-45.
           EXIT.
       S-50.
           MOVE SPACE TO W-P.
           MOVE "　　　【　総　合　計　】　　　　　　　　" TO P-HNA.
           MOVE WA-SU TO P-SU.
           MOVE WA-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-55.
           EXIT.

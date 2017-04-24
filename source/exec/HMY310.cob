       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY310.
      *********************************************************
      *    PROGRAM         :  得意先品種別年間売上集計表      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/20                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
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
           02  F              PIC  X(021) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-SNEN         PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-SGET         PIC Z9.
           02  F              PIC  N(004) VALUE "月　～　".
           02  H-ENEN         PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-EGET         PIC Z9.
           02  F              PIC  N(023) VALUE
                "月　得意先　品種別　売上粗利　集計表　　＊＊＊".
           02  F              PIC  X(012) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(112) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  X(007) VALUE "ｺ ｰ ﾄﾞ ".
           02  F              PIC  N(005) VALUE "品　　　名".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上数量".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "足単価".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上原価".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "足単価".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(005) VALUE "売上粗利益".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "利益率".
           02  F              PIC  X(001) VALUE "%".
       01  W-P1.
           02  P-20K          PIC  X(005).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNAME        PIC  N(026).
           02  F              PIC  X(075).
       01  W-P2.
           02  P-15K          PIC  X(005).
           02  F              PIC  X(016).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNAME        PIC  N(024).
           02  P-SU           PIC ---,---,--9.
           02  P-UK           PIC --,---,---,--9.
           02  P-UT           PIC ---,--9.
           02  P-GK           PIC --,---,---,--9.
           02  P-GT           PIC ---,--9.
           02  P-AR           PIC ----,---,--9.
           02  P-RR           PIC -----9.9.
       01  W-DATA.
           02  W-TCD          PIC  9(004).
           02  W-HCD          PIC  9(006).
           02  W-D.
             03  W-SU         PIC S9(007).
             03  W-UK         PIC S9(010).
             03  W-GK         PIC S9(010).
             03  W-AR         PIC S9(009).
           02  W-UT           PIC S9(005).
           02  W-GT           PIC S9(005).
           02  W-RR           PIC S9(003)V9(01).
           02  W-KIN          PIC S9(010).
           02  CNT            PIC  9(003).
           02  W-PAGE         PIC  9(003) VALUE ZERO.
           02  W-STCD         PIC  9(004).
           02  W-ETCD         PIC  9(004) VALUE 9999.
           02  W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SND   REDEFINES W-SNEN.
               04  W-SN1      PIC  9(002).
               04  W-SN2      PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-ENG.
             03  W-ENEN       PIC  9(004).
             03  W-END   REDEFINES W-ENEN.
               04  W-EN1      PIC  9(002).
               04  W-EN2      PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-ENGL  REDEFINES W-ENG.
             03  F            PIC  9(002).
             03  W-ENGS       PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  CHK            PIC  9(001).
       01  WN-D.
           02  WN-SU          PIC S9(007).
           02  WN-UK          PIC S9(010).
           02  WN-GK          PIC S9(010).
           02  WN-AR          PIC S9(009).
       01  WS-D.
           02  WS-SU          PIC S9(007).
           02  WS-UK          PIC S9(010).
           02  WS-GK          PIC S9(010).
           02  WS-AR          PIC S9(009).
       01  WA-D.
           02  WA-SU          PIC S9(007).
           02  WA-UK          PIC S9(010).
           02  WA-GK          PIC S9(010).
           02  WA-AR          PIC S9(009).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LSPF.
       01  SSR-YF_HMY310.
           02  SSR-YF_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F               PIC  X(001).
           02  SSR-YF_LNAME    PIC  X(013)  VALUE "SSR-YF_HMY310".
           02  F               PIC  X(001).
           02  SSR-YF_KEY1     PIC  X(100)  VALUE SPACE.
           02  SSR-YF_KEY2     PIC  X(100)  VALUE SPACE.
           02  SSR-YF_SORT     PIC  X(100)  VALUE SPACE.
           02  SSR-YF_IDLST    PIC  X(100)  VALUE SPACE.
           02  SSR-YF_RES      USAGE  POINTER.
       01  SSR-YR.
           02  Y-TCD          PIC  9(004).
           02  Y-HCD          PIC  9(006).
           02  Y-SU           PIC S9(007).
           02  Y-UK           PIC S9(010).
           02  Y-GK           PIC S9(010).
           02  F              PIC  X(015).
           02  Y-NG           PIC  9(006).
           02  F              PIC  X(006).
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
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　得意先品種別　年間売上集計表　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(024) VALUE
                "'  年  月 より '  年  月".
           02  FILLER  PIC  X(024) VALUE
                "得意先ｺｰﾄﾞ  0000 ～ 9999".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-STCD    PIC  9(004).
             03  A-ETCD    PIC  9(004).
           02  A-DMM       PIC  9(001).
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
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
            "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "406" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "48" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "48" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "48" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "48" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "48" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "14" "22" "24" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "16" "22" "24" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "22" "23" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "16" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-STCD" "9" "16" "34" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ETCD" "9" "16" "42" "4" "A-STCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "22" "40" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "14" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "9" "14" "23" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-SN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "9" "14" "27" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NG" "9" "14" "38" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE W-EN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NG" "9" "14" "42" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NG" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
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
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF.
           IF  W-DMM = 9
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-30
           END-IF.
           MOVE 0 TO CHK.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SSR-YF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SSR-YF_PNAME1 " " BY REFERENCE SSR-YF_IDLST "0".
       M-35.
      *           READ SSR-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSR-YF_PNAME1 BY REFERENCE SSR-YR " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SSR-YF_IDLST SSR-YF_PNAME1
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  Y-NG < W-SNG OR > W-ENG
               GO TO M-35
           END-IF.
           IF  Y-TCD < W-STCD OR > W-ETCD
               GO TO M-35
           END-IF.
           IF  ZERO = Y-SU AND Y-UK AND Y-GK
               GO TO M-35
           END-IF.
           MOVE ZERO TO WA-D.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-40.
           MOVE ZERO TO WS-D CNT.
           MOVE Y-TCD TO W-TCD.
       M-45.
           MOVE ZERO TO WN-D.
           MOVE Y-HCD TO W-HCD.
       M-50.
           ADD Y-SU TO WN-SU.
           ADD Y-UK TO WN-UK.
           ADD Y-GK TO WN-GK.
       M-55.
      *           READ SSR-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSR-YF_PNAME1 BY REFERENCE SSR-YR " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
           IF  Y-NG < W-SNG OR > W-ENG
               GO TO M-55
           END-IF.
           IF  Y-TCD < W-STCD OR > W-ETCD
               GO TO M-55
           END-IF.
           IF  ZERO = Y-SU AND Y-UK AND Y-GK
               GO TO M-55
           END-IF.
           IF  Y-TCD NOT = W-TCD
               GO TO M-60
           END-IF.
           IF  Y-HCD = W-HCD
               GO TO M-50
           END-IF.
           PERFORM S-30 THRU S-40.
           GO TO M-45.
       M-60.
           PERFORM S-30 THRU S-40.
           PERFORM S-45 THRU S-55.
           GO TO M-40.
       M-90.
           PERFORM S-30 THRU S-40.
           PERFORM S-45 THRU S-55.
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15K.
           MOVE "　　　　　　　　【　総　合　計　】　" TO P-HNAME.
           MOVE WA-SU TO P-SU.
           MOVE WA-UK TO P-UK.
           MOVE WA-GK TO P-GK.
           MOVE WA-AR TO P-AR.
           MOVE WA-D TO W-D.
           PERFORM S-60 THRU S-65.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           IF  CHK = 5
               CALL "PR_Close" RETURNING RESP
           END-IF.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SSR-YF_IDLST SSR-YF_PNAME1.
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
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           IF  CHK = 0
               MOVE 5 TO CHK
               CALL "PR_Open" RETURNING RESP
               MOVE W-SN2 TO H-SNEN
               MOVE W-SGET TO H-SGET
               MOVE W-EN2 TO H-ENEN
               MOVE W-EGET TO H-EGET
               MOVE DATE-02R TO H-DATE
               PERFORM S-10 THRU S-15
           END-IF.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　得意先マスター　無し　＊＊" TO T-NAME
           END-IF.
           MOVE SPACE TO W-P1.
           MOVE W-20K TO P-20K.
           MOVE W-TCD TO P-TCD.
           MOVE T-NAME TO P-TNAME.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           EXIT.
       S-30.
           IF  ZERO = WN-SU AND WN-UK AND WN-GK
               GO TO S-40
           END-IF.
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15K.
           MOVE W-HCD TO P-HCD.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　ＨＩ－Ｍ　無し　＊＊" TO HI-NAME
           END-IF.
           MOVE HI-NAME TO P-HNAME.
           MOVE WN-SU TO P-SU.
           MOVE WN-UK TO P-UK.
           MOVE WN-GK TO P-GK.
           COMPUTE WN-AR = WN-UK - WN-GK.
           MOVE WN-AR TO P-AR.
           MOVE ZERO TO W-UT W-GT.
           IF  WN-SU NOT = ZERO
               IF  WN-UK NOT = ZERO
                   COMPUTE W-UT ROUNDED = WN-UK / WN-SU
               END-IF
           END-IF.
           IF  WN-SU NOT = ZERO
               IF  WN-GK NOT = ZERO
                   COMPUTE W-GT ROUNDED = WN-GK / WN-SU
               END-IF
           END-IF.
           MOVE W-UT TO P-UT.
           MOVE W-GT TO P-GT.
           MOVE WN-D TO W-D.
           PERFORM S-60 THRU S-65.
           MOVE W-RR TO P-RR.
           IF  CNT = ZERO
               PERFORM S-20 THRU S-25
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 63
               GO TO S-35
           END-IF.
           PERFORM S-05 THRU S-15.
           PERFORM S-20 THRU S-25.
       S-35.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD WN-SU TO WS-SU.
           ADD WN-UK TO WS-UK.
           ADD WN-GK TO WS-GK.
           ADD WN-AR TO WS-AR.
           ADD 1 TO CNT.
       S-40.
           EXIT.
       S-45.
           ADD WS-SU TO WA-SU.
           ADD WS-UK TO WA-UK.
           ADD WS-GK TO WA-GK.
           ADD WS-AR TO WA-AR.
           IF  CNT < 2
               GO TO S-55
           END-IF.
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15K.
           MOVE ALL "　" TO P-HNAME.
           MOVE "　　　　　　　　　　　　　［　合　計　］" TO P-HNAME.
           MOVE WS-SU TO P-SU.
           MOVE WS-UK TO P-UK.
           MOVE WS-GK TO P-GK.
           MOVE WS-AR TO P-AR.
           MOVE WS-D TO W-D.
           PERFORM S-60 THRU S-65.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 63
               GO TO S-50
           END-IF.
           PERFORM S-05 THRU S-15.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-50.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-55.
           EXIT.
       S-60.
           MOVE ZERO TO W-RR.
           IF  W-AR = ZERO
               GO TO S-65
           END-IF.
           MOVE W-UK TO W-KIN.
           IF  W-KIN = ZERO
               IF  W-AR > ZERO
                   MOVE 100 TO W-RR
               ELSE
                   MOVE -100 TO W-RR
               END-IF
           END-IF.
           IF  W-KIN = ZERO
               GO TO S-65
           END-IF.
           IF  W-KIN < ZERO
               COMPUTE W-KIN = W-KIN * -1
           END-IF.
           COMPUTE W-RR ROUNDED = (W-AR * 100) / W-KIN.
       S-65.
           EXIT.

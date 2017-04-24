       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY330.
      **************************************************************
      *    PROGRAM         :  担当者品種別年間売上粗利集計表       *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  ******                               *
      *    COMPILE TYPE    :  COBOL                                *
      **************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
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
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-SN           PIC 99.
           02  F              PIC  N(001) VALUE "年".
           02  H-SG           PIC Z9.
           02  F              PIC  N(001) VALUE "月".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(001) VALUE "～".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-EN           PIC 99.
           02  F              PIC  N(001) VALUE "年".
           02  H-EG           PIC Z9.
           02  F              PIC  N(018) VALUE
                "月　担当　品種別　年間売上粗利集計表".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD1A.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(102) VALUE SPACE.
           02  F              PIC  N(012) VALUE
                "（教育・ＶＩＶは含まず）".
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "担当".
           02  F              PIC  X(008) VALUE "  ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上数量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　足単価".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上原価".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　足単価".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　売上粗利益".
           02  F              PIC  N(004) VALUE "　利益率".
           02  F              PIC  X(001) VALUE "%".
       01  W-P.
           02  P-15K          PIC  X(005).
           02  P-TC           PIC  9(002).
           02  F              PIC  X(002).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  P-SU           PIC ---,---,--9.
           02  P-UK           PIC --,---,---,--9.
           02  P-UT           PIC ---,--9.
           02  P-GK           PIC --,---,---,--9.
           02  P-GT           PIC ---,--9.
           02  P-AR           PIC -----,---,--9.
           02  P-RR           PIC ----9.9.
           02  P-20K          PIC  X(005).
       01  W-DATA.
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
           02  W-STNC         PIC  9(002).
           02  W-ETNC         PIC  9(002) VALUE 99.
           02  W-UT           PIC S9(005).
           02  W-GT           PIC S9(005).
           02  W-RR           PIC S9(003)V9(01).
           02  W-KIN          PIC S9(010).
           02  W-TNC          PIC  9(002).
           02  W-TNCD  REDEFINES W-TNC.
             03  W-TNC1       PIC  9(001).
             03  W-TNC2       PIC  9(001).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-DMM          PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-HCD          PIC  9(006).
           02  W-D.
             03  W-SU         PIC S9(007).
             03  W-UK         PIC S9(010).
             03  W-GK         PIC S9(010).
             03  W-AR         PIC S9(009).
           02  WN-D.
             03  WN-SU        PIC S9(007).
             03  WN-UK        PIC S9(010).
             03  WN-GK        PIC S9(010).
             03  WN-AR        PIC S9(009).
       01  WT-D.
           02  WT-SU          PIC S9(007).
           02  WT-UK          PIC S9(010).
           02  WT-GK          PIC S9(010).
           02  WT-AR          PIC S9(009).
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
           COPY LIHIM.
           COPY LSPF.
       01  SSR-YF_HMY330.
           02  SSR-YF_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F               PIC  X(001).
           02  SSR-YF_LNAME    PIC  X(013)  VALUE "SSR-YF_HMY330".
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
           02  Y-TC1          PIC  9(002).
           02  Y-TNC          PIC  9(002).
           02  Y-TNCD  REDEFINES Y-TNC.
             03  Y-TNC1       PIC  9(001).
             03  Y-TNC2       PIC  9(001).
           02  Y-BC1          PIC  9(002).
           02  Y-BC2.
             03  Y-BC21       PIC  9(001).
             03  Y-BC22       PIC  9(001).
           02  Y-BC3          PIC  9(002).
           02  F              PIC  X(003).
           02  Y-FKC          PIC  9(002).
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　担当者品種別　売上集計表　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(024) VALUE
                "'  年   月 ～ '  年   月".
           02  FILLER  PIC  X(020) VALUE
                "担当者ｺｰﾄﾞ  00 ～ 99".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-STNC    PIC  9(002).
             03  A-ETNC    PIC  9(002).
           02  A-DMM     PIC  9(001).
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
            "C-MID" " " "0" "0" "350" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "15" "20" "24" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "17" "22" "20" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "20" "21" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "17" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-STNC" "9" "17" "34" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-STNC" BY REFERENCE W-STNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ETNC" "9" "17" "40" "2" "A-STNC" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-ETNC" BY REFERENCE W-ETNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "38" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "15" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "9" "15" "21" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-SN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "9" "15" "26" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NG" "9" "15" "35" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE W-EN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NG" "9" "15" "40" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NG" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                         RETURNING RESU.
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
           CALL "SD_Output" USING "D-NG" D-NG "p" 
                                         RETURNING RESU.
       M-06.
           CALL "SD_Accept" USING BY REFERENCE A-STNC "A-STNC" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-06
           END-IF.
       M-07.
           CALL "SD_Accept" USING BY REFERENCE A-ETNC "A-ETNC" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-06
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-07
           END-IF.
           IF  W-STNC > W-ETNC
               GO TO M-07
           END-IF.
       M-10.
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
               GO TO M-07
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
           IF  W-DMM = 9
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SSR-YF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SSR-YF_PNAME1 " " BY REFERENCE SSR-YF_IDLST "0".
       M-15.
      *           READ SSR-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSR-YF_PNAME1 BY REFERENCE SSR-YR " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SSR-YF_IDLST SSR-YF_PNAME1
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  Y-TNC < W-STNC OR > W-ETNC
               GO TO M-15
           END-IF.
           IF (Y-BC1 = 32) AND (Y-BC21 = 2)
               GO TO M-15
           END-IF.
           IF  Y-BC3 = 30
               GO TO M-15
           END-IF.
           IF  Y-NG < W-SNG OR > W-ENG
               GO TO M-15
           END-IF.
           IF  ZERO = Y-SU AND Y-UK AND Y-GK
               GO TO M-15
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE W-SN2 TO H-SN.
           MOVE W-SGET TO H-SG.
           MOVE W-EN2 TO H-EN.
           MOVE W-EGET TO H-EG.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO WA-D.
           PERFORM S-10 THRU S-15.
       M-20.
           MOVE Y-TNC1 TO W-TNC1.
           MOVE ZERO TO WS-D.
       M-25.
           MOVE Y-TNC2 TO W-TNC2.
           MOVE ZERO TO WT-D CHK.
       M-30.
           MOVE ZERO TO WN-D.
           MOVE Y-HCD TO W-HCD.
       M-35.
           ADD Y-SU TO WN-SU.
           ADD Y-UK TO WN-UK.
           ADD Y-GK TO WN-GK.
       M-40.
      *           READ SSR-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSR-YF_PNAME1 BY REFERENCE SSR-YR " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
           IF  Y-TNC < W-STNC OR > W-ETNC
               GO TO M-40
           END-IF.
           IF (Y-BC1 = 32) AND (Y-BC21 = 2)
               GO TO M-40
           END-IF.
           IF  Y-BC3 = 30
               GO TO M-40
           END-IF.
           IF  Y-NG < W-SNG OR > W-ENG
               GO TO M-40
           END-IF.
           IF  ZERO = Y-SU AND Y-UK AND Y-GK
               GO TO M-40
           END-IF.
           IF  Y-TNC1 NOT = W-TNC1
               GO TO M-50
           END-IF.
           IF  Y-TNC2 NOT = W-TNC2
               GO TO M-45
           END-IF.
           IF  Y-HCD = W-HCD
               GO TO M-35
           END-IF.
           PERFORM S-20 THRU S-25.
           GO TO M-30.
       M-45.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-35.
           GO TO M-25.
       M-50.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
           PERFORM S-05 THRU S-15.
           GO TO M-20.
       M-90.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
           PERFORM S-50 THRU S-55.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SSR-YF_IDLST SSR-YF_PNAME1.
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
           MOVE HEAD1A TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           IF  ZERO = WN-SU AND WN-UK AND WN-GK
               GO TO S-25
           END-IF.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           IF  CHK = 0
               MOVE 1 TO CHK
               MOVE W-TNC TO P-TC
           END-IF.
           MOVE W-HCD TO P-HCD.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊＊　ＨＩ－Ｍ　無し　＊＊＊" TO HI-NAME
           END-IF.
           MOVE HI-NAME TO P-HNA.
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
           MOVE ZERO TO W-D.
           MOVE WN-SU TO W-SU.
           MOVE WN-UK TO W-UK.
           MOVE WN-GK TO W-GK.
           MOVE WN-AR TO W-AR.
           PERFORM S-60 THRU S-70.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TNC TO P-TC
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD WN-SU TO WT-SU.
           ADD WN-UK TO WT-UK.
           ADD WN-GK TO WT-GK.
           ADD WN-AR TO WT-AR.
       S-25.
           EXIT.
       S-30.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE "　　　　　　　　　　　　（　小　計　）　" TO P-HNA.
           MOVE WT-SU TO P-SU.
           MOVE WT-UK TO P-UK.
           MOVE WT-GK TO P-GK.
           MOVE WT-AR TO P-AR.
           MOVE ZERO TO W-D.
           MOVE WT-SU TO W-SU.
           MOVE WT-UK TO W-UK.
           MOVE WT-GK TO W-GK.
           MOVE WT-AR TO W-AR.
           PERFORM S-60 THRU S-70.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE W-TNC TO P-TC
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WT-SU TO WS-SU.
           ADD WT-UK TO WS-UK.
           ADD WT-GK TO WS-GK.
           ADD WT-AR TO WS-AR.
       S-35.
           EXIT.
       S-40.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE "　　　　　　　　［　合　計　］　　　" TO P-HNA.
           MOVE WS-SU TO P-SU.
           MOVE WS-UK TO P-UK.
           MOVE WS-GK TO P-GK.
           MOVE WS-AR TO P-AR.
           MOVE ZERO TO W-D.
           MOVE WS-SU TO W-SU.
           MOVE WS-UK TO W-UK.
           MOVE WS-GK TO W-GK.
           MOVE WS-AR TO W-AR.
           PERFORM S-60 THRU S-70.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE W-TNC TO P-TC
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-SU TO WA-SU.
           ADD WS-UK TO WA-UK.
           ADD WS-GK TO WA-GK.
           ADD WS-AR TO WA-AR.
       S-45.
           EXIT.
       S-50.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE "　　　　　【　総　合　計　】　　　" TO P-HNA.
           MOVE WA-SU TO P-SU.
           MOVE WA-UK TO P-UK.
           MOVE WA-GK TO P-GK.
           MOVE WA-AR TO P-AR.
           MOVE ZERO TO W-D.
           MOVE WA-SU TO W-SU.
           MOVE WA-UK TO W-UK.
           MOVE WA-GK TO W-GK.
           MOVE WA-AR TO W-AR.
           PERFORM S-60 THRU S-70.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-55.
           EXIT.
       S-60.
           MOVE ZERO TO W-RR.
           IF  W-AR = ZERO
               GO TO S-70
           END-IF.
           IF  W-UK = ZERO
               GO TO S-65
           END-IF.
           IF  W-UK < ZERO
               COMPUTE W-UK = W-UK * -1
           END-IF.
           COMPUTE W-RR ROUNDED = (W-AR * 100) / W-UK.
           GO TO S-70.
       S-65.
           IF  W-AR > ZERO
               MOVE 100 TO W-RR
           ELSE
               MOVE -100 TO W-RR
           END-IF.
       S-70.
           EXIT.

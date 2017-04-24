       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HMY460.
       AUTHOR.        S-NAKAO.
      *********************************************************
      *    PROGRAM         :  得意先　月別　売上金額明細表    *
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
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-15K              PIC  X(005) VALUE X"1A24212078".
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-SN           PIC 99.
           02  F              PIC  N(001) VALUE "年".
           02  H-SG           PIC Z9.
           02  F              PIC  X(006) VALUE "月 ～ ".
           02  H-EN           PIC 99.
           02  F              PIC  N(001) VALUE "年".
           02  H-EG           PIC Z9.
           02  F              PIC  N(020) VALUE
                "月　得意先月別　売上金額明細表　　＊＊＊".
           02  F              PIC  X(014) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(020) VALUE SPACE.
           02  F              PIC  N(007) VALUE "住　　　　　所".
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  N(004) VALUE "電話番号".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(001) VALUE "年".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(001) VALUE "月".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　合　計".
       01  W-P.
           02  P-15K          PIC  X(005).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(026).
           02  F              PIC  X(001).
           02  P-JS           PIC  N(020).
           02  F              PIC  X(001).
           02  P-TEL          PIC  X(014).
           02  F              PIC  X(001).
           02  P-NG           PIC 99/99.
           02  P-UA           PIC --,---,---,--9.
           02  P-UAZ          PIC ---,---,--9.
           02  P-KEI          PIC --,---,---,--9.
       01  W-D.
           02  W-TCD          PIC  9(004).
           02  W-UA           PIC S9(010).
           02  W-UAZ          PIC S9(009).
           02  W-KEI          PIC S9(010).
           02  W-PC           PIC  9(002).
           02  W-PCD          PIC  9(002).
           02  CNT            PIC  9(002).
           02  W-ZC           PIC  9(001).
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
           02  W-GC           PIC  9(002).
           02  W-GCD          PIC  9(002).
           02  W-STCD         PIC  9(004).
           02  W-ETCD         PIC  9(004) VALUE 9999.
           02  W-PAGE         PIC  9(003) VALUE ZERO.
           02  W-DMM          PIC  9(001).
       01  W-TD.
           02  WT-UA          PIC S9(010).
           02  WT-UAZ         PIC S9(009).
           02  WT-KEI         PIC S9(010).
       01  W-RDA.
           02  W-RD    OCCURS  12.
             03  WR-NG        PIC  9(006).
             03  WR-NGD   REDEFINES WR-NG.
               04  WR-N       PIC  9(004).
               04  WR-G       PIC  9(002).
             03  WR-NGL   REDEFINES WR-NG.
               04  F          PIC  9(002).
               04  WR-NGS     PIC  9(004).
             03  WR-UA        PIC S9(010).
             03  WR-UAZ       PIC S9(009).
             03  WR-KEI       PIC S9(010).
       01  W-ATA.
           02  W-AT    OCCURS  12.
             03  WA-NG        PIC  9(006).
             03  WA-NGD   REDEFINES WA-NG.
               04  WA-N       PIC  9(004).
               04  WA-G       PIC  9(002).
             03  WA-NGL   REDEFINES WA-NG.
               04  F          PIC  9(002).
               04  WA-NGS     PIC  9(004).
             03  WA-UA        PIC S9(010).
             03  WA-UAZ       PIC S9(009).
             03  WA-KEI       PIC S9(010).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LITM.
           COPY LSPF.
       01  TM-YF_HMY460.
           02  TM-YF_PNAME1  PIC  X(009)  VALUE SPACE.
           02  F             PIC  X(001).
           02  TM-YF_LNAME   PIC  X(012)  VALUE "TM-YF_HMY460".
           02  F             PIC  X(001).
           02  TM-YF_KEY1    PIC  X(100)  VALUE SPACE.
           02  TM-YF_KEY2    PIC  X(100)  VALUE SPACE.
           02  TM-YF_SORT    PIC  X(100)  VALUE SPACE.
           02  TM-YF_IDLST   PIC  X(100)  VALUE SPACE.
           02  TM-YF_RES     USAGE  POINTER.
       01  TM-R.
           02  TM-TCD         PIC  9(004).
           02  TM-ZZ          PIC S9(009).
           02  TM-ZZZ         PIC S9(007).
           02  TM-UZ          PIC S9(009).
           02  TM-UZZ         PIC S9(007).
           02  TM-UA          PIC S9(009).
           02  TM-UAZ         PIC S9(007).
           02  TM-UN          PIC S9(008).
           02  TM-UNZ         PIC S9(006).
           02  TM-NK          PIC S9(009).
           02  TM-NKZ         PIC S9(007).
           02  TM-UG          PIC S9(009).
           02  TM-TC          PIC  9(002).
           02  F              PIC  X(029).
           02  TM-NG.
             03  TM-NEN       PIC  9(004).
             03  TM-GET       PIC  9(002).
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　得意先月別　売上金額明細表　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(024) VALUE
                "'  年  月 より '  年  月".
           02  FILLER  PIC  X(020) VALUE
                "ｺｰﾄﾞ  0000 より 9999".
           02  FILLER  PIC  X(022) VALUE
                "確認　OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-STCD    PIC  9(004).
             03  A-ETCD    PIC  9(004).
           02  A-DMM     PIC  9(001).
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC 99.
             03  FILLER  PIC Z9.
             03  FILLER  PIC 99.
             03  FILLER  PIC Z9.
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(021) VALUE
                  "***  DATA ｶﾞ ｵｵｲ  ***".
             03  E-ME2     PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "388" " " " " RETURNING RESU.
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
            "08C-MID" "X" "14" "22" "24" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "16" "24" "20" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "20" "23" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "16" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-STCD" "9" "16" "30" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ETCD" "9" "16" "40" "4" "A-STCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "40" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "14" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "99" "14" "23" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-SN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "Z9" "14" "27" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NG" "99" "14" "38" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE W-EN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NG" "Z9" "14" "42" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NG" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "48" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "48" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "21" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
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
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF.
           IF  W-STCD > W-ETCD
               GO TO M-25
           END-IF.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-40
           END-IF.
           IF  W-DMM = 9
               GO TO M-95
           END-IF.
           IF  W-DMM NOT = 1
              GO TO M-40
           END-IF.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO TM-YF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TM-YF_PNAME1 " " BY REFERENCE TM-YF_IDLST "0".
       M-45.
      *           READ TM-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TM-YF_PNAME1 BY REFERENCE TM-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TM-YF_IDLST TM-YF_PNAME1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" 
                                              RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                              RETURNING RESU
               GO TO M-95
           END-IF.
           IF  TM-TCD < W-STCD
               GO TO M-45
           END-IF.
           IF  TM-TCD > W-ETCD
               CALL "DB_F_Close" USING
                BY REFERENCE TM-YF_IDLST TM-YF_PNAME1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" 
                                              RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                              RETURNING RESU
               GO TO M-95
           END-IF.
           IF  TM-NG < W-SNG OR > W-ENG
               GO TO M-45
           END-IF.
           COMPUTE W-GCD = W-SGET - 1.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE W-SN2 TO H-SN.
           MOVE W-SGET TO H-SG.
           MOVE W-EN2 TO H-EN.
           MOVE W-EGET TO H-EG.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO W-ATA.
       M-50.
           MOVE TM-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME T-JSU T-JSS T-TEL
               MOVE "　＊＊　得意先マスター　無し　＊＊"  TO T-NAME
           END-IF.
           MOVE ZERO TO W-RDA W-ZC CNT.
       M-55.
           ADD 1 TO CNT.
           COMPUTE W-UA = TM-UA - TM-UN.
           COMPUTE W-UAZ = TM-UAZ - TM-UNZ.
           COMPUTE W-KEI = W-UA + W-UAZ.
           IF  ZERO NOT = W-UA OR W-UAZ
               MOVE 5 TO W-ZC
           END-IF.
           MOVE TM-NG TO WR-NG(CNT).
           MOVE W-UA TO WR-UA(CNT).
           MOVE W-UAZ TO WR-UAZ(CNT).
           MOVE W-KEI TO WR-KEI(CNT).
       M-60.
      *           READ TM-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TM-YF_PNAME1 BY REFERENCE TM-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF.
           IF  TM-TCD > W-ETCD
               GO TO M-85
           END-IF.
           IF  TM-NG < W-SNG OR > W-ENG
               GO TO M-60
           END-IF.
           IF  W-TCD = TM-TCD
               GO TO M-55
           END-IF.
           IF  W-ZC NOT = ZERO
               PERFORM S-20 THRU S-40
           END-IF.
           GO TO M-50.
       M-85.
           IF  W-ZC NOT = ZERO
               PERFORM S-20 THRU S-40
           END-IF.
           PERFORM S-45 THRU S-60.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TM-YF_IDLST TM-YF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
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
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE 5 TO W-PC.
       S-15.
           EXIT.
       S-20.
           ADD 2 TO CNT.
           COMPUTE W-PCD = 62 - W-PC.
           IF  CNT > W-PCD
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE ZERO TO CNT W-TD.
       S-25.
           ADD 1 TO CNT.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE ALL "　" TO P-NAME P-JS.
           IF  CNT = 1
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-NAME
               MOVE T-JSU TO P-JS
               MOVE T-TEL TO P-TEL
           END-IF.
           IF  CNT = 2
               MOVE T-JSS TO P-JS
           END-IF.
           IF  CNT = 13
               GO TO S-30
           END-IF.
           IF  WR-NG(CNT) = ZERO
               GO TO S-30.
           MOVE WR-NGS(CNT) TO P-NG.
           MOVE WR-UA(CNT) TO P-UA.
           MOVE WR-UAZ(CNT) TO P-UAZ.
           MOVE WR-KEI(CNT) TO P-KEI.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE WR-G(CNT) TO W-GC.
           IF  W-GCD >= W-GC
               ADD 12 TO W-GC
           END-IF.
           SUBTRACT W-GCD FROM W-GC.
           IF  WA-NG(W-GC) = ZERO
               MOVE WR-NG(CNT) TO WA-NG(W-GC)
           END-IF.
           ADD WR-UA(CNT) TO WT-UA WA-UA(W-GC).
           ADD WR-UAZ(CNT) TO WT-UAZ WA-UAZ(W-GC).
           ADD WR-KEI(CNT) TO WT-KEI WA-KEI(W-GC).
           ADD 1 TO W-PC.
           GO TO S-25.
       S-30.
           IF  CNT = 2
               IF  T-JSS = SPACE
                   GO TO S-35
           END-IF.
           IF  CNT > 2
               MOVE "[  TOTAL  ] " TO P-TEL
               MOVE WT-UA TO P-UA
               MOVE WT-UAZ TO P-UAZ
               MOVE WT-KEI TO P-KEI
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD 1 TO W-PC.
       S-35.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD 1 TO W-PC.
       S-40.
           EXIT.
       S-45.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 50
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE ZERO TO CNT W-TD W-ZC.
       S-50.
           ADD 1 TO CNT.
           IF  CNT = 13
               GO TO S-55
           END-IF.
           IF  WA-NG(CNT) = ZERO
               IF  W-ZC = ZERO
                   GO TO S-50
               ELSE
                   GO TO S-55
               END-IF
           END-IF.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE ALL "　" TO P-NAME P-JS.
           IF  W-ZC = ZERO
               MOVE 5 TO W-ZC
               MOVE "　　　　　　　　【　　総　合　計　　】" TO P-JS
           END-IF.
           MOVE WA-NGS(CNT) TO P-NG.
           MOVE WA-UA(CNT) TO P-UA.
           MOVE WA-UAZ(CNT) TO P-UAZ.
           MOVE WA-KEI(CNT) TO P-KEI.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD WA-UA(CNT) TO WT-UA.
           ADD WA-UAZ(CNT) TO WT-UAZ.
           ADD WA-KEI(CNT) TO WT-KEI.
           ADD 1 TO W-PC.
           GO TO S-50.
       S-55.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE ALL "　" TO P-NAME P-JS.
           MOVE "[  TOTAL  ] " TO P-TEL.
           MOVE WT-UA TO P-UA.
           MOVE WT-UAZ TO P-UAZ.
           MOVE WT-KEI TO P-KEI.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-60.
           EXIT.

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      HMM550.
      *********************************************************
      *    PROGRAM         :  担当得意先品名別単価　リスト    *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. NEAC-SYSTEM3100.
       OBJECT-COMPUTER. NEAC-SYSTEM3100.
       DATA    DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-MSG              PIC  X(040).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(040) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　担当得意先品名別単価　リスト　　＊＊＊".
           02  F              PIC  X(024) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE.".
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(002) VALUE "P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "担当".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
               "得　　意　　先　　名".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  X(005) VALUE "  :  ".
           02  F              PIC  N(002) VALUE "担当".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
               "得　　意　　先　　名".
           02  F              PIC  X(041) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE
               "品　　　　　名　".
           02  F              PIC  X(024) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｻｲｽﾞ  ".
           02  F              PIC  N(002) VALUE "単価".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "最終年月".
           02  F              PIC  X(005) VALUE "  :  ".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE
               "品　　　　　名　".
           02  F              PIC  X(024) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｻｲｽﾞ  ".
           02  F              PIC  N(002) VALUE "単価".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "最終年月".
       01  W-PR.
           02  W-P     OCCURS  60.
             03  P-DF1.
               04  F          PIC  X(001).
               04  P-TNC1     PIC  9(002).
               04  F          PIC  X(001).
               04  P-TCD1     PIC  9(004).
               04  F          PIC  X(001).
               04  P-TNA1     PIC  N(026).
               04  F          PIC  X(017).
             03  P-DF2   REDEFINES P-DF1.
               04  P-NF       PIC  N(002).
               04  F          PIC  X(003).
               04  P-HCD1     PIC  9(006).
               04  F          PIC  X(001).
               04  P-HNA1     PIC  N(024).
               04  F          PIC  X(001).
               04  P-SIZ1     PIC  9(001).
               04  P-TAN1     PIC ZZZ,ZZ9.
               04  F          PIC  X(002).
               04  P-NG1      PIC 99/99.
             03  F            PIC  X(002).
             03  P-C          PIC  X(001).
             03  F            PIC  X(002).
             03  P-DR1.
               04  F          PIC  X(001).
               04  P-TNC2     PIC  9(002).
               04  F          PIC  X(001).
               04  P-TCD2     PIC  9(004).
               04  F          PIC  X(001).
               04  P-TNA2     PIC  N(026).
               04  F          PIC  X(017).
             03  P-DR2   REDEFINES P-DR1.
               04  P-NR       PIC  N(002).
               04  F          PIC  X(003).
               04  P-HCD2     PIC  9(006).
               04  F          PIC  X(001).
               04  P-HNA2     PIC  N(024).
               04  F          PIC  X(001).
               04  P-SIZ2     PIC  9(001).
               04  P-TAN2     PIC ZZZ,ZZ9.
               04  F          PIC  X(002).
               04  P-NG2      PIC 99/99.
       01  W-DATA.
           02  W-POC          PIC  9(001).
           02  W-PAGE         PIC  9(003).
           02  W-TNCD.
             03  W-FTNC       PIC  9(002).
             03  W-TTNC       PIC  9(002).
           02  W-TCDD.
             03  W-FTCD       PIC  9(004).
             03  W-TTCD       PIC  9(004).
           02  W-HCDD.
             03  W-FHCD       PIC  9(006).
             03  W-THCD       PIC  9(006).
           02  W-DMM          PIC  9(001).
           02  W-TNC          PIC  9(002).
           02  W-TCD          PIC  9(004).
           02  W-HCD          PIC  9(006).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
             03  CHK3         PIC  9(001).
           02  W-LCD.
             03  W-LD         PIC  9(002).
             03  W-CD         PIC  9(001).
           COPY LSTAT.
      *
           COPY LITM.
           COPY LIHIM.
           COPY LSPF.
      *FD  THTM
       01  THTM_HMM550.
           02  THTM_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  THTM_LNAME     PIC  X(011) VALUE "THTM_HMM550".
           02  F              PIC  X(001).
           02  THTM_KEY1      PIC  X(100) VALUE SPACE.
           02  THTM_KEY2      PIC  X(100) VALUE SPACE.
           02  THTM_SORT      PIC  X(100) VALUE SPACE.
           02  THTM_IDLST     PIC  X(100) VALUE SPACE.
           02  THTM_RES       USAGE  POINTER.
       01  THT-R.
           02  THT-KD.
             03  THT-KEY.
               04  THT-TCD    PIC  9(004).
               04  THT-HCD.
                 05  THT-HCD1 PIC  9(004).
                 05  THT-HCD2 PIC  9(002).
               04  THT-SIZ    PIC  9(001).
             03  THT-TCD1     PIC  9(004).
           02  THT-KDD   REDEFINES THT-KD.
             03  THT-TCD3     PIC  9(004).
             03  THT-KEY2.
               04  THT-HCD2   PIC  9(006).
               04  THT-SIZ2   PIC  9(001).
               04  THT-TCD2   PIC  9(004).
           02  THT-T          PIC  9(005).
           02  THT-TT         PIC  9(005).
           02  F              PIC  X(009).
           02  THT-TNC        PIC  9(002).
           02  F              PIC  X(002).
           02  THT-NG         PIC  9(004).
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　担当得意先品名別　単価リスト　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(038) VALUE
                "担当ｺｰﾄﾞ      00 ～ 99       終了=ｆ･9".
           02  FILLER  PIC  X(024) VALUE
                "得意先ｺｰﾄﾞ  0000 ～ 9999".
           02  FILLER  PIC  X(026) VALUE
                "品名ｺｰﾄﾞ  000000 ～ 999999".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-FTNC  PIC  9(002).
             03  A-TTNC  PIC  9(002).
           02  FILLER.
             03  A-FTCD  PIC  9(004).
             03  A-TTCD  PIC  9(004).
           02  FILLER.
             03  A-FHCD  PIC  9(006).
             03  A-THCD  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  DSP-ERR.
           02  FILLER.
             03  E-ME    PIC  X(040).
             03  E-STAT  PIC  X(002).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
       PROCEDURE   DIVISION.
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
           "C-MID" " " "0" "0" "446" " " " " RETURNING RESU.
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
           "08C-MID" "X" "12" "21" "38" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "14" "21" "24" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "10C-MID" "X" "16" "21" "26" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "11C-MID" "X" "23" "23" "22" "10C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "25" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "12" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-FTNC" "9" "12" "35" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-FTNC" BY REFERENCE W-FTNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-TTNC" "9" "12" "41" "2" "A-FTNC" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-TTNC" BY REFERENCE W-TTNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-ACP" " " "14" "0" "8" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-FTCD" "9" "14" "33" "4" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-FTCD" BY REFERENCE W-FTCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-TTCD" "9" "14" "41" "4" "A-FTCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-TTCD" BY REFERENCE W-TTCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03C-ACP" " " "16" "0" "12" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-FHCD" "9" "16" "31" "6" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-FHCD" BY REFERENCE W-FHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-THCD" "9" "16" "41" "6" "A-FHCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-THCD" BY REFERENCE W-THCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "40" "1" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "102" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "102" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME" "X" "24" "15" "40" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-ME" BY REFERENCE W-MSG "40" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" "E-ME" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           MOVE 99 TO W-TTNC.
           MOVE 9999 TO W-TTCD.
           MOVE 999999 TO W-THCD.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-FTNC "A-FTNC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-TTNC "A-TTNC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-FTNC > W-TTNC
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-FTCD "A-FTCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-TTCD "A-TTCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-FTCD > W-TTCD
               GO TO M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-FHCD "A-FHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-THCD "A-THCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-30
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF
           IF  W-FHCD > W-THCD
               GO TO M-35
           END-IF.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-35
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-40
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-40
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO THTM_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" THTM_PNAME1 " " BY REFERENCE THTM_IDLST "0".
       M-45.
      *           READ THTM AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" THTM_PNAME1 BY REFERENCE THT-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE THTM_IDLST THTM_PNAME1
               MOVE SPACE TO W-MSG
               MOVE "***  DATA ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-05
           END-IF
           IF  W-TTNC < THT-TNC
               CALL "DB_F_Close" USING
                BY REFERENCE THTM_IDLST THTM_PNAME1
               MOVE SPACE TO W-MSG
               MOVE "***  DATA ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-05
           END-IF
           IF  W-FTNC > THT-TNC
               GO TO M-45
           END-IF
           IF  THT-TCD < W-FTCD OR > W-TTCD
               GO TO M-45
           END-IF
           IF  THT-HCD < W-FHCD OR > W-THCD
               GO TO M-45
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           ACCEPT H-DATE FROM DATE.
           PERFORM S-20 THRU S-30.
       M-50.
           MOVE THT-TNC TO W-TNC.
           MOVE ZERO TO CHK.
       M-55.
           MOVE THT-TCD TO W-TCD.
           MOVE THT-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "　　＊＊　得意先なし　＊＊" TO T-NAME
           END-IF
           MOVE ZERO TO CHK2 CHK3.
       M-60.
           MOVE THT-HCD TO W-HCD.
           MOVE THT-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "　　＊＊　品名なし　＊＊" TO HI-NAME
           END-IF
           MOVE ZERO TO CHK3.
       M-65.
           PERFORM S-60 THRU S-75.
       M-70.
      *           READ THTM AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" THTM_PNAME1 BY REFERENCE THT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  W-TTNC < THT-TNC
               GO TO M-90
           END-IF
           IF  THT-TCD < W-FTCD OR > W-TTCD
               GO TO M-70
           END-IF
           IF  THT-HCD < W-FHCD OR > W-THCD
               GO TO M-70
           END-IF
           IF  W-TNC NOT = THT-TNC
               GO TO M-50
           END-IF
           IF  W-TCD NOT = THT-TCD
               GO TO M-55
           END-IF
           IF  W-HCD NOT = THT-HCD
               GO TO M-60
           END-IF
           GO TO M-65.
       M-90.
           PERFORM S-45 THRU S-55.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           IF  W-POC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF
           GO TO M-05.
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
           MOVE HEAD1 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-PR.
           MOVE ZERO TO W-LD.
       S-25.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               MOVE SPACE TO P-HNA1(W-LD) P-HNA2(W-LD)
                             P-NF(W-LD) P-NR(W-LD)
               GO TO S-25
           END-IF
           MOVE ZERO TO W-LD W-CD CHK.
       S-30.
           EXIT.
       S-35.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               GO TO S-40
           END-IF
           IF  W-CD = 0
               MOVE 1 TO W-CD W-LD
               MOVE ZERO TO CHK
               GO TO S-40
           END-IF
           PERFORM S-45 THRU S-55.
           PERFORM S-20 THRU S-30.
           GO TO S-35.
       S-40.
           EXIT.
       S-45.
           IF  W-POC = 0
               MOVE 5 TO W-POC
               CALL "PR_Open" RETURNING RESP
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE ZERO TO W-LD.
       S-50.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               IF  P-C(W-LD) NOT = SPACE
                   MOVE SPACE TO SP-R
                   MOVE W-P(W-LD) TO SP-R
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   MOVE SPACE TO SP-R
                   GO TO S-50
               END-IF
           END-IF.
       S-55.
           EXIT.
       S-60.
           PERFORM S-35 THRU S-40.
           IF  CHK2 NOT = 0
               GO TO S-65
           END-IF
           IF  W-LD = 58
               PERFORM S-35 THRU S-40
           END-IF
           MOVE 1 TO CHK2.
           IF  W-CD = 0
               MOVE ":" TO P-C(W-LD)
               MOVE SPACE TO P-DF1(W-LD)
               MOVE W-TCD TO P-TCD1(W-LD)
               MOVE T-NAME TO P-TNA1(W-LD)
               IF  CHK1 = 0
                   MOVE 1 TO CHK1
                   MOVE W-TNC TO P-TNC1(W-LD)
               END-IF
           END-IF
           IF  W-CD = 1
               MOVE SPACE TO P-DR1(W-LD)
               MOVE W-TCD TO P-TCD2(W-LD)
               MOVE T-NAME TO P-TNA2(W-LD)
               IF  CHK1 = 0
                   MOVE 1 TO CHK1
                   MOVE W-TNC TO P-TNC2(W-LD)
               END-IF
           END-IF
           PERFORM S-35 THRU S-40.
       S-65.
           IF  CHK3 NOT = 0
               GO TO S-70
           END-IF
           MOVE 1 TO CHK3.
           IF  W-CD = 0
               MOVE ":" TO P-C(W-LD)
               MOVE SPACE TO P-NF(W-LD)
               MOVE W-HCD TO P-HCD1(W-LD)
               MOVE HI-NAME TO P-HNA1(W-LD)
               MOVE THT-SIZ TO P-SIZ1(W-LD)
               MOVE THT-T TO P-TAN1(W-LD)
               IF  THT-NG NOT = ZERO
                   MOVE THT-NG TO P-NG1(W-LD)
               END-IF
           END-IF
           IF  W-CD NOT = 0
               MOVE SPACE TO P-NR(W-LD)
               MOVE W-HCD TO P-HCD2(W-LD)
               MOVE HI-NAME TO P-HNA2(W-LD)
               MOVE THT-SIZ TO P-SIZ2(W-LD)
               MOVE THT-T TO P-TAN2(W-LD)
               IF  THT-NG NOT = ZERO
                   MOVE THT-NG TO P-NG2(W-LD)
               END-IF
           END-IF
           GO TO S-75.
       S-70.
           IF  W-CD = 0
               MOVE ":" TO P-C(W-LD)
               MOVE THT-SIZ TO P-SIZ1(W-LD)
               MOVE THT-T TO P-TAN1(W-LD)
               IF  THT-NG NOT = ZERO
                   MOVE THT-NG TO P-NG1(W-LD)
               END-IF
           END-IF
           IF  W-CD NOT = 0
               MOVE THT-SIZ TO P-SIZ2(W-LD)
               MOVE THT-T TO P-TAN2(W-LD)
               IF  THT-NG NOT = ZERO
                   MOVE THT-NG TO P-NG2(W-LD)
               END-IF
           END-IF.
       S-75.
           EXIT.

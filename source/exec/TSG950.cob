       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TSG950.
       AUTHOR. T-WAKIMOTO.
       DATE-WRITTEN. 1982-05-12.
      *********************************************************
      *    PROGRAM         :  支払手形　期日　参考表          *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCBM01                          *
      *        変更　　　  :  62/06/02                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE   SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-15K              PIC  X(005) VALUE X"1A24212078".
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(003) VALUE  "【　　".
           02  H-NEN          PIC  Z(002).
           02  F              PIC  N(001) VALUE  "年".
           02  H-GET          PIC  Z(002).
           02  F              PIC  N(016) VALUE
                 "月振出し　支手　期日参考表　　】".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(006) VALUE "    P.".
           02  H-PAGE         PIC  Z(002).
       01  HEAD2.
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE  "期　日".
           02  F              PIC  X(012) VALUE "      ｺｰﾄﾞ  ".
           02  F              PIC  N(010) VALUE
                 "取　　引　　先　　名".
           02  F              PIC  X(028) VALUE SPACE.
       01  W-P1.
           02  F              PIC  X(003).
           02  P-NEN          PIC Z9.
           02  P-NM           PIC  N(001).
           02  P-GET          PIC Z9.
           02  P-GM           PIC  N(001).
           02  P-PEY          PIC Z9.
           02  P-PM           PIC  N(001).
           02  F              PIC  X(003).
           02  P-KEY          PIC  9(004).
           02  F              PIC  X(001).
           02  P-C            PIC  X(001).
           02  P-NAME         PIC  N(024).
       01  W-P2.
           02  F              PIC  X(050) VALUE
                "   -----------------------------------------------".
           02  F              PIC  X(022) VALUE
                "----------------------".
       01  W-D.
           02  W-WNG.
             03  W-WNEN       PIC  9(002).
             03  W-WGET       PIC  9(002).
             03  W-WPEY       PIC  9(002).
           02  W-NGPD         PIC  9(008).
           02  W-DATE         PIC  9(008).
           02  W-NGP   REDEFINES W-DATE.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-NGL   REDEFINES W-NG.
               04  F          PIC  9(002).
               04  W-NGS      PIC  9(004).
             03  W-PEY        PIC  9(002).
           02  W-NGD          PIC  9(006).
           02  CHK            PIC  9(001).
           02  W-ST.
             03  W-TTS        PIC  9(002).
             03  W-THN        PIC  9(002).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LISM.
           COPY LICAL.
           COPY LSPF.
      *FD  SAITO-F
       01  SAITO-F_TSG950.
           02  SAITO-F_PNAME1 PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  SAITO-F_LNAME  PIC  X(014)  VALUE "SAITO-F_TSG950".
           02  F              PIC  X(001).
           02  SAITO-F_KEY1   PIC  X(100)  VALUE SPACE.
           02  SAITO-F_KEY2   PIC  X(100)  VALUE SPACE.
           02  SAITO-F_SORT   PIC  X(100)  VALUE SPACE.
           02  SAITO-F_IDLST  PIC  X(100)  VALUE SPACE.
           02  SAITO-F_RES    USAGE  POINTER.
       01  SAITO-R.
           02  SAITO-TCD      PIC  9(004).
           02  SAITO-ST.
             03  SAITO-TTS    PIC  9(002).
             03  SAITO-THN    PIC  9(002).
           02  SAITO-TMC      PIC  9(001).
           02  F              PIC  X(055).
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
           02  FILLER  PIC  N(021) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                 "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                 "＊＊＊　　支払手形　期日　参考表　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                 "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER.
             03  FILLER  PIC  X(024) VALUE
                  "（    年   月 振出し　）".
             03  FILLER  PIC Z9 .
             03  FILLER  PIC Z9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  CALNM ﾅｼ  ***".
             03  E-KEY   PIC  9(004).
             03  E-DATE  PIC  9(008).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "322" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "42" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" " " "12" "0" "28" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "0108C-MID" "X" "12" "19" "24" " " "08C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "0208C-MID" "Z9" "12" "23" "2" "0108C-MID" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0208C-MID" BY REFERENCE W-WNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0308C-MID" "Z9" "12" "28" "2" "0208C-MID" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0308C-MID" BY REFERENCE W-WGET "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "109" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "109" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-KEY" "9" "24" "40" "4" "E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING
            "E-KEY" BY REFERENCE SAITO-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-DATE" "9" "24" "47" "8" "E-KEY" " " RETURNING RESU.
       CALL "SD_From" USING
            "E-DATE" BY REFERENCE W-DATE "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-DATE" " " RETURNING RESU.
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
           MOVE DATE-04R TO H-DATE.
           MOVE ZERO TO W-DATE.
           MOVE D-NTNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-NGD.
           COMPUTE W-WNEN = W-NEN - DATE-YC1.
           MOVE W-GET TO W-WGET.
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SAITO-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SAITO-F_PNAME1 " " BY REFERENCE SAITO-F_IDLST "0".
       M-10.
      *           READ SAITO-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SAITO-F_PNAME1 BY REFERENCE SAITO-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SAITO-F_IDLST SAITO-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  SAITO-TTS = ZERO
               GO TO M-10
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" CALNM_PNAME1 "SHARED" BY REFERENCE CALNM_IDLST "1"
            "CL-KEY" BY REFERENCE CL-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO W-PAGE.
           MOVE W-WNEN TO H-NEN.
           MOVE W-WGET TO H-GET.
           PERFORM S-10 THRU S-15.
       M-15.
           MOVE SAITO-ST TO W-ST.
           MOVE ZERO TO W-DATE CHK.
           MOVE W-NGD TO W-NG.
           MOVE SAITO-THN TO W-PEY.
           ADD SAITO-TTS TO W-GET.
           IF  W-PEY = ZERO
               SUBTRACT 1 FROM W-GET
           END-IF
           IF  W-GET > 12
               ADD 1 TO W-NEN
               SUBTRACT 12 FROM W-GET
           END-IF
           IF  W-GET = 01 OR 05
               IF  W-PEY = 05
                   MOVE 10 TO W-PEY
               END-IF
           END-IF
           IF  W-PEY NOT = ZERO
               GO TO M-19
           END-IF
           MOVE ZERO TO W-NGPD.
           MOVE W-DATE TO CL-KEY.
      *           START CALNM KEY NOT < CL-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CALNM_PNAME1 "CL-KEY" " NOT < " CL-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DATE" E-DATE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-35
           END-IF
      *           READ CALNM NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CALNM_PNAME1 BY REFERENCE CALN-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DATE" E-DATE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-35
           END-IF.
       M-16.
           IF  CL-NG NOT = W-NG
               MOVE W-NGPD TO W-DATE
               GO TO M-19
           END-IF
           MOVE CL-DATE TO W-NGPD.
      *           READ CALNM NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CALNM_PNAME1 BY REFERENCE CALN-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE W-NGPD TO W-DATE
           END-IF
           GO TO M-16.
       M-19.
           MOVE W-DATE TO CL-KEY.
      *           READ CALNM WITH UNLOCK INVALID KEY 
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CALNM_PNAME1 BY REFERENCE CALN-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DATE" E-DATE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-35
           END-IF
           GO TO M-21. 
       M-20.
      *           READ CALNM NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CALNM_PNAME1 BY REFERENCE CALN-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DATE" E-DATE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-35
           END-IF.
       M-21.
           IF  CL-SJ = 1
               GO TO M-20
           END-IF
           MOVE CL-KEY TO W-DATE.
           COMPUTE W-WNEN = W-NEN - DATE-YC1.
           MOVE W-GET TO W-WGET.
           MOVE W-PEY TO W-WPEY.
       M-25.
           MOVE SAITO-TCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
           END-IF
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-NM P-GM P-PM P-NAME.
           IF  CHK = 0
               MOVE 5 TO CHK
               MOVE W-WNEN TO P-NEN
               MOVE W-WGET TO P-GET
               MOVE W-WPEY TO P-PEY
               MOVE  "年" TO P-NM
               MOVE  "月" TO P-GM
               MOVE  "日" TO P-PM
           END-IF
           MOVE S-KEY TO P-KEY.
           MOVE S-NAME TO P-NAME.
           IF  SAITO-TMC NOT = ZERO
               MOVE "*" TO P-C
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-WNEN TO P-NEN
               MOVE W-WGET TO P-GET
               MOVE W-WPEY TO P-PEY
               MOVE  "年" TO P-NM
               MOVE  "月" TO P-GM
               MOVE  "日" TO P-PM
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       M-35.
      *           READ SAITO-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SAITO-F_PNAME1 BY REFERENCE SAITO-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  SAITO-TTS = ZERO
               GO TO M-35
           END-IF
           IF  SAITO-ST = W-ST
               GO TO M-25
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO M-15.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE SAITO-F_IDLST SAITO-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CALNM_IDLST CALNM_PNAME1.
           CALL "PR_Close" RETURNING RESP.
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
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.

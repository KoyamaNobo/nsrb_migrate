       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSA050.
      **********************************************************
      *****     送金案内　明細表　（領収証チェック用）     *****
      *****              FDL : FTA050                      *****
      **********************************************************
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
       01  HEAD1.
           02  F            PIC  X(017) VALUE SPACE.
           02  W-15K        PIC  X(005) VALUE X"1A24212078".
           02  F            PIC  X(008) VALUE X"1A26212068222176".
           02  F            PIC  N(008) VALUE   "送金案内　明細表".
           02  F            PIC  X(008) VALUE X"1A26212068212078".
           02  W-20K        PIC  X(005) VALUE X"1A24212474".
           02  F            PIC  X(003) VALUE SPACE.
           02  F            PIC  N(010) VALUE   "（領収証チェック用）".
           02  F            PIC  X(008) VALUE SPACE.
           02  F            PIC  X(005) VALUE "DATE ".
           02  H-DATE       PIC 99B99B99.
       01  HEAD2.
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(003) VALUE   "送金日".
           02  F            PIC  X(010) VALUE " ｺｰﾄﾞ     ".
           02  F            PIC  N(007) VALUE   "送　金　先　名".
           02  F            PIC  X(027) VALUE SPACE.
           02  F            PIC  N(003) VALUE   "金　額".
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(002) VALUE   "小手".
           02  F            PIC  X(002) VALUE SPACE.
           02  F            PIC  N(006) VALUE   "領収証受取日".
           02  F            PIC  X(002) VALUE SPACE.
       01  W-P.
           02  P-15K        PIC  X(005).
           02  F            PIC  X(001).
           02  P-DATE       PIC  9(006).
           02  F            PIC  X(001).
           02  P-SCD        PIC  9(004).
           02  F            PIC  X(002).
           02  P-NAME       PIC  N(024).
           02  P-KIN        PIC ZZ,ZZZ,ZZZ,ZZ9.
           02  P-KS         PIC  Z(003).
           02  P-TS         PIC  Z(002).
           02  F            PIC  X(016).
           02  P-20K        PIC  X(005).
       01  W-DATA.
           02  W-KEY.
             03  W-DATE     PIC  9(006).
             03  W-SCD      PIC  9(004).
           02  W-D.
             03  W-KS       PIC  9(002).
             03  W-TS       PIC  9(002).
             03  W-KIN      PIC  9(010).
           02  W-SC         PIC  9(001).
           02  W-DATED      PIC  9(006).
           02  W-NGP        PIC  9(006).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NEN      PIC  9(002).
             03  W-GET      PIC  9(002).
             03  W-PEY      PIC  9(002).
       01  ERR-STAT         PIC  X(002).
      *
           COPY LISM.
           COPY LSPF.
      *FD  TSA-F
       01  TSA-F_TSA050.
           02  TSA-F_PNAME1 PIC  X(009) VALUE SPACE.
           02  F            PIC  X(001).
           02  TSA-F_LNAME  PIC  X(012) VALUE "TSA-F_TSA050".
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
           02  FILLER  PIC  N(018) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                  "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                  "＊＊＊　　送金案内　明細表　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                  "＊＊＊　（領収証チェック用）　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999-FTA050" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "252" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "36" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "36" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "36" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "36" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "36" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "36" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "36" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "29" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "29" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO TSA-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TSA-F_PNAME1 " " BY REFERENCE TSA-F_IDLST "0".
      *           READ TSA-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TSA-F_PNAME1 BY REFERENCE TSA-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE TSA-F_IDLST TSA-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           ACCEPT W-NGP FROM DATE.
           MOVE W-NGP TO H-DATE.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO W-DATED.
       M-10.
           MOVE SA-KEY TO W-KEY.
           MOVE SA-SC TO W-SC.
           MOVE ZERO TO W-D.
       M-15.
           ADD SA-KIN TO W-KIN.
           IF  SA-SC = 1
               ADD 1 TO W-KS
           END-IF
           IF  SA-SC = 2
               ADD 1 TO W-TS
           END-IF.
       M-20.
      *           READ TSA-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TSA-F_PNAME1 BY REFERENCE TSA-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  W-SC = 3
               GO TO M-25
           END-IF
           IF  SA-KEY = W-KEY
               GO TO M-15
           END-IF.
       M-25.
           PERFORM S-20 THRU S-25.
           GO TO M-10.
       M-90.
           PERFORM S-20 THRU S-25.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TSA-F_IDLST TSA-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           MOVE W-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   "　＊＊　マスター　なし　＊＊　" TO S-NAME
           END-IF
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME.
           IF  W-DATE NOT = W-DATED
               MOVE W-DATE TO W-DATED
               MOVE W-DATE TO P-DATE
           END-IF
           MOVE W-SCD TO P-SCD.
           MOVE S-NAME TO P-NAME.
           MOVE W-KIN TO P-KIN.
           MOVE W-KS TO P-KS.
           MOVE W-TS TO P-TS.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               MOVE W-DATE TO P-DATE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           EXIT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG460.
      *********************************************************
      *    PROGRAM         :  材料日付別　受払表              *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/10                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-END              PIC  9(001) VALUE 0.
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
           02  F              PIC  X(042) VALUE SPACE.
           02  F              PIC  N(019) VALUE
                "＊＊＊　　材料日付別　受払表　　＊＊＊".
           02  F              PIC  X(032) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  F              PIC  X(039) VALUE SPACE.
           02  F              PIC  X(018) VALUE ":---------------  ".
           02  F              PIC  N(003) VALUE "入　庫".
           02  F              PIC  X(022) VALUE
                "  --------------:---  ".
           02  F              PIC  N(003) VALUE "出　庫".
           02  F              PIC  X(022) VALUE
                "  ---:--------------  ".
           02  F              PIC  N(003) VALUE "在　庫".
           02  F              PIC  X(016) VALUE "  --------------".
       01  HEAD3.
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(007) VALUE "材　　料　　名".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(001) VALUE "日".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(001) VALUE "付".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(003) VALUE "修正日".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "単　　価".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "単　　価".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
       01  W-P1.
           02  P-15K          PIC  X(005).
           02  P-JCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(037).
           02  P-20K          PIC  X(005).
           02  P-ZKM          PIC  N(008).
           02  P-C1           PIC  X(001).
           02  P-ZSU          PIC ---,---,--9.99.
           02  P-ZTN          PIC -----,--9.99.
           02  P-ZKIN         PIC ----,---,--9.
       01  W-P2.
           02  F              PIC  X(007).
           02  P-TM           PIC  N(016).
           02  P-DATA  REDEFINES P-TM.
             03  F            PIC  X(018).
             03  P-GP         PIC 99/99.
             03  F            PIC  X(002).
             03  P-SGP        PIC 99/99.
             03  F            PIC  X(002).
           02  P-C2           PIC  X(001).
           02  P-NSU          PIC ---,---,--9.99.
           02  P-NTN          PIC -----,--9.99.
           02  P-NKIN         PIC ----,---,--9.
           02  F              PIC  X(001).
           02  P-C3           PIC  X(001).
           02  P-SSU          PIC ----,---,--9.99.
           02  F              PIC  X(001).
           02  P-C4           PIC  X(001).
           02  P-YSU          PIC ---,---,--9.99.
           02  P-YTN          PIC -----,--9.99.
           02  P-YKIN         PIC ----,---,--9.
       01  W-TOTAL.
           02  WN-T.
             03  WN-NSU       PIC S9(008)V9(02).
             03  WN-NKIN      PIC S9(009).
             03  WN-SSU       PIC S9(008)V9(02).
           02  WH-T.
             03  WH-ZKIN      PIC S9(009).
             03  WH-NKIN      PIC S9(009).
             03  WH-YKIN      PIC S9(009).
           02  WB-T.
             03  WB-ZKIN      PIC S9(009).
             03  WB-NKIN      PIC S9(009).
             03  WB-YKIN      PIC S9(009).
           02  WJ-T.
             03  WJ-ZKIN      PIC S9(009).
             03  WJ-NKIN      PIC S9(009).
             03  WJ-YKIN      PIC S9(009).
           02  WS-T.
             03  WS-ZKIN      PIC S9(009).
             03  WS-NKIN      PIC S9(009).
             03  WS-YKIN      PIC S9(009).
           02  WA-T.
             03  WA-ZKIN      PIC S9(009).
             03  WA-NKIN      PIC S9(009).
             03  WA-YKIN      PIC S9(009).
       01  W-DATA.
           02  W-T.
             03  W-SGP        PIC  9(004).
             03  W-NSU        PIC S9(008)V9(02).
             03  W-NTN        PIC S9(007)V9(02).
             03  W-NKIN       PIC S9(009).
             03  W-SSU        PIC S9(008)V9(02).
           02  W-KRK.
             03  W-KSU        PIC S9(008)V9(02).
             03  W-KTN        PIC S9(007)V9(02).
             03  W-KKIN       PIC S9(009).
           02  W-BKC          PIC  9(002).
           02  W-YC           PIC  9(001).
           02  W-JCD          PIC  9(006).
           02  W-JCDD  REDEFINES W-JCD.
             03  W-JCD1       PIC  9(001).
             03  W-JCD2       PIC  9(002).
             03  W-JCD3       PIC  9(003).
           02  W-JCDD2        PIC  9(002).
           02  W-JCDW         PIC  9(006).
           02  W-GP           PIC  9(004).
           02  CNT            PIC  9(003).
           02  CHK            PIC  9(001).
           02  W-ZSU          PIC S9(007)V9(02).
           02  W-ZTN          PIC S9(007)V9(02).
           02  W-PAGE         PIC  9(003).
           02  W-SJCD         PIC  9(006).
           02  W-EJCD         PIC  9(006) VALUE 999999.
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIJM.
           COPY LIJTM.
           COPY LSPF.
      *FD  JUH-F
       01  JUH-F_KBG460.
           02  JUH-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  JUH-F_LNAME    PIC  X(012) VALUE "JUH-F_KBG460".
           02  F              PIC  X(001).
           02  JUH-F_KEY1     PIC  X(100) VALUE SPACE.
           02  JUH-F_SORT     PIC  X(100) VALUE SPACE.
           02  JUH-F_IDLST    PIC  X(100) VALUE SPACE.
           02  JUH-F_RES      USAGE  POINTER.
       01  JUH-R.
           02  JUH-JCD        PIC  9(006).
           02  JUH-JCDD  REDEFINES JUH-JCD.
             03  JUH-JCD1     PIC  9(001).
             03  JUH-JCD2     PIC  9(002).
             03  JUH-JCD3     PIC  9(003).
           02  JUH-NGP.
             03  JUH-NEN      PIC  9(004).
             03  JUH-GP       PIC  9(004).
           02  JUH-KNSC       PIC  9(001).
           02  JUH-SU         PIC S9(007)V9(02).
           02  JUH-TN         PIC S9(006)V9(02).
           02  JUH-KIN        PIC S9(008).
           02  JUH-SD.
             03  JUH-SNEN     PIC  9(002).
             03  JUH-SGP      PIC  9(004).
           02  JUH-DNO        PIC  9(007).
           02  JUH-YC         PIC  9(001).
           02  JUH-BKC        PIC  9(002).
           02  JUH-BKNO       PIC  9(002).
           02  F              PIC  X(006).
           02  F              PIC  X(024).
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
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　材料日付別　受払表　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(032) VALUE
                "材料ｺｰﾄﾞ 000000 より 999999 まで".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SJCD  PIC  9(006).
             03  A-EJCD  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-ERRM.
             03  FILLER  PIC  N(010) VALUE
                  "マスター残高　エラー".
             03  FILLER  PIC  9(006).
             03  FILLER  PIC --,---,--9.99 .
           02  D-ERRC.
             03  FILLER  PIC  X(020) VALUE "                    ".
             03  FILLER  PIC  X(006) VALUE "      ".
             03  FILLER  PIC  X(013) VALUE "             ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(015) VALUE
                  "***  JM ﾅｼ  ***".
             03  E-ME3   PIC  X(016) VALUE
                  "***  JTM ﾅｼ  ***".
             03  E-JCD   PIC  9(006).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "320" " " " " RETURNING RESU.
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
            "08C-MID" "X" "14" "13" "32" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "18" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "14" "0" "12" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SJCD" "9" "14" "22" "6" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SJCD" BY REFERENCE W-SJCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EJCD" "9" "14" "34" "6" "A-SJCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EJCD" BY REFERENCE W-EJCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "35" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "78" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ERRM" " " "17" "0" "39" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-ERRM" "N" "17" "12" "20" " " "D-ERRM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-ERRM" "9" "17" "35" "6" "01D-ERRM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-ERRM" BY REFERENCE JT-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-ERRM" "--,---,--9.99" "17" "41" "13" "02D-ERRM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-ERRM" BY REFERENCE W-ZSU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ERRC" " " "17" "0" "39" "D-ERRM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-ERRC" "X" "17" "12" "20" " " "D-ERRC" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-ERRC" "X" "17" "35" "6" "01D-ERRC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-ERRC" "X" "17" "41" "13" "02D-ERRC" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "114" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "114" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "15" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "16" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JCD" "9" "24" "40" "6" "E-ME3" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JCD" BY REFERENCE W-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-JCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           PERFORM ACP-RTN THRU ACP-EX.
           IF  W-END NOT = 0
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO JUH-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JUH-F_PNAME1 " " BY REFERENCE JUH-F_IDLST "0".
       M-10.
      *           READ JUH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JUH-F_PNAME1 BY REFERENCE JUH-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JUH-F_IDLST JUH-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  JUH-JCD < W-SJCD
               GO TO M-10
           END-IF
           IF  JUH-JCD > W-EJCD
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JUH-F_IDLST JUH-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ZERO = JUH-SU AND JUH-KIN
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JT-M_PNAME1 "SHARED" BY REFERENCE JT-M_IDLST "1"
            "JT-KEY" BY REFERENCE JT-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-05R TO H-DATE.
           PERFORM MID-010 THRU MID-EX.
           MOVE ZERO TO W-TOTAL.
       M-15.
           MOVE ZERO TO WS-T.
           MOVE JUH-BKC TO W-BKC.
       M-20.
           MOVE ZERO TO WJ-T.
           MOVE JUH-YC TO W-YC.
       M-25.
           MOVE ZERO TO WB-T.
           MOVE JUH-JCD1 TO W-JCD1.
       M-30.
           MOVE ZERO TO WH-T.
           MOVE JUH-JCD2 TO W-JCDD2.
           PERFORM KMC-RTN THRU KMC-EX.
           MOVE W-JCDD2 TO W-JCD2.
       M-35.
           MOVE ZERO TO WN-T CHK W-KRK CNT.
           MOVE JUH-JCD3 TO W-JCD3.
           MOVE JUH-JCD TO W-JCDW.
           MOVE JUH-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD" E-JCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               MOVE SPACE TO J-NAME
               MOVE "　＊＊　材料マスター　なし　＊＊　" TO J-NAME
               MOVE ZERO TO J-ST
           END-IF
           MOVE JUH-JCD TO JT-KEY.
      *           READ JT-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JT-M_PNAME1 BY REFERENCE JT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD" E-JCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE SPACE TO W-P1.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME P-ZKM.
           MOVE JUH-JCD TO P-JCD.
           MOVE J-NAME TO P-NAME.
       M-40.
           MOVE ZERO TO W-T.
           MOVE JUH-GP TO W-GP.
       M-45.
           MOVE JUH-SGP TO W-SGP.
           IF  JUH-KNSC = 0
               MOVE JUH-SU TO W-KSU
               MOVE JUH-TN TO W-KTN
               MOVE JUH-KIN TO W-KKIN
           END-IF
           IF  JUH-KNSC = 1
               MOVE JUH-SU TO W-NSU
               MOVE JUH-TN TO W-NTN
               MOVE JUH-KIN TO W-NKIN
               ADD JUH-SU TO W-KSU
           END-IF
           IF  JUH-KNSC = 2
               ADD JUH-SU TO W-SSU
               SUBTRACT JUH-SU FROM W-KSU
           END-IF.
       M-50.
      *           READ JUH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JUH-F_PNAME1 BY REFERENCE JUH-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JUH-JCD > W-EJCD
               GO TO M-90
           END-IF
           IF  ZERO = JUH-SU AND JUH-KIN
               GO TO M-50
           END-IF
           IF  W-BKC NOT = JUH-BKC
               GO TO M-80
           END-IF
           IF  W-YC NOT = JUH-YC
               GO TO M-75
           END-IF
           IF  W-JCD1 NOT = JUH-JCD1
               GO TO M-70
           END-IF
           MOVE JUH-JCD2 TO W-JCDD2.
           PERFORM KMC-RTN THRU KMC-EX.
           IF  W-JCD2 NOT = W-JCDD2
               GO TO M-65
           END-IF
           IF  W-JCD3 NOT = JUH-JCD3
               GO TO M-60
           END-IF
           IF  W-JCDW NOT = JUH-JCD
               GO TO M-60
           END-IF
           IF  W-GP NOT = JUH-GP
               GO TO M-55
           END-IF
           IF  JUH-KNSC = 1
               IF (W-NSU NOT = ZERO) OR (W-NKIN NOT = ZERO)
                   GO TO M-55
               END-IF
           END-IF
           GO TO M-45.
       M-55.
           PERFORM MEI-RTN THRU MEI-EX.
           GO TO M-40.
       M-60.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM SP1-RTN THRU SP1-EX.
           GO TO M-35.
       M-65.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM SP1-RTN THRU SP1-EX.
           PERFORM SP2-RTN THRU SP2-EX.
           PERFORM MID-RTN THRU MID-EX.
           GO TO M-30.
       M-70.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM SP1-RTN THRU SP1-EX.
           PERFORM SP2-RTN THRU SP2-EX.
           PERFORM SP3-RTN THRU SP3-EX.
           PERFORM MID-RTN THRU MID-EX.
           GO TO M-25.
       M-75.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM SP1-RTN THRU SP1-EX.
           PERFORM SP2-RTN THRU SP2-EX.
           PERFORM SP3-RTN THRU SP3-EX.
           PERFORM SP4-RTN THRU SP4-EX.
           PERFORM MID-RTN THRU MID-EX.
           GO TO M-20.
       M-80.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM SP1-RTN THRU SP1-EX.
           PERFORM SP2-RTN THRU SP2-EX.
           PERFORM SP3-RTN THRU SP3-EX.
           PERFORM SP4-RTN THRU SP4-EX.
           PERFORM SP5-RTN THRU SP5-EX.
           PERFORM MID-RTN THRU MID-EX.
           GO TO M-15.
       M-90.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM SP1-RTN THRU SP1-EX.
           PERFORM SP2-RTN THRU SP2-EX.
           PERFORM SP3-RTN THRU SP3-EX.
           PERFORM SP4-RTN THRU SP4-EX.
           PERFORM SP5-RTN THRU SP5-EX.
           MOVE SPACE TO W-P1.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME P-ZKM.
           MOVE "【　　総　　合　　計　・　残　】" TO P-NAME.
           MOVE "－　前月繰越　－" TO P-ZKM.
           MOVE WA-ZKIN TO P-ZKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-P2.
           MOVE ":" TO P-C2 P-C3 P-C4.
           MOVE WA-NKIN TO P-NKIN.
           MOVE WA-YKIN TO P-YKIN.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE JUH-F_IDLST JUH-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JT-M_IDLST JT-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-SJCD "A-SJCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               MOVE 1 TO W-END
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-RTN
           END-IF.
       ACP-010.
           CALL "SD_Accept" USING BY REFERENCE A-EJCD "A-EJCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-010
           END-IF
           IF  W-SJCD > W-EJCD
               GO TO ACP-010
           END-IF.
       ACP-020.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-010
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-020
           END-IF
           IF  W-DMM = 9
               MOVE 1 TO W-END
               GO TO ACP-EX
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-020
           END-IF.
       ACP-EX.
           EXIT.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-010.
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
       MID-EX.
           EXIT.
       MEI-RTN.
           IF  CHK NOT = 0
               GO TO MEI-010
           END-IF
           MOVE 5 TO CHK.
           IF  W-GP = ZERO
               MOVE "－　前月繰越　－" TO P-ZKM
               MOVE W-KSU TO P-ZSU
               MOVE W-KTN TO P-ZTN
               MOVE W-KKIN TO P-ZKIN
               ADD W-KKIN TO WH-ZKIN
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-P1.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME P-ZKM.
           MOVE W-JCD TO P-JCD.
           MOVE J-NAME TO P-NAME.
           IF  W-GP = ZERO
               GO TO MEI-EX
           END-IF.
       MEI-010.
           MOVE SPACE TO W-P2.
           MOVE ":" TO P-C2 P-C3 P-C4.
           MOVE W-GP TO P-GP.
           IF  W-SGP NOT = ZERO
               MOVE W-SGP TO P-SGP
           END-IF
           IF  W-NSU NOT = ZERO
               MOVE W-NSU TO P-NSU
           END-IF
           IF  W-NTN NOT = ZERO
               MOVE W-NTN TO P-NTN
           END-IF
           IF  W-NKIN NOT = ZERO
               MOVE W-NKIN TO P-NKIN
           END-IF
           IF  W-SSU NOT = ZERO
               MOVE W-SSU TO P-SSU
           END-IF
           IF  W-KSU NOT = ZERO
               MOVE W-KSU TO P-YSU
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 62
               GO TO MEI-020
           END-IF
           PERFORM MID-RTN THRU MID-EX.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MEI-020.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD W-NSU TO WN-NSU.
           ADD W-NKIN TO WN-NKIN.
           ADD W-SSU TO WN-SSU.
           ADD 1 TO CNT.
       MEI-EX.
           EXIT.
       SP1-RTN.
           COMPUTE W-KKIN = W-KSU * J-ST.
           ADD WN-NKIN TO WH-NKIN.
           ADD W-KKIN TO WH-YKIN.
           IF (CNT = ZERO) AND (W-KTN = J-ST)
               GO TO SP1-020
           END-IF
           MOVE SPACE TO W-P2.
           MOVE ":" TO P-C2 P-C3 P-C4.
           MOVE "　　　　　　　　＊　計・残　＊　" TO P-TM.
           MOVE WN-NSU TO P-NSU.
           MOVE WN-NKIN TO P-NKIN.
           MOVE WN-SSU TO P-SSU.
           MOVE W-KSU TO P-YSU.
           MOVE J-ST TO P-YTN.
           MOVE W-KKIN TO P-YKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 62
               GO TO SP1-010
           END-IF
           PERFORM MID-RTN THRU MID-EX.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       SP1-010.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       SP1-020.
           COMPUTE W-ZSU = JT-ZKS + JT-SSU - JT-HSU.
           IF  W-KSU NOT = W-ZSU
               CALL "SD_Output" USING
                "D-ERRM" D-ERRM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-ERRC" D-ERRC "p" RETURNING RESU
           END-IF.
       SP1-EX.
           EXIT.
       SP2-RTN.
           MOVE SPACE TO W-P1.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME P-ZKM.
           MOVE "　　　　　（　品目合計・残　）　" TO P-NAME.
           MOVE "－　前月繰越　－" TO P-ZKM.
           MOVE WH-ZKIN TO P-ZKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-P2.
           MOVE ":" TO P-C2 P-C3 P-C4.
           MOVE WH-NKIN TO P-NKIN.
           MOVE WH-YKIN TO P-YKIN.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WH-ZKIN TO WB-ZKIN.
           ADD WH-NKIN TO WB-NKIN.
           ADD WH-YKIN TO WB-YKIN.
       SP2-EX.
           EXIT.
       SP3-RTN.
           MOVE SPACE TO W-P1.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME P-ZKM.
           MOVE "　　　　＜　部門合計・残　＞　　" TO P-NAME.
           MOVE "－　前月繰越　－" TO P-ZKM.
           MOVE WB-ZKIN TO P-ZKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-P2.
           MOVE ":" TO P-C2 P-C3 P-C4.
           MOVE WB-NKIN TO P-NKIN.
           MOVE WB-YKIN TO P-YKIN.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WB-ZKIN TO WJ-ZKIN.
           ADD WB-NKIN TO WJ-NKIN.
           ADD WB-YKIN TO WJ-YKIN.
       SP3-EX.
           EXIT.
       SP4-RTN.
           MOVE SPACE TO W-P1.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME P-ZKM.
           MOVE "　　　［　材料合計・残　］　　　" TO P-NAME.
           MOVE "－　前月繰越　－" TO P-ZKM.
           MOVE WJ-ZKIN TO P-ZKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-P2.
           MOVE ":" TO P-C2 P-C3 P-C4.
           MOVE WJ-NKIN TO P-NKIN.
           MOVE WJ-YKIN TO P-YKIN.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WJ-ZKIN TO WS-ZKIN.
           ADD WJ-NKIN TO WS-NKIN.
           ADD WJ-YKIN TO WS-YKIN.
       SP4-EX.
           EXIT.
       SP5-RTN.
           MOVE SPACE TO W-P1.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME P-ZKM.
           MOVE "　｛　部門管理合計・残　｝　　　" TO P-NAME.
           MOVE "－　前月繰越　－" TO P-ZKM.
           MOVE WS-ZKIN TO P-ZKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-P2.
           MOVE ":" TO P-C2 P-C3 P-C4.
           MOVE WS-NKIN TO P-NKIN.
           MOVE WS-YKIN TO P-YKIN.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-ZKIN TO WA-ZKIN.
           ADD WS-NKIN TO WA-NKIN.
           ADD WS-YKIN TO WA-YKIN.
       SP5-EX.
           EXIT.
       KMC-RTN.
           IF  W-JCDD2 < 05
               MOVE 00 TO W-JCDD2
               GO TO KMC-EX
           END-IF
           IF  W-JCDD2 < 10
               MOVE 05 TO W-JCDD2
               GO TO KMC-EX
           END-IF
           IF  W-JCDD2 < 15
               MOVE 10 TO W-JCDD2
               GO TO KMC-EX
           END-IF
           IF  W-JCDD2 < 20
               MOVE 15 TO W-JCDD2
               GO TO KMC-EX
           END-IF
           IF  W-JCDD2 < 22
               MOVE 20 TO W-JCDD2
               GO TO KMC-EX
           END-IF
           IF  W-JCDD2 < 23
               MOVE 22 TO W-JCDD2
               GO TO KMC-EX
           END-IF
           IF  W-JCDD2 < 24
               MOVE 23 TO W-JCDD2
               GO TO KMC-EX
           END-IF
           IF  W-JCDD2 < 25
               MOVE 24 TO W-JCDD2
               GO TO KMC-EX
           END-IF
           IF  W-JCDD2 < 30
               MOVE 25 TO W-JCDD2
               GO TO KMC-EX
           END-IF
           IF  W-JCDD2 < 35
               MOVE 30 TO W-JCDD2
               GO TO KMC-EX
           END-IF
           IF  W-JCDD2 < 40
               MOVE 35 TO W-JCDD2
               GO TO KMC-EX
           END-IF
           IF  W-JCDD2 < 45
               MOVE 40 TO W-JCDD2
               GO TO KMC-EX
           END-IF
           IF  W-JCDD2 < 60
               MOVE 45 TO W-JCDD2
               GO TO KMC-EX
           END-IF
           IF  W-JCDD2 < 62
               MOVE 60 TO W-JCDD2
               GO TO KMC-EX
           END-IF
           IF  W-JCDD2 < 64
               MOVE 62 TO W-JCDD2
               GO TO KMC-EX
           END-IF
           IF  W-JCDD2 < 68
               MOVE 64 TO W-JCDD2
               GO TO KMC-EX
           END-IF
           IF  W-JCDD2 < 80
               MOVE 68 TO W-JCDD2
               GO TO KMC-EX
           END-IF.
       KMC-EX.
           EXIT.

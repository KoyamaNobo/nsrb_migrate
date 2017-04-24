       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG570.
      *********************************************************
      *    PROGRAM         :  材料在庫明細表                  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/10                        *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  在庫=0 , 棚卸=1                 *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0064".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0128".
           02  W-FID22        PIC  X(003).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(009) VALUE "＊＊＊　　材　料　".
           02  H-MID          PIC  N(003) VALUE "在　庫".
           02  F              PIC  N(011) VALUE
                "　明　細　表　　＊＊＊".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  N(009) VALUE "コード　材　料　名".
           02  F              PIC  X(045) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "単　　価".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(005) VALUE "最終仕入日".
       01  W-P.
           02  P-KEY          PIC  9(006).
           02  F              PIC  X(002).
           02  P-NA           PIC  N(024).
           02  P-NAD   REDEFINES P-NA.
             03  P-NA1        PIC  N(013).
             03  P-NA2        PIC  N(011).
           02  P-ZAI          PIC ----,---,--9.99.
           02  P-T            PIC --,---,--9.99.
           02  P-ZAK          PIC -----,---,--9.
           02  F              PIC  X(003).
           02  P-ED           PIC 99/99/99B.
       01  W-D.
           02  W-SEKEY.
             03  W-SKEY       PIC  9(006).
             03  W-EKEY       PIC  9(006) VALUE 999999.
           02  W-BKC          PIC  9(002).
           02  W-KEY          PIC  9(006).
           02  W-DMM          PIC  9(001).
           02  W-ZSU          PIC S9(007)V9(02).
           02  W-ZAK          PIC S9(008).
           02  WS-ZAK         PIC S9(009).
           02  WG-ZAK         PIC S9(009).
           02  WA-ZAK         PIC S9(009).
           02  W-PC           PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-JCD1.
             03  W-JCD11      PIC  9(001).
             03  W-JCD12      PIC  9(002).
           02  W-JCDD12       PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIJM.
           COPY LSJTM.
           COPY BUMONF.
           COPY LSPF.
      *FD  JTIF
       01  JTIF_KBG570.
           02  JTIF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  JTIF_LNAME     PIC  X(011) VALUE "JTIF_KBG570".
           02  F              PIC  X(001).
           02  JTIF_KEY1      PIC  X(100) VALUE SPACE.
           02  JTIF_SORT      PIC  X(100) VALUE SPACE.
           02  JTIF_IDLST     PIC  X(100) VALUE SPACE.
           02  JTIF_RES       USAGE  POINTER.
       01  JTI-R.
           02  JTI-KEY.
             03  JTI-JCD1     PIC  9(003).
             03  JTI-JCDD1 REDEFINES JTI-JCD1.
               04  JTI-JCD11  PIC  9(001).
               04  JTI-JCD12  PIC  9(002).
             03  JTI-JCD2     PIC  9(003).
           02  JTI-BSC        PIC  9(002).
           02  JTI-TSU        PIC S9(007)V9(02).
           02  JTI-BKNO       PIC  9(002).
           02  F              PIC  X(001).
           02  JTI-PC         PIC  9(001).
           02  F              PIC  X(043).
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
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　材料在庫明細表　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(027) VALUE
                "[  ｺｰﾄﾞ 000000 ～ 999999  ]".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SKEY  PIC  9(006).
             03  A-EKEY  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-TM    PIC  N(002) VALUE
                "棚卸".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(015) VALUE
                  "***  JM ﾅｼ  ***".
             03  E-ME2   PIC  X(022) VALUE
                  "***  ｻﾞｲｺｸﾌﾞﾝ ｴﾗｰ  ***".
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
            "C-MID" " " "0" "0" "301" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING 
            "08C-MID" "X" "14" "14" "27" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "17" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "14" "0" "12" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SKEY" "9" "14" "22" "6" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SKEY" BY REFERENCE W-SKEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EKEY" "9" "14" "32" "6" "A-SKEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EKEY" BY REFERENCE W-EKEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "34" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "4" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TM" "N" "6" "24" "4" " " "C-DSP" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "105" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "105" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "22" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JCD" "9" "24" "40" "6" "E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JCD" BY REFERENCE J-KEY "6" "0" RETURNING RESU.
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
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               GO TO M-05
           END-IF
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "D-TM" D-TM "p" RETURNING RESU
               MOVE "棚　卸" TO H-MID
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SKEY "A-SKEY" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-EKEY "A-EKEY" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-SKEY > W-EKEY
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
      *
           MOVE ZERO TO W-PC.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           IF  JS-SIGN = 1
               GO TO M-50
           END-IF
           MOVE STN-NO2 TO W-FID22.
           MOVE W-FID2 TO WK0128ID.
           MOVE WK0128ID TO JT-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-F_PNAME1 " " BY REFERENCE JT-F_IDLST "0".
       M-25.
      *           READ JT-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-F_PNAME1 BY REFERENCE JT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  JT-ZC = 1
               GO TO M-25
           END-IF
           COMPUTE W-ZSU = JT-ZKS + JT-SSU - JT-HSU.
           IF  W-ZSU = ZERO
               GO TO M-25
           END-IF
           IF  JT-KEY < W-SKEY
               GO TO M-25
           END-IF
           IF  JT-KEY > W-EKEY
               GO TO M-25
           END-IF
           PERFORM S-20 THRU S-25.
       M-27.
           MOVE JT-BKC TO W-BKC.
           MOVE ZERO TO WG-ZAK.
       M-30.
           MOVE JT-KEYD TO W-JCD1.
           MOVE W-JCD12 TO W-JCDD12.
           PERFORM S-75 THRU S-80.
           MOVE W-JCDD12 TO W-JCD12.
           MOVE ZERO TO WS-ZAK.
       M-35.
           PERFORM S-30 THRU S-35.
       M-40.
      *           READ JT-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-F_PNAME1 BY REFERENCE JT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JT-ZC = 1
               GO TO M-40
           END-IF
           COMPUTE W-ZSU = JT-ZKS + JT-SSU - JT-HSU.
           IF  W-ZSU = ZERO
               GO TO M-40
           END-IF
           IF  JT-KEY > W-EKEY
               GO TO M-90
           END-IF
           IF  JT-BKC NOT = W-BKC
               GO TO M-45
           END-IF
           MOVE JT-RC TO W-JCDD12.
           PERFORM S-75 THRU S-80.
           IF  JT-BC = W-JCD11
               IF  W-JCDD12 = W-JCD12
                   GO TO M-35
               END-IF
           END-IF
           PERFORM S-40 THRU S-45.
           GO TO M-30.
       M-45.
           PERFORM S-40 THRU S-45.
           PERFORM S-50 THRU S-60.
           GO TO M-27.
      *-----------------------------------------------------------------
       M-50.
           MOVE STN-NO2 TO W-FID12.
           MOVE W-FID1 TO WK0064ID.
           MOVE WK0064ID TO JTIF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JTIF_PNAME1 " " BY REFERENCE JTIF_IDLST "0".
       M-55.
      *           READ JTIF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JTIF_PNAME1 BY REFERENCE JTI-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  JTI-KEY < W-SKEY OR > W-EKEY
               GO TO M-55
           END-IF
           PERFORM S-20 THRU S-25.
       M-57.
           MOVE JTI-KEY TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO J-BKC
           END-IF
           MOVE J-BKC TO W-BKC.
           MOVE ZERO TO WG-ZAK.
       M-60.
           MOVE JTI-JCD1 TO W-JCD1.
           MOVE W-JCD12 TO W-JCDD12.
           PERFORM S-75 THRU S-80.
           MOVE W-JCDD12 TO W-JCD12.
           MOVE ZERO TO WS-ZAK.
       M-65.
           MOVE JTI-KEY TO W-KEY.
           MOVE ZERO TO W-ZSU.
       M-70.
           ADD JTI-TSU TO W-ZSU.
       M-72.
      *           READ JTIF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JTIF_PNAME1 BY REFERENCE JTI-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JTI-KEY < W-SKEY OR > W-EKEY
               GO TO M-72
           END-IF
           MOVE JTI-KEY TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO J-BKC
           END-IF
           IF  J-BKC NOT = W-BKC
               GO TO M-80
           END-IF
           IF  JTI-KEY = W-KEY
               GO TO M-70
           END-IF
           MOVE JTI-JCD12 TO W-JCDD12.
           PERFORM S-75 THRU S-80.
           IF (JTI-JCD11 NOT = W-JCD11) OR (W-JCDD12 NOT = W-JCD12)
               GO TO M-75
           END-IF
           PERFORM S-30 THRU S-35.
           GO TO M-65.
       M-75.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
           GO TO M-60.
       M-80.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
           PERFORM S-50 THRU S-60.
           GO TO M-57.
      *-----------------------------------------------------------------
       M-90.
           IF  JS-SIGN = 1
               PERFORM S-30 THRU S-35
           END-IF
           PERFORM S-40 THRU S-45.
           PERFORM S-50 THRU S-60.
           PERFORM S-65 THRU S-70.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           IF  JS-SIGN = 0
               CALL "DB_F_Close" USING
                BY REFERENCE JT-F_IDLST JT-F_PNAME1
           ELSE
               CALL "DB_F_Close" USING
                BY REFERENCE JTIF_IDLST JTIF_PNAME1
           END-IF
           IF  W-PC NOT = ZERO
               CALL "PR_Close" RETURNING RESP
               CALL "DB_F_Close" USING
                BY REFERENCE BNM_IDLST BNM_PNAME1
           END-IF
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
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BNM_PNAME1 "SHARED" BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE 5 TO W-PC.
           MOVE ZERO TO W-PAGE WA-ZAK.
           MOVE DATE-05R TO H-DATE.
           PERFORM S-10 THRU S-15.
       S-25.
           EXIT.
       S-30.
           IF  JS-SIGN = 0
               MOVE JT-KEY TO J-KEY
           ELSE
               MOVE W-KEY TO J-KEY
           END-IF
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD" E-JCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               MOVE "　＊＊　ＪＭ　なし　＊＊　" TO J-NAME
               MOVE ZERO TO J-ZC J-ST J-ED
           END-IF
           IF  JS-SIGN = 1
               IF  J-ZC = 1
                   CALL "SD_Output" USING
                    "E-ME2" E-ME2 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-JCD" E-JCD "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
                   GO TO S-35
               END-IF
           END-IF
           COMPUTE W-ZAK = W-ZSU * J-ST.
      *
           MOVE SPACE TO W-P.
           IF  JS-SIGN = 0
               MOVE JT-KEY TO P-KEY
           ELSE
               MOVE W-KEY TO P-KEY
           END-IF
           MOVE J-NAME TO P-NA.
           MOVE W-ZSU TO P-ZAI.
           MOVE J-ST TO P-T.
           MOVE W-ZAK TO P-ZAK.
           IF  J-ED NOT = ZERO
               MOVE J-ED TO P-ED
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD W-ZAK TO WS-ZAK.
       S-35.
           EXIT.
       S-40.
           IF  WS-ZAK = ZERO
               GO TO S-45
           END-IF
           MOVE SPACE TO W-P.
           MOVE "　　　　　　　　　　　　（　　小　計　　）" TO P-NA.
           MOVE WS-ZAK TO P-ZAK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-ZAK TO WG-ZAK.
       S-45.
           EXIT.
       S-50.
           IF  WG-ZAK = ZERO
               GO TO S-55
           END-IF
           MOVE ZERO TO BNM-KEY.
           MOVE W-BKC TO BNM-BU.
      *           READ BNM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BNM_PNAME1 BY REFERENCE BNM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO BNMNMN
           END-IF
           MOVE SPACE TO W-P.
           MOVE "　　　［　　合　計　　］　" TO P-NA1.
           MOVE BNMNMN TO P-NA2.
           MOVE WG-ZAK TO P-ZAK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-55.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WG-ZAK TO WA-ZAK.
       S-60.
           EXIT.
       S-65.
           MOVE SPACE TO W-P.
           MOVE "　【　　総　合　計　　】　　　　　　　　　" TO P-NA.
           MOVE WA-ZAK TO P-ZAK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-70.
           EXIT.
       S-75.
           IF  W-JCDD12 < 05
               MOVE 00 TO W-JCDD12
               GO TO S-80
           END-IF
           IF  W-JCDD12 < 10
               MOVE 05 TO W-JCDD12
               GO TO S-80
           END-IF
           IF  W-JCDD12 < 15
               MOVE 10 TO W-JCDD12
               GO TO S-80
           END-IF
           IF  W-JCDD12 < 20
               MOVE 15 TO W-JCDD12
               GO TO S-80
           END-IF
           IF  W-JCDD12 < 22
               MOVE 20 TO W-JCDD12
               GO TO S-80
           END-IF
           IF  W-JCDD12 < 23
               MOVE 22 TO W-JCDD12
               GO TO S-80
           END-IF
           IF  W-JCDD12 < 24
               MOVE 23 TO W-JCDD12
               GO TO S-80
           END-IF
           IF  W-JCDD12 < 25
               MOVE 24 TO W-JCDD12
               GO TO S-80
           END-IF
           IF  W-JCDD12 < 30
               MOVE 25 TO W-JCDD12
               GO TO S-80
           END-IF
           IF  W-JCDD12 < 35
               MOVE 30 TO W-JCDD12
               GO TO S-80
           END-IF
           IF  W-JCDD12 < 40
               MOVE 35 TO W-JCDD12
               GO TO S-80
           END-IF
           IF  W-JCDD12 < 45
               MOVE 40 TO W-JCDD12
               GO TO S-80
           END-IF
           IF  W-JCDD12 < 60
               MOVE 45 TO W-JCDD12
               GO TO S-80
           END-IF
           IF  W-JCDD12 < 62
               MOVE 60 TO W-JCDD12
               GO TO S-80
           END-IF
           IF  W-JCDD12 < 64
               MOVE 62 TO W-JCDD12
               GO TO S-80
           END-IF
           IF  W-JCDD12 < 68
               MOVE 64 TO W-JCDD12
               GO TO S-80
           END-IF
           IF  W-JCDD12 < 80
               MOVE 68 TO W-JCDD12
           END-IF.
       S-80.
           EXIT.

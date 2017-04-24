       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG550.
      *********************************************************
      *    PROGRAM         :  材　料　受　払　表              *
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
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  N(019) VALUE
                "＊＊＊　　材　料　受　払　表　　＊＊＊".
           02  F              PIC  X(034) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  F              PIC  X(044) VALUE SPACE.
           02  F              PIC  X(039) VALUE
                "I------------  入　　庫  ------------I ".
           02  F              PIC  X(053) VALUE
                "I-- 出 庫 --I I------------  在　　庫  ------------I ".
       01  HEAD3.
           02  F              PIC  N(009) VALUE "コード　材　料　名".
           02  F              PIC  X(033) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(016) VALUE
                "単　価　　　金　額　　　　数　量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(009) VALUE "数　量　　　単　価".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
       01  W-P.
           02  P-15K          PIC  X(005).
           02  P-JCD          PIC  9(006).
           02  F              PIC  X(002).
           02  P-JNA          PIC  N(024).
           02  P-20K          PIC  X(005).
           02  P-NS           PIC --,---,--9.99.
           02  P-NT           PIC -----,--9.999.
           02  P-W1.
             03  P-NK         PIC ----,---,--9.
             03  P-SS         PIC ---,---,--9.99.
           02  P-ZM    REDEFINES P-W1  PIC  N(013).
           02  P-ZS           PIC ---,---,--9.99.
           02  P-ZT           PIC -----,--9.99.
           02  P-ZK           PIC -----,---,--9.
           02  P-X            PIC  X(001).
       01  W-TOTAL.
           02  W-HT.
             03  W-HZK        PIC S9(009).
             03  W-HNK        PIC S9(009).
             03  W-HYK        PIC S9(009).
           02  W-BT.
             03  W-BZK        PIC S9(009).
             03  W-BNK        PIC S9(009).
             03  W-BYK        PIC S9(009).
           02  W-JT.
             03  W-JZK        PIC S9(009).
             03  W-JNK        PIC S9(009).
             03  W-JYK        PIC S9(009).
           02  W-ST.
             03  W-SZK        PIC S9(009).
             03  W-SNK        PIC S9(009).
             03  W-SYK        PIC S9(009).
           02  W-AT.
             03  W-AZK        PIC S9(009).
             03  W-ANK        PIC S9(009).
             03  W-AYK        PIC S9(009).
       01  W-D.
           02  W-BKC          PIC  9(002).
           02  W-YC           PIC  9(001).
           02  W-DC           PIC  9(002).
           02  W-DCD          PIC  9(002).
           02  W-CO.
             03  W-CO1        PIC  9(001).
             03  W-CO2        PIC  9(002).
           02  W-PAGE         PIC  9(003).
           02  W-ZT           PIC S9(007)V9(02).
           02  W-ZK           PIC S9(009).
           02  W-ZAI          PIC S9(007)V9(02).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIJM.
           COPY LSPF.
      *FD  JT-F
       01  JT-F_KBG550.
           02  JT-F_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  JT-F_LNAME     PIC  X(011) VALUE "JT-F_KBG550".
           02  F              PIC  X(001).
           02  JT-F_KEY1      PIC  X(100) VALUE SPACE.
           02  JT-F_SORT      PIC  X(100) VALUE SPACE.
           02  JT-F_IDLST     PIC  X(100) VALUE SPACE.
           02  JT-F_RES       USAGE  POINTER.
       01  JT-R.
           02  JT-JCD.
             03  JT-JCD1.
               04  JT-JCD11   PIC  9(001).
               04  JT-JCD12   PIC  9(002).
             03  JT-JCD2      PIC  9(003).
           02  JT-TSU         PIC S9(007)V9(02).
           02  JT-NS          PIC S9(007)V9(02).
           02  JT-NK          PIC S9(008).
           02  JT-SS          PIC S9(007)V9(02).
           02  JT-ZS          PIC S9(007)V9(02).
           02  JT-ZK          PIC S9(008).
           02  JT-YC          PIC  9(001).
           02  JT-ZC          PIC  9(001).
           02  JT-SC          PIC  9(001).
           02  F              PIC  X(009).
           02  JT-BKC         PIC  9(002).
           02  JT-BKNO        PIC  9(002).
           02  F              PIC  X(054).
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
                "＊＊＊　　材　料　受　払　表　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(015) VALUE
                  "***  JM ﾅｼ  ***".
             03  E-JCD   PIC  9(006).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "266" " " " " RETURNING RESU.
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
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "48" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "48" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "15" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JCD" "9" "24" "40" "6" "E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JCD" BY REFERENCE JT-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-JCD" " " RETURNING RESU.
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
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JT-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-F_PNAME1 " " BY REFERENCE JT-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "PR_Open" RETURNING RESP.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
      *           READ JT-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-F_PNAME1 BY REFERENCE JT-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JT-ZC = 1
               GO TO M-10
           END-IF
           IF  JT-JCD11 = 1
               GO TO M-10
           END-IF
           IF  ZERO = JT-NS AND JT-NK AND JT-SS AND JT-ZS AND JT-ZK
               GO TO M-10
           END-IF
           MOVE DATE-05R TO H-DATE.
           MOVE ZERO TO W-D.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO W-TOTAL.
       M-15.
           MOVE ZERO TO W-ST.
           MOVE JT-BKC TO W-BKC.
       M-20.
           MOVE ZERO TO W-JT.
           MOVE JT-YC TO W-YC.
       M-25.
           MOVE ZERO TO W-BT.
           MOVE JT-JCD11 TO W-CO1.
       M-30.
           MOVE ZERO TO W-HT.
           MOVE JT-JCD12 TO W-DCD.
           PERFORM S-85 THRU S-90.
           MOVE W-DCD TO W-DC.
       M-35.
           PERFORM S-20 THRU S-40.
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
           IF  JT-JCD11 = 1
               GO TO M-40
           END-IF
           IF  ZERO = JT-NS AND JT-NK AND JT-SS AND JT-ZS AND JT-ZK
               GO TO M-40
           END-IF
           IF  W-BKC NOT = JT-BKC
               GO TO M-60
           END-IF
           IF  W-YC NOT = JT-YC
               GO TO M-55
           END-IF
           IF  W-CO1 NOT = JT-JCD11
               GO TO M-50
           END-IF
           MOVE JT-JCD12 TO W-DCD.
           PERFORM S-85 THRU S-90.
           IF  W-DCD = W-DC
               GO TO M-35
           END-IF.
       M-45.
           PERFORM S-45 THRU S-50.
           GO TO M-30.
       M-50.
           PERFORM S-45 THRU S-50.
           PERFORM S-55 THRU S-60.
           GO TO M-25.
       M-55.
           PERFORM S-45 THRU S-50.
           PERFORM S-55 THRU S-60.
           PERFORM S-65 THRU S-70.
           GO TO M-20.
       M-60.
           PERFORM S-45 THRU S-50.
           PERFORM S-55 THRU S-60.
           PERFORM S-65 THRU S-70.
           PERFORM S-75 THRU S-80.
           GO TO M-15.
       M-90.
           PERFORM S-45 THRU S-50.
           PERFORM S-55 THRU S-60.
           PERFORM S-65 THRU S-70.
           PERFORM S-75 THRU S-80.
           IF  ZERO = W-AZK AND W-ANK AND W-AYK
               GO TO M-95
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE "　【　　総　　合　　計　　】　　　" TO P-JNA.
           MOVE W-20K TO P-20K.
           IF  W-AZK NOT = ZERO
               MOVE W-AZK TO P-ZK
               MOVE "　　　　　＊　繰　越　＊　" TO P-ZM
               MOVE "*" TO P-X
               MOVE W-P TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO W-P
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-15K TO P-15K.
           MOVE ALL "　" TO P-JNA.
           MOVE W-20K TO P-20K.
           MOVE W-ANK TO P-NK.
           MOVE W-AYK TO P-ZK.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE JT-F_IDLST JT-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
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
           MOVE JT-JCD TO J-KEY.
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
               MOVE "　　＊＊　ＪＭ　なし　＊＊　" TO J-NAME
               MOVE ZERO TO J-ST
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R W-P.
           MOVE W-15K TO P-15K.
           MOVE JT-JCD TO P-JCD.
           MOVE J-NAME TO P-JNA.
           MOVE W-20K TO P-20K.
           MOVE ZERO TO W-ZT.
           IF  ZERO = JT-ZS AND JT-ZK
               GO TO S-25
           END-IF
           IF  JT-ZS NOT = ZERO
               COMPUTE W-ZT = JT-ZK / JT-ZS + 0.009
           END-IF
           MOVE "　　　　　＊　繰　越　＊　" TO P-ZM.
           MOVE JT-ZS TO P-ZS.
           MOVE JT-ZK TO P-ZK.
           MOVE W-ZT TO P-ZT.
           MOVE "*" TO P-X.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE ALL "　" TO P-JNA.
           MOVE W-20K TO P-20K.
       S-25.
           COMPUTE W-ZAI = JT-ZS + JT-NS - JT-SS.
           IF  JT-ZS NOT = W-ZAI
               GO TO S-30
           END-IF
           IF  ZERO NOT = JT-NS OR JT-NK OR JT-SS
               GO TO S-30
           END-IF
           MOVE ZERO TO W-ZK.
           IF  W-ZAI NOT = ZERO
               IF  J-ST NOT = ZERO
                   COMPUTE W-ZK = (JT-ZS + JT-NS - JT-SS) * J-ST
               END-IF
           END-IF
           GO TO S-35.
       S-30.
           MOVE ZERO TO W-ZT.
           IF  JT-NS NOT = ZERO
               IF  JT-NK NOT = ZERO
                   COMPUTE W-ZT ROUNDED = JT-NK / JT-NS
               END-IF
           END-IF
           MOVE SPACE TO SP-R.
           MOVE JT-NS TO P-NS.
           MOVE W-ZT TO P-NT.
           MOVE JT-NK TO P-NK.
           MOVE JT-SS TO P-SS.
           MOVE W-ZAI TO P-ZS.
           MOVE J-ST TO P-ZT.
           MOVE ZERO TO W-ZK.
           IF  W-ZAI NOT = ZERO
               IF  J-ST NOT = ZERO
                   COMPUTE W-ZK = (JT-ZS + JT-NS - JT-SS) * J-ST
               END-IF
           END-IF
           MOVE W-ZK TO P-ZK.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-35.
           MOVE SPACE TO SP-R W-P.
           ADD JT-ZK TO W-HZK.
           ADD JT-NK TO W-HNK.
           ADD W-ZK TO W-HYK.
       S-40.
           EXIT.
       S-45.
           IF  ZERO = W-HZK AND W-HNK AND W-HYK
               GO TO S-50
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R W-P.
           MOVE W-15K TO P-15K.
           MOVE "　　　　　　（　品目合計　）　" TO P-JNA.
           MOVE W-20K TO P-20K.
           MOVE W-HZK TO P-ZK.
           MOVE "　　　　　＊　繰　越　＊　" TO P-ZM.
           MOVE "*" TO P-X.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO W-P.
           ADD W-HZK TO W-BZK.
           MOVE SPACE TO SP-R.
           MOVE W-15K TO P-15K.
           MOVE ALL "　" TO P-JNA.
           MOVE W-20K TO P-20K.
           MOVE W-HNK TO P-NK.
           MOVE W-HYK TO P-ZK.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD W-HNK TO W-BNK.
           ADD W-HYK TO W-BYK.
       S-50.
           EXIT.
       S-55.
           IF  ZERO = W-BZK AND W-BNK AND W-BYK
               GO TO S-60
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE "　　　　　＜　部門合計　＞　　" TO P-JNA.
           MOVE W-20K TO P-20K.
           IF  W-BZK NOT = ZERO
               MOVE W-BZK TO P-ZK
               MOVE "　　　　　＊　繰　越　＊　" TO P-ZM
               MOVE "*" TO P-X
               MOVE W-P TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO W-P
               ADD W-BZK TO W-JZK
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-15K TO P-15K.
           MOVE ALL "　" TO P-JNA.
           MOVE W-20K TO P-20K.
           MOVE W-BNK TO P-NK.
           MOVE W-BYK TO P-ZK.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD W-BNK TO W-JNK.
           ADD W-BYK TO W-JYK.
       S-60.
           EXIT.
       S-65.
           IF  ZERO = W-JZK AND W-JNK AND W-JYK
               GO TO S-70
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE "　　　　［　材料合計　］　　　" TO P-JNA.
           MOVE W-20K TO P-20K.
           IF  W-JZK NOT = ZERO
               MOVE W-JZK TO P-ZK
               MOVE "　　　　　＊　繰　越　＊　" TO P-ZM
               MOVE "*" TO P-X
               MOVE W-P TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO W-P
               ADD W-JZK TO W-SZK
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-15K TO P-15K.
           MOVE ALL "　" TO P-JNA.
           MOVE W-20K TO P-20K.
           MOVE W-JNK TO P-NK.
           MOVE W-JYK TO P-ZK.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD W-JNK TO W-SNK.
           ADD W-JYK TO W-SYK.
       S-70.
           EXIT.
       S-75.
           IF  ZERO = W-SZK AND W-SNK AND W-SYK
               GO TO S-80
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE "　　　｛　部門管理合計　｝　　" TO P-JNA.
           MOVE W-20K TO P-20K.
           IF  W-SZK NOT = ZERO
               MOVE W-SZK TO P-ZK
               MOVE "　　　　　＊　繰　越　＊　" TO P-ZM
               MOVE "*" TO P-X
               MOVE W-P TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO W-P
               ADD W-SZK TO W-AZK
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-15K TO P-15K.
           MOVE ALL "　" TO P-JNA.
           MOVE W-20K TO P-20K.
           MOVE W-SNK TO P-NK.
           MOVE W-SYK TO P-ZK.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD W-SNK TO W-ANK.
           ADD W-SYK TO W-AYK.
       S-80.
           EXIT.
       S-85.
           IF  W-DCD < 05
               MOVE 00 TO W-DCD
               GO TO S-90
           END-IF
           IF  W-DCD < 10
               MOVE 05 TO W-DCD
               GO TO S-90
           END-IF
           IF  W-DCD < 15
               MOVE 10 TO W-DCD
               GO TO S-90
           END-IF
           IF  W-DCD < 20
               MOVE 15 TO W-DCD
               GO TO S-90
           END-IF
           IF  W-DCD < 22
               MOVE 20 TO W-DCD
               GO TO S-90
           END-IF
           IF  W-DCD < 23
               MOVE 22 TO W-DCD
               GO TO S-90
           END-IF
           IF  W-DCD < 24
               MOVE 23 TO W-DCD
               GO TO S-90
           END-IF
           IF  W-DCD < 25
               MOVE 24 TO W-DCD
               GO TO S-90
           END-IF
           IF  W-DCD < 30
               MOVE 25 TO W-DCD
               GO TO S-90
           END-IF
           IF  W-DCD < 35
               MOVE 30 TO W-DCD
               GO TO S-90
           END-IF
           IF  W-DCD < 40
               MOVE 35 TO W-DCD
               GO TO S-90
           END-IF
           IF  W-DCD < 45
               MOVE 40 TO W-DCD
               GO TO S-90
           END-IF
           IF  W-DCD < 60
               MOVE 45 TO W-DCD
               GO TO S-90
           END-IF
           IF  W-DCD < 62
               MOVE 60 TO W-DCD
               GO TO S-90
           END-IF
           IF  W-DCD < 64
               MOVE 62 TO W-DCD
               GO TO S-90
           END-IF
           IF  W-DCD < 68
               MOVE 64 TO W-DCD
               GO TO S-90
           END-IF
           IF  W-DCD < 80
               MOVE 68 TO W-DCD
           END-IF.
       S-90.
           EXIT.

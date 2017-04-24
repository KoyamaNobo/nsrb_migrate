       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG520.
      *********************************************************
      *    PROGRAM         :　材料品目区分別受払表　　　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/12                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(021) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　材料　製品品目区分別　受払表　　＊＊＊".
           02  F              PIC  X(014) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  N(005) VALUE "製　品　名".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  N(005) VALUE "品目区分名".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(004) VALUE "前月繰越".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "当月仕入".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "当月払出".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "翌月繰越".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE "増　減".
       01  W-P.
           02  P-SN           PIC  N(013).
           02  P-HN           PIC  X(015).
           02  P-HNR REDEFINES P-HN.
             03  P-HNN        PIC  N(007).
             03  P-HNX        PIC  X(001).
           02  P-ZK           PIC -----,---,--9.
           02  P-SK           PIC -----,---,--9.
           02  P-HK           PIC -----,---,--9.
           02  P-YK           PIC -----,---,--9.
           02  P-ZG           PIC ----,---,--9.
       01  W-D.
           02  W-YK           PIC S9(009).
           02  W-HK           PIC S9(009).
           02  W-ZG           PIC S9(009).
       01  W-TOTAL.
           02  W-HT.
             03  W-DC         PIC  9(002).
             03  WH-ZK        PIC S9(009).
             03  WH-SK        PIC S9(009).
             03  WH-HK        PIC S9(009).
             03  WH-YK        PIC S9(009).
             03  WH-ZG        PIC S9(009).
           02  W-ST.
             03  W-SC         PIC  9(001).
             03  WS-ZK        PIC S9(009).
             03  WS-SK        PIC S9(009).
             03  WS-HK        PIC S9(009).
             03  WS-YK        PIC S9(009).
             03  WS-ZG        PIC S9(009).
             03  CNT          PIC  9(002).
           02  W-AT.
             03  WA-ZK        PIC S9(009).
             03  WA-SK        PIC S9(009).
             03  WA-HK        PIC S9(009).
             03  WA-YK        PIC S9(009).
             03  WA-ZG        PIC S9(009).
       01  W-PAGE             PIC  9(002) VALUE ZERO.
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIJM.
           COPY LSPF.
      *FD  JT-F
       01  JT-F_KBG520.
           02  JT-F_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  JT-F_LNAME     PIC  X(011) VALUE "JT-F_KBG520".
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
           02  JT-SS          PIC S9(007)V9(02).
           02  JT-SK          PIC S9(008).
           02  JT-HS          PIC S9(007)V9(02).
           02  JT-ZS          PIC S9(007)V9(02).
           02  JT-ZK          PIC S9(008).
           02  JT-YC          PIC  9(001).
           02  JT-ZC          PIC  9(001).
           02  JT-SC          PIC  9(001).
           02  F              PIC  X(067).
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　材料　製品品目区分別　受払表　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
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
            "C-MID" " " "0" "0" "336" " " " " RETURNING RESU.
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
           IF  JT-YC = 4
               GO TO M-10
           END-IF
           IF  JT-SC = ZERO
               GO TO M-10
           END-IF
           IF  JT-JCD12 > 50
               GO TO M-10
           END-IF
           IF  JT-JCD11 NOT = 0 AND 1 AND 5 AND 6
               GO TO M-10
           END-IF
           IF  ZERO = JT-SS AND JT-SK AND JT-HS AND JT-ZS AND JT-ZK
               GO TO M-10
           END-IF.
       M-15.
           MOVE DATE-05R TO H-DATE.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO W-D W-TOTAL.
       M-20.
           MOVE ZERO TO W-ST.
           MOVE JT-SC TO W-SC.
       M-25.
           MOVE ZERO TO W-HT.
           MOVE JT-JCD12 TO W-DC.
       M-30.
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
               MOVE ZERO TO J-ST
           END-IF
           IF  JT-ZC = 1
               ADD JT-SK TO WH-SK WH-HK
               GO TO M-35
           END-IF
           COMPUTE W-YK = (JT-ZS  + JT-SS - JT-HS) * J-ST.
           COMPUTE W-HK = JT-ZK + JT-SK - W-YK.
           ADD JT-ZK TO WH-ZK.
           ADD JT-SK TO WH-SK.
           ADD W-HK TO WH-HK.
           ADD W-YK TO WH-YK.
       M-35.
      *           READ JT-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-F_PNAME1 BY REFERENCE JT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JT-YC = 4
               GO TO M-35
           END-IF
           IF  JT-SC = ZERO
               GO TO M-35
           END-IF
           IF  JT-JCD12 > 50
               GO TO M-35
           END-IF
           IF  JT-JCD11 NOT = 0 AND 1 AND 5 AND 6
               GO TO M-35
           END-IF
           IF  ZERO = JT-SS AND JT-SK AND JT-HS AND JT-ZS AND JT-ZK
               GO TO M-35
           END-IF
           IF  JT-SC NOT = W-SC
               GO TO M-50
           END-IF
           IF  JT-JCD12 = W-DC
               GO TO M-30
           END-IF.
       M-45.
           PERFORM S-20 THRU S-30.
           GO TO M-25.
       M-50.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-40.
           GO TO M-20.
       M-90.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-40.
           MOVE SPACE TO W-P.
           MOVE "　［　合　計　］" TO P-SN.
           MOVE WA-ZK TO P-ZK.
           MOVE WA-SK TO P-SK.
           MOVE WA-HK TO P-HK.
           MOVE WA-YK TO P-YK.
           COMPUTE WA-ZG = WA-YK - WA-ZK.
           MOVE WA-ZG TO P-ZG.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
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
       S-15.
           EXIT.
       S-20.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE ZERO TO CNT
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R W-P.
           IF  CNT NOT = ZERO
               GO TO S-25
           END-IF
           IF  W-SC = 1
               MOVE "１．　地下足袋　　　　　" TO P-SN
           END-IF
           IF  W-SC = 2
               MOVE "２．　教育シューズ　　　" TO P-SN
           END-IF
           IF  W-SC = 3
               MOVE "３．　Ｐ．Ｂスタンダード" TO P-SN
           END-IF
           IF  W-SC = 4
               MOVE "４．　　　　カジュアル　" TO P-SN
           END-IF
           IF  W-SC = 5
               MOVE "５．　自社スタンダード　" TO P-SN
           END-IF
           IF  W-SC = 6
               MOVE "６．　　　　　カジュアル" TO P-SN
           END-IF
           IF  W-SC = 7
               MOVE "７．　その他　　　　　　" TO P-SN
           END-IF.
       S-25.
           IF  W-DC = 00
               MOVE "原　　反　　　" TO P-HNN
           END-IF
           IF  W-DC = 05
               MOVE "型　　底　　　" TO P-HNN
           END-IF
           IF  W-DC = 10
               MOVE "クラリーノ　　" TO P-HNN
           END-IF
           IF  W-DC = 15
               MOVE "中　　底　　　" TO P-HNN
           END-IF
           IF  W-DC = 20
               MOVE "　　糸　　　　" TO P-HNN
           END-IF
           IF  W-DC = 22
               MOVE "　　紐　　　　" TO P-HNN
           END-IF
           IF  W-DC = 23
               MOVE "テ　ー　プ　　" TO P-HNN
           END-IF
           IF  W-DC = 24
               MOVE "胛　ゴ　ム　　" TO P-HNN
           END-IF
           IF  W-DC = 25
               MOVE "　　糊　　　　" TO P-HNN
           END-IF
           IF  W-DC = 30
               MOVE "馳・鳩目　　　" TO P-HNN
           END-IF
           IF  W-DC = 35
               MOVE "シール・ネーム" TO P-HNN
           END-IF
           IF  W-DC = 40
               MOVE "紙　　函　　　" TO P-HNN
           END-IF
           IF  W-DC = 45
               MOVE "ケ　ー　ス　　" TO P-HNN
           END-IF
           MOVE WH-ZK TO P-ZK.
           MOVE WH-SK TO P-SK.
           MOVE WH-HK TO P-HK.
           MOVE WH-YK TO P-YK.
           COMPUTE WH-ZG = WH-YK - WH-ZK.
           MOVE WH-ZG TO P-ZG.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD WH-ZK TO WS-ZK.
           ADD WH-SK TO WS-SK.
           ADD WH-HK TO WS-HK.
           ADD WH-YK TO WS-YK.
           ADD 1 TO CNT.
       S-30.
           EXIT.
       S-35.
           MOVE SPACE TO W-P.
           MOVE "　　　　　＜　小　計　＞" TO P-SN.
           MOVE WS-ZK TO P-ZK.
           MOVE WS-SK TO P-SK.
           MOVE WS-HK TO P-HK.
           MOVE WS-YK TO P-YK.
           COMPUTE WS-ZG = WS-YK - WS-ZK.
           MOVE WS-ZG TO P-ZG.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-ZK TO WA-ZK.
           ADD WS-SK TO WA-SK.
           ADD WS-HK TO WA-HK.
           ADD WS-YK TO WA-YK.
       S-40.
           EXIT.

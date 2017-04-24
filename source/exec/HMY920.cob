       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY920.
      **************************************************************
      *    PROGRAM         :  履物年間サイズ別受払表　ワークＦ作成 *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  ******                               *
      *    COMPILE TYPE    :  COBOL                                *
      **************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM7200.
       OBJECT-COMPUTER. SYSTEM7200.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERR-STAT           PIC  X(002).
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       77  WK0768ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0512".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0768".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  CNT            PIC  9(002).
           02  W-C            PIC  9(002).
           02  W-ZC           PIC  9(001).
       01  HHTYW_HMY920.
           02  HHTYW_PNAME1  PIC  X(009)  VALUE SPACE.
           02  F             PIC  X(001).
           02  HHTYW_LNAME   PIC  X(012)  VALUE "HHTYW_HMY920".
           02  F             PIC  X(001).
           02  HHTYW_KEY1    PIC  X(100)  VALUE SPACE.
           02  HHTYW_KEY2    PIC  X(100)  VALUE SPACE.
           02  HHTYW_SORT    PIC  X(100)  VALUE SPACE.
           02  HHTYW_IDLST   PIC  X(100)  VALUE SPACE.
           02  HHTYW_RES     USAGE  POINTER.
       01  HHTYW-R.
           02  HHTYW-KEY.
             03  HHTYW-HCD    PIC  9(006).
             03  HHTYW-SIZ    PIC  9(001).
           02  HHTYW-AZSU.                                                前月残
             03  HHTYW-ZSUD  OCCURS  10.
               04  HHTYW-ZSU  PIC S9(006).
           02  HHTYW-ANSU.                                                入庫数
             03  HHTYW-NSUD  OCCURS  10.
               04  HHTYW-NSU  PIC S9(006).
           02  HHTYW-AUSU.                                                出庫数
             03  HHTYW-USUD  OCCURS  10.
               04  HHTYW-USU  PIC S9(006).
           02  HHTYW-AYSU.                                                前月残
             03  HHTYW-YSUD  OCCURS  10.
               04  HHTYW-YSU  PIC S9(006).
           02  HHTYW-SNG      PIC  9(006).
           02  HHTYW-ENG      PIC  9(006).
           02  HHTYW-BC.
             03  HHTYW-BC1    PIC  9(002).
             03  HHTYW-BC2    PIC  9(002).
             03  HHTYW-BC3    PIC  9(002).
           02  HHTYW-BMNO     PIC  9(001).
           02  HHTYW-NG       PIC  9(006).
           02  F              PIC  X(240).
       77  F                  PIC  X(001).
       01  HHTY.
           02  HHTY_PNAME1  PIC  X(009)  VALUE SPACE.
           02  F            PIC  X(001).
           02  HHTY_LNAME   PIC  X(011)  VALUE "HHTY_HMY920".
           02  F            PIC  X(001).
           02  HHTY_KEY1    PIC  X(100)  VALUE SPACE.
           02  HHTY_KEY2    PIC  X(100)  VALUE SPACE.
           02  HHTY_SORT    PIC  X(100)  VALUE SPACE.
           02  HHTY_IDLST   PIC  X(100)  VALUE SPACE.
           02  HHTY_RES     USAGE  POINTER.
       01  HHTY-R.
           02  HHTY-KEY.
             03  HHTY-HCD     PIC  9(006).
             03  HHTY-SIZ     PIC  9(001).
           02  HHTY-AZSU.                                                 前月残
             03  HHTY-ZSUD  OCCURS  10.
               04  HHTY-ZSU   PIC S9(006)  COMP-3.
           02  HHTY-ANSU.                                                 入庫数
             03  HHTY-NSUD  OCCURS  10.
               04  HHTY-NSU   PIC S9(006)  COMP-3.
           02  HHTY-AUSU.                                                 出庫数
             03  HHTY-USUD  OCCURS  10.
               04  HHTY-USU   PIC S9(006)  COMP-3.
           02  HHTY-AYSU.                                                 前月残
             03  HHTY-YSUD  OCCURS  10.
               04  HHTY-YSU   PIC S9(006)  COMP-3.
           02  HHTY-SNG       PIC  9(006).
           02  HHTY-ENG       PIC  9(006).
           02  HHTY-BC.
             03  HHTY-BC1     PIC  9(002).
             03  HHTY-BC2     PIC  9(002).
             03  HHTY-BC3     PIC  9(002).
           02  HHTY-BMNO      PIC  9(001).
           02  F              PIC  X(070).
           02  F              PIC  X(512).
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
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　履物年間サイズ別受払ワーク作成　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME3     PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
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
            "01C-MID" "N" "3" "10" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "50" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "50" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "50" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "50" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "50" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "50" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "27" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "27" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0512ID.
           MOVE W-FID2 TO WK0768ID.
           MOVE WK0512ID TO HHTYW_PNAME1.
           MOVE WK0768ID TO HHTY_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HHTYW_PNAME1 " " BY REFERENCE HHTYW_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" HHTY_PNAME1 " " BY REFERENCE HHTY_IDLST "0".
      *           READ HHTYW AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HHTYW_PNAME1 BY REFERENCE HHTYW-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "E-ME3" E-ME3 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-90
           END-IF.
       M-10.
           INITIALIZE HHTY-R.
           MOVE HHTYW-KEY TO HHTY-KEY.
           MOVE HHTYW-SNG TO HHTY-SNG.
           MOVE HHTYW-ENG TO HHTY-ENG.
           MOVE HHTYW-BC TO HHTY-BC.
           MOVE HHTYW-BMNO TO HHTY-BMNO.
       M-15.
           MOVE ZERO TO CNT.
       M-20.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               ADD HHTYW-ZSU(CNT) TO HHTY-ZSU(CNT)
               ADD HHTYW-NSU(CNT) TO HHTY-NSU(CNT)
               ADD HHTYW-USU(CNT) TO HHTY-USU(CNT)
               ADD HHTYW-YSU(CNT) TO HHTY-YSU(CNT)
               GO TO M-20
           END-IF.
      *           READ HHTYW AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HHTYW_PNAME1 BY REFERENCE HHTYW-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF.
           IF  HHTYW-KEY = HHTY-KEY
               GO TO M-15
           END-IF.
           PERFORM CHK-RTN THRU CHK-EX.
           IF  W-ZC NOT = 0
      *               WRITE HHTY-R.
      *//////////////////////
               CALL "DB_Insert" USING
                HHTY_PNAME1 HHTY_LNAME HHTY-R RETURNING RET
           END-IF.
           GO TO M-10.
       M-80.
           PERFORM CHK-RTN THRU CHK-EX.
           IF  W-ZC NOT = 0
      *               WRITE HHTY-R.
      *//////////////////////
               CALL "DB_Insert" USING
                HHTY_PNAME1 HHTY_LNAME HHTY-R RETURNING RET
           END-IF.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HHTY_IDLST HHTY_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HHTYW_IDLST HHTYW_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       CHK-RTN.
           MOVE ZERO TO W-ZC W-C.
       CHK-10.
           ADD 1 TO W-C.
           IF  W-C = 11
               GO TO CHK-EX
           END-IF.
           IF  HHTY-ZSU(W-C) NOT = ZERO
               MOVE 1 TO W-ZC
               GO TO CHK-EX
           END-IF.
           IF  HHTY-NSU(W-C) NOT = ZERO
               MOVE 1 TO W-ZC
               GO TO CHK-EX
           END-IF.
           IF  HHTY-USU(W-C) NOT = ZERO
               MOVE 1 TO W-ZC
               GO TO CHK-EX
           END-IF.
           IF  HHTY-YSU(W-C) NOT = ZERO
               MOVE 1 TO W-ZC
               GO TO CHK-EX
           END-IF.
           GO TO CHK-10.
       CHK-EX.
           EXIT.

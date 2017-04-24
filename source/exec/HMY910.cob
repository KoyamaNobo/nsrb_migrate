       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY910.
      **************************************************************
      *    PROGRAM         :  履物年間サイズ別受払表　期間日付入力 *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  ******                               *
      *    COMPILE TYPE    :  COBOL                                *
      *    JS-SIGN         :  0=日付入力 , 1=５月より当月迄        *
      **************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM7200.
       OBJECT-COMPUTER. SYSTEM7200.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  JS-SIGN            PIC  9(001).
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0512".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-SNG          PIC  9(006).
           02  W-SNGD  REDEFINES W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SND   REDEFINES W-SNEN.
               04  W-SN1      PIC  9(002).
               04  W-SN2      PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-ENG          PIC  9(006).
           02  W-ENGD  REDEFINES W-ENG.
             03  W-ENEN       PIC  9(004).
             03  W-END   REDEFINES W-ENEN.
               04  W-EN1      PIC  9(002).
               04  W-EN2      PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-ENGL  REDEFINES W-ENG.
             03  F            PIC  9(002).
             03  W-ENGS       PIC  9(004).
           02  W-SYM.
             03  W-SYY        PIC  9(004).
             03  W-SYD   REDEFINES W-SYY.
               04  W-SY1      PIC  9(002).
               04  W-SY2      PIC  9(002).
             03  W-SMM        PIC  9(002).
           02  W-SYML  REDEFINES W-SYM.
             03  F            PIC  9(002).
             03  W-SYMS       PIC  9(004).
           02  W-EYM.
             03  W-EYY        PIC  9(004).
             03  W-EYD   REDEFINES W-EYY.
               04  W-EY1      PIC  9(002).
               04  W-EY2      PIC  9(002).
             03  W-EMM        PIC  9(002).
           02  W-EYML  REDEFINES W-EYM.
             03  F            PIC  9(002).
             03  W-EYMS       PIC  9(004).
           02  W-SEN          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-ZC           PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-AYSU.                                                    前月残
             03  W-YSUD  OCCURS  10.
               04  W-YSU      PIC S9(006).
      *
           COPY LSTAT.
           COPY LIBFDD.
           COPY LIHHTF.
      *
       01  HHTYR_HMY910.
           02  HHTYR_PNAME1  PIC  X(005)  VALUE "HHTYR".
           02  F             PIC  X(001).
           02  HHTYR_LNAME   PIC  X(012)  VALUE "HHTYR_HMY910".
           02  F             PIC  X(001).
           02  HHTYR_KEY1    PIC  X(100)  VALUE SPACE.
           02  HHTYR_KEY2    PIC  X(100)  VALUE SPACE.
           02  HHTYR_SORT    PIC  X(100)  VALUE SPACE.
           02  HHTYR_IDLST   PIC  X(100)  VALUE SPACE.
           02  HHTYR_RES     USAGE  POINTER.
       01  HHTYR-R.
           02  HHTYR-KEY2.
             03  HHTYR-MHCD   PIC  9(006).
             03  HHTYR-KEY.
               04  HHTYR-HCD  PIC  9(006).
               04  HHTYR-SIZ  PIC  9(001).
           02  HHTYR-AZSU.                                                前月残
             03  HHTYR-ZSUD  OCCURS  10.
               04  HHTYR-ZSU  PIC S9(006) COMP-3.
           02  HHTYR-ANSU.                                                入庫数
             03  HHTYR-NSUD  OCCURS  10.
               04  HHTYR-NSU  PIC S9(006) COMP-3.
           02  HHTYR-AUSU.                                                出庫数
             03  HHTYR-USUD  OCCURS  10.
               04  HHTYR-USU  PIC S9(006) COMP-3.
           02  HHTYR-AASS.                                                預り出
             03  HHTYR-ASSD  OCCURS  10.
               04  HHTYR-ASS  PIC S9(004) COMP-3.
           02  HHTYR-ATZS.
             03  HHTYR-TSZD  OCCURS  10.
               04  HHTYR-TZS  PIC S9(006) COMP-3.
           02  HHTYR-ATSU.                                                棚卸数
             03  HHTYR-TSUD  OCCURS  10.
               04  HHTYR-TSU  PIC S9(006) COMP-3.
           02  HHTYR-BC.
             03  HHTYR-BC1    PIC  9(002).
             03  HHTYR-BC2    PIC  9(002).
             03  HHTYR-BC3    PIC  9(002).
           02  HHTYR-BMNO     PIC  9(001).
           02  HHTYR-NG       PIC  9(006).
       77  F                  PIC  X(001).
       01  HHTYW_HMY910.
           02  HHTYW_PNAME1  PIC  X(009)  VALUE SPACE.
           02  F             PIC  X(001).
           02  HHTYW_LNAME   PIC  X(012)  VALUE "HHTYW_HMY910".
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
           02  FILLER  PIC  X(049) VALUE
                "全体 = 0  ,  一般・ワーク = 1  ,  教育 = 2  ...  ".
           02  FILLER  PIC  X(040) VALUE
                "ＤＡＴＡ期間    '  年   月 ～ '  年   月".
           02  FILLER  PIC  X(040) VALUE
                "作 表 期 間     '  年   月 ～ '  年   月".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SEN     PIC  9(001).
           02  A-PNG.
             03  A-SNEN    PIC  9(002).
             03  A-SGET    PIC  9(002).
             03  A-ENEN    PIC  9(002).
             03  A-EGET    PIC  9(002).
           02  A-DMM     PIC  9(001).
       01  C-DSP.
           02  D-DNG.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(018) VALUE
                  "***  DATEM ﾅｼ  ***".
             03  E-ME2     PIC  X(027) VALUE
                  "***  DATEM REWRITE ｴﾗｰ  ***".
             03  E-ME3     PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
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
            "C-MID" " " "0" "0" "501" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING
            "08C-MID" "X" "12" "10" "49" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "15" "10" "40" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "17" "10" "40" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "11C-MID" "X" "20" "24" "22" "10C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SEN" "9" "12" "58" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-PNG" " " "17" "0" "8" "A-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SNEN" "9" "17" "27" "2" " " "A-PNG" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SNEN" BY REFERENCE W-SN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SGET" "9" "17" "32" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ENEN" "9" "17" "41" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-ENEN" BY REFERENCE W-EN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EGET" "9" "17" "46" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "41" "1" "A-PNG" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DNG" " " "15" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DNG" "9" "15" "27" "2" " " "D-DNG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DNG" BY REFERENCE W-SY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DNG" "9" "15" "32" "2" "01D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DNG" BY REFERENCE W-SMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-DNG" "9" "15" "41" "2" "02D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-DNG" BY REFERENCE W-EY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-DNG" "9" "15" "46" "2" "03D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-DNG" BY REFERENCE W-EMM "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "72" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "72" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "27" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" "X" "24" "15" "17" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP RUN
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-NHNG TO W-EYMS.
           IF  W-EY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-EYY
           END-IF.
           IF  W-EY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-EYY
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" HHTYR_PNAME1 " " BY REFERENCE HHTYR_IDLST "0".
      *           READ HHTYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HHTYR_PNAME1 BY REFERENCE HHTYR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HHTYR_IDLST HHTYR_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "E-ME3" E-ME3 "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                  RETURNING RESU
               GO TO M-95
           END-IF.
           MOVE HHTYR-NG TO W-SYM.
           CALL "DB_F_Close" USING
            BY REFERENCE HHTYR_IDLST HHTYR_PNAME1.
           MOVE W-EYM TO W-SNG W-ENG.
           IF  W-SGET < 5
               SUBTRACT 1 FROM W-SNEN
           END-IF.
           MOVE 5 TO W-SGET.
           CALL "SD_Output" USING "D-DNG" D-DNG "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "A-PNG" A-PNG "p"
                                  RETURNING RESU.
           IF  JS-SIGN = 1
               MOVE 0 TO W-SEN
               MOVE 1 TO W-DMM
               CALL "SD_Output" USING "A-SEN" A-SEN "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "A-DMM" A-DMM "p"
                                  RETURNING RESU
               GO TO M-10
           END-IF.
           PERFORM ACP-RTN THRU ACP-EX.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                  RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
       M-10.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0512ID.
           MOVE WK0512ID TO HHTYW_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" HHTYW_PNAME1 " " BY REFERENCE HHTYW_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HHTYR_PNAME1 " " BY REFERENCE HHTYR_IDLST "0".
       M-15.
      *           READ HHTYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HHTYR_PNAME1 BY REFERENCE HHTYR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF.
           IF  HHTYR-HCD > 999899
               GO TO M-15
           END-IF.
           IF  HHTYR-NG > W-ENG
               GO TO M-40
           END-IF.
           IF  HHTYR-NG < W-SNG
               GO TO M-15
           END-IF.
           IF  W-SEN = 1
               IF  HHTYR-BC3 = 30
                   GO TO M-15
               END-IF
           END-IF.
           IF  W-SEN = 2
               IF  HHTYR-BC3 NOT = 30
                   GO TO M-15
               END-IF
           END-IF.
           PERFORM CHK1-RTN THRU CHK1-EX.
           IF  W-ZC = 0
               GO TO M-15
           END-IF.
           IF  HHTYR-NG NOT = W-ENG
               GO TO M-30
           END-IF.
       M-20.
           MOVE ZERO TO W-AYSU.
           MOVE ZERO TO CNT.
       M-25.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-30
           END-IF.
           IF (HHTYR-SIZ = 4) AND (CNT = 10)
               MOVE ZERO TO HHTYR-ZSU(CNT) W-YSU(CNT)
           ELSE
               COMPUTE W-YSU(CNT) = HHTYR-ZSU(CNT) + HHTYR-NSU(CNT)
                                      - HHTYR-USU(CNT) - HHTYR-ASS(CNT)
           END-IF.
           GO TO M-25.
       M-30.
           INITIALIZE HHTYW-R.
           MOVE HHTYR-KEY     TO HHTYW-KEY.
           IF  HHTYR-NG = W-SNG
               MOVE HHTYR-ZSU(01) TO HHTYW-ZSU(01)
               MOVE HHTYR-ZSU(02) TO HHTYW-ZSU(02)
               MOVE HHTYR-ZSU(03) TO HHTYW-ZSU(03)
               MOVE HHTYR-ZSU(04) TO HHTYW-ZSU(04)
               MOVE HHTYR-ZSU(05) TO HHTYW-ZSU(05)
               MOVE HHTYR-ZSU(06) TO HHTYW-ZSU(06)
               MOVE HHTYR-ZSU(07) TO HHTYW-ZSU(07)
               MOVE HHTYR-ZSU(08) TO HHTYW-ZSU(08)
               MOVE HHTYR-ZSU(09) TO HHTYW-ZSU(09)
               MOVE HHTYR-ZSU(10) TO HHTYW-ZSU(10)
           ELSE
               MOVE ZERO TO HHTYW-ZSU(01) HHTYW-ZSU(02) HHTYW-ZSU(03)
                            HHTYW-ZSU(04) HHTYW-ZSU(05) HHTYW-ZSU(06)
                            HHTYW-ZSU(07) HHTYW-ZSU(08) HHTYW-ZSU(09)
                            HHTYW-ZSU(10)
           END-IF.
           MOVE HHTYR-NSU(01) TO HHTYW-NSU(01).
           MOVE HHTYR-NSU(02) TO HHTYW-NSU(02).
           MOVE HHTYR-NSU(03) TO HHTYW-NSU(03).
           MOVE HHTYR-NSU(04) TO HHTYW-NSU(04).
           MOVE HHTYR-NSU(05) TO HHTYW-NSU(05).
           MOVE HHTYR-NSU(06) TO HHTYW-NSU(06).
           MOVE HHTYR-NSU(07) TO HHTYW-NSU(07).
           MOVE HHTYR-NSU(08) TO HHTYW-NSU(08).
           MOVE HHTYR-NSU(09) TO HHTYW-NSU(09).
           MOVE HHTYR-NSU(10) TO HHTYW-NSU(10).
           MOVE HHTYR-USU(01) TO HHTYW-USU(01).
           MOVE HHTYR-USU(02) TO HHTYW-USU(02).
           MOVE HHTYR-USU(03) TO HHTYW-USU(03).
           MOVE HHTYR-USU(04) TO HHTYW-USU(04).
           MOVE HHTYR-USU(05) TO HHTYW-USU(05).
           MOVE HHTYR-USU(06) TO HHTYW-USU(06).
           MOVE HHTYR-USU(07) TO HHTYW-USU(07).
           MOVE HHTYR-USU(08) TO HHTYW-USU(08).
           MOVE HHTYR-USU(09) TO HHTYW-USU(09).
           MOVE HHTYR-USU(10) TO HHTYW-USU(10).
           ADD HHTYR-ASS(01) TO HHTYW-USU(01).
           ADD HHTYR-ASS(02) TO HHTYW-USU(02).
           ADD HHTYR-ASS(03) TO HHTYW-USU(03).
           ADD HHTYR-ASS(04) TO HHTYW-USU(04).
           ADD HHTYR-ASS(05) TO HHTYW-USU(05).
           ADD HHTYR-ASS(06) TO HHTYW-USU(06).
           ADD HHTYR-ASS(07) TO HHTYW-USU(07).
           ADD HHTYR-ASS(08) TO HHTYW-USU(08).
           ADD HHTYR-ASS(09) TO HHTYW-USU(09).
           ADD HHTYR-ASS(10) TO HHTYW-USU(10).
           IF  HHTYR-NG = W-ENG
               MOVE W-YSU(01) TO HHTYW-YSU(01)
               MOVE W-YSU(02) TO HHTYW-YSU(02)
               MOVE W-YSU(03) TO HHTYW-YSU(03)
               MOVE W-YSU(04) TO HHTYW-YSU(04)
               MOVE W-YSU(05) TO HHTYW-YSU(05)
               MOVE W-YSU(06) TO HHTYW-YSU(06)
               MOVE W-YSU(07) TO HHTYW-YSU(07)
               MOVE W-YSU(08) TO HHTYW-YSU(08)
               MOVE W-YSU(09) TO HHTYW-YSU(09)
               MOVE W-YSU(10) TO HHTYW-YSU(10)
           ELSE
               MOVE ZERO TO HHTYW-YSU(01) HHTYW-YSU(02) HHTYW-YSU(03)
                            HHTYW-YSU(04) HHTYW-YSU(05) HHTYW-YSU(06)
                            HHTYW-YSU(07) HHTYW-YSU(08) HHTYW-YSU(09)
                            HHTYW-YSU(10)
           END-IF.
           MOVE W-SNG         TO HHTYW-SNG.
           MOVE W-ENG         TO HHTYW-ENG.
           MOVE HHTYR-BC      TO HHTYW-BC.
           MOVE HHTYR-BMNO    TO HHTYW-BMNO.
           MOVE HHTYR-NG      TO HHTYW-NG.
      *           WRITE HHTYW-R.
      *//////////////////////
           CALL "DB_Insert" USING
            HHTYW_PNAME1 HHTYW_LNAME HHTYW-R RETURNING RET.
       M-35.
      *           READ HHTYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HHTYR_PNAME1 BY REFERENCE HHTYR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF.
           IF  HHTYR-HCD > 999899
               GO TO M-35
           END-IF.
           IF  HHTYR-NG < W-SNG OR > W-ENG
               GO TO M-35
           END-IF.
           IF  W-SEN = 1
               IF  HHTYR-BC3 = 30
                   GO TO M-35
               END-IF
           END-IF.
           IF  W-SEN = 2
               IF  HHTYR-BC3 NOT = 30
                   GO TO M-35
               END-IF
           END-IF.
           PERFORM CHK1-RTN THRU CHK1-EX.
           IF  W-ZC = 0
               GO TO M-35
           END-IF.
           IF  HHTYR-NG NOT = W-ENG
               GO TO M-30
           END-IF.
           GO TO M-20.
       M-40.
           CALL "DB_F_Close" USING
            BY REFERENCE HHTYR_IDLST HHTYR_PNAME1.
           IF  W-EYM NOT = W-ENG
               GO TO M-90
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" HHTF_PNAME1 "SHARED" BY REFERENCE HHTF_IDLST "2"
            "HHT-KEY" BY REFERENCE HHT-KEY "HHT-KEY2" BY REFERENCE
            HHT-KEY2.
       M-45.
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-70
           END-IF.
           IF  HHT-HCD > 999899
               GO TO M-45
           END-IF.
           IF  W-SEN = 1
               IF  HHT-BC3 = 30
                   GO TO M-45
               END-IF
           END-IF.
           IF  W-SEN = 2
               IF  HHT-BC3 NOT = 30
                   GO TO M-45
               END-IF
           END-IF.
           PERFORM CHK2-RTN THRU CHK2-EX.
           IF  W-ZC = 0
               GO TO M-45
           END-IF.
       M-50.
           MOVE ZERO TO W-AYSU.
           MOVE ZERO TO CNT.
       M-55.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-60
           END-IF.
           IF (HHT-SIZ = 4) AND (CNT = 10)
               MOVE ZERO TO W-YSU(CNT) HHT-ZSU(CNT)
           ELSE
               COMPUTE W-YSU(CNT) = HHT-ZSU(CNT) + HHT-NSU(CNT)
                                      - HHT-USU(CNT) - HHT-ASS(CNT)
           END-IF.
           GO TO M-55.
       M-60.
           INITIALIZE HHTYW-R.
           MOVE HHT-KEY     TO HHTYW-KEY.
           IF  W-EYM = W-SNG
               MOVE HHT-ZSU(01) TO HHTYW-ZSU(01)
               MOVE HHT-ZSU(02) TO HHTYW-ZSU(02)
               MOVE HHT-ZSU(03) TO HHTYW-ZSU(03)
               MOVE HHT-ZSU(04) TO HHTYW-ZSU(04)
               MOVE HHT-ZSU(05) TO HHTYW-ZSU(05)
               MOVE HHT-ZSU(06) TO HHTYW-ZSU(06)
               MOVE HHT-ZSU(07) TO HHTYW-ZSU(07)
               MOVE HHT-ZSU(08) TO HHTYW-ZSU(08)
               MOVE HHT-ZSU(09) TO HHTYW-ZSU(09)
               MOVE HHT-ZSU(10) TO HHTYW-ZSU(10)
           ELSE
               MOVE ZERO TO HHTYW-ZSU(01) HHTYW-ZSU(02) HHTYW-ZSU(03)
                            HHTYW-ZSU(04) HHTYW-ZSU(05) HHTYW-ZSU(06)
                            HHTYW-ZSU(07) HHTYW-ZSU(08) HHTYW-ZSU(09)
                            HHTYW-ZSU(10)
           END-IF.
           MOVE HHT-NSU(01) TO HHTYW-NSU(01).
           MOVE HHT-NSU(02) TO HHTYW-NSU(02).
           MOVE HHT-NSU(03) TO HHTYW-NSU(03).
           MOVE HHT-NSU(04) TO HHTYW-NSU(04).
           MOVE HHT-NSU(05) TO HHTYW-NSU(05).
           MOVE HHT-NSU(06) TO HHTYW-NSU(06).
           MOVE HHT-NSU(07) TO HHTYW-NSU(07).
           MOVE HHT-NSU(08) TO HHTYW-NSU(08).
           MOVE HHT-NSU(09) TO HHTYW-NSU(09).
           MOVE HHT-NSU(10) TO HHTYW-NSU(10).
           MOVE HHT-USU(01) TO HHTYW-USU(01).
           MOVE HHT-USU(02) TO HHTYW-USU(02).
           MOVE HHT-USU(03) TO HHTYW-USU(03).
           MOVE HHT-USU(04) TO HHTYW-USU(04).
           MOVE HHT-USU(05) TO HHTYW-USU(05).
           MOVE HHT-USU(06) TO HHTYW-USU(06).
           MOVE HHT-USU(07) TO HHTYW-USU(07).
           MOVE HHT-USU(08) TO HHTYW-USU(08).
           MOVE HHT-USU(09) TO HHTYW-USU(09).
           MOVE HHT-USU(10) TO HHTYW-USU(10).
           ADD HHT-ASS(01) TO HHTYW-USU(01).
           ADD HHT-ASS(02) TO HHTYW-USU(02).
           ADD HHT-ASS(03) TO HHTYW-USU(03).
           ADD HHT-ASS(04) TO HHTYW-USU(04).
           ADD HHT-ASS(05) TO HHTYW-USU(05).
           ADD HHT-ASS(06) TO HHTYW-USU(06).
           ADD HHT-ASS(07) TO HHTYW-USU(07).
           ADD HHT-ASS(08) TO HHTYW-USU(08).
           ADD HHT-ASS(09) TO HHTYW-USU(09).
           ADD HHT-ASS(10) TO HHTYW-USU(10).
           MOVE W-YSU(01) TO HHTYW-YSU(01).
           MOVE W-YSU(02) TO HHTYW-YSU(02).
           MOVE W-YSU(03) TO HHTYW-YSU(03).
           MOVE W-YSU(04) TO HHTYW-YSU(04).
           MOVE W-YSU(05) TO HHTYW-YSU(05).
           MOVE W-YSU(06) TO HHTYW-YSU(06).
           MOVE W-YSU(07) TO HHTYW-YSU(07).
           MOVE W-YSU(08) TO HHTYW-YSU(08).
           MOVE W-YSU(09) TO HHTYW-YSU(09).
           MOVE W-YSU(10) TO HHTYW-YSU(10).
           MOVE W-SNG       TO HHTYW-SNG.
           MOVE W-ENG       TO HHTYW-ENG HHTYW-NG.
           MOVE HHT-BC1     TO HHTYW-BC1.
           MOVE HHT-BC2     TO HHTYW-BC2.
           MOVE HHT-BC3     TO HHTYW-BC3.
           MOVE HHT-BMNO    TO HHTYW-BMNO.
      *           WRITE HHTYW-R.
      *//////////////////////
           CALL "DB_Insert" USING
            HHTYW_PNAME1 HHTYW_LNAME HHTYW-R RETURNING RET.
       M-65.
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-70
           END-IF.
           IF  HHT-HCD > 999899
               GO TO M-65
           END-IF.
           IF  W-SEN = 1
               IF  HHT-BC3 = 30
                   GO TO M-65
               END-IF
           END-IF.
           IF  W-SEN = 2
               IF  HHT-BC3 NOT = 30
                   GO TO M-65
               END-IF
           END-IF.
           PERFORM CHK2-RTN THRU CHK2-EX.
           IF  W-ZC = 0
               GO TO M-65
           END-IF.
           GO TO M-50.
       M-70.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE HHTYW_IDLST HHTYW_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                  RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACP-EX
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-RTN
           END-IF.
           IF  W-SEN > 2
               GO TO ACP-RTN
           END-IF.
       ACP-10.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACP-EX
           END-IF.
           IF  ESTAT = BTB
               GO TO ACP-RTN
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-10
           END-IF.
           MOVE ZERO TO W-SN1.
           IF  W-SN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF.
           IF  W-SN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
           IF  W-SNEN < W-SYY OR > W-EYY
               GO TO ACP-10
           END-IF.
       ACP-20.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-10
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-20
           END-IF.
           IF  W-SNG < W-SYM OR > W-EYM
               GO TO ACP-20
           END-IF.
           IF  W-SGET < 1 OR > 12
               GO TO ACP-20
           END-IF.
       ACP-30.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-20
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-30
           END-IF.
           MOVE ZERO TO W-EN1.
           IF  W-EN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF.
           IF  W-EN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
           IF  W-ENEN < W-SNEN OR > W-EYY
               GO TO ACP-30
           END-IF.
       ACP-40.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-30
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-40
           END-IF.
           IF  W-ENG < W-SNG
               GO TO ACP-40
           END-IF.
           IF  W-ENG < W-SYM OR > W-EYM
               GO TO ACP-40
           END-IF.
           IF  W-EGET < 1 OR > 12
               GO TO ACP-40
           END-IF.
       ACP-50.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-40
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-50
           END-IF.
           IF  W-DMM = 9
               GO TO ACP-RTN
           END-IF.
           IF  W-DMM NOT = 1
               GO TO ACP-50
           END-IF.
       ACP-EX.
           EXIT.
       CHK1-RTN.
           MOVE ZERO TO W-ZC CNT.
       CHK1-10.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO CHK1-EX
           END-IF.
           IF  HHTYR-NSU(CNT) NOT = ZERO
               MOVE 1 TO W-ZC
               GO TO CHK1-EX
           END-IF.
           IF  HHTYR-USU(CNT) NOT = ZERO
               MOVE 1 TO W-ZC
               GO TO CHK1-EX
           END-IF.
           IF  HHTYR-ASS(CNT) NOT = ZERO
               MOVE 1 TO W-ZC
               GO TO CHK1-EX
           END-IF.
           IF  HHTYR-NG = W-SNG
               IF  HHTYR-ZSU(CNT) NOT = ZERO
                   MOVE 1 TO W-ZC
                   GO TO CHK1-EX
               END-IF
           END-IF.
           GO TO CHK1-10.
       CHK1-EX.
           EXIT.
       CHK2-RTN.
           MOVE ZERO TO W-ZC CNT.
       CHK2-10.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO CHK2-EX
           END-IF.
           IF  HHT-NSU(CNT) NOT = ZERO
               MOVE 1 TO W-ZC
               GO TO CHK2-EX
           END-IF.
           IF  HHT-USU(CNT) NOT = ZERO
               MOVE 1 TO W-ZC
               GO TO CHK2-EX
           END-IF.
           IF  HHT-ASS(CNT) NOT = ZERO
               MOVE 1 TO W-ZC
               GO TO CHK2-EX
           END-IF.
           IF  HHT-ZSU(CNT) NOT = ZERO
               MOVE 1 TO W-ZC
               GO TO CHK2-EX
           END-IF.
           GO TO CHK2-10.
       CHK2-EX.
           EXIT.

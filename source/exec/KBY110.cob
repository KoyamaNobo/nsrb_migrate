       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         KBY110.
      *********************************************************
      *    PROGRAM         :  年間取引先別仕入高明細表        *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  96/05/28                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(048) VALUE SPACE.
           02  F              PIC  N(019) VALUE
                "【　　年間取引先別　仕入高明細表　　】".
           02  F              PIC  X(044) VALUE SPACE.
           02  F              PIC  X(002) VALUE "P.".
           02  H-PAGE         PIC  Z9.
       01  HEAD2.
           02  F              PIC  X(052) VALUE SPACE.
           02  F              PIC  X(004) VALUE "(   ".
           02  H-HN           PIC  Z9.
           02  F              PIC  N(001) VALUE "年".
           02  H-HG           PIC  Z9.
           02  F              PIC  N(001) VALUE "月".
           02  F              PIC  N(003) VALUE "　～　".
           02  H-ON           PIC  Z9.
           02  F              PIC  N(001) VALUE "年".
           02  H-OG           PIC  Z9.
           02  F              PIC  N(001) VALUE "月".
           02  F              PIC  X(004) VALUE "   )".
       01  HEAD3.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　№".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                 "仕　　入　　先　　名".
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(011) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　№".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                 "仕　　入　　先　　名".
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
       01  W-P.
           02  W-PD    OCCURS  56.
             03  F            PIC  X(002).
             03  P-NO01       PIC  9(003).
             03  F            PIC  X(001).
             03  P-KEY01      PIC  9(004).
             03  F            PIC  X(001).
             03  P-NAME01     PIC  N(024).
             03  F            PIC  X(001).
             03  P-YSK01      PIC  --,---,---,---.
             03  F            PIC  X(005).
             03  P-X01        PIC  X(001).
             03  F            PIC  X(005).
             03  P-NO02       PIC  9(003).
             03  F            PIC  X(001).
             03  P-KEY02      PIC  9(004).
             03  F            PIC  X(001).
             03  P-NAME02     PIC  N(024).
             03  F            PIC  X(001).
             03  P-YSK02      PIC  --,---,---,---.
       01  W-D.
           02  W-DATE.
             03  W-HNG.
               04  F          PIC  9(002).
               04  W-HN       PIC  9(002).
               04  W-HG       PIC  9(002).
             03  W-ONG.
               04  F          PIC  9(002).
               04  W-ON       PIC  9(002).
               04  W-OG       PIC  9(002).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-T.
             03  W-STK        PIC S9(010).
             03  W-ATK        PIC S9(010).
           02  W-DMM          PIC  9(001).
           02  W-NO           PIC  9(003) VALUE ZERO.
           02  W-CD           PIC  9(001).
           02  W-LD           PIC  9(002).
           02  W-PC           PIC  9(001).
           02  W-BR           PIC  9(001).
           02  W-BRD          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY  LSTAT.
      *
           COPY LIBFDD.
           COPY LIKBNO.
           COPY LISM.
           COPY LSPF.
      *FD  ST-M
       01  ST-M_KBY110.
           02  ST-M_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  ST-M_LNAME     PIC  X(011) VALUE "ST-M_KBY110".
           02  F              PIC  X(001).
           02  ST-M_KEY1      PIC  X(100) VALUE SPACE.
           02  ST-M_SORT      PIC  X(100) VALUE SPACE.
           02  ST-M_IDLST     PIC  X(100) VALUE SPACE.
           02  ST-M_RES       USAGE  POINTER.
       01  ST-R.
           02  ST-KEY.
             03  ST-KEY1      PIC  9(001).
             03  ST-KEY2      PIC  9(003).
           02  ST-YSK         PIC S9(010).
           02  F              PIC  X(050).
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
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊　　年間取引先別　仕入高明細表　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                 "  年  月　～　  年  月".
           02  FILLER  PIC  X(022) VALUE
                   "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC Z9 .
             03  FILLER  PIC  9(002).
             03  FILLER  PIC Z9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1    PIC X(017)   VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2    PIC X(018)   VALUE
                  "***  KBNOM ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY  LIBSCR.
       PROCEDURE           DIVISION.
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
            "C-MID" " " "0" "0" "366" " " " " RETURNING RESU.
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
            "08C-MID" "X" "12" "22" "22" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "22" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "39" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NG" " " "12" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NG" "9" "12" "22" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-HN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NG" "Z9" "12" "26" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-HG "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-NG" "9" "12" "36" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE W-ON "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-NG" "Z9" "12" "40" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NG" BY REFERENCE W-OG "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "45" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "45" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
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
           COPY  LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" KBNO-M_PNAME1 "SHARED" BY REFERENCE KBNO-M_IDLST "1"
            "BNO-KEY" BY REFERENCE BNO-KEY.
           MOVE SPACE TO BNO-KEY.
           MOVE "01" TO BNO-KEYD.
      *           READ KBNO-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KBNO-M_PNAME1 BY REFERENCE KBNO-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KBNO-M_IDLST KBNO-M_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE BNO-SNG TO W-HNG.
           MOVE BNO-ENG TO W-ONG.
           CALL "DB_F_Close" USING
            BY REFERENCE KBNO-M_IDLST KBNO-M_PNAME1.
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-10
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  W-DMM NOT = 1
               GO  TO  M-10
           END-IF
      *
           CALL  "CBLSTNNO"  USING  STN-NO USER_ID.
           MOVE  STN-NO2  TO  W-FID2.
           MOVE  W-FID    TO  WK0064ID.
           MOVE WK0064ID TO ST-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" ST-M_PNAME1 " " BY REFERENCE ST-M_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           MOVE  ZERO  TO  W-PAGE.
           CALL "PR_Open" RETURNING RESP.
           MOVE  W-HN TO H-HN.
           MOVE  W-HG TO H-HG.
           MOVE  W-ON TO H-ON.
           MOVE  W-OG TO H-OG.
           MOVE  SPACE  TO  W-P.
           MOVE  ZERO   TO  W-CD  W-LD  W-PC  W-ATK.
       M-15.
      *           READ  ST-M  AT  END
      *///////////////
           CALL "DB_Read" USING
            "AT END" ST-M_PNAME1 BY REFERENCE ST-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-95
           END-IF
           IF  ST-YSK = ZERO
               GO  TO  M-15
           END-IF
           IF  ST-KEY1 > 4
               GO  TO  M-25
           END-IF.
       M-20.
           PERFORM  S-55  THRU  S-60.
       M-25.
      *           READ  ST-M  AT  END
      *///////////////
           CALL "DB_Read" USING
            "AT END" ST-M_PNAME1 BY REFERENCE ST-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE ST-M_IDLST ST-M_PNAME1
               GO  TO  M-30
           END-IF
           IF  ST-YSK = ZERO
               GO  TO  M-25
           END-IF
           IF  ST-KEY1 > 4
               GO  TO  M-25
           END-IF
           GO  TO  M-20.
       M-30.
           PERFORM  S-20  THRU  S-25.
           MOVE  ZERO  TO  W-STK  W-NO.
           CALL "DB_F_Open" USING
            "INPUT" ST-M_PNAME1 " " BY REFERENCE ST-M_IDLST "0".
       M-35.
      *           READ  ST-M  AT  END
      *///////////////
           CALL "DB_Read" USING
            "AT END" ST-M_PNAME1 BY REFERENCE ST-R " " RETURNING RET.
           IF  RET = 1
               GO  TO  M-90
           END-IF
           IF  ST-YSK = ZERO
               GO  TO  M-35
           END-IF
           IF  ST-KEY1 < 5
               GO  TO  M-45
           END-IF.
       M-40.
           PERFORM  S-55  THRU  S-60.
       M-45.
      *           READ  ST-M  AT  END
      *///////////////
           CALL "DB_Read" USING
            "AT END" ST-M_PNAME1 BY REFERENCE ST-R " " RETURNING RET.
           IF  RET = 1
               GO  TO  M-50
           END-IF
           IF  ST-YSK = ZERO
               GO  TO  M-45
           END-IF
           IF  ST-KEY1 < 5
               GO  TO  M-45
           END-IF
           GO  TO  M-40.
       M-50.
           PERFORM  S-20  THRU  S-25.
       M-90.
           PERFORM  S-30  THRU  S-35.
           IF  W-CD = ZERO
               MOVE "　［　ＡＬＬ　ＴＯＴＡＬ　］" TO P-NAME01(W-LD)
               MOVE  W-ATK  TO  P-YSK01(W-LD)
               MOVE  ":"    TO  P-X01(W-LD)
           END-IF
           IF  W-CD = 1
               MOVE "　［　ＡＬＬ　ＴＯＴＡＬ　］" TO P-NAME02(W-LD)
               MOVE  W-ATK  TO  P-YSK02(W-LD)
           END-IF
           PERFORM  S-40  THRU  S-50.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE ST-M_IDLST ST-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.
       S-05.
           MOVE   SPACE   TO     SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD    1       TO     W-PAGE.
           MOVE   W-PAGE  TO     H-PAGE.
           MOVE   SPACE   TO     SP-R.
           MOVE   HEAD1   TO     SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE   TO     SP-R.
           MOVE   HEAD2   TO     SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE   TO     SP-R.
           MOVE   HEAD3   TO     SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE   TO     SP-R.
       S-15.
           EXIT.
       S-20.
           PERFORM  S-30  THRU  S-35.
           IF  W-CD = ZERO
               MOVE "　　＜　ＳＵＢ　ＴＯＴＡＬ　＞" TO P-NAME01(W-LD)
               MOVE  W-STK  TO  P-YSK01(W-LD)
               MOVE  ":"    TO  P-X01(W-LD)
           END-IF
           IF  W-CD = 1
               MOVE "　　＜　ＳＵＢ　ＴＯＴＡＬ　＞" TO P-NAME02(W-LD)
               MOVE  W-STK  TO  P-YSK02(W-LD)
           END-IF
           ADD  W-STK  TO  W-ATK.
           PERFORM  S-30  THRU  S-35.
           IF  W-CD = ZERO
               MOVE  SPACE  TO  P-NAME01(W-LD)
               MOVE  ZERO   TO  P-YSK01(W-LD)
               MOVE  ":"    TO  P-X01(W-LD)
           END-IF
           IF  W-CD = 1
               MOVE  SPACE  TO  P-NAME02(W-LD)
               MOVE  ZERO   TO  P-YSK02(W-LD)
           END-IF.
       S-25.
           EXIT.
       S-30.
           ADD  1  TO  W-LD.
           IF  W-LD NOT = 57
               GO  TO  S-35
           END-IF
           IF  W-CD NOT = 1
               MOVE  1     TO  W-CD
               MOVE  ZERO  TO  W-LD
               GO  TO  S-30
           END-IF
           PERFORM  S-40  THRU  S-50.
           MOVE  SPACE  TO  W-P.
           MOVE  ZERO   TO  W-LD  W-CD.
           GO  TO  S-30.
       S-35.
           EXIT.
       S-40.
           IF  W-PC = ZERO
               MOVE  5  TO  W-PC
               PERFORM  S-10  THRU  S-15
           ELSE
               PERFORM  S-05  THRU  S-15
           END-IF
           MOVE  ZERO  TO  W-LD.
       S-45.
           ADD  1  TO  W-LD.
           IF  W-LD NOT = 57
               IF  P-X01(W-LD) NOT = SPACE
                   MOVE   SPACE       TO  SP-R
                   MOVE   W-PD(W-LD)  TO  SP-R
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   MOVE   SPACE       TO  SP-R
                   GO  TO  S-45
               END-IF
           END-IF.
       S-50.
           EXIT.
       S-55.
           MOVE  ST-KEY  TO  S-KEY.
      *           READ  S-M  WITH  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊　仕入先マスター　なし　＊　　　" TO S-NAME
           END-IF
           PERFORM  S-30  THRU  S-35.
           IF  W-CD = ZERO
               ADD   1       TO  W-NO
               MOVE  W-NO    TO  P-NO01(W-LD)
               MOVE  ST-KEY  TO  P-KEY01(W-LD)
               MOVE  S-NAME  TO  P-NAME01(W-LD)
               MOVE  ST-YSK  TO  P-YSK01(W-LD)
               MOVE  ":"     TO  P-X01(W-LD)
           END-IF
           IF  W-CD = 1
               ADD   1       TO  W-NO
               MOVE  W-NO    TO  P-NO02(W-LD)
               MOVE  ST-KEY  TO  P-KEY02(W-LD)
               MOVE  S-NAME  TO  P-NAME02(W-LD)
               MOVE  ST-YSK  TO  P-YSK02(W-LD)
           END-IF
           ADD  ST-YSK  TO  W-STK.
       S-60.
           EXIT.

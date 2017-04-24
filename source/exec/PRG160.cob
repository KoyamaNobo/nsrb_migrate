      *************************************************
      *    PROGRAM        :　経費相手科目内訳表       *
      *    AUTHOR         :  MAYUMI.I                 *
      *    DATE           :  90/12/26                 *
      *    COMPILE  TYPE  :  COBOL                    *
      *    PRINTER  TYPE  :  JIPS                     *
      *************************************************
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PRG160.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM3100.
       OBJECT-COMPUTER.    NEAC-SYSTEM3100.
       INPUT-OUTPUT        SECTION.
       DATA           DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT            PIC X(02).
       77  W-POC               PIC 9(01)  VALUE  0.
       77  WK0064ID            PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1         PIC  X(003).
           02  STN-NO2         PIC  X(003).
       01  W-FID.
           02  W-FID1          PIC  X(006) VALUE "WK0064".
           02  W-FID2          PIC  X(003).
       01  HEAD1.
           02  W-15K           PIC X(05)  VALUE  X"1A24212078".
           02  F               PIC X(07)  VALUE  SPACE.
           02  F               PIC N(22)  VALUE
               "　＊＊＊　　経費　相手科目　内訳表　　＊＊＊".
           02  F               PIC X(05)  VALUE  SPACE.
           02  W-20K           PIC X(05)  VALUE  X"1A24212474".
           02  H-NEN           PIC 9(02).
           02  F               PIC N(01)  VALUE  "年".
           02  H-GET           PIC 9(02).
           02  F               PIC N(02)  VALUE  "月分".
           02  F               PIC X(05)  VALUE  SPACE.
           02  F               PIC X(05)  VALUE "DATE ".
           02  H-DATE          PIC 99/99/99.
           02  F               PIC X(07)  VALUE "     P.".
           02  H-PAGE          PIC Z9.
       01  HEAD2.
           02  F               PIC X(06)  VALUE  "ｺｰﾄﾞ  ".
           02  F               PIC N(05)  VALUE  "科　目　名".
           02  F               PIC X(18)  VALUE  SPACE.
           02  F               PIC N(03)  VALUE  "現預金".
           02  F               PIC X(08)  VALUE  SPACE.
           02  F               PIC N(03)  VALUE  "手　形".
           02  F               PIC X(08)  VALUE  SPACE.
           02  F               PIC N(03)  VALUE  "振　替".
           02  F               PIC X(08)  VALUE  SPACE.
           02  F               PIC N(03)  VALUE  "合　計".
       01  W-P.
           02  P-KACD          PIC 9(04).
           02  F               PIC X(02).
           02  P-KANA          PIC N(10).
           02  P-GYK           PIC --,---,---,--9.
           02  P-TGT           PIC --,---,---,--9.
           02  P-ETC           PIC --,---,---,--9.
           02  P-KEI           PIC --,---,---,--9.
       01  W-DATA.
           02  WS-D.
             03  WS-GYK        PIC S9(10).
             03  WS-TGT        PIC S9(10).
             03  WS-ETC        PIC S9(10).
             03  WS-KEI        PIC S9(10).
           02  WA-D.
             03  WA-GYK        PIC S9(10).
             03  WA-TGT        PIC S9(10).
             03  WA-ETC        PIC S9(10).
             03  WA-KEI        PIC S9(10).
           02  W-KEI           PIC S9(10).
           02  W-NGP.
             03  W-NG.
               04  W-NEN.
                 05  W-NEN1    PIC 9(02).
                 05  W-NEN2    PIC 9(02).
               04  W-GET       PIC 9(02).
             03  W-PEY         PIC 9(02).
           02  W-PAGE          PIC 9(02).
           02  W-KACD          PIC 9(04).
           02  W-KACDD    REDEFINES  W-KACD.
             03  W-KACD1       PIC 9(01).
             03  W-KACD2       PIC 9(03).
           COPY LWMSG_PR.
           COPY  KANGEL.
      ***
           COPY  FCTL.
       01  KEI-PRN_PRG160.
           02  KEI-PRN_PNAME1    PIC  X(009)  VALUE SPACE.
           02  F                 PIC  X(001).
           02  KEI-PRN_LNAME     PIC  X(014)  VALUE "KEI-PRN_PRG160".
           02  F                 PIC  X(001).
           02  KEI-PRN_KEY1      PIC  X(100)  VALUE SPACE.
           02  KEI-PRN_KEY2      PIC  X(100)  VALUE SPACE.
           02  KEI-PRN_SORT      PIC  X(100)  VALUE SPACE.
           02  KEI-PRN_IDLST     PIC  X(100)  VALUE SPACE.
           02  KEI-PRN_RES       USAGE  POINTER.
       01  KEI-REC.
           02  KEI-KACD        PIC 9(04).
           02  KEI-KACDD    REDEFINES  KEI-KACD.
             03  KEI-KACD1     PIC 9(01).
             03  KEI-KACD2     PIC 9(03).
           02  KEI-KIN         PIC S9(10).
           02  KEI-AKC         PIC 9(01).
           02  FILLER          PIC X(49).
       77  F                       PIC  X(001).
       77  SP-R                PIC X(206).
       77  F                   PIC  X(001).
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER PIC X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER.
               03  FILLER  PIC N(11)   VALUE
                   "経費　相手科目　内訳表".
           02  FILLER.
               03  FILLER     PIC  N(01) VALUE "年".
               03  FILLER     PIC  N(02) VALUE "月度".
       01  D-NG.
           02  FILLER         PIC  N(02).
           02  FILLER         PIC  N(02).
       01  C-ERR.
           02  E-ME1    PIC  X(23)  VALUE
               "***  ｺﾝﾄﾛｰﾙﾏｽﾀｰ ﾅｼ  ***".
           02  E-ME2    PIC  X(17)  VALUE
               "***  DATA ﾅｼ  ***".
           02  E-ME3    PIC  X(20)  VALUE
               "***  ｶﾓｸﾏｽﾀｰ ﾅｼ  ***".
           02  E-KEY    PIC  9(04).
           02  E-ME98   PIC  X(05)  VALUE  X"1B4A05".
           02  E-ME99   PIC  X(05)  VALUE  X"1B4205".
           02  E-CL     PIC  X(50)  VALUE 
           "                                                  ".
           COPY LSMSG_PR.
       PROCEDURE       DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
      * 01  C-CLEAR.
           CALL "SD_Init" USING
                "C-CLEAR" " " "0" "0" "12" " " " "  RETURNING RESU.
           CALL "SD_Init" USING
                "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR"
                RETURNING RESU.
      * 01  C-MID.
           CALL "SD_Init" USING 
                "C-MID" " " "0" "0" "28" " " " "  RETURNING RESU.
           CALL "SD_Init" USING 
                "01C-MID" " " "1" "0" "22" " " "C-MID"  RETURNING RESU.
           CALL "SD_Init" USING 
                "0101C-MID" "RN" "1" "29" "22" " " "01C-MID"
                                             RETURNING RESU.
           CALL "SD_Init" USING 
               "02C-MID" " " "5" "0" "6" "01C-MID" " "  RETURNING RESU.
           CALL "SD_Init" USING 
               "0102C-MID" "N" "5" "6" "2" " " "02C-MID" RETURNING RESU.
           CALL "SD_Init" USING 
               "0202C-MID" "N" "5" "12" "4" "0102C-MID" " "
                                              RETURNING RESU.
      * 01  D-NG
           CALL "SD_Init" USING
                "D-NG" " " "5" "0" "8" " " " "  RETURNING RESU.
           CALL "SD_Init" USING
                "01D-NG" "N" "5" "2" "4" " " "D-NG"  RETURNING RESU.
           CALL "SD_From" USING
                "01D-NG" BY REFERENCE Z-GEMYY2 "2" "0" RETURNING RESU.
           CALL "SD_Init" USING
                "02D-NG" "N" "5" "8" "4" "01D-NG" " "  RETURNING RESU.
           CALL "SD_From" USING
                "02D-NG" BY REFERENCE Z-GEMMM "2" "0" RETURNING RESU.
      * 01  C-ERR
           CALL "SD_Init" USING
                "C-ERR" " " "24" "0" "124" " " " "  RETURNING RESU.
           CALL "SD_Init" USING
                "E-ME1" "X" "24" "15" "23" " " "C-ERR"  RETURNING RESU.
           CALL "SD_Init" USING
                "E-ME2" "X" "24" "15" "17" "E-ME1" " "  RETURNING RESU.
           CALL "SD_Init" USING
                "E-ME3" "X" "24" "15" "20" "E-ME2" " "  RETURNING RESU.
           CALL "SD_Init" USING
                "E-KEY" "9" "24" "40" "4" "E-ME3" " "  RETURNING RESU.
           CALL "SD_From" USING
                "E-KEY" BY REFERENCE W-KACD "4" "0" RETURNING RESU.
           CALL "SD_Init" USING
                "E-ME98" "X" "24" "75" "5" "E-KEY" " "  RETURNING RESU.
           CALL "SD_Init" USING
                "E-ME99" "X" "24" "75" "5" "E-ME98" " "  
                RETURNING RESU.
           CALL "SD_Init" USING
                "E-CL" "X" "24" "10" "50" "E-ME99" " "  RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p"
                                         RETURNING RESU.
           MOVE  ZERO         TO  W-DATA.
      *
           CALL "DB_F_Open" USING "INPUT"
            FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE  "DATE  "     TO  FCTL-KEY.
      *           READ  FCTL-F  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           MOVE  FCTL-REC     TO  Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
      *
           CALL "SD_Output" USING "D-NG" D-NG "p"
                                         RETURNING RESU.
           MOVE  Z-KONYMD     TO  ZYMD   W-NGP.
           PERFORM  Z-RTN     THRU  Z-EXT.
           IF  ZI > 15
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO KEI-PRN_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KEI-PRN_PNAME1 " " BY REFERENCE KEI-PRN_IDLST "0".
      *
       M-15.
      *           READ  KEI-PRN  AT  END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" KEI-PRN_PNAME1 BY REFERENCE KEI-REC " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p"
                                       RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                       RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KEI-PRN_IDLST KEI-PRN_PNAME1
               GO  TO  M-95
           END-IF.
           IF  KEI-KIN          =  ZERO
               GO  TO  M-15
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           MOVE W-NEN2   TO H-NEN.
           MOVE W-GET    TO H-GET.
           ACCEPT  H-DATE  FROM  DATE.
           CALL "PR_Open" RETURNING RESP.
           PERFORM  S-10   THRU  S-15.
       M-20.
           MOVE KEI-KACD     TO W-KACD.
           MOVE ZERO         TO WS-D.
       M-25.
           MOVE KEI-KACD     TO W-KACD.
           MOVE ZERO         TO KNG-KEY.
           MOVE W-KACD       TO K-ACCD.
      *           READ  KNG    UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME3" E-ME3 "p"
                                       RETURNING RESU
               CALL "SD_Output" USING "E-KEY" E-KEY "p"
                                       RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                       RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p"
                                       RETURNING RESU
               MOVE  SPACE      TO  KNGNMN
           END-IF.
      *
           MOVE ZERO         TO W-KEI.
           MOVE SPACE        TO W-P.
           MOVE W-KACD       TO P-KACD.
           MOVE KNGNMN       TO P-KANA.
       M-30.
           IF  KEI-AKC         =  1
               ADD   KEI-KIN       TO  WS-GYK
               MOVE  KEI-KIN       TO  P-GYK
           END-IF.
           IF  KEI-AKC         =  2
               ADD   KEI-KIN       TO  WS-TGT
               MOVE  KEI-KIN       TO  P-TGT
           END-IF.
           IF  KEI-AKC         =  3
               ADD   KEI-KIN       TO  WS-ETC
               MOVE  KEI-KIN       TO  P-ETC
           END-IF.
           ADD  KEI-KIN        TO  W-KEI  WS-KEI.
       M-35.
      *           READ  KEI-PRN  AT  END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" KEI-PRN_PNAME1 BY REFERENCE KEI-REC " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-80
           END-IF.
           IF  KEI-KIN          =  ZERO
               GO  TO  M-35
           END-IF.
           IF  KEI-KACD1    NOT =  W-KACD1
               GO  TO  M-40
           END-IF.
           IF  KEI-KACD2        =  W-KACD2
               GO  TO  M-30
           END-IF.
      *
           PERFORM  S-20   THRU  S-25.
           GO  TO  M-25.
       M-40.
           PERFORM  S-20   THRU  S-25.
           PERFORM  S-30   THRU  S-40.
           GO  TO  M-20.
       M-80.
           PERFORM  S-20   THRU  S-25.
           PERFORM  S-30   THRU  S-40.
           PERFORM  S-45   THRU  S-55.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KEI-PRN_IDLST KEI-PRN_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *
       S-05.
           MOVE SPACE    TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD  1        TO W-PAGE.
           MOVE W-PAGE   TO H-PAGE.
           MOVE SPACE    TO SP-R.
           MOVE HEAD1    TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE    TO SP-R.
           MOVE HEAD2    TO SP-R.
           CALL "PR_LineFeed" USING "5" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE    TO SP-R.
       S-15.
           EXIT.
       S-20.
           MOVE W-KEI    TO P-KEI.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  59
               PERFORM  S-05   THRU  S-15
           END-IF.
           MOVE SPACE    TO SP-R.
           MOVE W-P      TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE    TO SP-R.
       S-25.
           EXIT.
       S-30.
           MOVE SPACE        TO W-P.
           MOVE "　　（　小　計　）　" TO P-KANA.
           MOVE  WS-GYK        TO  P-GYK.
           MOVE  WS-TGT        TO  P-TGT.
           MOVE  WS-ETC        TO  P-ETC.
           MOVE  WS-KEI        TO  P-KEI.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  59
               PERFORM  S-05   THRU  S-15
           END-IF.
           MOVE SPACE    TO SP-R.
           MOVE W-P      TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE    TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD   WS-GYK        TO  WA-GYK.
           ADD   WS-TGT        TO  WA-TGT.
           ADD   WS-ETC        TO  WA-ETC.
           ADD   WS-KEI        TO  WA-KEI.
       S-40.
           EXIT.
       S-45.
           MOVE SPACE        TO W-P.
           MOVE "　【　総　合　計　】" TO P-KANA.
           MOVE  WA-GYK        TO  P-GYK.
           MOVE  WA-TGT        TO  P-TGT.
           MOVE  WA-ETC        TO  P-ETC.
           MOVE  WA-KEI        TO  P-KEI.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  59
               PERFORM  S-05   THRU  S-15
           END-IF.
           MOVE SPACE    TO SP-R.
           MOVE W-P      TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-55.
           EXIT.
       CLSE-ENT.
       CLSE-EXT.
           EXIT.
           COPY LPMSG_PR.

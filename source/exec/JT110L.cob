       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT110L.
       AUTHOR.                        E-SHIGIHARA.
      ***************************************************
      *    PROGRAM        : 出荷日報（得意先別）　      *
      *    DATA WRITTEN   : 87/08/05                    *
      *    SCREEN USED    : UNUSED                      *
      *    FORM   USED    : UNUSED                      *
      *    PRINTER TYPE   : JIPS                        *
      *    COMPILE TYPE   : COBOL                       *
      *    JS-SIGN        : 1=指図 , 2=日報 , 3=指図残  *
      *                   : 4=日報                      *
      ***************************************************
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM150.
       OBJECT-COMPUTER.               SYSTEM150.
       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77  ERR-STAT                  PIC X(02)    VALUE SPACE.
       77  W-OKC                     PIC 9(01)    VALUE ZERO.
       77  JS-SIGN                   PIC 9(01).
       77  END-SW                    PIC 9(01).
       77  WK0256ID                  PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1               PIC  X(003).
           02  STN-NO2               PIC  X(003).
       01  W-FID.
           02  W-FID1                PIC  X(006) VALUE "WK0256".
           02  W-FID2                PIC  X(003).
       01  MID1-R.
           02  M1-K2                 PIC X(05)    VALUE X"1A24212474".
           02  M1-SEN                PIC N(05).
           02  M1-D1.
               03  FILLER            PIC X(03).
               03  M1-SOU            PIC N(06).
               03  FILLER            PIC X(14).
           02  M1-D2    REDEFINES M1-D1.
               03  FILLER            PIC X(02).
               03  M1-DATE           PIC X(24).
               03  FILLER            PIC X(03).
           02  FILLER                PIC N(04)    VALUE "＊＊＊　".
           02  M1-DAI                PIC N(15).
           02  FILLER                PIC N(04)    VALUE "　＊＊＊".
           02  FILLER                PIC X(14)    VALUE   SPACE.
           02  FILLER                PIC X(05)    VALUE   "DATE.".
           02  M1-01                 PIC 9(02).
           02  FILLER                PIC X(01)    VALUE   "/".
           02  M1-02                 PIC 9(02).
           02  FILLER                PIC X(01)    VALUE   "/".
           02  M1-03                 PIC 9(02).
           02  FILLER                PIC X(15)    VALUE   SPACE.
           02  FILLER                PIC X(05)    VALUE   "PAGE.".
           02  M1-04                 PIC ZZZ9.
       01  MID2-R.
           02  M2-K1                 PIC X(05)  VALUE X"1A24212078".
           02  FILLER                PIC X(05)    VALUE   "ｺｰﾄﾞ ".
           02  FILLER                PIC N(08)  VALUE
                   "得　意　先　名　".
           02  FILLER                PIC X(42)    VALUE   SPACE.
           02  FILLER                PIC X(01)    VALUE   "1".
           02  FILLER                PIC X(02)    VALUE   SPACE.
           02  FILLER                PIC N(02)    VALUE   "３号".
           02  FILLER                PIC X(02)    VALUE   SPACE.
           02  FILLER                PIC N(02)    VALUE   "２号".
           02  FILLER                PIC X(02)    VALUE   SPACE.
           02  FILLER                PIC N(02)    VALUE   "１号".
           02  FILLER                PIC X(02)    VALUE   SPACE.
           02  FILLER                PIC N(02)    VALUE   "０号".
           02  FILLER                PIC X(02)    VALUE   SPACE.
           02  FILLER                PIC N(02)    VALUE   "　中".
           02  FILLER                PIC X(02)    VALUE   SPACE.
           02  FILLER                PIC N(02)    VALUE   "　大".
           02  FILLER                PIC X(02)    VALUE   SPACE.
           02  FILLER                PIC N(02)    VALUE   "特大".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "28.0".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "29.0".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "30.0".
           02  FILLER                PIC X(05)    VALUE   SPACE.
           02  FILLER                PIC N(02)    VALUE   "　計".
           02  FILLER                PIC X(18)    VALUE   SPACE.
       01  MID3-R.
           02  M3-K1                 PIC X(05)  VALUE X"1A24212078".
           02  FILLER                PIC N(02)    VALUE   "伝区".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC N(04)    VALUE   "　指図№".
           02  FILLER                PIC X(06)    VALUE   " ｺｰﾄﾞ ".
           02  FILLER                PIC N(08)    VALUE
                       "直　送　先　名　".
           02  FILLER                PIC X(31)    VALUE   SPACE.
           02  FILLER                PIC X(01)    VALUE   "2".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "12.5".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "13.0".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "13.5".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "14.0".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "15.0".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "16.0".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "17.0".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "18.0".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "19.0".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "20.0".
           02  FILLER                PIC X(26)    VALUE   SPACE.
       01  MID4-R.
           02  M4-K1                 PIC X(05)  VALUE X"1A24212078".
           02  FILLER                PIC X(08)    VALUE   SPACE.
           02  FILLER                PIC X(01)    VALUE   "(".
           02  FILLER                PIC N(04)    VALUE   "　受注№".
           02  FILLER                PIC X(05)    VALUE   SPACE.
           02  FILLER                PIC X(01)    VALUE   ")".
           02  FILLER                PIC X(02)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "ｺｰﾄﾞ".
           02  FILLER                PIC X(02)    VALUE   SPACE.
           02  FILLER                PIC N(08)    VALUE
                        "品　　　　　名　".
           02  FILLER                PIC X(18)    VALUE   SPACE.
           02  FILLER                PIC X(01)    VALUE   "3".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "21.0".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "21.5".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "22.0".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "22.5".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "23.0".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "23.5".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "24.0".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "24.5".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "25.0".
           02  FILLER                PIC X(31)    VALUE   SPACE.
       01  MID5-R.
           02  FILLER                PIC X(59)    VALUE   SPACE.
           02  FILLER                PIC X(01)    VALUE   "4".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "24.0".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "24.5".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "25.0".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "25.5".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "26.0".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "26.5".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "27.0".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   "27.5".
           02  FILLER                PIC X(18)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  M5-01                 PIC N(02)    VALUE  "　　".
           02  FILLER                PIC X(05)    VALUE   SPACE.
           02  M5-02                 PIC N(04)    VALUE  "　　　　".
       01  ACT-WORK.
           02  OLD-1                 PIC 9(04)    VALUE   ZERO.
           02  OLD-2                 PIC 9(06)    VALUE   ZERO.
           02  W-TOT                PIC S9(06)    VALUE   ZERO.
           02  OLD-BIKO1            PIC N(09)     VALUE   ALL "　".
           02  OLD-BIKO2            PIC N(23)     VALUE   ALL "　".
           02  OLD-UNSO             PIC N(06)     VALUE   ALL "　".
           02  OLD-SEN              PIC 9(01)     VALUE 0.
           02  OLD-KCD              PIC 9(01)     VALUE 0.
           02  W-KOSU               PIC S9(03)    VALUE 0.
           02  OLD-ONO              PIC 9(06)     VALUE 0.
           02  OLD-OKNO             PIC 9(06)     VALUE 0.
           02  NEW-OKNO             PIC 9(06)     VALUE 0.
       01  W-AREA.
           02  LCNT                  PIC 9(02)   VALUE  90.
           02  PCNT                  PIC 9(04)   VALUE  ZERO.
           02  W-SEN                 PIC 9(01)   VALUE  ZERO.
           02  S-YMD                 PIC 9(06).
           02  S-YMDR    REDEFINES   S-YMD.
               03  SYY               PIC 9(02).
               03  SMM               PIC 9(02).
               03  SDD               PIC 9(02).
           02  W-HCD.
               03  W-HCD1            PIC 9(04).
               03  W-HCD2            PIC 9(02).
           02  W-NGP                 PIC 9(08).
           02  W-NGPD    REDEFINES   W-NGP.
               03  F                 PIC 9(02).
               03  W-NEN             PIC 9(02).
               03  W-GET             PIC 9(02).
               03  W-PEY             PIC 9(02).
           02  W-TAN                 PIC S9(05).
           02  W-KIN                 PIC S9(08).
           02  W-TKIN                PIC S9(08).
           02  W-AKIN                PIC S9(08).
       01  W-YMD.
           02  WYY                   PIC 9(02).
           02  WMM                   PIC 9(02).
           02  WDD                   PIC 9(02).
       01  K-1                     PIC X(05)  VALUE  X"1A24212078".
       01  K-2                     PIC X(05)  VALUE  X"1A24212474".
       01  W-DM                      PIC N(11).
       01  W-MID                     PIC N(05).
       01  H-DATE.
           02  F                     PIC N(02)  VALUE "《　".
           02  H-NEN                 PIC 9(02).
           02  F                     PIC N(01)  VALUE "年".
           02  F                     PIC X(01)  VALUE SPACE.
           02  H-GET                 PIC Z(02).
           02  F                     PIC N(01)  VALUE "月".
           02  F                     PIC X(01)  VALUE SPACE.
           02  H-PEY                 PIC Z(02).
           02  F                     PIC N(04)  VALUE "日分　》".
       COPY    LWMSG.
      *
           COPY  LJWTOK.
           COPY  LITCM.
           COPY  LIHIM2.
           COPY  L-JCON.
           COPY  LITHTM.
           COPY  LOKJF.
      *FD  P-F
       01  P-R.
           02  R1-R.
               03  R1-K1                 PIC X(05).
               03  R1-02                 PIC 9(04).
               03  F                     PIC X(01).
               03  R1-01                 PIC N(26).
               03  R1-K2                 PIC X(05).
               03  F                     PIC X(189).
           02  R2-R REDEFINES R1-R.
               03  R2-K1                 PIC X(05).
               03  R2-01                 PIC N(02).
               03  FILLER                PIC X(01).
               03  R2-02                 PIC 9(06).
               03  FILLER                PIC X(02).
               03  R2-06                 PIC 9(03).
               03  F                     PIC X(01).
               03  R2-03                 PIC N(26).
               03  FILLER                PIC X(01).
               03  R2-071                PIC 9(02).
               03  R2-07S1               PIC X(01).
               03  R2-072                PIC Z(02).
               03  R2-07S2               PIC X(01).
               03  R2-073                PIC Z(02).
               03  FILLER                PIC X(04).
               03  R2-M1                 PIC X(02).
               03  R2-04                 PIC ZZ9.
               03  FILLER                PIC X(03).
               03  R2-M2                 PIC N(02).
               03  R2-M3                 PIC X(01).
               03  R2-05                 PIC 9(06).
               03  FILLER                PIC X(04).
               03  R2-M4                 PIC N(04).
               03  R2-08                 PIC \\\\\,\\\.
               03  R2-K2                 PIC X(05).
               03  F                     PIC X(124).
           02  R3-R REDEFINES R1-R.
               03  R3-K1                 PIC X(05).
               03  FILLER                PIC X(01).
               03  R3-01                 PIC X(01).
               03  R3-021                PIC 9(06).
               03  R3-02V                PIC X(01).
               03  R3-022                PIC 9(01).
               03  R3-04                 PIC N(02).
               03  R3-05                 PIC X(01).
               03  FILLER                PIC X(01).
               03  R3-19                 PIC 9(06).
               03  F                     PIC X(01).
               03  R3-06                 PIC N(24).
               03  FILLER                PIC X(01).
               03  R3-07                 PIC 9(01).
               03  R3-08                 PIC -(05).
               03  R3-09                 PIC -(05).
               03  R3-10                 PIC -(05).
               03  R3-11                 PIC -(05).
               03  R3-12                 PIC -(05).
               03  R3-13                 PIC -(05).
               03  R3-14                 PIC -(05).
               03  R3-15                 PIC -(05).
               03  R3-16                 PIC -(05).
               03  R3-17                 PIC -(05).
               03  R3-18                 PIC ----,--9.
               03  R3-D1.
                 04  FILLER              PIC X(01).
                 04  R3-20               PIC X(10).
                 04  FILLER              PIC X(07).
               03  R3-D2    REDEFINES R3-D1.
                 04  R3-21               PIC ---,---.
                 04  R3-22               PIC ---,---,---.
               03  R3-K2                 PIC X(05).
               03  F                     PIC X(97).
           02  R4-R REDEFINES R1-R.
               03  R4-K1                 PIC X(05).
               03  FILLER                PIC X(32).
               03  R4-0                  PIC N(06).
               03  FILLER                PIC X(02).
               03  R4-A                  PIC N(10).
               03  R4-B                  PIC N(24).
               03  FILLER                PIC X(03).
               03  R4-C                  PIC ---9.
               03  R4-D                  PIC N(02).
               03  FILLER                PIC X(03).
               03  R4-01                 PIC N(02).
               03  R4-02                 PIC ----,--9.
               03  FILLER                PIC X(07).
               03  R4-03                 PIC ---,---,---.
               03  R4-K2                 PIC X(05).
               03  F                     PIC X(88).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLE-01.
           02  FILLER  PIC X(12) VALUE "CLEAR SCREEN".
       01  DSP-AREA.
           02  DSP-01.
               03  FILLER  PIC X(24) VALUE  "                        ".
               03  DSM-011 PIC N(11).
               03  DSM-012 PIC N(05).
       COPY    LSMSG.
      *
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "P1-999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "CLE-01" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLE-01" "X" "1" "0" "12" " " "CLE-01" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "56" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "56" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "RX" "1" "20" "24" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSM-011" "N" "1" "21" "22" "01DSP-01" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSM-011" BY REFERENCE W-DM "22" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSM-012" "N" "1" "1" "10" "DSM-011" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSM-012" BY REFERENCE W-MID "10" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ************************************
      *    メイン　ルーチン              *
      ************************************
       HAJIME.
           PERFORM   INT-RTN   THRU  INT-EX.
           MOVE    0        TO    END-SW.
       MR010.
      *           READ    JWTOK   NEXT    UNLOCK   AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JWTOK_PNAME1 BY REFERENCE JWTOK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   1     TO   END-SW
               PERFORM      TOT-RTN   THRU    TOT-EX
               GO  TO  MR999
           END-IF
           MOVE  JWTOK-06  TO  HI-MHCD HI-HCD  W-HCD.
      *           READ  HI2-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  MR010
           END-IF
           IF (JWTOK-011 NOT  =  OLD-1)  OR  (JWTOK-031 NOT  =  OLD-2)
              OR (JWTOK-14  NOT =  OLD-SEN) OR (JWTOK-12 NOT = OLD-KCD)
               PERFORM  TOT-RTN  THRU   TOT-EX
           END-IF
           IF  LCNT       NOT  <  62
               PERFORM  HED-RTN  THRU   HED-EX
           END-IF.
       MR020.
           PERFORM     PRI-RTN    THRU   PRI-EX.
           MOVE    JWTOK-14    TO  OLD-SEN.
           MOVE    JWTOK-12    TO  OLD-KCD.
           GO  TO  MR010.
       MR999.
           PERFORM     END-RTN    THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
      *********************************************
      *    ＩＮＴ－ＲＴＮ                         *
      *********************************************
       INT-RTN.
           ACCEPT     JS-SIGN FROM ARGUMENT-VALUE.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JWTOK_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JWTOK_PNAME1 "SHARED" BY REFERENCE JWTOK_IDLST "0".
      *           READ    JWTOK   NEXT    UNLOCK   AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JWTOK_PNAME1 BY REFERENCE JWTOK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JWTOK_IDLST JWTOK_PNAME1
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE  JWTOK-JS  TO  IPN-KYO-KBN.
           CALL "DB_F_Close" USING
            BY REFERENCE JWTOK_IDLST JWTOK_PNAME1.
      *
           IF  JS-SIGN  =  1
               MOVE  "出荷指図リスト　　　　"  TO  W-DM
           ELSE
               IF  JS-SIGN  =  2  OR  4
                   MOVE  "出荷日報（得意先別）　"  TO  W-DM
               ELSE
                   MOVE  "出荷指図残リスト　　　"  TO  W-DM
               END-IF
           END-IF
           IF  IPN-KYO-KBN  =  0
               MOVE  "〔教　育〕"   TO  W-MID
           END-IF
           IF  IPN-KYO-KBN  =  1
               MOVE  "〔一　般〕"   TO  W-MID
           END-IF.
      *
       INT-050.
           ACCEPT     W-YMD     FROM    DATE.
           INITIALIZE     ACT-WORK.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" JWTOK_PNAME1 "SHARED" BY REFERENCE JWTOK_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON3-KEY" BY REFERENCE JCON3-KEY.
           CALL "PR_Open" RETURNING RESP.
           IF  JS-SIGN        =  1
               CALL "DB_F_Open" USING
                "INPUT" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
                "OKJF-KEY" BY REFERENCE OKJF-KEY
           END-IF
           IF  JS-SIGN   NOT  =  4
               GO  TO  INT-EX
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
           MOVE  SPACE    TO  JCON6-KEY.
           MOVE  6        TO  JCON6-01.
      *           READ  JCON     UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ZERO      TO  JCON6-09
           END-IF
           MOVE  JCON6-09    TO  W-NGP.
           MOVE  W-NEN       TO  H-NEN.
           MOVE  W-GET       TO  H-GET.
           MOVE  W-PEY       TO  H-PEY.
           MOVE  ZERO        TO  W-AKIN.
           MOVE  "単価"    TO  M5-01.
           MOVE  "　金　額"   TO  M5-02.
       INT-EX.
           EXIT.
      *********************************************
      *    ＥＮＤ－ＲＴＮ                         *
      *********************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JWTOK_IDLST JWTOK_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           IF  JS-SIGN        =  1
               CALL "DB_F_Close" USING
                BY REFERENCE OKJF_IDLST OKJF_PNAME1
           END-IF
           IF  JS-SIGN        =  4
               CALL "DB_F_Close" USING
                BY REFERENCE THTM_IDLST THTM_PNAME1
           END-IF
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
       END-EX.
           EXIT.
      **********************************************
      *    ＨＥＤ－ＲＴＮ                          *
      **********************************************
       HED-RTN.
           MOVE SPACE  TO  P-R.
           IF  LCNT    NOT =  90
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           ADD     1   TO  PCNT.
           IF  OLD-SEN   =   0
               MOVE   "【教　育】"  TO   M1-SEN
           END-IF
           IF  OLD-SEN   =   1
               MOVE   "【ワーク】"  TO   M1-SEN
           END-IF
           IF  OLD-SEN   =   2
               MOVE   "【カジュ】"  TO   M1-SEN
           END-IF
           MOVE    SPACE  TO  M1-D1.
           IF  JS-SIGN      =  4
               MOVE  H-DATE    TO  M1-DATE
               GO  TO  HED-010
           END-IF
           IF  JS-SIGN  NOT =  1
               GO  TO  HED-010
           END-IF
           MOVE    3      TO  JCON3-01.
           MOVE    OLD-KCD    TO  JCON3-02.
      *           READ    JCON   UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   SPACE   TO  JCON3-03
           END-IF
           MOVE    JCON3-03   TO  M1-SOU.
       HED-010.
           IF  JS-SIGN  =  1
               MOVE  "出　荷　指　図　リ　ス　ト　　"   TO  M1-DAI
           ELSE
               IF  JS-SIGN  =  2  OR  4
                   MOVE  "　　　　出　荷　日　報　　　　"  TO  M1-DAI
               ELSE
                   MOVE  "出　荷　指　図　残　リ　ス　ト"  TO  M1-DAI
               END-IF
           END-IF
      *
           MOVE    WYY    TO  M1-01.
           MOVE    WMM    TO  M1-02.
           MOVE    WDD    TO  M1-03.
           MOVE   PCNT    TO  M1-04.
           MOVE    MID1-R TO  P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE  TO  P-R.
           MOVE    MID2-R  TO  P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE TO  P-R.
           MOVE    MID3-R  TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE TO  P-R.
           MOVE    MID4-R  TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE TO  P-R.
           MOVE    MID5-R  TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE TO  P-R.
           MOVE     8   TO   LCNT.
       HED-EX.
           EXIT.
      ***************************************************
      *    ＰＲＩ－ＲＴＮ                               *
      ***************************************************
       PRI-RTN.
           MOVE    SPACE   TO  P-R.
           IF  (LCNT  = 7) OR (OLD-1 NOT = JWTOK-011)
               GO  TO  PRI-020
           END-IF.
       PRI-010.
           IF  (LCNT  =  8) OR (OLD-1 NOT = JWTOK-011)
                            OR (OLD-2 NOT = JWTOK-031)
               GO  TO  PRI-030
           END-IF
           GO  TO  PRI-040.
       PRI-020.
           MOVE   K-1      TO  R1-K1.
           MOVE  JWTOK-011 TO  TC-TCD.
           MOVE   "001"    TO  TC-CCD.
      *           READ   TC-M      UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ALL "　"   TO    TC-NAME
           END-IF
           MOVE  JWTOK-011 TO  R1-02.
           MOVE   TC-NAME  TO  R1-01.
           MOVE   K-2      TO  R1-K2.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           ADD    1        TO  LCNT.
           GO  TO  PRI-010.
       PRI-030.
           MOVE   K-1       TO    R2-K1.
           MOVE  ALL "　" TO    R2-01  R2-M4.
           IF  JWTOK-02    =  "0"
               MOVE    "出荷"      TO  R2-01
           END-IF
           IF  JWTOK-02    =  "3"
               MOVE    "訂正"      TO  R2-01
           END-IF
           IF  JWTOK-02    =  "5"
               MOVE    "返品"      TO  R2-01
           END-IF
           IF  JWTOK-02    =  "6"
               MOVE    "不良"      TO  R2-01
           END-IF
           MOVE   JWTOK-031  TO  R2-02.
           MOVE   JWTOK-011  TO  TC-TCD.
           MOVE   JWTOK-012  TO  TC-CCD.
      *           READ   TC-M      UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ALL "　"  TO   TC-NAME
           END-IF
           MOVE   JWTOK-012 TO    R2-06.
           MOVE   TC-NAME   TO    R2-03.
           MOVE   JWTOK-091 TO    R2-071.
           MOVE   JWTOK-092 TO    R2-072.
           MOVE   JWTOK-093 TO    R2-073.
           MOVE   "/"       TO    R2-07S1  R2-07S2.
           IF  JWTOK-15    NOT =  ZERO
               MOVE   "S="      TO    R2-M1
               MOVE   JWTOK-15  TO    R2-04
           END-IF
           MOVE   "送№"  TO    R2-M2.
           MOVE   "="       TO    R2-M3.
           MOVE   JWTOK-17  TO    R2-05.
      *
           IF  JS-SIGN  NOT =  1
               GO  TO  PRI-035
           END-IF
           IF  JWTOK-13 NOT =  6
               GO  TO  PRI-035
           END-IF
           IF  JWTOK-17     =  OLD-ONO
               GO  TO  PRI-035
           END-IF
           MOVE   JWTOK-17  TO    OLD-ONO.
           MOVE   JWTOK-17  TO    OKJF-KEY.
      *           READ   OKJF      UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" OKJF_PNAME1 BY REFERENCE OKJF-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ZERO           TO    OKJF-12
           END-IF
           MOVE   "代引金額"  TO    R2-M4.
           MOVE   OKJF-12       TO    R2-08.
       PRI-035.
           MOVE   K-2       TO    R2-K2.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           ADD    1        TO  LCNT.
       PRI-040.
           MOVE    K-1      TO    R3-K1.
           MOVE   "　"  TO    R3-04.
           IF  JWTOK-051   NOT  =  ZERO
               MOVE    "("      TO    R3-01
               MOVE  JWTOK-051  TO    R3-021
               MOVE    "-"      TO    R3-02V
               MOVE  JWTOK-052  TO    R3-022
               MOVE    ")"      TO    R3-05
               IF  JWTOK-04  =  4 OR 5
                   MOVE    "預"   TO    R3-04
                   IF  JWTOK-04  =  6
                       MOVE    "取"   TO    R3-04
                   END-IF
               END-IF
           END-IF
           MOVE    JWTOK-06 TO    HI-MHCD HI-HCD  W-HCD.
      *           READ    HI2-M    UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ZERO         TO  HI-SB
               MOVE  ALL "　" TO  HI-NAME
           END-IF
           MOVE    JWTOK-06 TO    R3-19.
           MOVE    HI-NAME  TO    R3-06.
           MOVE   JWTOK-07  TO    R3-07.
           MOVE   JWTOK-0811(1) TO    R3-08.
           MOVE   JWTOK-0811(2) TO    R3-09.
           MOVE   JWTOK-0811(3) TO    R3-10.
           MOVE   JWTOK-0811(4) TO    R3-11.
           MOVE   JWTOK-0811(5) TO    R3-12.
           MOVE   JWTOK-0811(6) TO    R3-13.
           MOVE   JWTOK-0811(7) TO    R3-14.
           MOVE   JWTOK-0811(8) TO    R3-15.
           MOVE   JWTOK-0811(9) TO    R3-16.
           MOVE   JWTOK-0811(10) TO    R3-17.
           MOVE   JWTOK-082  TO    R3-18.
           IF  JS-SIGN     NOT  =  4
               MOVE   JWTOK-20   TO    R3-20
               GO  TO  PRI-060
           END-IF
           MOVE    ZERO       TO  W-TAN  W-KIN.
           IF  JWTOK-051   NOT  =  ZERO
               IF  JWTOK-04  =  4 OR 5
                   GO  TO  PRI-060
               END-IF
           END-IF
           MOVE   SPACE      TO    THT-KEY.
           MOVE   JWTOK-011  TO    THT-TCD.
           MOVE   JWTOK-06   TO    THT-HCD.
           MOVE   JWTOK-07   TO    THT-SIZ.
      *           START  THTM    KEY  NOT <  THT-KEY     INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            THTM_PNAME1 "THT-KEY" " NOT < " THT-KEY RETURNING RET.
           IF  RET = 1
               MOVE    HI-SB      TO  W-TAN
               GO  TO  PRI-057
           END-IF.
       PRI-055.
      *           READ   THTM    NEXT RECORD  WITH UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE    HI-SB      TO  W-TAN
               GO  TO  PRI-057
           END-IF
           IF (JWTOK-011  NOT = THT-TCD)  OR
              (JWTOK-06   NOT = THT-HCD)
               MOVE    HI-SB      TO  W-TAN
               GO  TO  PRI-057
           END-IF
           IF  THT-SIZ        =  9
               MOVE    THT-T      TO  W-TAN
               GO  TO  PRI-057
           END-IF
           IF  JWTOK-07   NOT = THT-SIZ
               GO  TO  PRI-055
           END-IF
           MOVE    THT-T      TO  W-TAN.
       PRI-057.
           COMPUTE  W-KIN    =   JWTOK-082  *  W-TAN.
           MOVE   W-TAN      TO    R3-21.
           MOVE   W-KIN      TO    R3-22.
       PRI-060.
           MOVE   K-2        TO    R3-K2.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE      TO  P-R.
           ADD     1       TO  LCNT.
           IF  W-HCD1      NOT =  9999
               ADD     JWTOK-082  TO  W-TOT
           END-IF
           IF  JS-SIGN          =  4
               ADD     W-KIN      TO  W-TKIN
           END-IF
           MOVE    JWTOK-011  TO  OLD-1.
           MOVE    JWTOK-031  TO  OLD-2.
           MOVE    JWTOK-10   TO  OLD-BIKO1.
           MOVE    JWTOK-11   TO  OLD-BIKO2.
           MOVE    JWTOK-16   TO  W-KOSU
           MOVE    JWTOK-17   TO  NEW-OKNO.
           MOVE    2          TO  JCON2-01.
           MOVE    JWTOK-13   TO  JCON2-02.
      *           READ    JCON  UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE    TO    JCON2-03
           END-IF
           MOVE    JCON2-03   TO  OLD-UNSO.
       PRI-EX.
           EXIT.
      ***************************************************
      *    ＴＯＴーＲＴＮ                               *
      ***************************************************
       TOT-RTN.
           IF (LCNT  =   90)
               IF  END-SW  =  1
                   GO  TO  TOT-EX
               ELSE
                   MOVE    JWTOK-14     TO     OLD-SEN
                   MOVE    JWTOK-12     TO     OLD-KCD
                   GO  TO  TOT-EX
               END-IF
           END-IF
           IF  LCNT  NOT   <   62
               PERFORM   HED-RTN   THRU   HED-EX
           END-IF
           MOVE   SPACE   TO  P-R.
           MOVE   K-1     TO  R4-K1.
           MOVE   ALL "　"  TO    R4-0  R4-A  R4-B  R4-D  R4-01.
           MOVE   OLD-UNSO      TO    R4-0.
           MOVE   OLD-BIKO1     TO    R4-A.
           MOVE   OLD-BIKO2     TO    R4-B.
           IF  NEW-OKNO  NOT =  OLD-OKNO
               MOVE   W-KOSU        TO    R4-C
               MOVE   "個口"      TO    R4-D
           END-IF
           MOVE   "合計"  TO    R4-01.
           MOVE   W-TOT         TO    R4-02.
           IF  JS-SIGN          =  4
               MOVE   W-TKIN        TO    R4-03
           END-IF
           MOVE   K-2           TO    R4-K2.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE   TO  P-R.
           ADD    1       TO  LCNT.
           IF  JS-SIGN          =  4
               ADD    W-TKIN        TO    W-AKIN
           END-IF
           IF  END-SW  =  1
               GO  TO  TOT-010
           END-IF
           IF  (OLD-SEN  NOT =  JWTOK-14)
               OR  (OLD-KCD  NOT =  JWTOK-12)
               MOVE    JWTOK-14     TO     OLD-SEN
               MOVE    JWTOK-12     TO     OLD-KCD
               MOVE    70     TO    LCNT
           END-IF
           IF  LCNT   NOT    <   60
               PERFORM   HED-RTN   THRU   HED-EX
           END-IF
           MOVE   ZERO      TO     W-TOT  W-TKIN.
           MOVE   NEW-OKNO  TO     OLD-OKNO.
           GO  TO  TOT-EX.
       TOT-010.
           IF  JS-SIGN     NOT =   4
               GO  TO  TOT-EX
           END-IF
           IF  LCNT  NOT   <   62
               PERFORM   HED-RTN   THRU   HED-EX
           END-IF
           MOVE   SPACE   TO  P-R.
           MOVE   K-1     TO  R4-K1.
           MOVE   ALL "　"  TO    R4-0  R4-A  R4-B  R4-D  R4-01.
           MOVE   "　　　　　　　　　　［　合　計　］"   TO    R4-B.
           MOVE   W-AKIN        TO    R4-03.
           MOVE   K-2           TO    R4-K2.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
       TOT-EX.
           EXIT.
       COPY    LPACPT.
       COPY    LPMSG.

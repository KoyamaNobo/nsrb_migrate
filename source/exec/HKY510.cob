       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKY510.
      *******************************************************
      *****     履物・工品　売上消費税ワーク　作成     ******
      *****       JS-SIGN   0:STRANYR  1:URIRYR    　　******
      *******************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SNENL REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-ENG.
             03  W-ENEN       PIC  9(004).
             03  W-ENENL REDEFINES W-ENEN.
               04  W-ENEN1    PIC  9(002).
               04  W-ENEN2    PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-ENGL  REDEFINES W-ENG.
             03  F            PIC  9(002).
             03  W-ENGS       PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  W-TD.
             03  W-KIN        PIC S9(010).
             03  W-SHZ        PIC S9(009).
           02  W-DNO          PIC  9(006).
           02  W-TCD          PIC  9(004).
           02  W-NG           PIC  9(006).
           02  W-DC           PIC  9(001).
           02  W-ZC           PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
      *FD  STRAN
       01  STRAN_HKY510.
           02  STRAN_PNAME1   PIC  X(007) VALUE "STRANYR".
           02  F              PIC  X(001).
           02  STRAN_LNAME    PIC  X(012) VALUE "STRAN_HKY510".
           02  F              PIC  X(001).
           02  STRAN_KEY1     PIC  X(100) VALUE SPACE.
           02  STRAN_SORT     PIC  X(100) VALUE SPACE.
           02  STRAN_IDLST    PIC  X(100) VALUE SPACE.
           02  STRAN_RES      USAGE  POINTER.
       01  STRAN-R.
           02  ST-DNO         PIC  9(006).
           02  ST-GNO         PIC  9(001).
           02  ST-DATE        PIC  9(008).
           02  ST-DATED REDEFINES ST-DATE.
             03  ST-NG        PIC  9(006).
             03  F            PIC  9(002).
           02  ST-TCD         PIC  9(004).
           02  ST-D1.
             03  ST-HCD       PIC  9(006).
             03  F            PIC  X(031).
             03  ST-SU        PIC S9(005).
             03  ST-T         PIC S9(005).
             03  ST-KIN       PIC S9(008).
             03  ST-CSC       PIC  9(001).
             03  ST-DC        PIC  9(001).
             03  ST-FT        PIC  9(005).
             03  ST-CCD       PIC  9(003).
             03  F            PIC  X(007).
             03  ST-TNC       PIC  9(002).
             03  F            PIC  X(002).
             03  ST-ZC        PIC  9(001).
             03  F            PIC  X(031).
           02  ST-D2    REDEFINES ST-D1.
             03  ST-BI        PIC  N(024).
             03  ST-HNO       PIC  9(006).
             03  F            PIC  X(030).
             03  ST-SHZ       PIC S9(007).
             03  F            PIC  X(017).
           02  ST-SNC         PIC  9(001).
       77  F                  PIC  X(001).
      *FD  URIRYR
       01  URIRYR_HKY510.
           02  URIRYR_PNAME1  PIC  X(006) VALUE "URIRYR".
           02  F              PIC  X(001).
           02  URIRYR_LNAME   PIC  X(013) VALUE "URIRYR_HKY510".
           02  F              PIC  X(001).
           02  URIRYR_KEY1    PIC  X(100) VALUE SPACE.
           02  URIRYR_SORT    PIC  X(100) VALUE SPACE.
           02  URIRYR_IDLST   PIC  X(100) VALUE SPACE.
           02  URIRYR_RES     USAGE  POINTER.
       01  URIRY-R.
           02  URIRY-DC       PIC  9(001).
           02  URIRY-NG       PIC  9(006).
           02  F              PIC  X(002).
           02  URIRY-TCD      PIC  9(004).
           02  URIRY-HCD      PIC  X(005).
           02  F              PIC  X(016).
           02  URIRY-KIN      PIC S9(008).
           02  F              PIC  X(013).
           02  URIRY-CSC      PIC  9(001).
           02  F              PIC  X(010).
           02  URIRY-DNO      PIC  9(006).
           02  F              PIC  X(054).
           02  URIRY-ZC       PIC  9(001).
           02  F              PIC  X(001).
       77  F                  PIC  X(001).
      *FD  URIAGE
       01  URIAGE_HKY510.
           02  URIAGE_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  URIAGE_LNAME   PIC  X(013) VALUE "URIAGE_HKY510".
           02  F              PIC  X(001).
           02  URIAGE_KEY1    PIC  X(100) VALUE SPACE.
           02  URIAGE_SORT    PIC  X(100) VALUE SPACE.
           02  URIAGE_IDLST   PIC  X(100) VALUE SPACE.
           02  URIAGE_RES     USAGE  POINTER.
       01  URIAGE-R.
           02  UR-NG          PIC  9(006).
           02  UR-BMC         PIC  9(002).
           02  UR-TCD         PIC  9(004).
           02  UR-NAME        PIC  N(026).
           02  UR-KIN0        PIC S9(010).
           02  UR-KIN5        PIC S9(010).
           02  UR-KIN8        PIC S9(010).
           02  UR-SHZ5        PIC S9(009).
           02  UR-SHZ8        PIC S9(009).
           02  UR-TNC         PIC  9(002).
           02  F              PIC  X(014).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　売上・消費税ワーク　作成　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(036) VALUE
                "抽出期間    '  年   月 ～ '  年   月".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER  PIC  N(007) VALUE
                "（　履　物　）".
       01  C-MID2.
           02  FILLER  PIC  N(007) VALUE
                "（　工　品　）".
       01  C-ACP.
           02  A-PNG.
             03  A-SNEN  PIC  9(002).
             03  A-SGET  PIC  9(002).
             03  A-ENEN  PIC  9(002).
             03  A-EGET  PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME31.
               04  FILLER  PIC  X(039) VALUE
                    "***  ﾃﾞﾝﾋﾟｮｳNO ｴﾗｰ (      ･      )  ***".
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  9(006).
             03  E-ME32.
               04  FILLER  PIC  X(037) VALUE
                    "***  ｾﾞｲｸﾌﾞﾝ ｴﾗｰ (      ･      )  ***".
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  9(006).
             03  E-TCD   PIC  9(004).
           COPY LSSEM.
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
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
            "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "14" "36" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "22" "22" "08C-MID" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "14" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "N" "5" "25" "14" " " "C-MID1" RETURNING RESU.
      *C-MID2
       CALL "SD_Init" USING 
            "C-MID2" " " "0" "0" "14" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID2" "N" "5" "25" "14" " " "C-MID2" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PNG" " " "12" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNEN" "9" "12" "27" "2" " " "A-PNG" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNEN" BY REFERENCE W-SNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SGET" "9" "12" "32" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENEN" "9" "12" "41" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENEN" BY REFERENCE W-ENEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EGET" "9" "12" "46" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "39" "1" "A-PNG" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "104" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "104" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME31" " " "24" "0" "51" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME31" "X" "24" "15" "39" " " "E-ME31" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME31" "9" "24" "35" "6" "01E-ME31" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME31" BY REFERENCE ST-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03E-ME31" "9" "24" "42" "6" "02E-ME31" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03E-ME31" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME32" " " "24" "0" "49" "E-ME31" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME32" "X" "24" "15" "37" " " "E-ME32" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME32" "9" "24" "33" "6" "01E-ME32" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME32" BY REFERENCE ST-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03E-ME32" "9" "24" "40" "6" "02E-ME32" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03E-ME32" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TCD" "9" "24" "57" "4" "E-ME32" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE ST-TCD "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU
           ELSE
               IF  JS-SIGN = 1
                   CALL "SD_Output" USING
                    "C-MID2" C-MID2 "p" RETURNING RESU
               END-IF
           END-IF
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           PERFORM S-10 THRU S-35.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO URIAGE_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "OUTPUT" URIAGE_PNAME1 " " BY REFERENCE URIAGE_IDLST "0".
           IF  JS-SIGN = 1
               GO TO M-50
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" STRAN_PNAME1 " " BY REFERENCE STRAN_IDLST "0".
       M-10.
      *           READ STRAN AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" STRAN_PNAME1 BY REFERENCE STRAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  ST-NG < W-SNG
               GO TO M-10
           END-IF
           IF  ST-NG > W-ENG
               GO TO M-90
           END-IF
           IF  ST-GNO = 9
               CALL "SD_Output" USING
                "E-ME31" E-ME31 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-85
           END-IF.
       M-15.
           MOVE ZERO TO W-TD.
           MOVE ST-DNO TO W-DNO.
           MOVE ST-ZC TO W-ZC.
           MOVE ST-DC TO W-DC.
       M-20.
           IF  ST-SNC = 1
               SUBTRACT ST-KIN FROM W-KIN
           ELSE
               IF  ST-DC = 0 OR 3 OR 7
                   ADD ST-KIN TO W-KIN
               ELSE
                   IF  ST-DC = 1 OR 2 OR 5
                       SUBTRACT ST-KIN FROM W-KIN
                   END-IF
               END-IF
           END-IF.
       M-25.
      *           READ STRAN AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" STRAN_PNAME1 BY REFERENCE STRAN-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME31" E-ME31 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-85
           END-IF
           IF  ST-NG < W-SNG
               GO TO M-25
           END-IF
           IF  ST-NG > W-ENG
               GO TO M-90
           END-IF
           IF  ST-GNO = 9
               GO TO M-30
           END-IF
           IF  ST-DNO NOT = W-DNO
               CALL "SD_Output" USING
                "E-ME31" E-ME31 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-85
           END-IF
           GO TO M-20.
       M-30.
           IF  ST-DNO NOT = W-DNO
               CALL "SD_Output" USING
                "E-ME31" E-ME31 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-85
           END-IF
           IF  ZERO = ST-SHZ AND W-KIN
               GO TO M-10
           END-IF
           IF  ST-SNC = 0
               IF  W-ZC NOT = 0
                   IF  ST-SHZ = ZERO
                       CALL "SD_Output" USING
                        "E-ME32" E-ME32 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-TCD" E-TCD "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME99" E-ME99 "p" RETURNING RESU
                       GO TO M-85
                   END-IF
               END-IF
           END-IF
           IF  ST-SNC = 0
               IF  W-ZC = 0
                   IF  ST-SHZ NOT = ZERO
                       CALL "SD_Output" USING
                        "E-ME32" E-ME32 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-TCD" E-TCD "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME99" E-ME99 "p" RETURNING RESU
                       GO TO M-85
                   END-IF
               END-IF
           END-IF
      *
           IF  ST-SNC = 1
               SUBTRACT ST-SHZ FROM W-SHZ
           ELSE
               ADD ST-SHZ TO W-SHZ
           END-IF
      *
           MOVE ST-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO T-BC T-DCC
           END-IF
      *
           MOVE ZERO TO URIAGE-R.
           MOVE ST-NG TO UR-NG.
           MOVE T-BC TO UR-BMC.
           MOVE ST-TCD TO UR-TCD.
           MOVE T-NAME TO UR-NAME.
           IF  W-ZC = 0
               MOVE W-KIN TO UR-KIN0
           ELSE
               IF  W-ZC = 5
                   MOVE W-KIN TO UR-KIN5
                   MOVE W-SHZ TO UR-SHZ5
               ELSE
                   IF  W-ZC = 8
                       MOVE W-KIN TO UR-KIN8
                       MOVE W-SHZ TO UR-SHZ8
                   END-IF
               END-IF
           END-IF
           MOVE T-TNC TO UR-TNC.
      *           WRITE URIAGE-R.
      *//////////////
           CALL "DB_Insert" USING
            URIAGE_PNAME1 URIAGE_LNAME URIAGE-R RETURNING RET.
           GO TO M-10.
      *-----------------------------------------------------------------
       M-50.
           CALL "DB_F_Open" USING
            "INPUT" URIRYR_PNAME1 " " BY REFERENCE URIRYR_IDLST "0".
       M-55.
      *           READ URIRYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" URIRYR_PNAME1 BY REFERENCE URIRY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  URIRY-NG < W-SNG
               GO TO M-55
           END-IF
           IF  URIRY-NG > W-ENG
               GO TO M-90
           END-IF
           IF  URIRY-DC = 4
               GO TO M-55
           END-IF.
       M-60.
           MOVE ZERO TO W-TD.
           MOVE URIRY-DNO TO W-DNO.
           MOVE URIRY-TCD TO W-TCD.
           MOVE URIRY-ZC TO W-ZC.
           MOVE URIRY-NG TO W-NG.
       M-65.
           IF  URIRY-DC = 9
               SUBTRACT URIRY-KIN FROM W-SHZ
           ELSE
               IF  URIRY-DC = 8
                   SUBTRACT URIRY-KIN FROM W-KIN
               ELSE
                   IF  URIRY-DC = 5
                       ADD URIRY-KIN TO W-SHZ
                   ELSE
                       ADD URIRY-KIN TO W-KIN
                   END-IF
               END-IF
           END-IF.
       M-70.
      *           READ URIRYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" URIRYR_PNAME1 BY REFERENCE URIRY-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               GO TO M-75
           END-IF
           IF  URIRY-NG < W-SNG
               GO TO M-70
           END-IF
           IF  URIRY-NG > W-ENG
               MOVE 1 TO W-END
               GO TO M-75
           END-IF
           IF  URIRY-DC = 4
               GO TO M-70
           END-IF
           IF  URIRY-DNO = W-DNO
               IF  URIRY-TCD = W-TCD
                   IF  URIRY-NG = W-NG
                       GO TO M-65
                   END-IF
               END-IF
           END-IF.
       M-75.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO T-BC T-DCC
           END-IF
      *
           MOVE ZERO TO URIAGE-R.
           MOVE W-NG TO UR-NG.
           MOVE T-BC TO UR-BMC.
           MOVE W-TCD TO UR-TCD.
           MOVE T-NAME TO UR-NAME.
           IF  W-ZC = 0
               MOVE W-KIN TO UR-KIN0
           ELSE
               IF  W-ZC = 5
                   MOVE W-KIN TO UR-KIN5
                   MOVE W-SHZ TO UR-SHZ5
               ELSE
                   IF  W-ZC = 8
                       MOVE W-KIN TO UR-KIN8
                       MOVE W-SHZ TO UR-SHZ8
                   END-IF
               END-IF
           END-IF
           MOVE T-TNC TO UR-TNC.
      *           WRITE URIAGE-R.
      *//////////////
           CALL "DB_Insert" USING
            URIAGE_PNAME1 URIAGE_LNAME URIAGE-R RETURNING RET.
           IF  W-END = 0
               GO TO M-60
           ELSE
               GO TO M-90
           END-IF.
       M-85.
           CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255.
       M-90.
           IF  JS-SIGN = 0
               CALL "DB_F_Close" USING
                BY REFERENCE STRAN_IDLST STRAN_PNAME1
           ELSE
               IF  JS-SIGN = 1
                   CALL "DB_F_Close" USING
                    BY REFERENCE URIRYR_IDLST URIRYR_PNAME1
               END-IF
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE URIAGE_IDLST URIAGE_PNAME1.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-10.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO S-35
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-10
           END-IF
           MOVE ZERO TO W-SNEN1.
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF
           IF  W-SNEN < 2014
               GO TO S-10
           END-IF.
       S-15.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO S-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-15
           END-IF
           IF  W-SGET < 1 OR > 12
               GO TO S-15
           END-IF.
       S-20.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO S-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-20
           END-IF
           MOVE ZERO TO W-ENEN1.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF
           IF  W-ENEN < W-SNEN
               GO TO S-20
           END-IF.
       S-25.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO S-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-25
           END-IF
           IF  W-ENG < W-SNG
               GO TO S-25
           END-IF
           IF  W-EGET < 1 OR > 12
               GO TO S-25
           END-IF.
       S-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO S-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-30
           END-IF
           IF  W-DMM = 9
               GO TO S-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO S-30
           END-IF.
       S-35.
           EXIT.

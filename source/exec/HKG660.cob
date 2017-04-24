       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG660.
      *************************************************************
      *    PROGRAM         :  生協請求サイズ別明細表              *
      *    PRINTER TYPE    :  *****                               *
      *    SCREEN          :  ******                              *
      *    COMPILE TYPE    :  COBOL                               *
      *    JS-SIGN         :  0=請求後 , 1=請求前(ﾁｪｯｸ用)         *
      *************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM100.
       OBJECT-COMPUTER. SYSTEM100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  H-MID          PIC  N(007) VALUE SPACE.
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  N(023) VALUE
                "＊＊＊　　生協　請求サイズ別　明細表　　＊＊＊".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(002) VALUE "【　".
           02  F              PIC  X(001) VALUE "'".
           02  H-NEN          PIC  9(002).
           02  F              PIC  N(002) VALUE "年".
           02  H-GET          PIC Z9.
           02  F              PIC  N(004) VALUE "月分　】".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(004) VALUE "得意先　".
           02  H-F            PIC  X(001) VALUE "(".
           02  H-TCD          PIC  9(004).
           02  H-R            PIC  X(001) VALUE ")".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-TNA          PIC  N(026).
           02  F              PIC  X(084) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "月日".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　伝票№".
           02  F              PIC  X(001) VALUE "-".
           02  F              PIC  N(002) VALUE "行　".
           02  F              PIC  X(004) VALUE "ｺ-ﾄﾞ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(006) VALUE "品　　　　名".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｻｲｽﾞ".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "区　分　".
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　単　価".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(042) VALUE SPACE.
       01  W-P1.
           02  P-GP1          PIC 99/99.
           02  F              PIC  X(001).
           02  P-DNO1         PIC  9(006).
           02  P-V            PIC  X(001).
           02  P-GNO          PIC  9(001).
           02  F              PIC  X(001).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  X(004).
           02  F              PIC  X(001).
           02  P-KBN          PIC  N(004).
           02  P-SU           PIC --,--9.
           02  P-T            PIC ---,--9.
           02  P-KIN          PIC ---,---,--9.
           02  F              PIC  X(042).
       01  W-P2.
           02  P-GP2          PIC 99/99.
           02  F              PIC  X(001).
           02  P-DNO2         PIC  9(006).
           02  F              PIC  X(031).
           02  P-CM           PIC  N(004).
           02  P-CC           PIC  X(001).
           02  P-CF           PIC  X(001).
           02  P-CCD          PIC  9(003).
           02  P-CR           PIC  X(001).
           02  P-CNA          PIC  N(026).
           02  F              PIC  X(002).
           02  P-TM           PIC  N(002).
           02  P-TC           PIC  X(001).
           02  P-TEK          PIC  N(024).
       01  W-DATA.
           02  W-D.
             03  W-TCD        PIC  9(004).
             03  W-CCD        PIC  9(004).
             03  CHK.
               04  CHK1       PIC  9(001).
               04  CHK2       PIC  9(001).
               04  CHK3       PIC  9(001).
             03  W-DNO        PIC  9(006).
             03  W-DC         PIC  9(001).
             03  CNT          PIC  9(002).
             03  W-C          PIC  9(002).
             03  W-SC         PIC  9(002).
             03  W-SCD        PIC  9(002).
             03  W-GP         PIC  9(004).
             03  W-SU         PIC S9(005).
             03  W-T          PIC S9(005).
             03  W-KIN        PIC S9(008).
             03  W-TD.
               04  W-TSU      PIC S9(004).
               04  W-TKIN     PIC S9(008).
             03  W-NGD.
               04  W-NEND     PIC  9(002).
               04  W-GETD     PIC  9(002).
             03  W-PAGE       PIC  9(002).
           02  W-ASMD.
             03  F            PIC  X(040) VALUE
                  "3ｺﾞｳ2ｺﾞｳ1ｺﾞｳ0ｺﾞｳ ﾁｭｳ ﾀﾞｲﾄｸﾀﾞ28.029.030.0".
             03  F            PIC  X(040) VALUE
                  "         SS   S   M   L  LL  XL  XXL    ".
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.0    ".
             03  F            PIC  X(040) VALUE
                  "24.024.525.025.526.026.527.027.5        ".
           02  W-ASD.
             03  W-SD    OCCURS  50.
               04  W-S        PIC  X(004).
           02  W-MID          PIC  N(007) VALUE "（チェック用）".
       01  W-AR.
           02  W-ARD   OCCURS  6.
             03  W-R.
               04  WR-DNO     PIC  9(006).
               04  WR-GNO     PIC  9(001).
               04  WR-DATE.
                 05  WR-NEN   PIC  9(004).
                 05  WR-GP    PIC  9(004).
               04  WR-TCD     PIC  9(004).
               04  WR-HCD     PIC  9(006).
               04  WR-SIZ     PIC  9(001).
               04  WR-SUD   OCCURS  10.
                 05  WR-SU    PIC S9(004)  COMP-3.
               04  WR-SUT     PIC S9(005).
               04  WR-T       PIC S9(005).
               04  WR-KIN     PIC S9(008).
               04  WR-CSC     PIC  9(001).
               04  WR-DC      PIC  9(001).
               04  F          PIC  X(005).
               04  WR-CCD     PIC  9(003).
               04  WR-BC1     PIC  9(002).
               04  F          PIC  X(041).
               04  WR-SNC     PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LITCM.
           COPY LIHIM.
           COPY LSPF.
      *FD  SNTR-F
       01  SNTR-F_HKG660.
           02  SNTR-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SNTR-F_LNAME   PIC  X(013) VALUE "SNTR-F_HKG660".
           02  F              PIC  X(001).
           02  SNTR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  SNTR-F_SORT    PIC  X(100) VALUE SPACE.
           02  SNTR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  SNTR-F_RES     USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DNO       PIC  9(006).
           02  SNTR-GNO       PIC  9(001).
           02  SNTR-DATE.
             03  SNTR-NEN     PIC  9(004).
             03  SNTR-GP      PIC  9(004).
           02  SNTR-TCD       PIC  9(004).
           02  SNTR-D1.
             03  SNTR-HCD     PIC  9(006).
             03  SNTR-SIZ     PIC  9(001).
             03  SNTR-ASU.
               04 SNTR-SUD   OCCURS  10.
                 05  SNTR-SU  PIC S9(004)  COMP-3.
             03  SNTR-SUT     PIC S9(005).
             03  SNTR-T       PIC S9(005).
             03  SNTR-KIN     PIC S9(008).
             03  SNTR-CSC     PIC  9(001).
             03  SNTR-DC      PIC  9(001).
             03  F            PIC  X(005).
             03  SNTR-CCD     PIC  9(003).
             03  SNTR-BC1     PIC  9(002).
             03  F            PIC  X(028).
             03  SNTR-SNGP.
               04  F          PIC  9(002).
               04  SNTR-SNEN  PIC  9(002).
               04  SNTR-SGET  PIC  9(002).
               04  F          PIC  9(002).
             03  F            PIC  X(005).
           02  SNTR-D2    REDEFINES SNTR-D1.
             03  SNTR-BI      PIC  N(024).
             03  F            PIC  X(036).
             03  SNTR-SHZ     PIC S9(007).
             03  F            PIC  X(017).
           02  SNTR-SNC       PIC  9(001).
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　生協　請求サイズ別　明細表　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(018) VALUE
                  "[  '  年   月分  ]".
       01  C-DSP.
           02  D-MID   PIC  N(007).
           02  D-NG.
             03  01D-NG  PIC  9(002).
             03  02D-NG  PIC Z9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
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
            "C-MID" " " "0" "0" "340" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "46" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "46" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "46" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "46" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "46" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "46" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "46" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "14" "24" "18" "07C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "18" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "D-MID" "N" "7" "26" "14" " " "C-DSP"  RETURNING RESU.
       CALL "SD_From" USING
            "D-MID" BY REFERENCE W-MID "14" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "14" "0" "4" "D-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "9" "14" "28" "2" " " "D-NG"  RETURNING RESU.
       CALL "SD_From" USING
            "01D-NG" BY REFERENCE W-NEND "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "Z9" "14" "33" "2" "01D-NG" " "  RETURNING RESU.
       CALL "SD_From" USING
            "02D-NG" BY REFERENCE W-GETD "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "45" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "45" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 0 AND 1
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "D-MID" D-MID "p" RETURNING RESU
               MOVE W-MID TO H-MID
           END-IF
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO W-D.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO SNTR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SNTR-F_PNAME1 "SHARED" BY REFERENCE
            SNTR-F_IDLST "0".
       M-10.
      *           READ SNTR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  SNTR-GNO = 9
               GO TO M-10
           END-IF
           IF  SNTR-DC = 4
               GO TO M-10
           END-IF
      *
           MOVE SNTR-SNEN TO H-NEN.
           MOVE SNTR-SGET TO H-GET.
           MOVE SNTR-SNEN TO W-NEND.
           MOVE SNTR-SGET TO W-GETD.
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
           MOVE W-ASMD TO W-ASD.
       M-15.
           MOVE ZERO TO W-TD CHK.
           MOVE SNTR-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "　　＊＊　　マスター　なし　　＊＊" TO T-NAME
           END-IF
           IF  JS-SIGN = 1
               IF  W-TCD = 3241 OR 3247
                   MOVE SNTR-CCD TO W-CCD
               END-IF
           END-IF.
       M-20.
           MOVE SNTR-GP TO W-GP.
           MOVE 0 TO CHK2.
       M-25.
           MOVE SNTR-DNO TO W-DNO.
           INITIALIZE W-AR.
           MOVE 0 TO W-DC CHK3.
       M-30.
           ADD 1 TO W-DC.
           IF  W-DC > 6
               GO TO M-50
           END-IF
           INITIALIZE W-R(W-DC).
           MOVE SNTR-R TO W-R(W-DC).
       M-35.
      *           READ SNTR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  SNTR-GNO NOT = 9
               IF  SNTR-DC = 4
                   GO TO M-35
               END-IF
           END-IF
           IF  SNTR-DNO NOT = W-DNO
               GO TO M-50
           END-IF
           IF  SNTR-GNO NOT = 9
               GO TO M-30
           END-IF
      *
           PERFORM S-20 THRU S-65.
       M-40.
      *           READ SNTR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  SNTR-GNO = 9
               GO TO M-40
           END-IF
           IF  SNTR-DC = 4
               GO TO M-40
           END-IF
           IF  SNTR-TCD NOT = W-TCD
               GO TO M-45
           END-IF
           IF  JS-SIGN = 1
               IF  W-TCD = 3241 OR 3247
                   IF  SNTR-CCD NOT = W-CCD
                       GO TO M-45
                   END-IF
               END-IF
           END-IF
           IF  SNTR-GP = W-GP
               GO TO M-25
           END-IF
           GO TO M-20.
       M-45.
           PERFORM S-80 THRU S-85.
           MOVE ZERO TO W-PAGE.
           PERFORM S-05 THRU S-15.
           GO TO M-15.
       M-50.
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           GO TO M-90.
       M-85.
           PERFORM S-80 THRU S-85.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
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
       S-15.
           EXIT.
       S-20.
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               PERFORM S-70 THRU S-75
           END-IF
           MOVE 0 TO W-DC.
       S-25.
           ADD 1 TO W-DC.
           IF  W-DC > 6
               GO TO S-55
           END-IF
           IF  WR-GNO(W-DC) = 0
               GO TO S-55
           END-IF
      *
           MOVE WR-HCD(W-DC) TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
           END-IF
           IF (WR-HCD(W-DC) > 999899) OR (WR-DC(W-DC) = 3)
               GO TO S-35
           END-IF
           IF  WR-SNC(W-DC) = 1
               GO TO S-35
           END-IF
      *
           MOVE ZERO TO CNT W-C.
           IF  WR-SIZ(W-DC) = 2
               ADD 10 TO W-C
           END-IF
           IF  WR-SIZ(W-DC) = 3
               ADD 20 TO W-C
           END-IF
           IF  WR-SIZ(W-DC) = 4
               ADD 30 TO W-C
           END-IF.
       S-30.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO S-25
           END-IF
           IF  WR-SU(W-DC,CNT) = ZERO
               GO TO S-30
           END-IF
           COMPUTE W-SC = W-C + CNT.
       S-35.
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-HNA P-KBN.
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               MOVE W-GP TO P-GP1
           END-IF
           IF  CHK3 = 0
               MOVE 1 TO CHK3
               MOVE W-DNO TO P-DNO1
           END-IF
           MOVE "-" TO P-V.
           MOVE WR-GNO(W-DC) TO P-GNO.
           MOVE WR-HCD(W-DC) TO P-HCD.
           MOVE HI-NAME TO P-HNA.
           IF  WR-HCD(W-DC) > 999899
               GO TO S-45
           END-IF
           IF  WR-SNC(W-DC) NOT = 0
               GO TO S-45
           END-IF
           IF  WR-DC(W-DC) = 3
               GO TO S-45
           END-IF
           COMPUTE W-SCD = W-SC + 10.
           IF  WR-SIZ(W-DC) NOT = 1
               GO TO S-40
           END-IF
           IF  HI-BC3 = 30
               SUBTRACT 10 FROM W-SCD
               GO TO S-40
           END-IF
           IF  ZERO NOT = HI-SS2 OR HI-SS3 OR HI-SS4
               IF  HI-SS1 NOT = ZERO
                   SUBTRACT 10 FROM W-SCD
               END-IF
           END-IF.
       S-40.
           MOVE W-S(W-SCD) TO P-SIZ.
       S-45.
           IF  WR-DC(W-DC) = 0
               MOVE "売　上" TO P-KBN
           END-IF
           IF  WR-DC(W-DC) = 1
               MOVE "返　品" TO P-KBN
           END-IF
           IF  WR-DC(W-DC) = 2
               MOVE "不良返" TO P-KBN
           END-IF
           IF  WR-DC(W-DC) = 3
               MOVE "預　り" TO P-KBN
           END-IF
           IF  WR-DC(W-DC) = 5
               MOVE "振　替" TO P-KBN
           END-IF
           IF  WR-SNC(W-DC) = 1
               MOVE "値　引" TO P-KBN
           END-IF
           IF  WR-DC(W-DC) = 8
               MOVE "調　整" TO P-KBN
           END-IF
           MOVE ZERO TO W-SU W-T W-KIN.
           IF  WR-HCD(W-DC) > 999899
               IF  WR-SNC(W-DC) = 1 OR 3
                   COMPUTE W-KIN = WR-KIN(W-DC) * -1
               ELSE
                   IF  WR-DC(W-DC) = 1 OR 2 OR 5
                       COMPUTE W-KIN = WR-KIN(W-DC) * -1
                   ELSE
                       MOVE WR-KIN(W-DC) TO W-KIN
                   END-IF
               END-IF
           END-IF
           MOVE W-KIN TO P-KIN.
           IF  WR-HCD(W-DC) > 999899
               GO TO S-50
           END-IF
           IF  WR-DC(W-DC) = 3
               MOVE WR-SUT(W-DC) TO W-SU
               MOVE WR-KIN(W-DC) TO W-KIN
               MOVE W-SU TO P-SU
               MOVE WR-T(W-DC) TO P-T
               MOVE W-KIN TO P-KIN
               GO TO S-50
           END-IF
           IF  WR-SNC(W-DC) = 1 OR 3
               COMPUTE W-T = WR-T(W-DC) * -1
               COMPUTE W-KIN = WR-SUT(W-DC) * W-T
               MOVE WR-SUT(W-DC) TO P-SU
               MOVE W-T TO P-T
               MOVE W-KIN TO P-KIN
           ELSE
               IF  WR-DC(W-DC) = 1 OR 2 OR 5
                   COMPUTE W-SU = WR-SU(W-DC,CNT) * -1
                   MOVE WR-T(W-DC) TO W-T
                   COMPUTE W-KIN = W-SU * W-T
                   MOVE W-SU TO P-SU
                   MOVE W-T TO P-T
                   MOVE W-KIN TO P-KIN
               ELSE
                   MOVE WR-SU(W-DC,CNT) TO W-SU
                   COMPUTE W-KIN = W-SU * WR-T(W-DC)
                   MOVE W-SU TO P-SU
                   MOVE WR-T(W-DC) TO P-T
                   MOVE W-KIN TO P-KIN
               END-IF
           END-IF.
       S-50.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-GP TO P-GP1
               MOVE W-DNO TO P-DNO1
               PERFORM S-05 THRU S-15
               PERFORM S-70 THRU S-75
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-SU TO W-TSU.
           ADD W-KIN TO W-TKIN.
      *
           IF  WR-HCD(W-DC) > 999899
               GO TO S-25
           END-IF
           IF  WR-DC(W-DC) = 3
               GO TO S-25
           END-IF
           IF  WR-SNC(W-DC) = 1
               GO TO S-25
           END-IF
           GO TO S-30.
       S-55.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-CNA P-CM P-TM P-TEK.
           IF  WR-CCD(1) < 2
               IF  SNTR-BI = SPACE
                   GO TO S-65
               ELSE
                   GO TO S-60
               END-IF
           END-IF
           MOVE WR-TCD(1) TO TC-TCD.
           MOVE WR-CCD(1) TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
               MOVE "　　＊＊　　直送　なし　＊＊" TO TC-NAME
           END-IF
           MOVE "　送り先" TO P-CM.
           MOVE ":" TO P-CC.
           MOVE "(" TO P-CF.
           MOVE WR-CCD(1) TO P-CCD.
           MOVE ")" TO P-CR.
           MOVE TC-NAME TO P-CNA.
       S-60.
           IF  SNTR-BI NOT = SPACE
               MOVE "摘要" TO P-TM
               MOVE ":" TO P-TC
               MOVE SNTR-BI TO P-TEK
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE W-GP TO P-GP2
               MOVE W-DNO TO P-DNO2
               PERFORM S-05 THRU S-15
               PERFORM S-70 THRU S-75
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-65.
           EXIT.
       S-70.
           MOVE SPACE TO H-TNA.
           MOVE W-TCD TO H-TCD.
           MOVE T-NAME TO H-TNA.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-75.
           EXIT.
       S-80.
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-HNA P-KBN.
           MOVE "　　　　　　　　　　　　　［　合　計　］" TO P-HNA.
           MOVE W-TSU TO P-SU.
           MOVE W-TKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
               PERFORM S-70 THRU S-75
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-85.
           EXIT.

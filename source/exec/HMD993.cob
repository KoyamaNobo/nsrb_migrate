       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD990.
      *************************************************************
      *    PROGRAM         :  生協売上値引データ抽出（エクセル用）*
      *    PRINTER TYPE    :  *****                               *
      *    SCREEN          :  ******                              *
      *    COMPILE TYPE    :  COBOL                               *
      *************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM100.
       OBJECT-COMPUTER. SYSTEM100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-D.
             03  W-STA        PIC  9(001).
             03  CHK          PIC  9(001).
             03  W-DNO        PIC  9(006).
             03  W-DC         PIC  9(001).
             03  CNT          PIC  9(002).
             03  W-C          PIC  9(002).
             03  W-SC         PIC  9(002).
             03  W-SCD        PIC  9(002).
             03  W-TSU        PIC S9(004).
             03  W-TKIN       PIC S9(008).
             03  W-DATE       PIC 99/99/99.
             03  W-NGD.
               04  W-NEND     PIC  9(002).
               04  W-GETD     PIC  9(002).
           02  W-NAME         PIC  N(026).
           02  W-ATNA  REDEFINES W-NAME.
             03  W-TNAD  OCCURS  26.
               04  W-TNA      PIC  N(001).
           02  W-MID.
             03  F            PIC  N(008) VALUE SPACE.
             03  F            PIC  N(001) VALUE "’".
             03  W-NEN        PIC  N(002).
             03  F            PIC  N(001) VALUE "年".
             03  W-GET        PIC  N(002).
             03  F            PIC  N(010) VALUE
                  "月分　請求詳細リスト".
           02  W-NGPM.
             03  F            PIC  X(034) VALUE SPACE.
             03  F            PIC  X(006) VALUE "DATE  ".
             03  W-NGP        PIC 99/99/99.
           02  W-ASMD.
             03  F            PIC  X(040) VALUE
                  "3ｺﾞｳ2ｺﾞｳ1ｺﾞｳ0ｺﾞｳ ﾁｭｳ ﾀﾞｲﾄｸﾀﾞ28.029.030.0".
             03  F            PIC  X(040) VALUE
                  "          SS   S   M   L  LL  XL XXL    ".
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.0    ".
             03  F            PIC  X(040) VALUE
                  "24.024.525.025.526.026.527.027.5        ".
           02  W-ASD.
             03  W-SD    OCCURS  50.
               04  W-S        PIC  X(004).
       01  W-AR.
           02  W-ARD   OCCURS  6.
             03  W-R.
               04  WR-DNO     PIC  9(006).
               04  WR-GNO     PIC  9(001).
               04  WR-DATE.
                 05  F        PIC  9(002).
                 05  WR-NGPS  PIC  9(006).
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
           COPY LITM.
           COPY LITCM.
           COPY LIHIM.
      *FD  SIKF
       01  SIKF_HMD993.
           02  SIKF_PNAME1    PIC  X(005) VALUE "SURIF".
           02  F              PIC  X(001).
           02  SIKF_LNAME     PIC  X(011) VALUE "SIKF_HMD993".
           02  F              PIC  X(001).
           02  SIKF_KEY1      PIC  X(100) VALUE SPACE.
           02  SIKF_SORT      PIC  X(100) VALUE SPACE.
           02  SIKF_IDLST     PIC  X(100) VALUE SPACE.
           02  SIKF_RES       USAGE  POINTER.
       01  SIK-R.
           02  SIK-R1.
               03  F              PIC  X(008).
               03  F              PIC  X(052).
               03  F              PIC  X(006).
               03  SIK-MID        PIC  N(024).
               03  F              PIC  X(004).
               03  F              PIC  X(006).
               03  F              PIC  X(004).
               03  F              PIC  X(006).
               03  F              PIC  X(008).
               03  F              PIC  X(048).
               03  F              PIC  X(006).
               03  F              PIC  X(002).
               03  SIK-1          PIC  9(001).
           02  SIK-R2    REDEFINES SIK-R1.
               03  SIK-TCD        PIC  9(008).
               03  SIK-TNA        PIC  N(026).
               03  F              PIC  X(006).
               03  F              PIC  X(048).
               03  F              PIC  X(004).
               03  F              PIC  X(006).
               03  F              PIC  X(004).
               03  F              PIC  X(006).
               03  F              PIC  X(008).
               03  SIK-NGP        PIC  X(048).
               03  F              PIC  X(006).
               03  F              PIC  X(002).
               03  SIK-2          PIC  9(001).
           02  SIK-R3    REDEFINES SIK-R1.
               03  SIK-MDATE      PIC  N(004).
               03  SIK-MCNA       PIC  N(026).
               03  SIK-MHCD       PIC  X(006).
               03  SIK-MHNA       PIC  N(024).
               03  SIK-MSIZ       PIC  X(004).
               03  SIK-MKBN       PIC  N(003).
               03  SIK-MSU        PIC  N(002).
               03  SIK-MT         PIC  N(003).
               03  SIK-MKIN       PIC  N(004).
               03  SIK-MBI        PIC  N(024).
               03  SIK-MDNO       PIC  N(003).
               03  SIK-MGNO       PIC  N(001).
               03  SIK-3          PIC  9(001).
           02  SIK-R4    REDEFINES SIK-R1.
               03  SIK-DATE       PIC  X(008).
               03  SIK-CNA        PIC  N(026).
               03  SIK-HCD        PIC  9(006).
               03  SIK-HNA        PIC  N(024).
               03  SIK-SIZ        PIC  X(004).
               03  SIK-KBN        PIC  N(003).
               03  SIK-SU         PIC S9(004).
               03  SIK-T          PIC S9(006).
               03  SIK-KIN        PIC S9(008).
               03  SIK-BI         PIC  N(024).
               03  SIK-DNO        PIC  9(006).
               03  SIK-GNO        PIC  9(002).
               03  SIK-4          PIC  9(001).
           02  SIK-R5    REDEFINES SIK-R1.
               03  F              PIC  X(008).
               03  F              PIC  X(052).
               03  F              PIC  X(006).
               03  SIK-KEIM       PIC  N(024).
               03  F              PIC  X(004).
               03  F              PIC  X(006).
               03  SIK-TSU        PIC S9(004).
               03  F              PIC  X(006).
               03  SIK-TKIN       PIC S9(008).
               03  F              PIC  X(048).
               03  F              PIC  X(006).
               03  F              PIC  X(002).
               03  SIK-5          PIC  9(001).
       77  F                      PIC  X(001).
      *FD  SNTR-F
       01  SNTR-F_HMD993.
           02  SNTR-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SNTR-F_LNAME   PIC  X(013) VALUE "SNTR-F_HMD993".
           02  F              PIC  X(001).
           02  SNTR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  SNTR-F_SORT    PIC  X(100) VALUE SPACE.
           02  SNTR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  SNTR-F_RES     USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DNO       PIC  9(006).
           02  SNTR-GNO       PIC  9(001).
           02  SNTR-DATE      PIC  9(008).
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
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　売上値引データ　抽出　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(018) VALUE
                  "[  '  年   月分  ]".
       01  C-DSP.
           02  D-NG.
             03  01D-NG  PIC  9(002).
             03  02D-NG  PIC Z9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME3   PIC  X(022) VALUE
                  "***  得意先　なし  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "298" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "40" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "40" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "40" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "40" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "40" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "40" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "40" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "14" "21" "18" "07C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "4" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NG" " " "14" "0" "4" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NG" "9" "14" "25" "2" " " "D-NG"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-NEND "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NG" "Z9" "14" "30" "2" "01D-NG" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-GETD "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "67" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "67" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "22" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-D.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO SNTR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SNTR-F_PNAME1 "SHARED" BY REFERENCE 
            SNTR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE 
            T-M_IDLST T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "OUTPUT" SIKF_PNAME1 " " BY REFERENCE SIKF_IDLST "0".
       M-10.
      *           READ SNTR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  SNTR-GNO = 9
               GO TO M-10
           END-IF
           IF  SNTR-DC = 4
               GO TO M-10
           END-IF
           IF  W-STA NOT = 0
               GO TO M-15
           END-IF
           MOVE 1 TO W-STA.
           MOVE SNTR-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
      *
           MOVE SNTR-SNEN TO W-NEN.
           MOVE SNTR-SGET TO W-GET.
           IF  W-NGD = ZERO
               MOVE SNTR-SNEN TO W-NEND
               MOVE SNTR-SGET TO W-GETD
               CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU
           END-IF
      *
           MOVE SPACE TO SIK-R1.
           MOVE W-MID TO SIK-MID.
           MOVE 1 TO SIK-1.
      *           WRITE SIK-R1.
      *//////////////
           CALL "DB_Insert" USING
            SIKF_PNAME1 SIKF_LNAME SIK-R1 RETURNING RET.
      *
           PERFORM S-45 THRU S-55.
           ACCEPT W-NGP FROM DATE.
      *
           MOVE SPACE TO SIK-R2.
           MOVE SNTR-TCD TO SIK-TCD.
           MOVE W-NAME TO SIK-TNA.
           MOVE W-NGPM TO SIK-NGP.
           MOVE 2 TO SIK-2.
      *           WRITE SIK-R2.
      *//////////////
           CALL "DB_Insert" USING
            SIKF_PNAME1 SIKF_LNAME SIK-R2 RETURNING RET.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
      *
           MOVE SPACE TO SIK-R3.
           MOVE "　日付　" TO SIK-MDATE.
           MOVE "　　　　　　　　　　送　り　先　名　" TO SIK-MCNA.
           MOVE " ｺｰﾄﾞ " TO SIK-MHCD.
           MOVE "　　　　　　　　　　品　　名　" TO SIK-MHNA.
           MOVE "ｻｲｽﾞ" TO SIK-MSIZ.
           MOVE "区　分" TO SIK-MKBN.
           MOVE "数量" TO SIK-MSU.
           MOVE "　単価" TO SIK-MT.
           MOVE "　　金額" TO SIK-MKIN.
           MOVE "　　　　　　　　　　摘　　要　" TO SIK-MBI.
           MOVE "伝票№" TO SIK-MDNO.
           MOVE "行" TO SIK-MGNO.
           MOVE 3 TO SIK-3.
      *           WRITE SIK-R3.
      *//////////////
           CALL "DB_Insert" USING
            SIKF_PNAME1 SIKF_LNAME SIK-R3 RETURNING RET.
      *
           MOVE W-ASMD TO W-ASD.
       M-15.
           MOVE SNTR-DNO TO W-DNO.
           INITIALIZE W-AR.
           MOVE 0 TO W-DC.
       M-20.
           ADD 1 TO W-DC.
           IF  W-DC > 6
               GO TO M-30
           END-IF
           INITIALIZE W-R(W-DC).
           MOVE SNTR-R TO W-R(W-DC).
       M-25.
      *           READ SNTR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
           IF  SNTR-GNO NOT = 9
               IF  SNTR-DC = 4
                   GO TO M-25
               END-IF
           END-IF
           IF  SNTR-DNO NOT = W-DNO
               GO TO M-30
           END-IF
           IF  SNTR-GNO NOT = 9
               GO TO M-20
           END-IF
      *
           PERFORM S-05 THRU S-40.
           GO TO M-10.
       M-30.
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           GO TO M-90.
       M-85.
           MOVE SPACE TO SIK-R5.
           MOVE "　　　　　　　　　　　　［　合　計　］" TO SIK-KEIM.
           MOVE W-TSU TO SIK-TSU.
           MOVE W-TKIN TO SIK-TKIN.
           MOVE 5 TO SIK-5.
      *           WRITE SIK-R5.
      *//////////////
           CALL "DB_Insert" USING
            SIKF_PNAME1 SIKF_LNAME SIK-R5 RETURNING RET.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SIKF_IDLST SIKF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE WR-TCD(1) TO TC-TCD.
           IF  WR-CCD(1) = ZERO
               MOVE 001 TO TC-CCD
           ELSE
               MOVE WR-CCD(1) TO TC-CCD
           END-IF
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
           END-IF
           MOVE 0 TO W-DC.
       S-10.
           ADD 1 TO W-DC.
           IF  W-DC > 6
               GO TO S-40
           END-IF
           IF  WR-GNO(W-DC) = 0
               GO TO S-40
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
               GO TO S-20
           END-IF
           IF  WR-SNC(W-DC) = 1
               GO TO S-20
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
       S-15.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO S-10
           END-IF
           IF  WR-SU(W-DC,CNT) = ZERO
               GO TO S-15
           END-IF.
           COMPUTE W-SC = W-C + CNT.
       S-20.
           MOVE SPACE TO SIK-R3.
           MOVE WR-DNO(W-DC) TO SIK-DNO.
           MOVE WR-GNO(W-DC) TO SIK-GNO.
           MOVE WR-NGPS(W-DC) TO W-DATE.
           MOVE W-DATE TO SIK-DATE.
           MOVE TC-NAME TO SIK-CNA.
           MOVE WR-HCD(W-DC) TO SIK-HCD.
           MOVE HI-NAME TO SIK-HNA.
           IF  WR-HCD(W-DC) > 999899
               GO TO S-30
           END-IF
           IF  WR-SNC(W-DC) NOT = 0
               GO TO S-30
           END-IF
           IF  WR-DC(W-DC) = 3
               GO TO S-30
           END-IF
           COMPUTE W-SCD = W-SC + 10.
           IF  WR-SIZ(W-DC) NOT = 1
               GO TO S-25
           END-IF
           IF  HI-BC3 = 30
               SUBTRACT 10 FROM W-SCD
               GO TO S-25
           END-IF
           IF  ZERO NOT = HI-SS2 OR HI-SS3 OR HI-SS4
               IF  HI-SS1 NOT = ZERO
                   SUBTRACT 10 FROM W-SCD
               END-IF
           END-IF.
       S-25.
           MOVE W-S(W-SCD) TO SIK-SIZ.
       S-30.
           IF  WR-DC(W-DC) = 0
               MOVE "売　上" TO SIK-KBN
           END-IF
           IF  WR-DC(W-DC) = 1
               MOVE "返　品" TO SIK-KBN
           END-IF
           IF  WR-DC(W-DC) = 2
               MOVE "不良返" TO SIK-KBN
           END-IF
           IF  WR-DC(W-DC) = 3
               MOVE "預　り" TO SIK-KBN
           END-IF
           IF  WR-DC(W-DC) = 5
               MOVE "振　替" TO SIK-KBN
           END-IF
           IF  WR-SNC(W-DC) = 1
               MOVE "値　引" TO SIK-KBN
           END-IF
           IF  WR-DC(W-DC) = 8
               MOVE "調　整" TO SIK-KBN
           END-IF
           IF  WR-HCD(W-DC) > 999899
               IF  WR-SNC(W-DC) = 1 OR 3
                   COMPUTE SIK-KIN = WR-KIN(W-DC) * -1
               ELSE
                   IF  WR-DC(W-DC) = 1 OR 2 OR 5
                       COMPUTE SIK-KIN = WR-KIN(W-DC) * -1
                   ELSE
                       MOVE WR-KIN(W-DC) TO SIK-KIN
                   END-IF
               END-IF
           END-IF
           IF  WR-HCD(W-DC) > 999899
               GO TO S-35
           END-IF
           IF  WR-DC(W-DC) = 3
               MOVE WR-SUT(W-DC) TO SIK-SU
               MOVE WR-T(W-DC) TO SIK-T
               MOVE WR-KIN(W-DC) TO SIK-KIN
               GO TO S-35
           END-IF
           IF  WR-SNC(W-DC) = 1 OR 3
               MOVE WR-SUT(W-DC) TO SIK-SU
               COMPUTE SIK-T = WR-T(W-DC) * -1
               COMPUTE SIK-KIN = SIK-SU * SIK-T
           ELSE
               IF  WR-DC(W-DC) = 1 OR 2 OR 5
                   COMPUTE SIK-SU = WR-SU(W-DC,CNT) * -1
                   MOVE WR-T(W-DC) TO SIK-T
                   COMPUTE SIK-KIN = SIK-SU * SIK-T
               ELSE
                   MOVE WR-SU(W-DC,CNT) TO SIK-SU
                   MOVE WR-T(W-DC) TO SIK-T
                   COMPUTE SIK-KIN = SIK-SU * SIK-T
               END-IF
           END-IF.
       S-35.
           IF  SNTR-DNO = W-DNO
               IF  SNTR-GNO = 9
                   MOVE SNTR-BI TO SIK-BI
               END-IF
           END-IF
           MOVE 4 TO SIK-4.
      *           WRITE SIK-R4.
      *//////////////
           CALL "DB_Insert" USING
            SIKF_PNAME1 SIKF_LNAME SIK-R4 RETURNING RET.
           ADD SIK-SU TO W-TSU.
           ADD SIK-KIN TO W-TKIN.
           IF  WR-HCD(W-DC) > 999899
               GO TO S-10
           END-IF
           IF  WR-DC(W-DC) = 3
               GO TO S-10
           END-IF
           IF  WR-SNC(W-DC) = 1
               GO TO S-10
           END-IF
           GO TO S-15.
       S-40.
           EXIT.
       S-45.
           MOVE SPACE TO W-NAME.
           MOVE T-NAME TO W-NAME.
           MOVE 27 TO CNT.
       S-50.
           SUBTRACT 1 FROM CNT.
           IF  W-TNA(CNT) = SPACE
               GO TO S-50
           END-IF
           ADD 2 TO CNT.
           IF  CNT > 26
               MOVE 26 TO CNT
           END-IF
           MOVE "殿" TO W-TNA(CNT).
       S-55.
           EXIT.

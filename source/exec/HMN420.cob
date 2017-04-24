       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN420.
      *********************************************************
      *    PROGRAM         :  品名サイズ別棚卸ワーク　作成　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0128".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0256".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  W-DC           PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-C            PIC  9(001).
           02  CHK.
             03  CHK0         PIC  9(001).
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  W-ZCD.
             03  W-ZCD0.
               04  W-ZC0   OCCURS   4  PIC  9(001).
             03  W-ZCD1.
               04  W-ZC1   OCCURS   4  PIC  9(001).
             03  W-ZCD2.
               04  W-ZC2   OCCURS   4  PIC  9(001).
           02  W-ZSD.
             03  W-ZSD0.
               04  W-ZS0   OCCURS  10  PIC S9(006).
             03  W-ZSD1.
               04  W-ZS1   OCCURS  10  PIC S9(006).
             03  W-ZSD2.
               04  W-ZS2   OCCURS  10  PIC S9(006).
           02  W-D.
             03  W-HCD        PIC  9(006).
             03  W-ASUD0.
               04  W-ASU0  OCCURS  4.
                 05  W-SUD0  OCCURS  10.
                   06  W-SU0  PIC S9(006).
             03  W-TSU0       PIC S9(006).
             03  W-ASUD1.
               04  W-ASU1  OCCURS  4.
                 05  W-SUD1  OCCURS  10.
                   06  W-SU1  PIC S9(006).
             03  W-TSU1       PIC S9(006).
             03  W-ASUD2.
               04  W-ASU2  OCCURS  4.
                 05  W-SUD2  OCCURS  10.
                   06  W-SU2  PIC S9(006).
             03  W-TSU2       PIC S9(006).
             03  W-SEN        PIC  9(001).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIHIM.
      *FD  HHTF
       01  HHTF_HMN420.
           02  HHTF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HHTF_LNAME     PIC  X(011) VALUE "HHTF_HMN420".
           02  F              PIC  X(001).
           02  HHTF_KEY1      PIC  X(100) VALUE SPACE.
           02  HHTF_SORT      PIC  X(100) VALUE SPACE.
           02  HHTF_IDLST     PIC  X(100) VALUE SPACE.
           02  HHTF_RES       USAGE  POINTER.
       01  HHT-R.
           02  HHT-KEY2.
             03  HHT-MHCD     PIC  9(006).
             03  HHT-KEY.
               04  HHT-HCD    PIC  9(006).
               04  HHT-SIZ    PIC  9(001).
           02  HHT-AZSU.                                                前月残数
             03  HHT-ZSUD  OCCURS  10.
               04  HHT-ZSU    PIC S9(006) COMP-3.
           02  HHT-ANSU.                                                入庫数
             03  HHT-NSUD  OCCURS  10.
               04  HHT-NSU    PIC S9(006) COMP-3.
           02  HHT-AUSU.                                                出庫数
             03  HHT-USUD  OCCURS  10.
               04  HHT-USU    PIC S9(006) COMP-3.
           02  HHT-AASS.                                                預り出荷
             03  HHT-ASSD  OCCURS  10.
               04  HHT-ASS    PIC S9(004) COMP-3.
           02  HHT-ATZS.                                                棚卸帳簿
             03  HHT-TSZD  OCCURS  10.
               04  HHT-TZS    PIC S9(006) COMP-3.
           02  HHT-ATSU.                                                棚卸数
             03  HHT-TSUD  OCCURS  10.
               04  HHT-TSU    PIC S9(006) COMP-3.
           02  F              PIC  X(012).
           02  HHT-SEN        PIC  9(001).
       77  F                  PIC  X(001).
      *FD  HZW-F
       01  HZW-F_HMN420.
           02  HZW-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HZW-F_LNAME    PIC  X(012) VALUE "HZW-F_HMN420".
           02  F              PIC  X(001).
           02  HZW-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HZW-F_SORT     PIC  X(100) VALUE SPACE.
           02  HZW-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HZW-F_RES      USAGE  POINTER.
       01  HZW-R.
           02  HZW-HCD        PIC  9(006).
           02  HZW-KBN        PIC  9(001).
           02  HZW-SIZ        PIC  9(001).
           02  HZW-AZS.
             03  HZW-ZSD   OCCURS  10.
               04  HZW-ZS     PIC S9(006).
           02  HZW-TSU        PIC S9(006).
           02  HZW-TC         PIC  9(001).
           02  HZW-BC         PIC  9(006).
           02  HZW-BMC        PIC  9(002).
           02  HZW-BMNO       PIC  9(001).
           02  HZW-NO         PIC  9(001).
           02  HZW-FT         PIC  9(005).
           02  F              PIC  X(037).
           02  HZW-SEN        PIC  9(001).
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
           02  FILLER  PIC N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊　　品名サイズ別棚卸誤差Ｆ　作成　　＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME2.
               04  FILLER  PIC  X(016) VALUE
                    "***  HIM ﾅｼ  ***".
               04  02E-ME2 PIC  9(006).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
           "C-ERR" " " "0" "0" "82" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "82" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" " " "24" "0" "22" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "01E-ME2" "X" "24" "15" "16" " " "E-ME2" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-ME2" "9" "24" "33" "6" "01E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING
           "02E-ME2" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           COPY LIBCPR.
           IF  D-NHG = 5 OR 8 OR 11
               MOVE 0 TO W-DC
           ELSE
               MOVE 1 TO W-DC
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0128ID.
           MOVE WK0128ID TO HZW-F_PNAME1.
           MOVE W-FID2 TO WK0256ID.
           MOVE WK0256ID TO HHTF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HHTF_PNAME1 " " BY REFERENCE HHTF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "OUTPUT" HZW-F_PNAME1 " " BY REFERENCE HZW-F_IDLST "0".
      *
      *           READ HHTF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HHTF_PNAME1 BY REFERENCE HHT-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  HHT-SIZ = 4
               MOVE ZERO TO HHT-ZSU(10) HHT-NSU(10) HHT-USU(10)
                            HHT-ASS(10) HHT-TZS(10) HHT-TSU(10)
           END-IF.
       M-10.
           MOVE ZERO TO W-D W-ZCD.
           MOVE HHT-HCD TO W-HCD.
           MOVE HHT-SEN TO W-SEN.
       M-15.
           PERFORM S-05 THRU S-20.
      *
      *           READ HHTF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HHTF_PNAME1 BY REFERENCE HHT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  HHT-SIZ = 4
               MOVE ZERO TO HHT-ZSU(10) HHT-NSU(10) HHT-USU(10)
                            HHT-ASS(10) HHT-TZS(10) HHT-TSU(10)
           END-IF
           IF  HHT-HCD = W-HCD
               GO TO M-15
           END-IF
      *
           PERFORM S-25 THRU S-45.
           GO TO M-10.
       M-80.
           PERFORM S-25 THRU S-45.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HZW-F_IDLST HZW-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE ZERO TO W-ZSD.
           MOVE ZERO TO CNT CHK.
       S-10.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO S-15
           END-IF
           IF  W-DC = 0
               MOVE HHT-TZS(CNT) TO W-ZS0(CNT)
           ELSE
               COMPUTE W-ZS0(CNT) = HHT-ZSU(CNT) + HHT-NSU(CNT)
                                  - HHT-USU(CNT) - HHT-ASS(CNT)
           END-IF
           IF  CHK0 = 0
               IF  W-ZS0(CNT) NOT = ZERO
                   MOVE 1 TO CHK0
               END-IF
           END-IF
           ADD W-ZS0(CNT) TO W-TSU0.
      *
           MOVE HHT-TSU(CNT) TO W-ZS1(CNT).
           IF  CHK1 = 0
               IF  W-ZS1(CNT) NOT = ZERO
                   MOVE 1 TO CHK1
               END-IF
           END-IF
           ADD W-ZS1(CNT) TO W-TSU1.
      *
           COMPUTE W-ZS2(CNT) = W-ZS0(CNT) - W-ZS1(CNT).
           IF  CHK2 = 0
               IF  W-ZS2(CNT) NOT = ZERO
                   MOVE 1 TO CHK2
               END-IF
           END-IF
           ADD W-ZS2(CNT) TO W-TSU2.
           GO TO S-10.
       S-15.
           IF  CHK0 NOT = 0
               MOVE CHK0 TO W-ZC0(HHT-SIZ)
               MOVE W-ZSD0 TO W-ASU0(HHT-SIZ)
           END-IF
           IF  CHK1 NOT = 0
               MOVE CHK1 TO W-ZC1(HHT-SIZ)
               MOVE W-ZSD1 TO W-ASU1(HHT-SIZ)
           END-IF
           IF  CHK2 NOT = 0
               MOVE CHK2 TO W-ZC2(HHT-SIZ)
               MOVE W-ZSD2 TO W-ASU2(HHT-SIZ)
           END-IF.
       S-20.
           EXIT.
       S-25.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO HI-R
           END-IF
      *
           MOVE ZERO TO W-C.
       S-30.
           ADD 1 TO W-C.
           IF  W-C = 5
               GO TO S-45
           END-IF
           IF  W-ZC0(W-C) = 0
               GO TO S-35
           END-IF
      *
           MOVE ZERO TO HZW-R.
           MOVE W-HCD TO HZW-HCD.
           MOVE 0 TO HZW-KBN.
           MOVE W-C TO HZW-SIZ.
           MOVE W-ASU0(W-C) TO HZW-AZS.
           MOVE HI-BC TO HZW-BC.
           MOVE HI-BMC TO HZW-BMC.
           MOVE HI-BMNO TO HZW-BMNO.
           MOVE HI-FT TO HZW-FT.
           MOVE W-SEN TO HZW-SEN.
           COMPUTE HZW-NO = W-C - 1.
           IF  HZW-NO = 0
               MOVE 4 TO HZW-NO
           END-IF
           IF (W-C = 1) OR ((W-C = 4) AND (W-ZC0(1) = 0)) OR
              ((W-C = 3) AND (W-ZC0(1) = 0) AND (W-ZC0(4) = 0)) OR
              ((W-C = 2) AND
                  (W-ZC0(1) = 0) AND (W-ZC0(4) = 0) AND (W-ZC0(3) = 0))
               MOVE 1 TO HZW-TC
               MOVE W-TSU0 TO HZW-TSU
           END-IF
      *           WRITE HZW-R.
      *//////////////
           CALL "DB_Insert" USING
            HZW-F_PNAME1 HZW-F_LNAME HZW-R RETURNING RET.
       S-35.
           IF  W-ZC1(W-C) = 0
               GO TO S-40
           END-IF
      *
           MOVE ZERO TO HZW-R.
           MOVE W-HCD TO HZW-HCD.
           MOVE 1 TO HZW-KBN.
           MOVE W-C TO HZW-SIZ.
           MOVE W-ASU1(W-C) TO HZW-AZS.
           MOVE HI-BC TO HZW-BC.
           MOVE HI-BMC TO HZW-BMC.
           MOVE HI-BMNO TO HZW-BMNO.
           MOVE HI-FT TO HZW-FT.
           MOVE W-SEN TO HZW-SEN.
           COMPUTE HZW-NO = W-C - 1.
           IF  HZW-NO = 0
               MOVE 4 TO HZW-NO
           END-IF
           IF (W-C = 1) OR ((W-C = 4) AND (W-ZC1(1) = 0)) OR
              ((W-C = 3) AND (W-ZC1(1) = 0) AND (W-ZC1(4) = 0)) OR
              ((W-C = 2) AND
                  (W-ZC1(1) = 0) AND (W-ZC1(4) = 0) AND (W-ZC1(3) = 0))
               MOVE 1 TO HZW-TC
               MOVE W-TSU1 TO HZW-TSU
           END-IF
      *           WRITE HZW-R.
      *//////////////
           CALL "DB_Insert" USING
            HZW-F_PNAME1 HZW-F_LNAME HZW-R RETURNING RET.
       S-40.
           IF  W-ZC2(W-C) = 0
               GO TO S-30
           END-IF
      *
           MOVE ZERO TO HZW-R.
           MOVE W-HCD TO HZW-HCD.
           MOVE 2 TO HZW-KBN.
           MOVE W-C TO HZW-SIZ.
           MOVE W-ASU2(W-C) TO HZW-AZS.
           MOVE HI-BC TO HZW-BC.
           MOVE HI-BMC TO HZW-BMC.
           MOVE HI-BMNO TO HZW-BMNO.
           MOVE HI-FT TO HZW-FT.
           MOVE W-SEN TO HZW-SEN.
           COMPUTE HZW-NO = W-C - 1.
           IF  HZW-NO = 0
               MOVE 4 TO HZW-NO
           END-IF
           IF (W-C = 1) OR ((W-C = 4) AND (W-ZC2(1) = 0)) OR
              ((W-C = 3) AND (W-ZC2(1) = 0) AND (W-ZC2(4) = 0)) OR
              ((W-C = 2) AND
                  (W-ZC2(1) = 0) AND (W-ZC2(4) = 0) AND (W-ZC2(3) = 0))
               MOVE 1 TO HZW-TC
               MOVE W-TSU2 TO HZW-TSU
           END-IF
      *           WRITE HZW-R.
      *//////////////
           CALL "DB_Insert" USING
            HZW-F_PNAME1 HZW-F_LNAME HZW-R RETURNING RET.
           GO TO S-30.
       S-45.
           EXIT.

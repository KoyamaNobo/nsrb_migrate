       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG880.
      *********************************************************
      *    PROGRAM         :  履物出荷・値引データ作成        *
      *                    :  （月末品名振替）                *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NG         PIC  9(006).
             03  W-NGD   REDEFINES W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NEND  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-NGL   REDEFINES W-NG.
               04  F          PIC  9(002).
               04  W-NGS      PIC  9(004).
             03  W-PEY        PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-DS           PIC  9(001).
           02  W-DSD          PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-UNC          PIC  9(001).
           02  W-DNO          PIC  9(006).
           02  W-GNO          PIC  9(001).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHKBM.
           COPY LICAL.
      *FD  STWF
       01  STWF_HMG880.
           02  STWF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  STWF_LNAME     PIC  X(011) VALUE "STWF_HMG880".
           02  F              PIC  X(001).
           02  STWF_KEY1      PIC  X(100) VALUE SPACE.
           02  STWF_SORT      PIC  X(100) VALUE SPACE.
           02  STWF_IDLST     PIC  X(100) VALUE SPACE.
           02  STWF_RES       USAGE  POINTER.
       01  STW-R.
           02  STW-DNO        PIC  9(006).
           02  STW-GNO        PIC  9(001).
           02  STW-DATE       PIC  9(008).
           02  STW-TCD        PIC  9(004).
           02  STW-HCD        PIC  9(006).
           02  STW-SIZ        PIC  9(001).
           02  STW-SUS.
             03  STW-SUSD  OCCURS  10.
               04  STW-SU     PIC S9(004)  COMP-3.
           02  STW-SUT        PIC S9(005).
           02  STW-BT         PIC S9(005).
           02  STW-KIN        PIC S9(008).
           02  STW-CSC        PIC  9(001).
           02  STW-DC         PIC  9(001).
           02  STW-FT         PIC  9(005).
           02  STW-CCD        PIC  9(003).
           02  STW-BC1        PIC  9(002).
           02  STW-BC2        PIC  9(002).
           02  STW-BC3        PIC  9(002).
           02  STW-SOK        PIC  9(001).
           02  STW-TC2        PIC  9(002).
           02  STW-FKC        PIC  9(002).
           02  STW-HSC        PIC  9(001).
           02  STW-KOSU       PIC  9(003).
           02  STW-FRC        PIC  9(001).
           02  STW-TCD2       PIC  9(004).
           02  STW-BIK        PIC  X(010).
           02  STW-SDT        PIC  9(008).
           02  F              PIC  X(004).
           02  STW-DHC        PIC  9(001).
           02  STW-UNC        PIC  9(001).
       77  F                  PIC  X(001).
      *FD  S-TRAN
       01  S-TRAN_HMG880.
           02  S-TRAN_PNAME1  PIC  X(005) VALUE "STRAN".
           02  F              PIC  X(001).
           02  S-TRAN_LNAME   PIC  X(013) VALUE "S-TRAN_HMG880".
           02  F              PIC  X(001).
           02  S-TRAN_KEY1    PIC  X(100) VALUE SPACE.
           02  S-TRAN_SORT    PIC  X(100) VALUE SPACE.
           02  S-TRAN_IDLST   PIC  X(100) VALUE SPACE.
           02  S-TRAN_RES     USAGE  POINTER.
       01  S-R.
           02  S-DNO          PIC  9(006).
           02  S-GNO          PIC  9(001).
           02  S-DATE         PIC  9(008).
           02  S-TCD          PIC  9(004).
           02  S-DT1.
             03  S-HCD        PIC  9(006).
             03  S-SIZ        PIC  9(001).
             03  S-SUS.
               04  S-SUSD  OCCURS  10.
                 05  S-SU     PIC S9(004)  COMP-3.
             03  S-SUT        PIC S9(005).
             03  S-BT         PIC S9(005).
             03  S-KIN        PIC S9(008).
             03  S-CSC        PIC  9(001).
             03  S-DC         PIC  9(001).
             03  S-FT         PIC  9(005).
             03  S-CCD        PIC  9(003).
             03  S-BC1        PIC  9(002).
             03  S-BC2        PIC  9(002).
             03  S-BC3        PIC  9(002).
             03  S-SOK        PIC  9(001).
             03  S-TC2        PIC  9(002).
             03  S-FKC        PIC  9(002).
             03  S-HSC        PIC  9(001).
             03  S-KOSU       PIC  9(003).
             03  S-FRC        PIC  9(001).
             03  S-TCD2       PIC  9(004).
             03  S-BIK        PIC  X(010).
             03  S-SDT        PIC  9(008).
             03  F            PIC  X(004).
           02  S-DT2   REDEFINES S-DT1.
             03  S-BI         PIC  N(024).
             03  S-HANO       PIC  9(006).
             03  F            PIC  X(030).
             03  S-TAX        PIC S9(007).
             03  S-UZZ        PIC S9(007).
             03  S-UZ         PIC S9(009).
           02  S-DHC          PIC  9(001).
           02  S-UNC          PIC  9(001).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　履物出荷・値引データ作成　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　（月末品名振替）　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(023) VALUE
                "【  '  年   月   日  】".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
           02  A-CHK   PIC  9(001).
       01  C-DSP.
           02  D-NGP.
             03  01D-NGP  PIC  9(002).
             03  02D-NGP  PIC Z9 .
             03  03D-NGP  PIC Z9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  CALNF ﾅｼ  ***".
             03  E-ME2   PIC  X(025) VALUE
                  "***  STRAN WRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(017) VALUE
                  "***  ﾃﾞｰﾀ ﾅｼ  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  HKBM ﾅｼ  ***".
             03  E-ME5   PIC  X(026) VALUE
                  "***  HKBM REWRITE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(027) VALUE
                  "***  DATEM REWRITE ｴﾗｰ  ***".
             03  E-ME50  PIC  X(028) VALUE
                  "***  未更新データ　有り  ***".
             03  E-ME51  PIC  X(041) VALUE
                  "未更新データ　有り     消す  OK=1 NO=5   ".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "353" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "15" "44" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "15" "44" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "15" "44" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "15" "44" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "15" "44" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "15" "44" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "15" "44" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "14" "25" "23" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "26" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "2" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "43" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CHK" "9" "24" "55" "1" "A-DMM" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CHK" BY REFERENCE CHK "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "6" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NGP" " " "14" "0" "6" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NGP" "9" "14" "30" "2" " " "D-NGP"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NGP" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NGP" "Z9" "14" "35" "2" "01D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NGP" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-NGP" "Z9" "14" "40" "2" "02D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NGP" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "199" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "199" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "25" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "17" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "26" "E-ME4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "27" "E-ME5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME50" "X" "24" "15" "28" "E-ME6" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME51" "X" "24" "15" "41" "E-ME50" " "  RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           PERFORM CAL-RTN THRU CAL-EX.
           CALL "SD_Output" USING "D-NGP" D-NGP "p" RETURNING RESU.
           IF  W-PEY = ZERO
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" S-TRAN_PNAME1 " " BY REFERENCE S-TRAN_IDLST "0".
      *           READ S-TRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" S-TRAN_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF.
       M-15.
           IF  S-DHC NOT = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1
               CALL "SD_Output" USING
                "E-ME50" E-ME50 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *           READ S-TRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" S-TRAN_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME51" E-ME51 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-20
           END-IF
           GO TO M-15.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-CHK "A-CHK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  CHK = 1
               GO TO M-25
           END-IF
           IF  CHK = 5
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1
               GO TO M-95
           END-IF
           GO TO M-20.
       M-25.
           CALL "DB_F_Close" USING
            BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE HKB-UNN TO W-DNO.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO STWF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" STWF_PNAME1 " " BY REFERENCE STWF_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" S-TRAN_PNAME1 " " BY REFERENCE S-TRAN_IDLST "0".
      *
      *           READ STWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" STWF_PNAME1 BY REFERENCE STW-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE 0 TO W-DSD.
           IF  STW-UNC = 0
               IF  STW-DC = 0 OR 6
                   MOVE 1 TO W-DSD
               ELSE
                   IF  STW-DC = 1 OR 2 OR 5
                       MOVE 2 TO W-DSD
                   ELSE
                       IF  STW-SUT >= 0
                           MOVE 1 TO W-DSD
                       ELSE
                           MOVE 2 TO W-DSD
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  STW-UNC = 1
               IF  STW-KIN >= 0
                   MOVE 2 TO W-DSD
               ELSE
                   MOVE 1 TO W-DSD
               END-IF
           END-IF.
       M-35.
           MOVE STW-DC TO W-DC.
           MOVE W-DSD TO W-DS.
           MOVE STW-TCD TO W-TCD.
       M-40.
           MOVE STW-UNC TO W-UNC.
           ADD 1 TO W-DNO.
           MOVE 0 TO W-GNO.
       M-45.
           ADD 1 TO W-GNO.
           IF  W-GNO NOT = 7
               GO TO M-50
           END-IF
           PERFORM WRB-RTN THRU WRB-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF
           PERFORM HKB-RTN THRU HKB-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF
           GO TO M-40.
       M-50.
           PERFORM WRM-RTN THRU WRM-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF
      *
      *           READ STWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" STWF_PNAME1 BY REFERENCE STW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           MOVE 0 TO W-DSD.
           IF  STW-UNC = 0
               IF  STW-DC = 0 OR 6
                   MOVE 1 TO W-DSD
               ELSE
                   IF  STW-DC = 1 OR 2 OR 5
                       MOVE 2 TO W-DSD
                   ELSE
                       IF  STW-SUT >= 0
                           MOVE 1 TO W-DSD
                       ELSE
                           MOVE 2 TO W-DSD
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  STW-UNC = 1
               IF  STW-KIN >= 0
                   MOVE 2 TO W-DSD
               ELSE
                   MOVE 1 TO W-DSD
               END-IF
           END-IF
           IF (STW-DC = W-DC) AND (W-DS = W-DSD) AND (STW-TCD = W-TCD)
               GO TO M-45
           END-IF
           PERFORM WRB-RTN THRU WRB-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF
           PERFORM HKB-RTN THRU HKB-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF
           GO TO M-35.
       M-55.
           PERFORM WRB-RTN THRU WRB-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF
           PERFORM HKB-RTN THRU HKB-EX.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE STWF_IDLST STWF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1.
           IF  COMPLETION_CODE = 255
               CALL "DB_F_Open" USING
                "OUTPUT" S-TRAN_PNAME1 " " BY REFERENCE S-TRAN_IDLST "0"
               CALL "DB_F_Close" USING
                BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" M-DATE_PNAME1 "SHARED" BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
           MOVE "01" TO DATE-KEY.
      *           READ M-DATE INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" M-DATE_PNAME1 BY REFERENCE DATE-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE M-DATE_IDLST M-DATE_PNAME1
               CALL "SD_Output" USING
                "ERR-DATE" ERR-DATE "p" RETURNING RESU
               CALL "SD_Output" USING
                "ERR-BUZ" ERR-BUZ "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE 1 TO DATE-HFC.
      *           REWRITE DATE-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            M-DATE_PNAME1 M-DATE_LNAME DATE-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       CAL-RTN.
           COPY LIBCPR.
           MOVE ZERO TO W-NGP.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" CALNM_PNAME1 "SHARED" BY REFERENCE CALNM_IDLST "1"
            "CL-KEY" BY REFERENCE CL-KEY.
           MOVE ZERO TO CL-KEY.
           MOVE W-NGP TO CL-KEY.
      *           START CALNM KEY NOT < CL-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CALNM_PNAME1 "CL-KEY" " NOT < " CL-KEY RETURNING RET.
           IF  RET = 1
               GO TO CAL-020
           END-IF.
       CAL-010.
      *           READ CALNM NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CALNM_PNAME1 BY REFERENCE CALN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO CAL-020
           END-IF
           IF  CL-NG = W-NG
               MOVE CL-KEY TO W-NGP
               GO TO CAL-010
           END-IF.
       CAL-020.
           CALL "DB_F_Close" USING
            BY REFERENCE CALNM_IDLST CALNM_PNAME1.
       CAL-EX.
           EXIT.
       WRM-RTN.
           MOVE ZERO TO S-R.
           MOVE STW-R TO S-R.
           MOVE W-DNO TO S-DNO.
           MOVE W-GNO TO S-GNO.
           MOVE W-NGP TO S-DATE.
      *           WRITE S-R.
      *//////////////
           CALL "DB_Insert" USING
            S-TRAN_PNAME1 S-TRAN_LNAME S-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO WRM-EX
           END-IF
      *
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRM-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1.
           MOVE "STRAN        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" S-TRAN_PNAME1 " " BY REFERENCE S-TRAN_IDLST "0".
           GO TO WRM-RTN.
       WRM-EX.
           EXIT.
       WRB-RTN.
           INITIALIZE S-R.
           MOVE SPACE TO S-BI.
           MOVE W-DNO TO S-DNO.
           MOVE 9 TO S-GNO.
           MOVE W-NGP TO S-DATE.
           MOVE W-TCD TO S-TCD.
           MOVE "＊　（在庫振替）" TO S-BI.
           MOVE W-UNC TO S-UNC.
      *           WRITE S-R.
      *//////////////
           CALL "DB_Insert" USING
            S-TRAN_PNAME1 S-TRAN_LNAME S-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO WRB-EX
           END-IF
      *
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRB-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1.
           MOVE "STRAN        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" S-TRAN_PNAME1 " " BY REFERENCE S-TRAN_IDLST "0".
           GO TO WRB-RTN.
       WRB-EX.
           EXIT.
       HKB-RTN.
           CALL "DB_F_Open" USING
            "I-O" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
      *           READ HKBM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO HKB-EX
           END-IF
           MOVE W-DNO TO HKB-UNN.
           IF  HKB-UNN = ZERO
               MOVE 1 TO HKB-UNN
           END-IF
      *           REWRITE HKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
       HKB-EX.
           EXIT.

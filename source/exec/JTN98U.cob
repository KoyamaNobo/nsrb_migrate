       IDENTIFICATION DIVISION.
       PROGRAM-ID. JTN98U.
      *********************************************************
      *    PROGRAM         :  ＳＴＮ№日付別ファイル作成      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0256".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0064".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  W-DATE         PIC  9(006).
           02  W-SNGP  REDEFINES W-DATE.
             03  W-SNEN       PIC  9(002).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-NGD.
             03  W-NEND       PIC  9(004).
             03  W-GETD       PIC  9(002).
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-GET        PIC  9(002).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
      *FD  JSTRF
       01  JSTRF_JTN98U.
           02  JSTRF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  JSTRF_LNAME    PIC  X(012) VALUE "JSTRF_JTN98U".
           02  F              PIC  X(001).
           02  JSTRF_KEY1     PIC  X(100) VALUE SPACE.
           02  JSTRF_SORT     PIC  X(100) VALUE SPACE.
           02  JSTRF_IDLST    PIC  X(100) VALUE SPACE.
           02  JSTRF_RES      USAGE  POINTER.
       01  JSTR-R.
           02  JSTR-DNO       PIC  9(06).
           02  F              PIC  X(01).
           02  JSTR-DC        PIC  9(01).
           02  F              PIC  X(08).
           02  JSTR-DATE.
             03  JSTR-NG      PIC  9(06).
             03  F            PIC  9(02).
           02  JSTR-TCD       PIC  9(04).
           02  F              PIC  X(198).
           02  JSTR-NC        PIC  9(01).
           02  JSTR-STN       PIC  X(03).
           02  F              PIC  X(26).
       77  F                  PIC  X(01).
      *FD  SHSSF
       01  SHSSF_JTN98U.
           02  SHSSF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SHSSF_LNAME    PIC  X(012) VALUE "SHSSF_JTN98U".
           02  F              PIC  X(001).
           02  SHSSF_KEY1     PIC  X(100) VALUE SPACE.
           02  SHSSF_SORT     PIC  X(100) VALUE SPACE.
           02  SHSSF_IDLST    PIC  X(100) VALUE SPACE.
           02  SHSSF_RES      USAGE  POINTER.
       01  SHSS-R.
           02  SHSS-DNO       PIC  9(06).
           02  SHSS-DC        PIC  9(01).
           02  SHSS-DATE      PIC  9(08).
           02  SHSS-TCD       PIC  9(04).
           02  SHSS-NC        PIC  9(01).
           02  SHSS-STN       PIC  X(03).
           02  F              PIC  X(41).
       77  F                  PIC  X(01).
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
                "＊＊＊　　ＳＴＮ№日付別ワーク作成　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(021) VALUE
                  "【      年   月分  】".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-NEN   PIC  9(004).
             03  A-GET   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "**  DATA ﾅｼ  **".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "351" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "13" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "13" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "13" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "13" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "13" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "13" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "13" "44" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "28" "21" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "22" "30" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "12" "0" "6" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "12" "32" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "12" "39" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "47" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "25" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "25" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           ACCEPT W-DATE FROM DATE.
           COMPUTE W-NEND = 2000 + W-SNEN.
           MOVE W-SGET TO W-GETD.
           SUBTRACT 1 FROM W-GETD.
           IF  W-GETD = ZERO
               MOVE 12 TO W-GETD
               SUBTRACT 1 FROM W-NEND
           END-IF
           MOVE W-NGD TO W-NG.
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-NEN = ZERO
               MOVE W-NEND TO W-NEN
               CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-GET = ZERO
               MOVE W-GETD TO W-GET
               CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU
           END-IF
           IF  W-GET > 12
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0256ID.
           MOVE WK0256ID TO JSTRF_PNAME1.
           MOVE W-FID2 TO WK0064ID.
           MOVE WK0064ID TO SHSSF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSTRF_PNAME1 " " BY REFERENCE JSTRF_IDLST "0".
       M-25.
      *           READ JSTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSTRF_PNAME1 BY REFERENCE JSTR-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JSTRF_IDLST JSTRF_PNAME1
               GO TO M-95
           END-IF
           IF  JSTR-NG < W-NG
               GO TO M-25
           END-IF
           IF  JSTR-NG > W-NG
               GO TO M-95
           END-IF
           CALL "DB_F_Open" USING
            "OUTPUT" SHSSF_PNAME1 " " BY REFERENCE SHSSF_IDLST "0".
       M-30.
           MOVE ZERO TO SHSS-R.
           MOVE JSTR-DNO TO SHSS-DNO.
           MOVE JSTR-DC TO SHSS-DC.
           MOVE JSTR-DATE TO SHSS-DATE.
           MOVE JSTR-TCD TO SHSS-TCD.
           MOVE JSTR-NC TO SHSS-NC.
           MOVE JSTR-STN TO SHSS-STN.
      *           WRITE SHSS-R.
      *//////////////
           CALL "DB_Insert" USING
            SHSSF_PNAME1 SHSSF_LNAME SHSS-R RETURNING RET.
      *
      *           READ JSTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSTRF_PNAME1 BY REFERENCE JSTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JSTR-NG = W-NG
               GO TO M-30
           END-IF.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE JSTRF_IDLST JSTRF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SHSSF_IDLST SHSSF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

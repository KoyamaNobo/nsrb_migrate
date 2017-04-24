       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN120.
      *********************************************************
      *    PROGRAM         :  履物棚卸入力Ｆ　コード一括変更  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :                                  *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-MHCD         PIC  9(006).
           02  W-SHCD         PIC  9(006).
           02  W-DMM          PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-HCD          PIC  9(006).
           02  W-ISU          PIC  9(003).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIHIM.
           COPY LIHTIM.
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　履物棚卸入力ファイル　品名変更　＊＊＊".
           02  FILLER  PIC  X(010) VALUE
                "変更前ｺｰﾄﾞ".
           02  FILLER  PIC  X(010) VALUE
                "終了=ｆ･9".
           02  FILLER  PIC  N(001) VALUE "↓".
           02  FILLER  PIC  X(010) VALUE
                "変更後ｺｰﾄﾞ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-MHCD  PIC  9(006).
           02  A-SHCD  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MHCD  PIC  N(024).
           02  D-SHCD  PIC  N(024).
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(025) VALUE
                  "***  ﾋﾝﾒｲ ﾅｼ  ***        ".
             03  E-ME2   PIC  X(025) VALUE
                  "***  REWRITE ｴﾗｰ  ***    ".
             03  E-ME3   PIC  X(025) VALUE
                  "***  DATA ﾅｼ  ***        ".
             03  E-ME4   PIC  X(025) VALUE
                  "***  ｲﾘｽｳ ｴﾗｰ  ***       ".
             03  E-ME5   PIC  X(025) VALUE
                  "***  ｺｰﾄﾞ ｴﾗｰ  ***       ".
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-KEY   PIC  X(007).
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
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
           "C-MID" " " "0" "0" "102" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "15" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "X" "7" "6" "10" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "X" "8" "18" "10" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "9" "36" "2" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "X" "11" "6" "10" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "X" "20" "40" "22" "05C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-MHCD" "9" "7" "18" "6" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-MHCD" BY REFERENCE W-MHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-SHCD" "9" "11" "18" "6" "A-MHCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-SHCD" BY REFERENCE W-SHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "57" "1" "A-SHCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "96" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-MHCD" "N" "7" "27" "48" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING
           "D-MHCD" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-SHCD" "N" "11" "27" "48" "D-MHCD" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-SHCD" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "228" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "228" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "25" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "25" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" "X" "24" "15" "25" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME4" "X" "24" "15" "25" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME5" "X" "24" "15" "25" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME78" "N" "24" "5" "4" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME78" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "X" "24" "45" "7" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE HTI-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" " " "24" "0" "80" "E-KEY" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01E-CL" "X" "24" "1" "40" " " "E-CL" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-CL" "X" "24" "41" "40" "01E-CL" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-10.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-MHCD "A-MHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
      *
           MOVE W-MHCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           CALL "SD_Output" USING "D-MHCD" D-MHCD "p" RETURNING RESU.
           MOVE HI-ISU TO W-ISU.
           IF  HI-HCD NOT = HI-MHCD
               MOVE HI-MHCD TO W-HCD
           ELSE
               MOVE ZERO TO W-HCD
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD "A-SHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
      *
           MOVE W-SHCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-20
           END-IF
           CALL "SD_Output" USING "D-SHCD" D-SHCD "p" RETURNING RESU.
           IF (HI-HCD NOT = W-HCD) AND (HI-MHCD NOT = W-HCD) AND
              (HI-MHCD NOT = W-MHCD)
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-20
           END-IF
           IF  HI-ISU NOT = W-ISU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-DMM = 9
               GO TO M-15
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-25
           END-IF
      *
           MOVE 0 TO W-DC.
           CALL "DB_F_Open" USING
            "I-O" HTI-M_PNAME1 "SHARED" BY REFERENCE HTI-M_IDLST "1"
            "HTI-KEY" BY REFERENCE HTI-KEY.
       M-30.
      *           READ HTI-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HTI-M_PNAME1 BY REFERENCE HTI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           IF  W-MHCD NOT = HTI-HCD
               GO TO M-30
           END-IF
           MOVE W-SHCD TO HTI-HCD.
      *           REWRITE HTI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HTI-M_PNAME1 HTI-M_LNAME HTI-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HTI-M_IDLST HTI-M_PNAME1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF
           GO TO M-30.
       M-35.
           CALL "DB_F_Close" USING
            BY REFERENCE HTI-M_IDLST HTI-M_PNAME1.
           IF  W-DC = 0
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-10.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

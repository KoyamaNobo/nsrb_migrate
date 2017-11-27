       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HMY520.
       AUTHOR.        S-NAKAO.
      *********************************************************
      *    PROGRAM         :  都道府県マスター　更新　　      *
      *    PRINTER TYPE    :  ****                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/20                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  CHK            PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LIHKBM.
       01  TM-GF_HMY520.
           02  TM-GF_PNAME1  PIC  X(009)  VALUE SPACE.
           02  F             PIC  X(001).
           02  TM-GF_LNAME   PIC  X(012)  VALUE "TM-GF_HMY520".
           02  F             PIC  X(001).
           02  TM-GF_KEY1    PIC  X(100)  VALUE SPACE.
           02  TM-GF_KEY2    PIC  X(100)  VALUE SPACE.
           02  TM-GF_SORT    PIC  X(100)  VALUE SPACE.
           02  TM-GF_IDLST   PIC  X(100)  VALUE SPACE.
           02  TM-GF_RES     USAGE  POINTER.
       01  TG-R.
           02  G-TCD          PIC  9(004).
           02  G-KC           PIC  9(002).
           02  G-TC1          PIC  9(002).
           02  G-TC2          PIC  9(002).
           02  G-GU           PIC S9(010).
           02  G-AR           PIC S9(010).
           02  F              PIC  X(034).
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
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(019) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
              "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
              "＊＊＊　都道府県マスター　更新　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
              "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME2.
               04  FILLER  PIC  X(033) VALUE
                    "***  REWRITE ｴﾗｰ (       )    ***".
               04  FILLER  PIC  X(007).
               04  FILLER  PIC  9(001).
             03  E-ME3.
               04  FILLER  PIC  X(027) VALUE
                    "***  HKBM ﾅｼ (       )  ***".
               04  FILLER  PIC  X(007).
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
             03  E-TCD     PIC  9(004).
             03  E-STAT    PIC  X(002).
             03  E-CL      PIC  X(050) VALUE
                  "                                                  ".
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "266" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "38" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "38" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "38" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "38" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "38" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "38" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "38" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "141" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "141" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" " " "24" "0" "41" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME2" "X" "24" "15" "33" " " "E-ME2" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME2" "X" "24" "33" "7" "01E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME2" BY REFERENCE HKB-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03E-ME2" "9" "24" "42" "1" "02E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03E-ME2" BY REFERENCE CHK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" " " "24" "0" "34" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME3" "X" "24" "15" "27" " " "E-ME3" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME3" "X" "24" "29" "7" "01E-ME3" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME3" BY REFERENCE HKB-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-TCD" "9" "24" "51" "4" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE G-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-STAT" "X" "24" "10" "2" "E-TCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                              RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO TM-GF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TM-GF_PNAME1 " " BY REFERENCE TM-GF_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
       M-10.
      *           READ HKBM NEXT RECORD AT END 
      *///////////////
           CALL "DB_Read" USING
            "AT END" HKBM_PNAME1 BY REFERENCE HKB-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF.
           IF  HKB-NO > 01
               GO TO M-15
           END-IF.
           MOVE ZERO TO HKB-KIN.
      *           REWRITE HKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               MOVE 1 TO CHK
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                              RETURNING RESU
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" 
                                              RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                              RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF.
           GO TO M-10.
       M-15.
      *           READ TM-GF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TM-GF_PNAME1 BY REFERENCE TG-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                              RETURNING RESU.
           IF  G-TC1 > 89
               GO TO M-15
           END-IF.
           IF  G-GU = ZERO
               GO TO M-15
           END-IF.
           MOVE SPACE TO HKB-KEY.
           MOVE "01" TO HKB-NO.
           MOVE G-KC TO HKB-TDFK.
      *           READ HKBM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME3" E-ME3 "p" 
                                              RETURNING RESU
               CALL "SD_Output" USING "E-TCD" E-TCD "p" 
                                              RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                              RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF.
           ADD G-GU TO HKB-KIN.
      *           REWRITE HKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               MOVE 2 TO CHK
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                              RETURNING RESU
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" 
                                              RETURNING RESU
               CALL "SD_Output" USING "E-TCD" E-TCD "p" 
                                              RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                              RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF.
           GO TO M-15.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE TM-GF_IDLST TM-GF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

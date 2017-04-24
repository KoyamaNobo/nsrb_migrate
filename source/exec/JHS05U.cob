       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS05U.
      **************************************************
      *****    受注受信データチェック（ナフコ）    *****
      **************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  W-DATA.
           02  CHK            PIC  9(001) VALUE 0.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
      *
           COPY LITDNN.
      *FD  JCANF
       01  JCANF_JHS05U.
           02  JCANF_PNAME1   PIC  X(005) VALUE "JCANF".
           02  F              PIC  X(001).
           02  JCANF_LNAME    PIC  X(012) VALUE "JCANF_JHS05U".
           02  F              PIC  X(001).
           02  JCANF_KEY1     PIC  X(100) VALUE SPACE.
           02  JCANF_SORT     PIC  X(100) VALUE SPACE.
           02  JCANF_IDLST    PIC  X(100) VALUE SPACE.
           02  JCANF_RES      USAGE  POINTER.
       01  JCAN-R.
           02  JCAN-RC1       PIC  X(001).
           02  JCAN-DR.
             03  JCAN-ID      PIC  X(001).
             03  F            PIC  X(254).
           02  JCAN-HR    REDEFINES JCAN-DR.
             03  JCANH-DC1    PIC  9(002).
             03  JCANH-DNO.
               04  F          PIC  X(002).
               04  JCANH-DNOD PIC  9(007).
             03  JCANH-STC.
               04  JCANH-SCD  PIC  9(002).
               04  JCANH-TCD  PIC  9(003).
               04  F          PIC  X(004).
             03  F            PIC  X(002).
             03  JCANH-BCD    PIC  9(002).
             03  JCANH-DPC    PIC  9(002).
             03  JCANH-HNGP   PIC  9(006).
             03  JCANH-NNGP   PIC  9(006).
             03  JCANH-THC    PIC  9(006).
             03  JCANH-STA    PIC  X(002).
             03  JCANH-SNA    PIC  X(015).
             03  JCANH-TNA    PIC  X(015).
             03  JCANH-TSN    PIC  X(015).
             03  JCANH-TST    PIC  X(012).
             03  JCANH-HCC    PIC  9(001).
             03  JCANH-F1     PIC  X(022).
             03  JCANH-DH1    PIC  9(001).
             03  JCANH-RC2    PIC  X(001).
             03  JCANH-DC2    PIC  9(002).
             03  JCANH-AR     PIC  X(007).
             03  JCANH-DUR    PIC  X(026).
             03  JCANH-DSHR   PIC  X(014).
             03  JCANH-DSMR   PIC  X(007).
             03  JCANH-ER     PIC  X(005).
             03  JCANH-FSR    PIC  X(015).
             03  JCANH-FUR    PIC  X(007).
             03  JCANH-LCR    PIC  X(016).
             03  JCANH-LUR    PIC  X(020).
             03  JCANH-LSR    PIC  X(007).
             03  JCANH-DH2    PIC  9(001).
           02  JCAN-MR    REDEFINES JCAN-DR.
             03  JCANM-DC1    PIC  9(002).
             03  JCANM-DGN    PIC  9(002).
             03  JCANM-JAN    PIC  X(013).
             03  JCANM-GAR    PIC  X(006).
             03  F            PIC  X(001).
             03  JCANM-TNI    PIC  X(003).
             03  JCANM-SU     PIC  9(005).
             03  F            PIC  X(001).
             03  JCANM-GTN    PIC  9(007).
             03  F            PIC  X(002).
             03  JCANM-UTN    PIC  9(007).
             03  JCANM-GKIN   PIC  9(010).
             03  JCANM-UKIN   PIC  9(010).
             03  F            PIC  X(009).
             03  JCANM-SHN    PIC  X(025).
             03  JCANM-HSC    PIC  X(008).
             03  JCANM-COR    PIC  X(006).
             03  JCANM-SIZ    PIC  X(005).
             03  F            PIC  X(004).
             03  JCANM-DH1    PIC  9(001).
             03  JCANM-RC2    PIC  X(001).
             03  JCANM-DC2    PIC  9(002).
             03  JCANM-KKK    PIC  X(025).
             03  JCANM-PCH    PIC  X(001).
             03  JCANM-PSI    PIC  X(001).
             03  JCANM-PBM    PIC  9(002).
             03  JCANM-PJAN   PIC  X(013).
             03  JCANM-PSHN   PIC  X(020).
             03  JCANM-PKKK   PIC  X(020).
             03  JCANM-PUTN   PIC  9(007).
             03  JCANM-PMS    PIC  9(005).
             03  F            PIC  X(030).
             03  JCANM-DH2    PIC  9(001).
      *FD  TDNNW
       01  TDNNW_JHS05U.
           02  TDNNW_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TDNNW_LNAME    PIC  X(012) VALUE "TDNNW_JHS05U".
           02  F              PIC  X(001).
           02  TDNNW_KEY1     PIC  X(100) VALUE SPACE.
           02  TDNNW_SORT     PIC  X(100) VALUE SPACE.
           02  TDNNW_IDLST    PIC  X(100) VALUE SPACE.
           02  TDNNW_RES      USAGE  POINTER.
       01  TDNNW-R.
           02  F              PIC  X(008).
           02  TDNNW-STC.
             03  TDNNW-SCD    PIC  9(002).
             03  TDNNW-TCD    PIC  9(003).
             03  F            PIC  X(004).
           02  TDNNW-DNO.
             03  F            PIC  X(002).
             03  TDNNW-DNOD   PIC  9(007).
           02  F              PIC  X(038).
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
                "＊＊＊　　受注受信データ　チェック　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　（ナフコ）　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  N(008) VALUE
                  "累積データ　なし".
             03  E-ME2   PIC  N(006) VALUE
                  "データ　なし".
             03  E-ME3   PIC  N(006) VALUE
                  "データエラー".
             03  E-ME4   PIC  N(009) VALUE
                  "データエラー　重複".
           COPY LSSEM.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "308" " " " " RETURNING RESU.
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
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "58" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "58" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "N" "24" "15" "16" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "N" "24" "15" "12" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "N" "24" "15" "12" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "N" "24" "15" "18" "E-ME3" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO TDNNW_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TDNNW_PNAME1 " " BY REFERENCE TDNNW_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" TDNNF_PNAME1 "SHARED" BY REFERENCE TDNNF_IDLST "1"
            "TDNN1-KEY" BY REFERENCE TDNN1-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JCANF_PNAME1 " " BY REFERENCE JCANF_IDLST "0".
       M-10.
      *           READ TDNNW AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TDNNW_PNAME1 BY REFERENCE TDNNW-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
      *           READ TDNNF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               MOVE 9 TO CHK
           END-IF
      *           READ JCANF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" JCANF_PNAME1 BY REFERENCE JCAN-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  JCAN-RC1 NOT = "A"
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-15.
           IF  JCAN-RC1 NOT = "B"
               GO TO M-20
           END-IF
           IF (JCANH-STC = TDNNW-STC) AND (JCANH-DNO = TDNNW-DNO)
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  CHK = 9
               GO TO M-20
           END-IF
           IF (JCANH-STC = TDNN1-STC) AND (JCANH-DNO = TDNN1-DNO)
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-20.
      *           READ JCANF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" JCANF_PNAME1 BY REFERENCE JCAN-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           GO TO M-15.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE JCANF_IDLST JCANF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNNW_IDLST TDNNW_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNNF_IDLST TDNNF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

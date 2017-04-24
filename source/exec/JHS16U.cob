       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS16U.
      ********************************************************
      *****    受注受信データ生成（赤ちゃん本舗）        *****
      ********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       77  W-INV              PIC  9(001) VALUE 0.
       01  W-DATA.
           02  W-DD.
             03  W-DNO        PIC  9(007).
             03  W-HNO        PIC  9(009).
             03  W-DPC        PIC  9(002).
             03  W-HNGP       PIC  9(006).
             03  W-NNGP       PIC  9(006).
             03  W-THC        PIC  9(006).
             03  W-STC        PIC  9(007).
             03  W-BI         PIC  X(010).
             03  W-SNGP       PIC  9(008).
             03  W-HNA        PIC  X(006).
             03  W-ZON        PIC  9(004).
           02  W-DGN          PIC  9(002).
           02  W-C            PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-ASIZD.
             03  W-ASIZ  OCCURS   4.
               04  W-SIZD  OCCURS  10.
                 05  W-SIZ    PIC  X(004).
           02  W-DSZ          PIC  X(004).
           02  W-DSZD  REDEFINES W-DSZ.
             03  F            PIC  X(003).
             03  W-DSZH       PIC  X(001).
           02  W-MSIZ.
             03  F            PIC  X(040) VALUE
                  "                            28.029.030.0".
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.0    ".
             03  F            PIC  X(040) VALUE
                  "24.024.525.025.526.026.527.027.5        ".
           02  W-AHN          PIC  N(024).
           02  W-AHND  REDEFINES W-AHN.
             03  W-HND   OCCURS  24.
               04  W-HN       PIC  N(001).
           02  W-COR          PIC  N(004).
           02  W-ANAD  REDEFINES W-COR.
             03  W-NAD  OCCURS   4.
               04  W-NA       PIC  N(001).
           02  CNT            PIC  9(002).
           02  CNTD           PIC  9(002).
      *
           COPY LITDNA.
           COPY LICODE.
           COPY LIAHNH.
           COPY LIHIM.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　受注受信データ　生成　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　（赤ちゃん本舗）　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  N(006) VALUE
                  "データ　なし".
             03  E-ME2   PIC  N(007) VALUE
                  "データエラー１".
             03  E-ME3   PIC  N(007) VALUE
                  "データエラー２".
             03  E-ME4   PIC  N(005) VALUE
                  "行　エラー".
             03  E-ME6   PIC  X(027) VALUE
                  "***  TDNAF REWRITE ｴﾗｰ  ***".
             03  E-ME9   PIC  X(020) VALUE
                  "***  JANｺｰﾄﾞ ﾅｼ  ***".
             03  E-ME10  PIC  X(019) VALUE
                  "***  ﾉｳﾋﾝｻｷ ﾅｼ  ***".
             03  E-ME11  PIC  X(025) VALUE
                  "***  ﾉｳﾋﾝｻｷ ﾁｮｸｿｳ ﾅｼ  ***".
             03  E-ME13  PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-DNO   PIC  9(007).
             03  E-JAN   PIC  X(013).
             03  E-STC   PIC  9(007).
           COPY LSSEM.
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
            "C-MID" " " "0" "0" "280" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "40" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "40" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "40" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "40" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "40" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "40" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "184" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "184" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "N" "24" "15" "12" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "N" "24" "15" "14" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "N" "24" "15" "14" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "N" "24" "15" "10" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "27" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "20" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "19" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "25" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "16" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-DNO" "9" "24" "59" "7" "E-ME13" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-DNO" BY REFERENCE TDNA-DNO "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JAN" "X" "24" "44" "13" "E-DNO" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JAN" BY REFERENCE TDNA-JAN "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STC" "9" "24" "44" "7" "E-JAN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STC" BY REFERENCE TDNA-STC "7" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "INPUT" AHNHF_PNAME1 "SHARED" BY REFERENCE AHNHF_IDLST "1"
            "AHNH-KEY" BY REFERENCE AHNH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "I-O" TDNAF_PNAME1 "SHARED" BY REFERENCE TDNAF_IDLST "1"
            "TDNA-KEY" BY REFERENCE TDNA-KEY.
           MOVE W-MSIZ TO W-ASIZD.
       M-10.
      *           READ TDNAF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  TDNA-RC = 0
               GO TO M-10
           END-IF
           IF (TDNA-TNA NOT = ALL "＊") AND (TDNA-CCD > 2) AND
              (TDNA-COR NOT = ALL "＊") AND (TDNA-HCD NOT = ZERO)
                                          AND (TDNA-SIZ NOT = "****")
               GO TO M-10
           END-IF
           MOVE 0 TO CHK.
      *
           IF (TDNA-TNA NOT = ALL "＊") AND (TDNA-CCD > 2)
               GO TO M-20
           END-IF
           MOVE TDNA-STC TO AHNH-KEY.
      *           READ AHNHF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AHNHF_PNAME1 BY REFERENCE AHNH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STC" E-STC "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DNO" E-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-20
           END-IF
           IF  AHNH-CCD < 2
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STC" E-STC "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DNO" E-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-20
           END-IF
           MOVE AHNH-NHSN TO TDNA-TNA.
           MOVE AHNH-CCD TO TDNA-CCD.
           MOVE 1 TO CHK.
       M-20.
           IF (TDNA-COR NOT = ALL "＊") AND (TDNA-HCD NOT = ZERO)
                                          AND (TDNA-SIZ NOT = "****")
               GO TO M-30
           END-IF
           MOVE SPACE TO W-DSZ W-COR.
           MOVE ZERO TO CODE-KEY.
           MOVE TDNA-JAN TO CODE-JAN.
      *           START CODEF KEY NOT < CODE-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CODEF_PNAME1 "CODE-KEY" " NOT < " CODE-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DNO" E-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-30
           END-IF
      *           READ CODEF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DNO" E-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-30
           END-IF
           IF (CODE-TCD NOT = ZERO) OR (CODE-JAN NOT = TDNA-JAN)
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DNO" E-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-30
           END-IF
           MOVE CODE-HCD TO HI-KEY.
           MOVE 1 TO CHK.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DNO" E-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-30
           END-IF
           MOVE HI-HCD TO TDNA-HCD.
           PERFORM COR-RTN THRU COR-EX.
           MOVE W-COR TO TDNA-COR.
           IF (CODE-SIZ < 1 OR > 4) OR (CODE-SNO < 1 OR > 10)
               GO TO M-30
           END-IF
           MOVE W-SIZ(CODE-SIZ,CODE-SNO) TO W-DSZ.
           IF  W-DSZ = SPACE
               GO TO M-30
           END-IF
           IF  HI-HKB = 1
               MOVE 5 TO W-DSZH
           END-IF
           MOVE W-DSZ TO TDNA-SIZ.
       M-30.
      *           REWRITE TDNA-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDNAF_PNAME1 TDNAF_LNAME TDNA-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DNO" E-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STC" E-STC "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-10.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE AHNHF_IDLST AHNHF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNAF_IDLST TDNAF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       COR-RTN.
           MOVE SPACE TO W-AHN W-COR.
           MOVE HI-NAME TO W-AHN.
           MOVE ZERO TO CNT.
       COR-010.
           ADD 1 TO CNT.
           IF  CNT < 25
               IF  W-HN(CNT) NOT = SPACE
                   GO TO COR-010
               END-IF
           END-IF
           ADD 1 TO CNT.
           IF  CNT < 25
               IF  W-HN(CNT) NOT = SPACE
                   GO TO COR-010
               END-IF
           END-IF.
       COR-020.
           ADD 1 TO CNT.
           IF  CNT > 24
               GO TO COR-EX
           END-IF
           IF  W-HN(CNT) = SPACE
               GO TO COR-020
           END-IF
           MOVE ZERO TO CNTD.
       COR-030.
           ADD 1 TO CNTD.
           IF  CNTD > 4
               GO TO COR-EX
           END-IF
           MOVE W-HN(CNT) TO W-NA(CNTD).
           ADD 1 TO CNT.
           IF  CNT < 25
               GO TO COR-030
           END-IF.
       COR-EX.
           EXIT.

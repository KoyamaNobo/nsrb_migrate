       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG950.
      *******************************************************
      *    PROGRAM        :  購買月次更新クリア (JS-SIGN:0) *
      *                   :  購買棚卸更新       (JS-SIGN:1) *
      *    PRINTER TYPE   :  ****                           *
      *    SCREEN         :  ******                         *
      *        変更　　　 :  62/06/11                       *
      *    COMPILE TYPE   :  COBOL                          *
      *******************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       01  W-DATA.
           02  W-NG           PIC  9(006).
           02  W-NGD   REDEFINES W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-DNG.
             03  W-DNEN       PIC  9(004).
             03  W-DGET       PIC  9(002).
           02  W-FILE         PIC  X(013).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIJM.
           COPY LIJTM.
           COPY LISTM.
           COPY LIHSSF.
      *FD  JSSR-F
       01  JSSR-F_KBG950.
           02  JSSR-F_PNAME1  PIC  X(005) VALUE "JSSRF".
           02  F              PIC  X(001).
           02  JSSR-F_LNAME   PIC  X(013) VALUE "JSSR-F_KBG950".
           02  F              PIC  X(001).
           02  JSSR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  JSSR-F_SORT    PIC  X(100) VALUE SPACE.
           02  JSSR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  JSSR-F_RES     USAGE  POINTER.
       01  JSSR-R.
           02  JS-DK          PIC  9(002).
           02  JS-SD          PIC  9(008).
           02  JS-SCD         PIC  9(004).
           02  JS-JCD         PIC  9(006).
           02  JS-SSU         PIC S9(007)V9(02).
           02  JS-TN          PIC S9(006)V9(02).
           02  JS-KIN         PIC S9(008).
           02  F              PIC  X(007).
           02  JS-CD          PIC  9(006).
           02  F              PIC  X(013).
           02  JS-YOC         PIC  9(001).
           02  JS-TNC         PIC  9(001).
           02  JS-SEC         PIC  9(001).
           02  JS-SC          PIC  9(001).
           02  F              PIC  X(027).
       77  F                  PIC  X(001).
      *FD  HAR-F
       01  HAR-F_KBG950.
           02  HAR-F_PNAME1   PIC  X(004) VALUE "HARF".
           02  F              PIC  X(001).
           02  HAR-F_LNAME    PIC  X(012) VALUE "HAR-F_KBG950".
           02  F              PIC  X(001).
           02  HAR-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HAR-F_SORT     PIC  X(100) VALUE SPACE.
           02  HAR-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HAR-F_RES      USAGE  POINTER.
       01  HAR-R              PIC  X(032).
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
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　購買　月次更新　クリア　　＊＊＊".
           02  FILLER  PIC  X(018) VALUE
                  "［　  年  月分　］".
       01  C-MID2.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　購買　棚卸更新　　　　＊＊＊".
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC Z9 .
       01  E-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(025) VALUE
                  "***  STM REWRITE ｴﾗｰ  ***".
             03  E-ME2   PIC  X(025) VALUE
                  "***  JTM REWRITE ｴﾗｰ  ***".
             03  E-ME5   PIC  X(018) VALUE
                  "***  DATEM ﾅｼ  ***".
             03  E-ME6   PIC  X(027) VALUE
                  "***  DATEM REWRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(015) VALUE
                  "***  JM ﾅｼ  ***".
             03  E-ME13  PIC  X(025) VALUE
                  "***  HSSF DELETE ｴﾗｰ  ***".
             03  E-SCD   PIC  9(004).
             03  E-JCD   PIC  9(006).
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
            "C-MID" " " "0" "0" "274" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "7" "10" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "8" "10" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "9" "10" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "X" "20" "20" "22" "06C-MID" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "60" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "N" "6" "10" "42" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID1" "X" "12" "22" "18" "01C-MID1" " "
            RETURNING RESU.
      *C-MID2
       CALL "SD_Init" USING 
            "C-MID2" " " "0" "0" "42" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID2" "N" "6" "10" "42" " " "C-MID2" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "4" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NG" " " "12" "0" "4" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NG" "9" "12" "26" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NG" "Z9" "12" "30" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-GET "2" "0" RETURNING RESU.
      *E-ERR
       CALL "SD_Init" USING 
            "E-ERR" " " "0" "0" "145" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ERR" " " "24" "0" "145" " " "E-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "25" " " "01E-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "25" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "27" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "15" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "25" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-SCD" "9" "24" "50" "4" "E-ME13" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-SCD" BY REFERENCE ST-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JCD" "9" "24" "50" "6" "E-SCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JCD" BY REFERENCE JT-KEY "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               GO TO M-05
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "C-MID2" C-MID2 "p" RETURNING RESU
               GO TO M-20
           END-IF
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
           INITIALIZE W-DATA.
           COPY LIBCPR.
           MOVE D-NBNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU
           CALL "DB_F_Open" USING
            "I-O SEQUENTIAL" ST-M_PNAME1 " " BY REFERENCE ST-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
       M-10.
      *           READ ST-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" ST-M_PNAME1 BY REFERENCE ST-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           MOVE ST-KZ TO ST-ZKZ.
           MOVE ST-KZZ TO ST-ZKZZ.
           MOVE ZERO TO ST-TSK ST-TSKZ ST-THK ST-THKZ.
      *           REWRITE ST-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            ST-M_PNAME1 ST-M_LNAME ST-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SCD" E-SCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-10.
       M-15.
           CALL "DB_F_Close" USING BY REFERENCE ST-M_IDLST ST-M_PNAME1.
       M-20.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "I-O SEQUENTIAL" JT-M_PNAME1 " " BY REFERENCE JT-M_IDLST "1"
            "JT-KEY" BY REFERENCE JT-KEY.
       M-25.
      *           READ JT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JT-M_PNAME1 BY REFERENCE JT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
           IF  JS-SIGN = 0
               COMPUTE JT-ZKS = JT-ZKS + JT-SSU - JT-HSU
               MOVE ZERO TO JT-SSU JT-SIK JT-HSU
           ELSE
               MOVE JT-TSU TO JT-ZKS
           END-IF
           MOVE JT-KEY TO J-KEY.
      *           READ J-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD" E-JCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO J-ST
           END-IF
           COMPUTE JT-ZKK = JT-ZKS * J-ST.
           IF  JS-SIGN = 0
               IF  W-GET = 4
                   MOVE JT-ZKS TO JT-CSU
               END-IF
           END-IF
      *           REWRITE J-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            J-M_PNAME1 J-M_LNAME J-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD" E-JCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-25.
       M-30.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JT-M_IDLST JT-M_PNAME1.
           IF  JS-SIGN NOT = 0
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "OUTPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" HAR-F_PNAME1 " " BY REFERENCE HAR-F_IDLST "0".
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HAR-F_IDLST HAR-F_PNAME1.
      *
           MOVE W-NG TO W-DNG.
           SUBTRACT 1 FROM W-DGET.
           IF  W-DGET = ZERO
               SUBTRACT 1 FROM W-DNEN
               MOVE 12 TO W-DGET
           END-IF
           SUBTRACT 1 FROM W-DGET.
           IF  W-DGET = ZERO
               SUBTRACT 1 FROM W-DNEN
               MOVE 12 TO W-DGET
           END-IF
           CALL "DB_F_Open" USING
            "I-O" HSS-F_PNAME1 " " BY REFERENCE HSS-F_IDLST "1"
            "HSS-KEY" BY REFERENCE HSS-KEY.
       M-85.
      *           READ HSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSS-F_PNAME1 BY REFERENCE HSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  W-DNG <= HSS-NG
               GO TO M-85
           END-IF
      *           DELETE HSS-F INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HSS-F_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-85.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE HSS-F_IDLST HSS-F_PNAME1.
      *
           ADD 1 TO W-GET.
           IF  W-GET = 13
               ADD 1 TO W-NEN
               MOVE 1 TO W-GET
           END-IF
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
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE W-NGS TO D-NBNG.
      *           REWRITE DATE-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            M-DATE_PNAME1 M-DATE_LNAME DATE-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
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

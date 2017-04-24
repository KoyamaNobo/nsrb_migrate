       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBD530.
      *********************************************************
      *    PROGRAM         :  購買累積ファイル作成　　　　    *
      *    PRINTER TYPE    :  ****                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/05                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-KBD          PIC  9(008).
           02  W-KBDL  REDEFINES W-KBD.
             03  F            PIC  9(002).
             03  W-KBDS       PIC  9(006).
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GP         PIC  9(004).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
       01  W-DC               PIC  9(001) VALUE 0.
       01  W-FILE             PIC  X(013).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIJM.
      *FD  JSS-F
       01  JSS-F_KBD530.
           02  JSS-F_PNAME1   PIC  X(004) VALUE "JSSF".
           02  F              PIC  X(001).
           02  JSS-F_LNAME    PIC  X(012) VALUE "JSS-F_KBD530".
           02  F              PIC  X(001).
           02  JSS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  JSS-F_KEY2     PIC  X(100) VALUE SPACE.
           02  JSS-F_SORT     PIC  X(100) VALUE SPACE.
           02  JSS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  JSS-F_RES      USAGE  POINTER.
       01  JSS-R.
           02  JS-DC.
             03  JS-DC1       PIC  9(001).
             03  JS-DC2       PIC  9(001).
           02  JS-DATE        PIC  9(008).
           02  JS-SCD         PIC  9(004).
           02  JS-JCD         PIC  9(006).
           02  JS-SU          PIC S9(007)V9(02).
           02  JS-T           PIC S9(006)V9(02).
           02  JS-KIN         PIC S9(008).
           02  F              PIC  X(007).
           02  JS-CD          PIC  9(006).
           02  JS-SJCD        PIC  9(006).
           02  F              PIC  X(007).
           02  JS-YC          PIC  9(001).
           02  JS-TC          PIC  9(001).
           02  JS-SEC         PIC  9(001).
           02  JS-SC          PIC  9(001).
           02  JS-BSC         PIC  9(001).
           02  JS-BKC         PIC  9(002).
           02  JS-KCO         PIC  X(005).
           02  JS-KHC         PIC  9(001).
           02  F              PIC  X(010).
           02  JS-KEY         PIC  X(007).
           02  JS-PCNT        PIC  9(001).
       77  F                  PIC  X(001).
      *FD  HA-F
       01  HA-F_KBD530.
           02  HA-F_PNAME1    PIC  X(003) VALUE "HAF".
           02  F              PIC  X(001).
           02  HA-F_LNAME     PIC  X(011) VALUE "HA-F_KBD530".
           02  F              PIC  X(001).
           02  HA-F_KEY1      PIC  X(100) VALUE SPACE.
           02  HA-F_KEY2      PIC  X(100) VALUE SPACE.
           02  HA-F_SORT      PIC  X(100) VALUE SPACE.
           02  HA-F_IDLST     PIC  X(100) VALUE SPACE.
           02  HA-F_RES       USAGE  POINTER.
       01  HA-R.
           02  HA-DATE        PIC  9(008).
           02  HA-JCD         PIC  9(006).
           02  HA-SU          PIC S9(007)V9(02).
           02  HA-KEY         PIC  X(007).
           02  F              PIC  X(001).
           02  HA-PCNT        PIC  9(001).
       77  F                  PIC  X(001).
      *FD  JSSR-F
       01  JSSR-F_KBD530.
           02  JSSR-F_PNAME1  PIC  X(005) VALUE "JSSRF".
           02  F              PIC  X(001).
           02  JSSR-F_LNAME   PIC  X(013) VALUE "JSSR-F_KBD530".
           02  F              PIC  X(001).
           02  JSSR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  JSSR-F_SORT    PIC  X(100) VALUE SPACE.
           02  JSSR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  JSSR-F_RES     USAGE  POINTER.
       01  JSSR-R             PIC  X(102).
       77  F                  PIC  X(001).
      *FD  HAR-F
       01  HAR-F_KBD530.
           02  HAR-F_PNAME1   PIC  X(004) VALUE "HARF".
           02  F              PIC  X(001).
           02  HAR-F_LNAME    PIC  X(012) VALUE "HAR-F_KBD530".
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
                "＊＊＊　　購買累積ファイル　作成　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "***  JM ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATEM ﾅｼ  ***".
             03  E-ME3   PIC  X(027) VALUE
                  "***  DATEM REWRITE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(025) VALUE
                  "***  JSSRF WRITE ｴﾗｰ  ***".
             03  E-ME8   PIC  X(024) VALUE
                  "***  HARF WRITE ｴﾗｰ  ***".
             03  E-ME10  PIC  X(025) VALUE
                  "***  JSSF DELETE ｴﾗｰ  ***".
             03  E-JSS.
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  X(007).
             03  E-HA.
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  X(007).
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "294" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "42" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "160" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "160" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "27" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "25" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "24" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "25" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JSS" " " "24" "0" "13" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-JSS" "9" "24" "50" "6" " " "E-JSS" RETURNING RESU.
       CALL "SD_From" USING 
            "01E-JSS" BY REFERENCE JS-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-JSS" "X" "24" "58" "7" "01E-JSS" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-JSS" BY REFERENCE JS-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HA" " " "24" "0" "13" "E-JSS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-HA" "9" "24" "50" "6" " " "E-HA" RETURNING RESU.
       CALL "SD_From" USING 
            "01E-HA" BY REFERENCE HA-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-HA" "X" "24" "58" "7" "01E-HA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-HA" BY REFERENCE HA-KEY "7" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JSS-F_PNAME1 " " BY REFERENCE JSS-F_IDLST "1"
            "JS-KEY" BY REFERENCE JS-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HA-F_PNAME1 " " BY REFERENCE HA-F_IDLST "1"
            "HA-KEY" BY REFERENCE HA-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HAR-F_PNAME1 " " BY REFERENCE HAR-F_IDLST "0".
           MOVE ZERO TO W-KBD.
       M-10.
      *           READ JSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSS-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           IF  JS-PCNT = 0
               MOVE 1 TO W-DC
               GO TO M-10
           END-IF
           IF  JS-KCO NOT = SPACE AND ZERO
               IF  JS-JCD > 489999 AND < 499000
                   IF  JS-KHC NOT = 2
                       MOVE 1 TO W-DC
                       GO TO M-10
                   END-IF
               END-IF
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
       M-15.
           MOVE ZERO TO JSSR-R.
           MOVE JSS-R TO JSSR-R.
      *           WRITE JSSR-R.
      *//////////////
           CALL "DB_Insert" USING
            JSSR-F_PNAME1 JSSR-F_LNAME JSSR-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               GO TO M-20
           END-IF
           GO TO M-25.
       M-20.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME4" E-ME4 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-JSS" E-JSS "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT = "34"
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1
               MOVE "JSSRF        " TO W-FILE
               CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "SD_Output" USING " " " " "STOP" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Open" USING
                "EXTEND" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0"
               GO TO M-15
           END-IF
           CALL "SD_Output" USING "E-ME78" E-ME78 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           GO TO M-85.
       M-25.
           IF  W-KBD < JS-DATE
               MOVE JS-DATE TO W-KBD
           END-IF
           GO TO M-10.
       M-40.
      *           READ HA-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HA-F_PNAME1 BY REFERENCE HA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HA-PCNT = 0
               GO TO M-40
           END-IF.
       M-45.
           MOVE ZERO TO HAR-R.
           MOVE HA-R TO HAR-R.
      *           WRITE HAR-R.
      *//////////////
           CALL "DB_Insert" USING
            HAR-F_PNAME1 HAR-F_LNAME HAR-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               GO TO M-50
           END-IF
           GO TO M-40.
       M-50.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME8" E-ME8 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-HA" E-HA "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT = "34"
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HAR-F_IDLST HAR-F_PNAME1
               MOVE "HARF         " TO W-FILE
               CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "SD_Output" USING " " " " "STOP" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Open" USING
                "EXTEND" HAR-F_PNAME1 " " BY REFERENCE HAR-F_IDLST "0"
               GO TO M-45
           END-IF
           CALL "SD_Output" USING "E-ME78" E-ME78 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
       M-85.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSS-F_IDLST JSS-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HA-F_IDLST HA-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HAR-F_IDLST HAR-F_PNAME1.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
           PERFORM S-05 THRU S-10.
           IF  W-DC = 0
               CALL "DB_F_Open" USING
                "OUTPUT" JSS-F_PNAME1 " " BY REFERENCE JSS-F_IDLST "1"
                "JS-KEY" BY REFERENCE JS-KEY
               CALL "DB_F_Close" USING
                BY REFERENCE JSS-F_IDLST JSS-F_PNAME1
           ELSE
               PERFORM S-15 THRU S-30
           END-IF
           CALL "DB_F_Open" USING
            "OUTPUT" HA-F_PNAME1 " " BY REFERENCE HA-F_IDLST "1"
            "HA-KEY" BY REFERENCE HA-KEY.
           CALL "DB_F_Close" USING BY REFERENCE HA-F_IDLST HA-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
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
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE M-DATE_IDLST M-DATE_PNAME1
               GO TO S-10
           END-IF
           MOVE ZERO TO W-NGP.
           MOVE D-KBD TO W-NGPS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-KBD > W-NGP
               MOVE W-KBDS TO D-KBD
           END-IF
      *           REWRITE DATE-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            M-DATE_PNAME1 M-DATE_LNAME DATE-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
       S-10.
           EXIT.
       S-15.
           CALL "DB_F_Open" USING
            "I-O" JSS-F_PNAME1 " " BY REFERENCE JSS-F_IDLST "1"
            "JS-KEY" BY REFERENCE JS-KEY.
       S-20.
      *           READ JSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSS-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-25
           END-IF
           IF  JS-PCNT = 0
               GO TO S-20
           END-IF
           IF  JS-KCO NOT = SPACE AND ZERO
               IF  JS-JCD > 489999 AND < 499000
                   IF  JS-KHC NOT = 2
                       GO TO S-20
                   END-IF
               END-IF
           END-IF
      *           DELETE JSS-F INVALID KEY
      *///////////////
           CALL "DB_Delete" USING JSS-F_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-25
           END-IF
           GO TO S-20.
       S-25.
           CALL "DB_F_Close" USING
            BY REFERENCE JSS-F_IDLST JSS-F_PNAME1.
       S-30.
           EXIT.

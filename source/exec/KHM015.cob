       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHM015.
      *********************************************************
      *    PROGRAM         :  工品累計ファイル　用途区分セット*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  NO                              *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-FILE         PIC  X(013).
       01  ERR-STAT           PIC  X(002).
           COPY LIKHM.
       01  URIR-F_KHM015.
           02  URIR-F_PNAME1   PIC  X(005)  VALUE "URIRF".
           02  F               PIC  X(001).
           02  URIR-F_LNAME    PIC  X(013)  VALUE "URIR-F_KHM015".
           02  F               PIC  X(001).
           02  URIR-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  URIR-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  URIR-F_SORT     PIC  X(100)  VALUE SPACE.
           02  URIR-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  URIR-F_RES      USAGE  POINTER.
       01  URIR-R.
           02  U-DC           PIC  9(001).
           02  U-DATE         PIC  9(008).
           02  U-NGP   REDEFINES U-DATE.
             03  U-NG         PIC  9(006).
             03  F            PIC  9(002).
           02  U-TCD          PIC  9(004).
           02  U-HCD          PIC  X(005).
           02  U-SU           PIC S9(006)V9(02).
           02  U-T            PIC S9(006)V9(02).
           02  U-KIN          PIC S9(008).
           02  U-YC           PIC  9(002).
           02  U-SD           PIC  9(004).
           02  U-NNO          PIC  X(006).
           02  U-HYC          PIC  9(001).
           02  U-CSC          PIC  9(001).
           02  U-BKC          PIC  9(002).
           02  U-SKD          PIC  9(008).
           02  U-DNO          PIC  9(006).
           02  U-GNO          PIC  9(001).
           02  U-JCD          PIC  9(006).
           02  U-GT           PIC S9(006)V9(02).
           02  U-TEK          PIC  N(018).
           02  F              PIC  X(005).
       77  F                  PIC  X(001).
       01  KNHR-F_KHM015.
           02  KNHR-F_PNAME1   PIC  X(005)  VALUE "KNHRF".
           02  F               PIC  X(001).
           02  KNHR-F_LNAME    PIC  X(013)  VALUE "KNHR-F_KHM015".
           02  F               PIC  X(001).
           02  KNHR-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  KNHR-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  KNHR-F_SORT     PIC  X(100)  VALUE SPACE.
           02  KNHR-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  KNHR-F_RES      USAGE  POINTER.
       01  KNHR-R.
           02  NH-NHC         PIC  9(002).
           02  NH-DATE        PIC  9(008).
           02  NH-HCD         PIC  X(005).
           02  F              PIC  X(007).
           02  NH-SU          PIC S9(006)V9(02).
           02  F              PIC  X(016).
           02  NH-YC          PIC  9(002).
           02  F              PIC  X(016).
       77  F                  PIC  X(001).
       01  URIRYR_KHM015.
           02  URIRYR_PNAME1   PIC  X(006)  VALUE "URIRYR".
           02  F               PIC  X(001).
           02  URIRYR_LNAME    PIC  X(013)  VALUE "URIRYR_KHM015".
           02  F               PIC  X(001).
           02  URIRYR_KEY1     PIC  X(100)  VALUE SPACE.
           02  URIRYR_KEY2     PIC  X(100)  VALUE SPACE.
           02  URIRYR_SORT     PIC  X(100)  VALUE SPACE.
           02  URIRYR_IDLST    PIC  X(100)  VALUE SPACE.
           02  URIRYR_RES      USAGE  POINTER.
       01  URIRY-R.
           02  UY-DC          PIC  9(001).
           02  UY-DATE        PIC  9(008).
           02  UY-NGP   REDEFINES UY-DATE.
             03  UY-NG        PIC  9(006).
             03  F            PIC  9(002).
           02  UY-TCD         PIC  9(004).
           02  UY-HCD         PIC  X(005).
           02  UY-SU          PIC S9(006)V9(02).
           02  UY-T           PIC S9(006)V9(02).
           02  UY-KIN         PIC S9(008).
           02  UY-YC          PIC  9(002).
           02  UY-SD          PIC  9(004).
           02  UY-NNO         PIC  X(006).
           02  UY-HYC         PIC  9(001).
           02  UY-CSC         PIC  9(001).
           02  UY-BKC         PIC  9(002).
           02  UY-SKD         PIC  9(008).
           02  UY-DNO         PIC  9(006).
           02  UY-GNO         PIC  9(001).
           02  UY-JCD         PIC  9(006).
           02  UY-GT          PIC S9(006)V9(02).
           02  UY-TEK         PIC  N(018).
           02  F              PIC  X(005).
       77  F                  PIC  X(001).
       01  KNHRYR_KHM015.
           02  KNHRYR_PNAME1   PIC  X(006)  VALUE "KNHRYR".
           02  F               PIC  X(001).
           02  KNHRYR_LNAME    PIC  X(013)  VALUE "KNHRYR_KHM015".
           02  F               PIC  X(001).
           02  KNHRYR_KEY1     PIC  X(100)  VALUE SPACE.
           02  KNHRYR_KEY2     PIC  X(100)  VALUE SPACE.
           02  KNHRYR_SORT     PIC  X(100)  VALUE SPACE.
           02  KNHRYR_IDLST    PIC  X(100)  VALUE SPACE.
           02  KNHRYR_RES      USAGE  POINTER.
       01  KNHRY-R.
           02  NHY-NHC        PIC  9(002).
           02  NHY-DATE       PIC  9(008).
           02  NHY-HCD        PIC  X(005).
           02  F              PIC  X(007).
           02  NHY-SU         PIC S9(006)V9(02).
           02  F              PIC  X(016).
           02  NHY-YC         PIC  9(002).
           02  F              PIC  X(016).
       77  F                  PIC  X(001).
       01  KHTMYR_KHM015.
           02  KHTMYR_PNAME1   PIC  X(006)  VALUE "KHTMYR".
           02  F               PIC  X(001).
           02  KHTMYR_LNAME    PIC  X(013)  VALUE "KHTMYR_KHM015".
           02  F               PIC  X(001).
           02  KHTMYR_KEY1     PIC  X(100)  VALUE SPACE.
           02  KHTMYR_KEY2     PIC  X(100)  VALUE SPACE.
           02  KHTMYR_SORT     PIC  X(100)  VALUE SPACE.
           02  KHTMYR_IDLST    PIC  X(100)  VALUE SPACE.
           02  KHTMYR_RES      USAGE  POINTER.
       01  KHTMY-R.
           02  KHTY-YC        PIC  9(002).
           02  KHTY-NC        PIC  9(001).
           02  KHTY-HCD       PIC  X(005).
           02  F              PIC  X(162).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-AMID.
           02  C-MID.
             03  FILLER  PIC  N(024) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
             03  FILLER  PIC  N(024) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
             03  FILLER  PIC  N(024) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
             03  FILLER  PIC  N(024) VALUE
                  "＊＊＊　　工品累積ファイル　区分セット　　＊＊＊".
             03  FILLER  PIC  N(024) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
             03  FILLER  PIC  N(024) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
             03  FILLER  PIC  N(024) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(023) VALUE
                  "***  KHM ﾅｼ (URIRF) ***".
             03  E-ME2     PIC  X(024) VALUE
                  "***  KHM ﾅｼ (URIRYR) ***".
             03  E-ME3     PIC  X(023) VALUE
                  "***  KHM ﾅｼ (KNHRF) ***".
             03  E-ME4     PIC  X(024) VALUE
                  "***  KHM ﾅｼ (KNHRYR) ***".
             03  E-ME5     PIC  X(024) VALUE
                  "***  KHM ﾅｼ (KHTMYR) ***".
             03  E-KEY     PIC  X(005).
           COPY LSSEM.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-AMID
       CALL "SD_Init" USING
            "C-AMID" " " "0" "0" "336" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "336" " " "C-AMID" RETURNING RESU.
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
            "C-ERR" " " "0" "0" "123" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "123" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "23" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "24" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" "X" "24" "15" "23" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME4" "X" "24" "15" "24" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME5" "X" "24" "15" "24" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-KEY" "X" "24" "45" "5" "E-ME5" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE KH-KEY "5" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                  RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "I-O" URIR-F_PNAME1 " " BY REFERENCE URIR-F_IDLST "0".
       M-10.
      *           READ URIR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" URIR-F_PNAME1 BY REFERENCE URIR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF.
           IF  U-HCD = ZERO OR SPACE
               GO TO M-10
           END-IF.
           MOVE U-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-KEY" E-KEY "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-10
           END-IF.
           IF  U-YC = KH-YC
               GO TO M-10
           END-IF.
           MOVE KH-YC TO U-YC.
      *           REWRITE URIR-R.
      *///////////////
           CALL "DB_Update" USING
            URIR-F_PNAME1 URIR-F_LNAME URIR-R RETURNING RET.
           GO TO M-10.
       M-15.
           CALL "DB_F_Close" USING
            BY REFERENCE URIR-F_IDLST URIR-F_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" URIRYR_PNAME1 " " BY REFERENCE URIRYR_IDLST "0".
       M-20.
      *           READ URIRYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" URIRYR_PNAME1 BY REFERENCE URIRY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF.
           IF  UY-HCD = ZERO OR SPACE
               GO TO M-20
           END-IF.
           MOVE UY-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-KEY" E-KEY "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-20
           END-IF.
           IF  UY-YC = KH-YC
               GO TO M-20
           END-IF.
           MOVE KH-YC TO UY-YC.
      *           REWRITE URIRY-R.
      *///////////////
           CALL "DB_Update" USING
            URIRYR_PNAME1 URIRYR_LNAME URIRY-R RETURNING RET.
           GO TO M-20.
       M-25.
           CALL "DB_F_Close" USING
            BY REFERENCE URIRYR_IDLST URIRYR_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" KNHR-F_PNAME1 " " BY REFERENCE KNHR-F_IDLST "0".
       M-30.
      *           READ KNHR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" KNHR-F_PNAME1 BY REFERENCE KNHR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF.
           MOVE NH-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME3" E-ME3 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-KEY" E-KEY "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-30
           END-IF.
           IF  NH-YC = KH-YC
               GO TO M-30
           END-IF.
           MOVE KH-YC TO NH-YC.
      *           REWRITE KNHR-R.
      *///////////////
           CALL "DB_Update" USING
            KNHR-F_PNAME1 KNHR-F_LNAME KNHR-R RETURNING RET.
           GO TO M-30.
       M-35.
           CALL "DB_F_Close" USING
            BY REFERENCE KNHR-F_IDLST KNHR-F_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" KNHRYR_PNAME1 " " BY REFERENCE KNHRYR_IDLST "0".
       M-40.
      *           READ KNHRYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" KNHRYR_PNAME1 BY REFERENCE KNHRY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF.
           MOVE NHY-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME4" E-ME4 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-KEY" E-KEY "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-40
           END-IF.
           IF  NHY-YC = KH-YC
               GO TO M-40
           END-IF.
           MOVE KH-YC TO NHY-YC.
      *           REWRITE KNHRY-R.
      *///////////////
           CALL "DB_Update" USING
            KNHRYR_PNAME1 KNHRYR_LNAME KNHRY-R RETURNING RET.
           GO TO M-40.
       M-45.
           CALL "DB_F_Close" USING
            BY REFERENCE KNHRYR_IDLST KNHRYR_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" KHTMYR_PNAME1 " " BY REFERENCE KHTMYR_IDLST "0".
       M-50.
      *           READ KHTMYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" KHTMYR_PNAME1 BY REFERENCE KHTMY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF.
           MOVE KHTY-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME5" E-ME5 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-KEY" E-KEY "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-50
           END-IF.
           IF  KHTY-YC = KH-YC
               IF  KHTY-NC = KH-NC
                   GO TO M-50
               END-IF
           END-IF.
           MOVE KH-YC TO KHTY-YC.
           MOVE KH-NC TO KHTY-NC.
      *           REWRITE KHTMY-R.
      *///////////////
           CALL "DB_Update" USING
            KHTMYR_PNAME1 KHTMYR_LNAME KHTMY-R RETURNING RET.
           GO TO M-50.
       M-55.
           CALL "DB_F_Close" USING
            BY REFERENCE KHTMYR_IDLST KHTMYR_PNAME1.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
	       CALL "DB_Close".
           STOP RUN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG450.
      *********************************************************
      *    PROGRAM         :  �ޗ��󕥃��[�N�@�쐬            *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        �ύX�@�@�@  :  62/06/10                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
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
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIJM.
           COPY LIJTM.
      *FD  JSSR-F
       01  JSSR-F_KBG450.
           02  JSSR-F_PNAME1  PIC  X(005) VALUE "JSSRF".
           02  F              PIC  X(001).
           02  JSSR-F_LNAME   PIC  X(013) VALUE "JSSR-F_KBG450".
           02  F              PIC  X(001).
           02  JSSR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  JSSR-F_SORT    PIC  X(100) VALUE SPACE.
           02  JSSR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  JSSR-F_RES     USAGE  POINTER.
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
           02  F              PIC  X(001).
           02  JS-BKC         PIC  9(002).
           02  F              PIC  X(016).
           02  JS-KEY         PIC  X(007).
           02  F              PIC  X(001).
       77  F                  PIC  X(001).
      *FD  HAR-F
       01  HAR-F_KBG450.
           02  HAR-F_PNAME1   PIC  X(004) VALUE "HARF".
           02  F              PIC  X(001).
           02  HAR-F_LNAME    PIC  X(012) VALUE "HAR-F_KBG450".
           02  F              PIC  X(001).
           02  HAR-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HAR-F_SORT     PIC  X(100) VALUE SPACE.
           02  HAR-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HAR-F_RES      USAGE  POINTER.
       01  HA-R.
           02  HA-DATE        PIC  9(008).
           02  HA-JCD         PIC  9(006).
           02  HA-SU          PIC S9(007)V9(02).
           02  HA-KEY         PIC  X(007).
           02  F              PIC  X(002).
       77  F                  PIC  X(001).
      *FD  JUHW-F
       01  JUHW-F_KBG450.
           02  JUHW-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  JUHW-F_LNAME   PIC  X(013) VALUE "JUHW-F_KBG450".
           02  F              PIC  X(001).
           02  JUHW-F_KEY1    PIC  X(100) VALUE SPACE.
           02  JUHW-F_SORT    PIC  X(100) VALUE SPACE.
           02  JUHW-F_IDLST   PIC  X(100) VALUE SPACE.
           02  JUHW-F_RES     USAGE  POINTER.
       01  JUHW-R.
           02  JUHW-KEY       PIC  9(006).
           02  JUHW-NGP       PIC  9(008).
           02  JUHW-KNSC      PIC  9(001).
           02  JUHW-SU        PIC S9(007)V9(02).
           02  JUHW-TN        PIC S9(006)V9(02).
           02  JUHW-KIN       PIC S9(008).
           02  JUHW-SD        PIC  9(006).
           02  JUHW-DNO       PIC  9(007).
           02  JUHW-YC        PIC  9(001).
           02  JUHW-BKC       PIC  9(002).
           02  JUHW-BKNO      PIC  9(002).
           02  F              PIC  X(006).
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
           02  FILLER  PIC  N(020) VALUE
                "����������������������������������������".
           02  FILLER  PIC  N(020) VALUE
                "����������������������������������������".
           02  FILLER  PIC  N(020) VALUE
                "�������@�@�@�@�@�@�@�@�@�@�@�@�@�@������".
           02  FILLER  PIC  N(020) VALUE
                "�������@�@�ޗ��󕥃��[�N�@�쐬�@�@������".
           02  FILLER  PIC  N(020) VALUE
                "�������@�@�@�@�@�@�@�@�@�@�@�@�@�@������".
           02  FILLER  PIC  N(020) VALUE
                "����������������������������������������".
           02  FILLER  PIC  N(020) VALUE
                "����������������������������������������".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(015) VALUE
                  "***  JM ż  ***".
             03  E-JCD   PIC  9(006).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-ERR" " " "0" "0" "83" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "83" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JCD" "9" "24" "40" "6" "E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JCD" BY REFERENCE J-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-JCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO JUHW-F_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" JUHW-F_PNAME1 " " BY REFERENCE JUHW-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JT-M_PNAME1 "SHARED" BY REFERENCE JT-M_IDLST "1"
            "JT-KEY" BY REFERENCE JT-KEY.
       M-10.
      *           READ JT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JT-M_PNAME1 BY REFERENCE JT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           IF  JT-ZC NOT = 0
               GO TO M-10
           END-IF
           IF  JT-ZKS = ZERO
               GO TO M-10
           END-IF
      *
           MOVE JT-KEY TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD" E-JCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               MOVE ZERO TO J-ST
               MOVE 9 TO J-YC
           END-IF
      *
           MOVE ZERO TO JUHW-R.
           MOVE JT-KEY TO JUHW-KEY.
           MOVE JT-ZKS TO JUHW-SU.
           MOVE J-ST TO JUHW-TN.
           MOVE JT-ZKK TO JUHW-KIN.
           MOVE J-YC TO JUHW-YC.
           MOVE J-BKC TO JUHW-BKC.
           MOVE J-BKNO TO JUHW-BKNO.
      *           WRITE JUHW-R.
      *//////////////
           CALL "DB_Insert" USING
            JUHW-F_PNAME1 JUHW-F_LNAME JUHW-R RETURNING RET.
           GO TO M-10.
       M-15.
           CALL "DB_F_Close" USING BY REFERENCE JT-M_IDLST JT-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
       M-20.
      *           READ JSSR-F NEXT RECORD AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSSR-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  JS-JCD = 999000
               GO TO M-20
           END-IF
           IF  JS-DC1 = 3
               GO TO M-20
           END-IF
           IF  JS-DC2 = 2 OR 3
               GO TO M-20
           END-IF
           MOVE JS-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD" E-JCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-20
           END-IF
           IF  J-ZC NOT = ZERO
               GO TO M-20
           END-IF
      *
           MOVE ZERO TO JUHW-R.
           MOVE JS-JCD TO JUHW-KEY.
           MOVE JS-DATE TO JUHW-NGP.
           MOVE 1 TO JUHW-KNSC.
           MOVE JS-SU TO JUHW-SU.
           MOVE JS-T TO JUHW-TN.
           MOVE JS-KIN TO JUHW-KIN.
           MOVE JS-CD TO JUHW-SD.
           MOVE JS-KEY TO JUHW-DNO.
           MOVE J-YC TO JUHW-YC.
           MOVE J-BKC TO JUHW-BKC.
           MOVE J-BKNO TO JUHW-BKNO.
      *           WRITE JUHW-R.
      *//////////////
           CALL "DB_Insert" USING
            JUHW-F_PNAME1 JUHW-F_LNAME JUHW-R RETURNING RET.
           GO TO M-20.
       M-25.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HAR-F_PNAME1 " " BY REFERENCE HAR-F_IDLST "0".
       M-30.
      *           READ HAR-F NEXT RECORD AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HAR-F_PNAME1 BY REFERENCE HA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           MOVE HA-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD" E-JCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-30
           END-IF
      *
           MOVE ZERO TO JUHW-R.
           MOVE HA-JCD TO JUHW-KEY.
           MOVE HA-DATE TO JUHW-NGP.
           MOVE 2 TO JUHW-KNSC.
           MOVE HA-SU TO JUHW-SU.
           MOVE HA-KEY TO JUHW-DNO.
           MOVE J-YC TO JUHW-YC.
           MOVE J-BKC TO JUHW-BKC.
           MOVE J-BKNO TO JUHW-BKNO.
      *           WRITE JUHW-R.
      *//////////////
           CALL "DB_Insert" USING
            JUHW-F_PNAME1 JUHW-F_LNAME JUHW-R RETURNING RET.
           GO TO M-30.
       M-35.
           CALL "DB_F_Close" USING
            BY REFERENCE HAR-F_IDLST HAR-F_PNAME1.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JUHW-F_IDLST JUHW-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

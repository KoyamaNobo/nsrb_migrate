       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHG570.
      *********************************************************
      *    PROGRAM         :  工品品名受払ワーク　作成　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  NO                              *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=作表,1=問合せ                 *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-FILE         PIC  X(013).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIKHT1.
      *FD  URIR-F
       01  URIR-F_KHG570.
           02  URIR-F_PNAME1  PIC  X(005) VALUE "URIRF".
           02  F              PIC  X(001).
           02  URIR-F_LNAME   PIC  X(013) VALUE "URIR-F_KHG570".
           02  F              PIC  X(001).
           02  URIR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  URIR-F_SORT    PIC  X(100) VALUE SPACE.
           02  URIR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  URIR-F_RES     USAGE  POINTER.
       01  URIR-R.
           02  U-DC           PIC  9(001).
           02  F              PIC  9(002).
           02  U-DATE         PIC  9(006).
           02  U-TCD          PIC  9(004).
           02  U-HCD          PIC  X(005).
           02  U-SU           PIC S9(006)V9(02).
           02  U-T            PIC S9(006)V9(02).
           02  U-KIN          PIC S9(008).
           02  U-YC           PIC  9(002).
           02  U-SD           PIC  9(004).
           02  U-DNO          PIC  X(006).
           02  F              PIC  X(074).
       77  F                  PIC  X(001).
      *FD  KNHR-F
       01  KNHR-F_KHG570.
           02  KNHR-F_PNAME1  PIC  X(005) VALUE "KNHRF".
           02  F              PIC  X(001).
           02  KNHR-F_LNAME   PIC  X(013) VALUE "KNHR-F_KHG570".
           02  F              PIC  X(001).
           02  KNHR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  KNHR-F_SORT    PIC  X(100) VALUE SPACE.
           02  KNHR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  KNHR-F_RES     USAGE  POINTER.
       01  KNH-R.
           02  NH-NHC         PIC  9(002).
           02  F              PIC  9(002).
           02  NH-DATE        PIC  9(006).
           02  NH-HCD         PIC  X(005).
           02  F              PIC  X(007).
           02  NH-SU          PIC S9(006)V9(02).
           02  F              PIC  X(016).
           02  NH-YC          PIC  9(002).
           02  F              PIC  X(001).
           02  NH-KIS         PIC  9(002).
           02  F              PIC  X(002).
           02  NH-NC          PIC  9(001).
           02  F              PIC  X(010).
       77  F                  PIC  X(001).
      *FD  KHUH-F
       01  KHUH-F_KHG570.
           02  KHUH-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  KHUH-F_LNAME   PIC  X(013) VALUE "KHUH-F_KHG570".
           02  F              PIC  X(001).
           02  KHUH-F_KEY1    PIC  X(100) VALUE SPACE.
           02  KHUH-F_SORT    PIC  X(100) VALUE SPACE.
           02  KHUH-F_IDLST   PIC  X(100) VALUE SPACE.
           02  KHUH-F_RES     USAGE  POINTER.
       01  KHUH-R.
           02  KU-HCD         PIC  X(005).
           02  KU-DATE        PIC  9(006).
           02  KU-KS          PIC S9(006).
           02  KU-HS          PIC S9(006).
           02  KU-IS          PIC S9(006).
           02  KU-SS          PIC S9(006).
           02  KU-ZS          PIC S9(006).
           02  KU-YC          PIC  9(002).
           02  F              PIC  X(021).
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
       01  C-AMID.
           02  C-MID.
             03  FILLER  PIC  N(022) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
             03  FILLER  PIC  N(022) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
             03  FILLER  PIC  N(022) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
             03  FILLER  PIC  N(022) VALUE
                  "＊＊＊　　工品品名受払ワーク　作成　　＊＊＊".
             03  FILLER  PIC  N(022) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
             03  FILLER  PIC  N(022) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
             03  FILLER  PIC  N(022) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
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
      *C-AMID
       CALL "SD_Init" USING 
            "C-AMID" " " "0" "0" "308" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "308" " " "C-AMID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "44" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "44" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "44" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "44" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "44" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "44" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "44" "06C-MID" " "  RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO KHUH-F_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" KHUH-F_PNAME1 " " BY REFERENCE KHUH-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" URIR-F_PNAME1 " " BY REFERENCE URIR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" KNHR-F_PNAME1 " " BY REFERENCE KNHR-F_IDLST "0".
           IF  JS-SIGN NOT = 0
               GO TO M-15
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" KHT-M_PNAME1 "SHARED" BY REFERENCE KHT-M_IDLST "1"
            "KHT-KEY" BY REFERENCE KHT-KEY.
       M-10.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           IF  KHT-ZSU = ZERO
               GO TO M-10
           END-IF
      *
           MOVE ZERO TO KHUH-R.
           MOVE KHT-KEY TO KU-HCD.
           MOVE KHT-ZSU TO KU-ZS.
           MOVE KHT-YC TO KU-YC.
      *           WRITE KHUH-R.
      *//////////////
           CALL "DB_Insert" USING
            KHUH-F_PNAME1 KHUH-F_LNAME KHUH-R RETURNING RET.
           GO TO M-10.
       M-15.
      *           READ URIR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URIR-F_PNAME1 BY REFERENCE URIR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF
           IF  U-DC = 4
               GO TO M-15
           END-IF
           IF  U-SU = ZERO
               GO TO M-15
           END-IF
      *
           MOVE ZERO TO KHUH-R.
           MOVE U-HCD TO KU-HCD.
           MOVE U-DATE TO KU-DATE.
           MOVE U-SU TO KU-SS.
           MOVE U-YC  TO KU-YC.
      *           WRITE KHUH-R.
      *//////////////
           CALL "DB_Insert" USING
            KHUH-F_PNAME1 KHUH-F_LNAME KHUH-R RETURNING RET.
           GO TO M-15.
       M-20.
      *           READ KNHR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KNHR-F_PNAME1 BY REFERENCE KNH-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           IF  NH-SU = ZERO
               GO TO M-20
           END-IF
      *
           MOVE ZERO TO KHUH-R.
           MOVE NH-HCD TO KU-HCD.
           MOVE NH-DATE TO KU-DATE.
           IF  NH-NHC NOT = ZERO
               MOVE NH-SU TO KU-HS
           ELSE
               IF  NH-NC = 0
                   MOVE NH-SU TO KU-KS
               ELSE
                   MOVE NH-SU TO KU-IS
               END-IF
           END-IF
           MOVE NH-YC TO KU-YC.
      *           WRITE KHUH-R.
      *//////////////
           CALL "DB_Insert" USING
            KHUH-F_PNAME1 KHUH-F_LNAME KHUH-R RETURNING RET.
           GO TO M-20.
       M-25.
       M-95.
           IF  JS-SIGN = 0
               CALL "DB_F_Close" USING
                BY REFERENCE KHT-M_IDLST KHT-M_PNAME1
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE URIR-F_IDLST URIR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KNHR-F_IDLST KNHR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KHUH-F_IDLST KHUH-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

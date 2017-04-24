       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHY520.
      *********************************************************
      *    PROGRAM         :  工品品名年間累積ファイル　変換  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/13                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0128".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0256".
           02  W-FID22        PIC  X(003).
       01  ERR-STAT           PIC  X(002).
       01  KHY-F_KHY520.
           02  KHY-F_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  KHY-F_LNAME    PIC  X(012)  VALUE "KHY-F_KHY520".
           02  F              PIC  X(001).
           02  KHY-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  KHY-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  KHY-F_SORT     PIC  X(100)  VALUE SPACE.
           02  KHY-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  KHY-F_RES      USAGE  POINTER.
       01  KHY-R.
           02  Y-HCD          PIC  X(005).
           02  Y-YC           PIC  9(002).
           02  Y-NC           PIC  9(001).
           02  Y-ZS           PIC S9(006)V9(02).
           02  Y-ZK           PIC S9(008).
           02  Y-NS           PIC S9(006)V9(02).
           02  Y-NK           PIC S9(008).
           02  Y-SS           PIC S9(006)V9(02).
           02  Y-SK           PIC S9(008).
           02  Y-YS           PIC S9(006)V9(02).
           02  Y-YK           PIC S9(008).
           02  Y-UG           PIC S9(008).
           02  Y-NG           PIC  9(006).
           02  Y-SNG.
             03  F            PIC  9(002).
             03  Y-SNGS       PIC  9(004).
           02  Y-ENG.
             03  F            PIC  9(002).
             03  Y-ENGS       PIC  9(004).
           02  F              PIC  X(030).
       77  F                  PIC  X(001).
       01  KHYD-F_KHY520.
           02  KHYD-F_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F               PIC  X(001).
           02  KHYD-F_LNAME    PIC  X(013)  VALUE "KHYD-F_KHY520".
           02  F               PIC  X(001).
           02  KHYD-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  KHYD-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  KHYD-F_SORT     PIC  X(100)  VALUE SPACE.
           02  KHYD-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  KHYD-F_RES      USAGE  POINTER.
       01  KHYD-R.
           02  YD-HCD         PIC  X(005).
           02  YD-YC          PIC  9(002).
           02  YD-NC          PIC  9(001).
           02  YD-ZS          PIC S9(007)V9(02).
           02  YD-ZK          PIC S9(009).
           02  YD-NS          PIC S9(008)V9(02).
           02  YD-NK          PIC S9(011).
           02  YD-SS          PIC S9(008)V9(02).
           02  YD-SK          PIC S9(011).
           02  YD-YS          PIC S9(007)V9(02).
           02  YD-YK          PIC S9(009).
           02  YD-NG.
             03  YD-SNG       PIC  9(004).
             03  YD-ENG       PIC  9(004).
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
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　工品品名年間累積ファイル　変換　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "350" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "50" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "50" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "50" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "50" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "50" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "50" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "27" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "27" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
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
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0128ID.
           MOVE W-FID2 TO WK0256ID.
           MOVE WK0128ID TO KHY-F_PNAME1.
           MOVE WK0256ID TO KHYD-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KHY-F_PNAME1 " " BY REFERENCE KHY-F_IDLST "0".
       M-40.
      *           READ KHY-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" KHY-F_PNAME1 BY REFERENCE KHY-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                         RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KHY-F_IDLST KHY-F_PNAME1
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           CALL "DB_F_Open" USING
            "OUTPUT" KHYD-F_PNAME1 " " BY REFERENCE KHYD-F_IDLST "0".
       M-45.
           MOVE ZERO TO KHYD-R.
           MOVE Y-HCD TO YD-HCD.
           MOVE Y-YC TO YD-YC.
           MOVE Y-NC TO YD-NC.
       M-50.
           IF  Y-SNG = Y-NG
               MOVE Y-ZS TO YD-ZS
               MOVE Y-ZK TO YD-ZK
           END-IF.
           ADD Y-NS TO YD-NS.
           ADD Y-NK TO YD-NK.
           ADD Y-SS TO YD-SS.
           ADD Y-UG TO YD-SK.
           IF  Y-ENG = Y-NG
               MOVE Y-YS TO YD-YS
               MOVE Y-YK TO YD-YK
           END-IF.
           IF  YD-NG = ZERO
               MOVE Y-SNGS TO YD-SNG
               MOVE Y-ENGS TO YD-ENG
           END-IF.
       M-55.
      *           READ KHY-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" KHY-F_PNAME1 BY REFERENCE KHY-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
           IF  Y-HCD NOT = YD-HCD
               GO TO M-60
           END-IF.
           IF  Y-YC NOT = YD-YC
               GO TO M-60
           END-IF.
           IF  Y-NC NOT = YD-NC
               GO TO M-60
           END-IF.
           GO TO M-50.
       M-60.
           IF  ZERO = YD-ZS AND YD-ZK AND YD-NS AND YD-NK AND
                     YD-SS AND YD-SK AND YD-YS AND YD-YK
               GO TO M-45
           END-IF.
      *           WRITE KHYD-R.
      *//////////////////////
           CALL "DB_Insert" USING
            KHYD-F_PNAME1 KHYD-F_LNAME KHYD-R RETURNING RET.
           GO TO M-45.
       M-90.
           IF  ZERO = YD-ZS AND YD-ZK AND YD-NS AND YD-NK AND
                     YD-SS AND YD-SK AND YD-YS AND YD-YK
               GO TO M-95
           END-IF.
      *           WRITE KHYD-R.
      *//////////////////////
           CALL "DB_Insert" USING
            KHYD-F_PNAME1 KHYD-F_LNAME KHYD-R RETURNING RET.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE KHY-F_IDLST KHY-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KHYD-F_IDLST KHYD-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

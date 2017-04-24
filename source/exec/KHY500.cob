       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHY500.
      *********************************************************
      *    PROGRAM         :  工品品名年間累積Ｆ　用途Ｃ変換  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=年間 , 1=月(累積Ｆ)           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIKHM.
      *FD  KHTMYR
       01  KHTMYR_KHY500.
           02  KHTMYR_PNAME1  PIC  X(006) VALUE "KHTMYR".
           02  F              PIC  X(001).
           02  KHTMYR_LNAME   PIC  X(013) VALUE "KHTMYR_KHY500".
           02  F              PIC  X(001).
           02  KHTMYR_KEY1    PIC  X(100) VALUE SPACE.
           02  KHTMYR_KEY2    PIC  X(100) VALUE SPACE.
           02  KHTMYR_SORT    PIC  X(100) VALUE SPACE.
           02  KHTMYR_IDLST   PIC  X(100) VALUE SPACE.
           02  KHTMYR_RES     USAGE  POINTER.
       01  KHTY-R.
           02  KHTY-YC        PIC  9(002).
           02  KHTY-NC        PIC  9(001).
           02  KHTY-HCD       PIC  X(005).
           02  F              PIC  X(162).
       77  F                  PIC  X(001).
      *FD  URIRF
       01  URIRF_KHY500.
           02  URIRF_PNAME1   PIC  X(005) VALUE "URIRF".
           02  F              PIC  X(001).
           02  URIRF_LNAME    PIC  X(012) VALUE "URIRF_KHY500".
           02  F              PIC  X(001).
           02  URIRF_KEY1     PIC  X(100) VALUE SPACE.
           02  URIRF_KEY2     PIC  X(100) VALUE SPACE.
           02  URIRF_SORT     PIC  X(100) VALUE SPACE.
           02  URIRF_IDLST    PIC  X(100) VALUE SPACE.
           02  URIRF_RES      USAGE  POINTER.
       01  URIR-R.
           02  F              PIC  X(013).
           02  URIR-HCD       PIC  X(005).
           02  F              PIC  X(024).
           02  URIR-YC        PIC  9(002).
           02  F              PIC  X(084).
       77  F                  PIC  X(001).
      *FD  KNHRF
       01  KNHRF_KHY500.
           02  KNHRF_PNAME1   PIC  X(005) VALUE "KNHRF".
           02  F              PIC  X(001).
           02  KNHRF_LNAME    PIC  X(012) VALUE "KNHRF_KHY500".
           02  F              PIC  X(001).
           02  KNHRF_KEY1     PIC  X(100) VALUE SPACE.
           02  KNHRF_KEY2     PIC  X(100) VALUE SPACE.
           02  KNHRF_SORT     PIC  X(100) VALUE SPACE.
           02  KNHRF_IDLST    PIC  X(100) VALUE SPACE.
           02  KNHRF_RES      USAGE  POINTER.
       01  KNHR-R.
           02  F              PIC  X(010).
           02  KNHR-HCD       PIC  X(005).
           02  F              PIC  X(031).
           02  KNHR-YC        PIC  9(002).
           02  F              PIC  X(016).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　工品年間Ｆ　用途入庫区分セット　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　工品月次累積Ｆ　用途区分セット　　＊＊＊".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "372" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "50" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "50" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "50" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "50" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "50" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "50" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "50" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "20" "24" "22" "07C-MID" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "50" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "N" "6" "10" "50" " " "C-MID1"  RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "41" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "10" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "10" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
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
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           IF  JS-SIGN = 1
               GO TO M-40
           END-IF
           CALL "DB_F_Open" USING
            "I-O" KHTMYR_PNAME1 " " BY REFERENCE KHTMYR_IDLST "0".
       M-15.
      *           READ KHTMYR AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KHTMYR_PNAME1 BY REFERENCE KHTY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF
           MOVE KHTY-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           MOVE KH-YC TO KHTY-YC.
           MOVE KH-NC TO KHTY-NC.
      *           REWRITE KHTY-R.
      *///////////////
           CALL "DB_Update" USING
            KHTMYR_PNAME1 KHTMYR_LNAME KHTY-R RETURNING RET.
           GO TO M-15.
       M-20.
           CALL "DB_F_Close" USING
            BY REFERENCE KHTMYR_IDLST KHTMYR_PNAME1.
           GO TO M-90.
       M-40.
           CALL "DB_F_Open" USING
            "I-O" URIRF_PNAME1 " " BY REFERENCE URIRF_IDLST "0".
       M-45.
      *           READ URIRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URIRF_PNAME1 BY REFERENCE URIR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF URIR-HCD = ZERO OR SPACE
               GO TO M-45
           END-IF
           MOVE URIR-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF
           IF  KH-YC NOT = URIR-YC
               MOVE KH-YC TO URIR-YC
      *               REWRITE URIR-R.
      *///////////////
               CALL "DB_Update" USING
                URIRF_PNAME1 URIRF_LNAME URIR-R RETURNING RET
           END-IF
           GO TO M-45.
       M-50.
           CALL "DB_F_Close" USING
            BY REFERENCE URIRF_IDLST URIRF_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" KNHRF_PNAME1 " " BY REFERENCE KNHRF_IDLST "0".
       M-55.
      *           READ KNHRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KNHRF_PNAME1 BY REFERENCE KNHR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-60
           END-IF
           IF  KNHR-HCD = ZERO OR SPACE
               GO TO M-55
           END-IF
           MOVE KNHR-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           IF  KH-YC NOT = KNHR-YC
               MOVE KH-YC TO KNHR-YC
      *               REWRITE KNHR-R.
      *///////////////
               CALL "DB_Update" USING
                KNHRF_PNAME1 KNHRF_LNAME KNHR-R RETURNING RET
           END-IF
           GO TO M-55.
       M-60.
           CALL "DB_F_Close" USING
            BY REFERENCE KNHRF_IDLST KNHRF_PNAME1.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

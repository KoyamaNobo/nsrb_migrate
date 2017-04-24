       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHY250.
      *********************************************************
      *    PROGRAM         :  工品品名統計マスター　棚卸セット*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :                                  *
      *        変更　　　  :  62/04/10                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  CHK            PIC  9(001).
           02  W-DMM          PIC  9(001).
           COPY LSTAT.
      *
           COPY LIKHTM.
      *FD  KHTN-F
       01  KHTN-F_KHY280.
           02  KHTN-F_PNAME1  PIC  X(005) VALUE "KHTNF".
           02  F              PIC  X(001).
           02  KHTN-F_LNAME   PIC  X(013) VALUE "KHTN-F_KHY280".
           02  F              PIC  X(001).
           02  KHTN-F_KEY1    PIC  X(100) VALUE SPACE.
           02  KHTN-F_SORT    PIC  X(100) VALUE SPACE.
           02  KHTN-F_IDLST   PIC  X(100) VALUE SPACE.
           02  KHTN-F_RES     USAGE  POINTER.
       01  KHTN-R.
           02  KHTN-KC        PIC  9(001).
           02  KHTN-HCD       PIC  X(005).
           02  KHTN-SU        PIC S9(006)V9(02).
           02  KHTN-YC        PIC  9(002).
           02  KHTN-PC        PIC  9(001).
           02  F              PIC  X(004).
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
       01  C-MID.
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊　　工品品名統計マスター　棚卸セット　　＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-FCM   PIC  N(009) VALUE
                "ＺＥＲＯ　セット中".
           02  D-FSM   PIC  N(009) VALUE
                "　棚卸　セット中　".
           02  D-MC    PIC  X(018) VALUE
                  "                  ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(026) VALUE
                  "***  KHTM REWRITE ｴﾗｰ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  KHTM ﾅｼ  ***".
             03  E-HCD   PIC  X(005).
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "74" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "15" "52" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "20" "29" "22" "01C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "46" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "54" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-FCM" "RN" "10" "31" "18" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-FSM" "RN" "10" "31" "18" "D-FCM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MC" "X" "10" "31" "18" "D-FSM" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "48" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "48" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "26" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HCD" "X" "24" "50" "5" "E-ME2" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-HCD" BY REFERENCE KHT-KEY "5" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
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
           CALL "SD_Output" USING "D-FCM" D-FCM "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" KHT-M_PNAME1 "SHARED" BY REFERENCE KHT-M_IDLST "2"
            "KHT-KEY" BY REFERENCE KHT-KEY "KHT-KEYD" BY REFERENCE
            KHT-KEYD.
       M-15.
      *           READ KHT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF
           MOVE ZERO TO KHT-JTS.
      *           REWRITE KHT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KHT-M_PNAME1 KHT-M_LNAME KHT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KHT-M_IDLST KHT-M_PNAME1
               GO TO M-95
           END-IF
           GO TO M-15.
       M-20.
           CALL "SD_Output" USING "D-FSM" D-FSM "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" KHTN-F_PNAME1 " " BY REFERENCE KHTN-F_IDLST "0".
       M-25.
      *           READ KHTN-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KHTN-F_PNAME1 BY REFERENCE KHTN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  KHTN-SU = ZERO
               GO TO M-25
           END-IF
           MOVE KHTN-HCD TO KHT-KEY.
      *           READ KHT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KHT-M_PNAME1 BY REFERENCE KHT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           ADD KHTN-SU TO KHT-JTS.
      *           REWRITE KHT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KHT-M_PNAME1 KHT-M_LNAME KHT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-25.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE KHT-M_IDLST KHT-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KHTN-F_IDLST KHTN-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

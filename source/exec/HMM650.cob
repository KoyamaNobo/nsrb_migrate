       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMM650.
      *****************************************************************
      *    PROGRAM         :  履物　前月繰越金額　更新　(原価修正時)  *
      *    PRINTER TYPE    :  JIPS                                    *
      *    SCREEN          :  ******                                  *
      *        変更　　　  :  62/05/16                                *
      *    COMPILE TYPE    :  COBOL                                   *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-FILE         PIC  X(013).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIHUHM.
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
                "＊＊＊　　履物　前月繰越金額　更新　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　（原価修正時）　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-SM2   PIC  N(011) VALUE
                  "【　ＨＵＨＭ　更新　】".
       01  C-ERR.
           02  FILLER.
             03  E-ME2   PIC  X(027) VALUE
                  "***  HUH-M REWRITE ｴﾗｰ  ***".
             03  E-HCD   PIC  9(006).
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
           "C-MID" " " "0" "0" "330" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING
           "08C-MID" "X" "15" "21" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "15" "38" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "22" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-DSP" " " "12" "0" "22" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "D-SM2" "bN" "12" "12" "22" " " "01C-DSP" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "33" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "33" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "27" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-HCD" "9" "24" "45" "6" "E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-HCD" BY REFERENCE HUH-HCD "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
           CALL "DB_F_Open" USING
            "I-O" HUH-M_PNAME1 " " BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
           CALL "SD_Output" USING "D-SM2" D-SM2 "p" RETURNING RESU.
       M-20.
      *           READ HUH-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HUH-M_PNAME1 BY REFERENCE HUH-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           MOVE HUH-YK TO HUH-ZK.
      *           REWRITE HUH-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HUH-M_PNAME1 HUH-M_LNAME HUH-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HUH-M_IDLST HUH-M_PNAME1
               GO TO M-95
           END-IF
           GO TO M-20.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-M_IDLST HUH-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

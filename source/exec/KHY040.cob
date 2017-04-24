       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHY040.
      *********************************************************
      *    PROGRAM         :  工品棚卸他　更新                *
      *    PRINTER TYPE    :  JIPS                            *
      *        変更　　　  :  62/04/10                        *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=棚卸 , 1=廃棄                 *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-FILE             PIC  X(013).
       01  W-D.
           02  W-ZKIN         PIC S9(008).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIKHTM.
           COPY LIKHM.
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　工品品名マスター　棚卸更新　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MID1  PIC  N(023) VALUE
                "＊＊＊　　工品品名マスター　廃棄更新　　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(026) VALUE
                  "***  KHTM REWRITE ｴﾗｰ  ***".
             03  E-ME2   PIC  X(016) VALUE
                  "***  KHM ﾅｼ  ***".
             03  E-ME90  PIC  N(015) VALUE
                  "＊＊＊　　キャンセル　　＊＊＊".
             03  E-KEY   PIC  X(005).
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
            "C-MID" " " "0" "0" "344" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "46" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "46" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "46" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "46" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "46" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "46" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "46" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "12" "22" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "12" "39" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "46" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "D-MID1" "N" "6" "10" "46" " " "C-DSP"  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "77" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "77" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "26" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "16" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME90" "N" "24" "15" "30" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-KEY" "X" "24" "45" "5" "E-ME90" " "  RETURNING RESU.
       CALL "SD_From" USING
            "E-KEY" BY REFERENCE KHT-KEY "5" "0" RETURNING RESU.
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
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "D-MID1" D-MID1 "p" RETURNING RESU
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "E-ME90" E-ME90 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "I-O" KHT-M_PNAME1 " " BY REFERENCE KHT-M_IDLST "2"
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
           MOVE KHT-KEY TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JS-SIGN = 1
               COMPUTE W-ZKIN = (KHT-ZSU - KHT-HKS) * KH-GT1
               COMPUTE KHT-ZSU = KHT-ZSU - KHT-HKS
           ELSE
               COMPUTE W-ZKIN = KHT-JTS * KH-GT1
               MOVE KHT-JTS TO KHT-ZSU
           END-IF
           MOVE W-ZKIN TO KHT-ZKIN.
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
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-20
           END-IF
           GO TO M-15.
       M-20.
           CALL "DB_F_Close" USING
            BY REFERENCE KHT-M_IDLST KHT-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

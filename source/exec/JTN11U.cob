       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JTN11U.
      *================================================================*
      *            福山通運  荷札変換ファイル  クリア　　　　　      *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA                           DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
      *FD  FUKUF
       01  FUKUF_JTN11U.
           02  FUKUF_PNAME1 PIC  X(005) VALUE "FUKUF".
           02  F            PIC  X(001).
           02  FUKUF_LNAME  PIC  X(012) VALUE "FUKUF_JTN11U".
           02  F            PIC  X(001).
           02  FUKUF_KEY1   PIC  X(100) VALUE SPACE.
           02  FUKUF_SORT   PIC  X(100) VALUE SPACE.
           02  FUKUF_IDLST  PIC  X(100) VALUE SPACE.
           02  FUKUF_RES    USAGE  POINTER.
       01  FUKU-R.
           02  FUKU-X1      PIC X(15).
           02  FUKU-TEL     PIC X(17).
           02  FUKU-JSU     PIC N(20).
           02  FUKU-JSS     PIC N(20).
           02  FUKU-N1      PIC N(20).
           02  FUKU-NAU     PIC N(20).
           02  FUKU-NASD    PIC N(20).
           02  FUKU-NASW  REDEFINES FUKU-NASD.
             03  FUKU-NAS   PIC N(06).
             03  FUKU-N2    PIC N(14).
           02  FUKU-UNO     PIC X(08).
           02  FUKU-X2      PIC X(05).
           02  FUKU-X3      PIC X(03).
           02  FUKU-NR      PIC X(12).
           02  FUKU-KSU     PIC 9(02).
           02  FUKU-91      PIC 9(03).
           02  FUKU-92      PIC 9(04).
           02  FUKU-F1.
             03  FUKU-HSI   PIC N(09).
             03  FUKU-N3    PIC N(06).
           02  FUKU-F1.
             03  FUKU-N4    PIC N(15).
           02  FUKU-X4      PIC X(30).
           02  FUKU-X5      PIC X(30).
           02  FUKU-X6      PIC X(30).
           02  FUKU-95      PIC 9(08).
           02  FUKU-NO.
             03  FUKU-ONO   PIC X(06).
             03  FUKU-X7    PIC X(10).
           02  FUKU-X8      PIC X(30).
           02  FUKU-1       PIC 9(01).
           02  FUKU-96      PIC 9(04).
           02  FUKU-NGP1    PIC 9(08).
           02  FUKU-NGP2    PIC 9(08).
       77  F                PIC X(01).
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
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊　　　福通変換ファイル　クリア　　＊＊＊".
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
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
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
            "C-MID" " " "0" "0" "344" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "46" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "22" "26" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "43" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "17" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "17" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
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
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DMM" A-DMM "p" RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "OUTPUT" FUKUF_PNAME1 " " BY REFERENCE FUKUF_IDLST "0".
           CALL "DB_F_Close" USING
            BY REFERENCE FUKUF_IDLST FUKUF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

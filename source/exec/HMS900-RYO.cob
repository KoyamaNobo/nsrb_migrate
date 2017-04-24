       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMS900.
      *********************************************************
      *    PROGRAM         :  荷札・入日記ファイル　クリア    *
      *    PRINTER TYPE    :  ****                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/01                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. NEAC-SYSTEM100.
       OBJECT-COMPUTER. NEAC-SYSTEM100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DMM        PIC 9.
       01  ERR-STAT     PIC X(2).
      *
      *FD  SNF-F
       01  SNF-F_HMS900.
           02  SNF-F_PNAME1   PIC  X(008) VALUE "SNFF-RYO".
           02  F              PIC  X(001).
           02  SNF-F_LNAME    PIC  X(012) VALUE "SNF-F_HMS900".
           02  F              PIC  X(001).
           02  SNF-F_KEY1     PIC  X(100) VALUE SPACE.
           02  SNF-F_SORT     PIC  X(100) VALUE SPACE.
           02  SNF-F_IDLST    PIC  X(100) VALUE SPACE.
           02  SNF-F_RES      USAGE  POINTER.
       01  SNF-R.
           02  N-NO           PIC 9(3).
           02  N-CCD          PIC 9(7).
           02  N-UC           PIC 9.
           02  N-KS           PIC 9(3).
           02  N-MS           PIC 9(3).
           02  N-TE           PIC N(36).
           02  F              PIC X(13).
       77  F                  PIC X(01).
      *FD  SIN-F
       01  SIN-F_HMS900.
           02  SIN-F_PNAME1   PIC  X(008) VALUE "SINF-RYO".
           02  F              PIC  X(001).
           02  SIN-F_LNAME    PIC  X(012) VALUE "SIN-F_HMS900".
           02  F              PIC  X(001).
           02  SIN-F_KEY1     PIC  X(100) VALUE SPACE.
           02  SIN-F_SORT     PIC  X(100) VALUE SPACE.
           02  SIN-F_IDLST    PIC  X(100) VALUE SPACE.
           02  SIN-F_RES      USAGE  POINTER.
       01  SIN-R.
           02  I-NO           PIC 9(3).
           02  I-HCD          PIC 9(6).
           02  I-ASU.
             03  I-SU  OCCURS 27  PIC 9(2).
           02  I-GSU          PIC 9(2).
           02  F              PIC X(20).
       77  F                  PIC X(01).
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
           02  FILLER  PIC N(22) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(22) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(22) VALUE
              "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(22) VALUE
              "＊＊＊　荷札・入日記ファイル　クリア　＊＊＊".
           02  FILLER  PIC N(22) VALUE
              "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(22) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(22) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC X(28) VALUE
                "<  確認 OK=1 NO=9    ﾘﾀｰﾝ  >".
       01  C-ACP.
           02  A-DMM   PIC 9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC N(13) VALUE
                "＊＊＊　キャンセル　＊＊＊".
             03  E-ME98  PIC  X(05) VALUE X"1B4A05".
             03  E-ME99  PIC  X(05) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "336" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "15" "44" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "15" "44" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "15" "44" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "15" "44" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "15" "44" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "15" "44" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "15" "44" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "18" "28" "07C-MID" " "
            RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "12" "38" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "36" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "36" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "N" "24" "18" "26" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = "01"
               GO TO M-10
           END-IF
           IF  W-DMM = 1
               GO TO M-15
           END-IF
           IF  W-DMM NOT = 9
               GO TO M-10
           END-IF
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           GO TO M-95.
       M-15.
           CALL "DB_F_Open" USING
            "OUTPUT" SNF-F_PNAME1 " " BY REFERENCE SNF-F_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" SIN-F_PNAME1 " " BY REFERENCE SIN-F_IDLST "0".
           CALL "DB_F_Close" USING
            BY REFERENCE SNF-F_IDLST SNF-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SIN-F_IDLST SIN-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

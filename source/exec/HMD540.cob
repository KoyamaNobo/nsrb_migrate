       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD540.
      *********************************************************
      *    PROGRAM         :  履物　日次更新　クリア　　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/17                        *
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
      *
           COPY LUTRAN.
      *FD  STRAN
       01  STRAN_HMD540.
           02  STRAN_PNAME1   PIC  X(005) VALUE "STRAN".
           02  F              PIC  X(001).
           02  STRAN_LNAME    PIC  X(012) VALUE "STRAN_HMD540".
           02  F              PIC  X(001).
           02  STRAN_KEY1     PIC  X(100) VALUE SPACE.
           02  STRAN_KEY2     PIC  X(100) VALUE SPACE.
           02  STRAN_SORT     PIC  X(100) VALUE SPACE.
           02  STRAN_IDLST    PIC  X(100) VALUE SPACE.
           02  STRAN_RES      USAGE  POINTER.
       01  STRAN-R            PIC  X(128).
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
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　履物　日次更新　クリア　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
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
            "C-MID" " " "0" "0" "294" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "42" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "42" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "42" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "42" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "42" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "42" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "42" "06C-MID" " "  RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-10.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "OUTPUT" UTRAN_PNAME1 " " BY REFERENCE UTRAN_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" STRAN_PNAME1 " " BY REFERENCE STRAN_IDLST "0".
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE UTRAN_IDLST UTRAN_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE STRAN_IDLST STRAN_PNAME1.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

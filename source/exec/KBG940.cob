       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG940.
      *************************************************************
      *    PROGRAM         :  買掛金残高ファイル　作成            *
      *    PRINTER TYPE    :  JIPS                                *
      *    SCREEN          :  ******                              *
      *        変更　　　  :  92/11/12                            *
      *    COMPILE TYPE    :  COBOL                               *
      *************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-NG           PIC  9(004).
           02  W-NGD   REDEFINES W-NG.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-KIN          PIC S9(010).
           02  W-FILE         PIC  X(013).
      *
           COPY LIBFDD.
           COPY LISTM.
      *FD  ZD-F
       01  ZD-F_KBG940.
           02  ZD-F_PNAME1    PIC  X(003) VALUE "ZDF".
           02  F              PIC  X(001).
           02  ZD-F_LNAME     PIC  X(011) VALUE "ZD-F_KBG940".
           02  F              PIC  X(001).
           02  ZD-F_KEY1      PIC  X(100) VALUE SPACE.
           02  ZD-F_SORT      PIC  X(100) VALUE SPACE.
           02  ZD-F_IDLST     PIC  X(100) VALUE SPACE.
           02  ZD-F_RES       USAGE  POINTER.
       01  ZD-R.
           02  ZD-NO          PIC  9(002).
           02  ZD-KEY         PIC  9(004).
           02  ZD-KIN         PIC S9(010).
           02  ZD-NG          PIC  9(004).
           02  F              PIC  X(001).
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　買掛金残高ファイル　作成　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(019) VALUE
                  "***  WRITE ｴﾗｰ  ***".
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "308" " " " " RETURNING RESU.
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
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "19" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "19" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "19" " " "01C-ERR" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           MOVE D-NBNG TO W-NG.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
               IF  W-NEN = 00
                   MOVE 99 TO W-NEN
               ELSE
                   SUBTRACT 1 FROM W-NEN
               END-IF
           END-IF
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" ST-M_PNAME1 
            "SHARED" BY REFERENCE ST-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" ZD-F_PNAME1 " " BY REFERENCE ZD-F_IDLST "0".
       M-10.
      *           READ ST-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" ST-M_PNAME1 BY REFERENCE ST-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           COMPUTE W-KIN = ST-ZKZ + ST-ZKZZ
           IF  W-KIN = ZERO
               GO TO M-10
           END-IF.
       M-15.
           MOVE ZERO TO ZD-R.
           MOVE 21 TO ZD-NO.
           MOVE ST-KEY TO ZD-KEY.
           MOVE W-KIN TO ZD-KIN.
           MOVE W-NG TO ZD-NG.
      *           WRITE ZD-R.
      *//////////////
           CALL "DB_Insert" USING
            ZD-F_PNAME1 ZD-F_LNAME ZD-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               GO TO M-20
           END-IF
           GO TO M-10.
       M-20.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT = "34"
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE ZD-F_IDLST ZD-F_PNAME1
               MOVE "ZDF          " TO W-FILE
               CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "SD_Output" USING " " " " "STOP" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "DB_F_Open" USING
                "EXTEND" ZD-F_PNAME1 " " BY REFERENCE ZD-F_IDLST "0"
               GO TO M-15
           END-IF
           CALL "SD_Output" USING "E-ME78" E-ME78 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE ST-M_IDLST ST-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE ZD-F_IDLST ZD-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

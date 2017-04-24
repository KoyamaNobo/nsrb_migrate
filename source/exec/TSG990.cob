       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSG990.
      ********************************************************
      *****     手形・領収書・買掛支払ファイル　更新     *****
      *****        ( JS-SIGN : 0=手形 , 1=購買 )         *****
      ********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       01  W-DATA.
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-GET        PIC  9(002).
           02  W-NGN          PIC  9(006).
           02  W-NGD.
             03  W-NEND       PIC  9(004).
             03  W-NENL  REDEFINES W-NEND.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GETD       PIC  9(002).
           02  W-NGL   REDEFINES W-NGD.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  CHK            PIC  9(001).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
      *FD  TDT-M
       01  TDT-M_TSG990.
           02  TDT-M_PNAME1   PIC  X(004) VALUE "TDTM".
           02  F              PIC  X(001).
           02  TDT-M_LNAME    PIC  X(012) VALUE "TDT-M_TSG990".
           02  F              PIC  X(001).
           02  TDT-M_KEY1     PIC  X(100) VALUE SPACE.
           02  TDT-M_SORT     PIC  X(100) VALUE SPACE.
           02  TDT-M_IDLST    PIC  X(100) VALUE SPACE.
           02  TDT-M_RES      USAGE  POINTER.
       01  TDT-R.
           02  TD-KEY.
             03  TD-KBN       PIC  X(002).
             03  TD-NO        PIC  X(004).
           02  TD-TCD         PIC  9(004).
           02  TD-DAT         PIC  9(006).
           02  TD-NGP   REDEFINES TD-DAT.
             03  TD-NEN       PIC  9(002).
             03  TD-GET       PIC  9(002).
             03  TD-PEY       PIC  9(002).
           02  TD-MAN         PIC  9(006).
           02  TD-KIN         PIC S9(010).
           02  TD-BK          PIC  9(004).
           02  TD-HAC         PIC  N(024).
           02  TD-ZR          PIC S9(008).
           02  TD-SS          PIC S9(008).
           02  TD-SB          PIC  9(008).
           02  TD-GC          PIC  9(008).
           02  TD-SZ          PIC  9(008).
           02  TD-EG          PIC  9(008).
           02  TD-ST          PIC  9(008).
           02  TD-ZSHZ        PIC S9(007).
           02  TD-SSHZ        PIC S9(007).
           02  F              PIC  X(006).
           02  TD-SNEN        PIC  9(004).
           02  TD-HCR         PIC  9(001).
           02  TD-HCT         PIC  9(001).
           02  TD-HCK         PIC  9(001).
           02  TD-HCZ         PIC  9(001).
           02  TD-PCHK        PIC  9(001).
           02  TD-RSC         PIC  9(001).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊　　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊　手形・領収書・買掛支払ファイル　更新　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊　　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(026) VALUE
                  "***  TDTM REWRITE ｴﾗｰ  ***".
             03  E-ME2   PIC  X(025) VALUE
                  "***  TDTM DELETE ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-KEY   PIC  X(006).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "364" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "52" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "52" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "52" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "52" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "52" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "52" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "52" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "119" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "119" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "26" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "25" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "45" "6" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE TD-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-KEY" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               GO TO M-05
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           IF  JS-SIGN = 0
               MOVE D-NTNG TO W-NGS
           ELSE
               MOVE D-NBNG TO W-NGS
           END-IF
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEND
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEND
           END-IF
           MOVE W-NGD TO W-NGN.
           SUBTRACT 1 FROM W-GETD.
           IF  W-GETD = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEND
           END-IF
           SUBTRACT 1 FROM W-GETD.
           IF  W-GETD = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEND
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" TDT-M_PNAME1 " " BY REFERENCE TDT-M_IDLST "1"
            "TD-KEY" BY REFERENCE TD-KEY.
       M-10.
      *           READ TDT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDT-M_PNAME1 BY REFERENCE TDT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           MOVE TD-SNEN TO W-NEN.
           MOVE TD-GET TO W-GET.
           IF  W-NG NOT < W-NGN
               GO TO M-10
           END-IF
           IF  W-NG > W-NGD
               GO TO M-15
           END-IF
           IF  JS-SIGN = 1
               GO TO M-10
           END-IF
      *
      *           DELETE TDT-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TDT-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-10.
       M-15.
           IF  JS-SIGN = 0
               IF (TD-HCR NOT = 0) AND (TD-HCT NOT = 0)
                   GO TO M-10
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  TD-HCR = 0
                   MOVE 5 TO TD-HCR
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  TD-HCT = 0
                   MOVE 5 TO TD-HCT
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  TD-HCK = 0
                   MOVE 5 TO TD-HCR
               ELSE
                   GO TO M-10
               END-IF
           END-IF
      *           REWRITE TDT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDT-M_PNAME1 TDT-M_LNAME TDT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-10.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE TDT-M_IDLST TDT-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

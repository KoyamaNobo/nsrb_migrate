       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMT830.
      *********************************************************
      *    PROGRAM         : 　得意先品名別出荷集計ワーク作成 *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-FILE             PIC  X(013).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-DATE         PIC  9(008).
           02  W-NGP   REDEFINES W-DATE.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-NGL   REDEFINES W-NG.
               04  F          PIC  9(002).
               04  W-NGS      PIC  9(004).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-DATE.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-RD.
             03  W-SU         PIC S9(005).
             03  W-UKIN       PIC S9(008).
             03  W-GKIN       PIC S9(008).
           02  W-KIN          PIC S9(009).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
      *FD  SNTR-F
       01  SNTR-F_HMT830.
           02  SNTR-F_PNAME1  PIC  X(005) VALUE "SNTRF".
           02  F              PIC  X(001).
           02  SNTR-F_LNAME   PIC  X(013) VALUE "SNTR-F_HMT830".
           02  F              PIC  X(001).
           02  SNTR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  SNTR-F_SORT    PIC  X(100) VALUE SPACE.
           02  SNTR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  SNTR-F_RES     USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DNO       PIC  9(006).
           02  SNTR-GNO       PIC  9(001).
           02  SNTR-DATE.
             03  SNTR-NG      PIC  9(006).
             03  SNTR-PEY     PIC  9(002).
           02  SNTR-TCD       PIC  9(004).
           02  SNTR-HCD       PIC  9(006).
           02  F              PIC  X(031).
           02  SNTR-SU        PIC S9(005).
           02  SNTR-BT        PIC S9(005).
           02  SNTR-UKIN      PIC S9(008).
           02  F              PIC  X(001).
           02  SNTR-DC        PIC  9(001).
           02  SNTR-FT        PIC  9(005).
           02  F              PIC  X(003).
           02  SNTR-BC.
             03  SNTR-BC1     PIC  9(002).
             03  SNTR-BC2     PIC  9(002).
             03  SNTR-BC3     PIC  9(002).
           02  SNTR-BCD   REDEFINES SNTR-BC.
             03  SNTR-BCD1    PIC  9(003).
             03  F            PIC  9(003).
           02  F              PIC  X(001).
           02  SNTR-TNC       PIC  9(002).
           02  SNTR-FKC       PIC  9(002).
           02  F              PIC  X(027).
           02  SNTR-BMC       PIC  9(002).
           02  SNTR-BMNO      PIC  9(001).
           02  F              PIC  X(002).
           02  SNTR-SNC       PIC  9(001).
       77  F                  PIC  X(001).
      *FD  SSR-F
       01  SSR-F_HMT830.
           02  SSR-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SSR-F_LNAME    PIC  X(012) VALUE "SSR-F_HMT830".
           02  F              PIC  X(001).
           02  SSR-F_KEY1     PIC  X(100) VALUE SPACE.
           02  SSR-F_SORT     PIC  X(100) VALUE SPACE.
           02  SSR-F_IDLST    PIC  X(100) VALUE SPACE.
           02  SSR-F_RES      USAGE  POINTER.
       01  SSR-R.
           02  SSR-TCD        PIC  9(004).
           02  SSR-HCD        PIC  9(006).
           02  SSR-SU         PIC S9(007).
           02  SSR-UKIN       PIC S9(010).
           02  SSR-GKIN       PIC S9(010).
           02  SSR-TKC        PIC  9(002).
           02  SSR-TNC        PIC  9(002).
           02  SSR-BC.
             03  SSR-BC1      PIC  9(002).
             03  SSR-BC2      PIC  9(002).
             03  SSR-BC3      PIC  9(002).
           02  SSR-BMC        PIC  9(002).
           02  SSR-BMNO       PIC  9(001).
           02  SSR-FKC        PIC  9(002).
           02  SSR-NG         PIC  9(006).
           02  F              PIC  X(006).
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
                "＊＊＊　　得意先品名別出荷集計ワーク作成　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
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
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           MOVE ZERO TO W-DATE.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SSR-F_PNAME1.
           CALL "DB_F_Open" USING
            "EXTEND" SSR-F_PNAME1 " " BY REFERENCE SSR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" SNTR-F_PNAME1 " " BY REFERENCE SNTR-F_IDLST "0".
       M-10.
      *           READ SNTR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SNTR-GNO = 9
               GO TO M-10
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-10
           END-IF
           PERFORM DST-RTN THRU DST-EX.
           IF  ZERO = W-SU AND W-UKIN AND W-GKIN
               GO TO M-10
           END-IF
      *
           MOVE ZERO      TO SSR-R.
           MOVE SNTR-TCD  TO SSR-TCD.
           MOVE SNTR-HCD  TO SSR-HCD.
           MOVE W-SU   TO SSR-SU.
           MOVE W-UKIN TO SSR-UKIN.
           MOVE W-GKIN TO SSR-GKIN.
           MOVE SNTR-TNC  TO SSR-TNC.
           MOVE SNTR-BC   TO SSR-BC.
           MOVE SNTR-BMC  TO SSR-BMC.
           MOVE SNTR-BMNO TO SSR-BMNO.
           MOVE SNTR-FKC  TO SSR-FKC.
           MOVE SNTR-NG TO SSR-NG.
      *           WRITE SSR-R.
      *//////////////
           CALL "DB_Insert" USING
            SSR-F_PNAME1 SSR-F_LNAME SSR-R RETURNING RET.
           GO TO M-10.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SSR-F_IDLST SSR-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       DST-RTN.
           MOVE ZERO TO W-RD.
           IF (SNTR-SNC = 0) OR (SNTR-DC NOT = 2)
               COMPUTE W-GKIN = SNTR-SU * SNTR-FT
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-SU = SNTR-SU * -1
               COMPUTE W-GKIN = W-GKIN * -1
               COMPUTE W-UKIN = SNTR-UKIN * -1
           ELSE
               MOVE SNTR-SU TO W-SU
               MOVE SNTR-UKIN TO W-UKIN
           END-IF
           IF (SNTR-HCD > 999899) OR (SNTR-SNC = 1) OR (SNTR-DC = 2)
               MOVE ZERO TO W-SU
           END-IF.
       DST-EX.
           EXIT.

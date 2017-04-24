       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG930.
      *********************************************************
      *    PROGRAM         :  売上値引・入庫伝票ワーク　作成  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  売上値引=0(月次),入庫=1(問合せ) *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-MSG              PIC  X(040).
       77  JS-SIGN            PIC  9(001).
       77  W-FILE             PIC  X(013) VALUE SPACE.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NEND  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPD  REDEFINES W-NGP.
             03  F            PIC  X(002).
             03  W-NGS        PIC  9(004).
             03  F            PIC  X(002).
           02  W-ENGP         PIC  9(008).
      *
           COPY LIBFDD.
      *FD  STRAN3
       01  STRAN3_HMG930.
           02  STRAN3_PNAME1     PIC  X(007) VALUE "STRAN-3".
           02  F                 PIC  X(001).
           02  STRAN3_LNAME      PIC  X(013) VALUE "STRAN3_HMG930".
           02  F                 PIC  X(001).
           02  STRAN3_KEY1       PIC  X(100) VALUE SPACE.
           02  STRAN3_SORT       PIC  X(100) VALUE SPACE.
           02  STRAN3_IDLST      PIC  X(100) VALUE SPACE.
           02  STRAN3_RES        USAGE  POINTER.
       01  STRAN3-R.
           02  ST3-KEY.
             03  ST3-DNO         PIC  9(006).
             03  ST3-GNO         PIC  9(001).
           02  F                 PIC  X(121).
       77  F                     PIC  X(001).
      *FD  STRANYR
       01  STRANYR_HMG930.
           02  STRANYR_PNAME1    PIC  X(007) VALUE "STRANYR".
           02  F                 PIC  X(001).
           02  STRANYR_LNAME     PIC  X(014) VALUE "STRANYR_HMG930".
           02  F                 PIC  X(001).
           02  STRANYR_KEY1      PIC  X(100) VALUE SPACE.
           02  STRANYR_SORT      PIC  X(100) VALUE SPACE.
           02  STRANYR_IDLST     PIC  X(100) VALUE SPACE.
           02  STRANYR_RES       USAGE  POINTER.
       01  STRANY-R.
           02  STRANY-DNO        PIC  9(006).
           02  STRANY-GNO        PIC  9(001).
           02  STRANY-NGP.
             03  STRANY-NG.
               04  STRANY-NEN    PIC  9(004).
               04  STRANY-GET    PIC  9(002).
             03  STRANY-PEY      PIC  9(002).
           02  STRANY-TCD        PIC  9(004).
           02  STRANY-D1         PIC  X(107).
           02  STRANY-D2    REDEFINES STRANY-D1.
             03  STRANY-BIF      PIC  N(010).
             03  STRANY-BIR      PIC  N(010).
             03  F               PIC  X(067).
           02  F                 PIC  X(002).
       77  F                     PIC  X(001).
      *FD  UTRF3
       01  UTRF3_HMG930.
           02  UTRF3_PNAME1      PIC  X(009) VALUE SPACE.
           02  F                 PIC  X(001).
           02  UTRF3_LNAME       PIC  X(012) VALUE "UTRF3_HMG930".
           02  F                 PIC  X(001).
           02  UTRF3_KEY1        PIC  X(100) VALUE SPACE.
           02  UTRF3_SORT        PIC  X(100) VALUE SPACE.
           02  UTRF3_IDLST       PIC  X(100) VALUE SPACE.
           02  UTRF3_RES         USAGE  POINTER.
       01  UTRF3-R               PIC  X(128).
       77  F                     PIC  X(001).
      *FD  UTRYR
       01  UTRYR_HMG930.
           02  UTRYR_PNAME1      PIC  X(009) VALUE "UTRYR-RDB".
           02  F                 PIC  X(001).
           02  UTRYR_LNAME       PIC  X(012) VALUE "UTRYR_HMG930".
           02  F                 PIC  X(001).
           02  UTRYR_KEY1        PIC  X(100) VALUE SPACE.
           02  UTRYR_SORT        PIC  X(100) VALUE SPACE.
           02  UTRYR_IDLST       PIC  X(100) VALUE SPACE.
           02  UTRYR_RES         USAGE  POINTER.
       01  UTRY-R.
           02  UTRY-DNO          PIC  9(006).
           02  UTRY-GNO          PIC  9(001).
           02  UTRY-NGP.
             03  UTRY-NG.
               04  UTRY-NEN      PIC  9(004).
               04  UTRY-GET      PIC  9(002).
             03  UTRY-PEY        PIC  9(002).
           02  F                 PIC  X(113).
       77  F                     PIC  X(001).
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
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-MID0.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　売上・値引伝票ワーク　作成　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　（　３ヶ月　）　　　　　＊＊＊".
       01  C-MID1.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　仕上・受入伝票ワーク　作成　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　（　９ヶ月　）　　　　　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(040).
             03  E-FILE  PIC  X(013).
             03  E-KEY   PIC  X(007).
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
           "C-MID" " " "0" "0" "230" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "15" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "15" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "15" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "8" "15" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "9" "15" "46" "04C-MID" " " RETURNING RESU.
      *C-MID0
       CALL "SD_Init" USING
           "C-MID0" " " "0" "0" "92" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID0" "N" "6" "15" "46" " " "C-MID0" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID0" "N" "7" "15" "46" "01C-MID0" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING
           "C-MID1" " " "0" "0" "92" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID1" "N" "6" "15" "46" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID1" "N" "7" "15" "46" "01C-MID1" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "60" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "60" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME" "X" "24" "15" "40" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-ME" BY REFERENCE W-MSG "40" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-FILE" "X" "24" "55" "13" "E-ME" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-FILE" BY REFERENCE ST3-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "X" "24" "68" "7" "E-FILE" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE ST3-KEY "7" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
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
           IF  JS-SIGN = 0
               CALL "SD_Output" USING "C-MID0" C-MID0 "p"
                RETURNING RESU
           ELSE
               CALL "SD_Output" USING "C-MID1" C-MID1 "p"
                RETURNING RESU
           END-IF
           MOVE ZERO TO W-NGP.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NGP TO W-ENGP.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           IF  JS-SIGN = 0
               GO TO M-20
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           GO TO M-40.
       M-20.
           CALL "DB_F_Open" USING
            "OUTPUT" STRAN3_PNAME1 " " BY REFERENCE STRAN3_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" STRANYR_PNAME1 " " BY REFERENCE STRANYR_IDLST "0".
       M-25.
      *           READ STRANYR AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" STRANYR_PNAME1 BY REFERENCE STRANY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           IF  STRANY-NGP < W-NGP
               GO TO M-25
           END-IF
           IF  STRANY-NGP > W-ENGP
               GO TO M-25
           END-IF.
       M-30.
           MOVE SPACE TO STRAN3-R.
           MOVE STRANY-R TO STRAN3-R.
      *           WRITE STRAN3-R.
      *//////////////
           CALL "DB_Insert" USING
            STRAN3_PNAME1 STRAN3_LNAME STRAN3-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-25
           END-IF
           MOVE SPACE TO W-MSG W-FILE.
           MOVE "***  WRITE ｴﾗｰ  ***" TO W-MSG.
           MOVE "STRAN-3" TO W-FILE.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU.
           CALL "SD_Output" USING "E-FILE" E-FILE "p" RETURNING RESU.
           CALL "SD_Output" USING "E-KEY" E-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-35
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE STRAN3_IDLST STRAN3_PNAME1.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" STRAN3_PNAME1 " " BY REFERENCE STRAN3_IDLST "0".
           GO TO M-30.
       M-35.
           CALL "DB_F_Close" USING
            BY REFERENCE STRAN3_IDLST STRAN3_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE STRANYR_IDLST STRANYR_PNAME1.
           GO TO M-95.
       M-40.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO UTRF3_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" UTRF3_PNAME1 " " BY REFERENCE UTRF3_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" UTRYR_PNAME1 " " BY REFERENCE UTRYR_IDLST "0".
      *           SELECT UTRYR WHERE UTRY-NGP >= W-NGP.
      *///////////////
           CALL "DB_Select" USING
            UTRYR_PNAME1 "WHERE" 
            "UTRY-NGP" ">=" W-NGP RETURNING RET.
       M-45.
      *           READ UTRYR AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" UTRYR_PNAME1 BY REFERENCE UTRY-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_Scratch" USING UTRYR_PNAME1
               GO TO M-50
           END-IF
           MOVE SPACE TO UTRF3-R.
           MOVE UTRY-R TO UTRF3-R.
      *           WRITE UTRF3-R.
      *//////////////
           CALL "DB_Insert" USING
            UTRF3_PNAME1 UTRF3_LNAME UTRF3-R RETURNING RET.
           GO TO M-45.
       M-50.
           CALL "DB_F_Close" USING
            BY REFERENCE UTRF3_IDLST UTRF3_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UTRYR_IDLST UTRYR_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

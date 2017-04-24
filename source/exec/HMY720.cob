       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY720.
      **************************************************************
      *    PROGRAM         :  出荷集計年間累積Ｆ　変換             *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  ******                               *
      *        変更　　　  :  95/07/13                             *
      *    COMPILE TYPE    :  COBOL                                *
      **************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SND   REDEFINES W-SNEN.
               04  W-SN1      PIC  9(002).
               04  W-SN2      PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-ENG.
             03  W-ENEN       PIC  9(004).
             03  W-END   REDEFINES W-ENEN.
               04  W-EN1      PIC  9(002).
               04  W-EN2      PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-ENGL  REDEFINES W-ENG.
             03  F            PIC  9(002).
             03  W-ENGS       PIC  9(004).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
       01  SSR-YF_HMY720.
           02  SSR-YF_PNAME1   PIC  X(005)  VALUE "SSRYF".
           02  F               PIC  X(001).
           02  SSR-YF_LNAME    PIC  X(013)  VALUE "SSR-YF_HMY720".
           02  F               PIC  X(001).
           02  SSR-YF_KEY1     PIC  X(100)  VALUE SPACE.
           02  SSR-YF_KEY2     PIC  X(100)  VALUE SPACE.
           02  SSR-YF_SORT     PIC  X(100)  VALUE SPACE.
           02  SSR-YF_IDLST    PIC  X(100)  VALUE SPACE.
           02  SSR-YF_RES      USAGE  POINTER.
       01  SSR-YR.
           02  Y-TCD          PIC  9(004).
           02  Y-HCD          PIC  9(006).
           02  Y-SU           PIC S9(007).
           02  Y-UK           PIC S9(010).
           02  Y-GK           PIC S9(010).
           02  Y-TC1          PIC  9(002).
           02  Y-TC2          PIC  9(002).
           02  Y-BC1          PIC  9(002).
           02  Y-BC2          PIC  9(002).
           02  Y-BC3          PIC  9(002).
           02  F              PIC  X(003).
           02  Y-FKC          PIC  9(002).
           02  Y-NG           PIC  9(006).
           02  F              PIC  X(006).
       77  F                  PIC  X(001).
       01  WSSR-YF_HMY720.
           02  WSSR-YF_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F                PIC  X(001).
           02  WSSR-YF_LNAME    PIC  X(014)  VALUE "WSSR-YF_HMY720".
           02  F                PIC  X(001).
           02  WSSR-YF_KEY1     PIC  X(100)  VALUE SPACE.
           02  WSSR-YF_KEY2     PIC  X(100)  VALUE SPACE.
           02  WSSR-YF_SORT     PIC  X(100)  VALUE SPACE.
           02  WSSR-YF_IDLST    PIC  X(100)  VALUE SPACE.
           02  WSSR-YF_RES      USAGE  POINTER.
       01  WSSR-YR.
           02  WY-TCD         PIC  9(004).
           02  WY-HCD         PIC  9(006).
           02  WY-SU          PIC S9(007).
           02  WY-UK          PIC S9(010).
           02  WY-GK          PIC S9(010).
           02  WY-TC1         PIC  9(002).
           02  WY-TC2         PIC  9(002).
           02  WY-BC1         PIC  9(002).
           02  WY-BC2         PIC  9(002).
           02  WY-BC3         PIC  9(002).
           02  F              PIC  X(003).
           02  WY-FKC         PIC  9(002).
           02  WY-NG          PIC  9(006).
           02  F              PIC  X(006).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　出荷集計年間累積ファイル　変換　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(024) VALUE
                "'  年   月 ～ '  年   月".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM     PIC  9(001).
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "396" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING
            "08C-MID" "X" "15" "23" "24" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "20" "24" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "41" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "15" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "9" "15" "24" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-SN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "9" "15" "29" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NG" "9" "15" "38" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE W-EN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NG" "9" "15" "43" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NG" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "27" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "27" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                         RETURNING RESU.
           COPY LIBCPR.
           MOVE ZERO TO W-SNG W-ENG.
           MOVE D-SPNG TO W-SNGS.
           MOVE D-EPNG TO W-ENGS.
           IF  W-SN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF.
           IF  W-SN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
           IF  W-EN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF.
           IF  W-EN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
           CALL "SD_Output" USING "D-NG" D-NG "p" 
                                         RETURNING RESU.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF.
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-30
           END-IF.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO WSSR-YF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SSR-YF_PNAME1 "SHARED" BY REFERENCE
            SSR-YF_IDLST "0".
       M-35.
      *           READ SSR-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSR-YF_PNAME1 BY REFERENCE SSR-YR " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SSR-YF_IDLST SSR-YF_PNAME1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                         RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  Y-NG < W-SNG OR > W-ENG
               GO TO M-35
           END-IF.
           IF  ZERO = Y-SU AND Y-UK AND Y-GK
               GO TO M-35
           END-IF.
           CALL "DB_F_Open" USING
            "OUTPUT" WSSR-YF_PNAME1 " " BY REFERENCE WSSR-YF_IDLST "0".
       M-40.
           MOVE ZERO TO WSSR-YR.
           MOVE SSR-YR TO WSSR-YR.
      *           WRITE WSSR-YR.
      *//////////////////////
           CALL "DB_Insert" USING
            WSSR-YF_PNAME1 WSSR-YF_LNAME WSSR-YR RETURNING RET.
       M-45.
      *           READ SSR-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSR-YF_PNAME1 BY REFERENCE SSR-YR " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF.
           IF  Y-NG < W-SNG OR > W-ENG
               GO TO M-45
           END-IF.
           IF  ZERO = Y-SU AND Y-UK AND Y-GK
               GO TO M-45
           END-IF.
           GO TO M-40.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE SSR-YF_IDLST SSR-YF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WSSR-YF_IDLST WSSR-YF_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

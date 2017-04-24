       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HMY610.
       AUTHOR.        S-NAKAO.
      *********************************************************
      *    PROGRAM         :  年間得意先売上集計ファイル作成  *
      *    PRINTER TYPE    :  *****                           *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  95/07/20                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0128".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0064".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  W-SNG          PIC  9(006).
           02  W-SNGD  REDEFINES W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SND   REDEFINES W-SNEN.
               04  W-SN1      PIC  9(002).
               04  W-SN2      PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-ENG          PIC  9(006).
           02  W-ENGD  REDEFINES W-ENG.
             03  W-ENEN       PIC  9(004).
             03  W-END   REDEFINES W-ENEN.
               04  W-EN1      PIC  9(002).
               04  W-EN2      PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-ENGL  REDEFINES W-ENG.
             03  F            PIC  9(002).
             03  W-ENGS       PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  W-D.
             03  W-TCD        PIC  9(004).
             03  W-FKC        PIC  9(002).
             03  W-TKC        PIC  9(002).
             03  W-TNC        PIC  9(002).
             03  W-UKIN       PIC S9(010).
             03  W-RKIN       PIC S9(010).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LITM.
       01  TMY-F_HMY610.
           02  TMY-F_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  TMY-F_LNAME    PIC  X(012)  VALUE "TMY-F_HMY610".
           02  F              PIC  X(001).
           02  TMY-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  TMY-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  TMY-F_SORT     PIC  X(100)  VALUE SPACE.
           02  TMY-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  TMY-F_RES      USAGE  POINTER.
       01  TMY-R.
           02  TMY-TCD        PIC  9(004).
           02  F              PIC  X(032).
           02  TMY-UKIN       PIC S9(009).
           02  F              PIC  X(007).
           02  TMY-NKIN       PIC S9(008).
           02  F              PIC  X(022).
           02  TMY-GKIN       PIC S9(009).
           02  TMY-TKC        PIC  9(002).
           02  TMY-TNC        PIC  9(002).
           02  TMY-FKC        PIC  9(002).
           02  TMY-BMC        PIC  9(001).
           02  F              PIC  X(024).
           02  TMY-NG         PIC  9(006).
       77  F                  PIC  X(001).
       01  TMG-F_HMY610.
           02  TMG-F_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  TMG-F_LNAME    PIC  X(012)  VALUE "TMG-F_HMY610".
           02  F              PIC  X(001).
           02  TMG-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  TMG-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  TMG-F_SORT     PIC  X(100)  VALUE SPACE.
           02  TMG-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  TMG-F_RES      USAGE  POINTER.
       01  TMG-R.
           02  TMG-TCD        PIC  9(004).
           02  TMG-FKC        PIC  9(002).
           02  TMG-TKC        PIC  9(002).
           02  TMG-TNC        PIC  9(002).
           02  TMG-UKIN       PIC S9(010).
           02  TMG-RKIN       PIC S9(010).
           02  F              PIC  X(034).
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
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊　　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊　　得意先年間売上集計ファイル　作成　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊　　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(026) VALUE
               "'  年   月　～　'  年   月".
           02  FILLER  PIC  X(022) VALUE
               "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM     PIC  9(001).
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC 99.
             03  FILLER  PIC Z9.
             03  FILLER  PIC 99.
             03  FILLER  PIC Z9.
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
             03  E-STAT    PIC  X(002).
             03  E-CL      PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "412" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING
            "08C-MID" "X" "15" "23" "26" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "20" "25" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "42" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "15" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "99" "15" "24" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-SN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "Z9" "15" "29" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NG" "99" "15" "40" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE W-EN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NG" "Z9" "15" "45" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NG" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "79" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "79" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
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
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" 
                                              RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
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
               GO TO M-10
           END-IF.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0128ID.
           MOVE W-FID2 TO WK0064ID.
           MOVE WK0128ID TO TMY-F_PNAME1.
           MOVE WK0064ID TO TMG-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TMY-F_PNAME1 " " BY REFERENCE TMY-F_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" TMG-F_PNAME1 " " BY REFERENCE TMG-F_IDLST "0".
       M-15.
      *           READ TMY-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TMY-F_PNAME1 BY REFERENCE TMY-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                              RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                              RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE TMY-F_IDLST TMY-F_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  TMY-NG < W-SNG OR > W-ENG
               GO TO M-15
           END-IF.
           IF  ZERO = TMY-UKIN AND TMY-NKIN AND TMY-GKIN
               GO TO M-15
           END-IF.
       M-20.
           MOVE ZERO TO W-D.
           MOVE TMY-TCD TO W-TCD.
           MOVE TMY-TKC TO W-TKC.
           MOVE TMY-TNC TO W-TNC.
           MOVE TMY-FKC TO W-FKC.
       M-25.
           COMPUTE W-UKIN = W-UKIN + TMY-UKIN - TMY-NKIN.
           COMPUTE W-RKIN = W-RKIN + TMY-UKIN - TMY-NKIN - TMY-GKIN.
       M-30.
      *           READ TMY-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TMY-F_PNAME1 BY REFERENCE TMY-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
           IF  TMY-NG < W-SNG OR > W-ENG
               GO TO M-30
           END-IF.
           IF  ZERO = TMY-UKIN AND TMY-NKIN AND TMY-GKIN
               GO TO M-30
           END-IF.
           IF  TMY-TCD = W-TCD
               GO TO M-25
           END-IF.
           MOVE ZERO TO TMG-R.
           MOVE W-TCD TO TMG-R.
           MOVE W-FKC TO TMG-FKC.
           MOVE W-TKC TO TMG-TKC.
           MOVE W-TNC TO TMG-TNC.
           MOVE W-UKIN TO TMG-UKIN.
           MOVE W-RKIN TO TMG-RKIN.
      *           WRITE TMG-R.
      *//////////////////////
           CALL "DB_Insert" USING
            TMG-F_PNAME1 TMG-F_LNAME TMG-R RETURNING RET.
           GO TO M-20.
       M-90.
           MOVE ZERO TO TMG-R.
           MOVE W-TCD TO TMG-R.
           MOVE W-FKC TO TMG-FKC.
           MOVE W-TKC TO TMG-TKC.
           MOVE W-TNC TO TMG-TNC.
           MOVE W-UKIN TO TMG-UKIN.
           MOVE W-RKIN TO TMG-RKIN.
      *           WRITE TMG-R.
      *//////////////////////
           CALL "DB_Insert" USING
            TMG-F_PNAME1 TMG-F_LNAME TMG-R RETURNING RET.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TMY-F_IDLST TMY-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TMY-F_IDLST TMG-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

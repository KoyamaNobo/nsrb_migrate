       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG080.
      *********************************************************
      *    PROGRAM         :  請求書控え目次作成　　　　      *
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
       01  W-DATA.
           02  W-NG           PIC  9(004).
           02  W-NGD   REDEFINES W-NG.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-DC           PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
      *FD  SM-F
       01  SM-F_HKG080.
           02  SM-F_PNAME1    PIC  X(003) VALUE "SMF".
           02  F              PIC  X(001).
           02  SM-F_LNAME     PIC  X(011) VALUE "SM-F_HKG080".
           02  F              PIC  X(001).
           02  SM-F_KEY1      PIC  X(100) VALUE SPACE.
           02  SM-F_SORT      PIC  X(100) VALUE SPACE.
           02  SM-F_IDLST     PIC  X(100) VALUE SPACE.
           02  SM-F_RES       USAGE  POINTER.
       01  SM-R.
           02  SM-TCD         PIC  9(004).
           02  SM-DATE.
             03  F            PIC  9(002).
             03  SM-SNG       PIC  9(004).
             03  F            PIC  9(002).
           02  SM-ZS          PIC S9(009).
           02  SM-ZSZ         PIC S9(007).
           02  SM-UR          PIC S9(009).
           02  SM-URZ         PIC S9(007).
           02  SM-TS          PIC S9(007).
           02  SM-TSZ         PIC S9(005).
           02  SM-NK          PIC S9(009).
           02  SM-NKZ         PIC S9(007).
           02  SM-NNGP        PIC  9(006).
           02  SM-SIT         PIC  9(003).
           02  SM-SU          PIC  9(001).
           02  SM-DNO         PIC  9(006).
           02  F              PIC  X(004).
           02  SM-TNC         PIC  9(002).
           02  F              PIC  X(007).
           02  SM-PC          PIC  9(001).
       77  F                  PIC  X(001).
      *FD  AD-F
       01  AD-F_HKG080.
           02  AD-F_PNAME1    PIC  X(009) VALUE "WK0128000".
           02  F              PIC  X(001).
           02  AD-F_LNAME     PIC  X(011) VALUE "AD-F_HKG080".
           02  F              PIC  X(001).
           02  AD-F_KEY1      PIC  X(100) VALUE SPACE.
           02  AD-F_SORT      PIC  X(100) VALUE SPACE.
           02  AD-F_IDLST     PIC  X(100) VALUE SPACE.
           02  AD-F_RES       USAGE  POINTER.
       01  AD-R.
           02  AD-KANA        PIC  X(036).
           02  AD-TCD         PIC  9(004).
           02  AD-NAME        PIC  N(026).
           02  AD-DATE        PIC  9(008).
           02  AD-DNO         PIC  9(006).
           02  AD-TNC         PIC  9(002).
           02  F              PIC  X(020).
       77  F                  PIC  X(001).
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
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　請求書控　目次作表　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(013) VALUE
                "'  年   月 分".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-NG.
             03  A-NEN   PIC  9(002).
             03  A-GET   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
           "C-MID" " " "0" "0" "301" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "38" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "38" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "38" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "38" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "38" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "38" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "38" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "15" "22" "13" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "20" "18" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-NG" " " "15" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-NEN" "9" "15" "23" "2" " " "A-NG" RETURNING RESU.
       CALL "SD_Using" USING
           "A-NEN" BY REFERENCE W-NEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-GET" "9" "15" "28" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "35" "1" "A-NG" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
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
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NG.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           CALL "SD_Output" USING "A-NG" A-NG "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" SM-F_PNAME1 " " BY REFERENCE SM-F_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" AD-F_PNAME1 " " BY REFERENCE AD-F_IDLST "0".
       M-25.
      *           READ SM-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SM-F_PNAME1 BY REFERENCE SM-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SM-SNG NOT = W-NG
               GO TO M-25
           END-IF
           IF  SM-DNO = ZERO
               GO TO M-25
           END-IF
           IF  SM-PC NOT = 0
               GO TO M-25
           END-IF
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF
      *
           MOVE SM-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME T-KANA
               MOVE "　＊＊　マスター　なし　＊＊　" TO T-NAME
           END-IF
      *
           INITIALIZE AD-R.
           MOVE SPACE TO AD-NAME AD-KANA.
           MOVE T-KANA TO AD-KANA.
           MOVE SM-TCD TO AD-TCD.
           MOVE T-NAME TO AD-NAME.
           MOVE SM-DATE TO AD-DATE.
           MOVE SM-DNO TO AD-DNO.
           MOVE SM-TNC TO AD-TNC.
      *           WRITE AD-R.
      *///////////////
           CALL "DB_Insert" USING
            AD-F_PNAME1 AD-F_LNAME AD-R RETURNING RET.
           GO TO M-25.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SM-F_IDLST SM-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE AD-F_IDLST AD-F_PNAME1.
           IF  W-DC = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

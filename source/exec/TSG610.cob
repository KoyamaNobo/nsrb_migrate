       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSG610.
      **********************************
      *****     手形　月次更新     *****
      **********************************
       AUTHOR. S-NAKAO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SNENL REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-WNG.
             03  W-WNEN       PIC  9(002).
             03  W-WGET       PIC  9(002).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIBANK.
      *FD  TYB-F
       01  TYB-F_TSG610.
           02  TYB-F_PNAME1   PIC  X(004) VALUE "TYBF".
           02  F              PIC  X(001).
           02  TYB-F_LNAME    PIC  X(012) VALUE "TYB-F_TSG610".
           02  F              PIC  X(001).
           02  TYB-F_KEY1     PIC  X(100) VALUE SPACE.
           02  TYB-F_SORT     PIC  X(100) VALUE SPACE.
           02  TYB-F_IDLST    PIC  X(100) VALUE SPACE.
           02  TYB-F_RES      USAGE  POINTER.
       01  YRIT-R             PIC  X(051).
       77  F                  PIC  X(001).
      *FD  RNOUSTYR
       01  RNOUSTYR_TSG610.
           02  RNOUSTYR_PNAME1   PIC  X(008) VALUE "RNOUSTYR".
           02  F                 PIC  X(001).
           02  RNOUSTYR_LNAME    PIC  X(015) VALUE "RNOUSTYR_TSG610".
           02  F                 PIC  X(001).
           02  RNOUSTYR_KEY1     PIC  X(100) VALUE SPACE.
           02  RNOUSTYR_SORT     PIC  X(100) VALUE SPACE.
           02  RNOUSTYR_IDLST    PIC  X(100) VALUE SPACE.
           02  RNOUSTYR_RES      USAGE  POINTER.
       01  RNOUSTY-R.
           02  F                 PIC  X(194).
           02  RNOUSTY-NGP.
             03  RNOUSTY-NG      PIC  9(004).
             03  F               PIC  9(002).
       77  F                     PIC  X(001).
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
           02  FILLER  PIC  N(019) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                 "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                 "＊＊＊　　　手形　月次更新　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                 "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(027) VALUE
                "<   H.  年  月分   ﾘﾀｰﾝ   >".
       01  C-ACP.
           02  FILLER.
             03  A-NEN   PIC  9(002).
             03  A-GET   PIC  9(002).
             03  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  DATEM ﾅｼ  ***".
             03  E-ME2   PIC  X(027) VALUE
                  "***  DATEM REWRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(027) VALUE
                  "***  BANKM REWRITE ｴﾗｰ  ***".
             03  E-ME9   PIC  N(009) VALUE
                   "［　業務　放棄　］".
             03  E-ME78  PIC  N(002) VALUE  "連絡".
             03  E-KEY   PIC  9(004).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "293" " " " " RETURNING RESU.
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
            "08C-MID" "X" "14" "15" "27" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "14" "0" "5" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "14" "21" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-WNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "14" "25" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-WGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "14" "33" "1" "A-GET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "108" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "108" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "27" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "27" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "N" "24" "50" "18" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "9" "24" "45" "4" "E-ME78" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE B-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-KEY" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" M-DATE_PNAME1 "SHARED" BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
           MOVE "01" TO DATE-KEY.
      *           READ M-DATE INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" M-DATE_PNAME1 BY REFERENCE DATE-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE M-DATE_IDLST M-DATE_PNAME1
               GO TO M-95
           END-IF
           MOVE ZERO TO W-SNG W-WNG.
           MOVE D-NTNG TO W-SNGS.
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF
           COMPUTE W-WNEN = W-SNEN - DATE-YC1.
           MOVE W-SGET TO W-WGET.
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU.
           GO TO M-25.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE M-DATE_IDLST M-DATE_PNAME1
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-WGET < 1 OR > 12
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-DMM = 9
               GO TO M-15
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-25
           END-IF
      *
           MOVE ZERO TO W-SNG.
           MOVE W-WNG TO W-SNGS.
           ADD 1 TO W-SGET.
           IF  W-SGET = 13
               ADD 1 TO W-SNEN
               MOVE 1 TO W-SGET
           END-IF
           IF  W-SNEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-SNEN
           ELSE
               IF  W-SNEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-SNEN
               END-IF
           END-IF
           MOVE W-SNGS TO D-NTNG.
      *           REWRITE DATE-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            M-DATE_PNAME1 M-DATE_LNAME DATE-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE M-DATE_IDLST M-DATE_PNAME1
               GO TO M-95
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" TYB-F_PNAME1 " " BY REFERENCE TYB-F_IDLST "0".
           CALL "DB_F_Close" USING
            BY REFERENCE TYB-F_IDLST TYB-F_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" BANK-M_PNAME1 " " BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
       M-40.
      *           READ BANK-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" BANK-M_PNAME1 BY REFERENCE BANK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF
           IF  B-YBC = 0
               GO TO M-40
           END-IF
           MOVE B-YBZ TO B-ZYZ.
      *           REWRITE BANK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            BANK-M_PNAME1 BANK-M_LNAME BANK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-45
           END-IF
           GO TO M-40.
       M-45.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           IF  W-SGET NOT = 5
               GO TO M-95
           END-IF
           SUBTRACT 1 FROM W-SNEN.
           CALL "DB_F_Open" USING
            "I-O" RNOUSTYR_PNAME1 " " BY REFERENCE RNOUSTYR_IDLST "0".
       M-50.
      *           READ RNOUSTYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" RNOUSTYR_PNAME1 BY REFERENCE RNOUSTY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           IF  RNOUSTY-NG >= W-SNG
               GO TO M-55
           END-IF
           MOVE X"FF" TO RNOUSTY-R.
      *           REWRITE RNOUSTY-R.
      *///////////////
           CALL "DB_Update" USING
            RNOUSTYR_PNAME1 RNOUSTYR_LNAME RNOUSTY-R RETURNING RET.
           GO TO M-50.
       M-55.
           CALL "DB_F_Close" USING
            BY REFERENCE RNOUSTYR_IDLST RNOUSTYR_PNAME1.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  100.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

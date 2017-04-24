       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY100.
      **************************************************************
      *    PROGRAM         :  履物年間作表　期間日付入力           *
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
           02  W-ENG.
             03  W-ENEN       PIC  9(004).
             03  W-ENENL REDEFINES W-ENEN.
               04  W-ENEN1    PIC  9(002).
               04  W-ENEN2    PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-ENGL  REDEFINES W-ENG.
             03  F            PIC  9(002).
             03  W-ENGS       PIC  9(004).
           02  W-SYM.
             03  W-SYY        PIC  9(004).
             03  W-SYYL  REDEFINES W-SYY.
               04  W-SYY1     PIC  9(002).
               04  W-SYY2     PIC  9(002).
             03  W-SMM        PIC  9(002).
           02  W-SYML  REDEFINES W-SYM.
             03  F            PIC  9(002).
             03  W-SYMS       PIC  9(004).
           02  W-EYM.
             03  W-EYY        PIC  9(004).
             03  W-EYYL  REDEFINES W-EYY.
               04  W-EYY1     PIC  9(002).
               04  W-EYY2     PIC  9(002).
             03  W-EMM        PIC  9(002).
           02  W-EYML  REDEFINES W-EYM.
             03  F            PIC  9(002).
             03  W-EYMS       PIC  9(004).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　履物年間作表　期間日付入力　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(040) VALUE
                "ＤＡＴＡ期間    '  年   月 ～ '  年   月".
           02  FILLER  PIC  X(040) VALUE
                "作 表 期 間     '  年   月 ～ '  年   月".
           02  FILLER2  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DNG.
             03  A-SYY     PIC  9(002).
             03  A-SMM     PIC  9(002).
             03  A-EYY     PIC  9(002).
             03  A-EMM     PIC  9(002).
           02  A-PNG.
             03  A-SNEN    PIC  9(002).
             03  A-SGET    PIC  9(002).
             03  A-ENEN    PIC  9(002).
             03  A-EGET    PIC  9(002).
           02  A-DMM     PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(018) VALUE
                  "***  DATEM ﾅｼ  ***".
             03  E-ME2     PIC  X(027) VALUE
                  "***  DATEM REWRITE ｴﾗｰ  ***".
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "424" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "46" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "13" "13" "40" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "15" "13" "40" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "20" "22" "22" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "17" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-DNG" " " "13" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SYY" "9" "13" "30" "2" " " "A-DNG" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SYY" BY REFERENCE W-SYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SMM" "9" "13" "35" "2" "A-SYY" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-SMM" BY REFERENCE W-SMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EYY" "9" "13" "44" "2" "A-SMM" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EYY" BY REFERENCE W-EYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EMM" "9" "13" "49" "2" "A-EYY" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EMM" BY REFERENCE W-EMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-PNG" " " "15" "0" "8" "A-DNG" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SNEN" "9" "15" "30" "2" " " "A-PNG" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SNEN" BY REFERENCE W-SNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SGET" "9" "15" "35" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ENEN" "9" "15" "44" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-ENEN" BY REFERENCE W-ENEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EGET" "9" "15" "49" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "39" "1" "A-PNG" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "55" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "55" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "27" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           MOVE D-SNG TO W-SYMS.
           MOVE D-ENG TO W-EYMS.
           MOVE D-SPNG TO W-SNGS.
           MOVE D-EPNG TO W-ENGS.
           CALL "SD_Output" USING "A-DNG" A-DNG "p" RETURNING RESU.
           CALL "SD_Output" USING "A-PNG" A-PNG "p" RETURNING RESU.
           IF  W-SYY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SYY
           END-IF.
           IF  W-SYY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SYY
           END-IF.
           IF  W-EYY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-EYY
           END-IF.
           IF  W-EYY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-EYY
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
           MOVE ZERO TO W-SNEN1.
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF.
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
           IF  W-SNEN < W-SYY OR > W-EYY
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
           IF  W-SNG < W-SYM OR > W-EYM
               GO TO M-15
           END-IF.
           IF  W-SGET < 1 OR > 12
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
           MOVE ZERO TO W-ENEN1.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF.
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
           IF  W-ENEN < W-SNEN OR > W-EYY
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF.
           IF  W-ENG < W-SNG
               GO TO M-25
           END-IF.
           IF  W-ENG < W-SYM OR > W-EYM
               GO TO M-25
           END-IF.
           IF  W-EGET < 1 OR > 12
               GO TO M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF.
           IF  W-DMM = 9
               GO TO M-10
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-30
           END-IF.
           CALL "DB_F_Open" USING
            "I-O" M-DATE_PNAME1 "SHARED" BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
       M-35.
           MOVE "01" TO DATE-KEY.
      *           READ M-DATE INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" M-DATE_PNAME1 BY REFERENCE DATE-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                         RETURNING RESU
               GO TO M-95
           END-IF.
           MOVE W-SNGS TO D-SPNG.
           MOVE W-ENGS TO D-EPNG.
      *           REWRITE DATE-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            M-DATE_PNAME1 M-DATE_LNAME DATE-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" 
                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                         RETURNING RESU
           END-IF.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

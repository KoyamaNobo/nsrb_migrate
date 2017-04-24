       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY650.
      **************************************************************
      *    PROGRAM         :  履物年間サイズ別入庫作表期間日付入力 *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  ******                               *
      *    COMPILE TYPE    :  COBOL                                *
      **************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM7200.
       OBJECT-COMPUTER. SYSTEM7200.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
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
           02  W-SYM.
             03  W-SYY        PIC  9(004).
             03  W-SYD   REDEFINES W-SYY.
               04  W-SY1      PIC  9(002).
               04  W-SY2      PIC  9(002).
             03  W-SMM        PIC  9(002).
           02  W-SYML  REDEFINES W-SYM.
             03  F            PIC  9(002).
             03  W-SYMS       PIC  9(004).
           02  W-EYM.
             03  W-EYY        PIC  9(004).
             03  W-EYD   REDEFINES W-EYY.
               04  W-EY1      PIC  9(002).
               04  W-EY2      PIC  9(002).
             03  W-EMM        PIC  9(002).
           02  W-EYML  REDEFINES W-EYM.
             03  F            PIC  9(002).
             03  W-EYMS       PIC  9(004).
           02  W-SEN          PIC  9(001).
           02  W-DMM          PIC  9(001).
           COPY LSTAT.
           COPY LIBFDD.
       01  UTRF_HMY650.
           02  UTRF_PNAME1  PIC  X(004)  VALUE "UTRF".
           02  F            PIC  X(001).
           02  UTRF_LNAME   PIC  X(011)  VALUE "UTRF_HMY650".
           02  F            PIC  X(001).
           02  UTRF_KEY1    PIC  X(100)  VALUE SPACE.
           02  UTRF_KEY2    PIC  X(100)  VALUE SPACE.
           02  UTRF_SORT    PIC  X(100)  VALUE SPACE.
           02  UTRF_IDLST   PIC  X(100)  VALUE SPACE.
           02  UTRF_RES     USAGE  POINTER.
       01  UTR-R              PIC  X(128).
       77  F                  PIC  X(001).
       01  UTRYR_HMY650.
           02  UTRYR_PNAME1  PIC  X(009)  VALUE "UTRYR-RDB".
           02  F             PIC  X(001).
           02  UTRYR_LNAME   PIC  X(012)  VALUE "UTRYR_HMY650".
           02  F             PIC  X(001).
           02  UTRYR_KEY1    PIC  X(100)  VALUE SPACE.
           02  UTRYR_KEY2    PIC  X(100)  VALUE SPACE.
           02  UTRYR_SORT    PIC  X(100)  VALUE SPACE.
           02  UTRYR_IDLST   PIC  X(100)  VALUE SPACE.
           02  UTRYR_RES     USAGE  POINTER.
       01  UTRY-R.
           02  F              PIC  9(007).
           02  UTR-DATE.
             03  UTR-NG       PIC  9(006).
             03  UTR-PEY      PIC  9(002).
           02  UTR-HCD        PIC  9(006).
           02  UTR-SC         PIC  9(001).
           02  UTR-SUD.
             03  UTR-SU    OCCURS  10  PIC S9(004).
           02  UTR-TSU        PIC S9(005).
           02  F              PIC  X(016).
           02  UTR-NC         PIC  9(001).
           02  F              PIC  X(007).
           02  UTR-BC3        PIC  9(002).
           02  F              PIC  X(035).
       77  F                  PIC  X(001).
       01  NSSWF_HMY650.
           02  NSSWF_PNAME1  PIC  X(006)  VALUE "WK0064".
           02  F             PIC  X(001).
           02  NSSWF_LNAME   PIC  X(012)  VALUE "NSSWF_HMY650".
           02  F             PIC  X(001).
           02  NSSWF_KEY1    PIC  X(100)  VALUE SPACE.
           02  NSSWF_KEY2    PIC  X(100)  VALUE SPACE.
           02  NSSWF_SORT    PIC  X(100)  VALUE SPACE.
           02  NSSWF_IDLST   PIC  X(100)  VALUE SPACE.
           02  NSSWF_RES     USAGE  POINTER.
       01  NSSW-R.
           02  NSS-HCD        PIC  9(006).
           02  NSS-SC         PIC  9(001).
           02  NSS-SUD.
             03  NSS-SUDA  OCCURS  10.
               04  NSS-SU     PIC S9(006)  COMP-3.
           02  NSS-TSU        PIC S9(008)  COMP-3.
           02  NSS-TCD        PIC  9(004).
           02  NSS-NG         PIC  9(006).
           02  NSS-BC3        PIC  9(002).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
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
                "＊＊＊　　履物年間サイズ別　入庫作表　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(049) VALUE
                "全体 = 0  ,  一般・ワーク = 1  ,  教育 = 2  ...  ".
           02  FILLER  PIC  X(040) VALUE
                "ＤＡＴＡ期間    '  年   月 ～ '  年   月".
           02  FILLER  PIC  X(040) VALUE
                "作 表 期 間     '  年   月 ～ '  年   月".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SEN     PIC  9(001).
           02  A-PNG.
             03  A-SNEN    PIC  9(002).
             03  A-SGET    PIC  9(002).
             03  A-ENEN    PIC  9(002).
             03  A-EGET    PIC  9(002).
           02  A-DMM     PIC  9(001).
       01  C-DSP.
           02  D-DNG.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(018) VALUE
                  "***  DATEM ﾅｼ  ***".
             03  E-ME2     PIC  X(027) VALUE
                  "***  DATEM REWRITE ｴﾗｰ  ***".
             03  E-ME3     PIC  X(017) VALUE
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
            "C-MID" " " "0" "0" "473" " " " " RETURNING RESU.
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
            "08C-MID" "X" "12" "10" "49" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "15" "15" "40" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "17" "15" "40" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "11C-MID" "X" "20" "24" "22" "10C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SEN" "9" "12" "58" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-PNG" " " "17" "0" "8" "A-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SNEN" "9" "17" "32" "2" " " "A-PNG" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SNEN" BY REFERENCE W-SN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SGET" "9" "17" "37" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ENEN" "9" "17" "46" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-ENEN" BY REFERENCE W-EN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EGET" "9" "17" "51" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "41" "1" "A-PNG" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-DNG" " " "15" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-DNG" "9" "15" "32" "2" " " "D-DNG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DNG" BY REFERENCE W-SY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-DNG" "9" "15" "37" "2" "01D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DNG" BY REFERENCE W-SMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-DNG" "9" "15" "46" "2" "02D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-DNG" BY REFERENCE W-EY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-DNG" "9" "15" "51" "2" "03D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-DNG" BY REFERENCE W-EMM "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "72" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "72" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "27" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" "X" "24" "15" "17" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME3" " " RETURNING RESU.
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
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-NHNG TO W-EYMS.
           IF  W-EY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-EYY
           END-IF.
           IF  W-EY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-EYY
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" UTRF_PNAME1 " " BY REFERENCE UTRF_IDLST "0".
      *           READ UTRF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" UTRF_PNAME1 BY REFERENCE UTR-R " " RETURNING RET.
           IF  RET = 1
               SUBTRACT 1 FROM W-EMM
           END-IF.
           CALL "DB_F_Close" USING BY REFERENCE UTRF_IDLST UTRF_PNAME1.
           IF  W-EMM = ZERO
               SUBTRACT 1 FROM W-EYY
               MOVE 12 TO W-EMM
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" UTRYR_PNAME1 "SHARED" BY REFERENCE UTRYR_IDLST "0".
      *           READ UTRYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" UTRYR_PNAME1 BY REFERENCE UTRY-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE UTRYR_IDLST UTRYR_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "E-ME3" E-ME3 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-95
           END-IF.
           MOVE UTR-NG TO W-SYM.
           CALL "DB_F_Close" USING
            BY REFERENCE UTRYR_IDLST UTRYR_PNAME1.
           MOVE W-EYM TO W-SNG W-ENG.
           IF  W-EGET < 4
               COMPUTE W-ENEN = W-ENEN - 1
           END-IF.
           MOVE 4 TO W-EGET.
           COMPUTE W-SNEN = W-ENEN - 1.
           MOVE 5 TO W-SGET.
           IF  W-SYM > W-SNG
               MOVE W-SYM TO W-SNG
           END-IF.
           CALL "SD_Output" USING "D-DNG" D-DNG "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "A-PNG" A-PNG "p" 
                                  RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1" 
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
           IF  W-SEN > 2
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
           MOVE ZERO TO W-SN1.
           IF  W-SN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF.
           IF  W-SN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
           IF  W-SNEN < W-SYY OR > W-EYY
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
           IF  W-SNG < W-SYM OR > W-EYM
               GO TO M-20
           END-IF.
           IF  W-SGET < 1 OR > 12
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF.
           MOVE ZERO TO W-EN1.
           IF  W-EN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF.
           IF  W-EN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
           IF  W-ENEN < W-SNEN OR > W-EYY
               GO TO M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF.
           IF  W-ENG < W-SNG
               GO TO M-30
           END-IF.
           IF  W-ENG < W-SYM OR > W-EYM
               GO TO M-30
           END-IF.
           IF  W-EGET < 1 OR > 12
               GO TO M-30
           END-IF.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-30
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF.
           IF  W-DMM = 9
               GO TO M-10
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-35
           END-IF.
       M-40.
           CALL "DB_F_Open" USING
            "INPUT" UTRYR_PNAME1 "SHARED" BY REFERENCE UTRYR_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" NSSWF_PNAME1 " " BY REFERENCE NSSWF_IDLST "0".
       M-45.
      *           READ UTRYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" UTRYR_PNAME1 BY REFERENCE UTRY-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF.
           IF  UTR-NC = 2 OR 4
               GO TO M-45
           END-IF.
           IF  UTR-NG < W-SNG
               GO TO M-45
           END-IF.
           IF  UTR-NG > W-ENG
               GO TO M-80
           END-IF.
           IF  UTR-HCD > 999899
               GO TO M-45
           END-IF.
           IF  W-SEN = 1
               IF  UTR-BC3 = 30
                   GO TO M-45
               END-IF
           END-IF.
           IF  W-SEN = 2
               IF  UTR-BC3 NOT = 30
                   GO TO M-45
               END-IF
           END-IF.
           IF  ZERO = UTR-SU(01) AND UTR-SU(02) AND UTR-SU(03) AND
                     UTR-SU(04) AND UTR-SU(05) AND UTR-SU(06) AND
                     UTR-SU(07) AND UTR-SU(08) AND UTR-SU(09) AND
                     UTR-SU(10)
               GO TO M-45
           END-IF.
           MOVE ZERO TO NSSW-R.
           MOVE UTR-HCD TO NSS-HCD.
           MOVE UTR-SC TO NSS-SC.
           MOVE UTR-NG TO NSS-NG.
           MOVE UTR-BC3 TO NSS-BC3.
           MOVE UTR-SU(01) TO NSS-SU(01).
           MOVE UTR-SU(02) TO NSS-SU(02).
           MOVE UTR-SU(03) TO NSS-SU(03).
           MOVE UTR-SU(04) TO NSS-SU(04).
           MOVE UTR-SU(05) TO NSS-SU(05).
           MOVE UTR-SU(06) TO NSS-SU(06).
           MOVE UTR-SU(07) TO NSS-SU(07).
           MOVE UTR-SU(08) TO NSS-SU(08).
           MOVE UTR-SU(09) TO NSS-SU(09).
           MOVE UTR-SU(10) TO NSS-SU(10).
           MOVE UTR-TSU TO NSS-TSU.
           IF  UTR-NC = 5
               COMPUTE NSS-SU(01) = NSS-SU(01) * -1
               COMPUTE NSS-SU(02) = NSS-SU(02) * -1
               COMPUTE NSS-SU(03) = NSS-SU(03) * -1
               COMPUTE NSS-SU(04) = NSS-SU(04) * -1
               COMPUTE NSS-SU(05) = NSS-SU(05) * -1
               COMPUTE NSS-SU(06) = NSS-SU(06) * -1
               COMPUTE NSS-SU(07) = NSS-SU(07) * -1
               COMPUTE NSS-SU(08) = NSS-SU(08) * -1
               COMPUTE NSS-SU(09) = NSS-SU(09) * -1
               COMPUTE NSS-SU(10) = NSS-SU(10) * -1
               COMPUTE NSS-TSU = NSS-TSU * -1
           END-IF.
      *           WRITE NSSW-R.
      *//////////////////////
           CALL "DB_Insert" USING
            NSSWF_PNAME1 NSSWF_LNAME NSSW-R RETURNING RET.
           GO TO M-45.
       M-80.
           CALL "DB_F_Close" USING
            BY REFERENCE UTRYR_IDLST UTRYR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NSSWF_IDLST NSSWF_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" M-DATE_PNAME1 "SHARED" BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
       M-85.
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
           MOVE W-SNGS TO D-SSNG.
           MOVE W-ENGS TO D-ESNG.
      *           REWRITE DATE-R INVALID KEY
      *//////////////////////
           CALL "DB_Update" USING
            M-DATE_PNAME1 M-DATE_LNAME DATE-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
           END-IF.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU
           CALL "DB_Close".
           STOP RUN.

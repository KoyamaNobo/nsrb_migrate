       IDENTIFICATION   DIVISION.
       PROGRAM-ID. HMY010.
      *********************************************************
      *    PROGRAM         :  売上値引伝票合計　抽出    　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-R.
           02  W-DATE         PIC  9(008).
           02  W-NGPD  REDEFINES W-DATE.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
             03  F            PIC  9(002).
           02  W-DNO          PIC  9(006).
           02  W-TCD          PIC  9(004).
           02  W-SHZ          PIC S9(006).
           02  W-KIN          PIC S9(008).
           02  W-CSC          PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-HSC          PIC  9(001).
           02  W-TNC          PIC  9(002).
           02  W-UNC          PIC  9(001).
           02  F              PIC  X(026).
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
           02  W-DMM          PIC  9(001).
           02  W-DCHK         PIC  9(001) VALUE 0.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LITM.
       01  S-TRAN_HMY010.
           02  S-TRAN_PNAME1  PIC  X(007)  VALUE "STRANYR".
           02  F              PIC  X(001).
           02  S-TRAN_LNAME   PIC  X(013)  VALUE "S-TRAN_HMY010".
           02  F              PIC  X(001).
           02  S-TRAN_KEY1    PIC  X(100)  VALUE SPACE.
           02  S-TRAN_KEY2    PIC  X(100)  VALUE SPACE.
           02  S-TRAN_SORT    PIC  X(100)  VALUE SPACE.
           02  S-TRAN_IDLST   PIC  X(100)  VALUE SPACE.
           02  S-TRAN_RES     USAGE  POINTER.
       01  S-R.
           02  S-DNO          PIC  9(006).
           02  S-GNO          PIC  9(001).
           02  S-DATE         PIC  9(008).
           02  S-NGP   REDEFINES S-DATE.
             03  S-NG         PIC  9(006).
             03  F            PIC  9(002).
           02  S-TCD          PIC  9(004).
           02  S-HCD          PIC  9(006).
           02  S-SIZ          PIC  9(001).
           02  S-SU           PIC  X(030).
           02  S-SUT          PIC S9(005).
           02  S-T            PIC  9(005).
           02  S-KIN          PIC S9(008).
           02  S-CSC          PIC  9(001).
           02  S-DC           PIC  9(001).
           02  S-FT           PIC  9(005).
           02  S-CCD          PIC  9(003).
           02  F              PIC  X(007).
           02  S-TNC          PIC  9(002).
           02  F              PIC  X(002).
           02  S-HSC          PIC  9(001).
           02  S-KOSU         PIC  9(003).
           02  S-FRC          PIC  9(001).
           02  S-TCD2         PIC  9(004).
           02  F              PIC  X(023).
           02  S-UNC          PIC  9(001).
       77  F                  PIC  X(001).
       01  HUND-F_HMY010.
           02  HUND-F_PNAME1  PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  HUND-F_LNAME   PIC  X(013)  VALUE "HUND-F_HMY010".
           02  F              PIC  X(001).
           02  HUND-F_KEY1    PIC  X(100)  VALUE SPACE.
           02  HUND-F_KEY2    PIC  X(100)  VALUE SPACE.
           02  HUND-F_SORT    PIC  X(100)  VALUE SPACE.
           02  HUND-F_IDLST   PIC  X(100)  VALUE SPACE.
           02  HUND-F_RES     USAGE  POINTER.
       01  HUND-R.
           02  UN-DATE        PIC  9(008).
           02  UN-NO          PIC  9(006).
           02  UN-TCD         PIC  9(004).
           02  UN-SHZ         PIC S9(006).
           02  UN-KIN         PIC S9(008).
           02  UN-CSC         PIC  9(001).
           02  UN-DC          PIC  9(001).
           02  UN-HSC         PIC  9(001).
           02  UN-TNC         PIC  9(002).
           02  UN-UNC         PIC  9(001).
           02  F              PIC  X(026).
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
           02  C-CL  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　売上値引伝票　集計　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(040) VALUE
                "ＤＡＴＡ期間    '  年   月 ～ '  年   月".
           02  FILLER  PIC  X(040) VALUE
                "作 表 期 間     '  年   月 ～ '  年   月".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-PNG.
             03  A-SNEN    PIC  9(002).
             03  A-SGET    PIC  9(002).
             03  A-ENEN    PIC  9(002).
             03  A-EGET    PIC  9(002).
           02  A-DMM       PIC  9(001).
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
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
             03  E-STAT    PIC  X(002) .
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
            "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "368" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "2" "15" "38" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "3" "15" "38" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "4" "15" "38" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "5" "15" "38" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "6" "15" "38" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "7" "15" "38" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "8" "15" "38" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "13" "14" "40" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "15" "14" "40" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "20" "23" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-PNG" " " "15" "0" "4" " " "C-ACP" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-PNG" " " "15" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SNEN" "9" "15" "31" "2" " " "A-PNG" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SNEN" BY REFERENCE W-SN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SGET" "9" "15" "36" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ENEN" "9" "15" "45" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-ENEN" BY REFERENCE W-EN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EGET" "9" "15" "50" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "40" "1" "A-PNG" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-DNG" " " "13" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-DNG" "9" "13" "31" "2" " " "D-DNG" RETURNING RESU.
       CALL "SD_From" USING
            "01D-DNG" BY REFERENCE W-SY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-DNG" "9" "13" "36" "2" "01D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING
            "02D-DNG" BY REFERENCE W-SMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-DNG" "9" "13" "45" "2" "02D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING
            "03D-DNG" BY REFERENCE W-EY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-DNG" "9" "13" "50" "2" "03D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING
            "04D-DNG" BY REFERENCE W-EMM "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "107" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "107" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "27" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
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
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           PERFORM S-25 THRU S-55.
           IF  COMPLETION_CODE = 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                  RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO HUND-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" S-TRAN_PNAME1 " " BY REFERENCE S-TRAN_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" HUND-F_PNAME1 " " BY REFERENCE HUND-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
       M-10.
      *           READ S-TRAN AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" S-TRAN_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF.
           IF  S-GNO > 6
               GO TO M-10
           END-IF.
           IF  S-NG < W-SNG OR > W-ENG
               GO TO M-10
           END-IF.
           IF  S-DC = 4 OR 9
               GO TO M-10
           END-IF.
           IF  S-KIN = ZERO
               GO TO M-10
           END-IF.
       M-15.
           MOVE ZERO TO W-R.
           MOVE S-DNO TO W-DNO.
           MOVE S-DATE TO W-DATE.
           MOVE S-TCD TO W-TCD.
           MOVE S-CSC TO W-CSC.
           MOVE S-DC TO W-DC.
           MOVE S-HSC TO W-HSC.
           MOVE S-TNC TO W-TNC.
           MOVE S-UNC TO W-UNC.
       M-20.
           ADD S-KIN TO W-KIN.
       M-25.
      *           READ S-TRAN AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" S-TRAN_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF.
           IF  S-GNO > 6
               GO TO M-25
           END-IF.
           IF  S-NG < W-SNG OR > W-ENG
               GO TO M-25
           END-IF.
           IF  S-DC = 4 OR 9
               GO TO M-25
           END-IF.
           IF  S-KIN = ZERO
               GO TO M-25
           END-IF.
           IF  S-DNO = W-DNO
               GO TO M-20
           END-IF.
           PERFORM S-05 THRU S-10.
           GO TO M-15.
       M-30.
           PERFORM S-05 THRU S-10.
       M-35.
           CALL "DB_F_Close" USING
            BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HUND-F_IDLST HUND-F_PNAME1.
           IF  W-DCHK = 0
               GO TO M-95
           END-IF.
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
               GO TO M-90
           END-IF.
           MOVE W-SNGS TO D-SSNG.
           MOVE W-ENGS TO D-ESNG.
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
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           IF  W-HSC = 8
               COMPUTE W-SHZ ROUNDED = W-KIN * 0.08
           ELSE
               IF  W-HSC = 5
                   COMPUTE W-SHZ ROUNDED = W-KIN * 0.05
               ELSE
                   IF  W-HSC = 3
                       COMPUTE W-SHZ ROUNDED = W-KIN * 0.03
                   ELSE
                       IF  W-HSC = 1
                           COMPUTE W-SHZ ROUNDED = W-KIN * 0.10
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF (W-UNC = 1) OR (W-DC = 1 OR 2 OR 5)
               COMPUTE W-KIN = W-KIN * -1
               COMPUTE W-SHZ = W-SHZ * -1
           END-IF.
           IF  ZERO = W-KIN AND W-SHZ
               GO TO S-10
           END-IF.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO T-TNC
           END-IF.
           MOVE T-TNC TO W-TNC.
           MOVE ZERO TO HUND-R.
           MOVE W-R TO HUND-R.
      *           WRITE HUND-R.
      *///////////////
           CALL "DB_Insert" USING
            HUND-F_PNAME1 HUND-F_LNAME HUND-R RETURNING RET.
           IF  W-DCHK = 0
               MOVE 1 TO W-DCHK
           END-IF.
       S-10.
           EXIT.
       S-25.
           MOVE ZERO TO W-DATA.
           MOVE D-NHNG TO W-EYMS.
           IF  W-EY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-EYY
           END-IF.
           IF  W-EY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-EYY
           END-IF.
           SUBTRACT 1 FROM W-EMM.
           IF  W-EMM = 0
               SUBTRACT 1 FROM W-EYY
               MOVE 12 TO W-EMM
           END-IF.
           MOVE 5 TO W-SMM.
           IF  W-EMM < 5
               COMPUTE W-SYY = W-EYY - 2
           ELSE
               COMPUTE W-SYY = W-EYY - 1
           END-IF.
           MOVE W-SYM TO W-SNG W-ENG.
           ADD 1 TO W-ENEN.
           MOVE 4 TO W-EGET.
           CALL "SD_Output" USING "D-DNG" D-DNG "p"
                         RETURNING RESU.
           CALL "SD_Output" USING "A-PNG" A-PNG "p"
                         RETURNING RESU.
       S-30.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO S-55
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-30
           END-IF.
           MOVE ZERO TO W-SN1.
           IF  W-SN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF.
           IF  W-SN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
           IF  W-SNEN < W-SYY OR > W-EYY
               GO TO S-30
           END-IF.
       S-35.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO S-30
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-35
           END-IF.
           IF  W-SNG < W-SYM OR > W-EYM
               GO TO S-35
           END-IF.
           IF  W-SGET < 1 OR > 12
               GO TO S-35
           END-IF.
       S-40.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO S-35
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-40
           END-IF.
           MOVE ZERO TO W-EN1.
           IF  W-EN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF.
           IF  W-EN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
           IF  W-ENEN < W-SNEN OR > W-EYY
               GO TO S-40
           END-IF.
       S-45.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO S-40
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-45
           END-IF.
           IF  W-ENG < W-SNG
               GO TO S-45
           END-IF.
           IF  W-ENG < W-SYM OR > W-EYM
               GO TO S-45
           END-IF.
           IF  W-EGET < 1 OR > 12
               GO TO S-45
           END-IF.
       S-50.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO S-45
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-50
           END-IF.
           IF  W-DMM = 9
               GO TO S-30
           END-IF.
           IF  W-DMM NOT = 1
               GO TO S-50
           END-IF.
       S-55.
           EXIT.

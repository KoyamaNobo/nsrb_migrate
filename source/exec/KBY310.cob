       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBY310.
      *********************************************************
      *    PROGRAM         :  年間製品仕入明細表　　　　　    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *        変更　　　  :  62/06/05                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(014) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-HN           PIC  9(002).
           02  F              PIC  X(001) VALUE "/".
           02  H-HG           PIC  9(002).
           02  F              PIC  X(003) VALUE " - ".
           02  H-ON           PIC  9(002).
           02  F              PIC  X(001) VALUE "/".
           02  H-OG           PIC  9(002).
           02  F              PIC  N(017) VALUE
                "　年間　製品仕入　明細表　　＊＊＊".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
           02  F              PIC  X(020) VALUE SPACE.
       01  HEAD2.
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(007) VALUE "製　　品　　名".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  N(015) VALUE
                "数　量　　単　価　　　　金　額".
           02  F              PIC  X(007) VALUE "  ｺｰﾄﾞ ".
           02  F              PIC  N(007) VALUE "仕　入　先　名".
           02  F              PIC  X(022) VALUE SPACE.
       01  W-P.
           02  P-15K          PIC  X(005).
           02  P-JCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-JNA          PIC  N(024).
           02  P-SU           PIC --,---,--9.
           02  P-T            PIC ---,--9.99.
           02  P-KIN          PIC --,---,---,--9.
           02  F              PIC  X(002).
           02  P-SCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-SNA          PIC  N(024).
           02  P-20K          PIC  X(005).
       01  W-D.
           02  W-BKC          PIC  9(002).
           02  W-JCD          PIC  9(006).
           02  W-SCD          PIC  9(004).
           02  W-SCDD         PIC  9(004).
           02  W-SU           PIC S9(007).
           02  W-KIN          PIC S9(009).
           02  W-SSU          PIC S9(006).
           02  W-SKIN         PIC S9(009).
           02  W-ASU          PIC S9(006).
           02  W-AKIN         PIC S9(009).
           02  W-T            PIC S9(005)V9(02).
           02  W-PAGE         PIC  9(002).
           02  W-NG.
             03  F            PIC  9(002).
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
           02  CNT            PIC  9(001).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIKBNO.
           COPY LISM.
           COPY LIJM.
           COPY LSPF.
      *FD  SS-F
       01  SS-F_KBY310.
           02  SS-F_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SS-F_LNAME     PIC  X(011) VALUE "SS-F_KBY310".
           02  F              PIC  X(001).
           02  SS-F_KEY1      PIC  X(100) VALUE SPACE.
           02  SS-F_SORT      PIC  X(100) VALUE SPACE.
           02  SS-F_IDLST     PIC  X(100) VALUE SPACE.
           02  SS-F_RES       USAGE  POINTER.
       01  SS-R.
           02  SS-JCD         PIC  9(006).
           02  SS-SCD         PIC  9(004).
           02  SS-SU          PIC S9(006).
           02  SS-KIN         PIC S9(009).
           02  SS-SC          PIC  9(001).
           02  SS-SJCD        PIC  9(006).
           02  SS-NG          PIC  9(006).
           02  SS-BKC         PIC  9(002).
           02  SS-BKNO        PIC  9(002).
           02  F              PIC  X(022).
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
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　年間　製品仕入　明細表　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  KBNOM ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "294" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "42" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "185" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "185" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "75" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "75" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           COPY LIBCPR.
           MOVE DATE-05R TO H-DATE.
           CALL "DB_F_Open" USING
            "INPUT" KBNO-M_PNAME1 "SHARED" BY REFERENCE KBNO-M_IDLST "1"
            "BNO-KEY" BY REFERENCE BNO-KEY.
           MOVE SPACE TO BNO-KEY.
           MOVE "01" TO BNO-KEYD.
      *           READ KBNO-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KBNO-M_PNAME1 BY REFERENCE KBNO-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO BNO-SNG BNO-ENG
           END-IF
           MOVE BNO-SNG TO W-NG.
           MOVE W-NEN TO H-HN.
           MOVE W-GET TO H-HG.
           MOVE BNO-ENG TO W-NG.
           MOVE W-NEN TO H-ON.
           MOVE W-GET TO H-OG.
           CALL "DB_F_Close" USING
            BY REFERENCE KBNO-M_IDLST KBNO-M_PNAME1.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SS-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SS-F_PNAME1 " " BY REFERENCE SS-F_IDLST "0".
       M-10.
      *           READ SS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SS-F_PNAME1 BY REFERENCE SS-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SS-F_IDLST SS-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO W-ASU W-AKIN W-PAGE.
           PERFORM S-10 THRU S-15.
       M-15.
           MOVE ZERO TO W-SSU W-SKIN W-SCDD.
           MOVE SS-BKC TO W-BKC.
       M-20.
           MOVE ZERO TO W-SU W-KIN.
           MOVE SS-JCD TO J-KEY W-JCD.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊＊　マスター　なし　＊＊＊　　" TO J-NAME
           END-IF
           MOVE SS-SCD TO S-KEY W-SCD.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊＊　マスター　なし　＊＊＊　　" TO S-NAME
           END-IF
           MOVE ZERO TO CNT.
       M-25.
           PERFORM S-20 THRU S-35.
       M-30.
      *           READ SS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SS-F_PNAME1 BY REFERENCE SS-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SS-BKC NOT = W-BKC
               GO TO M-40
           END-IF
           IF  W-JCD NOT = SS-JCD
               GO TO M-35
           END-IF
           IF  W-SCD NOT = SS-SCD
               GO TO M-35
           END-IF
           GO TO M-25.
       M-35.
           PERFORM S-40 THRU S-50.
           GO TO M-20.
       M-40.
           PERFORM S-40 THRU S-50.
           PERFORM S-55 THRU S-60.
           GO TO M-15.
       M-90.
           PERFORM S-40 THRU S-50.
           PERFORM S-55 THRU S-60.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-JNA P-SNA.
           MOVE "　　　［　ＡＬＬ　ＴＯＴＡＬ　］　　　　" TO P-JNA.
           MOVE W-ASU TO P-SU.
           MOVE W-AKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SS-F_IDLST SS-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           IF  CNT NOT = ZERO
               GO TO S-25
           END-IF
           IF  SS-SJCD = ZERO
               GO TO S-25
           END-IF
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-JNA P-SNA.
           MOVE SS-JCD TO P-JCD.
           MOVE J-NAME TO P-JNA.
           IF  W-SCDD NOT = SS-SCD
               MOVE SS-SCD TO P-SCD W-SCDD
               MOVE S-NAME TO P-SNA
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE SS-SCD TO P-SCD
               MOVE S-NAME TO P-SNA
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           IF  CNT = 5
               MOVE 9 TO CNT
           END-IF
           IF  CNT = ZERO
               MOVE 5 TO CNT
           END-IF
           IF  SS-SJCD = ZERO
               GO TO S-30
           END-IF
           MOVE SS-SJCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊＊　マスター　なし　＊＊＊　　" TO J-NAME
           END-IF.
       S-30.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-JNA P-SNA.
           MOVE SS-JCD TO P-JCD.
           MOVE J-NAME TO P-JNA.
           IF  W-SCDD NOT = SS-SCD
               MOVE SS-SCD TO P-SCD W-SCDD
               MOVE S-NAME TO P-SNA
           END-IF
           MOVE SS-KIN TO P-KIN.
           ADD SS-KIN TO W-KIN.
           IF  SS-SU NOT = ZERO
               MOVE SS-SU TO P-SU
               ADD SS-SU TO W-SU
               MOVE ZERO TO W-T
               IF  SS-KIN = ZERO
                   MOVE W-T TO P-T
               ELSE
                   COMPUTE W-T = SS-KIN / SS-SU
                   MOVE W-T TO P-T
               END-IF
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               MOVE SS-SCD TO P-SCD
               MOVE S-NAME TO P-SNA
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-35.
           EXIT.
       S-40.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-JNA P-SNA.
           IF  CNT = 5
               GO TO S-45
           END-IF
           MOVE "　　　　　　　　　　＜　ＴＯＴＡＬ　＞　" TO P-JNA.
           IF (W-SU NOT = ZERO) AND (W-KIN NOT = ZERO)
               COMPUTE W-T = W-KIN / W-SU
               MOVE W-SU TO P-SU
               MOVE W-T TO P-T
           END-IF
           MOVE W-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               MOVE SS-SCD TO P-SCD
               MOVE S-NAME TO P-SNA
               PERFORM S-05 THRU S-10
           END-IF.
       S-45.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD W-SU TO W-SSU.
           ADD W-KIN TO W-SKIN.
       S-50.
           EXIT.
       S-55.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-JNA P-SNA.
           MOVE "　　　　　［　ＳＵＢ　ＴＯＴＡＬ　］　　" TO P-JNA.
           MOVE W-SSU TO P-SU.
           MOVE W-SKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD W-SSU TO W-ASU.
           ADD W-SKIN TO W-AKIN.
       S-60.
           EXIT.

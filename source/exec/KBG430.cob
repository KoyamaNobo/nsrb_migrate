       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG430.
      *****************************************************
      *    PROGRAM         :  部門材料別　仕入明細表      *
      *    PRINTER TYPE    :  JIPS                        *
      *    SCREEN          :  ******                      *
      *    COMPILE TYPE    :  COBOL                       *
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  N(021) VALUE
                "＊＊＊　　部門材料別　仕入明細表　　＊＊＊".
           02  F              PIC  X(012) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(006) VALUE "部　門　名　".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(002) VALUE "伝区".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "材　　料　　名　".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "単　　価".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
       01  W-P.
           02  P-BKC          PIC  9(002).
           02  F              PIC  X(001).
           02  P-BMN          PIC  N(010).
           02  F              PIC  X(002).
           02  P-DC           PIC  9(002).
           02  F              PIC  X(001).
           02  P-JCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-JNA          PIC  N(024).
           02  P-SU           PIC --,---,--9.99.
           02  P-T            PIC ----,--9.99.
           02  P-KIN          PIC ----,---,--9.
       01  W-DATA.
           02  W-PAGE         PIC  9(002).
           02  W-SBKC         PIC  9(002).
           02  W-EBKC         PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  W-BKC          PIC  9(002).
           02  W-DC           PIC  9(002).
           02  W-JCD1         PIC  9(001).
           02  W-JCD          PIC  9(006).
           02  W-D.
             03  W-SU         PIC S9(007)V9(02).
             03  W-T          PIC S9(006)V9(02).
             03  W-KIN        PIC S9(009).
           02  WN-KIN         PIC S9(009).
           02  WT-KIN         PIC S9(009).
           02  WS-KIN         PIC S9(009).
           02  WA-KIN         PIC S9(009).
           02  CNT            PIC  9(005).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIJM.
           COPY BUMONF.
           COPY LSJSSW.
           COPY LSPF.
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
                "＊＊＊　　部門材料別　仕入明細表　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(018) VALUE
                "部門ｺｰﾄﾞ  00 ～ 99".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SBKC  PIC  9(002).
             03  A-EBKC  PIC  9(002).
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
            "C-MID" " " "0" "0" "334" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING 
            "08C-MID" "X" "14" "22" "18" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "40" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "14" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SBKC" "9" "14" "32" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SBKC" BY REFERENCE W-SBKC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EBKC" "9" "14" "38" "2" "A-SBKC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EBKC" BY REFERENCE W-EBKC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "57" "1" "01C-ACP" " " RETURNING RESU.
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
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           MOVE 99 TO W-EBKC.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SBKC "A-SBKC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-EBKC "A-EBKC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-SBKC > W-EBKC
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
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JSSR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
       M-25.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JR-BKC > W-EBKC
               CALL "DB_F_Close" USING
                BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JR-BKC = ZERO
               GO TO M-25
           END-IF
           IF  JR-BKC < W-SBKC
               GO TO M-25
           END-IF
           IF  JR-DC = 30
               GO TO M-25
           END-IF
           IF  JR-JCD2 > 89
               GO TO M-25
           END-IF
      *
           COPY LIBCPR.
           MOVE DATE-05R TO H-DATE.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BNM_PNAME1 "SHARED" BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO WA-KIN.
       M-30.
           MOVE ZERO TO WS-KIN CHK.
           MOVE JR-BKC TO W-BKC.
           MOVE ZERO TO BNM-KEY.
           MOVE W-BKC TO BNM-BU.
      *           READ BNM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BNM_PNAME1 BY REFERENCE BNM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO BNMNMN
           END-IF.
       M-35.
           MOVE ZERO TO WT-KIN CHK2.
           MOVE JR-DC TO W-DC.
       M-37.
           MOVE ZERO TO WN-KIN.
           MOVE JR-JCD1 TO W-JCD1.
       M-40.
           MOVE ZERO TO W-D.
           MOVE JR-JCD TO W-JCD.
           MOVE W-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO J-NAME
               MOVE "　＊　材料なし　＊" TO J-NAME
           END-IF.
       M-45.
           IF  JR-DC2 NOT = 2 AND 3
               ADD JR-SU TO W-SU
           END-IF
           ADD JR-KIN TO W-KIN.
       M-50.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-65
           END-IF
           IF  JR-BKC > W-EBKC
               GO TO M-65
           END-IF
           IF  JR-BKC = ZERO
               GO TO M-50
           END-IF
           IF  JR-DC = 30
               GO TO M-50
           END-IF
           IF  JR-JCD2 > 89
               GO TO M-50
           END-IF
           IF  JR-BKC NOT = W-BKC
               GO TO M-60
           END-IF
           IF  JR-DC NOT = W-DC
               GO TO M-55
           END-IF
           IF  JR-JCD1 NOT = W-JCD1
               GO TO M-52
           END-IF
           IF  JR-JCD = W-JCD
               GO TO M-45
           END-IF
           PERFORM S-20 THRU S-25.
           GO TO M-40.
       M-52.
           PERFORM S-20 THRU S-25.
           PERFORM S-26 THRU S-27.
           GO TO M-37.
       M-55.
           PERFORM S-20 THRU S-25.
           PERFORM S-26 THRU S-27.
           PERFORM S-30 THRU S-40.
           GO TO M-35.
       M-60.
           PERFORM S-20 THRU S-25.
           PERFORM S-26 THRU S-27.
           PERFORM S-30 THRU S-40.
           PERFORM S-45 THRU S-50.
           GO TO M-30.
       M-65.
           PERFORM S-20 THRU S-25.
           PERFORM S-26 THRU S-27.
           PERFORM S-30 THRU S-40.
           PERFORM S-45 THRU S-50.
           PERFORM S-55 THRU S-60.
       M-90.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
       M-95.
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
           IF  ZERO = W-SU AND W-T AND W-KIN
               GO TO S-25
           END-IF
           IF  W-KIN NOT = ZERO
               IF  W-SU NOT = ZERO
                   COMPUTE W-T ROUNDED = W-KIN / W-SU
               END-IF
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-BMN P-JNA.
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               MOVE W-BKC TO P-BKC
               MOVE BNMNMN TO P-BMN
           END-IF
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               MOVE W-DC TO P-DC
           END-IF
           MOVE W-JCD TO P-JCD.
           MOVE J-NAME TO P-JNA.
           MOVE W-SU TO P-SU.
           MOVE W-T TO P-T.
           MOVE W-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-BKC TO P-BKC
               MOVE BNMNMN TO P-BMN
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD W-KIN TO WN-KIN.
           ADD 1 TO CNT.
       S-25.
           EXIT.
       S-26.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-BMN P-JNA.
           MOVE "　　　　　　　　　　　　　　　｛　計　｝" TO P-JNA.
           MOVE WN-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-BKC TO P-BKC
               MOVE BNMNMN TO P-BMN
               MOVE W-DC TO P-DC
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WN-KIN TO WT-KIN.
       S-27.
           EXIT.
       S-30.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-BMN P-JNA.
           MOVE "　　　　　　　　　　　（　小計　）　" TO P-JNA.
           MOVE WT-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-BKC TO P-BKC
               MOVE BNMNMN TO P-BMN
               MOVE W-DC TO P-DC
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WT-KIN TO WS-KIN.
       S-40.
           EXIT.
       S-45.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-BMN P-JNA.
           MOVE "　　　　　　［　合　計　］　" TO P-JNA.
           MOVE WS-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-BKC TO P-BKC
               MOVE BNMNMN TO P-BMN
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-KIN TO WA-KIN.
       S-50.
           EXIT.
       S-55.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-BMN P-JNA.
           MOVE "　【　　総　合　計　　】　" TO P-JNA.
           MOVE WA-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-60.
           EXIT.

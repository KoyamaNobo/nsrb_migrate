       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG150.
      *********************************************************
      *    PROGRAM         :  購買品目区分別集計表１　　　    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/09                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(039) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　材料品目区分別　仕入集計表１　　＊＊＊".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(044) VALUE
                " 日　付     　原　反   　型　底 クラリーノ  ".
           02  F              PIC  X(044) VALUE
                "   中　底 糸･紐･ﾃｰﾌﾟ         糊　   馳･鳩目 ".
           02  F              PIC  X(043) VALUE
                "  ｼｰﾙ･ﾈｰﾑ     紙　函     ケース    　合　計".
       01  W-P.
           02  P-DATE         PIC 99/99/99B.
           02  P-DATED REDEFINES P-DATE  PIC  X(009).
           02  P-GT           PIC ---,---,--9.
           02  P-KZ           PIC ---,---,--9.
           02  P-KR           PIC ---,---,--9.
           02  P-NZ           PIC ---,---,--9.
           02  P-IH           PIC ---,---,--9.
           02  P-NR           PIC ---,---,--9.
           02  P-HH           PIC ---,---,--9.
           02  P-SN           PIC ---,---,--9.
           02  P-KB           PIC ---,---,--9.
           02  P-KS           PIC ---,---,--9.
           02  P-TOTAL        PIC ----,---,--9.
       01  W-D.
           02  W-DATED        PIC  9(006).
           02  W-DATE  REDEFINES W-DATED.
             03  W-NG         PIC  9(004).
             03  W-H          PIC  9(002).
           02  WN-D.
             03  W-GT         PIC S9(008).
             03  W-KZ         PIC S9(008).
             03  W-KR         PIC S9(008).
             03  W-NZ         PIC S9(008).
             03  W-IH         PIC S9(008).
             03  W-NR         PIC S9(008).
             03  W-HH         PIC S9(008).
             03  W-SN         PIC S9(008).
             03  W-KB         PIC S9(008).
             03  W-KS         PIC S9(008).
             03  W-TOTAL      PIC S9(009).
       01  WS-D.
           02  WS-GT          PIC S9(008).
           02  WS-KZ          PIC S9(008).
           02  WS-KR          PIC S9(008).
           02  WS-NZ          PIC S9(008).
           02  WS-IH          PIC S9(008).
           02  WS-NR          PIC S9(008).
           02  WS-HH          PIC S9(008).
           02  WS-SN          PIC S9(008).
           02  WS-KB          PIC S9(008).
           02  WS-KS          PIC S9(008).
           02  WS-TOTAL       PIC S9(009).
       01  WA-D.
           02  WA-GT          PIC S9(008).
           02  WA-KZ          PIC S9(008).
           02  WA-KR          PIC S9(008).
           02  WA-NZ          PIC S9(008).
           02  WA-IH          PIC S9(008).
           02  WA-NR          PIC S9(008).
           02  WA-HH          PIC S9(008).
           02  WA-SN          PIC S9(008).
           02  WA-KB          PIC S9(008).
           02  WA-KS          PIC S9(008).
           02  WA-TOTAL       PIC S9(009).
       01  W-CNT              PIC  9(001).
       01  W-CNT1             PIC  9(001).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　品目区分別　仕入集計表１　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
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
            "C-MID" " " "0" "0" "308" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
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
       M-00.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JSSR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
           CALL "PR_Open" RETURNING RESP.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-05.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JR-DC1 = 3
               GO TO M-05
           END-IF
           IF  JR-JCD1 NOT = 0 AND 1 AND 5 AND 6
               GO TO M-05
           END-IF
           IF  JR-JCD2 > 59
               GO TO M-05
           END-IF
           MOVE DATE-05R TO H-DATE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO WA-D.
       M-10.
           MOVE ZERO TO WS-D.
           IF  JR-PEY < 11
               MOVE 1 TO W-CNT
               GO TO M-15
           END-IF
           IF  JR-PEY < 21
               MOVE 2 TO W-CNT
               GO TO M-15
           END-IF
           MOVE 3 TO W-CNT.
       M-15.
           MOVE ZERO TO W-D.
           MOVE JR-NGPS TO W-DATE.
       M-20.
           PERFORM S-05 THRU S-15.
       M-25.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JR-DC1 = 3
               GO TO M-25
           END-IF
           IF  JR-JCD1 NOT = 0 AND 1 AND 5 AND 6
               GO TO M-25
           END-IF
           IF  JR-JCD2 > 59
               GO TO M-25
           END-IF
           IF  W-DATE = JR-NGPS
               GO TO M-20
           END-IF
           PERFORM S-30 THRU S-35.
           IF  JR-PEY < 11
               MOVE 1 TO W-CNT1
               GO TO M-30
           END-IF
           IF  JR-PEY < 21
               MOVE 2 TO W-CNT1
               GO TO M-30
           END-IF
           MOVE 3 TO W-CNT1.
       M-30.
           IF  W-CNT = W-CNT1
               GO TO M-15
           END-IF
           PERFORM S-40 THRU S-45.
           GO TO M-10.
       M-90.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
           MOVE SPACE TO SP-R W-P.
           MOVE "  合 計  " TO P-DATED.
           MOVE ZERO TO WN-D.
           MOVE WA-D TO WN-D.
           PERFORM S-50 THRU S-55.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           IF  JR-JCD2 < 05
               ADD JR-KIN TO W-GT
               GO TO S-10
           END-IF
           IF  JR-JCD2 < 10
               ADD JR-KIN TO W-KZ
               GO TO S-10
           END-IF
           IF  JR-JCD2 < 15
               ADD JR-KIN TO W-KR
               GO TO S-10
           END-IF
           IF  JR-JCD2 < 20
               ADD JR-KIN TO W-NZ
               GO TO S-10
           END-IF
           IF  JR-JCD2 < 25
               ADD JR-KIN TO W-IH
               GO TO S-10
           END-IF
           IF  JR-JCD2 < 30
               ADD JR-KIN TO W-NR
               GO TO S-10
           END-IF
           IF  JR-JCD2 < 35
               ADD JR-KIN TO W-HH
               GO TO S-10
           END-IF
           IF  JR-JCD2 < 40
               ADD JR-KIN TO W-SN
               GO TO S-10
           END-IF
           IF  JR-JCD2 < 45
               ADD JR-KIN TO W-KB
               GO TO S-10
           END-IF
           IF  JR-JCD2 < 60
               ADD JR-KIN TO W-KS
           END-IF.
       S-10.
           ADD JR-KIN TO W-TOTAL.
       S-15.
           EXIT.
       S-30.
           MOVE SPACE TO SP-R W-P.
           MOVE W-DATED TO P-DATE.
           PERFORM S-50 THRU S-55.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-GT TO WS-GT.
           ADD W-KZ TO WS-KZ.
           ADD W-KR TO WS-KR.
           ADD W-NZ TO WS-NZ.
           ADD W-IH TO WS-IH.
           ADD W-NR TO WS-NR.
           ADD W-HH TO WS-HH.
           ADD W-SN TO WS-SN.
           ADD W-KB TO WS-KB.
           ADD W-KS TO WS-KS.
           ADD W-TOTAL TO WS-TOTAL.
       S-35.
           EXIT.
       S-40.
           MOVE SPACE TO SP-R W-P.
           MOVE "  小 計  " TO P-DATED.
           MOVE ZERO TO WN-D.
           MOVE WS-D TO WN-D.
           PERFORM S-50 THRU S-55.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WS-GT TO WA-GT.
           ADD WS-KZ TO WA-KZ.
           ADD WS-KR TO WA-KR.
           ADD WS-NZ TO WA-NZ.
           ADD WS-IH TO WA-IH.
           ADD WS-NR TO WA-NR.
           ADD WS-HH TO WA-HH.
           ADD WS-SN TO WA-SN.
           ADD WS-KB TO WA-KB.
           ADD WS-KS TO WA-KS.
           ADD WS-TOTAL TO WA-TOTAL.
       S-45.
           EXIT.
       S-50.
           MOVE W-GT TO P-GT.
           MOVE W-KZ TO P-KZ.
           MOVE W-KR TO P-KR.
           MOVE W-NZ TO P-NZ.
           MOVE W-IH TO P-IH.
           MOVE W-NR TO P-NR.
           MOVE W-HH TO P-HH.
           MOVE W-SN TO P-SN.
           MOVE W-KB TO P-KB.
           MOVE W-KS TO P-KS.
           MOVE W-TOTAL TO P-TOTAL.
           MOVE W-P TO SP-R.
       S-55.
           EXIT.

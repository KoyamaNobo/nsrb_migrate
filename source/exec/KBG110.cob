       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG110.
      *********************************************************
      *    PROGRAM         :  材料区分集計表　　　　　　　    *
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
       77  W-PAGE             PIC  9(002) VALUE ZERO.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(033) VALUE SPACE.
           02  F              PIC  N(021) VALUE
                "＊＊＊　　材料区分別　仕入集計表　　＊＊＊".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(048) VALUE
                ":          原　　材　　料　           :         ".
           02  F              PIC  X(050) VALUE
                "補　　助　　材　　料      :　　荷　造　材　料　　:".
       01  HEAD3.
           02  F              PIC  X(034) VALUE
                " 日　付  :      履　物　　　工　品".
           02  F              PIC  X(042) VALUE
                "      素　材 :      履　物   　工　品     ".
           02  F              PIC  X(044) VALUE
                "素　材 :   　履　物  　工　品 :     　合　計".
       01  W-P.
           02  P-DATE         PIC 99/99/99B.
           02  P-DATED REDEFINES P-DATE  PIC  X(009).
           02  P-X1           PIC  X(001).
           02  P-GH           PIC ----,---,--9.
           02  P-GK           PIC ----,---,--9.
           02  P-GS           PIC ----,---,--9.
           02  F              PIC  X(001).
           02  P-X2           PIC  X(001).
           02  P-HH           PIC ----,---,--9.
           02  P-HK           PIC ---,---,--9.
           02  P-HS           PIC ---,---,--9.
           02  F              PIC  X(001).
           02  P-X3           PIC  X(001).
           02  P-NH           PIC ---,---,--9.
           02  P-NK           PIC --,---,--9.
           02  F              PIC  X(001).
           02  P-X4           PIC  X(001).
           02  P-TOTAL        PIC -----,---,--9.
       01  W-D.
           02  W-DATED        PIC  9(006).
           02  W-DATE  REDEFINES W-DATED.
             03  W-NG         PIC  9(004).
             03  W-H          PIC  9(002).
           02  W-GH           PIC S9(009).
           02  W-GK           PIC S9(009).
           02  W-GS           PIC S9(009).
           02  W-HH           PIC S9(009).
           02  W-HK           PIC S9(008).
           02  W-HS           PIC S9(008).
           02  W-NH           PIC S9(008).
           02  W-NK           PIC S9(008).
           02  W-TOTAL        PIC S9(009).
       01  WS-D.
           02  WS-GH          PIC S9(009).
           02  WS-GK          PIC S9(009).
           02  WS-GS          PIC S9(009).
           02  WS-HH          PIC S9(009).
           02  WS-HK          PIC S9(008).
           02  WS-HS          PIC S9(008).
           02  WS-NH          PIC S9(008).
           02  WS-NK          PIC S9(008).
           02  WS-TOTAL       PIC S9(009).
       01  WA-D.
           02  WA-GH          PIC S9(009).
           02  WA-GK          PIC S9(009).
           02  WA-GS          PIC S9(009).
           02  WA-HH          PIC S9(009).
           02  WA-HK          PIC S9(008).
           02  WA-HS          PIC S9(008).
           02  WA-NH          PIC S9(008).
           02  WA-NK          PIC S9(008).
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
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　材料区分別　仕入集計表　　＊＊＊".
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
       M-10.
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
               GO TO M-10
           END-IF
           IF  JR-YC = 4
               GO TO M-10
           END-IF
      *
           MOVE DATE-05R TO H-DATE.
           MOVE SPACE TO SP-R.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO WA-D.
       M-15.
           MOVE ZERO TO WS-D.
       M-20.
           IF  JR-PEY < 11
               MOVE 1 TO W-CNT
           END-IF
           IF  JR-PEY > 10 AND < 21
               MOVE 2 TO W-CNT
           END-IF
           IF  JR-PEY > 20
               MOVE 3 TO W-CNT
           END-IF.
       M-25.
           MOVE ZERO TO W-D.
           MOVE JR-NGPS TO W-DATE.
       M-30.
           PERFORM S-05 THRU S-35.
       M-35.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JR-DC1 = 3
               GO TO M-35
           END-IF
           IF  JR-YC = 4
               GO TO M-35
           END-IF
           IF  W-DATE = JR-NGPS
               GO TO M-30
           END-IF.
       M-40.
           PERFORM S-40 THRU S-45.
       M-45.
           IF  JR-PEY < 11
               MOVE 1 TO W-CNT1
           END-IF
           IF  JR-PEY > 10 AND < 21
               MOVE 2 TO W-CNT1
           END-IF
           IF  JR-PEY > 20
               MOVE 3 TO W-CNT1
           END-IF.
       M-50.
           IF  W-CNT = W-CNT1
               GO TO M-25
           END-IF
           PERFORM S-50 THRU S-55.
           MOVE W-CNT1 TO W-CNT.
           MOVE ZERO TO WS-D.
           GO TO M-25.
       M-90.
           PERFORM S-40 THRU S-45.
           PERFORM S-50 THRU S-55.
           MOVE SPACE TO SP-R W-P.
           MOVE ":" TO P-X1 P-X2 P-X3 P-X4.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE "  合 計  " TO P-DATED.
           MOVE WA-GH TO P-GH.
           MOVE WA-GK TO P-GK.
           MOVE WA-GS TO P-GS.
           MOVE WA-HH TO P-HH.
           MOVE WA-HK TO P-HK.
           MOVE WA-HS TO P-HS.
           MOVE WA-NH TO P-NH.
           MOVE WA-NK TO P-NK.
           MOVE WA-TOTAL TO P-TOTAL.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           IF  JR-YC NOT = 1
               GO TO S-20
           END-IF
           IF  JR-JCD1 NOT = 0 AND 1 AND 5 AND 6
               GO TO S-15
           END-IF
           IF  JR-JCD1 = 0 OR 6
               GO TO S-10
           END-IF
           IF  JR-JCD2 < 60 OR > 80
               GO TO S-10
           END-IF
           ADD JR-KIN TO W-GS.
           GO TO S-30.
       S-10.
           ADD JR-KIN TO W-GH.
           GO TO S-30.
       S-15.
           IF  JR-JCD1 = 2
               ADD JR-KIN TO W-GS
               GO TO S-30
           END-IF
           IF  JR-JCD1 < 6
               ADD JR-KIN TO W-GK
               GO TO S-30
           END-IF
           IF  JR-JCD1 < 7
               GO TO S-30
           END-IF.
       S-20.
           IF  JR-YC NOT = 2
               GO TO S-25
           END-IF
           IF  JR-JCD1 = 0 OR 1 OR 5 OR 6
               ADD JR-KIN TO W-HH
               GO TO S-30
           END-IF
           IF  JR-JCD1 = 2
               ADD JR-KIN TO W-HS
               GO TO S-30
           END-IF
           IF  JR-JCD1 < 6
               ADD JR-KIN TO W-HK
               GO TO S-30
           END-IF
           IF  JR-JCD1 < 7
               GO TO S-30
           END-IF.
       S-25.
           IF  JR-YC NOT = 3
               GO TO S-30
           END-IF
           IF  JR-JCD1 = 0 OR 1 OR 5 OR 6
               ADD JR-KIN TO W-NH
               GO TO S-30
           END-IF
           IF  JR-JCD1 = 2
               GO TO S-30
           END-IF
           IF  JR-JCD1 < 6
               ADD JR-KIN TO W-NK
           END-IF.
       S-30.
           ADD JR-KIN TO W-TOTAL.
       S-35.
           EXIT.
       S-40.
           MOVE SPACE TO SP-R W-P.
           MOVE ":" TO P-X1 P-X2 P-X3 P-X4.
           MOVE W-DATED TO P-DATE.
           MOVE W-GH TO P-GH.
           MOVE W-GK TO P-GK.
           MOVE W-GS TO P-GS.
           MOVE W-HH TO P-HH.
           MOVE W-HK TO P-HK.
           MOVE W-HS TO P-HS.
           MOVE W-NH TO P-NH.
           MOVE W-NK TO P-NK.
           MOVE W-TOTAL TO P-TOTAL.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-GH TO WS-GH.
           ADD W-GK TO WS-GK.
           ADD W-GS TO WS-GS.
           ADD W-HH TO WS-HH.
           ADD W-HK TO WS-HK.
           ADD W-HS TO WS-HS.
           ADD W-NH TO WS-NH.
           ADD W-NK TO WS-NK.
           ADD W-TOTAL TO WS-TOTAL.
       S-45.
           EXIT.
       S-50.
           MOVE SPACE TO SP-R W-P.
           MOVE ":" TO P-X1 P-X2 P-X3 P-X4.
           MOVE "  小 計  " TO P-DATED.
           MOVE WS-GH TO P-GH.
           MOVE WS-GK TO P-GK.
           MOVE WS-GS TO P-GS.
           MOVE WS-HH TO P-HH.
           MOVE WS-HK TO P-HK.
           MOVE WS-HS TO P-HS.
           MOVE WS-NH TO P-NH.
           MOVE WS-NK TO P-NK.
           MOVE WS-TOTAL TO P-TOTAL.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
           MOVE ":" TO P-X1 P-X2 P-X3 P-X4.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD WS-GH TO WA-GH.
           ADD WS-GK TO WA-GK.
           ADD WS-GS TO WA-GS.
           ADD WS-HH TO WA-HH.
           ADD WS-HK TO WA-HK.
           ADD WS-HS TO WA-HS.
           ADD WS-NH TO WA-NH.
           ADD WS-NK TO WA-NK.
           ADD WS-TOTAL TO WA-TOTAL.
       S-55.
           EXIT.

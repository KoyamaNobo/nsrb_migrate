       IDENTIFICATION DIVISION.
       PROGRAM-ID.  KHG250.
      *********************************************************
      *    PROGRAM         :  工品預り受払表                  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/06                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-PAGE             PIC  9(002) VALUE ZERO.
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  N(019) VALUE
                "＊＊＊　　工品　預り　受払表　　＊＊＊".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(039) VALUE SPACE.
           02  F              PIC  X(044) VALUE
                "I----  前月繰越  ---I I----  当月預り  --ｰI ".
           02  F              PIC  X(043) VALUE
                "I----  当月出荷　---I I----  翌月繰越　---I".
       01  HEAD3.
           02  F              PIC  X(038) VALUE
                " ｺｰﾄﾞ  品　　　名　　         単　　価".
           02  F              PIC  X(044) VALUE
                "    数　量      金　額    数　量   　 金　額".
           02  F              PIC  X(044) VALUE
                "    数　量　　  金　額　  数　量   　 金　額".
       01  W-PR.
           02  P-HCD          PIC  X(005).
           02  F              PIC  X(002).
           02  P-HNA          PIC  X(020).
           02  P-T            PIC ----,--9.99.
           02  P-ZAS          PIC ------,--9.
           02  P-ZAK          PIC ----,---,--9.
           02  P-AUS          PIC ------,--9.
           02  P-AUK          PIC ----,---,--9.
           02  P-AHS          PIC ------,--9.
           02  P-AHK          PIC ----,---,--9.
           02  P-ASU          PIC ------,--9.
           02  P-AKI          PIC ----,---,--9.
       01  W-D.
           02  W-YC           PIC  9(002).
           02  W-ZAK          PIC S9(009).
           02  W-AUK          PIC S9(009).
           02  W-AHK          PIC S9(009).
           02  W-AKI          PIC S9(009).
       01  WS-D.
           02  WS-ZAS         PIC S9(006).
           02  WS-ZAK         PIC S9(009).
           02  WS-AUS         PIC S9(006).
           02  WS-AUK         PIC S9(009).
           02  WS-AHS         PIC S9(006).
           02  WS-AHK         PIC S9(009).
           02  WS-ASU         PIC S9(006).
           02  WS-AKI         PIC S9(009).
       01  WA-D.
           02  WA-ZAS         PIC S9(006).
           02  WA-ZAK         PIC S9(009).
           02  WA-AUS         PIC S9(006).
           02  WA-AUK         PIC S9(009).
           02  WA-AHS         PIC S9(006).
           02  WA-AHK         PIC S9(009).
           02  WA-ASU         PIC S9(006).
           02  WA-AKI         PIC S9(009).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIKHM.
           COPY LIKHT2.
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　工品預り受払表　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(016) VALUE
                  "***  KHM ﾅｼ  ***".
             03  E-KEY   PIC  X(005).
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "238" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "34" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "34" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "34" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "34" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "34" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "34" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "34" "06C-MID" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "48" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "48" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "10" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "10" "16" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "35" "5" "E-ME2" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE KHT-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "15" "5" "E-KEY" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "15" "5" "E-ME98" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" KHT-M_PNAME1 "SHARED" BY REFERENCE KHT-M_IDLST "1"
            "KHT-KEYD" BY REFERENCE KHT-KEYD.
       M-10.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KHT-M_IDLST KHT-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  KHT-YC = 10 OR 11
               GO TO M-10
           END-IF
           IF  ZERO = KHT-AZS AND KHT-AAS AND KHT-AUS AND KHT-ASS
               GO TO M-10
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-03R TO H-DATE.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO WA-D.
       M-15.
           MOVE ZERO TO WS-D.
           MOVE KHT-YC TO W-YC.
       M-20.
           MOVE KHT-KEY TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *
           COMPUTE W-ZAK = KHT-AZS * KH-T1.
           COMPUTE W-AKI = KHT-AAS * KH-T1.
           COMPUTE W-AUK = KHT-AUS * KH-T1.
           COMPUTE W-AHK = KHT-ASS * KH-T1.
      *
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R W-PR.
           MOVE KHT-KEY TO P-HCD.
           MOVE KH-NAME TO P-HNA.
           MOVE KH-T1 TO P-T.
           MOVE KHT-AZS TO P-ZAS.
           MOVE W-ZAK TO P-ZAK.
           MOVE KHT-AUS TO P-AUS.
           MOVE W-AUK TO P-AUK.
           MOVE KHT-ASS TO P-AHS.
           MOVE W-AHK TO P-AHK.
           MOVE KHT-AAS TO P-ASU.
           MOVE W-AKI TO P-AKI.
           MOVE W-PR TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-PR.
      *
           ADD KHT-AZS TO WS-ZAS.
           ADD W-ZAK TO WS-ZAK.
           ADD KHT-AUS TO WS-AUS.
           ADD W-AUK TO WS-AUK.
           ADD KHT-ASS TO WS-AHS.
           ADD W-AHK TO WS-AHK.
           ADD KHT-AAS TO WS-ASU.
           ADD W-AKI TO WS-AKI.
       M-25.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  KHT-YC = 10 OR 11
               GO TO M-25
           END-IF
           IF  ZERO = KHT-AZS AND KHT-AAS AND KHT-AUS AND KHT-ASS
               GO TO M-25
           END-IF
           IF  W-YC = KHT-YC
               GO TO M-20
           END-IF
           PERFORM S-20 THRU S-25.
           GO TO M-15.
       M-90.
           PERFORM S-20 THRU S-25.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R W-PR.
           MOVE "   [  ALL TOTAL  ]  " TO P-HNA.
           MOVE WA-ZAS TO P-ZAS.
           MOVE WA-ZAK TO P-ZAK.
           MOVE WA-AUS TO P-AUS.
           MOVE WA-AUK TO P-AUK.
           MOVE WA-AHS TO P-AHS.
           MOVE WA-AHK TO P-AHK.
           MOVE WA-ASU TO P-ASU.
           MOVE WA-AKI TO P-AKI.
           MOVE W-PR TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KHT-M_IDLST KHT-M_PNAME1.
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
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R W-PR.
           MOVE "   <  SUB TOTAL  >  " TO P-HNA.
           MOVE WS-ZAS TO P-ZAS.
           MOVE WS-ZAK TO P-ZAK.
           MOVE WS-AUS TO P-AUS.
           MOVE WS-AUK TO P-AUK.
           MOVE WS-AHS TO P-AHS.
           MOVE WS-AHK TO P-AHK.
           MOVE WS-ASU TO P-ASU.
           MOVE WS-AKI TO P-AKI.
           MOVE W-PR TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO W-PR.
      *
           ADD WS-ZAS TO WA-ZAS.
           ADD WS-ZAK TO WA-ZAK.
           ADD WS-AUS TO WA-AUS.
           ADD WS-AUK TO WA-AUK.
           ADD WS-AHS TO WA-AHS.
           ADD WS-AHK TO WA-AHK.
           ADD WS-ASU TO WA-ASU.
           ADD WS-AKI TO WA-AKI.
       S-25.
           EXIT.

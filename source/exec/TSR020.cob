       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSR020.
      ****************************************
      *****     領収書チェックリスト     *****
      ****************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-15K            PIC  X(005) VALUE X"1A24212078".
       01  HEAD1.
           02  W-20K        PIC  X(005) VALUE X"1A24212474".
           02  F            PIC  X(040) VALUE SPACE.
           02  F            PIC  N(021) VALUE
                "＊＊＊　　領収書　チェックリスト　　＊＊＊".
           02  F            PIC  X(031) VALUE SPACE.
           02  F            PIC  X(005) VALUE "DATE ".
           02  H-DATE       PIC 99B99B99.
           02  F            PIC  X(007) VALUE "     P.".
           02  H-PAGE       PIC Z9.
       01  HEAD2.
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(003) VALUE "日　付".
           02  F            PIC  X(007) VALUE "  ｺｰﾄﾞ ".
           02  F            PIC  N(007) VALUE "取　引　先　名".
           02  F            PIC  X(012) VALUE SPACE.
           02  F            PIC  N(001) VALUE "№".
           02  F            PIC  X(006) VALUE SPACE.
           02  F            PIC  N(003) VALUE "金　額".
           02  F            PIC  X(004) VALUE SPACE.
           02  F            PIC  N(002) VALUE "印紙".
           02  F            PIC  X(009) VALUE "     :   ".
           02  F            PIC  N(003) VALUE "日　付".
           02  F            PIC  X(007) VALUE "  ｺｰﾄﾞ ".
           02  F            PIC  N(007) VALUE "取　引　先　名".
           02  F            PIC  X(012) VALUE SPACE.
           02  F            PIC  N(001) VALUE "№".
           02  F            PIC  X(006) VALUE SPACE.
           02  F            PIC  N(003) VALUE "金　額".
           02  F            PIC  X(004) VALUE SPACE.
           02  F            PIC  N(002) VALUE "印紙".
           02  F            PIC  X(003) VALUE SPACE.
       01  W-P.
           02  W-PD    OCCURS  58.
             03  P-15K1     PIC  X(005).
             03  P-DATE1    PIC 99/99/99.
             03  F          PIC  X(001).
             03  P-TCD1     PIC  9(004).
             03  F          PIC  X(001).
             03  P-NAME1    PIC  N(016).
             03  F          PIC  X(001).
             03  P-SEQ1     PIC ZZ9.
             03  P-20K1     PIC  X(005).
             03  P-KIN1     PIC ----,---,--9.
             03  P-INS1     PIC ----,--9.
             03  P-SS1   REDEFINES P-INS1.
               04  F        PIC  X(002).
               04  P-SOS1   PIC  N(003).
             03  F          PIC  X(001).
             03  P-SHK1     PIC  N(001).
             03  F          PIC  X(002).
             03  P-X        PIC  X(001).
             03  F          PIC  X(002).
             03  P-15K2     PIC  X(005).
             03  P-DATE2    PIC 99/99/99.
             03  F          PIC  X(001).
             03  P-TCD2     PIC  9(004).
             03  F          PIC  X(001).
             03  P-NAME2    PIC  N(016).
             03  F          PIC  X(001).
             03  P-SEQ2     PIC ZZ9.
             03  P-20K2     PIC  X(005).
             03  P-KIN2     PIC ----,---,--9.
             03  P-INS2     PIC ----,--9.
             03  P-SS2   REDEFINES P-INS2.
               04  F        PIC  X(002).
               04  P-SOS2   PIC  N(003).
             03  F          PIC  X(001).
             03  P-SHK2     PIC  N(001).
       01  W-D.
           02  W-KEY.
             03  W-DATE     PIC  9(006).
             03  W-TCD      PIC  9(004).
           02  W-GKIN       PIC S9(009).
           02  W-GC         PIC  9(001).
           02  CHK          PIC  9(001).
           02  W-PAGE       PIC  9(002) VALUE ZERO.
           02  W-NAME       PIC  N(016).
           02  W-PC         PIC  9(001).
           02  W-LD         PIC  9(002).
           02  W-CD         PIC  9(001).
       01  ERR-STAT         PIC  X(002).
      *
           COPY LITM.
           COPY LSPF.
      *FD  RS-F
       01  RS-F_TSR020.
           02  RS-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F            PIC  X(001).
           02  RS-F_LNAME   PIC  X(011) VALUE "RS-F_TSR020".
           02  F            PIC  X(001).
           02  RS-F_KEY1    PIC  X(100) VALUE SPACE.
           02  RS-F_SORT    PIC  X(100) VALUE SPACE.
           02  RS-F_IDLST   PIC  X(100) VALUE SPACE.
           02  RS-F_RES     USAGE  POINTER.
       01  RS-R.
           02  F            PIC  X(012).
           02  RS-KEY.
             03  RS-DATE    PIC  9(006).
             03  RS-TCD     PIC  9(004).
           02  RS-KIN       PIC S9(009).
           02  RS-INS       PIC  9(005).
           02  RS-SOC       PIC  9(001).
           02  RS-SHC       PIC  9(001).
           02  F            PIC  X(023).
           02  RS-SEQ       PIC  9(003).
       77  F                PIC  X(001).
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
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　領収書　チェックリスト　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(021) VALUE
                  "***  PROGRAM ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
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
            "C-ERR" " " "0" "0" "33" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "33" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "21" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO RS-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" RS-F_PNAME1 " " BY REFERENCE RS-F_IDLST "0".
       M-10.
      *           READ RS-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" RS-F_PNAME1 BY REFERENCE RS-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE RS-F_IDLST RS-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           PERFORM S-20 THRU S-30.
           MOVE ZERO TO W-PC W-LD W-CD.
       M-15.
           MOVE RS-KEY TO W-KEY.
           MOVE RS-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊＊　ＴＭ　なし　＊＊＊　　" TO T-TNA
           END-IF
           IF  T-TNA = SPACE
               MOVE T-NAME TO W-NAME
           ELSE
               MOVE T-TNA TO W-NAME
           END-IF
           MOVE ZERO TO W-GKIN W-GC CHK.
       M-20.
           PERFORM S-35 THRU S-40.
           IF  W-GC = 0
               MOVE 5 TO W-GC
               IF  W-CD = 0
                   MOVE RS-DATE TO P-DATE1(W-LD)
                   MOVE RS-TCD TO P-TCD1(W-LD)
                   MOVE W-NAME TO P-NAME1(W-LD)
               ELSE
                   MOVE RS-DATE TO P-DATE2(W-LD)
                   MOVE RS-TCD TO P-TCD2(W-LD)
                   MOVE W-NAME TO P-NAME2(W-LD)
               END-IF
           END-IF
           IF  CHK = 5
               MOVE 9 TO CHK
           END-IF
           IF  CHK = 0
               MOVE 5 TO CHK
           END-IF
           ADD RS-KIN TO W-GKIN.
           IF  W-CD NOT = 0
               GO TO M-25
           END-IF
           MOVE W-15K TO P-15K1(W-LD) P-15K2(W-LD).
           MOVE W-20K TO P-20K1(W-LD) P-20K2(W-LD).
           MOVE RS-SEQ TO P-SEQ1(W-LD).
           MOVE RS-KIN TO P-KIN1(W-LD).
           IF  RS-SOC = 0
               MOVE RS-INS TO P-INS1(W-LD)
           END-IF
           IF  RS-SOC = 5
               MOVE "相　殺" TO P-SOS1(W-LD)
           END-IF
           IF  RS-SOC = 8
               MOVE "消費税" TO P-SOS1(W-LD)
           END-IF
           IF  RS-SHC = 5
               MOVE "再" TO P-SHK1(W-LD)
           END-IF
           MOVE ":" TO P-X(W-LD).
           GO TO M-30.
       M-25.
           MOVE RS-SEQ TO P-SEQ2(W-LD).
           MOVE RS-KIN TO P-KIN2(W-LD).
           IF  RS-SOC = 0
               MOVE RS-INS TO P-INS2(W-LD)
           END-IF
           IF  RS-SOC = 5
               MOVE "相　殺" TO P-SOS2(W-LD)
           END-IF
           IF  RS-SOC = 8
               MOVE "消費税" TO P-SOS2(W-LD)
           END-IF
           IF  RS-SHC = 5
               MOVE "再" TO P-SHK2(W-LD)
           END-IF.
       M-30.
      *           READ RS-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" RS-F_PNAME1 BY REFERENCE RS-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  RS-KEY = W-KEY
               GO TO M-20
           END-IF
           PERFORM S-45 THRU S-55.
           GO TO M-15.
       M-90.
           PERFORM S-45 THRU S-55.
           PERFORM S-60 THRU S-70.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE RS-F_IDLST RS-F_PNAME1.
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
           MOVE SPACE TO W-P.
           MOVE ZERO TO W-LD.
       S-25.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               MOVE SPACE TO P-NAME1(W-LD) P-NAME2(W-LD)
                             P-SHK1(W-LD) P-SHK2(W-LD)
               GO TO S-25
           END-IF.
       S-30.
           EXIT.
       S-35.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               GO TO S-40
           END-IF
           MOVE 0 TO W-GC.
           IF  W-CD = 0
               MOVE 5 TO W-CD
               MOVE ZERO TO W-LD
               GO TO S-35
           END-IF
           PERFORM S-60 THRU S-70.
           PERFORM S-20 THRU S-30.
           MOVE ZERO TO W-LD W-CD.
           GO TO S-35.
       S-40.
           EXIT.
       S-45.
           IF  CHK NOT = 9
               GO TO S-50
           END-IF
           PERFORM S-35 THRU S-40.
           IF  W-CD = 0
               MOVE W-15K TO P-15K1(W-LD) P-15K2(W-LD)
               MOVE W-20K TO P-20K1(W-LD) P-20K2(W-LD)
               MOVE "　　　　　　　　［　合　計　］" TO P-NAME1(W-LD)
               MOVE W-GKIN TO P-KIN1(W-LD)
               MOVE ":" TO P-X(W-LD)
           ELSE
               MOVE "　　　　　　　　［　合　計　］" TO P-NAME2(W-LD)
               MOVE W-GKIN TO P-KIN2(W-LD)
           END-IF.
       S-50.
           PERFORM S-35 THRU S-40.
           IF  W-CD = 0
               MOVE W-15K TO P-15K1(W-LD) P-15K2(W-LD)
               MOVE W-20K TO P-20K1(W-LD) P-20K2(W-LD)
               MOVE ":" TO P-X(W-LD)
           END-IF.
       S-55.
           EXIT.
       S-60.
           IF  W-PC = 0
               MOVE 5 TO W-PC
               CALL "PR_Open" RETURNING RESP
               ACCEPT H-DATE FROM DATE
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE ZERO TO W-LD.
       S-65.
           ADD 1 TO W-LD.
           IF  W-LD = 59
               GO TO S-70
           END-IF
           IF  P-X(W-LD) = SPACE
               GO TO S-70
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-PD(W-LD) TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO S-65.
       S-70.
           EXIT.

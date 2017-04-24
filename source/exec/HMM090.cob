       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      HMM090.
      *********************************************************
      *    PROGRAM         :  履物品名マスター削除参考リスト  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *    DATA WRITTN     :  00/06/22                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(022) VALUE SPACE.
           02  F              PIC  N(014) VALUE
                "＊＊＊　　履物品名マスター　".
           02  F              PIC  N(013) VALUE
                "削除参考　リスト　　＊＊＊".
           02  F              PIC  X(011) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  N(004) VALUE "最終年月".
           02  F              PIC  X(007) VALUE "   :   ".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  N(004) VALUE "最終年月".
       01  W-P.
           02  W-PD    OCCURS  58.
             03  P-HCD1       PIC  9(006).
             03  F            PIC  X(001).
             03  P-NAME1      PIC  N(024).
             03  F            PIC  X(001).
             03  P-DNG1       PIC 9999/99.
             03  F            PIC  X(003).
             03  P-C          PIC  X(001).
             03  F            PIC  X(003).
             03  P-HCD2       PIC  9(006).
             03  F            PIC  X(001).
             03  P-NAME2      PIC  N(024).
             03  F            PIC  X(001).
             03  P-DNG2       PIC 9999/99.
       01  W-DATA.
           02  W-POC          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-CD           PIC  9(001).
           02  W-LD           PIC  9(002).
           02  W-PAGE         PIC  9(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊　　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊　　履物品名マスター　削除参考リスト　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊　　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                 "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  N(013) VALUE
                  "＊　　ＤＡＴＡ　なし　　＊".
             03  E-KEY   PIC  X(006).
           COPY LSSEM.
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
           "C-MID" " " "0" "0" "386" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "52" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "52" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "52" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "52" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "52" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "52" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "52" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "20" "40" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "57" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "32" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "32" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "N" "24" "15" "26" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "X" "24" "55" "6" "E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE HI-KEY "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-15
           END-IF
      *
           PERFORM S-55 THRU S-65.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-20.
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HI-DELC = 1
               GO TO M-20
           END-IF
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-35.
           GO TO M-20.
       M-90.
           PERFORM S-40 THRU S-50.
           IF  W-POC NOT = 0
               CALL "PR_Close" RETURNING RESP
           ELSE
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
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
           ADD 1 TO W-LD.
           IF  W-LD < 59
               GO TO S-25
           END-IF
           IF  W-CD = 0
               MOVE 1 TO W-CD W-LD
               GO TO S-25
           END-IF
           PERFORM S-40 THRU S-50.
           PERFORM S-55 THRU S-65.
           GO TO S-20.
       S-25.
           EXIT.
       S-30.
           IF  W-CD = 0
               MOVE ":" TO P-C(W-LD)
               MOVE HI-KEY TO P-HCD1(W-LD)
               MOVE HI-NAME TO P-NAME1(W-LD)
               IF  HI-DNG NOT = ZERO
                   MOVE HI-DNG TO P-DNG1(W-LD)
               END-IF
           END-IF
           IF  W-CD = 1
               MOVE HI-KEY TO P-HCD2(W-LD)
               MOVE HI-NAME TO P-NAME2(W-LD)
               IF  HI-DNG NOT = ZERO
                   MOVE HI-DNG TO P-DNG2(W-LD)
               END-IF
           END-IF.
       S-35.
           EXIT.
       S-40.
           IF  W-POC = 0
               MOVE 1 TO W-POC
               CALL "PR_Open" RETURNING RESP
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE ZERO TO W-LD.
       S-45.
           ADD 1 TO W-LD.
           IF  W-LD = 59
               GO TO S-50
           END-IF
           IF  P-C(W-LD) NOT = SPACE
               MOVE SPACE TO SP-R
               MOVE W-PD(W-LD) TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               GO TO S-45
           END-IF.
       S-50.
           EXIT.
       S-55.
           MOVE SPACE TO W-P.
           MOVE ZERO TO W-LD.
       S-60.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               MOVE SPACE TO P-NAME1(W-LD) P-NAME2(W-LD)
               GO TO S-60
           END-IF
           MOVE ZERO TO W-LD W-CD.
       S-65.
           EXIT.

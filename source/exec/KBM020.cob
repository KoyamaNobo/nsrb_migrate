       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBM020.
      *******************************************************************
      *    PROGRAM         :  仕入先　名簿　　　　　　　　              *
      *    PRINTER TYPE    :  JIPS                                      *
      *    SCREEN          :  ******                                    *
      *        変更　　　  :  62/03/26                                  *
      *    COMPILE TYPE    :  COBOL                                     *
      *******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-15K              PIC  X(005) VALUE X"1A24212078".
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  N(013) VALUE
                "仕　　入　　先　　名　　簿".
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99B.
           02  F              PIC  N(002) VALUE "現在".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(002) VALUE "P.".
           02  H-PAGE         PIC  Z(002).
       01  HEAD2.
           02  F              PIC  X(005) VALUE " ｺｰﾄﾞ".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(007) VALUE "仕　入　先　名".
           02  F              PIC  X(108) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(035) VALUE SPACE.
           02  F              PIC  N(004) VALUE "郵便番号".
           02  F              PIC  X(020) VALUE SPACE.
           02  F              PIC  N(007) VALUE "住　　　　　所".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(004) VALUE "電話番号".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(005) VALUE "ＦＡＸ番号".
           02  F              PIC  X(003) VALUE SPACE.
       01  W-P1.
           02  P-15K          PIC  X(005).
           02  F              PIC  X(001).
           02  P-KEY          PIC  9(004).
           02  F              PIC  X(002).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(090).
       01  W-P2.
           02  F              PIC  X(035).
           02  P-UNO          PIC  X(008).
           02  F              PIC  X(002).
           02  P-JSU          PIC  N(024).
           02  F              PIC  X(001).
           02  P-JSS          PIC  N(012).
           02  F              PIC  X(002).
           02  P-TEL          PIC  X(014).
           02  F              PIC  X(002).
           02  P-FAX          PIC  X(014).
           02  F              PIC  X(001).
       01  W-D.
           02  W-SCD.
             03  W-KEY1       PIC  9(004).
             03  W-KEY2       PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  W-PAGE         PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LISM.
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
           02  FILLER  PIC  X(033) VALUE
                "*********************************".
           02  FILLER  PIC  X(033) VALUE
                "*********************************".
           02  FILLER  PIC  X(033) VALUE
                "***                           ***".
           02  FILLER  PIC  X(033) VALUE
                "***     仕入先 名簿　作成     ***".
           02  FILLER  PIC  X(033) VALUE
                "***                           ***".
           02  FILLER  PIC  X(033) VALUE
                "*********************************".
           02  FILLER  PIC  X(033) VALUE
                "*********************************".
           02  FILLER  PIC  X(035) VALUE
                "<  ｺｰﾄﾞ      ﾖﾘ      迄打出し  >".
           02  FILLER  PIC  X(028) VALUE
                "[  確認  OK=1 NO=9   ﾘﾀｰﾝ  ]".
       01  C-ACP.
           02  FILLER.
             03  A-KEY1  PIC  9(004).
             03  A-KEY2  PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999-FHK530" RETURNING RESP.
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
            "01C-MID" "X" "3" "10" "33" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "4" "10" "33" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "5" "10" "33" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "6" "10" "33" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "X" "7" "10" "33" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "X" "8" "10" "33" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "X" "9" "10" "33" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "10" "35" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "13" "28" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "12" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY1" "9" "12" "18" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY1" BY REFERENCE W-KEY1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY2" "9" "12" "26" "4" "A-KEY1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY2" BY REFERENCE W-KEY2 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "33" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "62" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "62" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" " " "01C-ERR" RETURNING RESU.
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
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-KEY1 "A-KEY1" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-KEY2 "A-KEY2" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-KEY1 > W-KEY2
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
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
       M-25.
      *           READ S-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  S-KEY < W-KEY1
               GO TO M-25
           END-IF
           IF  S-KEY > W-KEY2
               CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO W-PAGE.
           PERFORM S-10 THRU S-15.
       M-30.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 57
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-P1 W-P2.
           MOVE ALL "　" TO P-NAME P-JSU P-JSS.
           MOVE W-15K TO P-15K.
           MOVE S-KEY TO P-KEY.
           MOVE S-NAME TO P-NAME.
           MOVE S-JSU TO P-JSU.
           MOVE S-JSS TO P-JSS.
           MOVE S-UNO TO P-UNO.
           MOVE S-TEL TO P-TEL.
           MOVE S-FAX TO P-FAX.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *           READ S-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           GO TO M-30.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
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

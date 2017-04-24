       IDENTIFICATION DIVISION.
       PROGRAM-ID. JTN90I.
      *********************************************************
      *    PROGRAM         :  受注サイズ無預り入力            *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHM21                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  ERR-STAT           PIC  X(002).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(022) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　サイズ未決定　預りありリスト　　＊＊＊".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  N(004) VALUE "入力日付".
           02  F              PIC  X(001) VALUE SPACE.
       01  W-P.
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(001).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(002).
           02  P-DATE         PIC  99/99/99.
       01  W-DATA.
           02  W-ACT          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-KEY.
             03  W-TCD        PIC  9(004).
             03  W-HCD        PIC  9(006).
           02  W-TCDF         PIC  9(004).
           02  W-TCDR         PIC  9(004).
           02  W-HCDF         PIC  9(006).
           02  W-HCDR         PIC  9(006).
           02  CHK            PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-PAGE         PIC  9(002).
           02  W-DATE         PIC  9(006).
           02  W-END          PIC  9(001).
           COPY LSTAT.
      *
           COPY LITM.
           COPY LIHIM.
      *FD  JAZF
       01  JAZF_JTN90I.
           02  JAZF_PNAME1        PIC  X(004) VALUE "JAZF".
           02  F                  PIC  X(001).
           02  JAZF_LNAME         PIC  X(011) VALUE "JAZF_JTN31I".
           02  F                  PIC  X(001).
           02  JAZF_KEY1          PIC  X(100) VALUE SPACE.
           02  JAZF_SORT          PIC  X(100) VALUE SPACE.
           02  JAZF_IDLST         PIC  X(100) VALUE SPACE.
           02  JAZF_RES           USAGE  POINTER.
       01  JAZ-R.
           02  JAZ-KEY.
             03  JAZ-TCD          PIC 9(04).
             03  JAZ-HCD          PIC 9(06).
           02  JAZ-DATE           PIC 9(06).
           02  F                  PIC X(05).
       77  F                      PIC X(01).
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
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　サイズ未決定預り　入力　　＊＊＊".
           02  FILLER  PIC  X(043) VALUE
                  "登録=1 削除=3 作表=4 問合せ=5 終了=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER.
             03  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER  PIC  N(006) VALUE "得意先名".
           02  FILLER.
             03  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER  PIC  N(004) VALUE "品　　名".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID4.
           02  FILLER.
             03  FILLER  PIC  N(003) VALUE "得意先".
             03  FILLER  PIC  N(001) VALUE "～".
           02  FILLER.
             03  FILLER  PIC  N(003) VALUE "品　名".
             03  FILLER  PIC  N(001) VALUE "～".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID5.
           02  FILLER.
             03  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER  PIC  N(006) VALUE "得意先名".
           02  FILLER.
             03  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER  PIC  N(004) VALUE "品　　名".
             03  FILLER  PIC  N(004) VALUE "入力日".
           02  FILLER  PIC  X(031) VALUE
                "NEXT=ENTER , ｺｰﾄﾞ入力=BACKSPACE".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  C-ACP1.
             03  A-TCD   PIC  9(004).
             03  A-HCD   PIC  9(006).
           02  C-ACP4.
             03  FILLER.
               04  A-TCDF  PIC  9(004).
               04  A-TCDR  PIC  9(004).
             03  FILLER.
               04  A-HCDF  PIC  9(006).
               04  A-HCDR  PIC  9(006).
           02 A-DMM    PIC  9(001).
       01  C-DSP.
           02  D-TNA   PIC  N(026).
           02  D-HNA   PIC  N(024).
           02  D-MEI.
             03  FILLER  PIC  9(006).
             03  FILLER  PIC  N(024).
             03  FILLER  PIC 99/99/99.
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(023) VALUE
                  "***  ﾄｳﾛｸ ｽﾞﾐ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  END DATA  ***".
             03  E-ME5   PIC  X(024) VALUE
                  "***  JAZF WRITE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(025) VALUE
                  "***  JAZF DELETE ｴﾗｰ  ***".
           COPY LSSEM.
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
            "C-MID" " " "0" "0" "85" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "19" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "3" "19" "43" "01C-MID" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "50" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" " " "5" "0" "16" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-MID1" "X" "5" "4" "4" " " "01C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201C-MID1" "N" "5" "16" "12" "0101C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID1" " " "7" "0" "12" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID1" "X" "7" "4" "4" " " "02C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID1" "N" "7" "16" "8" "0102C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID1" "X" "22" "39" "22" "02C-MID1" " " RETURNING RESU.
      *C-MID4
       CALL "SD_Init" USING 
            "C-MID4" " " "0" "0" "38" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID4" " " "9" "0" "8" " " "C-MID4" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-MID4" "N" "9" "25" "6" " " "01C-MID4" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201C-MID4" "N" "9" "39" "2" "0101C-MID4" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID4" " " "11" "0" "8" "01C-MID4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID4" "N" "11" "25" "6" " " "02C-MID4"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID4" "N" "11" "39" "2" "0102C-MID4" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID4" "X" "22" "39" "22" "02C-MID4" " " RETURNING RESU.
      *C-MID5
       CALL "SD_Init" USING 
            "C-MID5" " " "0" "0" "67" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID5" " " "5" "0" "16" " " "C-MID5" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-MID5" "X" "5" "4" "4" " " "01C-MID5" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201C-MID5" "N" "5" "16" "12" "0101C-MID5" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID5" " " "7" "0" "20" "01C-MID5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID5" "X" "7" "5" "4" " " "02C-MID5" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID5" "N" "7" "11" "8" "0102C-MID5" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302C-MID5" "N" "7" "61" "8" "0202C-MID5" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID5" "X" "22" "23" "31" "02C-MID5" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "32" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "57" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-ACP1" " " "0" "0" "10" "A-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "5" "10" "4" " " "C-ACP1" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "7" "9" "6" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-ACP4" " " "0" "0" "20" "C-ACP1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP4" " " "9" "57" "8" " " "C-ACP4" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCDF" "9" "9" "33" "4" " " "01C-ACP4" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCDF" BY REFERENCE W-TCDF "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCDR" "9" "9" "43" "4" "A-TCDF" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCDR" BY REFERENCE W-TCDR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP4" " " "11" "57" "12" "01C-ACP4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCDF" "9" "11" "32" "6" " " "02C-ACP4" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCDF" BY REFERENCE W-HCDF "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCDR" "9" "11" "42" "6" "A-HCDF" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCDR" BY REFERENCE W-HCDR "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "56" "1" "C-ACP4" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "162" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "5" "25" "52" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNA" "N" "7" "25" "48" "D-TNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEI" " " "W-L" "0" "62" "D-HNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MEI" "9" "W-L" "4" "6" " " "D-MEI" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MEI" BY REFERENCE JAZ-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MEI" "N" "W-L" "11" "48" "01D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MEI" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MEI" "99/99/99" "W-L" "60" "8" "02D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-MEI" BY REFERENCE JAZ-DATE "6" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "107" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "107" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "23" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "24" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "25" "E-ME5" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           ACCEPT W-DATE FROM DATE.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-ACT = 9
               GO TO M-95
           END-IF
           IF  W-ACT = 4
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU
               CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-MID4" C-MID4 "p" RETURNING RESU
               GO TO M-45
           END-IF
           IF  W-ACT = 5
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU
               CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-MID5" C-MID5 "p" RETURNING RESU
               CALL "DB_F_Open" USING
                "INPUT" JAZF_PNAME1 "SHARED" BY REFERENCE JAZF_IDLST "1"
                "JAZ-KEY" BY REFERENCE JAZ-KEY
               GO TO M-15
           END-IF
           IF  W-ACT NOT = 1 AND 3
               GO TO M-15
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" JAZF_PNAME1 "SHARED" BY REFERENCE JAZF_IDLST "1"
            "JAZ-KEY" BY REFERENCE JAZ-KEY.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               CALL "DB_F_Close" USING
                BY REFERENCE JAZF_IDLST JAZF_PNAME1
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
      *
           MOVE W-TCD TO T-TCD.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           IF  W-ACT = 5
               GO TO M-40
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
      *
           MOVE W-HCD TO HI-HCD.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
      *
           MOVE W-KEY TO JAZ-KEY.
      *           READ JAZF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JAZF_PNAME1 BY REFERENCE JAZ-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           IF  W-ACT = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-20
           END-IF
           GO TO M-30.
       M-25.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-20
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-DMM = 9
               GO TO M-15
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-30
           END-IF
      *
           IF  W-ACT = 3
               GO TO M-35
           END-IF
           INITIALIZE JAZ-R.
           MOVE W-KEY TO JAZ-KEY.
           MOVE W-DATE TO JAZ-DATE.
      *           WRITE JAZ-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            JAZF_PNAME1 JAZF_LNAME JAZ-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JAZF_IDLST JAZF_PNAME1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-10
           END-IF
           GO TO M-20.
       M-35.
      *           DELETE JAZF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING JAZF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JAZF_IDLST JAZF_PNAME1
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-10
           END-IF
           GO TO M-20.
       M-40.
           MOVE ZERO TO JAZ-KEY.
           MOVE W-TCD TO JAZ-TCD.
      *           START JAZF KEY NOT < JAZ-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JAZF_PNAME1 "JAZ-KEY" " NOT < " JAZ-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
      *           READ JAZF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JAZF_PNAME1 BY REFERENCE JAZ-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  JAZ-TCD NOT = W-TCD
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           MOVE 0 TO W-END.
           MOVE 7 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           PERFORM DSP-RTN THRU DSP-EX.
           IF  W-END = 1
               GO TO M-15
           END-IF.
       M-45.
           PERFORM PAC-RTN THRU PAC-EX.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" JAZF_PNAME1 "SHARED" BY REFERENCE JAZF_IDLST "1"
            "JAZ-KEY" BY REFERENCE JAZ-KEY.
           MOVE ZERO TO JAZ-KEY.
           MOVE W-TCDF TO JAZ-TCD.
           MOVE W-HCDF TO JAZ-HCD.
      *           START JAZF KEY NOT < JAZ-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JAZF_PNAME1 "JAZ-KEY" " NOT < " JAZ-KEY RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JAZF_IDLST JAZF_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-45
           END-IF.
       M-50.
      *           READ JAZF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JAZF_PNAME1 BY REFERENCE JAZ-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JAZF_IDLST JAZF_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-45
           END-IF
           IF  JAZ-TCD < W-TCDF OR > W-TCDR
               GO TO M-50
           END-IF
           IF  JAZ-HCD < W-HCDF OR > W-HCDR
               GO TO M-50
           END-IF
      *
           CALL "PR_Open" RETURNING RESP.
           PERFORM MID-020 THRU MID-EX.
       M-55.
           PERFORM LST-RTN THRU LST-EX.
       M-60.
      *           READ JAZF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JAZF_PNAME1 BY REFERENCE JAZ-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JAZF_IDLST JAZF_PNAME1
               GO TO M-10
           END-IF
           IF  JAZ-TCD > W-TCDR
               CALL "DB_F_Close" USING
                BY REFERENCE JAZF_IDLST JAZF_PNAME1
               GO TO M-10
           END-IF
           IF  JAZ-HCD < W-HCDF OR > W-HCDR
               GO TO M-60
           END-IF
           GO TO M-55.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       DSP-RTN.
           MOVE JAZ-HCD TO HI-HCD.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
           END-IF
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 21
               GO TO DSP-020
           END-IF
           CALL "SD_Output" USING "D-MEI" D-MEI "p" RETURNING RESU.
      *
      *           READ JAZF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JAZF_PNAME1 BY REFERENCE JAZ-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               GO TO DSP-020
           END-IF
           IF  JAZ-TCD NOT = W-TCD
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               GO TO DSP-020
           END-IF
           GO TO DSP-RTN.
       DSP-020.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP AND BTB
               GO TO DSP-020
           END-IF
           IF  ESTAT = BTB
               MOVE 1 TO W-END
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID5" C-MID5 "p" RETURNING RESU.
           IF  W-END = 1
               GO TO DSP-EX
           END-IF
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           MOVE 7 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           GO TO DSP-RTN.
       DSP-EX.
           EXIT.
       PAC-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-TCDF "A-TCDF" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PAC-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO PAC-RTN
           END-IF.
       PAC-020.
           CALL "SD_Accept" USING BY REFERENCE A-TCDR "A-TCDR" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PAC-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO PAC-020
           END-IF
           IF  W-TCDF > W-TCDR
               GO TO PAC-020
           END-IF.
       PAC-040.
           CALL "SD_Accept" USING BY REFERENCE A-HCDF "A-HCDF" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PAC-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO PAC-040
           END-IF.
       PAC-060.
           CALL "SD_Accept" USING BY REFERENCE A-HCDR "A-HCDR" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PAC-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO PAC-060
           END-IF
           IF  W-HCDF > W-HCDR
               GO TO PAC-060
           END-IF.
       PAC-080.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PAC-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO PAC-080
           END-IF
           IF  W-DMM = 9
               GO TO PAC-RTN
           END-IF
           IF  W-DMM NOT = 1
               GO TO PAC-080
           END-IF.
       PAC-EX.
           EXIT.
       LST-RTN.
           MOVE 0 TO CHK.
           MOVE JAZ-TCD TO W-TCD.
           MOVE W-TCD TO T-TCD.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "　　＊＊　マスターなし　＊＊" TO T-NAME
           END-IF.
       LST-020.
           MOVE JAZ-HCD TO HI-HCD.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "　　＊＊　マスターなし　＊＊" TO HI-NAME
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA P-HNA.
           IF  CHK = 0
               MOVE 5 TO CHK
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
           END-IF
           MOVE JAZ-HCD TO P-HCD
           MOVE HI-NAME TO P-HNA.
           MOVE JAZ-DATE TO P-DATE.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       LST-EX.
           EXIT.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-020.
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
       MID-EX.
           EXIT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSA010.
      ********************************************
      *****     送　金　案　内　　入　力     *****
      *****        ( SCREEN : SCTA01)        *****
      ********************************************
       AUTHOR. S-NAKAO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-15K            PIC  X(005) VALUE X"1A24212078".
       01  HEAD1.
           02  W-20K        PIC  X(005) VALUE X"1A24212474".
           02  F            PIC  X(004) VALUE SPACE.
           02  F            PIC  N(022) VALUE
                  "＊＊＊　　送金案内　プルーフリスト　　＊＊＊".
           02  F            PIC  X(005) VALUE SPACE.
           02  F            PIC  X(005) VALUE "DATE ".
           02  H-DATE       PIC 99B99B99.
           02  F            PIC  X(007) VALUE "     P.".
           02  H-PAGE       PIC Z9.
       01  HEAD2.
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(003) VALUE   "日　付".
           02  F            PIC  X(008) VALUE "   ｺｰﾄﾞ ".
           02  F            PIC  N(007) VALUE   "送　金　先　名".
           02  F            PIC  X(021) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "送金区分".
           02  F            PIC  X(006) VALUE SPACE.
           02  F            PIC  N(003) VALUE   "金　額".
           02  F            PIC  X(003) VALUE SPACE.
           02  F            PIC  N(001) VALUE   "№".
       01  W-P.
           02  P-15K        PIC  X(005).
           02  P-DATE       PIC 99B99B99.
           02  F            PIC  X(002).
           02  P-SCD        PIC  9(004).
           02  F            PIC  X(001).
           02  P-NAME       PIC  N(024).
           02  F            PIC  X(003).
           02  P-SC         PIC  9(001).
           02  P-KIN        PIC ZZZZZZZ,ZZZ,ZZ9.
           02  F            PIC  9(002).
           02  P-SEQ        PIC  9(003).
           02  P-20K        PIC  X(005).
       01  W-R.
           02  W-RD.
             03  W-DATE     PIC  9(006).
             03  W-DATED REDEFINES W-DATE.
               04  W-NEN    PIC  9(002).
               04  W-GET    PIC  9(002).
               04  W-PEY    PIC  9(002).
             03  W-SCD      PIC  9(004).                                ｼｲﾚｻｷC
           02  W-SC         PIC  9(001).                                ｿｳｷﾝC
           02  W-KIN        PIC S9(009).                                ｷﾝｶﾞｸ
           02  F            PIC  X(009).
           02  W-SEQ        PIC  9(003).                                NO
       01  W-DATA.
           02  W-KEY        PIC  9(003).
           02  W-NO         PIC  9(003).
           02  W-ACT        PIC  9(001).
           02  W-DMM        PIC  9(001).
           02  CHK          PIC  9(001).
           02  W-PAGE       PIC  9(002).
           02  W-D          PIC  9(010).
           02  W-SCN        PIC  N(003).
       01  ERR-STAT         PIC  X(002).
           COPY LSTAT.
      *
           COPY LISM.
           COPY LSPF.
      *FD  TSA-F
       01  TSA-F_TSA010.
           02  TSA-F_PNAME1 PIC  X(004) VALUE "TSAF".
           02  F            PIC  X(001).
           02  TSA-F_LNAME  PIC  X(012) VALUE "TSA-F_TSA010".
           02  F            PIC  X(001).
           02  TSA-F_KEY1   PIC  X(100) VALUE SPACE.
           02  TSA-F_SORT   PIC  X(100) VALUE SPACE.
           02  TSA-F_IDLST  PIC  X(100) VALUE SPACE.
           02  TSA-F_RES    USAGE  POINTER.
       01  TSA-R.
           02  SA-D.
             03  SA-DATE    PIC  9(006).                                ﾋｽﾞｹ
             03  SA-SCD     PIC  9(004).                                ｼｲﾚｻｷC
           02  SA-SC        PIC  9(001).                                ｿｳｷﾝC
           02  SA-KIN       PIC  9(009).                                ｷﾝｶﾞｸ
           02  F            PIC  X(009).
           02  SA-SEQ       PIC  9(003).                                NO
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
           02  FILLER  PIC  N(022) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(028) VALUE
                "[  する=5 しない=1   ﾘﾀｰﾝ  ]".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MIDC.
           02  FILLER  PIC  N(022) VALUE
                  "＊＊＊　　送金案内ファイル　クリア　　＊＊＊".
       01  C-MIDP.
           02  FILLER  PIC  N(022) VALUE
                  "＊＊＊　送金案内ファイル　入力リスト　＊＊＊".
       01  C-ACP.
           02  A-CHK   PIC  9(001).
           02  A-ACT   PIC  9(001).
           02  A-KEY   PIC  9(003).
           02  A-DATE  PIC  9(006).
           02  A-SCD   PIC  9(004).
           02  A-SC    PIC  9(001).
           02  A-KIN   PIC  9(009).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NAME  PIC  N(024).
           02  D-SCN   PIC  N(003).
           02  D-KIN   PIC ZZZZZZZZ9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  ﾄｳﾛｸ ｽﾞﾐ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  ﾌｧｲﾙ ﾅｼ  ***".
             03  E-ME3   PIC  X(017) VALUE
                  "***  ﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME4   PIC  X(038) VALUE
                  "***  ﾕｳﾋﾞﾝNO･ｼﾞｭｳｼｮ ｶﾞ ﾄｳﾛｸ ｻﾚﾃﾅｲ  ***".
             03  E-ME9   PIC  X(021) VALUE
                  "***  PROGRAM ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "226" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "6" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "7" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "X" "10" "18" "28" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "X" "20" "25" "22" "05C-MID" " " RETURNING RESU.
      *C-MIDC
       CALL "SD_Init" USING 
            "C-MIDC" " " "0" "0" "44" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MIDC" "N" "5" "10" "44" " " "C-MIDC" RETURNING RESU.
      *C-MIDP
       CALL "SD_Init" USING 
            "C-MIDP" " " "0" "0" "44" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MIDP" "N" "5" "10" "44" " " "C-MIDP" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "26" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CHK" "9" "10" "38" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CHK" BY REFERENCE CHK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "49" "1" "A-CHK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "9" "5" "20" "3" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DATE" "9" "7" "20" "6" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DATE" BY REFERENCE W-DATE "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SCD" "9" "9" "20" "4" "A-DATE" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SCD" BY REFERENCE W-SCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SC" "9" "12" "20" "1" "A-SCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SC" BY REFERENCE W-SC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KIN" "9" "13" "20" "9" "A-SC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KIN" BY REFERENCE W-KIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "42" "1" "A-KIN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "63" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "10" "20" "48" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE S-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SCN" "N" "12" "22" "6" "D-NAME" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SCN" BY REFERENCE W-SCN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN" "ZZZZZZZZ9" "13" "20" "9" "D-SCN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-KIN" BY REFERENCE W-KIN "9" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "173" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "173" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "17" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "38" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "21" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MIDC" C-MIDC "p" RETURNING RESU.
           PERFORM S-30 THRU S-40.
           IF  CHK = 5
               CALL "DB_F_Open" USING
                "OUTPUT" TSA-F_PNAME1 " " BY REFERENCE TSA-F_IDLST "0"
               CALL "DB_F_Close" USING
                BY REFERENCE TSA-F_IDLST TSA-F_PNAME1
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" TSA-F_PNAME1 " " BY REFERENCE TSA-F_IDLST "0".
           MOVE ZERO TO W-NO.
       M-10.
      *           READ TSA-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TSA-F_PNAME1 BY REFERENCE TSA-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           IF  SA-SEQ > W-NO
               MOVE SA-SEQ TO W-NO
           END-IF
           GO TO M-10.
       M-15.
           CALL "DB_F_Close" USING
            BY REFERENCE TSA-F_IDLST TSA-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           MOVE ZERO TO W-D.
       M-20.
           CALL "SD_Screen_Output" USING "SCTA01" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-ACT = 9
               GO TO M-80
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Screen_Output" USING "SCTA01" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           IF  W-ACT = 1
               COMPUTE W-KEY = W-NO + 1
               CALL "SD_Output" USING "A-KEY" A-KEY "p" RETURNING RESU
               CALL "DB_F_Open" USING
                "EXTEND" TSA-F_PNAME1 " " BY REFERENCE TSA-F_IDLST "0"
               INITIALIZE W-R
               MOVE W-KEY TO W-SEQ
               MOVE W-D TO W-RD
               GO TO M-40
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           CALL "DB_F_Open" USING
            "I-O" TSA-F_PNAME1 " " BY REFERENCE TSA-F_IDLST "0".
       M-35.
      *           READ TSA-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TSA-F_PNAME1 BY REFERENCE TSA-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE TSA-F_IDLST TSA-F_PNAME1
               GO TO M-30
           END-IF
           IF  W-KEY NOT = SA-SEQ
               GO TO M-35
           END-IF
           MOVE TSA-R TO W-R.
           MOVE W-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   "　　＊＊　マスター　なし　＊＊　" TO S-NAME
           END-IF
           MOVE SPACE TO W-SCN.
           IF  W-SC = 1
               MOVE   "小切手" TO W-SCN
           END-IF
           IF  W-SC = 2
               MOVE   "手　形" TO W-SCN
           END-IF
           IF  W-SC = 3
               MOVE   "相　殺" TO W-SCN
           END-IF
           CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SCD" A-SCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SC" A-SC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SCN" D-SCN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO M-65
           END-IF.
       M-40.
           IF  W-ACT = 1
               CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-DATE "A-DATE" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = BTB
               GO TO M-45
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE TSA-F_IDLST TSA-F_PNAME1.
           IF  W-ACT = 2 OR 3
               GO TO M-30
           END-IF
           GO TO M-20.
       M-45.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-40
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-40
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-40
           END-IF.
       M-50.
           IF  W-ACT = 1
               CALL "SD_Output" USING "A-SCD" A-SCD "p" RETURNING RESU
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-SCD "A-SCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-40
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-50
           END-IF
           MOVE W-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-50
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           IF  SPACE = S-JSU OR S-UNO
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
           END-IF.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-SC "A-SC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-50
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-55
           END-IF
           IF  W-SC NOT = 1 AND 2 AND 3
               GO TO M-55
           END-IF
           MOVE SPACE TO W-SCN.
           IF  W-SC = 1
               MOVE   "小切手" TO W-SCN
           END-IF
           IF  W-SC = 2
               MOVE   "手　形" TO W-SCN
           END-IF
           IF  W-SC = 3
               MOVE   "相　殺" TO W-SCN
           END-IF
           CALL "SD_Output" USING "D-SCN" D-SCN "p" RETURNING RESU.
       M-60.
           CALL "SD_Accept" USING BY REFERENCE A-KIN "A-KIN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-55
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-60
           END-IF
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           IF  W-KIN = ZERO
               GO TO M-60
           END-IF.
       M-65.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = BTB
               GO TO M-70
           END-IF
           IF  W-ACT = 3
               CALL "DB_F_Close" USING BY REFERENCE TSA-F_IDLST
               GO TO M-25
           END-IF
           GO TO M-60.
       M-70.
           IF  ESTAT = HTB AND SKP
               GO TO M-65
           END-IF
           IF  W-DMM = 9
               CALL "DB_F_Close" USING
                BY REFERENCE TSA-F_IDLST TSA-F_PNAME1
               GO TO M-25
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-65
           END-IF
           IF  W-ACT = 1
               INITIALIZE TSA-R
               MOVE W-R TO TSA-R
      *               WRITE TSA-R
      *//////////////
               CALL "DB_Insert" USING
                TSA-F_PNAME1 TSA-F_LNAME TSA-R RETURNING RET
               MOVE W-SEQ TO W-NO
           END-IF
           IF  W-ACT = 2
               INITIALIZE TSA-R
               MOVE W-R TO TSA-R
      *               REWRITE TSA-R.
      *///////////////
               CALL "DB_Update" USING
                TSA-F_PNAME1 TSA-F_LNAME TSA-R RETURNING RET
           END-IF
           IF  W-ACT = 3
               MOVE X"FF" TO TSA-R
      *               REWRITE TSA-R.
      *///////////////
               CALL "DB_Update" USING
                TSA-F_PNAME1 TSA-F_LNAME TSA-R RETURNING RET
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE TSA-F_IDLST TSA-F_PNAME1.
           MOVE W-RD TO W-D.
           GO TO M-25.
       M-80.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MIDP" C-MIDP "p" RETURNING RESU.
           PERFORM S-30 THRU S-40.
           IF  CHK = 1
               GO TO M-95
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" TSA-F_PNAME1 " " BY REFERENCE TSA-F_IDLST "0".
      *           READ TSA-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TSA-F_PNAME1 BY REFERENCE TSA-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TSA-F_IDLST TSA-F_PNAME1
               GO TO M-95
           END-IF
           ACCEPT H-DATE FROM DATE.
           MOVE ZERO TO W-D W-PAGE.
           CALL "PR_Open" RETURNING RESP.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           PERFORM S-10 THRU S-15.
           PERFORM S-20 THRU S-25.
           CALL "DB_F_Close" USING
            BY REFERENCE TSA-F_IDLST TSA-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
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
           MOVE SA-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE   "　　＊＊　マスター　なし　＊＊　" TO S-NAME
           END-IF
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME.
           IF  W-D NOT = SA-D
               MOVE SA-DATE TO P-DATE
               MOVE SA-SCD TO P-SCD
               MOVE S-NAME TO P-NAME
           END-IF
           MOVE SA-SC TO P-SC.
           MOVE SA-KIN TO P-KIN.
           MOVE SA-SEQ TO P-SEQ.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SA-D TO W-D.
      *           READ TSA-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TSA-F_PNAME1 BY REFERENCE TSA-R " " RETURNING RET.
           IF  RET = 1
               GO TO S-25
           END-IF
           GO TO S-20.
       S-25.
           EXIT.
       S-30.
           CALL "SD_Accept" USING BY REFERENCE A-CHK "A-CHK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-30
           END-IF
           IF  CHK NOT = 1 AND 5
               GO TO S-30
           END-IF.
       S-35.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO S-30
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-35
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO S-35
           END-IF.
       S-40.
           EXIT.

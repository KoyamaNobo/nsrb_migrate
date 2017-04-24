       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSR200.
      ********************************************************
      *****     領収書（控）・領収書ファイル　クリア     *****
      ********************************************************
       AUTHOR. S-NAKAO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1.
           02  W-15K        PIC  X(005) VALUE X"1A24212078".
           02  F            PIC  X(008) VALUE X"1A26212068222176".
           02  F            PIC  X(025) VALUE SPACE.
           02  F            PIC  N(009) VALUE  "領　収　書　（控）".
           02  F            PIC  X(014) VALUE SPACE.
           02  F            PIC  X(005) VALUE "DATE ".
           02  H-DATE       PIC 99B99B99.
           02  F            PIC  X(007) VALUE "     P.".
           02  H-PAGE       PIC Z9.
           02  F            PIC  X(008) VALUE X"1A26212068212078".
           02  W-20K        PIC  X(005) VALUE X"1A24212474".
       01  HEAD2.
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(007) VALUE  "領収書№受取日".
           02  F            PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F            PIC  N(007) VALUE  "取　引　先　名".
           02  F            PIC  X(016) VALUE SPACE.
           02  F            PIC  N(003) VALUE  "金　額".
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(003) VALUE  "発行日".
           02  F            PIC  X(023) VALUE SPACE.
       01  W-P.
           02  P-15K        PIC  X(005).
           02  F            PIC  X(002).
           02  P-RNO        PIC  9(006).
           02  F            PIC  X(001).
           02  P-DATE.
             03  P-NEN      PIC Z9.
             03  P-GET      PIC Z9.
             03  P-PEY      PIC Z9.
           02  F            PIC  X(001).
           02  P-TCD        PIC  9(004).
           02  F            PIC  X(002).
           02  P-TNA        PIC  N(016).
           02  P-KIN        PIC ----,---,--9.
           02  F            PIC  X(001).
           02  P-HNGP.
             03  P-HNEN     PIC Z9.
             03  P-HGET     PIC Z9.
             03  P-HPEY     PIC Z9.
           02  F            PIC  X(001).
           02  P-SHM        PIC  N(004).
           02  F            PIC  X(016).
       01  W-DATA.
           02  W-KEY.
             03  W-DATE     PIC  9(006).
             03  W-TCD      PIC  9(004).
           02  W-HNGP       PIC  9(006).
           02  CHK          PIC  9(001).
           02  W-DMM        PIC  9(001).
           02  W-PAGE       PIC  9(002) VALUE ZERO.
           02  W-NAME       PIC  N(016).
       01  ERR-STAT         PIC  X(002).
           COPY LSTAT.
      *
           COPY LITM.
           COPY LSPF.
      *FD  RS-F
       01  RS-F_TSR200.
           02  RS-F_PNAME1  PIC  X(004) VALUE "RSRF".
           02  F            PIC  X(001).
           02  RS-F_LNAME   PIC  X(011) VALUE "RS-F_TSR200".
           02  F            PIC  X(001).
           02  RS-F_KEY1    PIC  X(100) VALUE SPACE.
           02  RS-F_SORT    PIC  X(100) VALUE SPACE.
           02  RS-F_IDLST   PIC  X(100) VALUE SPACE.
           02  RS-F_RES     USAGE  POINTER.
       01  RS-R.
           02  RS-RNO       PIC  9(006).
           02  RS-HNGP.
             03  RS-HNEN    PIC  9(002).
             03  RS-HGET    PIC  9(002).
             03  RS-HPEY    PIC  9(002).
           02  RS-KEY.
             03  RS-DATE.
               04  RS-NEN   PIC  9(002).
               04  RS-GET   PIC  9(002).
               04  RS-PEY   PIC  9(002).
             03  RS-TCD     PIC  9(004).
           02  RS-KIN       PIC S9(009).
           02  RS-INS       PIC  9(005).
           02  RS-SOC       PIC  9(001).
           02  RS-SHC       PIC  9(001).
           02  F            PIC  X(023).
           02  RS-SEQ       PIC  9(003).
       77  F                PIC  X(001).
      *
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
           02  FILLER  PIC  N(019) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                 "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                 "＊＊＊　　領　収　書　（控）　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                 "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  D-CM.
           02  FILLER  PIC  N(013) VALUE
                 "領収書累積ファイル　クリア".
           02  FILLER  PIC  X(017) VALUE
                "ｽﾙ=1 ｼﾅｲ=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
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
       CALL "PR_Initialize" USING "999-FTR200" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "266" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "38" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "38" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "38" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "38" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "38" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "38" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "38" "06C-MID" " " RETURNING RESU.
      *D-CM
       CALL "SD_Init" USING 
            "D-CM" " " "0" "0" "43" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-CM" "N" "12" "16" "26" " " "D-CM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-CM" "X" "13" "21" "17" "01D-CM" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "13" "33" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
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
           CALL "PR_Open" RETURNING RESP.
           ACCEPT H-DATE FROM DATE.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO W-KEY W-HNGP.
       M-15.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE SPACE TO P-TNA P-SHM.
           MOVE RS-RNO TO P-RNO.
           IF  W-KEY = RS-KEY
               GO TO M-20
           END-IF
           MOVE RS-NEN TO P-NEN.
           MOVE RS-GET TO P-GET.
           MOVE RS-PEY TO P-PEY.
           MOVE RS-TCD TO P-TCD.
           MOVE RS-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  "　＊＊＊　ＴＭ　なし　＊＊＊　　" TO T-TNA
           END-IF
           MOVE SPACE TO W-NAME.
           IF  T-TNA = SPACE
               MOVE T-NAME TO W-NAME
           ELSE
               MOVE T-TNA TO W-NAME
           END-IF.
           MOVE W-NAME TO P-TNA.
       M-20.
           MOVE RS-KIN TO P-KIN.
           IF  RS-SHC = 5
               MOVE  "再発行　" TO P-SHM
           END-IF
           IF  W-HNGP NOT = RS-HNGP
               MOVE RS-HNEN TO P-HNEN
               MOVE RS-HGET TO P-HGET
               MOVE RS-HPEY TO P-HPEY
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               MOVE RS-NEN TO P-NEN
               MOVE RS-GET TO P-GET
               MOVE RS-PEY TO P-PEY
               MOVE RS-TCD TO P-TCD
               MOVE W-NAME TO P-TNA
               MOVE RS-HNEN TO P-HNEN
               MOVE RS-HGET TO P-HGET
               MOVE RS-HPEY TO P-HPEY
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE RS-KEY TO W-KEY.
           MOVE RS-HNGP TO W-HNGP.
       M-25.
      *           READ RS-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" RS-F_PNAME1 BY REFERENCE RS-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
           GO TO M-15.
       M-30.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "D-CM" D-CM "p" RETURNING RESU.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-35
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE RS-F_IDLST RS-F_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" RS-F_PNAME1 " " BY REFERENCE RS-F_IDLST "0".
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE RS-F_IDLST RS-F_PNAME1.
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
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.

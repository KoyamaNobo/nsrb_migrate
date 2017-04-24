       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKD210.
      ***************************************************************
      *    PROGRAM         :  請求明細累積ファイル　入金日入力　　  *
      *    PRINTER TYPE    :  JIPS                                  *
      *    SCREEN          :  ------                                *
      *    COMPILE TYPE    :  COBOL                                 *
      ***************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERR-STAT           PIC  X(002).
       01  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-DATE         PIC  9(006).
           02  W-NGPD  REDEFINES W-DATE.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-CNGP         PIC  9(006).
           02  W-SYSD         PIC  9(006).
           02  W-KIN          PIC S9(009).
           02  W-DMM          PIC  9(001).
           02  W-KEY.
             03  W-SNO        PIC  9(006).
           02  W-END          PIC  9(001) VALUE 0.
           COPY LSTAT.
      *
           COPY LITM.
      *FD  SM-F
       01  SM-F_HKD210.
           02  SM-F_PNAME1    PIC  X(003) VALUE "SMF".
           02  F              PIC  X(001).
           02  SM-F_LNAME     PIC  X(011) VALUE "SM-F_HKD210".
           02  F              PIC  X(001).
           02  SM-F_KEY1      PIC  X(100) VALUE SPACE.
           02  SM-F_SORT      PIC  X(100) VALUE SPACE.
           02  SM-F_IDLST     PIC  X(100) VALUE SPACE.
           02  SM-F_RES       USAGE  POINTER.
       01  SM-R.
           02  S-TCD          PIC  9(004).
           02  S-NGP.
             03  S-NG         PIC  9(002).
             03  S-NGPS       PIC  9(006).
           02  S-ZS           PIC S9(009).
           02  S-ZSZ          PIC S9(007).
           02  S-UR           PIC S9(009).
           02  S-URZ          PIC S9(007).
           02  S-TS           PIC S9(007).
           02  S-TSZ          PIC S9(005).
           02  S-NK           PIC S9(009).
           02  S-NKZ          PIC S9(007).
           02  S-DATE         PIC  9(006).
           02  S-SI           PIC  9(003).
           02  S-SU           PIC  9(001).
           02  S-SNO          PIC  9(006).
           02  F              PIC  X(013).
           02  S-DC           PIC  9(001).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　請求累積ファイル　入金日入力　　＊＊＊".
           02  FILLER.
             03  FILLER  PIC  X(008) VALUE "請 求 №".
             03  FILLER  PIC  X(009) VALUE "終了=ｆ･9".
           02  FILLER  PIC  X(017) VALUE
                "（*印は修正不可）".
           02  FILLER  PIC  X(010) VALUE "* 請求日付".
           02  FILLER  PIC  X(010) VALUE "* 得 意 先".
           02  FILLER  PIC  X(010) VALUE "* 請求金額".
           02  FILLER  PIC  X(008) VALUE "入 金 日".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SNO   PIC  9(006).
           02  A-DATE  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MD.
             03  FILLER.
               04  D-TCD   PIC  9(004).
               04  D-NAME  PIC  N(026).
             03  D-DATE  PIC  9(006).
             03  D-KIN.
               04  FILLER  PIC ----,---,--- .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  ﾋｽﾞｹ ｴﾗｰ  ***".
             03  E-ME5   PIC  X(025) VALUE
                  "***  SMF REWRITE ｴﾗｰ  ***".
           COPY LSSEM.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "142" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "7" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" " " "4" "0" "17" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID" "X" "4" "8" "8" " " "02C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID" "X" "4" "33" "9" "0102C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "5" "34" "17" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "7" "6" "10" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "X" "9" "6" "10" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "X" "11" "6" "10" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "X" "13" "8" "8" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "22" "30" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNO" "9" "4" "18" "6" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNO" BY REFERENCE W-SNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DATE" "9" "13" "20" "6" "A-SNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DATE" BY REFERENCE W-DATE "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "47" "1" "A-DATE" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "74" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MD" " " "0" "0" "74" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MD" " " "9" "0" "56" " " "D-MD" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TCD" "9" "9" "18" "4" " " "01D-MD" RETURNING RESU.
       CALL "SD_From" USING 
            "D-TCD" BY REFERENCE S-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "9" "23" "52" "D-TCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATE" "9" "7" "20" "6" "01D-MD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-DATE" BY REFERENCE S-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN" " " "11" "0" "12" "D-DATE" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-KIN" "----,---,---" "11" "17" "12" " " "D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-KIN" BY REFERENCE W-KIN "9" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "75" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "75" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "15" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "25" "E-ME3" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           ACCEPT W-SYSD FROM DATE.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
       M-10.
           PERFORM ACP-RTN THRU ACP-EX.
           IF  W-END = 8
               GO TO M-90
           END-IF
           PERFORM REW-RTN THRU REW-EX.
           IF  W-END = 8
               GO TO M-90
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           GO TO M-10.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-SNO "A-SNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               MOVE 8 TO W-END
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-RTN
           END-IF
           IF  W-SNO = ZERO
               GO TO ACP-RTN
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" SM-F_PNAME1 " " BY REFERENCE SM-F_IDLST "0".
       ACP-020.
      *           READ SM-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SM-F_PNAME1 BY REFERENCE SM-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SM-F_IDLST SM-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-RTN
           END-IF
           IF  S-SNO NOT = W-SNO
               GO TO ACP-020
           END-IF
      *
           MOVE S-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "　マスター　なし　" TO T-NAME
           END-IF
      *
           MOVE S-NGPS TO W-DATE.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEN
           END-IF
           MOVE W-DATE TO W-CNGP.
           COMPUTE W-KIN = S-ZS + S-ZSZ + S-UR + S-URZ + S-TS + S-TSZ
                         - S-NK + S-NKZ.
           MOVE S-DATE TO W-DATE.
           CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU.
           CALL "SD_Output" USING "D-MD" D-MD "p" RETURNING RESU.
       ACP-040.
           CALL "SD_Accept" USING BY REFERENCE A-DATE "A-DATE" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               CALL "DB_F_Close" USING
                BY REFERENCE SM-F_IDLST SM-F_PNAME1
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-040
           END-IF
           IF  W-DATE = ZERO
               GO TO ACP-200
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO ACP-040
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO ACP-040
           END-IF
           IF  W-CNGP NOT < W-DATE
               GO TO ACP-040
           END-IF
           IF  W-SYSD < W-DATE
               GO TO ACP-040
           END-IF.
       ACP-200.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-200
           END-IF
           IF  W-DMM = 9
               CALL "DB_F_Close" USING
                BY REFERENCE SM-F_IDLST SM-F_PNAME1
               GO TO ACP-RTN
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-200
           END-IF.
       ACP-EX.
           EXIT.
       REW-RTN.
      *           REWRITE SM-R.
      *///////////////
           CALL "DB_Update" USING
            SM-F_PNAME1 SM-F_LNAME SM-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 8 TO W-END
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE SM-F_IDLST SM-F_PNAME1.
       REW-EX.
           EXIT.

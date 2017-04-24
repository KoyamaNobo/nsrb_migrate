       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         JJM520.
      ************************************
      ******    振込銀行　問合せ    ******
      ************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE      SECTION.
       01  ERR-STAT                PIC  X(002).
       01  W-DATA.
           02  W-ACT               PIC  9(001).
           02  W-KEY               PIC  9(007).
           02  W-L                 PIC  9(002).
           02  W-C1                PIC  9(002).
           02  W-C2                PIC  9(002).
           COPY LSTAT.
      *
           COPY LIFBKM.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(018)    VALUE
                  "＊＊＊　　振込銀行　問合せ　　＊＊＊".
           02  FILLER.
             03  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER  PIC  N(003) VALUE   "銀行名".
             03  FILLER  PIC  N(004) VALUE   "本支店名".
             03  FILLER  PIC  N(003) VALUE   "最終日".
           02  FILLER  PIC  X(034)    VALUE
                "NEXT=ﾘﾀｰﾝ , 入力=ｆ･10 , 終了=ｆ･9".
       01  C-ACP.
           02  A-KEY   PIC  9(007).
           02  A-ACT   PIC  9(001).
       01  C-DSP.
           02  D-DATA.
               03  D-KEY    PIC  9(007).
               03  D-BKN    PIC  X(015).
               03  D-HSN    PIC  X(015).
               03  D-ENG    PIC 99/99 .
       01  C-ERR.
           02  FILLER.
               03  E-STAT   PIC X(010).
               03  E-ME98   PIC X(005)   VALUE X"1B4A05".
               03  E-ME99   PIC X(005)   VALUE X"1B4205".
       PROCEDURE           DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "94" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "RN" "1" "20" "36" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" " " "3" "0" "24" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID" "RX" "3" "19" "4" " " "02C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID" "RN" "3" "25" "6" "0102C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302C-MID" "RN" "3" "41" "8" "0202C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402C-MID" "RN" "3" "56" "6" "0302C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "23" "30" "34" "02C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "9" "4" "17" "7" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "23" "67" "1" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "42" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATA" " " "W-L" "0" "42" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KEY" "9" "W-L" "17" "7" " " "D-DATA" RETURNING RESU.
       CALL "SD_From" USING 
            "D-KEY" BY REFERENCE FBK-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BKN" "X" "W-L" "25" "15" "D-KEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-BKN" BY REFERENCE FBK-BKN "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HSN" "X" "W-L" "41" "15" "D-BKN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-HSN" BY REFERENCE FBK-HSN "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ENG" "99/99" "W-L" "57" "5" "D-HSN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-ENG" BY REFERENCE FBK-ENG "4" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "20" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "20" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "10" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" FBKM_PNAME1 "SHARED" BY REFERENCE FBKM_IDLST "1"
            "FBK-KEY" BY REFERENCE FBK-KEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO  TO  M-90
           END-IF
           IF  ESTAT = HTB
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU
               GO  TO  M-20
           END-IF
           IF  ESTAT NOT = ADV AND SKP
               GO  TO  M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-10
           END-IF
           IF  ESTAT = PF9
               GO  TO  M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-15
           END-IF
           MOVE  W-KEY  TO  FBK-KEY.
       M-20.
           MOVE  3      TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-25.
      *           START  FBKM KEY NOT < FBK-KEY  INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            FBKM_PNAME1 "FBK-KEY" " NOT < " FBK-KEY RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE FBKM_IDLST FBKM_PNAME1
               GO  TO  M-15
           END-IF.
      *           READ  FBKM NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" FBKM_PNAME1 BY REFERENCE FBK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE FBKM_IDLST FBKM_PNAME1
               GO  TO  M-15
           END-IF.
       M-30.
           ADD  1  TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 23
               GO  TO  M-10
           END-IF
           CALL "SD_Output" USING "D-KEY" D-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BKN" D-BKN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-HSN" D-HSN "p" RETURNING RESU.
           IF  FBK-ENG NOT = ZERO
               CALL "SD_Output" USING "D-ENG" D-ENG "p" RETURNING RESU
           END-IF.
       M-35.
      *           READ  FBKM NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" FBKM_PNAME1 BY REFERENCE FBK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE FBKM_IDLST FBKM_PNAME1
               CALL "DB_F_Open" USING
                "INPUT" FBKM_PNAME1 "SHARED" BY REFERENCE FBKM_IDLST "1"
                "FBK-KEY" BY REFERENCE FBK-KEY
               MOVE  ZERO  TO  FBK-KEY
               GO  TO  M-25
           END-IF
           GO  TO  M-30.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE FBKM_IDLST FBKM_PNAME1.
       M-99.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.

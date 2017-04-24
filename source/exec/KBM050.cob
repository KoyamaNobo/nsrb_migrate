       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         KBM050.
      ****************************************
      ******    仕入先コード　問合せ    ******
      ****************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE      SECTION.
       01  ERR-STAT                PIC  X(002).
       01  W-DATA.
           02  W-SEN               PIC  9(001).
           02  W-SENK              PIC  9(001).
           02  W-DMM               PIC  9(001).
           02  W-KEY               PIC  9(004).
           02  W-L                 PIC  9(002).
           02  W-C1                PIC  9(002).
           02  W-C2                PIC  9(002).
           02  W-END               PIC  9(001).
           02  W-KM                PIC  N(001).
           COPY LSTAT.
      *
           COPY LISM.
           COPY LISTM.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(020)    VALUE
                "＊＊＊　　仕入先コード　問合せ　　＊＊＊".
           02  FILLER  PIC  X(036)    VALUE
                "停止分   非表示 = 0 , 表示 = 1 ...  ".
           02  FILLER  PIC  X(027)    VALUE
                "全件 = 0 , 買掛先 = 1 ...  ".
           02  FILLER  PIC  X(022)    VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER  PIC  N(020)    VALUE
                "＊＊＊　　仕入先コード　問合せ　　＊＊＊".
           02  FILLER.
             03  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER  PIC  N(020) VALUE
                  "仕　　入　　先　　名　　　　　　　　　　".
           02  FILLER  PIC  X(034)    VALUE
                "NEXT=ﾘﾀｰﾝ , 入力=ｆ･10 , 終了=ｆ･9".
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  A-SENK  PIC  9(001).
           02  A-KEY   PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
               03  D-KEY    PIC 9(004).
               03  D-NAME   PIC N(024).
               03  D-KM     PIC N(001).
       01  C-ERR.
           02  FILLER.
               03  E-STAT   PIC X(010).
               03  E-ME01   PIC N(008)   VALUE
                    "ＥＮＤ　ＤＡＴＡ".
               03  E-ME98   PIC  X(005)  VALUE X"1B4A05".
               03  E-ME99   PIC  X(005)  VALUE X"1B4205".
               03  E-CL     PIC X(050)   VALUE
                   "                                                  ".
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
            "C-MID" " " "0" "0" "125" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "RN" "1" "20" "40" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "10" "22" "36" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "13" "22" "27" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "23" "50" "22" "03C-MID" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "118" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "RN" "1" "20" "40" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID1" " " "3" "0" "44" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "0102C-MID1" "RX" "3" "14" "4" " " "02C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID1" "RN" "3" "19" "40" "0102C-MID1" " "
             RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID1" "X" "23" "30" "34" "02C-MID1" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "7" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "10" "57" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SENK" "9" "13" "48" "1" "A-SEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SENK" BY REFERENCE W-SENK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "9" "4" "14" "4" "A-SENK" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "67" "1" "A-KEY" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "54" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "W-L" "0" "54" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KEY" "9" "W-L" "14" "4" " " "01C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-KEY" BY REFERENCE S-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "W-L" "19" "48" "D-KEY" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE S-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KM" "N" "W-L" "69" "2" "D-NAME" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-KM" BY REFERENCE W-KM "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "86" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "86" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "10" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME01" "N" "24" "15" "16" "E-STAT" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "15" "50" "E-ME99" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-06.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-06
           END-IF
           IF  W-SEN > 1
               GO TO M-06
           END-IF.
       M-07.
           CALL "SD_Accept" USING BY REFERENCE A-SENK "A-SENK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-06
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-07
           END-IF
           IF  W-SENK > 1
               GO TO M-07
           END-IF.
       M-08.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-07
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-08
           END-IF
           IF  W-DMM = 9
               GO TO M-06
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-08
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" ST-M_PNAME1 "SHARED" BY REFERENCE ST-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE ST-M_IDLST ST-M_PNAME1
               GO  TO  M-95
           END-IF
           IF  ESTAT = HTB
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-MID1" C-MID1 "p" RETURNING RESU
               MOVE  3      TO  W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               IF  W-END NOT = 0
                   GO TO M-15
               ELSE
                   GO  TO  M-20
               END-IF
           END-IF
           IF  ESTAT NOT = ADV AND SKP
               GO  TO  M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-10
           END-IF
           IF  ESTAT = PF9
               GO  TO  M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-15
           END-IF
           MOVE 0 TO W-END.
           MOVE  W-KEY  TO  S-KEY.
           MOVE  3      TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-20.
      *           START  S-M  KEY NOT < S-KEY  INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            S-M_PNAME1 "S-KEY" " NOT < " S-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME01" E-ME01 "p" RETURNING RESU
               GO  TO  M-15
           END-IF.
       M-25.
      *           READ  S-M  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME01" E-ME01 "p" RETURNING RESU
               GO  TO  M-15
           END-IF
           IF  W-SEN = 0
               IF  S-TNG NOT = ZERO
                   GO TO M-25
               END-IF
           END-IF.
       M-30.
           MOVE S-KEY TO ST-KEY.
      *           READ ST-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" ST-M_PNAME1 BY REFERENCE ST-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO ST-PC
           END-IF
           IF  W-SENK = 1
               IF  ST-PC = ZERO
                   GO TO M-35
               END-IF
           END-IF
           IF  ST-PC = ZERO
               MOVE SPACE TO W-KM
           ELSE
               MOVE "買" TO W-KM
           END-IF
           ADD  1  TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 23
               GO  TO  M-10
           END-IF
           CALL "SD_Output" USING "D-KEY" D-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KM" D-KM "p" RETURNING RESU.
       M-35.
      *           READ  S-M  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE ST-M_IDLST ST-M_PNAME1
               CALL "DB_F_Open" USING
                "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
                "S-KEY" BY REFERENCE S-KEY
               CALL "DB_F_Open" USING
                "INPUT" ST-M_PNAME1 "SHARED" BY REFERENCE ST-M_IDLST "1"
                "ST-KEY" BY REFERENCE ST-KEY
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME01" E-ME01 "p" RETURNING RESU
               GO  TO  M-10
           END-IF
           IF  W-SEN = 0
               IF  S-TNG NOT = ZERO
                   GO TO M-35
               END-IF
           END-IF
           GO  TO  M-30.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.

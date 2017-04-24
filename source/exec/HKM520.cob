       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         HKM520.
      ****************************************
      ******    得意先コード　問合せ    ******
      ****************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE      SECTION.
       01  ERR-STAT                PIC  X(002).
       01  W-DATA.
           02  W-BC                PIC  9(001).
           02  W-DC                PIC  9(001).
           02  W-ZC                PIC  9(001).
           02  W-ACT               PIC  9(001).
           02  W-KEY               PIC  9(004).
           02  W-L                 PIC  9(002).
           02  W-C1                PIC  9(002).
           02  W-C2                PIC  9(002).
           COPY LSTAT.
      *
           COPY LITM.
           COPY LITTM.
           COPY LIHKBM.
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
                "＊＊＊　　得意先コード　問合せ　　＊＊＊".
       01  C-MID1.
           02  FILLER.
             03  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER  PIC  N(020) VALUE
                  "得　　意　　先　　名　　　　　　　　　　".
             03  FILLER  PIC  N(011) VALUE
                  "担　当　者　　　　　　" .
           02  FILLER  PIC  X(034)    VALUE
                "NEXT=ﾘﾀｰﾝ , 入力=ｆ･10 , 終了=ｆ･9".
       01  C-MID2.
           02  FILLER  PIC  X(008)    VALUE
                "全件 = 0".
           02  FILLER  PIC  X(008)    VALUE
                "履物 = 1".
           02  FILLER  PIC  X(008)    VALUE
                "工品 = 2".
           02  FILLER  PIC  X(016)    VALUE
                "材料 = 3 ... [ ]".
           02  FILLER  PIC  X(035)    VALUE
                "停止分非表示 = 0  ,  表示 = 1 ...  ".
           02  FILLER  PIC  X(035)    VALUE
                "全件 = 0  ,  売掛残有り　 = 1 ...  ".
       01  C-ACP.
           02  A-BC    PIC  9(001).
           02  A-DC    PIC  9(001).
           02  A-ZC    PIC  9(001).
           02  A-KEY   PIC  9(004).
           02  A-ACT   PIC  9(001).
       01  C-DSP.
           02  D-DATA.
               03  D-KEY   PIC 9(004).
               03  D-NAME  PIC N(026).
               03  FILLER  PIC X(001) VALUE "(".
               03  D-TNC   PIC 9(002).
               03  FILLER  PIC X(001) VALUE ")".
               03  D-TNNA  PIC N(009).
       01  C-ERR.
           02  FILLER.
               03  E-STAT  PIC X(010).
               03  E-ME98  PIC  X(005) VALUE X"1B4A05".
               03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
           "C-MID" " " "0" "0" "40" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "RN" "1" "20" "40" " " "C-MID" RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING
           "C-MID1" " " "0" "0" "100" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID1" " " "3" "0" "66" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID1" "RX" "3" "1" "4" " " "01C-MID1" RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID1" "RN" "3" "6" "40" "02C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID1" "RN" "3" "59" "22" "03C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID1" "X" "23" "30" "34" "01C-MID1" " " RETURNING RESU.
      *C-MID2
       CALL "SD_Init" USING
           "C-MID2" " " "0" "0" "110" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID2" "X" "10" "24" "8" " " "C-MID2" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID2" "X" "11" "24" "8" "01C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID2" "X" "12" "24" "8" "02C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID2" "X" "13" "24" "16" "03C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID2" "X" "15" "24" "35" "04C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID2" "X" "17" "24" "35" "05C-MID2" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-BC" "9" "13" "38" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-BC" BY REFERENCE W-BC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DC" "9" "15" "58" "1" "A-BC" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DC" BY REFERENCE W-DC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-ZC" "9" "17" "58" "1" "A-DC" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-ZC" BY REFERENCE W-ZC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-KEY" "9" "4" "1" "4" "A-ZC" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-KEY" BY REFERENCE W-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-ACT" "9" "23" "67" "1" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "78" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-DATA" " " "W-L" "0" "78" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "D-KEY" "9" "W-L" "1" "4" " " "D-DATA" RETURNING RESU.
       CALL "SD_From" USING
           "D-KEY" BY REFERENCE T-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-NAME" "N" "W-L" "6" "52" "D-KEY" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-NAME" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "01D-DATA" "X" "W-L" "59" "1" "D-NAME" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-TNC" "9" "W-L" "60" "2" "01D-DATA" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-TNC" BY REFERENCE T-TNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-DATA" "X" "W-L" "62" "1" "D-TNC" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-TNNA" "N" "W-L" "63" "18" "02D-DATA" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-TNNA" BY REFERENCE HKB-TNNA "28" "0" RETURNING RESU.
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
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID2" C-MID2 "p" RETURNING RESU.
       M-06.
           CALL "SD_Accept" USING BY REFERENCE A-BC "A-BC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-99
           END-IF
           IF  ESTAT = HTB AND SKP
               GO TO M-06
           END-IF
           IF  W-BC > 3
               GO TO M-06
           END-IF.
       M-07.
           CALL "SD_Accept" USING BY REFERENCE A-DC "A-DC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-99
           END-IF
           IF  ESTAT = BTB
               GO TO M-06
           END-IF
           IF  ESTAT = HTB AND SKP
               GO TO M-07
           END-IF
           IF  W-DC > 1
               GO TO M-07
           END-IF.
       M-08.
           CALL "SD_Accept" USING BY REFERENCE A-ZC "A-ZC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-99
           END-IF
           IF  ESTAT = BTB
               GO TO M-07
           END-IF
           IF  ESTAT = HTB AND SKP
               GO TO M-08
           END-IF
           IF  W-ZC > 1
               GO TO M-08
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           IF  W-ZC = 1
               CALL "DB_F_Open" USING
                "INPUT" TT-M_PNAME1 "SHARED" BY REFERENCE TT-M_IDLST "1"
                "TT-KEY" BY REFERENCE TT-KEY
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO  TO  M-90
           END-IF
           IF  ESTAT = HTB
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-MID" C-MID "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-MID1" C-MID1 "p" RETURNING RESU
               GO  TO  M-20
           END-IF
           IF  ESTAT NOT = ADV AND SKP
               GO  TO  M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "1"
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
           MOVE  W-KEY  TO  T-KEY.
       M-20.
           MOVE  3      TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-25.
      *           START  T-M  KEY NOT < T-KEY  INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            T-M_PNAME1 "T-KEY" " NOT < " T-KEY RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
               GO  TO  M-15
           END-IF.
       M-27.
      *           READ  T-M  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
               GO  TO  M-15
           END-IF
           IF  W-BC = 1
               IF  T-BC NOT = 0
                   GO TO M-27
               END-IF
           END-IF
           IF  W-BC = 2
               IF  T-BC NOT = 1
                   GO TO M-27
               END-IF
           END-IF
           IF  W-BC = 3
               IF  T-BC NOT = 3
                   GO TO M-27
               END-IF
           END-IF
           IF  W-DC = 0
               IF  T-ENG NOT = ZERO
                   GO TO M-27
               END-IF
           END-IF
           IF  W-ZC = 0
               GO TO M-30
           END-IF
           MOVE T-KEY TO TT-KEY.
      *           READ TT-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TT-M_PNAME1 BY REFERENCE TT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-27
           END-IF
           IF  ZERO = TT-TUZ AND TT-TUZZ
               GO TO M-27
           END-IF.
       M-30.
           ADD  1  TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 23
               GO  TO  M-10
           END-IF
           MOVE SPACE TO HKB-KEY.
           MOVE 04 TO HKB-NO.
           MOVE T-TNC TO HKB-TNC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-TNNA
           END-IF
           CALL "SD_Output" USING "D-DATA" D-DATA "p" RETURNING RESU.
       M-35.
      *           READ  T-M  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
               CALL "DB_F_Open" USING
                "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
                "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2
               MOVE  ZERO  TO  T-KEY
               GO  TO  M-25
           END-IF
           IF  W-BC = 1
               IF  T-BC NOT = 0
                   GO TO M-35
               END-IF
           END-IF
           IF  W-BC = 2
               IF  T-BC NOT = 1
                   GO TO M-35
               END-IF
           END-IF
           IF  W-BC = 3
               IF  T-BC NOT = 3
                   GO TO M-35
               END-IF
           END-IF
           IF  W-DC = 0
               IF  T-ENG NOT = ZERO
                   GO TO M-35
               END-IF
           END-IF
           IF  W-ZC = 0
               GO TO M-30
           END-IF
           MOVE T-KEY TO TT-KEY.
      *           READ TT-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TT-M_PNAME1 BY REFERENCE TT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           IF  ZERO = TT-TUZ AND TT-TUZZ
               GO TO M-35
           END-IF
           GO  TO  M-30.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           IF  W-ZC = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TT-M_IDLST TT-M_PNAME1
           END-IF.
       M-99.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.

       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         KHM910.
      ******************************************
      ******    工品品名コード　問合せ    ******
      ******************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE      SECTION.
       01  ERR-STAT                PIC  X(002).
       01  W-DATA.
           02  CHK                 PIC  9(001).
           02  W-DMM               PIC  9(001).
           02  W-KEY               PIC  X(005).
           02  W-L                 PIC  9(002).
           02  W-SEN               PIC  9(001).
           02  W-SYC               PIC  9(002).
           02  W-EYC               PIC  9(002) VALUE 99.
           COPY LSTAT.
      *
           COPY LIKHM.
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
           02  FILLER  PIC  N(021)    VALUE
                "＊＊＊　　工品品名コード　問合せ　　＊＊＊".
       01  C-MID1.
           02  FILLER  PIC  X(027) VALUE
                "廃止分   表示=0,非表示=1   ".
           02  FILLER  PIC  X(019) VALUE
                "用途区分   00 ～ 99".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID2.
           02  FILLER.
             03  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER  PIC  N(005) VALUE "品　　　名".
             03  FILLER  PIC  N(002) VALUE "用途".
             03  FILLER  PIC  N(003) VALUE "売　価".
             03  FILLER  PIC  N(003) VALUE "原　価".
             03  FILLER  PIC  N(003) VALUE "最終日".
             03  FILLER  PIC  N(003) VALUE "廃止日".
           02  FILLER  PIC  X(034)    VALUE
                "NEXT=ﾘﾀｰﾝ , 入力=ｆ･10 , 終了=ｆ･9".
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  FILLER.
             03  A-SYC   PIC  9(002).
             03  A-EYC   PIC  9(002).
           02  A-KEY   PIC  X(005).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-KEY   PIC  X(005).
             03  D-NAME  PIC  X(020).
             03  D-YC    PIC  9(002).
             03  D-BT    PIC ZZZ,ZZ9.99 .
             03  D-FT    PIC ZZZ,ZZ9.99 .
             03  D-ENG   PIC 99/99 .
             03  D-DNG   PIC 99/99 .
       01  C-ERR.
           02  FILLER.
               03  E-ME1    PIC X(012)   VALUE
                    "< END DATA >".
               03  E-STAT   PIC X(010).
               03  E-ME98  PIC  X(005) VALUE X"1B4A05".
               03  E-ME99  PIC  X(005) VALUE X"1B4205".
       PROCEDURE           DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "42" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "RN" "1" "14" "42" " " "C-MID"  RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "68" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "X" "13" "21" "27" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID1" "X" "15" "21" "19" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID1" "X" "23" "50" "22" "02C-MID1" " " RETURNING RESU.
      *C-MID2
       CALL "SD_Init" USING 
            "C-MID2" " " "0" "0" "76" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID2" " " "3" "0" "42" " " "C-MID2"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-MID2" "RX" "3" "5" "4" " " "01C-MID2" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201C-MID2" "RN" "3" "10" "10" "0101C-MID2" " "
             RETURNING RESU.
       CALL "SD_Init" USING 
            "0301C-MID2" "RN" "3" "29" "4" "0201C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0401C-MID2" "RN" "3" "38" "6" "0301C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0501C-MID2" "RN" "3" "49" "6" "0401C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0601C-MID2" "RN" "3" "56" "6" "0501C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0701C-MID2" "RN" "3" "63" "6" "0601C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID2" "X" "23" "30" "34" "01C-MID2" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "11" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "13" "47" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "15" "0" "4" "A-SEN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SYC" "9" "15" "32" "2" " " "02C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SYC" BY REFERENCE W-SYC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EYC" "9" "15" "38" "2" "A-SYC" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EYC" BY REFERENCE W-EYC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "X" "4" "4" "5" "02C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "67" "1" "A-KEY" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "57" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "W-L" "0" "57" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KEY" "X" "W-L" "4" "5" " " "01C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-KEY" BY REFERENCE KH-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "X" "W-L" "10" "20" "D-KEY" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE KH-NAME "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-YC" "9" "W-L" "31" "2" "D-NAME" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-YC" BY REFERENCE KH-YC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BT" "ZZZ,ZZ9.99" "W-L" "34" "10" "D-YC" " "
             RETURNING RESU.
       CALL "SD_From" USING 
            "D-BT" BY REFERENCE KH-T1 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-FT" "ZZZ,ZZ9.99" "W-L" "45" "10" "D-BT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-FT" BY REFERENCE KH-GT1 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ENG" "99/99" "W-L" "57" "5" "D-FT" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-ENG" BY REFERENCE KH-ENG "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DNG" "99/99" "W-L" "64" "5" "D-ENG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-DNG" BY REFERENCE KH-DNG "4" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "32" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "32" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "12" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "10" "E-ME1" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-STAT" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
       M-06.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-06
           END-IF
           IF  W-SEN > 1
               GO TO M-06
           END-IF.
       M-07.
           CALL "SD_Accept" USING BY REFERENCE A-SYC "A-SYC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-06
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-07
           END-IF.
       M-08.
           CALL "SD_Accept" USING BY REFERENCE A-EYC "A-EYC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-07
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-08
           END-IF
           IF  W-SYC > W-EYC
               GO TO M-08
           END-IF.
       M-09.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-08
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-09
           END-IF
           IF  W-DMM = 9
               GO TO M-06
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-09
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID2" C-MID2 "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           MOVE 0 TO CHK.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE KH-M_IDLST KH-M_PNAME1
               GO  TO  M-99
           END-IF
           IF  ESTAT = HTB
               IF  CHK = 1
                   GO TO M-15
               ELSE
                   CALL "SD_Output" USING
                    "C-CLEAR" C-CLEAR "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "C-MID" C-MID "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "C-MID2" C-MID2 "p" RETURNING RESU
                   GO  TO  M-20
               END-IF
           END-IF
           IF  ESTAT NOT = ADV AND SKP
               GO  TO  M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "X" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-10
           END-IF
           IF  ESTAT = PF9
               GO  TO  M-99
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-15
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID2" C-MID2 "p" RETURNING RESU.
           MOVE 0 TO CHK.
           MOVE  W-KEY  TO  KH-KEY.
       M-20.
           MOVE  3      TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
      *           START  KH-M  KEY NOT < KH-KEY  INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            KH-M_PNAME1 "KH-KEY" " NOT < " KH-KEY RETURNING RET.
           IF  RET = 1
               MOVE 1 TO CHK
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               GO  TO  M-10
           END-IF.
       M-25.
      *           READ  KH-M  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO CHK
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               GO  TO  M-10
           END-IF
           IF  W-SEN = 1
               IF  KH-DNG NOT = ZERO
                   GO TO M-25
               END-IF
           END-IF
           IF  KH-YC < W-SYC OR > W-EYC
               GO TO M-25
           END-IF.
       M-30.
           ADD  1  TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 23
               GO  TO  M-10
           END-IF
           CALL "SD_Output" USING "D-KEY" D-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "D-YC" D-YC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BT" D-BT "p" RETURNING RESU.
           CALL "SD_Output" USING "D-FT" D-FT "p" RETURNING RESU.
           IF  KH-ENG NOT = ZERO
               CALL "SD_Output" USING "D-ENG" D-ENG "p" RETURNING RESU
           END-IF
           IF  KH-DNG NOT = ZERO
               CALL "SD_Output" USING "D-DNG" D-DNG "p" RETURNING RESU
           END-IF.
       M-35.
      *           READ  KH-M  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO CHK
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               GO  TO  M-10
           END-IF
           IF  W-SEN = 1
               IF  KH-DNG NOT = ZERO
                   GO TO M-35
               END-IF
           END-IF
           IF  KH-YC < W-SYC OR > W-EYC
               GO TO M-35
           END-IF
           GO  TO  M-30.
       M-99.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.

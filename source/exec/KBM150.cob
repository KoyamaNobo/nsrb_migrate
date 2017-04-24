       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         KBM150.
      **************************************
      ******    材料コード　問合せ    ******
      **************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       INPUT-OUTPUT        SECTION.
       DATA                DIVISION.
       WORKING-STORAGE      SECTION.
       01  ERR-STAT                PIC  X(002).
       01  W-DATA.
           02  W-SBKC              PIC  9(002).
           02  W-EBKC              PIC  9(002) VALUE 99.
           02  W-ACT               PIC  9(001).
           02  W-KEY               PIC  9(006).
           02  W-L                 PIC  9(002).
           02  W-C1                PIC  9(002).
           02  W-C2                PIC  9(002).
           02  W-EC                PIC  9(001).
           COPY LSTAT.
      *
           COPY LIJM.
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
           02  FILLER  PIC  N(019)    VALUE
                "＊＊＊　　材料コード　問合せ　　＊＊＊".
           02  FILLER  PIC  X(022)    VALUE
                  "部門管理ｺｰﾄﾞ  00 ～ 99".
           02  FILLER  PIC  X(022)    VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MIDM.
           02  FILLER  PIC  N(019)    VALUE
                "＊＊＊　　材料コード　問合せ　　＊＊＊".
           02  FILLER.
             03  FILLER  PIC  X(012)    VALUE "部門管理ｺｰﾄﾞ".
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  N(001)    VALUE  "～".
             03  FILLER  PIC  9(002).
           02  FILLER.
             03  FILLER  PIC  X(006) VALUE " ｺｰﾄﾞ ".
             03  FILLER  PIC  N(020) VALUE
                  "材　　料　　名　　　　　　　　　　　　　".
             03  FILLER  PIC  N(004) VALUE "　　　　".
             03  FILLER  PIC  X(011) VALUE
                    "   単    価".
             03  FILLER  PIC  N(004) VALUE "最終仕入".
           02  FILLER  PIC  X(034)    VALUE
                "NEXT=ﾘﾀｰﾝ , 入力=ｆ･10 , 終了=ｆ･9".
       01  C-ACP.
           02  FILLER.
             03  A-SBKC  PIC  9(002).
             03  A-EBKC  PIC  9(002).
           02  A-KEY   PIC  9(006).
           02  A-ACT   PIC  9(001).
       01  C-DSP.
           02  FILLER.
               03  D-KEY    PIC 9(006).
               03  D-NAME   PIC N(024).
               03  D-ST     PIC ----,--9.99 .
               03  D-BKC    PIC 9(002).
               03  D-ED     PIC 99/99/99 .
       01  C-ERR.
           02  FILLER.
               03  E-ME1    PIC X(017)   VALUE
                    "***  DATA ﾅｼ  ***".
               03  E-ME2    PIC X(018)   VALUE
                    "***  END DATA  ***".
               03  E-STAT   PIC X(010).
               03  E-CL     PIC X(050)   VALUE
                   "                                                  ".
               03  E-ME98   PIC  X(005)  VALUE X"1B4A05".
               03  E-ME99   PIC  X(005)  VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "82" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "RN" "1" "21" "38" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "10" "29" "22" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "23" "50" "22" "02C-MID" " " RETURNING RESU.
      *C-MIDM
       CALL "SD_Init" USING 
            "C-MIDM" " " "0" "0" "163" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MIDM" "RN" "1" "21" "38" " " "C-MIDM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MIDM" " " "2" "0" "18" "01C-MIDM" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "0102C-MIDM" "X" "2" "58" "12" " " "02C-MIDM" RETURNING RESU.
       CALL "SD_Init" USING 
          "0202C-MIDM" "9" "2" "72" "2" "0102C-MIDM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0202C-MIDM" BY REFERENCE W-SBKC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
          "0302C-MIDM" "N" "2" "75" "2" "0202C-MIDM" " " RETURNING RESU.
       CALL "SD_Init" USING 
          "0402C-MIDM" "9" "2" "78" "2" "0302C-MIDM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0402C-MIDM" BY REFERENCE W-EBKC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MIDM" " " "3" "0" "73" "02C-MIDM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103C-MIDM" "RX" "3" "1" "6" " " "03C-MIDM" RETURNING RESU.
       CALL "SD_Init" USING 
         "0203C-MIDM" "RN" "3" "8" "40" "0103C-MIDM" " " RETURNING RESU.
       CALL "SD_Init" USING 
         "0303C-MIDM" "RN" "3" "48" "8" "0203C-MIDM" " " RETURNING RESU.
       CALL "SD_Init" USING 
        "0403C-MIDM" "RX" "3" "57" "11" "0303C-MIDM" " " RETURNING RESU.
       CALL "SD_Init" USING 
         "0503C-MIDM" "RN" "3" "72" "8" "0403C-MIDM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MIDM" "X" "23" "30" "34" "03C-MIDM" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "11" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "10" "0" "4" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SBKC" "9" "10" "43" "2" " " "01C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SBKC" BY REFERENCE W-SBKC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EBKC" "9" "10" "49" "2" "A-SBKC" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EBKC" BY REFERENCE W-EBKC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "9" "4" "1" "6" "01C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "23" "67" "1" "A-KEY" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "75" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "W-L" "0" "75" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KEY" "9" "W-L" "1" "6" " " "01C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-KEY" BY REFERENCE J-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "W-L" "8" "48" "D-KEY" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE J-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ST" "----,--9.99" "W-L" "57" "11" "D-NAME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-ST" BY REFERENCE J-ST "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BKC" "9" "W-L" "69" "2" "D-ST" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-BKC" BY REFERENCE J-BKC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ED" "99/99/99" "W-L" "72" "8" "D-BKC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-ED" BY REFERENCE J-ED "6" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "105" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "105" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "10" "E-ME2" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-STAT" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-CL" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SBKC "A-SBKC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO  TO  M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-EBKC "A-EBKC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-20
           END-IF
           IF  W-ACT = 9
               GO TO M-95
           END-IF
           IF W-ACT NOT = 1
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MIDM" C-MIDM "p" RETURNING RESU.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-05
           END-IF
           IF  ESTAT = PF9
               GO  TO  M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-30
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           MOVE  W-KEY  TO  J-KEY.
      *
      *           START  J-M  KEY NOT < J-KEY  INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            J-M_PNAME1 "J-KEY" " NOT < " J-KEY RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-30
           END-IF.
       M-35.
      *           READ  J-M  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-30
           END-IF
           IF  J-BKC < W-SBKC OR > W-EBKC
               GO TO M-35
           END-IF
           MOVE  3      TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE 0 TO W-EC.
       M-40.
           ADD  1  TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 23
               GO  TO  M-50
           END-IF
           CALL "SD_Output" USING "D-KEY" D-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BKC" D-BKC "p" RETURNING RESU.
           IF  J-ST  NOT = ZERO
               CALL "SD_Output" USING "D-ST" D-ST "p" RETURNING RESU
           END-IF
           IF  J-ED  NOT = ZERO
               CALL "SD_Output" USING "D-ED" D-ED "p" RETURNING RESU
           END-IF.
       M-45.
      *           READ  J-M  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-EC
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-50
           END-IF
           IF  J-BKC < W-SBKC OR > W-EBKC
               GO TO M-45
           END-IF
           GO  TO  M-40.
       M-50.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1
               GO  TO  M-95
           END-IF
           IF  ESTAT = ADV
               IF  W-EC = 0
                   CALL "DB_F_Close" USING
                    BY REFERENCE J-M_IDLST J-M_PNAME1
                   GO  TO  M-30
               END-IF
           END-IF
           IF  ESTAT NOT = HTB
               GO  TO  M-50
           END-IF
           IF  W-EC = 1
               CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1
               GO TO M-25
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MIDM" C-MIDM "p" RETURNING RESU.
           MOVE  3      TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           GO  TO  M-40.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.

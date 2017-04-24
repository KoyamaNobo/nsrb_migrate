       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         PR070R.
      ****************************************
      ******    取引先コード　問合せ    ******
      ****************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       INPUT-OUTPUT        SECTION.
       DATA                DIVISION.
       WORKING-STORAGE      SECTION.
       01  ERR-STAT                PIC  X(002).
       01  W-DATA.
           02  W-ACT               PIC  9(001).
           02  W-KEY               PIC  9(005).
           02  W-L                 PIC  9(002).
           02  W-C1                PIC  9(002).
           02  W-C2                PIC  9(002).
           02  CHK                 PIC  9(001).
      *
           COPY LSTAT.
           COPY TKLIB.
      *
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL            PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER          PIC  N(020)    VALUE
                "＊＊＊　　取引先コード　問合せ　　＊＊＊".
           02  FILLER.
             03  FILLER        PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER        PIC  N(010) VALUE
                  "取　引　先　名　　　".
             03  FILLER        PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER        PIC  N(010) VALUE
                  "取　引　先　名　　　".
           02    FILLER    PIC  X(034)    VALUE
                "NEXT=ﾘﾀｰﾝ , 入力=ｆ･10 , 終了=ｆ･9".
       01  C-ACP.
           02  A-KEY           PIC  9(005).
           02  A-ACT           PIC  9(001).
       01  C-DSP.
           02  FILLER.
               03  D-KEY.
                 04  FILLER    PIC 9(005).
               03  D-NAME      PIC N(010).
       01  C-ERR.
           02  FILLER.
               03  E-STAT      PIC X(010).
               03  E-ME98      PIC X(005)   VALUE X"1B4A05".
               03  E-ME99      PIC X(005)   VALUE X"1B4205".
       PROCEDURE           DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "122" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "RN" "1" "20" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" " " "3" "0" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "RX" "3" "7" "4" " " "02C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "RN" "3" "13" "20" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "RX" "3" "43" "4" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "RN" "3" "49" "20" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "X" "23" "30" "34" "02C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-KEY" "9" "4" "7" "5" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-KEY" BY REFERENCE W-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ACT" "9" "23" "67" "1" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "25" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-DSP" " " "W-L" "0" "25" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "D-KEY" " " "W-L" "0" "5" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-KEY" "9" "W-L" "W-C1" "5" " " "D-KEY" RETURNING RESU.
       CALL "SD_From" USING
            "01D-KEY" BY REFERENCE TK-KEY "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-NAME" "N" "W-L" "W-C2" "20" "D-KEY" " " RETURNING RESU.
       CALL "SD_From" USING
            "D-NAME" BY REFERENCE TK-NAMEN "20" "0" RETURNING RESU.
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
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p"
                                         RETURNING RESU.
           MOVE ZERO TO W-DATA.
           CALL "DB_F_Open" USING
            "INPUT" TK_PNAME1 "SHARED" BY REFERENCE TK_IDLST "1"
            "TK-KEY" BY REFERENCE TK-KEY.
       M-10.
           CALL "SD_Accept" USING
                 BY REFERENCE A-ACT "A-ACT" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO  TO  M-95
           END-IF.
           IF  ESTAT = HTB
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "C-MID" C-MID "p"
                                         RETURNING RESU
               GO  TO  M-20
           END-IF.
           IF  ESTAT NOT = ADV AND SKP
               GO  TO  M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING
                 BY REFERENCE A-KEY "A-KEY" "9" "5"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-10
           END-IF.
           IF  ESTAT = PF9
               GO  TO  M-95
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-15
           END-IF.
           MOVE ZERO TO TK-KEY.
           MOVE  W-KEY  TO TK-KEY.
       M-20.
           MOVE 0 TO CHK.
           MOVE  3      TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2"  W-L
                          RETURNING RESU.
           MOVE 7 TO W-C1.
           CALL "SD_Arg_Match_Col" USING "W-C1" "2" W-C1
                             RETURNING RESU.
           MOVE 13 TO W-C2.
           CALL "SD_Arg_Match_Col" USING "W-C2" "2" W-C2
                             RETURNING RESU.
       M-25.
      *           START TK    KEY NOT <  TK-KEY  INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            TK_PNAME1 "TK-KEY" " NOT < " TK-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  M-15
           END-IF
      *           READ TK    NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TK_PNAME1 BY REFERENCE TK-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-15
           END-IF.
       M-30.
           ADD  1  TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2"  W-L
                          RETURNING RESU.
           IF  W-L = 23
               IF  CHK = 1
                   GO  TO  M-10
               ELSE
                   MOVE 1 TO CHK
                   MOVE  4 TO  W-L
                   CALL "SD_Arg_Match_Line" USING "W-L" "2"  W-L
                          RETURNING RESU
                   MOVE 43 TO W-C1
                   CALL "SD_Arg_Match_Col" USING "W-C1" "2" W-C1
                             RETURNING RESU
                   MOVE 49 TO W-C2
                   CALL "SD_Arg_Match_Col" USING "W-C2" "2" W-C2
                             RETURNING RESU
               END-IF
           END-IF.
           CALL "SD_Output" USING "D-KEY" D-KEY "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "D-NAME" D-NAME "p"
                                         RETURNING RESU.
       M-35.
      *           READ TK    NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TK_PNAME1 BY REFERENCE TK-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING BY REFERENCE TK_IDLST TK_PNAME1
               CALL "DB_F_Open" USING
                "INPUT" TK_PNAME1 "SHARED" BY REFERENCE TK_IDLST "1"
                "TK-KEY" BY REFERENCE TK-KEY
               MOVE  ZERO  TO TK-KEY
               GO  TO  M-25
           END-IF.
           GO  TO  M-30.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE TK_IDLST TK_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.

       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR070M.
      *****************************************************
      *    PROGRAM       :  取引先マスタメンテナンス      *
      *****************************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.     SYSTEM3100.
       OBJECT-COMPUTER.     SYSTEM3100.
       INPUT-OUTPUT      SECTION.
       DATA              DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT                PIC  X(02).
       01  W-SPACE                 PIC  N(10)  VALUE
                                   "　　　　　　　　　　".              取引先名
       01  W-SPACE1                PIC  N(26)  VALUE
             "　　　　　　　　　　　　　　　　　　　　　　　　　　".
       01  W-AREA.
           02  W-AREA1.
               03  W-ACT           PIC  9(01).                          ACT
               03  W-AREA2.
                   04  W-TORIHIKISAKICD          PIC  9(05).            取引ＣＤ
                   04  W-AREA3.
                       05  W-TORIHIKISAKIMEI     PIC  N(10).            取引先名
                       05  W-PRC       PIC  9(02).
                       05  W-BKC       PIC  9(02).
                       05  W-TCD       PIC  9(04).
                       05  W-KAKU      PIC  X(01).                      確認
      *
       COPY  LWMSG_PR.
       COPY  TKLIB.
       COPY  LITM.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
           03  FILLER   PIC  X(12)  VALUE "CLEAR SCREEN".
       01  CLR-AREA1.
           02  FILLER   PIC  X(001) VALUE " ".                          ACT
           02  CLR-AREA2.
               03  FILLER  PIC X(005) VALUE "     ".                    取引ＣＤ
               03  CLR-AREA3.
                   04  CLR-TORIMEI    PIC N(10).                        取引先名
                   04  CLR-PRC        PIC X(002) VALUE "  ".
                   04  CLR-BKC        PIC X(002) VALUE "  ".
                   04  CLR-TCD        PIC X(004) VALUE "    ".
                   04  CLR-NAME       PIC N(26).
                   04  FILLER  PIC X(001) VALUE " ".                    確認
       01  ACP-AREA.
           03  ACP-ACT                  PIC 9(01).                      ACT
           03  ACP-TORIHIKISAKICD       PIC 9(05).                      取引ＣＤ
           03  ACP-TORIHIKISAKIMEI      PIC N(10).                      銀行名
           03  ACP-PRC                  PIC 9(02).
           03  ACP-BKC                  PIC 9(02).
           03  ACP-TCD                  PIC 9(04).
           03  ACP-NAME                 PIC N(26).
           03  ACP-KAKU                 PIC X(01).                      確認
       COPY  LSMSG_PR.
       PROCEDURE          DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-CLR
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *CLR-AREA1
       CALL "SD_Init" USING
            "CLR-AREA1" " " "0" "0" "87" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA1" "X" "3" "67" "1" " " "CLR-AREA1"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-AREA2" " " "0" "0" "86" "01CLR-AREA1" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA2" "X" "5" "33" "5" " " "CLR-AREA2"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-AREA3" " " "0" "0" "81" "01CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-TORIMEI" "N" "6" "18" "20" " " "CLR-AREA3"
            RETURNING RESU.
       CALL "SD_From" USING
            "CLR-TORIMEI" BY REFERENCE W-SPACE "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-PRC" "X" "7" "18" "2" "CLR-TORIMEI" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-BKC" "X" "8" "18" "2" "CLR-PRC" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-TCD" "X" "9" "18" "4" "CLR-BKC" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-NAME" "N" "9" "24" "52" "CLR-TCD" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "CLR-NAME" BY REFERENCE W-SPACE1 "52" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA3" "X" "24" "77" "1" "CLR-NAME" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "87" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-ACT" "9" "3" "67" "1" " " "ACP-AREA"
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TORIHIKISAKICD" "9" "5" "33" "5" "ACP-ACT" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-TORIHIKISAKICD" BY REFERENCE W-TORIHIKISAKICD "5" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TORIHIKISAKIMEI" "N" "6" "18" "20"
            "ACP-TORIHIKISAKICD" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-TORIHIKISAKIMEI" BY REFERENCE W-TORIHIKISAKIMEI
             "20" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-PRC" "9" "7" "18" "2" "ACP-TORIHIKISAKIMEI" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-PRC" BY REFERENCE W-PRC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-BKC" "9" "8" "18" "2" "ACP-PRC" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-BKC" BY REFERENCE W-BKC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TCD" "9" "9" "18" "4" "ACP-BKC" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-NAME" "N" "9" "24" "52" "ACP-TCD" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "ACP-NAME" BY REFERENCE T-NAME "52" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAKU" "X" "24" "77" "1" "ACP-NAME" " "
            RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-KAKU" BY REFERENCE W-KAKU "1" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       HAJIME.
           PERFORM  INI-RTN     THRU  INI-EX.
           PERFORM  MAIN-RTN    THRU  MAIN-EX.
           PERFORM  CLSE-ENT     THRU  CLSE-EXT.
           CALL "DB_Close".
           STOP  RUN.
       INI-RTN.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "CLR-TORIMEI" CLR-TORIMEI "p"
                                  RETURNING RESU.
           CALL "SD_Screen_Output" USING "GR0700" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TK_PNAME1 "SHARED" BY REFERENCE TK_IDLST "1"
            "TK-KEY" BY REFERENCE TK-KEY.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
       INI-EX.
           EXIT.
       MAIN-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-ACT "ACP-ACT" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT = "P9"
               GO  TO  MAIN-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-RTN
           END-IF.
           CALL "SD_Output" USING "ACP-ACT" ACP-ACT "p"
                                  RETURNING RESU.
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO  TO  MAIN-RTN
           END-IF.
       MAIN-005.
           CALL "SD_Output" USING "CLR-AREA2" CLR-AREA2 "p"
                                  RETURNING RESU.
           INITIALIZE  W-AREA2.
           IF  W-ACT NOT = 1
               GO  TO  MAIN-010
           END-IF.
           PERFORM  SET-RTN      THRU  SET-EX.
           CALL "SD_Output" USING "ACP-TORIHIKISAKICD"
                                  ACP-TORIHIKISAKICD "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "CLR-AREA3" CLR-AREA3 "p"
                                  RETURNING RESU.
           INITIALIZE  W-AREA3.
           GO  TO  MAIN-040.
       MAIN-010.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TORIHIKISAKICD "ACP-TORIHIKISAKICD"
                 "9" "5" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-010
           END-IF.
           IF  W-TORIHIKISAKICD     <  30000
               GO  TO  MAIN-010
           END-IF.
           CALL "SD_Output" USING "CLR-AREA3" CLR-AREA3 "p"
                                  RETURNING RESU.
           INITIALIZE  W-AREA3.
           MOVE  W-TORIHIKISAKICD     TO  TK-KEY.
      *           READ  TK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TK_PNAME1 BY REFERENCE TK-REC " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                  RETURNING RESU
               GO  TO  MAIN-010
           END-IF.
           MOVE  TK-NAMEN    TO  W-TORIHIKISAKIMEI.
           MOVE  TK-PRC      TO  W-PRC.
           MOVE  TK-BKC      TO  W-BKC.
           MOVE  TK-TCD      TO  W-TCD.
           CALL "SD_Output" USING "ACP-TORIHIKISAKIMEI"
                                  ACP-TORIHIKISAKIMEI "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACP-PRC" ACP-PRC "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACP-BKC" ACP-BKC "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACP-TCD" ACP-TCD "p"
                                  RETURNING RESU.
           IF  W-ACT NOT = 2
               GO  TO  MAIN-050
           END-IF.
       MAIN-040.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TORIHIKISAKIMEI "ACP-TORIHIKISAKIMEI"
                 "N" "20" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  W-ACT   =  1
                   GO  TO  MAIN-RTN
                 ELSE
                   GO  TO  MAIN-010
               END-IF
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-040
           END-IF.
           CALL "SD_Output" USING "ACP-TORIHIKISAKIMEI"
                                  ACP-TORIHIKISAKIMEI "p"
                                  RETURNING RESU.
       MAIN-045.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-PRC "ACP-PRC"
                 "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-040
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-045
           END-IF.
           CALL "SD_Output" USING "ACP-PRC"  ACP-PRC "p"
                                  RETURNING RESU.
       MAIN-047.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-BKC "ACP-BKC"
                 "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-045
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-047
           END-IF.
           CALL "SD_Output" USING "ACP-BKC" ACP-BKC "p"
                                  RETURNING RESU.
       MAIN-048.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TCD "ACP-TCD"
                 "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-047
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-048
           END-IF.
           CALL "SD_Output" USING "ACP-TCD" ACP-TCD "p"
                                  RETURNING RESU.
           IF  W-TCD     =  ZERO
               CALL "SD_Output" USING "CLR-NAME" CLR-NAME "p"
                                  RETURNING RESU
               GO  TO  MAIN-050
           END-IF.
           MOVE  W-TCD                 TO  T-TCD.
      *           READ  T-M   WITH UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE    TO  ERR-MSGN
               MOVE  "得意先マスタ　なし"  TO  ERR-MSGN
               CALL "SD_Output" USING "DISP-MSG-01" DISP-MSG-01 "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                  RETURNING RESU
               GO  TO  MAIN-048
           END-IF.
           CALL "SD_Output" USING "ACP-NAME" ACP-NAME "p"
                                         RETURNING RESU.
       MAIN-050.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  W-ACT = 3
                   GO  TO  MAIN-010
               ELSE
                   GO  TO  MAIN-048
               END-IF
           END-IF.
           IF  W-KAKU = 9                                               = "02"
               CALL "SD_Output" USING "CAN-01" CAN-01 "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "CLR-AREA1" CLR-AREA1 "p"
                                  RETURNING RESU
               INITIALIZE  W-AREA1
               GO  TO  MAIN-RTN
           END-IF.
           IF  W-KAKU NOT = 1                                           = "04"
               GO  TO  MAIN-050
           END-IF.
           PERFORM  KOU-RTN     THRU  KOU-EX.
           CALL "SD_Output" USING "OK-01" OK-01 "p"
                                  RETURNING RESU.
           GO  TO  MAIN-005.
       MAIN-EX.
           EXIT.
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TK_IDLST TK_PNAME1.
       CLSE-EXT.
           EXIT.
       KOU-RTN.
           IF  W-ACT = 1
               PERFORM  WRITE-RTN     THRU  WRITE-EX
           END-IF.
           IF  W-ACT = 2
               PERFORM  REWRITE-RTN   THRU  REWRITE-EX
           END-IF.
           IF  W-ACT = 3
               PERFORM  DELETE-RTN    THRU  DELETE-EX
           END-IF.
       KOU-EX.
           EXIT.
       WRITE-RTN.
           MOVE  SPACE     TO  TK-REC.
           INITIALIZE  TK-REC.
           MOVE  W-TORIHIKISAKICD      TO  TK-KEY.                      取引ＣＤ
           MOVE  W-TORIHIKISAKIMEI     TO  TK-NAMEN.                    取引先名
           MOVE  W-PRC                 TO  TK-PRC.
           MOVE  W-BKC                 TO  TK-BKC.
           MOVE  W-TCD                 TO  TK-TCD.
           MOVE  TK-KEY                TO  ERR-K.
      *           WRITE  TK-REC  INVALID
      *///////////////
           CALL "DB_Insert" USING
            TK_PNAME1 TK_LNAME TK-REC RETURNING RET.
           IF  RET = 1
               MOVE  "TK"     TO  ERR-F
               MOVE  "W"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       WRITE-EX.
           EXIT.
       REWRITE-RTN.
           MOVE  W-TORIHIKISAKICD      TO  TK-KEY.                      取引ＣＤ
           MOVE  W-TORIHIKISAKIMEI     TO  TK-NAMEN.                    取引先名
           MOVE  W-PRC                 TO  TK-PRC.
           MOVE  W-BKC                 TO  TK-BKC.
           MOVE  W-TCD                 TO  TK-TCD.
           MOVE  TK-KEY                TO  ERR-K.
      *           REWRITE  TK-REC  INVALID
      *///////////////
           CALL "DB_Update" USING
            TK_PNAME1 TK_LNAME TK-REC RETURNING RET.
           IF  RET = 1
               MOVE  "TK"     TO  ERR-F
               MOVE  "R"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       REWRITE-EX.
           EXIT.
       DELETE-RTN.
           MOVE  W-TORIHIKISAKICD      TO  TK-KEY.                      取引ＣＤ
           MOVE  W-TORIHIKISAKIMEI     TO  TK-NAMEN.                    取引先名
           MOVE  W-PRC                 TO  TK-PRC.
           MOVE  W-BKC                 TO  TK-BKC.
           MOVE  W-TCD                 TO  TK-TCD.
           MOVE  TK-KEY                TO  ERR-K.
      *           DELETE  TK  INVALID
      *///////////////
           CALL "DB_Delete" USING TK_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "TK"     TO  ERR-F
               MOVE  "D"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       DELETE-EX.
           EXIT.
       SET-RTN.
           MOVE  30000             TO  W-TORIHIKISAKICD.
       SET-010.
           ADD   1                 TO  W-TORIHIKISAKICD.
           MOVE  W-TORIHIKISAKICD  TO  TK-KEY.
      *           READ   TK       WITH  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TK_PNAME1 BY REFERENCE TK-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  SET-EX
           END-IF.
           GO  TO  SET-010.
       SET-EX.
           EXIT.
       COPY  LPMSG_PR.

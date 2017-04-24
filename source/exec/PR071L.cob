       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         PR071L.
       AUTHOR.             KAMASAKA    1995/10/05.
      **********************************************
      ******    取引先マスタ　メンテナンス    ******
      **********************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       INPUT-OUTPUT        SECTION.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       01  HEAD1.
           02  W-20K               PIC X(05)    VALUE  X"1A24212474".
           02  F                   PIC X(40)    VALUE  SPACE.
           02  F                   PIC N(23)    VALUE
                 "＊＊＊　　取引先マスタ　メンテナンス　　＊＊＊".
           02  F                   PIC X(22)    VALUE  SPACE.
           02  F                   PIC X(05)    VALUE  "DATE ".
           02  H-DATE              PIC 99/99/99.
           02  F                   PIC X(07)    VALUE  "     P.".
           02  H-PAGE              PIC Z9.
       01  HEAD2.
           02  W-15K               PIC X(05)    VALUE  X"1A24212078".
           02  F                   PIC X(01)    VALUE  SPACE.
           02  F                   PIC X(04)    VALUE  "ｺｰﾄﾞ".
           02  F                   PIC X(01)    VALUE  SPACE.
           02  F                   PIC N(04)    VALUE  "取引先名".
           02  F                   PIC X(09)    VALUE  SPACE.
           02  F                   PIC N(02)    VALUE  "作表".
           02  F                   PIC X(01)    VALUE  SPACE.
           02  F                   PIC N(02)    VALUE  "部門".
           02  F                   PIC N(04)    VALUE  "　得意先".
           02  F                   PIC X(02)    VALUE  SPACE.
           02  F                   PIC N(02)    VALUE  "日付".
           02  F                   PIC X(03)    VALUE  SPACE.
           02  F                   PIC X(01)    VALUE  ":".
           02  F                   PIC X(03)    VALUE  SPACE.
           02  F                   PIC X(04)    VALUE  "ｺｰﾄﾞ".
           02  F                   PIC X(01)    VALUE  SPACE.
           02  F                   PIC N(04)    VALUE  "取引先名".
           02  F                   PIC X(09)    VALUE  SPACE.
           02  F                   PIC N(02)    VALUE  "作表".
           02  F                   PIC X(01)    VALUE  SPACE.
           02  F                   PIC N(02)    VALUE  "部門".
           02  F                   PIC N(04)    VALUE  "　得意先".
           02  F                   PIC X(02)    VALUE  SPACE.
           02  F                   PIC N(02)    VALUE  "日付".
           02  F                   PIC X(03)    VALUE  SPACE.
           02  F                   PIC X(01)    VALUE  ":".
           02  F                   PIC X(03)    VALUE  SPACE.
           02  F                   PIC X(04)    VALUE  "ｺｰﾄﾞ".
           02  F                   PIC X(01)    VALUE  SPACE.
           02  F                   PIC N(04)    VALUE  "取引先名".
           02  F                   PIC X(09)    VALUE  SPACE.
           02  F                   PIC N(02)    VALUE  "作表".
           02  F                   PIC X(01)    VALUE  SPACE.
           02  F                   PIC N(02)    VALUE  "部門".
           02  F                   PIC N(04)    VALUE  "　得意先".
           02  F                   PIC X(02)    VALUE  SPACE.
           02  F                   PIC N(02)    VALUE  "日付".
           02  F                   PIC X(01)    VALUE  SPACE.
       01  W-P.
           02  W-PD        OCCURS  58.
               03  P-15K           PIC X(05).
               03  P-KEY01         PIC 9(05).
               03  F               PIC X(01).
               03  P-NAME01        PIC N(10).
               03  F               PIC X(01).
               03  P-PRC01         PIC 9(02).
               03  F               PIC X(02).
               03  P-BKC01         PIC 9(02).
               03  F               PIC X(02).
               03  P-TCD01         PIC 9(04).
               03  F               PIC X(01).
               03  P-NG01          PIC 99/99.
               03  F               PIC X(02).
               03  P-X01           PIC X(01).
               03  F               PIC X(02).
               03  P-KEY02         PIC 9(05).
               03  F               PIC X(01).
               03  P-NAME02        PIC N(10).
               03  F               PIC X(01).
               03  P-PRC02         PIC 9(02).
               03  F               PIC X(02).
               03  P-BKC02         PIC 9(02).
               03  F               PIC X(02).
               03  P-TCD02         PIC 9(04).
               03  F               PIC X(01).
               03  P-NG02          PIC 99/99.
               03  F               PIC X(02).
               03  P-X02           PIC X(01).
               03  F               PIC X(02).
               03  P-KEY03         PIC 9(05).
               03  F               PIC X(01).
               03  P-NAME03        PIC N(10).
               03  F               PIC X(01).
               03  P-PRC03         PIC 9(02).
               03  F               PIC X(02).
               03  P-BKC03         PIC 9(02).
               03  F               PIC X(02).
               03  P-TCD03         PIC 9(04).
               03  F               PIC X(01).
               03  P-NG03          PIC 99/99.
               03  P-20K           PIC X(05).
       01  W-DATA.
           02  W-PAGE              PIC 9(02)   VALUE ZERO.
           02  W-SKEY              PIC 9(05).
           02  W-EKEY              PIC 9(05) VALUE 99999.
           02  W-DMM               PIC 9(01).
           02  W-PC                PIC 9(01).
           02  W-LD                PIC 9(02).
           02  W-CD                PIC 9(02).
       01  W-STAT.
           02  HTB                 PIC X(02)    VALUE  "01".
           02  SKP                 PIC X(02)    VALUE  "06".
           02  BTB                 PIC X(02)    VALUE  "09".
           02  PF9                 PIC X(02)    VALUE  "P9".
       01  ERR-STAT                PIC X(02).
      *
           COPY TKLIB.
      *
       77  SP-R               PIC  X(204).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL     PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER    PIC N(23)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER    PIC N(23)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER    PIC N(23)    VALUE
                 "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER    PIC N(23)    VALUE
                 "＊＊＊　　取引先マスタ　メンテナンス　　＊＊＊".
           02  FILLER    PIC N(23)    VALUE
                 "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER    PIC N(23)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER    PIC N(23)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER    PIC X(20)    VALUE
                   "ｺｰﾄﾞ  00000 ～ 99999".
           02  FILLER    PIC X(22)    VALUE
                   "確認　OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SKEY        PIC 9(05).
           02  A-EKEY        PIC 9(05).
           02  A-DMM         PIC 9(01).
       01  C-ERR.
           02  FILLER.
               03  E-STAT    PIC X(02).
               03  E-ME1     PIC X(17)  VALUE
                     "***  DATA ﾅｼ  ***".
               03  E-ME98    PIC X(05)  VALUE  X"1B4A05".
               03  E-ME99    PIC X(05)  VALUE  X"1B4205".
               03  E-CL      PIC X(50)  VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "364" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "46" " " "C-MID"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "46" "01C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "46" "02C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "46" "03C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "46" "04C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "46" "05C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "46" "06C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "13" "20" "20" "07C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "22" "34" "22" "08C-MID" " "
            RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "11" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SKEY" "9" "13" "26" "5" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SKEY" BY REFERENCE W-SKEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EKEY" "9" "13" "35" "5" "A-SKEY" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EKEY" BY REFERENCE W-EKEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "22" "51" "1" "A-EKEY" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "79" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "79" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
            "E-STAT" BY REFERENCE ERR-STAT "2" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p"
                                         RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" TK_PNAME1 "SHARED" BY REFERENCE TK_IDLST "1"
            "TK-KEY" BY REFERENCE TK-KEY.
           ACCEPT  H-DATE  FROM  DATE.
       M-10.
           CALL "SD_Accept" USING
                 BY REFERENCE A-SKEY "A-SKEY" "9" "5"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU.
           IF  ESTAT = PF9
               GO  TO  M-95
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING
                 BY REFERENCE A-EKEY "A-EKEY" "9" "5"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-10
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING
                 BY REFERENCE A-DMM "A-DMM" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-15
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-20
           END-IF.
           IF  W-DMM = 9
               GO  TO  M-10
           END-IF.
           IF  W-DMM NOT = 1
               GO  TO  M-20
           END-IF.
           MOVE  W-SKEY  TO  TK-KEY.
      *           START  TK  KEY NOT < TK-KEY  INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            TK_PNAME1 "TK-KEY" " NOT < " TK-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO  TO  M-10
           END-IF.
      *           READ  TK  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TK_PNAME1 BY REFERENCE TK-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO  TO  M-10
           END-IF.
           IF  TK-KEY > W-EKEY
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO  TO  M-10
           END-IF.
           PERFORM S-45 THRU S-55.
           MOVE  ZERO   TO  W-LD  W-CD  W-PC.
       M-25.
           PERFORM  S-20  THRU  S-25.
           IF  W-CD NOT = ZERO
               GO  TO  M-26
           END-IF.
           MOVE  ":"         TO  P-X01(W-LD).
           MOVE  TK-KEY      TO  P-KEY01(W-LD).
           MOVE  TK-NAMEN    TO  P-NAME01(W-LD).
           IF  TK-PRC   NOT = ZERO
               MOVE  TK-PRC      TO  P-PRC01(W-LD)
           END-IF.
           IF  TK-BKC   NOT = ZERO
               MOVE  TK-BKC      TO  P-BKC01(W-LD)
           END-IF.
           IF  TK-TCD   NOT = ZERO
               MOVE  TK-TCD      TO  P-TCD01(W-LD)
           END-IF.
           IF  TK-NG    NOT = ZERO
               MOVE TK-NG        TO  P-NG01(W-LD)
           END-IF.
       M-26.
           IF  W-CD NOT = 01
               GO  TO  M-27
           END-IF.
           MOVE  ":"         TO  P-X02(W-LD).
           MOVE  TK-KEY      TO  P-KEY02(W-LD).
           MOVE  TK-NAMEN    TO  P-NAME02(W-LD).
           IF  TK-PRC   NOT = ZERO
               MOVE  TK-PRC      TO  P-PRC02(W-LD)
           END-IF.
           IF  TK-BKC   NOT = ZERO
               MOVE  TK-BKC      TO  P-BKC02(W-LD)
           END-IF.
           IF  TK-TCD   NOT = ZERO
               MOVE  TK-TCD      TO  P-TCD02(W-LD)
           END-IF.
           IF  TK-NG NOT = ZERO
               MOVE TK-NG        TO  P-NG02(W-LD)
           END-IF.
       M-27.
           IF  W-CD NOT = 02
               GO  TO  M-28
           END-IF.
           MOVE  TK-KEY      TO  P-KEY03(W-LD).
           MOVE  TK-NAMEN    TO  P-NAME03(W-LD).
           IF  TK-PRC   NOT = ZERO
               MOVE  TK-PRC      TO  P-PRC03(W-LD)
           END-IF.
           IF  TK-BKC   NOT = ZERO
               MOVE  TK-BKC      TO  P-BKC03(W-LD)
           END-IF.
           IF  TK-TCD   NOT = ZERO
               MOVE  TK-TCD      TO  P-TCD03(W-LD)
           END-IF.
           IF  TK-NG NOT = ZERO
               MOVE TK-NG        TO  P-NG03(W-LD)
           END-IF.
       M-28.
      *           READ  TK  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TK_PNAME1 BY REFERENCE TK-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-30
           END-IF.
           IF  TK-KEY > W-EKEY
               GO  TO  M-30
           END-IF.
           GO  TO  M-25.
       M-30.
           PERFORM  S-30  THRU  S-40.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE TK_IDLST TK_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.
       S-05.
           MOVE   SPACE  TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD    1       TO  W-PAGE.
           MOVE   W-PAGE  TO  H-PAGE.
           MOVE   SPACE   TO  SP-R.
           MOVE   HEAD1   TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE   TO  SP-R.
           MOVE   HEAD2   TO  SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           ADD  1  TO  W-LD.
           IF  W-LD NOT = 59
               GO  TO  S-25
           END-IF.
           IF  W-CD NOT = 2
               ADD   1     TO  W-CD
               MOVE  ZERO  TO  W-LD
               GO  TO  S-20
           END-IF.
           PERFORM  S-30  THRU  S-40.
           PERFORM  S-45  THRU  S-55.
           MOVE  ZERO   TO  W-LD  W-CD.
           GO  TO  S-20.
       S-25.
           EXIT.
       S-30.
           IF  W-PC = ZERO
               MOVE  5  TO  W-PC
               CALL "PR_Open" RETURNING RESP
               PERFORM  S-10  THRU  S-15
           ELSE
               PERFORM  S-05  THRU  S-15
           END-IF.
           MOVE  ZERO  TO  W-LD.
       S-35.
           ADD  1  TO  W-LD.
           IF  W-LD NOT = 59
               IF  P-X01(W-LD) NOT = SPACE
                   MOVE   SPACE       TO  SP-R
                   MOVE   W-PD(W-LD)  TO  SP-R
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   MOVE   SPACE       TO  SP-R
                   GO  TO  S-35
               END-IF
           END-IF.
       S-40.
           EXIT.
       S-45.
           MOVE SPACE TO W-P.
           MOVE ZERO TO W-LD.
       S-50.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               MOVE SPACE TO W-PD(W-LD)
               MOVE SPACE TO P-NAME01(W-LD) P-NAME02(W-LD)
                             P-NAME03(W-LD)
               MOVE W-15K TO P-15K(W-LD)
               MOVE W-20K TO P-20K(W-LD)
               GO TO S-50
           END-IF.
       S-55.
           EXIT.

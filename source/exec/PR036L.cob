       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         PR036L.
       AUTHOR.             KAMASAKA    1995/10/02.
      **************************************
      ******    損益マスタ　リスト    ******
      **************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       INPUT-OUTPUT        SECTION.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       01  HEAD1.
           02  F                   PIC X(05)    VALUE  X"1A24212474".
           02  F                   PIC X(21)    VALUE  SPACE.
           02  F                   PIC N(19)    VALUE
                 "＊＊＊　　損益マスタ　リスト　　＊＊＊".
           02  F                   PIC X(22)    VALUE  SPACE.
           02  F                   PIC X(05)    VALUE  "DATE ".
           02  H-DATE              PIC 99/99/99.
           02  F                   PIC X(07)    VALUE  "     P.".
           02  H-PAGE              PIC Z9.
       01  HEAD2.
           02  F                   PIC X(05)    VALUE  X"1A24212078".
           02  F                   PIC N(08)    VALUE
                 "ライン№　改行数".
           02  F                   PIC X(01)    VALUE  SPACE.
           02  F                   PIC N(04)    VALUE  "合計区分".
           02  F                   PIC X(01)    VALUE  SPACE.
           02  F                   PIC N(08)    VALUE
                 "科目名（項目名）".
           02  F                   PIC X(03)    VALUE  SPACE.
           02  F                   PIC N(04)    VALUE  "売上区分".
           02  F                   PIC X(01)    VALUE  SPACE.
           02  F                   PIC N(04)    VALUE  "印字区分".
           02  F                   PIC X(03)    VALUE  SPACE.
           02  F                   PIC X(01)    VALUE  ":".
           02  F                   PIC X(03)    VALUE  SPACE.
           02  F                   PIC N(08)    VALUE
                 "ライン№　改行数".
           02  F                   PIC X(01)    VALUE  SPACE.
           02  F                   PIC N(04)    VALUE  "合計区分".
           02  F                   PIC X(01)    VALUE  SPACE.
           02  F                   PIC N(08)    VALUE
                 "科目名（項目名）".
           02  F                   PIC X(03)    VALUE  SPACE.
           02  F                   PIC N(04)    VALUE  "売上区分".
           02  F                   PIC X(01)    VALUE  SPACE.
           02  F                   PIC N(04)    VALUE  "印字区分".
       01  W-P.
           02  W-PD        OCCURS  58.
               03  F               PIC X(02).
               03  P-PL-KEY01      PIC 9(03).
               03  F               PIC X(05).
               03  P-PL-LIN01      PIC 9(01).
               03  F               PIC X(05).
               03  P-PL-GKB01      PIC 9(01).
               03  F               PIC X(03).
               03  P-PL-NAMN01     PIC N(10).
               03  F               PIC X(03).
               03  P-PL-URIKB01    PIC X(01).
               03  F               PIC X(06).
               03  P-PL-PKB01      PIC 9(01).
               03  F               PIC X(05).
               03  P-X01           PIC X(01).
               03  F               PIC X(05).
               03  P-PL-KEY02      PIC 9(03).
               03  F               PIC X(05).
               03  P-PL-LIN02      PIC 9(01).
               03  F               PIC X(05).
               03  P-PL-GKB02      PIC 9(01).
               03  F               PIC X(03).
               03  P-PL-NAMN02     PIC N(10).
               03  F               PIC X(03).
               03  P-PL-URIKB02    PIC X(01).
               03  F               PIC X(06).
               03  P-PL-PKB02      PIC 9(01).
           02  F                   PIC X(05)    VALUE  X"1A24212474".
       01  W-DATA.
           02  W-PAGE              PIC 9(02).
           02  W-SKEY              PIC 9(03).
           02  W-EKEY              PIC 9(03).
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
       01  PL_PR036L.
           02  PL_PNAME1  PIC  X(004)  VALUE "PL-K".
           02  F          PIC  X(001).
           02  PL_LNAME   PIC  X(009)  VALUE "PL_PR036L".
           02  F          PIC  X(001).
           02  PL_KEY1    PIC  X(100)  VALUE SPACE.
           02  PL_KEY2    PIC  X(100)  VALUE SPACE.
           02  PL_SORT    PIC  X(100)  VALUE SPACE.
           02  PL_IDLST   PIC  X(100)  VALUE SPACE.
           02  PL_RES     USAGE  POINTER.
       01  PL-R.
           02  PL-KEY              PIC X(03).
           02  PL-LIN              PIC 9(01).
           02  PL-GKB              PIC 9(01).
           02  PL-NAMN             PIC N(10).
           02  PL-YY.
               03  PL-ZENKI        PIC 9(11).
               03  PL-TOUKI        PIC 9(11).
           02  PL-MM.
               03  PL-ZENMM        PIC 9(11).
               03  PL-TOUMM        PIC 9(11).
           02  PL-URIKB            PIC X(01).
           02  PL-PKB              PIC 9(01).
           02  PL-TANA             PIC 9(01).
           02  PL-YM.
               03  PL-YYWK         PIC 9(02).
               03  PL-MMWK         PIC 9(02).
           02  FILLER              PIC X(09).
       77  F                       PIC  X(001).
      *
       77  SP-R                    PIC X(181).
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
           02  C-CL      PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER    PIC N(19)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER    PIC N(19)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER    PIC N(19)    VALUE
                 "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER    PIC N(19)    VALUE
                 "＊＊＊　　損益マスタ　リスト　　＊＊＊".
           02  FILLER    PIC N(19)    VALUE
                 "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER    PIC N(19)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER    PIC N(19)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER    PIC X(19)    VALUE
                   "ﾗｲﾝ NO   000 ～ 999".
           02  FILLER    PIC X(22)    VALUE
                   "確認　OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SKEY        PIC 9(03).
           02  A-EKEY        PIC 9(03).
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
            "C-MID" " " "0" "0" "307" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING
            "08C-MID" "X" "13" "17" "19" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "22" "34" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SKEY" "9" "13" "26" "3" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SKEY" BY REFERENCE W-SKEY "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EKEY" "9" "13" "33" "3" "A-SKEY" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EKEY" BY REFERENCE W-EKEY "3" "0" RETURNING RESU.
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
           CALL "SD_Output" USING
            "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" PL_PNAME1 "SHARED" BY REFERENCE PL_IDLST "1"
            "PL-KEY" BY REFERENCE PL-KEY.
           MOVE  SPACE  TO  W-P.
           MOVE  ZERO   TO  W-DATA  W-LD  W-CD  W-PC.
           MOVE  999    TO  W-EKEY.
           ACCEPT  H-DATE  FROM  DATE.
       M-10.
           CALL "SD_Accept" USING
                 BY REFERENCE A-SKEY "A-SKEY" "9" "3"
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
                 BY REFERENCE A-EKEY "A-EKEY" "9" "3"
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
           MOVE  W-SKEY  TO  PL-KEY.
      *           START  PL  KEY NOT < PL-KEY  INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            PL_PNAME1 "PL-KEY" " NOT < " PL-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO  TO  M-10
           END-IF.
      *           READ  PL  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" PL_PNAME1 BY REFERENCE PL-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO  TO  M-10
           END-IF.
           IF  PL-KEY > W-EKEY
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO  TO  M-10
           END-IF.
       M-25.
           PERFORM  S-20  THRU  S-25.
           IF  W-CD = ZERO
               MOVE  PL-KEY    TO  P-PL-KEY01(W-LD)
               MOVE  PL-LIN    TO  P-PL-LIN01(W-LD)
               MOVE  PL-GKB    TO  P-PL-GKB01(W-LD)
               MOVE  PL-NAMN   TO  P-PL-NAMN01(W-LD)
               MOVE  PL-URIKB  TO  P-PL-URIKB01(W-LD)
               MOVE  PL-PKB    TO  P-PL-PKB01(W-LD)
               MOVE  ":"       TO  P-X01(W-LD)
           END-IF.
           IF  W-CD = 01
               MOVE  PL-KEY    TO  P-PL-KEY02(W-LD)
               MOVE  PL-LIN    TO  P-PL-LIN02(W-LD)
               MOVE  PL-GKB    TO  P-PL-GKB02(W-LD)
               MOVE  PL-NAMN   TO  P-PL-NAMN02(W-LD)
               MOVE  PL-URIKB  TO  P-PL-URIKB02(W-LD)
               MOVE  PL-PKB    TO  P-PL-PKB02(W-LD)
           END-IF.
      *           READ  PL  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" PL_PNAME1 BY REFERENCE PL-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-30
           END-IF.
           IF  PL-KEY > W-EKEY
               GO  TO  M-30
           END-IF.
           GO  TO  M-25.
       M-30.
           PERFORM  S-30  THRU  S-40.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE PL_IDLST PL_PNAME1.
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
           MOVE  SPACE  TO  W-P.
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

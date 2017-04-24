       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         PR017L.
       AUTHOR.             KAMASAKA.
      **********************************************
      ******    科目マスター（１）　リスト    ******
      **********************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       INPUT-OUTPUT        SECTION.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       01  HEAD1.
           02  F                  PIC X(05)    VALUE  X"1A24212474".
           02  F                  PIC X(10)    VALUE  SPACE.
           02  F                  PIC N(23)    VALUE
                "＊＊＊　　科目マスター（１）　リスト　　＊＊＊".
           02  F                  PIC X(11)    VALUE  SPACE.
           02  F                  PIC X(05)    VALUE  "DATE ".
           02  H-DATE             PIC 99/99/99.
           02  F                  PIC X(07)    VALUE  "     P.".
           02  H-PAGE             PIC Z9.
       01  HEAD2.
           02  F                  PIC X(05)    VALUE  X"1A24212078".
           02  F                  PIC N(02)    VALUE  "科目".
           02  F                  PIC X(05)    VALUE  "ｺｰﾄﾞ ".
           02  F                  PIC N(04)    VALUE  "貸借区分".
           02  F                  PIC X(01)    VALUE  SPACE.
           02  F                  PIC N(06)    VALUE  "科　目　名　".
           02  F                  PIC X(14)    VALUE  SPACE.
           02  F                  PIC N(04)    VALUE  "前日残高".
           02  F                  PIC X(01)    VALUE  SPACE.
           02  F                  PIC N(02)    VALUE  "手形".
           02  F                  PIC X(01)    VALUE  "･".
           02  F                  PIC N(10)    VALUE
                "銀行区分　補助簿区分".
           02  F                  PIC X(01)    VALUE  SPACE.
           02  F                  PIC N(02)    VALUE  "貸借".
           02  F                  PIC X(01)    VALUE  "･".
           02  F                  PIC N(04)    VALUE  "損益区分".
           02  F                  PIC X(01)    VALUE  SPACE.
           02  F                  PIC N(04)    VALUE  "経費区分".
           02  F                  PIC X(01)    VALUE  SPACE.
           02  F                  PIC N(04)    VALUE  "元帳区分".
       01  W-P.
           02  F                  PIC X(02).
           02  P-AOCTCD1          PIC 9(01).
           02  P-AOCTCD2          PIC 9(01).
           02  P-AOCTCD3          PIC 9(01).
           02  P-AOCTCD4          PIC 9(01).
           02  F                  PIC X(05).
           02  P-DR-CR            PIC 9(01).
           02  F                  PIC X(03).
           02  P-KNGNMN           PIC N(10).
           02  P-BFDZN            PIC ------,---,--9.
           02  F                  PIC X(07).
           02  P-TEG-BAN          PIC 9(02).
           02  F                  PIC X(08).
           02  P-HOJYO            PIC 9(01).
           02  F                  PIC X(10).
           02  P-BS-PL            PIC 9(01).
           02  F                  PIC X(06).
           02  P-KEIHI            PIC 9(01).
           02  F                  PIC X(06).
           02  P-MOTKB            PIC 9(01).
           02  F                  PIC X(05)    VALUE  X"1A24212474".
       01  W-DATA.
           02  W-PAGE             PIC 9(02).
           02  W-DMM              PIC 9(01).
           02  W-SKEY             PIC 9(04).
           02  W-EKEY             PIC 9(04).
       01  W-STAT.
           02  HTB                PIC X(02)    VALUE  "01".
           02  SKP                PIC X(02)    VALUE  "06".
           02  BTB                PIC X(02)    VALUE  "09".
           02  PF9                PIC X(02)    VALUE  "P9".
       01  ERR-STAT               PIC X(02).
      *
       01  AM_PR017L.
           02  AM_PNAME1  PIC  X(008)  VALUE "KAMOKU-K".
           02  F          PIC  X(001).
           02  AM_LNAME   PIC  X(009)  VALUE "AM_PR017L".
           02  F          PIC  X(001).
           02  AM_KEY1    PIC  X(100)  VALUE SPACE.
           02  AM_KEY2    PIC  X(100)  VALUE SPACE.
           02  AM_SORT    PIC  X(100)  VALUE SPACE.
           02  AM_IDLST   PIC  X(100)  VALUE SPACE.
           02  AM_RES     USAGE  POINTER.
       01  AM-R.
           02  AM-KEY.
               03  AOCTCD1        PIC 9(01).
               03  AOCTCD2        PIC 9(01).
               03  AOCTCD3        PIC 9(01).
               03  AOCTCD4        PIC 9(01).
           02  DR-CR              PIC 9(01).
           02  DDRCR.
               03  BFDZN          PIC 9(11).
               03  DDR            PIC 9(11).
               03  DCR            PIC 9(11).
           02  TEG-BAN            PIC 9(02).
           02  TANA               PIC 9(01).
           02  HOJYO              PIC 9(01).
           02  BS-PL              PIC 9(01).
           02  BSKOU.
               03  BSGOU1.
                   04  BSKEY1     PIC 9(03).
                   04  BSDA-CR1   PIC 9(01).
                   04  BSCOM1     PIC 9(01).
               03  BSGOU2.
                   04  BSKEY2     PIC 9(03).
                   04  BSDA-CR2   PIC 9(01).
                   04  BSCOM2     PIC 9(01).
               03  BSGOU3.
                   04  BSKEY3     PIC 9(03).
                   04  BSDA-CR3   PIC 9(01).
                   04  BSCOM3     PIC 9(01).
               03  BSGOU4.
                   04  BSKEY4     PIC 9(03).
                   04  BSDA-CR4   PIC 9(01).
                   04  BSCOM4     PIC 9(01).
               03  BSGOU5.
                   04  BSKEY5     PIC 9(03).
                   04  BSDA-CR5   PIC 9(01).
                   04  BSCOM5     PIC 9(01).
               03  BSGOU6.
                   04  BSKEY6     PIC 9(03).
                   04  BSDA-CR6   PIC 9(01).
                   04  BSCOM6     PIC 9(01).
           02  PLKOU.
               03  PLGOU1.
                   04  PLKEY1     PIC 9(03).
                   04  PLCOM1     PIC 9(01).
               03  PLGOU2.
                   04  PLKEY2     PIC 9(03).
                   04  PLCOM2     PIC 9(01).
               03  PLGOU3.
                   04  PLKEY3     PIC 9(03).
                   04  PLCOM3     PIC 9(01).
               03  PLGOU4.
                   04  PLKEY4     PIC 9(03).
                   04  PLCOM4     PIC 9(01).
               03  PLGOU5.
                   04  PLKEY5     PIC 9(03).
                   04  PLCOM5     PIC 9(01).
               03  PLGOU6.
                   04  PLKEY6     PIC 9(03).
                   04  PLCOM6     PIC 9(01).
               03  PLGOU7.
                   04  PLKEY7     PIC 9(03).
                   04  PLCOM7     PIC 9(01).
               03  PLGOU8.
                   04  PLKEY8     PIC 9(03).
                   04  PLCOM8     PIC 9(01).
               03  PLGOU9.
                   04  PLKEY9     PIC 9(03).
                   04  PLCOM9     PIC 9(01).
               03  PLGOU10.
                   04  PLKEY10    PIC 9(03).
                   04  PLCOM10    PIC 9(01).
               03  PLGOU11.
                   04  PLKEY11    PIC 9(03).
                   04  PLCOM11    PIC 9(01).
               03  PLGOU12.
                   04  PLKEY12    PIC 9(03).
                   04  PLCOM12    PIC 9(01).
           02  KEIHI              PIC 9(01).
           02  MOTKB              PIC 9(01).
           02  GNKOU.
               03  GNGOU1.
                   04  GNKEY1     PIC 9(03).
                   04  GNCOM1     PIC 9(01).
               03  GNGOU2.
                   04  GNKEY2     PIC 9(03).
                   04  GNCOM2     PIC 9(01).
               03  GNGOU3.
                   04  GNKEY3     PIC 9(03).
                   04  GNCOM3     PIC 9(01).
               03  GNGOU4.
                   04  GNKEY4     PIC 9(03).
                   04  GNCOM4     PIC 9(01).
               03  GNGOU5.
                   04  GNKEY5     PIC 9(03).
                   04  GNCOM5     PIC 9(01).
               03  GNGOU6.
                   04  GNKEY6     PIC 9(03).
                   04  GNCOM6     PIC 9(01).
               03  GNGOU7.
                   04  GNKEY7     PIC 9(03).
                   04  GNCOM7     PIC 9(01).
               03  GNGOU8.
                   04  GNKEY8     PIC 9(03).
                   04  GNCOM8     PIC 9(01).
               03  GNGOU9.
                   04  GNKEY9     PIC 9(03).
                   04  GNCOM9     PIC 9(01).
               03  GNGOU10.
                   04  GNKEY10    PIC 9(03).
                   04  GNCOM10    PIC 9(01).
               03  GNGOU11.
                   04  GNKEY11    PIC 9(03).
                   04  GNCOM11    PIC 9(01).
               03  GNGOU12.
                   04  GNKEY12    PIC 9(03).
                   04  GNCOM12    PIC 9(01).
           02  SKNKOU.
               03  SKNKEY         PIC 9(03).
               03  SKNCOM         PIC 9(01).
               03  SKNHAT         PIC 9(01).
           02  FILLER             PIC X(80).
       77  F                      PIC X(001).
      *
       01  KNG_PR017L.
           02  KNG_PNAME1  PIC  X(010)  VALUE "KAMOKU-KNG".
           02  F           PIC  X(001).
           02  KNG_LNAME   PIC  X(010)  VALUE "KNG_PR017L".
           02  F           PIC  X(001).
           02  KNG_KEY1    PIC  X(100)  VALUE SPACE.
           02  KNG_KEY2    PIC  X(100)  VALUE SPACE.
           02  KNG_SORT    PIC  X(100)  VALUE SPACE.
           02  KNG_IDLST   PIC  X(100)  VALUE SPACE.
           02  KNG_RES     USAGE  POINTER.
       01  KNG-R.
           02  KNG-KEY.
               03  K-ACCD         PIC 9(04).
               03  K-HOCD         PIC 9(04).
           02  KNGNMN             PIC N(10).
           02  KNGTAX             PIC X(01).
           02  FILLER             PIC X(03).
       77  F                      PIC  X(001).
      *
           COPY LSPF.
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
           02  C-CL     PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER    PIC N(23)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER    PIC N(23)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER    PIC N(23)    VALUE
                 "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER    PIC N(23)    VALUE
                 "＊＊＊　　科目マスター（１）　リスト　　＊＊＊".
           02  FILLER    PIC N(23)    VALUE
                 "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER    PIC N(23)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER    PIC N(23)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER    PIC X(21)    VALUE
                   "科目ｺｰﾄﾞ 0000 ～ 9999".
           02  FILLER    PIC X(22)    VALUE
                   "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SKEY        PIC 9(04).
           02  A-EKEY        PIC 9(04).
           02  A-DMM         PIC 9(01).
       01  C-ERR.
           02  FILLER.
               03  E-STAT    PIC X(02).
               03  E-ME1     PIC X(18)  VALUE
                     "***  DATA  ﾅｼ  ***".
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
            "C-MID" " " "0" "0" "365" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "46" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "13" "17" "21" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "22" "34" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SKEY" "9" "13" "26" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SKEY" BY REFERENCE W-SKEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EKEY" "9" "13" "34" "4" "SKEY" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EKEY" BY REFERENCE W-EKEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "22" "51" "1" "EKEY" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "80" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "80" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
            "E-STAT" BY REFERENCE ERR-STAT "2" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "18" "E-STAT" " " RETURNING RESU.
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
            "INPUT" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE  ZERO  TO  W-DATA.
           MOVE  9999  TO  W-EKEY.
       M-10.
           CALL "SD_Accept" USING
                 BY REFERENCE A-SKEY "A-SKEY" "9" "4"
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
                 BY REFERENCE A-EKEY "A-EKEY" "9" "4"
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
           MOVE  W-SKEY  TO  AM-KEY.
      *           START  AM  KEY NOT < AM-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            AM_PNAME1 "AM-KEY" " NOT < " AM-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO  TO  M-10
           END-IF
      *           READ  AM  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" AM_PNAME1 BY REFERENCE AM-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO  TO  M-10
           END-IF
           IF  AM-KEY > W-EKEY
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO  TO  M-10
           END-IF.
           MOVE  SPACE   TO  W-P.
           ACCEPT   H-DATE  FROM  DATE.
           PERFORM  S-10  THRU  S-15.
       M-25.
           MOVE  AM-KEY  TO  K-ACCD.
           MOVE  ZERO    TO  K-HOCD.
      *           READ  KNG  WITH  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  "マスター　なし　"  TO  KNGNMN
           END-IF.
           MOVE  AOCTCD1  TO  P-AOCTCD1.
           MOVE  AOCTCD2  TO  P-AOCTCD2.
           MOVE  AOCTCD3  TO  P-AOCTCD3.
           MOVE  AOCTCD4  TO  P-AOCTCD4.
           MOVE  DR-CR    TO  P-DR-CR.
           MOVE  KNGNMN   TO  P-KNGNMN.
           MOVE  BFDZN    TO  P-BFDZN.
           MOVE  TEG-BAN  TO  P-TEG-BAN.
           MOVE  HOJYO    TO  P-HOJYO.
           MOVE  BS-PL    TO  P-BS-PL.
           MOVE  KEIHI    TO  P-KEIHI.
           MOVE  MOTKB    TO  P-MOTKB.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM  S-05  THRU  S-15
           END-IF.
           MOVE   SPACE  TO  SP-R.
           MOVE   W-P    TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *           READ  AM  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" AM_PNAME1 BY REFERENCE AM-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-95
           END-IF.
           IF  AM-KEY > W-EKEY
               GO  TO  M-95
           END-IF.
           GO  TO  M-25.
       M-95.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.
       S-05.
           MOVE   SPACE   TO     SP-R.
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

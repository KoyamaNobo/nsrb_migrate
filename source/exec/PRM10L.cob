       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PRM10L.
       AUTHOR.           KAMASAKA.
      **************************************
      ******   漢字科目マスタリスト   ******
      **************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       DATA              DIVISION.
       WORKING-STORAGE   SECTION.
       01  HEAD1.
           02  F           PIC X(05)  VALUE  X"1A24212474".
           02  F           PIC X(33)  VALUE  SPACE.
           02  F           PIC X(42)  VALUE
                  "＊＊＊    漢字科目マスタ  リスト    ＊＊＊".
           02  F           PIC X(34)  VALUE  SPACE.
           02  F           PIC X(5)   VALUE  "DATE".
           02  H-DATE      PIC 99/99/99.
           02  F           PIC X(5)   VALUE  SPACE.
           02  F           PIC X(2)   VALUE  "P.".
           02  H-PAGE      PIC Z9.
       01  HEAD2.
           02  F           PIC N(2)   VALUE  "科目".
           02  F           PIC X(2)   VALUE  SPACE.
           02  F           PIC N(2)   VALUE  "補助".
           02  F           PIC X(2)   VALUE  SPACE.
           02  F           PIC N(4)   VALUE  "科目名称".
           02  F           PIC X(11)  VALUE  SPACE.
           02  F           PIC N(4)   VALUE  "課税区分".
           02  F           PIC X(3)   VALUE  SPACE.
           02  F           PIC X      VALUE  ":".
           02  F           PIC X(3)   VALUE  SPACE.
           02  F           PIC N(2)   VALUE  "科目".
           02  F           PIC X(2)   VALUE  SPACE.
           02  F           PIC N(2)   VALUE  "補助".
           02  F           PIC X(2)   VALUE  SPACE.
           02  F           PIC N(4)   VALUE  "科目名称".
           02  F           PIC X(11)  VALUE  SPACE.
           02  F           PIC N(4)   VALUE  "課税区分".
           02  F           PIC X(3)   VALUE  SPACE.
           02  F           PIC X      VALUE  ":".
           02  F           PIC X(3)   VALUE  SPACE.
           02  F           PIC N(2)   VALUE  "科目".
           02  F           PIC X(2)   VALUE  SPACE.
           02  F           PIC N(2)   VALUE  "補助".
           02  F           PIC X(2)   VALUE  SPACE.
           02  F           PIC N(4)   VALUE  "科目名称".
           02  F           PIC X(11)  VALUE  SPACE.
           02  F           PIC N(4)   VALUE  "課税区分".
       01  W-P.
           02  W-PD     OCCURS  58.
               03  P-KC01      PIC 9(4).
               03  F           PIC X(2).
               03  P-HC01      PIC 9(4).
               03  F           PIC X(2).
               03  P-KM01      PIC N(10).
               03  F           PIC X(4).
               03  P-KK01      PIC X.
               03  F           PIC X(5).
               03  P-X01       PIC X(1).
               03  F           PIC X(3).
               03  P-KC02      PIC 9(4).
               03  F           PIC X(2).
               03  P-HC02      PIC 9(4).
               03  F           PIC X(2).
               03  P-KM02      PIC N(10).
               03  F           PIC X(4).
               03  P-KK02      PIC X.
               03  F           PIC X(5).
               03  P-X02       PIC X(1).
               03  F           PIC X(3).
               03  P-KC03      PIC 9(4).
               03  F           PIC X(2).
               03  P-HC03      PIC 9(4).
               03  F           PIC X(2).
               03  P-KM03      PIC N(10).
               03  F           PIC X(4).
               03  P-KK03      PIC X.
       01  W-DATA.
           02  W-PAGE      PIC 9(2).
           02  W-KC        PIC 9(4).
           02  W-PC        PIC 9(1).
           02  W-LD        PIC 9(2).
           02  W-CD        PIC 9(2).
           02  W-SKEY      PIC 9(4).
           02  W-EKEY      PIC 9(4).
           02  W-DMM       PIC 9(1).
       01  W-STAT.
           02  HTB         PIC X(2)    VALUE  "01".
           02  SKP         PIC X(2)    VALUE  "06".
           02  BTB         PIC X(2)    VALUE  "09".
       01  ERR-STAT        PIC X(2).
      *
       01  KNG_PRM10L.
           02  KNG_PNAME1  PIC  X(010)  VALUE "KAMOKU-KNG".
           02  F           PIC  X(001).
           02  KNG_LNAME   PIC  X(010)  VALUE "KNG_PRM10L".
           02  F           PIC  X(001).
           02  KNG_KEY1    PIC  X(100)  VALUE SPACE.
           02  KNG_KEY2    PIC  X(100)  VALUE SPACE.
           02  KNG_SORT    PIC  X(100)  VALUE SPACE.
           02  KNG_IDLST   PIC  X(100)  VALUE SPACE.
           02  KNG_RES     USAGE  POINTER.
       01  KNG-R.
           02  KNG-KEY.
               03  K-ACCD  PIC 9(4).
               03  K-HOCD  PIC 9(4).
           02  KNGNMN      PIC N(10).
           02  KNGTAX      PIC X.
           02  F           PIC X(3).
       77  F                         PIC  X(001).
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
           02  FILLER   PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC X(34)  VALUE
                  "**********************************".
           02  FILLER  PIC X(34)  VALUE
                  "**********************************".
           02  FILLER  PIC X(34)  VALUE
                  "***                            ***".
           02  FILLER  PIC X(34)  VALUE
                  "***   漢字科目マスタ  リスト   ***".
           02  FILLER  PIC X(34)  VALUE
                  "***                            ***".
           02  FILLER  PIC X(34)  VALUE
                  "**********************************".
           02  FILLER  PIC X(34)  VALUE
                  "**********************************".
           02  FILLER  PIC X(21)  VALUE
                  "科目ｺｰﾄﾞ 0000 ～ 9999".
           02  FILLER  PIC X(22)  VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SKEY        PIC 9(04).
           02  A-EKEY        PIC 9(04).
           02  A-DMM         PIC 9(01).
       01  C-ERR.
           02  FILLER.
               03  E-ME1    PIC X(20)  VALUE
                     "***   DATA  ﾅｼ   ***".
               03  E-ME98   PIC X(005)  VALUE  X"1B4A09".
               03  E-ME99   PIC X(005)  VALUE  X"1B4209".
               03  E-CL     PIC X(050)  VALUE
                   "                                                  ".
       PROCEDURE         DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "281" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "X" "3" "10" "34" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "X" "4" "10" "34" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "X" "5" "10" "34" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "X" "6" "10" "34" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "X" "7" "10" "34" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "X" "8" "10" "34" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "X" "9" "10" "34" "06C-MID" " " RETURNING RESU.
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
            "A-EKEY" "9" "13" "34" "4" "A-SKEY" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EKEY" BY REFERENCE W-EKEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "22" "51" "1" "A-EKEY" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "80" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "80" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "20" " " "01C-ERR" RETURNING RESU.
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
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" KNG_PNAME1 
            "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           MOVE  ZERO  TO  W-DATA.
           MOVE  9999  TO  W-EKEY. 
       M-10.
           CALL "SD_Accept" USING
                 BY REFERENCE A-SKEY "A-SKEY" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU.
           IF  ESTAT = BTB
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
           IF W-SKEY > W-EKEY
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
       M-25.
      *           READ  KNG  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO  TO  M-95
           END-IF.
           IF  KNG-KEY < W-SKEY  OR  > W-EKEY
               GO  TO  M-25
           END-IF.
           ACCEPT  H-DATE  FROM  DATE.
           MOVE  SPACE  TO  W-P.
       M-30.
           PERFORM  S-20  THRU  S-35.
       M-35.
      *           READ  KNG  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-90
           END-IF.
           IF  KNG-KEY > W-EKEY
               GO  TO  M-35
           END-IF.
           GO TO M-30.
       M-90.
           PERFORM  S-50  THRU  S-60.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           IF  W-PC  NOT = ZERO
               CALL "PR_Close" RETURNING RESP
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.
       S-05.
           MOVE   SPACE  TO     SP-R.
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           ADD  1  TO  W-LD.
           IF  W-LD NOT = 59
               GO  TO  S-30
           END-IF.
           MOVE  ZERO  TO  W-KC.
           ADD  1  TO  W-CD.
           IF  W-CD NOT = 3
               MOVE  ZERO  TO  W-LD
               GO  TO  S-20
           END-IF.
           PERFORM  S-50   THRU  S-60.
           MOVE     SPACE  TO    W-P.
           MOVE     ZERO   TO    W-LD  W-CD.
           GO  TO  S-20.
       S-30.
           IF  W-CD = ZERO
               MOVE  ":"     TO  P-X01(W-LD)
               MOVE  K-HOCD  TO  P-HC01(W-LD)
               MOVE  KNGNMN  TO  P-KM01(W-LD)
               MOVE  KNGTAX  TO  P-KK01(W-LD)
               IF  K-ACCD NOT = W-KC
                   MOVE  K-ACCD  TO  P-KC01(W-LD)
               END-IF
           END-IF.
           IF  W-CD = 01
               MOVE  ":"     TO  P-X02(W-LD)
               MOVE  K-HOCD  TO  P-HC02(W-LD)
               MOVE  KNGNMN  TO  P-KM02(W-LD)
               MOVE  KNGTAX  TO  P-KK02(W-LD)
               IF  K-ACCD NOT = W-KC
                   MOVE  K-ACCD  TO  P-KC02(W-LD)
               END-IF
           END-IF.
           IF  W-CD = 02
               MOVE  K-HOCD  TO  P-HC03(W-LD)
               MOVE  KNGNMN  TO  P-KM03(W-LD)
               MOVE  KNGTAX  TO  P-KK03(W-LD)
               IF  K-ACCD NOT = W-KC
                   MOVE  K-ACCD  TO  P-KC03(W-LD)
               END-IF
           END-IF.
           MOVE  K-ACCD  TO  W-KC.
       S-35.
           EXIT.
       S-50.
           IF  W-PC = ZERO
               MOVE  5  TO  W-PC
               CALL "PR_Open" RETURNING RESP
               PERFORM  S-10  THRU  S-15
           ELSE
               PERFORM  S-05  THRU  S-15
           END-IF.
           MOVE  ZERO  TO  W-LD.
       S-55.
           ADD  1  TO  W-LD.
           IF  W-LD NOT = 59
               IF  P-X01(W-LD) NOT = SPACE
                   MOVE   SPACE       TO  SP-R
                   MOVE   W-PD(W-LD)  TO  SP-R
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   MOVE   SPACE       TO  SP-R
                   GO  TO  S-55
               END-IF
           END-IF.
       S-60.
           EXIT.

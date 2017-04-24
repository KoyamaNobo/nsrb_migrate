       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JT500M.
       AUTHOR.          SAKIKO.D
      *********************************************************
      *    PROGRAM         :  類似品マスターメンテナンス      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SJ500M                          *
      *    DATA WRITTN     :  91/08/12                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. NEAC-SYSTEM3100.
       OBJECT-COMPUTER. NEAC-SYSTEM3100.
       DATA    DIVISION.
       WORKING-STORAGE  SECTION.
       77  LCNT               PIC  9(02)   VALUE  90.
       77  PCNT               PIC  9(02)   VALUE  ZERO.
       77  CHK-SW             PIC  9(01).
       77  ERR-STAT           PIC X(2).
       77  OLDKEY             PIC 9(06).
       77  W-KBN              PIC 9(02).
       77  K15                PIC X(05)  VALUE  X"1A24212078".
       01  WORK-AREA.
           02  W-SPA          PIC N(24)   VALUE  SPACE.
           02  HIZUKE.
               03  HI-YY      PIC 9(2).
               03  HI-MM      PIC 9(2).
               03  HI-DD      PIC 9(2).
       01  HEAD1.
           02  F              PIC X(05) VALUE X"1A24212474".
           02  F              PIC X(04) VALUE SPACE.
           02  M1-01          PIC N(04).
           02  F              PIC X(07) VALUE SPACE.
           02  F              PIC X(31) VALUE
                "＊＊＊　　類 似 品 マ ス タ ー ".
           02  F              PIC X(30) VALUE
                "プ ル ー フ リ ス ト　　＊＊＊".
           02  F              PIC X(19) VALUE SPACE.
           02  F              PIC X(5)  VALUE "DATE.".
           02  M-YY           PIC Z9.
           02  F              PIC X     VALUE "/".
           02  M-MM           PIC Z9.
           02  F              PIC X     VALUE "/".
           02  M-DD           PIC Z9.
           02  F              PIC X(15) VALUE SPACE.
           02  F              PIC X(5)  VALUE "PAGE.".
           02  WPCNT          PIC ZZZ9.
       01  HEAD2.
           02  M2-01          PIC N(05).
           02  F              PIC X(39) VALUE SPACE.
           02  M2-02          PIC N(05).
       01  W-AREA.
           02  ACT            PIC 9 VALUE ZERO.
           02  OKC            PIC 9.
           02  W-AREA1.
               03  KBN        PIC 9.
               03  W-HCD1     PIC 9(6).
               03  W-AREA2.
                   04  W-A    PIC N(07).
                   04  W-B    PIC N(07).
                   04  W-C    PIC N(05).
                   04  W-D    PIC N(05).
                   04  W-HIM1 PIC N(24).
           02  W-AREA3.
               03  W-HCD2     PIC 9(6).
               03  W-HIM2     PIC N(24).
           02  W-AREA4.
               03  W-KUBUN    PIC 9.
               03  W-FROM     PIC 9(06) VALUE ZERO.
               03  W-TO       PIC 9(06) VALUE ZERO.
       01  W-STAT.
           02  HTB            PIC X(2)  VALUE  "01".
           02  SKP            PIC X(2)  VALUE  "06".
           02  BTB            PIC X(2)  VALUE  "09".
       COPY  LWMSG.
      *
           COPY   LTRUIJ.
           COPY   LIHIM2.
      *FD  P-F
       01  P-R.
           02  PR1.
               03  KCD1           PIC X(05).
               03  P1-01          PIC 9(06).
               03  FILLER         PIC X(01).
               03  P1-02          PIC N(24).
               03  FILLER         PIC X(06).
               03  P1-03          PIC 9(06).
               03  F              PIC X(1).
               03  P1-04          PIC N(24).
               03  F              PIC X(49).
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
           02  C-CL     PIC X(12) VALUE "CLEAR SCREEN".
           02  C-CL1    PIC X(30) VALUE
                   "                              ".
           02  C-CL2    PIC X(01) VALUE " ".
           02  C-CL3    PIC X(06) VALUE " ".
           02  C-CL4    PIC X(06) VALUE " ".
           02  C-CL5    PIC X(01) VALUE " ".
       01  C-CLE.
           02  CLE-01  PIC  X(01)  VALUE " ".
           02  CLE-02  PIC  N(07).
           02  CLE-03  PIC  N(05).
           02  C-CLE1.
               03  CLE-04  PIC  X(06)
                           VALUE "      ".
               03  CLE-05  PIC  N(24).
           02  C-CLE11.
               03  CLE-06  PIC  N(07).
               03  CLE-07  PIC  N(05).
           02  C-CLE2.
               03  CLE-08  PIC  X(06)
                           VALUE "      ".
               03  CLE-09  PIC  N(24).
       01  C-ACP.
           02  A-ACT    PIC 9 .
           02  A-KBN    PIC 9 .
           02  A-HCD1   PIC 9(06).
           02  A-HCD2   PIC 9(06).
           02  FILLER.
             03  A-KUBUN    PIC 9(01).
             03  A-FROM     PIC 9(06).
             03  A-TO       PIC 9(06).
           02  A-OKC    PIC 9 .
       01  C-DSP.
           02  D-A      PIC N(07).
           02  D-B      PIC N(07).
           02  D-C      PIC N(05).
           02  D-D      PIC N(05).
       01  C-DSP1.
           02  D-HIM1   PIC N(24).
           02  D-HIM2   PIC N(24).
       01  C-PM.
           02  FILLER   PIC N(22) VALUE
              "＊＊＊　類似品マスター　メンテナンス　＊＊＊".
           02  FILLER   PIC X(46) VALUE
                "登録=1        削除=3 作表=4 終了=9　ACT   ﾘﾀｰﾝ".
           02  FILLER   PIC X(21) VALUE
                "確認 OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-PM1.
           02  FILLER   PIC X(11) VALUE "CLEAR  LINE".
           02  FILLER   PIC X(53) VALUE
               "<  区分   の品名ｺｰﾄﾞ        より        まで打出し  >".
       01  DISP-ERR-AREA.
           02  INV-HIM.
               03  FILLER   PIC X(32) VALUE
                   "品名ＩＮＤＥＸＥＤマスタ　未登録".
       COPY  LSMSG.
       PROCEDURE   DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "56" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-CL1" "X" "24" "1" "30" "C-CL" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-CL2" "X" "6" "25" "1" "C-CL1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-CL3" "X" "6" "38" "6" "C-CL2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-CL4" "X" "6" "50" "6" "C-CL3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-CL5" "X" "23" "38" "1" "C-CL4" " " RETURNING RESU.
      *C-CLE
       CALL "SD_Init" USING 
            "C-CLE" " " "0" "0" "157" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-01" "X" "6" "4" "1" " " "C-CLE" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-02" "N" "8" "2" "14" "CLE-01" " " RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-02" BY REFERENCE W-SPA "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-03" "N" "8" "17" "10" "CLE-02" " " RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-03" BY REFERENCE W-SPA "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-CLE1" " " "0" "0" "54" "CLE-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-04" "X" "9" "6" "6" " " "C-CLE1" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-05" "N" "9" "17" "48" "CLE-04" " " RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-05" BY REFERENCE W-SPA "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-CLE11" " " "0" "0" "24" "C-CLE1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-06" "N" "11" "2" "14" " " "C-CLE11" RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-06" BY REFERENCE W-SPA "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-07" "N" "11" "17" "10" "CLE-06" " " RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-07" BY REFERENCE W-SPA "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-CLE2" " " "0" "0" "54" "C-CLE11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-08" "X" "12" "6" "6" " " "C-CLE2" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-09" "N" "12" "17" "48" "CLE-08" " " RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-09" BY REFERENCE W-SPA "48" "0" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "28" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "58" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KBN" "9" "6" "4" "1" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KBN" BY REFERENCE KBN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD1" "9" "9" "6" "6" "A-KBN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD1" BY REFERENCE W-HCD1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD2" "9" "12" "6" "6" "A-HCD1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD2" BY REFERENCE W-HCD2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-ACP" " " "6" "0" "13" "A-HCD2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KUBUN" "9" "6" "25" "1" " " "05C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KUBUN" BY REFERENCE W-KUBUN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FROM" "9" "6" "38" "6" "A-KUBUN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FROM" BY REFERENCE W-FROM "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TO" "9" "6" "50" "6" "A-FROM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TO" BY REFERENCE W-TO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-OKC" "9" "23" "38" "1" "05C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "48" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-A" "N" "8" "2" "14" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-A" BY REFERENCE W-A "14" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-B" "N" "11" "2" "14" "D-A" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-B" BY REFERENCE W-B "14" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-C" "N" "8" "17" "10" "D-B" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-C" BY REFERENCE W-C "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-D" "N" "11" "17" "10" "D-C" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-D" BY REFERENCE W-D "10" "0" RETURNING RESU.
      *C-DSP1
       CALL "SD_Init" USING 
            "C-DSP1" " " "0" "0" "96" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HIM1" "N" "9" "17" "48" " " "C-DSP1" RETURNING RESU.
       CALL "SD_From" USING 
            "D-HIM1" BY REFERENCE W-HIM1 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HIM2" "N" "12" "17" "48" "D-HIM1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-HIM2" BY REFERENCE W-HIM2 "48" "0" RETURNING RESU.
      *C-PM
       CALL "SD_Init" USING 
            "C-PM" " " "0" "0" "111" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-PM" "N" "1" "17" "44" " " "C-PM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-PM" "X" "3" "17" "46" "01C-PM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-PM" "X" "23" "22" "21" "02C-PM" " " RETURNING RESU.
      *C-PM1
       CALL "SD_Init" USING 
            "C-PM1" " " "0" "0" "64" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-PM1" "X" "5" "12" "11" " " "C-PM1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-PM1" "X" "6" "17" "53" "01C-PM1" " " RETURNING RESU.
      *DISP-ERR-AREA
       CALL "SD_Init" USING 
            "DISP-ERR-AREA" " " "24" "0" "32" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-HIM" " " "24" "0" "32" " " "DISP-ERR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01INV-HIM" "X" "24" "1" "32" " " "INV-HIM" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
      ****************************
      ***  ﾒ ｲ ﾝ  R T N        ***
      ****************************
      **
       MR-RTN.
           PERFORM  INIT-RTN  THRU   INIT-EX.
           CALL "SD_Output" USING "C-CL" C-CL "p" RETURNING RESU.
           CALL "SD_Output" USING "C-PM" C-PM "p" RETURNING RESU.
       MR-10.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO  MR-10
           END-IF
           IF  ACT  =  1  OR  3
               PERFORM    ENT-RTN THRU ENT-EX
           END-IF
           IF  ACT  =  4
               PERFORM    LIS-RTN THRU LIS-EX
           END-IF
           IF  ACT  =  9
               PERFORM    END-RTN THRU END-EX
               CALL "DB_Close"
               STOP RUN
           END-IF
           GO   TO   MR-10.
      *********************************
      ***   ｲﾆｼｬﾙ   R T N           ***
      *********************************
      **
       INIT-RTN.
           CALL "DB_F_Open" USING
            "I-O" JT-RUIJ_PNAME1 "SHARED" BY REFERENCE JT-RUIJ_IDLST "1"
            "RUIJ-KEY" BY REFERENCE RUIJ-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           ACCEPT   HIZUKE  FROM  DATE.
           MOVE  HI-YY     TO  M-YY.
           MOVE  HI-MM     TO  M-MM.
           MOVE  HI-DD     TO  M-DD.
       INIT-EX.
           EXIT.
      *********************************
      *     ﾄｳﾛｸ ｻｸｼﾞｮ  R T N       ***
      *********************************
      **
       ENT-RTN.
           CALL "SD_Screen_Output" USING "SJ500M" RETURNING RESU.
           CALL "SD_Output" USING "C-CLE" C-CLE "p" RETURNING RESU.
       ENT-10.
           CALL "SD_Accept" USING BY REFERENCE A-KBN "A-KBN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT       =  BTB
               GO  TO ENT-EX
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO ENT-10
           END-IF
           IF  KBN    NOT  =   1   AND   2
               GO  TO ENT-10
           END-IF
           IF  KBN    =    1
               MOVE   "　親品名ＣＤ　"   TO   W-A
               MOVE   "　子品名ＣＤ　"   TO   W-B
               MOVE   "親品名　　"       TO   W-C
               MOVE   "子品名　　"       TO   W-D
           ELSE
               MOVE   "変更元品名ＣＤ"   TO   W-A
               MOVE   "変更先品名ＣＤ"   TO   W-B
               MOVE   "変更元品名"       TO   W-C
               MOVE   "変更先品名"       TO   W-D
           END-IF
           CALL "SD_Output" USING "C-DSP" C-DSP "p" RETURNING RESU.
           CALL "SD_Output" USING "C-CLE1" C-CLE1 "p" RETURNING RESU.
           CALL "SD_Output" USING "C-CLE2" C-CLE2 "p" RETURNING RESU.
       ENT-20.
           CALL "SD_Accept" USING BY REFERENCE A-HCD1 "A-HCD1" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT       =  BTB
               GO  TO ENT-10
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO ENT-20
           END-IF
           MOVE   W-HCD1  TO   HI-MHCD HI-HCD.
      *           READ   HI2-M        INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R " " RETURNING RET.
           IF  RET = 1
               GO  TO ENT-30
           END-IF
           MOVE   HI-NAME TO   W-HIM1.
           CALL "SD_Output" USING "D-HIM1" D-HIM1 "p" RETURNING RESU.
           GO   TO   ENT-40.
       ENT-30.
           MOVE   SPACE   TO   W-HIM1.
           CALL "SD_Output" USING "CLE-05" CLE-05 "p" RETURNING RESU.
           IF  ACT  =  3
               GO TO ENT-40
           END-IF
           CALL "SD_Output" USING "INV-HIM" INV-HIM "p" RETURNING RESU.
           GO TO ENT-20.
       ENT-40.
           CALL "SD_Accept" USING BY REFERENCE A-HCD2 "A-HCD2" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT       =  BTB
               GO  TO ENT-20
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO ENT-40
           END-IF
           MOVE   W-HCD2  TO   HI-MHCD HI-HCD.
      *           READ   HI2-M        INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R " " RETURNING RET.
           IF  RET = 1
               GO  TO ENT-50
           END-IF
           MOVE   HI-NAME TO   W-HIM2.
           CALL "SD_Output" USING "D-HIM2" D-HIM2 "p" RETURNING RESU.
           GO   TO   ENT-60.
       ENT-50.
           MOVE   SPACE   TO   W-HIM2.
           CALL "SD_Output" USING "CLE-09" CLE-09 "p" RETURNING RESU.
           IF  ACT  =  3
               GO TO ENT-60
           END-IF
           CALL "SD_Output" USING "INV-HIM" INV-HIM "p" RETURNING RESU.
           GO TO ENT-40.
       ENT-60.
           IF  KBN  =  1
               MOVE  10      TO  RUIJ-01
               MOVE  W-HCD1  TO  RUIJ-02
               MOVE  W-HCD2  TO  RUIJ-03
           ELSE
               MOVE  20      TO  RUIJ-01
               MOVE  W-HCD1  TO  RUIJ-02
               MOVE  W-HCD2  TO  RUIJ-03
           END-IF
      *           READ   JT-RUIJ     INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JT-RUIJ_PNAME1 BY REFERENCE RUIJ-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  ENT-70
           END-IF
           GO   TO   ENT-80.
       ENT-70.
           IF  ACT  NOT  =  1
               CALL "SD_Output" USING
                "INV-M01" INV-M01 "p" RETURNING RESU
               GO  TO  ENT-40
           END-IF
           IF  KBN   =  1
               GO  TO  ENT-90
           END-IF
           GO  TO  ENT-100.
       ENT-80.
           IF  ACT  NOT  =  1
               GO  TO  ENT-100
           END-IF
           CALL "SD_Output" USING "NOR-M01" NOR-M01 "p" RETURNING RESU.
           GO  TO  ENT-40.
       ENT-90.
           PERFORM  CHK-RTN  THRU  CHK-EX.
           IF  CHK-SW  =  1
               CALL "SD_Output" USING
                "NOR-M01" NOR-M01 "p" RETURNING RESU
               GO  TO  ENT-40
           END-IF.
       ENT-100.
           CALL "SD_Output" USING "C-CL5" C-CL5 "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-OKC "A-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  BTB
               GO  TO  ENT-40
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO  ENT-100
           END-IF
           IF  OKC              =  9
               CALL "SD_Output" USING "CAN-01" CAN-01 "p" RETURNING RESU
               CALL "SD_Output" USING "C-CLE" C-CLE "p" RETURNING RESU
               CALL "SD_Output" USING "C-CL5" C-CL5 "p" RETURNING RESU
               MOVE   ZERO   TO  KBN  W-HCD1  OKC
               MOVE   SPACE  TO  W-AREA2
               GO  TO  ENT-10
           END-IF
           IF  OKC       NOT  =  1
               GO  TO  ENT-100
           END-IF
           PERFORM    GET-RTN   THRU    GET-EX.
           CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU.
           CALL "SD_Output" USING "C-CLE2" C-CLE2 "p" RETURNING RESU.
           CALL "SD_Output" USING "C-CL5" C-CL5 "p" RETURNING RESU.
           MOVE  ZERO   TO  W-HCD2  OKC
           MOVE  SPACE  TO  W-HIM2.
           GO   TO   ENT-40.
       ENT-EX.
           EXIT.
      ************************************
      ***    C H E C K     R T N       ***
      ************************************
      **
       CHK-RTN.
           MOVE  ZERO    TO  CHK-SW.
           MOVE  11      TO  RUIJ-01.
           MOVE  W-HCD2  TO  RUIJ-02.
           MOVE  ZERO    TO  RUIJ-03.
      *           START JT-RUIJ KEY IS NOT < RUIJ-KEY INVALID
      *///////////////
           CALL "DB_Start" USING
            JT-RUIJ_PNAME1 "RUIJ-KEY" " NOT < " RUIJ-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  CHK-EX
           END-IF.
       CHK-01.
      *           READ  JT-RUIJ    NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JT-RUIJ_PNAME1 BY REFERENCE RUIJ-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  CHK-EX
           END-IF
           IF  RUIJ-01  >  11
               GO  TO  CHK-EX
           END-IF
           IF  RUIJ-02  >  W-HCD2
               GO  TO  CHK-EX
           END-IF
           IF  RUIJ-02  NOT =  W-HCD2
               GO  TO  CHK-01
           END-IF
           MOVE   1    TO   CHK-SW.
       CHK-EX.
           EXIT.
      ******************************
      ***   ﾘ ｽ ﾄ   R T N        ***
      ******************************
      **
       LIS-RTN.
           CALL "SD_Output" USING "C-PM1" C-PM1 "p" RETURNING RESU.
       LIS-00.
           CALL "SD_Accept" USING BY REFERENCE A-KUBUN "A-KUBUN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT       =  BTB
               GO  TO  LIS-EX
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO  LIS-00
           END-IF
           IF  W-KUBUN  NOT =  1   AND   2
               GO  TO  LIS-00
           END-IF.
       LIS-01.
           CALL "SD_Accept" USING BY REFERENCE A-FROM "A-FROM" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT       =  BTB
               GO  TO  LIS-00
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO  LIS-01
           END-IF.
       LIS-01A.
           CALL "SD_Accept" USING BY REFERENCE A-TO "A-TO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  BTB
               GO  TO LIS-01
           END-IF
           IF  ESTAT  NOT  =   HTB  AND  SKP
               GO  TO  LIS-01A
           END-IF
           IF  W-FROM  >  W-TO
               GO  TO  LIS-01
           END-IF.
       LIS-02.
           CALL "SD_Output" USING "C-CL5" C-CL5 "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-OKC "A-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  BTB
               GO  TO  LIS-01A
           END-IF
           IF  ESTAT  NOT  =  HTB  AND  SKP
               GO  TO  LIS-02
           END-IF
           IF  OKC         =    9
               MOVE  ZERO  TO  W-AREA4
               CALL "SD_Output" USING "C-CL2" C-CL2 "p" RETURNING RESU
               CALL "SD_Output" USING "C-CL3" C-CL3 "p" RETURNING RESU
               CALL "SD_Output" USING "C-CL4" C-CL4 "p" RETURNING RESU
               CALL "SD_Output" USING "C-CL5" C-CL5 "p" RETURNING RESU
               CALL "SD_Output" USING "CAN-01" CAN-01 "p" RETURNING RESU
               GO  TO  LIS-00
           END-IF
           IF  OKC    NOT  =    1
               GO  TO  LIS-02
           END-IF
           CALL "PR_Open" RETURNING RESP.
           MOVE  90    TO  LCNT.
           MOVE   0    TO  PCNT.
           IF  W-KUBUN  =  1
               MOVE  10  TO  RUIJ-01  W-KBN
           ELSE
               MOVE  20  TO  RUIJ-01  W-KBN
           END-IF
           MOVE  W-FROM  TO  RUIJ-02.
           MOVE  ZERO    TO  RUIJ-03.
      *           START JT-RUIJ KEY  IS  NOT  <  RUIJ-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JT-RUIJ_PNAME1 "RUIJ-KEY" " NOT < " RUIJ-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  LIS-99
           END-IF.
       LIS-03.
      *           READ  JT-RUIJ  NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JT-RUIJ_PNAME1 BY REFERENCE RUIJ-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  LIS-99
           END-IF
           IF  RUIJ-02   >    W-TO
               GO  TO  LIS-99
           END-IF
           IF  RUIJ-01  NOT =  W-KBN
               GO  TO  LIS-99
           END-IF
           IF  LCNT  NOT <  62
               PERFORM  HEAD-RTN  THRU  HEAD-EX
           END-IF
           IF  RUIJ-01  =  10
               GO  TO  LIS-04
           END-IF
      *
           MOVE    RUIJ-02     TO    P1-01  HI-MHCD HI-HCD.
           PERFORM  HIM-RTN  THRU  HIM-EX.
           MOVE    HI-NAME     TO    P1-02.
           MOVE    RUIJ-03     TO    P1-03  HI-MHCD HI-HCD.
           PERFORM  HIM-RTN  THRU  HIM-EX.
           MOVE    HI-NAME     TO    P1-04.
           MOVE    K15         TO    KCD1.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE       TO  P-R.
           ADD     2           TO  LCNT.
           GO  TO  LIS-03.
       LIS-04.
           IF  RUIJ-02  NOT =  OLDKEY
               GO  TO  LIS-05
           END-IF
           IF  LCNT         =  4
               GO  TO  LIS-05
           END-IF
           MOVE    SPACE       TO    P1-02.
           MOVE    RUIJ-03     TO    P1-03  HI-MHCD HI-HCD.
           PERFORM  HIM-RTN  THRU  HIM-EX.
           MOVE    HI-NAME     TO    P1-04.
           MOVE    K15         TO    KCD1.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE       TO  P-R.
           ADD     1           TO  LCNT.
           MOVE    RUIJ-02     TO  OLDKEY.
           GO  TO  LIS-03.
       LIS-05.
           MOVE    RUIJ-02     TO    P1-01  HI-MHCD HI-HCD.
           PERFORM  HIM-RTN  THRU  HIM-EX.
           MOVE    HI-NAME     TO    P1-02.
           MOVE    RUIJ-03     TO    P1-03  HI-MHCD HI-HCD.
           PERFORM  HIM-RTN  THRU  HIM-EX.
           MOVE    HI-NAME     TO    P1-04.
           MOVE    K15         TO    KCD1.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE       TO  P-R.
           ADD     2           TO  LCNT.
           MOVE    RUIJ-02     TO  OLDKEY.
           GO  TO  LIS-03.
       LIS-99.
           MOVE  ZERO  TO  W-AREA4.
           CALL "SD_Output" USING "C-CL2" C-CL2 "p" RETURNING RESU.
           CALL "SD_Output" USING "C-CL3" C-CL3 "p" RETURNING RESU.
           CALL "SD_Output" USING "C-CL4" C-CL4 "p" RETURNING RESU.
           CALL "SD_Output" USING "C-CL5" C-CL5 "p" RETURNING RESU.
           CALL "PR_Close" RETURNING RESP.
           IF  LCNT = 90
               CALL "SD_Output" USING "ERR-02" ERR-02 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU
           END-IF
           GO  TO  LIS-00.
       LIS-EX.
           EXIT.
      *****************************
      ***    G E T   R T N      ***
      *****************************
      **
       GET-RTN.
           IF  ACT    =    1
               GO    TO    GET-R1
           END-IF
           IF  ACT    =    3
               GO    TO    GET-R3
           END-IF.
      *
      *    W R I T E           *
      *
       GET-R1.
           IF  KBN  =  2
               GO    TO    GET-R2
           END-IF
           MOVE  SPACE  TO  RUIJ-R.
           INITIALIZE       RUIJ-R.
           MOVE  10     TO  RUIJ-01.
           MOVE  W-HCD1 TO  RUIJ-02.
           MOVE  W-HCD2 TO  RUIJ-03.
           PERFORM  WRI-RTN  THRU  WRI-EX.
      *
           MOVE  SPACE  TO  RUIJ-R.
           INITIALIZE       RUIJ-R.
           MOVE  11     TO  RUIJ-01.
           MOVE  W-HCD2 TO  RUIJ-02.
           MOVE  W-HCD1 TO  RUIJ-03.
           PERFORM  WRI-RTN  THRU  WRI-EX.
           GO  TO  GET-EX.
       GET-R2.
           MOVE  SPACE  TO  RUIJ-R.
           INITIALIZE       RUIJ-R.
           MOVE  20     TO  RUIJ-01.
           MOVE  W-HCD1 TO  RUIJ-02.
           MOVE  W-HCD2 TO  RUIJ-03.
           PERFORM  WRI-RTN  THRU  WRI-EX.
           GO  TO  GET-EX.
      *
      *    D E L E T E         *
      *
       GET-R3.
           IF  KBN  =  2
               GO    TO    GET-R4
           END-IF
           MOVE  10     TO  RUIJ-01.
           MOVE  W-HCD1 TO  RUIJ-02.
           MOVE  W-HCD2 TO  RUIJ-03.
           PERFORM  DEL-RTN  THRU  DEL-EX.
      *
           MOVE  11     TO  RUIJ-01.
           MOVE  W-HCD2 TO  RUIJ-02.
           MOVE  W-HCD1 TO  RUIJ-03.
           PERFORM  DEL-RTN  THRU  DEL-EX.
           GO  TO  GET-EX.
       GET-R4.
           MOVE  20     TO  RUIJ-01.
           MOVE  W-HCD1 TO  RUIJ-02.
           MOVE  W-HCD2 TO  RUIJ-03.
           PERFORM  DEL-RTN  THRU  DEL-EX.
           GO  TO  GET-EX.
       GET-EX.
           EXIT.
      *
      *****************************
      ***   W R I T E   R T N   ***
      *****************************
      **
       WRI-RTN.
           MOVE  RUIJ-KEY  TO  ERR-K.
      *           WRITE RUIJ-R      INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            JT-RUIJ_PNAME1 JT-RUIJ_LNAME RUIJ-R RETURNING RET.
           IF  RET = 1
               MOVE  "JT-RUIJ"  TO  ERR-F
               MOVE  "W"        TO  ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       WRI-EX.
           EXIT.
      *
      *****************************
      ***  D E L E T E  R T N   ***
      *****************************
      **
       DEL-RTN.
           MOVE  RUIJ-KEY  TO  ERR-K.
      *           DELETE JT-RUIJ   INVALID KEY
      *///////////////
           CALL "DB_Delete" USING JT-RUIJ_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "JT-RUIJ"  TO  ERR-F
               MOVE  "D"        TO  ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       DEL-EX.
           EXIT.
      *
      *****************************
      ***    ﾐ ﾀﾞ ｼ  R T N      ***
      *****************************
      **
       HEAD-RTN.
           IF  LCNT  NOT =  90
               MOVE   SPACE   TO   P-R
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           ADD   1   TO    PCNT.
           MOVE  PCNT  TO  WPCNT.
           IF  RUIJ-01  =  10
               MOVE  "類似品"      TO  M1-01
           ELSE
               MOVE  "品名変更"    TO  M1-01
           END-IF
           MOVE   HEAD1    TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE   TO     P-R.
           IF  RUIJ-01  =  10
               MOVE  "親品名"      TO  M2-01
               MOVE  "子品名"      TO  M2-02
           ELSE
               MOVE  "変更元品名"  TO  M2-01
               MOVE  "変更先品名"  TO  M2-02
           END-IF
           MOVE   HEAD2    TO    P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE   TO     P-R.
           MOVE   4       TO     LCNT.
       HEAD-EX.
           EXIT.
      **************************
      ***  H I M - R T N     ***
      **************************
      **
       HIM-RTN.
      *           READ  HI2-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
                 MOVE SPACE   TO HI-NAME
           END-IF.
       HIM-EX.
           EXIT.
      ****
      **************************
      ***  ﾌ ｧ ｲ ﾙ  CLOSE    ***
      **************************
      **
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-RUIJ_IDLST JT-RUIJ_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
       END-EX.
           EXIT.
      ****
       COPY  LPMSG.

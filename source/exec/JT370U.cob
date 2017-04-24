       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 JT370U.
      **************************************************************************
      *    PROGRAM  :  月次繰越                                                *
      *    COMPILE  :  CBL85(74MODE)                                           *
      **************************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       SOURCE-COMPUTER.            SYSTEM3100.
       OBJECT-COMPUTER.            SYSTEM3100.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
       01  ERR-STAT                PIC  X(02).
       01  W-FILE                  PIC  X(13).
       01  WORK-AREA.
           03  W-SSU               PIC S9(07).
           03  W-JSU               PIC S9(07).
           03  W-NGD.
             04  W-NEND            PIC  9(04).
             04  W-NENL  REDEFINES W-NEND.
               05  W-NEND1         PIC  9(02).
               05  W-NEND2         PIC  9(02).
             04  W-GETD            PIC  9(02).
           03  W-DATE              PIC  9(08).
           03  W-NGP   REDEFINES W-DATE.
             04  W-NG.
               05  W-NEN           PIC  9(04).
               05  W-GET           PIC  9(02).
             04  W-PEY             PIC  9(02).
           03  W-JNGP.
             04  W-NEN1            PIC  9(02).
             04  W-NGPS            PIC  9(06).
           03  OKC                 PIC  9(01).
           03  CNT                 PIC  9(02).
           03  W-ZC                PIC  9(01).
           03  W-MSG               PIC  N(05).
           COPY    LWMSG.
      *
           COPY    LJMST1.
           COPY    L-JNSR.
           COPY    LNJZAI.
           COPY    L-JCON.
           COPY    LJNYZ.
      *FD  NJZAIYR
       01  NJZAIYR_JT370U.
           02  NJZAIYR_PNAME1     PIC  X(007) VALUE "NJZAIYR".
           02  F                  PIC  X(001).
           02  NJZAIYR_LNAME      PIC  X(014) VALUE "NJZAIYR_JT370U".
           02  F                  PIC  X(001).
           02  NJZAIYR_KEY1       PIC  X(100) VALUE SPACE.
           02  NJZAIYR_SORT       PIC  X(100) VALUE SPACE.
           02  NJZAIYR_IDLST      PIC  X(100) VALUE SPACE.
           02  NJZAIYR_RES        USAGE  POINTER.
       01  NJZAIY-R.
           02   NJZAIY-KEY        PIC 9(08).
           02   F                 PIC X(326).
           02   NJZAIY-NG         PIC 9(06).
           02   F                 PIC X(01).
       77  F                      PIC X(01).
      *FD  HSKIF
       01  HSKIF_JT370U.
           02  HSKIF_PNAME1   PIC  X(005) VALUE "HSKIF".
           02  F              PIC  X(001).
           02  HSKIF_LNAME    PIC  X(012) VALUE "HSKIF_JT370U".
           02  F              PIC  X(001).
           02  HSKIF_KEY1     PIC  X(100) VALUE SPACE.
           02  HSKIF_SORT     PIC  X(100) VALUE SPACE.
           02  HSKIF_IDLST    PIC  X(100) VALUE SPACE.
           02  HSKIF_RES      USAGE  POINTER.
       01  HSKI-R.
           02  HSKI-NO        PIC  9(007).
           02  HSKI-NOD   REDEFINES HSKI-NO.
             03  HSKI-UNO     PIC  9(006).
             03  HSKI-GYO     PIC  9(001).
           02  HSKI-DATE      PIC  9(008).
           02  HSKI-NGPD  REDEFINES HSKI-DATE.
             03  HSKI-NG      PIC  9(006).
             03  F            PIC  9(002).
           02  HSKI-NGP   REDEFINES HSKI-DATE.
             03  F            PIC  9(002).
             03  HSKI-NGPS    PIC  9(006).
           02  HSKI-HCD       PIC  9(006).
           02  HSKI-SIZ       PIC  9(001).
           02  HSKI-SUD.
             03  HSKI-SU      PIC S9(005)  OCCURS  10.
           02  HSKI-SKC       PIC  9(001).
           02  HSKI-IDC       PIC  9(001).
           02  F              PIC  X(009).
           02  HSKI-PRN       PIC  9(001).
           02  HSKI-UPD       PIC  9(001).
       77  F                  PIC  X(001).
      *FD  JNSRRF
       01  JNSRRF_JT370U.
           02  JNSRRF_PNAME1  PIC  X(006) VALUE "JNSRRF".
           02  F              PIC  X(001).
           02  JNSRRF_LNAME   PIC  X(013) VALUE "JNSRRF_JT370U".
           02  F              PIC  X(001).
           02  JNSRRF_KEY1    PIC  X(100) VALUE SPACE.
           02  JNSRRF_SORT    PIC  X(100) VALUE SPACE.
           02  JNSRRF_IDLST   PIC  X(100) VALUE SPACE.
           02  JNSRRF_RES     USAGE  POINTER.
       01  JNSRR-R.
           02  JNSRR-D        PIC  X(170).
           02  F              PIC  X(16).
           02  JNSRR-NG       PIC  9(06).
       77  F                  PIC  X(01).
      *FD  JMSTYF
       01  JMSTYF_JT370U.
           02  JMSTYF_PNAME1  PIC  X(006) VALUE "JMSTYF".
           02  F              PIC  X(001).
           02  JMSTYF_LNAME   PIC  X(013) VALUE "JMSTYF_JT370U".
           02  F              PIC  X(001).
           02  JMSTYF_KEY1    PIC  X(100) VALUE SPACE.
           02  JMSTYF_SORT    PIC  X(100) VALUE SPACE.
           02  JMSTYF_IDLST   PIC  X(100) VALUE SPACE.
           02  JMSTYF_RES     USAGE  POINTER.
       01  JMSTY-R.
           02  F              PIC  X(323).
           02  JMSTY-NG       PIC  9(06).
           02  F              PIC  X(12).
       77  F                  PIC  X(01).
      *FD  J-DATE
       01  J-DATE_JT370U.
           02  J-DATE_PNAME1  PIC  X(006) VALUE "J-DATE".
           02  F              PIC  X(001).
           02  J-DATE_LNAME   PIC  X(013) VALUE "J-DATE_JT370U".
           02  F              PIC  X(001).
           02  J-DATE_KEY1    PIC  X(100) VALUE SPACE.
           02  J-DATE_SORT    PIC  X(100) VALUE SPACE.
           02  J-DATE_IDLST   PIC  X(100) VALUE SPACE.
           02  J-DATE_RES     USAGE  POINTER.
       01  JDT-R.
           02  JDT-NGP        PIC 9(06).
           02  JDT-TIME       PIC 9(06).
           02  JDT-STN        PIC 9(03).
           02  JDT-DNO        PIC 9(06).
           02  JDT-ACT        PIC 9(01).
           02  JDT-NC         PIC 9(01).
           02  F              PIC X(02).
       77  F                  PIC  X(01).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-AREA.
           03  FILLER.
               05  CLEAR   PIC  X(12) VALUE "CLEAR SCREEN".
               05  FILLER  PIC X(22) VALUE   "                      ".
               05  FILLER  PIC N(10) VALUE "月　　次　　繰　　越".
           03  FILLER.
               05  FILLER  PIC X(21) VALUE   "（  '  年   月 分  ）".
           03  FILLER.
               05  FILLER  PIC X(26) VALUE
                                 "確認(OK=1,NO=9)-->    ﾘﾀｰﾝ".
       01  ACP-AREA.
           02  ACP-NEN  PIC  9(02).
           02  ACP-GET  PIC  9(02).
           02  ACP-OKC  PIC  9(01).
       01  DSP-MSG.
           02  FILLER  PIC  N(05).
       01  DSP-ERR.
           02  ERR-MSG1   PIC  N(07) VALUE
               "ＪＣＯＮ　なし".
           02  ERR-MSG2   PIC  X(27) VALUE
                 "***  NJZAIYR WRITE ｴﾗｰ  ***".
           02  ERR-MSG3   PIC  X(26) VALUE
                 "***  JNSRRF WRITE ｴﾗｰ  ***".
           02  ERR-MSG4   PIC  X(26) VALUE
                 "***  JMSTYR WRITE ｴﾗｰ  ***".
           COPY LSSEM.
           COPY LSMSG.
      ***************************************
       PROCEDURE                   DIVISION.
      ***************************************
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "101" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" " " "1" "0" "54" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLEAR" "X" "1" "0" "12" " " "01DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
           "0201DSP-AREA" "RX" "1" "25" "22" "CLEAR" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0301DSP-AREA" "N" "1" "26" "20" "0201DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" " " "12" "0" "21" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-AREA" "RX" "12" "29" "21" " " "02DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" " " "24" "0" "26" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0103DSP-AREA" "X" "24" "41" "26" " " "03DSP-AREA"
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-NEN" "9" "12" "34" "2" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_From" USING 
            "ACP-NEN" BY REFERENCE W-NEND2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-GET" "9" "12" "39" "2" "ACP-NEN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "ACP-GET" BY REFERENCE W-GETD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "24" "61" "1" "ACP-GET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *DSP-MSG
       CALL "SD_Init" USING 
            "DSP-MSG" " " "16" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MSG" "N" "16" "32" "10" " " "DSP-MSG" RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-MSG" BY REFERENCE W-MSG "10" "0" RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "24" "0" "93" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG1" "N" "24" "1" "14" " " "DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG2" "X" "24" "15" "27" "ERR-MSG1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG3" "X" "24" "15" "26" "ERR-MSG2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG4" "X" "24" "15" "26" "ERR-MSG3" " " RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       HAJIME.
           PERFORM   INI-RTN   THRU   INI-EX.
           IF  COMPLETION_CODE  =  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE    "１　／　５"     TO   W-MSG.
           CALL "SD_Output" USING "DSP-MSG" DSP-MSG "p" RETURNING RESU.
           PERFORM   UP0-RTN   THRU   UP0-EX.
           MOVE    "２　／　５"     TO   W-MSG.
           CALL "SD_Output" USING "DSP-MSG" DSP-MSG "p" RETURNING RESU.
           PERFORM   UP2-RTN   THRU   UP2-EX.
           MOVE    "３　／　５"     TO   W-MSG.
           CALL "SD_Output" USING "DSP-MSG" DSP-MSG "p" RETURNING RESU.
           PERFORM   UP7-RTN   THRU   UP7-EX.
           MOVE    "４　／　５"     TO   W-MSG.
           CALL "SD_Output" USING "DSP-MSG" DSP-MSG "p" RETURNING RESU.
           PERFORM   UP5-RTN   THRU   UP5-EX.
           MOVE    "５　／　５"     TO   W-MSG.
           CALL "SD_Output" USING "DSP-MSG" DSP-MSG "p" RETURNING RESU.
           PERFORM   UP6-RTN   THRU   UP6-EX.
       OWARI.
           PERFORM   END-RTN   THRU   END-EX.
           CALL "DB_Close".
           STOP      RUN.
      ***************************************
      *    初期処理                         *
      ***************************************
       INI-RTN.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU..
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON6-KEY" BY REFERENCE JCON6-KEY.
           MOVE    SPACE          TO  JCON6-KEY.
           MOVE    6              TO  JCON6-01.
      *           READ    JCON       UNLOCK   INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "ERR-MSG1" ERR-MSG1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  INI-EX
           END-IF
           MOVE    JCON6-031          TO  W-NEN.
           MOVE    JCON6-032          TO  W-GET.
           MOVE    JCON6-03           TO  W-NGD.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "SD_Output" USING "ACP-NEN" ACP-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-GET" ACP-GET "p" RETURNING RESU.
       INI-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      NOT  =  "01"  AND  "06"
               GO  TO  INI-030
           END-IF
           IF  OKC             =  9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  INI-EX
           END-IF
           IF  OKC        NOT  =  1
               GO  TO  INI-030
           END-IF
      *
           SUBTRACT  1      FROM  W-GET.
           IF  W-GET        =  ZERO
               MOVE     12       TO    W-GET
               SUBTRACT  1       FROM  W-NEN
           END-IF
           SUBTRACT  1      FROM  W-GET.
           IF  W-GET        =  ZERO
               MOVE     12       TO    W-GET
               SUBTRACT  1       FROM  W-NEN
           END-IF
           MOVE      ZERO   TO  W-PEY.
      *
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU..
           CALL "SD_Output" USING "ACP-NEN" ACP-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-GET" ACP-GET "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-OKC" ACP-OKC "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" JMST1_PNAME1 " " BY REFERENCE JMST1_IDLST "1"
            "JMST1-KEY1" BY REFERENCE JMST1-KEY1.
           CALL "DB_F_Open" USING
            "I-O" JNSR_PNAME1 " " BY REFERENCE JNSR_IDLST "3"
            "JNSR-KEY1" BY REFERENCE JNSR-KEY1 "JNSR-KEY2" BY REFERENCE
            JNSR-KEY2 "JNSR-KEY3" BY REFERENCE JNSR-KEY3.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 " " BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON6-KEY" BY REFERENCE JCON6-KEY.
           CALL "DB_F_Open" USING
            "I-O" JNYZ_PNAME1 " " BY REFERENCE JNYZ_IDLST "1"
            "JNYZ-KEY" BY REFERENCE JNYZ-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" NJZAIYR_PNAME1 " " BY REFERENCE NJZAIYR_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" JNSRRF_PNAME1 " " BY REFERENCE JNSRRF_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" JMSTYF_PNAME1 " " BY REFERENCE JMSTYF_IDLST "0".
       INI-EX.
           EXIT.
      ***************************************
      *    終了処理                         *
      ***************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JMST1_IDLST JMST1_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JNSR_IDLST JNSR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JNYZ_IDLST JNYZ_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAIYR_IDLST NJZAIYR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JNSRRF_IDLST JNSRRF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTYF_IDLST JMSTYF_PNAME1.
       END-EX.
           EXIT.
      ***************************************
      *    入庫予定残ファイル更新・削除     *
      ***************************************
       UP0-RTN.
      *           READ      JNYZ   NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JNYZ_PNAME1 BY REFERENCE JNYZ-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP0-EX
           END-IF
           MOVE      9            TO  NJZAI-01.
           MOVE      JNYZ-01      TO  NJZAI-02.
           MOVE      JNYZ-02      TO  NJZAI-03.
      *           READ      NJZAI       INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP0-RTN
           END-IF
           COMPUTE   JNYZ-0311(01)  =  JNYZ-0311(01) - NJZAI-0711(01).
           COMPUTE   JNYZ-0311(02)  =  JNYZ-0311(02) - NJZAI-0711(02).
           COMPUTE   JNYZ-0311(03)  =  JNYZ-0311(03) - NJZAI-0711(03).
           COMPUTE   JNYZ-0311(04)  =  JNYZ-0311(04) - NJZAI-0711(04).
           COMPUTE   JNYZ-0311(05)  =  JNYZ-0311(05) - NJZAI-0711(05).
           COMPUTE   JNYZ-0311(06)  =  JNYZ-0311(06) - NJZAI-0711(06).
           COMPUTE   JNYZ-0311(07)  =  JNYZ-0311(07) - NJZAI-0711(07).
           COMPUTE   JNYZ-0311(08)  =  JNYZ-0311(08) - NJZAI-0711(08).
           COMPUTE   JNYZ-0311(09)  =  JNYZ-0311(09) - NJZAI-0711(09).
           COMPUTE   JNYZ-0311(10)  =  JNYZ-0311(10) - NJZAI-0711(10).
           IF  ZERO              =  JNYZ-0311(01)  AND  JNYZ-0311(02)
                               AND  JNYZ-0311(03)  AND  JNYZ-0311(04)
                               AND  JNYZ-0311(05)  AND  JNYZ-0311(06)
                               AND  JNYZ-0311(07)  AND  JNYZ-0311(08)
                               AND  JNYZ-0311(09)  AND  JNYZ-0311(10)
               PERFORM  JNYZ-DEL-RTN  THRU  JNYZ-DEL-EX
           ELSE
               PERFORM  JNYZ-REW-RTN  THRU  JNYZ-REW-EX
           END-IF
           GO  TO  UP0-RTN.
       UP0-EX.
           EXIT.
      ***************************************
      *    受注マスタの繰り越し             *
      ***************************************
       UP2-RTN.
           MOVE      SPACE  TO  JMST1-KEY1.
      *           START     JMST1  KEY NOT < JMST1-KEY1  INVALID
      *///////////////
           CALL "DB_Start" USING
            JMST1_PNAME1 "JMST1-KEY1" " NOT < " JMST1-KEY1
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP2-EX
           END-IF.
       UP2-010.
      *           READ      JMST1  NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST1_PNAME1 BY REFERENCE JMST1-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP2-EX
           END-IF
           PERFORM   JMSTY-WRI-RTN THRU   JMSTY-WRI-EX.
           PERFORM   COM-RTN   THRU   COM-EX.
           IF  JMST1-21   NOT =  1
               GO  TO  UP2-020
           END-IF
           IF  (JMST1-151(01)      =  ZERO )
               AND  (JMST1-151(02)      =  ZERO )
               AND  (JMST1-151(03)      =  ZERO )
               AND  (JMST1-151(04)      =  ZERO )
               AND  (JMST1-151(05)      =  ZERO )
               AND  (JMST1-151(06)      =  ZERO )
               AND  (JMST1-151(07)      =  ZERO )
               AND  (JMST1-151(08)      =  ZERO )
               AND  (JMST1-151(09)      =  ZERO )
               AND  (JMST1-151(10)      =  ZERO )
               AND  (W-JSU              =  W-SSU)
               GO  TO  UP2-020
           END-IF
           MOVE  0  TO  JMST1-21.
           PERFORM   JMST-REW-RTN   THRU   JMST-REW-EX.
       UP2-020.
           IF  JMST1-21       =  1
               PERFORM  JMST-DEL-RTN  THRU  JMST-DEL-EX
               GO   TO  UP2-010
           END-IF
           IF  (W-JSU          =  W-SSU )
                AND (JMST1-151(01)  =  ZERO  )
                AND (JMST1-151(02)  =  ZERO  )
                AND (JMST1-151(03)  =  ZERO  )
                AND (JMST1-151(04)  =  ZERO  )
                AND (JMST1-151(05)  =  ZERO  )
                AND (JMST1-151(06)  =  ZERO  )
                AND (JMST1-151(07)  =  ZERO  )
                AND (JMST1-151(08)  =  ZERO  )
                AND (JMST1-151(09)  =  ZERO  )
                AND (JMST1-151(10)  =  ZERO  )
               MOVE  1  TO  JMST1-21
           END-IF
      *****
           COMPUTE  JMST1-141(01)  =  JMST1-141(01)  +  JMST1-1211(01).
           COMPUTE  JMST1-141(02)  =  JMST1-141(02)  +  JMST1-1211(02).
           COMPUTE  JMST1-141(03)  =  JMST1-141(03)  +  JMST1-1211(03).
           COMPUTE  JMST1-141(04)  =  JMST1-141(04)  +  JMST1-1211(04).
           COMPUTE  JMST1-141(05)  =  JMST1-141(05)  +  JMST1-1211(05).
           COMPUTE  JMST1-141(06)  =  JMST1-141(06)  +  JMST1-1211(06).
           COMPUTE  JMST1-141(07)  =  JMST1-141(07)  +  JMST1-1211(07).
           COMPUTE  JMST1-141(08)  =  JMST1-141(08)  +  JMST1-1211(08).
           COMPUTE  JMST1-141(09)  =  JMST1-141(09)  +  JMST1-1211(09).
           COMPUTE  JMST1-141(10)  =  JMST1-141(10)  +  JMST1-1211(10).
           MOVE     ZERO   TO   JMST1-1211(01)  JMST1-1211(06)
                                JMST1-1211(02)  JMST1-1211(07)
                                JMST1-1211(03)  JMST1-1211(08)
                                JMST1-1211(04)  JMST1-1211(09)
                                JMST1-1211(05)  JMST1-1211(10).
           PERFORM   JMST-REW-RTN   THRU   JMST-REW-EX.
           GO   TO   UP2-010.
       UP2-EX.
           EXIT.
      ***************************************
      *    倉別在庫マスタ　繰越  (NJZAI)    *
      ***************************************
       UP5-RTN.
           MOVE      SPACE  TO  NJZAI-KEY.
      *           START     NJZAI  KEY NOT < NJZAI-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            NJZAI_PNAME1 "NJZAI-KEY" " NOT < " NJZAI-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  UP5-EX
           END-IF.
       UP5-010.
      *           READ      NJZAI  NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP5-EX
           END-IF
           PERFORM   NJZAIY-WRI-RTN THRU   NJZAIY-WRI-EX.
           MOVE      ZERO   TO  CNT  W-ZC.
           IF  NJZAI-01      =  9
               MOVE   1         TO  W-ZC
           END-IF.
       UP5-020.
           ADD       1      TO  CNT.
           IF  CNT           >  10
               GO  TO  UP5-030
           END-IF
           COMPUTE  NJZAI-0611(CNT)  =  NJZAI-0611(CNT)
                                +  NJZAI-0411(CNT)  -  NJZAI-0511(CNT)
                                +  NJZAI-0711(CNT)  -  NJZAI-0811(CNT).
           MOVE  ZERO          TO  NJZAI-0411(CNT)  NJZAI-0511(CNT)
                                   NJZAI-0711(CNT)  NJZAI-0811(CNT).
           IF  W-ZC                   =  1
               GO  TO  UP5-020
           END-IF
           IF  NJZAI-0611(CNT)   NOT  =  ZERO
               MOVE  1             TO  W-ZC
           END-IF
           IF  NJZAI-0911(CNT)   NOT  =  ZERO
               MOVE  1             TO  W-ZC
           END-IF
           IF  NJZAI-1111(CNT)   NOT  =  ZERO
               MOVE  1             TO  W-ZC
           END-IF
           GO  TO  UP5-020.
       UP5-030.
           IF  W-ZC          =  0
               PERFORM   NJZAI-DEL-RTN   THRU   NJZAI-DEL-EX
           ELSE
               PERFORM   NJZAI-REW-RTN   THRU   NJZAI-REW-EX
           END-IF
           GO  TO  UP5-010.
       UP5-EX.
           EXIT.
      ***************************************
      *    コントロールＦ　日付更新　　　　 *
      ***************************************
       UP6-RTN.
           MOVE    SPACE          TO  JCON6-KEY.
           MOVE    6              TO  JCON6-01.
      *           READ    JCON       INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "ERR-MSG1" ERR-MSG1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           ADD     1              TO  JCON6-032.
           IF  JCON6-032     =  13
               MOVE   1         TO  JCON6-032
               ADD    1         TO  JCON6-031
           END-IF
      *           REWRITE   JCON6-R INVALID
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON6-R RETURNING RET.
           IF  RET = 1
               MOVE    "JCON"      TO  ERR-F
               MOVE    "R"         TO  ERR-M
               MOVE     JCON6-KEY  TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       UP6-100.
           CALL "DB_F_Open" USING
            "I-O" HSKIF_PNAME1 " " BY REFERENCE HSKIF_IDLST "0".
       UP6-110.
      *           READ HSKIF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HSKIF_PNAME1 BY REFERENCE HSKI-R " " RETURNING RET.
           IF  RET = 1
               GO TO UP6-120
           END-IF
           IF  HSKI-DATE  >  W-DATE
               GO TO UP6-110
           END-IF
           MOVE X"FF" TO HSKI-R.
      *           REWRITE HSKI-R.
      *///////////////
           CALL "DB_Update" USING
            HSKIF_PNAME1 HSKIF_LNAME HSKI-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               MOVE    "HSKIF"     TO  ERR-F
               MOVE    "R"         TO  ERR-M
               MOVE     HSKI-NO    TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           GO TO UP6-110.
       UP6-120.
           CALL "DB_F_Close" USING
            BY REFERENCE HSKIF_IDLST HSKIF_PNAME1.
       *
           CALL "DB_F_Open" USING
            "I-O" J-DATE_PNAME1 " " BY REFERENCE J-DATE_IDLST "0".
           MOVE 20 TO W-NEN1.
       UP6-210.
      *           READ J-DATE AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" J-DATE_PNAME1 BY REFERENCE JDT-R " " RETURNING RET.
           IF  RET = 1
               GO TO UP6-220
           END-IF
           MOVE JDT-NGP TO W-NGPS.
           IF  W-JNGP     >  W-DATE
               GO TO UP6-210
           END-IF
           MOVE X"FF" TO JDT-R.
      *           REWRITE JDT-R.
      *///////////////
           CALL "DB_Update" USING
            J-DATE_PNAME1 J-DATE_LNAME JDT-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               MOVE    "J-DATE"    TO  ERR-F
               MOVE    "R"         TO  ERR-M
               MOVE     JDT-R      TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           GO TO UP6-210.
       UP6-220.
           CALL "DB_F_Close" USING
            BY REFERENCE J-DATE_IDLST J-DATE_PNAME1.
       UP6-EX.
           EXIT.
      ***************************************
      *    入出庫累積ファイル更新・削除     *
      ***************************************
       UP7-RTN.
      *           READ      JNSR   NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JNSR_PNAME1 BY REFERENCE JNSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP7-EX
           END-IF
           IF  JNSR-02        >  W-DATE
               GO  TO  UP7-010
           END-IF
           PERFORM   JNSRR-WRI-RTN  THRU   JNSRR-WRI-EX.
           PERFORM   JNSR-DEL-RTN   THRU   JNSR-DEL-EX.
           GO   TO   UP7-RTN.
       UP7-010.
           IF  JNSR-92        =  ZERO  OR  SPACE
               MOVE      W-GETD      TO  JNSR-92
               PERFORM   JNSR-REW-RTN   THRU   JNSR-REW-EX
           END-IF
           GO   TO   UP7-RTN.
       UP7-EX.
           EXIT.
      ***************************************
      *    入庫予定残ファイル　 更新　　    *
      ***************************************
       JNYZ-REW-RTN.
      *           REWRITE   JNYZ-R  INVALID
      *///////////////
           CALL "DB_Update" USING
            JNYZ_PNAME1 JNYZ_LNAME JNYZ-R RETURNING RET.
           IF  RET = 1
               MOVE    "JNYZ"      TO  ERR-F
               MOVE    "R"         TO  ERR-M
               MOVE     JNYZ-KEY   TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       JNYZ-REW-EX.
           EXIT.
      ***************************************
      *    入庫予定残ファイル　削除         *
      ***************************************
       JNYZ-DEL-RTN.
      *           DELETE    JNYZ    INVALID
      *///////////////
           CALL "DB_Delete" USING JNYZ_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE    "JNYZ"      TO  ERR-F
               MOVE    "D"         TO  ERR-M
               MOVE     JNYZ-KEY   TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       JNYZ-DEL-EX.
           EXIT.
      ***************************************
      *    受注マスタ.完了区分のセット      *
      ***************************************
       JMST-REW-RTN.
      *           REWRITE   JMST1-R INVALID
      *///////////////
           CALL "DB_Update" USING
            JMST1_PNAME1 JMST1_LNAME JMST1-R RETURNING RET.
           IF  RET = 1
               MOVE    "JMST1"     TO  ERR-F
               MOVE    "R"         TO  ERR-M
               MOVE     JMST1-KEY1 TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       JMST-REW-EX.
           EXIT.
      ***************************************
      *    受注マスタの削除                 *
      ***************************************
       JMST-DEL-RTN.
      *           DELETE    JMST1   INVALID
      *///////////////
           CALL "DB_Delete" USING JMST1_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE    "JMST1"     TO  ERR-F
               MOVE    "D"         TO  ERR-M
               MOVE     JMST1-KEY1 TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       JMST-DEL-EX.
           EXIT.
      ***************************************
      *    入出庫累積ファイルの更新         *
      ***************************************
       JNSR-REW-RTN.
      *           REWRITE   JNSR-R INVALID
      *///////////////
           CALL "DB_Update" USING
            JNSR_PNAME1 JNSR_LNAME JNSR-R RETURNING RET.
           IF  RET = 1
               MOVE    "JNSR"      TO  ERR-F
               MOVE    "R"         TO  ERR-M
               MOVE     JNSR-KEY1  TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       JNSR-REW-EX.
           EXIT.
      ***************************************
      *    入出庫累積ファイルの削除         *
      ***************************************
       JNSR-DEL-RTN.
      *           DELETE    JNSR    INVALID
      *///////////////
           CALL "DB_Delete" USING JNSR_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE    "JNSR"      TO  ERR-F
               MOVE    "D"         TO  ERR-M
               MOVE     JNSR-KEY1  TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       JNSR-DEL-EX.
           EXIT.
      ***************************************
      *    倉別在庫マスタの累積             *
      ***************************************
       NJZAIY-WRI-RTN.
           MOVE NJZAI-R TO NJZAIY-R.
           MOVE W-NGD TO NJZAIY-NG.
      *           WRITE   NJZAIY-R.
      *//////////////
           CALL "DB_Insert" USING
            NJZAIYR_PNAME1 NJZAIYR_LNAME NJZAIY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO NJZAIY-WRI-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ERR-MSG2" ERR-MSG2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAIYR_IDLST NJZAIYR_PNAME1.
           MOVE "NJZAIYR      " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" NJZAIYR_PNAME1 " " BY REFERENCE NJZAIYR_IDLST "0".
           GO TO NJZAIY-WRI-RTN.
       NJZAIY-WRI-EX.
           EXIT.
      ***************************************
      *    倉別在庫マスタの更新  (NEW)      *
      ***************************************
       NJZAI-REW-RTN.
      *           REWRITE   NJZAI-R INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE    "NJZAI"     TO  ERR-F
               MOVE    "R"         TO  ERR-M
               MOVE     NJZAI-KEY  TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       NJZAI-REW-EX.
           EXIT.
      ***************************************
      *    倉別在庫マスタの削除  (NEW)      *
      ***************************************
       NJZAI-DEL-RTN.
      *           DELETE    NJZAI   INVALID
      *///////////////
           CALL "DB_Delete" USING NJZAI_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE    "NJZAI"     TO  ERR-F
               MOVE    "D"         TO  ERR-M
               MOVE     NJZAI-KEY  TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       NJZAI-DEL-EX.
           EXIT.
      ***************************************
      *    受注数／出荷数の計算             *
      ***************************************
       COM-RTN.
           COMPUTE   W-JSU   =    JMST1-1111(01)
                             +    JMST1-1111(02)
                             +    JMST1-1111(03)
                             +    JMST1-1111(04)
                             +    JMST1-1111(05)
                             +    JMST1-1111(06)
                             +    JMST1-1111(07)
                             +    JMST1-1111(08)
                             +    JMST1-1111(09)
                             +    JMST1-1111(10).
           COMPUTE   W-SSU   =    JMST1-1211(01)   +   JMST1-141(01)
                             +    JMST1-1211(02)   +   JMST1-141(02)
                             +    JMST1-1211(03)   +   JMST1-141(03)
                             +    JMST1-1211(04)   +   JMST1-141(04)
                             +    JMST1-1211(05)   +   JMST1-141(05)
                             +    JMST1-1211(06)   +   JMST1-141(06)
                             +    JMST1-1211(07)   +   JMST1-141(07)
                             +    JMST1-1211(08)   +   JMST1-141(08)
                             +    JMST1-1211(09)   +   JMST1-141(09)
                             +    JMST1-1211(10)   +   JMST1-141(10).
       COM-EX.
           EXIT.
      ***************************************
      *    受注マスタの累積                 *
      ***************************************
       JMSTY-WRI-RTN.
           MOVE JMST1-R TO JMSTY-R.
           MOVE W-NGD TO JMSTY-NG.
      *           WRITE   JMSTY-R.
      *//////////////
           CALL "DB_Insert" USING
            JMSTYF_PNAME1 JMSTYF_LNAME JMSTY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO JMSTY-WRI-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ERR-MSG4" ERR-MSG4 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTYF_IDLST JMSTYF_PNAME1.
           MOVE "JMSTYF       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" JMSTYF_PNAME1 " " BY REFERENCE JMSTYF_IDLST "0".
           GO TO JMSTY-WRI-RTN.
       JMSTY-WRI-EX.
           EXIT.
      ***************************************
      *    入出庫累積Ｆの累積               *
      ***************************************
       JNSRR-WRI-RTN.
           INITIALIZE JNSRR-R.
           MOVE JNSR-R TO JNSRR-R.
           MOVE W-NG TO JNSRR-NG.
      *           WRITE   JNSRR-R.
      *//////////////
           CALL "DB_Insert" USING
            JNSRRF_PNAME1 JNSRRF_LNAME JNSRR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO JNSRR-WRI-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ERR-MSG3" ERR-MSG3 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE JNSRRF_IDLST JNSRRF_PNAME1.
           MOVE "JNSRRF       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" JNSRRF_PNAME1 " " BY REFERENCE JNSRRF_IDLST "0".
           GO TO JNSRR-WRI-RTN.
       JNSRR-WRI-EX.
           EXIT.
      ***
           COPY    LPMSG.
      *******************    E N D    O F    P R O G R A M    ******************

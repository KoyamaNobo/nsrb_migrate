       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             JT280L.
       AUTHOR.                 K.HENMI.
      *********************************************
      *    PROGRAM NAME  :  ‘q•Ê•i–¼•ÊÝŒÉ–¾×•\@*
      *    DATA WRITTEN  :  62/08/07              *  (•ÏX:'89.01.11)
      *    SCREEN  USED  :  ***                   *
      *    FORM    USED  :  ***                   *
      *    PRINTER TYPE  :  JIPS                  *
      *    COMPILE TYPE  :  COBOL                 *
      *********************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       SOURCE-COMPUTER.        SYSTEM100.
       OBJECT-COMPUTER.        SYSTEM100.
       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
       77  ERR-STAT            PIC  X(02)   VALUE  SPACE.
       77  LCNT                PIC  9(02)   VALUE  90.
       77  PCNT                PIC  9(03)   VALUE   0.
       77  END-SW              PIC  9(01)   VALUE   0.
       77  W-GOKEI             PIC  S9(07).
       77  W-SEN               PIC  9.
       77  W-KBN               PIC  9.
       77  W-OK                PIC  9.
       77  P                   PIC  9.
       77  I                   PIC  9(02).
       77  END-P               PIC  9(01).
       77  WRI-SW              PIC  9(01).
       77  W-MP                PIC  9(01) VALUE  0.
       77  WK0128ID            PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1         PIC  X(003).
           02  STN-NO2         PIC  X(003).
       01  W-FID.
           02  W-FID1          PIC  X(006) VALUE "WK0128".
           02  W-FID2          PIC  X(003).
      *
       01  W-DATA.
           02  W-HCD           PIC  9(06).
           02  W-HCDD  REDEFINES W-HCD.
               03  W-HCD1      PIC  9(04).
               03  W-HCD2      PIC  9(02).
       01  NEW-CD.
           02  NEW-SOUKO       PIC  9.
           02  NEW-HINMEI      PIC  9(06).
           02  NEW-YC          PIC  9(01).
       01  OLD-CD.
           02  OLD-SOUKO       PIC  9.
           02  OLD-HINMEI      PIC  9(06).
           02  OLD-YC          PIC  9(01).
       01  W-SAIZU-TBL.
           02  W-S-TBL         OCCURS  4.
               03  W-SA        PIC  S9(06)  OCCURS  10.
               03  ZR-CNT      PIC  9(02).
       01  W-P.
           02  M-1             PIC  9(06).
           02  M-1R      REDEFINES  M-1   PIC  X(06).
           02  F               PIC  X(01).
           02  M-2             PIC  N(24).
           02  M-F             PIC  X(01).
           02  M-3             PIC  9.
           02  M-4             PIC  ----,---.
           02  M-5             PIC  ----,---.
           02  M-6             PIC  ----,---.
           02  M-7             PIC  ----,---.
           02  M-8             PIC  ----,---.
           02  M-9             PIC  ----,---.
           02  M-10            PIC  ----,---.
           02  M-11            PIC  ----,---.
           02  M-12            PIC  ----,---.
           02  M-13            PIC  ----,---.
           02  M-14            PIC  --,---,--9.
           02  M-14R      REDEFINES  M-14  PIC  X(10).
           02  M-R             PIC  X(01).
       01  HEAD1.
           02  W-20K           PIC  X(05)   VALUE  X"1A24212474".
           02  F               PIC  N(06)   VALUE  "‘qŒÉ–¼@F@".
           02  H-SON           PIC  N(06).
           02  F               PIC  X(12)   VALUE  SPACE.
           02  F               PIC  N(19)   VALUE
                "–––@@Ý@ŒÉ@–¾@×@•\@@–––".
           02  F               PIC  X(07)   VALUE  SPACE.
           02  H-KBN           PIC  X(22).
           02  F               PIC  X(09)   VALUE  SPACE.
           02  F               PIC  X(05)   VALUE  "DATE ".
           02  H-DATE          PIC  99/99/99.
           02  F               PIC  X(05)   VALUE  SPACE.
           02  F               PIC  X(02)   VALUE  "P.".
           02  H-PAGE          PIC ZZ9.
       01  HEAD1A.
           02  F               PIC  X(96)   VALUE  SPACE.
           02  F               PIC  X(25)   VALUE
                "u@( )“à‚Í“üŒÉ—\’è”@v".
           02  F               PIC  X(15)   VALUE  SPACE.
       01  HEAD2.
           02  W-15K           PIC  X(05)   VALUE  X"1A24212078".
           02  F               PIC  X(07)   VALUE  " º°ÄÞ  ".
           02  F               PIC  N(08)   VALUE  "•i@@@@@–¼@".
           02  F               PIC  X(25)   VALUE  SPACE.
           02  F               PIC  X(01)   VALUE  "1".
           02  F               PIC  X(05)   VALUE  SPACE.
           02  F               PIC  N(02)   VALUE  "‚R†".
           02  F               PIC  X(05)   VALUE  SPACE.
           02  F               PIC  N(02)   VALUE  "‚Q†".
           02  F               PIC  X(05)   VALUE  SPACE.
           02  F               PIC  N(02)   VALUE  "‚P†".
           02  F               PIC  X(05)   VALUE  SPACE.
           02  F               PIC  N(02)   VALUE  "‚O†".
           02  F               PIC  X(05)   VALUE  SPACE.
           02  F               PIC  N(02)   VALUE  "@’†".
           02  F               PIC  X(05)   VALUE  SPACE.
           02  F               PIC  N(02)   VALUE  "@‘å".
           02  F               PIC  X(05)   VALUE  SPACE.
           02  F               PIC  N(02)   VALUE  "“Á‘å".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "28.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "29.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "30.0".
           02  F               PIC  X(10)   VALUE  SPACE.
       01  HEAD3.
           02  F               PIC  X(44)   VALUE  SPACE.
           02  F               PIC  X(01)   VALUE  "2".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "12.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "13.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "13.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "14.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "15.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "16.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "17.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "18.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "19.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "20.0".
           02  F               PIC  X(10)   VALUE  SPACE.
       01  HEAD4.
           02  F               PIC  X(44)   VALUE  SPACE.
           02  F               PIC  X(01)   VALUE  "3".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "21.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "21.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "22.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "22.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "23.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "23.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "24.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "24.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "25.0".
           02  F               PIC  X(18)   VALUE  SPACE.
       01  HEAD5.
           02  F               PIC  X(44)   VALUE  SPACE.
           02  F               PIC  X(01)   VALUE  "4".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "24.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "24.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "25.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "25.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "26.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "26.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "27.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "27.5".
           02  F               PIC  X(20)   VALUE  SPACE.
           02  F               PIC  N(04)   VALUE  "@‡@Œv".
      **
           COPY       L-JCON.
           COPY       LIHIM2.
      *FD  JT-YZAI
       01  JT-YZAI_JT281L.
           02  JT-YZAI_PNAME1       PIC  X(009) VALUE SPACE.
           02  F                    PIC  X(001).
           02  JT-YZAI_LNAME        PIC  X(014) VALUE "JT-YZAI_JT281L".
           02  F                    PIC  X(001).
           02  JT-YZAI_KEY1         PIC  X(100) VALUE SPACE.
           02  JT-YZAI_SORT         PIC  X(100) VALUE SPACE.
           02  JT-YZAI_IDLST        PIC  X(100) VALUE SPACE.
           02  JT-YZAI_RES          USAGE  POINTER.
       01  YZAI-R.
           02   YZAI-01             PIC 9(1).
           02   YZAI-02             PIC 9(6).
           02   YZAI-03             PIC 9(1).
           02   YZAI-04.
                03  YZAI-041     OCCURS  10.
                    04  YZAI-0411   PIC S9(6).
           02   FILLER              PIC X(14).
           02   YZAI-91             PIC 9(02).
           02   YZAI-92             PIC 9(01).
           02   YZAI-93             PIC 9(01).
           02   FILLER              PIC X(42).
       77  F                        PIC X(01).
      *FD  P-F
       77  P-R                      PIC X(170).
      **
      *
       77  ESTAT                    PIC  X(002).
       77  RESU                     PIC  9(001).
       77  RESP                     PIC  9(001).
       77  RET                      PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER            PIC  9(003).
       77  USER_ID                  PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE          PIC  X(003) VALUE ZERO.
      *
       01  DSP-AREA.
           02  DSP-CLR   PIC  X(12) VALUE "CLEAR SCREEN".
           02  DSP-01    PIC  X(22)
               VALUE   "                      " .
           02  DSP-02    PIC  N(10)
                       VALUE   "‘q•Ê•i–¼•ÊÝŒÉ–¾×•\".
           02  DSP-07    PIC  X(45)
               VALUE "‘SŒ=0 , ƒTƒjƒNƒŠ[ƒ“=1 , ƒqƒƒvƒ‰‘¼=2  ...  ".
           02  DSP-09    PIC  N(03)
                       VALUE   "Šm”Fi".
           02  DSP-10    PIC  X(11)
                       VALUE   "OK=1,NO=9j".
           02  DSP-11    PIC  X(08)
                       VALUE   "--> ØÀ°Ý".
       01  ACT-AREA.
           02  ACT-07    PIC  9 .
           02  ACT-OK    PIC  9 .
      **
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "124" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-CLR" "X" "1" "0" "12" " " "DSP-AREA"  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" "RX" "1" "23" "22" "DSP-CLR" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-02" "N" "1" "24" "20" "DSP-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-07" "X" "7" "12" "45" "DSP-02" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-09" "N" "23" "41" "6" "DSP-07" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-10" "X" "23" "47" "11" "DSP-09" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-11" "X" "23" "58" "8" "DSP-10" " "  RETURNING RESU.
      *ACT-AREA
       CALL "SD_Init" USING 
            "ACT-AREA" " " "0" "0" "2" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-07" "9" "7" "56" "1" " " "ACT-AREA"  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-07" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-OK" "9" "23" "61" "1" "ACT-07" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-OK" BY REFERENCE W-OK "1" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ************************************
      *    ‚l‚`‚h‚m@@‚q‚n‚t‚s‚h‚m‚d@  *
      ************************************
       MR000.
           PERFORM    ACT-RTN    THRU     ACT-EX.
           IF  W-OK     =  9
               CALL "DB_Close"
               STOP  RUN
           END-IF.
      *
       MR010.
           ACCEPT      H-DATE   FROM  DATE.
           CALL    "CBLSTNNO"   USING STN-NO USER_ID.
           MOVE     STN-NO2     TO    W-FID2.
           MOVE     W-FID       TO    WK0128ID.
           MOVE     WK0128ID    TO    JT-YZAI_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-YZAI_PNAME1 " " BY REFERENCE JT-YZAI_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON3-KEY" BY REFERENCE JCON3-KEY.
           CALL "PR_Open" RETURNING RESP.
      *
       MR020.
      *           READ        JT-YZAI               AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-YZAI_PNAME1 BY REFERENCE YZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO  OWARI
           END-IF
           MOVE    YZAI-02   TO   W-HCD.
           IF  W-SEN      =   1
               IF  W-HCD2    >=   90
                   GO TO  MR020
               END-IF
           END-IF
           IF  W-SEN  NOT =   2
               GO TO  MR021
           END-IF
           MOVE    W-HCD         TO    HI-MHCD HI-HCD.
      *           READ        HI2-M    UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO  MR020
           END-IF
           IF  HI-SCC      NOT =     0
               GO TO  MR020
           END-IF.
       MR021.
           IF  W-SEN      =   1
               IF     (W-HCD1    >=   1461  AND  <=  1462)
                  OR  (W-HCD1    >=   1464  AND  <=  1465)
                  OR  (W-HCD1    >=   1483  AND  <=  1483)
                  OR  (W-HCD1    >=   1487  AND  <=  1487)
                  OR  (W-HCD1    >=   1744  AND  <=  1744)
                  OR  (W-HCD1    >=   9656  AND  <=  9656)
                   GO TO  MR025
               ELSE
                   GO TO  MR020
               END-IF
           END-IF
           IF  W-SEN      =   2
               IF     (W-HCD1    >=   1461  AND  <=  1462)
                  OR  (W-HCD1    >=   1464  AND  <=  1465)
                  OR  (W-HCD1    >=   1469  AND  <=  1470)
                  OR  (W-HCD1    >=   1474  AND  <=  1474)
                  OR  (W-HCD1    >=   1478  AND  <=  1478)
                  OR  (W-HCD1    >=   1482  AND  <=  1483)
                  OR  (W-HCD1    >=   1487  AND  <=  1487)
                  OR  (W-HCD1    >=   1491  AND  <=  1491)
                  OR  (W-HCD1    >=   1497  AND  <=  1498)
                  OR  (W-HCD1    >=   1507  AND  <=  1507)
                  OR  (W-HCD1    >=   1544  AND  <=  1544)
                  OR  (W-HCD1    >=   1546  AND  <=  1546)
                  OR  (W-HCD1    >=   1549  AND  <=  1549)
                  OR  (W-HCD1    >=   1738  AND  <=  1738)
                  OR  (W-HCD1    >=   1740  AND  <=  1740)
                  OR  (W-HCD1    >=   1743  AND  <=  1744)
                  OR  (W-HCD1    >=   1846  AND  <=  1846)
                  OR  (W-HCD1    >=   9652  AND  <=  9653)
                  OR  (W-HCD1    >=   9656  AND  <=  9656)
                   GO TO  MR025
               ELSE
                   GO TO  MR020
               END-IF
           END-IF.
       MR025.
           MOVE    YZAI-01   TO   NEW-SOUKO   OLD-SOUKO.
           MOVE    YZAI-02   TO   NEW-HINMEI  OLD-HINMEI.
           MOVE    YZAI-93   TO   NEW-YC      OLD-YC.
           MOVE    YZAI-92   TO   W-KBN.
           IF  W-KBN   =  1
               MOVE    "y  ÝŒÉ”(Žw}ŠÜ)  z"   TO  H-KBN
           END-IF
           IF  W-KBN   =  2
               MOVE    "y  ÝŒÉ”(Žw}–³)  z"   TO  H-KBN
           END-IF
           IF  W-KBN   =  3
               MOVE    "y  —LŒøÝŒÉ”  z    "   TO  H-KBN
           END-IF
           IF  W-KBN   =  4
               MOVE    "yo‰×‰Â”\¥“üŒÉ—\’èz "   TO  H-KBN
           END-IF
           MOVE    10    TO    ZR-CNT(1) ZR-CNT(2) ZR-CNT(3) ZR-CNT(4).
      *
       MR030.
           PERFORM    PRN-RTN    THRU     PRN-EX.
           IF  END-SW  =  1
               GO  TO   OWARI
           END-IF
           GO TO      MR030.
      *
       OWARI.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-YZAI_IDLST JT-YZAI_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_Close".
           STOP       RUN.
      ***********************************
      *     ‚r‚t‚a@@‚q‚n‚t‚s‚h‚m‚d    *
      ***********************************
      *------------------*
      *    ˆóŽš@ˆ—    *
      *------------------*
       PRN-RTN.
           MOVE    YZAI-03     TO  P.
           MOVE    1          TO  I.
           PERFORM   COM-RTN  THRU  COM-EX.
       PRN-030.
      *           READ    JT-YZAI                   AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-YZAI_PNAME1 BY REFERENCE YZAI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE     1    TO  END-SW
               GO  TO   PRN-040
           END-IF
           MOVE    YZAI-02   TO   W-HCD.
           IF  W-SEN      =   1
               IF  W-HCD2    >=   90
                   GO TO  PRN-030
               END-IF
           END-IF
           IF  W-SEN  NOT =   2
               GO TO  PRN-031
           END-IF
           MOVE    W-HCD         TO    HI-MHCD HI-HCD.
      *           READ        HI2-M    UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO  PRN-030
           END-IF
           IF  HI-SCC      NOT =     0
               GO TO  PRN-030
           END-IF.
       PRN-031.
           IF  W-SEN      =   1
               IF     (W-HCD1    >=   1461  AND  <=  1462)
                  OR  (W-HCD1    >=   1464  AND  <=  1465)
                  OR  (W-HCD1    >=   1483  AND  <=  1483)
                  OR  (W-HCD1    >=   1487  AND  <=  1487)
                  OR  (W-HCD1    >=   1744  AND  <=  1744)
                  OR  (W-HCD1    >=   9656  AND  <=  9656)
                   GO TO  PRN-035
               ELSE
                   GO TO  PRN-030
               END-IF
           END-IF
           IF  W-SEN      =   2
               IF     (W-HCD1    >=   1461  AND  <=  1462)
                  OR  (W-HCD1    >=   1464  AND  <=  1465)
                  OR  (W-HCD1    >=   1469  AND  <=  1470)
                  OR  (W-HCD1    >=   1474  AND  <=  1474)
                  OR  (W-HCD1    >=   1478  AND  <=  1478)
                  OR  (W-HCD1    >=   1482  AND  <=  1483)
                  OR  (W-HCD1    >=   1487  AND  <=  1487)
                  OR  (W-HCD1    >=   1491  AND  <=  1491)
                  OR  (W-HCD1    >=   1497  AND  <=  1498)
                  OR  (W-HCD1    >=   1507  AND  <=  1507)
                  OR  (W-HCD1    >=   1544  AND  <=  1544)
                  OR  (W-HCD1    >=   1546  AND  <=  1546)
                  OR  (W-HCD1    >=   1549  AND  <=  1549)
                  OR  (W-HCD1    >=   1738  AND  <=  1738)
                  OR  (W-HCD1    >=   1740  AND  <=  1740)
                  OR  (W-HCD1    >=   1743  AND  <=  1744)
                  OR  (W-HCD1    >=   1846  AND  <=  1846)
                  OR  (W-HCD1    >=   9652  AND  <=  9653)
                  OR  (W-HCD1    >=   9656  AND  <=  9656)
                   GO TO  PRN-035
               ELSE
                   GO TO  PRN-030
               END-IF
           END-IF.
       PRN-035.
           MOVE    YZAI-01     TO    NEW-SOUKO.
           MOVE    YZAI-02     TO    NEW-HINMEI.
           MOVE    YZAI-93     TO    NEW-YC.
           IF  NEW-SOUKO  NOT = OLD-SOUKO
                 OR      NEW-HINMEI  NOT =    OLD-HINMEI
                     OR      NEW-YC      NOT =    OLD-YC
               GO   TO   PRN-040
           END-IF
           GO    TO     PRN-EX.
       PRN-040.
           PERFORM  CHK-RTN      THRU  CHK-EX.
           MOVE    SPACE     TO   W-P.
           MOVE    SPACE     TO   M-2.
           IF  W-MP       NOT =  0
               GO   TO   PRN-050
           END-IF
           MOVE     OLD-HINMEI   TO    M-1.
           MOVE     OLD-HINMEI   TO    HI-MHCD HI-HCD.
      *           READ        HI2-M    UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE  TO  HI-NAME
           END-IF
           MOVE     HI-NAME      TO    M-2.
       PRN-050.
           MOVE     SPACE     TO    M-14R.
           MOVE     1         TO    P.
           MOVE     0         TO    WRI-SW.
       PRN-055.
           IF  ZR-CNT(P)  =    10
               GO   TO    PRN-060
           END-IF
           IF  LCNT       >    62
               PERFORM    HED-RTN   THRU   HED-EX
           END-IF
           IF  OLD-YC     =    1
               MOVE     "("       TO    M-F
               MOVE     ")"       TO    M-R
           ELSE
               MOVE     SPACE     TO    M-F  M-R
           END-IF
           MOVE     P         TO    M-3.
           MOVE    W-SA(P , 1)    TO  M-4.
           MOVE    W-SA(P , 2)    TO  M-5.
           MOVE    W-SA(P , 3)    TO  M-6.
           MOVE    W-SA(P , 4)    TO  M-7.
           MOVE    W-SA(P , 5)    TO  M-8.
           MOVE    W-SA(P , 6)    TO  M-9.
           MOVE    W-SA(P , 7)    TO  M-10.
           MOVE    W-SA(P , 8)    TO  M-11.
           MOVE    W-SA(P , 9)    TO  M-12.
           MOVE    W-SA(P , 10)   TO  M-13.
           IF  P           =      END-P
               MOVE     W-GOKEI   TO      M-14
           END-IF
           MOVE    SPACE          TO  P-R.
           MOVE    W-P            TO  P-R.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE     SPACE   TO  P-R.
           MOVE     SPACE   TO  M-1R     M-2.
           MOVE     1         TO    WRI-SW.
           ADD      1       TO  LCNT.
       PRN-060.
           ADD      1       TO  P.
           IF  P       >   4
               GO     TO     PRN-070
           END-IF
           GO       TO      PRN-055.
       PRN-070.
           IF  WRI-SW   NOT =  1
               GO  TO  PRN-080
           END-IF
           IF  NEW-HINMEI  NOT =  OLD-HINMEI
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING P-R RETURNING RESP
               ADD    1    TO    LCNT
           END-IF.
       PRN-080.
           IF  NEW-SOUKO    NOT =    OLD-SOUKO
               MOVE    70   TO    LCNT
           END-IF.
       PRN-090.
           MOVE    ZERO       TO    W-GOKEI.
           MOVE    1          TO    P    I.
       PRN-095.
           IF  P     >    4
               GO  TO  PRN-100
           END-IF
           MOVE    ZERO       TO    W-SA(P , I).
           IF  I     =    10
               MOVE       10   TO   ZR-CNT(P)
           END-IF
           ADD     1     TO   I.
           IF  I     >    10
               ADD        1    TO   P
               MOVE       1    TO   I
           END-IF
           GO      TO    PRN-095.
       PRN-100.
           IF  NEW-SOUKO      = OLD-SOUKO
                 AND     NEW-HINMEI      =    OLD-HINMEI
               MOVE     1    TO    W-MP
           ELSE
               MOVE     0    TO    W-MP
           END-IF
           MOVE     NEW-SOUKO    TO    OLD-SOUKO.
           MOVE     NEW-HINMEI   TO    OLD-HINMEI.
           MOVE     NEW-YC       TO    OLD-YC.
       PRN-EX.
           EXIT.
      **
       HED-RTN.
           IF  LCNT   <  90
               MOVE   SPACE  TO  P-R
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               GO  TO  HED-010
           END-IF
           MOVE SPACE  TO  P-R.
       HED-010.
           ADD   1    TO    PCNT.
           IF  OLD-SOUKO        =  9
               MOVE  "‘S@@ŽÐ@@"     TO  H-SON
               GO  TO  HED-020
           END-IF
           MOVE        3        TO  JCON3-01.
           MOVE        OLD-SOUKO    TO   JCON3-02.
      *           READ        JCON     UNLOCK   INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE  TO  JCON3-03
           END-IF
           MOVE  JCON3-03       TO   H-SON.
       HED-020.
           MOVE  PCNT           TO   H-PAGE.
           MOVE  SPACE          TO   P-R.
           MOVE  HEAD1          TO   P-R.
           CALL "PR_Write" USING P-R RETURNING RESP.
           IF  W-KBN            =  4
               MOVE  SPACE          TO   P-R
               MOVE  HEAD1A         TO   P-R
               CALL "PR_Write" USING P-R RETURNING RESP
               MOVE  SPACE          TO   P-R
               MOVE  HEAD2          TO   P-R
               CALL "PR_Write" USING P-R RETURNING RESP
           ELSE
               MOVE  SPACE          TO   P-R
               MOVE  HEAD2          TO   P-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING P-R RETURNING RESP
           END-IF
           MOVE  SPACE          TO   P-R.
           MOVE  HEAD3          TO   P-R.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE          TO   P-R.
           MOVE  HEAD4          TO   P-R.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE          TO   P-R.
           MOVE  HEAD5          TO   P-R.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE          TO   P-R.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  7    TO    LCNT.
           MOVE  OLD-HINMEI    TO     M-1.
           MOVE  HI-NAME       TO     M-2.
       HED-EX.
           EXIT.
      **
       ACT-RTN.
       G-000.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
       G-005.
           CALL "SD_Accept" USING BY REFERENCE ACT-07 "ACT-07" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "P9"
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT    NOT = "01" AND "06"
               GO TO    G-005
           END-IF
           IF  W-KBN        >  2
               GO TO    G-005
           END-IF
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           CALL "SD_Output" USING "ACT-07" ACT-07 "p" RETURNING RESU.
       G-060.
           CALL "SD_Accept" USING BY REFERENCE ACT-OK "ACT-OK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT    =  "P9"
               MOVE   9         TO  W-OK
               GO TO  ACT-EX
           END-IF
           IF  ESTAT    =  "09"
               GO TO  G-005
           END-IF
           IF  ESTAT    NOT = "01" AND "06"
               GO TO    G-060
           END-IF
           IF  W-OK     =  9
               GO  TO    ACT-EX
           END-IF
           IF  W-OK  NOT  =  1
               GO  TO   G-060
           END-IF.
       ACT-EX.
           EXIT.
       COM-RTN.
           IF  I        >      10
               GO   TO   COM-EX
           END-IF
           MOVE     YZAI-0411(I) TO    W-SA(P,I).
           ADD       1        TO     I.
           GO   TO   COM-RTN.
       COM-EX.
           EXIT.
       CHK-RTN.
           MOVE     ZERO         TO   P.
       CHK-010.
           ADD      1            TO   P.
           IF  P        >      4
               GO   TO   CHK-EX
           END-IF
           MOVE     1            TO   I.
       CHK-020.
           IF  I        >      10
               GO   TO   CHK-030
           END-IF
           ADD       W-SA(P,I)   TO   W-GOKEI.
           IF  W-SA(P,I)     NOT =     ZERO
               SUBTRACT   1    FROM  ZR-CNT(P)
           END-IF
           ADD       1        TO     I.
           GO   TO   CHK-020.
       CHK-030.
           IF  ZR-CNT(P)     NOT =     10
               MOVE  P            TO  END-P
           END-IF
           GO  TO  CHK-010.
       CHK-EX.
           EXIT.

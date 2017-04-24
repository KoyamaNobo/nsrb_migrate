       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 PR615L.
       AUTHOR.                     OZAKI.
      *===============================================================*
      *    消費税振替明細書                                           *
      *                            --- 91/03/08 ---                   *
      *===============================================================*
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.
       SOURCE-COMPUTER.           NEAC-SYSTEM100.
       OBJECT-COMPUTER.           NEAC-SYSTEM100.
       INPUT-OUTPUT               SECTION.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
       77  ERR-STAT            PIC  X(02).
       77  PCNT                PIC  9(05).
       77  K20                 PIC  X(05) VALUE X"1A24212474".
       77  K15                 PIC  X(05) VALUE X"1A24212078".
       77  W-PC                PIC  9(01) VALUE 0.
       77  W-MC                PIC  9(01) VALUE 0.
       77  W-BMC               PIC  9(01) VALUE 0.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  WORK-AREA.
           02  I               PIC  9(02).
           02  ERR-SW          PIC  9(01).
           02  INV-SW          PIC  9(01).
           02  W-ZAN           PIC S9(11).
           02  WTAX-A          PIC  9(02)V9(02).
           02  WTAX-B          PIC  9(03)V9(02).
           02  WTAX-C          PIC  9(02)V9(02).
           02  WTAX-D          PIC  9(03)V9(02).
           02  WSUB5           PIC  9(01).
           02  SYSDATE.
             03  SYS-YY        PIC  9(02).
             03  SYS-MM        PIC  9(02).
             03  SYS-DD        PIC  9(02).
           02  WK-YMD.
             03  WK-YY         PIC  9(04).
             03  WK-YYL  REDEFINES WK-YY.
               04  WK-YY1      PIC  9(02).
               04  WK-YY2      PIC  9(02).
             03  WK-MM         PIC  9(02).
             03  WK-DD         PIC  9(02).
           02  WK-ZZ           PIC  Z9.
           02  WK-ZZZZZ        PIC  ZZZZ9.
       01  NEW-KEY.
           02  NWHTAXKB        PIC  X(01).
           02  NWHKACD1        PIC  X(08).
           02  NWHSECTCD       PIC  9(04).
       01  OLD-KEY.
           02  OWHTAXKB        PIC  X(01).
           02  OWHKACD1        PIC  X(08).
           02  OWHSECTCD       PIC  9(04).
       01  OLD-AREA.
           02  OWHTRDATE3.
             03  OWHTRDATE3YY  PIC  9(04).
             03  OWHTRDATE3MM  PIC  9(02).
             03  OWHTRDATE3DD  PIC  9(02).
           02  OWHJUNLNO3      PIC  9(06).
       01  TOT-WK.
           02  TOT-KR          PIC S9(12).
           02  TOT-KS          PIC S9(12).
       01  WK-NAMER.
           02  FILLER          PIC  N(01) VALUE "（".
           02  WK-NAME1        PIC  N(02).
           02  FILLER          PIC  N(05) VALUE "消費税額）".
       01  WK-NAME REDEFINES WK-NAMER     PIC  N(08).
      ***
       01  MID03.
           02  FILLER          PIC X(05) VALUE X"1A24212474".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  M3-YY           PIC N(02).
           02  FILLER          PIC N(01) VALUE "年".
           02  M3-MM           PIC N(02).
           02  FILLER          PIC N(01) VALUE "月".
           02  M3-DD           PIC N(02).
           02  FILLER          PIC N(03) VALUE "日作成".
           02  FILLER          PIC X(23) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE SPACE.
           02  FILLER          PIC N(15)
               VALUE "消　費　税　振　替　明　細　書".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  FILLER          PIC X(40) VALUE SPACE.
           02  M3-PCNT         PIC N(05).
           02  FILLER          PIC N(01) VALUE "頁".
       01  MID05.
           02  FILLER          PIC X(35) VALUE SPACE.
           02  FILLER          PIC N(03) VALUE "（　自".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M5-YY1          PIC N(02).
           02  FILLER          PIC N(01) VALUE "年".
           02  FILLER          PIC X(01).
           02  M5-MM1          PIC N(02).
           02  FILLER          PIC N(01) VALUE "月".
           02  FILLER          PIC X(01).
           02  M5-DD1          PIC N(02).
           02  FILLER          PIC N(04) VALUE "日　　至".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M5-YY2          PIC N(02).
           02  FILLER          PIC N(01) VALUE "年".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M5-MM2          PIC N(02).
           02  FILLER          PIC N(01) VALUE "月".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M5-DD2          PIC N(02).
           02  FILLER          PIC N(03) VALUE "日　）".
       01  MID06.
           02  FILLER          PIC X(05) VALUE X"1A24212474".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(01) VALUE "部".
           02  FILLER          PIC N(01) VALUE "門".
           02  FILLER          PIC N(01) VALUE "名".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M6-01           PIC N(10).
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(01) VALUE "（".
           02  M6-02           PIC N(04).
           02  FILLER          PIC N(01) VALUE "）".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC X(51) VALUE SPACE.
       01  MID08.
           02  FILLER          PIC X(04) VALUE SPACE.
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(04) VALUE "勘定科目".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M8-01           PIC N(10).
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(01) VALUE "（".
           02  M8-02           PIC N(04).
           02  FILLER          PIC N(01) VALUE "）".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(04) VALUE "補助科目".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M8-03           PIC N(10).
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(01) VALUE "（".
           02  M8-04           PIC N(04).
           02  FILLER          PIC N(01) VALUE "）".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M8-05           PIC N(02).
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC X(36) VALUE SPACE.
       01  MID10.
           02  FILLER          PIC X(05) VALUE X"1A24212474".
           02  FILLER          PIC X(16) VALUE SPACE.
           02  FILLER          PIC N(03) VALUE "日　付".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(04) VALUE "伝票番号".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  FILLER          PIC X(01) VALUE SPACE.
           02  F               PIC X(05) VALUE "ｺｰﾄﾞ ".
           02  FILLER          PIC N(04) VALUE "取引先名".
           02  FILLER          PIC X(13) VALUE SPACE.
           02  FILLER          PIC N(04) VALUE "借方金額".
           02  FILLER          PIC X(06) VALUE SPACE.
           02  FILLER          PIC N(04) VALUE "貸方金額".
           02  FILLER          PIC X(04) VALUE SPACE.
           02  FILLER          PIC N(05) VALUE "当月発生額".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  FILLER          PIC N(05) VALUE "摘　　　要".
           02  FILLER          PIC X(20) VALUE SPACE.
           02  FILLER          PIC N(01) VALUE "税".
      *********
       COPY LWMSG_PR.
       COPY KANGEL.
       COPY ACCUNT.
       COPY BUMONF.
       COPY FCTL.
       COPY SIWAKW.
      *********
       01  P-F.
           02  P-R                 PIC X(200).
           02  P1-R   REDEFINES   P-R.
               03  P1-K15          PIC X(05).
               03  FILLER          PIC X(15).
               03  P1-01.
                 04  P1-01Y        PIC 9(02).
                 04  P1-01YH       PIC X(01).
                 04  P1-01M        PIC Z9.
                 04  P1-01MH       PIC X(01).
                 04  P1-01D        PIC Z9.
               03  FILLER          PIC X(01).
               03  FILLER          PIC X(01).
               03  P1-02           PIC 9(06).
               03  P1-02H          PIC X(01).
               03  P1-03           PIC 9(02).
               03  FILLER          PIC X(01).
               03  P1-05           PIC 9(05).
               03  FILLER          PIC X(01).
               03  P1-04           PIC N(10).
               03  P1-06           PIC --,---,---,--9.
               03  P1-07           PIC --,---,---,--9.
               03  P1-08           PIC --,---,---,--9.
               03  P1-08X          PIC X(01).
               03  FILLER          PIC X(01).
               03  P1-09           PIC N(20).
               03  FILLER          PIC X(01).
               03  P1-10           PIC X(01).
               03  P1-K20          PIC X(05).
      **********
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      *
       01  DISP-C.
           02  DISP-CLE  PIC  X(12)  VALUE "CLEAR SCREEN".
      **********
       COPY LSMSG_PR.
       PROCEDURE                   DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-C" " " "1" "0" "12" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-CLE" "X" "1" "0" "12" " " "DISP-C"  RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MR000.
           PERFORM INI-RTN THRU INI-EX.
           IF  ERR-SW = 1
               GO TO MR999
           END-IF.
           PERFORM OPEN-RTN THRU OPEN-EX.
      **
           MOVE Z-GESYY2   TO M5-YY1.
           MOVE Z-GESMM    TO WK-ZZ.
           MOVE WK-ZZ      TO M5-MM1.
           MOVE Z-GESDD    TO WK-ZZ.
           MOVE WK-ZZ      TO M5-DD1.
      *
           MOVE Z-GEMYY2   TO M5-YY2.
           MOVE Z-GEMMM    TO WK-ZZ.
           MOVE WK-ZZ      TO M5-MM2.
           MOVE Z-GEMDD    TO WK-ZZ.
           MOVE WK-ZZ      TO M5-DD2.
      **
       MR100.
      *           READ SDW AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SDW_PNAME1 BY REFERENCE SW-REC " " RETURNING RET.
           IF  RET = 1
               GO TO MR900
           END-IF.
           MOVE WHTAXKB     TO NWHTAXKB.
           MOVE WHKACD1     TO NWHKACD1.
           MOVE WHSECTCD    TO NWHSECTCD.
           MOVE NEW-KEY     TO OLD-KEY.
           PERFORM MSET-RTN THRU MSET-EX.
           PERFORM MID-RTN  THRU MID-EX.
           GO TO MR120.
       MR110.
      *           READ SDW AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SDW_PNAME1 BY REFERENCE SW-REC " " RETURNING RET.
           IF  RET = 1
               GO TO MR190
           END-IF.
           MOVE WHTAXKB     TO NWHTAXKB.
           MOVE WHKACD1     TO NWHKACD1.
           MOVE WHSECTCD    TO NWHSECTCD.
       MR120.
           IF  NEW-KEY NOT = OLD-KEY
               PERFORM TOT1-RTN THRU TOT1-EX
               PERFORM TAX-RTN  THRU TAX-EX
               PERFORM MSET-RTN THRU MSET-EX
               PERFORM MID-RTN  THRU MID-EX
               MOVE ZERO     TO OLD-AREA
           END-IF.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM ADD-RTN THRU ADD-EX.
           MOVE NEW-KEY      TO OLD-KEY.
           MOVE WHTRDATE     TO OWHTRDATE3.
           MOVE WHJUNLNO     TO OWHJUNLNO3.
           GO TO MR110.
       MR190.
           PERFORM TOT1-RTN THRU TOT1-EX.
           PERFORM TAX-RTN  THRU TAX-EX.
       MR900.
           PERFORM CLSE-ENT THRU CLSE-EXT.
       MR999.
           CALL "DB_Close".
           STOP RUN.
       INI-RTN.
           ACCEPT SYSDATE     FROM DATE.
           MOVE SYS-YY     TO M3-YY.
           MOVE SYS-MM     TO WK-ZZ.
           MOVE WK-ZZ      TO M3-MM.
           MOVE SYS-DD     TO WK-ZZ.
           MOVE WK-ZZ      TO M3-DD.
      *
           MOVE 0     TO ERR-SW.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE "DATE  "     TO FCTL-KEY1 ERR-K.
      *           READ FCTL-F UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "FCTL-F"     TO ERR-F
               MOVE "G"          TO ERR-M
               CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                             RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               MOVE 1     TO ERR-SW
               GO TO INI-EX
           END-IF.
           MOVE FCTL-REC1     TO Z-R.
      *
           MOVE "SUB   "     TO FCTL-KEY1 ERR-K.
      *           READ FCTL-F UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "FCTL-F"     TO ERR-F
               MOVE "G"          TO ERR-M
               CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                             RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               MOVE 1     TO ERR-SW
               GO TO INI-EX
           END-IF.
           MOVE FCTL-SUB5     TO WSUB5.
      *
           MOVE "TAX   "     TO FCTL-KEY1 ERR-K.
      *           READ FCTL-F UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "FCTL-F"     TO ERR-F
               MOVE "G"          TO ERR-M
               CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                             RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               MOVE 1     TO ERR-SW
               GO TO INI-EX
           END-IF.
           MOVE 1     TO I.
       INI-000.
           IF (TAX-FROM(I) NOT > Z-GEMYMD) AND
              (TAX-TO(I)   NOT < Z-GEMYMD)
               CONTINUE
           ELSE
               IF  I NOT = 2
                   ADD 1     TO I
                   GO TO INI-000
               ELSE
                   MOVE "TAX???"     TO ERR-F
                   MOVE "G"          TO ERR-M
                   CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p"
                                                 RETURNING RESU
                   CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                                 RETURNING RESU
                   CALL "DB_F_Close" USING
                    BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
                   MOVE 1     TO ERR-SW
                   GO TO INI-EX
               END-IF
           END-IF.
           MOVE TAX-RITU(I)     TO WTAX-A.
           IF  I          = 2
               MOVE  TAX-RITU(1) TO WTAX-C
           END-IF.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
      *
           COMPUTE WTAX-B = WTAX-A + 100.
           IF  I          = 2
               COMPUTE WTAX-D = WTAX-C + 100
           END-IF.
       INI-EX.
           EXIT.
       OPEN-RTN.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO SDW_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "DB_F_Open" USING
            "INPUT" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BNM_PNAME1 "SHARED" BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" SDW_PNAME1 "EXCLUSIVE" BY REFERENCE SDW_IDLST "0".
           CALL "PR_Open" RETURNING RESP.
       OPEN-EX.
           EXIT.
       SET-RTN.
           IF  DR-CR = WHDR-CR
               COMPUTE W-ZAN = W-ZAN + WHAMOUNT
           ELSE
               COMPUTE W-ZAN = W-ZAN - WHAMOUNT
           END-IF.
       SET-EX.
           EXIT.
       PRI-RTN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE   0   TO  W-MC   W-BMC
               PERFORM MID-RTN THRU MID-EX
           END-IF.
           MOVE SPACE        TO P-R.
           MOVE K15          TO P1-K15.
           MOVE K20          TO P1-K20.
           MOVE WHTRDATE     TO WK-YMD.
           IF  WHTRDATE  = OWHTRDATE3
               GO TO PRI-030
           END-IF.
           IF  WK-YY NOT = OWHTRDATE3YY
               GO TO PRI-000
           END-IF.
           IF  WK-MM NOT = OWHTRDATE3MM
               GO TO PRI-010
           END-IF.
           IF  WK-DD NOT = OWHTRDATE3DD
               GO TO PRI-020
           END-IF.
       PRI-000.
           MOVE WK-YY2   TO P1-01Y.
           MOVE "."      TO P1-01YH.
       PRI-010.
           MOVE WK-MM    TO P1-01M.
           MOVE "."      TO P1-01MH.
       PRI-020.
           MOVE WK-DD    TO P1-01D.
           GO TO PRI-040.
       PRI-030.
           IF  WHJUNLNO  = OWHJUNLNO3
               GO TO PRI-050
           END-IF.
       PRI-040.
           MOVE WHJUNLNO   TO P1-02.
           MOVE "-"        TO P1-02H.
       PRI-050.
           MOVE WHLINENO   TO P1-03.
           IF  WHCUSTCD = ZERO
               MOVE SPACE        TO P1-04
           ELSE
               MOVE WHNAMEN      TO P1-04
           END-IF.
           IF  WHCUSTCD NOT = ZERO
               MOVE WHCUSTCD   TO P1-05
           END-IF.
           IF  WHDR-CR = 1
               MOVE WHAMOUNT    TO P1-06
           ELSE
               MOVE WHAMOUNT    TO P1-07
           END-IF.
           PERFORM SET-RTN THRU SET-EX.
           MOVE W-ZAN      TO P1-08.
           MOVE WHTEKIYO   TO P1-09.
           IF  WHTAXKB     = "1" OR "5"
               MOVE "*"       TO P1-10
           END-IF.
           IF  WHTAXKB     = "3" OR "7"
               MOVE "#"       TO P1-10
           END-IF.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE     TO P-R.
       PRI-EX.
           EXIT.
       ADD-RTN.
           IF  WHDR-CR = 1
               ADD WHAMOUNT     TO TOT-KR
           ELSE
               ADD WHAMOUNT     TO TOT-KS
           END-IF.
       ADD-EX.
           EXIT.
       MID-RTN.
           IF  NWHSECTCD   NOT = OWHSECTCD
               MOVE  0      TO  W-MC  W-BMC W-PC
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 54
               MOVE   0   TO  W-MC   W-BMC   W-PC
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF.
           IF  W-PC  = 0
               MOVE  5  TO W-PC
           ELSE   
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING P-R RETURNING RESP
           END-IF.
           IF  W-MC = ZERO
               MOVE  5      TO  W-MC
               ADD 1     TO PCNT
               MOVE PCNT     TO WK-ZZZZZ
               MOVE WK-ZZZZZ     TO M3-PCNT
               MOVE MID03 TO P-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING P-R RETURNING RESP
               MOVE MID05 TO P-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING P-R RETURNING RESP
           END-IF.
           IF  W-BMC = ZERO
               MOVE  5      TO  W-BMC
               MOVE MID06 TO P-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING P-R RETURNING RESP
           END-IF.
           MOVE MID08 TO P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE MID10 TO P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE     TO P-R.
       MID-EX.
           EXIT.
       MSET-RTN.
           MOVE WHACCNTCD     TO AM-KEY.
           PERFORM AMG-RTN THRU AMG-EX.
           IF (BS-PL = 1) AND (WSUB5 = 0)
               GO TO MSET-000
           END-IF.
           IF  KEIHI = 1
               GO TO MSET-000
           END-IF.
           MOVE SPACE     TO M6-01 M6-02.
           GO TO MSET-100.
       MSET-000.
           MOVE WHSECTCD      TO BNM-KEY M6-02.
           PERFORM BNMG-RTN THRU BNMG-EX.
           MOVE BNMNMN        TO M6-01.
       MSET-100.
           MOVE WHACCNTCD     TO K-ACCD M8-02.
           MOVE ZERO          TO K-HOCD.
           PERFORM KNGG-RTN THRU KNGG-EX.
           MOVE KNGNMN        TO M8-01.
           IF  WHHOACCNT = ZERO
               MOVE SPACE     TO M8-03 M8-04
           ELSE
               MOVE WHKACD1       TO KNG-KEY
               PERFORM KNGG-RTN THRU KNGG-EX
               MOVE KNGNMN        TO M8-03
               MOVE WHHOACCNT     TO M8-04
           END-IF.
           IF  DR-CR = 1
               MOVE "借方"     TO M8-05
           ELSE
               MOVE "貸方"     TO M8-05
           END-IF.
       MSET-EX.
           EXIT.
       TOT1-RTN.
           IF  PCNT = ZERO
               GO TO TOT1-EX
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE   0   TO  W-MC   W-BMC
               PERFORM MID-RTN THRU MID-EX
           END-IF.
           MOVE SPACE              TO P-R.
           MOVE K15                TO P1-K15.
           MOVE K20                TO P1-K20.
           MOVE "（当月計）"     TO P1-04.
           MOVE TOT-KR             TO P1-06.
           MOVE TOT-KS             TO P1-07.
           MOVE W-ZAN              TO P1-08.
           MOVE SPACE              TO P1-09.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE     TO P-R.
           MOVE ZERO     TO TOT-KR TOT-KS.
       TOT1-EX.
           EXIT.
       TAX-RTN.
           IF  PCNT  = ZERO
               GO TO TAX-EX
           END-IF.
           IF  W-ZAN = ZERO
               GO TO TAX-999
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE   0   TO  W-MC   W-BMC
               PERFORM MID-RTN THRU MID-EX
           END-IF.
           IF  (I   =  2) AND (OWHTAXKB = "1" OR "5")
               COMPUTE W-ZAN ROUNDED = W-ZAN * WTAX-C / WTAX-D
           ELSE
               COMPUTE W-ZAN ROUNDED = W-ZAN * WTAX-A / WTAX-B
           END-IF.
           MOVE SPACE                TO P-R.
           MOVE K15                  TO P1-K15.
           MOVE K20                  TO P1-K20.
           IF  OWHTAXKB = "1" OR "3"
               MOVE "仮払"     TO WK-NAME1
           ELSE
               MOVE "仮受"     TO WK-NAME1
           END-IF.
           MOVE WK-NAME              TO P1-04.
           MOVE W-ZAN                TO P1-08.
           MOVE ")"                  TO P1-08X.
           MOVE SPACE                TO P1-09.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE     TO P-R.
       TAX-999.
           MOVE SPACE     TO P-R.
           MOVE ZERO     TO W-ZAN.
       TAX-EX.
           EXIT.
       AMG-RTN.
           MOVE 0     TO INV-SW.
      *           READ AM UNLOCK INVALID KEY
      *//////////////////////
           CALL "DB_Read" USING
            "INVALID KEY" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1         TO INV-SW
               INITIALIZE AM-REC
           END-IF.
       AMG-EX.
           EXIT.
       KNGG-RTN.
           MOVE 0     TO INV-SW.
      *           READ KNG UNLOCK INVALID KEY
      *//////////////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1         TO INV-SW
               MOVE SPACE     TO KNGNMN
           END-IF.
       KNGG-EX.
           EXIT.
       BNMG-RTN.
           MOVE 0     TO INV-SW.
      *           READ BNM UNLOCK INVALID KEY
      *//////////////////////
           CALL "DB_Read" USING
            "INVALID KEY" BNM_PNAME1 BY REFERENCE BNM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1         TO INV-SW
               MOVE SPACE     TO BNMNMN
           END-IF.
       BNMG-EX.
           EXIT.
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDW_IDLST SDW_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       CLSE-EXT.
           EXIT.
      *********
       COPY LPMSG_PR.
      *********

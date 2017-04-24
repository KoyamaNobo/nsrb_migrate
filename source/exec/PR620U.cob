       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 PR620U.
       AUTHOR.                     OZAKI.
      *===============================================================*
      *    Á”ïÅU‘Ö–¾×¶¬                                         *
      *                            --- 91/03/08 ---                   *
      *===============================================================*
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.
       SOURCE-COMPUTER.           NEAC-SYSTEM100.
       OBJECT-COMPUTER.           NEAC-SYSTEM100.
       INPUT-OUTPUT               SECTION.
       DATA    DIVISION.
       WORKING-STORAGE             SECTION.
       77  ERR-STAT            PIC  X(02).
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
           02  WTAX-A          PIC  9(02)V9(02).
           02  WTAX-B          PIC  9(03)V9(02).
           02  WTAX-J          PIC  9(02)V9(02).
           02  WTAX-K          PIC  9(03)V9(02).
           02  WTAX-CD         PIC  9(04).
           02  WTAX-CD1        PIC  9(04).
           02  WDNO1-023       PIC  9(06).
           02  CNT             PIC  9(01).
       01  SUM-AREA.
           02  W-KR            PIC S9(11).
           02  W-KS            PIC S9(11).
           02  W-TK            PIC S9(11).
           02  W-TAX           PIC S9(11).
       01  NEW-KEY.
           02  NWHTAXKB        PIC  X(01).
           02  NWHKACD1        PIC  X(08).
           02  NWHSECTCD       PIC  9(04).
       01  OLD-KEY.
           02  OWHTAXKB        PIC  X(01).
           02  OWHKACD1        PIC  X(08).
           02  OWHSECTCD       PIC  9(04).
       01  WK-NAME.
           02  WK-NAME1        PIC  N(09) VALUE "‰¼•¥Á”ïÅŽ©“®U‘Ö".
           02  WK-NAME2        PIC  N(09) VALUE "‰¼ŽóÁ”ïÅŽ©“®U‘Ö".
       01  W-TEKIW.
           02  W-TEKI          PIC  N(20).
           02  W-TEKID   REDEFINES  W-TEKI.
             03  W-TEKI1       PIC  N(10).
             03  W-TEKI2       PIC  N(10).
      *********
       COPY    LWMSG_PR.
       COPY    SIWAKW.
       COPY    FCTL.
       COPY    ACCUNT.
      *
       01  SDH_PR620U.
           02  SDH_PNAME1   PIC  X(009)  VALUE "SIWAKE-H1".
           02  F            PIC  X(001).
           02  SDH_LNAME    PIC  X(003)  VALUE "SDH".
           02  F            PIC  X(001).
           02  SDH_KEY1     PIC  X(100)  VALUE SPACE.
           02  SDH_KEY2     PIC  X(100)  VALUE SPACE.
           02  SDH_KEY3     PIC  X(100)  VALUE SPACE.
           02  SDH_KEY4     PIC  X(100)  VALUE SPACE.
           02  SDH_SORT     PIC  X(100)  VALUE SPACE.
           02  SDH_IDLST    PIC  X(100)  VALUE SPACE.
           02  SDH_RES      USAGE  POINTER.
       COPY    SIWAKH.
       77  F                PIC  X(001).
      *
       COPY    SIWAID.
       COPY    LNSSIW.
       COPY    LNSDNO.
       COPY    KANGEL.
      *****
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      *********
       01  DISP-C.
           02  DISP-CLE  PIC  X(12)  VALUE  "CLEAR SCREEN".
      **********
       COPY LSMSG_PR.
      ******************************************************************
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
           MOVE WHACCNTCD   TO AM-KEY.
           PERFORM AMG-RTN THRU AMG-EX.
           MOVE 1           TO CNT.
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
               PERFORM TAX-RTN THRU TAX-EX
               MOVE WHACCNTCD   TO AM-KEY
               PERFORM AMG-RTN THRU AMG-EX
               IF  NWHTAXKB NOT = OWHTAXKB
                   MOVE 1     TO CNT
               END-IF
           END-IF.
           PERFORM ADD-RTN THRU ADD-EX.
           PERFORM SDH-RTN THRU SDH-EX.
           MOVE NEW-KEY      TO OLD-KEY.
           GO TO MR110.
       MR190.
           PERFORM TAX-RTN THRU TAX-EX.
       MR900.
           PERFORM CLSE-ENT THRU CLSE-EXT.
       MR999.
           CALL "DB_Close".
           STOP RUN.
      ************************
       INI-RTN.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE 255.
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
           IF   I    =  2
                MOVE TAX-RITU(1)   TO WTAX-J
           END-IF.
           MOVE TAX-CODE        TO WTAX-CD.
           MOVE TAX-CODE1       TO WTAX-CD1.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
      *
           COMPUTE WTAX-B = WTAX-A + 100.
           IF  I    =  2
               COMPUTE WTAX-K = WTAX-J + 100
           END-IF.
      *
           CALL "DB_F_Open" USING
            "I-O" NS-DNO_PNAME1 "SHARED" BY REFERENCE NS-DNO_IDLST "1"
            "DNO1-KEY" BY REFERENCE DNO1-KEY.
           MOVE "10"     TO DNO1-KEY ERR-K.
      *           READ NS-DNO UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NS-DNO_PNAME1 BY REFERENCE DNO-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "NS-DNO"     TO ERR-F
               MOVE "G"          TO ERR-M
	           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                             RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE NS-DNO_IDLST NS-DNO_PNAME1
               MOVE 1     TO ERR-SW
               GO TO INI-EX
           END-IF.
       INI-EX.
           EXIT.
      *********
       OPEN-RTN.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO SDW_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SDW_PNAME1 "EXCLUSIVE" BY REFERENCE SDW_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "DB_F_Open" USING
            "I-O" SDH_PNAME1 "SHARED" BY REFERENCE SDH_IDLST "1"
            "SH-KEY1" BY REFERENCE SH-KEY1.
           CALL "DB_F_Open" USING
            "I-O" SDI_PNAME1 "EXCLUSIVE" BY REFERENCE SDI_IDLST "1"
            "SDI-KEY" BY REFERENCE SDI-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" NS-SIW_PNAME1 "EXCLUSIVE" BY REFERENCE 
            NS-SIW_IDLST "0".
       OPEN-EX.
           EXIT.
      *********
       TAX-RTN.
           IF  DR-CR = 1
               COMPUTE W-TK = W-KR - W-KS
           ELSE
               COMPUTE W-TK = W-KS - W-KR
           END-IF.
           IF  W-TK = ZERO
               GO TO TAX-999
           END-IF.
           IF  (I    =  2) AND (OWHTAXKB = "1" OR "5")
               COMPUTE W-TAX ROUNDED = W-TK * WTAX-J / WTAX-K
           ELSE
               COMPUTE W-TAX ROUNDED = W-TK * WTAX-A / WTAX-B
           END-IF.
      *
           IF  CNT = 1
               PERFORM DNO-RTN THRU DNO-EX
           END-IF.
           PERFORM SDI-RTN THRU SDI-EX.
           PERFORM SIW-RTN THRU SIW-EX.
           IF  CNT NOT = 5
               ADD  1     TO CNT
           ELSE
               MOVE 1     TO CNT
           END-IF.
       TAX-999.
           MOVE ZERO     TO SUM-AREA.
       TAX-EX.
           EXIT.
      *********
       ADD-RTN.
           IF  WHDR-CR = 1
               ADD WHAMOUNT     TO W-KR
           ELSE
               ADD WHAMOUNT     TO W-KS
           END-IF.
       ADD-EX.
           EXIT.
      *********
       SDH-RTN.
           MOVE WHTRDATE     TO HTRDATE.
           MOVE WHJUNLNO     TO HJUNLNO.
           MOVE WHLINENO     TO HLINENO.
           MOVE WHDR-CR      TO HDR-CR.
      *           READ SDH INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" SDH_PNAME1 BY REFERENCE SH-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO SDH-EX
           END-IF.
           MOVE 1            TO HCOM.
           MOVE SH-KEY1      TO ERR-K.
      *           REWRITE SH-REC INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SDH_PNAME1 SDH_LNAME SH-REC RETURNING RET.
           IF  RET = 1
               MOVE "SDH"     TO ERR-F
               MOVE "R"       TO ERR-M
               PERFORM  ERR-ENT  THRU  ERR-EXT
           END-IF.
       SDH-EX.
           EXIT.
      *********
       DNO-RTN.
           MOVE "10"     TO DNO1-KEY ERR-K.
      *           READ NS-DNO INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NS-DNO_PNAME1 BY REFERENCE DNO-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE "NS-DNO"     TO ERR-F
               MOVE "G"          TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
       DNO-010.
           IF  DNO1-023 = DNO1-022
               MOVE DNO1-021     TO DNO1-023
           ELSE
               ADD  1            TO DNO1-023
           END-IF.
           MOVE Z-GEMYMD     TO SDIYMD.
           MOVE DNO1-023     TO SDIJNO.
           MOVE ZERO         TO SDILNO.
      *           START SDI KEY NOT < SDI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDI_PNAME1 "SDI-KEY" " NOT < " SDI-KEY RETURNING RET.
           IF  RET = 1
               GO TO DNO-020
           END-IF
      *           READ SDI NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDI_PNAME1 BY REFERENCE SDI-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO DNO-020
           END-IF.
           IF (Z-GEMYMD = SDIYMD) AND (DNO1-023 = SDIJNO)
               GO TO DNO-010
           END-IF.
       DNO-020.
           MOVE Z-GEMYMD     TO HTRDATE.
           MOVE DNO1-023     TO HJUNLNO.
           MOVE ZERO         TO HLINENO HDR-CR.
      *           START SDH KEY NOT < SH-KEY1 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDH_PNAME1 "SH-KEY1" " NOT LESS " SH-KEY1 RETURNING RET.
           IF  RET = 1
               GO TO DNO-030
           END-IF
      *           READ SDH NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO DNO-030
           END-IF.
           IF (Z-GEMYMD = HTRDATE) AND (DNO1-023 = HJUNLNO)
               GO TO DNO-010
           END-IF.
       DNO-030.
           MOVE DNO1-023     TO WDNO1-023.
      *           REWRITE DNO1-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            NS-DNO_PNAME1 NS-DNO_LNAME DNO1-R RETURNING RET.
           IF  RET = 1
               MOVE "NS-DNO"     TO ERR-F
               MOVE "R"          TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
       DNO-EX.
           EXIT.
      *********
       SDI-RTN.
           MOVE SPACE     TO SDI-REC.
           INITIALIZE        SDI-REC.
           MOVE Z-GEMYMD     TO SDIYMD.
           MOVE WDNO1-023    TO SDIJNO.
           MOVE CNT          TO SDILNO.
           IF  DR-CR  =  1
               MOVE W-TAX       TO KRKIN
               MOVE OWHKACD1    TO KSCD
               MOVE OWHSECTCD   TO KSSECT
               MOVE W-TAX       TO KSKIN
               MOVE TEG-BAN     TO KS-TB
               IF  OWHTAXKB = "1" OR "3"
                   MOVE WTAX-CD     TO KRCDM
               ELSE
                   MOVE WTAX-CD1    TO KRCDM
               END-IF
           ELSE
               MOVE OWHKACD1    TO KRCD
               MOVE OWHSECTCD   TO KRSECT
               MOVE W-TAX       TO KRKIN
               MOVE TEG-BAN     TO KR-TB
               MOVE W-TAX       TO KSKIN
               IF  OWHTAXKB = "1" OR "3"
                   MOVE WTAX-CD     TO KSCDM
               ELSE
                   MOVE WTAX-CD1    TO KSCDM
               END-IF
           END-IF.
           MOVE OWHTAXKB    TO SDIETAX.
           MOVE OWHKACD1    TO KNG-KEY.
      *           READ KNG UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               INITIALIZE KNG-R
           END-IF.
           MOVE SPACE       TO W-TEKI.
           MOVE KNGNMN      TO W-TEKI2.
           IF  OWHTAXKB = "1" OR "3"
               MOVE WK-NAME1     TO W-TEKI1
           ELSE
               MOVE WK-NAME2     TO W-TEKI1
           END-IF.
           MOVE W-TEKI      TO SDITEKI.
           MOVE SDI-REC     TO SIW-R.
           MOVE SDI-KEY     TO ERR-K.
      *           WRITE SDI-REC INVALID KEY
      *///////////////
           CALL "DB_Insert" USING
            SDI_PNAME1 SDI_LNAME SDI-REC RETURNING RET.
           IF  RET = 1
               MOVE "SDI"     TO ERR-F
               MOVE "W"       TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE 000.
       SDI-EX.
           EXIT.
      *********
       SIW-RTN.
           INITIALIZE SIW-97 SIW-98 SIW-99.
           MOVE  SIW-R     TO ERR-K.
      *           WRITE SIW-R.
      *///////////////
           CALL "DB_Insert" USING
            NS-SIW_PNAME1 NS-SIW_LNAME SIW-R RETURNING RET.
           IF  ERR-STAT  NOT =  "00"
               MOVE "NS-SIW"     TO   ERR-F
               MOVE  "W"         TO   ERR-M
               PERFORM ERR-ENT   THRU ERR-EXT
           END-IF.
       SIW-EX.
           EXIT.
      *********
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
      *********
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE SDW_IDLST SDW_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDI_IDLST SDI_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NS-SIW_IDLST NS-SIW_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NS-DNO_IDLST NS-DNO_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
       CLSE-EXT.
           EXIT.
      *********
       COPY LPMSG_PR.
      *********

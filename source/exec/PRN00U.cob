       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 PRMENU.
       AUTHOR.                     OZAKI.
      *===============================================================*
      *    財務管理システム（初期メニュー）                           *
      *                            --- 90/01/21 ---                   *
      *===============================================================*
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.
       SOURCE-COMPUTER.           NEAC-SYSTEM100.
       OBJECT-COMPUTER.           NEAC-SYSTEM100.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
       77  ERR-STAT            PIC  X(02).
       77  WKSP                PIC  X(01) VALUE SPACE.
       01  CRT-WK.
           02  W-01.
             03  W-01Y         PIC  9(04).
             03  W-01YD  REDEFINES W-01Y.
               04  W-01Y1      PIC  9(02).
               04  W-01Y2      PIC  9(02).
             03  W-01M         PIC  9(02).
             03  W-01D         PIC  9(02).
           02  W-01R REDEFINES W-01     PIC  9(08).
           02  W-OKC           PIC  X(01).
       01  WORK-AREA.
           02  I               PIC  9(02).
           02  ERR-SW          PIC  9(01).
           02  FROM-YMD.
             03  FROM-YM.
               04  FROM-Y      PIC  9(04).
               04  FROM-M      PIC  9(02).
             03  FROM-D        PIC  9(02).
           02  TO-YMD.
             03  TO-YM.
               04  TO-Y        PIC  9(04).
               04  TO-M        PIC  9(02).
             03  TO-D          PIC  9(02).
      *********
       COPY LWMSG_PR.
      *********
       COPY LIBFDD.
       COPY FCTL.
      *****
       77  USER_ID      PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
       77  ESTAT        PIC  X(002).
       77  RESU         PIC  9(001).
       77  RET          PIC  9(001) VALUE ZERO.
      *
       01  DISP-C.
           02  DISP-CLE  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  ACP-AREA.
           02  ACP-W01.
               03  ACP-W01Y              PIC 9(02).
               03  ACP-W01M              PIC 9(02).
               03  ACP-W01D              PIC 9(02).
           02  ACP-WOKC                  PIC X(01).
      ***
       01  DSP-AREA.
           02  DSP-UPDYM   .
               03  ACP-UPDYY             PIC 9(02).
               03  ACP-UPDMM             PIC 9(02).
           02  DSP-TOUF    .
               03  ACP-TOUFYY            PIC 9(02).
               03  ACP-TOUFMM            PIC 9(02).
               03  ACP-TOUFDD            PIC 9(02).
           02  DSP-TOUT    .
               03  ACP-TOUTYY            PIC 9(02).
               03  ACP-TOUTMM            PIC 9(02).
               03  ACP-TOUTDD            PIC 9(02).
      ***
       01  MG-AREA.
           02  MG-01              PIC N(07)
               VALUE "日付期間エラー".
      ***
       01  DSP-SP-AREA.
           02  SP-WOKC            PIC X(01).
      *********
       COPY LSMSG_PR.
       COPY LIBSCR.
      **********
       PROCEDURE                   DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *DISP-C
       CALL "SD_Init" USING
            "DISP-C" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-CLE" "X" "1" "0" "12" " " "DISP-C" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-W01" " " "11" "0" "6" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-W01Y" "9" "11" "13" "2" " " "ACP-W01" RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-W01Y" BY REFERENCE W-01Y2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-W01M" "9" "11" "19" "2" "ACP-W01Y" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-W01M" BY REFERENCE W-01M "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-01D" "9" "11" "25" "2" "ACP-W01M" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-01D" BY REFERENCE W-01D "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-WOKC" "X" "24" "77" "1" "ACP-W01" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-WOKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "16" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-UPDYM" " " "2" "0" "4" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-UPDYY" "9" "2" "65" "2" " " "DSP-UPDYM"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-UPDYY" BY REFERENCE FCTL-UPDYY2 "2" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-UPDMM" "9" "2" "69" "2" "DSP-UPDYY" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-UPDMM" BY REFERENCE FCTL-UPDMM "2" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TOUF" " " "14" "0" "6" "DSP-UPDYM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TOUFYY" "9" "14" "21" "2" " " "DSP-TOUF"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TOUFYY" BY REFERENCE FCTL-TOUFYY2(1) "2" "1"
            BY REFERENCE I 16 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TOUFMM" "9" "14" "25" "2" "DSP-TOUFYY" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TOUFMM" BY REFERENCE FCTL-TOUFMM(1) "2" "1"
            BY REFERENCE I 16 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TOUFDD" "9" "14" "29" "2" "DSP-TOUFMM" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TOUFDD" BY REFERENCE FCTL-TOUFDD(1) "2" "1"
            BY REFERENCE I 16 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TOUT" " " "16" "0" "6" "DSP-TOUF" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TOUTYY" "9" "16" "21" "2" " " "DSP-TOUT"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TOUTYY" BY REFERENCE FCTL-TOUTYY2(1) "2" "1"
            BY REFERENCE I 16 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TOUTMM" "9" "16" "25" "2" "DSP-TOUTYY" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TOUTMM" BY REFERENCE FCTL-TOUTMM(1) "2" "1"
            BY REFERENCE I 16 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TOUTDD" "9" "16" "29" "2" "DSP-TOUTMM" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TOUTDD" BY REFERENCE FCTL-TOUTDD(1) "2" "1"
            BY REFERENCE I 16 RETURNING RESU.
      *MG-AREA
       CALL "SD_Init" USING
            "MG-AREA" " " "0" "0" "14" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "MG-01" "N" "24" "2" "14" " " "MG-AREA" RETURNING RESU.
      *DSP-SP-AREA
       CALL "SD_Init" USING
            "DSP-SP-AREA" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "SP-WOKC" "X" "24" "77" "1" " " "DSP-SP-AREA"
            RETURNING RESU.
       CALL "SD_From" USING
            "SP-WOKC" BY REFERENCE WKSP "1" "0"
            RETURNING RESU.
      *
           COPY LSMSG_PR_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MR000.
           PERFORM INI-RTN THRU INI-EX.
           IF  ERR-SW = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO MR999
           END-IF.
       MR010.
           CALL "SD_Output" USING "DSP-SP-AREA" DSP-SP-AREA "p"
                                  RETURNING RESU.
           PERFORM W01-RTN THRU W01-EX.
           IF  ESTAT = "P9"
               MOVE SPACE     TO W-OKC
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO MR900
           END-IF.
       MR020.
           PERFORM WOKC-RTN THRU WOKC-EX.
           IF  ESTAT = "09"
               GO TO MR010
           END-IF.
           IF  W-OKC = "9"
               GO TO MR010
           END-IF.
      **
           PERFORM UPD-RTN THRU UPD-EX.
       MR900.
           PERFORM CLSE-ENT THRU CLSE-EXT.
       MR999.
           CALL "DB_Close".
           STOP RUN.
      ************************
       INI-RTN.
      *           CALL "GRMENU".
           CALL "SD_Screen_Output" USING "GRMENU" RETURNING RESU.
           MOVE 0     TO ERR-SW.
           COPY LIBCPR.
           CALL "DB_F_Open" USING
            "I-O" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE "DATE  "     TO FCTL-KEY1 ERR-K.
      *           READ FCTL-F INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE "FCONTRL"     TO ERR-F
               MOVE "G"           TO ERR-M
               CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p"
                                        RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                        RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               MOVE 1     TO ERR-SW
               GO TO INI-EX
           END-IF.
           CALL "SD_Output" USING "DSP-UPDYM" DSP-UPDYM "p"
                                  RETURNING RESU.
      *
           MOVE FCTL-TOUT(15)     TO TO-YMD.
           MOVE FCTL-UPDYM        TO FROM-YM.
           MOVE 01                TO FROM-D.
           MOVE 1     TO I.
       INI-000.
           IF  (FCTL-TOUF(I) NOT > FROM-YMD) AND
               (FCTL-TOUT(I) NOT < FROM-YMD)
               GO TO INI-100
           END-IF.
           IF  I NOT = 15
               ADD 1     TO I
               GO TO INI-000
           END-IF.
           IF  FCTL-KSMM = 12
               MOVE 1     TO I
           ELSE
               COMPUTE I = FCTL-KSMM + 1
           END-IF.
           MOVE FCTL-TOUF(I)              TO FROM-YMD.
           GO TO INI-EX.
       INI-100.
           IF  I = FCTL-KSMM
               MOVE 13     TO I
           ELSE
               IF  I = 12
                   MOVE 1     TO I
               ELSE
                   ADD 1     TO I
               END-IF
           END-IF.
           IF I > 15
              CALL "SD_Output" USING "MG-01" MG-01 "p"
                                  RETURNING RESU
              CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                  RETURNING RESU
              MOVE 1     TO ERR-SW
              GO TO INI-EX
           END-IF.
           MOVE FCTL-TOUF(I)     TO FROM-YMD.
       INI-EX.
           EXIT.
      *********
       W01-RTN.
           CALL "SD_Accept" USING
               BY REFERENCE ACP-W01 "ACP-W01" " " "2"
               BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                             RETURNING RESU.
           IF  ESTAT = "P9"
               GO TO W01-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W01-RTN
           END-IF.
           IF  (W-01M < 1 OR > 12) OR
               (W-01D < 1 OR > 31)
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                            RETURNING RESU
               GO TO W01-RTN
           END-IF.
      *
           MOVE ZERO TO W-01Y1.
           IF  W-01Y2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-01Y
           END-IF.
           IF  W-01Y2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-01Y
           END-IF.
      *
           IF  W-01 < FROM-YMD OR > TO-YMD
               CALL "SD_Output" USING "MG-01" MG-01 "p"
                                            RETURNING RESU
               GO TO W01-RTN
           END-IF.
       W01-000.
           MOVE 1     TO I.
       W01-100.
           IF  (FCTL-TOUF(I) NOT > W-01) AND
               (FCTL-TOUT(I) NOT < W-01)
               CONTINUE
           ELSE
              IF  I NOT = 15
                  ADD 1     TO I
                  GO TO W01-100
              ELSE
                  CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                            RETURNING RESU
                  GO TO W01-RTN
              END-IF
           END-IF.
      *
           CALL "SD_Output" USING "DSP-TOUF" DSP-TOUF "p"
                                           RETURNING RESU.
           CALL "SD_Output" USING "DSP-TOUT" DSP-TOUT "p"
                                           RETURNING RESU.
       W01-EX.
           EXIT.
      *********
       WOKC-RTN.
           CALL "SD_Accept" USING
               BY REFERENCE ACP-WOKC "ACP-WOKC" "X" "1"
               BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                             RETURNING RESU.
           IF  ESTAT = "09"
               GO TO WOKC-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO WOKC-RTN
           END-IF.
           IF  W-OKC NOT = "1" AND "9"
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                            RETURNING RESU
               GO TO WOKC-RTN
           END-IF.
       WOKC-EX.
           EXIT.
      *********
       UPD-RTN.
           MOVE W-01             TO FCTL-KONYMD.
           MOVE FCTL-TOUF(I)     TO FCTL-GESYMD.
           MOVE FCTL-TOUT(I)     TO FCTL-GEMYMD.
      *           REWRITE FCTL-REC INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            FCTL-F_PNAME1 FCTL-F_LNAME FCTL-REC RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               MOVE "FCONTRL"     TO ERR-F
               MOVE "R"           TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
       UPD-EX.
           EXIT.
      *********
       CLSE-ENT.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           CALL "SD_Output" USING "DISP-C" DISP-C "p"
                                  RETURNING RESU.
       CLSE-EXT.
           EXIT.
      *********
       COPY LPMSG_PR.
      *********

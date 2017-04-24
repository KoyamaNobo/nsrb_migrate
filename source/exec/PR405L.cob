       IDENTIFICATION                     DIVISION.
       PROGRAM-ID.                        PR405L.
      *>=========================================================<*
      *>                                                         <*
      *>       USER     NAME.....                                <*
      *>       BASE         ..... ZA0212                         <*
      *>       PROGRAM  TITLE.... 総勘定内訳表                   <*
      *>       AUTHOR   ......... MAYUMI.I                       <*
      *>       DATE     WRITTEN.. 91/01/10                       <*
      *>                                                         <*
      *>=========================================================<*
      *
       ENVIRONMENT                        DIVISION.
       CONFIGURATION                      SECTION.
       SOURCE-COMPUTER.                   SYSTEM3100.
       OBJECT-COMPUTER.                   SYSTEM3100.
       INPUT-OUTPUT                       SECTION.
       DATA                  DIVISION.
      ******************************************************
       WORKING-STORAGE                SECTION.
      ******************************************************
      *
       01  W1.
           02  ERR-STAT      PIC X(2).
           02  PCNT          PIC 9(5).
           02  W-ZZZZ9       PIC ZZZZ9.
           02  W-Z9          PIC Z9.
           02  W-KAKU        PIC X(1).
       01  WORK-AREA.
           02  SET-WORK.
             03  W-ZAN         PIC S9(11).
             03  W-KARI        PIC S9(11).
             03  W-KASI        PIC S9(11).
             03  T-ZAN         PIC S9(11).
           02  SOE             PIC  9(02).
           02  FI              PIC  9(02).
           02  TI              PIC  9(02).
           02  SW              PIC  9(01).
           02  LCNT            PIC  9(02).
      *
       01  HIZUKE            PIC  9(06).
       01  HIZUKER  REDEFINES  HIZUKE.
           02  W-YY              PIC  9(02).
           02  W-MM              PIC  9(02).
           02  W-DD              PIC  9(02).
      *
       01  SV-KEY.
           02  SV-KY1        PIC  9(04).
       01  FROM-TO.
           02  FROM-CD       PIC  9(04).
           02  TO-CD         PIC  9(04).
       01  GOKEI-R.
           02  GOKEI-1       PIC  S9(12).
           02  GOKEI-2       PIC  S9(12).
           02  GOKEI-3       PIC  S9(12).
           02  GOKEI-4       PIC  S9(12).
      *
       01  MEISAI-R.
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(01)  VALUE   SPACE.
           02  HOJYMEI       PIC  N(10).
           02  FILLER        PIC  X(01)  VALUE   "(".
           02  HJ-COD        PIC  9(04).
           02  FILLER        PIC  X(01)  VALUE   ")".
           02  FILLER        PIC  X(01)  VALUE   SPACE.
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  ZENZAN        PIC  ----,---,---,--9.
           02  FILLER        PIC  X(01).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  T-KARI        PIC  ----,---,---,--9.
           02  FILLER        PIC  X(01).
           02  T-KASHI       PIC  ----,---,---,--9.
           02  FILLER        PIC  X(01).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  THOZAN        PIC  ----,---,---,--9.
           02  FILLER        PIC  X(01).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(01).
           02  FILLER        PIC  N(01)  VALUE   "　".
       01  TY-SEN.
           02  FILLER        PIC  X(02)  VALUE   X"1AC0".
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(28).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(17).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(34).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(17).
           02  FILLER        PIC  X(02)  VALUE   X"1AC1".
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(01).
           02  FILLER        PIC  N(01)  VALUE   "　".
       01  TATESEN.
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(28).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(17).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(34).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(17).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(01).
           02  FILLER        PIC  N(01)  VALUE   "　".
      *
       01  TATESEN-1.
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(12).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(22).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(06).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  N(01)  VALUE   "　".
      *
       01  YOKOSEN-2.
           02  FILLER        PIC  X(02)  VALUE   X"1AC0".
           02  FILLER        PIC  X(40).
           02  FILLER        PIC  X(02)  VALUE   X"1AC1".
      *
       01  MIDASHI-1.
           02  P-K           PIC  X(05)  VALUE   X"1A24212474".
           02  FILLER        PIC  X(02).
           02  Y-1           PIC  N(02).
           02  FILLER        PIC  N(01)  VALUE   "年".
           02  M-1           PIC  N(02).
           02  FILLER        PIC  N(01)  VALUE   "月".
           02  D-1           PIC  N(02).
           02  FILLER        PIC  N(01)  VALUE   "日".
           02  FILLER        PIC  N(02)  VALUE   "作成".
           02  FILLER        PIC  X(18)  VALUE   SPACE.
           02  FILLER        PIC  X(02)  VALUE   X"1AC0".
           02  FILLER        PIC  X(24)
                             VALUE  " 総　勘　定　内　訳　表 ".
           02  FILLER        PIC  X(02)  VALUE   X"1AC1".
           02  FILLER        PIC  X(17).
           02  PA-G          PIC  N(05).
           02  FILLER        PIC  N(01)  VALUE   "頁".
      *
       01  MIDASHI-2.
           02  FILLER        PIC  X(02)  VALUE   X"1AC0".
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(12)  VALUE   " 勘定科目名 ".
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(01)  VALUE   SPACE.
           02  KAMO-MEI      PIC  N(10).
           02  FILLER        PIC  X(01)  VALUE   SPACE.
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(01)  VALUE   " ".
           02  HOSYU-CD      PIC  9(04).
           02  FILLER        PIC  X(01)  VALUE   " ".
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(01).
           02  FILLER        PIC  N(01)  VALUE   "（".
           02  Y-2           PIC  N(02).
           02  FILLER        PIC  N(01)  VALUE   "年".
           02  M-2           PIC  N(02).
           02  FILLER        PIC  N(01)  VALUE   "月".
           02  D-2           PIC  N(02).
           02  FILLER        PIC  N(03)  VALUE   "日現在".
           02  FILLER        PIC  N(01)  VALUE   "）".
           02  FILLER        PIC  X(29).
           02  FILLER        PIC  X(02)  VALUE   X"1AC1".
      *
       01  MIDASHI-3.
           02  FILLER        PIC  X(02)  VALUE   X"1AC0".
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(05).
           02  FILLER        PIC  N(05)  VALUE   "補助科目名".
           02  FILLER        PIC  X(06).
           02  FILLER        PIC  X(07)  VALUE   "(ｺｰﾄﾞ) ".
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(02).
           02  FILLER        PIC  N(07)  VALUE   "前　月　残　高".
           02  FILLER        PIC  X(01).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(02).
           02  FILLER        PIC  N(07)  VALUE   "当　月　借　方".
           02  FILLER        PIC  X(03).
           02  FILLER        PIC  N(07)  VALUE   "当　月　貸　方".
           02  FILLER        PIC  X(01).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(02).
           02  FILLER        PIC  N(07)  VALUE   "当　月　残　高".
           02  FILLER        PIC  X(01).
           02  FILLER        PIC  X(02)  VALUE   X"1AC1".
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(01).
           02  FILLER        PIC  N(01)  VALUE   "　".
      *
       01  GOKEI-M.
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(05).
           02  FILLER        PIC  N(01)  VALUE   "合".
           02  FILLER        PIC  X(14).
           02  FILLER        PIC  N(01)  VALUE   "計".
           02  FILLER        PIC  X(05).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  GOK-1         PIC  ----,---,---,--9.
           02  FILLER        PIC  X(01).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  GOK-2         PIC  ----,---,---,--9.
           02  FILLER        PIC  X(01).
           02  GOK-3         PIC  ----,---,---,--9.
           02  FILLER        PIC  X(01).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  GOK-4         PIC  ----,---,---,--9.
           02  FILLER        PIC  X(01).
           02  FILLER        PIC  X(02)  VALUE   X"1AC2".
           02  FILLER        PIC  X(01).
           02  FILLER        PIC  N(01)  VALUE   "　".
      *
           COPY              LWMSG_PR.
      *
           COPY              KANGEL.
      *
           COPY              LHOZAN.
      *
           COPY              ACCUNT.
      *
           COPY              FCTL.
      *
      *       FD  PRN-F
       77  PRN-R             PIC      X(270).
      *
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      *
       01  DSP-CLR-AREA.
           03  DSP-CLR.
               05  FILLER     PIC  X(12)  VALUE  "CLEAR SCREEN".
           03  DSP-REVERSE    PIC  X(14)  VALUE  "              " .
       01  DSP-AREA.
           03  DSP-NEN.
               05  FILLER     PIC  N(01)  VALUE  "年".
               05  FILLER     PIC  N(02)  VALUE  "月度".
               05  FILLER     PIC  9(02).
               05  FILLER     PIC  9(02).
           03  DSP-010.
               05  DSP-011    PIC  N(06)  VALUE  "総勘定内訳表".
           03  DSP-020.
               05  DSP-021    PIC  N(05)
                              VALUE  "科目コード".
               05  DSP-022    PIC  N(01)  VALUE  "～".
           03  DSP-030.
               05  DSP-031    PIC  N(04)  VALUE  "ＦＲＯＭ".
               05  DSP-032    PIC  N(02)  VALUE  "ＴＯ".
           03  DSP-040.
               05  DSP-041    PIC  X(18)  VALUE  "確認 OK=1,NO=9 ( )".
       01  ACP-AREA.
           03  ACP-010.
               05  ACP-011    PIC 9(4).
               05  ACP-012    PIC 9(4).
           03  ACP-020        PIC X(1).
      *
           COPY  LSMSG_PR.
      *
      ******************************************************
       PROCEDURE                      DIVISION.
      ******************************************************
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "PRNF-999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-CLR-AREA
       CALL "SD_Init" USING
            "DSP-CLR-AREA" " " "0" "0" "26" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-CLR" " " "1" "0" "26" " " "DSP-CLR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-REVERSE" "RX" "1" "34" "14" "DSP-CLR" " "
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "64" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-NEN" " " "1" "0" "10" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-NEN" "N" "1" "4" "2" " " "DSP-NEN" RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-NEN" "N" "1" "8" "4" "01DSP-NEN" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-NEN" "9" "1" "2" "2" "02DSP-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
            "03DSP-NEN" BY REFERENCE Z-GEMYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04DSP-NEN" "9" "1" "6" "2" "03DSP-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
            "04DSP-NEN" BY REFERENCE Z-GEMMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-010" " " "1" "0" "12" "DSP-NEN" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-011" "N" "1" "35" "12" " " "DSP-010" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-020" " " "6" "0" "12" "DSP-010" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-021" "N" "6" "11" "10" " " "DSP-020" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-022" "N" "6" "40" "2" "DSP-021" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-030" " " "4" "0" "12" "DSP-020" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-031" "N" "4" "27" "8" " " "DSP-030" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-032" "N" "4" "49" "4" "DSP-031" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-040" " " "24" "0" "18" "DSP-030" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-041" "X" "24" "61" "18" " " "DSP-040" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-010" " " "6" "0" "9" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-011" "9" "6" "29" "4" " " "ACP-010" RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-011" BY REFERENCE FROM-CD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-012" "9" "6" "49" "4" "ACP-011" "　" RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-012" BY REFERENCE TO-CD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-020" "X" "24" "77" "1" "ACP-010" "　" RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-020" BY REFERENCE W-KAKU "1" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
      *
       PROG-ST.
      *
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE  "DATE  "     TO  FCTL-KEY.
      *           READ  FCTL-F  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "INV-MCT" INV-MCT "p"
                                          RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                          RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               GO  TO  END-RTN
           END-IF.
           MOVE  FCTL-REC       TO  Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
      *
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DSP-REVERSE" DSP-REVERSE "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
                                         RETURNING RESU.
      *
      *------------[  ｶﾞ ﾒ ﾝ  ｼ ｮ ﾘ  ]
       ACEP-FR.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-011 "ACP-011" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT     =    "P9"
               CALL "DB_Close"
               STOP                RUN
           END-IF.
           IF  ESTAT NOT =    "01" AND "06"
               GO        TO        ACEP-FR
           END-IF.
      *
       ACEP-TO.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-012 "ACP-012" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT     =    "09"
               GO        TO        ACEP-FR
           END-IF.
           IF  ESTAT NOT =    "01" AND "06"
               GO        TO        ACEP-TO
           END-IF.
           IF  FROM-CD        >    TO-CD
               GO  TO   ACEP-FR
           END-IF.
      *
      *------------[  ｶ ｸ ﾆ ﾝ  ﾆ ｭ ｳ ﾘ ｮ ｸ  ]
       ACEP-KA.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-020 "ACP-020" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT     =    "09"
               GO        TO        ACEP-FR
           END-IF.
           IF  ESTAT NOT =    "01" AND "06"
               GO        TO        ACEP-KA
           END-IF.
           IF  W-KAKU = "9"
               GO        TO        ACEP-FR
           END-IF.
           IF  W-KAKU NOT = "1"
               GO        TO        ACEP-KA
           END-IF.
      *
      *------------[  ﾌ ｧ ｲ ﾙ  ｵ ｰ ﾌﾟ ﾝ  ]
      *
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HZM-F_PNAME1 "SHARED" BY REFERENCE HZM-F_IDLST "1"
            "HZM-KEY" BY REFERENCE HZM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE     ZERO      TO   SW   PCNT   LCNT.
      *
           ACCEPT  HIZUKE  FROM  DATE.
           MOVE  W-YY     TO  Y-1.
           MOVE  W-MM     TO  W-Z9.
           MOVE  W-Z9     TO  M-1.
           MOVE  W-DD     TO  W-Z9.
           MOVE  W-Z9     TO  D-1.
           MOVE  Z-KONYY2    TO  Y-2.
           MOVE  Z-KONMM     TO  W-Z9.
           MOVE  W-Z9        TO  M-2.
           MOVE  Z-KONDD     TO  W-Z9.
           MOVE  W-Z9        TO  D-2.
      *
           MOVE  Z-KONYMD     TO  ZYMD.
           PERFORM  Z-RTN     THRU  Z-EXT.
           IF  ZI > 15
               GO TO END-RTN
           END-IF.
           MOVE ZI     TO TI.
           IF  TI > 12
               MOVE 13     TO FI
           ELSE
               IF  Z-KSMM = 12
                   MOVE 1     TO FI
               ELSE
                   COMPUTE FI = Z-KSMM + 1
               END-IF
           END-IF.
      *****
           MOVE FROM-CD     TO K-ACCD.
           MOVE ZERO        TO K-HOCD.
      *           START KNG KEY NOT LESS KNG-KEY INVALID
      *///////////////
           CALL "DB_Start" USING
            KNG_PNAME1 "KNG-KEY" " NOT LESS " KNG-KEY RETURNING RET.
           IF  RET = 1
               GO TO END-RTN
           END-IF.
       PRO-010.
      *           READ KNG NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO END-RTN
           END-IF.
           IF  K-ACCD < FROM-CD
               GO TO PRO-010
           END-IF.
           IF  K-ACCD > TO-CD
               GO TO END-RTN
           END-IF.
           GO TO HO-RD1.
       HO-RD.
      *           READ KNG NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO END-RTN.
           IF  K-ACCD > TO-CD
               GO TO END-RTN
           END-IF.
       HO-RD1.
           IF  K-HOCD = ZERO
               MOVE KNGNMN     TO KAMO-MEI
               MOVE K-ACCD     TO HOSYU-CD
               PERFORM AMG-RTN THRU AMG-EX
               GO TO HO-RD
           END-IF.
      *
           PERFORM HZMG-RTN THRU HZMG-EX.
           MOVE ZERO     TO SET-WORK.
           PERFORM ZAN-SET-RTN THRU ZAN-SET-EX.
      *
           IF  PCNT = 0
               MOVE K-ACCD     TO SV-KY1
           END-IF.
           IF  SV-KY1 NOT = K-ACCD
               PERFORM HEN-RTN THRU HEN-EX
           END-IF.
      *
           PERFORM MID-SUB THRU MID-EX.
      *
           MOVE KNGNMN           TO HOJYMEI.
           MOVE K-HOCD           TO HJ-COD.
           MOVE W-ZAN            TO ZENZAN.
           MOVE HZM-TJKR(TI)     TO T-KARI.
           MOVE HZM-TJKS(TI)     TO T-KASHI.
           IF  DR-CR OF AM-REC = 1
               COMPUTE T-ZAN = W-ZAN + HZM-TJKR(TI) - HZM-TJKS(TI)
           ELSE
               COMPUTE T-ZAN = W-ZAN + HZM-TJKS(TI) - HZM-TJKR(TI)
           END-IF.
           MOVE T-ZAN            TO THOZAN.
           MOVE MEISAI-R TO PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           ADD 1     TO LCNT.
           MOVE K-ACCD     TO SV-KY1.
      **
           ADD W-ZAN            TO GOKEI-1.
           ADD HZM-TJKR(TI)     TO GOKEI-2.
           ADD HZM-TJKS(TI)     TO GOKEI-3.
           ADD T-ZAN            TO GOKEI-4.
           MOVE 1     TO SW.
           GO TO HO-RD.
       END-RTN.
           IF  PCNT NOT = 0
               PERFORM HEN-SUB THRU HEN-SUB-EX
               MOVE TY-SEN TO PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
           END-IF.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HZM-F_IDLST HZM-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_Close".
           STOP               RUN.
      ******************************************************************
       HEN-RTN.
           IF  SW NOT = 0
               PERFORM HEN-SUB THRU HEN-SUB-EX
               MOVE 0        TO SW
               MOVE 90       TO LCNT
           END-IF.
           MOVE ZERO     TO GOKEI-R.
       HEN-EX.
           EXIT.
      *****
       MID-SUB.
           IF  PCNT = 0
               GO TO CROL-RD
           END-IF.
           IF  LCNT NOT > 60
               GO TO MID-EX
           END-IF.
           MOVE TY-SEN TO PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE     SPACE     TO   PRN-R.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       CROL-RD.
           ADD  1     TO  PCNT.
           MOVE     PCNT      TO   W-ZZZZ9.
           MOVE     W-ZZZZ9   TO   PA-G.
           MOVE MIDASHI-1 TO PRN-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE YOKOSEN-2 TO PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE TATESEN-1 TO PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE MIDASHI-2 TO PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE TATESEN TO PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE MIDASHI-3 TO PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE TATESEN TO PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE     8         TO   LCNT.
       MID-EX.
           EXIT.
      *
      *-----------[  ﾍ ﾝ ｼ ｭ ｳ  S U B  ]
       HEN-SUB.
           MOVE     GOKEI-1   TO   GOK-1.
           MOVE     GOKEI-2   TO   GOK-2.
           MOVE     GOKEI-3   TO   GOK-3.
           MOVE     GOKEI-4   TO   GOK-4.
           MOVE TY-SEN TO PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE TATESEN TO PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE GOKEI-M TO PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
       HEN-SUB-EX.
           EXIT.
      *****
       AMG-RTN.
           MOVE K-ACCD     TO AM-KEY.
      *           READ AM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               INITIALIZE AM-REC
           END-IF.
       AMG-EX.
           EXIT.
      *****
       HZMG-RTN.
           MOVE KNG-KEY     TO HZM-KEY.
      *           READ HZM-F UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HZM-F_PNAME1 BY REFERENCE HZM-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               INITIALIZE HZM-R
           END-IF.
       HZMG-EX.
           EXIT.
      *****
       ZAN-SET-RTN.
           IF  TI > 12
               GO TO ZAN-SET-500
           END-IF.
           MOVE FI          TO SOE.
       ZAN-SET-000.
           ADD HZM-TJKR(SOE)     TO W-KARI.
           ADD HZM-TJKS(SOE)     TO W-KASI.
           IF  SOE = TI
               GO TO ZAN-SET-900
           END-IF.
           IF  SOE = 12
               MOVE 1     TO SOE
               GO TO ZAN-SET-000
           END-IF.
           ADD 1     TO SOE.
           GO TO ZAN-SET-000.
       ZAN-SET-500.
           IF  BS-PL = 0
               MOVE 1      TO SOE
           ELSE
               MOVE 13     TO SOE.
       ZAN-SET-600.
           ADD HZM-TJKR(SOE)     TO W-KARI.
           ADD HZM-TJKS(SOE)     TO W-KASI.
           IF  SOE = TI
               GO TO ZAN-SET-900
           END-IF.
           IF  SOE = 15
               GO TO ZAN-SET-900
           END-IF.
           ADD 1     TO SOE.
           GO TO ZAN-SET-600.
       ZAN-SET-900.
           IF  DR-CR = 1
               COMPUTE W-ZAN = HZM-ZAN + (W-KARI - HZM-TJKR(TI)) -
                               ( W-KASI - HZM-TJKS(TI))
           ELSE
               COMPUTE W-ZAN = HZM-ZAN + (W-KASI - HZM-TJKS(TI)) -
                               ( W-KARI - HZM-TJKR(TI))
           END-IF.
       ZAN-SET-EX.
           EXIT.
      *****
       CLSE-ENT.
       CLSE-EXT.
      *
           COPY  LPMSG_PR.
      *

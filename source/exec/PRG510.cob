      *************************************************
      *    PROGRAM        :　仕訳データ抽出(EXCEL)    *
      *************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         PRG510.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM3100.
       OBJECT-COMPUTER.    NEAC-SYSTEM3100.
       INPUT-OUTPUT        SECTION.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT            PIC X(02).
       77  W-DMM               PIC 9(01).
       01  W-DATA.
           02  W-SNGP          PIC  9(08).
           02  W-SNGPD REDEFINES W-SNGP.
             03  W-SNEN        PIC 9(04).
             03  W-SGET        PIC 9(02).
             03  W-SPEY        PIC 9(02).
           02  W-ENGP          PIC  9(08).
           02  W-ENGPD REDEFINES W-ENGP.
             03  W-ENEN        PIC 9(04).
             03  W-EGET        PIC 9(02).
             03  W-EPEY        PIC 9(02).
           02  W-L             PIC  9(02).
           02  CNT             PIC  9(02).
           02  W-DC            PIC  9(01).
           02  W-AKAM.
             03  W-KAMD  OCCURS 10.
               04  W-KAM       PIC  9(04).
               04  W-HOJ       PIC  9(04).
           02  W-AKAN.
             03  W-KAND  OCCURS 10.
               04  W-KAN       PIC  N(10).
       01  SDH_PRG510.
           02  SDH_PNAME1      PIC  X(009)  VALUE "SIWAKE-H1".
           02  F               PIC  X(001).
           02  SDH_LNAME       PIC  X(003)  VALUE "SDH".
           02  F               PIC  X(001).
           02  SDH_KEY1        PIC  X(100)  VALUE SPACE.
           02  SDH_KEY2        PIC  X(100)  VALUE SPACE.
           02  SDH_KEY3        PIC  X(100)  VALUE SPACE.
           02  SDH_SORT        PIC  X(100)  VALUE SPACE.
           02  SDH_IDLST       PIC  X(100)  VALUE SPACE.
           02  SDH_RES         USAGE  POINTER.
       COPY  SIWAKH.
       COPY  KANGEL.
       01  SIWA-F_PRG510.
           02  SIWA-F_PNAME1   PIC  X(009)  VALUE "WK0128000".
           02  F               PIC  X(001).
           02  SIWA-F_LNAME    PIC  X(013)  VALUE "SIWA-F_PRG510".
           02  F               PIC  X(001).
           02  SIWA-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  SIWA-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  SIWA-F_SORT     PIC  X(100)  VALUE SPACE.
           02  SIWA-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  SIWA-F_RES         USAGE  POINTER.
       01  SIWA-REC.
           02  S-KACD1.
             03  S-ACCNTCD     PIC 9(4).
             03  S-HOACCNT     PIC 9(4).
           02  S-KNGNMN        PIC N(10).
           02  S-TRDATE        PIC 9(8).
           02  S-JUNLNO        PIC 9(6).
           02  S-LINENO        PIC 9(2).
           02  S-DR-CR         PIC 9(1).
           02  S-SECTCD        PIC 9(4).
           02  S-AMOUNT        PIC S9(10).
           02  S-KACD2.
             03  S-OPPCD       PIC 9(4).
             03  S-HOOPPCD     PIC 9(4).
           02  S-TEKIYO        PIC N(20).
           02  S-NAMEN         PIC N(10).
           02  F               PIC X(01).
       77      F               PIC X(001).
           COPY LWMSG_PR.
       77  USER_ID             PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE     PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER       PIC  9(003).
       77  ESTAT               PIC  X(002).
       77  RESU                PIC  9(001).
       77  RESP                PIC  9(001).
       77  RET                 PIC  9(001) VALUE ZERO.
       01  C-CLEAR.
           02  FILLER          PIC  X(12)  VALUE  "CLEAR SCREEN".
       01  C-MID.
           02  FILLER.
               03  FILLER      PIC  X(20)  VALUE "　仕訳データ　抽出　".
           02  FILLER.
               03  FILLER      PIC  X(38)  VALUE
                   "'    年   月   日 ～ '    年   月   日".
           02  FILLER          PIC  X(40)  VALUE
                   "科目ｺｰﾄﾞ      -                         ".
           02  FILLER          PIC  X(40)  VALUE
                   "              -                         ".
           02  FILLER          PIC  X(40)  VALUE
                   "              -                         ".
           02  FILLER          PIC  X(40)  VALUE
                   "              -                         ".
           02  FILLER          PIC  X(40)  VALUE
                   "              -                         ".
           02  FILLER          PIC  X(40)  VALUE
                   "              -                         ".
           02  FILLER          PIC  X(40)  VALUE
                   "              -                         ".
           02  FILLER          PIC  X(40)  VALUE
                   "              -                         ".
           02  FILLER          PIC  X(40)  VALUE
                   "              -                         ".
           02  FILLER          PIC  X(40)  VALUE
                   "              -                         ".
           02  FILLER          PIC  X(18)  VALUE  "確認 OK=1,NO=9 ( )".
      *
       01  C-ACP.
           02  FILLER.
               03  A-SNEN      PIC 9(04).
               03  A-SGET      PIC 9(02).
               03  A-SPEY      PIC 9(02).
               03  A-ENEN      PIC 9(04).
               03  A-EGET      PIC 9(02).
               03  A-EPEY      PIC 9(02).
           02  FILLER.
               03  A-KAM       PIC 9(04).
               03  A-HOJ       PIC 9(04).
           02  A-DMM           PIC 9(01).
       01  C-DSP.
           02  DSP-KAMD        PIC N(10).
           02  DSP-KAMC.
             03  FILLER        PIC X(04)   VALUE "    ".
             03  FILLER        PIC X(04)   VALUE "    ".
             03  FILLER        PIC X(20)   VALUE "                    ".
       01  C-ERR.
           02  E-ME1           PIC  X(17)  VALUE
                "***  DATA ﾅｼ  ***".
           02  E-ME2           PIC  X(16)  VALUE
                "***  ｶﾓｸ ﾅｼ  ***".
           02  E-ME99          PIC  X(05)  VALUE  X"1B4205".
       COPY LSMSG_PR.
       PROCEDURE       DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *01  C-CLEAR.
       CALL "SD_Init" USING
            "C-CLEAR" " " "0" "0" "12" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "CLEAR" "X" "0" "0" "12" " " "C-CLEAR"  RETURNING RESU.
      *01  C-MID.
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "476" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" " " "1" "0" "20" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING
            "0101C-MID" "RX" "1" "25" "20" " " "01C-MID"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" " " "5" "0" "38" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "0102C-MID" "X" "5" "16" "38" " " "02C-MID"
            RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "X" "9" "16" "40" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "X" "10" "16" "40" "03C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "X" "11" "16" "40" "04C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "X" "12" "16" "40" "05C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "X" "13" "16" "40" "06C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "14" "16" "40" "07C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "15" "16" "40" "08C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "16" "16" "40" "09C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "11C-MID" "X" "17" "16" "40" "10C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "12C-MID" "X" "18" "16" "40" "11C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "13C-MID" "X" "23" "61" "18" "12C-MID" " "
            RETURNING RESU.
      *01  C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "25" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "5" "0" "16" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Init" USING
            "A-SNEN" "9" "5" "17" "4" " " "01C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING
            "A-SNEN" BY REFERENCE W-SNEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SGET" "9" "5" "24" "2" "A-SNEN" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SPEY" "9" "5" "29" "2" "A-SGET" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ENEN" "9" "5" "38" "4" "A-SPEY" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-ENEN" BY REFERENCE W-ENEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EGET" "9" "5" "45" "2" "A-ENEN" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EPEY" "9" "5" "50" "2" "A-EGET" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-ACP" " " "W-L" "0" "8" "01C-ACP" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "A-KAM" "9" "W-L" "26" "4" " " "02C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING
            "A-KAM" BY REFERENCE W-KAM(1) "4" "1"
            BY REFERENCE CNT 8 RETURNING RESU.
       CALL "SD_Init" USING
            "A-HOJ" "9" "W-L" "31" "4" "A-KAM" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-HOJ" BY REFERENCE W-HOJ(1) "4" "1"
            BY REFERENCE CNT 8 RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "23" "77" "1" "02C-ACP" " "  RETURNING RESU.
       CALL "SD_Into" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *01  C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "48" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-KAMD" "N" "W-L" "36" "20" " " "C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-KAMD" BY REFERENCE KNGNMN "20" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-KAMC" " " "W-L" "0" "28" "DSP-KAMD" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-KAMC" "X" "W-L" "26" "4" " " "DSP-KAMC"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-KAMC" "X" "W-L" "31" "4" "01DSP-KAMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-KAMC" "X" "W-L" "36" "20" "02DSP-KAMC" " "
            RETURNING RESU.
      *01  C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "24" "0" "38" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "16" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME2" " "  RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING
            "C-CLEAR" C-CLEAR "p" RETURNING RESU
           CALL "SD_Output" USING
            "C-MID" C-MID "p" RETURNING RESU
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE 000.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
      *
           MOVE  ZERO    TO  W-AKAM.
           MOVE  SPACE   TO  W-AKAN.
           PERFORM  S-05    THRU  S-95.
           IF  COMPLETION_CODE   =  255
               CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1
               GO  TO  M-95
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" SDH_PNAME1 "SHARED" BY REFERENCE SDH_IDLST "1"
            "SH-KEY3" BY REFERENCE SH-KEY3.
           CALL "DB_F_Open" USING
            "OUTPUT" SIWA-F_PNAME1 " " BY REFERENCE SIWA-F_IDLST "0".
      *
           MOVE  ZERO  TO  CNT  W-DC.
       M-10.
           ADD   1       TO  CNT.
           IF  CNT   =  11
               GO  TO  M-50
           END-IF.
           IF  ZERO  =  W-KAM(CNT)  AND  W-HOJ(CNT)
               GO  TO  M-50
           END-IF.
       M-15.
           MOVE SPACE        TO SH-KEY3.
           MOVE W-KAM(CNT)   TO HACCNTCD.
           MOVE W-HOJ(CNT)   TO HHOACCNT.
           MOVE W-SNGP       TO HTRDATE.
      *           START SDH KEY NOT LESS SH-KEY3 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDH_PNAME1 "SH-KEY3" " NOT LESS " SH-KEY3 RETURNING RET.
           IF  RET = 1
               GO  TO  M-10
           END-IF.
       M-20.
      *           READ  SDH  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-10
           END-IF.
           IF (W-KAM(CNT)  NOT =  HACCNTCD)  OR
              (W-HOJ(CNT)  NOT =  HHOACCNT)
               GO  TO  M-10
           END-IF.
           IF  HTRDATE      >  W-ENGP
               GO  TO  M-10
           END-IF.
           IF  W-DC         =  0
               MOVE  1     TO  W-DC
           END-IF.
       M-25.
           MOVE  SPACE        TO  SIWA-REC.
           MOVE  HKACD1       TO  S-KACD1.
           MOVE  W-KAN(CNT)   TO  S-KNGNMN.
           MOVE  HTRDATE      TO  S-TRDATE.
           MOVE  HJUNLNO      TO  S-JUNLNO.
           MOVE  HLINENO      TO  S-LINENO.
           MOVE  HDR-CR       TO  S-DR-CR.
           MOVE  HSECTCD      TO  S-SECTCD.
           MOVE  HAMOUNT      TO  S-AMOUNT.
           MOVE  HKACD2       TO  S-KACD2.
           MOVE  HTEKIYO      TO  S-TEKIYO.
           MOVE  HNAMEN       TO  S-NAMEN.
           CALL "PR_Write" USING SIWA-REC RETURNING RESP.
       M-30.
      *           READ  SDH  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-10
           END-IF.
           IF (W-KAM(CNT)  NOT =  HACCNTCD)  OR
              (W-HOJ(CNT)  NOT =  HHOACCNT)
               GO  TO  M-10
           END-IF.
           IF  HTRDATE      >  W-ENGP
               GO  TO  M-10
           END-IF.
           GO  TO  M-25.
       M-50.
           IF  W-DC     =  0
               CALL "SD_Output" USING
                " E-ME1"  E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
           END-IF.
           CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SIWA-F_IDLST SIWA-F_PNAME1.
       M-95.
           CALL "SD_Output" USING
                   "C-CLEAR" C-CLEAR "p" RETURNING RESU
           CALL "DB_Close".
           STOP RUN.
      *
       S-05.
           CALL "SD_Accept" USING
            BY REFERENCE A-SNEN "A-SNEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT         = "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO S-95
           END-IF.
           IF  ESTAT     NOT = "01" AND "06"
               GO TO S-05
           END-IF.
       S-10.
           CALL "SD_Accept" USING
            BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT         = "09"
               GO TO S-05
           END-IF.
           IF  ESTAT     NOT = "01" AND "06"
               GO TO S-10
           END-IF.
           IF  W-SGET       <  1    OR  >  12
               GO  TO  S-10
           END-IF.
       S-15.
           CALL "SD_Accept" USING
            BY REFERENCE A-SPEY "A-SPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT         = "09"
               GO TO S-10
           END-IF.
           IF  ESTAT     NOT = "01" AND "06"
               GO TO S-15
           END-IF.
           IF  W-SPEY       <  1    OR  >  31
               GO  TO  S-15
           END-IF.
       S-20.
           CALL "SD_Accept" USING
            BY REFERENCE A-ENEN "A-ENEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT         = "09"
               GO TO S-15
           END-IF.
           IF  ESTAT     NOT = "01" AND "06"
               GO TO S-20
           END-IF.
       S-25.
           CALL "SD_Accept" USING
            BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT         = "09"
               GO TO S-20
           END-IF.
           IF  ESTAT     NOT = "01" AND "06"
               GO TO S-25
           END-IF.
           IF  W-EGET       <  1    OR  >  12
               GO  TO  S-25
           END-IF.
       S-30.
           CALL "SD_Accept" USING
            BY REFERENCE A-EPEY "A-EPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT         = "09"
               GO TO S-25
           END-IF.
           IF  ESTAT     NOT = "01" AND "06"
               GO TO S-30
           END-IF.
           IF  W-EPEY       <  1    OR  >  31
               GO  TO  S-30
           END-IF.
           IF  W-SNGP    >  W-ENGP
               GO TO S-20
           END-IF.
      *
           MOVE  ZERO    TO  CNT.
           MOVE  8       TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       S-35.
           ADD   1       TO  CNT  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT   =  11
               GO  TO  S-80
           END-IF.
       S-40.
           CALL "SD_Accept" USING
            BY REFERENCE A-KAM "A-KAM" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
           IF  ESTAT         = "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO S-95
           END-IF.
           IF  ESTAT = "09"
               GO  TO  S-50
           END-IF.
           IF  ESTAT     NOT = "01" AND "06"
               GO  TO  S-40
           END-IF.
       S-45.
           CALL "SD_Accept" USING
            BY REFERENCE A-HOJ "A-HOJ" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  S-40
           END-IF.
           IF  ESTAT     NOT = "01" AND "06"
               GO  TO  S-45
           END-IF.
      *
           IF  ZERO  =  W-KAM(CNT)  AND  W-HOJ(CNT)
               IF  CNT      =   1
                   GO  TO  S-40
               ELSE
                   GO  TO  S-55
               END-IF
           END-IF.
           MOVE  W-KAM(CNT)  TO K-ACCD.
           MOVE  W-HOJ(CNT)  TO K-HOCD.
      *           READ KNG UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  S-40
           END-IF.
           CALL "SD_Output" USING
                   "DSP-KAMD" DSP-KAMD "p" RETURNING RESU
           MOVE  KNGNMN      TO W-KAN(CNT).
           GO  TO  S-35.
       S-50.
           SUBTRACT  1   FROM  CNT  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT   =  ZERO
               GO  TO  S-30
           END-IF.
       S-55.
           MOVE  ZERO   TO  W-KAM(CNT)  W-HOJ(CNT).
           MOVE  SPACE       TO W-KAN(CNT).
           CALL "SD_Output" USING
                   "DSP-KAMC" DSP-KAMC "p" RETURNING RESU
           ADD   1       TO  CNT  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT   <  11
               GO  TO  S-55
           END-IF.
       S-80.
           CALL "SD_Accept" USING
            BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  S-85
           END-IF.
           IF  ESTAT     NOT = "01" AND "06"
               GO  TO  S-80
           END-IF.
           IF  W-DMM  =  9
               GO  TO  S-05
           END-IF.
           IF  W-DMM  NOT =  1
               GO  TO  S-80
           END-IF.
           GO  TO  S-95.
       S-85.
           SUBTRACT  1   FROM  CNT  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  ZERO  =  W-KAM(CNT)  AND  W-HOJ(CNT)
               GO  TO  S-85
           END-IF.
           IF  CNT   =  ZERO
               GO  TO  S-30
           ELSE
               GO  TO  S-40
           END-IF.
       S-95.
           EXIT.
       CLSE-ENT.
       CLSE-EXT.
           EXIT.
           COPY LPMSG_PR.

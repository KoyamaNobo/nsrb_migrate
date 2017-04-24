       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             PR100U.
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       SOURCE-COMPUTER.        SYSTEM100.
       OBJECT-COMPUTER.        SYSTEM100.
       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
       01  W-KAKU              PIC X(01).
       01  ERR-STAT            PIC X(02).
      *****
       COPY BUMONF.
      *****
      ***  損益マスタ     (85/3)
       01  PL_PR100U.
           02  PL_PNAME1   PIC  X(004)  VALUE "PL-K".
           02  F           PIC  X(001).
           02  PL_LNAME    PIC  X(009)  VALUE "PL_PR100U".
           02  F           PIC  X(001).
           02  PL_KEY1     PIC  X(100)  VALUE SPACE.
           02  PL_KEY2     PIC  X(100)  VALUE SPACE.
           02  PL_SORT     PIC  X(100)  VALUE SPACE.
           02  PL_IDLST    PIC  X(100)  VALUE SPACE.
           02  PL_RES      USAGE  POINTER.
       01  PL.
           02  PL-KEY          PIC X(3).
           02  PL-LIN          PIC 9.
           02  PL-GKB          PIC 9.
           02  PL-NAM          PIC X(20).
           02  PL-NAMN     REDEFINES   PL-NAM   PIC N(10).
           02  PL-YY.
             03  PL-ZENKI      PIC S9(11).
             03  PL-TOUKI      PIC S9(11).
           02  PL-MM.
             03  PL-ZENMM      PIC S9(11).
             03  PL-TOUMM      PIC S9(11).
           02  PL-URIKB        PIC X.
           02  PL-PKB          PIC 9.
           02  PL-TANA         PIC 9.
           02  PL-YM.
             03  PL-YYWK       PIC 99.
             03  PL-MMWK       PIC 99.
           02  FILLER          PIC X(9).
       77  F                         PIC  X(001).
      *****
      ***  製造原価マスタ (85/3)
       01  GEN_PR100U.
           02  GEN_PNAME1   PIC  X(007)  VALUE "GENKA-F".
           02  F            PIC  X(001).
           02  GEN_LNAME    PIC  X(010)  VALUE "GEN_PR100U".
           02  F            PIC  X(001).
           02  GEN_KEY1     PIC  X(100)  VALUE SPACE.
           02  GEN_KEY2     PIC  X(100)  VALUE SPACE.
           02  GEN_SORT     PIC  X(100)  VALUE SPACE.
           02  GEN_IDLST    PIC  X(100)  VALUE SPACE.
           02  GEN_RES      USAGE  POINTER.
       01  GEN.
           02  GEN-KEY         PIC X(3).
           02  PL-LIN          PIC 9.
           02  PL-GKB          PIC 9.
           02  PL-NAM          PIC X(20).
           02  PL-NAMN     REDEFINES   PL-NAM   PIC N(10).
           02  PL-YY.
             03  PL-ZENKI      PIC S9(11).
             03  PL-TOUKI      PIC S9(11).
           02  PL-MM.
             03  PL-ZENMM      PIC S9(11).
             03  PL-TOUMM      PIC S9(11).
           02  PL-URIKB        PIC X.
           02  PL-PKB          PIC 9.
           02  PL-TANA         PIC 9.
           02  PL-YM.
             03  PL-YYWK       PIC 99.
             03  PL-MMWK       PIC 99.
           02  FILLER          PIC X(9).
       77  F                         PIC  X(001).
      *****
      ****************************************************************
      *                                                              *
      *               < ﾌﾞﾍﾞﾂ  ｿﾝｴｷ  ﾌｧｲﾙ >     * 85 REC / 3 B *     *
      *                                                              *
      ****************************************************************
      *
       01  BU-F_PR100U.
           02  BU-F_PNAME1   PIC  X(006)  VALUE "BUPL-K".
           02  F             PIC  X(001).
           02  BU-F_LNAME    PIC  X(011)  VALUE "BU-F_PR100U".
           02  F             PIC  X(001).
           02  BU-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  BU-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  BU-F_SORT     PIC  X(100)  VALUE SPACE.
           02  BU-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  BU-F_RES      USAGE  POINTER.
       01  BU-F.
           02  BU-KEY.
               03  BU-BUMN.
                   04  BU-BUCD     PIC 9(02).
                   04  BU-YOBI     PIC 9(02).
               03  BU-LINNO        PIC 9(03).
           02      BU-KAIP         PIC 9(01).
           02      BU-GOKBN        PIC 9(01).
           02      BU-KMKNM        PIC N(10).
           02  BU-ZEN.
               03  BU-ZENKI        PIC S9(11).
               03  BU-TOUKI        PIC S9(11).
           02  BU-DOG.
               03  BU-DOGET        PIC S9(11).
               03  BU-TOGET        PIC S9(11).
           02      BU-URKBN        PIC X(01).
           02      BU-PRKBN        PIC 9(01).
           02      BU-TBKBN        PIC 9(01).
           02      F               PIC X(09).
       77  F                       PIC  X(001).
      *****
      ****************************************************************
      *                                                              *
      *               < ﾌﾞﾍﾞﾂ  ｾｲｿﾞｳｹﾞﾝｶ F >    * 85 REC / 3 B *     *
      *                                                              *
      ****************************************************************
      *
       01  BUGEN-F_PR100U.
           02  BUGEN-F_PNAME1   PIC  X(007)  VALUE "BUGEN-K".
           02  F                PIC  X(001).
           02  BUGEN-F_LNAME    PIC  X(014)  VALUE "BUGEN-F_PR100U".
           02  F                PIC  X(001).
           02  BUGEN-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  BUGEN-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  BUGEN-F_SORT     PIC  X(100)  VALUE SPACE.
           02  BUGEN-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  BUGEN-F_RES      USAGE  POINTER.
       01  BUGEN-F.
           02  BUGEN-KEY.
               03  BU-BUMN.
                   04  BU-BUCD     PIC 9(02).
                   04  BU-YOBI     PIC 9(02).
               03  BU-LINNO        PIC 9(03).
           02      BU-KAIP         PIC 9(01).
           02      BU-GOKBN        PIC 9(01).
           02      BU-KMKNM        PIC N(10).
           02  BU-ZEN.
               03  BU-ZENKI        PIC S9(11).
               03  BU-TOUKI        PIC S9(11).
           02  BU-DOG.
               03  BU-DOGET        PIC S9(11).
               03  BU-TOGET        PIC S9(11).
           02      BU-URKBN        PIC X(01).
           02      BU-PRKBN        PIC 9(01).
           02      BU-TBKBN        PIC 9(01).
           02      F               PIC X(09).
       77  F                         PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
      ******************************
      *　　画面クリアー項目　　    *
      ******************************
       01  DSP-CLR.
           03  FILLER      PIC  X(12)  VALUE "CLEAR SCREEN".
      *******************
      *    画面表示     *
      *******************
       01  DSP-AREA.
           03  FILLER  PIC N(017) VALUE
               " 部門別損益・製造原価ファイル生成 ".
           03  FILLER  PIC X(018) VALUE  "確認 OK=1,NO=9 ( )".
      ***********************
      *    画面入力         *
      ***********************
       01  ACP-AREA.
           03  ACP-KAKU       PIC X(01).
       PROCEDURE               DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-CLR
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "52" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA" "RN" "1" "24" "34" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-AREA" "X" "24" "61" "18" "01DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAKU" "X" "24" "77" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-KAKU" BY REFERENCE W-KAKU "1" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       HAJIME.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
                                  RETURNING RESU.
       HAJIME-1.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  HAJIME-1
           END-IF.
           IF  W-KAKU  NOT =  "1" AND "9"
               GO  TO  HAJIME-1
           END-IF.
           IF  W-KAKU  =  "9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" BNM_PNAME1 
            " " BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" PL_PNAME1 
            " " BY REFERENCE PL_IDLST "1"
            "PL-KEY" BY REFERENCE PL-KEY.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" GEN_PNAME1 
            " " BY REFERENCE GEN_IDLST "1"
            "GEN-KEY" BY REFERENCE GEN-KEY.
           CALL "DB_F_Open" USING "OUTPUT SEQUENTIAL" BU-F_PNAME1 
            " " BY REFERENCE BU-F_IDLST "1"
            "BU-KEY" BY REFERENCE BU-KEY.
           CALL "DB_F_Open" USING "OUTPUT SEQUENTIAL" BUGEN-F_PNAME1 
            " " BY REFERENCE BUGEN-F_IDLST "1"
            "BUGEN-KEY" BY REFERENCE BUGEN-KEY.
       SHORI-1.
      *           READ  BNM      RECORD  AT  END
      *///////////////
           CALL "DB_Read" USING
            "RECORD AT END" BNM_PNAME1 BY REFERENCE BNM-REC " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  OWARI
           END-IF.
           IF  BNM-KA     NOT  =  "00"
               GO  TO  SHORI-1
           END-IF.
       SHORI-2.
      *           READ  PL       RECORD  AT  END
      *///////////////
           CALL "DB_Read" USING
            "RECORD AT END" PL_PNAME1 BY REFERENCE PL " " RETURNING RET.
           IF  RET = 1
               GO  TO  SHORI-3
           END-IF.
           MOVE  BNM-KEY           TO  BU-BUMN OF BU-F.
           MOVE  PL-KEY  OF PL     TO  BU-LINNO OF BU-F.
           MOVE  PL-LIN  OF PL     TO  BU-KAIP OF BU-F.
           MOVE  PL-GKB  OF PL     TO  BU-GOKBN OF BU-F.
           MOVE  PL-NAMN OF PL     TO  BU-KMKNM OF BU-F.
           MOVE  PL-URIKB OF PL    TO  BU-URKBN OF BU-F.
           MOVE  PL-PKB  OF PL     TO  BU-PRKBN OF BU-F.
           MOVE  PL-TANA OF PL     TO  BU-TBKBN OF BU-F.
           INITIALIZE       BU-ZEN OF BU-F    BU-DOG OF BU-F.
      *           WRITE   BU-F OF BU-F INVALID  KEY
      *///////////////
           CALL "DB_Insert" USING
            BU-F_PNAME1 BU-F_LNAME BU-F RETURNING RET.
           IF  RET = 1
               GO  TO  SHORI-1
           END-IF.
           GO  TO  SHORI-2.
       SHORI-3.
           CALL "DB_F_Close" USING BY REFERENCE PL_IDLST PL_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" PL_PNAME1 " " BY REFERENCE PL_IDLST "1"
            "PL-KEY" BY REFERENCE PL-KEY.
      **
       SHORI-4.
      *           READ  GEN      RECORD  AT  END
      *///////////////
           CALL "DB_Read" USING
            "RECORD AT END" GEN_PNAME1 BY REFERENCE GEN " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  SHORI-5
           END-IF.
           MOVE  BNM-KEY            TO  BU-BUMN OF BUGEN-F.
           MOVE  GEN-KEY OF GEN     TO  BU-LINNO OF BUGEN-F.
           MOVE  PL-LIN  OF GEN     TO  BU-KAIP OF BUGEN-F.
           MOVE  PL-GKB  OF GEN     TO  BU-GOKBN OF BUGEN-F.
           MOVE  PL-NAMN OF GEN     TO  BU-KMKNM OF BUGEN-F.
           MOVE  PL-URIKB OF GEN    TO  BU-URKBN OF BUGEN-F.
           MOVE  PL-PKB  OF GEN     TO  BU-PRKBN OF BUGEN-F.
           MOVE  PL-TANA OF GEN     TO  BU-TBKBN OF BUGEN-F.
           INITIALIZE       BU-ZEN OF BUGEN-F    BU-DOG OF BUGEN-F.
      *           WRITE   BUGEN-F OF BUGEN-F   INVALID  KEY
      *///////////////
           CALL "DB_Insert" USING
            BUGEN-F_PNAME1 BUGEN-F_LNAME BUGEN-F RETURNING RET.
           IF  RET = 1
               GO  TO  SHORI-1
           END-IF.
           GO  TO  SHORI-4.
       SHORI-5.
           CALL "DB_F_Close" USING BY REFERENCE GEN_IDLST GEN_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" GEN_PNAME1 " " BY REFERENCE GEN_IDLST "1"
            "GEN-KEY" BY REFERENCE GEN-KEY.
           GO  TO  SHORI-1.
       OWARI.
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE PL_IDLST PL_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BU-F_IDLST BU-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE GEN_IDLST GEN_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BUGEN-F_IDLST BUGEN-F_PNAME1.
           CALL "DB_Close".
           STOP    RUN.

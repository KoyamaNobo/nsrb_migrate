       IDENTIFICATION                     DIVISION.
       PROGRAM-ID.                        PR465L.
      *>=========================================================<*
      *>                                                         <*
      *>       USER     NAME.....                                <*
      *>       PROGRAM  NAME..... PR465L                         <*
      *>       PROGRAM  TITLE.... ﾌﾞﾓﾝﾍﾞﾂ  ｿﾝｴｷｹｲｻﾝｼｮ            <*
      *>       AUTHOR   .........                                <*
      *>       DATE     WRITTEN.. 58/07/01                       <*
      *>                                                         <*
      *>=========================================================<*
      *
       ENVIRONMENT                        DIVISION.
       CONFIGURATION                      SECTION.
       SOURCE-COMPUTER.                   SYSTEM100.
       OBJECT-COMPUTER.                   SYSTEM100.
       DATA                  DIVISION.
      ******************************************************
       WORKING-STORAGE                SECTION.
      ******************************************************
      *
       77  ERR-STAT           PIC  X(02).
       01  WK-AREA.
           02  I              PIC  9(02).
           02  P-CNT          PIC  9(03).
           02  L-CNT          PIC  9(02).
           02  SW             PIC  9(01).
           02  HIZUKE.
               03  WK-YY      PIC  9(02).
               03  WK-MM      PIC  9(02).
               03  WK-DD      PIC  9(02).
           02  SYUKEI.
               03  WK-TOUKI   PIC  S9(11).
               03  WK-ZENKI   PIC  S9(11).
           02  WK-KOSEI       PIC  S9(03)V9(12).
           02  A-CNT          PIC  9(02).
           02  WK-KONYY       PIC  9(02).
           02  WK-KONMM       PIC  9(02).
           02  WK-BUMN        PIC  9(04).
      *
       01  CHK                PIC  X(01).
       01  YMD-1              PIC  Z9.
       01  PAGE-1             PIC  ZZZ9.
      *
       01  MID-01.
           02  FILLER         PIC  X(02).
           02  P-YY           PIC  N(02).
           02  FILLER         PIC  N(01)   VALUE   "年".
           02  P-MM           PIC  N(02).
           02  FILLER         PIC  N(01)   VALUE   "月".
           02  P-DD           PIC  N(02).
           02  FILLER         PIC  N(03)   VALUE   "日作成".
           02  FILLER         PIC  X(16).
           02  FILLER         PIC  X(02)   VALUE   X"1AC0".
           02  FILLER         PIC  X(02).
           02  FILLER         PIC  N(10)   VALUE
                            "部　門　別　損　益　".
           02  FILLER         PIC  N(11)   VALUE
                            "計　算　書　（期末）　".
           02  FILLER         PIC  X(02)   VALUE   X"1AC1".
           02  FILLER         PIC  X(18).
           02  P-PAGE         PIC  N(04).
           02  FILLER         PIC  N(01)   VALUE   "頁".
      *
       01  MID-02.
           02  FILLER         PIC  X(02)   VALUE   X"1AC0".
           02  FILLER         PIC  X(36).
           02  FILLER         PIC  X(02)   VALUE   X"1AC1".
      *
       01  MID-03.
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(08).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(22).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(06).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(16).
           02  FILLER         PIC  N(01)   VALUE   "（".
           02  P-KONYY        PIC  N(02).
           02  FILLER         PIC  N(01)   VALUE   "年".
           02  P-KONMM        PIC  N(02).
           02  FILLER         PIC  N(03)   VALUE   "月分）".
      *
       01  MID-04.
           02  FILLER         PIC  X(02)   VALUE   X"1AC0".
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(01).
           02  FILLER         PIC  N(03)   VALUE   "部門名".
           02  FILLER         PIC  X(01).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(01).
           02  P-BNMNM        PIC  X(20).
           02  FILLER         PIC  X(01).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(01).
           02  P-BNMCD        PIC  9(04).
           02  FILLER         PIC  X(01).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(76).
           02  FILLER         PIC  X(02)   VALUE   X"1AC1".
      *
       01  MID-05.
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(34).
           02  FILLER         PIC  X(02)   VALUE   X"1AC0".
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(04).
           02  FILLER         PIC  N(01)   VALUE   "当".
           02  FILLER         PIC  X(06).
           02  FILLER         PIC  N(01)   VALUE   "年".
           02  FILLER         PIC  X(06).
           02  FILLER         PIC  N(01)   VALUE   "欄".
           02  FILLER         PIC  X(04).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(04).
           02  FILLER         PIC  N(01)   VALUE   "前".
           02  FILLER         PIC  X(06).
           02  FILLER         PIC  N(01)   VALUE   "年".
           02  FILLER         PIC  X(06).
           02  FILLER         PIC  N(01)   VALUE   "欄".
           02  FILLER         PIC  X(04).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(04).
           02  FILLER         PIC  N(01)   VALUE   "差".
           02  FILLER         PIC  X(06).
           02  FILLER         PIC  N(01)   VALUE   "額".
           02  FILLER         PIC  X(06).
           02  FILLER         PIC  N(01)   VALUE   "欄".
           02  FILLER         PIC  X(04).
           02  FILLER         PIC  X(02)   VALUE   X"1AC1".
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(01).
           02  FILLER         PIC  N(01)   VALUE   "　".
      *
       01  MID-06.
           02  FILLER         PIC  X(02)   VALUE   X"1AC0".
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(34).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(04).
           02  FILLER         PIC  N(01)   VALUE   "金".
           02  FILLER         PIC  X(05).
           02  FILLER         PIC  N(01)   VALUE   "額".
           02  FILLER         PIC  X(04).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(02).
           02  FILLER         PIC  N(03)   VALUE   "構成比".
           02  FILLER         PIC  X(01).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(04).
           02  FILLER         PIC  N(01)   VALUE   "金".
           02  FILLER         PIC  X(05).
           02  FILLER         PIC  N(01)   VALUE   "額".
           02  FILLER         PIC  X(04).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(02).
           02  FILLER         PIC  N(03)   VALUE   "構成比".
           02  FILLER         PIC  X(01).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(04).
           02  FILLER         PIC  N(01)   VALUE   "金".
           02  FILLER         PIC  X(05).
           02  FILLER         PIC  N(01)   VALUE   "額".
           02  FILLER         PIC  X(04).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(02).
           02  FILLER         PIC  N(03)   VALUE   "比　率".
           02  FILLER         PIC  X(01).
           02  FILLER         PIC  X(02)   VALUE   X"1AC1".
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(01).
           02  FILLER         PIC  N(01)   VALUE   "　".
      *
       01  MID-07.
           02  K-05           PIC  X(02).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(07).
           02  P-MID          PIC  N(07).
           02  FILLER         PIC  X(13).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(17).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(09).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(17).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(09).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(17).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(09).
           02  K-06           PIC  X(02).
           02  FILLER         PIC  X(02)   VALUE   X"1AC2".
           02  FILLER         PIC  X(01).
           02  FILLER         PIC  N(01)   VALUE   "　".
      *
       01  DTL-01.
           02  K-01           PIC  X(02).
           02  FILLER         PIC  X(01).
           02  P-KMK1.
               03  P-KMKNM1   PIC  N(10).
               03  FILLER     PIC  X(12).
           02  P-KMK1R        REDEFINES    P-KMK1.
               03  FILLER     PIC  X(02).
               03  P-KMKNM2   PIC  N(10).
               03  FILLER     PIC  X(10).
           02  P-KMK1R        REDEFINES    P-KMK1.
               03  FILLER     PIC  X(06).
               03  P-KMKNM3   PIC  N(10).
               03  FILLER     PIC  X(06).
           02  P-KMK1R        REDEFINES    P-KMK1.
               03  FILLER     PIC  X(10).
               03  P-KMKNM4   PIC  N(10).
               03  FILLER     PIC  X(02).
           02  P-KMK1R        REDEFINES    P-KMK1.
               03  FILLER     PIC  X(12).
               03  P-KMKNM5   PIC  N(10).
           02  FILLER         PIC  X(01).
           02  K-02           PIC  X(02).
           02  P-TBL.
               03  P-TBL1     OCCURS   3.
                   04  P-KIN1   PIC  ----,---,---,--9.
                   04  FILLER   PIC  X(01).
                   04  K-03     PIC  X(02).
                   04  FILLER   PIC  X(01).
                   04  P-KOHI1  PIC  ----.99.
                   04  FILLER   PIC  X(01).
                   04  K-04     PIC  X(02).
           02  K-07           PIC  N(01).
       COPY  LWMSG_PR.
      *
           COPY       BUMONF.
           COPY       BUPL.
           COPY       FCTL.
      *
       77  PR-REC             PIC      X(136).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DISP-BUZZER.
           02  DISP-BUZ-J-03  PIC X(05) VALUE X"1B4A03".
       01  DSP-AREA.
           03  DSP-CLR        PIC  X(11) VALUE "CLEAR  DATA".
           03  DSP-010.
               05  DSP-011    PIC  X(14)
                              VALUE  "ｺﾝﾄﾛｰﾙ DATE ﾅｼ".
               05  DSP-012    PIC  X(13)
                              VALUE  "ｺﾝﾄﾛｰﾙ SEL ﾅｼ".
               05  DSP-013    PIC  X(33)
                      VALUE  "ﾌﾞﾓﾝﾍﾞﾂ ｿﾝｴｷ ﾌｧｲﾙ START ｴﾗｰ KEY= ".
               05  DSP-014    PIC  9(04).
           03  DSP-020        PIC X(2)  VALUE "  ".
       01  ACP-AREA.
           03  ACP-010        PIC X(1).
       COPY  LSMSG_PR.
      ******************************************************
       PROCEDURE                      DIVISION.
      ******************************************************
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DISP-BUZZER
       CALL "SD_Init" USING 
            "DISP-BUZZER" " " "24" "0" "5" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-BUZ-J-03" "X" "24" "80" "5" " " "DISP-BUZZER"
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "77" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-CLR" "X" "24" "0" "11" " " "DSP-AREA"  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-010" " " "24" "0" "64" "DSP-CLR" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-011" "X" "24" "1" "14" " " "DSP-010"  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-012" "X" "24" "1" "13" "DSP-011" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-013" "X" "24" "1" "33" "DSP-012" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-014" "9" "24" "35" "4" "DSP-013" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-014" BY REFERENCE FCTL-FROM "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-020" "X" "23" "1" "2" "DSP-010" " "  RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-010" "X" "23" "1" "1" " " "ACP-AREA"  RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-010" BY REFERENCE CHK "1" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MAIN-000.
           CALL "DB_F_Open" USING
            "INPUT" BU-F_PNAME1 "SHARED" BY REFERENCE BU-F_IDLST "1"
            "BU-KEY" BY REFERENCE BU-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BNM_PNAME1 "SHARED" BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE    90       TO      L-CNT.
           MOVE    0        TO      SW     P-CNT.
      *************<  ｺﾝﾄﾛｰﾙ F  ﾘｰﾄﾞ  >***************
           MOVE    "DATE  " TO      FCTL-KEY1.
      *           READ    FCTL-F   WITH  UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "DSP-011" DSP-011 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J-03" DISP-BUZ-J-03 "p" RETURNING RESU
               PERFORM  CHK-RTN  THRU  CHK-RTNEX
               GO  TO   END-99
           END-IF
      *
           MOVE    FCTL-REC1  TO    Z-R.
           MOVE    FCTL-KONYMD TO   ZYMD.
           PERFORM Z-RTN      THRU  Z-EXT.
           MOVE    ZI         TO    I.
           IF  I  >  15
               CALL "SD_Output" USING
                "DSP-011" DSP-011 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J-03" DISP-BUZ-J-03 "p" RETURNING RESU
               PERFORM  CHK-RTN THRU CHK-RTNEX
               GO  TO  END-99
           END-IF
           MOVE    Z-TOUTYY2(I)     TO    WK-KONYY.
           MOVE    Z-TOUTMM(I)      TO    WK-KONMM.
      *
           MOVE    "SEL   "   TO    FCTL-KEY3.
      *           READ    FCTL-F   WITH  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "DSP-012" DSP-012 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J-03" DISP-BUZ-J-03 "p" RETURNING RESU
               PERFORM  CHK-RTN  THRU   CHK-RTNEX
               GO  TO   END-99
           END-IF
      *
           ACCEPT  HIZUKE   FROM    DATE.
      *
           MOVE    WK-YY    TO      P-YY.
      *
           MOVE    WK-MM    TO      YMD-1.
           MOVE    YMD-1    TO      P-MM.
      *
           MOVE    WK-DD    TO      YMD-1.
           MOVE    YMD-1    TO      P-DD.
      *
           MOVE    WK-KONYY   TO    P-KONYY.
      *
           MOVE    WK-KONMM   TO    YMD-1.
           MOVE    YMD-1    TO      P-KONMM.
      *
      *===============<  ﾌﾞﾓﾝﾍﾞﾂ ｿﾝｴｷ F  ﾘｰﾄﾞ  >=================*
       MAIN-010.
           MOVE    SPACE      TO    BU-KEY.
           MOVE    FCTL-FROM  TO    BU-KEY.
      *           START   BU-F    KEY    NOT    <    BU-KEY    INVALID
      *///////////////
           CALL "DB_Start" USING
            BU-F_PNAME1 "BU-KEY" " NOT < " BU-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "DSP-013" DSP-013 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DSP-014" DSP-014 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J-03" DISP-BUZ-J-03 "p" RETURNING RESU
               PERFORM  CHK-RTN   THRU   CHK-RTNEX
               GO   TO  END-99
           END-IF.
      *
       MAIN-020.
      *           READ    BU-F    NEXT    AT    END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" BU-F_PNAME1 BY REFERENCE BU-REC " "
            RETURNING RET.
           IF  RET = 1
               PERFORM    LINE-SUB   THRU   LINE-EXT
               GO    TO    END-99
           END-IF
           IF  SW    =    1
               GO    TO    MAIN-030
           END-IF
           IF  SW    =    2
               GO    TO    MAIN-040
           END-IF
           MOVE    1          TO    SW.
      *
       MAIN-025.
           MOVE    BU-BUMN    TO    WK-BUMN.
      *
       MAIN-030.
           IF  BU-BUMN    NOT    =    WK-BUMN
               MOVE   WK-BUMN   TO   FCTL-FROM
               MOVE    2    TO    SW
               GO    TO    MAIN-010
           END-IF
           IF  BU-URKBN    NOT    =    "U"
               GO    TO    MAIN-020
           END-IF
           COMPUTE    WK-TOUKI    =    WK-TOUKI    +    BU-TOUKI.
           COMPUTE    WK-ZENKI    =    WK-ZENKI    +    BU-ZENKI.
           GO    TO    MAIN-020.
      *
       MAIN-040.
           IF  BU-BUMN   =    WK-BUMN
               GO   TO    MAIN-055
           END-IF
           PERFORM    LINE-SUB    THRU    LINE-EXT.
           IF  BU-BUMN    >    FCTL-TO
               GO   TO    END-99
           ELSE
               MOVE    90    TO    L-CNT
               MOVE    1     TO    SW
               MOVE    ZERO  TO    SYUKEI
               GO      TO    MAIN-025
           END-IF.
      *
       MAIN-055.
           IF  L-CNT    >    49
               PERFORM    LINE-SUB    THRU    LINE-EXT
               PERFORM    PAGE-SUB    THRU    PAGE-EXT
           END-IF
      *
      *===============<  ﾒｲｻｲ  ﾌﾟﾘﾝﾄ  ﾙｰﾁﾝ  >====================*
           MOVE    SPACE     TO    DTL-01.
           MOVE    X"1AC2"  TO   K-01  K-02  K-03(1)  K-03(2)  K-03(3).
           MOVE    X"1AC2"  TO   K-04(1)  K-04(2)  K-04(3).
           MOVE    "　"    TO   K-07.
           IF  BU-KAIP  =  0
               MOVE  1        TO  BU-KAIP
           END-IF
           IF  BU-KAIP  NOT =  1
               MOVE  DTL-01   TO  PR-REC
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PR-REC RETURNING RESP
               SUBTRACT  1    FROM  BU-KAIP
               ADD  1         TO  L-CNT
               GO  TO  MAIN-055
           END-IF
      *
      *========<  ｺｳｾｲﾋ AND ﾋﾘﾂ  ｹｲｻﾝ  >==============*
           IF  BU-PRKBN    =    1
               GO    TO    MAIN-070
           END-IF
           IF  WK-TOUKI    =    0
               MOVE    0    TO    WK-KOSEI
           ELSE
               COMPUTE   WK-KOSEI   =   BU-TOUKI   /   WK-TOUKI
               COMPUTE   P-KOHI1(1) =   WK-KOSEI   *   100  +  0.005
           END-IF
           IF  WK-ZENKI    =    0
               MOVE    0    TO    WK-KOSEI
           ELSE
               COMPUTE   WK-KOSEI   =   BU-ZENKI   /   WK-ZENKI
               COMPUTE   P-KOHI1(2) =   WK-KOSEI   *   100  +  0.005
           END-IF
           IF ( BU-TOUKI   <   0 )   OR   ( BU-ZENKI   <   0 )
               GO    TO    MAIN-060
           END-IF
           IF  BU-ZENKI    =    0
               MOVE    0    TO    WK-KOSEI
           ELSE
               COMPUTE   WK-KOSEI   =   BU-TOUKI   /   BU-ZENKI
               COMPUTE   P-KOHI1(3) =   WK-KOSEI   *   100  +  0.005
           END-IF.
      *
       MAIN-060.
           MOVE    BU-TOUKI    TO    P-KIN1(1).
           MOVE    BU-ZENKI    TO    P-KIN1(2).
           COMPUTE    P-KIN1(3)    =    BU-TOUKI    -    BU-ZENKI.
      *
       MAIN-070.
           IF  BU-GOKBN    =    1
               MOVE   BU-KMKNM   TO   P-KMKNM1
           END-IF
           IF  BU-GOKBN    =    2
               MOVE   BU-KMKNM   TO   P-KMKNM2
           END-IF
           IF  BU-GOKBN    =    3
               MOVE   BU-KMKNM   TO   P-KMKNM3
           END-IF
           IF  BU-GOKBN    =    4
               MOVE   BU-KMKNM   TO   P-KMKNM4
           END-IF
           IF  BU-GOKBN    =    5
               MOVE   BU-KMKNM   TO   P-KMKNM5
           END-IF
           IF  BU-GOKBN    =    0
               MOVE   SPACE      TO   P-KMK1
           END-IF
      *
           MOVE    DTL-01      TO    PR-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PR-REC RETURNING RESP.
      *
           ADD     1           TO    L-CNT.
           GO    TO    MAIN-020.
      *
       END-99.
           IF  SW    =    0    OR   2
               PERFORM  CLSE-ENT    THRU  CLSE-EXT
               CALL "DB_Close"
               STOP     RUN
           ELSE
               MOVE     WK-BUMN     TO    FCTL-FROM
               MOVE     2           TO    SW
               GO    TO    MAIN-010
           END-IF.
      ***********************************************************
      *     <<   ﾐﾀﾞｼ  ｶｲﾍﾟｰｼﾞ   ｻﾌﾞﾙｰﾁﾝ  >>                    *
      ***********************************************************
       PAGE-SUB.
           IF  P-CNT    NOT    =    0
               MOVE    SPACE    TO    PR-REC
               CALL "PR_Write" USING PR-REC RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
      *
           ADD    1    TO    P-CNT.
           MOVE    P-CNT       TO    PAGE-1.
           MOVE    PAGE-1      TO    P-PAGE.
      *
           MOVE    MID-01      TO    PR-REC.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PR-REC RETURNING RESP.
      *
           MOVE    MID-02      TO    PR-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PR-REC RETURNING RESP.
      *
           MOVE    MID-03      TO    PR-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PR-REC RETURNING RESP.
      *
      *========<  ﾌﾞﾓﾝﾒｲ  ﾏｽﾀ  ﾘｰﾄﾞ  >================*
           MOVE    WK-BUMN    TO    BNM-KEY.
      *           READ    BNM     INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" BNM_PNAME1 BY REFERENCE BNM-REC " " RETURNING RET.
           IF  RET = 1
               MOVE    SPACE    TO    BNMNM
           END-IF
      *
           MOVE    BNMNM       TO    P-BNMNM.
           MOVE    BNM-KEY     TO    P-BNMCD.
           MOVE    MID-04      TO    PR-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PR-REC RETURNING RESP.
      *
           MOVE    MID-05      TO    PR-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PR-REC RETURNING RESP.
      *
           MOVE    "項　　目　　名"                  TO   P-MID.
           MOVE    X"0000"    TO    K-05    K-06.
           MOVE    MID-07      TO    PR-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PR-REC RETURNING RESP.
      *
           MOVE    MID-06      TO    PR-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PR-REC RETURNING RESP.
      *
           MOVE    SPACE       TO    P-MID.
           MOVE    MID-07      TO    PR-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PR-REC RETURNING RESP.
      *
           MOVE    0           TO    L-CNT.
       PAGE-EXT.
           EXIT.
      ***********************************************************
      *     <<  ﾗｲﾝ ﾌﾟﾘﾝﾄ   ｻﾌﾞﾙｰﾁﾝ  >>                         *
      ***********************************************************
       LINE-SUB.
           IF  L-CNT    =    90
               GO    TO    LINE-EXT
           END-IF
           MOVE    SPACE       TO    P-MID.
           MOVE    X"1AC0"    TO    K-05.
           MOVE    X"1AC1"    TO    K-06.
           MOVE    MID-07      TO    PR-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PR-REC RETURNING RESP.
      *
       LINE-EXT.
           EXIT.
       CHK-RTN.
           CALL "SD_Accept" USING BY REFERENCE ACP-010 "ACP-010" "X" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT         NOT  = "01"
               GO  TO  CHK-RTN
           END-IF
           CALL "SD_Output" USING "DSP-020" DSP-020 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
       CHK-RTNEX.
           EXIT.
      *
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE BU-F_IDLST BU-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       CLSE-EXT.
           EXIT.
      **
       COPY  LPMSG_PR.

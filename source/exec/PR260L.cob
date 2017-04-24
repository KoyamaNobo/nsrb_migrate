       IDENTIFICATION  DIVISION.
      ***  日計表　
       PROGRAM-ID.     PR260L.
      ***  BASE  ZA0111.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM3100.
       OBJECT-COMPUTER.    NEAC-SYSTEM3100.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       01  H1.
           02  FILLER          PIC X(05)   VALUE   X"1A24212474".
           02  FILLER          PIC X(02)   VALUE   SPACE.
           02  M1-YY           PIC N(02).
           02  FILLER          PIC N(01)   VALUE   "年".
           02  M1-MM           PIC N(02).
           02  FILLER          PIC N(01)   VALUE   "月".
           02  M1-DD           PIC N(02).
           02  FILLER          PIC N(03)   VALUE   "日作成".
           02  FILLER          PIC X(23)   VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   X"1AC0".
           02  FILLER          PIC N(09)   VALUE
                               "　日　　計　　表　".
           02  FILLER          PIC X(2)    VALUE   X"1AC1".
           02  FILLER          PIC X(22)   VALUE   SPACE.
           02  M-PCNT          PIC N(05).
           02  FILLER          PIC N(01)   VALUE   "頁".
       01  H2.
           02  FILLER          PIC X(43)   VALUE   SPACE.
           02  FILLER          PIC N(01)   VALUE   "（".
           02  M2-YY           PIC N(02).
           02  FILLER          PIC N(1)    VALUE   "年".
           02  M2-MM           PIC N(02).
           02  FILLER          PIC N(1)    VALUE   "月".
           02  M2-DD           PIC N(02).
           02  FILLER          PIC N(4)    VALUE   "日現在）".
       01  H3.
           02  FILLER          PIC X(2)    VALUE   X"1AC0".
           02  FILLER          PIC X(67)   VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   X"1AC1".
       01  H4.
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC X(1)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "貸".
           02  FILLER          PIC X(1)    VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC X(29)   VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   X"1AC0".
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC X(8)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "当".
           02  FILLER          PIC X(6)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "日".
           02  FILLER          PIC X(6)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "欄".
           02  FILLER          PIC X(8)    VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   X"1AC1".
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC X(3)    VALUE   X"602120".
       01  H5.
           02  FILLER          PIC X(2)    VALUE   X"1AC0".
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC X(1)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "借".
           02  FILLER          PIC X(1)    VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC X(04)   VALUE   SPACE.
           02  FILLER          PIC N(07)   VALUE   "科　　目　　名".
           02  FILLER          PIC X(11)   VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC X(1)    VALUE   SPACE.
           02  FILLER          PIC N(08)   VALUE   "発　生　借　方　".
           02  FILLER          PIC X(02)   VALUE   X"1AC2".
           02  FILLER          PIC X(1)    VALUE   SPACE.
           02  FILLER          PIC N(08)   VALUE   "発　生　貸　方　".
           02  FILLER          PIC X(02)   VALUE   X"1AC2".
           02  FILLER          PIC X(2)    VALUE   X"1AC1".
           02  FILLER          PIC X(3)    VALUE   X"602120".
       01  H6.
           02  FILLER          PIC X(2)    VALUE   X"1AC0".
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC X(4)    VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC X(29)   VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC X(17)   VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC X(17)   VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC X(2)    VALUE   X"1AC1".
           02  FILLER          PIC X(3)    VALUE   X"602120".
       01  M1.
           02  FILLER          PIC X(2)    VALUE   SPACE.
           02  FILLER          PIC X(1)    VALUE   SPACE.
           02  M1-DRCR         PIC N(1).
           02  FILLER          PIC X(1)    VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   SPACE.
           02  M1-MSG.
               03  FILLER      PIC X(1)    VALUE   SPACE.
               03  M1-ACCTNM   PIC X(20).
               03  FILLER      PIC X(1)    VALUE   SPACE.
               03  M1-FIL1     PIC X(1).
               03  M1-ACCTCD   PIC X(4).
               03  M1-FIL2     PIC X(1).
               03  FILLER      PIC X(1)    VALUE   SPACE.
           02  FILLER          PIC X(02)   VALUE   SPACE.
           02  FILLER          PIC X(1)    VALUE   SPACE.
           02  M1-HASDR        PIC ---,---,---,---.
           02  FILLER          PIC X(1)    VALUE   SPACE.
           02  FILLER          PIC X(02)   VALUE   SPACE.
           02  FILLER          PIC X(1)    VALUE   SPACE.
           02  M1-HASCR        PIC ---,---,---,---.
           02  FILLER          PIC X(1)    VALUE   SPACE.
           02  FILLER          PIC X(02)   VALUE   SPACE.
           02  FILLER          PIC X(03)   VALUE   SPACE.
       01  W1.
           02  KAIGYOU-SW      PIC 9(1).
           02  W-Z9            PIC Z9.
           02  W-ZZZZ9         PIC ZZZZ9.
           02  ERR-STAT        PIC X(02).
           02  LINCNT          PIC 9(2)    VALUE 70.
           02  PCNT            PIC 9(05).
           02  INIT-SW         PIC X(3)    VALUE "OFF".
           02  PRINT-SW        PIC 9(1).
           02  I               PIC 9(1).
           02  SEL-IN          PIC 9(1).
           02  KB-KAKU         PIC X(1).
           02  W1-TOTAL.
             03  W1-GOKE   OCCURS  6.
               04  W1-KURDR    PIC S9(11).
               04  W1-KURCR    PIC S9(11).
               04  W1-HASDR    PIC S9(11).
               04  W1-HASCR    PIC S9(11).
               04  W1-ZANDR    PIC S9(11).
               04  W1-ZANCR    PIC S9(11).
               04  W1-CNTER    PIC 9(4).
           02  W1-OLDKEY.
             03  OLD-CODE1     PIC 9(1).
             03  OLD-CODE2     PIC 9(1).
             03  OLD-CODE3     PIC 9(1).
             03  OLD-CODE4     PIC 9(1).
       01  W2.
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC X(4)    VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC X(29)   VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC X(17)   VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC X(17)   VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC X(3)    VALUE   X"602120".
       01  MSG.
           02  KARIMSG         PIC X(20)   VALUE
                               "（借方合計）　　　　".
           02  KASIMSG         PIC X(20)   VALUE
                               "（貸方合計）　　　　".
       01  SYS-YMD.
           02  SYS-YY          PIC 9(02).
           02  SYS-MM          PIC 9(02).
           02  SYS-DD          PIC 9(02).
       01  OHTRDATE.
           02  F               PIC 9(02) VALUE ZERO.
           02  OHTRDATE-Y      PIC 9(02) VALUE ZERO.
           02  OHTRDATE-M      PIC 9(02) VALUE ZERO.
           02  OHTRDATE-D      PIC 9(02) VALUE ZERO.
       01  SW-AREA.
           02  END-SW          PIC 9(01).
       01  CRT-WK.
           02  W-02F.
             03  W-02FY        PIC  9(04).
             03  W-02FYL  REDEFINES W-02FY.
               04  W-02FY1     PIC  9(02).
               04  W-02FY2     PIC  9(02).
             03  W-02FM        PIC  9(02).
             03  W-02FD        PIC  9(02).
           02  W-02FR REDEFINES W-02F     PIC  9(08).
           02  W-02T.
             03  W-02TY        PIC  9(04).
             03  W-02TYL  REDEFINES W-02TY.
               04  W-02TY1     PIC  9(02).
               04  W-02TY2     PIC  9(02).
             03  W-02TM        PIC  9(02).
             03  W-02TD        PIC  9(02).
           02  W-02TR REDEFINES W-02T     PIC  9(08).
      *
       COPY LWMSG_PR.
      *
       COPY LIBFDD.
      *FD  SDH
       01  SDH_PR260L.
           02  SDH_PNAME1           PIC  X(009)  VALUE "SIWAKE-H1".
           02  F                    PIC  X(001).
           02  SDH_LNAME            PIC  X(010)  VALUE "SDH_PR260L".
           02  F                    PIC  X(001).
           02  SDH_KEY1             PIC  X(100)  VALUE SPACE.
           02  SDH_KEY2             PIC  X(100)  VALUE SPACE.
           02  SDH_KEY3             PIC  X(100)  VALUE SPACE.
           02  SDH_KEY4             PIC  X(100)  VALUE SPACE.
           02  SDH_SORT             PIC  X(100)  VALUE SPACE.
           02  SDH_IDLST            PIC  X(100)  VALUE SPACE.
           02  SDH_RES              USAGE  POINTER.
       COPY    SIWAKH.
       77  F                        PIC X(1).
      ***  科目マスタ
       COPY    ACCUNT.
      ***  漢字科目マスタ
       COPY    KANGEL.
      *
      *FD  F5  LABEL   RECORD  OMITTED
       77  F5-REC                   PIC X(250).
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
           02  DISP-BUZ-Q  PIC X(05)  VALUE X"1B4202".
       01  DSP-AREA1.
           02  DSP-CLR.
               04  FILLER  PIC X(12) VALUE "CLEAR SCREEN".
           02  DSP-REVERSE PIC X(08) VALUE "        ".
           02  DSP-000     PIC X(20) VALUE  "ｺﾝﾄﾛｰﾙ ﾌｧｨﾙ  INVALID".
       01  DSP-AREA2.
           02  FILLER  PIC N(03) VALUE "日計表".
           02  FILLER  PIC N(06) VALUE "（１＝全件，".
           02  FILLER  PIC N(08) VALUE "２＝当日発生分）".
           02  FILLER  PIC X(07) VALUE  "--->( )".
           02  FILLER.
               04  FILLER  PIC N(02) VALUE "確認".
               04  FILLER  PIC X(13) VALUE "OK=1,NO=9 ( )".
           02  FILLER.
               03  FILLER  PIC N(04) VALUE "ＦＲＯＭ".
               03  FILLER  PIC N(02) VALUE "ＴＯ".
           02  FILLER.
               03  FILLER  PIC N(05) VALUE "日　　　付".
               03  FILLER  PIC N(01) VALUE "～".
               03  FILLER  PIC X(04) VALUE "/  /".
               03  FILLER  PIC X(04) VALUE "/  /".
       01  ACP-AREA.
           02  ACP-010    PIC 9(01).
           02  ACP-020    PIC X(01).
           02  ACP-W02F.
               03  FILLER  PIC 9(02).
               03  FILLER  PIC 9(02).
               03  FILLER  PIC 9(02).
           02  ACP-W02T.
               03  FILLER  PIC 9(02).
               03  FILLER  PIC 9(02).
               03  FILLER  PIC 9(02).
      *
           COPY LSMSG_PR.
           COPY LIBSCR.
      *
       PROCEDURE       DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "SPOUT1-999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DISP-BUZZER
       CALL "SD_Init" USING 
            "DISP-BUZZER" " " "24" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-BUZ-Q" "X" "24" "80" "5" " " "DISP-BUZZER"
            RETURNING RESU.
      *DSP-AREA1
       CALL "SD_Init" USING 
            "DSP-AREA1" " " "0" "0" "40" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-CLR" " " "1" "0" "12" " " "DSP-AREA1" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLEAR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-REVERSE" "RX" "1" "37" "8" "DSP-CLR" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-000" "X" "1" "52" "20" "DSP-REVERSE" " "
            RETURNING RESU.
      *DSP-AREA2
       CALL "SD_Init" USING 
            "DSP-AREA2" " " "0" "0" "90" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA2" "N" "1" "38" "6" " " "DSP-AREA2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA2" "N" "4" "20" "12" "01DSP-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA2" "N" "4" "32" "16" "02DSP-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-AREA2" "X" "4" "48" "7" "03DSP-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-AREA2" " " "24" "0" "17" "04DSP-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0105DSP-AREA2" "N" "24" "61" "4" " " "05DSP-AREA2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0205DSP-AREA2" "X" "24" "66" "13" "0105DSP-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-AREA2" " " "8" "0" "12" "05DSP-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0106DSP-AREA2" "N" "8" "27" "8" " " "06DSP-AREA2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0206DSP-AREA2" "N" "8" "47" "4" "0106DSP-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-AREA2" " " "10" "0" "20" "06DSP-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0107DSP-AREA2" "N" "10" "11" "10" " " "07DSP-AREA2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0207DSP-AREA2" "N" "10" "39" "2" "0107DSP-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0307DSP-AREA2" "X" "10" "29" "4" "0207DSP-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0407DSP-AREA2" "X" "10" "47" "4" "0307DSP-AREA2" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "14" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-010" "9" "4" "53" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-010" BY REFERENCE SEL-IN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-020" "X" "24" "77" "1" "ACP-010" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-020" BY REFERENCE KB-KAKU "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W02F" " " "10" "0" "6" "ACP-020" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-W02F" "9" "10" "27" "2" " " "ACP-W02F"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "01ACP-W02F" BY REFERENCE W-02FY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACP-W02F" "9" "10" "30" "2" "01ACP-W02F" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "02ACP-W02F" BY REFERENCE W-02FM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03ACP-W02F" "9" "10" "33" "2" "02ACP-W02F" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "03ACP-W02F" BY REFERENCE W-02FD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W02T" " " "10" "0" "6" "ACP-W02F" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-W02T" "9" "10" "45" "2" " " "ACP-W02T"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "01ACP-W02T" BY REFERENCE W-02TY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACP-W02T" "9" "10" "48" "2" "01ACP-W02T" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "02ACP-W02T" BY REFERENCE W-02TM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03ACP-W02T" "9" "10" "51" "2" "02ACP-W02T" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "03ACP-W02T" BY REFERENCE W-02TD "2" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       ST.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-REVERSE" DSP-REVERSE "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA2" DSP-AREA2 "p" RETURNING RESU.
           ACCEPT SYS-YMD FROM DATE.
           MOVE    SYS-YY        TO   M1-YY.
           MOVE    SYS-MM        TO   W-Z9.
           MOVE    W-Z9          TO   M1-MM.
           MOVE    SYS-DD        TO   W-Z9.
           MOVE    W-Z9          TO   M1-DD.
           COPY LIBCPR.
      *
       ST-05.
           CALL "SD_Accept" USING BY REFERENCE ACP-010 "ACP-010" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT        =      "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  ESTAT        NOT =  "01" AND "06"
               GO TO ST-05
           END-IF
           IF  SEL-IN NOT = 1 AND 2
               GO TO ST-05
           END-IF.
       ST-05F.
           CALL "SD_Accept" USING BY REFERENCE ACP-W02F "ACP-W02F"
            " " "6" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT     = "09"
               GO TO ST-05
           END-IF
           IF  ESTAT NOT = "01" AND "06"
               GO TO ST-05F
           END-IF.
       ST-05T.
           CALL "SD_Accept" USING BY REFERENCE ACP-W02T "ACP-W02T"
            " " "6" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT     = "09"
               GO TO ST-05F
           END-IF
           IF  ESTAT NOT = "01" AND "06"
               GO TO ST-05T
           END-IF
      *
           MOVE ZERO TO W-02FY1 W-02TY1.
           IF  W-02FY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-02FY
           END-IF
           IF  W-02FY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-02FY
           END-IF
           IF  W-02TY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-02TY
           END-IF
           IF  W-02TY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-02TY
           END-IF
      *
           IF  W-02F > W-02T
               GO TO ST-05T
           END-IF.
       ST-06.
           CALL "SD_Accept" USING BY REFERENCE ACP-020 "ACP-020" "X" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT        =     "09"
               GO TO ST-05T
           END-IF
           IF  ESTAT        NOT = "01" AND "06"
               GO TO ST-06
           END-IF
           IF  KB-KAKU      =     "9"
               GO TO ST-05
           END-IF
           IF  KB-KAKU      NOT = "1"
               GO TO ST-06
           END-IF.
       ST-07.
           MOVE       ZERO         TO     W1-TOTAL.
           CALL "DB_F_Open" USING
            "I-O" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "DB_F_Open" USING
            "INPUT" SDH_PNAME1 "SHARED" BY REFERENCE SDH_IDLST "1"
            "SH-KEY1" BY REFERENCE SH-KEY1.
           CALL "PR_Open" RETURNING RESP.
      *
           MOVE W-02F     TO OHTRDATE.
           MOVE 0         TO END-SW.
       ST-09S.
           PERFORM AMC-RTN THRU AMC-EX.
           PERFORM SDHG-RTN THRU SDHG-EX.
           MOVE LOW-VALUE     TO AM-KEY.
      *           START AM KEY NOT LESS AM-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            AM_PNAME1 "AM-KEY" " NOT LESS " AM-KEY RETURNING RET.
           IF  RET = 1
               GO TO CLSE-ENT
           END-IF
      *
           MOVE OHTRDATE-Y     TO M2-YY.
           MOVE OHTRDATE-M     TO W-Z9.
           MOVE W-Z9           TO M2-MM.
           MOVE OHTRDATE-D     TO W-Z9.
           MOVE W-Z9           TO M2-DD.
       ST-10.
      ***  科目マスタ　ＲＥＡＤ　
      *           READ       AM   NEXT    AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" AM_PNAME1 BY REFERENCE AM-REC " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  CLSE-ENT
           END-IF
      *
           IF  SEL-IN       =      1
               GO TO ST-15
           END-IF
           IF  DDR OF AM-REC     NOT = 0
               GO TO ST-15
           END-IF
           IF  DCR OF AM-REC     =     0
               GO TO ST-10
           END-IF.
       ST-15.
           IF  INIT-SW      =      "OFF"
               MOVE     "ON "        TO     INIT-SW
               GO TO    ST-20
           END-IF
           IF  ACCTCD1      NOT =  OLD-CODE1
               MOVE     4            TO     PRINT-SW
               GO TO    ST-30
           END-IF
           IF  ACCTCD2      NOT =  OLD-CODE2
               MOVE     3            TO     PRINT-SW
               GO TO    ST-30
           END-IF
           IF  ACCTCD3      NOT =  OLD-CODE3
               MOVE     2            TO     PRINT-SW
               GO TO    ST-30
           END-IF.
       ST-20.
           MOVE       1            TO     PRINT-SW.
       ST-30.
           PERFORM    PRINT-RTN    THRU   PRINT-EXT.
           PERFORM    ADD-RTN      THRU   ADD-EXT.
           MOVE       AM-KEY       TO     W1-OLDKEY.
           GO TO      ST-10.
       CLSE-ENT.
           IF  LINCNT   NOT =   70
               MOVE     5            TO     PRINT-SW
               PERFORM  PRINT-RTN    THRU   PRINT-EXT
           END-IF
           IF  END-SW NOT = 1
               MOVE HTRDATE      TO OHTRDATE
               MOVE 60           TO LINCNT
               MOVE ZERO         TO W1-TOTAL
               GO TO ST-09S
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_Close".
           STOP     RUN.
       CLSE-EXT.
      **********
       PRINT-RTN.
           IF  PRINT-SW     =      1
               GO TO PR-20
           END-IF
           IF  W1-CNTER (1) =      1
               GO TO PR-02
           END-IF
           IF  PRINT-SW     =      2
               GO TO PR-01
           END-IF
           IF  W1-CNTER (1) =      W1-CNTER (2)
               GO TO PR-02
           END-IF.
       PR-01.
       PR-02.
           IF  PRINT-SW     =      2
               GO TO PR-10
           END-IF
           IF  W1-CNTER (2) =      1
               GO TO PR-04
           END-IF
           IF  PRINT-SW     =      3
               GO TO PR-03
           END-IF
           IF  W1-CNTER (2) =      W1-CNTER (3)
               GO TO PR-04
           END-IF.
       PR-03.
       PR-04.
           IF  PRINT-SW     =      3
               GO TO PR-10
           END-IF
           IF  PRINT-SW     =      4
               GO TO PR-10
           END-IF
           MOVE    W2              TO      M1.
           MOVE    H6              TO      F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
      *
           ADD     1               TO     LINCNT.
           IF  LINCNT NOT < 55
               MOVE  1     TO  KAIGYOU-SW
           END-IF
      *
           MOVE    KARIMSG         TO     M1-ACCTNM.
           MOVE       W1-GOKE (4)  TO     W1-GOKE (6).
           MOVE       0            TO     W1-HASCR (6).
           MOVE       0            TO     W1-ZANCR (6).
           PERFORM    PRI-RTN      THRU   PRI-EXT.
           MOVE    H6              TO      F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
      *
           ADD     1               TO     LINCNT.
           IF  LINCNT NOT < 55
               MOVE  1     TO  KAIGYOU-SW
           END-IF
      *
           MOVE       W2           TO     M1.
           MOVE    KASIMSG         TO     M1-ACCTNM.
           MOVE       W1-GOKE (5)  TO     W1-GOKE  (6).
           MOVE       0            TO     W1-HASDR (6).
           MOVE       0            TO     W1-ZANDR (6).
           PERFORM    PRI-RTN      THRU   PRI-EXT.
           MOVE    H6              TO      F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           GO TO      PRINT-EXT.
       PR-10.
       PR-20.
           MOVE       W2           TO     M1.
           IF  DR-CR  OF AM-REC =      1
               MOVE   "借"       TO     M1-DRCR
               GO  TO  PR-21
           END-IF
           IF  DR-CR  OF AM-REC =      2
               MOVE   "貸"       TO     M1-DRCR
               GO  TO  PR-21
           END-IF
           MOVE    DR-CR  OF AM-REC    TO     M1-DRCR.
       PR-21.
           MOVE    AM-KEY          TO      K-ACCD.
           MOVE    0               TO      K-HOCD.
      *           READ    KNG             INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  KNGNM
           END-IF
           MOVE    KNGNM           TO      M1-ACCTNM.
           MOVE    "("             TO      M1-FIL1.
           MOVE    ")"             TO      M1-FIL2.
           MOVE       AM-KEY       TO     M1-ACCTCD.
           MOVE       ZERO         TO     W1-GOKE  (6).
           MOVE       DDR   OF AM-REC     TO  W1-HASDR (6).
           MOVE       DCR   OF AM-REC     TO  W1-HASCR (6).
           IF  DR-CR OF AM-REC  =      2
               GO TO PR-30
           END-IF
           ADD        W1-KURDR (6)        W1-HASDR (6)
                                   GIVING W1-ZANDR (6).
           SUBTRACT   W1-HASCR (6) FROM   W1-ZANDR (6).
           GO TO      PR-40.
       PR-30.
           ADD        W1-KURCR (6)        W1-HASCR (6)
                                   GIVING W1-ZANCR (6).
           SUBTRACT   W1-HASDR (6) FROM   W1-ZANCR (6).
       PR-40.
           PERFORM    PRI-RTN      THRU   PRI-EXT.
           SUBTRACT   1            FROM   LINCNT.
       PRINT-EXT.
           EXIT.
       PRI-RTN.
           IF  LINCNT      NOT <       55
               PERFORM  HEAD-RTN     THRU   HEAD-EXT
           END-IF
           MOVE       W1-HASDR (6)  TO    M1-HASDR.
           MOVE       W1-HASCR (6)  TO    M1-HASCR.
           MOVE       M1           TO     F5-REC.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           ADD        2            TO     LINCNT.
       PRI-EXT.
           EXIT.
       ADD-RTN.
           IF  PRINT-SW     =      1
               GO TO AR-10
           END-IF
           MOVE       ZERO         TO     W1-GOKE (1).
           IF  PRINT-SW     =      2
               GO TO AR-10
           END-IF
           MOVE       ZERO         TO     W1-GOKE (2).
           IF  PRINT-SW     =      3
               GO TO AR-10
           END-IF
           MOVE       ZERO         TO     W1-GOKE (3).
       AR-10.
           MOVE       1            TO     I.
       AR-20.
           ADD        W1-HASDR (6) TO     W1-HASDR (I).
           ADD        W1-HASCR (6) TO     W1-HASCR (I).
           ADD        W1-ZANDR (6) TO     W1-ZANDR (I).
           ADD        W1-ZANCR (6) TO     W1-ZANCR (I).
           ADD        1            TO     W1-CNTER (I).
           IF  I            <      5
               ADD      1            TO     I
               GO TO    AR-20
           END-IF.
       ADD-EXT.
           EXIT.
       HEAD-RTN.
           IF  LINCNT          =       70
               GO  TO  HEAD-02
           END-IF
           IF  LINCNT          =       60
               GO  TO  HEAD-01
           END-IF
           IF  KAIGYOU-SW      =       1
               MOVE  ZERO     TO  KAIGYOU-SW
               GO  TO  HEAD-01
           END-IF
           MOVE    H6              TO      F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
       HEAD-01.
           MOVE    SPACE           TO      F5-REC.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       HEAD-02.
           ADD     1               TO      PCNT.
           MOVE    PCNT            TO      W-ZZZZ9.
           MOVE    W-ZZZZ9         TO      M-PCNT.
           MOVE    H1              TO      F5-REC.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE    H2              TO      F5-REC.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE    H3              TO      F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE    H4              TO      F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE    H5              TO      F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE    W2              TO      F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       9            TO     LINCNT.
       HEAD-EXT.
           EXIT.
      ***********  < 91/04/01 >  ***************************************
       SDHG-RTN.
           MOVE ZERO         TO SH-KEY1.
           MOVE OHTRDATE     TO HTRDATE.
      *           START SDH KEY NOT LESS SH-KEY1 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDH_PNAME1 "SH-KEY1" "NOT LESS" SH-KEY1 RETURNING RET.
           IF  RET = 1
               GO TO SDHG-999
           END-IF
      *           READ SDH NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO SDHG-999
           END-IF
           IF  HTRDATE > W-02T
               GO TO SDHG-999
           END-IF
           GO TO SDHG-100.
       SDHG-000.
      *           READ SDH NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO SDHG-999
           END-IF
           IF  HTRDATE > W-02T
               GO TO SDHG-999
           END-IF
           IF  HTRDATE  NOT = OHTRDATE
               GO TO SDHG-EX
           END-IF.
       SDHG-100.
           MOVE HACCNTCD     TO AM-KEY.
      *           READ AM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AM_PNAME1 BY REFERENCE AM-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO SDHG-000
           END-IF
           IF  HDR-CR = 1
               ADD  HAMOUNT     TO DDR
           ELSE
               ADD  HAMOUNT     TO DCR
           END-IF
           PERFORM AMU-RTN THRU AMU-EX.
      *
           MOVE HTRDATE      TO OHTRDATE.
           GO TO SDHG-000.
       SDHG-999.
           MOVE 1     TO END-SW.
       SDHG-EX.
           EXIT.
      *****
       AMC-RTN.
           MOVE LOW-VALUE     TO AM-KEY.
      *           START AM KEY NOT LESS AM-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            AM_PNAME1 "AM-KEY" "NOT LESS" AM-KEY RETURNING RET.
           IF  RET = 1
               GO TO AMC-EX
           END-IF.
       AMC-000.
      *           READ AM NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" AM_PNAME1 BY REFERENCE AM-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO AMC-EX
           END-IF
           MOVE ZERO     TO DDR DCR.
           PERFORM AMU-RTN THRU AMU-EX.
           GO TO AMC-000.
       AMC-EX.
           EXIT.
      *****
       AMU-RTN.
           MOVE AM-KEY     TO ERR-K.
      *           REWRITE AM-REC INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            AM_PNAME1 AM_LNAME AM-REC RETURNING RET.
           IF  RET = 1
               MOVE "AM"     TO ERR-F
               MOVE "R"      TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
       AMU-EX.
           EXIT.
      **********
           COPY  LPMSG_PR.
      **********

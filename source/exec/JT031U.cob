       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         JT031U.
       AUTHOR.             I.NAKANISHI.
      ***********************************************************
      *    PROGRAM      :  送り状データファイル生成１           *
      *    DATA WRITTEN :  63/09/22                             *
      *    SCREEN USED  :  UNUSED                               *
      *    FORM   USED  :  UNUSED                               *
      *    PRINTER TYPE :  UNUSED                               *
      *    COMPILE TYPE :  COBOL                                *
      ***********************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT                PIC X(02)   VALUE  SPACE.
       77  INV-SW                  PIC 9(01)   VALUE  0.
       77  ERR-SW                  PIC 9(01)   VALUE  0.
       77  JS-SIGN                 PIC 9(01).
       77  JS-M                    PIC N(07).
       01  WK-AREA.
           03  OKC                 PIC 9(01).
           03  I                   PIC 9(02).
           03  P                   PIC 9(02).
           03  W-SYUKA             PIC 9(08).
           03  SYUKA     REDEFINES  W-SYUKA.
               05  S-YY            PIC 9(04).
               05  S-YYL   REDEFINES S-YY.
                   06  S-YY1       PIC 9(02).
                   06  S-YY2       PIC 9(02).
               05  S-MM            PIC 9(02).
               05  S-DD            PIC 9(02).
           03  SYUKAL    REDEFINES  W-SYUKA.
               05  F               PIC 9(02).
               05  SYUKAS          PIC 9(06).
           03  W-YMD               PIC 9(06).
           03  WK-KIRI             PIC 9(03).
           03  WK-SOKU.
               05  WK-SIZE    OCCURS   4.
                   07  WK-KBN    OCCURS  10   PIC 9(01).
           03  WK-HOSU             PIC 9(03).
           03  WK-SASOK            PIC S9(04).
           03  WK-ZAN              PIC S9(04).
           03  WK-SOIRI            PIC S9(04).
           03  WK-GSOK             PIC 9(05).
           03  WWK3-KEY.
               04  WWK3-01         PIC 9(06).
               04  WWK3-02         PIC 9(02).
               04  WWK3-03         PIC 9(01).
           03  WWK3-09             PIC 9(03).
           03  WK-SIZCD.
               07  WK-SIZCD1  OCCURS  10      PIC 9(03).
           03  OLD3-KEY.
               07  OLD3-01         PIC 9(06).
               07  OLD3-021        PIC 9(02).
           03  W-SET               PIC 9(03).
           03  W-KBN               PIC X(01).
       01  WK-SIZE1                PIC X(30)      VALUE
                   "063072081090201301401280290300".
       01  WK-SIZE2                PIC X(30)      VALUE
                   "125130135140150160170180190200".
       01  WK-SIZE3                PIC X(30)      VALUE
                   "210215220225230235240245250000".
       01  WK-SIZE4                PIC X(30)      VALUE
                   "240245250255260265270275000000".
       01  NF-WK1-ID               PIC X(17).
       01  NF-WK1-IDR  REDEFINES  NF-WK1-ID.
           02  W-ID1               PIC X(07).
       01  NF-WK3-ID               PIC X(17).
       01  NF-WK3-IDR  REDEFINES  NF-WK3-ID.
           02  W-ID3               PIC X(07).
      ***
       COPY  LWMSG.
      *
           COPY    LIBFDD.
           COPY    L-JSTR.
           COPY    LIHIM2.
           COPY    LNFWK1.
           COPY    LNFWK3.
           COPY    L-JCON.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-INI.
           02  FILLER  PIC  X(28) VALUE " ".
           02  FILLER  PIC  X(26) VALUE "送り状データファイル生成１".
       01  DSP-INI2.
           02  FILLER  PIC  X(08) VALUE "出荷日：".
           02  FILLER  PIC  X(02) VALUE "年".
           02  FILLER  PIC  X(02) VALUE "月".
           02  FILLER  PIC  X(02) VALUE "日".
       01  DSP-INI2A.
           02  FILLER  PIC N(7).
       01  DSP-INI3.
           02  FILLER  PIC  X(20) VALUE   "確認（OK=1,NO=9）-->".
           02  FILLER  PIC  X(04) VALUE "ﾘﾀｰﾝ".
       01  DSP-END.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  ACP-AREA.
           03  ACP-YY   PIC 9(02).
           03  ACP-MM   PIC 9(02).
           03  DSP-MM   PIC Z9 .
           03  ACP-DD   PIC 9(02).
           03  DSP-DD   PIC Z9 .
           03  ACP-OK   PIC 9(01).
       01  DSP-ERR.
           02  ERR-HIM.
               03  FILLER  PIC  X(24) VALUE  "＊　品名マスター無し　＊".
               03  FILLER  PIC  X(04) VALUE    "KEY=".
               03  FILLER  PIC  9(06).
           02  ERR-SET.
               03  FILLER  PIC  X(22) VALUE  "＊　セット数エラー　＊".
               03  FILLER  PIC  9(06).
               03  FILLER  PIC  X(01) VALUE    "+".
               03  FILLER  PIC  9(02).
           02  ERR-IRI.
               03  FILLER  PIC  X(22) VALUE    "＊　入　数　エラー　＊".
               03  FILLER  PIC  X(06) VALUE    "伝№＝".
               03  FILLER  PIC  9(06).
               03  FILLER  PIC  9(03).
               03  FILLER  PIC  9(03).
           02  ERR-KBN.
               03  FILLER  PIC  N(21)  VALUE
                   "他でデータ生成中です。処理を実行しますか？".
               03  FILLER  PIC  X(05)  VALUE  "(Y/N)".
           02  ACP-KBN  PIC X(01).
      *
           COPY  LSMSG.
           COPY  LIBSCR.
      *
       PROCEDURE           DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "DSP-CLR" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *DSP-INI
       CALL "SD_Init" USING 
            "DSP-INI" " " "1" "0" "54" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-INI" "RX" "1" "20" "28" " " "DSP-INI" RETURNING RESU.
       CALL "SD_Init" USING 
           "02DSP-INI" "X" "1" "21" "26" "01DSP-INI" " " RETURNING RESU.
      *DSP-INI2
       CALL "SD_Init" USING 
            "DSP-INI2" " " "8" "0" "14" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-INI2" "X" "8" "26" "8" " " "DSP-INI2" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-INI2" "X" "8" "36" "2" "01DSP-INI2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-INI2" "X" "8" "40" "2" "02DSP-INI2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-INI2" "X" "8" "44" "2" "03DSP-INI2" " "
            RETURNING RESU.
      *DSP-INI2A
       CALL "SD_Init" USING 
            "DSP-INI2A" " " "5" "0" "14" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-INI2A" "N" "5" "20" "14" " " "DSP-INI2A"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-INI2A" BY REFERENCE JS-M "14" "0" RETURNING RESU.
      *DSP-INI3
       CALL "SD_Init" USING 
            "DSP-INI3" " " "23" "0" "24" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-INI3" "X" "23" "41" "20" " " "DSP-INI3"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-INI3" "X" "23" "62" "4" "01DSP-INI3" " "
            RETURNING RESU.
      *DSP-END
       CALL "SD_Init" USING
           "DSP-END" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-END" "X" "1" "0" "12" " " "DSP-END" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "11" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-YY" "9" "8" "34" "2" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-YY" BY REFERENCE S-YY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-MM" "9" "8" "38" "2" "ACP-YY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-MM" BY REFERENCE S-MM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MM" "Z9" "8" "38" "2" "ACP-MM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MM" BY REFERENCE S-MM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-DD" "9" "8" "42" "2" "DSP-MM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-DD" BY REFERENCE S-DD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DD" "Z9" "8" "42" "2" "ACP-DD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-DD" BY REFERENCE S-DD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OK" "9" "23" "61" "1" "DSP-DD" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-OK" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "153" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-HIM" " " "24" "0" "34" " " "DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-HIM" "X" "24" "1" "24" " " "ERR-HIM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ERR-HIM" "X" "24" "25" "4" "01ERR-HIM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03ERR-HIM" "9" "24" "30" "6" "02ERR-HIM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03ERR-HIM" BY REFERENCE HI-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-SET" " " "24" "0" "31" "ERR-HIM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-SET" "X" "24" "1" "22" " " "ERR-SET" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ERR-SET" "9" "24" "25" "6" "01ERR-SET" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02ERR-SET" BY REFERENCE WK1-01 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03ERR-SET" "X" "24" "32" "1" "02ERR-SET" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04ERR-SET" "9" "24" "34" "2" "03ERR-SET" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04ERR-SET" BY REFERENCE WK1-021 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-IRI" " " "24" "0" "40" "ERR-SET" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-IRI" "X" "24" "1" "22" " " "ERR-IRI" RETURNING RESU.
       CALL "SD_Init" USING 
           "02ERR-IRI" "X" "24" "25" "6" "01ERR-IRI" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "03ERR-IRI" "9" "24" "31" "6" "02ERR-IRI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03ERR-IRI" BY REFERENCE WK1-08 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "04ERR-IRI" "9" "24" "39" "3" "03ERR-IRI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04ERR-IRI" BY REFERENCE WK1-09 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "05ERR-IRI" "9" "24" "43" "3" "04ERR-IRI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "05ERR-IRI" BY REFERENCE WWK3-09 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-KBN" " " "24" "0" "47" "ERR-IRI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-KBN" "N" "24" "1" "42" " " "ERR-KBN" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ERR-KBN" "X" "24" "44" "5" "01ERR-KBN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KBN" "X" "24" "50" "1" "ERR-KBN" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-KBN" BY REFERENCE W-KBN "1" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *************************
      *    メイン　ルーチン   *
      *************************
       MAIN.
           PERFORM  INI-RTN    THRU  INI-EX.
           PERFORM  UPD-RTN    THRU  UPD-EX.
           PERFORM  END-RTN    THRU  END-EX.
           CALL "SD_Output" USING "DSP-END" DSP-END "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *----------------------*
      *    ＩＮＩ－ＲＴＮ    *
      *----------------------*
       INI-RTN.
           ACCEPT   JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN  =  0
               MOVE  "【　教　育　】"  TO  JS-M
               MOVE  "NF-WK1 "  TO  W-ID1
               MOVE  W-ID1      TO  NF-WK1_PNAME1
               MOVE  "NF-WK3 "  TO  W-ID3
               MOVE  W-ID3      TO  NF-WK3_PNAME1
           END-IF
           IF  JS-SIGN  =  1
               MOVE  "【　一　般　】"  TO  JS-M
               MOVE  "NF-WK1I"  TO  W-ID1
               MOVE  W-ID1      TO  NF-WK1_PNAME1
               MOVE  "NF-WK3I"  TO  W-ID3
               MOVE  W-ID3      TO  NF-WK3_PNAME1
           END-IF
      *
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-INI" DSP-INI "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-INI2" DSP-INI2 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-INI2A" DSP-INI2A "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-INI3" DSP-INI3 "p" RETURNING RESU.
      *
           INITIALIZE       WK-AREA.
      **
       INI-000.
           ACCEPT  W-YMD     FROM  DATE.
       INI-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-YY "ACP-YY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT   =   "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       INI-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-MM "ACP-MM" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT   =   "09"
               GO    TO   INI-010
           END-IF
           CALL "SD_Output" USING "DSP-MM" DSP-MM "p" RETURNING RESU.
           IF  S-MM    = ZERO
               IF  S-YY2      =  ZERO
                   GO  TO  INI-030
               END-IF
           END-IF
           IF  S-MM    <     1    OR  >   12
               GO    TO   INI-020
           END-IF.
       INI-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-DD "ACP-DD" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT   =   "09"
               GO    TO   INI-020
           END-IF
           IF  ESTAT   NOT   =   "01" AND  "06"
               GO    TO   INI-030
           END-IF
           CALL "SD_Output" USING "DSP-DD" DSP-DD "p" RETURNING RESU.
           IF  S-DD    = ZERO
               IF  S-MM       =  ZERO
                   MOVE  W-YMD        TO  SYUKAS
                   CALL "SD_Output" USING
                    "ACP-YY" ACP-YY "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "DSP-MM" DSP-MM "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "DSP-DD" DSP-DD "p" RETURNING RESU
               END-IF
           END-IF
           IF  S-DD    <     1    OR  >   31
               GO    TO   INI-030
           END-IF.
       INI-OKC.
           CALL "SD_Accept" USING BY REFERENCE ACP-OK "ACP-OK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT   =   "09"
               GO    TO   INI-030
           END-IF
           IF  ESTAT   NOT   =   "01" AND  "06"
               GO    TO   INI-OKC
           END-IF
           IF  OKC     NOT   =     1  AND  9
               GO    TO   INI-OKC
           END-IF
           IF  OKC     =     9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP  RUN
           END-IF
      **
           COPY  LIBCPR.
           MOVE  ZERO       TO  S-YY1.
           IF  S-YY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO S-YY
           END-IF
           IF  S-YY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO S-YY
           END-IF.
      **
       INI-EX.
           EXIT.
      *----------------------*
      *    ＥＮＤ－ＲＴＮ    *
      *----------------------*
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NF-WK1_IDLST NF-WK1_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NF-WK3_IDLST NF-WK3_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JCON_IDLST JCON_PNAME1.
       END-EX.
           EXIT.
      *--------------------------------*
      *    データ　　抽出　            *
      *--------------------------------*
       UPD-RTN.
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           MOVE     SPACE    TO    JCON7-KEY.
           MOVE     7        TO    JCON7-01.
      *           READ     JCON     UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               MOVE    "JCON"      TO     ERR-F
               MOVE     JCON1-KEY  TO     ERR-K
               MOVE    "A"         TO     ERR-M
               PERFORM  ERR-RTN    THRU   ERR-EX
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP     RUN
           END-IF
           IF (JS-SIGN = 0) AND (JCON7-06 = 0)
               GO  TO  UPD-BBB
           END-IF
           IF (JS-SIGN = 1) AND (JCON7-08 = 0)
               GO  TO  UPD-BBB
           END-IF
           CALL "SD_Output" USING "ERR-KBN" ERR-KBN "p" RETURNING RESU.
       UPD-AAA.
           CALL "SD_Accept" USING BY REFERENCE ACP-KBN "ACP-KBN" "X" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT   NOT   =   "01" AND  "06"
               GO  TO  UPD-AAA
           END-IF
           IF  W-KBN    NOT =    "Y"  AND  "N"
               GO  TO  UPD-AAA
           END-IF
           IF  W-KBN    =  "Y"
               GO  TO  UPD-BBB
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "DB_Close".
           STOP     RUN.
       UPD-BBB.
           CALL "DB_F_Open" USING
            "OUTPUT" NF-WK1_PNAME1 "EXCLUSIVE" BY REFERENCE NF-WK1_IDLST
            "1" "WK1-KEY" BY REFERENCE WK1-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" NF-WK3_PNAME1 "EXCLUSIVE" BY REFERENCE NF-WK3_IDLST
            "1" "WK3-KEY" BY REFERENCE WK3-KEY.
           CALL "DB_F_Close" USING
            BY REFERENCE NF-WK1_IDLST NF-WK1_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NF-WK3_IDLST NF-WK3_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "I-O" NF-WK1_PNAME1 "EXCLUSIVE" BY REFERENCE NF-WK1_IDLST
            "1" "WK1-KEY" BY REFERENCE WK1-KEY.
           CALL "DB_F_Open" USING
            "I-O" NF-WK3_PNAME1 "EXCLUSIVE" BY REFERENCE NF-WK3_IDLST
            "1" "WK3-KEY" BY REFERENCE WK3-KEY.
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
       UPD-000.
      *           READ     JSTR     NEXT  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-050
           END-IF
           IF  JSTR-05  NOT =    ZERO
               GO  TO  UPD-000
           END-IF
           IF  JSTR-03  NOT =    0 AND 7
               GO  TO  UPD-000
           END-IF
           IF  JSTR-14      =    9
               GO  TO  UPD-000
           END-IF
           IF  JSTR-4012    =    0
               GO  TO  UPD-000
           END-IF
           MOVE     1        TO       I.
       UPD-010.
           IF  I        >        10
               GO  TO  UPD-020
           END-IF
           IF  JSTR-1111(I)   <   0
               GO  TO  UPD-000
           END-IF
           ADD 1        TO       I.
           GO  TO   UPD-010.
       UPD-020.
           IF  JSTR-04  NOT =    SYUKA
               GO  TO  UPD-000
           END-IF
           IF  JSTR-16  NOT =    JS-SIGN
               GO  TO  UPD-000
           END-IF
           PERFORM  JSTR-RTN     THRU     JSTR-EX.
      *
           MOVE     JSTR-09  TO       HI-MHCD HI-HCD.
      *           READ     HI2-M    UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "ERR-HIM" ERR-HIM "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO   UPD-EX
           END-IF
           MOVE     HI-ISU   TO       WK-KIRI.
           MOVE     0        TO       HI-S4(10).
           MOVE     HI-AHSD  TO       WK-SOKU.
           MOVE     1        TO       I.
       UPD-030.
           IF  I        >        10
               GO  TO  UPD-046
           END-IF
           IF  JSTR-1111(I)  NOT =   ZERO
               GO  TO  UPD-040
           END-IF
           ADD 1        TO       I.
           GO  TO   UPD-030.
       UPD-040.
           MOVE     0        TO       INV-SW.
           PERFORM  MOV-RTN  THRU     MOV-EX.
           IF  ERR-SW   =        1
               GO  TO  UPD-EX
           END-IF
           ADD 1        TO       I.
           GO  TO   UPD-030.
       UPD-046.
           PERFORM  JSTR-RTN     THRU     JSTR-EX.
           IF  ERR-SW   =        1
               GO  TO  UPD-EX
           END-IF
           GO  TO   UPD-000.
       UPD-050.
           MOVE     SPACE    TO       WK1-KEY.
      *           START    NF-WK1   KEY  NOT <   WK1-KEY INVALID
      *///////////////
           CALL "DB_Start" USING
            NF-WK1_PNAME1 "WK1-KEY" " NOT < " WK1-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-999
           END-IF.
       UPD-060.
      *           READ     NF-WK1   NEXT AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" NF-WK1_PNAME1 BY REFERENCE WK1-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-999
           END-IF
           IF  WK1-06   =        ZERO
               GO  TO  UPD-063
           END-IF
           IF  WK1-01   NOT =    OLD3-01
               GO  TO  UPD-061
           END-IF
           IF  WK1-021  NOT =    OLD3-021
               GO  TO  UPD-061
           END-IF
           IF  WK1-06   NOT =    W-SET
               GO  TO  UPD-062
           END-IF
           GO  TO   UPD-060.
       UPD-061.
           MOVE     WK1-01   TO       OLD3-01.
           MOVE     WK1-021  TO       OLD3-021.
           MOVE     WK1-06   TO       W-SET.
           GO  TO   UPD-060.
       UPD-062.
           CALL "SD_Output" USING "ERR-SET" ERR-SET "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           GO  TO   UPD-EX.
       UPD-063.
           IF  WK1-01   NOT =    WWK3-01
               GO  TO  UPD-065
           END-IF
           IF  WK1-021  NOT =    WWK3-02
               GO  TO  UPD-065
           END-IF
           IF  WK1-03   NOT =    WWK3-03
               GO  TO  UPD-065
           END-IF
           IF  WK1-09   NOT =    WWK3-09
               CALL "SD_Output" USING
                "ERR-IRI" ERR-IRI "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO   UPD-EX
           END-IF.
       UPD-065.
           MOVE     0        TO       INV-SW.
           MOVE     WK1-01   TO       WK3-01.
           MOVE     WK1-021  TO       WK3-02.
           MOVE     WK1-03   TO       WK3-03.
      *           READ     NF-WK3   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NF-WK3_PNAME1 BY REFERENCE WK3-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE     1   TO        INV-SW
               MOVE     SPACE    TO       WK3-R
               INITIALIZE                 WK3-R
               MOVE     WK1-01   TO       WK3-01
               MOVE     WK1-021  TO       WK3-02
               MOVE     WK1-03   TO       WK3-03
           END-IF.
       UPD-070.
           ADD      WK1-07   TO       WK3-04.
           MOVE     WK1-09   TO       WK3-05.
           IF  WK1-09   =    ZERO
               MOVE     ZERO       TO    WK3-06
           ELSE
               COMPUTE  WK3-06   =    WK3-04     /     WK1-09
           END-IF
           COMPUTE  WK3-07   =    WK3-04   -    (WK3-06  *  WK3-05).
           ADD      WK1-10   TO   WK3-08.
       UPD-085.
           IF  INV-SW   NOT =    1
               GO  TO  UPD-090
           END-IF
      *           WRITE    WK3-R    INVALID
      *//////////////
           CALL "DB_Insert" USING
            NF-WK3_PNAME1 NF-WK3_LNAME WK3-R RETURNING RET.
           IF  RET = 1
               MOVE    "NF-WK3"    TO    ERR-F
               MOVE     WK3-KEY    TO    ERR-K
               MOVE    "W"         TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO   UPD-EX
           END-IF
           GO  TO   UPD-100.
       UPD-090.
      *           REWRITE  WK3-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            NF-WK3_PNAME1 NF-WK3_LNAME WK3-R RETURNING RET.
           IF  RET = 1
               MOVE    "NF-WK3"    TO    ERR-F
               MOVE     WK3-KEY    TO    ERR-K
               MOVE    "R"         TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO   UPD-EX
           END-IF.
       UPD-100.
           MOVE     WK3-KEY  TO       WWK3-KEY.
           MOVE     WK1-09   TO       WWK3-09.
           GO  TO   UPD-060.
       UPD-999.
           MOVE     SPACE    TO       JCON7-KEY.
           MOVE     7        TO       JCON7-01.
      *           READ     JCON     INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               MOVE    "JCON"      TO     ERR-F
               MOVE     JCON1-KEY  TO     ERR-K
               MOVE    "A"         TO     ERR-M
               PERFORM  ERR-RTN    THRU   ERR-EX
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO   UPD-EX
           END-IF
           IF  JS-SIGN  =  0
               MOVE     SYUKAS   TO       JCON7-05
               MOVE     1        TO       JCON7-06
           ELSE
               MOVE     SYUKAS   TO       JCON7-07
               MOVE     1        TO       JCON7-08
           END-IF
      *           REWRITE  JCON1-R  INVALID
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               MOVE    "JCON"      TO     ERR-F
               MOVE     JCON1-KEY  TO     ERR-K
               MOVE    "R"         TO     ERR-M
               PERFORM  ERR-RTN    THRU   ERR-EX
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
           END-IF.
       UPD-EX.
           EXIT.
      ***************************************
      *    荷札ワーク１作成                 *
      ***************************************
       MOV-RTN.
           MOVE     JSTR-14B TO       WK1-01.
           MOVE     JSTR-14C TO       WK1-021.
           MOVE     ZERO     TO       WK1-022.
           IF  JSTR-14A NOT =    ZERO
               MOVE     0       TO    WK1-03
               MOVE     JSTR-10 TO    P
           ELSE
               MOVE     JSTR-10          TO    P
               MOVE     WK-KBN(P , I)    TO    WK1-03
           END-IF
           MOVE     JSTR-09  TO       WK1-04.
           IF  P        =        1
               MOVE     WK-SIZE1  TO  WK-SIZCD
           END-IF
           IF  P        =        2
               MOVE     WK-SIZE2  TO  WK-SIZCD
           END-IF
           IF  P        =        3
               MOVE     WK-SIZE3  TO  WK-SIZCD
           END-IF
           IF  P        =        4
               MOVE     WK-SIZE4  TO  WK-SIZCD
           END-IF
           COMPUTE  WK-HOSU  =        1000      -   WK-SIZCD1(I).
           MOVE     WK-HOSU  TO       WK1-05.
      *           READ     NF-WK1   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NF-WK1_PNAME1 BY REFERENCE WK1-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE     1   TO        INV-SW
           END-IF
           IF  INV-SW   NOT =    1
               GO  TO   MOV-020
           END-IF
           MOVE     SPACE    TO       WK1-R.
           INITIALIZE                 WK1-R.
           MOVE     JSTR-14B TO       WK1-01.
           MOVE     JSTR-14C TO       WK1-021.
           MOVE     ZERO     TO       WK1-022.
           IF  JSTR-14A NOT =    ZERO
               MOVE     0      TO     WK1-03
           ELSE
               MOVE     WK-KBN(P , I)   TO     WK1-03
           END-IF
           MOVE     JSTR-09  TO       WK1-04.
           MOVE     WK-HOSU  TO       WK1-05.
       MOV-020.
           MOVE     JSTR-14A TO       WK1-06.
           MOVE     JSTR-01  TO       WK1-08.
           MOVE     WK-KIRI  TO       WK1-09.
           IF  JSTR-14A NOT =    ZERO
               COMPUTE  WK-SASOK =  JSTR-1111(I) / JSTR-14A
               ADD      WK-SASOK TO      WK1-07
               MOVE     ZERO     TO      WK1-10
           ELSE
               MOVE     JSTR-1111(I) TO  WK-SASOK
               ADD      WK-SASOK TO      WK1-07
               IF  WK-KIRI  =  0
                   MOVE     0      TO   WK1-10
               ELSE
                   COMPUTE  WK1-10   =  WK1-07   / WK-KIRI
               END-IF
           END-IF
           MOVE     WK1-10   TO       WK-SOIRI.
           COMPUTE  WK-ZAN   =  WK1-07  -  (WK-SOIRI * WK-KIRI).
           MOVE     WK-ZAN   TO       WK1-11.
           IF  INV-SW   =        1
               GO  TO   MOV-030
           END-IF
      *           REWRITE  WK1-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            NF-WK1_PNAME1 NF-WK1_LNAME WK1-R RETURNING RET.
           IF  RET = 1
               MOVE    "NF-WK1"  TO    ERR-F
               MOVE    WK1-KEY   TO    ERR-K
               MOVE    "R"       TO    ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
               MOVE     1        TO    ERR-SW
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
           END-IF
           GO  TO   MOV-EX.
       MOV-030.
      *           WRITE    WK1-R    INVALID
      *//////////////
           CALL "DB_Insert" USING
            NF-WK1_PNAME1 NF-WK1_LNAME WK1-R RETURNING RET.
           IF  RET = 1
               MOVE    "NF-WK1"  TO    ERR-F
               MOVE    WK1-KEY   TO    ERR-K
               MOVE    "W"       TO    ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
               MOVE     1        TO    ERR-SW
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
           END-IF.
       MOV-EX.
           EXIT.
      **************************************
      *    出荷指図トラン　更新ルーチン    *   ADD:890904
      **************************************
       JSTR-RTN.
           MOVE    9         TO    JSTR-17.
      *           REWRITE  JSTR-R   INVALID
      *///////////////
           CALL "DB_Update" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE   "JSTR"     TO    ERR-F
               MOVE    JSTR-KEY  TO    ERR-K
               MOVE    "R"       TO    ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
               MOVE     1        TO    ERR-SW
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
           END-IF.
       JSTR-EX.
           EXIT.
      *****************************
      *    ｴﾗｰ DISPLAY (ﾒｲﾝ)      *
      *****************************
       ERR-RTN.
           MOVE    ERR-STAT  TO  ERR-FLG.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
       ERR-EX.
           EXIT.
      *
      *

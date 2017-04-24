       IDENTIFICATION        DIVISION.
       PROGRAM-ID.           JT590I.
       AUTHOR.               I.N.
      **************************************************
      *    PROGRAM      :    出荷確定入力              *
      *    DATA WRITTEN :    91/10/21                  *
      *    SCREEN  USED :    SJ590I                    *
      *    COMPILE TYPE :    CBL85                     *
      **************************************************
       ENVIRONMENT           DIVISION.
       CONFIGURATION         SECTION.
       SOURCE-COMPUTER.      SYSTEM100.
       OBJECT-COMPUTER.      SYSTEM100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       01  SASIZU-SURYO.
           02  S-SURYO  OCCURS  6.
               03  S-S        PIC S9(06)  OCCURS  10.
       77  ERR-STAT           PIC  X(02).
       77  DIS-SW             PIC  9(01).
       77  JS-SIGN            PIC  9(01).
       77  INP-SW             PIC  9(01).
       77  INV-SW             PIC  9(01).
       77  HIZUKE             PIC  9(08).
       77  WRI-SW             PIC  9(01).
       77  W-C                PIC  9(02).
       77  CHK                PIC  9(01).
       01  N-24               PIC  N(24)  VALUE  "　".
       01  WJSTR-R            PIC  X(256).
       01  STN-NO.
           02  STN-NO-01      PIC  X(03).
           02  STN-NO-02      PIC  X(03).
       01  W-AREA1.
           02  WYMD.
               03  WYY        PIC  9(02).
               03  WMM        PIC  9(02).
               03  WDD        PIC  9(02).
           02  W-NGP.
             03  W-NEN        PIC  9(04).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(02).
               04  W-NEN2     PIC  9(02).
             03  W-GET        PIC  9(02).
             03  W-PEY        PIC  9(02).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(02).
             03  W-NGPS       PIC  9(06).
       01  W-AREA2.
           02  O              PIC  9(01)   VALUE  ZERO.
           02  M              PIC  9(02)   VALUE  ZERO.
           02  A              PIC  9(01)   VALUE  ZERO.
           02  B              PIC  9(02)   VALUE  ZERO.
           02  C              PIC  9(01)   VALUE  ZERO.
           02  I              PIC  9(02)   VALUE  ZERO.
           02  LIN1           PIC  9(02)   VALUE  ZERO.
           02  LIN2           PIC  9(02)   VALUE  ZERO.
           02  COL1           PIC  9(02)   VALUE  ZERO.
           02  WA             PIC  9(01)   VALUE  ZERO.
           02  WB             PIC  9(03)   VALUE  ZERO.
       01  ACT-WORK1.
           02  W-ACT          PIC  9(01)   VALUE  ZERO.
           02  W-NAM          PIC  N(02)   VALUE  SPACE.
       01  ACT-WORK2.
           02  W-1            PIC  9(06)   VALUE  ZERO.
           02  W-1R    REDEFINES   W-1.
               03  W1-1       PIC  9(01).
               03  W1-2       PIC  9(05).
           02  W-2.
               03  W-21       PIC  9(01)   VALUE  ZERO.
               03  W-22       PIC  N(02)   VALUE  SPACE.
           02  W-3.
               03  W-31       PIC  9(04)   VALUE  ZERO.
               03  W-31L   REDEFINES W-31.
                   04  W-311  PIC  9(02).
                   04  W-312  PIC  9(02).
               03  W-32       PIC  9(02)   VALUE  ZERO.
               03  W-33       PIC  9(02)   VALUE  ZERO.
           02  W-4.
               03  W-4A.
                   04  W-41   PIC  9(04)   VALUE  ZERO.
                   04  W-42   PIC  9(03)   VALUE  ZERO.
               03  W-43       PIC  N(24)   VALUE  SPACE.
               03  W-44       PIC  N(24)   VALUE  SPACE.
           02  W-5.
               03  W-51       PIC  9(01)   VALUE  ZERO.
               03  W-52       PIC  N(06)   VALUE  SPACE.
           02  W-5A           PIC  9(03)   VALUE  ZERO.
           02  W-6.
               03  W-6A    OCCURS   6.
                   04  W-61.
                       05  W-611   PIC  9(06).
                       05  W-612   PIC  9(01).
                   04  W-62.
                       05  W-621   PIC  9(06).
                       05  W-621R  REDEFINES  W-621.
                           06  W-6211   PIC  9(04).
                           06  W-6212   PIC  9(02).
                       05  W-622   PIC  N(24).
                   04  W-63        PIC  9(01).
                   04  W-64A.
                       05  W-64    PIC  S9(04)  OCCURS  10.
                   04  W-65        PIC  S9(05).
                   04  W-66        PIC  9(01).
                   04  W-67        PIC  X(10).
                   04  W-999       PIC  9(01).
           02  W-7.
                   04  W-71        PIC  9(01).
                   04  W-72        PIC  N(06).
           02  W-88.
               03  W-8A            PIC  N(09)   VALUE  SPACE.
               03  W-8.
                   05  W-81        PIC  N(04)   VALUE  SPACE.
                   05  W-82        PIC  N(19)   VALUE  SPACE.
           02  W-9.
                   04  W-91        PIC  9(01).
                   04  W-92        PIC  9(06).
                   04  W-93        PIC  9(02).
                   04  W-94        PIC  N(03).
           02  W-OKC          PIC  9(01)   VALUE  ZERO.
           02  W1-1D          PIC  9(01).
           02  W-KOS          PIC S9(03)   VALUE  ZERO.
           02  W-KEI          PIC S9(07)   VALUE  ZERO.
           02  W-OLD.
               03  CNT        PIC  9(01)   VALUE  ZERO.
               03  O-SCD      PIC  9(01)   VALUE  ZERO.
               03  OLD-SURYO   OCCURS   6.
                   04  O-HCD  PIC  9(06).
                   04  O-SIZ  PIC  9(01).
                   04  O-SUA.
                       05  O-SU    PIC  S9(04)  OCCURS  10.
           02  W-KATON.
               03  W-KA       PIC  9(03)   OCCURS  6.
           02  W-BUN.
               03  W-B1       PIC  9(01).
               03  W-B2       PIC  9(01).
           02  SAVE-HCD       OCCURS  6.
               03  S-HCD      PIC  9(06).
           02  W-DNO          PIC  9(06).
       01  WK-AREA.
           02  WK-TBL    OCCURS  6.
               03  WK-HIN              PIC  9(06).
               03  WK-SIZ              PIC  9(01).
       01  REV-AREA.
           02  REV-1.
               03  REV-1A     PIC  X(40)     VALUE
                   "1 ３号  ２号  １号  ０号   中    大   特".
               03  REV-1B     PIC  X(21)     VALUE
                   "大  28.0  29.0  30.0 ".
           02  REV-2.
               03  REV-2A     PIC  X(40)     VALUE
                   "2 12.5  13.0  13.5  14.0  15.0  16.0  17".
               03  REV-2B     PIC  X(21)     VALUE
                   ".0  18.0  19.0  20.0 ".
           02  REV-3.
               03  REV-3A     PIC  X(40)     VALUE
                   "3 21.0  21.5  22.0  22.5  23.0  23.5  24".
               03  REV-3B     PIC  X(21)     VALUE
                   ".0  24.5  25.0  ---- ".
           02  REV-4.
               03  REV-4A     PIC  X(40)     VALUE
                   "4 24.0  24.5  25.0  25.5  26.0  26.5  27".
               03  REV-4B     PIC  X(21)     VALUE
                   ".0  27.5  ----  ---- ".
      *
           COPY     LWMSG.
      *
           COPY     LIBFDD.
           COPY     L-JSTR.
           COPY     LIHIM2.
           COPY     LITCM.
           COPY     L-JSJD.
           COPY     L-JCON.
           COPY     LJMSTD.
           COPY     LTDNKN.
           COPY     L-JNSR.
           COPY     LNJZAI.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-AREA.
           02  FILLER  PIC  X(22)
                   VALUE "【　出荷確定　入力　】".
           02  FILLER  PIC  X(14)
                   VALUE "教　育     = 0".
           02  FILLER  PIC  X(20)
                   VALUE "一　般     = 1 ...  ".
           02  FILLER  PIC  X(22)
                   VALUE "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  ACP-AREA.
           02  ACP-SIGN    PIC  9(01).
           02  ACP-ACT     PIC  9(01).
           02  DSP-NAM     PIC  N(02).
           02  ACP-1       PIC  9(06).
           02  ACP-2.
               03  A-21    PIC  9(01).
               03  D-22    PIC  N(02).
           02  ACP-3.
               03  A-31    PIC  9(02).
               03  A-32    PIC  9(02).
               03  A-33    PIC  9(02).
           02  ACP-4.
               03  A-41    PIC  9(04).
               03  A-42    PIC  9(03).
               03  D-43    PIC  N(24).
           02  D-44   PIC  N(24).
           02  ACP-5.
               03  A-51    PIC  9(01).
               03  D-52    PIC  N(06).
               03  A-53    PIC  9(03).
               03  D-54    PIC  ZZ9 .
           02  ACP-6A.
               03  A-611   PIC  9(06).
               03  A-612   PIC  9(01).
               03  A-613   PIC  X(01)  VALUE  "-".
               03  A-612A  PIC  X(08)  VALUE  " ".
               03  D-622   PIC  N(24).
               03  A-67    PIC  X(10).
           02  ACP-6B.
               03  A-621   PIC  9(06) .
               03  A-63    PIC  9(01) .
               03  A-64    PIC  S9(04).
               03  D-64    PIC  ZZZZ- .
               03  D-65    PIC ZZZ,ZZZ- .
           02  DSP-6B.
               03  DIS-1   PIC  ZZZZ- .
               03  DIS-2   PIC  ZZZZ- .
               03  DIS-3   PIC  ZZZZ- .
               03  DIS-4   PIC  ZZZZ- .
               03  DIS-5   PIC  ZZZZ- .
               03  DIS-6   PIC  ZZZZ- .
               03  DIS-7   PIC  ZZZZ- .
               03  DIS-8   PIC  ZZZZ- .
               03  DIS-9   PIC  ZZZZ- .
               03  DIS-10  PIC  ZZZZ- .
           02  DSP-KEI     PIC  ZZZ,ZZZ- .
           02  DSP-LINE    PIC  X(01)  VALUE  "-".
           02  ACP-KOS     PIC S9(03).
           02  DSP-KOS     PIC ZZ9- .
           02  ACP-OKC     PIC  9(01).
           02  DSP-S1      PIC  X(06)  VALUE
                                  "教　育".
           02  DSP-S2      PIC  X(06)  VALUE
                                  "一　般".
           02  DSP-BIKHO.
               03  FILLER  PIC X(12) VALUE "[          ]".
               03  FILLER  PIC X(12) VALUE "[          ]".
               03  FILLER  PIC X(12) VALUE "[          ]".
               03  FILLER  PIC X(12) VALUE "[          ]".
               03  FILLER  PIC X(12) VALUE "[          ]".
               03  FILLER  PIC X(12) VALUE "[          ]".
           02  DSP-BIKHO-CLE.
               03  FILLER  PIC X(12) VALUE "            ".
               03  FILLER  PIC X(12) VALUE "            ".
               03  FILLER  PIC X(12) VALUE "            ".
               03  FILLER  PIC X(12) VALUE "            ".
               03  FILLER  PIC X(12) VALUE "            ".
               03  FILLER  PIC X(12) VALUE "            ".
       01  DSP-CLE.
           02  CLE-01.
               03  C-11.
                   04  FILLER  PIC X(06) VALUE "      ".
                   04  FILLER  PIC X(01) VALUE " ".
                   04  FILLER  PIC X(04) VALUE "    ".
                   04  FILLER  PIC X(02) VALUE "  ".
                   04  FILLER  PIC X(02) VALUE "  ".
                   04  FILLER  PIC X(02) VALUE "  ".
                   04  FILLER  PIC X(04) VALUE "    ".
                   04  FILLER  PIC X(03) VALUE "   ".
                   04  C-111   PIC N(24).
               03  C-12.
                   04  FILLER  PIC X(01) VALUE " ".
                   04  FILLER  PIC X(12) VALUE "　　　　　　".
                   04  FILLER  PIC X(04) VALUE "    ".
                   04  C-121   PIC N(24).
               03  C-13.
                   04  FILLER  PIC X(01) VALUE " ".
                   04  FILLER  PIC X(12) VALUE "　　　　　　".
                   04  C-131.
                       05  FILLER  PIC X(01) VALUE " ".
                       05  FILLER  PIC X(06) VALUE "      ".
                       05  FILLER  PIC X(02) VALUE "  ".
                   04  FILLER  PIC X(08) VALUE "        ".
               03  C-14.
                   04  FILLER  PIC X(04) VALUE  "    ".
               03  C-15    PIC X(09) VALUE "         ".
               03  C-16    PIC X(01) VALUE " ".
           02  CLE-02.
               03  CLE-21.
                   04  FILLER  PIC X(07) VALUE "       ".
                   04  FILLER  PIC X(01) VALUE " ".
                   04  FILLER  PIC X(24)
                               VALUE "                        ".
                   04  FILLER  PIC X(24)
                               VALUE "                        ".
                   04  FILLER  PIC X(12) VALUE "            ".
               03  CLE-22.
                   04  FILLER  PIC X(07) VALUE "       ".
                   04  FILLER  PIC X(01) VALUE " ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(05) VALUE "     ".
                   04  FILLER  PIC X(08) VALUE "        ".
           02  CLE-03      PIC X(06) VALUE  "      ".
           02  CLE-04.
               03  C-41    PIC X(08) VALUE  "        ".
               03  C-42    PIC X(07) VALUE  "       ".
       01  DSP-REV-AREA.
           02  DSP-RR-ALL.
               03 DSP-RR1     PIC  X(61).
               03 DSP-RR2     PIC  X(61).
               03 DSP-RR3     PIC  X(61).
               03 DSP-RR4     PIC  X(61).
           02  DSP-RC-ALL.
               03 DSP-RC1     PIC  X(61).
               03 DSP-RC2     PIC  X(61).
               03 DSP-RC3     PIC  X(61).
               03 DSP-RC4     PIC  X(61).
       01  DSP-ERR.
           02  INV-T01     PIC X(30)
                           VALUE "＊＊出荷指図トラン　未登録＊＊".
           02  INV-M02     PIC X(32)
                           VALUE "＊＊出荷品名マスター　未登録＊＊".
           02  INV-M03     PIC X(30)
                           VALUE "＊＊直送先マスター　未登録＊＊".
           02  ERR-05      PIC X(22)
                           VALUE "＊　指図数オーバー　＊".
           02  ERR-06      PIC X(14)
                           VALUE "＊　出荷済　＊".
           02  ERR-08      PIC X(24)
                           VALUE "＊　入数が同一でない　＊".
           02  ERR-13      PIC X(26)
                           VALUE "＊　　　　　アンマッチ　＊".
           02  ERR-13A     PIC N(03).
           02  ERR-14      PIC X(26)
                           VALUE "＊　入数がＺＥＲＯです　＊".
           02  ERR-HIM     PIC X(40)
               VALUE "品名マスター使用サイン　ＡＬＬ　ＺＥＲＯ".
           02  ERR-SYK     PIC X(18)
               VALUE "＊　出荷　不可　＊".
           02  ERR-SET     PIC X(26)
               VALUE "＊　セット数量　エラー　＊".
           02  ERR-KAK     PIC X(18)
               VALUE "＊　確定　不可　＊".
           02  ERR-GAI     PIC X(28)
               VALUE "＊　外部倉庫分　処理不可　＊".
           02  ERR-KSUMI   PIC X(24)
               VALUE "＊　確定済　処理不可　＊".
           02  ERR-TORER   PIC X(18)
               VALUE "＊　取消　不可　＊".
           02  ERR-LST     PIC X(26)
               VALUE "出荷指図書（実績）　未発行".
           02  ERR-LST2    PIC X(18)
               VALUE "出荷指図書　未発行".
           02  ERR-NGP     PIC X(14)
               VALUE "日付　チェック".
           02  ERR-READ    PIC X(26)
               VALUE "ＪＳＴＲ　ＲＥＡＤ　エラー".
           COPY     LIBSCR.
      **
       01  DISP-ERR-AREA.
           02  DISP-MSG-01.
               03  FILLER  PIC X(60).
           02  DISP-MSG-SPACE.
               03  FILLER  PIC X(40).
           02  DISP-BUZ-B.
               03  FILLER  PIC X(05) VALUE X"1B4210".
           02  DISP-BUZ-J.
               03  FILLER  PIC X(05) VALUE X"1B4A01".
           02  NOR-M01.
               03  FILLER  PIC X(22) VALUE
               "＊　マスタ　登録済　＊".
           02  NOR-D01.
               03  FILLER  PIC X(22) VALUE
               "＊　データ　登録済　＊".
           02  INV-M01.
               03  FILLER  PIC X(22) VALUE
               "＊　マスタ　未登録　＊".
           02  INV-D01.
               03  FILLER  PIC X(22) VALUE
               "＊　データ　未登録　＊".
           02  OK-01.
               03  FILLER  PIC X(14) VALUE
               "＊　Ｏ　Ｋ　＊".
           02  CAN-01.
               03  FILLER  PIC X(18) VALUE
               "＊　キャンセル　＊".
           02  ERR-01.
               03  FILLER  PIC X(18) VALUE
               "＊　入力エラー　＊".
           02  ERR-02.
               03  FILLER  PIC X(22) VALUE
               "＊　データ　なし　　＊".
           02  ERR-DIS.
               03  FILLER  PIC X(05) VALUE
               "<<<  ".
               03  FILLER  PIC X(12).
               03  FILLER  PIC X(01).
               03  FILLER  PIC X(11) VALUE
               "ｴﾗｰ STATUS=".
               03  FILLER  PIC X(02).
               03  FILLER  PIC X(05) VALUE
               "  >>>".
               03  FILLER  PIC X(05) VALUE
               " KEY=".
               03  FILLER  PIC X(30).
      ************************
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-CLR
       CALL "SD_Init" USING
           "DSP-CLR" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "78" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" "X" "1" "29" "22" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" "X" "6" "28" "14" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" "X" "8" "28" "20" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-AREA" "X" "24" "43" "22" "03DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "466" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SIGN" "9" "8" "47" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SIGN" BY REFERENCE JS-SIGN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ACT" "9" "1" "67" "1" "ACP-SIGN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-NAM" "N" "1" "69" "4" "ACP-ACT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-NAM" BY REFERENCE W-NAM "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-1" "9" "3" "2" "6" "DSP-NAM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-1" BY REFERENCE W-1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-2" " " "3" "0" "5" "ACP-1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-21" "9" "3" "9" "1" " " "ACP-2" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-21" BY REFERENCE W-21 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-22" "N" "3" "10" "4" "A-21" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-22" BY REFERENCE W-22 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-3" " " "3" "0" "6" "ACP-2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-31" "9" "3" "15" "2" " " "ACP-3" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-31" BY REFERENCE W-312 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-32" "9" "3" "18" "2" "A-31" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-32" BY REFERENCE W-32 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-33" "9" "3" "21" "2" "A-32" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-33" BY REFERENCE W-33 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-4" " " "3" "0" "55" "ACP-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-41" "9" "3" "24" "4" " " "ACP-4" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-41" BY REFERENCE W-41 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-42" "9" "3" "29" "3" "A-41" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-42" BY REFERENCE W-42 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-43" "N" "3" "33" "48" "A-42" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-43" BY REFERENCE W-43 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-44" "N" "4" "33" "48" "ACP-4" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-44" BY REFERENCE W-44 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-5" " " "4" "0" "19" "D-44" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-51" "9" "4" "7" "1" " " "ACP-5" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-51" BY REFERENCE W-51 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-52" "N" "4" "9" "12" "A-51" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-52" BY REFERENCE W-52 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-53" "9" "4" "29" "3" "D-52" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-53" BY REFERENCE W-5A "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-54" "ZZ9" "4" "29" "3" "A-53" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-54" BY REFERENCE W-5A "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-6A" " " "LIN1" "0" "74" "ACP-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-611" "9" "LIN1" "2" "6" " " "ACP-6A" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-611" BY REFERENCE W-611(1) "6" "1" BY REFERENCE A 119
             RETURNING RESU.
       CALL "SD_Init" USING 
            "A-612" "9" "LIN1" "9" "1" "A-611" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-612" BY REFERENCE W-612(1) "1" "1" BY REFERENCE A 119
             RETURNING RESU.
       CALL "SD_Init" USING 
            "A-613" "X" "LIN1" "8" "1" "A-612" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-612A" "X" "LIN1" "2" "8" "A-613" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-622" "N" "LIN1" "11" "48" "A-612A" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-622" BY REFERENCE W-622(1) "48" "1" BY REFERENCE A 119
             RETURNING RESU.
       CALL "SD_Init" USING 
            "A-67" "X" "LIN1" "70" "10" "D-622" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-67" BY REFERENCE W-67(1) "10" "1" BY REFERENCE A 119
             RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-6B" " " "LIN1 PLUS 1" "0" "24" "ACP-6A" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-621" "9" "LIN1 PLUS 1" "2" "6" " " "ACP-6B"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "A-621" BY REFERENCE W-621(1) "6" "1" BY REFERENCE A 119
             RETURNING RESU.
       CALL "SD_Init" USING 
            "A-63" "9" "LIN1 PLUS 1" "11" "1" "A-621" " "
             RETURNING RESU.
       CALL "SD_Using" USING 
            "A-63" BY REFERENCE W-63(1) "1" "1" BY REFERENCE A 119
             RETURNING RESU.
       CALL "SD_Init" USING 
            "A-64" "S9" "LIN1 PLUS 1" "COL1" "4" "A-63" " "
            RETURNING RESU.
       CALL "SD_Into" USING 
            "A-64" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 119
            BY REFERENCE B 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-64" "ZZZZ-" "LIN1 PLUS 1" "COL1" "5" "A-64" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-64" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 119
            BY REFERENCE B 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-65" "ZZZ,ZZZ-" "LIN1 PLUS 1" "73" "8" "D-64" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-65" BY REFERENCE W-65(1) "5" "1" BY REFERENCE A 119
             RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-6B" " " "LIN1 PLUS 1" "0" "50" "ACP-6B" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-1" "ZZZZ-" "LIN1 PLUS 1" "13" "5" " " "DSP-6B"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-1" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 119 
             "1" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-2" "ZZZZ-" "LIN1 PLUS 1" "19" "5" "DIS-1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-2" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 119 
            "2" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-3" "ZZZZ-" "LIN1 PLUS 1" "25" "5" "DIS-2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-3" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 119
            "3" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-4" "ZZZZ-" "LIN1 PLUS 1" "31" "5" "DIS-3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-4" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 119
            "4" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-5" "ZZZZ-" "LIN1 PLUS 1" "37" "5" "DIS-4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-5" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 119
            "5" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-6" "ZZZZ-" "LIN1 PLUS 1" "43" "5" "DIS-5" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-6" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 119
            "6" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-7" "ZZZZ-" "LIN1 PLUS 1" "49" "5" "DIS-6" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-7" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 119
            "7" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-8" "ZZZZ-" "LIN1 PLUS 1" "55" "5" "DIS-7" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-8" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 119
            "8" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-9" "ZZZZ-" "LIN1 PLUS 1" "61" "5" "DIS-8" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-9" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 119
            "9" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-10" "ZZZZ-" "LIN1 PLUS 1" "67" "5" "DIS-9" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-10" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 119
            "10" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KEI" "ZZZ,ZZZ-" "22" "73" "8" "DSP-6B" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KEI" BY REFERENCE W-KEI "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-LINE" "X" "3" "28" "1" "DSP-KEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KOS" "S9" "23" "7" "3" "DSP-LINE" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KOS" BY REFERENCE W-KOS "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KOS" "ZZ9-" "23" "7" "4" "ACP-KOS" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KOS" BY REFERENCE W-KOS "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "24" "60" "1" "DSP-KOS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-S1" "bX" "1" "21" "6" "ACP-OKC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-S2" "bX" "1" "21" "6" "DSP-S1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-BIKHO" " " "0" "0" "72" "DSP-S2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-BIKHO" "X" "10" "69" "12" " " "DSP-BIKHO"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-BIKHO" "X" "12" "69" "12" "01DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-BIKHO" "X" "14" "69" "12" "02DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-BIKHO" "X" "16" "69" "12" "03DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-BIKHO" "X" "18" "69" "12" "04DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-BIKHO" "X" "20" "69" "12" "05DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-BIKHO-CLE" " " "0" "0" "72" "DSP-BIKHO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-BIKHO-CLE" "X" "10" "69" "12" " " "DSP-BIKHO-CLE"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-BIKHO-CLE" "X" "12" "69" "12" "01DSP-BIKHO-CLE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-BIKHO-CLE" "X" "14" "69" "12" "02DSP-BIKHO-CLE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-BIKHO-CLE" "X" "16" "69" "12" "03DSP-BIKHO-CLE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-BIKHO-CLE" "X" "18" "69" "12" "04DSP-BIKHO-CLE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-BIKHO-CLE" "X" "20" "69" "12" "05DSP-BIKHO-CLE" " "
            RETURNING RESU.
      *DSP-CLE
       CALL "SD_Init" USING 
            "DSP-CLE" " " "0" "0" "336" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-01" " " "0" "0" "181" " " "DSP-CLE" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-11" " " "3" "0" "72" " " "CLE-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-11" "X" "3" "2" "6" " " "C-11" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-11" "X" "3" "9" "1" "01C-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-11" "X" "3" "10" "4" "02C-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-11" "X" "3" "15" "2" "03C-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-11" "X" "3" "18" "2" "04C-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-11" "X" "3" "21" "2" "05C-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-11" "X" "3" "24" "4" "06C-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-11" "X" "3" "29" "3" "07C-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-111" "N" "3" "33" "48" "08C-11" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-111" BY REFERENCE N-24 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-12" " " "4" "0" "65" "C-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-12" "X" "4" "7" "1" " " "C-12" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-12" "X" "4" "9" "12" "01C-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-12" "X" "4" "29" "4" "02C-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-121" "N" "4" "33" "48" "03C-12" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-121" BY REFERENCE N-24 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-13" " " "22" "0" "30" "C-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-13" "X" "22" "7" "1" " " "C-13" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-13" "X" "22" "9" "12" "01C-13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-131" " " "22" "0" "9" "02C-13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-131" "X" "22" "31" "1" " " "C-131" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-131" "X" "22" "33" "6" "01C-131" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-131" "X" "22" "40" "2" "02C-131" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-13" "X" "22" "73" "8" "C-131" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-14" " " "23" "0" "4" "C-13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-14" "X" "23" "7" "4" " " "C-14" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-15" "X" "21" "73" "9" "C-14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-16" "X" "24" "60" "1" "C-15" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-02" " " "0" "0" "134" "CLE-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-21" " " "LIN2" "0" "68" " " "CLE-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLE-21" "X" "LIN2" "2" "7" " " "CLE-21" RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLE-21" "X" "LIN2" "9" "1" "01CLE-21" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLE-21" "X" "LIN2" "11" "24" "02CLE-21" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLE-21" "X" "LIN2" "35" "24" "03CLE-21" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05CLE-21" "X" "LIN2" "69" "12" "04CLE-21" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-22" " " "LIN2 PLUS 1" "0" "66" "CLE-21" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLE-22" "X" "LIN2 PLUS 1" "2" "7" " " "CLE-22"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLE-22" "X" "LIN2 PLUS 1" "11" "1" "01CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLE-22" "X" "LIN2 PLUS 1" "13" "5" "02CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLE-22" "X" "LIN2 PLUS 1" "19" "5" "03CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05CLE-22" "X" "LIN2 PLUS 1" "25" "5" "04CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06CLE-22" "X" "LIN2 PLUS 1" "31" "5" "05CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07CLE-22" "X" "LIN2 PLUS 1" "37" "5" "06CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08CLE-22" "X" "LIN2 PLUS 1" "43" "5" "07CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09CLE-22" "X" "LIN2 PLUS 1" "49" "5" "08CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10CLE-22" "X" "LIN2 PLUS 1" "55" "5" "09CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "11CLE-22" "X" "LIN2 PLUS 1" "61" "5" "10CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "12CLE-22" "X" "LIN2 PLUS 1" "67" "5" "11CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "13CLE-22" "X" "LIN2 PLUS 1" "73" "8" "12CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-03" "X" "3" "2" "6" "CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-04" " " "0" "0" "15" "CLE-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-41" "X" "3" "24" "8" " " "CLE-04" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-42" "X" "LIN1 PLUS 1" "2" "7" "C-41" " "
            RETURNING RESU.
      *DSP-REV-AREA
       CALL "SD_Init" USING 
            "DSP-REV-AREA" " " "0" "0" "488" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR-ALL" " " "0" "0" "244" " " "DSP-REV-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR1" "RX" "6" "11" "61" " " "DSP-RR-ALL"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR1" BY REFERENCE REV-1 "61" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR2" "RX" "7" "11" "61" "DSP-RR1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR2" BY REFERENCE REV-2 "61" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR3" "RX" "8" "11" "61" "DSP-RR2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR3" BY REFERENCE REV-3 "61" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR4" "RX" "9" "11" "61" "DSP-RR3" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR4" BY REFERENCE REV-4 "61" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC-ALL" " " "0" "0" "244" "DSP-RR-ALL" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC1" "X" "6" "11" "61" " " "DSP-RC-ALL"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RC1" BY REFERENCE REV-1 "61" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC2" "X" "7" "11" "61" "DSP-RC1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RC2" BY REFERENCE REV-2 "61" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC3" "X" "8" "11" "61" "DSP-RC2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RC3" BY REFERENCE REV-3 "61" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC4" "X" "9" "11" "61" "DSP-RC3" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RC4" BY REFERENCE REV-4 "61" "0" RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "466" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-T01" "X" "24" "1" "30" " " "DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M02" "X" "24" "1" "32" "INV-T01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M03" "X" "24" "1" "30" "INV-M02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-05" "X" "24" "1" "22" "INV-M03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-06" "X" "24" "1" "14" "ERR-05" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-08" "X" "24" "1" "24" "ERR-06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-13" "X" "24" "1" "26" "ERR-08" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-13A" "N" "24" "5" "6" "ERR-13" " " RETURNING RESU.
       CALL "SD_From" USING 
            "ERR-13A" BY REFERENCE W-94 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-14" "X" "24" "1" "26" "ERR-13A" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-HIM" "X" "24" "1" "40" "ERR-14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-SYK" "X" "24" "1" "18" "ERR-HIM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-SET" "X" "24" "1" "26" "ERR-SYK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-KAK" "X" "24" "1" "18" "ERR-SET" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-GAI" "X" "24" "1" "28" "ERR-KAK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-KSUMI" "X" "24" "1" "24" "ERR-GAI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-TORER" "X" "24" "1" "18" "ERR-KSUMI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-LST" "X" "24" "1" "26" "ERR-TORER" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-LST2" "X" "24" "1" "18" "ERR-LST" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-NGP" "X" "24" "1" "14" "ERR-LST2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-READ" "X" "24" "1" "26" "ERR-NGP" " " RETURNING RESU.
      *DISP-ERR-AREA
       CALL "SD_Init" USING 
            "DISP-ERR-AREA" " " "24" "0" "341" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-MSG-01" " " "24" "0" "60" " " "DISP-ERR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-01" "X" "24" "1" "60" " " "DISP-MSG-01"
            RETURNING RESU.
       CALL "SD_From" USING 
          "01DISP-MSG-01" BY REFERENCE ERR-MSGX "60" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-MSG-SPACE" " " "24" "0" "40" "DISP-MSG-01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-SPACE" "X" "24" "1" "40" " " "DISP-MSG-SPACE"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DISP-MSG-SPACE" BY REFERENCE ERR-SPACE "60" "0"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-BUZ-B" " " "24" "0" "5" "DISP-MSG-SPACE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-BUZ-B" "X" "24" "80" "5" " " "DISP-BUZ-B"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-BUZ-J" " " "24" "0" "5" "DISP-BUZ-B" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-BUZ-J" "X" "24" "80" "5" " " "DISP-BUZ-J"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "NOR-M01" " " "24" "0" "22" "DISP-BUZ-J" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01NOR-M01" "X" "24" "1" "22" " " "NOR-M01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "NOR-D01" " " "24" "0" "22" "NOR-M01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01NOR-D01" "X" "24" "1" "22" " " "NOR-D01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M01" " " "24" "0" "22" "NOR-D01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01INV-M01" "X" "24" "1" "22" " " "INV-M01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-D01" " " "24" "0" "22" "INV-M01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01INV-D01" "X" "24" "1" "22" " " "INV-D01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "OK-01" " " "24" "0" "14" "INV-D01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01OK-01" "X" "24" "1" "14" " " "OK-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "CAN-01" " " "24" "0" "18" "OK-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CAN-01" "X" "24" "1" "18" " " "CAN-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-01" " " "24" "0" "18" "CAN-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-01" "X" "24" "1" "18" " " "ERR-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-02" " " "24" "0" "22" "ERR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-02" "X" "24" "1" "22" " " "ERR-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-DIS" " " "24" "0" "71" "ERR-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-DIS" "X" "24" "2" "5" " " "ERR-DIS" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ERR-DIS" "X" "24" "7" "12" "01ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02ERR-DIS" BY REFERENCE ERR-F "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03ERR-DIS" "X" "24" "19" "1" "02ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03ERR-DIS" BY REFERENCE ERR-M "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04ERR-DIS" "X" "24" "20" "11" "03ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05ERR-DIS" "X" "24" "31" "2" "04ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05ERR-DIS" BY REFERENCE ERR-FLG "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06ERR-DIS" "X" "24" "33" "5" "05ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07ERR-DIS" "X" "24" "38" "5" "06ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08ERR-DIS" "X" "24" "43" "30" "07ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08ERR-DIS" BY REFERENCE ERR-K "30" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MEIN.
           PERFORM  INI-RTN    THRU  INI-EX.
           PERFORM  ACT-RTN    THRU  ACT-EX.
           PERFORM  KBS-RTN    THRU  KBS-EX.
       MR999.
           PERFORM  END-RTN    THRU  END-EX.
           CALL "DB_Close".
           STOP     RUN.
      ******************************
      *    ＩＮＩ－ＲＴＮ          *
      *          ～初期処理～      *
      ******************************
       INI-RTN.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
       INI-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-SIGN "ACP-SIGN"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "P9"
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INI-010
           END-IF
           CALL "SD_Output" USING
            "ACP-SIGN" ACP-SIGN "p" RETURNING RESU.
           IF  JS-SIGN NOT =  0  AND 1
               GO  TO  INI-010
           END-IF.
       INI-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  INI-010
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INI-020
           END-IF
           CALL "SD_Output" USING "ACP-OKC" ACP-OKC "p" RETURNING RESU.
           IF  W-OKC  NOT =  1 AND 9
               GO  TO  INI-020
           END-IF
           IF  W-OKC      =  9
               CALL "DB_Close"
               STOP  RUN
           END-IF
      *
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ590I" RETURNING RESU.
           IF  JS-SIGN  =  0
               CALL "SD_Output" USING "DSP-S1" DSP-S1 "p" RETURNING RESU
           END-IF
           IF  JS-SIGN  =  1
               CALL "SD_Output" USING "DSP-S2" DSP-S2 "p" RETURNING RESU
           END-IF
           CALL     "CBLSTNNO"     USING  STN-NO USER_ID.
           ACCEPT   WYMD       FROM  DATE.
      ***
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "I-O" JSJD_PNAME1 "SHARED" BY REFERENCE JSJD_IDLST "2"
            "JSJD-KEY" BY REFERENCE JSJD-KEY "JSJD-KEY2" BY REFERENCE
            JSJD-KEY2.
           CALL "DB_F_Open" USING
            "I-O" JMSTD_PNAME1 "SHARED" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2"
            BY REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE
            JMSTD-KEY3.
           CALL "DB_F_Open" USING
            "I-O" JT-DNKN_PNAME1 "SHARED" BY REFERENCE JT-DNKN_IDLST "1"
            "DNKN-KEY" BY REFERENCE DNKN-KEY.
           CALL "DB_F_Open" USING
            "I-O" JNSR_PNAME1 "SHARED" BY REFERENCE JNSR_IDLST "3"
            "JNSR-KEY1" BY REFERENCE JNSR-KEY1 "JNSR-KEY2" BY REFERENCE
            JNSR-KEY2 "JNSR-KEY3" BY REFERENCE JNSR-KEY3.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           COPY  LIBCPR.
      *
           MOVE     SPACE      TO    ACT-WORK1  ACT-WORK2  W-AREA2.
           INITIALIZE                ACT-WORK1  ACT-WORK2  W-AREA2.
           MOVE     24         TO    ERR-LIN.
           CALL "SD_Arg_Match_Line" USING
            "ERR-LIN" "2" ERR-LIN RETURNING RESU.
       INI-EX.
           EXIT.
      ******************************
      *    ＥＮＤ－ＲＴＮ          *
      *          ～終了処理～      *
      ******************************
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JNSR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JSJD_IDLST JSJD_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-DNKN_IDLST JT-DNKN_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JNSR_IDLST JNSR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
       END-EX.
           EXIT.
      *************************************
      *    ＡＣＴ－ＲＴＮ                 *
      *          ～画面入力＊更新処理～   *
      *************************************
       ACT-RTN.
           CALL "SD_Accept" USING BY REFERENCE ACP-ACT "ACP-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "P9"
               GO  TO  ACT-EX
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-RTN
           END-IF
           CALL "SD_Output" USING "ACP-ACT" ACP-ACT "p" RETURNING RESU.
           IF  W-ACT  NOT =  2  AND  3
               GO  TO  ACT-RTN
           END-IF
           IF  W-ACT      =  2
               MOVE     "訂正"   TO    W-NAM
           END-IF
           IF  W-ACT      =  3
               MOVE     "取消"   TO    W-NAM
           END-IF
           CALL "SD_Output" USING "DSP-NAM" DSP-NAM "p" RETURNING RESU.
       ACT-005.
           MOVE     1          TO    A.
           MOVE     10         TO    LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           PERFORM  CR1-RTN    THRU  CR1-EX.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           INITIALIZE          ACT-WORK2.
       ACT-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-1 "ACP-1" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-RTN
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-010
           END-IF
           CALL "SD_Output" USING "ACP-1" ACP-1 "p" RETURNING RESU.
           MOVE     W1-1       TO    W1-1D.
           IF   W1-1D      =  2
                MOVE     1          TO    W1-1D
           END-IF
           IF   W1-1D NOT  =  JS-SIGN
                CALL "SD_Output" USING
                 "ERR-01" ERR-01 "p" RETURNING RESU
                GO  TO     ACT-010
           END-IF
           MOVE     0          TO    DIS-SW.
           MOVE     10         TO    LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           MOVE     W-1        TO    JSTR-01.
           MOVE     1          TO    JSTR-02  A  CNT.
      *           START    JSTR KEY   NOT <  JSTR-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            JSTR_PNAME1 "JSTR-KEY" " NOT < " JSTR-KEY RETURNING RET.
           IF  RET = 1
               GO  TO   ACT-014
           END-IF
           MOVE     ZERO       TO    SASIZU-SURYO.
       ACT-012.
      *           READ     JSTR       NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               ADD   1       TO  A
               GO  TO  ACT-014
           END-IF
           IF  W-1    NOT =  JSTR-01
               ADD   1       TO  A
               GO  TO  ACT-014
           END-IF
           MOVE  JSTR-02     TO  A.
           IF  DIS-SW NOT =  0
               GO  TO  ACT-013
           END-IF
           IF  JSTR-158   =  0
               CALL "SD_Output" USING
                "ERR-LST2" ERR-LST2 "p" RETURNING RESU
               GO  TO  ACT-010
           END-IF
           IF  JSTR-17    =  0
               CALL "SD_Output" USING
                "ERR-KAK" ERR-KAK "p" RETURNING RESU
               GO  TO  ACT-010
           END-IF
           IF  W-ACT      =  3
               IF  JSTR-17    NOT =  1
                   CALL "SD_Output" USING
                    "ERR-TORER" ERR-TORER "p" RETURNING RESU
                   GO  TO  ACT-010
               ELSE
                   GO  TO  ACT-012A
               END-IF
           END-IF
      ***** 処理部署＝日進の時
           IF  JSTR-19    =  SPACE
               IF  JSTR-17    NOT =  8  AND  9
                   CALL "SD_Output" USING
                    "ERR-KSUMI" ERR-KSUMI "p" RETURNING RESU
                   GO  TO  ACT-010
               ELSE
                   GO  TO  ACT-012A
               END-IF
           END-IF
      ***** 処理部署＝新岡の時
           IF  JSTR-17    NOT =  8  AND  9
               CALL "SD_Output" USING
                "ERR-KSUMI" ERR-KSUMI "p" RETURNING RESU
               GO  TO  ACT-010
           END-IF
           MOVE  1          TO  INV-SW.
           MOVE  JSTR-01    TO  JSJD-03.
           MOVE  0          TO  JSJD-04.
      *           START  JSJD KEY NOT < JSJD-KEY2 INVALID
      *///////////////
           CALL "DB_Start" USING
            JSJD_PNAME1 "JSJD-KEY2" " NOT < " JSJD-KEY2 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "ERR-KAK" ERR-KAK "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACT-010
           END-IF
      *           READ   JSJD NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSJD_PNAME1 BY REFERENCE JSJD-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "ERR-KAK" ERR-KAK "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACT-010
           END-IF
           IF  JSJD-03  NOT  =  JSTR-01
               CALL "SD_Output" USING
                "ERR-KAK" ERR-KAK "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACT-010
           END-IF
           MOVE   0    TO  INV-SW.
           IF  JSJD-158 = 0
               CALL "SD_Output" USING
                "ERR-LST" ERR-LST "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACT-010
           END-IF.
       ACT-012A.
           PERFORM  CR1-RTN    THRU  CR1-EX.
           MOVE     JSTR-14A   TO    W-5A.
           MOVE     JSTR-14D   TO    W-8A.
           MOVE     JSTR-15    TO    W-8.
           MOVE     JSTR-03    TO    W-21.
           MOVE     JSTR-14B   TO    W-92.
           MOVE     JSTR-14C   TO    W-93.
           MOVE     JSTR-05    TO    W-3.
           IF  W-21       =  0
               MOVE     "出荷"   TO    W-22
           END-IF
           IF  W-21       =  3
               MOVE     "訂正"   TO    W-22
           END-IF
           IF  W-21       =  5
               MOVE     "返品"   TO    W-22
           END-IF
           IF  W-21       =  6
               MOVE     "不良"   TO    W-22
           END-IF
           MOVE     JSTR-061   TO    W-41  TC-TCD.
           MOVE     1          TO    TC-CCD.
      *           READ     TC-M       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    TC-NAME
           END-IF
           MOVE     TC-NAME    TO    W-43.
           MOVE     JSTR-062   TO    W-42  TC-CCD.
      *           READ     TC-M       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    TC-NAME
           END-IF
           MOVE     TC-NAME    TO    W-44.
           MOVE     3          TO    JCON3-01.
           MOVE     JSTR-07    TO    W-51  JCON3-02  O-SCD.
      *           READ     JCON       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    JCON3-03
           END-IF
           MOVE     JCON3-03   TO    W-52.
           MOVE     2          TO    JCON2-01.
           MOVE     JSTR-14    TO    W-71  JCON2-02.
      *           READ     JCON       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    JCON2-03
           END-IF
           MOVE     JCON2-03   TO    W-72.
           MOVE     JSTR-15A   TO    W-KOS.
           IF  TC-BIK          =  ZERO
               CALL "SD_Output" USING
                "DSP-BIKHO-CLE" DSP-BIKHO-CLE "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "DSP-BIKHO" DSP-BIKHO "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "ACP-2" ACP-2 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-3" ACP-3 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-41" A-41 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-42" A-42 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-43" D-43 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-44" D-44 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-5" ACP-5 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-54" D-54 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-KOS" DSP-KOS "p" RETURNING RESU.
           MOVE  1     TO  DIS-SW.
       ACT-013.
           MOVE  JSTR-1111(01)    TO    S-S (A 01).
           MOVE  JSTR-1111(02)    TO    S-S (A 02).
           MOVE  JSTR-1111(03)    TO    S-S (A 03).
           MOVE  JSTR-1111(04)    TO    S-S (A 04).
           MOVE  JSTR-1111(05)    TO    S-S (A 05).
           MOVE  JSTR-1111(06)    TO    S-S (A 06).
           MOVE  JSTR-1111(07)    TO    S-S (A 07).
           MOVE  JSTR-1111(08)    TO    S-S (A 08).
           MOVE  JSTR-1111(09)    TO    S-S (A 09).
           MOVE  JSTR-1111(10)    TO    S-S (A 10).
           MOVE  JSTR-121 (01)    TO    W-64(A 01)  O-SU(A 01).
           MOVE  JSTR-121 (02)    TO    W-64(A 02)  O-SU(A 02).
           MOVE  JSTR-121 (03)    TO    W-64(A 03)  O-SU(A 03).
           MOVE  JSTR-121 (04)    TO    W-64(A 04)  O-SU(A 04).
           MOVE  JSTR-121 (05)    TO    W-64(A 05)  O-SU(A 05).
           MOVE  JSTR-121 (06)    TO    W-64(A 06)  O-SU(A 06).
           MOVE  JSTR-121 (07)    TO    W-64(A 07)  O-SU(A 07).
           MOVE  JSTR-121 (08)    TO    W-64(A 08)  O-SU(A 08).
           MOVE  JSTR-121 (09)    TO    W-64(A 09)  O-SU(A 09).
           MOVE  JSTR-121 (10)    TO    W-64(A 10)  O-SU(A 10).
           MOVE  JSTR-122         TO    W-65(A).
           MOVE  JSTR-08          TO    W-61(A).
           MOVE  JSTR-09          TO    W-621(A)  HI-MHCD  HI-HCD
                                                  S-HCD(A) O-HCD(A).
           MOVE  JSTR-10          TO    W-63(A)   O-SIZ(A).
      *           READ  HI2-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE    SPACE     TO    HI-NAME
           END-IF
           MOVE  HI-NAME          TO    W-622(A).
           MOVE     HI-ISU     TO    W-KA(A).
           MOVE  JSTR-13          TO    W-66(A).
           MOVE  JSTR-20          TO    W-67(A).
           IF  JSTR-04            NOT =  ZERO
               MOVE  1                TO    W-999(A)
           END-IF.
       ACT-013A.
           IF  CNT NOT = A
               ADD   1   TO  CNT
               ADD   2   TO  LIN1
               CALL "SD_Arg_Match_Line" USING
                "LIN1" "2" LIN1 RETURNING RESU
               GO  TO  ACT-013A
           END-IF
           IF  W-61(A)        =  ZERO
               CALL "SD_Output" USING "A-612A" A-612A "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-611" A-611 "p" RETURNING RESU
               CALL "SD_Output" USING "A-612" A-612 "p" RETURNING RESU
               CALL "SD_Output" USING "A-613" A-613 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-622" D-622 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-65" D-65 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-621" A-621 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-63" A-63 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-6B" DSP-6B "p" RETURNING RESU.
           CALL "SD_Output" USING "A-67" A-67 "p" RETURNING RESU.
           MOVE     A          TO    O.
           ADD      1          TO    CNT.
           ADD      2          TO    LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           GO  TO   ACT-012.
       ACT-014.
           IF  CNT        =  1
               CALL "SD_Output" USING
                "INV-T01" INV-T01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO   ACT-010
           END-IF
           MOVE  ZERO          TO  W-KEI.
           IF  W-6211(1)  NOT =  9999
               ADD   W-65(1)   TO  W-KEI
           END-IF
           IF  W-6211(2)  NOT =  9999
               ADD   W-65(2)   TO  W-KEI
           END-IF
           IF  W-6211(3)  NOT =  9999
               ADD   W-65(3)   TO  W-KEI
           END-IF
           IF  W-6211(4)  NOT =  9999
               ADD   W-65(4)   TO  W-KEI
           END-IF
           IF  W-6211(5)  NOT =  9999
               ADD   W-65(5)   TO  W-KEI
           END-IF
           IF  W-6211(6)  NOT =  9999
               ADD   W-65(6)   TO  W-KEI
           END-IF
           CALL "SD_Output" USING "DSP-KEI" DSP-KEI "p" RETURNING RESU.
           IF  W-ACT      =  3
               GO  TO  ACT-150
           END-IF
           MOVE 0 TO CHK.
       ACT-030.
           CALL "SD_Accept" USING BY REFERENCE A-31 "A-31" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-010
           END-IF
           IF  ESTAT  NOT =  "00" AND "01" AND "06"
               GO  TO  ACT-030
           END-IF
           CALL "SD_Output" USING "A-31" A-31 "p" RETURNING RESU.
       ACT-031.
           CALL "SD_Accept" USING BY REFERENCE A-32 "A-32" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-030
           END-IF
           IF  ESTAT  NOT =  "00" AND "01" AND "06"
               GO  TO  ACT-031
           END-IF
           CALL "SD_Output" USING "A-32" A-32 "p" RETURNING RESU.
           IF  (W-32  <  1)  OR  (W-32  >  12)
               GO  TO  ACT-031
           END-IF.
       ACT-032.
           CALL "SD_Accept" USING BY REFERENCE A-33 "A-33" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-031
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-032
           END-IF
           CALL "SD_Output" USING "A-33" A-33 "p" RETURNING RESU.
           IF  (W-33  <  1)  OR  (W-33  >  31)
               GO  TO  ACT-032
           END-IF
      *
           MOVE  ZERO     TO    W-311.
           IF  W-312 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-31
           END-IF
           IF  W-312 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-31
           END-IF
           MOVE  ZERO     TO    W-NGP.
           ACCEPT  W-NGPS FROM  DATE.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-NGP     =  W-3
               GO  TO  ACT-055
           END-IF
           IF  W-NGP     <  W-3
               GO  TO  ACT-035
           END-IF
           SUBTRACT  1   FROM  W-GET.
           IF  W-GET      =  ZERO
               SUBTRACT  1   FROM W-NEN
               MOVE  12      TO   W-GET
           END-IF
           IF  W-NGP      >  W-3
               CALL "SD_Output" USING
                "ERR-NGP" ERR-NGP "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               ADD 1 TO CHK
               IF  CHK < 1
                   GO  TO  ACT-030
               END-IF
           END-IF
           GO  TO  ACT-055.
       ACT-035.
           ADD   1       TO  W-GET.
           IF  W-GET      =  13
               ADD   1       TO  W-NEN
               MOVE  1       TO  W-GET
           END-IF
           IF  W-NGP      <  W-3
               CALL "SD_Output" USING
                "ERR-NGP" ERR-NGP "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               ADD 1 TO CHK
               IF  CHK < 1
                   GO  TO  ACT-030
               END-IF
           END-IF.
       ACT-055.
           CALL "SD_Accept" USING BY REFERENCE A-53 "A-53" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-030
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-055
           END-IF
           CALL "SD_Output" USING "D-54" D-54 "p" RETURNING RESU.
      *
           MOVE     10         TO    LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           MOVE     1          TO    A  B  C.
       ACT-061.
           MOVE     W-621(A)   TO    HI-MHCD HI-HCD.
      *           READ     HI2-M      UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               INITIALIZE       HI-R
           END-IF
           IF  W-61(A)      NOT =    ZERO
               GO  TO  ACT-086
           END-IF.
       ACT-080.
           CALL "SD_Accept" USING BY REFERENCE A-621 "A-621" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-081
           END-IF
           IF  (ESTAT = "04")  AND  (W-999(A) NOT =  1)
               PERFORM  CR1-RTN    THRU  CR1-EX
               GO  TO   ACT-120
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-080
           END-IF
           CALL "SD_Output" USING "A-621" A-621 "p" RETURNING RESU.
           GO  TO  ACT-082.
       ACT-081.
           IF  A           =     1
               GO  TO  ACT-055
           END-IF
           SUBTRACT   1       FROM  A.
           SUBTRACT   2       FROM  LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           IF  W-61(A)     NOT =    ZERO
               GO  TO  ACT-081
           END-IF
           GO  TO  ACT-080.
       ACT-082.
           MOVE     W-621(A)   TO    HI-MHCD HI-HCD.
      *           READ     HI2-M      UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-M02" INV-M02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO   ACT-080
           END-IF
           IF  W-621(A)  <  999900
               IF  HI-ISU  =    ZERO
                   CALL "SD_Output" USING
                    "ERR-14" ERR-14 "p" RETURNING RESU
                   GO  TO   ACT-080
               END-IF
           END-IF
           MOVE     HI-ISU     TO    W-KA(A).
           IF  W-5A    =    ZERO
               IF  W-KA(A)  NOT   =    W-KA(1)
                   CALL "SD_Output" USING
                    "ERR-08" ERR-08 "p" RETURNING RESU
               END-IF
           END-IF
           MOVE     HI-NAME    TO    W-622(A).
           MOVE     HI-BC3     TO    W-BUN.
           CALL "SD_Output" USING "D-622" D-622 "p" RETURNING RESU.
           IF  JS-SIGN   =   0
               IF  W-B1  NOT  =  3
                   CALL "SD_Output" USING
                    "ERR-01" ERR-01 "p" RETURNING RESU
                   GO  TO  ACT-080
               END-IF
           END-IF
           IF  JS-SIGN   =   1
               IF  W-B1      =  3
                   IF  W-1      <  200000
                       CALL "SD_Output" USING
                        "ERR-01" ERR-01 "p" RETURNING RESU
                       GO  TO  ACT-080
                   END-IF
               END-IF
           END-IF.
       ACT-085.
           CALL "SD_Accept" USING BY REFERENCE A-63 "A-63" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               PERFORM  REV-CLE-RTN  THRU  REV-CLE-EXT
               GO  TO  ACT-080
           END-IF
           IF  ESTAT  NOT  =  "01"  AND  "06"
               GO  TO  ACT-085
           END-IF
           CALL "SD_Output" USING "A-63" A-63 "p" RETURNING RESU.
           IF  W-63(A)  NOT  =  1  AND  2  AND  3  AND  4
               PERFORM  REV-CLE-RTN  THRU  REV-CLE-EXT
               GO  TO  ACT-085
           END-IF.
       ACT-086.
           PERFORM  REV-DSP-RTN  THRU  REV-DSP-EXT.
       ACT-090.
           MOVE     0          TO    INP-SW.
           MOVE     1          TO    B.
           MOVE     13         TO    COL1.
           CALL "SD_Arg_Match_Col" USING "COL1" "2" COL1 RETURNING RESU.
       ACT-091.
           IF  W-63(A)    =  1
               IF  HI-S1(B)     =    0
                   MOVE     ZERO       TO    W-64(A , B)
                   CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU
                   GO  TO   ACT-092
               END-IF
           END-IF
           IF  W-63(A)    =  2
               IF  HI-S2(B)     =    0
                   MOVE     ZERO       TO    W-64(A , B)
                   CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU
                   GO  TO   ACT-092
               END-IF
           END-IF
           IF  W-63(A)    =  3
               IF  HI-S3(B)     =    0
                   MOVE     ZERO       TO    W-64(A , B)
                   CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU
                   GO  TO   ACT-092
               END-IF
           END-IF
           IF  W-63(A)    =  4
               IF  HI-S4(B)     =    0
                   MOVE     ZERO       TO    W-64(A , B)
                   CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU
                   GO  TO   ACT-092
               END-IF
           END-IF
           IF  (W-63(A)    =  4)  AND  (B   =   10)
                MOVE     ZERO       TO    W-64(A , B)
                CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU
                GO  TO   ACT-092
           END-IF
           MOVE  1     TO  INP-SW.
           CALL "SD_Accept" USING BY REFERENCE A-64 "A-64" "S9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  (ESTAT  =  "09")  AND  (B  =  1)
               IF  W-611(A)  =  0
                   GO  TO  ACT-085
               ELSE
                   IF  A     =    1
                       GO  TO  ACT-055
                   ELSE
                       SUBTRACT   1       FROM  A
                       SUBTRACT   2       FROM  LIN1
                       CALL "SD_Arg_Match_Line" USING
                        "LIN1" "2" LIN1 RETURNING RESU
                       GO  TO  ACT-090
                   END-IF
               END-IF
           END-IF
           IF  ESTAT      =  "09"
               GO  TO  ACT-093
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-091
           END-IF
           CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU.
           IF  B          >  7
               IF  W-64(A,B)       >  999  OR  <  -999
                   GO  TO  ACT-091
               END-IF
           END-IF.
       ACT-092.
           IF  B      NOT =  10
               ADD      1          TO    B
               ADD      6          TO    COL1
               CALL "SD_Arg_Match_Col" USING
                "COL1" "2" COL1 RETURNING RESU
               GO  TO   ACT-091
           END-IF
           GO  TO   ACT-100.
       ACT-093.
           SUBTRACT 1          FROM  B.
           SUBTRACT 6          FROM  COL1.
           CALL "SD_Arg_Match_Col" USING "COL1" "2" COL1 RETURNING RESU.
           IF  B          =  ZERO
               IF  W-61(A)    =  ZERO
                   GO  TO  ACT-085
               ELSE
                   IF  A     =    1
                       GO  TO  ACT-055
                   ELSE
                       SUBTRACT   1       FROM  A
                       SUBTRACT   2       FROM  LIN1
                       CALL "SD_Arg_Match_Line" USING
                        "LIN1" "2" LIN1 RETURNING RESU
                       GO  TO  ACT-090
                   END-IF
               END-IF
           END-IF
           IF  W-63(A)    =  1
               IF  HI-S1(B)     =    0
                   GO  TO  ACT-093
               END-IF
           END-IF
           IF  W-63(A)    =  2
               IF  HI-S2(B)     =    0
                   GO  TO  ACT-093
               END-IF
           END-IF
           IF  W-63(A)    =  3
               IF  HI-S3(B)     =    0
                   GO  TO  ACT-093
               END-IF
           END-IF
           IF  W-63(A)    =  4
               IF  HI-S4(B)     =    0
                   GO  TO  ACT-093
               END-IF
           END-IF
           GO  TO   ACT-091.
       ACT-100.
           COMPUTE  W-65(A) =  W-64(A , 1) + W-64(A , 2) + W-64(A , 3)
               + W-64(A , 4) + W-64(A , 5) + W-64(A , 6) + W-64(A , 7)
               + W-64(A , 8) + W-64(A , 9) + W-64(A , 10).
           CALL "SD_Output" USING "D-65" D-65 "p" RETURNING RESU.
       ACT-105.
           IF  TC-BIK    =   ZERO
               MOVE  SPACE       TO  W-67(A)
               CALL "SD_Output" USING "A-67" A-67 "p" RETURNING RESU
               GO  TO  ACT-106
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-67 "A-67" "X" "10"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO  TO  ACT-093
           END-IF
           IF  ESTAT   NOT =  "01"  AND  "06"
               GO  TO  ACT-105
           END-IF.
       ACT-106.
           IF  W-5A      =   ZERO
               GO  TO  ACT-110
           END-IF
           DIVIDE   W-5A      INTO  W-65(A)
                    GIVING  WA      REMAINDER  WB.
           IF  WB    NOT =   ZERO
               CALL "SD_Output" USING
                "ERR-SET" ERR-SET "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
       ACT-110.
           PERFORM  REV-CLE-RTN  THRU  REV-CLE-EXT.
           ADD      1          TO    A.
           ADD      2          TO    LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           IF  A          >  6
               GO  TO  ACT-120
           END-IF
           GO  TO  ACT-061.
       ACT-120.
           MOVE  ZERO          TO  W-KEI.
           IF  W-6211(1)  NOT =  9999
               ADD   W-65(1)   TO  W-KEI
           END-IF
           IF  W-6211(2)  NOT =  9999
               ADD   W-65(2)   TO  W-KEI
           END-IF
           IF  W-6211(3)  NOT =  9999
               ADD   W-65(3)   TO  W-KEI
           END-IF
           IF  W-6211(4)  NOT =  9999
               ADD   W-65(4)   TO  W-KEI
           END-IF
           IF  W-6211(5)  NOT =  9999
               ADD   W-65(5)   TO  W-KEI
           END-IF
           IF  W-6211(6)  NOT =  9999
               ADD   W-65(6)   TO  W-KEI
           END-IF
           CALL "SD_Output" USING "DSP-KEI" DSP-KEI "p" RETURNING RESU.
       ACT-130.
           CALL "SD_Accept" USING BY REFERENCE ACP-KOS "ACP-KOS"
            "S9" "3" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-081
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-130
           END-IF
           CALL "SD_Output" USING "DSP-KOS" DSP-KOS "p" RETURNING RESU.
       ACT-150.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  (ESTAT  =  "09")  AND  (W-ACT  =  3)
               GO  TO  ACT-010
           END-IF
           IF  (ESTAT  =  "09")  AND  (W-3  =  ZERO)
               GO  TO  ACT-030
           END-IF
           IF  ESTAT      =  "09"
               GO  TO  ACT-130
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-150
           END-IF
           CALL "SD_Output" USING "ACP-OKC" ACP-OKC "p" RETURNING RESU.
           IF  W-OKC  NOT =  1 AND 9
               GO  TO  ACT-150
           END-IF
           IF  W-OKC      =  9
               CALL "SD_Output" USING "CAN-01" CAN-01 "p" RETURNING RESU
               GO  TO   ACT-160
           END-IF.
       ACT-155.
           PERFORM  UPD-RTN    THRU  UPD-EX.
           CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU.
       ACT-160.
           GO  TO   ACT-005.
       ACT-EX.
           EXIT.
      *************************************
      *    ＵＰＤ－ＲＴＮ                 *
      *          ～ファイル更新処理～     *
      *************************************
       UPD-RTN.
           IF  W-3         =    ZERO
               INITIALIZE       W-64A(1) W-64A(2) W-64A(3) W-64A(4)
                                W-64A(5) W-64A(6) W-65 (1) W-65 (2)
                                W-65 (3) W-65 (4) W-65 (5) W-65 (6)
                                W-KOS
           END-IF.
      ***
       UPD-010.
      *****出荷指図トラン　更新
           IF  W-ACT   NOT =    3
               GO  TO  UPD-020
           END-IF
           MOVE     W-1        TO    JSTR-01.
           MOVE     1          TO    JSTR-02.
      *           START    JSTR  KEY   NOT <  JSTR-KEY   INVALID
      *///////////////
           CALL "DB_Start" USING
            JSTR_PNAME1 "JSTR-KEY" " NOT < " JSTR-KEY RETURNING RET.
           IF  RET = 1
               GO  TO   UPD-030
           END-IF.
       UPD-015.
      *           READ     JSTR       NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO   UPD-030
           END-IF
           IF  JSTR-01     NOT =   W-1
               GO  TO  UPD-030
           END-IF
           MOVE  JSTR-02       TO  C.
           IF  JSTR-04         =   ZERO
               PERFORM  DNDEL-RTN  THRU  DNDEL-EX
               PERFORM  DEL-RTN    THRU  DEL-EX
           ELSE
               MOVE     STN-NO-02  TO    JSTR-4021
               MOVE     0          TO    JSTR-4022
               MOVE     W-ACT      TO    JSTR-4023
               MOVE     9          TO  JSTR-17
               PERFORM  REW-RTN    THRU  REW-EX
               PERFORM  JNSR-DEL-RTN   THRU  JNSR-DEL-EX
           END-IF
           GO  TO  UPD-015.
       UPD-020.
           PERFORM  DNDEL-RTN  THRU  DNDEL-EX.
           PERFORM  DNWRI-RTN  THRU  DNWRI-EX.
           MOVE     ZERO       TO    C.
       UPD-025.
           ADD      1          TO    C.
           IF  C                =    A
               GO  TO  UPD-027
           END-IF
           MOVE     W-1        TO    JSTR-01.
           MOVE     C          TO    JSTR-02.
           MOVE     JSTR-KEY   TO    ERR-K.
      *           READ     JSTR       INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JSTR_PNAME1 BY REFERENCE JSTR-R " " RETURNING RET.
           IF  RET = 1
               MOVE  WJSTR-R    TO  JSTR-R
               MOVE  LOW-VALUE  TO  JSTR-KEY
           END-IF
           IF  W-999(C)     NOT =    1
               INITIALIZE          JSTR-11  JSTR-04  JSTR-08
           END-IF
           IF  W-3          NOT =    ZERO
               MOVE     W-3        TO    JSTR-05
           END-IF
           MOVE     W-64(C 1)  TO    JSTR-121(1).
           MOVE     W-64(C 2)  TO    JSTR-121(2).
           MOVE     W-64(C 3)  TO    JSTR-121(3).
           MOVE     W-64(C 4)  TO    JSTR-121(4).
           MOVE     W-64(C 5)  TO    JSTR-121(5).
           MOVE     W-64(C 6)  TO    JSTR-121(6).
           MOVE     W-64(C 7)  TO    JSTR-121(7).
           MOVE     W-64(C 8)  TO    JSTR-121(8).
           MOVE     W-64(C 9)  TO    JSTR-121(9).
           MOVE     W-64(C 10) TO    JSTR-121(10).
           MOVE     W-65(C)    TO    JSTR-122.
           MOVE     W-621(C)   TO    JSTR-09.
           MOVE     W-63(C)    TO    JSTR-10.
           MOVE     W-5A       TO    JSTR-14A.
           MOVE     W-KOS      TO    JSTR-15A.
           MOVE     STN-NO-02  TO    JSTR-4021.
           MOVE     0          TO    JSTR-4022.
           MOVE     W-ACT      TO    JSTR-4023.
           MOVE     8          TO    JSTR-17.
           MOVE     W-67(C)    TO    JSTR-20.
           IF  JSTR-KEY    =    LOW-VALUE
               GO  TO  UPD-026
           END-IF
           MOVE     JSTR-R     TO    WJSTR-R.
      *           REWRITE  JSTR-R     INVALID
      *///////////////
           CALL "DB_Update" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE     "JSTR"     TO    ERR-F
               MOVE     "R"        TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
           GO  TO   UPD-025.
       UPD-026.
           MOVE     W-1        TO    JSTR-01.
           MOVE     C          TO    JSTR-02.
      *           WRITE    JSTR-R     INVALID
      *//////////////
           CALL "DB_Insert" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE     "JSTR"     TO    ERR-F
               MOVE     "W"        TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
           GO  TO   UPD-025.
       UPD-027.
           IF  C     >     6
               GO  TO  UPD-030
           END-IF
           MOVE  W-1           TO    JSTR-01.
           MOVE  C             TO    JSTR-02.
      *           READ  JSTR          INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JSTR_PNAME1 BY REFERENCE JSTR-R " " RETURNING RET.
           IF  RET = 1
               ADD       1         TO    C
               GO  TO  UPD-027
           END-IF
      **
           PERFORM  DEL-RTN    THRU  DEL-EX.
           ADD   1             TO    C.
           GO  TO   UPD-027.
       UPD-030.
           IF  W-ACT  NOT =  3
               GO  TO  UPD-036
           END-IF
      *    出荷実績ファイル　更新   *
           MOVE     W-1        TO    JSJD-03.
           MOVE     1          TO    JSJD-04.
      *           START    JSJD  KEY   NOT <  JSJD-KEY2  INVALID
      *///////////////
           CALL "DB_Start" USING
            JSJD_PNAME1 "JSJD-KEY2" " NOT < " JSJD-KEY2 RETURNING RET.
           IF  RET = 1
               GO  TO   UPD-040
           END-IF.
       UPD-035.
      *           READ     JSJD       NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSJD_PNAME1 BY REFERENCE JSJD-REC " "
            RETURNING RET.
           IF  RET = 1
               GO  TO   UPD-040
           END-IF
           IF  JSJD-03     NOT =   W-1
               GO  TO  UPD-040
           END-IF
           MOVE  9             TO  JSJD-17.
      ***
           MOVE  JSJD-KEY2     TO  ERR-K.
      *           REWRITE  JSJD-REC   INVALID
      *///////////////
           CALL "DB_Update" USING
            JSJD_PNAME1 JSJD_LNAME JSJD-REC RETURNING RET.
           IF  RET = 1
               MOVE     "JSTD"     TO    ERR-F
               MOVE     "R"        TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
           GO  TO   UPD-035.
       UPD-036.
           MOVE     ZERO       TO    C.
       UPD-037.
           ADD      1          TO    C.
           IF  C                =    A
               GO  TO  UPD-039
           END-IF
           MOVE     W-1        TO    JSJD-03.
           MOVE     C          TO    JSJD-04.
           MOVE     JSJD-KEY2  TO    ERR-K.
      *           READ     JSJD       KEY IS JSJD-KEY2       INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JSJD_PNAME1 BY REFERENCE JSJD-REC " " BY REFERENCE
            JSJD-KEY2 RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-037
           END-IF
           IF  W-3          NOT =    ZERO
               MOVE     W-3        TO    JSJD-07
           END-IF
           MOVE     W-64(C 1)  TO    JSJD-1111(1).
           MOVE     W-64(C 2)  TO    JSJD-1111(2).
           MOVE     W-64(C 3)  TO    JSJD-1111(3).
           MOVE     W-64(C 4)  TO    JSJD-1111(4).
           MOVE     W-64(C 5)  TO    JSJD-1111(5).
           MOVE     W-64(C 6)  TO    JSJD-1111(6).
           MOVE     W-64(C 7)  TO    JSJD-1111(7).
           MOVE     W-64(C 8)  TO    JSJD-1111(8).
           MOVE     W-64(C 9)  TO    JSJD-1111(9).
           MOVE     W-64(C 10) TO    JSJD-1111(10).
           MOVE     W-65(C)    TO    JSJD-112.
           MOVE     W-621(C)   TO    JSJD-09.
           MOVE     W-63(C)    TO    JSJD-10.
           MOVE     W-5A       TO    JSJD-14A.
           MOVE     W-KOS      TO    JSJD-15A.
           MOVE     8          TO    JSJD-17.
           MOVE     W-67(C)    TO    JSJD-20.
      *           REWRITE  JSJD-REC   INVALID
      *///////////////
           CALL "DB_Update" USING
            JSJD_PNAME1 JSJD_LNAME JSJD-REC RETURNING RET.
           IF  RET = 1
               MOVE     "JSJD"     TO    ERR-F
               MOVE     "R"        TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
           GO  TO   UPD-037.
       UPD-039.
           IF  C     >     6
               GO  TO  UPD-080
           END-IF
           MOVE  W-1           TO    JSJD-03.
           MOVE  C             TO    JSJD-04.
      *           READ  JSJD          KEY IS JSJD-KEY2    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JSJD_PNAME1 BY REFERENCE JSJD-REC " " BY REFERENCE
            JSJD-KEY2 RETURNING RET.
           IF  RET = 1
               ADD       1         TO    C
               GO  TO  UPD-039
           END-IF
      **
      *           DELETE   JSJD       INVALID
      *///////////////
           CALL "DB_Delete" USING JSJD_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE     "JSJD"     TO    ERR-F
               MOVE     "D"        TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
           ADD   1             TO    C.
           GO  TO   UPD-039.
       UPD-040.
           MOVE     ZERO       TO    C.
           MOVE  ZERO       TO    WK-AREA.
       UPD-061.
           ADD      1          TO    C.
           IF  C               =     A
               GO  TO  UPD-080
           END-IF
           IF  W-61(C)         =     ZERO OR 9999999
               GO  TO  UPD-061
           END-IF
           MOVE     W-61(C)    TO    JMSTD-KEY1  ERR-K.
      *           READ     JMSTD      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  ERR-STAT   TO    ERR-FLG
               MOVE  "G"        TO    ERR-M
               MOVE  "JMSTD"    TO    ERR-F
               CALL "SD_Output" USING
                "ERR-DIS" ERR-DIS "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               GO  TO  UPD-061
           END-IF
           IF  JMSTD-01    NOT =  0  AND  5  AND  6
               GO  TO  UPD-061
           END-IF
           MOVE  JMSTD-03   TO    WK-HIN(C)
           MOVE  JMSTD-09   TO    WK-SIZ(C).
           PERFORM  SET-RTN   THRU  SET-EX
               VARYING    I   FROM  1  BY  1
                 UNTIL    I    >  10.
           PERFORM  JMT-REW-RTN  THRU  JMT-REW-EX.
           GO  TO  UPD-061.
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       UPD-080.
      *    倉別在庫マスタ　更新   *
           IF  W-ACT       NOT  =     3
               GO  TO  UPD-086
           END-IF
           MOVE     ZERO       TO    C.
       UPD-082.
           ADD      1          TO    C.
           IF  C               >     6
               GO  TO  UPD-EX
           END-IF
           IF  O-HCD(C)        >     999899
               GO  TO  UPD-082
           END-IF
           IF  O-HCD(C)        =     ZERO
               GO  TO  UPD-082
           END-IF
           MOVE     O-SCD       TO    NJZAI-01.
           MOVE     O-HCD(C)    TO    NJZAI-02.
           MOVE     O-SIZ(C)    TO    NJZAI-03.
           MOVE     NJZAI-KEY   TO    ERR-K.
      *           READ     NJZAI      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-083
           END-IF
      *
           PERFORM  TZS-RTN     THRU  TZS-EX.
      *
      *           REWRITE  NJZAI-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"         TO    ERR-M
               MOVE    "NJZAI"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           GO  TO  UPD-084.
       UPD-083.
           MOVE     SPACE       TO    NJZAI-R.
           INITIALIZE                 NJZAI-R.
           MOVE     O-SCD       TO    NJZAI-01.
           MOVE     O-HCD(C)    TO    NJZAI-02.
           MOVE     O-SIZ(C)    TO    NJZAI-03.
           MOVE     NJZAI-KEY   TO    ERR-K.
      *
           PERFORM  TZS-RTN     THRU  TZS-EX.
      *
           PERFORM  NJW-RTN     THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  UPD-083
           END-IF.
       UPD-084.
           MOVE     9           TO    NJZAI-01.
           MOVE     O-HCD(C)    TO    NJZAI-02.
           MOVE     O-SIZ(C)    TO    NJZAI-03.
           MOVE     NJZAI-KEY   TO    ERR-K.
      *           READ     NJZAI      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-085
           END-IF
      *
           PERFORM  TZS-RTN     THRU  TZS-EX.
      *
      *           REWRITE  NJZAI-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"         TO    ERR-M
               MOVE    "NJZAI"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           GO  TO  UPD-082.
       UPD-085.
           MOVE     SPACE       TO    NJZAI-R.
           INITIALIZE                 NJZAI-R.
           MOVE     9           TO    NJZAI-01.
           MOVE     O-HCD(C)    TO    NJZAI-02.
           MOVE     O-SIZ(C)    TO    NJZAI-03.
           MOVE     NJZAI-KEY   TO    ERR-K.
      *
           PERFORM  TZS-RTN     THRU  TZS-EX.
      *
           PERFORM  NJW-RTN     THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  UPD-085
           END-IF
           GO  TO  UPD-082.
      *-------------
       UPD-086.
           MOVE     ZERO       TO    C.
       UPD-087.
           ADD      1          TO    C.
           IF  C               >     6
               GO  TO  UPD-092
           END-IF
           IF  W-621(C)        >     999899
               GO  TO  UPD-087
           END-IF
           IF  W-621(C)        =     ZERO
               GO  TO  UPD-087
           END-IF
           MOVE     W-51       TO    NJZAI-01.
           MOVE     W-621(C)   TO    NJZAI-02.
           MOVE     W-63 (C)   TO    NJZAI-03.
           MOVE     NJZAI-KEY  TO    ERR-K.
      *           READ     NJZAI      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-088
           END-IF
      *
           PERFORM  SZS-RTN     THRU  SZS-EX.
      *
      *           REWRITE  NJZAI-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"         TO    ERR-M
               MOVE    "NJZAI"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           GO  TO  UPD-089.
       UPD-088.
           MOVE     SPACE      TO    NJZAI-R.
           INITIALIZE                NJZAI-R.
           MOVE     W-51       TO    NJZAI-01.
           MOVE     W-621(C)   TO    NJZAI-02.
           MOVE     W-63 (C)   TO    NJZAI-03.
           MOVE     NJZAI-KEY  TO    ERR-K.
      *
           PERFORM  SZS-RTN     THRU  SZS-EX.
      *
           PERFORM  NJW-RTN     THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  UPD-088
           END-IF.
       UPD-089.
           MOVE     9          TO    NJZAI-01.
           MOVE     W-621(C)   TO    NJZAI-02.
           MOVE     W-63 (C)   TO    NJZAI-03.
           MOVE     NJZAI-KEY  TO    ERR-K.
      *           READ     NJZAI      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-090
           END-IF
      *
           PERFORM  SZS-RTN     THRU  SZS-EX.
      *
      *           REWRITE  NJZAI-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"         TO    ERR-M
               MOVE    "NJZAI"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           GO  TO  UPD-087.
       UPD-090.
           MOVE     SPACE      TO    NJZAI-R.
           INITIALIZE                NJZAI-R.
           MOVE     9          TO    NJZAI-01.
           MOVE     W-621(C)   TO    NJZAI-02.
           MOVE     W-63 (C)   TO    NJZAI-03.
           MOVE     NJZAI-KEY  TO    ERR-K.
      *
           PERFORM  SZS-RTN     THRU  SZS-EX.
      *
           PERFORM  NJW-RTN     THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  UPD-090
           END-IF
           GO  TO  UPD-087.
      *-------------
       UPD-092.
           MOVE     ZERO       TO    C.
       UPD-093.
           ADD      1          TO    C.
           IF  C               >     6
               GO  TO  UPD-EX
           END-IF
           IF  O-HCD(C)        >     999899
               GO  TO  UPD-093
           END-IF
           IF  O-HCD(C)        =     ZERO
               GO  TO  UPD-093
           END-IF
           MOVE     O-SCD      TO    NJZAI-01.
           MOVE     O-HCD(C)   TO    NJZAI-02.
           MOVE     O-SIZ(C)   TO    NJZAI-03.
           MOVE     NJZAI-KEY  TO    ERR-K.
      *           READ     NJZAI      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-094
           END-IF
      *
           PERFORM  DZS-RTN     THRU  DZS-EX.
      *
      *           REWRITE  NJZAI-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"         TO    ERR-M
               MOVE    "NJZAI"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           GO  TO  UPD-095.
       UPD-094.
           MOVE     SPACE      TO    NJZAI-R.
           INITIALIZE                NJZAI-R.
           MOVE     O-SCD      TO    NJZAI-01.
           MOVE     O-HCD(C)   TO    NJZAI-02.
           MOVE     O-SIZ(C)   TO    NJZAI-03.
           MOVE     NJZAI-KEY  TO    ERR-K.
      *
           PERFORM  DZS-RTN     THRU  DZS-EX.
      *
           PERFORM  NJW-RTN     THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  UPD-094
           END-IF.
       UPD-095.
           MOVE     9          TO    NJZAI-01.
           MOVE     O-HCD(C)   TO    NJZAI-02.
           MOVE     O-SIZ(C)   TO    NJZAI-03.
           MOVE     NJZAI-KEY  TO    ERR-K.
      *           READ     NJZAI      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-096
           END-IF
      *
           PERFORM  DZS-RTN     THRU  DZS-EX.
      *
      *           REWRITE  NJZAI-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"         TO    ERR-M
               MOVE    "NJZAI"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           GO  TO  UPD-093.
       UPD-096.
           MOVE     SPACE      TO    NJZAI-R.
           INITIALIZE                NJZAI-R.
           MOVE     9          TO    NJZAI-01.
           MOVE     O-HCD(C)   TO    NJZAI-02.
           MOVE     O-SIZ(C)   TO    NJZAI-03.
           MOVE     NJZAI-KEY  TO    ERR-K.
      *
           PERFORM  DZS-RTN     THRU  DZS-EX.
      *
           PERFORM  NJW-RTN     THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  UPD-096
           END-IF
           GO  TO  UPD-093.
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       UPD-EX.
           EXIT.
      *----受注マスタ　ＳＥＴ　ルーチン----*
       SET-RTN.
           ADD S-S(C I)   TO  JMSTD-151(I).
           SUBTRACT W-64(C I) FROM  JMSTD-1211(I).
       SET-EX.
           EXIT.
      *----出荷指図トラン　取消処理　ルーチン----*
       REW-RTN.
           MOVE     JSTR-KEY      TO    ERR-K.
      *           REWRITE  JSTR-R  INVALID
      *///////////////
           CALL "DB_Update" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"         TO    ERR-M
               MOVE    "JSTR"      TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF.
       REW-EX.
           EXIT.
       JNSR-DEL-RTN.
           MOVE     ZERO          TO    JNSR-KEY1.
           MOVE     JSTR-09       TO    JNSR-01.
           MOVE     JSTR-05       TO    HIZUKE.
           MOVE     HIZUKE        TO    JNSR-02.
           IF  JSTR-03         =  0
               MOVE   22       TO  JNSR-03
           ELSE
               IF  JSTR-03      =  5  OR  6
                   MOVE   23       TO  JNSR-03
               ELSE
                   MOVE   25       TO  JNSR-03
               END-IF
           END-IF
           MOVE     JSTR-01       TO    JNSR-04.
           MOVE     JSTR-02       TO    JNSR-05.
      *           READ     JNSR          INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JNSR_PNAME1 BY REFERENCE JNSR-R " " RETURNING RET.
           IF  RET = 1
               GO  TO  JNSR-DEL-EX
           END-IF
           MOVE     JNSR-KEY1     TO    ERR-K.
      *           DELETE  JNSR    INVALID
      *///////////////
           CALL "DB_Delete" USING JNSR_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE    "D"         TO    ERR-M
               MOVE    "JNSR"      TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF.
       JNSR-DEL-EX.
           EXIT.
      *----出荷指図トラン　ＤＥＬＥＴＥルーチン----*
       DEL-RTN.
      *           DELETE   JSTR       INVALID
      *///////////////
           CALL "DB_Delete" USING JSTR_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE     "JSTR"     TO    ERR-F
               MOVE     "D"        TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF.
       DEL-EX.
           EXIT.
      *----受注マスタ　ＲＥＷＲＩＴＥ　ルーチン----*
       JMT-REW-RTN.
      *           REWRITE  JMSTD-R   INVALID
      *///////////////
           CALL "DB_Update" USING
            JMSTD_PNAME1 JMSTD_LNAME JMSTD-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"         TO    ERR-M
               MOVE    "JMSTD"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF.
       JMT-REW-EX.
           EXIT.
      *----伝票№検索ファイル　ＤＥＬＥＴＥ----*
       DNDEL-RTN.
           IF  W-ACT            =  3
               GO  TO  DNDEL-020
           END-IF
           MOVE  ZERO          TO  C.
       DNDEL-010.
           ADD   1             TO  C.
           IF  C                >  6
               GO  TO  DNDEL-EX
           END-IF
           IF  S-HCD(C)         =  ZERO
               GO  TO  DNDEL-010
           END-IF.
       DNDEL-020.
           MOVE  W-51          TO  DNKN-01.
           MOVE  S-HCD(C)      TO  DNKN-02.
           MOVE  3             TO  DNKN-03.
           MOVE  W-1           TO  DNKN-041.
           MOVE  C             TO  DNKN-042.
      *           READ  JT-DNKN       INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JT-DNKN_PNAME1 BY REFERENCE DNKN-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  DNDEL-900
           END-IF
      *
      *           DELETE   JT-DNKN    INVALID
      *///////////////
           CALL "DB_Delete" USING JT-DNKN_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "JT-DNKN"     TO  ERR-F
               MOVE  "D"           TO  ERR-M
               MOVE  DNKN-KEY      TO  ERR-K
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF.
       DNDEL-900.
           IF  W-ACT            =  3
               GO  TO  DNDEL-EX
           END-IF
           GO  TO  DNDEL-010.
       DNDEL-EX.
           EXIT.
      *----伝票№検索ファイル　ＷＲＩＴＥ----*
       DNWRI-RTN.
           MOVE  ZERO          TO  C.
       DNWRI-010.
           ADD   1             TO  C.
           IF  C                >  6
               GO  TO  DNWRI-EX
           END-IF
           IF  W-621(C)         =  ZERO
               GO  TO  DNWRI-010
           END-IF
           MOVE  W-51          TO  DNKN-01.
           MOVE  W-621(C)      TO  DNKN-02.
           MOVE  3             TO  DNKN-03.
           MOVE  W-1           TO  DNKN-041.
           MOVE  C             TO  DNKN-042.
      *           READ  JT-DNKN       INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JT-DNKN_PNAME1 BY REFERENCE DNKN-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  DNWRI-020
           END-IF
           GO  TO  DNWRI-010.
       DNWRI-020.
      *           WRITE    DNKN-R     INVALID
      *//////////////
           CALL "DB_Insert" USING
            JT-DNKN_PNAME1 JT-DNKN_LNAME DNKN-R RETURNING RET.
           IF  RET = 1
               MOVE  "JT-DNKN"     TO  ERR-F
               MOVE  "W"           TO  ERR-M
               MOVE  DNKN-KEY      TO  ERR-K
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
           GO  TO  DNWRI-010.
       DNWRI-EX.
           EXIT.
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      **********************************************
      *    在庫マスター　確定取消数　セット        *
      **********************************************
       TZS-RTN.
           MOVE     ZERO        TO    W-C.
       TZS-010.
           ADD      1           TO    W-C.
           IF  W-C             >  10
               GO  TO  TZS-EX
           END-IF
           IF  W-21       NOT  =  5  AND 6
               ADD      O-SU(C W-C) TO    NJZAI-0911(W-C)
               SUBTRACT O-SU(C W-C) FROM  NJZAI-0811(W-C)
           ELSE
               IF  W-21            =  5
                   SUBTRACT O-SU(C W-C) FROM  NJZAI-0911(W-C)
                   ADD      O-SU(C W-C) TO    NJZAI-0811(W-C)
               END-IF
           END-IF
           GO  TO  TZS-010.
       TZS-EX.
           EXIT.
      **********************************************
      *    在庫マスター　確定修正　セット（ＡＤＤ）*
      **********************************************
       SZS-RTN.
           MOVE     ZERO        TO    W-C.
       SZS-010.
           ADD      1           TO    W-C.
           IF  W-C             >  10
               GO  TO  SZS-EX
           END-IF
           IF  W-21       NOT  =  5  AND 6
               ADD      W-64(C W-C) TO    NJZAI-0911(W-C)
           ELSE
               IF  W-21            =  5
                   SUBTRACT W-64(C W-C) FROM  NJZAI-0911(W-C)
               END-IF
           END-IF
           GO  TO  SZS-010.
       SZS-EX.
           EXIT.
      **********************************************
      *    在庫マスター　確定修正　セット（ＤＥＬ）*
      **********************************************
       DZS-RTN.
           MOVE     ZERO        TO    W-C.
       DZS-010.
           ADD      1           TO    W-C.
           IF  W-C             >  10
               GO  TO  DZS-EX
           END-IF
           IF  W-21       NOT  =  5  AND 6
               SUBTRACT O-SU(C W-C) FROM  NJZAI-0911(W-C)
           ELSE
               IF  W-21            =  5
                   ADD      O-SU(C W-C) TO    NJZAI-0911(W-C)
               END-IF
           END-IF
           GO  TO  DZS-010.
       DZS-EX.
           EXIT.
      *****  倉別在庫マスタ　ＷＲＩＴＥ　
       NJW-RTN.
           MOVE     0          TO    WRI-SW.
      *           WRITE  NJZAI-R    INVALID
      *//////////////
           CALL "DB_Insert" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               GO  TO  NJW-010
           END-IF
           GO  TO  NJW-EX.
       NJW-010.
           IF  ERR-STAT      =  "24"
               GO  TO  NJW-020
           END-IF
           IF  ERR-STAT  NOT =  "00"
               MOVE    "W"         TO    ERR-M
               MOVE    "NJZAI"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           MOVE     2          TO    WRI-SW.
           GO  TO  NJW-EX.
       NJW-020.
           MOVE     1          TO    WRI-SW.
           MOVE    "W"         TO    ERR-M.
           MOVE    "NJZAI"     TO    ERR-F.
           MOVE     NJZAI-KEY  TO    ERR-K.
           MOVE     ERR-STAT   TO    ERR-FLG.
           CALL "SD_Output" USING
            "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "SD_Output" USING
            " " "ｴﾘｱ ｶｸﾁｮｳｺﾞ,ｻｲｶｲｷｰ ｦ ｵｽ!" "STOP" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
       NJW-EX.
           EXIT.
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      *----ＣＲ１－ＲＴＮ              *
      *        ～クリア　ルーチン～----*
       CR1-RTN.
           MOVE     LIN1       TO    LIN2.
           CALL "SD_Arg_Match_Line" USING
            "LIN2" "2" LIN2 RETURNING RESU.
       CR1-010.
           PERFORM  CR2-RTN    THRU  CR2-EX  VARYING  C  FROM  A
                    BY  1  UNTIL  C  >   6.
       CR1-EX.
           EXIT.
      *----ＣＲ２－ＲＴＮ              *
      *      ～ワーク・画面クリア～----*
       CR2-RTN.
           INITIALIZE                W-6A(C).
           CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU.
           ADD      2          TO    LIN2.
           CALL "SD_Arg_Match_Line" USING
            "LIN2" "2" LIN2 RETURNING RESU.
       CR2-EX.
           EXIT.
       REV-DSP-RTN.
           CALL "SD_Output" USING
            "DSP-RC-ALL" DSP-RC-ALL "p" RETURNING RESU.
           IF  W-63(A)  =  1
               CALL "SD_Output" USING
                "DSP-RR1" DSP-RR1 "p" RETURNING RESU
           END-IF
           IF  W-63(A)  =  2
               CALL "SD_Output" USING
                "DSP-RR2" DSP-RR2 "p" RETURNING RESU
           END-IF
           IF  W-63(A)  =  3
               CALL "SD_Output" USING
                "DSP-RR3" DSP-RR3 "p" RETURNING RESU
           END-IF
           IF  W-63(A)  =  4
               CALL "SD_Output" USING
                "DSP-RR4" DSP-RR4 "p" RETURNING RESU
           END-IF.
       REV-DSP-EXT.
           EXIT.
       REV-CLE-RTN.
           CALL "SD_Output" USING
            "DSP-RC-ALL" DSP-RC-ALL "p" RETURNING RESU.
       REV-CLE-EXT.
           EXIT.
       KBS-RTN.
           MOVE  SPACE       TO  JSTR-KEY.
           IF  JS-SIGN    =  1
               MOVE  100000    TO  JSTR-01
           END-IF.
      *           START    JSTR KEY   NOT <  JSTR-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            JSTR_PNAME1 "JSTR-KEY" " NOT < " JSTR-KEY RETURNING RET.
           IF  RET = 1
               GO  TO   KBS-EX
           END-IF.
       KBS-010.
      *           READ     JSTR       NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO   KBS-EX
           END-IF
           IF  JS-SIGN    =  0
               IF  JSTR-01     >  099999
                   GO  TO  KBS-EX
               END-IF
           END-IF
           IF  JS-SIGN    =  1
               IF  JSTR-01     >  199999
                   GO  TO  KBS-EX
               END-IF
           END-IF
           IF  JSTR-4022   NOT =  0
               GO  TO  KBS-010
           END-IF
           IF  JSTR-4021   NOT =  STN-NO-02
               GO  TO  KBS-010
           END-IF.
       KBS-020.
           MOVE     JSTR-01    TO    W-DNO.
       KBS-030.
      *           READ     JSTR       INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JSTR_PNAME1 BY REFERENCE JSTR-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "ERR-READ" ERR-READ "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF
           MOVE     1          TO    JSTR-4022.
      *           REWRITE  JSTR-R     INVALID
      *///////////////
           CALL "DB_Update" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE     "JSTR"     TO    ERR-F
               MOVE     "R"        TO    ERR-M
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF.
       KBS-040.
      *           READ     JSTR       NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO   KBS-EX
           END-IF
           IF  JS-SIGN    =  0
               IF  JSTR-01     >  099999
                   GO  TO  KBS-EX
               END-IF
           END-IF
           IF  JS-SIGN    =  1
               IF  JSTR-01     >  199999
                   GO  TO  KBS-EX
               END-IF
           END-IF
           IF  JSTR-4022   NOT =  0
               GO  TO  KBS-040
           END-IF
           IF  JSTR-4021   NOT =  STN-NO-02
               GO  TO  KBS-040
           END-IF
           IF  JSTR-01         =  W-DNO
               GO  TO  KBS-030
           END-IF
           GO  TO  KBS-020.
       KBS-EX.
           EXIT.
      ***
           COPY    LPMSG.
      ***

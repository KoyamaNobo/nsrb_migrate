       IDENTIFICATION        DIVISION.
       PROGRAM-ID.           JTN85I.
       AUTHOR.               --------.
      ******************************************************
      *    PROGRAM      :    入庫予定残数　入力            *
      *    DATA WRITTEN :    96/01/30                      *
      *    SCREEN  USED :    SJN85I                        *
      *    COMPILE TYPE :    COBOL                         *
      ******************************************************
       ENVIRONMENT           DIVISION.
       CONFIGURATION         SECTION.
       SOURCE-COMPUTER.      SYSTEM7200.
       OBJECT-COMPUTER.      SYSTEM7200.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT           PIC  X(02).
       01  W-AREA.
           02  W-KEY.
               03  W-HCD      PIC  9(06).
               03  W-SC       PIC  9(01).
           02  W-TNSD.
               03  W-TNS      PIC S9(06)   OCCURS  10.
               03  W-TNST     PIC S9(06).
           02  W-OZKD.
               03  W-OZK      PIC S9(06)   OCCURS  10.
               03  W-OZKT     PIC S9(06).
           02  W-OGYD.
               03  W-OGY      PIC S9(06)   OCCURS  10.
               03  W-OGYT     PIC S9(06).
           02  W-NZKD.
               03  W-NZK      PIC S9(06)   OCCURS  10.
               03  W-NZKT     PIC S9(06).
           02  W-NGYD.
               03  W-NGY      PIC S9(06)   OCCURS  10.
               03  W-NGYT     PIC S9(06).
           02  W-ACT          PIC  9(01)  VALUE  0.
           02  W-OKC          PIC  9(01).
           02  W-PC           PIC  9(01).
           02  W-HCDD         PIC  9(06).
           02  W-C            PIC  9(02).
           02  WRI-SW         PIC  9(01).
           02  AZ-SW          PIC  9(01).
           02  INV-SW         PIC  9(01).
           02  CON-SW         PIC  9(01)  VALUE  0.
           02  W-TUKI         PIC  9(02).
           02  A              PIC  9(02).
           02  W-SDD          PIC  9(10).
           02  W-SD    REDEFINES  W-SDD.
               03  W-S        PIC  9(01)   OCCURS  10.
           02  W-SEKEY.
               03  W-SKEY     PIC  9(06).
               03  W-EKEY     PIC  9(06).
           02  W-PAGE         PIC  9(03).
           02  W-SUT          PIC S9(07).
       01  W-ASIZ.
           02  W-SIZ1         PIC  X(40)   VALUE
               "３号２号１号０号　中　大特大28.029.030.0".
           02  W-SIZ2         PIC  X(40)   VALUE
               "12.513.013.514.015.016.017.018.019.020.0".
           02  W-SIZ3         PIC  X(40)   VALUE
               "21.021.522.022.523.023.524.024.525.0    ".
           02  W-SIZ4         PIC  X(40)   VALUE
               "24.024.525.025.526.026.527.027.5        ".
       01  W-SIZD.
           02  W-SIZ          PIC  X(04)   OCCURS  10.
      *
       01  HEAD1.
           02  F              PIC  X(05)  VALUE X"1A24212474".
           02  F              PIC  X(38)  VALUE SPACE.
           02  F              PIC  N(09)  VALUE
               "＊＊＊　　入庫予定".
           02  H-MID          PIC  N(02).
           02  F              PIC  N(11)  VALUE
               "残数　リスト　　＊＊＊".
           02  F              PIC  X(30)  VALUE SPACE.
           02  F              PIC  X(05)  VALUE "DATE ".
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(07)  VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  F              PIC  X(05)  VALUE X"1A24212078".
           02  F              PIC  X(07)  VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(08)  VALUE "品　　　　　名　".
           02  F              PIC  X(25)  VALUE SPACE.
           02  F              PIC  X(01)  VALUE "1".
           02  F              PIC  X(05)  VALUE SPACE.
           02  F              PIC  N(02)  VALUE "３号".
           02  F              PIC  X(05)  VALUE SPACE.
           02  F              PIC  N(02)  VALUE "２号".
           02  F              PIC  X(05)  VALUE SPACE.
           02  F              PIC  N(02)  VALUE "１号".
           02  F              PIC  X(05)  VALUE SPACE.
           02  F              PIC  N(02)  VALUE "０号".
           02  F              PIC  X(05)  VALUE SPACE.
           02  F              PIC  N(02)  VALUE "　中".
           02  F              PIC  X(05)  VALUE SPACE.
           02  F              PIC  N(02)  VALUE "　大".
           02  F              PIC  X(05)  VALUE SPACE.
           02  F              PIC  N(02)  VALUE "特大".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "28.0".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "29.0".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "30.0".
           02  F              PIC  X(10)  VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(44)  VALUE SPACE.
           02  F              PIC  X(01)  VALUE "2".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "12.5".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "13.0".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "13.5".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "14.0".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "15.0".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "16.0".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "17.0".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "18.0".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "19.0".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "20.0".
           02  F              PIC  X(10)  VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(44)  VALUE SPACE.
           02  F              PIC  X(01)  VALUE "3".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "21.0".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "21.5".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "22.0".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "22.5".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "23.0".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "23.5".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "24.0".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "24.5".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "25.0".
           02  F              PIC  X(18)  VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(44)  VALUE SPACE.
           02  F              PIC  X(01)  VALUE "4".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "24.0".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "24.5".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "25.0".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "25.5".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "26.0".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "26.5".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "27.0".
           02  F              PIC  X(04)  VALUE SPACE.
           02  F              PIC  X(04)  VALUE "27.5".
           02  F              PIC  X(20)  VALUE SPACE.
           02  F              PIC  N(04)  VALUE "　合　計".
       01  W-P.
           02  P-HCD          PIC  9(06).
           02  F              PIC  X(01).
           02  P-NAME         PIC  N(24).
           02  F              PIC  X(01).
           02  P-SC           PIC  9(01).
           02  P-SUD.
               03  P-SU       PIC ----,--9    OCCURS  10.
           02  P-SUT          PIC --,---,--9.
      *
           COPY     LWMSG.
      *
           COPY     L-JCON.
           COPY     LIHIM2.
           COPY     LJNYZ.
           COPY     LJMST3.
           COPY     LNJZAI.
      **********************************************************
      *****     プリント　ファイル　(６６行／２０６列)     *****
      **********************************************************
      *FD  SP-F
       77  SP-R            PIC  X(206).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  ACP-AREA.
           02  ACP-ACT     PIC  9(01).
           02  ACP-1.
               03  A-HCD      PIC  9(06).
               03  D-HNA      PIC  N(24).
               03  A-SC       PIC  9(01).
           02  ACP-2.
               03  FILLER  PIC  X(04).
               03  FILLER  PIC  X(04).
               03  FILLER  PIC  X(04).
               03  FILLER  PIC  X(04).
               03  FILLER  PIC  X(04).
               03  FILLER  PIC  X(04).
               03  FILLER  PIC  X(04).
               03  FILLER  PIC  X(04).
               03  FILLER  PIC  X(04).
               03  FILLER  PIC  X(04).
           02  ACP-3.
               03  FILLER  PIC ZZZZZ9- .
               03  FILLER.
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
           02  ACP-4.
               03  FILLER  PIC ZZZZZ9- .
               03  FILLER.
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
           02  ACP-5.
               03  FILLER  PIC ZZZZZ9- .
               03  FILLER.
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
           02  ACP-6.
               03  D-NZKT  PIC ZZZZZ9- .
               03  D-NZK   PIC ZZZZZ9- .
           02  ACP-7.
               03  FILLER  PIC ZZZZZ9- .
               03  FILLER.
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
                   04  FILLER  PIC ZZZZZZ- .
           02  ACP-8.
               03  D-NGYT  PIC ZZZZZ9- .
               03  FILLER.
                   04  A-NGY   PIC S9(06).
                   04  D-NGY   PIC ZZZZZZ- .
           02  ACP-PRI.
               03  D-PRIM.
                   04  FILLER  PIC  X(22)  VALUE
                        "前月残=0 , 現在残=1   ".
                   04  FILLER  PIC  X(25)  VALUE
                        "品名ｺｰﾄﾞ 000000 ～ 999999".
               03  A-PC       PIC  9(01).
               03  A-SKEY     PIC  9(06).
               03  A-EKEY     PIC  9(06).
           02  ACP-OKC     PIC  9(01).
       01  DSP-CLE.
           02  CLE-ACT.
               03  FILLER  PIC  X(01)  VALUE " ".
           02  CLE-01.
               03  C-1.
                   04  FILLER  PIC  X(06)  VALUE "      ".
                   04  FILLER  PIC  X(48)  VALUE
                   "                                                ".
                   04  FILLER  PIC  X(01)  VALUE " ".
               03  C-2.
                   04  FILLER  PIC  X(04)  VALUE "    ".
                   04  FILLER  PIC  X(04)  VALUE "    ".
                   04  FILLER  PIC  X(04)  VALUE "    ".
                   04  FILLER  PIC  X(04)  VALUE "    ".
                   04  FILLER  PIC  X(04)  VALUE "    ".
                   04  FILLER  PIC  X(04)  VALUE "    ".
                   04  FILLER  PIC  X(04)  VALUE "    ".
                   04  FILLER  PIC  X(04)  VALUE "    ".
                   04  FILLER  PIC  X(04)  VALUE "    ".
                   04  FILLER  PIC  X(04)  VALUE "    ".
               03  C-3.
                   04  FILLER  PIC  X(08)  VALUE "        ".
                   04  FILLER.
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
               03  C-4.
                   04  FILLER  PIC  X(08)  VALUE "        ".
                   04  FILLER.
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
               03  C-5.
                   04  FILLER  PIC  X(08)  VALUE "        ".
                   04  FILLER.
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
               03  C-6.
                   04  FILLER  PIC  X(08)  VALUE "        ".
                   04  FILLER.
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
               03  C-7.
                   04  FILLER  PIC  X(08)  VALUE "        ".
                   04  FILLER.
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
               03  C-8.
                   04  FILLER  PIC  X(08)  VALUE "        ".
                   04  FILLER.
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
                       05  FILLER  PIC  X(08)  VALUE "        ".
           02  C-PRI.
               03  FILLER  PIC  X(22)  VALUE
                    "                      ".
               03  FILLER  PIC  X(25)  VALUE
                    "                         ".
           02  CLE-OKC.
               03  FILLER  PIC  X(01)  VALUE " ".
       01  DSP-ERR.
           02  INV-M00     PIC  X(22)
                           VALUE "＊＊ＪＣＯＮ　なし＊＊".
           02  INV-M02     PIC  X(28)
                           VALUE "＊＊品名マスター　未登録＊＊".
           02  INV-M03     PIC  X(20)
                           VALUE "＊＊サイズ　なし＊＊".
           02  INV-M04     PIC  X(30)
                           VALUE "＊＊入庫予定ファイル　なし＊＊".
           02  INV-D02     PIC  X(18)
                           VALUE "＊＊預り　あり＊＊".
           COPY     LSMSG.
      **
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-CLR
       CALL "SD_Init" USING
           "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "499" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ACT" "9" "1" "60" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-1" " " "3" "0" "55" "ACP-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "3" "6" "6" " " "ACP-1" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNA" "N" "3" "18" "48" "A-HCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SC" "9" "3" "76" "1" "D-HNA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SC" BY REFERENCE W-SC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-2" " " "5" "0" "40" "ACP-1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-2" "X" "5" "3" "4" " " "ACP-2" RETURNING RESU.
       CALL "SD_From" USING 
            "01ACP-2" BY REFERENCE W-SIZ(1) "4" "1" "01" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACP-2" "X" "5" "11" "4" "01ACP-2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02ACP-2" BY REFERENCE W-SIZ(1) "4" "1" "02" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03ACP-2" "X" "5" "19" "4" "02ACP-2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03ACP-2" BY REFERENCE W-SIZ(1) "4" "1" "03" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04ACP-2" "X" "5" "27" "4" "03ACP-2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04ACP-2" BY REFERENCE W-SIZ(1) "4" "1" "04" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05ACP-2" "X" "5" "35" "4" "04ACP-2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "05ACP-2" BY REFERENCE W-SIZ(1) "4" "1" "05" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06ACP-2" "X" "5" "43" "4" "05ACP-2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "06ACP-2" BY REFERENCE W-SIZ(1) "4" "1" "06" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07ACP-2" "X" "5" "51" "4" "06ACP-2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "07ACP-2" BY REFERENCE W-SIZ(1) "4" "1" "07" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08ACP-2" "X" "5" "59" "4" "07ACP-2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "08ACP-2" BY REFERENCE W-SIZ(1) "4" "1" "08" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09ACP-2" "X" "5" "67" "4" "08ACP-2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "09ACP-2" BY REFERENCE W-SIZ(1) "4" "1" "09" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10ACP-2" "X" "5" "75" "4" "09ACP-2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "10ACP-2" BY REFERENCE W-SIZ(1) "4" "1" "10" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-3" " " "0" "0" "77" "ACP-2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-3" "ZZZZZ9-" "7" "73" "7" " " "ACP-3" RETURNING RESU.
       CALL "SD_From" USING 
            "01ACP-3" BY REFERENCE W-OZKT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACP-3" " " "8" "60" "70" "01ACP-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102ACP-3" "ZZZZZZ-" "8" "1" "7" " " "02ACP-3"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0102ACP-3" BY REFERENCE W-OZK(1) "6" "1" "01" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202ACP-3" "ZZZZZZ-" "8" "9" "7" "0102ACP-3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0202ACP-3" BY REFERENCE W-OZK(1) "6" "1" "02" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302ACP-3" "ZZZZZZ-" "8" "17" "7" "0202ACP-3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0302ACP-3" BY REFERENCE W-OZK(1) "6" "1" "03" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402ACP-3" "ZZZZZZ-" "8" "25" "7" "0302ACP-3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0402ACP-3" BY REFERENCE W-OZK(1) "6" "1" "04" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0502ACP-3" "ZZZZZZ-" "8" "33" "7" "0402ACP-3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0502ACP-3" BY REFERENCE W-OZK(1) "6" "1" "05" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0602ACP-3" "ZZZZZZ-" "8" "41" "7" "0502ACP-3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0602ACP-3" BY REFERENCE W-OZK(1) "6" "1" "06" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0702ACP-3" "ZZZZZZ-" "8" "49" "7" "0602ACP-3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0702ACP-3" BY REFERENCE W-OZK(1) "6" "1" "07" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0802ACP-3" "ZZZZZZ-" "8" "57" "7" "0702ACP-3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0802ACP-3" BY REFERENCE W-OZK(1) "6" "1" "08" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0902ACP-3" "ZZZZZZ-" "8" "65" "7" "0802ACP-3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0902ACP-3" BY REFERENCE W-OZK(1) "6" "1" "09" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "1002ACP-3" "ZZZZZZ-" "8" "73" "7" "0902ACP-3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1002ACP-3" BY REFERENCE W-OZK(1) "6" "1" "10" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-4" " " "0" "0" "77" "ACP-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-4" "ZZZZZ9-" "9" "73" "7" " " "ACP-4" RETURNING RESU.
       CALL "SD_From" USING 
            "01ACP-4" BY REFERENCE W-TNST "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACP-4" " " "10" "60" "70" "01ACP-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102ACP-4" "ZZZZZZ-" "10" "1" "7" " " "02ACP-4"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0102ACP-4" BY REFERENCE W-TNS(1) "6" "1" "01" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202ACP-4" "ZZZZZZ-" "10" "9" "7" "0102ACP-4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0202ACP-4" BY REFERENCE W-TNS(1) "6" "1" "02" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302ACP-4" "ZZZZZZ-" "10" "17" "7" "0202ACP-4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0302ACP-4" BY REFERENCE W-TNS(1) "6" "1" "03" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402ACP-4" "ZZZZZZ-" "10" "25" "7" "0302ACP-4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0402ACP-4" BY REFERENCE W-TNS(1) "6" "1" "04" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0502ACP-4" "ZZZZZZ-" "10" "33" "7" "0402ACP-4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0502ACP-4" BY REFERENCE W-TNS(1) "6" "1" "05" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0602ACP-4" "ZZZZZZ-" "10" "41" "7" "0502ACP-4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0602ACP-4" BY REFERENCE W-TNS(1) "6" "1" "06" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0702ACP-4" "ZZZZZZ-" "10" "49" "7" "0602ACP-4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0702ACP-4" BY REFERENCE W-TNS(1) "6" "1" "07" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0802ACP-4" "ZZZZZZ-" "10" "57" "7" "0702ACP-4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0802ACP-4" BY REFERENCE W-TNS(1) "6" "1" "08" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0902ACP-4" "ZZZZZZ-" "10" "65" "7" "0802ACP-4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0902ACP-4" BY REFERENCE W-TNS(1) "6" "1" "09" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "1002ACP-4" "ZZZZZZ-" "10" "73" "7" "0902ACP-4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1002ACP-4" BY REFERENCE W-TNS(1) "6" "1" "10" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-5" " " "0" "0" "77" "ACP-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-5" "ZZZZZ9-" "11" "73" "7" " " "ACP-5" RETURNING RESU.
       CALL "SD_From" USING 
            "01ACP-5" BY REFERENCE W-OGYT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACP-5" " " "12" "60" "70" "01ACP-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102ACP-5" "ZZZZZZ-" "12" "1" "7" " " "02ACP-5"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0102ACP-5" BY REFERENCE W-OGY(1) "6" "1" "01" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202ACP-5" "ZZZZZZ-" "12" "9" "7" "0102ACP-5" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0202ACP-5" BY REFERENCE W-OGY(1) "6" "1" "02" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302ACP-5" "ZZZZZZ-" "12" "17" "7" "0202ACP-5" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0302ACP-5" BY REFERENCE W-OGY(1) "6" "1" "03" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402ACP-5" "ZZZZZZ-" "12" "25" "7" "0302ACP-5" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0402ACP-5" BY REFERENCE W-OGY(1) "6" "1" "04" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0502ACP-5" "ZZZZZZ-" "12" "33" "7" "0402ACP-5" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0502ACP-5" BY REFERENCE W-OGY(1) "6" "1" "05" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0602ACP-5" "ZZZZZZ-" "12" "41" "7" "0502ACP-5" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0602ACP-5" BY REFERENCE W-OGY(1) "6" "1" "06" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0702ACP-5" "ZZZZZZ-" "12" "49" "7" "0602ACP-5" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0702ACP-5" BY REFERENCE W-OGY(1) "6" "1" "07" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0802ACP-5" "ZZZZZZ-" "12" "57" "7" "0702ACP-5" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0802ACP-5" BY REFERENCE W-OGY(1) "6" "1" "08" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0902ACP-5" "ZZZZZZ-" "12" "65" "7" "0802ACP-5" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0902ACP-5" BY REFERENCE W-OGY(1) "6" "1" "09" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "1002ACP-5" "ZZZZZZ-" "12" "73" "7" "0902ACP-5" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1002ACP-5" BY REFERENCE W-OGY(1) "6" "1" "10" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-6" " " "0" "0" "14" "ACP-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NZKT" "ZZZZZ9-" "14" "73" "7" " " "ACP-6" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NZKT" BY REFERENCE W-NZKT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NZK" "ZZZZZ9-" "15" "W-C" "7" "D-NZKT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-NZK" BY REFERENCE W-NZK(1) "6" "1" BY REFERENCE A 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-7" " " "0" "0" "77" "ACP-6" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-7" "ZZZZZ9-" "16" "73" "7" " " "ACP-7" RETURNING RESU.
       CALL "SD_From" USING 
            "01ACP-7" BY REFERENCE W-TNST "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACP-7" " " "17" "60" "70" "01ACP-7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102ACP-7" "ZZZZZZ-" "17" "1" "7" " " "02ACP-7"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0102ACP-7" BY REFERENCE W-TNS(1) "6" "1" "01" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202ACP-7" "ZZZZZZ-" "17" "9" "7" "0102ACP-7" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0202ACP-7" BY REFERENCE W-TNS(1) "6" "1" "02" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302ACP-7" "ZZZZZZ-" "17" "17" "7" "0202ACP-7" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0302ACP-7" BY REFERENCE W-TNS(1) "6" "1" "03" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402ACP-7" "ZZZZZZ-" "17" "25" "7" "0302ACP-7" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0402ACP-7" BY REFERENCE W-TNS(1) "6" "1" "04" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0502ACP-7" "ZZZZZZ-" "17" "33" "7" "0402ACP-7" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0502ACP-7" BY REFERENCE W-TNS(1) "6" "1" "05" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0602ACP-7" "ZZZZZZ-" "17" "41" "7" "0502ACP-7" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0602ACP-7" BY REFERENCE W-TNS(1) "6" "1" "06" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0702ACP-7" "ZZZZZZ-" "17" "49" "7" "0602ACP-7" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0702ACP-7" BY REFERENCE W-TNS(1) "6" "1" "07" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0802ACP-7" "ZZZZZZ-" "17" "57" "7" "0702ACP-7" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0802ACP-7" BY REFERENCE W-TNS(1) "6" "1" "08" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0902ACP-7" "ZZZZZZ-" "17" "65" "7" "0802ACP-7" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0902ACP-7" BY REFERENCE W-TNS(1) "6" "1" "09" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "1002ACP-7" "ZZZZZZ-" "17" "73" "7" "0902ACP-7" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1002ACP-7" BY REFERENCE W-TNS(1) "6" "1" "10" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-8" " " "0" "0" "20" "ACP-7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NGYT" "ZZZZZ9-" "18" "73" "7" " " "ACP-8" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NGYT" BY REFERENCE W-NGYT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACP-8" " " "19" "60" "13" "D-NGYT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NGY" "S9" "19" "W-C" "6" " " "02ACP-8" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NGY" BY REFERENCE W-NGY(1) "6" "1" BY REFERENCE A 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NGY" "ZZZZZZ-" "19" "W-C" "7" "A-NGY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NGY" BY REFERENCE W-NGY(1) "6" "1" BY REFERENCE A 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-PRI" " " "21" "0" "60" "ACP-8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PRIM" " " "21" "60" "47" " " "ACP-PRI" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-PRIM" "X" "21" "1" "22" " " "D-PRIM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-PRIM" "X" "21" "31" "25" "01D-PRIM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PC" "9" "21" "22" "1" "D-PRIM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PC" BY REFERENCE W-PC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SKEY" "9" "21" "40" "6" "A-PC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SKEY" BY REFERENCE W-SKEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EKEY" "9" "21" "50" "6" "A-SKEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EKEY" BY REFERENCE W-EKEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "23" "61" "1" "ACP-PRI" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *DSP-CLE
       CALL "SD_Init" USING 
            "DSP-CLE" " " "0" "0" "672" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-ACT" " " "1" "0" "1" " " "DSP-CLE" RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLE-ACT" "X" "1" "60" "1" " " "CLE-ACT" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-01" " " "0" "0" "623" "CLE-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-1" " " "3" "0" "55" " " "CLE-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-1" "X" "3" "6" "6" " " "C-1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-1" "X" "3" "18" "48" "01C-1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-1" "X" "3" "76" "1" "02C-1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-2" " " "5" "0" "40" "C-1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-2" "X" "5" "3" "4" " " "C-2" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-2" "X" "5" "11" "4" "01C-2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-2" "X" "5" "19" "4" "02C-2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-2" "X" "5" "27" "4" "03C-2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-2" "X" "5" "35" "4" "04C-2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-2" "X" "5" "43" "4" "05C-2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-2" "X" "5" "51" "4" "06C-2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-2" "X" "5" "59" "4" "07C-2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-2" "X" "5" "67" "4" "08C-2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-2" "X" "5" "75" "4" "09C-2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-3" " " "0" "0" "88" "C-2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-3" "X" "7" "73" "8" " " "C-3" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-3" " " "8" "60" "80" "01C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-3" "X" "8" "1" "8" " " "02C-3" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-3" "X" "8" "9" "8" "0102C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0302C-3" "X" "8" "17" "8" "0202C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0402C-3" "X" "8" "25" "8" "0302C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0502C-3" "X" "8" "33" "8" "0402C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0602C-3" "X" "8" "41" "8" "0502C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0702C-3" "X" "8" "49" "8" "0602C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0802C-3" "X" "8" "57" "8" "0702C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0902C-3" "X" "8" "65" "8" "0802C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "1002C-3" "X" "8" "73" "8" "0902C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-4" " " "0" "0" "88" "C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-4" "X" "9" "73" "8" " " "C-4" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-4" " " "10" "60" "80" "01C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-4" "X" "10" "1" "8" " " "02C-4" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-4" "X" "10" "9" "8" "0102C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0302C-4" "X" "10" "17" "8" "0202C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0402C-4" "X" "10" "25" "8" "0302C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0502C-4" "X" "10" "33" "8" "0402C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0602C-4" "X" "10" "41" "8" "0502C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0702C-4" "X" "10" "49" "8" "0602C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0802C-4" "X" "10" "57" "8" "0702C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0902C-4" "X" "10" "65" "8" "0802C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "1002C-4" "X" "10" "73" "8" "0902C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-5" " " "0" "0" "88" "C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-5" "X" "11" "73" "8" " " "C-5" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-5" " " "12" "60" "80" "01C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-5" "X" "12" "1" "8" " " "02C-5" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-5" "X" "12" "9" "8" "0102C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0302C-5" "X" "12" "17" "8" "0202C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0402C-5" "X" "12" "25" "8" "0302C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0502C-5" "X" "12" "33" "8" "0402C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0602C-5" "X" "12" "41" "8" "0502C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0702C-5" "X" "12" "49" "8" "0602C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0802C-5" "X" "12" "57" "8" "0702C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0902C-5" "X" "12" "65" "8" "0802C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "1002C-5" "X" "12" "73" "8" "0902C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-6" " " "0" "0" "88" "C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-6" "X" "14" "73" "8" " " "C-6" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-6" " " "15" "60" "80" "01C-6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-6" "X" "15" "1" "8" " " "02C-6" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-6" "X" "15" "9" "8" "0102C-6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0302C-6" "X" "15" "17" "8" "0202C-6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0402C-6" "X" "15" "25" "8" "0302C-6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0502C-6" "X" "15" "33" "8" "0402C-6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0602C-6" "X" "15" "41" "8" "0502C-6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0702C-6" "X" "15" "49" "8" "0602C-6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0802C-6" "X" "15" "57" "8" "0702C-6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0902C-6" "X" "15" "65" "8" "0802C-6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "1002C-6" "X" "15" "73" "8" "0902C-6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-7" " " "0" "0" "88" "C-6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-7" "X" "16" "73" "8" " " "C-7" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-7" " " "17" "60" "80" "01C-7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-7" "X" "17" "1" "8" " " "02C-7" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-7" "X" "17" "9" "8" "0102C-7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0302C-7" "X" "17" "17" "8" "0202C-7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0402C-7" "X" "17" "25" "8" "0302C-7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0502C-7" "X" "17" "33" "8" "0402C-7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0602C-7" "X" "17" "41" "8" "0502C-7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0702C-7" "X" "17" "49" "8" "0602C-7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0802C-7" "X" "17" "57" "8" "0702C-7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0902C-7" "X" "17" "65" "8" "0802C-7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "1002C-7" "X" "17" "73" "8" "0902C-7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-8" " " "0" "0" "88" "C-7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-8" "X" "18" "73" "8" " " "C-8" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-8" " " "19" "60" "80" "01C-8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-8" "X" "19" "1" "8" " " "02C-8" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-8" "X" "19" "9" "8" "0102C-8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0302C-8" "X" "19" "17" "8" "0202C-8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0402C-8" "X" "19" "25" "8" "0302C-8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0502C-8" "X" "19" "33" "8" "0402C-8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0602C-8" "X" "19" "41" "8" "0502C-8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0702C-8" "X" "19" "49" "8" "0602C-8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0802C-8" "X" "19" "57" "8" "0702C-8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0902C-8" "X" "19" "65" "8" "0802C-8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "1002C-8" "X" "19" "73" "8" "0902C-8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-PRI" " " "21" "0" "47" "CLE-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-PRI" "X" "21" "1" "22" " " "C-PRI" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-PRI" "X" "21" "31" "25" "01C-PRI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-OKC" " " "23" "0" "1" "C-PRI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLE-OKC" "X" "23" "61" "1" " " "CLE-OKC" RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "118" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M00" "X" "24" "1" "22" " " "DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M02" "X" "24" "1" "28" "INV-M00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M03" "X" "24" "1" "20" "INV-M02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M04" "X" "24" "1" "30" "INV-M03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-D02" "X" "24" "1" "18" "INV-M04" " " RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       MEIN.
           PERFORM  INI-RTN    THRU  INI-EX.
           IF  CON-SW     NOT  =     0
               GO  TO  MR999
           END-IF
           PERFORM  ACT-RTN    THRU  ACT-EX.
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
           CALL "SD_Screen_Output" USING "SJN85I" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" JMST3_PNAME1 "SHARED" BY REFERENCE JMST3_IDLST "1"
            "JMST3-KEY" BY REFERENCE JMST3-KEY.
           CALL "DB_F_Open" USING
            "INPUT" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           CALL "DB_F_Open" USING
            "I-O" JNYZ_PNAME1 "SHARED" BY REFERENCE JNYZ_IDLST "1"
            "JNYZ-KEY" BY REFERENCE JNYZ-KEY.
           MOVE     SPACE      TO    W-AREA.
           INITIALIZE                W-AREA.
      *
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON6-KEY" BY REFERENCE JCON6-KEY.
           MOVE     SPACE      TO    JCON6-KEY.
           MOVE     6          TO    JCON6-01.
      *           READ     JCON       UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               MOVE  1           TO  CON-SW
               CALL "SD_Output" USING
                "INV-M00" INV-M00 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               GO  TO  INI-EX
           END-IF
           MOVE     JCON6-032  TO    W-TUKI.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
       INI-EX.
           EXIT.
      ******************************
      *    ＥＮＤ－ＲＴＮ          *
      *          ～終了処理～      *
      ******************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMST3_IDLST JMST3_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JNYZ_IDLST JNYZ_PNAME1.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
       END-EX.
           EXIT.
      *************************************
      *    ＡＣＴ－ＲＴＮ                 *
      *          ～画面入力＊更新処理～   *
      *************************************
       ACT-RTN.
           CALL "SD_Output" USING "DSP-CLE" DSP-CLE "p" RETURNING RESU.
           IF  W-ACT      =  1  OR  3
               CALL "SD_Output" USING
                "ACP-ACT" ACP-ACT "p" RETURNING RESU
               GO  TO  ACT-020
           END-IF.
       ACT-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-ACT "ACP-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "P9"
               GO  TO  ACT-EX
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-010
           END-IF
           IF  W-ACT      =  9
               GO  TO  ACT-EX
           END-IF
           CALL "SD_Output" USING "ACP-ACT" ACP-ACT "p" RETURNING RESU.
           IF  W-ACT      =  4
               CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU
               CALL "SD_Output" USING "D-PRIM" D-PRIM "p" RETURNING RESU
               GO  TO  ACT-070
           END-IF
           CALL "SD_Output" USING "C-PRI" C-PRI "p" RETURNING RESU.
           IF  W-ACT  NOT =  1  AND  3  AND  4
               GO  TO  ACT-010
           END-IF.
       ACT-020.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-010
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-020
           END-IF
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           MOVE     W-HCD      TO    HI-MHCD HI-HCD.
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
               GO  TO   ACT-020
           END-IF
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
      *
           PERFORM  JMS-RTN    THRU  JMS-EX.
       ACT-030.
           CALL "SD_Accept" USING BY REFERENCE A-SC "A-SC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-020
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-030
           END-IF
      *
           IF  W-SC       =   1
               MOVE  HI-SS1     TO  W-SDD
               MOVE  W-SIZ1     TO  W-SIZD
           END-IF
           IF  W-SC       =   2
               MOVE  HI-SS2     TO  W-SDD
               MOVE  W-SIZ2     TO  W-SIZD
           END-IF
           IF  W-SC       =   3
               MOVE  HI-SS3     TO  W-SDD
               MOVE  W-SIZ3     TO  W-SIZD
           END-IF
           IF  W-SC       =   4
               MOVE  HI-SS4     TO  W-SDD
               MOVE  0          TO  W-S(10)
               MOVE  W-SIZ4     TO  W-SIZD
           END-IF
           IF  W-SDD      =   ZERO
               CALL "SD_Output" USING
                "INV-M03" INV-M03 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO   ACT-020
           END-IF
           PERFORM  JZI-RTN    THRU  JZI-EX.
           CALL "SD_Output" USING "ACP-2" ACP-2 "p" RETURNING RESU.
      *
      *
           MOVE     ZERO       TO    INV-SW  W-OZKD  W-OGYD
                                             W-NZKD  W-NGYD.
           MOVE     ZERO       TO            W-OZKT  W-OGYT
                                             W-NZKT  W-NGYT.
           MOVE     W-HCD      TO    JNYZ-01.
           MOVE     W-SC       TO    JNYZ-02.
      *           READ     JNYZ       INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JNYZ_PNAME1 BY REFERENCE JNYZ-R " " RETURNING RET.
           IF  RET = 1
               MOVE  1          TO  INV-SW
               GO  TO  ACT-035
           END-IF
           PERFORM  ODS-RTN    THRU  ODS-EX
                    VARYING   A    FROM  1  BY  1
                    UNTIL     A    >   10.
       ACT-035.
           IF  W-ACT           =     3
               IF  INV-SW          =    1
                   CALL "SD_Output" USING
                    "INV-M04" INV-M04 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
                   GO  TO   ACT-030
               END-IF
           END-IF
           IF  INV-SW          =     0
               CALL "SD_Output" USING "ACP-3" ACP-3 "p" RETURNING RESU
               CALL "SD_Output" USING "ACP-5" ACP-5 "p" RETURNING RESU
           ELSE
               MOVE  ZERO      TO  W-OZKD  W-OGYD
               MOVE  ZERO      TO  W-OZKT  W-OGYT
               CALL "SD_Output" USING "C-3" C-3 "p" RETURNING RESU
               CALL "SD_Output" USING "C-5" C-5 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "ACP-4" ACP-4 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-7" ACP-7 "p" RETURNING RESU.
           IF  W-ACT           =     3
               CALL "SD_Output" USING "C-6" C-6 "p" RETURNING RESU
               CALL "SD_Output" USING "C-8" C-8 "p" RETURNING RESU
               GO  TO  ACT-090
           END-IF.
       ACT-040.
           MOVE     1          TO    A.
           MOVE     1          TO    W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
       ACT-045.
           IF  W-S(A)          =     0
               MOVE  ZERO       TO  W-NGY(A) W-NZK(A)
               GO  TO  ACT-055
           END-IF.
       ACT-050.
           CALL "SD_Accept" USING BY REFERENCE A-NGY "A-NGY" "S9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-060
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-050
           END-IF
           COMPUTE  W-NZK(A)   =  W-NGY(A)  +  W-TNS(A).
           CALL "SD_Output" USING "D-NZK" D-NZK "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NGY" D-NGY "p" RETURNING RESU.
       ACT-055.
           ADD      1          TO    A.
           ADD      8          TO    W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  A               >     10
               GO  TO  ACT-085
           END-IF
           GO  TO  ACT-045.
       ACT-060.
           SUBTRACT  1         FROM  A.
           IF  A               =     ZERO
               GO  TO  ACT-030
           END-IF
           SUBTRACT  8         FROM  W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  W-S(A)          =     0
               GO  TO  ACT-060
           END-IF
           GO  TO  ACT-050.
      *
       ACT-070.
           CALL "SD_Accept" USING BY REFERENCE A-PC "A-PC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-010
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-070
           END-IF
           IF  W-PC       >  1
               GO  TO  ACT-070
           END-IF
           IF  W-PC       =  0
               MOVE   "前月"    TO  H-MID
           ELSE
               MOVE   "現在"    TO  H-MID
           END-IF.
       ACT-075.
           CALL "SD_Accept" USING BY REFERENCE A-SKEY "A-SKEY" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-070
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-075
           END-IF.
       ACT-080.
           CALL "SD_Accept" USING BY REFERENCE A-EKEY "A-EKEY" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-075
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-080
           END-IF
           IF  W-SKEY     >  W-EKEY
               GO  TO  ACT-080
           END-IF
           GO  TO  ACT-090.
      *
       ACT-085.
           COMPUTE  W-NGYT  =  W-NGY(01)  +  W-NGY(02)  +  W-NGY(03)  +
                               W-NGY(04)  +  W-NGY(05)  +  W-NGY(06)  +
                               W-NGY(07)  +  W-NGY(08)  +  W-NGY(09)  +
                               W-NGY(10).
           COMPUTE  W-NZKT  =  W-NZK(01)  +  W-NZK(02)  +  W-NZK(03)  +
                               W-NZK(04)  +  W-NZK(05)  +  W-NZK(06)  +
                               W-NZK(07)  +  W-NZK(08)  +  W-NZK(09)  +
                               W-NZK(10).
           CALL "SD_Output" USING "D-NGYT" D-NGYT "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NZKT" D-NZKT "p" RETURNING RESU.
      *
       ACT-090.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               IF  W-ACT      =  1
                   GO  TO  ACT-060
               END-IF
           END-IF
           IF  ESTAT      =  "09"
               IF  W-ACT      =  3
                   GO  TO  ACT-030
               END-IF
           END-IF
           IF  ESTAT      =  "09"
               IF  W-ACT      =  4
                   GO  TO  ACT-080
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-090
           END-IF
           IF  W-OKC      =   9
               GO  TO  ACT-010
           END-IF
           IF  W-OKC  NOT =   1
               GO  TO  ACT-090
           END-IF.
      *
       ACT-100.
           IF  W-ACT      =   1
               PERFORM  UPD-RTN    THRU  UPD-EX
           END-IF
           IF  W-ACT      =   3
               PERFORM  DEL-RTN    THRU  DEL-EX
           END-IF
           IF  W-ACT      =   4
               PERFORM  PRI-RTN    THRU  PRI-EX
           END-IF
           GO  TO   ACT-RTN.
       ACT-EX.
           EXIT.
      *****　～ファイル更新処理～           *
       UPD-RTN.
           IF  INV-SW         =       1
               GO  TO  UPD-010
           END-IF
      *
           PERFORM  ZDS-RTN     THRU  ZDS-EX
                    VARYING   A    FROM  1  BY  1
                    UNTIL     A    >   10.
      *
      *           REWRITE  JNYZ-R     INVALID
      *///////////////
           CALL "DB_Update" USING
            JNYZ_PNAME1 JNYZ_LNAME JNYZ-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"         TO    ERR-M
               MOVE    "JNYZ"      TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           GO  TO  UPD-EX.
       UPD-010.
           MOVE     SPACE       TO    JNYZ-R.
           INITIALIZE                 JNYZ-R.
           MOVE     W-HCD       TO    JNYZ-01.
           MOVE     W-SC        TO    JNYZ-02.
           MOVE     W-TUKI      TO    JNYZ-99.
           MOVE     JNYZ-KEY    TO    ERR-K.
      *
           PERFORM  ZDS-RTN     THRU  ZDS-EX
                    VARYING   A    FROM  1  BY  1
                    UNTIL     A    >   10.
      *
           PERFORM  NJW-RTN     THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  UPD-010
           END-IF.
       UPD-EX.
           EXIT.
      *****　～ファイル削除～           *
       DEL-RTN.
      *           DELETE   JNYZ       INVALID
      *///////////////
           CALL "DB_Delete" USING JNYZ_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE    "D"         TO    ERR-M
               MOVE    "JNYZ"      TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF.
       DEL-EX.
           EXIT.
      *****　　修正前　セット                    *
       ODS-RTN.
           MOVE  JNYZ-0311(A)  TO  W-OZK(A).
           COMPUTE  W-OGY(A)   =  W-OZK(A)  -  W-TNS(A).
           ADD   W-OZK(A)      TO  W-OZKT.
           ADD   W-OGY(A)      TO  W-OGYT.
       ODS-EX.
           EXIT.
      *****　　修正後　セット                    *
       ZDS-RTN.
           MOVE     W-NZK(A)   TO    JNYZ-0311(A).
       ZDS-EX.
           EXIT.
      *****  入庫予定残ファイル　ＷＲＩＴＥ　
       NJW-RTN.
           MOVE     0          TO    WRI-SW.
      *           WRITE  JNYZ-R     INVALID
      *//////////////
           CALL "DB_Insert" USING
            JNYZ_PNAME1 JNYZ_LNAME JNYZ-R RETURNING RET.
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
               MOVE    "JNYZ"      TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           MOVE     2          TO    WRI-SW.
           GO  TO  NJW-EX.
       NJW-020.
           MOVE     1          TO    WRI-SW.
           MOVE    "W"         TO    ERR-M.
           MOVE    "JNYZ"      TO    ERR-F.
           MOVE     JNYZ-KEY   TO    ERR-K.
           MOVE     ERR-STAT   TO    ERR-FLG.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE JNYZ_IDLST JNYZ_PNAME1.
           CALL "SD_Output" USING
            " " "ｴﾘｱ ｶｸﾁｮｳｺﾞ,ｻｲｶｲｷｰ ｦ ｵｽ!" "STOP" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" JNYZ_PNAME1 "SHARED" BY REFERENCE JNYZ_IDLST "1"
            "JNYZ-KEY" BY REFERENCE JNYZ-KEY.
       NJW-EX.
           EXIT.
      *****  受注マスタ　預りチェック
       JMS-RTN.
           MOVE     0          TO    AZ-SW.
           MOVE     SPACE      TO    JMST3-KEY.
           MOVE     W-HCD      TO    JMST3-03.
      *           START    JMST3      KEY  NOT  <  JMST3-KEY   INVALID
      *///////////////
           CALL "DB_Start" USING
            JMST3_PNAME1 "JMST3-KEY" " NOT < " JMST3-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  JMS-EX
           END-IF.
       JMS-010.
      *           READ     JMST3      NEXT  UNLOCK    AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST3_PNAME1 BY REFERENCE JMST3-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  JMS-EX
           END-IF
           IF  W-HCD      NOT  =  JMST3-03
               GO  TO  JMS-EX
           END-IF
           IF  JMST3-01        =  0   OR   6
               GO  TO  JMS-010
           END-IF
           MOVE     1          TO    AZ-SW.
           CALL "SD_Output" USING "INV-D02" INV-D02 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
       JMS-EX.
           EXIT.
      *****  倉別在庫マスタ　入庫数セット
       JZI-RTN.
           MOVE     ZERO       TO    W-TNSD.
           MOVE     9          TO    NJZAI-01.
           MOVE     W-HCD      TO    NJZAI-02.
           MOVE     W-SC       TO    NJZAI-03.
      *           READ     NJZAI      UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  JZI-EX
           END-IF
           MOVE     NJZAI-0711(01)  TO  W-TNS(01).
           MOVE     NJZAI-0711(02)  TO  W-TNS(02).
           MOVE     NJZAI-0711(03)  TO  W-TNS(03).
           MOVE     NJZAI-0711(04)  TO  W-TNS(04).
           MOVE     NJZAI-0711(05)  TO  W-TNS(05).
           MOVE     NJZAI-0711(06)  TO  W-TNS(06).
           MOVE     NJZAI-0711(07)  TO  W-TNS(07).
           MOVE     NJZAI-0711(08)  TO  W-TNS(08).
           MOVE     NJZAI-0711(09)  TO  W-TNS(09).
           MOVE     NJZAI-0711(10)  TO  W-TNS(10).
           COMPUTE  W-TNST  =  W-TNS(01)  +  W-TNS(02)  +  W-TNS(03)  +
                               W-TNS(04)  +  W-TNS(05)  +  W-TNS(06)  +
                               W-TNS(07)  +  W-TNS(08)  +  W-TNS(09)  +
                               W-TNS(10).
       JZI-EX.
           EXIT.
      *****  作　　表
       PRI-RTN.
           MOVE     SPACE      TO    JNYZ-KEY.
           MOVE     W-SKEY     TO    JNYZ-01.
      *           START    JNYZ       KEY  NOT  <  JNYZ-KEY    INVALID
      *///////////////
           CALL "DB_Start" USING
            JNYZ_PNAME1 "JNYZ-KEY" " NOT < " JNYZ-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-M04" INV-M04 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               GO  TO  PRI-EX
           END-IF.
       PRI-005.
      *           READ     JNYZ       NEXT  UNLOCK    AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JNYZ_PNAME1 BY REFERENCE JNYZ-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-M04" INV-M04 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               GO  TO  PRI-EX
           END-IF
           IF  JNYZ-01         >  W-EKEY
               CALL "SD_Output" USING
                "INV-M04" INV-M04 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               GO  TO  PRI-EX
           END-IF
           PERFORM  PSS-RTN    THRU  PSS-EX.
           IF  W-TNSD          =  ZERO
               GO  TO  PRI-005
           END-IF.
       PRI-007.
           CALL "PR_Open" RETURNING RESP.
           ACCEPT  H-DATE   FROM  DATE.
           MOVE  ZERO       TO  W-PAGE.
           PERFORM  HED-010    THRU  HED-EX.
           MOVE  SPACE      TO  W-P.
           MOVE  SPACE      TO  P-NAME.
       PRI-010.
           MOVE  JNYZ-01    TO  W-HCDD.
           MOVE  ZERO       TO  W-SUT.
           MOVE  W-HCDD     TO  HI-MHCD HI-HCD.
      *           READ     HI2-M       UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE      TO  HI-NAME
               MOVE  "　＊　マスター　なし　＊"  TO  HI-NAME
           END-IF
           MOVE  W-HCDD     TO  P-HCD.
           MOVE  HI-NAME    TO  P-NAME.
       PRI-020.
           COMPUTE  W-SUT   =  JNYZ-0311(01) + JNYZ-0311(02) +
                               JNYZ-0311(03) + JNYZ-0311(04) +
                               JNYZ-0311(05) + JNYZ-0311(06) +
                               JNYZ-0311(07) + JNYZ-0311(08) +
                               JNYZ-0311(09) + JNYZ-0311(10) + W-SUT.
           MOVE  ZERO            TO  W-SDD.
           IF  JNYZ-02    =   1
               MOVE  HI-SS1     TO  W-SDD
           END-IF
           IF  JNYZ-02    =   2
               MOVE  HI-SS2     TO  W-SDD
           END-IF
           IF  JNYZ-02    =   3
               MOVE  HI-SS3     TO  W-SDD
           END-IF
           IF  JNYZ-02    =   4
               MOVE  HI-SS4     TO  W-SDD
               MOVE  0          TO  W-S(10)
           END-IF
           MOVE  JNYZ-02         TO  P-SC.
           IF  W-S(01)    NOT  =  ZERO
               MOVE  JNYZ-0311(01)   TO  P-SU(01)
           END-IF
           IF  W-S(02)    NOT  =  ZERO
               MOVE  JNYZ-0311(02)   TO  P-SU(02)
           END-IF
           IF  W-S(03)    NOT  =  ZERO
               MOVE  JNYZ-0311(03)   TO  P-SU(03)
           END-IF
           IF  W-S(04)    NOT  =  ZERO
               MOVE  JNYZ-0311(04)   TO  P-SU(04)
           END-IF
           IF  W-S(05)    NOT  =  ZERO
               MOVE  JNYZ-0311(05)   TO  P-SU(05)
           END-IF
           IF  W-S(06)    NOT  =  ZERO
               MOVE  JNYZ-0311(06)   TO  P-SU(06)
           END-IF
           IF  W-S(07)    NOT  =  ZERO
               MOVE  JNYZ-0311(07)   TO  P-SU(07)
           END-IF
           IF  W-S(08)    NOT  =  ZERO
               MOVE  JNYZ-0311(08)   TO  P-SU(08)
           END-IF
           IF  W-S(09)    NOT  =  ZERO
               MOVE  JNYZ-0311(09)   TO  P-SU(09)
           END-IF
           IF  W-S(10)    NOT  =  ZERO
               MOVE  JNYZ-0311(10)   TO  P-SU(10)
           END-IF.
       PRI-030.
      *           READ     JNYZ       NEXT  UNLOCK    AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JNYZ_PNAME1 BY REFERENCE JNYZ-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  PRI-050
           END-IF
           IF  JNYZ-01         >  W-EKEY
               GO  TO  PRI-050
           END-IF
           PERFORM  PSS-RTN    THRU  PSS-EX.
           IF  W-TNSD          =  ZERO
               GO  TO  PRI-030
           END-IF
           IF  JNYZ-01    NOT  =  W-HCDD
               MOVE  W-SUT               TO  P-SUT
           END-IF
           PERFORM  PWI-RTN    THRU  PWI-EX.
           IF  JNYZ-01    NOT  =  W-HCDD
               GO  TO  PRI-010
           END-IF
           GO  TO  PRI-020.
       PRI-050.
           MOVE  W-SUT               TO  P-SUT.
           PERFORM  PWI-RTN    THRU  PWI-EX.
           CALL "PR_Close" RETURNING RESP.
       PRI-EX.
           EXIT.
      *****  見出し　印字
       HED-RTN.
           MOVE     SPACE      TO    SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       HED-010.
           ADD      1        TO  W-PAGE.
           MOVE     W-PAGE   TO  H-PAGE.
           MOVE     SPACE    TO  SP-R.
           MOVE     HEAD1    TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     SPACE    TO  SP-R.
           MOVE     HEAD2    TO  SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     SPACE    TO  SP-R.
           MOVE     HEAD3    TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     SPACE    TO  SP-R.
           MOVE     HEAD4    TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     SPACE    TO  SP-R.
           MOVE     HEAD5    TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     SPACE    TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       HED-EX.
           EXIT.
      *****  明細　印字
       PWI-RTN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  60
               MOVE  W-HCDD              TO  P-HCD
               MOVE  HI-NAME             TO  P-NAME
               PERFORM  HED-RTN   THRU  HED-EX
           END-IF
           MOVE     SPACE      TO    SP-R.
           MOVE     W-P        TO    SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     SPACE      TO    SP-R.
           MOVE     SPACE      TO    W-P.
           MOVE     SPACE      TO    P-NAME.
       PWI-EX.
           EXIT.
      *****  作表　数量セット
       PSS-RTN.
           IF  W-PC            =  0
               MOVE     ZERO            TO  W-TNSD
               MOVE     JNYZ-03         TO  W-TNSD
               GO  TO  PSS-010
           END-IF
           MOVE     JNYZ-01         TO  W-HCD.
           MOVE     JNYZ-02         TO  W-SC.
           PERFORM  JZI-RTN    THRU  JZI-EX.
           COMPUTE  W-TNS(01)  =  JNYZ-0311(01)  -  W-TNS(01).
           COMPUTE  W-TNS(02)  =  JNYZ-0311(02)  -  W-TNS(02).
           COMPUTE  W-TNS(03)  =  JNYZ-0311(03)  -  W-TNS(03).
           COMPUTE  W-TNS(04)  =  JNYZ-0311(04)  -  W-TNS(04).
           COMPUTE  W-TNS(05)  =  JNYZ-0311(05)  -  W-TNS(05).
           COMPUTE  W-TNS(06)  =  JNYZ-0311(06)  -  W-TNS(06).
           COMPUTE  W-TNS(07)  =  JNYZ-0311(07)  -  W-TNS(07).
           COMPUTE  W-TNS(08)  =  JNYZ-0311(08)  -  W-TNS(08).
           COMPUTE  W-TNS(09)  =  JNYZ-0311(09)  -  W-TNS(09).
           COMPUTE  W-TNS(10)  =  JNYZ-0311(10)  -  W-TNS(10).
       PSS-010.
           COMPUTE  W-TNST  =  W-TNS(01)  +  W-TNS(02)  +  W-TNS(03)  +
                               W-TNS(04)  +  W-TNS(05)  +  W-TNS(06)  +
                               W-TNS(07)  +  W-TNS(08)  +  W-TNS(09)  +
                               W-TNS(10).
       PSS-EX.
           EXIT.
      *
           COPY    LPMSG.

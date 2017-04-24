       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBM110.
      *********************************************************
      *    PROGRAM         :  材料マスターメンテナンス        *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCBM11                          *
      *        変更　　　  :  62/06/04                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(024) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　材料マスター　プルーフリスト　　＊＊＊".
           02  F              PIC  X(011) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(010) VALUE "  ｺｰﾄﾞ    ".
           02  F              PIC  N(012) VALUE
                "材　　料　　名　　・　　".
           02  F              PIC  X(001) VALUE "[".
           02  F              PIC  N(006) VALUE "加工前材料名".
           02  F              PIC  X(001) VALUE "]".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(002) VALUE "用途".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "在庫".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "製品".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単位".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "最終単価".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単位".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(006) VALUE "原価計算単価".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "部門".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　最終仕入日".
       01  W-P.
           02  P-15K          PIC  X(005).
           02  P-F1           PIC  X(001).
           02  P-KEY          PIC  9(006).
           02  P-R1           PIC  X(001).
           02  F              PIC  X(001).
           02  P-F2           PIC  X(001).
           02  P-NA           PIC  N(024).
           02  P-R2           PIC  X(001).
           02  F              PIC  X(002).
           02  P-YC           PIC  9(001).
           02  F              PIC  X(003).
           02  P-ZC           PIC  9(001).
           02  F              PIC  X(003).
           02  P-SC           PIC  9(001).
           02  F              PIC  X(003).
           02  P-TC1          PIC  9(001).
           02  P-T            PIC -----,--9.99.
           02  F              PIC  X(003).
           02  P-TC2          PIC  9(001).
           02  P-YT           PIC -----,--9.99.
           02  F              PIC  X(003).
           02  P-BKC          PIC  9(002).
           02  F              PIC  X(002).
           02  P-ED           PIC 99/99/99.
           02  P-20K          PIC  X(005).
       01  W-R.
           02  W-KEY          PIC  9(006).
           02  W-KEYD  REDEFINES W-KEY.
             03  W-KEY1       PIC  9(001).
             03  W-KEY2       PIC  9(002).
             03  W-KEY20 REDEFINES W-KEY2.
               04  W-KEY21    PIC  9(001).
               04  W-KEY22    PIC  9(001).
             03  W-KEY3       PIC  9(003).
           02  W-NA           PIC  N(024).
           02  W-YC           PIC  9(001).
           02  W-ZC           PIC  9(001).
           02  W-SC           PIC  9(001).
           02  W-TC1          PIC  9(001).
           02  W-T            PIC S9(006)V9(02).
           02  W-TC2          PIC  9(001).
           02  W-YT           PIC S9(006)V9(02).
           02  W-KJCD         PIC  9(006).
           02  W-KTN          PIC S9(006)V9(02).
           02  F              PIC  X(022).
           02  W-BKC          PIC  9(002).
           02  W-BKNO         PIC  9(002).
           02  F              PIC  X(007).
           02  W-YMD          PIC  9(006).
       01  W-D.
           02  W-ACT          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-KNA          PIC  N(024).
           02  WK-KEY.
             03  WK-JCD1      PIC  9(006).
             03  WK-JCD2      PIC  9(006).
           02  W-JCD.
             03  W-JCD1       PIC  9(006).
             03  W-JCD2       PIC  9(006).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  CHK            PIC  9(001).
           02  W-PC           PIC  9(001) VALUE ZERO.
           02  W-YN           PIC  N(020) VALUE
                "　　　　原材料　補助材料荷造材料製品仕入".
           02  W-YCND  REDEFINES W-YN.
             03  W-YCN   OCCURS  5  PIC  N(004).
           02  W-YCD          PIC  9(001).
           02  W-ZN           PIC  N(012) VALUE
                "在庫とる　　在庫とらない".
           02  W-ZCND  REDEFINES W-ZN.
             03  W-ZCN   OCCURS  2  PIC  N(006).
           02  W-ZCD          PIC  9(001).
           02  W-SN.
             03  F            PIC  N(015) VALUE
                  "　　　　　　　　　　　　　　　".
             03  F            PIC  N(015) VALUE
                  "地下足袋　　　　　　　　　　　".
             03  F            PIC  N(015) VALUE
                  "教育シューズ　　　　　　　　　".
             03  F            PIC  N(015) VALUE
                  "Ｐ．Ｂ．ブランド　スタンダード".
             03  F            PIC  N(015) VALUE
                  "Ｐ．Ｂ．ブランド　カジュアル　".
             03  F            PIC  N(015) VALUE
                  "自社ブランド　スタンダード　　".
             03  F            PIC  N(015) VALUE
                  "自社ブランド　カジュアル　　　".
             03  F            PIC  N(015) VALUE
                  "長　靴　他　　　　　　　　　　".
             03  F            PIC  N(015) VALUE
                  "　　　　　　　　　　　　　　　".
             03  F            PIC  N(015) VALUE
                  "　　　　　　　　　　　　　　　".
             03  F            PIC  N(015) VALUE
                  "輸　入　　　　　　　　　　　　".
             03  F            PIC  N(015) VALUE
                  "仕　入　　　　　　　　　　　　".
             03  F            PIC  N(015) VALUE
                  "工　品　　　　　　　　　　　　".
             03  F            PIC  N(015) VALUE
                  "その他　　　　　　　　　　　　".
           02  W-SCND  REDEFINES W-SN.
             03  W-SCN   OCCURS 14  PIC  N(015).
           02  W-SCD          PIC  9(002).
           02  W-TN           PIC  N(010) VALUE
                "　㍍足ケ㎏㍑枚ｓ反　".
           02  W-TCND  REDEFINES W-TN.
             03  W-TCN   OCCURS 10  PIC  N(001).
           02  W-TCD1         PIC  9(002).
           02  W-TCD2         PIC  9(002).
           02  W-FILE         PIC  X(013).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIJM.
           COPY LIJTM.
           COPY BUMONF.
           COPY LSPF.
      *FD  JK-M
       01  JK-M_KBM110.
           02  JK-M_PNAME1    PIC  X(003) VALUE "JKM".
           02  F              PIC  X(001).
           02  JK-M_LNAME     PIC  X(011) VALUE "JK-M_KBM110".
           02  F              PIC  X(001).
           02  JK-M_KEY1      PIC  X(100) VALUE SPACE.
           02  JK-M_KEY2      PIC  X(100) VALUE SPACE.
           02  JK-M_SORT      PIC  X(100) VALUE SPACE.
           02  JK-M_IDLST     PIC  X(100) VALUE SPACE.
           02  JK-M_RES       USAGE  POINTER.
       01  JK-R.
           02  JK-KEY.
             03  JK-JCD1      PIC  9(006).
             03  JK-JCD2      PIC  9(006).
           02  JK-KEY1  REDEFINES JK-KEY  PIC  9(012).
           02  F              PIC  X(004).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  FILLER.
             03  A-KEY   PIC  9(006).
             03  A-NA    PIC  N(024).
           02  A-YC    PIC  9(001).
           02  A-ZC    PIC  9(001).
           02  A-SC    PIC  9(001).
           02  A-TC1   PIC  9(001).
           02  A-T     PIC  9(006)V9(02).
           02  A-TC2   PIC  9(001).
           02  A-YT    PIC  9(006)V9(02).
           02  A-KJCD  PIC  9(006).
           02  A-BKC   PIC  9(002).
           02  FILLER.
             03  A-JCD1  PIC  9(006).
             03  A-JCD2  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-YCN   PIC  N(004).
           02  D-ZCN   PIC  N(006).
           02  FILLER.
             03  D-SCN   PIC  N(015).
             03  D-SCNC  PIC  X(033) VALUE
                  "                                 ".
           02  D-TCN1  PIC  N(001).
           02  D-T     PIC ZZZZZ9.99 .
           02  FILLER.
             03  D-TCN2  PIC  N(001).
             03  D-TCNC  PIC  X(005) VALUE "     ".
           02  D-YT    PIC ZZZZZ9.99 .
           02  FILLER.
             03  D-KNA   PIC  N(024).
             03  D-KJCD.
               04  FILLER  PIC  X(038) VALUE
                    "                                      ".
               04  FILLER  PIC  X(017) VALUE
                    "                 ".
           02  D-BKN   PIC  N(010).
           02  D-PM    PIC  X(044) VALUE
                "[  材料ｺｰﾄﾞ        より        まで打出し  ]".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(040) VALUE
                  "***  材料コード ｴﾗｰ  ***                ".
             03  E-ME2   PIC  X(040) VALUE
                  "***  J-M ﾅｼ  ***                        ".
             03  E-ME3   PIC  X(040) VALUE
                  "***  J-M ﾄｳﾛｸｽﾞﾐ  ***                   ".
             03  E-ME4   PIC  X(040) VALUE
                  "***  JT-M ﾅｼ  ***                       ".
             03  E-ME5   PIC  X(040) VALUE
                  "***  JT-M ﾄｳﾛｸｽﾞﾐ  ***                  ".
             03  E-ME6   PIC  X(040) VALUE
                  "***  PROGRAM ｴﾗｰ  ***                   ".
             03  E-ME7   PIC  X(040) VALUE
                  "***  J-M WRITE ｴﾗｰ  ***                 ".
             03  E-ME8   PIC  X(040) VALUE
                  "***  J-M REWRITE ｴﾗｰ  ***               ".
             03  E-ME9   PIC  X(040) VALUE
                  "***  J-M DELETE ｴﾗｰ  ***                ".
             03  E-ME10  PIC  X(040) VALUE
                  "***  JK-M WRITE ｴﾗｰ  ***                ".
             03  E-ME12  PIC  X(040) VALUE
                  "***  JK-M DELETE ｴﾗｰ  ***               ".
             03  E-ME13  PIC  X(040) VALUE
                  "***  JT-M WRITE ｴﾗｰ  ***                ".
             03  E-ME14  PIC  X(040) VALUE
                  "***  JT-M REWRITE ｴﾗｰ  ***              ".
             03  E-ME15  PIC  X(040) VALUE
                  "***  JT-M DELETE ｴﾗｰ  ***               ".
             03  E-ME16  PIC  X(040) VALUE
                  "***  ﾄｳｹｲ ｺｳﾓｸ ｶﾞ ZERO ﾃﾞ ﾅｲ  ***       ".
             03  E-ME17  PIC  X(040) VALUE
                  "***  ﾌﾞﾓﾝ ｸﾌﾞﾝ CHECK  ***               ".
             03  E-ME18  PIC  X(040) VALUE
                  "***  ﾌﾞﾓﾝ ﾅｼ  ***                       ".
             03  E-ME20  PIC  X(040) VALUE
                  "***  キャンセル  ***                    ".
             03  E-JCD1  PIC  9(006).
             03  E-JCD2  PIC  9(006).
             03  E-KEY   PIC  9(012).
           COPY LSSEM.
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "93" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "52" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "6" "0" "54" "A-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "9" "6" "5" "6" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NA" "N" "6" "12" "48" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NA" BY REFERENCE W-NA "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YC" "9" "8" "16" "1" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YC" BY REFERENCE W-YC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ZC" "9" "9" "16" "1" "A-YC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ZC" BY REFERENCE W-ZC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SC" "9" "10" "16" "1" "A-ZC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SC" BY REFERENCE W-SC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TC1" "9" "11" "16" "1" "A-SC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TC1" BY REFERENCE W-TC1 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-T" "9" "12" "17" "6" "A-TC1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-T" BY REFERENCE W-T "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TC2" "9" "13" "16" "1" "A-T" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TC2" BY REFERENCE W-TC2 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YT" "9" "14" "17" "6" "A-TC2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YT" BY REFERENCE W-YT "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KJCD" "9" "15" "16" "6" "A-YT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KJCD" BY REFERENCE W-KJCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BKC" "9" "16" "16" "2" "A-KJCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BKC" BY REFERENCE W-BKC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "12C-ACP" " " "19" "0" "12" "A-BKC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JCD1" "9" "19" "26" "6" " " "12C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JCD1" BY REFERENCE W-JCD1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JCD2" "9" "19" "38" "6" "A-JCD1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JCD2" BY REFERENCE W-JCD2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "42" "1" "12C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "277" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-YCN" "N" "8" "19" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-YCN" BY REFERENCE W-YCN(1) "8" "1" BY REFERENCE W-YCD 8
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZCN" "N" "9" "19" "12" "D-YCN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-ZCN" BY REFERENCE W-ZCN(1) "12" "1" BY REFERENCE W-ZCD 12
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-DSP" " " "10" "0" "63" "D-ZCN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SCN" "N" "10" "19" "30" " " "03C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SCN" BY REFERENCE W-SCN(1) "30" "1" BY REFERENCE W-SCD 30
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SCNC" "X" "10" "16" "33" "D-SCN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TCN1" "N" "11" "19" "2" "03C-DSP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-TCN1" BY REFERENCE W-TCN(1) "2" "1" BY REFERENCE W-TCD1 2
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-T" "ZZZZZ9.99" "12" "17" "9" "D-TCN1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-T" BY REFERENCE W-T "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-DSP" " " "13" "0" "7" "D-T" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TCN2" "N" "13" "19" "2" " " "06C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-TCN2" BY REFERENCE W-TCN(1) "2" "1" BY REFERENCE W-TCD2 2
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TCNC" "X" "13" "16" "5" "D-TCN2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-YT" "ZZZZZ9.99" "14" "17" "9" "06C-DSP" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-YT" BY REFERENCE W-YT "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-DSP" " " "15" "0" "103" "D-YT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KNA" "N" "15" "23" "48" " " "08C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-KNA" BY REFERENCE W-KNA "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KJCD" " " "15" "19" "55" "D-KNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-KJCD" "X" "15" "16" "38" " " "D-KJCD" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-KJCD" "X" "15" "54" "17" "01D-KJCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BKN" "N" "16" "21" "20" "08C-DSP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-BKN" BY REFERENCE BNMNMN "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PM" "X" "19" "14" "44" "D-BKN" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "744" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "744" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "40" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "40" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "40" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "40" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "40" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "40" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "40" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "40" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "40" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "40" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "40" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "40" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME14" "X" "24" "15" "40" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME15" "X" "24" "15" "40" "E-ME14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME16" "X" "24" "15" "40" "E-ME15" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME17" "X" "24" "15" "40" "E-ME16" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME18" "X" "24" "15" "40" "E-ME17" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME20" "X" "24" "15" "40" "E-ME18" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JCD1" "9" "24" "45" "6" "E-ME20" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JCD1" BY REFERENCE J-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JCD2" "9" "24" "45" "6" "E-JCD1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JCD2" BY REFERENCE JT-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "9" "24" "45" "12" "E-JCD2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE JK-KEY "12" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-020.
           COPY LIBCPR.
           MOVE DATE-05R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "I-O" JT-M_PNAME1 "SHARED" BY REFERENCE JT-M_IDLST "1"
            "JT-KEY" BY REFERENCE JT-KEY.
           CALL "DB_F_Open" USING
            "I-O" JK-M_PNAME1 "SHARED" BY REFERENCE JK-M_IDLST "1"
            "JK-KEY" BY REFERENCE JK-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BNM_PNAME1 "SHARED" BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
       M-040.
           CALL "SD_Screen_Output" USING "SCBM11" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-ACT = 9
               GO TO M-980
           END-IF
           IF  W-ACT = 4
               CALL "SD_Output" USING "D-PM" D-PM "p" RETURNING RESU
               GO TO M-800
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO TO M-040
           END-IF.
       M-060.
           CALL "SD_Screen_Output" USING "SCBM11" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           MOVE SPACE TO W-R.
           INITIALIZE W-R.
           MOVE ALL "　" TO W-NA.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF
           IF  W-KEY1 > 7
               GO TO M-060
           END-IF
           MOVE W-KEY TO J-KEY.
      *           READ J-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-160
           END-IF
           IF  W-ACT = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-060
           END-IF
           MOVE W-KEY TO JT-KEY.
      *           READ JT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JT-M_PNAME1 BY REFERENCE JT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-060
           END-IF
           MOVE J-R TO W-R.
           IF  W-KJCD = ZERO
               GO TO M-100
           END-IF
           MOVE W-KJCD TO J-KEY.
      *           READ J-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R " " RETURNING RET.
           IF  RET = 1
               MOVE "＊＊＊　Ｊ－Ｍ　無し　＊＊＊　" TO J-NAME
           END-IF
           MOVE J-NAME TO W-KNA.
      *
           MOVE W-KEY TO J-KEY.
      *           READ J-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF.
       M-100.
           COMPUTE W-YCD = W-YC + 1.
           COMPUTE W-ZCD = W-ZC + 1.
           IF  W-KEY1 = 0 OR 6
               COMPUTE W-SCD = W-SC + 1
           ELSE
               COMPUTE W-SCD = W-SC + 10
           END-IF
           COMPUTE W-TCD1 = W-TC1 + 1.
           COMPUTE W-TCD2 = W-TC2 + 1.
           MOVE ZERO TO BNM-KEY.
           MOVE W-BKC TO BNM-BU.
      *           READ BNM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BNM_PNAME1 BY REFERENCE BNM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO BNMNMN
           END-IF
           CALL "SD_Output" USING "A-NA" A-NA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-YC" A-YC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-YCN" D-YCN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ZC" A-ZC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-ZCN" D-ZCN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SC" A-SC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SCN" D-SCN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TC1" A-TC1 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TCN1" D-TCN1 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TC2" A-TC2 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TCN2" D-TCN2 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-YT" D-YT "p" RETURNING RESU.
           CALL "SD_Output" USING "A-BKC" A-BKC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BKN" D-BKN "p" RETURNING RESU.
           IF  W-KJCD NOT = ZERO
               CALL "SD_Output" USING "A-KJCD" A-KJCD "p" RETURNING RESU
               CALL "SD_Output" USING "D-KNA" D-KNA "p" RETURNING RESU
           END-IF
           MOVE ZERO TO WK-KEY.
           IF  W-KJCD = ZERO
               GO TO M-140
           END-IF
      *
           MOVE W-KJCD TO JK-JCD1.
           MOVE W-KEY TO JK-JCD2.
      *           READ JK-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JK-M_PNAME1 BY REFERENCE JK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-140
           END-IF
           MOVE JK-KEY TO WK-KEY.
       M-140.
           IF  W-ACT NOT = 3
               GO TO M-180
           END-IF
           IF  ZERO NOT = JT-SSU OR JT-SIK OR JT-HSU OR JT-ZKS OR JT-ZKK
               CALL "SD_Output" USING
                "E-ME16" E-ME16 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
           END-IF
           GO TO M-540.
       M-160.
           MOVE ZERO TO WK-KEY.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-060
           END-IF
           MOVE W-KEY TO JT-KEY.
      *           READ JT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JT-M_PNAME1 BY REFERENCE JT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-180
           END-IF
           CALL "SD_Output" USING "E-ME5" E-ME5 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           GO TO M-060.
       M-180.
           CALL "SD_Output" USING "A-NA" A-NA "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-NA "A-NA" "N" "48"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-180
           END-IF.
       M-200.
           CALL "SD_Accept" USING BY REFERENCE A-YC "A-YC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-200
           END-IF
           IF  W-YC NOT = 1 AND 2 AND 3 AND 4
               GO TO M-200
           END-IF
           COMPUTE W-YCD = W-YC + 1.
           CALL "SD_Output" USING "D-YCN" D-YCN "p" RETURNING RESU.
           IF  W-KEY2 > 89 AND < 95
               IF  W-YC NOT = 4
                   GO TO M-200
               END-IF
           END-IF.
       M-220.
           CALL "SD_Accept" USING BY REFERENCE A-ZC "A-ZC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-200
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-220
           END-IF
           IF  W-ZC > 1
               GO TO M-220
           END-IF
           COMPUTE W-ZCD = W-ZC + 1.
           CALL "SD_Output" USING "D-ZCN" D-ZCN "p" RETURNING RESU.
           IF  W-KEY1 = 0 OR 6
               GO TO M-240
           END-IF
           IF  W-KEY21 NOT = 9
               MOVE 0 TO W-SC
               GO TO M-260
           END-IF.
       M-240.
           CALL "SD_Accept" USING BY REFERENCE A-SC "A-SC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-220
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-240
           END-IF
           IF  W-SC > 7
               GO TO M-240
           END-IF
           IF  W-KEY1 = 0 OR 6
               GO TO M-260
           END-IF
           IF  W-SC > 4
               GO TO M-240
           END-IF.
       M-260.
           IF  W-KEY1 = 0 OR 6
               COMPUTE W-SCD = W-SC + 1
           ELSE
               COMPUTE W-SCD = W-SC + 10
           END-IF
           CALL "SD_Output" USING "D-SCN" D-SCN "p" RETURNING RESU.
       M-280.
           CALL "SD_Accept" USING BY REFERENCE A-TC1 "A-TC1" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = BTB
               GO TO M-300
           END-IF
           IF  W-KEY1 = 0 OR 6
               GO TO M-240
           END-IF
           IF  W-KEY21 = 9
               GO TO M-240
           END-IF
           GO TO M-220.
       M-300.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-280
           END-IF
           COMPUTE W-TCD1 = W-TC1 + 1.
           CALL "SD_Output" USING "D-TCN1" D-TCN1 "p" RETURNING RESU.
       M-320.
           CALL "SD_Accept" USING BY REFERENCE A-T "A-T" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-280
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-320
           END-IF
           CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU.
           IF  W-KEY1 = 2
               MOVE ZERO TO W-TC2 W-YT
               CALL "SD_Output" USING "D-TCNC" D-TCNC "p" RETURNING RESU
               CALL "SD_Output" USING "D-YT" D-YT "p" RETURNING RESU
               GO TO M-380
           END-IF
           IF  W-KEY1 NOT = 0 AND 4 AND 5
               MOVE ZERO TO W-TC2 W-YT W-KJCD
               CALL "SD_Output" USING "D-TCNC" D-TCNC "p" RETURNING RESU
               CALL "SD_Output" USING "D-YT" D-YT "p" RETURNING RESU
               CALL "SD_Output" USING "D-KJCD" D-KJCD "p" RETURNING RESU
               GO TO M-500
           END-IF.
       M-340.
           CALL "SD_Accept" USING BY REFERENCE A-TC2 "A-TC2" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-320
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-340
           END-IF
           COMPUTE W-TCD2 = W-TC2 + 1.
           CALL "SD_Output" USING "D-TCN2" D-TCN2 "p" RETURNING RESU.
           IF  W-TC2 = 0
               MOVE ZERO TO W-YT
               CALL "SD_Output" USING "D-YT" D-YT "p" RETURNING RESU
               GO TO M-380
           END-IF.
       M-360.
           CALL "SD_Accept" USING BY REFERENCE A-YT "A-YT" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-340
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-360
           END-IF
           CALL "SD_Output" USING "D-YT" D-YT "p" RETURNING RESU.
       M-380.
           CALL "SD_Accept" USING BY REFERENCE A-KJCD "A-KJCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-KEY1 = 2
                   GO TO M-320
               ELSE
                   IF  W-TC2 = 0
                       GO TO M-340
                   ELSE
                       GO TO M-360
                   END-IF
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-380
           END-IF
           IF  W-KJCD = ZERO
               MOVE ZERO TO W-KTN
               CALL "SD_Output" USING "D-KJCD" D-KJCD "p" RETURNING RESU
               GO TO M-500
           END-IF
           MOVE W-KJCD TO J-KEY.
      *           READ J-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-380
           END-IF
           MOVE J-ST TO W-KTN
           MOVE J-NAME TO W-KNA.
           CALL "SD_Output" USING "D-KNA" D-KNA "p" RETURNING RESU.
           MOVE W-KEY TO J-KEY.
      *           READ J-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-420
           END-IF
           GO TO M-500.
       M-420.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF.
       M-500.
           CALL "SD_Accept" USING BY REFERENCE A-BKC "A-BKC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-KEY1 NOT = 0 AND 2 AND 4 AND 5
                   GO TO M-320
               ELSE
                   GO TO M-380
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-500
           END-IF
           MOVE ZERO TO BNM-KEY.
           MOVE W-BKC TO BNM-BU.
      *           READ BNM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BNM_PNAME1 BY REFERENCE BNM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME18" E-ME18 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-500
           END-IF
           CALL "SD_Output" USING "D-BKN" D-BKN "p" RETURNING RESU.
           IF  W-BKC = 10
               MOVE 01 TO W-BKNO
           END-IF
           IF  W-BKC = 22
               MOVE 11 TO W-BKNO
           END-IF
           IF  W-BKC = 26
               MOVE 12 TO W-BKNO
           END-IF
           IF  W-BKC = 29
               MOVE 13 TO W-BKNO
           END-IF
           IF  W-BKC = 23
               MOVE 14 TO W-BKNO
           END-IF
           IF  W-BKC = 24
               MOVE 15 TO W-BKNO
           END-IF
           IF  W-BKC = 20
               MOVE 16 TO W-BKNO
           END-IF
           IF  W-BKC = 21
               MOVE 17 TO W-BKNO
           END-IF
           IF  W-BKC = 25
               MOVE 18 TO W-BKNO
           END-IF
           IF  W-BKC = 27
               MOVE 21 TO W-BKNO
           END-IF
           IF  W-BKC = 28
               MOVE 22 TO W-BKNO
           END-IF
           IF  W-BKC = 32
               MOVE 32 TO W-BKNO
           END-IF
           IF  W-BKC = 33
               MOVE 33 TO W-BKNO
           END-IF
           IF  W-BKC = 41
               MOVE 41 TO W-BKNO
           END-IF
           IF  W-BKC = 31
               MOVE 51 TO W-BKNO
           END-IF
           IF  W-KEY1 NOT = 3 AND 4
               IF  W-BKC > 30 AND < 40
                   CALL "SD_Output" USING
                    "E-ME17" E-ME17 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
               END-IF
           END-IF
           IF  W-KEY1 = 3 OR 4
               IF  W-BKC > 20 AND < 30
                   CALL "SD_Output" USING
                    "E-ME17" E-ME17 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
               END-IF
           END-IF.
       M-540.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-500
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-540
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "E-ME20" E-ME20 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-060
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-540
           END-IF
      *
           IF  W-ACT = 3
               GO TO M-740
           END-IF.
       M-580.
           MOVE W-R TO J-R.
           IF  W-ACT = 2
               GO TO M-700
           END-IF
      *           WRITE J-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            J-M_PNAME1 J-M_LNAME J-R RETURNING RET.
           IF  RET = 1
               GO TO M-600
           END-IF
           GO TO M-620.
       M-600.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME7" E-ME7 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           MOVE "JM           " TO W-FILE.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           GO TO M-580.
       M-620.
           INITIALIZE JT-R.
           MOVE W-KEY TO JT-KEY.
           MOVE W-YC TO JT-YC.
           MOVE W-ZC TO JT-ZC.
           MOVE W-SC TO JT-SC.
           MOVE W-BKC TO JT-BKC.
           MOVE W-BKNO TO JT-BKNO.
      *           WRITE JT-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            JT-M_PNAME1 JT-M_LNAME JT-R RETURNING RET.
           IF  RET = 1
               GO TO M-640
           END-IF
           IF  W-KJCD = ZERO
               GO TO M-060
           END-IF
           GO TO M-660.
       M-640.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME13" E-ME13 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE JT-M_IDLST JT-M_PNAME1.
           MOVE "JTM          " TO W-FILE.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" JT-M_PNAME1 "SHARED" BY REFERENCE JT-M_IDLST "1"
            "JT-KEY" BY REFERENCE JT-KEY.
           GO TO M-620.
       M-660.
           INITIALIZE JK-R.
           MOVE W-KJCD TO JK-JCD1.
           MOVE W-KEY TO JK-JCD2.
      *           WRITE JK-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            JK-M_PNAME1 JK-M_LNAME JK-R RETURNING RET.
           IF  RET = 1
               GO TO M-680
           END-IF
           GO TO M-060.
       M-680.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME10" E-ME10 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE JK-M_IDLST JK-M_PNAME1.
           MOVE "JKM          " TO W-FILE.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" JK-M_PNAME1 "SHARED" BY REFERENCE JK-M_IDLST "1"
            "JK-KEY" BY REFERENCE JK-KEY.
           GO TO M-660.
       M-700.
      *           DELETE J-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING J-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           IF  COMPLETION_CODE = 000
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  150
           END-IF
           INITIALIZE  J-R.
           MOVE W-R TO J-R.
      *           WRITE J-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            J-M_PNAME1 J-M_LNAME J-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           MOVE W-YC TO JT-YC.
           MOVE W-ZC TO JT-ZC.
           MOVE W-SC TO JT-SC.
           MOVE W-BKC TO JT-BKC.
           MOVE W-BKNO TO JT-BKNO.
      *           REWRITE JT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JT-M_PNAME1 JT-M_LNAME JT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           IF  W-KJCD = ZERO
               GO TO M-720
           END-IF
           IF  W-KJCD = WK-JCD1
               GO TO M-060
           END-IF
           IF  WK-KEY = ZERO
               GO TO M-660
           END-IF
      *           DELETE JK-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING JK-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           GO TO M-660.
       M-720.
           IF  WK-KEY = ZERO
               GO TO M-060
           END-IF
      *           DELETE JK-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING JK-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           GO TO M-060.
       M-740.
      *           DELETE J-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING J-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           IF  COMPLETION_CODE = 000
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  150
           END-IF
      *           DELETE JT-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING JT-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           IF  WK-KEY = ZERO
               GO TO M-060
           END-IF
      *           DELETE JK-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING JK-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           MOVE ZERO TO JK-KEY.
           MOVE WK-JCD2 TO JK-JCD1.
      *           START JK-M KEY NOT < JK-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JK-M_PNAME1 "JK-KEY" " NOT < " JK-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-060
           END-IF.
       M-760.
      *           READ JK-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JK-M_PNAME1 BY REFERENCE JK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-060
           END-IF
           IF  WK-JCD2 NOT = JK-JCD1
               GO TO M-060
           END-IF
           MOVE JK-JCD2 TO J-KEY.
      *           READ J-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           MOVE ZERO TO J-MCD.
      *           REWRITE J-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            J-M_PNAME1 J-M_LNAME J-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD1" E-JCD1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
      *           DELETE JK-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING JK-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           GO TO M-760.
       M-800.
           CALL "SD_Accept" USING BY REFERENCE A-JCD1 "A-JCD1" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-800
           END-IF.
       M-820.
           CALL "SD_Accept" USING BY REFERENCE A-JCD2 "A-JCD2" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-800
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-820
           END-IF
           IF  W-JCD1 > W-JCD2
               GO TO M-820
           END-IF.
       M-840.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-820
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-840
           END-IF
      *
           MOVE W-JCD1 TO J-KEY.
      *           START J-M KEY NOT < J-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            J-M_PNAME1 "J-KEY" " NOT < " J-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-040
           END-IF.
       M-860.
      *           READ J-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-040
           END-IF
           IF  J-KEY > W-JCD2
               GO TO M-040
           END-IF
           IF  W-PC = ZERO
               MOVE 5 TO W-PC
               CALL "PR_Open" RETURNING RESP
               PERFORM S-10 THRU S-15
           END-IF
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE J-KEY TO P-KEY.
           MOVE J-NAME TO P-NA.
           MOVE J-YC TO P-YC.
           MOVE J-TC1 TO P-TC1.
           MOVE J-ST TO P-T.
           MOVE J-TC2 TO P-TC2.
           MOVE J-YT TO P-YT.
           MOVE J-SC TO P-SC.
           MOVE J-ZC TO P-ZC.
           MOVE J-BKC TO P-BKC.
           IF  J-ED NOT = ZERO
               MOVE J-ED TO P-ED
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-20K TO P-20K.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  J-MCD = ZERO
               GO TO M-860
           END-IF
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE "[" TO P-F1 P-F2.
           MOVE "]" TO P-R1 P-R2.
           MOVE J-KEY TO W-KEY.
           MOVE J-MCD TO P-KEY.
           MOVE J-MCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "＊＊＊　Ｊ－Ｍ　無し　＊＊＊　" TO J-NAME
           END-IF
           MOVE J-NAME TO P-NA.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-20K TO P-20K.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-KEY TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           GO TO M-860.
       M-980.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JT-M_IDLST JT-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JK-M_IDLST JK-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
           IF  W-PC NOT = ZERO
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.

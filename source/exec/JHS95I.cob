       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS95I.
      ******************************************
      *****    得意先別商品コード　入力    *****
      *****      SCREEN : SJH95I           *****
      ******************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(023) VALUE
                "＊＊＊　　得意先別商品コード　リスト　　＊＊＊".
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  X(005)  VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007)  VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD21.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(002) VALUE "商品".
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(004) VALUE "サイズ　".
       01  HEAD22.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "商品".
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(004) VALUE "サイズ　".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(006) VALUE "ＩＴＦコード".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(008) VALUE "トラスコ中山名称".
           02  F              PIC  X(015) VALUE SPACE.
       01  W-P1.
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(001).
           02  P-JAN          PIC  X(013).
           02  F              PIC  X(002).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-SIZN         PIC  N(004).
       01  W-P2.
           02  P-JAN2         PIC  X(013).
           02  F              PIC  X(002).
           02  P-HCD2         PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA2         PIC  N(024).
           02  F              PIC  X(001).
           02  P-SIZN2        PIC  N(004).
           02  F              PIC  X(001).
           02  P-ITF          PIC  X(016).
           02  F              PIC  X(001).
           02  P-NNAME        PIC  X(020).
           02  F              PIC  X(007).
       01  W-DATA.
           02  W-ACT          PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-JAN          PIC  X(013).
           02  W-JAND  REDEFINES W-JAN.
             03  W-JAN1       PIC  9(007).
             03  W-JD1   REDEFINES W-JAN1.
               04  W-J1       PIC  9(001).
               04  W-J2       PIC  9(001).
               04  W-J3       PIC  9(001).
               04  W-J4       PIC  9(001).
               04  W-J5       PIC  9(001).
               04  W-J6       PIC  9(001).
               04  W-J7       PIC  9(001).
             03  W-JAN2       PIC  9(005).
             03  W-JD2   REDEFINES W-JAN2.
               04  W-J8       PIC  9(001).
               04  W-J9       PIC  9(001).
               04  W-J10      PIC  9(001).
               04  W-J11      PIC  9(001).
               04  W-J12      PIC  9(001).
             03  W-JAN3       PIC  9(001).
           02  W-KSD.
             03  W-KS1        PIC  9(003).
             03  W-KS2        PIC  9(003).
             03  W-KS3        PIC  9(003).
             03  W-KS         PIC  9(003).
             03  W-KD    REDEFINES W-KS.
               04  W-K1       PIC  9(002).
               04  W-K2       PIC  9(001).
           02  W-HCD          PIC  9(006).
           02  W-SIZ          PIC  X(003).
           02  W-NNAME        PIC  X(020).
           02  W-DMM          PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-S            PIC  9(001).
           02  W-SD           PIC  9(001).
           02  W-SE           PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-L            PIC  9(002).
           02  W-SEN          PIC  9(001).
           02  W-SED.
             03  W-STCD       PIC  9(004).
             03  W-ETCD       PIC  9(004).
             03  W-SJAN       PIC  X(013).
             03  W-EJAN       PIC  X(013).
           02  W-SIZN         PIC  N(004).
           02  W-PAGE         PIC  9(003).
           02  W-NAME         PIC  N(026).
       01  W-SET.
           02  W-AISM.
             03  F            PIC  X(030) VALUE
                  "000   SS S  M  L  LL XL XXL   ".
             03  F            PIC  X(030) VALUE
                  "                     280290300".
             03  F            PIC  X(030) VALUE
                  "125130135140150160170180190200".
             03  F            PIC  X(030) VALUE
                  "210215220225230235240245250   ".
             03  F            PIC  X(030) VALUE
                  "240245250255260265270275      ".
           02  W-AISD.
             03  W-AIS   OCCURS   5.
               04  W-ISD   OCCURS  10.
                 05  W-IS     PIC  X(003).
           02  W-ADSM.
             03  F            PIC  X(040) VALUE
                  " SET    SS  S   M   L   LL  XL  XXL     ".
             03  F            PIC  X(040) VALUE
                  "                            28.029.030.0".
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.0    ".
             03  F            PIC  X(040) VALUE
                  "24.024.525.025.526.026.527.027.5     SET".
           02  W-ADSD.
             03  W-ADS   OCCURS   5.
               04  W-DSD   OCCURS  10.
                 05  W-DS     PIC  X(004).
           COPY LSTAT.
      *
           COPY LICODE.
           COPY LITCM.
           COPY LIHIM.
           COPY LSPF.
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  A-TCD   PIC  9(004).
           02  FILLER.
             03  A-JAN   PIC  X(013).
             03  A-HCD   PIC  9(006).
             03  A-SIZ   PIC  X(003).
           02  A-NNAME PIC  X(020).
           02  A-SEN   PIC  9(001).
           02  FILLER.
             03  A-STCD  PIC  9(004).
             03  A-SJAN  PIC  X(013).
           02  FILLER.
             03  A-ETCD  PIC  9(004).
             03  A-EJAN  PIC  X(013).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-TNA   PIC  N(026).
           02  FILLER.
             03  D-JAN1  PIC  9(007).
             03  D-JAN3  PIC  9(001).
             03  D-HNA   PIC  N(024).
             03  D-SIZ   PIC  X(004).
           02  FILLER.
             03  D-WD.
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  N(024).
               04  FILLER  PIC  X(004).
             03  D-WDC.
               04  FILLER  PIC  X(006) VALUE "      ".
               04  FILLER  PIC  X(048) VALUE
                    "                                                ".
               04  FILLER  PIC  X(004) VALUE "    ".
           02  D-PRI.
             03  FILLER  PIC  X(038) VALUE
                  "ＪＡＮコード = 1  ,  その他 = 2  ...  ".
             03  FILLER.
               04  FILLER  PIC  N(003) VALUE "得意先".
               04  FILLER  PIC  N(002) VALUE "商品".
               04  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER.
               04  FILLER  PIC  N(004) VALUE "ＦＲＯＭ".
               04  FILLER  PIC  X(004) VALUE "    ".
               04  FILLER  PIC  X(013) VALUE "             ".
             03  FILLER.
               04  FILLER  PIC  X(004) VALUE "    ".
               04  FILLER  PIC  X(013) VALUE "             ".
           02  D-PRIC.
             03  FILLER  PIC  X(038) VALUE
                  "                                      ".
             03  FILLER.
               04  FILLER  PIC  X(006) VALUE "      ".
               04  FILLER  PIC  X(004) VALUE "    ".
               04  FILLER  PIC  X(004) VALUE "    ".
             03  FILLER.
               04  FILLER  PIC  X(008) VALUE "        ".
               04  FILLER  PIC  X(004) VALUE "    ".
               04  FILLER  PIC  X(013) VALUE "             ".
             03  FILLER.
               04  FILLER  PIC  X(004) VALUE "    ".
               04  FILLER  PIC  X(004) VALUE "    ".
               04  FILLER  PIC  X(013) VALUE "             ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  ﾄｳﾛｸ ｽﾞﾐ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  ﾄｸｲｻｷ ﾅｼ  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  ﾋﾝﾒｲ ﾅｼ  ***".
             03  E-ME5   PIC  X(017) VALUE
                  "***  ｻｲｽﾞ ﾅｼ  ***".
             03  E-ME9   PIC  X(020) VALUE
                  "***  キャンセル  ***".
             03  E-ME11  PIC  X(019) VALUE
                  "***  WRITE ｴﾗｰ  ***".
             03  E-ME12  PIC  X(021) VALUE
                  "***  REWRITE ｴﾗｰ  ***".
             03  E-ME13  PIC  X(020) VALUE
                  "***  DELETE ｴﾗｰ  ***".
           COPY LSSEM.
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "83" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "56" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "5" "9" "4" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "8" "0" "22" "A-TCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JAN" "X" "8" "2" "13" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JAN" BY REFERENCE W-JAN "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "8" "16" "6" "A-JAN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SIZ" "X" "8" "72" "3" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SIZ" BY REFERENCE W-SIZ "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NNAME" "X" "9" "51" "20" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NNAME" BY REFERENCE W-NNAME "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "12" "57" "1" "A-NNAME" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-ACP" " " "16" "0" "17" "A-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STCD" "9" "16" "36" "4" " " "06C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SJAN" "X" "16" "42" "13" "A-STCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SJAN" BY REFERENCE W-SJAN "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-ACP" " " "18" "0" "17" "06C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ETCD" "9" "18" "36" "4" " " "07C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EJAN" "X" "18" "42" "13" "A-ETCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EJAN" BY REFERENCE W-EJAN "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "65" "1" "07C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "420" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "5" "14" "52" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE W-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "8" "0" "60" "D-TNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-JAN1" "9" "8" "2" "7" " " "02C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-JAN1" BY REFERENCE W-JAN1 "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-JAN3" "9" "8" "14" "1" "D-JAN1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-JAN3" BY REFERENCE W-JAN3 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNA" "N" "8" "23" "48" "D-JAN3" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SIZ" "X" "8" "76" "4" "D-HNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SIZ" BY REFERENCE W-DS(1,1) "4" "2" BY REFERENCE W-S 40
            BY REFERENCE CNT 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-DSP" " " "W-L" "0" "116" "02C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-WD" " " "W-L" "14" "58" " " "03C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-WD" "9" "W-L" "16" "6" " " "D-WD" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-WD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-WD" "N" "W-L" "23" "48" "01D-WD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-WD" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-WD" "X" "W-L" "76" "4" "02D-WD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-WD" BY REFERENCE W-DS(1,1) "4" "2" BY REFERENCE W-S 40
            BY REFERENCE CNT 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-WDC" " " "W-L" "14" "58" "D-WD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-WDC" "X" "W-L" "16" "6" " " "D-WDC" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-WDC" "X" "W-L" "23" "48" "01D-WDC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-WDC" "X" "W-L" "76" "4" "02D-WDC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PRI" " " "0" "0" "94" "03C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-PRI" "X" "12" "20" "38" " " "D-PRI" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-PRI" " " "14" "14" "14" "01D-PRI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-PRI" "N" "14" "35" "6" " " "02D-PRI" RETURNING RESU.
       CALL "SD_Init" USING 
           "0202D-PRI" "N" "14" "42" "4" "0102D-PRI" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "0302D-PRI" "X" "14" "46" "4" "0202D-PRI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-PRI" " " "16" "14" "25" "02D-PRI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103D-PRI" "N" "16" "25" "8" " " "03D-PRI" RETURNING RESU.
       CALL "SD_Init" USING 
           "0203D-PRI" "X" "16" "36" "4" "0103D-PRI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0303D-PRI" "X" "16" "42" "13" "0203D-PRI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-PRI" " " "18" "14" "17" "03D-PRI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0104D-PRI" "X" "18" "36" "4" " " "04D-PRI" RETURNING RESU.
       CALL "SD_Init" USING 
            "0204D-PRI" "X" "18" "42" "13" "0104D-PRI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PRIC" " " "0" "0" "98" "D-PRI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-PRIC" "X" "12" "20" "38" " " "D-PRIC" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-PRIC" " " "14" "14" "14" "01D-PRIC" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "0102D-PRIC" "X" "14" "35" "6" " " "02D-PRIC" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-PRIC" "X" "14" "42" "4" "0102D-PRIC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-PRIC" "X" "14" "46" "4" "0202D-PRIC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-PRIC" " " "16" "14" "25" "02D-PRIC" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "0103D-PRIC" "X" "16" "25" "8" " " "03D-PRIC" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203D-PRIC" "X" "16" "36" "4" "0103D-PRIC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0303D-PRIC" "X" "16" "42" "13" "0203D-PRIC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-PRIC" " " "18" "14" "21" "03D-PRIC" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "0104D-PRIC" "X" "18" "25" "4" " " "04D-PRIC" RETURNING RESU.
       CALL "SD_Init" USING 
            "0204D-PRIC" "X" "18" "36" "4" "0104D-PRIC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0304D-PRIC" "X" "18" "42" "13" "0204D-PRIC" " "
            RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "167" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "167" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "17" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "20" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "19" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "21" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "20" "E-ME12" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJH95I" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           MOVE W-AISM TO W-AISD.
           MOVE W-ADSM TO W-ADSD.
           CALL "DB_F_Open" USING
            "I-O" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-040.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-900
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-ACT = 9
               GO TO M-900
           END-IF
           IF  W-ACT = 4
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Screen_Output" USING "SJH95I" RETURNING RESU
               CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU
               GO TO M-300
           END-IF
           CALL "SD_Output" USING "D-PRIC" D-PRIC "p" RETURNING RESU.
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO TO M-040
           END-IF.
       M-060.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF
           IF  W-TCD NOT = ZERO
               GO TO M-070
           END-IF
           MOVE SPACE TO W-NAME.
           MOVE "ＪＡＮコード" TO W-NAME.
           MOVE 4932807 TO W-JAN1.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-JAN1" D-JAN1 "p" RETURNING RESU.
       M-065.
           CALL "SD_Accept" USING BY REFERENCE A-JAN "A-JAN" "X" "13"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-065
           END-IF
           COMPUTE W-KS1 = W-J2 + W-J4 + W-J6 + W-J8 + W-J10 + W-J12.
           COMPUTE W-KS1 = W-KS1 * 3.
           COMPUTE W-KS2 = W-J1 + W-J3 + W-J5 + W-J7 + W-J9 + W-J11.
           COMPUTE W-KS = W-KS1 + W-KS2.
           COMPUTE W-KS3 = 10 - W-K2.
           MOVE W-KS3 TO W-JAN3.
           CALL "SD_Output" USING "D-JAN3" D-JAN3 "p" RETURNING RESU.
           GO TO M-085.
       M-070.
           MOVE W-TCD TO TC-TCD.
           MOVE 001 TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-060
           END-IF
           MOVE TC-NAME TO W-NAME.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
       M-080.
           CALL "SD_Accept" USING BY REFERENCE A-JAN "A-JAN" "X" "13"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-080
           END-IF.
       M-085.
           PERFORM DSC-RTN THRU DSC-EX.
           MOVE 10 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE 0 TO W-DC.
           MOVE SPACE TO CODE-KEY.
           MOVE W-TCD TO CODE-TCD.
           MOVE W-JAN TO CODE-JAN.
      *           START CODEF KEY NOT < CODE-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CODEF_PNAME1 "CODE-KEY" " NOT < " CODE-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-100
           END-IF.
       M-090.
      *           READ CODEF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-100
           END-IF
           IF (W-TCD NOT = CODE-TCD) OR (W-JAN NOT = CODE-JAN)
               GO TO M-100
           END-IF.
       M-095.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L > 20
               GO TO M-120
           END-IF
           MOVE CODE-HCD TO HI-MHCD HI-HCD W-HCD.
           MOVE CODE-SIZ TO W-S.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "　品名　なし" TO HI-NAME
           END-IF
           IF  W-S NOT = 1
               ADD 1 TO W-S
           ELSE
               IF  0 = HI-S1(1) AND HI-S1(2) AND HI-S1(3) AND HI-S1(4)
                               AND HI-S1(5) AND HI-S1(6) AND HI-S1(7)
                   ADD 1 TO W-S
               END-IF
           END-IF
           MOVE CODE-SNO TO CNT.
           CALL "SD_Output" USING "D-WD" D-WD "p" RETURNING RESU.
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF
           GO TO M-090.
       M-100.
           IF  W-DC = 0
               IF  W-ACT NOT = 1
                   CALL "SD_Output" USING
                    "E-ME2" E-ME2 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   IF  W-TCD = ZERO
                       GO TO M-065
                   ELSE
                       GO TO M-080
                   END-IF
               END-IF
           END-IF.
       M-120.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-TCD = ZERO
                   GO TO M-065
               ELSE
                   GO TO M-080
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-120
           END-IF
      *
           MOVE W-HCD TO HI-MHCD HI-HCD.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-120
           END-IF
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           MOVE 0 TO HI-S(4,10).
      *
           MOVE SPACE TO CODE-KEY.
           MOVE W-TCD TO CODE-TCD.
           MOVE W-JAN TO CODE-JAN.
           MOVE W-HCD TO CODE-HCD.
      *           START CODEF KEY NOT < CODE-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CODEF_PNAME1 "CODE-KEY" " NOT < " CODE-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-140
           END-IF
      *           READ CODEF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-140
           END-IF
           IF (W-TCD NOT = CODE-TCD) OR (W-JAN NOT = CODE-JAN)
               GO TO M-140
           END-IF
           GO TO M-160.
       M-140.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-120
           END-IF.
       M-160.
           CALL "SD_Accept" USING BY REFERENCE A-SIZ "A-SIZ" "X" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-160
           END-IF
      *
           PERFORM SIZ-RTN THRU SIZ-EX.
           IF  W-SE = 1
               GO TO M-160
           END-IF
      *
           MOVE SPACE TO CODE-KEY.
           MOVE W-TCD TO CODE-TCD.
           MOVE W-JAN TO CODE-JAN.
           MOVE W-HCD TO CODE-HCD.
           MOVE W-SD TO CODE-SIZ.
           MOVE CNT TO CODE-SNO.
      *           READ CODEF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" CODEF_PNAME1 BY REFERENCE CODE-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-180
           END-IF
           IF  W-ACT = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-160
           END-IF
           MOVE CODE-NAME TO W-NNAME.
           CALL "SD_Output" USING "A-NNAME" A-NNAME "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO M-190
           END-IF
           IF  W-TCD = ZERO
               GO TO M-185
           ELSE
               MOVE SPACE TO W-NNAME
               CALL "SD_Output" USING
                "A-NNAME" A-NNAME "p" RETURNING RESU
               GO TO M-190
           END-IF.
       M-180.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-160
           END-IF
           IF  W-TCD NOT = ZERO
               MOVE SPACE TO W-NNAME
               CALL "SD_Output" USING
                "A-NNAME" A-NNAME "p" RETURNING RESU
               GO TO M-190
           END-IF.
       M-185.
           CALL "SD_Accept" USING BY REFERENCE A-NNAME "A-NNAME"
            "X" "20" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-160
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-185
           END-IF.
       M-190.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF (W-TCD = ZERO) AND (W-ACT NOT = 3)
                   GO TO M-185
               ELSE
                   GO TO M-160
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-190
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               IF  W-TCD = ZERO
                   GO TO M-065
               ELSE
                   GO TO M-080
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-190
           END-IF
      *
           IF  W-ACT = 2
               GO TO M-230
           END-IF
           IF  W-ACT = 3
               GO TO M-240
           END-IF.
       M-200.
           INITIALIZE CODE-R.
           MOVE W-TCD TO CODE-TCD.
           MOVE W-JAN TO CODE-JAN CODE-JAN2.
           MOVE W-HCD TO CODE-HCD.
           MOVE W-SD TO CODE-SIZ.
           MOVE CNT TO CODE-SNO.
           MOVE W-NNAME TO CODE-NAME.
      *           WRITE CODE-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            CODEF_PNAME1 CODEF_LNAME CODE-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-220
           END-IF
           IF  COMPLETION_CODE = 000
               IF  W-TCD = ZERO
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  100
               END-IF
           END-IF
           IF  W-TCD = ZERO
               GO TO M-065
           ELSE
               GO TO M-080
           END-IF.
       M-220.
           IF  ERR-STAT NOT = 22 AND 24
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-900
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ERR-STAT = 22
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               IF  W-TCD = ZERO
                   GO TO M-065
               ELSE
                   GO TO M-080
               END-IF
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
           GO TO M-200.
       M-230.
           MOVE W-NNAME TO CODE-NAME.
      *           REWRITE CODE-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            CODEF_PNAME1 CODEF_LNAME CODE-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-900
           END-IF
           IF  COMPLETION_CODE = 000
               IF  W-TCD = ZERO
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  100
               END-IF
           END-IF
           IF  W-TCD = ZERO
               GO TO M-065
           ELSE
               GO TO M-080
           END-IF.
       M-240.
      *           DELETE CODEF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING CODEF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-900
           END-IF
           IF  W-TCD = ZERO
               GO TO M-065
           ELSE
               GO TO M-080
           END-IF.
       M-300.
           CALL "SD_Output" USING "D-PRI" D-PRI "p" RETURNING RESU.
           PERFORM INP-RTN THRU INP-EX.
           IF  ESTAT = BTB
               GO TO M-040.
      *
           MOVE SPACE TO CODE-KEY.
           IF  W-SEN = 2
               MOVE 0001 TO CODE-TCD
           ELSE
               MOVE W-STCD TO CODE-TCD
           END-IF
           MOVE W-SJAN TO CODE-JAN.
      *           START CODEF KEY NOT < CODE-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CODEF_PNAME1 "CODE-KEY" " NOT < " CODE-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-300
           END-IF.
       M-320.
      *           READ CODEF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-300
           END-IF
           IF  W-ETCD < CODE-TCD
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-300
           END-IF
           IF  CODE-JAN < W-SJAN OR > W-EJAN
               GO TO M-320
           END-IF
           CALL "PR_Open" RETURNING RESP.
           PERFORM MID-020 THRU MID-EX.
           MOVE ZERO TO W-TCD W-HCD.
       M-340.
           PERFORM PRI-RTN THRU PRI-EX.
       M-360.
      *           READ CODEF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-380
           END-IF
           IF  W-ETCD < CODE-TCD
               GO TO M-380
           END-IF
           IF  CODE-JAN < W-SJAN OR > W-EJAN
               GO TO M-360
           END-IF
           GO TO M-340.
       M-380.
           CALL "PR_Close" RETURNING RESP.
           GO TO M-040.
       M-900.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       DSC-RTN.
           MOVE 10 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       DSC-020.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L < 21
               CALL "SD_Output" USING "D-WDC" D-WDC "p" RETURNING RESU
               GO TO DSC-020
           END-IF.
       DSC-EX.
           EXIT.
       SIZ-RTN.
           MOVE 0 TO W-S W-SE.
       SIZ-020.
           ADD 1 TO W-S.
           IF  W-S > 5
               GO TO SIZ-060
           END-IF
           MOVE ZERO TO CNT.
       SIZ-040.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO SIZ-020
           END-IF
           IF  W-SIZ NOT = W-IS(W-S,CNT)
               GO TO SIZ-040
           END-IF
           MOVE W-S TO W-SD.
           IF  W-SD > 1
               SUBTRACT 1 FROM W-SD
           END-IF
           IF  W-SIZ = 000
               CALL "SD_Output" USING "D-SIZ" D-SIZ "p" RETURNING RESU
               GO TO SIZ-EX
           END-IF
           IF  HI-S(W-SD,CNT) = 0
               GO TO SIZ-040
           END-IF
           IF  W-S = 2
               IF  ZERO = HI-SS(2) AND HI-SS(3) AND HI-SS(4)
                   GO TO SIZ-060
               END-IF
           END-IF
           IF  W-S = 1
               IF (ZERO NOT = HI-SS(2)) OR (ZERO NOT = HI-SS(3))
                                        OR (ZERO NOT = HI-SS(4))
                   GO TO SIZ-060
               END-IF
           END-IF
           CALL "SD_Output" USING "D-SIZ" D-SIZ "p" RETURNING RESU.
           GO TO SIZ-EX.
       SIZ-060.
           CALL "SD_Output" USING "E-ME5" E-ME5 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           MOVE 1 TO W-SE.
       SIZ-EX.
           EXIT.
       INP-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO INP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO INP-RTN
           END-IF
           IF  W-SEN NOT = 1 AND 2
               GO TO INP-RTN
           END-IF
           IF  W-SEN = 1
               MOVE ZERO TO W-STCD W-ETCD
               CALL "SD_Output" USING "A-STCD" A-STCD "p" RETURNING RESU
               CALL "SD_Output" USING "A-ETCD" A-ETCD "p" RETURNING RESU
               GO TO INP-040
           END-IF.
       INP-010.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO INP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO INP-RTN
           END-IF.
       INP-020.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO INP-010
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO INP-020
           END-IF
      *
           IF  W-STCD > W-ETCD
               GO TO INP-020
           END-IF.
       INP-040.
           CALL "SD_Accept" USING BY REFERENCE A-SJAN "A-SJAN" "X" "13"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-SEN = 1
                   GO TO INP-RTN
               ELSE
                   GO TO INP-020
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO INP-040
           END-IF.
       INP-060.
           CALL "SD_Accept" USING BY REFERENCE A-EJAN "A-EJAN" "X" "13"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO INP-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO INP-060
           END-IF
      *
           IF  W-SJAN > W-EJAN
               GO TO INP-060
           END-IF.
       INP-080.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO INP-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO INP-080
           END-IF
           IF  W-DMM = 9
               GO TO INP-RTN
           END-IF
           IF  W-DMM NOT = 1
               GO TO INP-080
           END-IF.
       INP-EX.
           EXIT.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-020.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  W-SEN = 1
               MOVE HEAD22 TO SP-R
           ELSE
               MOVE HEAD21 TO SP-R
           END-IF.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
       PRI-RTN.
           IF  W-SEN = 1
               MOVE SPACE TO W-P2
               MOVE SPACE TO P-HNA2 P-SIZN2
               GO TO PRI-020
           END-IF.
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-TNA P-HNA P-SIZN.
           IF  CODE-TCD = W-TCD OR ZERO
               GO TO PRI-020
           END-IF
           MOVE CODE-TCD TO W-TCD.
           MOVE W-TCD TO TC-TCD.
           MOVE 001 TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
               MOVE "　得意先　なし" TO TC-NAME
           END-IF
           MOVE W-TCD TO P-TCD.
           MOVE TC-NAME TO P-TNA.
       PRI-020.
           IF  W-SEN = 1
               MOVE CODE-JAN TO P-JAN2
               MOVE CODE-ITF TO P-ITF
               MOVE CODE-NAME TO P-NNAME
           ELSE
               MOVE CODE-JAN TO P-JAN
           END-IF
           IF  CODE-HCD = W-HCD
               GO TO PRI-040
           END-IF
           MOVE CODE-HCD TO W-HCD.
           MOVE W-HCD TO HI-MHCD HI-HCD.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "　品名　なし" TO HI-NAME
           END-IF
           MOVE 0 TO HI-S(4,10).
           IF  W-SEN = 1
               MOVE W-HCD TO P-HCD2
               MOVE HI-NAME TO P-HNA2
           ELSE
               MOVE W-HCD TO P-HCD
               MOVE HI-NAME TO P-HNA
           END-IF.
       PRI-040.
           MOVE CODE-SIZ TO W-S.
           MOVE CODE-SNO TO CNT.
           IF  1 NOT = W-S AND CNT
               IF  HI-S(W-S,CNT) = 0
                   IF  W-SEN = 1
                       MOVE "　なし　" TO P-SIZN2
                       GO TO PRI-060
                   ELSE
                       MOVE "　なし　" TO P-SIZN
                       GO TO PRI-060
                   END-IF
               END-IF
           END-IF
           IF (ZERO NOT = HI-SS(2)) OR (ZERO NOT = HI-SS(3))
                                    OR (ZERO NOT = HI-SS(4))
               ADD 1 TO W-S
           END-IF
           IF  W-SEN = 1
               MOVE W-DS(W-S,CNT) TO P-SIZN2
           ELSE
               MOVE W-DS(W-S,CNT) TO P-SIZN
           END-IF.
       PRI-060.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 63
               GO TO PRI-080
           END-IF
           PERFORM MID-RTN THRU MID-EX.
           IF  W-SEN = 2
               MOVE W-TCD TO P-TCD
               MOVE TC-NAME TO P-TNA
           END-IF
           IF  W-SEN = 1
               MOVE W-HCD TO P-HCD2
               MOVE HI-NAME TO P-HNA2
           ELSE
               MOVE W-HCD TO P-HCD
               MOVE HI-NAME TO P-HNA
           END-IF.
       PRI-080.
           IF  W-SEN = 1
               MOVE W-P2 TO SP-R
           ELSE
               MOVE W-P1 TO SP-R
           END-IF
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       PRI-EX.
           EXIT.

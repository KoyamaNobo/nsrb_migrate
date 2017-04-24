       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD150.
      *********************************************************
      *    PROGRAM         :  発送明細Ｆ　修正入力 (ﾅﾌｺ･ﾜｰｸﾏﾝ)*
      *    SCREEN          :  SCHD15                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       77  W-INV              PIC  9(001) VALUE 0.
       77  W-ADV              PIC  9(001) VALUE 0.
       01  ERR-STAT           PIC  X(002).
       01  WR-D1.
           02  WR-KEY.                                                  KEY
             03  WR-DNO       PIC  9(006).                              ｼｭｯｶｼｽﾞ
             03  WR-GNO       PIC  9(001).                              ｷﾞｮｳ
           02  WR-DC          PIC  9(001).                              ﾃﾞﾝｸ
           02  WR-DATE        PIC  9(008).                              ｼｭｯｶﾋﾞｼﾞ
           02  WR-OTD.                                                  ﾁｮｸｿｳ CD
             03  WR-TCD       PIC  9(004).                              ﾄｸｲｺｰﾄﾞ
             03  WR-CCD       PIC  9(003).                              ﾁｮｸ NO
           02  WR-SOK         PIC  9(001).                              ｸﾗ ｺｰﾄﾞ
           02  WR-HCD         PIC  9(006).                              ﾋﾝｺｰﾄﾞ
           02  WR-SIZ         PIC  9(001).                              ｻｲｽﾞｸﾌﾞﾝ
           02  WR-ASU.                                                  ｼｭｯｶｼﾞﾂ
             03  WR-SUD   OCCURS  10.                                   ｻｲｽﾞﾍﾞﾂ
               04  WR-SU      PIC S9(004).
             03  WR-SUT       PIC S9(006).
           02  WR-ACD         PIC  9(001).                              ｱｽﾞｶﾘ KB
           02  WR-KSU         PIC S9(003).                              個数
           02  WR-PRC         PIC  9(001).                              ｲﾝｼﾞｸﾌﾞﾝ
           02  WR-TNC         PIC  9(002).                              ﾀﾝﾄｳ
           02  F              PIC  9(002).                              ﾌﾞﾝﾙｲ2
           02  WR-TAN         PIC  9(005).                              ﾀﾝｶ
           02  WR-SKD         PIC  9(008).
           02  WR-GBI         PIC  X(010).
           02  WR-HKC         PIC  9(001).                              ﾍﾝｶﾝｸﾌﾞﾝ
           02  WR-USO         PIC  9(001).
           02  F              PIC  X(014).
           02  WR-TSC         PIC  9(001).
           02  WR-NRC         PIC  9(001).
           02  WR-SSU         PIC  9(001).                              ｼﾖｳｶｲｽｳ
       01  WR-AD.
           02  WR-D    OCCURS   6  PIC  X(128).
       01  WR-D2.
           02  W-KEY2.
             03  W-DNOB       PIC  9(006).                              ｼｭｯｶｼｽﾞ
             03  W-GNOB       PIC  9(001).                              ｷﾞｮｳ
           02  W-DCB          PIC  9(001).                              ﾃﾞﾝｸ
           02  W-DATEB        PIC  9(008).                              ｼｭｯｶﾋﾞｼﾞ
           02  W-OTDB.                                                  ﾁｮｸｿｳ CD
             03  W-TCDB       PIC  9(004).                              ﾄｸｲｺｰﾄﾞ
             03  W-CCDB       PIC  9(003).                              ﾁｮｸ NO
           02  W-SOKB         PIC  9(001).                              ｸﾗ ｺｰﾄﾞ
           02  W-BI           PIC  N(024).                              摘要
           02  F              PIC  X(056).
       01  W-ADC.
           02  W-DC    OCCURS   6  PIC  9(001).
       01  W-AKIN.
           02  W-KIN   OCCURS   6  PIC S9(008).
       01  WB-AD.
           02  WB-D    OCCURS  6.
             03  WB-HCD      PIC  9(006).
             03  WB-SIZ      PIC  9(001).
             03  WB-TAN      PIC  9(005).
       01  W-ASD.
           02  W-SD    OCCURS  10  PIC  9(001).
       01  W-DATA.
           02  W-L.
             03  W-L1         PIC  9(002).
             03  W-L2         PIC  9(002).
             03  W-L3         PIC  9(002).
           02  W-C            PIC S9(002).
           02  W-KIND         PIC S9(008).
           02  W-GSUT         PIC S9(006).
           02  W-RC           PIC  9(001).
           02  W-SC           PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-DNOD         PIC  9(006).
           02  W-DNOO         PIC  9(006).
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NGD.
               04  W-ND       PIC  9(004).
               04  W-NDD   REDEFINES W-ND.
                 05  W-ND1    PIC  9(002).
                 05  W-ND2    PIC  9(002).
               04  W-GD       PIC  9(002).
             03  W-PD         PIC  9(002).
           02  W-NGPSD REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-TCDD         PIC  9(004).
           02  W-CCDD         PIC  9(003).
           02  W-HCDD         PIC  9(006).
           02  W-HCDO         PIC  9(006).
           02  W-DCD          PIC  9(001).
           02  W-DCC          PIC  9(001).
           02  W-ZC           PIC  9(001).
           02  W-CNG.
             03  W-CN         PIC  9(004).
             03  W-CND   REDEFINES W-CN.
               04  W-CN1      PIC  9(002).
               04  W-CN2      PIC  9(002).
             03  W-CG         PIC  9(002).
           02  W-SNGD  REDEFINES W-CNG.
             03  F            PIC  9(002).
             03  W-CNGS       PIC  9(004).
           02  W-SNG          PIC  9(006).
           02  W-ENG          PIC  9(006).
           02  W-HSCD         PIC  9(001).
           02  W-HNC          PIC  9(001).
           02  W-HNCD         PIC  9(001).
           02  W-SCD          PIC  9(001).
           02  W-KOSUR        PIC  9(003).
           02  WK-TAN         PIC S9(005).
           02  W-FRCD         PIC  9(001).
           02  W-DTW1         PIC  9(003).
           02  W-DTW2         PIC  9(001).
           02  W-SDTD         PIC  9(008).
           02  W-SDTM  REDEFINES W-SDTD.
             03  W-SDTNG.
               04  W-SDTN     PIC  9(004).
               04  W-SDTG     PIC  9(002).
             03  W-SDTP       PIC  9(002).
           02  W-HMN          PIC  9(006).
           02  WK-SCD         PIC  9(001).
           02  WK-DC          PIC  9(001).
           02  WK-BI          PIC  N(024).
           02  W-EC           PIC  9(001).
           02  W-DEL          PIC  9(001).
           02  W-RCD          PIC  9(001).
           02  CNT            PIC  9(001).
           02  W-HC           PIC  9(001).
           02  W-LD           PIC  9(006).
           02  W-RCB          PIC  9(001).
           02  W-LB           PIC  9(006).
           02  W-DSPC         PIC  9(001).
           02  W-DKIN         PIC S9(008).
           02  W-SDATE        PIC  9(008).
           02  W-HIC          PIC  9(001).
           02  W-FTD          PIC  9(005).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LITCM.
           COPY LIHSMS.
           COPY LITHTM.
           COPY L-JCON.
           COPY LITSKF.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  FILLER.
             03  A-HMN   PIC  9(006).
             03  A-NGP   PIC  9(006).
             03  A-SOK   PIC  9(001).
           02  FILLER.
             03  A-CCD   PIC  9(003).
           02  FILLER.
             03  A-DC    PIC  9(001).
             03  A-HCD   PIC  9(006).
             03  A-GBI   PIC  X(010).
           02  FILLER.
             03  A-SIZ   PIC  9(001).
             03  A-SU    PIC S9(004).
             03  A-SU1   PIC S9(003).
             03  A-SUT   PIC S9(005).
             03  A-TAN   PIC  9(005).
             03  A-KIN   PIC S9(007).
           02  A-BI    PIC  N(024).
           02  FILLER.
             03  A-KOSU  PIC  9(003).
             03  A-SDT   PIC  9(008).
             03  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SNA   PIC  N(006).
           02  FILLER.
             03  D-TCD   PIC  9(004).
             03  D-TNA   PIC  N(026).
           02  D-CNA   PIC  N(026).
           02  FILLER.
             03  D-HNA   PIC  N(024).
           02  FILLER.
             03  D-S1.
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE " SS  ".
               04  FILLER  PIC  X(005) VALUE "  S  ".
               04  FILLER  PIC  X(005) VALUE "  M　".
               04  FILLER  PIC  X(005) VALUE "  L　".
               04  FILLER  PIC  X(005) VALUE " LL  ".
               04  FILLER  PIC  X(004) VALUE "28.0".
               04  FILLER  PIC  X(004) VALUE "29.0".
               04  FILLER  PIC  X(004) VALUE "30.0".
             03  D-S2.
               04  FILLER  PIC  X(005) VALUE "12.5 ".
               04  FILLER  PIC  X(005) VALUE "13.0 ".
               04  FILLER  PIC  X(005) VALUE "13.5 ".
               04  FILLER  PIC  X(005) VALUE "14.0 ".
               04  FILLER  PIC  X(005) VALUE "15.0 ".
               04  FILLER  PIC  X(005) VALUE "16.0 ".
               04  FILLER  PIC  X(005) VALUE "17.0 ".
               04  FILLER  PIC  X(004) VALUE "18.0".
               04  FILLER  PIC  X(004) VALUE "19.0".
               04  FILLER  PIC  X(004) VALUE "20.0".
             03  D-S3.
               04  FILLER  PIC  X(005) VALUE "21.0 ".
               04  FILLER  PIC  X(005) VALUE "21.5 ".
               04  FILLER  PIC  X(005) VALUE "22.0 ".
               04  FILLER  PIC  X(005) VALUE "22.5 ".
               04  FILLER  PIC  X(005) VALUE "23.0 ".
               04  FILLER  PIC  X(005) VALUE "23.5 ".
               04  FILLER  PIC  X(005) VALUE "24.0 ".
               04  FILLER  PIC  X(004) VALUE "24.5".
               04  FILLER  PIC  X(004) VALUE "25.0".
               04  FILLER  PIC  X(004) VALUE "    ".
             03  D-S4.
               04  FILLER  PIC  X(005) VALUE "24.0 ".
               04  FILLER  PIC  X(005) VALUE "24.5 ".
               04  FILLER  PIC  X(005) VALUE "25.0 ".
               04  FILLER  PIC  X(005) VALUE "25.5 ".
               04  FILLER  PIC  X(005) VALUE "26.0 ".
               04  FILLER  PIC  X(005) VALUE "26.5 ".
               04  FILLER  PIC  X(005) VALUE "27.0 ".
               04  FILLER  PIC  X(004) VALUE "27.5".
               04  FILLER  PIC  X(004) VALUE "    ".
               04  FILLER  PIC  X(004) VALUE "    ".
           02  FILLER.
             03  D-SU    PIC ZZZZ- .
             03  D-SU1   PIC ZZZ- .
             03  D-SUT   PIC ZZZZZ- .
             03  D-TAN   PIC ZZZZZ .
             03  D-KIN   PIC ZZZZZZZ- .
           02  FILLER.
             03  D-GSUT  PIC ZZZ,ZZZ- .
           02  FILLER.
             03  D-KOSU  PIC ZZ9 .
       01  C-SPC.
           02  SU-HNO.
             03  FILLER.
               04  SU-HMN   PIC  X(006) VALUE "      ".
               04  SU-DATE  PIC  X(006) VALUE "      ".
               04  SU-SOK   PIC  X(014) VALUE
                    "            ".
           02  SU-CNA.
             03  FILLER  PIC  X(003) VALUE "   ".
             03  FILLER  PIC  X(052) VALUE
                 "                                                    ".
           02  SU-D1.
             03  FILLER  PIC  X(001) VALUE " ".
             03  FILLER  PIC  X(006) VALUE "     ".
             03  FILLER  PIC  X(048) VALUE
                  "                                                ".
             03  SU-GBI   PIC  X(010) VALUE "          ".
           02  SU-D2.
             03  FILLER  PIC  X(005) VALUE "     ".
             03  FILLER  PIC  X(005) VALUE "     ".
             03  FILLER  PIC  X(005) VALUE "     ".
             03  FILLER  PIC  X(005) VALUE "     ".
             03  FILLER  PIC  X(005) VALUE "     ".
             03  FILLER  PIC  X(005) VALUE "     ".
             03  FILLER  PIC  X(005) VALUE "     ".
             03  FILLER  PIC  X(004) VALUE "    ".
             03  FILLER  PIC  X(004) VALUE "    ".
             03  FILLER  PIC  X(004) VALUE "    ".
           02  SU-D3.
             03  SU-D31.
               04  FILLER  PIC  X(001) VALUE " ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(004) VALUE "    ".
               04  FILLER  PIC  X(004) VALUE "    ".
               04  FILLER  PIC  X(004) VALUE "    ".
             03  FILLER  PIC  X(006) VALUE "      ".
             03  SU-TAN  PIC  X(005) VALUE "     ".
             03  SU-KIN  PIC  X(008) VALUE "        ".
           02  FILLER.
             03  SU-KOSU  PIC  X(003) VALUE "   ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME2   PIC  X(016) VALUE
                  "***  TCM ﾅｼ  ***".
             03  E-ME3   PIC  X(021) VALUE
                  "***  PROGRAM ｴﾗｰ  ***".
             03  E-ME4   PIC  X(020) VALUE
                  "***  区分エラー  ***".
             03  E-ME5   PIC  X(020) VALUE
                  "***  直送エラー ***".
             03  E-ME6.
               04  FILLER  PIC  X(016) VALUE
                    "***  HIM ﾅｼ  ***".
               04  02E-ME6 PIC  9(006).
             03  E-ME7.
               04  FILLER  PIC  X(022) VALUE
                    "***  振替単価無し  ***".
               04  02E-ME7 PIC  9(006).
             03  E-ME8   PIC  X(024) VALUE
                  "***  預りサイズ無し  ***".
             03  E-ME9   PIC  X(020) VALUE
                  "***  サイズ無し  ***".
             03  E-ME10  PIC  X(023) VALUE
                  "***  ＤＡＴＡエラー ***".
             03  E-ME12  PIC  X(020) VALUE
                  "***  日付エラー ***".
             03  E-ME14  PIC  X(025) VALUE
                  "***  発送明細№ CHECK ***".
             03  E-ME15  PIC  X(025) VALUE
                  "***  単価  ZERO CHECK ***".
             03  E-ME20  PIC  X(019) VALUE
                  "***  ﾄｸｲｻｷ ｴﾗｰ  ***".
             03  E-ME21  PIC  X(017) VALUE
                  "***  ﾃﾞｰﾀ ﾅｼ  ***".
             03  E-ME22  PIC  X(028) VALUE
                  "***  HSMSF DELETE ｴﾗｰ  ***".
             03  E-ME23  PIC  X(027) VALUE
                  "***  HSMSF WRITE ｴﾗｰ  ***".
             03  E-ME25.
               04  FILLER    PIC  N(008) VALUE
                    "請求書　発行済み".
               04  02E-ME25  PIC 9999/99/99 .
             03  E-ME26  PIC  X(024) VALUE
                  "***  THTM WRITE ｴﾗｰ  ***".
             03  E-ME27  PIC  X(026) VALUE
                  "***  THTM REWRITE ｴﾗｰ  ***".
             03  E-ME30  PIC  X(030) VALUE
                  "***  データが入っていない  ***".
             03  E-ME90  PIC  X(021) VALUE
                  "---   キャンセル  ---".
             03  MG-03   PIC  X(024) VALUE
                  "***  ｺﾝﾄﾛｰﾙＦ未登録  ***".
             03  MG-05   PIC  X(022) VALUE
                  "***  倉庫省略不可  ***".
             03  E-KEY   PIC  X(007).
           COPY LSSEM.
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "118" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "1" "0" "13" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HMN" "9" "1" "25" "6" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HMN" BY REFERENCE W-HMN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NGP" "9" "1" "37" "6" "A-HMN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NGP" BY REFERENCE W-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SOK" "9" "1" "49" "1" "A-NGP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SOK" BY REFERENCE W-SCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "3" "0" "3" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CCD" "9" "3" "6" "3" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CCD" BY REFERENCE W-CCDD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "W-L1" "0" "17" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DC" "9" "W-L1" "4" "1" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DC" BY REFERENCE W-DC(1) "1" "1" BY REFERENCE W-RC 1
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "W-L1" "11" "6" "A-DC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE WR-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GBI" "X" "W-L1" "71" "10" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GBI" BY REFERENCE WR-GBI "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "W-L3" "0" "25" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SIZ" "9" "W-L3" "1" "1" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SIZ" BY REFERENCE WR-SIZ "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SU" "S9" "W-L3" "W-C" "4" "A-SIZ" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SU" BY REFERENCE WR-SU(1) "4" "1" BY REFERENCE W-SC 2
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SU1" "S9" "W-L3" "W-C" "3" "A-SU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SU1" BY REFERENCE WR-SU(1) "4" "1" BY REFERENCE W-SC 2
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SUT" "S9" "W-L3" "60" "5" "A-SU1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SUT" BY REFERENCE WR-SUT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TAN" "9" "W-L3" "67" "5" "A-SUT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TAN" BY REFERENCE WR-TAN "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KIN" "S9" "W-L3" "73" "7" "A-TAN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KIN" BY REFERENCE W-KIND "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BI" "N" "22" "6" "48" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BI" BY REFERENCE W-BI "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-ACP" " " "23" "0" "12" "A-BI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KOSU" "9" "23" "6" "3" " " "06C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KOSU" BY REFERENCE W-KOSUR "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SDT" "9" "23" "33" "8" "A-KOSU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SDT" BY REFERENCE W-SDTD "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "63" "1" "A-SDT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "395" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SNA" "N" "1" "51" "12" " " "C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-SNA" BY REFERENCE JCON3-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "2" "0" "56" "D-SNA" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TCD" "9" "2" "6" "4" " " "02C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-TCD" BY REFERENCE W-TCDD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "2" "11" "52" "D-TCD" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CNA" "N" "3" "11" "52" "02C-DSP" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-CNA" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-DSP" " " "W-L1" "0" "48" "D-CNA" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNA" "N" "W-L1" "18" "48" " " "04C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-DSP" " " "W-L2" "0" "188" "04C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-S1" " " "W-L2" "0" "47" " " "05C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-S1" "X" "W-L2" "3" "5" " " "D-S1"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-S1" "X" "W-L2" "9" "5" "01D-S1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-S1" "X" "W-L2" "15" "5" "02D-S1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-S1" "X" "W-L2" "21" "5" "03D-S1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-S1" "X" "W-L2" "27" "5" "04D-S1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-S1" "X" "W-L2" "33" "5" "05D-S1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-S1" "X" "W-L2" "39" "5" "06D-S1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-S1" "X" "W-L2" "45" "4" "07D-S1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "09D-S1" "X" "W-L2" "50" "4" "08D-S1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "10D-S1" "X" "W-L2" "55" "4" "09D-S1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-S2" " " "W-L2" "0" "47" "D-S1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-S2" "X" "W-L2" "3" "5" " " "D-S2"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-S2" "X" "W-L2" "9" "5" "01D-S2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-S2" "X" "W-L2" "15" "5" "02D-S2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-S2" "X" "W-L2" "21" "5" "03D-S2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-S2" "X" "W-L2" "27" "5" "04D-S2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-S2" "X" "W-L2" "33" "5" "05D-S2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-S2" "X" "W-L2" "39" "5" "06D-S2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-S2" "X" "W-L2" "45" "4" "07D-S2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "09D-S2" "X" "W-L2" "50" "4" "08D-S2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "10D-S2" "X" "W-L2" "55" "4" "09D-S2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-S3" " " "W-L2" "0" "47" "D-S2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-S3" "X" "W-L2" "3" "5" " " "D-S3"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-S3" "X" "W-L2" "9" "5" "01D-S3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-S3" "X" "W-L2" "15" "5" "02D-S3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-S3" "X" "W-L2" "21" "5" "03D-S3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-S3" "X" "W-L2" "27" "5" "04D-S3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-S3" "X" "W-L2" "33" "5" "05D-S3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-S3" "X" "W-L2" "39" "5" "06D-S3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-S3" "X" "W-L2" "45" "4" "07D-S3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "09D-S3" "X" "W-L2" "50" "4" "08D-S3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "10D-S4" "X" "W-L2" "55" "4" "COLUMN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-S4" " " "W-L2" "0" "47" "D-S3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-S4" "X" "W-L2" "3" "5" " " "D-S4"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-S4" "X" "W-L2" "9" "5" "01D-S4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-S4" "X" "W-L2" "15" "5" "02D-S4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-S4" "X" "W-L2" "21" "5" "03D-S4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-S4" "X" "W-L2" "27" "5" "04D-S4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-S4" "X" "W-L2" "33" "5" "05D-S4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-S4" "X" "W-L2" "39" "5" "06D-S4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-S4" "X" "W-L2" "45" "4" "07D-S4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "09D-S4" "X" "W-L2" "50" "4" "08D-S4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "10D-S4" "X" "W-L2" "55" "4" "09D-S4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-DSP" " " "W-L3" "0" "28" "05C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU" "ZZZZ-" "W-L3" "W-C" "5" " " "06C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SU" BY REFERENCE WR-SU(1) "4" "1" BY REFERENCE W-SC 2
             RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU1" "ZZZ-" "W-L3" "W-C" "4" "D-SU" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-SU1" BY REFERENCE WR-SU(1) "4" "1" BY REFERENCE W-SC 2
             RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SUT" "ZZZZZ-" "W-L3" "60" "6" "D-SU1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SUT" BY REFERENCE WR-SUT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TAN" "ZZZZZ" "W-L3" "67" "5" "D-SUT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-TAN" BY REFERENCE WR-TAN "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN" "ZZZZZZZ-" "W-L3" "73" "8" "D-TAN" " "
             RETURNING RESU.
       CALL "SD_From" USING 
            "D-KIN" BY REFERENCE W-KIND "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-DSP" " " "22" "0" "8" "06C-DSP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GSUT" "ZZZ,ZZZ-" "22" "58" "8" " " "07C-DSP"
             RETURNING RESU.
       CALL "SD_From" USING 
            "D-GSUT" BY REFERENCE W-GSUT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-DSP" " " "23" "0" "3" "07C-DSP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KOSU" "ZZ9" "23" "6" "3" " " "08C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-KOSU" BY REFERENCE W-KOSUR "3" "0" RETURNING RESU.
      *C-SPC
       CALL "SD_Init" USING 
            "C-SPC" " " "0" "0" "263" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-HNO" " " "0" "0" "26" " " "C-SPC"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01SU-HNO" " " "1" "0" "26" " " "SU-HNO"  RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-HMN" "X" "1" "25" "6" " " "01SU-HNO"  RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-DATE" "X" "1" "37" "6" "SU-HMN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-SOK" "X" "1" "49" "14" "SU-DATE" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-CNA" " " "3" "0" "55" "SU-HNO" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01SU-CNA" "X" "3" "6" "3" " " "SU-CNA"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02SU-CNA" "X" "3" "11" "52" "01SU-CNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-D1" " " "W-L1" "0" "65" "SU-CNA" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01SU-D1" "X" "W-L1" "4" "1" " " "SU-D1"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02SU-D1" "X" "W-L1" "11" "6" "01SU-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03SU-D1" "X" "W-L1" "18" "48" "02SU-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-GBI" "X" "W-L1" "71" "10" "03SU-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-D2" " " "W-L2" "0" "47" "SU-D1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01SU-D2" "X" "W-L2" "3" "5" " " "SU-D2"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02SU-D2" "X" "W-L2" "9" "5" "01SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03SU-D2" "X" "W-L2" "15" "5" "02SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04SU-D2" "X" "W-L2" "21" "5" "03SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05SU-D2" "X" "W-L2" "27" "5" "04SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06SU-D2" "X" "W-L2" "33" "5" "05SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07SU-D2" "X" "W-L2" "39" "5" "06SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08SU-D2" "X" "W-L2" "45" "4" "07SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09SU-D2" "X" "W-L2" "50" "4" "08SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10SU-D2" "X" "W-L2" "55" "4" "09SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-D3" " " "W-L3" "0" "67" "SU-D2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-D31" " " "W-L3" "0" "48" " " "SU-D3"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01SU-D31" "X" "W-L3" "1" "1" " " "SU-D31" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SU-D31" "X" "W-L3" "3" "5" "01SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03SU-D31" "X" "W-L3" "9" "5" "02SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "04SU-D31" "X" "W-L3" "15" "5" "03SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "05SU-D31" "X" "W-L3" "21" "5" "04SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "06SU-D31" "X" "W-L3" "27" "5" "05SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "07SU-D31" "X" "W-L3" "33" "5" "06SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "08SU-D31" "X" "W-L3" "39" "5" "07SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "09SU-D31" "X" "W-L3" "45" "4" "08SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "10SU-D31" "X" "W-L3" "50" "4" "09SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "11SU-D31" "X" "W-L3" "55" "4" "10SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02SU-D3" "X" "W-L3" "60" "6" "SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-TAN" "X" "W-L3" "67" "5" "02SU-D3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-KIN" "X" "W-L3" "73" "8" "SU-TAN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-SPC" " " "23" "0" "3" "SU-D3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-KOSU" "X" "23" "6" "3" " " "06C-SPC"  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "550" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "550" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "16" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "21" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "20" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "20" "E-ME4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" " " "24" "0" "22" "E-ME5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME6" "X" "24" "15" "16" " " "E-ME6"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME6" "9" "24" "34" "6" "01E-ME6" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME6" BY REFERENCE WR-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" " " "24" "0" "28" "E-ME6" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME7" "X" "24" "15" "22" " " "E-ME7"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME7" "9" "24" "40" "6" "01E-ME7" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME7" BY REFERENCE WR-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "24" "E-ME7" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "20" "E-ME8" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "23" "E-ME9" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "20" "E-ME10" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME14" "X" "24" "15" "25" "E-ME12" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME15" "X" "24" "15" "25" "E-ME14" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME20" "X" "24" "15" "19" "E-ME15" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME21" "X" "24" "15" "17" "E-ME20" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME22" "X" "24" "15" "28" "E-ME21" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME23" "X" "24" "15" "27" "E-ME22" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME25" " " "24" "0" "26" "E-ME23" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME25" "N" "24" "15" "16" " " "E-ME25" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME25" "9999/99/99" "24" "34" "10" "01E-ME25" " "
             RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME25" BY REFERENCE W-SDATE "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME26" "X" "24" "15" "24" "E-ME25" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME27" "X" "24" "15" "26" "E-ME26" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME30" "X" "24" "15" "30" "E-ME27" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME90" "X" "24" "15" "21" "E-ME30" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "MG-03" "X" "24" "15" "24" "E-ME90" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "MG-05" "X" "24" "15" "22" "MG-03" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "45" "7" "MG-05" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE HSMS-KEY "7" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           MOVE ZERO TO W-CNG.
           MOVE D-NHNG TO W-CNGS.
           IF  W-CN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-CN
           END-IF
           IF  W-CN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-CN
           END-IF
           MOVE W-CNG TO W-SNG.
           ADD 1 TO W-CG.
           IF  W-CG = 13
               ADD 1 TO W-CN
               MOVE 1 TO W-CG
           END-IF
           MOVE W-CNG TO W-ENG.
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON3-KEY" BY REFERENCE JCON3-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
           CALL "DB_F_Open" USING
            "I-O" HSMSF_PNAME1 "SHARED" BY REFERENCE HSMSF_IDLST "1"
            "HSMS-KEY" BY REFERENCE HSMS-KEY.
           CALL "DB_F_Open" USING
            "I-O" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
       M-30.
           PERFORM UAC-RTN THRU UAC-EX.
           IF  W-END NOT = 0
               GO TO M-95
           END-IF
           PERFORM UPD-RTN THRU UPD-EX.
           IF  W-END = 0
               GO TO M-30
           END-IF.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSMSF_IDLST HSMSF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *----------  出荷　入力  -------------------------------------------------
       UAC-RTN.
           CALL "SD_Screen_Output" USING "SCHD15" RETURNING RESU.
       UAC-020.
           CALL "SD_Accept" USING BY REFERENCE A-HMN "A-HMN" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               MOVE 1 TO W-END
               GO TO UAC-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-020
           END-IF
           IF  W-HMN = ZERO
               GO TO UAC-020
           END-IF
           MOVE SPACE TO HSMS-KEY.
           MOVE W-HMN TO HSMS-01.
      *           START HSMSF KEY NOT < HSMS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HSMSF_PNAME1 "HSMS-KEY" " NOT < " HSMS-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-020
           END-IF
      *           READ HSMSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSMSF_PNAME1 BY REFERENCE HSMS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-020
           END-IF
           IF  W-HMN NOT = HSMS-01
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-020
           END-IF
           IF  HSMS-02 = 7
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-020
           END-IF
           PERFORM HMN-RTN THRU HMN-EX.
           IF  W-INV = 1
               GO TO UAC-020
           END-IF
           IF  W-EC NOT = 0
               MOVE 0 TO W-EC
               GO TO UAC-020
           END-IF
           MOVE 1 TO W-DSPC.
           PERFORM DSP-RTN THRU DSP-EX.
           IF  W-TCDD NOT = 5000 AND 9850
               CALL "SD_Output" USING
                "E-ME20" E-ME20 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-020
           END-IF.
       UAC-040.
           CALL "SD_Output" USING "A-NGP" A-NGP "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-NGP "A-NGP" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO UAC-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-040
           END-IF
           IF  W-NGPS = 999999
               GO TO UAC-990
           END-IF
           IF  W-NGPS = ZERO
               MOVE DATE-02R TO W-NGPS
               CALL "SD_Output" USING "A-NGP" A-NGP "p" RETURNING RESU
           END-IF
      *
           MOVE ZERO TO W-ND1.
           IF  W-ND2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ND
           END-IF
           IF  W-ND2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ND
           END-IF
           IF  W-NGD NOT = W-SNG AND W-ENG
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-020
           END-IF
           IF  W-PD < 1 OR > 31
               GO TO UAC-020
           END-IF
      *
           MOVE ZERO TO W-HNC.
           MOVE 0 TO W-RC.
           MOVE 1 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 2 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           MOVE 3 TO W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
       UAC-070.
           ADD 1 TO W-RC.
           ADD 3 TO W-L1 W-L2 W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           IF  W-RC > 6
               GO TO UAC-800
           END-IF
           MOVE WR-D(W-RC) TO WR-D1.
           IF  WR-GNO = 0
               GO TO UAC-360
           END-IF
           IF  W-RCB = ZERO
               GO TO UAC-070
           END-IF
           IF  W-RC NOT = W-RCB
               GO TO UAC-070
           END-IF
           MOVE W-RCB TO W-RC.
           MOVE W-LB TO W-L.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           GO TO UAC-360.
       UAC-080.
           MOVE W-SCD TO WK-SCD.
           CALL "SD_Accept" USING BY REFERENCE A-SOK "A-SOK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO UAC-040
           END-IF
           IF  ESTAT = ADV
               MOVE WK-SCD TO W-SCD
               MOVE 1 TO W-ADV
               GO TO UAC-090
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-080
           END-IF.
       UAC-090.
           IF  W-SCD = ZERO
               MOVE W-SCD TO WR-SOK
               CALL "SD_Output" USING "SU-SOK" SU-SOK "p" RETURNING RESU
               GO TO UAC-100
           END-IF
           MOVE 3 TO JCON3-01.
           MOVE W-SCD TO JCON3-02.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE TO JCON3-03
               CALL "SD_Output" USING
                "D-SNA" D-SNA "p" RETURNING RESU
               CALL "SD_Output" USING
                "MG-03" MG-03 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-080
           END-IF
           MOVE W-SCD TO WR-SOK.
           CALL "SD_Output" USING "A-SOK" A-SOK "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
       UAC-100.
           IF  ESTAT = ADV
               GO TO UAC-320
           END-IF.
       UAC-260.
           CALL "SD_Accept" USING BY REFERENCE A-CCD "A-CCD" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO UAC-080
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-260
           END-IF
           PERFORM TCM-RTN THRU TCM-EX.
           IF  W-INV = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-260
           END-IF
           IF  W-INV = 9
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 1 TO W-END
               GO TO UAC-EX
           END-IF.
       UAC-320.
           MOVE 1 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 2 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           MOVE 3 TO W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           MOVE ZERO TO W-RC W-DCD W-ZC W-HSCD.
       UAC-340.
           MOVE 0 TO W-DEL.
           ADD 1 TO W-RC.
           ADD 3 TO W-L1 W-L2 W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           IF  W-RC > 6
               GO TO UAC-760
           END-IF
           IF  W-ZC = 5
               GO TO UAC-420
           END-IF.
       UAC-360.
           MOVE WR-D(W-RC) TO WR-D1.
           MOVE W-SCD TO WR-SOK.
           MOVE W-NGP TO WR-DATE.
           MOVE W-TCDD TO WR-TCD.
           MOVE W-CCDD TO WR-CCD.
       UAC-380.
           IF  W-DEL = 9
               MOVE 0 TO W-DEL
               GO TO UAC-400
           END-IF.
       UAC-400.
           MOVE W-DC(W-RC) TO WK-DC.
           MOVE 0 TO W-ADV.
           CALL "SD_Accept" USING BY REFERENCE A-DC "A-DC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF21
               MOVE W-RC TO W-RCD
               MOVE W-L TO W-LD
               GO TO UAC-430
           END-IF
           IF  ESTAT = C1 OR PF6
               MOVE 5 TO W-ZC
               GO TO UAC-420
           END-IF
           IF  ESTAT = FUK
               GO TO UAC-760
           END-IF
           IF  ESTAT = ADV
               IF  WR-HCD NOT = ZERO
                   MOVE WK-DC TO W-DC(W-RC)
                   CALL "SD_Output" USING "A-DC" A-DC "p" RETURNING RESU
                   MOVE 1 TO W-ADV
                   GO TO UAC-440
               END-IF
           END-IF
           IF  ESTAT NOT = BTB
               GO TO UAC-440
           END-IF.
       UAC-410.
           SUBTRACT 1 FROM W-RC.
           SUBTRACT 3 FROM W-L1 W-L2 W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           IF  W-RC = ZERO
               GO TO UAC-260
           END-IF
           MOVE WR-D(W-RC) TO WR-D1.
           IF  WR-GNO = ZERO
               GO TO UAC-410
           END-IF
           GO TO UAC-400.
       UAC-420.
           IF  W-RC = 1
               MOVE ZERO TO WR-HCD WR-SIZ WR-SUT WR-TAN W-KIND WR-DC
                          WR-SU(01) WR-SU(02) WR-SU(03) WR-SU(04)
                          WR-SU(05) WR-SU(06) WR-SU(07) WR-SU(08)
                          WR-SU(09) WR-SU(10)           WR-SOK
               MOVE SPACE TO WR-GBI
               MOVE W-RC TO WR-GNO
               MOVE WR-D1 TO WR-D(W-RC)
               CALL "SD_Output" USING "SU-D1" SU-D1 "p" RETURNING RESU
               CALL "SD_Output" USING "SU-D2" SU-D2 "p" RETURNING RESU
               CALL "SD_Output" USING "SU-D3" SU-D3 "p" RETURNING RESU
               GO TO UAC-340
           END-IF
           MOVE ZERO TO WR-D1.
           MOVE ZERO TO WR-SU(01) WR-SU(02) WR-SU(03) WR-SU(04)
                        WR-SU(05) WR-SU(06) WR-SU(07) WR-SU(08)
                        WR-SU(09) WR-SU(10).
           MOVE SPACE TO WR-GBI.
           MOVE WR-D1 TO WR-D(W-RC).
           CALL "SD_Output" USING "SU-D1" SU-D1 "p" RETURNING RESU.
           CALL "SD_Output" USING "SU-D2" SU-D2 "p" RETURNING RESU.
           CALL "SD_Output" USING "SU-D3" SU-D3 "p" RETURNING RESU.
           GO TO UAC-340.
       UAC-430.
           IF  W-RC NOT = 6
               COMPUTE CNT = W-RC + 1
               MOVE WR-D(CNT) TO WR-D(W-RC)
               ADD 1 TO W-RC
               GO TO UAC-430
           END-IF
           MOVE ZERO TO WR-D(W-RC).
           MOVE 0 TO W-DSPC.
           PERFORM DSP-RTN THRU DSP-EX.
           MOVE W-RCD TO W-RC.
           MOVE W-LD TO W-L.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           MOVE 9 TO W-DEL.
           GO TO UAC-360.
       UAC-440.
           IF  ESTAT NOT = HTB AND SKP AND ADV
               GO TO UAC-380
           END-IF
           IF  W-DC(W-RC) NOT = 0 AND 1 AND 2 AND 4 AND 7
               GO TO UAC-380
           END-IF
           IF  W-DC(W-RC) NOT = 2
               IF  WR-SOK = 0
                   CALL "SD_Output" USING
                    "MG-05" MG-05 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO  TO  UAC-380
               END-IF
           END-IF
           IF  W-RC NOT = 1
               GO TO UAC-450
           END-IF
           IF  W-DC(W-RC) = 1 OR 2
               MOVE ZERO TO W-HNC
           END-IF
           MOVE W-DC(W-RC) TO W-DCD.
           IF  W-DC(W-RC) = 4  OR  7
               MOVE ZERO TO W-DCD
           END-IF.
       UAC-450.
           MOVE W-DC(W-RC) TO W-DCC.
           IF  W-DC(W-RC) = 4  OR  7
               MOVE ZERO TO W-DCC
           END-IF
           IF  W-DCD NOT = W-DCC
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-380
           END-IF
           MOVE WR-HCD TO W-HCDO.
       UAC-480.
           IF  W-ADV NOT = 0
               GO TO UAC-490
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "11"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO UAC-400
           END-IF
           IF  ESTAT = C1 OR PF6
               MOVE 5 TO W-ZC
               GO TO UAC-420
           END-IF
           IF  ESTAT = FUK
               GO TO UAC-760
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-480
           END-IF
           IF  WR-HCD = ZERO
               MOVE W-HCDD TO WR-HCD
           END-IF
           IF  WR-HCD NOT = W-HCDO
               MOVE ZERO TO WR-TAN
           END-IF.
       UAC-490.
           MOVE 0 TO W-HIC.
           PERFORM HI-RTN THRU HI-EX.
           IF  W-EC NOT = ZERO
               MOVE 0 TO W-EC W-ADV
               GO TO UAC-480
           END-IF
           IF  W-DC(W-RC) = 3 OR 4 OR 9
               IF  HI-S4(10) NOT = 1
                   CALL "SD_Output" USING
                    "E-ME8" E-ME8 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   MOVE 0 TO W-ADV
                   GO  TO    UAC-480
               END-IF
           END-IF
           IF  WR-HCD > 999899
               GO TO UAC-480
           END-IF.
       UAC-550.
           IF  W-ADV NOT = 0
               GO TO UAC-570
           END-IF.
       UAC-560.
           CALL "SD_Accept" USING BY REFERENCE A-SIZ "A-SIZ" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 0 TO W-ADV
               GO TO UAC-440
           END-IF.
       UAC-570.
           IF  WR-SIZ < 1 OR > 4
               MOVE 0 TO W-ADV
               GO TO UAC-560
           END-IF
           IF  WR-SIZ = 1
               CALL "SD_Output" USING "D-S1" D-S1 "p" RETURNING RESU
               MOVE HI-SS1 TO W-ASD
           END-IF
           IF  WR-SIZ = 2
               CALL "SD_Output" USING "D-S2" D-S2 "p" RETURNING RESU
               MOVE HI-SS2 TO W-ASD
           END-IF
           IF  WR-SIZ = 3
               CALL "SD_Output" USING "D-S3" D-S3 "p" RETURNING RESU
               MOVE HI-SS3 TO W-ASD
           END-IF
           IF  WR-SIZ = 4
               CALL "SD_Output" USING "D-S4" D-S4 "p" RETURNING RESU
               MOVE HI-SS4 TO W-ASD
           END-IF
           IF  W-ASD = ZERO OR SPACE
               MOVE 0 TO W-ADV
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-560
           END-IF
           IF  WR-SIZ = 4
               IF  W-ASD = "0000000001"
                   MOVE 0 TO W-ADV
                   CALL "SD_Output" USING
                    "E-ME9" E-ME9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO UAC-560
               END-IF
           END-IF
           IF  WR-TAN = ZERO
               PERFORM TAN-RTN THRU TAN-EX
           END-IF
           MOVE ZERO TO W-SC W-ZC.
           MOVE -3 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
       UAC-580.
           ADD 1 TO W-SC.
           IF  W-SC > 8
               ADD 5 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
           ELSE
               ADD 6 TO W-C
               CALL "SD_Arg_Match_Col"
                USING "W-C" "2" W-C RETURNING RESU
           END-IF
           IF  W-SC = 11
               MOVE ZERO TO W-SC WR-SUT
               GO TO UAC-630
           END-IF
           IF (W-SD(W-SC) = ZERO) OR (W-ZC = 5)
               MOVE ZERO TO WR-SU(W-SC)
               GO TO UAC-610
           END-IF
           IF  W-SC = 10
               IF  WR-SIZ = 4
                   MOVE ZERO TO WR-SU(W-SC)
                   GO TO UAC-580
               END-IF
           END-IF.
       UAC-600.
           IF  W-ADV NOT = 0
               GO TO UAC-610
           END-IF
           IF  W-SC > 7
               CALL "SD_Accept" USING BY REFERENCE A-SU1 "A-SU1"
                "S9" "3" BY REFERENCE ESTAT RETURNING RESU
           ELSE
               CALL "SD_Accept" USING BY REFERENCE A-SU "A-SU"
                "S9" "4" BY REFERENCE ESTAT RETURNING RESU
           END-IF
           IF  ESTAT = BTB
               MOVE 0 TO W-ADV
               GO TO UAC-620
           END-IF
           IF  ESTAT = C1 OR PF6
               MOVE 5 TO W-ZC
               MOVE ZERO TO WR-SU(W-SC)
               MOVE 0 TO W-ADV
               GO TO UAC-610
           END-IF
           IF  ESTAT = FUK
               MOVE ZERO TO W-SC WR-SUT
               GO TO UAC-630
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               MOVE 0 TO W-ADV
               GO TO UAC-600
           END-IF
           IF  W-SC > 7
               IF  WR-SU(W-SC) > 999 OR < -999
                   GO TO UAC-600
               END-IF
           END-IF.
       UAC-610.
           IF  W-SC < 8
               CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-SU1" D-SU1 "p" RETURNING RESU
           END-IF
           GO TO UAC-580.
       UAC-620.
           IF  W-ADV NOT = ZERO
               MOVE 0 TO W-ADV
           END-IF
           SUBTRACT 1 FROM W-SC.
           IF  W-SC > 7
               SUBTRACT 5 FROM W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
           ELSE
               SUBTRACT 6 FROM W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
           END-IF
           IF  W-SC = ZERO
               GO TO UAC-560
           END-IF
           IF  W-SD(W-SC) = ZERO
               GO TO UAC-620
           END-IF
           IF  W-SC = 10
               IF  WR-SIZ = 4
                   GO TO UAC-620
               END-IF
           END-IF
           GO TO UAC-600.
       UAC-630.
           ADD 1 TO W-SC.
           IF  W-SC < 11
               ADD WR-SU(W-SC) TO WR-SUT
               GO TO UAC-630
           END-IF
           MOVE ZERO TO W-ZC.
           IF  WR-SUT = ZERO
               MOVE ZERO TO W-SC
               MOVE -3 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
               MOVE 0 TO W-ADV
               GO TO UAC-580
           END-IF.
       UAC-660.
           CALL "SD_Output" USING "D-SUT" D-SUT "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TAN" D-TAN "p" RETURNING RESU.
           IF  W-DC(W-RC) = 7
               MOVE ZERO TO WR-TAN W-KIND
               CALL "SD_Output" USING "SU-TAN" SU-TAN "p" RETURNING RESU
               CALL "SD_Output" USING "SU-KIN" SU-KIN "p" RETURNING RESU
               GO TO UAC-720
           END-IF
           IF  W-DC(W-RC) = 4
               COMPUTE W-KIND = WR-SUT * WR-TAN
               CALL "SD_Output" USING "SU-KIN" SU-KIN "p" RETURNING RESU
               CALL "SD_Output" USING "SU-TAN" SU-TAN "p" RETURNING RESU
               GO TO UAC-720
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-TAN "A-TAN" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT NOT = BTB
               GO TO UAC-670
           END-IF
           MOVE 0 TO W-ADV.
           MOVE 11 TO W-SC.
           MOVE 60 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           GO TO UAC-620.
       UAC-670.
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-660
           END-IF
           CALL "SD_Output" USING "D-TAN" D-TAN "p" RETURNING RESU.
           IF  WR-HCD < 999900
               IF  WR-TAN = ZERO
                   IF  W-DC(W-RC) = 2
                       CALL "SD_Output" USING
                        "E-ME15" E-ME15 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME99" E-ME99 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-CL" E-CL "p" RETURNING RESU
                   ELSE
                       GO TO UAC-660
                   END-IF
               END-IF
           END-IF
           IF  WR-HCD > 999899
               IF  WR-TAN = ZERO
                   CALL "SD_Output" USING
                    "E-ME15" E-ME15 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               END-IF
           END-IF
           COMPUTE W-KIND = WR-SUT * WR-TAN.
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
       UAC-720.
           IF  T-BIK = 0
               MOVE SPACE TO WR-GBI
               CALL "SD_Output" USING "SU-GBI" SU-GBI "p" RETURNING RESU
               GO TO UAC-740
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-GBI "A-GBI" "X" "10"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-DC(W-RC) = 4 OR 7
                   GO TO UAC-620
               ELSE
                   GO TO UAC-660
               END-IF
           END-IF.
       UAC-740.
           MOVE W-RC TO WR-GNO.
           MOVE WR-D1 TO WR-D(W-RC).
           GO TO UAC-340.
       UAC-760.
           MOVE ZERO TO W-GSUT W-RC W-ZC.
       UAC-770.
           ADD 1 TO W-RC.
           IF  W-RC = 7
               CALL "SD_Output" USING "D-GSUT" D-GSUT "p" RETURNING RESU
               IF  W-ZC = 0
                   CALL "SD_Output" USING
                    "E-ME30" E-ME30 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO UAC-780
               ELSE
                   GO TO UAC-780
               END-IF
           END-IF
           MOVE ZERO TO WR-D1.
           MOVE WR-D(W-RC) TO WR-D1.
           IF  WR-HCD < 999900
               ADD WR-SUT TO W-GSUT
           END-IF
           IF  WR-DATE NOT = ZERO
               MOVE W-RC TO W-HNCD
           END-IF
           IF  ZERO = WR-SU(01) AND WR-SU(02) AND WR-SU(03) AND
                     WR-SU(04) AND WR-SU(05) AND WR-SU(06) AND
                     WR-SU(07) AND WR-SU(08) AND WR-SU(09) AND
                     WR-SU(10) AND WR-SUT AND W-KIND
               GO TO UAC-770
           END-IF
           MOVE 1 TO W-ZC.
           GO TO UAC-770.
       UAC-780.
           IF  W-BI = SPACE
               CALL "SD_Output" USING "A-BI" A-BI "p" RETURNING RESU
           END-IF
           IF  W-HNCD < W-HNC
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
           END-IF.
       UAC-800.
           MOVE 0 TO W-ADV.
           CALL "SD_Output" USING "A-BI" A-BI "p" RETURNING RESU.
           MOVE W-BI TO WK-BI.
           CALL "SD_Accept" USING BY REFERENCE A-BI "A-BI" "N" "48"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 7 TO W-RC
               MOVE 22 TO W-L1
               CALL "SD_Arg_Match_Line" USING
                "W-L1" "2" W-L1 RETURNING RESU
               MOVE 23 TO W-L2
               CALL "SD_Arg_Match_Line" USING
                "W-L2" "2" W-L2 RETURNING RESU
               MOVE 24 TO W-L3
               CALL "SD_Arg_Match_Line" USING
                "W-L3" "2" W-L3 RETURNING RESU
               GO TO UAC-410
           END-IF
           IF  ESTAT = ADV
               MOVE WK-BI TO W-BI
               CALL "SD_Output" USING "A-BI" A-BI "p" RETURNING RESU
               MOVE 1 TO W-ADV
               GO TO UAC-840
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-780
           END-IF.
       UAC-840.
           IF  W-ADV NOT = 0
               GO TO UAC-850
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-KOSU "A-KOSU" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO UAC-800
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-840
           END-IF.
       UAC-850.
           CALL "SD_Output" USING "D-KOSU" D-KOSU "p" RETURNING RESU.
       UAC-940.
           CALL "SD_Output" USING "A-SDT" A-SDT "p" RETURNING RESU.
           IF  W-SDTD NOT = ZERO
               GO TO UAC-980
           END-IF
           IF  T-SS = ZERO OR 99
               GO TO UAC-990
           END-IF.
       UAC-960.
           CALL "SD_Accept" USING BY REFERENCE A-SDT "A-SDT" "9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 0 TO W-ADV
               GO TO UAC-840
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-960
           END-IF
           IF  T-SS = ZERO OR 99
               IF  W-SDTD NOT = ZERO
                   GO TO UAC-960
               END-IF
           END-IF
           IF  W-DCD NOT = 3
               IF  W-SDTD = 99999999
                   GO TO UAC-960
               END-IF
           END-IF
           IF  W-SDTD = ZERO OR 99999999
               GO TO UAC-990
           END-IF
           IF  W-SDTG < 1 OR > 12
               GO TO UAC-960
           END-IF
           IF  W-SDTP < 1 OR > 31
               GO TO UAC-960
           END-IF
           IF  W-SDTN NOT = W-ND AND (W-ND + 1)
               GO TO UAC-960
           END-IF.
       UAC-980.
           IF  W-SDATE NOT = ZERO
               IF  W-SDATE >= W-SDTD
                   CALL "SD_Output" USING
                    "E-ME25" E-ME25 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
                   GO TO UAC-960
               END-IF
           END-IF
           IF W-NGP > W-SDTD
               GO TO UAC-960
           END-IF.
       UAC-990.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-NGPS = 999999
                   GO TO UAC-040
               ELSE
                   GO TO UAC-960
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-990
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "E-ME90" E-ME90 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-040
           END-IF
           IF  W-DMM NOT = 1
               GO TO UAC-990
           END-IF.
       UAC-EX.
           EXIT.
       TAN-RTN.
           MOVE 0 TO WK-TAN.
           MOVE WR-TCD TO THT-TCD.
           MOVE WR-HCD TO THT-HCD.
           MOVE WR-SIZ TO THT-SIZ.
      *           READ THTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO THT-T
           END-IF
           MOVE THT-T TO WK-TAN.
           IF  WK-TAN NOT = ZERO
               GO TO TAN-090
           END-IF
           MOVE WR-TCD TO THT-TCD.
           MOVE WR-HCD TO THT-HCD.
           MOVE 9 TO THT-SIZ.
      *           READ THTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO THT-T
           END-IF
           MOVE THT-T TO WK-TAN.
           IF  WK-TAN NOT = ZERO
               GO TO TAN-090
           END-IF
           IF  HI-MHCD = HI-HCD
               GO TO TAN-090
           END-IF
           MOVE WR-TCD TO THT-TCD.
           MOVE HI-MHCD TO THT-HCD.
           MOVE WR-SIZ TO THT-SIZ.
      *           READ THTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO THT-T
           END-IF
           MOVE THT-T TO WK-TAN.
           IF  WK-TAN NOT = ZERO
               GO TO TAN-090
           END-IF
           MOVE WR-TCD TO THT-TCD.
           MOVE HI-MHCD TO THT-HCD.
           MOVE 9 TO THT-SIZ.
      *           READ THTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO THT-T
           END-IF
           MOVE THT-T TO WK-TAN.
       TAN-090.
           IF  WK-TAN NOT = ZERO
               MOVE WK-TAN TO WR-TAN
               GO TO TAN-EX
           END-IF.
       TAN-EX.
           EXIT.
      *------------------------------------------------------------------------
       HMN-RTN.
           MOVE HSMS-05 TO W-NGP.
           CALL "SD_Output" USING "A-NGP" A-NGP "p" RETURNING RESU.
           INITIALIZE WR-AD WR-D2.
           MOVE SPACE TO W-BI.
           MOVE ZERO TO W-DCD W-AKIN W-ADC WB-AD.
           MOVE HSMS-061 TO W-TCDD.
           PERFORM TM-RTN THRU TM-EX.
           IF  W-INV = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO HMN-EX
           END-IF
           MOVE 0 TO W-RC.
       HMN-020.
           IF  HSMS-02 = 7
               MOVE HSMS-R2 TO WR-D2
               GO TO HMN-035
           END-IF
           INITIALIZE WR-D1.
           MOVE HSMS-R1 TO WR-D1.
           MOVE 1 TO W-HIC.
           PERFORM HI-RTN THRU HI-EX.
           IF  HSMS-25 NOT = 1
               PERFORM TAN-RTN THRU TAN-EX
           END-IF
           ADD 1 TO W-RC.
           IF  W-RC > 6
               MOVE 1 TO W-EC
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO HMN-EX
           END-IF
           MOVE WR-D1 TO WR-D(W-RC).
           MOVE WR-HCD TO WB-HCD(W-RC).
           MOVE WR-SIZ TO WB-SIZ(W-RC).
           MOVE WR-TAN TO WB-TAN(W-RC).
           IF  WR-ACD = 4 OR 5
               MOVE ZERO TO W-KIN(W-RC)
               MOVE 4 TO W-DC(W-RC)
           ELSE
               COMPUTE W-KIND = WR-SUT * WR-TAN
               MOVE W-KIND TO W-KIN(W-RC)
               MOVE WR-DC TO W-DC(W-RC)
           END-IF.
       HMN-035.
      *           READ HSMSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSMSF_PNAME1 BY REFERENCE HSMS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO HMN-040
           END-IF
           IF  W-HMN = HSMS-01
               GO TO HMN-020
           END-IF.
       HMN-040.
           MOVE WR-SOK TO W-SCD.
           MOVE WR-CCD TO W-CCDD.
           MOVE WR-KSU TO W-KOSUR.
       HMN-060.
           MOVE 3 TO JCON3-01.
           MOVE W-SCD TO JCON3-02.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE TO JCON3-03
           END-IF
           MOVE W-SCD TO WR-SOK.
           CALL "SD_Output" USING "A-SOK" A-SOK "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
      *
           IF  WR-DC = 1 OR 2
               MOVE WR-DC TO W-DCD
               MOVE ZERO TO W-HNC
           ELSE
               MOVE 1 TO W-HNC
           END-IF
           PERFORM TCM-RTN THRU TCM-EX.
           IF  W-INV = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO HMN-EX
           END-IF
           CALL "SD_Output" USING "A-BI" A-BI "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KOSU" D-KOSU "p" RETURNING RESU.
       HMN-EX.
           EXIT.
      *------------------------------------------------------------------------
       DSP-RTN.
           MOVE 1 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 2 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           MOVE 3 TO W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           MOVE ZERO TO W-RC W-ZC W-GSUT W-RCB W-LB.
       DSP-020.
           ADD 1 TO W-RC.
           ADD 3 TO W-L1 W-L2 W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           IF  W-RC > 6
               GO TO DSP-080
           END-IF
      *
           INITIALIZE WR-D1.
           MOVE WR-D(W-RC) TO WR-D1.
           IF  WR-HCD = ZERO
               CALL "SD_Output" USING "SU-D1" SU-D1 "p" RETURNING RESU
               CALL "SD_Output" USING "SU-D2" SU-D2 "p" RETURNING RESU
               CALL "SD_Output" USING "SU-D3" SU-D3 "p" RETURNING RESU
               GO TO DSP-020
           END-IF
           MOVE W-KIN(W-RC) TO W-KIND.
           MOVE WR-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
           END-IF
           CALL "SD_Output" USING "A-DC" A-DC "p" RETURNING RESU.
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           IF  WR-HCD >= 999900
               CALL "SD_Output" USING "SU-D2" SU-D2 "p" RETURNING RESU
               CALL "SD_Output" USING "SU-D31" SU-D31 "p" RETURNING RESU
               GO TO DSP-060
           END-IF
           CALL "SD_Output" USING "A-SIZ" A-SIZ "p" RETURNING RESU.
           IF  WR-SIZ = 1
               CALL "SD_Output" USING "D-S1" D-S1 "p" RETURNING RESU
           END-IF
           IF  WR-SIZ = 2
               CALL "SD_Output" USING "D-S2" D-S2 "p" RETURNING RESU
           END-IF
           IF  WR-SIZ = 3
               CALL "SD_Output" USING "D-S3" D-S3 "p" RETURNING RESU
           END-IF
           IF  WR-SIZ = 4
               CALL "SD_Output" USING "D-S4" D-S4 "p" RETURNING RESU
           END-IF
           MOVE ZERO TO W-SC.
           MOVE -3 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
       DSP-040.
           ADD 1 TO W-SC.
           IF  W-SC > 8
               ADD 5 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
           ELSE
               ADD 6 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
           END-IF
           IF  W-SC = 11
               GO TO DSP-060
           END-IF
           IF  W-SC < 8
               CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-SU1" D-SU1 "p" RETURNING RESU
           END-IF
           GO TO DSP-040.
       DSP-060.
           CALL "SD_Output" USING "D-SUT" D-SUT "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TAN" D-TAN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-GBI" A-GBI "p" RETURNING RESU.
           IF  WR-DC = 4
               CALL "SD_Output" USING "SU-KIN" SU-KIN "p" RETURNING RESU
               CALL "SD_Output" USING "SU-TAN" SU-TAN "p" RETURNING RESU
           END-IF
           ADD WR-SUT TO W-GSUT.
           IF  WR-DC NOT = 7
               IF  WR-TAN = ZERO
                   CALL "SD_Output" USING
                    "E-ME15" E-ME15 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
                   IF  W-DSPC = 1
                       IF  W-RCB = ZERO
                           MOVE W-RC TO W-RCB
                           MOVE W-L TO W-LB
                       END-IF
                   END-IF
               END-IF
           END-IF
           GO TO DSP-020.
       DSP-080.
           CALL "SD_Output" USING "D-GSUT" D-GSUT "p" RETURNING RESU.
       DSP-EX.
           EXIT.
      *------------------------------------------------------------------------
       TM-RTN.
           MOVE 0 TO W-INV.
           MOVE W-TCDD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE 1 TO W-INV
           END-IF
           CALL "SD_Output" USING "D-TCD" D-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
      *
           MOVE W-TCDD TO TSK-KEY.
      *           READ TSKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO TSK-R
           END-IF
           MOVE TSK-ZNGP(4) TO W-SDATE.
           IF  TSK-ZNGP(5) NOT = ZERO
               MOVE TSK-ZNGP(5) TO W-SDATE
           END-IF
      *
           MOVE ZERO TO W-SDTD.
           MOVE W-NGP TO W-SDTD.
       TM-020.
           MOVE T-SS TO W-SDTP.
           IF  W-SDTP = 00 OR 99
               MOVE ZERO TO W-SDTD
               GO TO TM-EX
           END-IF
           IF  W-SDTP = 30 OR 31
               IF  W-SDTG = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
                   MOVE 31 TO W-SDTP
               ELSE
                   IF  W-SDTG = 4 OR 6 OR 9 OR 11
                       MOVE 30 TO W-SDTP
                   ELSE
                       DIVIDE 4 INTO W-SDTN GIVING W-DTW1
                                                 REMAINDER W-DTW2
                       IF  W-DTW2 = 0
                           MOVE 29 TO W-SDTP
                       ELSE
                           MOVE 28 TO W-SDTP
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  W-NGP > W-SDTD
               IF  W-GD NOT = W-SDTG
                   MOVE ZERO TO W-SDTD
                   GO TO TM-EX
               ELSE
                   ADD 1 TO W-SDTG
                   GO TO TM-040
               END-IF
           END-IF
           IF  W-SDATE NOT = ZERO
               IF  W-SDATE >= W-SDTD
                   ADD 1 TO W-SDTG
                   GO TO TM-040
               END-IF
           END-IF
           GO TO TM-EX.
       TM-040.
           IF  W-SDTG = 13
               MOVE 1 TO W-SDTG
               ADD 1 TO W-SDTN
           END-IF
           GO TO TM-020.
       TM-EX.
           EXIT.
       TCM-RTN.
           IF  W-CCDD = ZERO
               CALL "SD_Output" USING "SU-CNA" SU-CNA "p" RETURNING RESU
               GO TO TCM-EX
           END-IF
           MOVE 0 TO W-INV.
           MOVE W-TCDD TO TC-TCD.
           MOVE W-CCDD TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
               MOVE 1 TO W-INV
           END-IF
           CALL "SD_Output" USING "A-CCD" A-CCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-CNA" D-CNA "p" RETURNING RESU.
           MOVE 9999999 TO TC-KEY.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-INV
           END-IF.
       TCM-EX.
           EXIT.
      *------------------------------------------------------------------------
       HI-RTN.
           MOVE WR-HCD TO W-HCDD.
           IF  W-HIC = 0
               CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU
           END-IF
           IF  WR-HCD = ZERO
               MOVE 9 TO W-EC
               GO TO HI-EX
           END-IF
           MOVE WR-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-EC
               GO TO HI-EX
           END-IF
           IF  W-HIC = 0
               CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU
           END-IF
           IF  WR-HCD < 999900
               IF  HI-FT = ZERO
                   CALL "SD_Output" USING
                    "E-ME7" E-ME7 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
               END-IF
           END-IF
           IF  WR-DC NOT = 0 AND 3 AND 4 AND 5 AND 7
               IF  WR-HCD < 999900
                   IF  HI-FT = 1
                       CALL "SD_Output" USING
                        "E-ME7" E-ME7 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME98" E-ME98 "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF.
       HI-EX.
           EXIT.
      *------------------------------------------------------------------------
       UPD-RTN.
           MOVE SPACE TO HSMS-KEY.
           MOVE W-HMN TO HSMS-01.
      *           START HSMSF KEY NOT < HSMS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HSMSF_PNAME1 "HSMS-KEY" " NOT < " HSMS-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-900
           END-IF
      *           READ HSMSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSMSF_PNAME1 BY REFERENCE HSMS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-900
           END-IF
           IF  W-HMN NOT = HSMS-01
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-900
           END-IF.
       UPD-020.
      *           DELETE HSMSF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HSMSF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME22" E-ME22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-900
           END-IF
      *
      *           READ HSMSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSMSF_PNAME1 BY REFERENCE HSMS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO UPD-040
           END-IF
           IF  W-HMN = HSMS-01
               GO TO UPD-020
           END-IF.
       UPD-040.
           IF  W-NGPS = 999999
               GO TO UPD-EX
           END-IF
           MOVE ZERO TO W-RC.
       UPD-060.
           ADD 1 TO W-RC.
           IF  W-RC > 6
               GO TO UPD-180
           END-IF
           MOVE WR-D(W-RC) TO WR-D1.
           IF  WR-DATE = ZERO
               GO TO UPD-180
           END-IF
           MOVE W-SDTD TO WR-SKD.
           MOVE W-KOSUR TO WR-KSU.
           IF  W-DC(W-RC) NOT = 4
               MOVE W-DC(W-RC) TO WR-DC
               MOVE 0 TO WR-ACD
           ELSE
               IF  WR-ACD NOT = 4 AND 5
                   MOVE 0 TO WR-DC
                   MOVE 4 TO WR-ACD
               END-IF
           END-IF
           IF  WR-NRC NOT = 1
               IF (WR-HCD = WB-HCD(W-RC)) AND (WR-SIZ = WB-SIZ(W-RC))
                   IF  WR-TAN NOT = WB-TAN(W-RC)
                       MOVE 1 TO WR-NRC
                   END-IF
               END-IF
           END-IF
           MOVE WR-D1 TO HSMS-R1.
      *           WRITE HSMS-R1 INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HSMSF_PNAME1 HSMSF_LNAME HSMS-R1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME23" E-ME23 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-900
           END-IF.
       UPD-080.
           IF  WR-HCD = ZERO
               GO TO UPD-180
           END-IF
           IF  W-DC(W-RC) NOT = 0
               GO TO UPD-060
           END-IF
           IF  WR-TAN = ZERO
               GO TO UPD-060
           END-IF
           MOVE SPACE TO THT-KEY.
           MOVE WR-TCD TO THT-TCD.
           MOVE WR-HCD TO THT-HCD.
           MOVE 9 TO THT-SIZ.
      *           READ THTM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" THTM_PNAME1 BY REFERENCE THT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO UPD-120
           END-IF
           GO TO UPD-160.
       UPD-120.
           MOVE SPACE TO THT-KEY.
           MOVE WR-TCD TO THT-TCD.
           MOVE WR-HCD TO THT-HCD.
           MOVE WR-SIZ TO THT-SIZ.
      *           READ THTM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" THTM_PNAME1 BY REFERENCE THT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO UPD-140
           END-IF
           GO TO UPD-160.
       UPD-140.
           MOVE ZERO TO THT-R.
           MOVE WR-TCD TO THT-TCD THT-TCD2.
           MOVE WR-HCD TO THT-HCD.
           MOVE 9 TO THT-SIZ.
           MOVE WR-TAN TO THT-T.
           MOVE T-TNC TO THT-TNC.
           MOVE W-NGD TO THT-NG.
      *           WRITE THT-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            THTM_PNAME1 THTM_LNAME THT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME26" E-ME26 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO UPD-020
           END-IF
           GO TO UPD-060.
       UPD-160.
           IF W-NGD NOT = THT-NG
               MOVE W-NGD TO THT-NG
      *               REWRITE THT-R INVALID KEY
      *///////////////
               CALL "DB_Update" USING
                THTM_PNAME1 THTM_LNAME THT-R RETURNING RET
               IF  RET = 1
                   CALL "SD_Output" USING
                    "E-STAT" E-STAT "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME27" E-ME27 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               END-IF
           END-IF
           GO TO UPD-060.
       UPD-180.
           MOVE W-NGP TO W-DATEB.
           MOVE W-TCDD TO W-TCDB.
           MOVE W-CCDD TO W-CCDB.
           MOVE W-SCD TO W-SOKB.
           MOVE WR-D2 TO HSMS-R2.
      *           WRITE HSMS-R2 INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HSMSF_PNAME1 HSMSF_LNAME HSMS-R2 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME23" E-ME23 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-900
           END-IF
           GO TO UPD-EX.
       UPD-900.
           MOVE 1 TO W-END.
       UPD-EX.
           EXIT.

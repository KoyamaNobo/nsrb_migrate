       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY930.
      *********************************************************
      *    PROGRAM         :  年間履物品名サイズ別　受払表    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  作表=0 , EXCEL変換=1            *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0512".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  N(002) VALUE "【　".
           02  F              PIC  X(001) VALUE "'".
           02  H-SNEN         PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-SGET         PIC Z9.
           02  F              PIC  N(001) VALUE "月".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(001) VALUE "～".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(001) VALUE "'".
           02  H-ENEN         PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-EGET         PIC Z9.
           02  F              PIC  N(001) VALUE "月".
           02  F              PIC  N(002) VALUE "　】".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(025) VALUE
                "＊＊＊　　履物　年間品名サイズ別　受払表　　＊＊＊".
           02  F              PIC  X(024) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(006) VALUE "品　　　名　".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(001) VALUE "1".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "  SS".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "   S".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "   M".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "   L".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "  LL".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "28.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "29.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "30.0".
           02  F              PIC  X(031) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(022) VALUE SPACE.
           02  F              PIC  X(001) VALUE "2".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "12.5".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.5".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "14.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "15.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "16.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "17.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "18.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "19.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "20.0".
           02  F              PIC  X(031) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(022) VALUE SPACE.
           02  F              PIC  X(001) VALUE "3".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.5".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.5".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.5".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(039) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(022) VALUE SPACE.
           02  F              PIC  X(001) VALUE "4".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.5".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.5".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.5".
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　合　計".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "振替単価".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "振替金額".
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
       01  W-P1.
           02  F              PIC  X(003).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(076).
       01  W-P2.
           02  P-TM           PIC  N(007).
           02  F              PIC  X(001).
           02  P-UHM          PIC  N(003).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  9(001).
           02  P-SUD.
             03  P-SU         PIC ----,--9  OCCURS  10 TIMES.
           02  P-SUT          PIC --,---,--9.
           02  P-FT           PIC ZZZ,ZZ9.
           02  P-KIN          PIC --,---,---,--9.
       01  W-DATA.
           02  W-SHCD         PIC  9(006).
           02  W-EHCD         PIC  9(006) VALUE 999999.
           02  W-DMM          PIC  9(001).
           02  W-HCD          PIC  9(006).
           02  CNT            PIC  9(002).
           02  W-DZC          PIC  9(001).
           02  W-PAGE         PIC  9(003) VALUE ZERO.
           02  W-BC3          PIC  9(002).
           02  W-BMNO         PIC  9(001).
           02  W-BC1          PIC  9(002).
           02  W-C            PIC  9(001).
           02  W-CC           PIC  9(001).
           02  W-MC           PIC  9(001) VALUE 0.
           02  W-HC           PIC  9(001).
           02  W-SC           PIC  9(001).
           02  W-ZCD.
             03  W-ZC    OCCURS   4  PIC  9(001).
           02  W-SU           PIC S9(006).
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NEND  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGD   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
       01  W-D.
           02  W-AZC.
             03  W-ZZCD.
               04  W-ZZC   OCCURS   4  PIC  9(001).
             03  W-NZCD.
               04  W-NZC   OCCURS   4  PIC  9(001).
             03  W-SZCD.
               04  W-SZC   OCCURS   4  PIC  9(001).
             03  W-YZCD.
               04  W-YZC   OCCURS   4  PIC  9(001).
           02  W-ASUD.
             03  W-ASU   OCCURS   4.
               04  W-ZSU   OCCURS  10  PIC S9(006).
               04  W-NSU   OCCURS  10  PIC S9(006).
               04  W-SSU   OCCURS  10  PIC S9(006).
               04  W-YSU   OCCURS  10  PIC S9(006).
           02  W-SUTD.
             03  W-SUT   OCCURS   4  PIC S9(007).
           02  W-KIND.
             03  W-KIN   OCCURS   4  PIC S9(010).
       01  WA-D.
           02  WA-AZC.
             03  WA-ZZCD.
               04  WA-ZZC   OCCURS   4  PIC  9(001).
             03  WA-NZCD.
               04  WA-NZC   OCCURS   4  PIC  9(001).
             03  WA-SZCD.
               04  WA-SZC   OCCURS   4  PIC  9(001).
             03  WA-YZCD.
               04  WA-YZC   OCCURS   4  PIC  9(001).
           02  WA-SUD.
             03  WA-SU    OCCURS   4.
               04  WA-ZSU   OCCURS  10  PIC S9(006).
               04  WA-NSU   OCCURS  10  PIC S9(006).
               04  WA-SSU   OCCURS  10  PIC S9(006).
               04  WA-YSU   OCCURS  10  PIC S9(006).
           02  WA-SUTD.
             03  WA-SUT   OCCURS   4  PIC S9(007).
           02  WA-KIND.
             03  WA-KIN   OCCURS   4  PIC S9(010).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LIHIM.
           COPY LSPF.
       01  HHTF_HMY930.
           02  HHTF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HHTF_LNAME     PIC  X(011) VALUE "HHTF_HMY930".
           02  F              PIC  X(001).
           02  HHTF_KEY1      PIC  X(100) VALUE SPACE.
           02  HHTF_KEY2      PIC  X(100) VALUE SPACE.
           02  HHTF_SORT      PIC  X(100) VALUE SPACE.
           02  HHTF_IDLST     PIC  X(100) VALUE SPACE.
           02  HHTF_RES       USAGE  POINTER.
       01  HHT-R.
           02  HHT-KEY.
             03  HHT-HCD      PIC  9(006).                              品名ｺｰﾄﾞ
             03  HHT-HCDD  REDEFINES HHT-HCD.
               04  HHT-HCD1   PIC  9(004).
               04  HHT-HCD2   PIC  9(002).
             03  HHT-SIZ      PIC  9(001).                              ｻｲｽﾞ区分
           02  HHT-AZSU.                                                前月残数
             03  HHT-ZSUD  OCCURS  10.
               04  HHT-ZSU    PIC S9(006)  COMP-3.
           02  HHT-ANSU.                                                入庫数
             03  HHT-NSUD  OCCURS  10.
               04  HHT-NSU    PIC S9(006)  COMP-3.
           02  HHT-AUSU.                                                出庫数
             03  HHT-USUD  OCCURS  10.
               04  HHT-USU    PIC S9(006)  COMP-3.
           02  HHT-AYSU.                                                前月残数
             03  HHT-YSUD  OCCURS  10.
               04  HHT-YSU    PIC S9(006)  COMP-3.
           02  F              PIC  9(002).
           02  HHT-SNEN       PIC  9(002).
           02  HHT-SGET       PIC  9(002).
           02  F              PIC  9(002).
           02  HHT-ENEN       PIC  9(002).
           02  HHT-EGET       PIC  9(002).
           02  HHT-BCD1.
             03  HHT-BC1      PIC  9(002).
             03  HHT-BC21     PIC  9(001).
           02  HHT-BC22       PIC  9(001).
           02  HHT-BC3        PIC  9(002).                              分類CD3
           02  HHT-BMNO       PIC  9(001).
           02  F              PIC  X(070).
           02  F              PIC  X(256).
       77  F                  PIC  X(001).
       01  EXL-F_HMY930.
           02  EXL-F_PNAME1  PIC  X(009)  VALUE "WK0256000".
           02  F             PIC  X(001).
           02  EXL-F_LNAME   PIC  X(012)  VALUE "EXL-F_HMY930".
           02  F             PIC  X(001).
           02  EXL-F_KEY1    PIC  X(100)  VALUE SPACE.
           02  EXL-F_KEY2    PIC  X(100)  VALUE SPACE.
           02  EXL-F_SORT    PIC  X(100)  VALUE SPACE.
           02  EXL-F_IDLST   PIC  X(100)  VALUE SPACE.
           02  EXL-F_RES     USAGE  POINTER.
       01  EXL-R.
           02  EXL-HCD        PIC  9(006).
           02  EXL-HNA        PIC  N(024).
           02  EXL-KBN        PIC  N(002).
           02  EXL-SIZ        PIC  9(001).
           02  EXL-ASU.
             03  EXL-SUD  OCCURS  10.
               04  EXL-SU     PIC S9(006).
             03  EXL-SUT      PIC S9(007).
           02  EXL-FT         PIC  9(005).
           02  EXL-KIN        PIC S9(010).
           02  F              PIC  X(115).
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
           02  C-CL  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　年間品名サイズ別　受払表　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(024) VALUE
                "'  年   月 ～ '  年   月".
           02  FILLER  PIC  X(026) VALUE
                "品名ｺｰﾄﾞ  000000 ～ 999999".
           02  FILLER  PIC X(22) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SHCD    PIC  9(006).
             03  A-EHCD    PIC  9(006).
           02  A-DMM     PIC  9(001).
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC Z9.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC Z9.
           02  D-EXL     PIC  N(022) VALUE
                "＊＊＊　　（　ＥＸＣＥＬ　変換　）　　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-STAT    PIC  X(002).
             03  E-ME1     PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME5     PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
             03  E-CL      PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "380" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "12" "19" "24" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "15" "19" "26" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "22" "34" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "15" "0" "12" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SHCD" "9" "15" "29" "6" " " "A-PNG" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SHCD" BY REFERENCE W-SHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EHCD" "9" "15" "39" "6" "A-SHCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EHCD" BY REFERENCE W-EHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "22" "51" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "52" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "12" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "9" "12" "20" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE HHT-SNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "Z9" "12" "25" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE HHT-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NG" "9" "12" "34" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE HHT-ENEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NG" "Z9" "12" "39" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NG" BY REFERENCE HHT-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-EXL" "N" "7" "10" "44" "D-NG" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "97" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "97" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME5" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                         RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "D-EXL" D-EXL "p" 
                                         RETURNING RESU
           END-IF.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0512ID.
           MOVE WK0512ID TO HHTF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HHTF_PNAME1 " " BY REFERENCE HHTF_IDLST "0".
      *           READ HHTF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HHTF_PNAME1 BY REFERENCE HHT-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HHTF_IDLST HHTF_PNAME1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-95
           END-IF.
           CALL "SD_Output" USING "D-NG" D-NG "p" 
                                  RETURNING RESU.
           IF  JS-SIGN = 0
               MOVE HHT-SNEN TO H-SNEN
               MOVE HHT-SGET TO H-SGET
               MOVE HHT-ENEN TO H-ENEN
               MOVE HHT-EGET TO H-EGET
           END-IF.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD "A-SHCD" "9" "6" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               GO TO M-95
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD "A-EHCD" "9" "6" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               GO TO M-95
           END-IF.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
           IF  W-SHCD > W-EHCD
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
           IF  W-DMM = 9
               GO TO M-10
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                         RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "D-EXL" D-EXL "p" 
                                  RETURNING RESU
           END-IF.
           CALL "SD_Output" USING "D-NG" D-NG "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "A-SHCD" A-SHCD "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "A-EHCD" A-EHCD "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "A-DMM" A-DMM "p" 
                                  RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" HHTF_PNAME1 " " BY REFERENCE HHTF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-30.
      *           READ HHTF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HHTF_PNAME1 BY REFERENCE HHT-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HHTF_IDLST HHTF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               GO TO M-95
           END-IF.
           IF  HHT-HCD < W-SHCD OR > W-EHCD
               GO TO M-30
           END-IF.
           MOVE HHT-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO HI-OL
           END-IF.
           IF  HI-OL = 1
               GO TO M-30
           END-IF.
           PERFORM CHK-RTN THRU CHK-EX.
           IF  W-DZC = 0
               GO TO M-30
           END-IF.
      *
           IF  JS-SIGN = 1
               CALL "DB_F_Open" USING 
                "OUTPUT" EXL-F_PNAME1 " " BY REFERENCE EXL-F_IDLST "0"
               GO TO M-35
           END-IF.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           CALL "PR_Open" RETURNING RESP.
           PERFORM MID-020 THRU MID-EX.
       M-35.
           MOVE ZERO TO WA-D W-C.
           MOVE HHT-BC3 TO W-BC3.
           MOVE HHT-BMNO TO W-BMNO.
           MOVE HHT-BC1 TO W-BC1.
       M-40.
           MOVE ZERO TO W-D W-HC.
           MOVE HHT-HCD TO W-HCD.
       M-45.
           PERFORM SET-RTN THRU SET-EX.
       M-50.
      *           READ HHTF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HHTF_PNAME1 BY REFERENCE HHT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
           IF  HHT-HCD < W-SHCD OR > W-EHCD
               GO TO M-50
           END-IF.
           MOVE HHT-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO HI-OL
           END-IF.
           IF  HI-OL = 1
               GO TO M-50
           END-IF.
           PERFORM CHK-RTN THRU CHK-EX.
           IF  W-DZC = 0
               GO TO M-50
           END-IF.
           IF (HHT-BC3 NOT = W-BC3) OR (HHT-BMNO NOT = W-BMNO) OR
              (HHT-BC1 NOT = W-BC1)
               GO TO M-55
           END-IF.
           IF  HHT-HCD = W-HCD
               GO TO M-45
           END-IF.
           IF  JS-SIGN = 0
               PERFORM PR2-RTN THRU PR2-EX
           ELSE
               PERFORM PRE-RTN THRU PRE-EX
           END-IF.
           GO TO M-40.
       M-55.
           IF  JS-SIGN = 0
               PERFORM PR2-RTN THRU PR2-EX
               PERFORM KEI-RTN THRU KEI-EX
           ELSE
               PERFORM PRE-RTN THRU PRE-EX
               PERFORM KEE-RTN THRU KEE-EX
           END-IF.
           GO TO M-35.
       M-90.
           IF  JS-SIGN = 0
               PERFORM PR2-RTN THRU PR2-EX
               PERFORM KEI-RTN THRU KEI-EX
           ELSE
               PERFORM PRE-RTN THRU PRE-EX
               PERFORM KEE-RTN THRU KEE-EX
           END-IF.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           IF  JS-SIGN = 1
               CALL "DB_F_Close" USING
                BY REFERENCE EXL-F_IDLST EXL-F_PNAME1
           ELSE
               CALL "PR_Close" RETURNING RESP
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
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
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD4 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
       CHK-RTN.
           MOVE ZERO TO W-DZC CNT.
       CHK-020.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO CHK-EX
           END-IF.
           IF  HHT-SIZ = 4
               IF  CNT = 10
                   GO TO CHK-020
               END-IF
           END-IF.
           IF  ZERO = HHT-ZSU(CNT) AND HHT-NSU(CNT)
                                  AND HHT-USU(CNT) AND HHT-YSU(CNT)
               GO TO CHK-020
           END-IF.
           MOVE 1 TO W-DZC.
       CHK-EX.
           EXIT.
       SET-RTN.
           MOVE ZERO TO CNT.
       SET-020.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO SET-EX
           END-IF.
           IF  HHT-ZSU(CNT) NOT = ZERO
               MOVE 1 TO W-ZZC(HHT-SIZ) WA-ZZC(HHT-SIZ)
               ADD HHT-ZSU(CNT) TO
                                 W-ZSU(HHT-SIZ,CNT) WA-ZSU(HHT-SIZ,CNT)
                                 W-SUT(1) WA-SUT(1)
           END-IF.
           IF  HHT-NSU(CNT) NOT = ZERO
               MOVE 1 TO W-NZC(HHT-SIZ) WA-NZC(HHT-SIZ)
               ADD HHT-NSU(CNT) TO
                                 W-NSU(HHT-SIZ,CNT) WA-NSU(HHT-SIZ,CNT)
                                 W-SUT(2) WA-SUT(2)
           END-IF.
           IF  HHT-USU(CNT) NOT = ZERO
               MOVE 1 TO W-SZC(HHT-SIZ) WA-SZC(HHT-SIZ)
               ADD HHT-USU(CNT) TO
                                 W-SSU(HHT-SIZ,CNT) WA-SSU(HHT-SIZ,CNT)
                                 W-SUT(3) WA-SUT(3)
           END-IF.
           IF  HHT-YSU(CNT) NOT = ZERO
               MOVE 1 TO W-YZC(HHT-SIZ) WA-YZC(HHT-SIZ)
               ADD HHT-YSU(CNT) TO
                                 W-YSU(HHT-SIZ,CNT) WA-YSU(HHT-SIZ,CNT)
                                 W-SUT(4) WA-SUT(4)
           END-IF.
           GO TO SET-020.
       SET-EX.
           EXIT.
       PR1-RTN.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO HI-FT
               MOVE "　＊＊　ＨＩＭ　なし　＊＊　" TO HI-NAME
           END-IF.
           MOVE SPACE TO W-P1.
           MOVE W-HCD TO P-HCD.
           MOVE HI-NAME TO P-HNA.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM MID-RTN THRU MID-EX
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE 1 TO W-HC.
       PR1-EX.
           EXIT.
       PR2-RTN.
           MOVE ZERO TO W-SC.
       PR2-020.
           ADD 1 TO W-SC.
           IF  W-SC > 4
               GO TO PR2-EX
           END-IF.
           MOVE ZERO TO W-CC W-MC.
       PR2-040.
           ADD 1 TO W-CC.
           IF  W-CC > 4
               GO TO PR2-020
           END-IF.
           MOVE ZERO TO CNT.
       PR2-060.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO PR2-040
           END-IF.
           IF  W-SC = 1
               MOVE W-ZSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 2
               MOVE W-NSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 3
               MOVE W-SSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 4
               MOVE W-YSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SU = ZERO
               GO TO PR2-060.
           MOVE SPACE TO W-P2.
           MOVE W-CC TO P-SIZ.
           MOVE ZERO TO CNT.
       PR2-080.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO PR2-100
           END-IF.
           IF (W-CC = 3) AND (CNT = 10)
               GO TO PR2-080
           END-IF.
           IF (W-CC = 4) AND (CNT > 8)
               GO TO PR2-080
           END-IF.
           IF  W-SC = 1
               MOVE W-ZSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 2
               MOVE W-NSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 3
               MOVE W-SSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 4
               MOVE W-YSU(W-CC,CNT) TO W-SU
           END-IF.
           MOVE W-SU TO P-SU(CNT).
           GO TO PR2-080.
       PR2-100.
           IF  W-HC = 0
               PERFORM PR1-RTN THRU PR1-EX
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER NOT > 60
               GO TO PR2-120
           END-IF.
           MOVE 0 TO W-MC.
           PERFORM MID-RTN THRU MID-EX.
           PERFORM PR1-RTN THRU PR1-EX.
       PR2-120.
           IF  W-SC = 1
               MOVE W-ZZCD TO W-ZCD
               IF  W-MC = 0
                   MOVE 1 TO W-MC
                   MOVE "前残　" TO P-UHM
                   IF  W-HC = 1
                       MOVE 9 TO W-HC
                   END-IF
               END-IF
           END-IF.
           IF  W-SC = 2
               MOVE W-NZCD TO W-ZCD
               IF  W-MC = 0
                   MOVE 1 TO W-MC
                   MOVE "入庫　" TO P-UHM
                   IF  W-HC = 9
                       MOVE SPACE TO SP-R
                       CALL "PR_Write" USING SP-R RETURNING RESP
                   ELSE
                       MOVE 9 TO W-HC
                   END-IF
               END-IF
           END-IF.
           IF  W-SC = 3
               MOVE W-SZCD TO W-ZCD
               IF  W-MC = 0
                   MOVE 1 TO W-MC
                   MOVE "出庫　" TO P-UHM
                   IF  W-HC = 9
                       MOVE SPACE TO SP-R
                       CALL "PR_Write" USING SP-R RETURNING RESP
                   ELSE
                       MOVE 9 TO W-HC
                   END-IF
               END-IF
           END-IF.
           IF  W-SC = 4
               MOVE W-YZCD TO W-ZCD
               IF  W-MC = 0
                   MOVE 1 TO W-MC
                   MOVE "在庫　" TO P-UHM
                   IF  W-HC = 9
                       MOVE SPACE TO SP-R
                       CALL "PR_Write" USING SP-R RETURNING RESP
                   END-IF
               END-IF
           END-IF.
           IF  W-CC = 1
               IF  ZERO NOT = W-ZC(2) OR W-ZC(3) OR W-ZC(4)
                   GO TO PR2-140
               END-IF
           END-IF.
           IF  W-CC = 2
               IF  ZERO NOT = W-ZC(3) OR W-ZC(4)
                   GO TO PR2-140
               END-IF
           END-IF.
           IF  W-CC = 3
               IF  ZERO NOT = W-ZC(4)
                   GO TO PR2-140
               END-IF
           END-IF.
           MOVE W-SUT(W-SC) TO P-SUT.
           MOVE HI-FT TO P-FT.
           COMPUTE W-KIN(W-SC) = W-SUT(W-SC) * HI-FT.
           MOVE W-KIN(W-SC) TO P-KIN.
           ADD W-KIN(W-SC) TO WA-KIN(W-SC).
       PR2-140.
           IF  W-SC = 1
               IF  ZERO = W-NZCD AND W-SZCD
                   MOVE 1 TO W-HC
                   GO TO PR2-040
               END-IF
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO PR2-040.
       PR2-EX.
           EXIT.
       KEI-RTN.
           MOVE ZERO TO W-SC W-HC.
       KEI-020.
           ADD 1 TO W-SC.
           IF  W-SC > 4
               GO TO KEI-160
           END-IF.
           MOVE ZERO TO W-CC W-MC.
       KEI-040.
           ADD 1 TO W-CC.
           IF  W-CC > 4
               GO TO KEI-020
           END-IF.
           MOVE ZERO TO CNT.
       KEI-060.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO KEI-040
           END-IF.
           IF  W-SC = 1
               MOVE WA-ZSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 2
               MOVE WA-NSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 3
               MOVE WA-SSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 4
               MOVE WA-YSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SU = ZERO
               GO TO KEI-060.
           MOVE SPACE TO W-P2.
           MOVE W-CC TO P-SIZ.
           MOVE ZERO TO CNT.
       KEI-080.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO KEI-100
           END-IF.
           IF (W-CC = 3) AND (CNT = 10)
               GO TO KEI-080
           END-IF.
           IF (W-CC = 4) AND (CNT > 8)
               GO TO KEI-080
           END-IF.
           IF  W-SC = 1
               MOVE WA-ZSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 2
               MOVE WA-NSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 3
               MOVE WA-SSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 4
               MOVE WA-YSU(W-CC,CNT) TO W-SU
           END-IF.
           MOVE W-SU TO P-SU(CNT).
           GO TO KEI-080.
       KEI-100.
           IF  W-HC = 0
               MOVE 1 TO W-HC
               MOVE "【　合　計　】" TO P-TM
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER NOT > 60
               GO TO KEI-120
           END-IF.
           MOVE 0 TO W-MC.
           PERFORM MID-RTN THRU MID-EX.
           MOVE 1 TO W-HC.
           MOVE "【　合　計　】" TO P-TM.
       KEI-120.
           IF  W-SC = 1
               MOVE WA-ZZCD TO W-ZCD
               IF  W-MC = 0
                   MOVE 1 TO W-MC
                   MOVE "前残　" TO P-UHM
                   IF  W-HC = 1
                       MOVE 9 TO W-HC
                   END-IF
               END-IF
           END-IF.
           IF  W-SC = 2
               MOVE WA-NZCD TO W-ZCD
               IF  W-MC = 0
                   MOVE 1 TO W-MC
                   MOVE "入庫　" TO P-UHM
                   IF  W-HC = 9
                       MOVE SPACE TO SP-R
                       CALL "PR_Write" USING SP-R RETURNING RESP
                   ELSE
                       MOVE 9 TO W-HC
                   END-IF
               END-IF
           END-IF.
           IF  W-SC = 3
               MOVE WA-SZCD TO W-ZCD
               IF  W-MC = 0
                   MOVE 1 TO W-MC
                   MOVE "出庫　" TO P-UHM
                   IF  W-HC = 9
                       MOVE SPACE TO SP-R
                       CALL "PR_Write" USING SP-R RETURNING RESP
                   ELSE
                       MOVE 9 TO W-HC
                   END-IF
               END-IF
           END-IF.
           IF  W-SC = 4
               MOVE WA-YZCD TO W-ZCD
               IF  W-MC = 0
                   MOVE 1 TO W-MC
                   MOVE "在庫　" TO P-UHM
                   IF  W-HC = 9
                       MOVE SPACE TO SP-R
                       CALL "PR_Write" USING SP-R RETURNING RESP
                   END-IF
               END-IF
           END-IF.
           IF  W-CC = 1
               IF  ZERO NOT = W-ZC(2) OR W-ZC(3) OR W-ZC(4)
                   GO TO KEI-140
               END-IF
           END-IF.
           IF  W-CC = 2
               IF  ZERO NOT = W-ZC(3) OR W-ZC(4)
                   GO TO KEI-140
               END-IF
           END-IF.
           IF  W-CC = 3
               IF  ZERO NOT = W-ZC(4)
                   GO TO KEI-140
               END-IF
           END-IF.
           MOVE WA-SUT(W-SC) TO P-SUT.
           MOVE WA-KIN(W-SC) TO P-KIN.
       KEI-140.
           IF  W-SC = 1
               IF  ZERO = WA-NZCD AND WA-SZCD
                   MOVE 0 TO W-HC
                   GO TO KEI-040
               END-IF
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO KEI-040.
       KEI-160.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       KEI-EX.
           EXIT.
      *------------------- ＥＸＣＥＬ変換 --------------------------------------
       PRE-RTN.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO HI-FT
               MOVE "　＊＊　ＨＩＭ　なし　＊＊　" TO HI-NAME
           END-IF.
           MOVE ZERO TO W-SC.
       PRE-020.
           ADD 1 TO W-SC.
           IF  W-SC > 4
               GO TO PRE-EX
           END-IF.
           MOVE ZERO TO W-CC W-MC.
       PRE-040.
           ADD 1 TO W-CC.
           IF  W-CC > 4
               GO TO PRE-020
           END-IF.
           MOVE ZERO TO CNT.
       PRE-060.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO PRE-040
           END-IF.
           IF  W-SC = 1
               MOVE W-ZSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 2
               MOVE W-NSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 3
               MOVE W-SSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 4
               MOVE W-YSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SU = ZERO
               GO TO PRE-060
           END-IF.
           MOVE SPACE TO EXL-R.
           MOVE SPACE TO EXL-HNA EXL-KBN.
           MOVE ZERO TO EXL-ASU EXL-FT EXL-KIN.
           MOVE W-CC TO EXL-SIZ.
           MOVE ZERO TO CNT.
       PRE-080.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO PRE-100
           END-IF.
           IF (W-CC = 3) AND (CNT = 10)
               GO TO PRE-080
           END-IF.
           IF (W-CC = 4) AND (CNT > 8)
               GO TO PRE-080
           END-IF.
           IF  W-SC = 1
               MOVE W-ZSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 2
               MOVE W-NSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 3
               MOVE W-SSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 4
               MOVE W-YSU(W-CC,CNT) TO W-SU
           END-IF.
           MOVE W-SU TO EXL-SU(CNT).
           GO TO PRE-080.
       PRE-100.
           IF  W-HC NOT = 0
               GO TO PRE-120
           END-IF.
           MOVE W-HCD TO EXL-HCD.
           MOVE HI-NAME TO EXL-HNA.
           MOVE 1 TO W-HC.
       PRE-120.
           IF  W-SC = 1
               MOVE W-ZZCD TO W-ZCD
               IF  W-MC = 0
                   MOVE 1 TO W-MC
                   MOVE "前残" TO EXL-KBN
                   IF  W-HC = 1
                       MOVE 9 TO W-HC
                   END-IF
               END-IF
           END-IF.
           IF  W-SC = 2
               MOVE W-NZCD TO W-ZCD
               IF  W-MC = 0
                   MOVE 1 TO W-MC
                   MOVE "入庫" TO EXL-KBN
                   IF  W-HC = 1
                       MOVE 9 TO W-HC
                   END-IF
               END-IF
           END-IF.
           IF  W-SC = 3
               MOVE W-SZCD TO W-ZCD
               IF  W-MC = 0
                   MOVE 1 TO W-MC
                   MOVE "出庫" TO EXL-KBN
                   IF  W-HC = 1
                       MOVE 9 TO W-HC
                   END-IF
               END-IF
           END-IF.
           IF  W-SC = 4
               MOVE W-YZCD TO W-ZCD
               IF  W-MC = 0
                   MOVE 1 TO W-MC
                   MOVE "在庫" TO EXL-KBN
               END-IF
           END-IF.
           IF  W-CC = 1
               IF  ZERO NOT = W-ZC(2) OR W-ZC(3) OR W-ZC(4)
                   GO TO PRE-140
               END-IF
           END-IF.
           IF  W-CC = 2
               IF  ZERO NOT = W-ZC(3) OR W-ZC(4)
                   GO TO PRE-140
               END-IF
           END-IF.
           IF  W-CC = 3
               IF  ZERO NOT = W-ZC(4)
                   GO TO PRE-140
               END-IF
           END-IF.
           MOVE W-SUT(W-SC) TO EXL-SUT.
           MOVE HI-FT TO EXL-FT.
           COMPUTE W-KIN(W-SC) = W-SUT(W-SC) * HI-FT.
           MOVE W-KIN(W-SC) TO EXL-KIN.
           ADD W-KIN(W-SC) TO WA-KIN(W-SC).
       PRE-140.
           IF  W-SC = 1
               IF  ZERO = W-NZCD AND W-SZCD
                   MOVE 0 TO W-HC
                   GO TO PRE-040
               END-IF
           END-IF.
      *           WRITE EXL-R.
      *//////////////////////
           CALL "DB_Insert" USING
            EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET.
           GO TO PRE-040.
       PRE-EX.
           EXIT.
       KEE-RTN.
           MOVE ZERO TO W-SC W-HC.
       KEE-020.
           ADD 1 TO W-SC.
           IF  W-SC > 4
               GO TO KEE-EX
           END-IF.
           MOVE ZERO TO W-CC W-MC.
       KEE-040.
           ADD 1 TO W-CC.
           IF  W-CC > 4
               GO TO KEE-020
           END-IF.
           MOVE ZERO TO CNT.
       KEE-060.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO KEE-040
           END-IF.
           IF  W-SC = 1
               MOVE WA-ZSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 2
               MOVE WA-NSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 3
               MOVE WA-SSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 4
               MOVE WA-YSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SU = ZERO
               GO TO KEE-060
           END-IF.
      *
           MOVE SPACE TO EXL-R.
           MOVE SPACE TO EXL-HNA EXL-KBN.
           MOVE ZERO TO EXL-ASU EXL-FT EXL-KIN.
           MOVE W-CC TO EXL-SIZ.
           MOVE ZERO TO CNT.
       KEE-080.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO KEE-100
           END-IF.
           IF (W-CC = 3) AND (CNT = 10)
               GO TO KEE-080
           END-IF.
           IF (W-CC = 4) AND (CNT > 8)
               GO TO KEE-080
           END-IF.
           IF  W-SC = 1
               MOVE WA-ZSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 2
               MOVE WA-NSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 3
               MOVE WA-SSU(W-CC,CNT) TO W-SU
           END-IF.
           IF  W-SC = 4
               MOVE WA-YSU(W-CC,CNT) TO W-SU
           END-IF.
           MOVE W-SU TO EXL-SU(CNT).
           GO TO KEE-080.
       KEE-100.
           IF  W-HC = 0
               MOVE 1 TO W-HC
               MOVE "　　　　　　　　　　【　合　計　】" TO EXL-HNA
           END-IF.
           IF  W-SC = 1
               MOVE WA-ZZCD TO W-ZCD
               IF  W-MC = 0
                   MOVE 1 TO W-MC
                   MOVE "前残" TO EXL-KBN
                   IF  W-HC = 1
                       MOVE 9 TO W-HC
                   END-IF
               END-IF
           END-IF.
           IF  W-SC = 2
               MOVE WA-NZCD TO W-ZCD
               IF  W-MC = 0
                   MOVE 1 TO W-MC
                   MOVE "入庫" TO EXL-KBN
                   IF  W-HC = 1
                       MOVE 9 TO W-HC
                   END-IF
               END-IF
           END-IF.
           IF  W-SC = 3
               MOVE WA-SZCD TO W-ZCD
               IF  W-MC = 0
                   MOVE 1 TO W-MC
                   MOVE "出庫" TO EXL-KBN
                   IF  W-HC = 1
                       MOVE 9 TO W-HC
                   END-IF
               END-IF
           END-IF.
           IF  W-SC = 4
               MOVE WA-YZCD TO W-ZCD
               IF  W-MC = 0
                   MOVE 1 TO W-MC
                   MOVE "在庫" TO EXL-KBN
               END-IF
           END-IF.
           IF  W-CC = 1
               IF  ZERO NOT = W-ZC(2) OR W-ZC(3) OR W-ZC(4)
                   GO TO KEE-140
               END-IF
           END-IF.
           IF  W-CC = 2
               IF  ZERO NOT = W-ZC(3) OR W-ZC(4)
                   GO TO KEE-140
               END-IF
           END-IF.
           IF  W-CC = 3
               IF  ZERO NOT = W-ZC(4)
                   GO TO KEE-140
               END-IF
           END-IF.
           MOVE WA-SUT(W-SC) TO EXL-SUT.
           MOVE WA-KIN(W-SC) TO EXL-KIN.
       KEE-140.
           IF  W-SC = 1
               IF  ZERO = WA-NZCD AND WA-SZCD
                   MOVE 0 TO W-HC
                   GO TO KEE-040
               END-IF
           END-IF.
      *           WRITE EXL-R.
      *//////////////////////
           CALL "DB_Insert" USING
            EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET.
           GO TO KEE-040.
       KEE-EX.
           EXIT.

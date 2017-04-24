       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG610.
      *********************************************************
      *    PROGRAM         :  履物品種別受払表                *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    W-JS1           :  月  =0  ,  年  =1               *
      *    W-JS2           :  4ｹﾀ =0  ,  6ｹﾀ =1               *
      *    W-JS3           :  ｺｰﾄﾞ=0  ,  分類=1               *
      *    W-JS4           :  全体=0  ,  ハイパー=1  ,  厨房=2*
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-JS1              PIC  9(001).
       77  W-JS2              PIC  9(001).
       77  W-JS3              PIC  9(001).
       77  W-JS4              PIC  9(001).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  H-SAN          PIC  N(001) VALUE SPACE.
           02  F              PIC  X(002) VALUE SPACE.
           02  H-HIV          PIC  N(008) VALUE SPACE.
           02  H-HED          PIC  X(093).
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  W-HED.
           02  W-HED1.
             03  F            PIC  X(023) VALUE SPACE.
             03  F            PIC  N(007) VALUE
                  "＊＊＊　　履物".
             03  H-MID1       PIC  N(002).
             03  F            PIC  N(010) VALUE
                  "別　受払表　　＊＊＊".
             03  F            PIC  X(032) VALUE SPACE.
           02  W-HED2.
             03  F            PIC  X(010) VALUE SPACE.
             03  F            PIC  N(005) VALUE "＊＊＊　　".
             03  P-HN         PIC  9(002).
             03  F            PIC  N(001) VALUE "年".
             03  P-HG         PIC Z9.
             03  F            PIC  N(004) VALUE "月　～　".
             03  P-ON         PIC  9(002).
             03  F            PIC  N(001) VALUE "年".
             03  P-OG         PIC Z9.
             03  F            PIC  N(004) VALUE "月　履物".
             03  H-MID2       PIC  N(002).
             03  F            PIC  N(012) VALUE
                  "別　年間受払表　　＊＊＊".
             03  F            PIC  X(017) VALUE SPACE.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(019) VALUE SPACE.
           02  F              PIC  X(008) VALUE "I-----  ".
           02  F              PIC  N(004) VALUE "前月繰越".
           02  F              PIC  X(017) VALUE "  ----I I------  ".
           02  F              PIC  N(004) VALUE "当月受入".
           02  F              PIC  X(018) VALUE "  -----I I------  ".
           02  F              PIC  N(004) VALUE "当月売上".
           02  F              PIC  X(017) VALUE "  -----I I-----  ".
           02  F              PIC  N(004) VALUE "翌月繰越".
           02  F              PIC  X(007) VALUE "  ----I".
           02  F              PIC  X(004) VALUE " (  ".
           02  F              PIC  N(002) VALUE "原価".
           02  F              PIC  X(001) VALUE ")".
           02  F              PIC  X(018) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(006) VALUE "品　　　　名".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "売価".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　販売粗利益".
           02  F              PIC  N(004) VALUE "　利益率".
           02  F              PIC  X(001) VALUE "%".
       01  W-P1.
           02  P-HCD          PIC  9(006).
           02  P-HCDD  REDEFINES P-HCD.
             03  F            PIC  X(002).
             03  P-HCD1       PIC  9(004).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(066).
           02  P-F            PIC  X(001).
           02  P-FT           PIC ---,---.
           02  P-R            PIC  X(001).
           02  F              PIC  X(018).
       01  W-P2.
           02  P-TM           PIC  N(012).
           02  P-ZS           PIC --,---,--9.
           02  P-ZK           PIC ----,---,--9.
           02  P-NS           PIC --,---,--9.
           02  P-NK           PIC --,---,---,--9.
           02  P-US           PIC --,---,--9.
           02  P-UK           PIC --,---,---,--9.
           02  P-YS           PIC --,---,--9.
           02  P-YK           PIC ----,---,--9.
           02  P-UT           PIC ---,--9.
           02  P-AR           PIC ----,---,--9.
           02  P-RR           PIC ----9.9.
       01  W-DATA.
           02  W-BC3          PIC  9(002).
           02  W-BMC          PIC  9(002).
           02  W-BC1          PIC  9(002).
           02  W-BC21         PIC  9(001).
           02  W-HCD.
             03  W-HCD1       PIC  9(004).
             03  F            PIC  9(002).
           02  W-PAGE         PIC  9(002).
           02  CNT            PIC  9(002).
           02  CHK            PIC  9(001).
           02  CHKD           PIC  9(001).
           02  W-ALL          PIC S9(006)V9(05).
           02  W-DMM          PIC  9(001).
           02  W-TC           PIC  9(002).
           02  W-MC           PIC  9(001).
           02  W-EC           PIC  9(002).
           02  W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SND   REDEFINES W-SNEN.
               04  W-SN1      PIC  9(002).
               04  W-SN2      PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-ENG.
             03  W-ENEN       PIC  9(004).
             03  W-END   REDEFINES W-ENEN.
               04  W-EN1      PIC  9(002).
               04  W-EN2      PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-ENGL  REDEFINES W-ENG.
             03  F            PIC  9(002).
             03  W-ENGS       PIC  9(004).
           02  W-SHCD         PIC  9(004).
           02  W-EHCD         PIC  9(004) VALUE 9999.
           02  W-SBC3         PIC  9(002).
           02  W-EBC3         PIC  9(002) VALUE 99.
           02  W-SBMNO        PIC  9(001).
           02  W-EBMNO        PIC  9(001) VALUE 9.
           02  W-SBCD1        PIC  9(003).
           02  W-EBCD1        PIC  9(003) VALUE 999.
      *
           02  W-NAME         PIC  N(024).
           02  W-ANM   REDEFINES W-NAME.
             03  W-NM    OCCURS  24  PIC  N(001).
           02  W-NAMED        PIC  N(024).
           02  W-ANMD  REDEFINES W-NAMED.
             03  W-NMD   OCCURS  24  PIC  N(001).
      *
           02  W-TM           PIC  N(012).
           02  W-TMDD  REDEFINES W-TM.
             03  W-TMD   OCCURS  12  PIC  N(001).
           02  W-MN           PIC  N(008).
           02  W-MNDD  REDEFINES W-MN.
             03  W-MND   OCCURS   8  PIC  N(001).
       01  W-D.
           02  W-DD    OCCURS  6.
             03  W-ZS         PIC S9(007).
             03  W-ZK         PIC S9(010).
             03  W-NS         PIC S9(007).
             03  W-NK         PIC S9(010).
             03  W-US         PIC S9(007).
             03  W-UK         PIC S9(010).
             03  W-YS         PIC S9(007).
             03  W-YK         PIC S9(010).
             03  W-AR         PIC S9(009).
       01  W-WD.
           02  W-UT           PIC S9(005).
           02  W-FT           PIC S9(005).
           02  W-RR           PIC S9(003)V9(01).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LIHKBM.
           COPY LSPF.
      *FD  HUH-F
       01  HUH-F_HMG610.
           02  HUH-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HUH-F_LNAME    PIC  X(012) VALUE "HUH-F_HMG610".
           02  F              PIC  X(001).
           02  HUH-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HUH-F_SORT     PIC  X(100) VALUE SPACE.
           02  HUH-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HUH-F_RES      USAGE  POINTER.
       01  HUH-R.
           02  UH-HCD.
             03  UH-HCD1      PIC  9(004).
             03  UH-HCD2      PIC  9(002).
           02  UH-NG          PIC  9(006).
           02  UH-ZSU         PIC S9(006).                                前月数
           02  UH-ZKIN        PIC S9(009).                                前月額
           02  UH-NSU         PIC S9(007).                                入庫数
           02  UH-NKIN        PIC S9(010).                                入庫額
           02  UH-USU         PIC S9(008).                                売上数
           02  UH-UKIN        PIC S9(010).                                売上額
           02  UH-YSU         PIC S9(006).                                翌月数
           02  UH-YKIN        PIC S9(009).                                翌月額
           02  UH-URG         PIC S9(010).                                売上原
           02  UH-BCD1.
             03  UH-BC1       PIC  9(002).
             03  UH-BC21      PIC  9(001).
           02  UH-BC22        PIC  9(001).
           02  UH-BC3         PIC  9(002).                                 分類
           02  UH-BMC         PIC  9(002).
           02  UH-BMNO        PIC  9(001).
           02  FILLER         PIC  X(032).
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　履物　品種別　受払表　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SHCD  PIC  9(004).
             03  A-EHCD  PIC  9(004).
             03  A-SBC3  PIC  9(002).
             03  A-EBC3  PIC  9(002).
           02  FILLER.
             03  A-SBMNO PIC  9(001).
             03  A-EBMNO PIC  9(001).
           02  FILLER.
             03  A-SBC1  PIC  9(003).
             03  A-EBC1  PIC  9(003).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MID   PIC  N(005) VALUE
                "ハイパーＶ".
           02  D-HCDM.
             03  FILLER  PIC  X(024) VALUE
                  "品名ｺｰﾄﾞ  0000 ～ 9999".
           02  D-BCM.
             03  FILLER  PIC  X(049) VALUE
                  "分類③  00 ～ 99    一　般=10,ワーク=20,教　育=30".
             03  FILLER.
               04  FILLER  PIC  X(029) VALUE
                    "部門№   0 ～ 9     国  内=1,".
               04  FILLER  PIC  N(003) VALUE "上　海".
               04  FILLER  PIC  X(011) VALUE "=2,仕  入=3".
             03  FILLER.
               04  FILLER  PIC  N(003) VALUE "ワーク".
               04  FILLER  PIC  X(011) VALUE
                    "=4,教　育=5".
             03  FILLER  PIC  X(017) VALUE
                  "分類① 000 ～ 999".
           02  D-NG.
             03  FILLER  PIC  N(022) VALUE
                  "＊＊＊　　履物　品種別　年間受払表　　＊＊＊".
             03  FILLER.
               04  FILLER  PIC  X(001) VALUE "'".
               04  04D-NG  PIC  9(002).
               04  FILLER  PIC  N(001) VALUE "年".
               04  06D-NG  PIC  9(002).
               04  FILLER  PIC  X(009) VALUE "月 より '".
               04  08D-NG  PIC  9(002).
               04  FILLER  PIC  N(001) VALUE "年".
               04  10D-NG  PIC  9(002).
               04  FILLER  PIC  N(001) VALUE "月".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"274a05".
             03  E-ME99  PIC  X(005) VALUE X"274205".
             03  E-STAT  PIC  X(002).
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
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "330" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "12" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "12" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "12" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "12" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "12" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "12" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "12" "44" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "22" "24" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "21" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "15" "0" "12" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-SHCD" "9" "15" "29" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SHCD" BY REFERENCE W-SHCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EHCD" "9" "15" "37" "4" "A-SHCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EHCD" BY REFERENCE W-EHCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-SBC3" "9" "15" "27" "2" "A-EHCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-SBC3" BY REFERENCE W-SBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EBC3" "9" "15" "33" "2" "A-SBC3" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EBC3" BY REFERENCE W-EBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-ACP" " " "17" "0" "2" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SBMNO" "9" "17" "28" "1" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SBMNO" BY REFERENCE W-SBMNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EBMNO" "9" "17" "33" "1" "A-SBMNO" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EBMNO" BY REFERENCE W-EBMNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03C-ACP" " " "19" "0" "6" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SBC1" "9" "19" "26" "3" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SBC1" BY REFERENCE W-SBCD1 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EBC1" "9" "19" "33" "3" "A-SBC1" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EBC1" BY REFERENCE W-EBCD1 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "22" "41" "1" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "231" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-MID" "N" "7" "28" "10" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "D-HCDM" " " "0" "0" "24" "D-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-HCDM" "X" "15" "19" "24" " " "D-HCDM" RETURNING RESU.
       CALL "SD_Init" USING
           "D-BCM" " " "0" "0" "129" "D-HCDM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-BCM" "X" "15" "19" "49" " " "D-BCM" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-BCM" " " "17" "0" "46" "01D-BCM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03D-BCM" "X" "17" "19" "29" " " "02D-BCM" RETURNING RESU.
       CALL "SD_Init" USING
           "04D-BCM" "N" "17" "48" "6" "03D-BCM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05D-BCM" "X" "17" "54" "11" "04D-BCM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06D-BCM" " " "18" "0" "17" "02D-BCM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07D-BCM" "N" "18" "39" "6" " " "06D-BCM" RETURNING RESU.
       CALL "SD_Init" USING
           "08D-BCM" "X" "18" "45" "11" "07D-BCM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09D-BCM" "X" "19" "19" "17" "06D-BCM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-NG" " " "0" "0" "68" "D-HCDM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-NG" "N" "6" "12" "44" " " "D-NG" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-NG" " " "13" "0" "24" "01D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03D-NG" "X" "13" "23" "1" " " "02D-NG" RETURNING RESU.
       CALL "SD_Init" USING
           "04D-NG" "9" "13" "24" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "04D-NG" BY REFERENCE W-SN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "05D-NG" "N" "13" "26" "2" "04D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06D-NG" "9" "13" "28" "2" "05D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "06D-NG" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "07D-NG" "X" "13" "30" "9" "06D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08D-NG" "9" "13" "39" "2" "07D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "08D-NG" BY REFERENCE W-EN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "09D-NG" "N" "13" "41" "2" "08D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING
           "10D-NG" "9" "13" "43" "2" "09D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "10D-NG" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "11D-NG" "N" "13" "45" "2" "10D-NG" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "29" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "29" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           ACCEPT W-JS1 FROM ARGUMENT-VALUE.
           ACCEPT W-JS2 FROM ARGUMENT-VALUE.
           ACCEPT W-JS3 FROM ARGUMENT-VALUE.
           ACCEPT W-JS4 FROM ARGUMENT-VALUE.
           IF  1 < W-JS1 OR W-JS2 OR W-JS3
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  2 < W-JS4
               CALL "DB_Close"
               STOP RUN
           END-IF
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  W-JS4 = 1
               MOVE "　［ハイパーＶ］" TO H-HIV
               CALL "SD_Output" USING "D-MID" D-MID "p" RETURNING RESU
           END-IF
           IF  W-JS4 = 2
               MOVE "　［　厨　房　］" TO H-HIV
               CALL "SD_Output" USING "D-MID" D-MID "p" RETURNING RESU
           END-IF
           IF  W-JS1 = 1
               PERFORM NGD-RTN THRU NGD-EX
           END-IF
           IF  W-JS3 = 1
               CALL "SD_Output" USING "D-BCM" D-BCM "p" RETURNING RESU
               GO TO M-100
           END-IF
           CALL "SD_Output" USING "D-HCDM" D-HCDM "p" RETURNING RESU.
       M-040.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD "A-SHCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF.
       M-060.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD "A-EHCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF
           IF  W-SHCD > W-EHCD
               GO TO M-060
           END-IF
           GO TO M-220.
       M-100.
           CALL "SD_Accept" USING BY REFERENCE A-SBC3 "A-SBC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-100
           END-IF.
       M-120.
           CALL "SD_Accept" USING BY REFERENCE A-EBC3 "A-EBC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-100
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-120
           END-IF
           IF  W-SBC3 > W-EBC3
               GO TO M-120
           END-IF.
       M-140.
           CALL "SD_Accept" USING BY REFERENCE A-SBMNO "A-SBMNO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-140
           END-IF.
       M-160.
           CALL "SD_Accept" USING BY REFERENCE A-EBMNO "A-EBMNO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-140
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-160
           END-IF
           IF  W-SBMNO > W-EBMNO
               GO TO M-160
           END-IF.
       M-180.
           CALL "SD_Accept" USING BY REFERENCE A-SBC1 "A-SBC1" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-160
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-180
           END-IF.
       M-200.
           CALL "SD_Accept" USING BY REFERENCE A-EBC1 "A-EBC1" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-200
           END-IF
           IF  W-SBCD1 > W-EBCD1
               GO TO M-200
           END-IF.
       M-220.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-JS3 = 0
                   GO TO M-060
               ELSE
                   GO TO M-200
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-220
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-220
           END-IF
           MOVE 0 TO W-DMM.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO HUH-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HUH-F_PNAME1 " " BY REFERENCE HUH-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-300.
      *           READ HUH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HUH-F_PNAME1 BY REFERENCE HUH-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HUH-F_IDLST HUH-F_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-JS3 = 0
               IF  UH-HCD1 < W-SHCD OR > W-EHCD
                   GO TO M-300
               END-IF
           END-IF
           IF  W-JS3 = 1
               IF  UH-BC3 < W-SBC3 OR > W-EBC3
                   GO TO M-300
               END-IF
           END-IF
           IF  W-JS3 = 1
               IF  UH-BMNO < W-SBMNO OR > W-EBMNO
                   GO TO M-300
               END-IF
           END-IF
           IF  W-JS3 = 1
               IF  UH-BCD1 < W-SBCD1 OR > W-EBCD1
                   GO TO M-300
               END-IF
           END-IF
           IF  W-JS1 = 1
               IF  UH-NG < W-SNG OR > W-ENG
                   GO TO M-300
               END-IF
           END-IF
           IF  ZERO = UH-ZSU AND UH-ZKIN AND UH-NSU AND UH-NKIN AND
                     UH-USU AND UH-UKIN AND UH-YSU AND UH-YKIN
               GO TO M-300
           END-IF
           IF  W-JS4 NOT = 1
               GO TO M-310
           END-IF
           MOVE UH-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 0 TO HI-HPV
           END-IF
           IF  HI-HPV NOT = 1
               GO TO M-300
           END-IF.
       M-310.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           IF  COMPLETION_CODE =  010
               MOVE "参" TO H-SAN
           END-IF
           MOVE ZERO TO W-PAGE.
           IF  W-JS2 = 0
               IF  W-JS1 = 0
                   MOVE "品種" TO H-MID1
               ELSE
                   MOVE "品種" TO H-MID2
               END-IF
           END-IF
           IF  W-JS2 = 1
               IF  W-JS1 = 0
                   MOVE "品名" TO H-MID1
               ELSE
                   MOVE "品名" TO H-MID2
               END-IF
           END-IF
           IF  W-JS1 = 0
               MOVE W-HED1 TO H-HED
           ELSE
               MOVE W-SN2 TO P-HN
               MOVE W-SGET TO P-HG
               MOVE W-EN2 TO P-ON
               MOVE W-EGET TO P-OG
               MOVE W-HED2 TO H-HED
           END-IF
           PERFORM MID-020 THRU MID-EX.
           MOVE ZERO TO W-D.
       M-320.
           MOVE ZERO TO W-DD(5).
           MOVE UH-BC3 TO W-BC3.
       M-340.
           MOVE ZERO TO W-DD(4).
           MOVE UH-BMC TO W-BMC.
       M-360.
           MOVE ZERO TO W-DD(3).
           MOVE UH-BC1 TO W-BC1.
       M-380.
           MOVE ZERO TO W-DD(2).
           MOVE UH-BC21 TO W-BC21.
       M-400.
           IF  W-JS2 = 0
               MOVE UH-HCD1 TO W-HCD1
           ELSE
               MOVE UH-HCD TO W-HCD
           END-IF
           MOVE ZERO TO W-DD(1).
           PERFORM HMS-RTN THRU HMS-EX.
       M-420.
           PERFORM SET-RTN THRU SET-EX.
           IF (W-JS1 = 1) OR (W-JS2 = 0)
               GO TO M-440
           END-IF
           MOVE 1 TO CHK.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
       M-440.
      *           READ HUH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HUH-F_PNAME1 BY REFERENCE HUH-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-920
           END-IF
           IF  W-JS3 = 0
               IF  UH-HCD1 < W-SHCD OR > W-EHCD
                   GO TO M-440
               END-IF
           END-IF
           IF  W-JS3 = 1
               IF  UH-BC3 < W-SBC3 OR > W-EBC3
                   GO TO M-440
               END-IF
           END-IF
           IF  W-JS3 = 1
               IF  UH-BMNO < W-SBMNO OR > W-EBMNO
                   GO TO M-440
               END-IF
           END-IF
           IF  W-JS3 = 1
               IF  UH-BCD1 < W-SBCD1 OR > W-EBCD1
                   GO TO M-440
               END-IF
           END-IF
           IF  W-JS1 = 1
               IF  UH-NG < W-SNG OR > W-ENG
                   GO TO M-440
               END-IF
           END-IF
           IF  ZERO = UH-ZSU AND UH-ZKIN AND UH-NSU AND UH-NKIN AND
                     UH-USU AND UH-UKIN AND UH-YSU AND UH-YKIN
               GO TO M-440
           END-IF
           IF  W-JS4 NOT = 1
               GO TO M-450
           END-IF
           MOVE UH-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 0 TO HI-HPV
           END-IF
           IF  HI-HPV NOT = 1
               GO TO M-440
           END-IF.
       M-450.
           IF  UH-BC3 NOT = W-BC3
               GO TO M-580
           END-IF
           IF  UH-BMC NOT = W-BMC
               GO TO M-540
           END-IF
           IF  UH-BC1 NOT = W-BC1
               GO TO M-500
           END-IF
           IF  UH-BC21 NOT = W-BC21
               GO TO M-460
           END-IF
           IF (W-JS1 = 0) AND (W-JS2 = 1)
               GO TO M-400
           END-IF
           IF  W-JS2 = 0
               IF  UH-HCD1 = W-HCD1
                   GO TO M-420
               END-IF
           END-IF
           IF (W-JS1 = 1) AND (W-JS2 = 1)
               IF  UH-HCD = W-HCD
                   GO TO M-420
               END-IF
           END-IF
           MOVE 1 TO CHK.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
           GO TO M-400.
       M-460.
           IF (W-JS1 = 0) AND (W-JS2 = 1)
               GO TO M-480
           END-IF
           MOVE 1 TO CHK.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
       M-480.
           MOVE 2 TO CHK.
           IF  W-BC21 NOT = 0
               PERFORM PRI-RTN THRU PRI-EX
           END-IF
           PERFORM TOT-RTN THRU TOT-EX.
           GO TO M-380.
       M-500.
           IF (W-JS1 = 0) AND (W-JS2 = 1)
               GO TO M-520
           END-IF
           MOVE 1 TO CHK.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
       M-520.
           MOVE 2 TO CHK.
           IF  W-BC21 NOT = 0
               PERFORM PRI-RTN THRU PRI-EX
           END-IF
           PERFORM TOT-RTN THRU TOT-EX.
           MOVE 3 TO CHK.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
           GO TO M-360.
       M-540.
           IF (W-JS1 = 0) AND (W-JS2 = 1)
               GO TO M-560
           END-IF
           MOVE 1 TO CHK.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
       M-560.
           MOVE 2 TO CHK.
           IF  W-BC21 NOT = 0
               PERFORM PRI-RTN THRU PRI-EX
           END-IF
           PERFORM TOT-RTN THRU TOT-EX.
           MOVE 3 TO CHK.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
      *
           MOVE 4 TO CHK.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
           GO TO M-340.
       M-580.
           IF (W-JS1 = 0) AND (W-JS2 = 1)
               GO TO M-600
           END-IF
           MOVE 1 TO CHK.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
       M-600.
           MOVE 2 TO CHK.
           IF  W-BC21 NOT = 0
               PERFORM PRI-RTN THRU PRI-EX
           END-IF
           PERFORM TOT-RTN THRU TOT-EX.
           MOVE 3 TO CHK.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
      *
           MOVE 4 TO CHK.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
      *
           MOVE 5 TO CHK.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
           GO TO M-320.
       M-920.
           IF (W-JS1 = 0) AND (W-JS2 = 1)
               GO TO M-940
           END-IF
           MOVE 1 TO CHK.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
       M-940.
           MOVE 2 TO CHK.
           IF  W-BC21 NOT = 0
               PERFORM PRI-RTN THRU PRI-EX
           END-IF
           PERFORM TOT-RTN THRU TOT-EX.
           MOVE 3 TO CHK.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
      *
           MOVE 4 TO CHK.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
      *
           MOVE 5 TO CHK.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
      *
           MOVE 6 TO CHK.
           PERFORM PRI-RTN THRU PRI-EX.
       M-980.
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-F_IDLST HUH-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close"
           STOP RUN.
       NGD-RTN.
           MOVE ZERO TO W-SNG W-ENG.
           MOVE D-SPNG TO W-SNGS.
           MOVE D-EPNG TO W-ENGS.
           IF  W-SN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-SN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF
           IF  W-EN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF
           IF  W-EN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
       NGD-EX.
           EXIT.
       HMS-RTN.
           IF  W-JS2 = 0
               GO TO HMS-010
           END-IF
           MOVE UH-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　　　　" TO HI-NAME
               MOVE ZERO TO HI-FT
           END-IF
           GO TO HMS-EX.
       HMS-010.
           MOVE SPACE TO W-NAME W-NAMED.
           MOVE ZERO TO HI-KEY.
           MOVE W-HCD1 TO HI-HCD1.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　　　　" TO W-NAME
               GO TO HMS-EX
           END-IF
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　　　　" TO W-NAME
               GO TO HMS-EX
           END-IF
           IF  W-HCD1 NOT = HI-HCD1
               MOVE "　＊＊　マスター　なし　＊＊　　　　" TO W-NAME
               GO TO HMS-EX
           END-IF
      *
           MOVE HI-NAME TO W-NAMED.
           MOVE SPACE TO W-NAME.
           MOVE ZERO TO CNT.
       HMS-020.
           ADD 1 TO CNT.
           IF  CNT = 24
               GO TO HMS-EX
           END-IF
           MOVE W-NMD(CNT) TO W-NM(CNT).
           IF  W-NMD(CNT) NOT = SPACE
               GO TO HMS-020
           END-IF
           ADD 1 TO CNT.
           IF  CNT = 24
               GO TO HMS-EX
           END-IF
           MOVE W-NMD(CNT) TO W-NM(CNT).
           IF  W-NMD(CNT) NOT = SPACE
               GO TO HMS-020
           END-IF.
       HMS-EX.
           EXIT.
       SET-RTN.
           IF  W-JS1 = 1
               IF  W-SNG = UH-NG
                   ADD UH-ZSU TO W-ZS(1)
                   ADD UH-ZKIN TO W-ZK(1)
               END-IF
           END-IF
           IF  W-JS1 = 1
               IF  W-ENG = UH-NG
                   ADD UH-YSU TO W-YS(1)
                   ADD UH-YKIN TO W-YK(1)
               END-IF
           END-IF
           IF  W-JS1 = 0
               ADD UH-ZSU TO W-ZS(1)
               ADD UH-ZKIN TO W-ZK(1)
               ADD UH-YSU TO W-YS(1)
               ADD UH-YKIN TO W-YK(1)
           END-IF
           ADD UH-NSU TO W-NS(1).
           ADD UH-NKIN TO W-NK(1).
           ADD UH-USU TO W-US(1).
           ADD UH-UKIN TO W-UK(1).
           COMPUTE W-AR(1) = W-YK(1) + W-UK(1) - W-NK(1) - W-ZK(1).
       SET-EX.
           EXIT.
       PRI-RTN.
           MOVE ZERO TO W-WD.
           IF  W-JS2 = 0
               IF  W-YS(CHK) NOT = ZERO
                   COMPUTE W-FT ROUNDED = W-YK(CHK) / W-YS(CHK)
               END-IF
           END-IF
           IF  W-UK(CHK) = ZERO
               GO TO PRI-020
           END-IF
           IF  W-US(CHK) NOT = ZERO
               COMPUTE W-UT ROUNDED = W-UK(CHK) / W-US(CHK)
           END-IF
           IF  W-AR(CHK) NOT = ZERO
               IF  W-UK(CHK) > ZERO
                   COMPUTE W-RR ROUNDED = (W-AR(CHK) * 100) / W-UK(CHK)
               ELSE
                   COMPUTE W-RR ROUNDED =
                                         (W-AR(CHK) * -100) / W-UK(CHK)
               END-IF
           END-IF.
       PRI-020.
           IF ZERO = W-ZK(CHK) AND W-NK(CHK) AND W-UK(CHK) AND W-YK(CHK)
                 AND W-ZS(CHK) AND W-NS(CHK) AND W-US(CHK) AND W-YS(CHK)
               GO TO PRI-EX
           END-IF
           MOVE SPACE TO W-P1 W-P2.
           MOVE SPACE TO P-HNA P-TM.
           IF  CHK = 1
               IF  W-JS2 = 1
                   MOVE W-HCD TO P-HCD
               ELSE
                   MOVE W-HCD1 TO P-HCD1
               END-IF
           END-IF
           MOVE W-ZS(CHK) TO P-ZS.
           MOVE W-ZK(CHK) TO P-ZK.
           MOVE W-NS(CHK) TO P-NS.
           MOVE W-NK(CHK) TO P-NK.
           MOVE W-US(CHK) TO P-US.
           MOVE W-UK(CHK) TO P-UK.
           MOVE W-YS(CHK) TO P-YS.
           MOVE W-YK(CHK) TO P-YK.
           MOVE W-AR(CHK) TO P-AR.
           IF  W-UT NOT = ZERO
               MOVE W-UT TO P-UT
           END-IF
           IF  W-RR NOT = ZERO
               MOVE W-RR TO P-RR
           END-IF
           IF  CHK = 1
               MOVE "(" TO P-F
               MOVE ")" TO P-R
               IF  W-JS2 = 1
                   MOVE HI-NAME TO P-HNA
                   MOVE HI-FT TO P-FT
                   GO TO PRI-240
               ELSE
                   MOVE W-NAME TO P-HNA
                   MOVE W-FT TO P-FT
                   GO TO PRI-240
               END-IF
           END-IF
           IF  CHK = 2
               MOVE "　　　　　　｛　小計　｝" TO P-TM
               GO TO PRI-240
           END-IF
           MOVE 1 TO W-DMM.
           IF  CHK = 6
               MOVE "【　総　合　計　】　　　" TO P-TM
               GO TO PRI-240
           END-IF
      *
           IF  CHK NOT = 3
               GO TO PRI-080
           END-IF
           MOVE SPACE TO HKB-KEY.
           MOVE "11" TO HKB-NO.
           MOVE W-BC1 TO HKB-BR1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BRN1
           END-IF
           MOVE SPACE TO W-TM W-MN.
           MOVE HKB-BRN1 TO W-MN.
           MOVE 9 TO W-EC.
       PRI-040.
           SUBTRACT 1 FROM W-EC.
           IF  W-EC NOT = 0
               IF  W-MND(W-EC) = SPACE
                   GO TO PRI-040
               END-IF
           END-IF
      *
           ADD 1 TO W-EC.
           MOVE "（" TO W-TMD(4).
           MOVE 4 TO W-TC.
           MOVE 0 TO W-MC.
       PRI-060.
           ADD 1 TO W-TC W-MC.
           IF  W-MC NOT = W-EC
               MOVE W-MND(W-MC) TO W-TMD(W-TC)
               GO TO PRI-060
           END-IF
           MOVE "）" TO W-TMD(W-TC).
           MOVE W-TM TO P-TM.
           GO TO PRI-240.
      *
       PRI-080.
           IF  CHK NOT = 4
               GO TO PRI-160
           END-IF
           MOVE SPACE TO HKB-KEY.
           MOVE "16" TO HKB-NO.
           MOVE W-BMC TO HKB-BMC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BMN
           END-IF
           MOVE SPACE TO W-TM W-MN.
           MOVE HKB-BMN TO W-TM.
           MOVE ZERO TO W-MC W-EC.
       PRI-100.
           ADD 1 TO W-MC.
           IF  W-MC = 5
               GO TO PRI-120
           END-IF
           IF  W-TMD(W-MC) NOT = SPACE
               ADD 1 TO W-EC
               MOVE W-TMD(W-MC) TO W-MND(W-EC)
           END-IF
           GO TO PRI-100.
       PRI-120.
           ADD 1 TO W-EC.
           MOVE "計" TO W-MND(W-EC).
           ADD 2 TO W-EC.
           MOVE SPACE TO W-TM.
           MOVE "＜" TO W-TMD(3).
           MOVE 4 TO W-TC.
           MOVE 0 TO W-MC.
       PRI-140.
           ADD 1 TO W-TC W-MC.
           IF  W-MC NOT = W-EC
               MOVE W-MND(W-MC) TO W-TMD(W-TC)
               GO TO PRI-140
           END-IF
           MOVE "＞" TO W-TMD(W-TC).
           MOVE W-TM TO P-TM.
           GO TO PRI-240.
      *
       PRI-160.
           IF  CHK NOT = 5
               GO TO PRI-240
           END-IF
           MOVE SPACE TO HKB-KEY.
           MOVE "14" TO HKB-NO.
           MOVE W-BC3 TO HKB-BR3.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BRN3
           END-IF
           MOVE SPACE TO W-TM W-MN.
           MOVE HKB-BRN3 TO W-TM.
           MOVE ZERO TO W-MC W-EC.
       PRI-180.
           ADD 1 TO W-MC.
           IF  W-MC = 5
               GO TO PRI-200
           END-IF
           IF  W-TMD(W-MC) NOT = SPACE
               ADD 1 TO W-EC
               MOVE W-TMD(W-MC) TO W-MND(W-EC)
           END-IF
           GO TO PRI-180.
       PRI-200.
           ADD 1 TO W-EC.
           MOVE "合" TO W-MND(W-EC).
           ADD 1 TO W-EC.
           MOVE "計" TO W-MND(W-EC).
           ADD 2 TO W-EC.
           MOVE SPACE TO W-TM.
           MOVE "［" TO W-TMD(2).
           MOVE 3 TO W-TC.
           MOVE 0 TO W-MC.
       PRI-220.
           ADD 1 TO W-TC W-MC.
           IF  W-MC NOT = W-EC
               MOVE W-MND(W-MC) TO W-TMD(W-TC)
               GO TO PRI-220
           END-IF
           MOVE "］" TO W-TMD(W-TC).
           MOVE W-TM TO P-TM.
      *
       PRI-240.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           IF  W-DMM NOT = 1
               MOVE W-P1 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           IF  W-DMM = 1
               MOVE W-P2 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           ELSE
               MOVE W-P2 TO SP-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
           MOVE SPACE TO SP-R.
           MOVE ZERO TO W-DMM.
       PRI-EX.
           EXIT.
       TOT-RTN.
           COMPUTE CHKD = CHK + 1.
           ADD W-ZS(CHK) TO W-ZS(CHKD).
           ADD W-ZK(CHK) TO W-ZK(CHKD).
           ADD W-NS(CHK) TO W-NS(CHKD).
           ADD W-NK(CHK) TO W-NK(CHKD).
           ADD W-US(CHK) TO W-US(CHKD).
           ADD W-UK(CHK) TO W-UK(CHKD).
           ADD W-YS(CHK) TO W-YS(CHKD).
           ADD W-YK(CHK) TO W-YK(CHKD).
           ADD W-AR(CHK) TO W-AR(CHKD).
       TOT-EX.
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
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.

       IDENTIFICATION   DIVISION.
       PROGRAM-ID. HKY010.
      *********************************************************
      *    PROGRAM         :  得意先台帳ワーク作成            *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-DC               PIC  9(001) VALUE 0.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0064".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0256".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  W-SNG          PIC  9(004).
           02  W-SNGW  REDEFINES  W-SNG.
             03  W-SNEN       PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-ENG          PIC  9(004).
           02  W-ENGW  REDEFINES  W-ENG.
             03  W-ENEN       PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-BM           PIC  9(001).
           02  W-STCD         PIC  9(004).
           02  W-ETCD         PIC  9(004) VALUE 9999.
           02  W-DMM          PIC  9(001).
      *
           02  W-NG.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-SNGD.
             03  W-SND        PIC  9(002).
             03  W-SGD        PIC  9(002).
           02  W-ENGD.
             03  W-END        PIC  9(002).
             03  W-EGD        PIC  9(002).
           02  W-DNO          PIC  9(006).
           02  W-TCD          PIC  9(004).
           02  W-NGP          PIC  9(008).
           02  CHK            PIC  9(001).
           COPY LSTAT.
      *
           COPY LIBFDD.
      *FD  TMS-F
       01  TMS-F_HKY010.
           02  TMS-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TMS-F_LNAME    PIC  X(012) VALUE "TMS-F_HKY010".
           02  F              PIC  X(001).
           02  TMS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  TMS-F_SORT     PIC  X(100) VALUE SPACE.
           02  TMS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  TMS-F_RES      USAGE  POINTER.
       01  TMS-R.
           02  TMS-TCD        PIC  9(004).
           02  F              PIC  9(002).
           02  TMS-DATE       PIC  9(006).
           02  TMS-KIN        PIC S9(009).
           02  TMS-SHZ        PIC S9(007).
           02  TMS-DC         PIC  9(001).
           02  TMS-BC         PIC  9(001).
           02  TMS-TNC        PIC  9(002).
           02  TMS-DCC        PIC  9(001).
           02  TMS-NGP.
             03  TMS-NG       PIC  9(006).
             03  F            PIC  9(002).
           02  TMS-SNG        PIC  9(004).
           02  TMS-ENG        PIC  9(004).
           02  TMS-BM         PIC  9(001).
           02  TMS-STCD       PIC  9(004).
           02  TMS-ETCD       PIC  9(004).
           02  F              PIC  X(006).
       77  F                  PIC  X(001).
      *FD  STRANYR
       01  STRANYR_HKY010.
           02  STRANYR_PNAME1 PIC  X(007) VALUE "STRANYR".
           02  F              PIC  X(001).
           02  STRANYR_LNAME  PIC  X(014) VALUE "STRANYR_HKY010".
           02  F              PIC  X(001).
           02  STRANYR_KEY1   PIC  X(100) VALUE SPACE.
           02  STRANYR_SORT   PIC  X(100) VALUE SPACE.
           02  STRANYR_IDLST  PIC  X(100) VALUE SPACE.
           02  STRANYR_RES    USAGE  POINTER.
       01  STRANY-R.
           02  ST-S1.
             03  S1-DNO       PIC  9(006).
             03  S1-GNO       PIC  9(001).
             03  S1-DATE      PIC  9(008).
             03  S1-NGP   REDEFINES S1-DATE.
               04  F          PIC  9(002).
               04  S1-NG      PIC  9(004).
               04  S1-PEY     PIC  9(002).
             03  S1-TCD       PIC  9(004).
             03  S1-HCD       PIC  9(006).
             03  S1-SIZ       PIC  9(001).
             03  S1-ASU.
               04  S1-SUD   OCCURS 10.
                 05  S1-SU    PIC S9(004)   COMP-3.
             03  S1-SUT       PIC S9(005).
             03  S1-T         PIC S9(005).
             03  S1-KIN       PIC S9(008).
             03  S1-CSC       PIC  9(001).
             03  S1-DC        PIC  9(001).
             03  S1-FT        PIC  9(005).
             03  S1-CCD       PIC  9(003).
             03  S1-BC        PIC  9(006).
             03  S1-SKC       PIC  9(001).
             03  S1-TNC       PIC  9(002).
             03  F            PIC  X(002).
             03  S1-HSC       PIC  9(001).
             03  S1-KOSU      PIC  9(003).
             03  S1-FRC       PIC  9(001).
             03  S1-TCD2      PIC  9(004).
             03  S1-BIK       PIC  X(010).
             03  S1-SKU       PIC  9(008).
             03  S1-BKC       PIC  9(002).
             03  S1-BMNO      PIC  9(001).
             03  S1-USC       PIC  9(001).
             03  S1-DHC       PIC  9(001).
             03  S1-SNC       PIC  9(001).
           02  ST-S2    REDEFINES ST-S1.
             03  S2-DNO       PIC  9(006).
             03  S2-GNO       PIC  9(001).
             03  S2-DATE      PIC  9(008).
             03  S2-TCD       PIC  9(004).
             03  S2-BI        PIC  N(024).
             03  S2-HNO       PIC  9(006).
             03  F            PIC  X(030).
             03  S2-TAX       PIC S9(007).
             03  S2-SHZZ      PIC S9(007).
             03  S2-UZ        PIC S9(009).
             03  S2-DHC       PIC  9(001).
             03  S2-SNC       PIC  9(001).
       77  F                  PIC  X(001).
      *FD  URIRYR
       01  URIRYR_HKY010.
           02  URIRYR_PNAME1  PIC  X(006) VALUE "URIRYR".
           02  F              PIC  X(001).
           02  URIRYR_LNAME   PIC  X(013) VALUE "URIRYR_HKY010".
           02  F              PIC  X(001).
           02  URIRYR_KEY1    PIC  X(100) VALUE SPACE.
           02  URIRYR_SORT    PIC  X(100) VALUE SPACE.
           02  URIRYR_IDLST   PIC  X(100) VALUE SPACE.
           02  URIRYR_RES     USAGE  POINTER.
       01  URIRY-R.
           02  UR-DC          PIC  9(001).
           02  UR-DATE        PIC  9(008).
           02  UR-NGP   REDEFINES UR-DATE.
             03  F            PIC  9(002).
             03  UR-NG        PIC  9(004).
             03  UR-PEY       PIC  9(002).
           02  UR-TCD         PIC  9(004).
           02  UR-HCD         PIC  X(005).
           02  UR-SUT         PIC S9(006)V9(02).
           02  UR-T           PIC S9(006)V9(02).
           02  UR-KIN         PIC S9(008).
           02  UR-YC          PIC  9(002).
           02  UR-SGP         PIC  9(004).
           02  UR-NNO         PIC  9(006).
           02  UR-HYC         PIC  9(001).
           02  UR-CSC         PIC  9(001).
           02  UR-BKC         PIC  9(002).
           02  UR-SKU         PIC  9(008).
           02  UR-DNO         PIC  9(006).
           02  UR-GNO         PIC  9(001).
           02  UR-JCD         PIC  9(006).
           02  UR-GT          PIC S9(006)V9(02).
           02  UR-TEK         PIC  N(018).
           02  UR-BMC         PIC  9(001).
           02  F              PIC  X(003).
           02  UR-PC          PIC  9(001).
       77  F                  PIC  X(001).
      *FD  NYURYR
       01  NYURYR_HKY010.
           02  NYURYR_PNAME1  PIC  X(006) VALUE "NYURYR".
           02  F              PIC  X(001).
           02  NYURYR_LNAME   PIC  X(013) VALUE "NYURYR_HKY010".
           02  F              PIC  X(001).
           02  NYURYR_KEY1    PIC  X(100) VALUE SPACE.
           02  NYURYR_SORT    PIC  X(100) VALUE SPACE.
           02  NYURYR_IDLST   PIC  X(100) VALUE SPACE.
           02  NYURYR_RES     USAGE  POINTER.
       01  NYURY-R.
           02  NY-DATE        PIC  9(008).
           02  NY-NGP   REDEFINES NY-DATE.
             03  F            PIC  9(002).
             03  NY-NG        PIC  9(004).
             03  NY-PEY       PIC  9(002).
           02  NY-TCD         PIC  9(004).
           02  NY-KIN         PIC S9(008).
           02  NY-NKC         PIC  9(002).
           02  NY-SSC         PIC  9(001).
           02  NY-TKG         PIC  9(008).
           02  NY-SKNG        PIC  9(006).
           02  NY-BMC         PIC  9(001).
           02  NY-TNC         PIC  9(002).
           02  NY-SKN         PIC  9(002).
           02  NY-SKK         PIC  9(001).
           02  NY-DNO         PIC  9(006).
           02  NY-GNO         PIC  9(001).
           02  F              PIC  X(008).
           02  NY-SKU         PIC  9(008).
           02  NY-DCC         PIC  9(001).
           02  F              PIC  X(013).
           02  NY-NRC         PIC  9(001).
           02  F              PIC  9(001).
           02  NY-PC          PIC  9(001).
           02  F              PIC  X(019).
       77  F                  PIC  X(001).
      *FD  DTWF
       01  DTWF_HKY010.
           02  DTWF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  DTWF_LNAME     PIC  X(011) VALUE "DTWF_HKY010".
           02  F              PIC  X(001).
           02  DTWF_KEY1      PIC  X(100) VALUE SPACE.
           02  DTWF_SORT      PIC  X(100) VALUE SPACE.
           02  DTWF_IDLST     PIC  X(100) VALUE SPACE.
           02  DTWF_RES       USAGE  POINTER.
       01  DTW-R.
           02  DTW-D1.
             03  DT1-BMC      PIC  9(001).
             03  DT1-UNC      PIC  9(001).
             03  DT1-DNO      PIC  9(006).
             03  DT1-GNO      PIC  9(001).
             03  DT1-DATE     PIC  9(008).
             03  DT1-TCD      PIC  9(004).
             03  DT1-HCD      PIC  9(006).
             03  DT1-HCDD  REDEFINES  DT1-HCD.
               04  DT1-KCD    PIC  X(005).
               04  F          PIC  X(001).
             03  DT1-SUT      PIC S9(006)V9(02).
             03  DT1-T        PIC S9(006)V9(02).
             03  DT1-KIN      PIC S9(009).
             03  DT1-CSC      PIC  9(001).
             03  DT1-DC       PIC  9(001).
             03  DT1-BKC      PIC  9(002).
             03  DT1-BMNO     PIC  9(001).
             03  DT1-FT       PIC  9(006)V9(02).
             03  DT1-SHZ      PIC S9(007).
             03  DT1-SIZ      PIC  9(001).
             03  DT1-ASU.
               04  DT1-SUD   OCCURS 10.
                 05  DT1-SU   PIC S9(004).
             03  DT1-CCD      PIC  9(003).
             03  DT1-BC       PIC  9(006).
             03  DT1-SKC      PIC  9(001).
             03  DT1-TNC      PIC  9(002).
             03  F            PIC  X(002).
             03  DT1-HSC      PIC  9(001).
             03  DT1-KOSU     PIC  9(003).
             03  DT1-FRC      PIC  9(001).
             03  DT1-TCD2     PIC  9(004).
             03  DT1-BIK      PIC  X(010).
             03  DT1-USC      PIC  9(001).
             03  DT1-SNC      PIC  9(001).
             03  DT1-HYC      PIC  9(001).
             03  DT1-YC       PIC  9(002).
             03  DT1-SGP      PIC  9(004).
             03  DT1-NNO      PIC  9(006).
             03  DT1-JCD      PIC  9(006).
             03  DT1-TEKI     PIC  N(018).
             03  DT1-NKC      PIC  9(002).
             03  DT1-SSC      PIC  9(001).
             03  DT1-TKG      PIC  9(008).
             03  DT1-SKNG     PIC  9(006).
             03  DT1-DCC      PIC  9(001).
             03  DT1-SNGP     PIC  9(006).
             03  F            PIC  X(019).
             03  DT1-EC       PIC  9(001).
             03  DT1-SKU      PIC  9(008).
             03  DT1-PC       PIC  9(001).
           02  DTW-D2   REDEFINES  DTW-D1.
             03  DT2-BMC      PIC  9(001).
             03  DT2-UNC      PIC  9(001).
             03  DT2-DNO      PIC  9(006).
             03  DT2-GNO      PIC  9(001).
             03  DT2-DATE     PIC  9(008).
             03  DT2-TCD      PIC  9(004).
             03  DT2-BI       PIC  N(024).
             03  DT2-HNO      PIC  9(006).
             03  F            PIC  X(028).
             03  DT2-TAX      PIC S9(007).
             03  F            PIC  X(029).
             03  DT2-SNC      PIC  9(001).
             03  F            PIC  X(106).
             03  DT2-EC       PIC  9(001).
             03  F            PIC  X(008).
             03  DT2-PC       PIC  9(001).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　得意先台帳ワーク　作成　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(039) VALUE
                  "データ期間   '  年   月　～　'  年   月".
           02  FILLER  PIC  X(039) VALUE
                  " 抽出期間    '  年   月　～　'  年   月".
           02  FILLER  PIC  X(042) VALUE
                  "全体 = 0  ,  履物 = 1  ,  工品他 = 2   [ ]".
           02  FILLER  PIC  X(024) VALUE
                  "得意先ｺｰﾄﾞ  0000 ～ 9999".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-DSP.
           02  D-DNG.
             03  01D-DNG  PIC  9(002).
             03  02D-DNG  PIC  Z(002).
             03  03D-DNG  PIC  9(002).
             03  04D-DNG  PIC  Z(002).
       01  C-ACP.
           02  FILLER.
             03  A-SNEN  PIC  9(002).
             03  A-SGET  PIC  9(002).
             03  A-ENEN  PIC  9(002).
             03  A-EGET  PIC  9(002).
           02  A-BM    PIC  9(001).
           02  FILLER.
             03  A-STCD  PIC  9(004).
             03  A-ETCD  PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  DATA ﾅｼ   ***".
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "460" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "2" "15" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "3" "15" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "4" "15" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "5" "15" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "6" "15" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "7" "15" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "8" "15" "42" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "11" "17" "39" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "13" "17" "39" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "10C-MID" "X" "16" "17" "42" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "11C-MID" "X" "18" "17" "24" "10C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "12C-MID" "X" "22" "30" "22" "11C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-DNG" " " "11" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "01D-DNG" "9" "11" "31" "2" " " "D-DNG" RETURNING RESU.
       CALL "SD_From" USING
           "01D-DNG" BY REFERENCE W-SND "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-DNG" "Z" "11" "36" "2" "01D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING
           "02D-DNG" BY REFERENCE W-SGD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-DNG" "9" "11" "47" "2" "02D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING
           "03D-DNG" BY REFERENCE W-END "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "04D-DNG" "Z" "11" "52" "2" "03D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING
           "04D-DNG" BY REFERENCE W-EGD "2" "0" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "18" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "13" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-SNEN" "9" "13" "31" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SNEN" BY REFERENCE W-SNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-SGET" "9" "13" "36" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-ENEN" "9" "13" "47" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-ENEN" BY REFERENCE W-ENEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EGET" "9" "13" "52" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-BM" "9" "16" "57" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-BM" BY REFERENCE W-BM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-ACP" " " "18" "0" "8" "A-BM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-STCD" "9" "18" "29" "4" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-ETCD" "9" "18" "37" "4" "A-STCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "22" "47" "1" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "18" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "18" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           COPY LIBCPR.
           MOVE D-SNG TO W-SNGD.
           MOVE D-NHNG TO W-ENGD W-SNG W-ENG.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0064ID.
           MOVE W-FID2 TO WK0256ID.
           MOVE WK0064ID TO TMS-F_PNAME1.
           MOVE WK0256ID TO DTWF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TMS-F_PNAME1 " " BY REFERENCE TMS-F_IDLST "0".
      *           READ TMS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TMS-F_PNAME1 BY REFERENCE TMS-R " " RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-DC
               GO TO M-10
           END-IF
           MOVE TMS-SNG TO W-SNG.
           MOVE TMS-ENG TO W-ENG.
           MOVE TMS-BM TO W-BM.
           MOVE TMS-STCD TO W-STCD.
           MOVE TMS-ETCD TO W-ETCD.
       M-10.
           CALL "DB_F_Close" USING
            BY REFERENCE TMS-F_IDLST TMS-F_PNAME1.
           CALL "SD_Output" USING "D-DNG" D-DNG "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SNEN" A-SNEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SGET" A-SGET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ENEN" A-ENEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-EGET" A-EGET "p" RETURNING RESU.
           IF  W-DC = 0
               CALL "SD_Output" USING
                "A-BM" A-BM "p" RETURNING RESU
               CALL "SD_Output" USING
                "A-STCD" A-STCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "A-ETCD" A-ETCD "p" RETURNING RESU
           END-IF
      *
           PERFORM ACP-RTN THRU ACP-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "OUTPUT" DTWF_PNAME1 " " BY REFERENCE DTWF_IDLST "0".
           PERFORM TMS-RTN THRU TMS-EX.
           IF  W-BM = 2
               GO TO M-35
           END-IF
      *
           PERFORM STR-RTN THRU STR-EX.
           IF  W-BM = 1
               GO TO M-60
           END-IF.
       M-35.
           PERFORM URI-RTN THRU URI-EX.
       M-60.
           PERFORM NYU-RTN THRU NYU-EX.
           CALL "DB_F_Close" USING BY REFERENCE DTWF_IDLST DTWF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           IF  W-DC = 0
               MOVE 1 TO W-DMM
               CALL "SD_Output" USING
                "A-DMM" A-DMM "p" RETURNING RESU
               GO TO ACP-EX
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-RTN
           END-IF
           IF  W-SNEN < W-SND
               GO TO ACP-RTN
           END-IF.
       ACP-020.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-020
           END-IF
           IF  W-SGET < 1 OR > 12
               GO TO ACP-020
           END-IF
           IF  W-SNG < W-SNGD
               GO TO ACP-020
           END-IF
           MOVE W-SNG TO W-NG.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
           ELSE
               ADD 1 TO W-NEN
           END-IF.
       ACP-040.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-040
           END-IF
           IF  W-ENEN > W-END
               GO TO ACP-040
           END-IF.
       ACP-060.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-060
           END-IF
           IF  W-ENG > W-ENGD
               GO TO ACP-060
           END-IF
           IF  W-ENG > W-NG
               GO TO ACP-060
           END-IF.
       ACP-080.
           CALL "SD_Accept" USING BY REFERENCE A-BM "A-BM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-060
           END-IF
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-080
           END-IF
           IF  W-BM > 2
               GO TO ACP-080
           END-IF.
       ACP-100.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-080
           END-IF
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-100
           END-IF.
       ACP-120.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-100
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-100
           END-IF
           IF  W-STCD > W-ETCD
               GO TO ACP-120
           END-IF.
       ACP-140.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  W-DC = 0
               IF  ESTAT = PF9
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   GO TO ACP-EX
               END-IF
           END-IF
           IF  ESTAT = BTB
               IF  W-DC = 1
                   GO TO ACP-120
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-140
           END-IF
           IF  W-DMM = 9
               IF  W-DC = 1
                   GO TO ACP-RTN
               ELSE
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   GO TO ACP-EX
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-140
           END-IF.
       ACP-EX.
           EXIT.
       TMS-RTN.
           CALL "DB_F_Open" USING
            "INPUT" TMS-F_PNAME1 " " BY REFERENCE TMS-F_IDLST "0".
       TMS-020.
      *           READ TMS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TMS-F_PNAME1 BY REFERENCE TMS-R " " RETURNING RET.
           IF  RET = 1
               GO TO TMS-100
           END-IF
           IF  ZERO = TMS-KIN AND TMS-SHZ
               GO TO TMS-020
           END-IF.
       TMS-040.
           MOVE TMS-TCD TO W-TCD.
           MOVE TMS-NGP TO W-NGP.
      *
           MOVE SPACE TO DTW-R.
           INITIALIZE DTW-D1.
           MOVE SPACE TO DT1-TEKI.
           MOVE TMS-BC  TO DT1-BMC.
           MOVE TMS-NGP TO DT1-DATE.
           MOVE TMS-TCD TO DT1-TCD.
           MOVE TMS-KIN TO DT1-KIN.
           MOVE TMS-DC  TO DT1-DC.
           MOVE TMS-SHZ TO DT1-SHZ.
           MOVE TMS-TNC TO DT1-TNC.
           MOVE TMS-DATE TO DT1-SNGP.
       TMS-060.
      *           READ TMS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TMS-F_PNAME1 BY REFERENCE TMS-R " " RETURNING RET.
           IF  RET = 1
               GO TO TMS-080
           END-IF
           IF  ZERO = TMS-KIN AND TMS-SHZ
               GO TO TMS-060
           END-IF
           IF (TMS-TCD NOT = W-TCD) OR (TMS-NGP NOT = W-NGP)
               MOVE 9 TO DT1-EC
           END-IF
      *           WRITE DTW-R.
      *///////////////
           CALL "DB_Insert" USING
            DTWF_PNAME1 DTWF_LNAME DTW-R RETURNING RET.
           GO TO TMS-040.
       TMS-080.
           MOVE 9 TO DT1-EC.
      *           WRITE DTW-R.
      *///////////////
           CALL "DB_Insert" USING
            DTWF_PNAME1 DTWF_LNAME DTW-R RETURNING RET.
       TMS-100.
           CALL "DB_F_Close" USING
            BY REFERENCE TMS-F_IDLST TMS-F_PNAME1.
       TMS-EX.
           EXIT.
       STR-RTN.
           CALL "DB_F_Open" USING
            "INPUT" STRANYR_PNAME1 " " BY REFERENCE STRANYR_IDLST "0".
           MOVE 0 TO CHK.
       STR-020.
      *           READ STRANYR AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" STRANYR_PNAME1 BY REFERENCE STRANY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO STR-100
           END-IF
           IF  W-SNG > S1-NG
               GO TO STR-020
           END-IF
           IF  W-ENG < S1-NG
               GO TO STR-100
           END-IF
           IF  S1-TCD < W-STCD OR > W-ETCD
               GO TO STR-020
           END-IF
           IF  S1-TCD = 5000 OR 9850
               GO TO STR-020
           END-IF.
       STR-040.
           MOVE S1-DNO TO W-DNO.
      *
           MOVE SPACE TO DTW-R.
           IF  S1-GNO > 6
               INITIALIZE DTW-D2
               MOVE SPACE TO DT2-BI
               MOVE S2-DNO TO DT2-DNO
               MOVE S2-GNO TO DT2-GNO
               MOVE S2-DATE TO DT2-DATE
               MOVE S2-TCD TO DT2-TCD
               MOVE S2-BI TO DT2-BI
               MOVE S2-HNO TO DT2-HNO
               MOVE S2-TAX TO DT2-TAX
               MOVE S2-SNC TO DT2-SNC
               MOVE S2-DHC TO DT2-PC
           ELSE
               INITIALIZE DTW-D1
               MOVE SPACE   TO DT1-TEKI
               MOVE S1-DNO  TO DT1-DNO
               MOVE S1-GNO  TO DT1-GNO
               MOVE S1-DATE TO DT1-DATE
               MOVE S1-TCD  TO DT1-TCD
               MOVE S1-HCD  TO DT1-HCD
               MOVE S1-SUT  TO DT1-SUT
               MOVE S1-T    TO DT1-T
               MOVE S1-KIN  TO DT1-KIN
               MOVE S1-CSC  TO DT1-CSC
               MOVE S1-DC   TO DT1-DC
               MOVE S1-BKC  TO DT1-BKC
               MOVE S1-BMNO TO DT1-BMNO
               MOVE S1-FT   TO DT1-FT
               MOVE S1-SIZ  TO DT1-SIZ
               MOVE S1-SU(01) TO DT1-SU(01)
               MOVE S1-SU(02) TO DT1-SU(02)
               MOVE S1-SU(03) TO DT1-SU(03)
               MOVE S1-SU(04) TO DT1-SU(04)
               MOVE S1-SU(05) TO DT1-SU(05)
               MOVE S1-SU(06) TO DT1-SU(06)
               MOVE S1-SU(07) TO DT1-SU(07)
               MOVE S1-SU(08) TO DT1-SU(08)
               MOVE S1-SU(09) TO DT1-SU(09)
               MOVE S1-SU(10) TO DT1-SU(10)
               MOVE S1-CCD  TO DT1-CCD
               MOVE S1-BC   TO DT1-BC
               MOVE S1-SKC  TO DT1-SKC
               MOVE S1-TNC  TO DT1-TNC
               MOVE S1-HSC  TO DT1-HSC
               MOVE S1-KOSU TO DT1-KOSU
               MOVE S1-FRC  TO DT1-FRC
               MOVE S1-TCD2 TO DT1-TCD2
               MOVE S1-BIK  TO DT1-BIK
               MOVE S1-USC  TO DT1-USC
               MOVE S1-SNC  TO DT1-SNC
               MOVE S1-SKU  TO DT1-SKU
               MOVE S1-DHC  TO DT1-PC
           END-IF.
       STR-060.
      *           READ STRANYR AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" STRANYR_PNAME1 BY REFERENCE STRANY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO STR-080
           END-IF
           IF  W-ENG < S1-NG
               GO TO STR-080
           END-IF
           IF  S1-TCD < W-STCD OR > W-ETCD
               GO TO STR-060
           END-IF
           IF  S1-TCD = 5000 OR 9850
               GO TO STR-060
           END-IF
           IF  S1-DNO NOT = W-DNO
               MOVE 9 TO DT1-EC
           END-IF
      *           WRITE DTW-R.
      *///////////////
           CALL "DB_Insert" USING
            DTWF_PNAME1 DTWF_LNAME DTW-R RETURNING RET.
           GO TO STR-040.
       STR-080.
           MOVE 9 TO DT1-EC.
      *           WRITE DTW-R.
      *///////////////
           CALL "DB_Insert" USING
            DTWF_PNAME1 DTWF_LNAME DTW-R RETURNING RET.
       STR-100.
           CALL "DB_F_Close" USING
            BY REFERENCE STRANYR_IDLST STRANYR_PNAME1.
       STR-EX.
           EXIT.
       URI-RTN.
           CALL "DB_F_Open" USING
            "INPUT" URIRYR_PNAME1 " " BY REFERENCE URIRYR_IDLST "0".
       URI-020.
      *           READ URIRYR AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" URIRYR_PNAME1 BY REFERENCE URIRY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO URI-100
           END-IF
           IF  W-SNG > UR-NG
               GO TO URI-020
           END-IF
           IF  W-ENG < UR-NG
               GO TO URI-100
           END-IF
           IF  UR-TCD < W-STCD OR > W-ETCD
               GO TO URI-020
           END-IF.
       URI-040.
           MOVE UR-DNO TO W-DNO.
      *
           MOVE SPACE TO DTW-R.
           INITIALIZE DTW-D1.
           MOVE SPACE TO DT1-TEKI.
           MOVE UR-BMC  TO DT1-BMC.
           MOVE UR-DNO  TO DT1-DNO.
           MOVE UR-GNO  TO DT1-GNO.
           MOVE UR-DATE TO DT1-DATE.
           MOVE UR-TCD  TO DT1-TCD.
           MOVE UR-HCD  TO DT1-KCD.
           MOVE UR-SUT  TO DT1-SUT.
           MOVE UR-T    TO DT1-T.
           MOVE UR-KIN  TO DT1-KIN.
           MOVE UR-CSC  TO DT1-CSC.
           MOVE UR-DC   TO DT1-DC.
           MOVE UR-BKC  TO DT1-BKC.
           MOVE UR-GT   TO DT1-FT.
           MOVE UR-HYC  TO DT1-HYC.
           MOVE UR-YC   TO DT1-YC.
           MOVE UR-SGP  TO DT1-SGP.
           MOVE UR-NNO  TO DT1-NNO.
           MOVE UR-JCD  TO DT1-JCD.
           MOVE UR-TEK  TO DT1-TEKI.
           MOVE UR-SKU  TO DT1-SKU.
           MOVE UR-PC   TO DT1-PC.
       URI-060.
      *           READ URIRYR AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" URIRYR_PNAME1 BY REFERENCE URIRY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO URI-080
           END-IF
           IF  W-ENG < UR-NG
               GO TO URI-080
           END-IF
           IF  UR-TCD < W-STCD OR > W-ETCD
               GO TO URI-060
           END-IF
           IF  UR-DNO NOT = W-DNO
               MOVE 9 TO DT1-EC
           END-IF
      *           WRITE DTW-R.
      *///////////////
           CALL "DB_Insert" USING
            DTWF_PNAME1 DTWF_LNAME DTW-R RETURNING RET.
           GO TO URI-040.
       URI-080.
           MOVE 9 TO DT1-EC.
      *           WRITE DTW-R.
      *///////////////
           CALL "DB_Insert" USING
            DTWF_PNAME1 DTWF_LNAME DTW-R RETURNING RET.
       URI-100.
           CALL "DB_F_Close" USING
            BY REFERENCE URIRYR_IDLST URIRYR_PNAME1.
       URI-EX.
           EXIT.
       NYU-RTN.
           CALL "DB_F_Open" USING
            "INPUT" NYURYR_PNAME1 " " BY REFERENCE NYURYR_IDLST "0".
       NYU-020.
      *           READ NYURYR AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" NYURYR_PNAME1 BY REFERENCE NYURY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO NYU-100
           END-IF
           IF  W-SNG > NY-NG
               GO TO NYU-020
           END-IF
           IF  W-ENG < NY-NG
               GO TO NYU-100
           END-IF
           IF  NY-TCD < W-STCD OR > W-ETCD
               GO TO NYU-020
           END-IF
           IF  NY-TCD = 5000 OR 9850
               GO TO NYU-020
           END-IF
           IF  W-BM = 1
               IF  NY-BMC NOT = 0
                   GO TO NYU-020
               END-IF
           END-IF
           IF  W-BM = 2
               IF  NY-BMC = 0
                   GO TO NYU-020
               END-IF
           END-IF.
       NYU-040.
           MOVE NY-DNO  TO W-DNO.
      *
           MOVE SPACE TO DTW-R.
           INITIALIZE DTW-D1.
           MOVE SPACE TO DT1-TEKI.
           MOVE NY-BMC  TO DT1-BMC.
           MOVE 1       TO DT1-UNC.
           MOVE NY-DNO  TO DT1-DNO.
           MOVE NY-GNO  TO DT1-GNO.
           MOVE NY-DATE TO DT1-DATE.
           MOVE NY-TCD  TO DT1-TCD.
           MOVE NY-KIN  TO DT1-KIN.
           MOVE NY-NKC  TO DT1-NKC.
           MOVE NY-SSC  TO DT1-SSC.
           MOVE NY-TKG  TO DT1-TKG.
           MOVE NY-SKNG TO DT1-SKNG.
           MOVE NY-DCC  TO DT1-DCC.
           MOVE NY-SKU  TO DT1-SKU.
           MOVE NY-PC   TO DT1-PC.
       NYU-060.
      *           READ NYURYR AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" NYURYR_PNAME1 BY REFERENCE NYURY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO NYU-080
           END-IF
           IF  W-ENG < NY-NG
               GO TO NYU-080
           END-IF
           IF  NY-TCD < W-STCD OR > W-ETCD
               GO TO NYU-060
           END-IF
           IF  NY-TCD = 5000 OR 9850
               GO TO NYU-060
           END-IF
           IF  W-BM = 1
               IF  NY-BMC NOT = 0
                   GO TO NYU-060
               END-IF
           END-IF
           IF  W-BM = 2
               IF  NY-BMC = 0
                   GO TO NYU-060
               END-IF
           END-IF
           IF  NY-DNO NOT = W-DNO
               MOVE 9 TO DT1-EC
           END-IF
      *           WRITE DTW-R.
      *///////////////
           CALL "DB_Insert" USING
            DTWF_PNAME1 DTWF_LNAME DTW-R RETURNING RET.
           GO TO NYU-040.
       NYU-080.
           MOVE 9 TO DT1-EC.
      *           WRITE DTW-R.
      *///////////////
           CALL "DB_Insert" USING
            DTWF_PNAME1 DTWF_LNAME DTW-R RETURNING RET.
       NYU-100.
           CALL "DB_F_Close" USING
            BY REFERENCE NYURYR_IDLST NYURYR_PNAME1.
       NYU-EX.
           EXIT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY080.
      ****************************************************************
      *    PROGRAM         :  年間品名サイズ別　出荷数明細表         *
      *    PRINTER TYPE    :  JIPS                                   *
      *    SCREEN          :  ******                                 *
      *        変更　　　  :  95/08/09                               *
      *    COMPILE TYPE    :  COBOL                                  *
      *    JS-SIGN         :  0=入力 , 1=教育 , 2=ワーク , 3=ハイパー*
      *    W-JS2           :  0=出荷返品 , 1=出荷   , 2=返品         *
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM7200.
       OBJECT-COMPUTER. SYSTEM7200.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-JS2              PIC  9(001).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
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
           02  F              PIC  X(015) VALUE SPACE.
           02  F              PIC  N(023) VALUE
                "＊＊＊　　品名サイズ別　出荷数明細表　　＊＊＊".
           02  F              PIC  X(004) VALUE SPACE.
           02  H-MID          PIC  N(004) VALUE SPACE.
           02  F              PIC  X(011) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(001) VALUE "1".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(002) VALUE "３号".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(002) VALUE "２号".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(002) VALUE "１号".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(002) VALUE "０号".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　中".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　大".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(002) VALUE "特大".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "28.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "29.0".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(004) VALUE "30.0".
           02  F              PIC  X(011) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(044) VALUE SPACE.
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
           02  F              PIC  X(011) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(044) VALUE SPACE.
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
           02  F              PIC  X(019) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(044) VALUE SPACE.
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
           02  F              PIC  X(021) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　合　計".
       01  W-P.
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  9(001).
           02  P-SUD.
             03  P-SU         PIC ----,--9  OCCURS  10 TIMES.
           02  P-SUT          PIC ---,---,--9.
       01  W-DATA.
           02  W-SNG.
             03  W-SNEN       PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-ENG.
             03  W-ENEN       PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-HCD          PIC  9(006).
           02  W-AHCDD.
             03  W-AHCD  OCCURS   15.
               04  W-HCDF     PIC  9(006).
               04  W-HCDT     PIC  9(006).
           02  W-L            PIC  9(002).
           02  CNT            PIC  9(002).
           02  W-C            PIC  9(001).
           02  CNTD           PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-GC           PIC  9(002).
           02  W-GCD          PIC  9(002).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-NC           PIC  9(001).
           02  W-TC           PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-SC           PIC  9(001).
           02  W-ZCD.
             03  W-ZC    OCCURS   4  PIC  9(001).
           02  W-ASUD.
             03  W-ASU   OCCURS   4.
               04  W-SU    OCCURS  10  PIC S9(007).
             03  W-SUT        PIC S9(009).
           02  WT-ZCD.
             03  WT-ZC   OCCURS   4  PIC  9(001).
           02  WT-ASUD.
             03  WT-ASU  OCCURS   4.
               04  WT-SU   OCCURS  10  PIC S9(007).
             03  WT-SUT       PIC S9(009).
           02  W-SSD.
             03  W-SS    OCCURS  10  PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LIHIM.
           COPY LSPF.
       01  NSSW-F_HMY080.
           02  NSSW-F_PNAME1  PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  NSSW-F_LNAME   PIC  X(013)  VALUE "NSSW-F_HMY080".
           02  F              PIC  X(001).
           02  NSSW-F_KEY1    PIC  X(100)  VALUE SPACE.
           02  NSSW-F_KEY2    PIC  X(100)  VALUE SPACE.
           02  NSSW-F_SORT    PIC  X(100)  VALUE SPACE.
           02  NSSW-F_IDLST   PIC  X(100)  VALUE SPACE.
           02  NSSW-F_RES     USAGE  POINTER.
       01  NSSW-R.
           02  NSS-HCD        PIC  9(006).
           02  NSS-SC         PIC  9(001).
           02  NSS-SUD.
             03  NSS-SUDA  OCCURS  10.
               04  NSS-SU     PIC S9(006)  COMP-3.
           02  NSS-TSU        PIC S9(008)  COMP-3.
           02  F              PIC  X(010).
           02  NSS-BC3        PIC  9(002).
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　品名サイズ別　出荷数明細表　　＊＊＊".
           02  FILLER.
             03  FILLER  PIC  N(002) VALUE "【　".
             03  FILLER  PIC  X(001) VALUE "'".
             03  FILLER  PIC  9(002) .
             03  FILLER  PIC  N(001) VALUE "年".
             03  FILLER  PIC Z9.
             03  FILLER  PIC  N(001) VALUE "月".
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  X(001) VALUE "'".
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  N(001) VALUE "年".
             03  FILLER  PIC Z9.
             03  FILLER  PIC  N(001) VALUE "月".
             03  FILLER  PIC  N(002) VALUE "　】".
       01  C-HCM.
           02  FILLER.
             03  FILLER  PIC  N(002) VALUE "品名".
             03  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(002) VALUE "終了".
             03  FILLER  PIC  X(005) VALUE "=ｆ･9".
           02  FILLER  PIC  N(001) VALUE "～".
           02  FILLER  PIC  N(001) VALUE "～".
           02  FILLER  PIC  N(001) VALUE "～".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(004) VALUE "以下なし".
             03  FILLER  PIC  X(006) VALUE "=ｆ･10".
           02  FILLER  PIC  N(001) VALUE "～".
           02  FILLER  PIC  N(001) VALUE "～".
           02  FILLER  PIC  N(001) VALUE "～".
           02  FILLER  PIC  N(001) VALUE "～".
           02  FILLER  PIC  N(001) VALUE "～".
           02  FILLER  PIC  N(001) VALUE "～".
           02  FILLER  PIC  N(001) VALUE "～".
           02  FILLER  PIC  N(001) VALUE "～".
           02  FILLER  PIC  N(001) VALUE "～".
           02  FILLER  PIC  N(001) VALUE "～".
           02  FILLER  PIC X(22) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-HCDF    PIC  9(006).
             03  A-HCDT    PIC  9(006).
           02  A-DMM     PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  S-HCDF    PIC  X(006) VALUE "      ".
             03  S-HCDT    PIC  X(006) VALUE "      ".
           02  FILLER.
             03  C-MIDK    PIC  N(005) VALUE "【教　育】".
             03  C-MIDW    PIC  N(005) VALUE "【ワーク】".
             03  C-MIDH    PIC  N(005) VALUE "ハイパーＶ".
       01  C-ERR.
           02  FILLER.
             03  E-STAT    PIC  X(002).
             03  E-ME1     PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2     PIC  X(018) VALUE
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
            "C-MID" " " "0" "0" "74" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "1" "10" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" " " "4" "0" "28" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "4" "18" "4" " " "02C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "X" "4" "22" "1" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "9" "4" "23" "2" "04C-MID" " " RETURNING RESU.
       CALL "SD_From" USING
            "05C-MID" BY REFERENCE W-SNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "4" "25" "2" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "Z9" "4" "27" "2" "06C-MID" " " RETURNING RESU.
       CALL "SD_From" USING
            "07C-MID" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "N" "4" "29" "2" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "N" "4" "32" "2" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "4" "35" "1" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "11C-MID" "9" "4" "36" "2" "10C-MID" " " RETURNING RESU.
       CALL "SD_From" USING
            "11C-MID" BY REFERENCE W-ENEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "12C-MID" "N" "4" "38" "2" "11C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "13C-MID" "Z9" "4" "40" "2" "12C-MID" " " RETURNING RESU.
       CALL "SD_From" USING
            "13C-MID" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "14C-MID" "N" "4" "42" "2" "13C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "15C-MID" "N" "4" "44" "4" "14C-MID" " " RETURNING RESU.
      *C-HCM.
       CALL "SD_Init" USING
            "C-HCM" " " "0" "0" "83" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-HCM" " " "6" "0" "19" " " "C-HCM" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-HCM" "N" "6" "20" "4" " " "01C-HCM" RETURNING RESU.
       CALL "SD_Init" USING
            "03C-HCM" "X" "6" "24" "4" "02C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-HCM" "N" "6" "37" "2" "03C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-HCM" "N" "6" "50" "4" "04C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-HCM" "X" "6" "54" "5" "05C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-HCM" "N" "7" "37" "2" "06C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-HCM" "N" "8" "37" "2" "07C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-HCM" "N" "9" "37" "2" "08C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-HCM" " " "10" "0" "16" "09C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "11C-HCM" "N" "10" "37" "2" " " "10C-HCM" RETURNING RESU.
       CALL "SD_Init" USING
            "12C-HCM" "N" "10" "50" "8" "11C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "13C-HCM" "X" "10" "58" "6" "12C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "14C-HCM" "N" "11" "37" "2" "10C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "15C-HCM" "N" "12" "37" "2" "14C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "16C-HCM" "N" "13" "37" "2" "15C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "17C-HCM" "N" "14" "37" "2" "16C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "18C-HCM" "N" "15" "37" "2" "17C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "19C-HCM" "N" "16" "37" "2" "18C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "20C-HCM" "N" "17" "37" "2" "19C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "21C-HCM" "N" "18" "37" "2" "20C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "22C-HCM" "N" "19" "37" "2" "21C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "23C-HCM" "N" "20" "37" "2" "22C-HCM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "24C-HCM" "X" "23" "37" "22" "23C-HCM" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "W-L" "0" "12" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-HCDF" "9" "W-L" "30" "6" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-HCDF" BY REFERENCE W-HCDF(1) "6" "1" BY REFERENCE CNT 12
             RETURNING RESU.
       CALL "SD_Init" USING
            "A-HCDT" "9" "W-L" "40" "6" "A-HCDF" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-HCDT" BY REFERENCE W-HCDT(1) "6" "1" BY REFERENCE CNT 12
             RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "23" "39" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "42" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-DSP" " " "W-L" "0" "12" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "S-HCDF" "X" "W-L" "30" "6" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "S-HCDT" "X" "W-L" "40" "6" "S-HCDF" " " RETURNING RESU.
       CALL "SD_Init" USING
            "02C-DSP" " " "1" "0" "30" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "C-MIDK" "N" "1" "62" "10" " " "02C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "C-MIDW" "N" "1" "62" "10" "C-MIDK" " " RETURNING RESU.
       CALL "SD_Init" USING
            "C-MIDH" "N" "1" "62" "10" "C-MIDW" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "97" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "97" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
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
           IF  JS-SIGN > 3
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP RUN
           END-IF.
           ACCEPT W-JS2 FROM ARGUMENT-VALUE.
           IF  W-JS2 > 2
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP RUN
           END-IF.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           IF  W-JS2 = 1
               MOVE "（出荷）" TO H-MID
           END-IF.
           IF  W-JS2 = 2
               MOVE "（返品）" TO H-MID
           END-IF.
           MOVE D-SSNG TO W-SNG.
           MOVE D-ESNG TO W-ENG.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                              RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p"
                                              RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "C-MIDK" C-MIDK "p"
                                              RETURNING RESU
               GO TO M-45
           END-IF.
           IF  JS-SIGN = 2
               CALL "SD_Output" USING "C-MIDW" C-MIDW "p"
                                              RETURNING RESU
               GO TO M-45
           END-IF.
           IF  JS-SIGN = 3
               CALL "SD_Output" USING "C-MIDH" C-MIDH "p"
                                              RETURNING RESU
               GO TO M-45
           END-IF.
           CALL "SD_Output" USING "C-HCM" C-HCM "p" 
                                              RETURNING RESU.
       M-10.
           MOVE ZERO TO CNT W-C.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-15.
           ADD 1 TO CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT > 15
               GO TO M-30
           END-IF.
       M-20.
           IF  W-C = 1
               MOVE ZERO TO W-HCDF(CNT)
               CALL "SD_Output" USING "S-HCDF" S-HCDF "p"
                                              RETURNING RESU
               GO TO M-25
           END-IF.
           CALL "SD_Accept" USING BY REFERENCE A-HCDF "A-HCDF" "9" "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                              RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ESTAT = ADV
               MOVE 1 TO W-C
               MOVE ZERO TO W-HCDF(CNT)
               CALL "SD_Output" USING "S-HCDF" S-HCDF "p"
                                              RETURNING RESU
               GO TO M-25
           END-IF.
           IF  ESTAT = BTB
               SUBTRACT 1 FROM CNT W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               IF  CNT = ZERO
                   GO TO M-15
               ELSE
                   GO TO M-25
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
           IF  W-HCDF(CNT) = 999999
               MOVE 1 TO W-C
               MOVE ZERO TO W-HCDF(CNT)
               CALL "SD_Output" USING "S-HCDF" S-HCDF "p"
                                              RETURNING RESU
               GO TO M-25
           END-IF.
           IF (CNT NOT = 1) AND (W-HCDF(CNT) = ZERO)
               MOVE 1 TO W-C
               MOVE ZERO TO W-HCDF(CNT)
               CALL "SD_Output" USING "S-HCDF" S-HCDF "p"
                                              RETURNING RESU
               GO TO M-25
           END-IF.
           IF (CNT NOT = 1) AND (W-HCDF(CNT) NOT = ZERO)
               COMPUTE CNTD = CNT - 1
               IF W-HCDT(CNTD) >= W-HCDF(CNT)
                   GO TO M-20
               END-IF
           END-IF.
       M-25.
           IF  W-C = 1
               MOVE ZERO TO W-HCDT(CNT)
               CALL "SD_Output" USING "S-HCDT" S-HCDT "p"
                                              RETURNING RESU
               GO TO M-15
           END-IF.
           CALL "SD_Accept" USING BY REFERENCE A-HCDT "A-HCDT" "9" "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF.
           IF  W-HCDF(CNT) > W-HCDT(CNT)
               GO TO M-25
           END-IF.
           IF  CNT = 1
               IF  W-HCDT(1) = ZERO
                   GO TO M-25
               END-IF
           END-IF.
           GO TO M-15.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = BTB
               GO TO M-40
           END-IF.
           MOVE ZERO TO W-C.
       M-35.
           SUBTRACT 1 FROM CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT = ZERO
               GO TO M-10
           END-IF.
           IF  W-HCDT(CNT) = ZERO
               GO TO M-35
           END-IF.
           GO TO M-25.
       M-40.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF.
           IF  W-DMM = 9
               GO TO M-10
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-30
           END-IF.
       M-45.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO NSSW-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NSSW-F_PNAME1 " " BY REFERENCE NSSW-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-50.
      *           READ NSSW-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" NSSW-F_PNAME1 BY REFERENCE NSSW-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE NSSW-F_IDLST NSSW-F_PNAME1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                              RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                              RETURNING RESU
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                              RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ZERO = NSS-SU(01) AND NSS-SU(02) AND NSS-SU(03) AND
                     NSS-SU(04) AND NSS-SU(05) AND NSS-SU(06) AND
                     NSS-SU(07) AND NSS-SU(08) AND NSS-SU(09) AND
                     NSS-SU(10)
               GO TO M-50
           END-IF.
           IF  JS-SIGN = 0
               PERFORM S-35 THRU S-45
               IF  W-GCD = ZERO
                   GO TO M-50
               END-IF
           END-IF.
           IF  JS-SIGN = 1
               IF  NSS-BC3 NOT = 30
                   GO TO M-50
               END-IF
           END-IF.
           IF  JS-SIGN = 2
               IF  NSS-BC3 NOT = 20
                   GO TO M-50
               END-IF
           END-IF.
           IF  JS-SIGN NOT = 3
               GO TO M-55
           END-IF.
           MOVE NSS-HCD TO HI-HCD.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 0 TO HI-HPV
           END-IF.
           IF  HI-HPV NOT = 1
               GO TO M-50
           END-IF.
       M-55.
           MOVE W-SNEN TO H-SNEN.
           MOVE W-SGET TO H-SGET.
           MOVE W-ENEN TO H-ENEN.
           MOVE W-EGET TO H-EGET.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
       M-60.
           IF  JS-SIGN = 0
               MOVE W-GCD TO W-GC
           END-IF.
           MOVE ZERO TO WT-ZCD WT-ASUD W-DC.
       M-65.
           MOVE NSS-HCD TO W-HCD.
           MOVE ZERO TO W-ZCD W-ASUD.
       M-70.
           PERFORM S-20 THRU S-30.
       M-75.
      *           READ NSSW-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" NSSW-F_PNAME1 BY REFERENCE NSSW-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
           IF  ZERO = NSS-SU(01) AND NSS-SU(02) AND NSS-SU(03) AND
                     NSS-SU(04) AND NSS-SU(05) AND NSS-SU(06) AND
                     NSS-SU(07) AND NSS-SU(08) AND NSS-SU(09) AND
                     NSS-SU(10)
               GO TO M-75
           END-IF.
           IF  JS-SIGN = 0
               PERFORM S-35 THRU S-45
               IF  W-GCD = ZERO
                   GO TO M-75
               END-IF
           END-IF.
           IF  JS-SIGN = 1
               IF  NSS-BC3 NOT = 30
                   GO TO M-75
               END-IF
           END-IF.
           IF  JS-SIGN = 2
               IF  NSS-BC3 NOT = 20
                   GO TO M-75
               END-IF
           END-IF.
           IF  JS-SIGN NOT = 3
               GO TO M-80
           END-IF.
           MOVE NSS-HCD TO HI-HCD.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 0 TO HI-HPV
           END-IF.
           IF  HI-HPV NOT = 1
               GO TO M-75
           END-IF.
       M-80.
           IF  JS-SIGN = 0
               IF  W-GC NOT = W-GCD
                   GO TO M-85
               END-IF
           END-IF.
           IF  NSS-HCD = W-HCD
               GO TO M-70
           END-IF.
           PERFORM S-50 THRU S-70.
           GO TO M-65.
       M-85.
           PERFORM S-50 THRU S-70.
           IF  W-DC = 2
               PERFORM S-75 THRU S-95
           ELSE
               MOVE SPACE TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF.
           GO TO M-60.
       M-90.
           PERFORM S-50 THRU S-70.
           IF  W-DC = 2
               PERFORM S-75 THRU S-95
           END-IF.
       M-95.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NSSW-F_IDLST NSSW-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                              RETURNING RESU.
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           IF  NSS-SC = 1
               MOVE 4 TO W-SC
           END-IF.
           IF  NSS-SC = 2
               MOVE 1 TO W-SC
           END-IF.
           IF  NSS-SC = 3
               MOVE 2 TO W-SC
           END-IF.
           IF  NSS-SC = 4
               MOVE 3 TO W-SC
           END-IF.
           ADD NSS-TSU TO W-SUT WT-SUT.
           MOVE 1 TO W-ZC(W-SC) WT-ZC(W-SC).
           MOVE ZERO TO CNTD.
       S-25.
           ADD 1 TO CNTD.
           IF  CNTD NOT = 11
               MOVE NSS-SU(CNTD) TO W-SU(W-SC,CNTD)
               ADD NSS-SU(CNTD) TO WT-SU(W-SC,CNTD)
               GO TO S-25
           END-IF.
       S-30.
           EXIT.
       S-35.
           MOVE ZERO TO CNT W-GCD.
       S-40.
           ADD 1 TO CNT.
           IF  CNT = 16
               GO TO S-45
           END-IF.
           IF  W-HCDT(CNT) NOT = ZERO
               IF  NSS-HCD >= W-HCDF(CNT) AND <= W-HCDT(CNT)
                   MOVE CNT TO W-GCD
                   GO TO S-45
               END-IF
           END-IF.
           GO TO S-40.
       S-45.
           EXIT.
       S-50.
           IF  W-DC = 1
               MOVE 2 TO W-DC
           END-IF.
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "　マスター　なし　" TO HI-NAME
           END-IF.
           MOVE ZERO TO W-SC W-NC.
       S-55.
           ADD 1 TO W-SC.
           IF  W-SC = 5
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               GO TO S-70
           END-IF.
           IF  W-ZC(W-SC) = 0
               GO TO S-55
           END-IF.
           MOVE ZERO TO W-SSD.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           IF  W-NC = 0
               MOVE 9 TO W-NC
               MOVE W-HCD TO P-HCD
               MOVE HI-NAME TO P-NAME
           END-IF.
           IF  W-SC = 1
               MOVE HI-SS2 TO W-SSD
               MOVE 2 TO P-SIZ
               IF  0 = W-ZC(2) AND W-ZC(3) AND W-ZC(4)
                   MOVE W-SUT TO P-SUT
               END-IF
           END-IF.
           IF  W-SC = 2
               MOVE HI-SS3 TO W-SSD
               MOVE 3 TO P-SIZ
               IF  0 = W-ZC(3) AND W-ZC(4)
                   MOVE W-SUT TO P-SUT
               END-IF
           END-IF.
           IF  W-SC = 3
               MOVE HI-SS4 TO W-SSD
               MOVE 0 TO W-SS(10)
               MOVE 4 TO P-SIZ
               IF  0 = W-ZC(4)
                   MOVE W-SUT TO P-SUT
               END-IF
           END-IF.
           IF  W-SC = 4
               MOVE HI-SS1 TO W-SSD
               MOVE 1 TO P-SIZ
               MOVE W-SUT TO P-SUT
           END-IF.
           MOVE ZERO TO CNTD.
       S-60.
           ADD 1 TO CNTD.
           IF  CNTD = 11
               GO TO S-65
           END-IF.
           IF  W-SS(CNTD) NOT = 0
               MOVE W-SU(W-SC,CNTD) TO P-SU(CNTD)
           END-IF.
           GO TO S-60.
       S-65.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-HCD TO P-HCD
               MOVE HI-NAME TO P-NAME
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO S-55.
       S-70.
           EXIT.
       S-75.
           MOVE ZERO TO W-SC W-NC.
       S-80.
           ADD 1 TO W-SC.
           IF  W-SC = 5
               MOVE SPACE TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               GO TO S-95
           END-IF.
           IF  WT-ZC(W-SC) = 0
               GO TO S-80
           END-IF.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           IF  W-NC = 0
               MOVE 9 TO W-NC
               MOVE "　　　　　（　合　計　）　" TO P-NAME
           END-IF.
           IF  W-SC = 1
               MOVE 2 TO P-SIZ
               IF  0 = WT-ZC(2) AND WT-ZC(3) AND WT-ZC(4)
                   MOVE WT-SUT TO P-SUT
               END-IF
           END-IF.
           IF  W-SC = 2
               MOVE 3 TO P-SIZ
               IF  0 = WT-ZC(3) AND WT-ZC(4)
                   MOVE WT-SUT TO P-SUT
               END-IF
           END-IF.
           IF  W-SC = 3
               MOVE 4 TO P-SIZ
               IF  0 = WT-ZC(4)
                   MOVE WT-SUT TO P-SUT
               END-IF
           END-IF.
           IF  W-SC = 4
               MOVE 1 TO P-SIZ
               MOVE WT-SUT TO P-SUT
           END-IF.
           MOVE ZERO TO CNTD.
       S-85.
           ADD 1 TO CNTD.
           IF  CNTD = 11
               GO TO S-90
           END-IF.
           IF  W-SC = 2
               IF  CNTD > 9
                   GO TO S-85
               END-IF
           END-IF.
           IF  W-SC = 3
               IF  CNTD > 8
                   GO TO S-85
               END-IF
           END-IF.
           MOVE WT-SU(W-SC,CNTD) TO P-SU(CNTD).
           GO TO S-85.
       S-90.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE "　　　　　（　合　計　）　" TO P-NAME
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO S-80.
       S-95.
           EXIT.

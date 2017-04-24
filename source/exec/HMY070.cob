       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY070.
      ***************************************************************
      *    PROGRAM         :  年間得意先品名サイズ別　出荷数明細表  *
      *    PRINTER TYPE    :  JIPS                                  *
      *    SCREEN          :  ******                                *
      *        変更　　　  :  95/08/09                              *
      *    COMPILE TYPE    :  COBOL                                 *
      *    JS-SIGN         :  0=得意先品名別  1=品名得意先別        *
      *    W-JS            :  0=全体  1=生協  2=ハイパーＶ          *
      *    W-JS2           :  0=全体  1=出荷  2=返品                *
      ***************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM7200.
       OBJECT-COMPUTER. SYSTEM7200.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-JS               PIC  9(001).
       77  W-JS2              PIC  9(001).
       77  W-POC              PIC  9(001) VALUE 0.
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
           02  F              PIC  X(012) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-MID          PIC  N(005) VALUE SPACE.
           02  F              PIC  N(016) VALUE
                "サイズ別　出荷数明細表　　＊＊＊".
           02  F              PIC  X(005) VALUE SPACE.
           02  H-KBN          PIC  N(004) VALUE SPACE.
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  H-OM1.
             03  H-TCD1       PIC  X(004).
             03  F            PIC  X(001).
             03  H-TNA1       PIC  N(008).
             03  F            PIC  X(002).
           02  H-OM2   REDEFINES H-OM1.
             03  F            PIC  X(001).
             03  H-HCD2       PIC  X(004).
             03  F            PIC  X(002).
             03  H-HNA2       PIC  N(008).
           02  F              PIC  X(117) VALUE SPACE.
       01  HEAD3.
           02  H-UM1.
             03  F            PIC  X(002).
             03  H-HCD1       PIC  X(004).
             03  F            PIC  X(002).
             03  H-HNA1       PIC  N(008).
           02  H-UM2   REDEFINES H-UM1.
             03  F            PIC  X(001).
             03  H-TCD2       PIC  X(004).
             03  F            PIC  X(001).
             03  H-TNA2       PIC  N(008).
             03  F            PIC  X(002).
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
           02  F              PIC  X(010) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(045) VALUE SPACE.
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
           02  F              PIC  X(010) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(045) VALUE SPACE.
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
           02  F              PIC  X(018) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(045) VALUE SPACE.
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
           02  F              PIC  X(020) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　合　計".
       01  W-P1.
           02  P-T1.
             03  P-TCD1       PIC  9(004).
             03  F            PIC  X(001).
             03  P-TNA1       PIC  N(026).
             03  F            PIC  X(002).
           02  P-T2    REDEFINES P-T1.
             03  P-HCD2       PIC  9(006).
             03  F            PIC  X(001).
             03  P-HNA2       PIC  N(026).
           02  F              PIC  X(090).
       01  W-P2.
           02  F              PIC  X(001).
           02  P-M1.
             03  P-HCD1       PIC  9(006).
             03  F            PIC  X(001).
             03  P-HNA1       PIC  N(024).
           02  P-M2    REDEFINES P-M1.
             03  P-TCD2       PIC  9(004).
             03  F            PIC  X(001).
             03  P-TNA2       PIC  N(024).
             03  F            PIC  X(002).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  9(001).
           02  P-SUD.
             03  P-SU         PIC ----,--9  OCCURS  10 TIMES.
           02  P-SUT          PIC --,---,--9.
       01  W-DATA.
           02  W-SNG.
             03  W-SNEN       PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-ENG.
             03  W-ENEN       PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-TCD          PIC  9(004).
           02  W-ATCDD.
             03  W-ATCD  OCCURS   15.
               04  W-TCDF     PIC  9(004).
               04  W-TCDT     PIC  9(004).
           02  W-HCD          PIC  9(006).
           02  W-AHCDD.
             03  W-AHCD  OCCURS   15.
               04  W-HCDF     PIC  9(006).
               04  W-HCDT     PIC  9(006).
           02  W-L            PIC  9(002).
           02  W-C1           PIC  9(002).
           02  W-C2           PIC  9(002).
           02  W-C3           PIC  9(002).
           02  W-C4           PIC  9(002).
           02  CNT            PIC  9(002).
           02  W-EC           PIC  9(001).
           02  W-AC           PIC  9(001).
           02  CNTD           PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-GC           PIC  9(002).
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
           02  W-TNA          PIC  N(026).
           02  W-HNA          PIC  N(024).
           02  W-HSD.
             03  W-SS1        PIC  9(010).
             03  W-SS2        PIC  9(010).
             03  W-SS3        PIC  9(010).
             03  W-SS4        PIC  9(010).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LSPF.
       01  NSSW-F_HMY070.
           02  NSSW-F_PNAME1  PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  NSSW-F_LNAME   PIC  X(013)  VALUE "NSSW-F_HMY070".
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
           02  NSS-TCD        PIC  9(004).
           02  NSS-NG         PIC  9(004).
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
           02  C-CL  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊　　　　　　　サイズ別　出荷数明細表　　＊＊＊".
           02  FILLER.
             03  FILLER  PIC  N(002) VALUE "【　".
             03  FILLER  PIC  X(001) VALUE "'".
             03  FILLER  PIC  9(002).
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
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(002) VALUE "終了".
             03  FILLER  PIC  X(005) VALUE ":ｆ･9".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(001) VALUE "～".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(004) VALUE "以下なし".
             03  FILLER  PIC  X(006) VALUE ":ｆ･10".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(001) VALUE "～".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(001) VALUE "～".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(001) VALUE "～".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(001) VALUE "～".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(001) VALUE "～".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(001) VALUE "～".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(001) VALUE "～".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(001) VALUE "～".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(001) VALUE "～".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(001) VALUE "～".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(001) VALUE "～".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC  N(001) VALUE "～".
           02  FILLER    PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID0.
           02    FILLER  PIC  N(005) VALUE "得意先品名".
           02  FILLER.
             03  FILLER  PIC  N(003) VALUE "得意先".
             03  FILLER  PIC  N(003) VALUE "品　名".
       01  C-MID1.
           02    FILLER  PIC  N(005) VALUE "品名得意先".
           02  FILLER.
             03  FILLER  PIC  N(003) VALUE "品　名".
             03  FILLER  PIC  N(003) VALUE "得意先".
       01  C-ACP.
           02  FILLER.
             03  A-TCDF   PIC  9(004).
             03  A-TCDT   PIC  9(004).
             03  A-HCDF   PIC  9(006).
             03  A-HCDT   PIC  9(006).
           02  A-DMM      PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  S-TCDF   PIC  X(004) VALUE "    ".
             03  S-TCDT   PIC  X(004) VALUE "    ".
             03  S-HCDF   PIC  X(006) VALUE "      ".
             03  S-HCDT   PIC  X(006) VALUE "      ".
       01  C-ERR.
           02  FILLER.
             03  E-STAT    PIC  X(002) .
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
            "C-MID" " " "0" "0" "185" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "1" "10" "52" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" " " "4" "0" "28" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "4" "21" "4" " " "02C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "X" "4" "25" "1" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "9" "4" "26" "2" "04C-MID" " " RETURNING RESU.
       CALL "SD_From" USING
            "05C-MID" BY REFERENCE W-SNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "4" "28" "2" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "Z9" "4" "30" "2" "06C-MID" " " RETURNING RESU.
       CALL "SD_From" USING
            "07C-MID" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "N" "4" "32" "2" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "N" "4" "35" "2" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "4" "38" "1" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "11C-MID" "9" "4" "39" "2" "10C-MID" " " RETURNING RESU.
       CALL "SD_From" USING
            "11C-MID" BY REFERENCE W-ENEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "12C-MID" "N" "4" "41" "2" "11C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "13C-MID" "Z9" "4" "43" "2" "12C-MID" " " RETURNING RESU.
       CALL "SD_From" USING
            "13C-MID" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "14C-MID" "N" "4" "45" "2" "13C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "15C-MID" "N" "4" "47" "4" "14C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "16C-MID" " " "7" "0" "13" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "17C-MID" "N" "7" "24" "2" " " "16C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "18C-MID" "N" "7" "44" "2" "17C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "19C-MID" "N" "7" "60" "4" "18C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "20C-MID" "X" "7" "64" "5" "19C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "21C-MID" " " "8" "0" "4" "16C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "22C-MID" "N" "8" "24" "2" " " "21C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "23C-MID" "N" "8" "44" "2" "22C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "24C-MID" " " "9" "0" "18" "21C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "25C-MID" "N" "9" "24" "2" " " "24C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "26C-MID" "N" "9" "44" "2" "25C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "27C-MID" "N" "9" "60" "8" "26C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "28C-MID" "X" "9" "68" "6" "27C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "29C-MID" " " "10" "0" "4" "24C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "30C-MID" "N" "10" "24" "2" " " "29C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "31C-MID" "N" "10" "44" "2" "30C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "32C-MID" " " "11" "0" "4" "29C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "33C-MID" "N" "11" "24" "2" " " "32C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "34C-MID" "N" "11" "44" "2" "33C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "35C-MID" " " "12" "0" "4" "32C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "36C-MID" "N" "12" "24" "2" " " "35C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "37C-MID" "N" "12" "44" "2" "36C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "38C-MID" " " "13" "0" "4" "35C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "39C-MID" "N" "13" "24" "2" " " "38C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "40C-MID" "N" "13" "44" "2" "39C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "41C-MID" " " "14" "0" "4" "38C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "42C-MID" "N" "14" "24" "2" " " "41C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "43C-MID" "N" "14" "44" "2" "42C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "44C-MID" " " "15" "0" "4" "41C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "45C-MID" "N" "15" "24" "2" " " "44C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "46C-MID" "N" "15" "44" "2" "45C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "47C-MID" " " "16" "0" "4" "44C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "48C-MID" "N" "16" "24" "2" " " "47C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "49C-MID" "N" "16" "44" "2" "48C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "50C-MID" " " "17" "0" "4" "47C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "51C-MID" "N" "17" "24" "2" " " "50C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "52C-MID" "N" "17" "44" "2" "51C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "53C-MID" " " "18" "0" "4" "50C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "54C-MID" "N" "18" "24" "2" " " "53C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "55C-MID" "N" "18" "44" "2" "54C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "56C-MID" " " "19" "0" "4" "53C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "57C-MID" "N" "19" "24" "2" " " "56C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "58C-MID" "N" "19" "44" "2" "57C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "59C-MID" " " "20" "0" "4" "56C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "60C-MID" "N" "20" "24" "2" " " "59C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "61C-MID" "N" "20" "44" "2" "60C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "62C-MID" " " "21" "0" "4" "59C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "63C-MID" "N" "21" "24" "2" " " "62C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "64C-MID" "N" "21" "44" "2" "63C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "65C-MID" "X" "23" "40" "22" "62C-MID" " " RETURNING RESU.
      *C-MID0
       CALL "SD_Init" USING
            "C-MID0" " " "0" "0" "22" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID0" "N" "1" "20" "10" " " "C-MID0" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID0" " " "6" "0" "12" "01C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID0" "N" "6" "22" "6" " " "02C-MID0" RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID0" "N" "6" "42" "6" "03C-MID0" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING
            "C-MID1" " " "0" "0" "22" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID1" "N" "1" "20" "10" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID1" " " "6" "0" "12" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID1" "N" "6" "22" "6" " " "02C-MID1" RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID1" "N" "6" "42" "6" "03C-MID1" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "21" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "W-L" "0" "21" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-TCDF" "9" "W-L" "W-C1" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-TCDF" BY REFERENCE W-TCDF(1) "4" "1" BY REFERENCE CNT 8
             RETURNING RESU.
       CALL "SD_Init" USING
            "A-TCDT" "9" "W-L" "W-C2" "4" "A-TCDF" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-TCDT" BY REFERENCE W-TCDT(1) "4" "1" BY REFERENCE CNT 8
             RETURNING RESU.
       CALL "SD_Init" USING
            "A-HCDF" "9" "W-L" "W-C3" "6" "A-TCDT" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-HCDF" BY REFERENCE W-HCDF(1) "6" "1" BY REFERENCE CNT 12
             RETURNING RESU.
       CALL "SD_Init" USING
            "A-HCDT" "9" "W-L" "W-C4" "6" "A-HCDF" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-HCDT" BY REFERENCE W-HCDT(1) "6" "1" BY REFERENCE CNT 12
             RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "23" "57" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "20" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-DSP" " " "W-L" "0" "20" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "S-TCDF" "X" "W-L" "W-C1" "4" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "S-TCDT" "X" "W-L" "W-C2" "4" "S-TCDF" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-HCDF" "X" "W-L" "W-C3" "6" "S-TCDT" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-HCDT" "X" "W-L" "W-C4" "6" "S-HCDF" " " RETURNING RESU.
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
           ACCEPT W-JS FROM ARGUMENT-VALUE.
           ACCEPT W-JS2 FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  W-JS > 2
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  W-JS2 > 2
               CALL "DB_Close"
               STOP RUN
           END-IF.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           MOVE D-SSNG TO W-SNG.
           MOVE D-ESNG TO W-ENG.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO NSSW-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NSSW-F_PNAME1 " " BY REFERENCE NSSW-F_IDLST "0".
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                              RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p"
                                              RETURNING RESU.
           MOVE 0 TO W-EC.
           IF  JS-SIGN = 0
               CALL "SD_Output" USING "C-MID0" C-MID0 "p"
                                              RETURNING RESU
               MOVE "得意先品名" TO H-MID
               MOVE SPACE TO H-OM1 H-UM1
               MOVE "ｺｰﾄﾞ" TO H-TCD1 H-HCD1
               MOVE "得　意　先　名　" TO H-TNA1
               MOVE "品　　　　　名　" TO H-HNA1
               MOVE 19 TO W-C1
               CALL "SD_Arg_Match_Col" USING
                "W-C1" "2" W-C1 RETURNING RESU
               MOVE 27 TO W-C2
               CALL "SD_Arg_Match_Col" USING
                "W-C2" "2" W-C2 RETURNING RESU
               MOVE 37 TO W-C3
               CALL "SD_Arg_Match_Col" USING
                "W-C3" "2" W-C3 RETURNING RESU
               MOVE 47 TO W-C4
               CALL "SD_Arg_Match_Col" USING
                "W-C4" "2" W-C4 RETURNING RESU
               PERFORM CR0-RTN THRU CR0-EX
           ELSE
               CALL "SD_Output" USING "C-MID1" C-MID1 "p"
                                              RETURNING RESU
               MOVE "品名得意先" TO H-MID
               MOVE SPACE TO H-OM2 H-UM2
               MOVE "ｺｰﾄﾞ" TO H-TCD2 H-HCD2
               MOVE "得　意　先　名　" TO H-TNA2
               MOVE "品　　　　　名　" TO H-HNA2
               MOVE 39 TO W-C1
               CALL "SD_Arg_Match_Col" USING
                "W-C1" "2" W-C1 RETURNING RESU
               MOVE 47 TO W-C2
               CALL "SD_Arg_Match_Col" USING
                "W-C2" "2" W-C2 RETURNING RESU
               MOVE 17 TO W-C3
               CALL "SD_Arg_Match_Col" USING
                "W-C3" "2" W-C3 RETURNING RESU
               MOVE 27 TO W-C4
               CALL "SD_Arg_Match_Col" USING
                "W-C4" "2" W-C4 RETURNING RESU
               PERFORM CR1-RTN THRU CR1-EX
           END-IF.
           IF  W-EC NOT = 0
               CALL "DB_F_Close" USING
                BY REFERENCE NSSW-F_IDLST NSSW-F_PNAME1
               GO TO M-95
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-10.
      *           READ NSSW-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" NSSW-F_PNAME1 BY REFERENCE NSSW-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                              RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                              RETURNING RESU
               GO TO M-90
           END-IF.
           IF  ZERO = NSS-SU(01) AND NSS-SU(02) AND NSS-SU(03) AND
                     NSS-SU(04) AND NSS-SU(05) AND NSS-SU(06) AND
                     NSS-SU(07) AND NSS-SU(08) AND NSS-SU(09) AND
                     NSS-SU(10)
               GO TO M-10
           END-IF.
           PERFORM CHK-RTN THRU CHK-EX.
           IF  CHK = ZERO
               GO TO M-10
           END-IF.
           MOVE W-SNEN TO H-SNEN.
           MOVE W-SGET TO H-SGET.
           MOVE W-ENEN TO H-ENEN.
           MOVE W-EGET TO H-EGET.
           IF  W-JS2 = 1
               MOVE "（出荷）" TO H-KBN
           END-IF.
           IF  W-JS2 = 2
               MOVE "（返品）" TO H-KBN
           END-IF.
           MOVE 1 TO W-POC.
           CALL "PR_Open" RETURNING RESP.
           PERFORM MID-010 THRU MID-EX.
           IF  JS-SIGN NOT = 0
               GO TO M-30
           END-IF.
       M-15.
           MOVE NSS-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "マスター　なし　" TO T-NAME
           END-IF.
           MOVE T-NAME TO W-TNA.
           MOVE ZERO TO WT-ZCD WT-ASUD W-DC W-TC.
       M-20.
           MOVE NSS-HCD TO W-HCD.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE ZERO TO HI-HSD
               MOVE "マスター　なし　" TO HI-NAME
           END-IF.
           MOVE HI-NAME TO W-HNA.
           MOVE ZERO TO W-HSD.
           MOVE HI-HSD TO W-HSD.
           MOVE ZERO TO W-ZCD W-ASUD.
           GO TO M-40.
       M-30.
           MOVE NSS-HCD TO W-HCD.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE ZERO TO HI-HSD
               MOVE "マスター　なし　" TO HI-NAME
           END-IF.
           MOVE HI-NAME TO W-HNA.
           MOVE ZERO TO W-HSD.
           MOVE HI-HSD TO W-HSD.
           MOVE ZERO TO WT-ZCD WT-ASUD W-DC W-TC.
       M-35.
           MOVE NSS-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "マスター　なし　" TO T-NAME
           END-IF.
           MOVE T-NAME TO W-TNA.
           MOVE ZERO TO W-ZCD W-ASUD.
       M-40.
           PERFORM SET-RTN THRU SET-EX.
       M-45.
      *           READ NSSW-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" NSSW-F_PNAME1 BY REFERENCE NSSW-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF.
           IF  ZERO = NSS-SU(01) AND NSS-SU(02) AND NSS-SU(03) AND
                     NSS-SU(04) AND NSS-SU(05) AND NSS-SU(06) AND
                     NSS-SU(07) AND NSS-SU(08) AND NSS-SU(09) AND
                     NSS-SU(10)
               GO TO M-45
           END-IF.
           PERFORM CHK-RTN THRU CHK-EX.
           IF  CHK = ZERO
               GO TO M-45
           END-IF.
           IF  JS-SIGN NOT = 0
               GO TO M-50
           END-IF.
           IF  NSS-TCD NOT = W-TCD
               GO TO M-70
           END-IF.
           IF  NSS-HCD = W-HCD
               GO TO M-40
           END-IF.
           GO TO M-60.
       M-50.
           IF  NSS-HCD NOT = W-HCD
               GO TO M-70
           END-IF.
           IF  NSS-TCD = W-TCD
               GO TO M-40
           END-IF.
       M-60.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  JS-SIGN NOT = 0
               GO TO M-35
           ELSE
               GO TO M-20
           END-IF.
       M-70.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  W-DC = 2
               PERFORM TOT-RTN THRU TOT-EX
           ELSE
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF.
           IF  JS-SIGN NOT = 0
               GO TO M-30
           ELSE
               GO TO M-15
           END-IF.
       M-85.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  W-DC = 2
               PERFORM TOT-RTN THRU TOT-EX
           END-IF.
       M-90.
           IF  W-POC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NSSW-F_IDLST NSSW-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                              RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-010.
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
           MOVE HEAD6 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
       SET-RTN.
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
       SET-010.
           ADD 1 TO CNTD.
           IF  CNTD NOT = 11
               MOVE NSS-SU(CNTD) TO W-SU(W-SC,CNTD)
               ADD NSS-SU(CNTD) TO WT-SU(W-SC,CNTD)
               GO TO SET-010
           END-IF.
       SET-EX.
           EXIT.
       CHK-RTN.
           MOVE ZERO TO CNT CHK.
       CHK-010.
           ADD 1 TO CNT.
           IF  CNT = 16
               GO TO CHK-EX
           END-IF.
           IF  W-TCDT(CNT) NOT = ZERO
               IF  NSS-TCD >= W-TCDF(CNT) AND <= W-TCDT(CNT)
                   GO TO CHK-020
               END-IF
           END-IF.
           GO TO CHK-010.
       CHK-020.
           MOVE ZERO TO CNT.
       CHK-030.
           ADD 1 TO CNT.
           IF  CNT = 16
               GO TO CHK-EX
           END-IF.
           IF  W-HCDT(CNT) NOT = ZERO
               IF  NSS-HCD >= W-HCDF(CNT) AND <= W-HCDT(CNT)
                   GO TO CHK-050
               END-IF
           END-IF.
           GO TO CHK-030.
       CHK-050.
           IF  W-JS = 0
               MOVE 1 TO CHK
               GO TO CHK-EX
           END-IF.
           IF  W-JS = 2
               GO TO CHK-080
           END-IF.
           IF  JS-SIGN NOT = 0
               GO TO CHK-060
           END-IF.
           MOVE NSS-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO T-TNC
           END-IF.
           IF  T-TNC = 13
               MOVE 1 TO CHK
           END-IF.
           GO TO CHK-EX.
       CHK-060.
           IF  JS-SIGN NOT = 1
               MOVE 1 TO CHK
               GO TO CHK-EX
           END-IF.
           MOVE NSS-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO HI-BC1
           END-IF.
           IF  HI-BC1 = 26
               MOVE 1 TO CHK
           END-IF.
           GO TO CHK-EX.
       CHK-080.
           MOVE NSS-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO HI-HPV
           END-IF.
           IF  HI-HPV = 1
               MOVE 1 TO CHK
           END-IF.
       CHK-EX.
           EXIT.
       TMP-RTN.
           MOVE SPACE TO W-P1.
           IF  JS-SIGN = 0
               MOVE SPACE TO P-TNA1
               MOVE W-TCD TO P-TCD1
               MOVE W-TNA TO P-TNA1
               CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
               IF  LINAGECOUNTER > 59
                   MOVE W-TCD TO P-TCD1
                   MOVE W-TNA TO P-TNA1
                   PERFORM MID-RTN THRU MID-EX
               END-IF
           END-IF.
           IF  JS-SIGN = 1
               MOVE SPACE TO P-HNA2
               MOVE W-HCD TO P-HCD2
               MOVE W-HNA TO P-HNA2
               CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
               IF  LINAGECOUNTER > 59
                   MOVE W-HCD TO P-HCD2
                   MOVE W-HNA TO P-HNA2
                   PERFORM MID-RTN THRU MID-EX
               END-IF
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE 1 TO W-TC.
       TMP-EX.
           EXIT.
       MEI-RTN.
           IF  W-DC = 1
               MOVE 2 TO W-DC
           END-IF.
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF.
           MOVE ZERO TO W-SC W-NC.
       MEI-010.
           ADD 1 TO W-SC.
           IF  W-SC = 5
               GO TO MEI-EX
           END-IF.
           IF  W-ZC(W-SC) = 0
               GO TO MEI-010
           END-IF.
           IF  W-TC = 0
               PERFORM TMP-RTN THRU TMP-EX
           END-IF.
           MOVE ZERO TO W-SSD.
           MOVE SPACE TO W-P2.
           IF  JS-SIGN = 0
               MOVE SPACE TO P-HNA1
           ELSE
               MOVE SPACE TO P-TNA2
           END-IF.
           IF  W-NC = 0
               MOVE 9 TO W-NC
               IF  JS-SIGN = 0
                   MOVE W-HCD TO P-HCD1
                   MOVE W-HNA TO P-HNA1
               ELSE
                   MOVE W-TCD TO P-TCD2
                   MOVE W-TNA TO P-TNA2
               END-IF
           END-IF.
           IF  W-SC = 1
               MOVE W-SS2 TO W-SSD
               MOVE 2 TO P-SIZ
               IF  0 = W-ZC(2) AND W-ZC(3) AND W-ZC(4)
                   MOVE W-SUT TO P-SUT
               END-IF
           END-IF.
           IF  W-SC = 2
               MOVE W-SS3 TO W-SSD
               MOVE 3 TO P-SIZ
               IF  0 = W-ZC(3) AND W-ZC(4)
                   MOVE W-SUT TO P-SUT
               END-IF
           END-IF.
           IF  W-SC = 3
               MOVE W-SS4 TO W-SSD
               MOVE 0 TO W-SS(10)
               MOVE 4 TO P-SIZ
               IF  0 = W-ZC(4)
                   MOVE W-SUT TO P-SUT
               END-IF
           END-IF.
           IF  W-SC = 4
               MOVE W-SS1 TO W-SSD
               MOVE 1 TO P-SIZ
               MOVE W-SUT TO P-SUT
           END-IF.
           MOVE ZERO TO CNTD.
       MEI-020.
           ADD 1 TO CNTD.
           IF  CNTD = 11
               GO TO MEI-030
           END-IF.
           IF  W-SS(CNTD) NOT = 0
               MOVE W-SU(W-SC,CNTD) TO P-SU(CNTD)
           END-IF.
           GO TO MEI-020.
       MEI-030.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER NOT > 61
               GO TO MEI-040
           END-IF.
           IF  JS-SIGN = 0
               MOVE W-HCD TO P-HCD1
               MOVE W-HNA TO P-HNA1
           ELSE
               MOVE W-TCD TO P-TCD2
               MOVE W-TNA TO P-TNA2
           END-IF.
           PERFORM MID-RTN THRU MID-EX.
           PERFORM TMP-RTN THRU TMP-EX.
       MEI-040.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO MEI-010.
       MEI-EX.
           EXIT.
       TOT-RTN.
           MOVE ZERO TO W-SC W-NC.
       TOT-010.
           ADD 1 TO W-SC.
           IF  W-SC = 5
               GO TO TOT-EX
           END-IF.
           IF  WT-ZC(W-SC) = 0
               GO TO TOT-010
           END-IF.
           MOVE SPACE TO W-P2.
           IF  JS-SIGN = 0
               MOVE SPACE TO P-HNA1
           ELSE
               MOVE SPACE TO P-TNA2
           END-IF.
           IF  W-NC = 0
               MOVE 9 TO W-NC
               IF  JS-SIGN = 0
                   MOVE "　　　　　（　合　計　）　" TO P-HNA1
               ELSE
                   MOVE "　　　　　（　合　計　）　" TO P-TNA2
               END-IF
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
       TOT-020.
           ADD 1 TO CNTD.
           IF  CNTD = 11
               GO TO TOT-030
           END-IF.
           IF  W-SC = 2
               IF  CNTD > 9
                   GO TO TOT-020
               END-IF
           END-IF.
           IF  W-SC = 3
               IF  CNTD > 8
                   GO TO TOT-020
               END-IF
           END-IF.
           MOVE WT-SU(W-SC,CNTD) TO P-SU(CNTD).
           GO TO TOT-020.
       TOT-030.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER NOT > 61
               GO TO TOT-040
           END-IF.
           IF  JS-SIGN = 0
               MOVE "　　　　　（　合　計　）　" TO P-HNA1
           ELSE
               MOVE "　　　　　（　合　計　）　" TO P-TNA2
           END-IF.
           PERFORM MID-RTN THRU MID-EX.
       TOT-040.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO TOT-010.
       TOT-EX.
           EXIT.
       CR0-RTN.
           MOVE ZERO TO CNT W-AC.
           MOVE 6 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       CR0-020.
           ADD 1 TO CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT > 15
               GO TO CR0-120
           END-IF.
       CR0-040.
           IF  W-AC = 1
               MOVE ZERO TO W-TCDF(CNT)
               CALL "SD_Output" USING "S-TCDF" S-TCDF "p"
                                              RETURNING RESU
               GO TO CR0-060
           END-IF.
           CALL "SD_Accept" USING BY REFERENCE A-TCDF "A-TCDF" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               MOVE 1 TO W-EC
               GO TO CR0-EX
           END-IF.
           IF  ESTAT = ADV
               MOVE 1 TO W-AC
               MOVE ZERO TO W-TCDF(CNT)
               CALL "SD_Output" USING "S-TCDF" S-TCDF "p"
                                              RETURNING RESU
               GO TO CR0-060
           END-IF.
           IF  ESTAT = BTB
               SUBTRACT 1 FROM CNT W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               IF  CNT = ZERO
                   GO TO CR0-020
               ELSE
                   GO TO CR0-060
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO CR0-040
           END-IF.
           IF (CNT NOT = 1) AND (W-TCDF(CNT) = 9999)
               MOVE 1 TO W-AC
               MOVE ZERO TO W-TCDF(CNT)
               CALL "SD_Output" USING "S-TCDF" S-TCDF "p"
                                              RETURNING RESU
               GO TO CR0-060
           END-IF.
           IF (CNT NOT = 1) AND (W-TCDF(CNT) NOT = ZERO)
               COMPUTE CNTD = CNT - 1
               IF  W-TCDT(CNTD) >= W-TCDF(CNT)
                   GO TO CR0-040
               END-IF
           END-IF.
       CR0-060.
           IF  W-AC = 1
               MOVE ZERO TO W-TCDT(CNT)
               CALL "SD_Output" USING "S-TCDT" S-TCDT "p"
                                              RETURNING RESU
               GO TO CR0-020
           END-IF.
           CALL "SD_Accept" USING BY REFERENCE A-TCDT "A-TCDT" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO CR0-040
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO CR0-060
           END-IF.
           IF  W-TCDF(CNT) > W-TCDT(CNT)
               GO TO CR0-060
           END-IF.
           IF  CNT = 1
               IF  W-TCDT(1) = ZERO
                   GO TO CR0-060
               END-IF
           END-IF.
           GO TO CR0-020.
       CR0-080.
           MOVE 16 TO CNT.
           MOVE 22 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE ZERO TO W-AC.
       CR0-100.
           SUBTRACT 1 FROM CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT = ZERO
               GO TO CR0-RTN
           END-IF.
           IF  W-TCDT(CNT) = ZERO
               GO TO CR0-100
           END-IF.
           GO TO CR0-060.
       CR0-120.
           MOVE ZERO TO CNT W-AC.
           MOVE 6 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       CR0-140.
           ADD 1 TO CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT > 15
               GO TO CR0-300
           END-IF.
       CR0-160.
           IF  W-AC = 1
               MOVE ZERO TO W-HCDF(CNT)
               CALL "SD_Output" USING "S-HCDF" S-HCDF "p"
                                              RETURNING RESU
               GO TO CR0-180
           END-IF.
           CALL "SD_Accept" USING BY REFERENCE A-HCDF "A-HCDF" "9" "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               MOVE 1 TO W-EC
               GO TO CR0-EX
           END-IF.
           IF  ESTAT = ADV
               MOVE 1 TO W-AC
               MOVE ZERO TO W-HCDF(CNT)
               CALL "SD_Output" USING "S-HCDF" S-HCDF "p"
                                              RETURNING RESU
               GO TO CR0-180
           END-IF.
           IF  ESTAT = BTB
               SUBTRACT 1 FROM CNT W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               IF  CNT = ZERO
                   GO TO CR0-020
               ELSE
                   GO TO CR0-180
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO CR0-160
           END-IF.
           IF (CNT NOT = 1) AND (W-HCDF(CNT) = 999999)
               MOVE 1 TO W-AC
               MOVE ZERO TO W-HCDF(CNT)
               CALL "SD_Output" USING "S-HCDF" S-HCDF "p"
                                              RETURNING RESU
               GO TO CR0-180
           END-IF.
           IF (CNT NOT = 1) AND (W-HCDF(CNT) NOT = ZERO)
               COMPUTE CNTD = CNT - 1
               IF  W-HCDT(CNTD) >= W-HCDF(CNT)
                   GO TO CR0-160
               END-IF
           END-IF.
       CR0-180.
           IF  W-AC = 1
               MOVE ZERO TO W-HCDT(CNT)
               CALL "SD_Output" USING "S-HCDT" S-HCDT "p"
                                              RETURNING RESU
               GO TO CR0-140
           END-IF.
           CALL "SD_Accept" USING BY REFERENCE A-HCDT "A-HCDT" "9" "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO CR0-160
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO CR0-180
           END-IF.
           IF  W-HCDF(CNT) > W-HCDT(CNT)
               GO TO CR0-180
           END-IF.
           IF  CNT = 1
               IF  W-HCDT(1) = ZERO
                   GO TO CR0-180
               END-IF
           END-IF.
           GO TO CR0-140.
       CR0-300.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF ESTAT NOT = BTB
               GO TO CR0-340
           END-IF.
           MOVE ZERO TO W-AC.
       CR0-320.
           SUBTRACT 1 FROM CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU
           IF  CNT = ZERO
               GO TO CR0-120
           END-IF.
           IF  W-HCDT(CNT) = ZERO
               GO TO CR0-320
           END-IF.
           GO TO CR0-180.
       CR0-340.
           IF  ESTAT NOT = HTB AND SKP
               GO TO CR0-300
           END-IF.
           IF  W-DMM = 9
               GO TO CR0-RTN
           END-IF.
           IF  W-DMM NOT = 1
               GO TO CR0-300
           END-IF.
       CR0-EX.
           EXIT.
       CR1-RTN.
           MOVE ZERO TO CNT W-AC.
           MOVE 6 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       CR1-020.
           ADD 1 TO CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT > 15
               GO TO CR1-120
           END-IF.
       CR1-040.
           IF  W-AC = 1
               MOVE ZERO TO W-HCDF(CNT)
               CALL "SD_Output" USING "S-HCDF" S-HCDF "p"
                                              RETURNING RESU
               GO TO CR1-060
           END-IF.
           CALL "SD_Accept" USING BY REFERENCE A-HCDF "A-HCDF" "9" "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               MOVE 1 TO W-EC
               GO TO CR1-EX
           END-IF.
           IF  ESTAT = ADV
               MOVE 1 TO W-AC
               MOVE ZERO TO W-HCDF(CNT)
               CALL "SD_Output" USING "S-HCDF" S-HCDF "p"
                                              RETURNING RESU
               GO TO CR1-060
           END-IF.
           IF  ESTAT = BTB
               SUBTRACT 1 FROM CNT W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               IF  CNT = ZERO
                   GO TO CR1-020
               ELSE
                   GO TO CR1-060
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO CR1-040
           END-IF.
           IF (CNT NOT = 1) AND (W-HCDF(CNT) = 999999)
               MOVE 1 TO W-AC
               MOVE ZERO TO W-HCDF(CNT)
               CALL "SD_Output" USING "S-HCDF" S-HCDF "p"
                                              RETURNING RESU
               GO TO CR1-060
           END-IF.
           IF (CNT NOT = 1) AND (W-HCDF(CNT) NOT = ZERO)
               COMPUTE CNTD = CNT - 1
               IF  W-HCDT(CNTD) >= W-HCDF(CNT)
                   GO TO CR1-040
               END-IF
           END-IF.
       CR1-060.
           IF  W-AC = 1
               MOVE ZERO TO W-HCDT(CNT)
               CALL "SD_Output" USING "S-HCDT" S-HCDT "p"
                                              RETURNING RESU
               GO TO CR1-020
           END-IF.
           CALL "SD_Accept" USING BY REFERENCE A-HCDT "A-HCDT" "9" "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO CR1-040
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO CR1-060
           END-IF.
           IF  W-HCDF(CNT) > W-HCDT(CNT)
               GO TO CR1-060
           END-IF.
           IF  CNT = 1
               IF  W-HCDT(1) = ZERO
                   GO TO CR1-060
               END-IF
           END-IF.
           GO TO CR1-020.
       CR1-080.
           MOVE 16 TO CNT.
           MOVE 22 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE ZERO TO W-AC.
       CR1-100.
           SUBTRACT 1 FROM CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT = ZERO
               GO TO CR1-RTN
           END-IF.
           IF  W-HCDT(CNT) = ZERO
               GO TO CR1-100
           END-IF.
           GO TO CR1-060.
       CR1-120.
           MOVE ZERO TO CNT W-AC.
           MOVE 6 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       CR1-140.
           ADD 1 TO CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT > 15
               GO TO CR1-300
           END-IF.
       CR1-160.
           IF  W-AC = 1
               MOVE ZERO TO W-TCDF(CNT)
               CALL "SD_Output" USING "S-TCDF" S-TCDF "p"
                                              RETURNING RESU
               GO TO CR1-180
           END-IF.
           CALL "SD_Accept" USING BY REFERENCE A-TCDF "A-TCDF" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               MOVE 1 TO W-EC
               GO TO CR1-EX
           END-IF.
           IF  ESTAT = ADV
               MOVE 1 TO W-AC
               MOVE ZERO TO W-TCDF(CNT)
               CALL "SD_Output" USING "S-TCDF" S-TCDF "p"
                                              RETURNING RESU
               GO TO CR1-180
           END-IF.
           IF  ESTAT = BTB
               SUBTRACT 1 FROM CNT W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               IF  CNT = ZERO
                   GO TO CR1-020
               ELSE
                   GO TO CR1-180
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO CR1-160.
           IF (CNT NOT = 1) AND (W-TCDF(CNT) = 9999)
               MOVE 1 TO W-AC
               MOVE ZERO TO W-TCDF(CNT)
               CALL "SD_Output" USING "S-TCDF" S-TCDF "p"
                                              RETURNING RESU
               GO TO CR1-180
           END-IF.
           IF (CNT NOT = 1) AND (W-TCDF(CNT) NOT = ZERO)
               COMPUTE CNTD = CNT - 1
               IF  W-TCDT(CNTD) >= W-TCDF(CNT)
                   GO TO CR1-160
               END-IF
           END-IF.
       CR1-180.
           IF  W-AC = 1
               MOVE ZERO TO W-TCDT(CNT)
               CALL "SD_Output" USING "S-TCDT" S-TCDT "p"
                                              RETURNING RESU
               GO TO CR1-140
           END-IF.
           CALL "SD_Accept" USING BY REFERENCE A-TCDT "A-TCDT" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO CR1-160
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO CR1-180
           END-IF.
           IF  W-TCDF(CNT) > W-TCDT(CNT)
               GO TO CR1-180
           END-IF.
           IF  CNT = 1
               IF  W-TCDT(1) = ZERO
                   GO TO CR1-180
               END-IF
           END-IF.
           GO TO CR1-140.
       CR1-300.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = BTB
               GO TO CR1-340
           END-IF.
           MOVE ZERO TO W-AC.
       CR1-320.
           SUBTRACT 1 FROM CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT = ZERO
               GO TO CR1-120
           END-IF.
           IF  W-TCDT(CNT) = ZERO
               GO TO CR1-320
           END-IF.
           GO TO CR1-180.
       CR1-340.
           IF  ESTAT NOT = HTB AND SKP
               GO TO CR1-300
           END-IF.
           IF  W-DMM = 9
               GO TO CR1-RTN
           END-IF.
           IF  W-DMM NOT = 1
               GO TO CR1-300
           END-IF.
       CR1-EX.
           EXIT.

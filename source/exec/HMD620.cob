       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD620.
      *********************************************************
      *    PROGRAM         :  履物在庫明細表（応用用紙）　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0 = 分類品名別在庫明細表        *
      *                    :  1 = 品名別        〃            *
      *                    :  2 = 分類品名別棚卸明細表        *
      *                    :  3 = 倉庫別   〃                 *
      *                    :  6 = 分類品名別在庫明細ＥＸＣＥＬ*
      *                    :  7 = 品名別        〃            *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  H-SON          PIC  N(006) VALUE SPACE.
           02  F              PIC  X(040) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-ZTM          PIC  N(003) VALUE SPACE.
           02  F              PIC  N(011) VALUE
                "　明　細　表　　＊＊＊".
           02  F              PIC  X(028) VALUE SPACE.
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(001) VALUE "1".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "３号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "２号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "１号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "０号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　中".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　大".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "特大".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "28.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "29.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "30.0".
           02  F              PIC  X(021) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(044) VALUE SPACE.
           02  F              PIC  X(001) VALUE "2".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "12.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "14.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "15.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "16.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "17.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "18.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "19.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "20.0".
           02  F              PIC  X(021) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(044) VALUE SPACE.
           02  F              PIC  X(001) VALUE "3".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "    ".
           02  F              PIC  X(021) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(044) VALUE SPACE.
           02  F              PIC  X(001) VALUE "4".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "    ".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "    ".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "合　　計".
           02  F              PIC  X(011) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(116) VALUE SPACE.
           02  F              PIC  X(001) VALUE "(".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　預り分".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　実在庫".
           02  F              PIC  X(001) VALUE ")".
       01  W-P.
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  Z(001).
           02  P-ZSU.
             03  P-ZS         PIC  ---,---  OCCURS  10 TIMES.
           02  P-TD.
             03  P-ZSK        PIC --,---,--9.
             03  F            PIC  X(011).
           02  P-TDD   REDEFINES P-TD.
             03  F            PIC  X(001).
             03  P-F          PIC  X(001).
             03  P-AZ         PIC ----,--9.
             03  P-GZ         PIC --,---,--9.
             03  P-R          PIC  X(001).
       01  W-DATA.
           02  W-SOC          PIC  9(001).
           02  W-SESO.
             03  W-SSOC       PIC  9(001).
             03  W-ESOC       PIC  9(001) VALUE 9.
           02  W-BC3          PIC  9(002).
           02  W-BCD1         PIC  9(003).
           02  W-BMNO         PIC  9(001).
           02  W-HCD          PIC  9(006).
           02  CHK            PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-UC           PIC  9(001) VALUE ZERO.
           02  W-POC          PIC  9(001) VALUE 0.
           02  W-SEBC.
             03  W-SBC3       PIC  9(002).
             03  W-EBC3       PIC  9(002) VALUE 99.
             03  W-SBMNO      PIC  9(001).
             03  W-EBMNO      PIC  9(001) VALUE 9.
             03  W-SBCD1      PIC  9(003).
             03  W-EBCD1      PIC  9(003) VALUE 999.
           02  W-SEHCD.
             03  W-SHCD       PIC  9(006).
             03  W-EHCD       PIC  9(006) VALUE 999999.
           02  W-DMM          PIC  9(001).
       01  WN-D.
           02  WN-ZSK         PIC S9(007).
           02  WN-AZ          PIC S9(006).
           02  WN-GZ          PIC S9(007).
       01  WS-D.
           02  WS-ZSK         PIC S9(007).
           02  WS-AZ          PIC S9(006).
           02  WS-GZ          PIC S9(007).
       01  WG-D.
           02  WG-ZSK         PIC S9(007).
           02  WG-AZ          PIC S9(006).
           02  WG-GZ          PIC S9(007).
       01  WA-D.
           02  WA-ZSK         PIC S9(007).
           02  WA-AZ          PIC S9(006).
           02  WA-GZ          PIC S9(007).
       01  WT-D.
           02  WT-ZSK         PIC S9(007).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY L-JCON.
           COPY LSPF.
      *FD  HZW-F
       01  HZW-F_HMD620.
           02  HZW-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HZW-F_LNAME    PIC  X(012) VALUE "HZW-F_HMD620".
           02  F              PIC  X(001).
           02  HZW-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HZW-F_SORT     PIC  X(100) VALUE SPACE.
           02  HZW-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HZW-F_RES      USAGE  POINTER.
       01  HZW-R.
           02  HZW-KEY.
             03  HZW-HCD      PIC  9(006).
             03  HZW-SIZ      PIC  9(001).
           02  HZW-AZS.
             03  HZW-ZSD   OCCURS  10.
               04  HZW-ZS     PIC S9(006).
           02  HZW-TSU.
             03  HZW-ZST      PIC S9(006).
             03  HZW-AS       PIC S9(006).
             03  HZW-GZS      PIC S9(006).
             03  HZW-TC       PIC  9(001).
           02  HZW-BCD1.
             03  HZW-BC1      PIC  9(002).
             03  HZW-BC21     PIC  9(001).
           02  HZW-BC22       PIC  9(001).
           02  HZW-BC3        PIC  9(002).
           02  HZW-BMC        PIC  9(002).
           02  HZW-BMNO       PIC  9(001).
           02  HZW-NO         PIC  9(001).
           02  HZW-SOC        PIC  9(001).
           02  F              PIC  X(031).
       77  F                  PIC  X(001).
      *FD  EXL-F
       01  EXL-F_HMD620.
           02  EXL-F_PNAME1   PIC  X(009) VALUE "WK0256000".
           02  F              PIC  X(001).
           02  EXL-F_LNAME    PIC  X(012) VALUE "EXL-F_HMD620".
           02  F              PIC  X(001).
           02  EXL-F_KEY1     PIC  X(100) VALUE SPACE.
           02  EXL-F_SORT     PIC  X(100) VALUE SPACE.
           02  EXL-F_IDLST    PIC  X(100) VALUE SPACE.
           02  EXL-F_RES      USAGE  POINTER.
       01  EXL-R.
           02  EXL-HCD        PIC  X(006).
           02  EXL-NAME       PIC  N(024).
           02  EXL-SIZ        PIC  9(001).
           02  EXL-ZSU.
             03  EXL-ZS       PIC S9(006)  OCCURS  10 TIMES.
           02  EXL-ZSK        PIC S9(007).
           02  EXL-AZ         PIC S9(007).
           02  EXL-GZ         PIC S9(007).
           02  F              PIC  X(120).
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
           02  FILLER  PIC N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊　　履物在庫明細表　（応用用紙）　　＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SHCD  PIC  9(006).
             03  A-EHCD  PIC  9(006).
             03  A-SBC3  PIC  9(002).
             03  A-EBC3  PIC  9(002).
           02  FILLER.
             03  A-SBMNO PIC  9(001).
             03  A-EBMNO PIC  9(001).
           02  FILLER.
             03  A-SBCD1 PIC  9(003).
             03  A-EBCD1 PIC  9(003).
           02  FILLER.
             03  A-SSOC  PIC  9(001).
             03  A-ESOC  PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-BMSG.
             03  FILLER  PIC  X(049) VALUE
                  "分類3  00 ～ 99   (一　般=10,ワーク=20,教　育=30)".
             03  FILLER.
               04  FILLER  PIC  X(018) VALUE
                    "部門    0 ～ 9    ".
               04  FILLER  PIC  X(039) VALUE
                    "(国内成型=1,上海成型=2,ワーク=3,教育=4)".
             03  FILLER  PIC  X(016) VALUE
                  "分類1 000 ～ 999".
           02  D-HMSG.
             03  FILLER  PIC  N(008) VALUE
                  "＜品名コード順＞".
             03  FILLER  PIC  X(052) VALUE
                 "<  品名ｺｰﾄﾞ 000000 ～ 999999 迄打出し  >   終了=ｆ･9".
           02  D-TANA  PIC  N(008) VALUE
                "｛　棚　　卸　｝".
           02  D-KURA.
             03  FILLER  PIC  N(024) VALUE
                  "＊＊＊　　倉別棚卸明細表　（応用用紙）　　＊＊＊".
             03  FILLER  PIC  X(044) VALUE
                  "<  倉庫ｺｰﾄﾞ  0  ～  9  迄打出し  >   終了=F9".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "358" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "48" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "48" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "48" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "48" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "48" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "20" "23" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "27" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "14" "0" "16" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SHCD" "9" "14" "28" "6" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SHCD" BY REFERENCE W-SHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EHCD" "9" "14" "38" "6" "A-SHCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EHCD" BY REFERENCE W-EHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SBC3" "9" "14" "26" "2" "A-EHCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-SBC3" BY REFERENCE W-SBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EBC3" "9" "14" "32" "2" "A-SBC3" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EBC3" BY REFERENCE W-EBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-ACP" " " "15" "0" "2" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SBMNO" "9" "15" "27" "1" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SBMNO" BY REFERENCE W-SBMNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EBMNO" "9" "15" "32" "1" "A-SBMNO" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EBMNO" BY REFERENCE W-EBMNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03C-ACP" " " "16" "0" "6" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SBCD1" "9" "16" "25" "3" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SBCD1" BY REFERENCE W-SBCD1 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EBCD1" "9" "16" "32" "3" "A-SBCD1" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EBCD1" BY REFERENCE W-EBCD1 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04C-ACP" " " "17" "0" "2" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SSOC" "9" "17" "29" "1" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SSOC" BY REFERENCE W-SSOC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ESOC" "9" "17" "36" "1" "A-SSOC" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-ESOC" BY REFERENCE W-ESOC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "40" "1" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "298" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-BMSG" " " "0" "0" "122" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-BMSG" "X" "14" "19" "49" " " "D-BMSG" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-BMSG" " " "15" "0" "57" "01D-BMSG" " " RETURNING RESU.
       CALL "SD_Init" USING
            "0102D-BMSG" "X" "15" "19" "18" " " "02D-BMSG"
            RETURNING RESU.
       CALL "SD_Init" USING
            "0202D-BMSG" "X" "15" "37" "39" "0102D-BMSG" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03D-BMSG" "X" "16" "19" "16" "02D-BMSG" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-HMSG" " " "0" "0" "68" "D-BMSG" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-HMSG" "N" "7" "26" "16" " " "D-HMSG" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-HMSG" "X" "14" "16" "52" "01D-HMSG" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-TANA" "N" "7" "26" "16" "D-HMSG" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-KURA" " " "0" "0" "92" "D-TANA" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-KURA" "N" "6" "10" "48" " " "D-KURA" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-KURA" "X" "17" "16" "44" "01D-KURA" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "28" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "28" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 0 AND 1 AND 2 AND 3 AND 6 AND 7
               CALL "DB_Close"
               STOP RUN
           END-IF
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 0 OR 6
               MOVE "在　庫" TO H-ZTM
               CALL "SD_Output" USING
                "D-BMSG" D-BMSG "p" RETURNING RESU
           END-IF
           IF  JS-SIGN = 1 OR 7
               MOVE "在　庫" TO H-ZTM
               CALL "SD_Output" USING
                "D-HMSG" D-HMSG "p" RETURNING RESU
           END-IF
           IF  JS-SIGN = 2
               MOVE "棚　卸" TO H-ZTM
               CALL "SD_Output" USING
                "D-TANA" D-TANA "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-BMSG" D-BMSG "p" RETURNING RESU
           END-IF
           IF  JS-SIGN = 3
               MOVE "棚　卸" TO H-ZTM
               CALL "SD_Output" USING
                "D-KURA" D-KURA "p" RETURNING RESU
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO HZW-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HZW-F_PNAME1 " " BY REFERENCE HZW-F_IDLST "0".
           MOVE ZERO TO WA-D.
       M-040.
           IF  JS-SIGN = 0 OR 2 OR 6
               GO TO M-060
           END-IF
           IF  JS-SIGN = 1 OR 7
               MOVE ZERO TO WA-D
               GO TO M-180
           END-IF
           IF  JS-SIGN = 3
               GO TO M-210
           END-IF.
       M-060.
           CALL "SD_Accept" USING BY REFERENCE A-SBC3 "A-SBC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               CALL "DB_F_Close" USING
                BY REFERENCE HZW-F_IDLST HZW-F_PNAME1
               GO TO M-980
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF.
       M-080.
           CALL "SD_Accept" USING BY REFERENCE A-EBC3 "A-EBC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               CALL "DB_F_Close" USING
                BY REFERENCE HZW-F_IDLST HZW-F_PNAME1
               GO TO M-980
           END-IF
           IF  ESTAT = BTB
               GO TO M-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-080
           END-IF
           IF  W-SBC3 > W-EBC3
               GO TO M-080
           END-IF.
       M-100.
           CALL "SD_Accept" USING BY REFERENCE A-SBMNO "A-SBMNO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               CALL "DB_F_Close" USING
                BY REFERENCE HZW-F_IDLST HZW-F_PNAME1
               GO TO M-980
           END-IF
           IF  ESTAT = BTB
               GO TO M-080
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF.
       M-120.
           CALL "SD_Accept" USING BY REFERENCE A-EBMNO "A-EBMNO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               CALL "DB_F_Close" USING
                BY REFERENCE HZW-F_IDLST HZW-F_PNAME1
               GO TO M-980
           END-IF
           IF  ESTAT = BTB
               GO TO M-100
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-120
           END-IF
           IF  W-SBMNO > W-EBMNO
               GO TO M-120
           END-IF.
       M-140.
           CALL "SD_Accept" USING BY REFERENCE A-SBCD1 "A-SBCD1" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               CALL "DB_F_Close" USING
                BY REFERENCE HZW-F_IDLST HZW-F_PNAME1
               GO TO M-980
           END-IF
           IF  ESTAT = BTB
               GO TO M-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-140
           END-IF.
       M-160.
           CALL "SD_Accept" USING BY REFERENCE A-EBCD1 "A-EBCD1" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               CALL "DB_F_Close" USING
                BY REFERENCE HZW-F_IDLST HZW-F_PNAME1
               GO TO M-980
           END-IF
           IF  ESTAT = BTB
               GO TO M-140
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-160
           END-IF
           IF  W-SBCD1 > W-EBCD1
               GO TO M-160
           END-IF
           GO TO M-220.
       M-180.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD "A-SHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               GO TO M-520
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-180
           END-IF
           IF  W-SHCD = 999999
               GO TO M-520
           END-IF.
       M-200.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD "A-EHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               CALL "DB_F_Close" USING
                BY REFERENCE HZW-F_IDLST HZW-F_PNAME1
               GO TO M-980
           END-IF
           IF  ESTAT = BTB
               GO TO M-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-200
           END-IF
           IF  W-SHCD > W-EHCD
               GO TO M-200
           END-IF
           GO TO M-220.
       M-210.
           CALL "SD_Accept" USING BY REFERENCE A-SSOC "A-SSOC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               CALL "DB_F_Close" USING
                BY REFERENCE HZW-F_IDLST HZW-F_PNAME1
               GO TO M-980
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-210
           END-IF.
       M-212.
           CALL "SD_Accept" USING BY REFERENCE A-ESOC "A-ESOC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               CALL "DB_F_Close" USING
                BY REFERENCE HZW-F_IDLST HZW-F_PNAME1
               GO TO M-980
           END-IF
           IF  ESTAT = BTB
               GO TO M-210
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-212
           END-IF
           IF  W-SSOC > W-ESOC
               GO TO M-212
           END-IF.
       M-220.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               CALL "DB_F_Close" USING
                BY REFERENCE HZW-F_IDLST HZW-F_PNAME1
               GO TO M-980
           END-IF
           IF  ESTAT = BTB
               IF  JS-SIGN = 0 OR 2 OR 6
                   GO TO M-160
               END-IF
           END-IF
           IF  ESTAT = BTB
               IF  JS-SIGN = 1 OR 7
                   GO TO M-200
               END-IF
           END-IF
           IF  ESTAT = BTB
               IF  JS-SIGN = 3
                   GO TO M-212
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-220
           END-IF
           IF  W-DMM = 9
               IF  JS-SIGN = 3
                   GO TO M-210
               ELSE
                   GO TO M-040
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-220
           END-IF
      *
           CALL "DB_F_Close" USING
            BY REFERENCE HZW-F_IDLST HZW-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HZW-F_PNAME1 " " BY REFERENCE HZW-F_IDLST "0".
       M-240.
      *           READ HZW-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HZW-F_PNAME1 BY REFERENCE HZW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-040
           END-IF
           IF  JS-SIGN = 0 OR 2 OR 6
               IF  HZW-BC3 < W-SBC3
                   GO TO M-240
               END-IF
           END-IF
           IF  JS-SIGN = 0 OR 2 OR 6
               IF  HZW-BC3 > W-EBC3
                   GO TO M-060
               END-IF
           END-IF
           IF  JS-SIGN = 0 OR 2 OR 6
               IF  HZW-BMNO < W-SBMNO OR > W-EBMNO
                   GO TO M-240
               END-IF
           END-IF
           IF  JS-SIGN = 0 OR 2 OR 6
               IF  HZW-BCD1 < W-SBCD1 OR > W-EBCD1
                   GO TO M-240
               END-IF
           END-IF
           IF  JS-SIGN = 1 OR 7
               IF  HZW-HCD < W-SHCD
                   GO TO M-240
               END-IF
           END-IF
           IF  JS-SIGN = 1 OR 7
               IF  HZW-HCD > W-EHCD
                   GO TO M-180
               END-IF
           END-IF
           IF  JS-SIGN = 3
               IF  HZW-SOC < W-SSOC OR > W-ESOC
                   GO TO M-240
               END-IF
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           IF JS-SIGN = 6 OR 7
               CALL "DB_F_Open" USING
                "OUTPUT" EXL-F_PNAME1 " " BY REFERENCE EXL-F_IDLST "0"
           END-IF
           IF JS-SIGN = 3
               MOVE ZERO TO WT-D
               CALL "DB_F_Open" USING
                "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
                "JCON3-KEY" BY REFERENCE JCON3-KEY
           END-IF.
       M-260.
           IF  JS-SIGN NOT = 3
               GO TO M-270
           END-IF
           MOVE HZW-SOC TO W-SOC.
           MOVE 3 TO JCON3-01.
           MOVE W-SOC TO JCON3-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
                MOVE "　　　　　　" TO JCON3-03
           END-IF
           MOVE JCON3-03 TO H-SON.
           IF  W-POC NOT = 0
               MOVE ZERO TO W-PAGE
               PERFORM S-05 THRU S-15
           END-IF.
       M-270.
           IF  JS-SIGN = 6 OR 7
               GO TO M-280
           END-IF
           IF  W-POC NOT = 0
               GO TO M-280
           END-IF
           MOVE 1 TO W-POC.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           IF  W-UC = 0
               MOVE 1 TO W-UC
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF.
       M-280.
           IF  JS-SIGN = 1 OR 7
               GO TO M-360
           END-IF
           MOVE ZERO TO WA-D.
       M-300.
           MOVE HZW-BC3 TO W-BC3.
           MOVE ZERO TO WG-D.
       M-320.
           MOVE HZW-BMNO TO W-BMNO.
           MOVE ZERO TO WS-D.
       M-340.
           MOVE HZW-BCD1 TO W-BCD1.
           MOVE ZERO TO WN-D.
       M-360.
           MOVE ZERO TO CHK.
           MOVE HZW-HCD TO W-HCD.
           MOVE HZW-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　　マスター　なし　　＊＊　　" TO HI-NAME
           END-IF.
       M-380.
           PERFORM S-20 THRU S-30.
       M-400.
      *           READ HZW-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HZW-F_PNAME1 BY REFERENCE HZW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-500
           END-IF
           IF  JS-SIGN = 0 OR 2 OR 6
               IF  HZW-BC3 > W-EBC3
                   GO TO M-500
               END-IF
           END-IF
           IF  JS-SIGN = 0 OR 2 OR 6
               IF  HZW-BMNO < W-SBMNO OR > W-EBMNO
                   GO TO M-400
               END-IF
           END-IF
           IF  JS-SIGN = 0 OR 2 OR 6
               IF  HZW-BCD1 < W-SBCD1 OR > W-EBCD1
                   GO TO M-400
               END-IF
           END-IF
           IF  JS-SIGN = 3
               IF  HZW-SOC < W-SSOC OR > W-ESOC
                   GO TO M-400
               END-IF
           END-IF
      *
           IF  JS-SIGN = 1 OR 7
               IF  HZW-HCD > W-EHCD
                   GO TO M-500
               END-IF
           END-IF
           IF  JS-SIGN = 3
               IF  W-SOC NOT = HZW-SOC
                   GO TO M-480
               END-IF
           END-IF
           IF  JS-SIGN = 0 OR 2 OR 3 OR 6
               IF  W-BC3 NOT = HZW-BC3
                   GO TO M-460
               END-IF
           END-IF
           IF  JS-SIGN = 0 OR 2 OR 3 OR 6
               IF  W-BMNO NOT = HZW-BMNO
                   GO TO M-440
               END-IF
           END-IF
           IF  JS-SIGN = 0 OR 2 OR 3 OR 6
               IF  W-BCD1 NOT = HZW-BCD1
                   GO TO M-420
               END-IF
           END-IF
           IF  HZW-HCD NOT = W-HCD
               GO TO M-360
           END-IF
           GO TO M-380.
       M-420.
           PERFORM S-35 THRU S-40.
           GO TO M-340.
       M-440.
           PERFORM S-35 THRU S-40.
           PERFORM S-45 THRU S-50.
           GO TO M-320.
       M-460.
           PERFORM S-35 THRU S-40.
           PERFORM S-45 THRU S-50.
           PERFORM S-55 THRU S-60.
           GO TO M-300.
       M-480.
           PERFORM S-35 THRU S-40.
           PERFORM S-45 THRU S-50.
           PERFORM S-55 THRU S-60.
           PERFORM S-65 THRU S-70.
           GO TO M-260.
       M-500.
           IF  JS-SIGN = 1 OR 7
               IF  W-EHCD = 999999
                   GO TO M-520
               ELSE
                   CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST
                   GO TO M-180
               END-IF
           END-IF
           PERFORM S-35 THRU S-40.
           PERFORM S-45 THRU S-50.
           PERFORM S-55 THRU S-60.
       M-520.
           IF  JS-SIGN NOT = 6 AND 7
               IF  W-POC = 0
                   GO TO M-900
               END-IF
           END-IF
           PERFORM S-65 THRU S-70.
           IF  JS-SIGN = 3
               PERFORM S-75 THRU S-80
           END-IF.
       M-900.
           IF  JS-SIGN NOT = 1 AND 7
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE HZW-F_IDLST HZW-F_PNAME1.
           IF  W-POC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF
           IF  JS-SIGN = 3
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
           END-IF
           IF  JS-SIGN = 6 OR 7
               CALL "DB_F_Close" USING
                BY REFERENCE EXL-F_IDLST EXL-F_PNAME1
           END-IF.
       M-980.
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
       S-15.
           EXIT.
       S-20.
           IF  HI-OL = 1
               IF  HZW-TC = 1
                   IF  JS-SIGN = 1
                       ADD HZW-ZST TO WA-ZSK
                       ADD HZW-GZS TO WA-GZ
                       ADD HZW-AS TO WA-AZ
                   ELSE
                       ADD HZW-ZST TO WN-ZSK
                       ADD HZW-GZS TO WN-GZ
                       ADD HZW-AS TO WN-AZ
                   END-IF
               END-IF
           END-IF
           IF  HI-OL = 1
               GO TO S-30
           END-IF
           IF  JS-SIGN = 6 OR 7
               INITIALIZE EXL-R
               MOVE SPACE TO EXL-NAME
           ELSE
               MOVE SPACE TO W-P
               MOVE SPACE TO P-NAME
           END-IF
           IF  CHK = 0
               MOVE 1 TO CHK
               IF  JS-SIGN = 6 OR 7
                   MOVE W-HCD TO EXL-HCD
                   MOVE HI-NAME TO EXL-NAME
               ELSE
                   MOVE W-HCD TO P-HCD
                   MOVE HI-NAME TO P-NAME
               END-IF
           END-IF
           IF  JS-SIGN = 6 OR 7
               MOVE HZW-SIZ TO EXL-SIZ
           ELSE
               MOVE HZW-SIZ TO P-SIZ
           END-IF
           MOVE ZERO TO CNT.
       S-25.
           IF  CNT NOT = 10
               ADD 1 TO CNT
               IF  JS-SIGN = 6 OR 7
                   MOVE HZW-ZS(CNT) TO EXL-ZS(CNT)
                   GO TO S-25
               ELSE
                   MOVE HZW-ZS(CNT) TO P-ZS(CNT)
                   GO TO S-25
               END-IF
           END-IF
      *
           IF  HZW-TC = 1
               IF  JS-SIGN = 6 OR 7
                   MOVE HZW-ZST TO EXL-ZSK
               ELSE
                   MOVE HZW-ZST TO P-ZSK
               END-IF
           END-IF
           IF  HZW-TC = 1
               IF  JS-SIGN = 1 OR 7
                   ADD HZW-ZST TO WA-ZSK
               ELSE
                   ADD HZW-ZST TO WN-ZSK
               END-IF
           END-IF
           IF  JS-SIGN NOT = 6 AND 7
               IF  HZW-AS = ZERO
                   CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
                   IF  LINAGECOUNTER > 62
                       MOVE W-HCD TO P-HCD
                       MOVE HI-NAME TO P-NAME
                       PERFORM S-05 THRU S-15
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN NOT = 6 AND 7
               IF  HZW-AS NOT = ZERO
                   CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
                   IF  LINAGECOUNTER > 61
                       MOVE W-HCD TO P-HCD
                       MOVE HI-NAME TO P-NAME
                       PERFORM S-05 THRU S-15
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN NOT = 6 AND 7
               MOVE SPACE TO SP-R
               MOVE W-P TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           IF  JS-SIGN = 3
               GO TO S-30
           END-IF
      *
           IF  JS-SIGN = 1 OR 7
               ADD HZW-GZS TO WA-GZ
           ELSE
               ADD HZW-GZS TO WN-GZ
           END-IF
           IF  JS-SIGN = 6 OR 7
               MOVE HZW-AS TO EXL-AZ
               MOVE HZW-GZS TO EXL-GZ
      *               WRITE EXL-R
      *///////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO S-27
           ELSE
               IF  HZW-AS = ZERO
                   GO TO S-30
               END-IF
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           MOVE "(" TO P-F.
           MOVE HZW-AS TO P-AZ.
           MOVE HZW-GZS TO P-GZ.
           MOVE ")" TO P-R.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-27.
           IF  JS-SIGN = 1 OR 7
               ADD HZW-AS TO WA-AZ
           ELSE
               ADD HZW-AS TO WN-AZ
           END-IF.
       S-30.
           EXIT.
       S-35.
           ADD WN-ZSK TO WS-ZSK.
           IF  JS-SIGN = 6 OR 7
               INITIALIZE EXL-R
               MOVE SPACE TO EXL-NAME
               MOVE "　　　　　　　　　　　　　　　　　　（　計　）"
                                                           TO EXL-NAME
               MOVE WN-ZSK TO EXL-ZSK
           ELSE
               MOVE SPACE TO W-P
               MOVE SPACE TO P-NAME
               MOVE "　　　　　　　　　　　　　　　　　　（　計　）"
                                                             TO P-NAME
               MOVE WN-ZSK TO P-ZSK
           END-IF
           IF  JS-SIGN NOT = 6 AND 7
               IF  WN-AZ = ZERO
                   CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
                   IF  LINAGECOUNTER > 62
                       PERFORM S-05 THRU S-15
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN NOT = 6 AND 7
               IF  WN-AZ NOT = ZERO
                   CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
                   IF  LINAGECOUNTER > 61
                       PERFORM S-05 THRU S-15
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN NOT = 6 AND 7
               MOVE SPACE TO SP-R
               MOVE W-P TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           IF  JS-SIGN = 3
               GO TO S-40
           END-IF
           ADD WN-GZ TO WS-GZ.
           ADD WN-AZ TO WS-AZ.
           IF  JS-SIGN = 6 OR 7
               MOVE WN-AZ TO EXL-AZ
               MOVE WN-GZ TO EXL-GZ
      *               WRITE EXL-R
      *///////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO S-40
           ELSE
               IF  WN-AZ = ZERO
                   GO TO S-40
               END-IF
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           MOVE "(" TO P-F
           MOVE WN-AZ TO P-AZ.
           MOVE WN-GZ TO P-GZ.
           MOVE ")" TO P-R
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-40.
           EXIT.
       S-45.
           IF  JS-SIGN = 6 OR 7
               INITIALIZE EXL-R
               MOVE SPACE TO EXL-NAME
               MOVE "　　　　　　　　　　　　　　　　（　小　計　）"
                                                           TO EXL-NAME
               MOVE WS-ZSK TO EXL-ZSK
           ELSE
               MOVE SPACE TO W-P
               MOVE SPACE TO P-NAME
               MOVE "　　　　　　　　　　　　　　　　（　小　計　）"
                                                             TO P-NAME
               MOVE WS-ZSK TO P-ZSK
           END-IF
           IF  JS-SIGN NOT = 6 AND 7
               IF  WS-AZ = ZERO
                   CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
                   IF  LINAGECOUNTER > 62
                       PERFORM S-05 THRU S-15
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN NOT = 6 AND 7
               IF  WS-AZ NOT = ZERO
                   CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
                   IF  LINAGECOUNTER > 61
                       PERFORM S-05 THRU S-15
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN NOT = 6 AND 7
               MOVE SPACE TO SP-R
               MOVE W-P TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           ADD WS-ZSK TO WG-ZSK.
           IF  JS-SIGN = 3
               GO TO S-50
           END-IF
           ADD WS-GZ TO WG-GZ.
           ADD WS-AZ TO WG-AZ.
           IF  JS-SIGN = 6 OR 7
               MOVE WS-AZ TO EXL-AZ
               MOVE WS-GZ TO EXL-GZ
      *               WRITE EXL-R
      *///////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO S-50
           ELSE
               IF  WS-AZ = ZERO
                   GO TO S-50
               END-IF
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           MOVE "(" TO P-F
           MOVE WS-AZ TO P-AZ.
           MOVE WS-GZ TO P-GZ.
           MOVE ")" TO P-R
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-50.
           EXIT.
       S-55.
           IF  JS-SIGN = 6 OR 7
               INITIALIZE EXL-R
               MOVE SPACE TO EXL-NAME
               MOVE "　　　　　　　　　　　　　｛　合　計　｝"
                                                            TO EXL-NAME
               MOVE WG-ZSK TO EXL-ZSK
           ELSE
               MOVE SPACE TO W-P
               MOVE SPACE TO P-NAME
               MOVE "　　　　　　　　　　　　　｛　合　計　｝"
                                                              TO P-NAME
               MOVE WG-ZSK TO P-ZSK
           END-IF
           IF  JS-SIGN NOT = 6 AND 7
               IF  WG-AZ = ZERO
                   CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
                   IF  LINAGECOUNTER > 62
                       PERFORM S-05 THRU S-15
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN NOT = 6 AND 7
               IF  WG-AZ NOT = ZERO
                   CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
                   IF  LINAGECOUNTER > 61
                       PERFORM S-05 THRU S-15
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN NOT = 6 AND 7
               MOVE SPACE TO SP-R
               MOVE W-P TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           ADD WG-ZSK TO WA-ZSK.
           IF  JS-SIGN = 3
               GO TO S-60
           END-IF
           ADD WG-GZ TO WA-GZ.
           ADD WG-AZ TO WA-AZ.
           IF  JS-SIGN = 6 OR 7
               MOVE WG-AZ TO EXL-AZ
               MOVE WG-GZ TO EXL-GZ
      *               WRITE EXL-R
      *///////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO S-60
           ELSE
               IF  WG-AZ = ZERO
                   GO TO S-60
               END-IF
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           MOVE "(" TO P-F
           MOVE WG-AZ TO P-AZ.
           MOVE WG-GZ TO P-GZ.
           MOVE ")" TO P-R
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-60.
           EXIT.
       S-65.
           IF  JS-SIGN = 6 OR 7
               INITIALIZE EXL-R
               MOVE SPACE TO EXL-NAME
               MOVE "　　　　　　　　【　総　合　計　】" TO EXL-NAME
               MOVE WA-ZSK TO EXL-ZSK
               MOVE WA-AZ TO EXL-AZ
               MOVE WA-GZ TO EXL-GZ
      *               WRITE EXL-R
      *///////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO S-70
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           MOVE WA-ZSK TO P-ZSK.
           IF  JS-SIGN = 3
               ADD WA-ZSK TO WT-ZSK
               MOVE "　　　　　　　　　　　［　倉庫計　］" TO P-NAME
           ELSE
               MOVE "　　　　　　　　【　総　合　計　】" TO P-NAME
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  JS-SIGN = 3
               GO TO S-70
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           MOVE "(" TO P-F.
           MOVE WA-AZ TO P-AZ.
           MOVE WA-GZ TO P-GZ.
           MOVE ")" TO P-R.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-70.
           EXIT.
       S-75.
           MOVE SPACE TO W-P P-NAME.
           MOVE "　　　　　　　　【　総　合　計　】" TO P-NAME.
           MOVE WT-ZSK TO P-ZSK
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-80.
           EXIT.

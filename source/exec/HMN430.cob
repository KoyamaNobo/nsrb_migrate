       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN430.
      ***********************************************************
      *    PROGRAM         :  品名サイズ別棚卸誤差表            *
      *    PRINTER TYPE    :  JIPS                              *
      *    SCREEN          :  ******                            *
      *    COMPILE TYPE    :  COBOL                             *
      *    JS-SIGN         :  作表=0 , エクセル=1 , Ｗチェック=5*
      ***********************************************************
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
           02  H-MID1         PIC  N(003) VALUE SPACE.
           02  F              PIC  X(002) VALUE SPACE.
           02  H-MID2         PIC  N(002) VALUE SPACE.
           02  H-MID3         PIC  N(003) VALUE SPACE.
           02  H-MID4         PIC  N(003) VALUE SPACE.
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  N(021) VALUE
                "＊＊＊　　品名サイズ別　棚卸誤差表　＊＊＊".
           02  F              PIC  X(028) VALUE SPACE.
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  X(001) VALUE "1".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "  SS".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "   S".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "   M".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "   L".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "  LL".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "28.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "29.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "30.0".
           02  F              PIC  X(010) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(048) VALUE SPACE.
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
           02  F              PIC  X(010) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(048) VALUE SPACE.
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
           02  F              PIC  X(010) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(048) VALUE SPACE.
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
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(002) VALUE "合計".
       01  HEAD9.
           02  F              PIC  X(050) VALUE
                "--------------------------------------------------".
           02  F              PIC  X(050) VALUE
                "--------------------------------------------------".
           02  F              PIC  X(029) VALUE
                "-----------------------------".
       01  W-P.
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(001).
           02  P-KBN          PIC  N(002).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  Z(001).
           02  P-ZSU.
             03  P-ZS         PIC  ---,---  OCCURS  10 TIMES.
           02  P-TSU          PIC --,---,--9.
       01  W-DATA.
           02  W-BC3          PIC  9(002).
           02  W-BCD1         PIC  9(003).
           02  W-BMNO         PIC  9(001).
           02  W-HCD          PIC  9(006).
           02  W-KBN          PIC  9(006).
           02  CHK            PIC  9(001).
           02  W-KBC          PIC  9(001).
           02  W-NC           PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-SOKU         PIC  9(003).
           02  W-SOKD         PIC S9(003).
           02  W-SEBC.
             03  W-SBC3       PIC  9(002).
             03  W-EBC3       PIC  9(002).
             03  W-SBMNO      PIC  9(001).
             03  W-EBMNO      PIC  9(001).
             03  W-SBCD1      PIC  9(003).
             03  W-EBCD1      PIC  9(003).
           02  W-DMM          PIC  9(001).
           02  W-KBND         PIC  N(002).
           02  W-ATBD.
             03  W-ATB   OCCURS   4.
               04  W-TBD.
                 05  W-TBDD  OCCURS  10.
                   06  W-TB   PIC S9(006).
               04  W-TBT      PIC S9(006).
               04  W-TBS      PIC  9(001).
               04  W-TBC      PIC  9(001).
           02  W-ATND.
             03  W-ATN   OCCURS   4.
               04  W-TND.
                 05  W-TNDD  OCCURS  10.
                   06  W-TN   PIC S9(006).
               04  W-TNT      PIC S9(006).
               04  W-TNS      PIC  9(001).
               04  W-TNC      PIC  9(001).
           02  W-ASGD.
             03  W-ASG   OCCURS   4.
               04  W-SGD.
                 05  W-SGDD  OCCURS  10.
                   06  W-SG   PIC S9(006).
               04  W-SGT      PIC S9(006).
               04  W-SGS      PIC  9(001).
               04  W-SGC      PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-SS           PIC  Z(003).
       01  WN-D.
           02  WN-TBK         PIC S9(007).
           02  WN-TNK         PIC S9(007).
           02  WN-SGK         PIC S9(007).
       01  WS-D.
           02  WS-TBK         PIC S9(007).
           02  WS-TNK         PIC S9(007).
           02  WS-SGK         PIC S9(007).
       01  WG-D.
           02  WG-TBK         PIC S9(007).
           02  WG-TNK         PIC S9(007).
           02  WG-SGK         PIC S9(007).
       01  WA-D.
           02  WA-TBK         PIC S9(007).
           02  WA-TNK         PIC S9(007).
           02  WA-SGK         PIC S9(007).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LSPF.
      *FD  HZW-F
       01  HZW-F_HMN430.
           02  HZW-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HZW-F_LNAME    PIC  X(012) VALUE "HZW-F_HMN430".
           02  F              PIC  X(001).
           02  HZW-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HZW-F_SORT     PIC  X(100) VALUE SPACE.
           02  HZW-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HZW-F_RES      USAGE  POINTER.
       01  HZW-R.
           02  HZW-HCD        PIC  9(006).
           02  HZW-KBN        PIC  9(001).
           02  HZW-SIZ        PIC  9(001).
           02  HZW-AZS.
             03  HZW-ZSD   OCCURS  10.
               04  HZW-ZS     PIC S9(006).
           02  HZW-TSU        PIC S9(006).
           02  HZW-TC         PIC  9(001).
           02  HZW-BCD1       PIC  9(003).
           02  HZW-BC2        PIC  9(001).
           02  HZW-BC3        PIC  9(002).
           02  HZW-BMC        PIC  9(002).
           02  HZW-BMNO       PIC  9(001).
           02  HZW-NO         PIC  9(001).
           02  HZW-FT         PIC  9(005).
           02  F              PIC  X(037).
           02  HZW-SEN        PIC  9(001).
       77  F                  PIC  X(001).
      *FD  EXLF
       01  EXLF_HMN430.
           02  EXLF_PNAME1    PIC  X(009) VALUE "WK0128000".
           02  F              PIC  X(001).
           02  EXLF_LNAME     PIC  X(011) VALUE "EXLF_HMN430".
           02  F              PIC  X(001).
           02  EXLF_KEY1      PIC  X(100) VALUE SPACE.
           02  EXLF_SORT      PIC  X(100) VALUE SPACE.
           02  EXLF_IDLST     PIC  X(100) VALUE SPACE.
           02  EXLF_RES       USAGE  POINTER.
       01  EXL-R.
           02  EXL-HCD        PIC  9(006).
           02  EXL-NAME       PIC  N(024).
           02  EXL-KBN        PIC  N(002).
           02  EXL-SIZ        PIC  9(001).
           02  EXL-ZSU.
             03  EXL-ZS       PIC S9(006)  OCCURS  10 TIMES.
           02  EXL-TSU        PIC S9(007).
           02  F              PIC  X(002).
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
           02  FILLER  PIC N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(022) VALUE
                "＊＊＊　　品名サイズ別　棚卸誤差表　　＊＊＊".
           02  FILLER  PIC N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-MID1.
           02  FILLER  PIC  X(023) VALUE
                "印字誤差範囲     足以上".
           02  FILLER  PIC  X(049) VALUE
                "分類3  00 ～ 99   (一　般=10,ワーク=20,教　育=30)".
           02  FILLER.
             03  FILLER  PIC  X(018) VALUE
                  "部門    0 ～ 9    ".
             03  FILLER  PIC  X(039) VALUE
                  "(国内成型=1,上海成型=2,ワーク=3,教育=4)".
           02  FILLER  PIC  X(016) VALUE
                "分類1 000 ～ 999".
           02  FILLER  PIC X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID2.
           02  FILLER  PIC  N(008) VALUE
                "【Ｗチェック用】".
       01  C-ACP.
           02  A-SOKU  PIC  9(003).
           02  FILLER.
             03  A-SBC3  PIC  9(002).
             03  A-EBC3  PIC  9(002).
           02  FILLER.
             03  A-SBMNO PIC  9(001).
             03  A-EBMNO PIC  9(001).
           02  FILLER.
             03  A-SBCD1 PIC  9(003).
             03  A-EBCD1 PIC  9(003).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-EXL   PIC  N(010) VALUE
                "（　エクセル変換　）".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  DATA ﾅｼ  ***".
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
           "C-MID" " " "0" "0" "308" " " " " RETURNING RESU.
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
      *C-MID1
       CALL "SD_Init" USING
           "C-MID1" " " "0" "0" "167" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID1" "X" "12" "19" "23" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID1" "X" "14" "19" "49" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID1" " " "15" "0" "57" "02C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID1" "X" "15" "19" "18" " " "03C-MID1" RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID1" "X" "15" "37" "39" "04C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID1" "X" "16" "19" "16" "03C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID1" "X" "20" "23" "22" "06C-MID1" " " RETURNING RESU.
      *C-MID2
       CALL "SD_Init" USING
           "C-MID2" " " "0" "0" "16" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID2" "X" "12" "24" "16" " " "C-MID2" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "16" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SOKU" "9" "12" "33" "3" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SOKU" BY REFERENCE W-SOKU "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "14" "0" "4" "A-SOKU" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SBC3" "9" "14" "26" "2" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SBC3" BY REFERENCE W-SBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EBC3" "9" "14" "32" "2" "A-SBC3" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EBC3" BY REFERENCE W-EBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "15" "0" "2" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SBMNO" "9" "15" "27" "1" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SBMNO" BY REFERENCE W-SBMNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EBMNO" "9" "15" "32" "1" "A-SBMNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EBMNO" BY REFERENCE W-EBMNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "16" "0" "6" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SBCD1" "9" "16" "25" "3" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SBCD1" BY REFERENCE W-SBCD1 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EBCD1" "9" "16" "32" "3" "A-SBCD1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EBCD1" BY REFERENCE W-EBCD1 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "40" "1" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "20" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-EXL" "N" "7" "22" "20" " " "C-DSP" RETURNING RESU.
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
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 0 AND 1 AND 5
               CALL "DB_Close"
               STOP RUN
           END-IF
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU
           ELSE
               IF  JS-SIGN = 1
                   CALL "SD_Output" USING
                    "D-EXL" D-EXL "p" RETURNING RESU
               ELSE
                   CALL "SD_Output" USING
                    "C-MID2" C-MID2 "p" RETURNING RESU
               END-IF
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO HZW-F_PNAME1.
           MOVE ZERO TO W-SOKU W-SBC3 W-SBMNO W-SBCD1.
           MOVE 99 TO W-EBC3.
           MOVE 9 TO W-EBMNO.
           MOVE 999 TO W-EBCD1.
           IF  JS-SIGN = 1
               GO TO M-10
           END-IF
           IF  JS-SIGN = 5
               MOVE 1 TO W-SOKU
               GO TO M-10
           END-IF
           PERFORM ACP-RTN THRU ACP-EX.
           IF  ESTAT = PF9 OR C2
               GO TO M-95
           END-IF.
       M-10.
           COMPUTE W-SOKD = W-SOKU * -1.
           MOVE ZERO TO WA-D.
           CALL "DB_F_Open" USING
            "INPUT" HZW-F_PNAME1 " " BY REFERENCE HZW-F_IDLST "0".
       M-15.
      *           READ HZW-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HZW-F_PNAME1 BY REFERENCE HZW-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HZW-F_IDLST HZW-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  HZW-BC3 < W-SBC3
               GO TO M-15
           END-IF
           IF  HZW-BC3 > W-EBC3
               CALL "DB_F_Close" USING
                BY REFERENCE HZW-F_IDLST HZW-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  HZW-BMNO < W-SBMNO OR > W-EBMNO
               GO TO M-15
           END-IF
           IF  HZW-BCD1 < W-SBCD1 OR > W-EBCD1
               GO TO M-15
           END-IF
      *
           MOVE ZERO TO WA-D.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           IF  JS-SIGN = 1
               CALL "DB_F_Open" USING
                "OUTPUT" EXLF_PNAME1 " " BY REFERENCE EXLF_IDLST "0"
               GO TO M-20
           END-IF
           IF  HZW-SEN = 1
               MOVE "（親）" TO H-MID1
           END-IF
           IF  W-SOKU NOT = ZERO
               IF  JS-SIGN = 5
                   MOVE "【Ｗ" TO H-MID2
                   MOVE "チェッ" TO H-MID3
                   MOVE "ク用】" TO H-MID4
               ELSE
                   MOVE "誤差" TO H-MID2
                   MOVE W-SOKU TO W-SS
                   MOVE W-SS TO H-MID3
                   MOVE "足より" TO H-MID4
               END-IF
           END-IF
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           PERFORM MID-10 THRU MID-EX.
       M-20.
           MOVE HZW-BC3 TO W-BC3.
           MOVE ZERO TO WG-D.
       M-25.
           MOVE HZW-BMNO TO W-BMNO.
           MOVE ZERO TO WS-D.
       M-30.
           MOVE HZW-BCD1 TO W-BCD1.
           MOVE ZERO TO WN-D W-DC.
       M-35.
           MOVE ZERO TO W-ATBD W-ATND W-ASGD.
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
       M-40.
           IF  HZW-KBN = 0
               MOVE HZW-AZS TO W-TBD(HZW-NO)
               MOVE HZW-TSU TO W-TBT(HZW-NO)
               MOVE HZW-SIZ TO W-TBS(HZW-NO)
               MOVE HZW-TC TO W-TBC(HZW-NO)
           END-IF
           IF  HZW-KBN = 1
               MOVE HZW-AZS TO W-TND(HZW-NO)
               MOVE HZW-TSU TO W-TNT(HZW-NO)
               MOVE HZW-SIZ TO W-TNS(HZW-NO)
               MOVE HZW-TC TO W-TNC(HZW-NO)
           END-IF
           IF  HZW-KBN = 2
               MOVE HZW-AZS TO W-SGD(HZW-NO)
               MOVE HZW-TSU TO W-SGT(HZW-NO)
               MOVE HZW-SIZ TO W-SGS(HZW-NO)
               MOVE HZW-TC TO W-SGC(HZW-NO)
           END-IF.
       M-45.
      *           READ HZW-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HZW-F_PNAME1 BY REFERENCE HZW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-65
           END-IF
           IF  HZW-BC3 > W-EBC3
               GO TO M-65
           END-IF
           IF  HZW-BMNO < W-SBMNO OR > W-EBMNO
               GO TO M-45
           END-IF
           IF  HZW-BCD1 < W-SBCD1 OR > W-EBCD1
               GO TO M-45
           END-IF
      *
           IF  W-BC3 NOT = HZW-BC3
               GO TO M-60
           END-IF
           IF  W-BMNO NOT = HZW-BMNO
               GO TO M-55
           END-IF
           IF  W-BCD1 NOT = HZW-BCD1
               GO TO M-50
           END-IF
           IF  HZW-HCD = W-HCD
               GO TO M-40
           END-IF
      *
           PERFORM MEI-RTN THRU MEI-EX.
           GO TO M-35.
       M-50.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM KE1-RTN THRU KE1-EX.
           GO TO M-30.
       M-55.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM KE1-RTN THRU KE1-EX.
           PERFORM KE2-RTN THRU KE2-EX.
           GO TO M-25.
       M-60.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM KE1-RTN THRU KE1-EX.
           PERFORM KE2-RTN THRU KE2-EX.
           PERFORM KE3-RTN THRU KE3-EX.
           GO TO M-20.
       M-65.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM KE1-RTN THRU KE1-EX.
           PERFORM KE2-RTN THRU KE2-EX.
           PERFORM KE3-RTN THRU KE3-EX.
           PERFORM KE4-RTN THRU KE4-EX.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HZW-F_IDLST HZW-F_PNAME1.
           IF  JS-SIGN = 1
               CALL "DB_F_Close" USING
                BY REFERENCE EXLF_IDLST EXLF_PNAME1
           ELSE
               CALL "PR_Close" RETURNING RESP
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-SOKU "A-SOKU" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-RTN
           END-IF.
       ACP-10.
           CALL "SD_Accept" USING BY REFERENCE A-SBC3 "A-SBC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-10
           END-IF.
       ACP-20.
           CALL "SD_Accept" USING BY REFERENCE A-EBC3 "A-EBC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-20
           END-IF
           IF  W-SBC3 > W-EBC3
               GO TO ACP-20
           END-IF.
       ACP-30.
           CALL "SD_Accept" USING BY REFERENCE A-SBMNO "A-SBMNO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-10
           END-IF.
       ACP-40.
           CALL "SD_Accept" USING BY REFERENCE A-EBMNO "A-EBMNO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-30
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-40
           END-IF
           IF  W-SBMNO > W-EBMNO
               GO TO ACP-40
           END-IF.
       ACP-50.
           CALL "SD_Accept" USING BY REFERENCE A-SBCD1 "A-SBCD1" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-40
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-50
           END-IF.
       ACP-60.
           CALL "SD_Accept" USING BY REFERENCE A-EBCD1 "A-EBCD1" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-50
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-60
           END-IF
           IF  W-SBCD1 > W-EBCD1
               GO TO ACP-60
           END-IF.
       ACP-70.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-60
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-70
           END-IF
           IF  W-DMM = 9
               GO TO ACP-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-70
           END-IF.
       ACP-EX.
           EXIT.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-10.
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
       MID-EX.
           EXIT.
       MEI-RTN.
           MOVE 0 TO W-NC.
       MEI-05.
           ADD 1 TO W-NC.
           IF  W-NC > 4
               GO TO MEI-EX
           END-IF
           MOVE ZERO TO CNT.
       MEI-10.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO MEI-05
           END-IF
           IF  W-SG(W-NC,CNT) < W-SOKU AND > W-SOKD
               GO TO MEI-10
           END-IF
      *
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF
           MOVE ZERO TO CHK W-KBC W-NC.
       MEI-15.
           ADD 1 TO W-NC.
           IF  W-NC > 4
               GO TO MEI-25
           END-IF
           IF  ZERO = W-TB(W-NC,01) AND W-TB(W-NC,02) AND W-TB(W-NC,03)
                 AND W-TB(W-NC,04) AND W-TB(W-NC,05) AND W-TB(W-NC,06)
                 AND W-TB(W-NC,07) AND W-TB(W-NC,08) AND W-TB(W-NC,09)
                 AND W-TB(W-NC,10)
               GO TO MEI-15
           END-IF
      *
           IF  JS-SIGN = 0 OR 5
               IF  W-KBC = 0
                   CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
                   IF  LINAGECOUNTER > 61
                       MOVE 0 TO CHK W-KBC
                       PERFORM MID-RTN THRU MID-EX
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 1
               MOVE SPACE TO EXL-R
               MOVE SPACE TO P-NAME P-KBN
           ELSE
               MOVE SPACE TO W-P
               MOVE SPACE TO P-NAME P-KBN
           END-IF
           IF  CHK = 0
               MOVE 1 TO CHK
               IF  JS-SIGN = 1
                   MOVE W-HCD TO EXL-HCD
                   MOVE HI-NAME TO EXL-NAME
               ELSE
                   MOVE W-HCD TO P-HCD
                   MOVE HI-NAME TO P-NAME
               END-IF
           END-IF
           IF  W-KBC = 0
               MOVE 1 TO W-KBC
               IF  JS-SIGN = 1
                   MOVE "帳簿" TO EXL-KBN
               ELSE
                   IF  JS-SIGN = 5
                       MOVE "ＷＣ" TO P-KBN
                   ELSE
                       MOVE "帳簿" TO P-KBN
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 1
               MOVE W-TBS(W-NC) TO EXL-SIZ
           ELSE
               MOVE W-TBS(W-NC) TO P-SIZ
           END-IF
           MOVE ZERO TO CNT.
       MEI-20.
           IF  CNT NOT = 10
               ADD 1 TO CNT
               IF  JS-SIGN = 1
                   MOVE W-TB(W-NC,CNT) TO EXL-ZS(CNT)
                   GO TO MEI-20
               ELSE
                   MOVE W-TB(W-NC,CNT) TO P-ZS(CNT)
                   GO TO MEI-20
               END-IF
           END-IF
           IF  W-TBC(W-NC) = 1
               IF  JS-SIGN = 1
                   MOVE W-TBT(W-NC) TO EXL-TSU
               ELSE
                   MOVE W-TBT(W-NC) TO P-TSU
               END-IF
           END-IF
           IF  W-TBC(W-NC) = 1
               IF  W-SOKU = ZERO
                   ADD W-TBT(W-NC) TO WN-TBK
               END-IF
           END-IF
           IF  JS-SIGN = 1
      *               WRITE EXL-R
      *//////////////
               CALL "DB_Insert" USING
                EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET
               GO TO MEI-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           GO TO MEI-15.
       MEI-25.
           MOVE ZERO TO W-KBC W-NC.
       MEI-30.
           ADD 1 TO W-NC.
           IF  W-NC > 4
               GO TO MEI-40
           END-IF
           IF  ZERO = W-TN(W-NC,01) AND W-TN(W-NC,02) AND W-TN(W-NC,03)
                 AND W-TN(W-NC,04) AND W-TN(W-NC,05) AND W-TN(W-NC,06)
                 AND W-TN(W-NC,07) AND W-TN(W-NC,08) AND W-TN(W-NC,09)
                 AND W-TN(W-NC,10)
               GO TO MEI-30
           END-IF
           IF  JS-SIGN = 0 OR 5
               IF  W-KBC = 0
                   CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
                   IF  LINAGECOUNTER > 61
                       MOVE 0 TO CHK W-KBC
                       PERFORM MID-RTN THRU MID-EX
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 1
               MOVE SPACE TO EXL-R
               MOVE SPACE TO P-NAME P-KBN
           ELSE
               MOVE SPACE TO W-P
               MOVE SPACE TO P-NAME P-KBN
           END-IF
           IF  CHK = 0
               MOVE 1 TO CHK
               IF  JS-SIGN = 1
                   MOVE W-HCD TO EXL-HCD
                   MOVE HI-NAME TO EXL-NAME
               ELSE
                   MOVE W-HCD TO P-HCD
                   MOVE HI-NAME TO P-NAME
               END-IF
           END-IF
           IF  W-KBC = 0
               MOVE 1 TO W-KBC
               IF  JS-SIGN = 1
                   MOVE "棚卸" TO EXL-KBN
               ELSE
                   IF  JS-SIGN = 5
                       MOVE "通常" TO P-KBN
                   ELSE
                       MOVE "棚卸" TO P-KBN
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 1
               MOVE W-TNS(W-NC) TO EXL-SIZ
           ELSE
               MOVE W-TNS(W-NC) TO P-SIZ
           END-IF
           MOVE ZERO TO CNT.
       MEI-35.
           IF  CNT NOT = 10
               ADD 1 TO CNT
               IF  JS-SIGN = 1
                   MOVE W-TN(W-NC,CNT) TO EXL-ZS(CNT)
                   GO TO MEI-35
               ELSE
                   MOVE W-TN(W-NC,CNT) TO P-ZS(CNT)
                   GO TO MEI-35
               END-IF
           END-IF
           IF  W-TNC(W-NC) = 1
               IF  JS-SIGN = 1
                   MOVE W-TNT(W-NC) TO EXL-TSU
               ELSE
                   MOVE W-TNT(W-NC) TO P-TSU
               END-IF
           END-IF
           IF  W-TNC(W-NC) = 1
               IF  W-SOKU = ZERO
                   ADD W-TNT(W-NC) TO WN-TNK
               END-IF
           END-IF
           IF  JS-SIGN = 1
      *               WRITE EXL-R
      *//////////////
               CALL "DB_Insert" USING
                EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET
               GO TO MEI-30
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO MEI-30.
       MEI-40.
           MOVE ZERO TO W-KBC W-NC.
       MEI-45.
           ADD 1 TO W-NC.
           IF  W-NC > 4
               GO TO MEI-55
           END-IF
           IF  ZERO = W-SG(W-NC,01) AND W-SG(W-NC,02) AND W-SG(W-NC,03)
                 AND W-SG(W-NC,04) AND W-SG(W-NC,05) AND W-SG(W-NC,06)
                 AND W-SG(W-NC,07) AND W-SG(W-NC,08) AND W-SG(W-NC,09)
                 AND W-SG(W-NC,10)
               GO TO MEI-45
           END-IF
           IF  JS-SIGN = 0
               IF  W-KBC = 0
                   CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
                   IF  LINAGECOUNTER > 61
                       MOVE 0 TO CHK W-KBC
                       PERFORM MID-RTN THRU MID-EX
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 1
               MOVE SPACE TO EXL-R
               MOVE SPACE TO P-NAME P-KBN
           ELSE
               MOVE SPACE TO W-P
               MOVE SPACE TO P-NAME P-KBN
           END-IF
           IF  CHK = 0
               MOVE 1 TO CHK
               IF  JS-SIGN = 1
                   MOVE W-HCD TO EXL-HCD
                   MOVE HI-NAME TO EXL-NAME
               ELSE
                   MOVE W-HCD TO P-HCD
                   MOVE HI-NAME TO P-NAME
               END-IF
           END-IF
           IF  W-KBC = 0
               MOVE 1 TO W-KBC
               IF  JS-SIGN = 1
                   MOVE "差額" TO EXL-KBN
               ELSE
                   MOVE "差額" TO P-KBN
               END-IF
           END-IF
           IF  JS-SIGN = 1
               MOVE W-SGS(W-NC) TO EXL-SIZ
           ELSE
               MOVE W-SGS(W-NC) TO P-SIZ
           END-IF
           MOVE ZERO TO CNT.
       MEI-50.
           IF  CNT NOT = 10
               ADD 1 TO CNT
               IF  JS-SIGN = 1
                   MOVE W-SG(W-NC,CNT) TO EXL-ZS(CNT)
                   GO TO MEI-50
               ELSE
                   MOVE W-SG(W-NC,CNT) TO P-ZS(CNT)
                   GO TO MEI-50
               END-IF
           END-IF
           IF  W-SGC(W-NC) = 1
               IF  JS-SIGN = 1
                   MOVE W-SGT(W-NC) TO EXL-TSU
               ELSE
                   MOVE W-SGT(W-NC) TO P-TSU
               END-IF
           END-IF
           IF  W-SGC(W-NC) = 1
               IF  W-SOKU = ZERO
                   ADD W-SGT(W-NC) TO WN-SGK
               END-IF
           END-IF
           IF  JS-SIGN = 1
      *               WRITE EXL-R
      *//////////////
               CALL "DB_Insert" USING
                EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET
               GO TO MEI-45
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO MEI-45.
       MEI-55.
           IF  JS-SIGN = 1
               GO TO MEI-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE HEAD9 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MEI-EX.
           EXIT.
       KE1-RTN.
           IF  W-SOKU NOT = ZERO
               GO TO KE1-EX
           END-IF
           IF  W-DC = 0
               GO TO KE1-EX
           END-IF
           IF  JS-SIGN = 1
               GO TO KE1-10
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-KBN.
           MOVE "　　　　　　　　　　　　　　　　　（　計　）"
                                                             TO P-NAME.
           IF  JS-SIGN = 5
               MOVE "ＷＣ" TO P-KBN
           ELSE
               MOVE "帳簿" TO P-KBN
           END-IF
           MOVE WN-TBK TO P-TSU.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-KBN.
           IF  JS-SIGN = 5
               MOVE "通常" TO P-KBN
           ELSE
               MOVE "棚卸" TO P-KBN
           END-IF
           MOVE WN-TNK TO P-TSU.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-KBN.
           MOVE "差額" TO P-KBN.
           MOVE WN-SGK TO P-TSU.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE HEAD9 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO KE1-90.
       KE1-10.
           MOVE SPACE TO EXL-R.
           MOVE SPACE TO EXL-NAME EXL-KBN.
           MOVE "　　　　　　　　　　　　　　　　　（　計　）"
                                                           TO EXL-NAME.
           MOVE "帳簿" TO EXL-KBN.
           MOVE WN-TBK TO EXL-TSU.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET.
      *
           MOVE SPACE TO EXL-R.
           MOVE SPACE TO EXL-NAME EXL-KBN.
           MOVE "棚卸" TO EXL-KBN.
           MOVE WN-TNK TO EXL-TSU.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET.
      *
           MOVE SPACE TO EXL-R.
           MOVE SPACE TO EXL-NAME EXL-KBN.
           MOVE "差額" TO EXL-KBN.
           MOVE WN-SGK TO EXL-TSU.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET.
       KE1-90.
           ADD WN-TBK TO WS-TBK.
           ADD WN-TNK TO WS-TNK.
           ADD WN-SGK TO WS-SGK.
       KE1-EX.
           EXIT.
       KE2-RTN.
           IF  W-SOKU NOT = ZERO
               GO TO KE2-EX
           END-IF
           IF  JS-SIGN = 1
               GO TO KE2-10
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-KBN.
           MOVE "　　　　　　　　　　　　（　小　計　）" TO P-NAME.
           IF  JS-SIGN = 5
               MOVE "ＷＣ" TO P-KBN
           ELSE
               MOVE "帳簿" TO P-KBN
           END-IF
           MOVE WS-TBK TO P-TSU.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-KBN.
           IF  JS-SIGN = 5
               MOVE "通常" TO P-KBN
           ELSE
               MOVE "棚卸" TO P-KBN
           END-IF
           MOVE WS-TNK TO P-TSU.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-KBN.
           MOVE "差額" TO P-KBN.
           MOVE WS-SGK TO P-TSU.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE HEAD9 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO KE2-90.
       KE2-10.
           MOVE SPACE TO EXL-R.
           MOVE SPACE TO EXL-NAME EXL-KBN.
           MOVE "　　　　　　　　　　　　（　小　計　）" TO EXL-NAME.
           MOVE "帳簿" TO EXL-KBN.
           MOVE WS-TBK TO EXL-TSU.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET.
      *
           MOVE SPACE TO EXL-R.
           MOVE SPACE TO EXL-NAME EXL-KBN.
           MOVE "棚卸" TO EXL-KBN.
           MOVE WS-TNK TO EXL-TSU.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET.
      *
           MOVE SPACE TO EXL-R.
           MOVE SPACE TO EXL-NAME EXL-KBN.
           MOVE "差額" TO EXL-KBN.
           MOVE WS-SGK TO EXL-TSU.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET.
       KE2-90.
           ADD WS-TBK TO WG-TBK.
           ADD WS-TNK TO WG-TNK.
           ADD WS-SGK TO WG-SGK.
       KE2-EX.
           EXIT.
       KE3-RTN.
           IF  W-SOKU NOT = ZERO
               GO TO KE3-EX
           END-IF
           IF  JS-SIGN = 1
               GO TO KE3-10
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-KBN.
           MOVE "　　　　　　　｛　合　計　｝" TO P-NAME.
           IF  JS-SIGN = 5
               MOVE "ＷＣ" TO P-KBN
           ELSE
               MOVE "帳簿" TO P-KBN
           END-IF
           MOVE WG-TBK TO P-TSU.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-KBN.
           IF  JS-SIGN = 5
               MOVE "通常" TO P-KBN
           ELSE
               MOVE "棚卸" TO P-KBN
           END-IF
           MOVE WG-TNK TO P-TSU.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-KBN.
           MOVE "差額" TO P-KBN.
           MOVE WG-SGK TO P-TSU.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE HEAD9 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO KE3-90.
       KE3-10.
           MOVE SPACE TO EXL-R.
           MOVE SPACE TO EXL-NAME EXL-KBN.
           MOVE "　　　　　　　｛　合　計　｝" TO EXL-NAME.
           MOVE "帳簿" TO EXL-KBN.
           MOVE WG-TBK TO EXL-TSU.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET.
      *
           MOVE SPACE TO EXL-R.
           MOVE SPACE TO EXL-NAME EXL-KBN.
           MOVE "棚卸" TO EXL-KBN.
           MOVE WG-TNK TO EXL-TSU.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET.
      *
           MOVE SPACE TO EXL-R.
           MOVE SPACE TO EXL-NAME EXL-KBN.
           MOVE "差額" TO EXL-KBN.
           MOVE WG-SGK TO EXL-TSU.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET.
       KE3-90.
           ADD WG-TBK TO WA-TBK.
           ADD WG-TNK TO WA-TNK.
           ADD WG-SGK TO WA-SGK.
       KE3-EX.
           EXIT.
       KE4-RTN.
           IF  W-SOKU NOT = ZERO
               GO TO KE4-EX
           END-IF
           IF  JS-SIGN = 1
               GO TO KE4-10
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-KBN.
           MOVE "　　　　　【　総　合　計　】" TO P-NAME.
           IF  JS-SIGN = 5
               MOVE "ＷＣ" TO P-KBN
           ELSE
               MOVE "帳簿" TO P-KBN
           END-IF
           MOVE WA-TBK TO P-TSU.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-KBN.
           IF  JS-SIGN = 5
               MOVE "通常" TO P-KBN
           ELSE
               MOVE "棚卸" TO P-KBN
           END-IF
           MOVE WA-TNK TO P-TSU.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-KBN.
           MOVE "差額" TO P-KBN.
           MOVE WA-SGK TO P-TSU.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO KE4-EX.
       KE4-10.
           MOVE SPACE TO EXL-R.
           MOVE SPACE TO EXL-NAME EXL-KBN.
           MOVE "　　　　　【　総　合　計　】" TO EXL-NAME.
           MOVE "帳簿" TO EXL-KBN.
           MOVE WA-TBK TO EXL-TSU.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET.
      *
           MOVE SPACE TO EXL-R.
           MOVE SPACE TO EXL-NAME EXL-KBN.
           MOVE "棚卸" TO EXL-KBN.
           MOVE WA-TNK TO EXL-TSU.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET.
      *
           MOVE SPACE TO EXL-R.
           MOVE SPACE TO EXL-NAME EXL-KBN.
           MOVE "差額" TO EXL-KBN.
           MOVE WA-SGK TO EXL-TSU.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET.
       KE4-EX.
           EXIT.

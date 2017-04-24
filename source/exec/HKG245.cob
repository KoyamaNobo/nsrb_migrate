       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG245.
      ******************************************************************
      *    PROGRAM         :  請求明細書　作成（ナフコ・ワークマン）   *
      *    PRINTER TYPE    :  JIPS                                     *
      *    SCREEN          :  ******                                   *
      *    JS-SIGN         :  発行=0 , 再発行=1                        *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-STC              PIC  9(001) VALUE 0.
       77  W-FILE             PIC  X(013).
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(039).
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  N(010) VALUE
                "請　求　明　細　書　".
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  X(010).
           02  H-NEN          PIC  9(004).
           02  F              PIC  N(002) VALUE "年　".
           02  H-GET          PIC Z9.
           02  F              PIC  N(002) VALUE "月　".
           02  H-PEY          PIC Z9.
           02  F              PIC  N(002) VALUE "日　".
           02  F              PIC  X(002).
           02  F              PIC  N(002) VALUE "№　".
           02  H-SNO          PIC  9(006).
           02  H-V            PIC  X(001).
           02  H-PAGE         PIC  9(002).
       01  HEAD2.
           02  H-TNA          PIC  N(026).
           02  F              PIC  X(003).
           02  H-F            PIC  X(001).
           02  H-TCD          PIC  9(004).
           02  H-R            PIC  X(001).
           02  F              PIC  X(002).
           02  H-TNC1         PIC  9(001).
           02  H-DV           PIC  X(001).
           02  H-DCC          PIC  9(001).
           02  F              PIC  X(057).
       01  HEAD3.
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　前回請求額".
           02  F              PIC  N(008) VALUE "　前回分ご入金額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　調整額".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "繰越残高".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(006) VALUE "当月お買上額".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(006) VALUE "当月消費税額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　今回請求額".
           02  F              PIC  X(015) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(013).
           02  H-ZSK          PIC ----,---,--9.
           02  H-ZNK          PIC ----,---,--9.
           02  H-CSK          PIC --,---,--9.
           02  H-KKZ          PIC ----,---,--9.
           02  H-URK          PIC ----,---,--9.
           02  H-SHZ          PIC --,---,--9.
           02  H-SKK          PIC ------,---,--9.
           02  F              PIC  X(015).
       01  HEAD5.
           02  F              PIC  X(050) VALUE
                "--------------------------------------------------".
           02  F              PIC  X(047) VALUE
                "-----------------------------------------------".
           02  F              PIC  X(013) VALUE
                "-------------".
       01  HEAD6.
           02  F              PIC  N(004) VALUE "　月日　".
           02  F              PIC  N(004) VALUE "伝票№　".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "区分".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(008) VALUE "品　　名　　他　".
           02  F              PIC  X(044) VALUE SPACE.
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　単　価".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
       01  W-P1.
           02  P-GET1         PIC Z9.
           02  P-S1           PIC  X(001).
           02  P-PEY1         PIC Z9.
           02  F              PIC  X(001).
           02  P-DNO1         PIC  9(006).
           02  F              PIC  X(001).
           02  P-KBN1         PIC  N(002).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(013).
           02  P-SU           PIC ----,--9.
           02  P-T            PIC ----,--9.
           02  P-KIN1         PIC ---,---,--9.
           02  P-SHZ1         PIC --,---,--9.
       01  W-P2.
           02  F              PIC  X(013).
           02  P-KBN2         PIC  N(002).
           02  F              PIC  X(001).
           02  P-BI           PIC  N(028).
           02  F              PIC  X(001).
           02  P-HM2          PIC  N(002).
           02  P-C2           PIC  X(001).
           02  P-HNO2         PIC  9(006).
           02  F              PIC  X(003).
           02  P-TM2          PIC  N(004).
           02  F              PIC  X(010).
           02  P-KIN2         PIC ---,---,--9.
           02  P-SHZ2         PIC --,---,--9.
       01  W-RD.
           02  W-R   OCCURS   8.
             03  WR-TCD       PIC  9(004).
             03  WR-DATE      PIC  9(008).
             03  WR-NGPD  REDEFINES WR-DATE.
               04  F          PIC  9(004).
               04  WR-GP      PIC  9(004).
             03  WR-DTC       PIC  9(001).
             03  WR-DNO       PIC  9(006).
             03  WR-HCD       PIC  9(006).
             03  WR-T         PIC S9(006)V9(02).
             03  WR-DC        PIC  9(001).
             03  WR-CSC       PIC  9(001).
             03  WR-SU        PIC S9(006)V9(02).
             03  WR-KIN       PIC S9(009).
             03  WR-SKD       PIC  9(008).
             03  WR-TNC       PIC  9(002).
             03  WR-BMC       PIC  9(001).
             03  WR-DCC       PIC  9(001).
             03  F            PIC  X(002).
             03  WR-TCD2      PIC  9(004).
             03  WR-CCD       PIC  9(003).
             03  WR-BI        PIC  N(024).
             03  WR-HNO       PIC  9(006).
             03  F            PIC  X(030).
             03  WR-SHZ       PIC S9(007).
             03  WR-KSU       PIC  9(003).
             03  F            PIC  X(019).
             03  WR-SNO       PIC  9(006).
       01  W-DATA.
           02  W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-TCD          PIC  9(004).
           02  W-SNO          PIC  9(006).
           02  W-SNOD         PIC  9(006).
           02  W-KEI.
             03  W-ZSK        PIC S9(009).
             03  W-ZNK        PIC S9(009).
             03  W-CSK        PIC S9(007).
             03  W-KKZ        PIC S9(009).
             03  W-SKK        PIC S9(009).
             03  W-HTZ        PIC S9(009).
             03  W-SZZ        PIC S9(007).
             03  W-HTS        PIC S9(009).
             03  W-SZS        PIC S9(007).
           02  W-DNO          PIC  9(006).
           02  W-C            PIC  9(001).
           02  W-GPD.
             03  W-GETD       PIC  9(002).
             03  W-PEYD       PIC  9(002).
           02  W-CCD          PIC  9(003).
           02  W-BI           PIC  N(024).
           02  W-HNO          PIC  9(006).
           02  W-D.
             03  W-SU         PIC S9(006)V9(02).
             03  W-TN         PIC S9(006)V9(02).
             03  W-KIN        PIC S9(009).
             03  W-SHZ        PIC S9(007).
           02  W-TKIN         PIC S9(009).
           02  W-GC           PIC  9(002).
           02  W-PAGE         PIC  9(002).
           02  W-SKDC         PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-KC           PIC  9(001).
           02  W-ZNC          PIC  9(001).
           02  W-PTC          PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-SEN          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-POC          PIC  9(001).
           02  W-NAME         PIC  N(028).
           02  W-NAM   REDEFINES W-NAME.
             03  W-NAD   OCCURS 28.
               04  W-NA       PIC  N(001).
           02  W-DATE         PIC  9(008).
       01  ERR-STAT           PIC  X(002).
           COPY LDAIW.
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LICAL.
           COPY LITM.
           COPY LITCM.
           COPY LIHIM.
           COPY LIHKBM.
           COPY LITUKF.
           COPY LITSKF.
           COPY LISKDF.
           COPY LSPF.
      *FD  TSKWF
       01  TSKWF_HKG245.
           02  TSKWF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TSKWF_LNAME    PIC  X(012) VALUE "TSKWF_HKG245".
           02  F              PIC  X(001).
           02  TSKWF_KEY1     PIC  X(100) VALUE SPACE.
           02  TSKWF_SORT     PIC  X(100) VALUE SPACE.
           02  TSKWF_IDLST    PIC  X(100) VALUE SPACE.
           02  TSKWF_RES      USAGE  POINTER.
       01  TSKW-R.
           02  TSKW-KEY.
             03  TSKW-TCD     PIC  9(004).
           02  TSKW-ZSDD.
             03  TSKW-ZSD   OCCURS   5.
               04  TSKW-HTS   PIC S9(009).
               04  TSKW-SZS   PIC S9(007).
               04  TSKW-ZNGP  PIC  9(008).
           02  TSKW-KKD.
             03  TSKW-HTN     PIC S9(009).
             03  TSKW-SZN     PIC S9(007).
             03  TSKW-HTC     PIC S9(007).
             03  TSKW-SZC     PIC S9(005).
             03  TSKW-HTU     PIC S9(009).
             03  TSKW-SZU     PIC S9(007).
             03  TSKW-KNGP    PIC  9(008).
           02  F              PIC  X(008).
           02  TSKW-TNC       PIC  9(002).
           02  TSKW-TNCD  REDEFINES TSKW-TNC.
             03  TSKW-TNC1    PIC  9(001).
             03  F            PIC  9(001).
           02  TSKW-BMC       PIC  9(001).
           02  TSKW-DCC       PIC  9(001).
           02  F              PIC  X(068).
       77  F                  PIC  X(001).
      *FD  SM-F
       01  SM-F_HKG245.
           02  SM-F_PNAME1    PIC  X(003) VALUE "SMF".
           02  F              PIC  X(001).
           02  SM-F_LNAME     PIC  X(011) VALUE "SM-F_HKG245".
           02  F              PIC  X(001).
           02  SM-F_KEY1      PIC  X(100) VALUE SPACE.
           02  SM-F_SORT      PIC  X(100) VALUE SPACE.
           02  SM-F_IDLST     PIC  X(100) VALUE SPACE.
           02  SM-F_RES       USAGE  POINTER.
       01  SM-R.
           02  SM-TCD         PIC  X(004).
           02  SM-DATE        PIC  9(008).
           02  SM-SZZ         PIC S9(009).
           02  SM-SZZZ        PIC S9(007).
           02  SM-SUK         PIC S9(009).
           02  SM-SUKZ        PIC S9(007).
           02  SM-STS         PIC S9(007).
           02  SM-STSZ        PIC S9(005).
           02  SM-SNK         PIC S9(009).
           02  SM-SNKZ        PIC S9(007).
           02  F              PIC  X(010).
           02  SM-DNO         PIC  9(006).
           02  SM-SP          PIC  9(002).
           02  SM-SK          PIC  9(001).
           02  SM-CHK         PIC  9(001).
           02  SM-TNC         PIC  9(002).
           02  F              PIC  X(004).
           02  SM-CCD         PIC  9(003).
           02  SM-PC          PIC  9(001).
       77  F                  PIC  X(001).
      *FD  SKDPF
       01  SKDPF_HKG245.
           02  SKDPF_PNAME1   PIC  X(005) VALUE "SKDPF".
           02  F              PIC  X(001).
           02  SKDPF_LNAME    PIC  X(012) VALUE "SKDPF_HKG245".
           02  F              PIC  X(001).
           02  SKDPF_KEY1     PIC  X(100) VALUE SPACE.
           02  SKDPF_SORT     PIC  X(100) VALUE SPACE.
           02  SKDPF_IDLST    PIC  X(100) VALUE SPACE.
           02  SKDPF_RES      USAGE  POINTER.
       01  SKDP-R.
           02  SKDP-KEY.
             03  SKDP-TCD     PIC  9(004).
             03  SKDP-DATE    PIC  9(008).
             03  SKDP-NGPD  REDEFINES SKDP-DATE.
               04  F          PIC  9(004).
               04  SKDP-GP    PIC  9(004).
             03  SKDP-DTC     PIC  9(001).
             03  SKDP-DNO     PIC  9(006).
             03  SKDP-HCD     PIC  9(006).
             03  SKDP-HCDD  REDEFINES SKDP-HCD.
               04  SKDP-KCD   PIC  X(005).
               04  F          PIC  X(001).
             03  SKDP-T       PIC S9(006)V9(02).
             03  SKDP-DC      PIC  9(001).
             03  SKDP-CSC     PIC  9(001).
           02  SKDP-SU        PIC S9(006)V9(02).
           02  SKDP-KIN       PIC S9(009).
           02  SKDP-SKD       PIC  9(008).
           02  SKDP-TNC       PIC  9(002).
           02  SKDP-BMC       PIC  9(001).
           02  SKDP-DCC       PIC  9(001).
           02  F              PIC  X(002).
           02  SKDP-TCD2      PIC  9(004).
           02  SKDP-CCD       PIC  9(003).
           02  SKDP-BI        PIC  N(024).
           02  SKDP-HNO       PIC  9(006).
           02  F              PIC  X(030).
           02  SKDP-SHZ       PIC S9(007).
           02  SKDP-KSU       PIC  9(003).
           02  SKDP-JCD       PIC  9(006).
           02  F              PIC  X(013).
           02  SKDP-SNO       PIC  9(006).
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
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　ナフコ・ワークマン　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　請求明細書　発行　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER.
             03  0106C-MID  PIC  9(004).
             03  FILLER     PIC  N(001) VALUE "年".
             03  0306C-MID  PIC Z9 .
             03  FILLER     PIC  N(001) VALUE "月".
             03  0506C-MID  PIC Z9 .
             03  FILLER     PIC  N(001) VALUE "日".
             03  FILLER     PIC  N(001) VALUE "分".
           02  FILLER  PIC  X(035) VALUE
                "一括発行=0 , 得意先別発行=1 .....  ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER  PIC  N(005) VALUE "（　再　）".
           02  FILLER  PIC  X(035) VALUE
                "                                   ".
       01  C-ACP.
           02  A-PEY   PIC  9(002).
           02  A-SEN   PIC  9(001).
           02  A-TCD   PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-THM.
             03  FILLER.
               04  FILLER  PIC  X(011) VALUE "ｺｰﾄﾞ       ".
               04  FILLER  PIC  N(004) VALUE "得意先名".
               04  FILLER  PIC  X(052) VALUE
                "                                                    ".
             03  FILLER  PIC  X(009) VALUE "終了=ｆ･9".
           02  D-NAME  PIC  N(026).
           02  D-THMC.
             03  FILLER.
               04  FILLER  PIC  X(011) VALUE "           ".
               04  FILLER  PIC  X(008) VALUE "        ".
               04  FILLER  PIC  X(052) VALUE
                "                                                    ".
             03  FILLER  PIC  X(009) VALUE "         ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(019) VALUE
                  "***  ｶﾚﾝﾀﾞｰ ﾅｼ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  ﾄｸｲｻｷ ﾅｼ  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  HKBM ﾅｼ  ***".
             03  E-ME5   PIC  X(026) VALUE
                  "***  SKDF REWRITE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(026) VALUE
                  "***  HKBM REWRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(023) VALUE
                  "***  SMF WRITE ｴﾗｰ  ***".
             03  E-ME8   PIC  X(017) VALUE
                  "***  TSKF ﾅｼ  ***".
             03  E-ME9   PIC  X(026) VALUE
                  "***  TSKF REWRITE ｴﾗｰ  ***".
             03  E-ME10  PIC  X(017) VALUE
                  "***  SKDF ﾅｼ  ***".
             03  E-ME11  PIC  X(023) VALUE
                  "***  SKDF ﾃﾞｰﾀ ｴﾗｰ  ***".
             03  E-ME12  PIC  X(018) VALUE
                  "***  ﾊｯｺｳ ｽﾞﾐ  ***".
             03  E-ME15  PIC  X(024) VALUE
                  "***  HKBM WRITE ｴﾗｰ  ***".
             03  E-ME28  PIC  X(024) VALUE
                  "***  TUKF WRITE ｴﾗｰ  ***".
             03  E-ME29  PIC  X(026) VALUE
                  "***  TUKF REWRITE ｴﾗｰ  ***".
             03  E-ME30  PIC  X(017) VALUE
                  "***  TUKF ﾅｼ  ***".
             03  E-ME50  PIC  X(024) VALUE
                  "***  ﾀﾞｲﾁｮｳNO ｵｰﾊﾞｰ  ***".
             03  E-TCD   PIC  9(004).
             03  E-DNO   PIC  9(006).
             03  E-KEY   PIC  X(020).
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
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "253" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "20" "36" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "20" "36" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "20" "36" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "20" "36" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "20" "36" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" " " "10" "0" "16" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0106C-MID" "9" "10" "29" "4" " " "06C-MID" RETURNING RESU.
       CALL "SD_From" USING 
            "0106C-MID" BY REFERENCE W-NEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "0206C-MID" "N" "10" "33" "2" "0106C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
          "0306C-MID" "Z9" "10" "36" "2" "0206C-MID" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0306C-MID" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "0406C-MID" "N" "10" "38" "2" "0306C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
          "0506C-MID" "Z9" "10" "41" "2" "0406C-MID" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0506C-MID" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "0606C-MID" "N" "10" "43" "2" "0506C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "0706C-MID" "N" "10" "46" "2" "0606C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "X" "14" "20" "35" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "22" "27" "22" "07C-MID" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "45" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "N" "7" "32" "10" " " "C-MID1"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID1" "X" "14" "20" "35" "01C-MID1" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "8" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PEY" "9" "10" "41" "2" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "14" "54" "1" "A-PEY" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "17" "9" "4" "A-SEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "44" "1" "A-TCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "212" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-THM" " " "0" "0" "80" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-THM" " " "17" "0" "71" " " "D-THM"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-THM" "X" "17" "4" "11" " " "01D-THM" RETURNING RESU.
       CALL "SD_Init" USING 
           "0201D-THM" "N" "17" "15" "8" "0101D-THM" " " RETURNING RESU.
       CALL "SD_Init" USING 
          "0301D-THM" "X" "17" "24" "52" "0201D-THM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-THM" "X" "18" "6" "9" "01D-THM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "17" "24" "52" "D-THM" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-THMC" " " "0" "0" "80" "D-NAME" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-THMC" " " "17" "0" "71" " " "D-THMC"  RETURNING RESU.
       CALL "SD_Init" USING 
           "0101D-THMC" "X" "17" "4" "11" " " "01D-THMC" RETURNING RESU.
       CALL "SD_Init" USING 
         "0201D-THMC" "X" "17" "15" "8" "0101D-THMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
        "0301D-THMC" "X" "17" "24" "52" "0201D-THMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-THMC" "X" "18" "6" "9" "01D-THMC" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "392" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "392" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "19" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "26" "E-ME4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "26" "E-ME5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "23" "E-ME6" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "17" "E-ME7" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "26" "E-ME8" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "17" "E-ME9" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "23" "E-ME10" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "18" "E-ME11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME15" "X" "24" "15" "24" "E-ME12" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME28" "X" "24" "15" "24" "E-ME15" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME29" "X" "24" "15" "26" "E-ME28" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME30" "X" "24" "15" "17" "E-ME29" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME50" "X" "24" "15" "24" "E-ME30" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TCD" "9" "24" "45" "4" "E-ME50" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE TSKW-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-DNO" "9" "24" "50" "6" "E-TCD" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-DNO" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "45" "20" "E-DNO" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE SKD-KEY "20" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU
               GO TO M-030
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           MOVE HKB-SKN TO W-SNO W-SNOD.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
       M-030.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO TSKWF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TSKWF_PNAME1 " " BY REFERENCE TSKWF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
      *
      *           READ TSKWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TSKWF_PNAME1 BY REFERENCE TSKW-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TSKWF_IDLST TSKWF_PNAME1
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           MOVE TSKW-KNGP TO W-NGP.
           CALL "DB_F_Close" USING
            BY REFERENCE TSKWF_IDLST TSKWF_PNAME1.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU
           END-IF
           IF  JS-SIGN = 1
               GO TO M-120
           END-IF
           GO TO M-050.
       M-040.
           CALL "SD_Accept" USING BY REFERENCE A-PEY "A-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-980
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF.
       M-050.
           CALL "DB_F_Open" USING
            "INPUT" CALNM_PNAME1 "SHARED" BY REFERENCE CALNM_IDLST "1"
            "CL-KEY" BY REFERENCE CL-KEY.
           MOVE W-NGP TO CL-KEY.
      *           READ CALNM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" CALNM_PNAME1 BY REFERENCE CALN-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE CALNM_IDLST CALNM_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-040
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE CALNM_IDLST CALNM_PNAME1.
       M-080.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  JS-SIGN = 0
                   GO TO M-040
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-080
           END-IF
           IF  W-SEN > 1
               GO TO M-080
           END-IF
           IF  W-SEN = 0
               CALL "SD_Output" USING "D-THMC" D-THMC "p" RETURNING RESU
               GO TO M-120
           END-IF
           CALL "SD_Output" USING "D-THM" D-THM "p" RETURNING RESU.
       M-100.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
               GO TO M-980
           END-IF
           IF  ESTAT = BTB
               IF  W-STC = 0
                   GO TO M-080
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-100
           END-IF
      *
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-100
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           IF  W-TCD NOT = 5000 AND 9850
               GO TO M-100
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
           MOVE W-TCD TO TSK-KEY.
      *           READ TSKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TSKF_IDLST TSKF_PNAME1
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-100
           END-IF
           IF  W-NGP = TSK-ZNGP(4) OR TSK-ZNGP(5)
               CALL "DB_F_Close" USING
                BY REFERENCE TSKF_IDLST TSKF_PNAME1
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-100
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
       M-120.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  JS-SIGN = 1
                   GO TO M-120
               ELSE
                   IF  W-SEN = 1
                       GO TO M-100
                   ELSE
                       GO TO M-080
                   END-IF
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-120
           END-IF
           IF  W-DMM = 9
               IF  JS-SIGN = 1
                   GO TO M-120
               ELSE
                   GO TO M-040
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-120
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" TSKWF_PNAME1 " " BY REFERENCE TSKWF_IDLST "0".
       M-140.
      *           READ TSKWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TSKWF_PNAME1 BY REFERENCE TSKW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-160
           END-IF
           IF  W-SEN = 1
               IF  TSKW-TCD NOT = W-TCD
                   GO TO M-140
               END-IF
           END-IF.
       M-150.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           IF  JS-SIGN = 0
               CALL "DB_F_Open" USING
                "I-O" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
                "TSK-KEY" BY REFERENCE TSK-KEY
               CALL "DB_F_Open" USING
                "I-O" TUKF_PNAME1 "SHARED" BY REFERENCE TUKF_IDLST "2"
                "TUK-KEY" BY REFERENCE TUK-KEY "TUK-KEY2" BY REFERENCE
                TUK-KEY2
               CALL "DB_F_Open" USING
                "EXTEND" SM-F_PNAME1 " " BY REFERENCE SM-F_IDLST "0"
           END-IF
           GO TO M-180.
       M-160.
           CALL "DB_F_Close" USING
            BY REFERENCE TSKWF_IDLST TSKWF_PNAME1.
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  W-SEN = 1
               GO TO M-100
           END-IF
           GO TO M-980.
       M-180.
           IF  W-STC = 0
               MOVE 1 TO W-STC
           END-IF
           IF  W-SEN = 1
               GO TO M-200
           END-IF
           MOVE TSKW-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               INITIALIZE T-R
               MOVE SPACE TO T-NAME T-SNA
           END-IF.
       M-200.
           MOVE ZERO TO W-KEI W-PAGE W-DC.
           IF  JS-SIGN = 0
               IF  TSKW-ZNGP(4) NOT = ZERO
                   MOVE 5 TO W-DC
                   COMPUTE W-ZSK = TSKW-HTS(4) + TSKW-SZS(4)
                   MOVE TSKW-HTS(4) TO W-HTZ
                   MOVE TSKW-SZS(4) TO W-SZZ
               ELSE
                   MOVE 4 TO W-DC
                   COMPUTE W-ZSK = TSKW-HTS(3) + TSKW-SZS(3)
                   MOVE TSKW-HTS(3) TO W-HTZ
                   MOVE TSKW-SZS(3) TO W-SZZ
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  TSKW-ZNGP(2) = TSKW-KNGP
                   COMPUTE W-ZSK = TSKW-HTS(1) + TSKW-SZS(1)
                   MOVE TSKW-HTS(2) TO W-HTZ
                   MOVE TSKW-SZS(2) TO W-SZZ
               ELSE
                   IF  TSKW-ZNGP(3) = TSKW-KNGP
                       COMPUTE W-ZSK = TSKW-HTS(2) + TSKW-SZS(2)
                       MOVE TSKW-HTS(3) TO W-HTZ
                       MOVE TSKW-SZS(3) TO W-SZZ
                   END-IF
               END-IF
           END-IF
           COMPUTE W-ZNK = TSKW-HTN + TSKW-SZN.
           COMPUTE W-CSK = TSKW-HTC + TSKW-SZC.
           COMPUTE W-KKZ = W-ZSK - W-ZNK + W-CSK.
           COMPUTE W-SKK = W-ZSK - W-ZNK + W-CSK + TSKW-HTU + TSKW-SZU.
           COMPUTE W-HTS = W-HTZ - TSKW-HTN + TSKW-HTC + TSKW-HTU.
           COMPUTE W-SZS = W-SZZ - TSKW-SZN + TSKW-SZC + TSKW-SZU.
           IF  JS-SIGN = 1
               PERFORM SNO-RTN THRU SNO-EX
           ELSE
               ADD 1 TO W-SNO
               IF  W-SNO = 999999
                   MOVE 1 TO W-SNO
               END-IF
           END-IF
      *
           MOVE W-NEN TO H-NEN.
           MOVE W-GET TO H-GET.
           MOVE W-PEY TO H-PEY.
           MOVE W-SNO TO H-SNO.
           MOVE "-" TO H-V.
           MOVE TSKW-TCD TO H-TCD.
           MOVE TSKW-TNC1 TO H-TNC1.
           MOVE "-" TO H-DV.
           MOVE TSKW-DCC TO H-DCC.
           IF  T-SNA NOT = SPACE
               MOVE T-SNA TO H-TNA
           ELSE
               MOVE T-NAME TO H-TNA
           END-IF
           MOVE W-ZSK TO H-ZSK.
           MOVE W-ZNK TO H-ZNK.
           MOVE W-CSK TO H-CSK.
           MOVE W-KKZ TO H-KKZ.
           MOVE TSKW-HTU TO H-URK.
           MOVE TSKW-SZU TO H-SHZ.
           MOVE W-SKK TO H-SKK.
           IF  W-POC = 0
               MOVE 1 TO W-POC
               CALL "PR_Open" RETURNING RESP
               PERFORM MID-020 THRU MID-EX
           ELSE
               PERFORM MID-RTN THRU MID-EX
           END-IF
      *
           IF  JS-SIGN = 0
               CALL "DB_F_Open" USING
                "I-O" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
                "SKD-KEY" BY REFERENCE SKD-KEY
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" SKDPF_PNAME1 " " BY REFERENCE SKDPF_IDLST "1"
            "SKDP-KEY" BY REFERENCE SKDP-KEY.
           MOVE SPACE TO SKDP-KEY.
           MOVE TSKW-TCD TO SKDP-TCD.
      *           START SKDPF KEY NOT < SKDP-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SKDPF_PNAME1 "SKDP-KEY" " NOT < " SKDP-KEY RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-400
           END-IF.
       M-220.
      *           READ SKDPF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDPF_PNAME1 BY REFERENCE SKDP-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-400
           END-IF
           IF  TSKW-TCD NOT = SKDP-TCD
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-400
           END-IF
           IF  TSKW-KNGP NOT = SKDP-SKD
               GO TO M-220
           END-IF.
       M-240.
           MOVE ZERO TO W-RD W-C W-GC.
           MOVE SPACE TO WR-BI(1) WR-BI(2) WR-BI(3)
                         WR-BI(4) WR-BI(5) WR-BI(6) WR-BI(7) WR-BI(8).
           MOVE SKDP-DNO TO W-DNO.
           IF (SKDP-DTC = 3 OR 5) OR (SKDP-CSC = 9) OR (SKDP-DC = 8)
               GO TO M-260
           END-IF
           IF  SKDP-CCD > 001
               ADD 1 TO W-GC
           END-IF
           IF  SKDP-BI NOT = SPACE
               ADD 1 TO W-GC
           END-IF
           IF  SKDP-HNO NOT = ZERO
               ADD 1 TO W-GC
               GO TO M-260
           END-IF
           IF (SKDP-CCD > 001) OR (SKDP-BI NOT = SPACE)
               GO TO M-260
           END-IF
           ADD 1 TO W-GC.
       M-260.
           ADD 1 TO W-C.
           IF  W-C > 8
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-300
           END-IF
           MOVE SKDP-R TO W-R(W-C).
           ADD 1 TO W-GC.
       M-280.
      *           READ SKDPF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDPF_PNAME1 BY REFERENCE SKDP-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-360
           END-IF
           IF  TSKW-TCD NOT = SKDP-TCD
               GO TO M-360
           END-IF
           IF  TSKW-KNGP NOT = SKDP-SKD
               GO TO M-280
           END-IF
           IF  SKDP-DNO = W-DNO
               GO TO M-260
           END-IF.
       M-300.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  W-ZNC = 0
               PERFORM ETC-RTN THRU ETC-EX
           END-IF
           IF  JS-SIGN = 0
               PERFORM SKD-RTN THRU SKD-EX
           END-IF
           IF  COMPLETION_CODE = 255
               GO TO M-400
           END-IF
           IF  JS-SIGN = 0
               IF  W-DNO NOT = ZERO
                   PERFORM TUK2-RTN THRU TUK2-EX
               END-IF
           END-IF
           IF  COMPLETION_CODE = 255
               GO TO M-400
           END-IF
           GO TO M-240.
       M-360.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  W-ZNC = 0
               PERFORM ETC-RTN THRU ETC-EX
           END-IF
           IF  JS-SIGN = 0
               PERFORM SKD-RTN THRU SKD-EX
           END-IF
           IF  COMPLETION_CODE = 255
               GO TO M-400
           END-IF
           IF  JS-SIGN = 0
               IF  W-DNO NOT = ZERO
                   PERFORM TUK2-RTN THRU TUK2-EX
               END-IF
           END-IF.
       M-400.
           CALL "DB_F_Close" USING
            BY REFERENCE SKDPF_IDLST SKDPF_PNAME1.
           IF  JS-SIGN = 0
               CALL "DB_F_Close" USING
                BY REFERENCE SKDF_IDLST SKDF_PNAME1
           END-IF
           IF  COMPLETION_CODE = 255
               GO TO M-900
           END-IF
           IF  JS-SIGN = 0
               PERFORM TSK-RTN THRU TSK-EX
           END-IF
           IF  COMPLETION_CODE = 255
               GO TO M-900
           END-IF
           IF  JS-SIGN = 0
               PERFORM SMW-RTN THRU SMW-EX
           END-IF
           IF  COMPLETION_CODE = 255
               GO TO M-900
           END-IF
           IF  JS-SIGN = 0
               PERFORM TUK1-RTN THRU TUK1-EX
           END-IF
           IF  COMPLETION_CODE = 255
               GO TO M-900
           END-IF
           IF  W-SEN NOT = 1
               GO TO M-420
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE TSKWF_IDLST TSKWF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TUKF_IDLST TUKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SM-F_IDLST SM-F_PNAME1.
           MOVE 0 TO W-POC.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "D-THM" D-THM "p" RETURNING RESU.
           GO TO M-100.
       M-420.
      *           READ TSKWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TSKWF_PNAME1 BY REFERENCE TSKW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-900
           END-IF
           GO TO M-180.
       M-900.
           IF  W-POC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           IF  W-SNO = W-SNOD
               GO TO M-980
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           IF  JS-SIGN = 0
               CALL "DB_F_Close" USING
                BY REFERENCE TSKF_IDLST TSKF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TUKF_IDLST TUKF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE SM-F_IDLST SM-F_PNAME1
           END-IF
           IF  W-SEN NOT = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TSKWF_IDLST TSKWF_PNAME1
           END-IF.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *･････････････････････････････････････････････････････････････････
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
           IF  W-PAGE = 1
               MOVE HEAD3 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD4 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD5 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           MOVE HEAD6 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
       MEI-RTN.
           IF (WR-DTC(1) = 3 OR 5) OR (WR-CSC(1) = 9) OR (WR-DC(1) = 8)
               MOVE 1 TO W-ZNC
               GO TO MEI-EX
           END-IF
           MOVE ZERO TO W-C W-TKIN W-ZNC.
       MEI-002.
           ADD 1 TO W-C.
           IF  W-C > 8
               GO TO MEI-EX
           END-IF
           IF  WR-TCD(W-C) = ZERO
               GO TO MEI-EX
           END-IF
           MOVE WR-GP(W-C) TO W-GPD.
           MOVE WR-CCD(W-C) TO W-CCD.
           IF  W-CCD < 002
               GO TO MEI-010
           END-IF
           MOVE WR-TCD2(W-C) TO TC-TCD.
           MOVE W-CCD TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
           END-IF.
       MEI-010.
           MOVE WR-BI(W-C) TO W-BI.
           MOVE WR-HNO(W-C) TO W-HNO.
      *
           IF  WR-HCD(W-C) = ZERO
               GO TO MEI-060
           END-IF
           MOVE WR-HCD(W-C) TO HI-HCD.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
           END-IF.
       MEI-060.
           IF  WR-DTC(W-C) = 1
               MOVE WR-SU(W-C) TO W-SU
               COMPUTE W-TN = WR-T(W-C) * -1
               COMPUTE W-KIN = WR-KIN(W-C) * -1
               COMPUTE W-SHZ = WR-SHZ(W-C) * -1
           ELSE
               IF  WR-DC(W-C) = 1 OR 2 OR 5
                   COMPUTE W-SU = WR-SU(W-C) * -1
                   MOVE WR-T(W-C) TO W-TN
                   COMPUTE W-KIN = WR-KIN(W-C) * -1
                   MOVE WR-SHZ(W-C) TO W-SHZ
               ELSE
                   MOVE WR-SU(W-C) TO W-SU
                   MOVE WR-T(W-C) TO W-TN
                   MOVE WR-KIN(W-C) TO W-KIN
                   MOVE WR-SHZ(W-C) TO W-SHZ
               END-IF
           END-IF
      *
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 64
               PERFORM MID-RTN THRU MID-EX.
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-KBN1 P-HNA.
           IF  W-C = 1
               MOVE W-GETD TO P-GET1
               MOVE "/" TO P-S1
               MOVE W-PEYD TO P-PEY1
               MOVE W-DNO TO P-DNO1
           END-IF
           IF  WR-DTC(W-C) = 0
               MOVE "売上" TO P-KBN1
           END-IF
           IF  WR-DTC(W-C) = 1
               MOVE "値引" TO P-KBN1
           END-IF
           IF  WR-DC(W-C) = 1
               MOVE "返品" TO P-KBN1
           END-IF
           IF  WR-DC(W-C) = 2
               MOVE "不良" TO P-KBN1
           END-IF
           IF  WR-DC(W-C) = 3
               MOVE "預売" TO P-KBN1
           END-IF
           IF  WR-DC(W-C) = 4
               MOVE "預出" TO P-KBN1
           END-IF
           IF  WR-HCD(W-C) NOT = ZERO
               MOVE HI-NAME TO P-HNA
           END-IF
           IF  WR-HCD(W-C) NOT = ZERO
               MOVE WR-HCD(W-C) TO P-HCD
           END-IF
           IF  WR-HCD(W-C) NOT = ZERO
               MOVE W-SU TO P-SU
           END-IF
           IF  WR-DC(W-C) = 4 OR 8
               GO TO MEI-080
           END-IF
           MOVE W-KIN TO P-KIN1.
           IF  WR-HCD(W-C) NOT = ZERO
               MOVE W-TN TO P-T
           END-IF.
       MEI-080.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  WR-DC(W-C) NOT = 4 AND 8
               ADD W-KIN TO W-TKIN
           END-IF
           GO TO MEI-002.
       MEI-EX.
           EXIT.
       ETC-RTN.
           MOVE 0 TO W-KC.
           IF  W-CCD < 002
               GO TO ETC-040
           END-IF
           MOVE SPACE TO W-NAME.
           MOVE TC-NAME TO W-NAME.
           MOVE 27 TO CNT.
       ETC-020.
           SUBTRACT 1 FROM CNT.
           IF  CNT NOT = ZERO
               IF  W-NA(CNT) = SPACE
                   GO TO ETC-020
               ELSE
                   ADD 1 TO CNT
                   MOVE "様" TO W-NA(CNT)
               END-IF
           END-IF
      *
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 64
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-KBN2 P-BI P-HM2 P-TM2.
           MOVE "直送" TO P-KBN2.
           MOVE W-NAME TO P-BI.
           IF  W-HNO NOT = ZERO
               MOVE "発送" TO P-HM2
               MOVE ":" TO P-C2
               MOVE W-HNO TO P-HNO2
           END-IF
           MOVE "（計）　" TO P-TM2.
           MOVE W-TKIN TO P-KIN2.
           MOVE W-SHZ TO P-SHZ2.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE 1 TO W-KC.
       ETC-040.
           IF  W-KC NOT = 0
               IF  W-BI = SPACE
                   IF  W-HNO = ZERO
                       GO TO ETC-EX
                   END-IF
               END-IF
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 64
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-KBN2 P-BI P-HM2 P-TM2.
           IF  W-BI NOT = SPACE
               MOVE "摘要" TO P-KBN2
               MOVE SPACE TO W-NAME
               MOVE W-BI TO W-NAME
               MOVE W-NAME TO P-BI
           END-IF
           IF  W-KC = 0
               MOVE 1 TO W-KC
               MOVE "（計）　" TO P-TM2
               MOVE W-TKIN TO P-KIN2
               MOVE W-SHZ TO P-SHZ2
               IF  W-HNO NOT = ZERO
                   MOVE "発送" TO P-HM2
                   MOVE ":" TO P-C2
                   MOVE W-HNO TO P-HNO2
               END-IF
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       ETC-EX.
           EXIT.
       SKD-RTN.
           MOVE SPACE TO SKD-KEY.
           MOVE W-TCD TO SKD-TCD.
           MOVE WR-DATE(1) TO SKD-DATE.
           MOVE WR-DTC(1) TO SKD-DTC.
           MOVE WR-DNO(1) TO SKD-DNO.
      *           START SKDF KEY NOT < SKD-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SKDF_PNAME1 "SKD-KEY" " NOT < " SKD-KEY RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SKD-EX
           END-IF
           MOVE 0 TO W-SKDC.
       SKD-020.
      *           READ SKDF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO SKD-080
           END-IF
           IF  SKD-TCD NOT = W-TCD
               GO TO SKD-080
           END-IF
           IF  SKD-SKD NOT = TSKW-KNGP
               GO TO SKD-020
           END-IF
           IF  SKD-DNO NOT = W-DNO
               GO TO SKD-020
           END-IF
           MOVE W-SNO TO SKD-SNO.
      *           REWRITE SKD-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SKDF_PNAME1 SKDF_LNAME SKD-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SKD-EX
           END-IF
           IF  SKD-DATE NOT = W-DATE
               MOVE SKD-DATE TO W-DATE
           END-IF
           IF  W-SKDC = 0
               MOVE 1 TO W-SKDC
           END-IF
           GO TO SKD-020.
       SKD-080.
           IF  W-SKDC = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       SKD-EX.
           EXIT.
       TUK2-RTN.
           MOVE SPACE TO TUK-KEY2.
           MOVE W-TCD TO TUK-TCD2.
           MOVE WR-DATE(1) TO TUK-DATE.
           IF  WR-DTC(1) = 0
               MOVE 1 TO TUK-DC
           ELSE
               IF  WR-DTC(1) = 1
                   MOVE 2 TO TUK-DC
               ELSE
                   IF  WR-DTC(1) = 3
                       MOVE 3 TO TUK-DC
                   ELSE
                       IF  WR-DTC(1) = 5
                           MOVE 4 TO TUK-DC
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE WR-DNO(1) TO TUK-DNO.
      *           START TUKF KEY NOT < TUK-KEY2 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TUKF_PNAME1 "TUK-KEY2" " NOT < " TUK-KEY2 RETURNING RET.
           IF  RET = 1
               GO TO TUK2-080
           END-IF.
       TUK2-020.
      *           READ TUKF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TUKF_PNAME1 BY REFERENCE TUK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO TUK2-080
           END-IF
           IF  TUK-TCD NOT = W-TCD
               GO TO TUK2-080
           END-IF
           IF  TUK-DATE NOT = W-DATE
               GO TO TUK2-020
           END-IF
           IF  TUK-DNO NOT = W-DNO
               GO TO TUK2-020
           END-IF
           MOVE W-NGP TO TUK-SKD.
      *           REWRITE TUK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TUKF_PNAME1 TUKF_LNAME TUK-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME29" E-ME29 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DNO" E-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO TUK2-EX.
       TUK2-080.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "SD_Output" USING "E-ME30" E-ME30 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-TCD" E-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "E-DNO" E-DNO "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
       TUK2-EX.
           EXIT.
       TSK-RTN.
           MOVE W-TCD TO TSK-KEY.
      *           READ TSKF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TSK-EX
           END-IF
           IF  W-DC = 4
               MOVE W-HTS TO TSK-HTS(4)
               MOVE W-SZS TO TSK-SZS(4)
               MOVE TSKW-KNGP TO TSK-ZNGP(4)
           ELSE
               MOVE W-HTS TO TSK-HTS(5)
               MOVE W-SZS TO TSK-SZS(5)
               MOVE TSKW-KNGP TO TSK-ZNGP(5)
           END-IF
      *           REWRITE TSK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TSKF_PNAME1 TSKF_LNAME TSK-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TSK-EX
           END-IF.
       TSK-EX.
           EXIT.
       SMW-RTN.
           INITIALIZE SM-R.
           MOVE W-TCD TO SM-TCD.
           MOVE W-NGP TO SM-DATE.
           MOVE W-HTZ TO SM-SZZ.
           MOVE W-SZZ TO SM-SZZZ.
           MOVE TSKW-HTU TO SM-SUK.
           MOVE TSKW-SZU TO SM-SUKZ.
           MOVE TSKW-HTC TO SM-STS.
           MOVE TSKW-SZC TO SM-STSZ.
           MOVE TSKW-HTN TO SM-SNK.
           MOVE TSKW-SZN TO SM-SNKZ.
           MOVE W-SNO TO SM-DNO.
           MOVE TSKW-TNC TO SM-TNC.
      *           WRITE SM-R.
      *//////////////
           CALL "DB_Insert" USING
            SM-F_PNAME1 SM-F_LNAME SM-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO SMW-EX
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME7" E-ME7 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SMW-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE SM-F_IDLST SM-F_PNAME1.
           MOVE "SMF          " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" SM-F_PNAME1 " " BY REFERENCE SM-F_IDLST "0".
           GO TO SMW-RTN.
       SMW-EX.
           EXIT.
       TUK1-RTN.
           CALL "DB_F_Open" USING
            "I-O" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
      *           READ HKBM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO TUK1-020
           END-IF
           MOVE HKB-DAI TO W-DAI.
           GO TO TUK1-040.
       TUK1-020.
           INITIALIZE HKB-R.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
      *           WRITE HKB-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TUK1-EX
           END-IF
           INITIALIZE W-DAI.
       TUK1-040.
           COPY LDAIP.
           IF  W-DAI1 = "999"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME50" E-ME50 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           MOVE W-SNO TO HKB-SKN.
           MOVE W-DAI TO HKB-DAI.
      *           REWRITE HKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
       TUK1-060.
           INITIALIZE TUK-R.
           MOVE W-TCD TO TUK-TCD TUK-TCD2.
           MOVE W-DAI TO TUK-DAI.
           MOVE W-NGP TO TUK-DATE.
           MOVE 4 TO TUK-DC.
           MOVE W-HTS TO TUK-KIN.
           MOVE W-SZS TO TUK-SHZ.
           MOVE W-SNO TO TUK-DNO.
           MOVE 0 TO TUK-GNO.
           MOVE T-TNC TO TUK-TNC.
           MOVE T-DCC TO TUK-DCC.
           MOVE T-BC TO TUK-BMC.
      *           WRITE TUK-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TUKF_PNAME1 TUKF_LNAME TUK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME28" E-ME28 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TUK1-080
           END-IF
           GO TO TUK1-EX.
       TUK1-080.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TUK1-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE TUKF_IDLST TUKF_PNAME1.
           MOVE "TUKF         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TUKF_PNAME1 "SHARED" BY REFERENCE TUKF_IDLST "2"
            "TUK-KEY" BY REFERENCE TUK-KEY "TUK-KEY2" BY REFERENCE
            TUK-KEY2.
           GO TO TUK1-060.
       TUK1-EX.
           EXIT.
       SNO-RTN.
           MOVE ZERO TO W-SNO W-SNOD.
           CALL "DB_F_Open" USING
            "INPUT" SKDPF_PNAME1 " " BY REFERENCE SKDPF_IDLST "1"
            "SKDP-KEY" BY REFERENCE SKDP-KEY.
      *           READ SKDPF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDPF_PNAME1 BY REFERENCE SKDP-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO SNO-010
           END-IF
           MOVE SKDP-SNO TO W-SNO.
       SNO-010.
           CALL "DB_F_Close" USING
            BY REFERENCE SKDPF_IDLST SKDPF_PNAME1.
       SNO-EX.
           EXIT.

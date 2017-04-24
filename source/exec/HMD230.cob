       IDENTIFICATION   DIVISION.
       PROGRAM-ID. HMD230.
      *********************************************************
      *    PROGRAM         :  売上・値引伝票発行  JS-SIGN=0   *
      *                    :              再発行  JS-SIGN=1   *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    BASE PROGRAM    :  HMD220                          *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  JS-SIGN            PIC  X(001).
       77  W-DCHK             PIC  9(001).
       77  W-NCHK             PIC  9(001).
       77  W-DZC              PIC  9(001) VALUE 0.
       77  W-FILE             PIC  X(013).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  N(005) VALUE "《　日付　".
           02  F              PIC  X(001) VALUE "'".
           02  H-NEN          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-GET          PIC  Z(002).
           02  F              PIC  N(001) VALUE "月".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-PEY          PIC  Z(002).
           02  F              PIC  N(003) VALUE "日　》".
           02  F              PIC  X(020) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  N(009) VALUE "【　売　上　伝　票".
           02  F              PIC  N(002) VALUE "　】".
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  X(005) VALUE SPACE.
           02  H-SHC          PIC  N(003) VALUE SPACE.
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  N(002) VALUE "伝区".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  X(001) VALUE "(".
           02  F              PIC  N(003) VALUE "伝票№".
           02  F              PIC  X(001) VALUE ")".
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(008) VALUE "得　意　先　名　".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  X(051) VALUE
                "1             SS    S    M    L   LL 28.0 29.0 30.0".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  N(004) VALUE "請求日　".
           02  F              PIC  X(002) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(051) VALUE
                "2 12.5 13.0 13.5 14.0 15.0 16.0 17.0 18.0 19.0 20.0".
           02  F              PIC  X(035) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(050) VALUE SPACE.
           02  F              PIC  X(051) VALUE
                "3 21.0 21.5 22.0 22.5 23.0 23.5 24.0 24.5 25.0     ".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(002) VALUE "備考".
           02  F              PIC  X(007) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(050) VALUE SPACE.
           02  F              PIC  X(051) VALUE
                "4 24.0 24.5 25.0 25.5 26.0 26.5 27.0 27.5          ".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "数量".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単価".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(002) VALUE "金額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　総合計".
       01  W-P1.
           02  P-15K1         PIC  X(005).
           02  P-DC           PIC  9(001).
           02  P-CSC          PIC  9(001).
           02  P-FRC          PIC  X(001).
           02  P-DCM          PIC  N(002).
           02  F              PIC  X(001).
           02  P-DCD          PIC  N(004).
           02  F              PIC  X(001).
           02  P-F            PIC  X(001).
           02  P-DNO          PIC  9(006).
           02  P-R            PIC  X(001).
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  P-X            PIC  X(001).
           02  P-CCD          PIC  9(003).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(052).
           02  P-MNGP         PIC 99/99/99.
       01  W-P2.
           02  P-15K2         PIC  X(005).
           02  F              PIC  X(006).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  9(001).
           02  P-20K2         PIC  X(005).
           02  P-SUD.
             03  P-SU    OCCURS  10  PIC  -(005).
           02  P-SUT          PIC ---,--9.
           02  P-T            PIC ---,--9.
           02  P-KIN          PIC --,---,--9.
           02  P-KIND  REDEFINES P-KIN.
             03  P-MSM        PIC  N(005).
           02  F              PIC  X(001).
           02  P-BIK          PIC  X(010).
       01  W-P3.
           02  P-15K3         PIC  X(005).
           02  F              PIC  X(007).
           02  P-KOSU         PIC  N(004).
           02  P-KO           PIC  N(002).
           02  F              PIC  X(004).
           02  P-CNAM         PIC  N(032).
           02  P-BID   REDEFINES P-CNAM.
             03  F            PIC  N(008).
             03  P-BI         PIC  N(024).
           02  F              PIC  X(004).
           02  P-NOM          PIC  N(006).
           02  F              PIC  X(001).
           02  P-HMN          PIC  9(006).
           02  F              PIC  X(007).
           02  P-TME          PIC  N(004).
           02  P-GSUT         PIC ---,--9.
           02  F              PIC  X(001).
           02  P-SZM          PIC  N(004).
           02  P-GKIN         PIC --,---,--9.
           02  P-TKIN         PIC ---,---,--9.
       01  W-P8.
           02  F              PIC  X(136).
       01  W-R.
           02  WR-S1.
             03  S1-DNO       PIC  9(006).
             03  S1-GNO       PIC  9(001).
             03  S1-DATE      PIC  9(008).
             03  S1-NGP   REDEFINES S1-DATE.
               04  S1-NG      PIC  9(006).
               04  S1-PEY     PIC  9(002).
             03  S1-TCD       PIC  9(004).
             03  S1-HCD       PIC  9(006).
             03  S1-SIZ       PIC  9(001).
             03  S1-ASU.
               04  S1-SUD   OCCURS 10.
                 05  S1-SU    PIC S9(004)   COMP-3.
             03  S1-SUT       PIC S9(005).
             03  S1-T         PIC  9(005).
             03  S1-KIN       PIC S9(008).
             03  S1-CSC       PIC  9(001).
             03  S1-DC        PIC  9(001).
             03  S1-FT        PIC  9(005).
             03  S1-CCD       PIC  9(003).
             03  F            PIC  X(011).
             03  S1-HSC       PIC  9(001).
             03  S1-KOSU      PIC  9(003).
             03  S1-FRC       PIC  9(001).
             03  S1-TCD2      PIC  9(004).
             03  S1-BIK       PIC  X(010).
             03  F            PIC  X(002).
             03  S1-MNGP      PIC  9(006).
             03  F            PIC  X(004).
             03  S1-DHC       PIC  9(001).
             03  S1-UNC       PIC  9(001).
           02  WR-S2    REDEFINES WR-S1.
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
             03  S2-UNC       PIC  9(001).
       01  W-DATA.
           02  W-PAGE         PIC  9(002).
           02  W-TD.
             03  W-ASUT       PIC S9(006).
             03  W-AKIN       PIC S9(008).
             03  W-SHZ        PIC S9(006).
             03  W-TKIN       PIC S9(008).
           02  W-D.
             03  W-TPC        PIC  9(001).
             03  W-GNO        PIC  9(001).
             03  W-L          PIC  9(002).
             03  W-LC         PIC  9(002).
             03  W-ASU.
               04  W-SU    OCCURS  10  PIC S9(004).
             03  W-SUT        PIC S9(005).
             03  W-KIN        PIC S9(008).
             03  W-KINZ       PIC S9(007).
             03  CNT          PIC  9(002).
             03  W-DC         PIC  9(001).
             03  W-BE         PIC  9(001).
             03  W-HSC        PIC  9(001).
             03  W-CSC        PIC  9(001).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  W-DNO          PIC  9(006).
           02  W-TCD          PIC  9(004).
           02  W-CCD          PIC  9(003).
           02  W-NGPD.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NEND  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-NGL   REDEFINES W-NG.
               04  F          PIC  9(002).
               04  W-NGS      PIC  9(004).
             03  W-PEY        PIC  9(002).
           02  W-SENGP.
             03  W-SNGP       PIC  9(008).
             03  W-SNGPD REDEFINES W-SNGP.
               04  W-SNG      PIC  9(006).
               04  F          PIC  9(002).
             03  W-ENGP       PIC  9(008).
             03  W-ENGPD REDEFINES W-ENGP.
               04  W-ENG      PIC  9(006).
               04  F          PIC  9(002).
           02  W-SETCD.
             03  W-STCD       PIC  9(004).
             03  W-ETCD       PIC  9(004) VALUE 9999.
           02  W-SEDNO.
             03  W-SDNO       PIC  9(006).
             03  W-EDNO       PIC  9(006) VALUE 999999.
           02  W-DMM          PIC  9(001).
           02  W-CCC          PIC  9(001).
           02  W-NAME         PIC  N(024).
           02  W-NAMED REDEFINES W-NAME.
             03  W-NAD   OCCURS  24.
               04  W-NA       PIC  N(001).
       01  ERR-STAT           PIC  X(002).
       01  W-AREA.
           02  W-KOSU         PIC ZZZZ.
           02  N              PIC  9(002).
           02  TBL-NAM        PIC  N(032).
           02  TBL-NAMR  REDEFINES TBL-NAM.
             03  T-NAM        PIC  N(001)  OCCURS  32.
           02  SV-KOSU        PIC  9(003).
           02  SV-TCD         PIC  9(004).
           02  SV-CCD2        PIC  9(003).
           COPY LNAMW.
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LITM.
           COPY LITCM.
           COPY LSPF.
      *FD  S-TRAN
       01  S-TRAN_HMD230.
           02  S-TRAN_PNAME1  PIC  X(005) VALUE "STRAN".
           02  F              PIC  X(001).
           02  S-TRAN_LNAME   PIC  X(013) VALUE "S-TRAN_HMD230".
           02  F              PIC  X(001).
           02  S-TRAN_KEY1    PIC  X(100) VALUE SPACE.
           02  S-TRAN_SORT    PIC  X(100) VALUE SPACE.
           02  S-TRAN_IDLST   PIC  X(100) VALUE SPACE.
           02  S-TRAN_RES     USAGE  POINTER.
       01  S-R                PIC  X(128).
       77  F                  PIC  X(001).
      *FD  STRANYR
       01  STRANYR_HMD230.
           02  STRANYR_PNAME1 PIC  X(007) VALUE "STRANYR".
           02  F              PIC  X(001).
           02  STRANYR_LNAME  PIC  X(014) VALUE "STRANYR_HMD230".
           02  F              PIC  X(001).
           02  STRANYR_KEY1   PIC  X(100) VALUE SPACE.
           02  STRANYR_SORT   PIC  X(100) VALUE SPACE.
           02  STRANYR_IDLST  PIC  X(100) VALUE SPACE.
           02  STRANYR_RES    USAGE  POINTER.
       01  STRANYR-R          PIC  X(128).
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　【ナフコ・ワークマンＡ４】　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　売上・返品・値引伝票　作成　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-DSP.
           02  D-SHM.
             03  FILLER  PIC  N(005) VALUE "（再発行）".
             03  FILLER  PIC  X(030) VALUE
                  "伝票№ 000000 より 999999 まで".
             03  FILLER  PIC  X(037) VALUE
                  "'00年00月00日 より '99年99月99日 まで".
             03  FILLER  PIC  X(030) VALUE
                  "得意先   0000 より 9999   まで".
       01  C-ACP.
           02  FILLER.
             03  A-SDNO  PIC  9(006).
             03  A-EDNO  PIC  9(006).
           02  FILLER.
             03  A-SNEN  PIC  9(002).
             03  A-SGET  PIC  9(002).
             03  A-SPEY  PIC  9(002).
             03  A-ENEN  PIC  9(002).
             03  A-EGET  PIC  9(002).
             03  A-EPEY  PIC  9(002).
           02  FILLER.
             03  A-STCD  PIC  9(004).
             03  A-ETCD  PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  EMSG-JS PIC  X(024) VALUE
                  "***  ｺﾝﾄﾛｰﾙ情報ｴﾗｰ   ***".
             03  E-ME3   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME4   PIC  X(034) VALUE
                  "***  ﾊｯｺｳｸﾌﾞﾝ ﾉ ﾄﾞｳｷｶﾞ ﾄﾚﾃｲﾅｲ  ***".
             03  E-ME5   PIC  X(026) VALUE
                  "***  STRAN ｴﾗｰ (OVER)  ***".
             03  E-ME6   PIC  X(024) VALUE
                  "***  STRAN ｴﾗｰ (NO)  ***".
             03  E-ME7   PIC  X(026) VALUE
                  "***  STRAN ｴﾗｰ (ﾋﾞｺｳ)  ***".
             03  E-ME10  PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME11  PIC  X(018) VALUE
                  "***  DATA ﾅｼ   ***".
             03  E-ME24  PIC  X(029) VALUE
                  "***   STRAN REWRITE ｴﾗｰ   ***".
             03  E-TCD   PIC  9(004).
             03  E-HCD   PIC  9(006).
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
            "C-MID" " " "0" "0" "344" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "2" "15" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "3" "15" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "4" "15" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "5" "15" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "6" "15" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "7" "15" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "8" "15" "46" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "16" "27" "22" "07C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "107" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SHM" " " "0" "0" "107" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-SHM" "N" "6" "33" "10" " " "D-SHM" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-SHM" "X" "10" "23" "30" "01D-SHM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03D-SHM" "X" "12" "23" "37" "02D-SHM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04D-SHM" "X" "14" "23" "30" "03D-SHM" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "33" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "10" "0" "12" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SDNO" "9" "10" "30" "6" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SDNO" BY REFERENCE W-SDNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EDNO" "9" "10" "42" "6" "A-SDNO" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EDNO" BY REFERENCE W-EDNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-ACP" " " "12" "0" "12" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SNEN" "9" "12" "24" "2" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SNEN" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SGET" "9" "12" "28" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-SGET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SPEY" "9" "12" "32" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-SPEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ENEN" "9" "12" "43" "2" "A-SPEY" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-ENEN" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EGET" "9" "12" "47" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EGET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EPEY" "9" "12" "51" "2" "A-EGET" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EPEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03C-ACP" " " "14" "0" "8" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-STCD" "9" "14" "32" "4" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ETCD" "9" "14" "42" "4" "A-STCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "16" "44" "1" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "222" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "222" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "EMSG-JS" "X" "24" "15" "24" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" "X" "24" "15" "16" "EMSG-JS" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME4" "X" "24" "15" "34" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME5" "X" "24" "15" "26" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME6" "X" "24" "15" "24" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME7" "X" "24" "15" "26" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME10" "X" "24" "15" "15" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME11" "X" "24" "15" "18" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME24" "X" "24" "15" "29" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-TCD" "9" "24" "50" "4" "E-ME24" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-TCD" BY REFERENCE S1-TCD "4" "0"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-HCD" "9" "24" "50" "6" "E-TCD" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-HCD" BY REFERENCE S1-HCD "6" "0"  RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN NOT = 0 AND 1
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "EMSG-JS" EMSG-JS "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  JS-SIGN = 0
               GO TO M-080
           END-IF
           CALL "SD_Output" USING "D-SHM" D-SHM "p" RETURNING RESU.
       M-040.
           CALL "SD_Accept" USING BY REFERENCE A-SDNO "A-SDNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF.
       M-060.
           CALL "SD_Accept" USING BY REFERENCE A-EDNO "A-EDNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-040
           END-IF
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF
           IF  W-SDNO > W-EDNO
               GO TO M-060
           END-IF.
       M-062.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-060
           END-IF
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-062
           END-IF.
       M-064.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-062
           END-IF
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-064
           END-IF.
       M-066.
           CALL "SD_Accept" USING BY REFERENCE A-SPEY "A-SPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-064
           END-IF
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-066
           END-IF
           IF  W-NEN2 = ZERO
               MOVE ZERO TO W-NEN1
           ELSE
               MOVE 20 TO W-NEN1
           END-IF
           MOVE W-NGPD TO W-SNGP.
           IF  W-NCHK = 0
               MOVE 1 TO W-NCHK
               MOVE 99 TO W-NEN2 W-GET W-PEY
           ELSE
               MOVE W-ENGP TO W-NGPD
           END-IF.
       M-068.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               MOVE W-SNGP TO W-NGPD
               GO TO M-066
           END-IF
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-068
           END-IF
           IF  W-NEN2 = ZERO
               GO TO M-068
           END-IF
           MOVE W-NGPD TO W-ENGP.
       M-070.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-068
           END-IF
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-070
           END-IF
           MOVE W-NGPD TO W-ENGP.
       M-072.
           CALL "SD_Accept" USING BY REFERENCE A-EPEY "A-EPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-070
           END-IF
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-072
           END-IF
           IF  W-NEN2 = ZERO
               MOVE ZERO TO W-NEN1
           ELSE
               MOVE 20 TO W-NEN1
           END-IF
           MOVE W-NGPD TO W-ENGP.
           IF  W-SNGP > W-ENGP
               GO TO M-068
           END-IF.
       M-074.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-072
           END-IF
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-074
           END-IF.
       M-076.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-074
           END-IF
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-076
           END-IF
           IF  W-STCD > W-ETCD
               GO TO M-076
           END-IF.
       M-080.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  JS-SIGN NOT = 0
               IF  ESTAT = BTB
                   GO TO M-076
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-080
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-080
           END-IF
           IF  JS-SIGN NOT = 0
               GO TO M-200
           END-IF.
       M-100.
           PERFORM CHK-RTN THRU CHK-EX.
           IF  W-DCHK = 0 OR 2
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DCHK = 3
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
       M-200.
           MOVE ZERO TO CHK W-NGPD W-PAGE.
           MOVE ALL "-" TO W-P8.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "PR_Open" RETURNING RESP.
           IF  JS-SIGN = 1
               CALL "DB_F_Open" USING
                "INPUT" STRANYR_PNAME1 " " BY REFERENCE
                STRANYR_IDLST "0"
               GO TO M-230
           END-IF
           CALL "DB_F_Open" USING
            "I-O" S-TRAN_PNAME1 " " BY REFERENCE S-TRAN_IDLST "0".
       M-220.
      *           READ S-TRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" S-TRAN_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-920
           END-IF
           MOVE S-R TO WR-S1.
           IF  S1-DHC NOT = 1
               GO TO M-235
           END-IF
           GO TO M-220.
       M-230.
      *           READ STRANYR AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" STRANYR_PNAME1 BY REFERENCE STRANYR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-920
           END-IF
           MOVE STRANYR-R TO WR-S1.
           IF  S1-NG < W-SNG
               GO TO M-230
           END-IF
           IF  S1-NG > W-ENG
               GO TO M-920
           END-IF
           IF  S1-DNO < W-SDNO OR > W-EDNO
               GO TO M-230
           END-IF
           IF  S1-DATE < W-SNGP OR > W-ENGP
               GO TO M-230
           END-IF
           IF  S1-TCD < W-STCD OR > W-ETCD
               GO TO M-230
           END-IF.
       M-235.
           IF  S1-DATE = W-NGPD
               GO TO M-240
           END-IF
           IF  W-NGPD NOT = ZERO
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           MOVE S1-DATE TO W-NGPD.
           MOVE W-NEN2 TO H-NEN.
           MOVE W-GET TO H-GET.
           MOVE W-PEY TO H-PEY.
           IF  JS-SIGN = 1
               MOVE "（再）" TO H-SHC
           END-IF
           ACCEPT H-DATE FROM DATE.
           PERFORM MID-010 THRU MID-EX.
       M-240.
           IF  W-DZC = 0
               MOVE 1 TO W-DZC
           END-IF
           MOVE ZERO TO W-TD W-D.
           MOVE S1-DNO TO W-DNO.
           MOVE S1-CSC TO W-CSC.
           MOVE S1-HSC TO W-HSC.
           MOVE S1-KOSU TO SV-KOSU.
           MOVE S1-TCD2 TO SV-TCD.
           MOVE S1-CCD TO SV-CCD2.
           MOVE S1-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
      *
           MOVE 0 TO W-CCC.
           IF  S1-CCD = ZERO OR 001
               GO TO M-300
           END-IF
           MOVE S1-TCD2 TO TC-TCD.
           MOVE S1-CCD TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 0 TO TC-DHC
           END-IF
           IF  TC-DHC NOT = 0
               MOVE 1 TO W-CCC
           END-IF.
       M-300.
           MOVE SPACE TO W-P1.
           MOVE W-15K TO P-15K1.
           MOVE SPACE TO P-DCM P-DCD P-TNA.
           MOVE S1-CSC TO P-CSC.
           IF  S1-UNC = 0
               IF  S1-DC = 4
                   MOVE 0 TO P-DC
               ELSE
                   MOVE S1-DC TO P-DC
               END-IF
           END-IF
           IF  S1-FRC NOT = 0
               MOVE S1-FRC TO P-FRC
           END-IF
           MOVE "(" TO P-F.
           MOVE W-DNO TO P-DNO.
           MOVE ")" TO P-R.
           IF  S1-UNC = 0
               IF  S1-DC = 0 OR 6
                   MOVE "売上" TO P-DCM
               ELSE
                   IF  S1-DC = 1 OR 2 OR 5
                       MOVE "返品" TO P-DCM
                   ELSE
                       IF  S1-DC = 3 OR 4 OR 7
                           IF  S1-SUT < ZERO
                               MOVE "値引" TO P-DCM
                           ELSE
                               MOVE "売上" TO P-DCM
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  S1-UNC = 1
               IF  S1-KIN < 0
                   MOVE "売上" TO P-DCM
               ELSE
                   MOVE "値引" TO P-DCM
               END-IF
           END-IF
           IF  S1-DC = 2
               MOVE "不良返品" TO P-DCD
           END-IF
           IF  S1-DC = 3
               MOVE "預り保管" TO P-DCD
           END-IF
           IF  S1-DC = 8
               MOVE "税調整分" TO P-DCD
           END-IF
           IF  W-CCC = 1
               MOVE TC-NAME TO WN-NAME
           ELSE
               MOVE T-NAME TO WN-NAME
           END-IF
           MOVE WN-NAME TO P-TNA.
           MOVE S1-TCD TO P-TCD.
           IF  W-CCC = 1
               MOVE "-" TO P-X
               MOVE S1-CCD TO P-CCD
           END-IF
           IF  S1-MNGP NOT = ZERO AND 99999999
               MOVE S1-MNGP TO P-MNGP
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  S1-DC NOT = 8
               GO TO M-360
           END-IF
           ADD S1-KIN TO W-AKIN.
           PERFORM MEI-120 THRU MEI-EX.
           GO TO M-400.
       M-360.
           ADD 1 TO W-GNO.
           IF  W-GNO = 7
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           IF  W-GNO NOT = S1-GNO
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           PERFORM MEI-RTN THRU MEI-EX.
       M-400.
           IF  JS-SIGN = 1
               GO TO M-420
           END-IF
      *           READ S-TRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" S-TRAN_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           MOVE S-R TO WR-S1.
           GO TO M-440.
       M-420.
      *           READ STRANYR AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" STRANYR_PNAME1 BY REFERENCE STRANYR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           MOVE STRANYR-R TO WR-S1.
       M-440.
           IF  S1-DNO = W-DNO
               IF  S1-GNO NOT = 9
                   GO TO M-360
               END-IF
           END-IF
           IF  S1-DNO NOT = W-DNO
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           PERFORM KEI-RTN THRU KEI-EX.
           IF  JS-SIGN = 1
               GO TO M-230
           ELSE
               GO TO M-220
           END-IF.
       M-920.
           IF  W-DZC = 0
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-980.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           IF  JS-SIGN = 1
               CALL "DB_F_Close" USING
                BY REFERENCE STRANYR_IDLST STRANYR_PNAME1
               GO TO M-040
           ELSE
               CALL "DB_F_Close" USING
                BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
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
       MID-EX.
           EXIT.
       MEI-RTN.
           MOVE S1-DC TO W-DC.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA.
           MOVE W-15K TO P-15K2.
           MOVE W-20K TO P-20K2.
           IF (S1-UNC = 1) OR (S1-HCD > 999899) OR (S1-DC = 3)
               GO TO MEI-040
           END-IF
           MOVE ZERO TO CNT W-ASU.
       MEI-020.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO MEI-040
           END-IF
           MOVE S1-SU(CNT) TO W-SU(CNT).
           IF  W-SU(CNT) NOT = ZERO
               MOVE W-SU(CNT) TO P-SU(CNT)
           END-IF
           GO TO MEI-020.
       MEI-040.
           IF  S1-HCD NOT = ZERO
               MOVE S1-HCD TO P-HCD
               IF  S1-UNC = 0
                   MOVE S1-SIZ TO P-SIZ
               END-IF
           END-IF
           MOVE S1-SUT TO W-SUT.
           IF  S1-HCD = ZERO
               GO TO MEI-100
           END-IF
           IF  S1-DC = 3 OR 4 OR 7
               IF  W-SUT < ZERO
                   COMPUTE W-SUT = W-SUT * -1
               END-IF
           END-IF
           MOVE W-SUT TO P-SUT.
           IF  S1-DC NOT = 4
               GO TO MEI-060
           END-IF
           IF  S1-SUT < ZERO
               MOVE "預りへ戻し" TO P-MSM
           ELSE
               MOVE "預り出荷　" TO P-MSM
           END-IF
           GO TO MEI-080.
       MEI-060.
           IF  S1-DC = 9
               MOVE "預り振替　"    TO P-MSM
               GO TO MEI-080
           END-IF
           MOVE S1-T TO P-T.
           MOVE S1-KIN TO W-KIN.
           IF (S1-DC = 3) OR (S1-UNC = 1)
               IF  W-KIN < ZERO
                   COMPUTE W-KIN = W-KIN * -1
               END-IF
           END-IF
           MOVE W-KIN TO P-KIN.
       MEI-080.
           MOVE S1-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE SPACE TO HI-NAME
           END-IF
           IF  S1-TCD = 0013 OR 0458 OR 0459 OR 0460
               MOVE HI-NAME TO W-NAME
               MOVE SPACE TO W-NA(24)
               MOVE W-NAME TO P-HNA
           ELSE
               MOVE HI-NAME TO P-HNA
           END-IF
           IF  S1-UNC = 0
               IF  S1-BIK NOT = SPACE
                   MOVE S1-BIK TO P-BIK
               END-IF
           END-IF.
       MEI-100.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  S1-HCD <  999900
               ADD W-SUT TO W-ASUT
           END-IF
           IF  S1-DC NOT = 4 AND 9
               ADD S1-KIN TO W-AKIN
           END-IF.
       MEI-120.
           IF  JS-SIGN = 1
               GO TO MEI-EX
           END-IF
           MOVE 1 TO S1-DHC.
           MOVE WR-S1 TO S-R.
      *           REWRITE S-R.
      *///////////////
           CALL "DB_Update" USING
            S-TRAN_PNAME1 S-TRAN_LNAME S-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME24" E-ME24 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       MEI-EX.
           EXIT.
       KEI-RTN.
           MOVE SPACE TO W-P3.
           MOVE W-15K TO P-15K3.
           MOVE ALL "　" TO P-KOSU P-KO P-CNAM P-NOM P-TME P-SZM.
           IF  SV-KOSU NOT = ZERO
               MOVE SV-KOSU TO   W-KOSU
               MOVE W-KOSU  TO   P-KOSU
               MOVE "個口" TO  P-KO
           END-IF
           IF  SV-CCD2 NOT = ZERO AND 001
               IF  W-CCC = 0
                   PERFORM TNA-RTN THRU TNA-EX
               END-IF
           END-IF
           MOVE "　合　計" TO P-TME.
           IF  S2-HNO NOT = ZERO
               MOVE "発送明細書№" TO P-NOM
               MOVE S2-HNO TO P-HMN
           END-IF
           IF  W-GNO = ZERO
               MOVE W-AKIN TO W-SHZ W-KINZ
               MOVE ZERO TO W-AKIN W-KIN
               GO TO KEI-060
           END-IF
           MOVE W-ASUT TO P-GSUT.                                       数計
           MOVE W-AKIN TO W-KIN.
           IF  W-HSC = 8
               COMPUTE W-SHZ ROUNDED = W-KIN * 0.08
           ELSE
               IF  W-HSC = 5
                   COMPUTE W-SHZ ROUNDED = W-KIN * 0.05
               ELSE
                   IF  W-HSC = 3
                       COMPUTE W-SHZ ROUNDED = W-KIN * 0.03
                   ELSE
                       IF  W-HSC = 1
                           COMPUTE W-SHZ ROUNDED = W-KIN * 0.10
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE W-SHZ TO W-KINZ.
           IF (W-DC = 3) OR (S2-UNC = 1)
               IF  W-KIN < ZERO
                   COMPUTE W-KIN = W-KIN * -1
                   COMPUTE W-KINZ = W-KINZ * -1
               END-IF
           END-IF.
       KEI-060.
           MOVE W-KIN TO P-GKIN.                                        計
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P3.
           MOVE W-15K TO P-15K3.
           MOVE ALL "　" TO P-KOSU P-KO P-CNAM P-NOM P-TME P-SZM.
           MOVE S2-BI TO P-BI.
           MOVE "消費税　" TO P-SZM.
           MOVE W-KINZ TO P-GKIN.                                       消
           COMPUTE W-TKIN = W-KIN + W-KINZ.
           MOVE W-TKIN TO P-TKIN.                                       総
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P8 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  JS-SIGN = 0
               IF  W-DC = 1 OR 2 OR 5
                   COMPUTE W-AKIN = W-AKIN * -1
                   COMPUTE W-SHZ = W-SHZ * -1
               END-IF
           END-IF
           IF  JS-SIGN = 1
               GO TO KEI-EX
           END-IF
           MOVE W-SHZ TO S2-TAX.
           MOVE 1 TO S2-DHC.
           MOVE WR-S2 TO S-R.
      *           REWRITE S-R.
      *///////////////
           CALL "DB_Update" USING
            S-TRAN_PNAME1 S-TRAN_LNAME S-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME24" E-ME24 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       KEI-EX.
           EXIT.
       TNA-RTN.
           MOVE SV-TCD TO TC-TCD.
           MOVE SV-CCD2 TO TC-CCD.
      *           READ TC-M UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
           END-IF
           MOVE TC-NAME TO P-CNAM.
           MOVE P-CNAM TO TBL-NAM.
           MOVE 26 TO N.
       TNA-020.
           IF  N = 0
               GO TO TNA-EX
           END-IF
           IF  T-NAM(N) NOT = SPACE AND "　"
               GO TO TNA-040
           END-IF
           COMPUTE N = N - 1.
           GO TO TNA-020.
       TNA-040.
           IF  W-DC = 1 OR 2
               ADD 1 TO N
               MOVE "様" TO T-NAM(N)
               ADD 1 TO N
               MOVE "よ" TO T-NAM(N)
               ADD 1 TO N
               MOVE "り" TO T-NAM(N)
               ADD 1 TO N
               MOVE "返" TO T-NAM(N)
               ADD 1 TO N
               MOVE "品" TO T-NAM(N)
               MOVE TBL-NAM TO P-CNAM
               GO TO TNA-EX
           END-IF
           ADD 1 TO N.
           MOVE "様" TO T-NAM(N).
           ADD 1 TO N.
           MOVE "　" TO T-NAM(N).
           ADD 1 TO N.
           MOVE "直" TO T-NAM(N).
           ADD 1 TO N.
           MOVE "送" TO T-NAM(N).
           MOVE TBL-NAM TO P-CNAM.
       TNA-EX.
           EXIT.
       CHK-RTN.
           MOVE 0 TO W-DCHK.
           CALL "DB_F_Open" USING
            "INPUT" S-TRAN_PNAME1 " " BY REFERENCE S-TRAN_IDLST "0".
       CHK-020.
      *           READ S-TRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" S-TRAN_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               GO TO CHK-040
           END-IF
           MOVE S-R TO WR-S1.
           IF  W-DCHK = 1
               IF  S1-DHC = 1
                   MOVE 3 TO W-DCHK
                   GO TO CHK-040
               ELSE
                   GO TO CHK-020
               END-IF
           END-IF
           IF  W-DCHK = 2
               IF  S1-DHC NOT = 1
                   MOVE 3 TO W-DCHK
                   GO TO CHK-040
               ELSE
                   GO TO CHK-020
               END-IF
           END-IF
           IF  S1-DHC NOT = 1
               MOVE 1 TO W-DCHK
           ELSE
               MOVE 2 TO W-DCHK
           END-IF
           GO TO CHK-020.
       CHK-040.
           CALL "DB_F_Close" USING
            BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1.
       CHK-EX.
           EXIT.
       ERR-RTN.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           GO TO ERR-RTN.
       ERR-EX.
           EXIT.

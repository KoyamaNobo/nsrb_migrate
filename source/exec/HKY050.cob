       IDENTIFICATION   DIVISION.
       PROGRAM-ID. HMY050.
      *********************************************************
      *    PROGRAM         :  得意先台帳発行                  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    BASE PROGRAM    :  HMD230                          *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       77  W-DCHK             PIC  9(001).
       77  W-DZC              PIC  9(001) VALUE 0.
       77  W-FILE             PIC  X(013).
       77  W-KSU              PIC  9(005).
       01  HEAD01.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(049) VALUE SPACE.
           02  W-30K          PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  N(008) VALUE "得意先元帳総括票".
           02  W-35K          PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　№".
           02  F              PIC  X(001) VALUE SPACE.
           02  P-DNO          PIC  N(004).
       01  HEAD02.
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(011) VALUE SPACE.
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(065) VALUE SPACE.
       01  HEAD08.
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  N(004) VALUE "日　　付".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売　　上".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "入　　金".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売掛残高".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　消費税残高".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "残高合計".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "備　考　".
       01  W-P01.
           02  F              PIC  X(062).
           02  P-UM           PIC  N(002).
           02  P-UNO          PIC  X(008).
           02  F              PIC  X(001).
           02  P-JSU          PIC  N(020).
           02  F              PIC  X(004).
       01  W-P02.
           02  F              PIC  X(013).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-NAME         PIC  N(026).
           02  F              PIC  X(020).
           02  P-JSS          PIC  N(020).
       01  W-P03.
           02  F              PIC  X(058).
           02  P-F            PIC  X(001).
           02  P-BMC          PIC  9(001).
           02  P-R            PIC  X(001).
           02  F              PIC  X(009).
           02  P-TM           PIC  X(003).
           02  F              PIC  X(001).
           02  P-TEL          PIC  X(014).
           02  F              PIC  X(002).
           02  P-FM           PIC  X(003).
           02  F              PIC  X(001).
           02  P-FAX          PIC  X(014).
       01  W-P04.
           02  F              PIC  X(013).
           02  P-DNM          PIC  N(008).
           02  F              PIC  X(083).
       01  W-P05.
           02  F              PIC  X(013).
           02  P-MSM          PIC  N(008).
           02  P-UK           PIC -----,---,---.
           02  P-UKZ          PIC ---,---,---.
           02  P-NK           PIC -----,---,---.
           02  P-UZ           PIC -----,---,---.
           02  P-UZZ          PIC ---,---,---.
           02  P-UZG          PIC -----,---,---.
           02  F              PIC  X(009).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(038) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  N(014) VALUE
                "【　得　意　先　台　帳　】　".
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  X(019) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  H-TCD          PIC  9(004).
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "得意先名".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  H-TNA          PIC  N(026).
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  X(041) VALUE SPACE.
       01  HEAD3H.
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "日　付　".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "伝票区分".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  X(001) VALUE "(".
           02  F              PIC  N(004) VALUE "伝票№　".
           02  F              PIC  X(001) VALUE ")".
           02  F              PIC  X(018) VALUE SPACE.
           02  F              PIC  X(051) VALUE
                "1             SS    S    M    L   LL 28.0 29.0 30.0".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(006) VALUE "請求予定日　".
           02  F              PIC  X(001) VALUE SPACE.
       01  HEAD4H.
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(051) VALUE
                "2 12.5 13.0 13.5 14.0 15.0 16.0 17.0 18.0 19.0 20.0".
           02  F              PIC  X(035) VALUE SPACE.
       01  HEAD5H.
           02  F              PIC  X(050) VALUE SPACE.
           02  F              PIC  X(051) VALUE
                "3 21.0 21.5 22.0 22.5 23.0 23.5 24.0 24.5 25.0     ".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(002) VALUE "備考".
           02  F              PIC  X(007) VALUE SPACE.
       01  HEAD6H.
           02  F              PIC  X(032) VALUE SPACE.
           02  F              PIC  N(004) VALUE "摘　　要".
           02  F              PIC  X(012) VALUE SPACE.
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
       01  HEAD3K.
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "日　付　".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "伝票区分".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  X(001) VALUE "(".
           02  F              PIC  N(004) VALUE "伝票№　".
           02  F              PIC  X(001) VALUE ")".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(006) VALUE "品　　　　名".
           02  F              PIC  X(048) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "単　　価".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(006) VALUE "請求予定日　".
           02  F              PIC  X(001) VALUE SPACE.
       01  HEAD4K.
           02  F              PIC  X(060) VALUE SPACE.
           02  F              PIC  N(004) VALUE "摘　　要".
           02  F              PIC  X(064) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　総合計".
       01  W-P1H.
           02  PH-15K1        PIC  X(005).
           02  PH-NGP         PIC 99/99/99.
           02  F              PIC  X(002).
           02  PH-DC          PIC  9(001).
           02  PH-CSC         PIC  9(001).
           02  PH-FRC         PIC  X(001).
           02  PH-DCM         PIC  N(002).
           02  F              PIC  X(001).
           02  PH-DCD         PIC  N(004).
           02  F              PIC  X(001).
           02  PH-F           PIC  X(001).
           02  PH-DNO         PIC  9(006).
           02  PH-R           PIC  X(001).
           02  F              PIC  X(094).
           02  PH-MNGP        PIC 99/99/99.
           02  F              PIC  X(002).
       01  W-P2H.
           02  PH-15K2        PIC  X(005).
           02  F              PIC  X(006).
           02  PH-HCD         PIC  9(006).
           02  F              PIC  X(001).
           02  PH-HNA         PIC  N(024).
           02  F              PIC  X(001).
           02  PH-SIZ         PIC  Z(001).
           02  PH-20K2        PIC  X(005).
           02  PH-SUD.
             03  PH-SU    OCCURS  10  PIC  -(005).
           02  PH-SUT         PIC ---,---.
           02  PH-T           PIC ---,---.
           02  PH-KIN         PIC --,---,---.
           02  PH-KIND  REDEFINES PH-KIN.
             03  PH-MSM       PIC  N(005).
           02  F              PIC  X(001).
           02  PH-BIK         PIC  X(010).
       01  W-P3H.
           02  PH-15K3        PIC  X(005).
           02  F              PIC  X(007).
           02  PH-KOSU        PIC  N(004).
           02  PH-KO          PIC  N(002).
           02  F              PIC  X(004).
           02  PH-CNAM        PIC  N(032).
           02  PH-BID   REDEFINES PH-CNAM.
             03  F            PIC  N(008).
             03  PH-BI        PIC  N(024).
           02  F              PIC  X(004).
           02  PH-NOM         PIC  N(006).
           02  F              PIC  X(001).
           02  PH-HMN         PIC  9(006).
           02  F              PIC  X(007).
           02  PH-TME         PIC  N(004).
           02  PH-GSUT        PIC ---,---.
           02  F              PIC  X(001).
           02  PH-SZM         PIC  N(004).
           02  PH-GKIN        PIC --,---,---.
           02  PH-TKIN        PIC ---,---,---.
       01  W-P1K.
           02  PK-15K1        PIC  X(005).
           02  PK-NGP         PIC 99/99/99.
           02  F              PIC  X(002).
           02  PK-DC          PIC  9(001).
           02  PK-CSC         PIC  9(001).
           02  F              PIC  X(001).
           02  PK-DCM         PIC  N(002).
           02  F              PIC  X(001).
           02  PK-DCD         PIC  N(004).
           02  F              PIC  X(001).
           02  PK-F           PIC  X(001).
           02  PK-DNO         PIC  9(006).
           02  PK-R           PIC  X(001).
           02  F              PIC  X(002).
           02  PK-HCD         PIC  X(005).
           02  F              PIC  X(001).
           02  PK-HNA         PIC  N(024).
           02  F              PIC  X(001).
           02  PK-JF          PIC  X(001).
           02  PK-JCD         PIC  9(006).
           02  PK-JR          PIC  X(001).
           02  F              PIC  X(007).
           02  PK-20K1        PIC  X(005).
           02  PK-SUT         PIC ----,--9.99.
           02  PK-T           PIC ----,--9.99.
           02  PK-KIN         PIC ---,---,--9.
           02  PK-KIND  REDEFINES PK-KIN.
             03  F            PIC  X(001).
             03  PK-MSM       PIC  N(005).
           02  F              PIC  X(001).
           02  PK-MNGP        PIC 99/99/99.
           02  F              PIC  X(002).
       01  W-P2K.
           02  PK-15K2        PIC  X(005).
           02  F              PIC  X(060).
           02  PK-BI          PIC  N(018).
           02  F              PIC  X(014).
           02  PK-TME         PIC  N(004).
           02  F              PIC  X(001).
           02  PK-SZM         PIC  N(004).
           02  PK-GKIN        PIC ---,---,--9.
           02  PK-TKIN        PIC ---,---,--9.
       01  W-P1N.
           02  PN-15K1        PIC  X(005).
           02  PN-NGP         PIC 99/99/99.
           02  F              PIC  X(002).
           02  PN-DCM         PIC  N(002).
           02  F              PIC  X(001).
           02  PN-NKC         PIC  N(006).
           02  F              PIC  X(001).
           02  PN-F           PIC  X(001).
           02  PN-DNO         PIC  9(006).
           02  PN-R           PIC  X(001).
           02  F              PIC  X(003).
           02  PN-TKGM        PIC  N(004).
           02  PN-TKGC        PIC  X(001).
           02  PN-TKGD        PIC 99/99/99.
           02  F              PIC  X(003).
           02  PN-KINM        PIC  N(002).
           02  PN-KINC        PIC  X(001).
           02  PN-KIND        PIC ---,---,--9.
           02  F              PIC  X(003).
           02  PN-SKBM        PIC  N(004).
           02  PN-SKBC        PIC  X(001).
           02  PN-SKNG        PIC 99/99.
           02  F              PIC  X(012).
           02  PN-TME         PIC  N(004).
           02  F              PIC  X(008).
           02  PN-SZM         PIC  N(004).
           02  PN-GKIN        PIC --,---,--9.
           02  PN-TKIN        PIC ---,---,--9.
       01  W-YOKO.
           02  F              PIC  X(136).
       01  W-DATA.
           02  W-PAGE         PIC  9(003).
           02  W-BMC          PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-NG           PIC  9(006).
           02  W-UNC          PIC  9(001).
           02  W-DNO          PIC  9(006).
           02  W-TD.
             03  W-ASUT       PIC S9(006).
             03  W-AKIN       PIC S9(008).
             03  W-SHZ        PIC S9(006).
             03  W-TKIN       PIC S9(008).
           02  W-D.
             03  W-GNO        PIC  9(001).
             03  W-ASU.
               04  W-SU    OCCURS  10  PIC S9(004).
             03  W-SUT        PIC S9(005).
             03  W-KIN        PIC S9(008).
             03  W-KINZ       PIC S9(007).
             03  CNT          PIC  9(002).
             03  W-DC         PIC  9(001).
             03  W-HSC        PIC  9(001).
             03  W-CSC        PIC  9(001).
           02  W-NENC         PIC  9(001).
           02  W-WARI         PIC  9(003).
           02  W-DMM          PIC  9(001).
           02  W-CCC          PIC  9(001).
           02  W-NAME         PIC  N(024).
           02  W-NAMED REDEFINES W-NAME.
             03  W-NAD   OCCURS  24.
               04  W-NA       PIC  N(001).
           02  WK-SUT         PIC S9(006)V9(02).
           02  WK-T           PIC S9(006)V9(02).
           02  WK-KIN         PIC S9(008).
           02  WK-SHZ         PIC S9(007).
           02  W-NGP          PIC  9(006).
           02  W-NGPD REDEFINES W-NGP.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-BIC          PIC  9(001).
           02  W-NEND         PIC  9(004).
       01  W-SDAT.
           02  W-SDC          PIC  9(001).
           02  W-SNGP         PIC  9(006).
           02  WP-D.
             03  WP-NGP.
               04  WP-NEN     PIC 99.
               04  WP-GET     PIC Z9.
               04  WP-PEY     PIC Z9.
             03  WP-TGP.
               04  WP-TG      PIC Z9.
               04  WP-TP      PIC Z9.
           02  W-DNMD.
             03  W-DNN        PIC  N(002).
             03  F            PIC  N(001) VALUE "．".
             03  W-DNG        PIC  N(002).
             03  F            PIC  N(001) VALUE "．".
             03  W-DNP        PIC  N(002).
           02  W-DNM   REDEFINES W-DNMD  PIC  N(008).
           02  W-MSM          PIC  N(008).
           02  W-SDNO         PIC  N(004).
           02  W-DNOD  REDEFINES W-SDNO.
             03  F            PIC  N(001).
             03  W-TNC1       PIC  N(001).
             03  W-V          PIC  N(001).
             03  W-DCC        PIC  N(001).
           02  W-NK           PIC S9(009).
           02  W-UZ           PIC S9(009).
           02  W-UZZ          PIC S9(007).
           02  W-UZG          PIC S9(009).
           02  W-STD.
             03  W-TUK        PIC S9(009).
             03  W-TUKZ       PIC S9(007).
             03  W-TNK        PIC S9(009).
           02  SCNT           PIC  9(002) VALUE ZERO.
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
           COPY LIHKBM.
           COPY LITCM.
           COPY LIKHM.
           COPY LIJM.
           COPY LSPF.
      *FD  DTWF
       01  DTWF_HKY050.
           02  DTWF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  DTWF_LNAME     PIC  X(011) VALUE "DTWF_HKY050".
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
             03  DT1-NGP      PIC  9(008).
             03  DT1-NGPD  REDEFINES DT1-NGP.
               04  F          PIC  9(002).
               04  DT1-DATE   PIC  9(006).
             03  DT1-NGPL  REDEFINES DT1-NGP.
               04  DT1-NG     PIC  9(006).
               04  F          PIC  9(002).
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
             03  DT1-NKCD  REDEFINES  DT1-NKC.
               04  DT1-NKC1   PIC  9(001).
               04  DT1-NKC2   PIC  9(001).
             03  DT1-SSC      PIC  9(001).
             03  F            PIC  9(002).
             03  DT1-TKG      PIC  9(006).
             03  F            PIC  9(002).
             03  DT1-SKNG     PIC  9(004).
             03  DT1-DCC      PIC  9(001).
             03  DT1-SNGP.
               04  DT1-SNEN   PIC  9(002).
               04  DT1-SGET   PIC  9(002).
               04  DT1-SPEY   PIC  9(002).
             03  F            PIC  X(019).
             03  DT1-EC       PIC  9(001).
             03  F            PIC  9(002).
             03  DT1-SKU      PIC  9(006).
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
                "＊＊＊　　　　得意先台帳　作成　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　（両面印刷）　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-KSM.
           02  FILLER.
             03  FILLER  PIC  X(016) VALUE
                "｛　      件　｝".
             03  FILLER  PIC ZZ,ZZ9.
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  EMSG-JS PIC  X(024) VALUE
                  "***  ｺﾝﾄﾛｰﾙ情報ｴﾗｰ   ***".
             03  E-ME2   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME3   PIC  X(016) VALUE
                  "***  KHM ﾅｼ  ***".
             03  E-ME4   PIC  X(015) VALUE
                  "***  JM ﾅｼ  ***".
             03  E-ME5   PIC  X(026) VALUE
                  "***  ｳﾘｱｹﾞ ｴﾗｰ (OVER)  ***".
             03  E-ME6   PIC  X(024) VALUE
                  "***  ｳﾘｱｹﾞ ｴﾗｰ (NO)  ***".
             03  E-ME7   PIC  X(026) VALUE
                  "***  ｳﾘｱｹﾞ ｴﾗｰ (ﾋﾞｺｳ)  ***".
             03  E-ME8   PIC  X(019) VALUE
                  "***  ﾆｭｳｷﾝ ｴﾗｰ  ***".
             03  E-ME10  PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME11  PIC  X(018) VALUE
                  "***  DATA ﾅｼ   ***".
             03  E-TCD   PIC  9(004).
             03  E-HCD   PIC  X(006).
             03  E-JCD   PIC  X(006).
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
           "C-MID" " " "0" "0" "330" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "2" "15" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "3" "15" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "4" "15" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "5" "15" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "6" "15" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "7" "15" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "8" "15" "44" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "20" "27" "22" "07C-MID" " " RETURNING RESU.
      *C-KSM
       CALL "SD_Init" USING 
            "C-KSM" " " "0" "0" "22" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-KSM" " " "12" "0" "22" " " "C-KSM" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-KSM" "X" "12" "29" "16" " " "01C-KSM" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201C-KSM" "ZZ,ZZ9" "12" "33" "6" "0101C-KSM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0201C-KSM" BY REFERENCE W-KSU "5" "0" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "44" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "215" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "215" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "EMSG-JS" "X" "24" "15" "24" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "16" "EMSG-JS" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" "X" "24" "15" "16" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME4" "X" "24" "15" "15" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME5" "X" "24" "15" "26" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME6" "X" "24" "15" "24" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME7" "X" "24" "15" "26" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME8" "X" "24" "15" "19" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME10" "X" "24" "15" "15" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME11" "X" "24" "15" "18" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-TCD" "9" "24" "50" "4" "E-ME11" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-TCD" BY REFERENCE DT1-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-HCD" "X" "24" "50" "6" "E-TCD" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-HCD" BY REFERENCE DT1-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-JCD" "X" "24" "50" "6" "E-HCD" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-JCD" BY REFERENCE DT1-JCD "4" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO DTWF_PNAME1.
           PERFORM CHK-RTN THRU CHK-EX.
           IF  COMPLETION_CODE = 255
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-KSM" C-KSM "p" RETURNING RESU.
       M-040.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-040
           END-IF.
       M-080.
           CALL "DB_F_Open" USING
            "INPUT" DTWF_PNAME1 " " BY REFERENCE DTWF_IDLST "0".
      *           READ DTWF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" DTWF_PNAME1 BY REFERENCE DTW-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE DTWF_IDLST DTWF_PNAME1
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO W-DATA.
           MOVE ALL "-" TO W-YOKO.
       M-100.
           MOVE 0 TO W-SDC.
           MOVE DT1-BMC TO W-BMC.
           MOVE DT1-NG TO W-NG.
           MOVE DT1-TCD TO T-KEY W-TCD.
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
               MOVE SPACE TO T-NAME
           END-IF
      *
           MOVE W-TCD TO H-TCD.
           MOVE T-NAME TO H-TNA.
           MOVE DT1-DATE TO W-NGP.
           IF  W-GET = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
               MOVE 31 TO W-PEY
           ELSE
               IF  W-GET = 4 OR 6 OR 9 OR 11
                   MOVE 30 TO W-PEY
               ELSE
                   MOVE 2000 TO W-NEND
                   ADD W-NEN TO W-NEND
                   DIVIDE 4 INTO W-NEND GIVING W-WARI REMAINDER W-NENC
                   IF  W-NENC = 3
                       MOVE 29 TO W-PEY
                   ELSE
                       MOVE 28 TO W-PEY
                   END-IF
               END-IF
           END-IF
           IF  DT1-SNGP = ZERO
               IF  W-SDC = 0
                   GO TO M-220
               END-IF
           END-IF
           CALL "PR_Close" RETURNING RESP.
           CALL "PR_Open" RETURNING RESP.
           MOVE 1 TO W-SDC.
           MOVE ZERO TO W-SNGP SCNT.
           PERFORM MDM-RTN THRU MDM-EX.
           PERFORM MDP-RTN THRU MDP-EX.
           PERFORM KKM-RTN THRU KKM-EX.
           IF  DT1-DC = 0
               GO TO M-140
           END-IF.
       M-120.
           PERFORM MSM-RTN THRU MSM-EX.
       M-140.
      *           READ DTWF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" DTWF_PNAME1 BY REFERENCE DTW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-160
           END-IF
           IF  DT1-BMC NOT = W-BMC
               GO TO M-180
           END-IF
           IF  DT1-NG NOT = W-NG
               GO TO M-180
           END-IF
           IF  DT1-TCD NOT = W-TCD
               GO TO M-180
           END-IF
           IF  DT1-SNGP = ZERO
               GO TO M-200
           END-IF
           GO TO M-120.
       M-160.
           PERFORM SKE-RTN THRU SKE-EX.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE DTWF_IDLST DTWF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           GO TO M-980.
       M-180.
           PERFORM SKE-RTN THRU SKE-EX.
           CALL "PR_Close" RETURNING RESP.
           CALL "PR_Open" RETURNING RESP.
           GO TO M-100.
       M-200.
           PERFORM SKE-RTN THRU SKE-EX.
       M-220.
           CALL "PR_Close" RETURNING RESP.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO W-PAGE.
           MOVE W-NGP TO H-DATE.
           PERFORM MID-010 THRU MID-EX.
           IF  DT1-BMC = 0
               CALL "DB_F_Open" USING
                "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
                "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE
                HI-KEY2
               CALL "DB_F_Open" USING
                "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
                "TC-KEY" BY REFERENCE TC-KEY
           ELSE
               CALL "DB_F_Open" USING
                "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
                "KH-KEY" BY REFERENCE KH-KEY
               CALL "DB_F_Open" USING
                "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
                "J-KEY" BY REFERENCE J-KEY
           END-IF.
       M-240.
           MOVE DT1-UNC TO W-UNC.
           MOVE DT1-DNO TO W-DNO.
           MOVE ZERO TO W-TD.
       M-260.
           IF  DT1-UNC NOT = 1
               GO TO M-320
           END-IF
           MOVE SPACE TO W-P1N.
           MOVE W-15K TO PN-15K1.
           MOVE SPACE TO PN-DCM PN-NKC PN-TKGM PN-KINM PN-SKBM PN-TME
                                                               PN-SZM.
           IF  DT1-GNO = 1
               MOVE DT1-DATE TO PN-NGP
               MOVE "入金" TO PN-DCM
           END-IF
           IF  DT1-NKC = 60
               GO TO M-280
           END-IF
           MOVE SPACE TO HKB-KEY.
           MOVE "31" TO HKB-NO.
           MOVE DT1-NKC1 TO HKB-NKC1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-NKNA
           END-IF
           GO TO M-300.
       M-280.
           MOVE SPACE TO HKB-KEY.
           MOVE "32" TO HKB-NO.
           MOVE DT1-SSC TO HKB-NSC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-NSNA
           END-IF.
       M-300.
           IF  DT1-GNO = 1
               MOVE "(" TO PN-F
               MOVE W-DNO TO PN-DNO
               MOVE ")" TO PN-R
           END-IF
           ADD DT1-KIN TO W-TKIN.
           IF  DT1-NKC2 = 8
               MOVE DT1-KIN TO W-SHZ
               MOVE "消　費　税　" TO PN-NKC
           ELSE
               ADD DT1-KIN TO W-AKIN
               IF  DT1-NKC = 60
                   MOVE HKB-NSNA TO PN-NKC
               ELSE
                   MOVE HKB-NKNA TO PN-NKC
               END-IF
           END-IF
           IF  DT1-TKG NOT = ZERO
               MOVE "手形期日" TO PN-TKGM
               MOVE ":" TO PN-TKGC
               MOVE DT1-TKG TO PN-TKGD
           END-IF
           MOVE "金額" TO PN-KINM.
           MOVE ":" TO PN-KINC.
           MOVE DT1-KIN TO PN-KIND.
           IF  DT1-SKNG NOT = ZERO
               MOVE "請求年月" TO PN-SKBM
               MOVE ":" TO PN-SKBC
               MOVE DT1-SKNG TO PN-SKNG
           END-IF
           IF  DT1-EC = 9
               MOVE "　合　計" TO PN-TME
               MOVE W-AKIN TO PN-GKIN
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               MOVE DT1-DATE TO PN-NGP
               MOVE "入金" TO PN-DCM
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1N TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  DT1-EC = 9
               MOVE SPACE TO W-P1N
               MOVE W-15K TO PN-15K1
               MOVE SPACE TO PN-DCM PN-NKC PN-TKGM PN-KINM
                             PN-SKBM PN-TME PN-SZM
               MOVE "　消費税" TO PN-SZM
               MOVE W-SHZ TO PN-GKIN
               MOVE W-TKIN TO PN-TKIN
               MOVE SPACE TO SP-R
               MOVE W-P1N TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE W-YOKO TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           GO TO M-540.
       M-320.
           IF  DT1-BMC NOT = ZERO
               GO TO M-460
           END-IF
      *
           IF  W-DZC = 0
               MOVE 1 TO W-DZC
           END-IF
           MOVE ZERO TO W-TD W-D.
           MOVE DT1-CSC TO W-CSC.
           MOVE DT1-HSC TO W-HSC.
           MOVE DT1-KOSU TO SV-KOSU.
           MOVE DT1-TCD2 TO SV-TCD.
           MOVE DT1-CCD TO SV-CCD2.
      *
           MOVE 0 TO W-CCC.
           IF  DT1-CCD = ZERO OR 001
               GO TO M-380
           END-IF
           MOVE DT1-TCD2 TO TC-TCD.
           MOVE DT1-CCD TO TC-CCD.
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
       M-380.
           MOVE SPACE TO W-P1H.
           MOVE W-15K TO PH-15K1.
           MOVE SPACE TO PH-DCM PH-DCD.
           MOVE DT1-DATE TO PH-NGP.
           MOVE DT1-CSC TO PH-CSC.
           IF  DT1-SNC = 0
               IF  DT1-DC = 4
                   MOVE 0 TO PH-DC
               ELSE
                   MOVE DT1-DC TO PH-DC
               END-IF
           END-IF
           IF  DT1-FRC NOT = 0
               MOVE DT1-FRC TO PH-FRC
           END-IF
           MOVE "(" TO PH-F.
           MOVE W-DNO TO PH-DNO.
           MOVE ")" TO PH-R.
           IF  DT1-SNC = 0
               IF  DT1-DC = 0 OR 6
                   MOVE "売上" TO PH-DCM
               ELSE
                   IF  DT1-DC = 1 OR 2 OR 5
                       MOVE "返品" TO PH-DCM
                   ELSE
                       IF  DT1-DC = 3 OR 4 OR 7
                           IF  DT1-SUT < ZERO
                               MOVE "値引" TO PH-DCM
                           ELSE
                               MOVE "売上" TO PH-DCM
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  DT1-SNC = 1
               IF  DT1-KIN < 0
                   MOVE "売上" TO PH-DCM
               ELSE
                   MOVE "値引" TO PH-DCM
               END-IF
           END-IF
           IF  DT1-DC = 2
               MOVE "不良返品" TO PH-DCD
           END-IF
           IF  DT1-DC = 3
               MOVE "預り保管" TO PH-DCD
           END-IF
           IF  DT1-DC = 8
               MOVE "税調整分" TO PH-DCD
           END-IF
           IF  DT1-SKU NOT = ZERO AND 999999
               MOVE DT1-SKU TO PH-MNGP
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1H TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  DT1-DC NOT = 8
               GO TO M-400
           END-IF
           ADD DT1-KIN TO W-AKIN.
           GO TO M-540.
       M-400.
           ADD 1 TO W-GNO.
           IF  W-GNO = 7
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           IF  W-GNO NOT = DT1-GNO
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           PERFORM MEI-RTN THRU MEI-EX.
           GO TO M-540.
      *-----------------------------------------------------------------
       M-460.
           MOVE SPACE TO W-P1K.
           MOVE W-15K TO PK-15K1.
           MOVE W-20K TO PK-20K1.
           MOVE SPACE TO PK-DCM PK-DCD PK-HNA.
           IF  DT1-GNO = 1
               PERFORM KM1-RTN THRU KM1-EX
           END-IF
           MOVE DT1-SUT TO WK-SUT.
           MOVE DT1-T TO WK-T.
           MOVE DT1-KIN TO WK-KIN.
           MOVE ZERO TO WK-SHZ.
           IF  DT1-DC = 8 OR 9
               COMPUTE WK-T = -1 * WK-T
               COMPUTE WK-KIN = -1 * WK-KIN
           END-IF
           IF  DT1-DC = 5 OR 9
               MOVE WK-KIN TO WK-SHZ
               MOVE ZERO TO WK-KIN
               IF (DT1-GNO = 1) AND (DT1-EC = 9)
                   GO TO M-500
               ELSE
                   GO TO M-520
               END-IF
           END-IF
           IF  WK-SUT NOT = ZERO
               MOVE WK-SUT TO PK-SUT
           END-IF
           IF  WK-T NOT = ZERO
               MOVE WK-T TO PK-T
           END-IF
           MOVE WK-KIN TO PK-KIN.
           MOVE DT1-KCD TO PK-HCD.
           IF  DT1-KCD = "00800"
               GO TO M-480
           END-IF
           MOVE DT1-KCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE SPACE TO KH-NAME
           END-IF
           MOVE KH-NAME TO PK-HNA.
           GO TO M-500.
       M-480.
           MOVE DT1-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JCD" E-JCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE SPACE TO J-NAME
           END-IF
           MOVE J-NAME TO PK-HNA.
           MOVE "<" TO PK-JF.
           MOVE DT1-JCD TO PK-JCD.
           MOVE ">" TO PK-JR.
       M-500.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
               PERFORM KM1-RTN THRU KM1-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1K TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       M-520.
           ADD WK-KIN TO W-AKIN.
           ADD WK-SHZ TO W-SHZ.
           IF  DT1-EC = 9
               PERFORM KKE-RTN THRU KKE-EX
           END-IF.
       M-540.
      *           READ DTWF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" DTWF_PNAME1 BY REFERENCE DTW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-900
           END-IF
           IF (DT1-BMC NOT = W-BMC) OR
              (DT1-NG  NOT = W-NG)  OR
              (DT1-TCD NOT = W-TCD)
               GO TO M-660
           END-IF
      *
           IF  W-UNC = 1
               IF  DT1-DNO = W-DNO
                   GO TO M-260
               ELSE
                   GO TO M-240
               END-IF
           END-IF
           IF  W-BMC NOT = 0
               GO TO M-620
           END-IF
           IF  DT1-DNO = W-DNO
               IF  DT1-GNO NOT = 9
                   GO TO M-400
               END-IF
           END-IF
           IF  DT1-DNO NOT = W-DNO
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           PERFORM KEI-RTN THRU KEI-EX.
      *
      *           READ DTWF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" DTWF_PNAME1 BY REFERENCE DTW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-900
           END-IF
           IF (DT1-BMC NOT = W-BMC) OR
              (DT1-NG  NOT = W-NG)  OR
              (DT1-TCD NOT = W-TCD)
               GO TO M-660
           END-IF
           GO TO M-240.
       M-620.
           IF  DT1-DNO = W-DNO
               GO TO M-460
           END-IF
           GO TO M-240.
       M-660.
           IF  W-BMC = 0
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TC-M_IDLST TC-M_PNAME1
           ELSE
               CALL "DB_F_Close" USING
                BY REFERENCE KH-M_IDLST KH-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE J-M_IDLST J-M_PNAME1
           END-IF
           GO TO M-100.
       M-900.
           IF  W-BMC = 0
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TC-M_IDLST TC-M_PNAME1
           ELSE
               CALL "DB_F_Close" USING
                BY REFERENCE KH-M_IDLST KH-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE J-M_IDLST J-M_PNAME1
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE DTWF_IDLST DTWF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       CHK-RTN.
           MOVE ZERO TO W-KSU.
           CALL "DB_F_Open" USING
            "INPUT" DTWF_PNAME1 " " BY REFERENCE DTWF_IDLST "0".
      *           READ DTWF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" DTWF_PNAME1 BY REFERENCE DTW-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO CHK-080
           END-IF.
       CHK-020.
           ADD 1 TO W-KSU.
      *           READ DTWF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" DTWF_PNAME1 BY REFERENCE DTW-R " " RETURNING RET.
           IF  RET = 1
               GO TO CHK-080
           END-IF
           GO TO CHK-020.
       CHK-080.
           CALL "DB_F_Close" USING BY REFERENCE DTWF_IDLST DTWF_PNAME1.
       CHK-EX.
           EXIT.
       MDM-RTN.
           MOVE SPACE TO W-P01 W-P02 W-P03.
           MOVE SPACE TO P-DNO P-UM P-JSU P-NAME P-JSS.
           MOVE SPACE TO W-SDNO.
           MOVE T-TNC1 TO W-TNC1.
           MOVE "－" TO W-V.
           MOVE T-DCC TO W-DCC.
           MOVE W-SDNO TO P-DNO.
           MOVE "　〒" TO P-UM.
           MOVE "TEL" TO P-TM.
           MOVE "FAX" TO P-FM.
           MOVE T-TCD TO P-TCD.
           MOVE T-NAME TO P-NAME.
           MOVE "<" TO P-F.
           MOVE ">" TO P-R.
           MOVE T-BC TO P-BMC.
           MOVE T-JSU TO P-JSU.
           MOVE T-JSS TO P-JSS.
           MOVE T-UNO TO P-UNO.
           MOVE T-TEL TO P-TEL.
           MOVE T-FAX TO P-FAX.
           MOVE ZERO TO W-UZ W-UZZ W-UZG W-STD.
       MDM-EX.
           EXIT.
       MDP-RTN.
           MOVE SPACE TO SP-R.
           MOVE HEAD01 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD02 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P01 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P02 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P03 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD08 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MDP-EX.
           EXIT.
       KKM-RTN.
           MOVE SPACE TO W-P04 W-P05.
           MOVE SPACE TO P-DNM P-MSM.
           IF (DT1-DC NOT = 0) OR (SCNT = 11)
               GO TO KKM-020
           END-IF
           MOVE DT1-SNEN TO WP-NEN.
           MOVE DT1-SGET TO WP-GET.
           MOVE DT1-SPEY TO WP-PEY.
           MOVE WP-NEN TO W-DNN.
           MOVE WP-GET TO W-DNG.
           MOVE WP-PEY TO W-DNP.
           MOVE W-DNM TO P-DNM.
           MOVE "　　　　　　繰越" TO P-MSM.
           MOVE DT1-KIN TO W-UZ.
           MOVE DT1-SHZ TO W-UZZ.
           COMPUTE W-UZG = W-UZ + W-UZZ.
           MOVE W-UZ TO P-UZ.
           MOVE W-UZZ TO P-UZZ.
           MOVE W-UZG TO P-UZG.
       KKM-020.
           MOVE SPACE TO SP-R.
           MOVE W-P04 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P05 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE 1 TO SCNT.
       KKM-EX.
           EXIT.
       MSM-RTN.
           MOVE SPACE TO W-P04 W-P05.
           MOVE SPACE TO P-DNM P-MSM.
           IF  DT1-SNGP NOT = W-SNGP
               MOVE DT1-SNGP TO W-SNGP
               MOVE DT1-SNEN TO WP-NEN
               MOVE DT1-SGET TO WP-GET
               MOVE DT1-SPEY TO WP-PEY
               MOVE WP-NEN TO W-DNN
               MOVE WP-GET TO W-DNG
               MOVE WP-PEY TO W-DNP
               MOVE W-DNM TO P-DNM
           END-IF
           IF  DT1-DC NOT = 1
               GO TO MSM-040
           END-IF
           MOVE "　　　　　　まで" TO P-MSM.
           ADD DT1-KIN TO W-TUK W-UZ.
           ADD DT1-SHZ TO W-TUKZ W-UZZ.
           COMPUTE W-UZG = W-UZ + W-UZZ.
           MOVE DT1-KIN TO P-UK.
           MOVE DT1-SHZ TO P-UKZ.
           MOVE W-UZ TO P-UZ.
           MOVE W-UZZ TO P-UZZ.
           MOVE W-UZG TO P-UZG.
           GO TO MSM-100.
       MSM-040.
           IF  DT1-DC NOT = 2
               GO TO MSM-060
           END-IF
           MOVE "　　　　　　入金" TO P-MSM.
           SUBTRACT DT1-KIN FROM W-UZ.
           SUBTRACT DT1-SHZ FROM W-UZZ.
           COMPUTE W-NK = DT1-KIN + DT1-SHZ.
           COMPUTE W-UZG = W-UZ + W-UZZ.
           MOVE W-NK TO P-NK.
           MOVE W-UZ TO P-UZ.
           MOVE W-UZZ TO P-UZZ.
           MOVE W-UZG TO P-UZG.
           ADD W-NK TO W-TNK.
           GO TO MSM-100.
       MSM-060.
           MOVE "　　　　　　請求" TO P-MSM.
           COMPUTE W-UZG = DT1-KIN + DT1-SHZ.
           MOVE DT1-KIN TO P-UZ.
           MOVE DT1-SHZ TO P-UZZ.
           MOVE W-UZG TO P-UZG.
       MSM-100.
           ADD 1 TO SCNT.
           IF  SCNT NOT = 11
               GO TO MSM-120
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
           MOVE ZERO TO W-SNGP SCNT.
           PERFORM MDP-RTN THRU MDP-EX.
           PERFORM KKM-RTN THRU KKM-EX.
           MOVE DT1-SNEN TO WP-NEN.
           MOVE DT1-SGET TO WP-GET.
           MOVE DT1-SPEY TO WP-PEY.
           MOVE WP-NEN TO W-DNN.
           MOVE WP-GET TO W-DNG.
           MOVE WP-PEY TO W-DNP.
           MOVE W-DNM TO P-DNM.
           IF  DT1-DC = 1
               SUBTRACT DT1-KIN FROM W-TUK W-UZ
               SUBTRACT DT1-SHZ FROM W-TUKZ W-UZZ
           ELSE
               IF  DT1-DC = 2
                   ADD DT1-KIN TO W-UZ
                   ADD DT1-SHZ TO W-UZZ
                   SUBTRACT DT1-KIN FROM W-TNK
                   SUBTRACT DT1-SHZ FROM W-TNK
               END-IF
           END-IF
           GO TO MSM-RTN.
       MSM-120.
           MOVE SPACE TO SP-R.
           MOVE W-P04 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P05 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MSM-EX.
           EXIT.
       SKE-RTN.
           IF  ZERO = W-TUK AND W-TUKZ AND W-TNK
               GO TO SKE-020
           END-IF
           MOVE SPACE TO W-P05.
           MOVE SPACE TO P-MSM.
           MOVE "合　　　計　　　" TO P-MSM.
           MOVE W-TUK TO P-UK.
           MOVE W-TUKZ TO P-UKZ.
           MOVE W-TNK TO P-NK.
           COMPUTE SCNT = 22 - (2 * SCNT).
           IF  SCNT = ZERO
               MOVE 22 TO SCNT
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P05 TO SP-R.
           CALL "PR_LineFeed" USING SCNT RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       SKE-020.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       SKE-EX.
           EXIT.
      *-------------------------------------------------------------------------
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  W-BMC = 0
               MOVE HEAD3H TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD4H TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD5H TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD6H TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
           ELSE
               MOVE HEAD3K TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD4K TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
       MEI-RTN.
           MOVE DT1-DC TO W-DC.
           MOVE SPACE TO W-P2H.
           MOVE SPACE TO PH-HNA.
           MOVE W-15K TO PH-15K2.
           MOVE W-20K TO PH-20K2.
           IF (DT1-SNC = 1) OR (DT1-HCD > 999899) OR (DT1-DC = 3)
               GO TO MEI-040
           END-IF
           MOVE ZERO TO CNT W-ASU.
       MEI-020.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO MEI-040
           END-IF
           MOVE DT1-SU(CNT) TO W-SU(CNT).
           IF  W-SU(CNT) NOT = ZERO
               MOVE W-SU(CNT) TO PH-SU(CNT)
           END-IF
           GO TO MEI-020.
       MEI-040.
           IF  DT1-HCD NOT = ZERO
               MOVE DT1-HCD TO PH-HCD
               IF  DT1-SNC = 0
                   MOVE DT1-SIZ TO PH-SIZ
               END-IF
           END-IF
           MOVE DT1-SUT TO W-SUT.
           IF  DT1-HCD = ZERO
               GO TO MEI-100
           END-IF
           IF  DT1-DC = 3 OR 4 OR 7
               IF  W-SUT < ZERO
                   COMPUTE W-SUT = W-SUT * -1
               END-IF
           END-IF
           MOVE W-SUT TO PH-SUT.
           IF  DT1-DC NOT = 4
               GO TO MEI-060
           END-IF
           IF  DT1-SUT < ZERO
               MOVE "預りへ戻し" TO PH-MSM
           ELSE
               MOVE "預り出荷　" TO PH-MSM
           END-IF
           GO TO MEI-080.
       MEI-060.
           IF  DT1-DC = 9
               MOVE "預り振替　"    TO PH-MSM
               GO TO MEI-080
           END-IF
           MOVE DT1-T TO PH-T.
           MOVE DT1-KIN TO W-KIN.
           IF (DT1-DC = 3) OR (DT1-SNC = 1)
               IF  W-KIN < ZERO
                   COMPUTE W-KIN = W-KIN * -1
               END-IF
           END-IF
           MOVE W-KIN TO PH-KIN.
       MEI-080.
           MOVE DT1-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE SPACE TO HI-NAME
           END-IF
           IF  DT1-TCD = 0013 OR 0458 OR 0459 OR 0460
               MOVE HI-NAME TO W-NAME
               MOVE SPACE TO W-NA(24)
               MOVE W-NAME TO PH-HNA
           ELSE
               MOVE HI-NAME TO PH-HNA
           END-IF
           IF  DT1-SNC = 0
               IF  DT1-BIK NOT = SPACE
                   MOVE DT1-BIK TO PH-BIK
               END-IF
           END-IF.
       MEI-100.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2H TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  DT1-HCD <  999900
               ADD W-SUT TO W-ASUT
           END-IF
           IF  DT1-DC NOT = 4 AND 9
               ADD DT1-KIN TO W-AKIN
           END-IF.
       MEI-EX.
           EXIT.
       KEI-RTN.
           MOVE SPACE TO W-P3H.
           MOVE W-15K TO PH-15K3.
           MOVE SPACE TO PH-KOSU PH-KO PH-CNAM PH-NOM PH-TME PH-SZM.
           IF  SV-KOSU NOT = ZERO
               MOVE SV-KOSU TO   W-KOSU
               MOVE W-KOSU  TO   PH-KOSU
               MOVE "個口" TO  PH-KO
           END-IF
           IF  SV-CCD2 NOT = ZERO AND 001
               IF  W-CCC = 0
                   PERFORM TNA-RTN THRU TNA-EX
               END-IF
           END-IF
           MOVE "　合　計" TO PH-TME.
           IF  DT2-HNO NOT = ZERO
               MOVE "発送明細書№" TO PH-NOM
               MOVE DT2-HNO TO PH-HMN
           END-IF
           IF  W-GNO = ZERO
               MOVE W-AKIN TO W-SHZ W-KINZ
               MOVE ZERO TO W-AKIN W-KIN
               GO TO KEI-060
           END-IF
           MOVE W-ASUT TO PH-GSUT.                                      数計
           MOVE W-AKIN TO W-KIN.
           IF  W-HSC = 1
               COMPUTE W-SHZ ROUNDED = W-KIN * 0.10
           ELSE
               IF  W-HSC = 3
                   COMPUTE W-SHZ ROUNDED = W-KIN * 0.03
               ELSE
                   IF  W-HSC = 5
                       COMPUTE W-SHZ ROUNDED = W-KIN * 0.05
                   ELSE
                       IF  W-HSC = 8
                           COMPUTE W-SHZ ROUNDED = W-KIN * 0.08
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE W-SHZ TO W-KINZ.
           IF (W-DC = 3) OR (DT2-SNC = 1)
               IF  W-KIN < ZERO
                   COMPUTE W-KIN = W-KIN * -1
                   COMPUTE W-KINZ = W-KINZ * -1
               END-IF
           END-IF.
       KEI-060.
           MOVE W-KIN TO PH-GKIN.                                       計
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P3H TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P3H.
           MOVE W-15K TO PH-15K3.
           MOVE SPACE TO PH-KOSU PH-KO PH-CNAM PH-NOM PH-TME PH-SZM.
           MOVE DT2-BI TO PH-BI.
           MOVE "消費税　" TO PH-SZM.
           MOVE W-KINZ TO PH-GKIN.                                      消
           COMPUTE W-TKIN = W-KIN + W-KINZ.
           MOVE W-TKIN TO PH-TKIN.                                      総
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P3H TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-YOKO TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  W-DC = 1 OR 2 OR 5
               COMPUTE W-AKIN = W-AKIN * -1
               COMPUTE W-SHZ = W-SHZ * -1
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
           MOVE TC-NAME TO PH-CNAM.
           MOVE PH-CNAM TO TBL-NAM.
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
               MOVE TBL-NAM TO PH-CNAM
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
           MOVE TBL-NAM TO PH-CNAM.
       TNA-EX.
           EXIT.
       KM1-RTN.
           MOVE DT1-DATE TO PK-NGP.
           MOVE DT1-CSC TO PK-CSC.
           IF  DT1-SNC = 0
               IF  DT1-DC = 4
                   MOVE 0 TO PK-DC
               ELSE
                   MOVE DT1-DC TO PK-DC
               END-IF
           END-IF
           MOVE "(" TO PK-F.
           MOVE W-DNO TO PK-DNO.
           MOVE ")" TO PK-R.
           IF  DT1-SNC = 0
               IF  DT1-DC = 0 OR 6
                   MOVE "売上" TO PK-DCM
               ELSE
                   IF  DT1-DC = 1 OR 2
                       MOVE "返品" TO PK-DCM
                   ELSE
                       IF  DT1-DC = 3 OR 4 OR 7
                           IF  DT1-SUT < ZERO
                               MOVE "値引" TO PK-DCM
                           ELSE
                               MOVE "売上" TO PK-DCM
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  DT1-SNC = 1
               IF  DT1-KIN < 0
                   MOVE "売上" TO PK-DCM
               ELSE
                   MOVE "値引" TO PK-DCM
               END-IF
           END-IF
           IF  DT1-DC = 2
               MOVE "不良返品" TO PK-DCD
           END-IF
           IF  DT1-DC = 3
               MOVE "預り保管" TO PK-DCD
           END-IF
           IF  DT1-DC = 5 OR 9
               IF  DT1-CSC = 9
                   MOVE "税調整分" TO PK-DCD
               END-IF
           END-IF
           IF  DT1-SKU NOT = ZERO AND 999999
               MOVE DT1-SKU TO PK-MNGP
           END-IF.
       KM1-EX.
           EXIT.
       KKE-RTN.
           MOVE 0 TO W-BIC.
           COMPUTE W-TKIN = W-AKIN + W-SHZ.
           MOVE SPACE TO W-P2K.
           MOVE W-15K TO PK-15K2.
           MOVE SPACE TO PK-BI PK-TME PK-SZM.
           MOVE "　合　計" TO PK-TME.
           IF  DT1-GNO = 1
               IF  DT1-DC = 5 OR 9
                   GO TO KKE-050
               END-IF
           END-IF
           MOVE W-AKIN TO PK-GKIN.
           MOVE DT1-TEKI TO PK-BI.
           MOVE 1 TO W-BIC.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
               PERFORM KM1-RTN THRU KM1-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2K TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P2K.
           MOVE W-15K TO PK-15K2.
           MOVE SPACE TO PK-BI PK-TME PK-SZM.
       KKE-050.
           IF  W-BIC = 0
               MOVE DT1-TEKI TO PK-BI
           END-IF
           MOVE "　消費税" TO PK-SZM.
           MOVE W-SHZ TO PK-GKIN.
           MOVE W-TKIN TO PK-TKIN.
           MOVE SPACE TO SP-R.
           MOVE W-P2K TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-YOKO TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       KKE-EX.
           EXIT.
       ERR-RTN.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           GO TO ERR-RTN.
       ERR-EX.
           EXIT.

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      HMM110.
      *********************************************************
      *    PROGRAM         :  óöï®ïiñºÉ}ÉXÉ^Å[ÉÅÉìÉeÉiÉìÉX    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHM11                          *
      *    DATA WRITTN     :  01/  /                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=ÉÅÉìÉe , 1=ñ‚çáÇπ             *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  JS-SIGN            PIC  9(001).
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-MSG              PIC  X(030).
       01  HEAD01.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  X(040) VALUE SPACE.
           02  F              PIC  N(013) VALUE
                "óöï®Å@ïiñºÉ}ÉXÉ^Å[Å@ÉäÉXÉg".
           02  F              PIC  X(044) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068212078".
       01  HEAD02.
           02  F              PIC  X(106) VALUE SPACE.
           02  F              PIC  N(003) VALUE "çÏê¨ì˙".
           02  F              PIC  X(002) VALUE " '".
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(014) VALUE SPACE.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  N(014) VALUE
                "ÅñÅñÅñÅ@Å@óöï®ïiñºÉ}ÉXÉ^Å[Å@".
           02  F              PIC  N(012) VALUE
                "ÉvÉãÅ[ÉtÉäÉXÉgÅ@Å@ÅñÅñÅñ".
           02  F              PIC  X(024) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-NGP          PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(007) VALUE " ∫∞ƒﬁ  ".
           02  F              PIC  N(008) VALUE "ïiÅ@Å@Å@Å@Å@ñºÅ@".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(006) VALUE "ó™Å@Å@Å@èÃÅ@".
           02  F              PIC  X(015) VALUE SPACE.
           02  F              PIC  N(002) VALUE "Å@áB".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "ïîñÂ".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "Å@á@".
           02  F              PIC  N(002) VALUE "Å@áA".
           02  F              PIC  N(002) VALUE "Å@áC".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "ó\íËå¥âø".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "êUë÷íPâø".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "ó\íËîÑâø".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "ì¸êî".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "â¡ó∞".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "îpî‘".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "éÌï ".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(003) VALUE " ∞Ã".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(005) VALUE " ≤ ﬂ-".
           02  F              PIC  X(004) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "Å@êe".
           02  F              PIC  X(002) VALUE " )".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(003) VALUE "1  ".
           02  F              PIC  N(002) VALUE "ÇRçÜ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "ÇQçÜ".
           02  F              PIC  X(025) VALUE
                "  SS    S    M    L   LL ".
           02  F              PIC  X(015) VALUE " 28.0 29.0 30.0".
           02  F              PIC  X(043) VALUE SPACE.
           02  F              PIC  N(004) VALUE "ìæà”êÊÅ@".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "íçÅ@Å@éﬂ".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "Å@ìoò^ì˙".
           02  F              PIC  N(004) VALUE "Å@îpé~ì˙".
       01  HEAD4.
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  X(051) VALUE
                "2 12.5 13.0 13.5 14.0 15.0 16.0 17.0 18.0 19.0 20.0".
           02  F              PIC  X(077) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  X(051) VALUE
                "3 21.0 21.5 22.0 22.5 23.0 23.5 24.0 24.5 25.0     ".
           02  F              PIC  X(077) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  X(048) VALUE
                "4 24.0 24.5 25.0 25.5 26.0 26.5 27.0 27.5       ".
           02  F              PIC  N(002) VALUE "óaÇË".
           02  F              PIC  X(077) VALUE SPACE.
       01  W-P1.
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(001).
           02  P-SMS          PIC  N(016).
           02  F              PIC  X(001).
           02  P-BC3          PIC  9(002).
           02  F              PIC  X(002).
           02  P-BMC          PIC  9(002).
           02  F              PIC  X(001).
           02  P-BC1          PIC  9(002).
           02  P-BC21         PIC  9(001).
           02  F              PIC  X(002).
           02  P-BC22         PIC  9(001).
           02  F              PIC  X(002).
           02  P-BC4          PIC  9(001).
           02  F              PIC  X(001).
           02  P-YG           PIC ZZ,ZZ9.
           02  F              PIC  X(001).
           02  P-FT           PIC ZZ,ZZ9.
           02  F              PIC  X(001).
           02  P-SB           PIC ZZ,ZZZ.
           02  F              PIC  X(001).
           02  P-ISU          PIC  Z(003).
           02  F              PIC  X(003).
           02  P-KRC          PIC  Z(001).
           02  F              PIC  X(003).
           02  P-SCC          PIC  Z(001).
           02  F              PIC  X(003).
           02  P-SSC          PIC  Z(001).
           02  F              PIC  X(003).
           02  P-HKB          PIC  Z(001).
           02  F              PIC  X(004).
           02  P-HPV          PIC  Z(001).
           02  F              PIC  X(005).
       01  W-P2.
           02  P-MHCD         PIC  9(006).
           02  P-C            PIC  X(001).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  9(001).
           02  P-ASC   OCCURS  10.
             03  F            PIC  X(003).
             03  P-SC         PIC  9(001).
             03  F            PIC  X(001).
           02  F              PIC  X(045).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-CS           PIC  N(010).
           02  F              PIC  X(001).
           02  P-SNG          PIC 99/99.
           02  F              PIC  X(001).
           02  P-ENG          PIC 99/99.
       01  W-R.
           02  W-KEY2.
             03  W-MHCD       PIC  9(006).
             03  W-KEY        PIC  9(006).
           02  W-NAME         PIC  N(024).
           02  W-NAMD  REDEFINES W-NAME.
             03  W-NAD   OCCURS 24.
               04  W-NA       PIC  N(001).
           02  W-NAAD  REDEFINES W-NAME.
             03  W-NAA   OCCURS 48  PIC  X(001).
           02  W-BC.
             03  W-BCD1.
               04  W-BC1      PIC  9(002).
               04  W-BC21     PIC  9(001).
             03  W-BC22       PIC  9(001).
             03  W-BC3        PIC  9(002).
           02  W-SSD.
             03  W-SS    OCCURS 40.
               04  W-S        PIC  9(001).
           02  W-SKDD  REDEFINES W-SSD.
             03  W-SKD   OCCURS  4.
               04  W-SK       PIC  9(010).
           02  W-SB           PIC  9(005).
           02  W-FT           PIC  9(005).
           02  F              PIC  X(019).
           02  W-KT           PIC  9(005).
           02  W-TCD          PIC  9(004).
           02  W-ISU          PIC  9(003).
           02  W-KRC          PIC  9(001).
           02  W-SCC          PIC  9(001).
           02  W-BMC          PIC  9(002).
           02  W-BMNO         PIC  9(001).
           02  W-YG           PIC  9(005).
           02  W-HKB          PIC  9(001).
           02  W-HPV          PIC  9(001).
           02  W-BC4          PIC  9(001).
           02  W-SSC          PIC  9(001).
           02  F              PIC  X(005).
           02  W-YG2          PIC  9(005).
           02  W-SMS          PIC  N(016).
           02  W-SMSD  REDEFINES W-SMS.
             03  W-MSD   OCCURS 16.
               04  W-MS       PIC  N(001).
           02  W-SMD   REDEFINES W-SMS.
             03  W-SM    OCCURS 32  PIC  X(001).
           02  F              PIC  9(006).
           02  F              PIC  9(006).
           02  F              PIC  X(001).
           02  W-CS           PIC  N(010).
           02  F              PIC  X(006).
           02  W-DNG          PIC  9(006).
           02  W-SNG          PIC  9(004).
           02  W-SNGD    REDEFINES W-SNG.
             03  W-SNEN       PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-ENG          PIC  9(004).
           02  W-ENGD    REDEFINES W-ENG.
             03  W-ENEN       PIC  9(002).
             03  W-EGET       PIC  9(002).
       01  W-DATA.
           02  W-END          PIC  9(001) VALUE ZERO.
           02  W-OVER         PIC  9(001) VALUE ZERO.
           02  W-INV          PIC  9(001).
           02  W-ZC           PIC  9(001).
           02  W-POC          PIC  9(001) VALUE ZERO.
           02  W-ACT          PIC  9(001) VALUE ZERO.
           02  W-ACTD         PIC  9(001).
           02  W-ACP          PIC  9(001).
           02  W-PMC          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-SKP          PIC  9(001).
           02  W-IC           PIC  9(001).
           02  W-LC.
             03  W-L          PIC  9(002).
             03  W-C          PIC  9(002).
           02  CNT            PIC  9(002).
           02  CNTD           PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-DCDD.
             03  W-DCD   OCCURS   4.
               04  W-DC       PIC  9(001).
           02  W-RC           PIC  9(001).
           02  W-FTD          PIC  9(005).
           02  W-KRCN         PIC  N(001).
           02  W-SCCN         PIC  N(002).
           02  W-SSCN         PIC  X(003).
           02  W-HKBN         PIC  X(003).
           02  W-BMCN         PIC  N(003).
           02  W-SEKEY.
             03  W-SKEY       PIC  9(006).
             03  W-EKEY       PIC  9(006).
           02  W-PAGE         PIC  9(003) VALUE ZERO.
           02  W-NGP          PIC  9(006).
           02  W-NG           PIC  9(006).
           02  W-NGD   REDEFINES W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NEND  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGSD  REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-OMD.
             03  W-OMHCD      PIC  9(006).
             03  W-OBC.
               04  W-OBCD1    PIC  9(003).
               04  W-OBC22    PIC  9(001).
               04  W-OBC3     PIC  9(002).
             03  W-OBMC       PIC  9(002).
             03  W-OBC4       PIC  9(001).
             03  W-OSSD.
               04  W-OSS    OCCURS 40.
                 05  W-OS     PIC  9(001).
             03  W-OSKDD REDEFINES W-OSSD.
               04  W-OSKD   OCCURS  4.
                 05  W-OSK    PIC  9(010).
           02  W-HCD.
             03  W-HCD1       PIC  9(004).
             03  W-HCD2       PIC  9(002).
           02  W-NMD          PIC  N(024).
           02  W-NMDC  REDEFINES W-NMD.
             03  W-NMDD  OCCURS  24.
               04  W-NM       PIC  N(001).
           02  W-BUD.
             03  W-BMHCD      PIC  9(006).
             03  W-BNAME      PIC  N(024).
             03  W-BBCD1      PIC  9(003).
             03  W-BBC22      PIC  9(001).
             03  W-BBC3       PIC  9(002).
             03  W-BFT        PIC  9(005).
             03  W-BSB        PIC  9(005).
             03  W-BYG        PIC  9(005).
             03  W-BYG2       PIC  9(005).
             03  W-BISU       PIC  9(003).
             03  W-BKRC       PIC  9(001).
             03  W-BSCC       PIC  9(001).
             03  W-BSSC       PIC  9(001).
             03  W-BHKB       PIC  9(001).
             03  W-BHPV       PIC  9(001).
             03  W-BBC4       PIC  9(001).
             03  W-BBMC       PIC  9(002).
             03  W-BSSD.
               04  W-BSS    OCCURS  40.
                 05  W-BS     PIC  9(001).
             03  W-BSNG       PIC  9(004).
             03  W-BENG       PIC  9(004).
             03  W-BTCD       PIC  9(004).
             03  W-BCS        PIC  N(010).
             03  W-BSMS       PIC  N(016).
           02  W-DDD.
             03  W-DD    OCCURS  20.
               04  W-DHCD     PIC  9(006).
           02  W-DCNT         PIC  9(002).
           02  W-BCN21        PIC  N(004).
           02  W-BCN4         PIC  N(003).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LIHHTF.
           COPY LIHUHM.
           COPY LIHKBM.
           COPY LITM.
           COPY LNJZAI.
           COPY LITHTM.
           COPY LSPF.
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
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  FILLER.
             03  A-HCD   PIC  9(006).
             03  A-HCD1  PIC  9(004).
             03  A-NAME  PIC  N(024).
           02  A-MHCD  PIC  9(006).
           02  FILLER.
             03  A-BC3   PIC  9(002).
             03  A-SMS   PIC  N(016).
             03  A-YG2   PIC  9(005).
           02  FILLER.
             03  A-BMC   PIC  9(002).
             03  A-YG    PIC  9(005).
             03  A-KRC   PIC  9(001).
           02  FILLER.
             03  A-BCD1  PIC  9(003).
             03  A-FT    PIC  9(005).
             03  A-SCC   PIC  9(001).
           02  FILLER.
             03  A-BC22  PIC  9(001).
             03  A-SB    PIC  9(005).
             03  A-SSC   PIC  9(001).
           02  FILLER.
             03  A-BC4   PIC  9(001).
             03  A-ISU   PIC  9(003).
             03  A-HKB   PIC  9(001).
           02  FILLER.
             03  A-HPV   PIC  9(001).
           02  A-S     PIC  9(001).
           02  FILLER.
             03  A-SNG   PIC  9(004).
             03  A-ENG   PIC  9(004).
             03  A-CS    PIC  N(010).
           02  A-TCD   PIC  9(004).
           02  A-PMC   PIC  9(001).
           02  FILLER.
             03  A-SKEY  PIC  9(006).
             03  A-EKEY  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-MHNA  PIC  N(024).
             03  D-CHNA  PIC  X(048) VALUE
                  "                                                ".
           02  D-BCN3  PIC  N(003).
           02  D-YG2   PIC  Z(005).
           02  FILLER.
             03  D-BM.
               04  D-BMC   PIC  Z(002).
               04  D-BMCN  PIC  N(004).
             03  D-YG    PIC  Z(005).
             03  D-KR.
               04  D-KRC   PIC  Z(001).
               04  D-KRCN  PIC  N(001).
           02  FILLER.
             03  D-BCN12.
               04  D-BCN1  PIC  N(008).
               04  D-BCN21 PIC  N(004).
             03  D-FT    PIC  Z(005).
             03  D-SC.
               04  D-SCC   PIC  Z(001).
               04  D-SCCN  PIC  N(002).
           02  FILLER.
             03  D-BCN22 PIC  N(003).
             03  D-SB    PIC  Z(005).
             03  D-SSCM.
               04  D-SSC   PIC  9(001).
               04  D-SSCN  PIC  X(003).
           02  FILLER.
             03  D-BCN4  PIC  N(003).
             03  D-ISU   PIC  Z(003).
             03  D-HK.
               04  D-HKB   PIC  Z(001).
               04  D-HKBN  PIC  X(003).
           02  D-S     PIC  Z(001).
           02  FILLER.
             03  D-SNGC  PIC  X(004) VALUE "    ".
             03  D-ENGC  PIC  X(004) VALUE "    ".
           02  D-TCD   PIC  N(026).
       01  C-MID.
           02  FILLER  PIC  X(050) VALUE
                "ÅñÅñÅñÅ@Å@óöï®ïiñºÉ}ÉXÉ^Å[Å@ÉÅÉìÉeÉiÉìÉXÅ@Å@ÅñÅñÅñ".
           02  FILLER  PIC  X(037) VALUE
                "ìoò^=1 èCê≥=2 çÌèú=3 çÏï\=4 ñ‚çáÇπ=5 ".
           02  FILLER  PIC  X(024) VALUE
                "àÍäáçÌèú=8 èIóπ=9   ÿ¿∞›".
           02  FILLER  PIC  X(022) VALUE
                 "ämîF  OK=1 NO=9   ÿ¿∞›".
       01  C-PM.
           02  FILLER  PIC  X(039) VALUE
                 "ëÂå©èoÇµÅ@çÏï\Å@Ç∑ÇÈ=5  ÇµÇ»Ç¢=0   ÿ¿∞›".
           02  FILLER  PIC  X(044) VALUE
                 "<  ïiñº∫∞ƒﬁ        ÇÊÇË        Ç‹Ç≈ë≈èoÇµ  >".
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(030).
             03  E-FILE  PIC  X(013).
             03  E-HI    PIC  9(006).
             03  E-HUH   PIC  9(006).
             03  E-HHT   PIC  9(007).
             03  E-THT   PIC  9(011).
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
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "181" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "64" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "5" "0" "58" "A-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "5" "11" "6" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD1" "9" "5" "11" "4" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD1" BY REFERENCE W-HCD1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NAME" "N" "5" "25" "48" "A-HCD1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NAME" BY REFERENCE W-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-MHCD" "9" "6" "11" "6" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-MHCD" BY REFERENCE W-MHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "7" "0" "39" "A-MHCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BC3" "9" "7" "12" "2" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BC3" BY REFERENCE W-BC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SMS" "N" "7" "30" "32" "A-BC3" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SMS" BY REFERENCE W-SMS "32" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YG2" "9" "7" "74" "5" "A-SMS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YG2" BY REFERENCE W-YG2 "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-ACP" " " "8" "0" "8" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BMC" "9" "8" "12" "2" " " "05C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BMC" BY REFERENCE W-BMC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YG" "9" "8" "53" "5" "A-BMC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YG" BY REFERENCE W-YG "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KRC" "9" "8" "70" "1" "A-YG" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KRC" BY REFERENCE W-KRC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-ACP" " " "9" "0" "9" "05C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BCD1" "9" "9" "12" "3" " " "06C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BCD1" BY REFERENCE W-BCD1 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FT" "9" "9" "53" "5" "A-BCD1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FT" BY REFERENCE W-FT "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SCC" "9" "9" "70" "1" "A-FT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SCC" BY REFERENCE W-SCC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-ACP" " " "10" "0" "7" "06C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BC22" "9" "10" "12" "1" " " "07C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BC22" BY REFERENCE W-BC22 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SB" "9" "10" "53" "5" "A-BC22" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SB" BY REFERENCE W-SB "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SSC" "9" "10" "70" "1" "A-SB" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SSC" BY REFERENCE W-SSC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-ACP" " " "11" "0" "5" "07C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BC4" "9" "11" "12" "1" " " "08C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BC4" BY REFERENCE W-BC4 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ISU" "9" "11" "55" "3" "A-BC4" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ISU" BY REFERENCE W-ISU "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HKB" "9" "11" "70" "1" "A-ISU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HKB" BY REFERENCE W-HKB "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-ACP" " " "12" "0" "1" "08C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HPV" "9" "12" "70" "1" " " "09C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HPV" BY REFERENCE W-HPV "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S" "9" "W-L" "W-C" "1" "09C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S" BY REFERENCE W-S(1) "1" "1" BY REFERENCE CNT 1
            RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-ACP" " " "22" "0" "28" "A-S" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNG" "9" "22" "4" "4" " " "11C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNG" BY REFERENCE W-SNG "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENG" "9" "22" "9" "4" "A-SNG" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENG" BY REFERENCE W-ENG "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CS" "N" "22" "23" "20" "A-ENG" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CS" BY REFERENCE W-CS "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "21" "23" "4" "11C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PMC" "9" "6" "51" "1" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PMC" BY REFERENCE W-PMC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "14C-ACP" " " "12" "0" "12" "A-PMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SKEY" "9" "12" "25" "6" " " "14C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SKEY" BY REFERENCE W-SKEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EKEY" "9" "12" "37" "6" "A-SKEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EKEY" BY REFERENCE W-EKEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "70" "1" "14C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "248" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "6" "0" "96" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MHNA" "N" "6" "25" "48" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-MHNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CHNA" "X" "6" "25" "48" "D-MHNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BCN3" "N" "7" "16" "6" "01C-DSP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-BCN3" BY REFERENCE HKB-BRN3 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-YG2" "Z" "7" "74" "5" "D-BCN3" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-YG2" BY REFERENCE W-YG2 "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-DSP" " " "8" "0" "18" "D-YG2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BM" " " "8" "0" "10" " " "04C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BMC" "Z" "8" "12" "2" " " "D-BM" RETURNING RESU.
       CALL "SD_From" USING 
            "D-BMC" BY REFERENCE W-BMC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BMCN" "N" "8" "16" "8" "D-BMC" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-BMCN" BY REFERENCE W-BMCN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-YG" "Z" "8" "53" "5" "D-BM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-YG" BY REFERENCE W-YG "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KR" " " "8" "0" "3" "D-YG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KRC" "Z" "8" "70" "1" " " "D-KR" RETURNING RESU.
       CALL "SD_From" USING 
            "D-KRC" BY REFERENCE W-KRC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KRCN" "N" "8" "72" "2" "D-KRC" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-KRCN" BY REFERENCE W-KRCN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-DSP" " " "9" "0" "34" "04C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BCN12" " " "9" "0" "24" " " "05C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BCN1" "N" "9" "16" "16" " " "D-BCN12" RETURNING RESU.
       CALL "SD_From" USING 
            "D-BCN1" BY REFERENCE HKB-BRN1 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BCN21" "N" "9" "33" "8" "D-BCN1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-BCN21" BY REFERENCE W-BCN21 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-FT" "Z" "9" "53" "5" "D-BCN12" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-FT" BY REFERENCE W-FT "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SC" " " "9" "0" "5" "D-FT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SCC" "Z" "9" "70" "1" " " "D-SC" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SCC" BY REFERENCE W-SCC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SCCN" "N" "9" "72" "4" "D-SCC" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SCCN" BY REFERENCE W-SCCN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-DSP" " " "10" "0" "15" "05C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BCN22" "N" "10" "16" "6" " " "06C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-BCN22" BY REFERENCE HKB-BRN22 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SB" "Z" "10" "53" "5" "D-BCN22" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SB" BY REFERENCE W-SB "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SSCM" " " "10" "0" "4" "D-SB" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SSC" "9" "10" "70" "1" " " "D-SSCM" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SSC" BY REFERENCE W-SSC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SSCN" "X" "10" "72" "3" "D-SSC" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SSCN" BY REFERENCE W-SSCN "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-DSP" " " "11" "0" "13" "06C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BCN4" "N" "11" "16" "6" " " "07C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-BCN4" BY REFERENCE W-BCN4 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ISU" "Z" "11" "55" "3" "D-BCN4" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-ISU" BY REFERENCE W-ISU "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HK" " " "11" "0" "4" "D-ISU" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HKB" "Z" "11" "70" "1" " " "D-HK" RETURNING RESU.
       CALL "SD_From" USING 
            "D-HKB" BY REFERENCE W-HKB "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HKBN" "X" "11" "72" "3" "D-HKB" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-HKBN" BY REFERENCE W-HKBN "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-S" "Z" "W-L" "W-C" "1" "07C-DSP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-S" BY REFERENCE W-S(1) "1" "1" BY REFERENCE CNT 1
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-DSP" " " "22" "0" "8" "D-S" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SNGC" "X" "22" "4" "4" " " "09C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ENGC" "X" "22" "9" "4" "D-SNGC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TCD" "N" "21" "28" "52" "09C-DSP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-TCD" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "133" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "X" "1" "14" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "3" "8" "37" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "3" "45" "24" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "23" "53" "22" "03C-MID" " " RETURNING RESU.
      *C-PM
       CALL "SD_Init" USING 
            "C-PM" " " "0" "0" "83" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-PM" "X" "6" "17" "39" " " "C-PM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-PM" "X" "12" "13" "44" "01C-PM" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "73" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "73" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME" "X" "24" "15" "30" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-ME" BY REFERENCE W-MSG "30" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-FILE" "X" "24" "46" "13" "E-ME" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-FILE" BY REFERENCE W-FILE "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HI" "9" "24" "60" "6" "E-FILE" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HI" BY REFERENCE HI-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HUH" "9" "24" "60" "6" "E-HI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HUH" BY REFERENCE HUH-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HHT" "9" "24" "60" "7" "E-HUH" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HHT" BY REFERENCE HHT-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-THT" "9" "24" "60" "11" "E-HHT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-THT" BY REFERENCE THT-KEY "11" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           IF  JS-SIGN = 1
               CALL "DB_F_Open" USING
                "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
                "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE
                HI-KEY2
           ELSE
               CALL "DB_F_Open" USING
                "I-O" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
                "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE
                HI-KEY2
               CALL "DB_F_Open" USING
                "I-O" HUH-M_PNAME1 "SHARED" BY REFERENCE HUH-M_IDLST "1"
                "HUH-KEY" BY REFERENCE HUH-KEY
               CALL "DB_F_Open" USING
                "I-O" HHTF_PNAME1 "SHARED" BY REFERENCE HHTF_IDLST "2"
                "HHT-KEY" BY REFERENCE HHT-KEY "HHT-KEY2" BY REFERENCE
                HHT-KEY2
               CALL "DB_F_Open" USING
                "INPUT" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST
                "1" "NJZAI-KEY" BY REFERENCE NJZAI-KEY
           END-IF
           ACCEPT W-NGP FROM DATE.
           COPY LIBCPR.
           MOVE ZERO TO W-NG.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE DATE-02R TO H-DATE H-NGP.
       M-15.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHM11" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-ACT = 9
               GO TO M-95
           END-IF
           IF  W-ACT = 4
               GO TO M-70
           END-IF
           IF  JS-SIGN = 0
               IF  W-ACT NOT = 1 AND 2 AND 3 AND 5 AND 8
                   GO TO M-15
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  W-ACT NOT = 5
                   GO TO M-15
               END-IF
           END-IF
      *
           INITIALIZE W-R.
           MOVE SPACE TO W-NAME W-CS.
       M-25.
           IF (W-ACT NOT = 1) OR (W-KEY = ZERO)
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Screen_Output" USING "SCHM11" RETURNING RESU
               CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU
               INITIALIZE W-R
               MOVE SPACE TO W-NAME W-CS
           END-IF
           IF  W-ACT = 8
               CALL "SD_Accept" USING BY REFERENCE A-HCD1 "A-HCD1"
                "9" "4" BY REFERENCE ESTAT RETURNING RESU
           ELSE
               CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
                BY REFERENCE ESTAT RETURNING RESU
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               IF  W-ACT NOT = 1 AND 8
                   GO TO M-30
               END-IF
           END-IF
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-ACT NOT = 8
               IF  W-HCD = ZERO
                   GO TO M-25
               END-IF
           END-IF
           IF  W-ACT = 8
               IF  W-HCD1 = ZERO OR 9999
                   GO TO M-25
               END-IF
           END-IF.
       M-30.
           PERFORM SET-RTN THRU SET-EX.
           IF  W-END = 9
               GO TO M-95
           END-IF
           IF  W-ACP = 9
               GO TO M-25
           END-IF.
      *--------------   ìoò^ÅEèCê≥Å@ì¸óÕ   -----------------------------
       M-35.
           PERFORM ACP-RTN THRU ACP-EX.
           IF  W-END = 9
               GO TO M-95
           END-IF
           IF  W-ACP = 9
               GO TO M-25
           END-IF
           IF  W-ACT = 5
               GO TO M-65
           END-IF
      *-----------------   ÇeÇhÇkÇdÅ@ÇvÇqÇhÇsÇd   ----------------------
           IF  W-ACT NOT = 1
               GO TO M-50
           END-IF.
       M-45.
           PERFORM WRI-RTN THRU WRI-EX.
           IF  W-END = 9
               GO TO M-95
           END-IF
           GO TO M-60.
      *------------------   ÇeÇhÇkÇdÅ@ÇqÇdÇvÇqÇhÇsÇd   -----------------
       M-50.
           IF  W-ACT NOT = 2
               GO TO M-55
           END-IF
           PERFORM REW-RTN THRU REW-EX.
           IF  W-END = 9
               GO TO M-95
           END-IF
           GO TO M-60.
      *------------------   ÇeÇhÇkÇdÅ@ÇcÇdÇkÇdÇsÇd   -------------------
       M-55.
           PERFORM DEL-RTN THRU DEL-EX.
           IF  W-END = 9
               GO TO M-95
           END-IF.
      *---------------   É}ÉXÉ^Å[ëóêMÉtÉ@ÉCÉãÅ@çÏê¨   ------------------
       M-60.
           IF  W-ACT = 1
               IF  W-MHCD NOT = W-KEY
                   GO TO M-25
               END-IF
           END-IF.
       M-65.
           IF  W-ACTD NOT = 0
               MOVE W-ACTD TO W-ACT
           END-IF
           GO TO M-25.
      *-----------------   ÉvÉãÅ[ÉtÅ@ÉäÉXÉg   --------------------------
       M-70.
           PERFORM PRI-RTN THRU PRI-EX.
           GO TO M-15.
      *--------------------   ÇdÅ@ÇmÅ@Çc   -----------------------------
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           IF  JS-SIGN NOT = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HHTF_IDLST HHTF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HUH-M_IDLST HUH-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE NJZAI_IDLST NJZAI_PNAME1
           END-IF
           IF  W-POC = 9
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      ******************************************
      *****    ì¸óÕâ¬î\Å@É`ÉFÉbÉNÅ@Å@Å@    *****
      ******************************************
       SET-RTN.
           MOVE 0 TO W-ACP.
           IF  W-ACT NOT = 8
               GO TO SET-040
           END-IF
           MOVE SPACE TO HI-KEY.
           MOVE W-HCD1 TO HI-HCD1.
      *///////////////add koyama 20170303
           CALL "DB_Select" USING
            HI-M_PNAME1 "WHERE"
            "HI-HCD1" "=" W-HCD1 
            RETURNING RET.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               GO TO SET-300
           END-IF
      *           READ HI-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO SET-300
           END-IF
      *           IF  W-HCD1 NOT = HI-HCD1
      *               GO TO SET-300
      *           END-IF
      *
           MOVE HI-R TO W-R.
           MOVE HI-KEY TO W-HCD.
           MOVE SPACE TO W-NMD.
           MOVE HI-NAME TO W-NMD.
           MOVE SPACE TO W-NAME.
           MOVE ZERO TO CNT.
       SET-020.
           ADD 1 TO CNT.
           IF  CNT = 25
               CALL "SD_Output" USING
                "A-NAME" A-NAME "p" RETURNING RESU
               GO TO SET-100
           END-IF
           MOVE W-NM(CNT) TO W-NA(CNT).
           IF  W-NA(CNT) NOT = SPACE
               GO TO SET-020
           END-IF
           ADD 1 TO CNT.
           IF  CNT = 25
               CALL "SD_Output" USING
                "A-NAME" A-NAME "p" RETURNING RESU
               GO TO SET-100
           END-IF
           MOVE W-NM(CNT) TO W-NA(CNT).
           IF  W-NA(CNT) NOT = SPACE
               GO TO SET-020
           END-IF
           CALL "SD_Output" USING "A-NAME" A-NAME "p" RETURNING RESU.
           GO TO SET-100.
       SET-040.
           IF  W-HCD = ZERO
               GO TO SET-060
           END-IF
           MOVE W-HCD TO HI-KEY.
           IF  JS-SIGN = 0
      *               READ HI-M INVALID KEY
      *///////////////
               CALL "DB_Read" USING
                "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R " "
                RETURNING RET
               IF  RET = 1
                   GO TO SET-300
               END-IF
           END-IF
           IF  JS-SIGN = 1
      *               READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
               CALL "DB_Read" USING
                "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
                RETURNING RET
               IF  RET = 1
                   GO TO SET-300
               END-IF
           END-IF
           GO TO SET-080.
       SET-060.
      *           READ HI-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO SET-300
           END-IF
           MOVE HI-KEY TO W-HCD.
       SET-080.
           MOVE HI-R TO W-R.
           IF  W-ACT NOT = 5
               PERFORM DO1-RTN THRU DO1-EX
           END-IF
           IF  W-END = 9
               GO TO SET-EX
           END-IF
           PERFORM CRT-RTN THRU CRT-EX.
           IF  W-ACP = 9
               GO TO SET-EX
           END-IF
           IF  W-ACT = 5
               GO TO SET-EX
           END-IF
           IF  W-ACT = 2
               MOVE W-FT TO W-FTD
               INITIALIZE W-OMD
               MOVE W-MHCD TO W-OMHCD
               MOVE W-BC TO W-OBC
               MOVE W-BMC TO W-OBMC
               MOVE W-SKDD TO W-OSKDD
               GO TO SET-EX
           END-IF
           IF  W-ACT = 1
               MOVE SPACE TO W-MSG
               MOVE "***  ìoò^çœÇ›  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 9 TO W-ACP
               GO TO SET-EX
           END-IF.
       SET-100.
           MOVE W-HCD TO HUH-HCD.
      *           READ HUH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO SET-120
           END-IF
           IF  ZERO NOT = HUH-ZS OR HUH-ZK OR HUH-NS OR HUH-NK OR
                          HUH-SS OR HUH-SK OR HUH-YS OR HUH-YK OR HUH-UG
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  DATA ∂ﬁ ZERO √ﬁ≈≤  ***" TO W-MSG
               MOVE "HUHM" TO W-FILE
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HUH" E-HUH "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-ACP
               GO TO SET-EX
           END-IF.
       SET-120.
           MOVE SPACE TO HHT-KEY.
           MOVE W-HCD TO HHT-HCD.
      *           START HHTF KEY NOT < HHT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HHTF_PNAME1 "HHT-KEY" " NOT < " HHT-KEY RETURNING RET.
           IF  RET = 1
               GO TO SET-180
           END-IF.
      *///////////////add koyama 20170303
           CALL "DB_Select" USING
            HHTF_PNAME1 "WHERE"
            "HHT-HCD" "=" W-HCD 
            RETURNING RET.
       SET-140.
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO SET-180
           END-IF
      *           IF  W-HCD NOT = HHT-HCD
      *               GO TO SET-180
      *           END-IF
      *
           MOVE ZERO TO CNT.
       SET-160.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO SET-140
           END-IF
           IF  ZERO = HHT-ZSU(CNT) AND HHT-NSU(CNT) AND HHT-USU(CNT)
               GO TO SET-160
           END-IF
           MOVE SPACE TO W-MSG W-FILE.
           MOVE "***  DATA ∂ﬁ ZERO √ﬁ≈≤  ***" TO W-MSG.
           MOVE "HHTF" TO W-FILE.
           CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU.
           CALL "SD_Output" USING "E-FILE" E-FILE "p" RETURNING RESU.
           CALL "SD_Output" USING "E-HHT" E-HHT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           MOVE 9 TO W-ACP.
           GO TO SET-EX.
       SET-180.
           IF  HI-MHCD NOT = HI-HCD
               GO TO SET-260
           END-IF
           MOVE SPACE TO HI-KEY2.
           MOVE W-HCD TO HI-MHCD.
      *///////////////add koyama 20170303
           CALL "DB_Select" USING
            HI-M_PNAME1 "WHERE"
            "HI-HCD" "NOT =" "HI-MHCD" "AND" "HI-MHCD" "=" W-HCD
            RETURNING RET.
      *           START HI-M KEY NOT < HI-KEY2 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY2" " NOT < " HI-KEY2 RETURNING RET.
           IF  RET = 1
               GO TO SET-220
           END-IF.
       SET-200.
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO SET-220
           END-IF
      *           IF  W-HCD NOT = HI-MHCD
      *               GO TO SET-220
      *           END-IF
      *           IF  HI-MHCD = HI-HCD
      *               GO TO SET-200
      *           END-IF
           MOVE SPACE TO W-MSG.
           MOVE "***  ∫ ∫∞ƒﬁ ±ÿ  ***" TO W-MSG.
           CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU.
           CALL "SD_Output" USING "E-HI" E-HI "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           MOVE 9 TO W-ACP.
           GO TO SET-EX.
       SET-220.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  DATA ≈º  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 9 TO W-ACP
               GO TO SET-EX
           END-IF.
       SET-260.
           IF  W-ACT = 3
               GO TO SET-EX
           END-IF
      *           READ HI-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO SET-EX
           END-IF
           IF  W-HCD NOT = HI-KEY
               GO TO SET-EX
           END-IF
           MOVE HI-R TO W-R.
           MOVE HI-KEY TO W-HCD.
           GO TO SET-100.
       SET-300.
           IF  W-ACT NOT = 1
               MOVE SPACE TO W-MSG
               MOVE "***  DATA ≈º  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 9 TO W-ACP
               GO TO SET-EX
           END-IF
           MOVE W-HCD TO W-KEY.
           PERFORM DO2-RTN THRU DO2-EX.
       SET-EX.
           EXIT.
      *******************************
      *****    ìØä˙ämîF(ADD)Å@Å@*****
      *******************************
       DO1-RTN.
           MOVE W-KEY TO HUH-KEY.
      *           READ HUH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  HUHM ≈º  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               PERFORM HU1-RTN THRU HU1-EX
           END-IF
           IF  W-END = 9
               GO TO DO1-EX
           END-IF
           MOVE ZERO TO CNT.
       DO1-020.
           ADD 1 TO CNT.
           IF  CNT = 5
               GO TO DO1-EX
           END-IF
           IF  HI-SS(CNT) = ZERO
               GO TO DO1-020
           END-IF
           MOVE HI-KEY TO HHT-HCD.
           MOVE CNT TO HHT-SIZ.
      *           READ HHTF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  HHTF ≈º  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               PERFORM HH1-RTN THRU HH1-EX
           END-IF
           IF  W-END = 9
               GO TO DO1-EX
           END-IF
           GO TO DO1-020.
       DO1-EX.
           EXIT.
      *******************************
      *****    ìØä˙ämîF(DEL)Å@Å@*****
      *******************************
       DO2-RTN.
           MOVE 0 TO W-ZC.
           MOVE W-HCD TO HUH-HCD.
      *           READ HUH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO DO2-020
           END-IF
           IF  W-ZC = 0
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  DATA ±ÿ  ***" TO W-MSG
               MOVE "HUHM" TO W-FILE
               MOVE 1 TO W-ZC
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HUH" E-HUH "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF  ZERO = HUH-ZS AND HUH-ZK AND HUH-NS AND HUH-NK AND
                      HUH-SS AND HUH-SK AND HUH-YS AND HUH-YK AND HUH-UG
               GO TO DO2-020
           END-IF
           MOVE 9 TO W-ZC.
           MOVE SPACE TO W-MSG W-FILE.
           MOVE "***  DATA ∂ﬁ ZERO √ﬁ≈≤  ***" TO W-MSG.
           MOVE "HUHM" TO W-FILE.
           CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU.
           CALL "SD_Output" USING "E-FILE" E-FILE "p" RETURNING RESU.
           CALL "SD_Output" USING "E-HUH" E-HUH "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           MOVE 9 TO W-END.
           GO TO DO2-EX.
       DO2-020.
           MOVE SPACE TO HHT-KEY.
           MOVE W-HCD TO HHT-HCD.
      *           START HHTF KEY NOT < HHT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HHTF_PNAME1 "HHT-KEY" " NOT < " HHT-KEY RETURNING RET.
           IF  RET = 1
               GO TO DO2-080
           END-IF.
       DO2-040.
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO DO2-080
           END-IF
           IF  W-HCD NOT = HHT-HCD
               GO TO DO2-080
           END-IF
           IF  W-ZC = 0 OR 1
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  DATA ±ÿ  ***" TO W-MSG
               MOVE "HHTF" TO W-FILE
               MOVE 2 TO W-ZC
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HHT" E-HHT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           MOVE ZERO TO CNT.
       DO2-060.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO DO2-040
           END-IF
           IF  ZERO = HHT-ZSU(CNT) AND HHT-NSU(CNT) AND HHT-USU(CNT)
               GO TO DO2-060
           END-IF
           MOVE 9 TO W-ZC.
           MOVE SPACE TO W-MSG W-FILE.
           MOVE "***  DATA ∂ﬁ ZERO √ﬁ≈≤  ***" TO W-MSG.
           MOVE "HUHM" TO W-FILE.
           CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU.
           CALL "SD_Output" USING "E-FILE" E-FILE "p" RETURNING RESU.
           CALL "SD_Output" USING "E-HHT" E-HHT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           MOVE 9 TO W-END.
           GO TO DO2-EX.
       DO2-080.
           IF  W-ZC = 0
               GO TO DO2-EX
           END-IF
           MOVE W-HCD TO HUH-HCD.
      *           READ HUH-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DO2-100
           END-IF
      *           DELETE HUH-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HUH-M_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  DELETE ¥◊∞  ***" TO W-MSG
               MOVE "HUHM" TO W-FILE
               MOVE 9 TO W-END
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HUH" E-HUH "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO DO2-EX
           END-IF.
       DO2-100.
           MOVE SPACE TO HHT-KEY.
           MOVE W-HCD TO HHT-HCD.
      *///////////////add koyama 20170303
           CALL "DB_Select" USING
            HHTF_PNAME1 "WHERE"
            W-HCD "=" "HHT-HCD"
            RETURNING RET.
      *           START HHTF KEY NOT < HHT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HHTF_PNAME1 "HHT-KEY" " NOT < " HHT-KEY RETURNING RET.
           IF  RET = 1
               GO TO DO2-EX
           END-IF.
       DO2-120.
      *           READ HHTF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DO2-EX
           END-IF
      *           IF  W-HCD NOT = HHT-HCD
      *               GO TO DO2-EX
      *           END-IF
      *           DELETE HHTF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HHTF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  DELETE ¥◊∞  ***" TO W-MSG
               MOVE "HHTF" TO W-FILE
               MOVE 9 TO W-END
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HHT" E-HHT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO DO2-EX
           END-IF
           GO TO DO2-120.
       DO2-EX.
           EXIT.
      ****************************
      *****    âÊñ Å@ì¸óÕÅ@Å@*****
      ****************************
       ACP-RTN.
           MOVE 0 TO W-ACP W-SKP.
           IF  W-ACT = 3 OR 5 OR 8
               GO TO ACP-980
           END-IF.
       ACP-020.
           MOVE W-MHCD TO W-BMHCD.
           CALL "SD_Accept" USING BY REFERENCE A-MHCD "A-MHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "A-MHCD" A-MHCD "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-MHCD TO W-MHCD
               CALL "SD_Output" USING "A-MHCD" A-MHCD "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               MOVE 9 TO W-ACP
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND PF7 AND ADV
               GO TO ACP-020
           END-IF
           IF  W-MHCD = ZERO
               MOVE W-KEY TO W-MHCD
               CALL "SD_Output" USING "A-MHCD" A-MHCD "p" RETURNING RESU
               CALL "SD_Output" USING "D-MHNA" D-MHNA "p" RETURNING RESU
           END-IF
           IF  W-MHCD = W-KEY
               CALL "SD_Output" USING "D-CHNA" D-CHNA "p" RETURNING RESU
               GO TO ACP-040
           END-IF
           MOVE W-MHCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  À›“≤ ≈º  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 0 TO W-SKP
               GO TO ACP-020
           END-IF
           CALL "SD_Output" USING "D-MHNA" D-MHNA "p" RETURNING RESU.
           IF  HI-KEY NOT = HI-MHCD
               MOVE SPACE TO W-MSG
               MOVE "***  êe∫∞ƒﬁ √ﬁ≈≤  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 0 TO W-SKP
               GO TO ACP-020
           END-IF
           IF  W-ACT NOT = 1
               GO TO ACP-040
           END-IF
      *
           IF  W-NAME = SPACE
               MOVE HI-NAME TO W-NAME
               CALL "SD_Output" USING "A-NAME" A-NAME "p" RETURNING RESU
           END-IF
           IF  W-BC1 = ZERO
               MOVE HI-BCD1 TO W-BCD1
               CALL "SD_Output" USING "A-BCD1" A-BCD1 "p" RETURNING RESU
           END-IF
           IF  W-BC22 = ZERO
               MOVE HI-BC22 TO W-BC22
               CALL "SD_Output" USING "A-BC22" A-BC22 "p" RETURNING RESU
           END-IF
           IF  W-BC3 = ZERO
               MOVE HI-BC3 TO W-BC3
               CALL "SD_Output" USING "A-BC3" A-BC3 "p" RETURNING RESU
           END-IF
           IF  W-SB = ZERO
               MOVE HI-SB TO W-SB
               CALL "SD_Output" USING "D-SB" D-SB "p" RETURNING RESU
           END-IF
           IF  W-ISU = ZERO
               MOVE HI-ISU TO W-ISU
               CALL "SD_Output" USING "D-ISU" D-ISU "p" RETURNING RESU
           END-IF
           IF  W-KRC = ZERO
               MOVE HI-KRC TO W-KRC
               CALL "SD_Output" USING "A-KRC" A-KRC "p" RETURNING RESU
           END-IF
           IF  W-SSC = ZERO
               MOVE HI-SSC TO W-SSC
               CALL "SD_Output" USING "D-SSCM" D-SSCM "p" RETURNING RESU
           END-IF
           IF  W-HKB = ZERO
               MOVE HI-HKB TO W-HKB
               CALL "SD_Output" USING "A-HKB" A-HKB "p" RETURNING RESU
           END-IF
           IF  W-HPV = ZERO
               MOVE HI-HPV TO W-HPV
               CALL "SD_Output" USING "A-HPV" A-HPV "p" RETURNING RESU
           END-IF
           IF  W-BC4 = ZERO
               MOVE HI-BC4 TO W-BC4
               CALL "SD_Output" USING "A-HPV" A-HPV "p" RETURNING RESU
           END-IF
           IF  W-TCD = ZERO
               MOVE HI-TCD TO W-TCD
               CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU
           END-IF
           IF  W-SMS = SPACE
               MOVE HI-SMS TO W-SMS
               CALL "SD_Output" USING "A-SMS" A-SMS "p" RETURNING RESU
           END-IF
           IF  W-SSD = ZERO
               MOVE HI-ASSD TO W-SSD
               PERFORM CRT-030 THRU CRT-EX
           END-IF.
       ACP-040.
           IF (W-ACT = 2) AND (W-HCD NOT = HI-KEY)
               MOVE W-HCD TO HI-KEY
      *               READ HI-M INVALID KEY
      *///////////////
               CALL "DB_Read" USING
                "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R " "
                RETURNING RET
               IF  RET = 1
                   MOVE SPACE TO W-MSG
                   MOVE "***  ”ƒ… À›“≤ ≈º  ***" TO W-MSG
                   CALL "SD_Output" USING
                    "E-ME" E-ME "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   MOVE 0 TO W-SKP
                   GO TO ACP-020
               END-IF
           END-IF
           IF  W-ACT NOT = 2
               GO TO ACP-050
           END-IF
           IF  W-OMHCD = W-MHCD
               GO TO ACP-050
           END-IF
           MOVE SPACE TO NJZAI-KEY.
           MOVE 9 TO NJZAI-01.
           MOVE W-OMHCD TO NJZAI-02.
      *///////////////add koyama 20170303
           CALL "DB_Select" USING
            NJZAI_PNAME1 "WHERE"
            "NJZAI-01" "=" 9 "AND" "NJZAI-02" "=" W-OMHCD
            RETURNING RET.
      *           START NJZAI KEY NOT < NJZAI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            NJZAI_PNAME1 "NJZAI-KEY" " NOT < " NJZAI-KEY RETURNING RET.
           IF  RET = 1
               GO TO ACP-050
           END-IF
      *           READ NJZAI NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO ACP-050
           END-IF
      *           IF (NJZAI-01 NOT = 9) OR (NJZAI-02 NOT = W-OMHCD)
      *               GO TO ACP-050
      *           END-IF
           MOVE SPACE TO W-MSG.
           MOVE "***  ∏◊Õﬁ¬ ªﬁ≤∫ √ﬁ∞¿ ±ÿ  ***" TO W-MSG.
           CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
       ACP-050.
           IF  W-SKP NOT = 0
               GO TO ACP-070
           END-IF.
       ACP-060.
           MOVE W-NAME TO W-BNAME.
           CALL "SD_Accept" USING BY REFERENCE A-NAME "A-NAME" "N" "48"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "A-NAME" A-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-BNAME TO W-NAME
               CALL "SD_Output" USING "A-NAME" A-NAME "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND PF7 AND ADV
               GO TO ACP-060
           END-IF.
       ACP-070.
           IF  SPACE = W-NAA(01) OR W-NAA(03) OR W-NAA(05) OR W-NAA(07)
                    OR W-NAA(09) OR W-NAA(11) OR W-NAA(13) OR W-NAA(15)
                    OR W-NAA(17) OR W-NAA(19) OR W-NAA(21) OR W-NAA(23)
                    OR W-NAA(25) OR W-NAA(27) OR W-NAA(29) OR W-NAA(31)
                    OR W-NAA(33) OR W-NAA(35) OR W-NAA(37) OR W-NAA(39)
                    OR W-NAA(41) OR W-NAA(43) OR W-NAA(45) OR W-NAA(47)
               MOVE SPACE TO W-MSG
               MOVE "***  ∫”ºﬁ … ΩÕﬂ∞Ω ±ÿ  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 0 TO W-SKP
               GO TO ACP-060
           END-IF
           IF  W-SKP = 6
               MOVE 0 TO W-SKP
           END-IF
           IF  W-SKP NOT = 0
               GO TO ACP-074
           END-IF.
       ACP-072.
           MOVE W-SMS TO W-BSMS.
           CALL "SD_Accept" USING BY REFERENCE A-SMS "A-SMS" "N" "32"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "A-SMS" A-SMS "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-BSMS TO W-SMS
               CALL "SD_Output" USING "A-SMS" A-SMS "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND PF7 AND ADV
               GO TO ACP-072
           END-IF.
       ACP-074.
           IF  SPACE = W-SM(01) OR W-SM(03) OR W-SM(05) OR W-SM(07)
                    OR W-SM(09) OR W-SM(11) OR W-SM(13) OR W-SM(15)
                    OR W-SM(17) OR W-SM(19) OR W-SM(21) OR W-SM(23)
                    OR W-SM(25) OR W-SM(27) OR W-SM(29) OR W-SM(31)
               MOVE SPACE TO W-MSG
               MOVE "***  ∫”ºﬁ … ΩÕﬂ∞Ω ±ÿ  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 0 TO W-SKP
               GO TO ACP-072
           END-IF
           IF  W-SMS = SPACE
               GO TO ACP-072
           END-IF
           IF  W-SKP = 6
               MOVE 0 TO W-SKP
           END-IF
           IF  W-SKP NOT = 0
               GO TO ACP-084
           END-IF.
       ACP-082.
           MOVE W-BC3 TO W-BBC3.
           CALL "SD_Accept" USING BY REFERENCE A-BC3 "A-BC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "A-BC3" A-BC3 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-BBC3 TO W-BC3
               CALL "SD_Output" USING "A-BC3" A-BC3 "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-072
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND PF7 AND ADV
               GO TO ACP-082
           END-IF.
       ACP-084.
           MOVE SPACE TO HKB-KEY.
           MOVE "14" TO HKB-NO.
           MOVE W-BC3 TO HKB-BR3.
           PERFORM HKB-RTN THRU HKB-EX.
           IF  W-INV NOT = 0
               MOVE 0 TO W-INV W-SKP
               GO TO ACP-082
           END-IF
           CALL "SD_Output" USING "A-BC3" A-BC3 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BCN3" D-BCN3 "p" RETURNING RESU.
           IF  W-SKP = 6
               MOVE 0 TO W-SKP
           END-IF
           IF  W-SKP NOT = 0
               GO TO ACP-088
           END-IF.
       ACP-086.
           MOVE W-BMC TO W-BBMC.
           CALL "SD_Accept" USING BY REFERENCE A-BMC "A-BMC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-BBMC TO W-BMC
               CALL "SD_Output" USING "A-BMC" A-BMC "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-082
           END-IF
           IF  ESTAT = HTB AND SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND ADV
               GO TO ACP-086
           END-IF.
       ACP-088.
           IF  W-BMC NOT = 22 AND 23 AND 24 AND 26 AND 29
               GO TO ACP-086
           END-IF
           IF  W-BC3 = 10
               IF  W-BMC = 23 OR 24
                   GO TO ACP-086
               END-IF
           END-IF
           IF  W-BC3 = 20
               IF  W-BMC NOT = 23
                   GO TO ACP-086
               END-IF
           END-IF
           IF  W-BC3 = 30
               IF  W-BMC NOT = 24
                   GO TO ACP-086
               END-IF
           END-IF
           IF  W-BMC = 22
               MOVE 1 TO W-BMNO
               MOVE "çëÅ@ì‡" TO W-BMCN
           END-IF
           IF  W-BMC = 23
               MOVE 4 TO W-BMNO
               MOVE "ÉèÅ[ÉN" TO W-BMCN
           END-IF
           IF  W-BMC = 24
               MOVE 5 TO W-BMNO
               MOVE "ã≥Å@àÁ" TO W-BMCN
           END-IF
           IF  W-BMC = 26
               MOVE 2 TO W-BMNO
               MOVE "è„Å@äC" TO W-BMCN
           END-IF
           IF  W-BMC = 29
               MOVE 3 TO W-BMNO
               MOVE "édÅ@ì¸" TO W-BMCN
           END-IF
      *
           CALL "SD_Output" USING "D-BM" D-BM "p" RETURNING RESU.
           IF  W-SKP NOT = 0
               GO TO ACP-120
           END-IF.
       ACP-100.
           MOVE W-BCD1 TO W-BBCD1.
           CALL "SD_Accept" USING BY REFERENCE A-BCD1 "A-BCD1" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "A-BCD1" A-BCD1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-BBCD1 TO W-BCD1
               CALL "SD_Output" USING "A-BCD1" A-BCD1 "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-086
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF7 AND PF6 AND ADV
               GO TO ACP-100
           END-IF.
       ACP-120.
           MOVE 0 TO W-INV.
           MOVE SPACE TO HKB-KEY.
           MOVE "11" TO HKB-NO.
           MOVE W-BC1 TO HKB-BR1.
           PERFORM HKB-RTN THRU HKB-EX.
           IF  W-INV NOT = 0
               MOVE 0 TO W-INV W-SKP
               GO TO ACP-100
           END-IF
           CALL "SD_Output" USING "A-BCD1" A-BCD1 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BCN1" D-BCN1 "p" RETURNING RESU.
           MOVE SPACE TO W-BCN21.
           PERFORM BC21-RTN THRU BC21-EX.
           IF  W-BC3 = 10
               IF  W-BC1 = 11 OR 12 OR 13 OR 41 OR 50 OR 51 OR 55 OR 56
                             OR 58 OR 71 OR 72
                   GO TO ACP-100
               END-IF
           END-IF
           IF  W-BC3 = 20
               IF  W-BC1 = 22 OR 23 OR 26 OR 27 OR 71 OR 72
                   GO TO ACP-100
               END-IF
           END-IF
           IF  W-BC3 = 30
               IF  W-BC1 NOT = 71 AND 72 AND 74 AND 76 AND 91
                   GO TO ACP-100
               END-IF
           END-IF
           IF  W-BC1 NOT = 26 AND 32 AND 51 AND 48 AND 58
               IF  W-BC21 NOT = 0
                   GO TO ACP-100
               END-IF
           END-IF
           IF  W-BC1 = 26
               IF  W-BC21 < 1 OR > 3
                   GO TO ACP-100
               END-IF
           END-IF
           IF  W-BC1 = 32 OR 51
               IF  W-BC21 < 1 OR > 2
                   GO TO ACP-100
               END-IF
           END-IF
           IF  W-BC1 = 48 OR 58
               IF  W-BC21 < 1 OR > 3
                   GO TO ACP-100
               END-IF
           END-IF
           CALL "SD_Output" USING "D-BCN21" D-BCN21 "p" RETURNING RESU.
           IF  W-SKP NOT = 0
               GO TO ACP-160
           END-IF.
       ACP-140.
           MOVE W-BC22 TO W-BBC22.
           CALL "SD_Accept" USING BY REFERENCE A-BC22 "A-BC22" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "A-BC22" A-BC22 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-BBC22 TO W-BC22
               CALL "SD_Output" USING "A-BC22" A-BC22 "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-100
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND PF7 AND ADV
               GO TO ACP-140
           END-IF.
       ACP-160.
           MOVE SPACE TO HKB-KEY.
           MOVE 13 TO HKB-NO.
           MOVE W-BC22 TO HKB-BR22.
           PERFORM HKB-RTN THRU HKB-EX.
           IF  W-INV NOT = 0
               MOVE 0 TO W-INV W-SKP
               GO TO ACP-140
           END-IF
           CALL "SD_Output" USING "A-BC22" A-BC22 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BCN22" D-BCN22 "p" RETURNING RESU.
           IF  W-SKP NOT = 0
               GO TO ACP-214
           END-IF.
       ACP-180.
           MOVE W-BC4 TO W-BBC4.
           CALL "SD_Accept" USING BY REFERENCE A-BC4 "A-BC4" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "A-BC4" A-BC4 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-BBC4 TO W-BC4
               CALL "SD_Output" USING "A-BC4" A-BC4 "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-140
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND PF7 AND ADV
               GO TO ACP-180
           END-IF
           IF  W-BC4 > 2
               GO TO ACP-180
           END-IF.
       ACP-214.
           MOVE SPACE TO W-BCN4.
           IF  W-BC3 = 20
               IF  W-BC4 = 0
                   MOVE "ÉèÅ[ÉN" TO W-BCN4
               ELSE
                   IF  W-BC4 = 1
                       MOVE "ê~Å@ñ[" TO W-BCN4
                   ELSE
                       IF  W-BC4 = 2
                           MOVE "âÓÅ@åÏ" TO W-BCN4
                       END-IF
                   END-IF
               END-IF
           END-IF
           CALL "SD_Output" USING "D-BCN4" D-BCN4 "p" RETURNING RESU.
           IF  W-SKP = 6
               MOVE 0 TO W-SKP
           END-IF
      *
           IF  W-HCD1 = 9999
               MOVE ZERO TO W-FT
               CALL "SD_Output" USING "D-FT" D-FT "p" RETURNING RESU
               GO TO ACP-400
           END-IF
           IF  W-SKP NOT = 0
               GO TO ACP-375
           END-IF.
       ACP-370.
           MOVE W-YG TO W-BYG.
           CALL "SD_Accept" USING BY REFERENCE A-YG "A-YG" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "D-YG" D-YG "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF5
               GO TO ACP-100
           END-IF
           IF  ESTAT = ADV
               MOVE W-BYG TO W-YG
               CALL "SD_Output" USING "D-YG" D-YG "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               IF  W-BC3 = 20
                   GO TO ACP-180
               ELSE
                   GO TO ACP-140
               END-IF
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND ADV
               GO TO ACP-370
           END-IF.
       ACP-375.
           CALL "SD_Output" USING "D-YG" D-YG "p" RETURNING RESU.
           IF  W-SKP NOT = 0
               GO TO ACP-400
           END-IF.
       ACP-380.
           MOVE W-FT TO W-BFT.
           CALL "SD_Accept" USING BY REFERENCE A-FT "A-FT" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "D-FT" D-FT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF5
               GO TO ACP-100
           END-IF
           IF  ESTAT = ADV
               MOVE W-BFT TO W-FT
               CALL "SD_Output" USING "D-FT" D-FT "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-370
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND ADV
               GO TO ACP-380
           END-IF.
       ACP-400.
           CALL "SD_Output" USING "D-FT" D-FT "p" RETURNING RESU.
           IF  W-ACT = 2
               GO TO ACP-410
           END-IF
           IF  W-HCD1 NOT = 9999
               IF  W-FT = ZERO
                   MOVE SPACE TO W-MSG
                   MOVE "îpä¸ïià»äOÇÕÇmÇf" TO W-MSG
                   CALL "SD_Output" USING
                    "E-ME" E-ME "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
               END-IF
           END-IF
           IF  W-ACT NOT = 2
               GO TO ACP-410
           END-IF
           IF  W-FT = W-FTD
               GO TO ACP-410
           END-IF
           IF  ZERO = HUH-ZS AND HUH-ZK AND HUH-NS AND HUH-NK AND
                      HUH-SS AND HUH-UG AND HUH-YS AND HUH-YK
               GO TO ACP-410
           END-IF
           IF  ZERO = HUH-NS AND HUH-NK AND
                      HUH-SS AND HUH-UG
               MOVE SPACE TO W-MSG
               MOVE "***  ªﬁ≤∫ ±ÿ  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-410
           END-IF
           MOVE SPACE TO W-MSG.
           MOVE "***  ƒ≥πﬁ¬ √ﬁ∞¿ ±ÿ  ***" TO W-MSG.
           CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           GO TO ACP-380.
       ACP-410.
           IF  W-SKP NOT = 0
               GO TO ACP-440
           END-IF.
       ACP-420.
           MOVE W-SB TO W-BSB.
           CALL "SD_Accept" USING BY REFERENCE A-SB "A-SB" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "D-SB" D-SB "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-BSB TO W-SB
               CALL "SD_Output" USING "D-SB" D-SB "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-380
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND ADV
               GO TO ACP-420
           END-IF.
       ACP-440.
           CALL "SD_Output" USING "D-SB" D-SB "p" RETURNING RESU.
           IF  W-SKP NOT = 0
               GO TO ACP-480
           END-IF.
       ACP-460.
           MOVE W-ISU TO W-BISU.
           CALL "SD_Accept" USING BY REFERENCE A-ISU "A-ISU" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "D-ISU" D-ISU "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-BISU TO W-ISU
               CALL "SD_Output" USING "A-ISU" A-ISU "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-420
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND ADV
               GO TO ACP-460
           END-IF.
       ACP-480.
           CALL "SD_Output" USING "D-ISU" D-ISU "p" RETURNING RESU.
           IF  W-SKP NOT = 0
               GO TO ACP-495
           END-IF.
       ACP-490.
           MOVE W-YG2 TO W-BYG2.
           CALL "SD_Accept" USING BY REFERENCE A-YG2 "A-YG2" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "D-YG2" D-YG2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-BYG2 TO W-YG2
               CALL "SD_Output" USING "D-YG2" D-YG2 "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-460
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND ADV
               GO TO ACP-490
           END-IF.
       ACP-495.
           CALL "SD_Output" USING "D-YG2" D-YG2 "p" RETURNING RESU.
           IF  W-SKP NOT = 0
               GO TO ACP-520
           END-IF
           IF (W-BC22 NOT = 1) OR (W-HCD1 = 9999)
               MOVE 0 TO W-KRC
               MOVE SPACE TO W-KRCN
               GO TO ACP-520
           END-IF.
       ACP-500.
           MOVE W-KRC TO W-BKRC.
           CALL "SD_Accept" USING BY REFERENCE A-KRC "A-KRC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-BKRC TO W-KRC
               CALL "SD_Output" USING "A-KRC" A-KRC "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-490
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND ADV
               GO TO ACP-500
           END-IF.
       ACP-520.
           IF  W-KRC > 2
               GO TO ACP-500
           END-IF
           IF  W-KRC = 1
               MOVE "â¡" TO W-KRCN
           ELSE
               IF  W-KRC = 2
                   MOVE "îÒ" TO W-KRCN
               ELSE
                   MOVE SPACE TO W-KRCN
               END-IF
           END-IF
           CALL "SD_Output" USING "D-KR" D-KR "p" RETURNING RESU.
      *
           IF  W-SKP NOT = 0
               GO TO ACP-540
           END-IF.
       ACP-530.
           MOVE W-SCC TO W-BSCC.
           CALL "SD_Accept" USING BY REFERENCE A-SCC "A-SCC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-BSCC TO W-SCC
               CALL "SD_Output" USING "A-SCC" A-SCC "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-500
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND ADV
               GO TO ACP-530
           END-IF.
       ACP-540.
           IF  W-SCC > 2
               GO TO ACP-530
           END-IF
           IF  W-SCC = 0
               MOVE "Å@Å@" TO W-SCCN
           ELSE
               IF  W-SCC = 1
                   MOVE "îpî‘" TO W-SCCN
               ELSE
                   MOVE "ïsó«" TO W-SCCN
               END-IF
           END-IF
           CALL "SD_Output" USING "D-SC" D-SC "p" RETURNING RESU.
           IF  W-SKP NOT = 0
               GO TO ACP-555
           END-IF.
       ACP-550.
           MOVE W-SSC TO W-BSSC.
           CALL "SD_Accept" USING BY REFERENCE A-SSC "A-SSC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-BSSC TO W-SSC
               CALL "SD_Output" USING "A-SSC" A-SSC "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-530
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND ADV
               GO TO ACP-550
           END-IF.
       ACP-555.
           IF  W-SSC > 3
               GO TO ACP-550
           END-IF
           IF  W-SSC = 0
               MOVE "æ›¡" TO W-SSCN
           ELSE
               IF  W-SSC = 1
                   MOVE "SML" TO W-SSCN
               ELSE
                   IF  W-SSC = 2
                       MOVE "¿ﬁ≤" TO W-SSCN
                   ELSE
                       IF  W-SSC = 3
                           MOVE "≈º " TO W-SSCN
                       END-IF
                   END-IF
               END-IF
           END-IF
           CALL "SD_Output" USING "D-SSCM" D-SSCM "p" RETURNING RESU.
           IF (W-BC3 = 20) OR (W-HCD1 = 9999) OR (W-SSC > 0)
               MOVE 0 TO W-HKB
               GO TO ACP-575
           END-IF.
       ACP-570.
           MOVE W-HKB TO W-BHKB.
           CALL "SD_Accept" USING BY REFERENCE A-HKB "A-HKB" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-BHKB TO W-HKB
               CALL "SD_Output" USING "A-HKB" A-HKB "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-550
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND ADV
               GO TO ACP-570
           END-IF.
       ACP-575.
           IF  W-HKB > 1
               GO TO ACP-570
           END-IF
           IF  W-HKB = 0
               MOVE SPACE TO W-HKBN
           ELSE
               MOVE " ∞Ã" TO W-HKBN
           END-IF
           CALL "SD_Output" USING "D-HK" D-HK "p" RETURNING RESU.
       ACP-580.
           MOVE W-HPV TO W-BHPV.
           CALL "SD_Accept" USING BY REFERENCE A-HPV "A-HPV" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-BHPV TO W-HPV
               CALL "SD_Output" USING "A-HPV" A-HPV "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               IF (W-BC3 NOT = 20) AND (W-HCD1 NOT = 9999) AND
                                       (W-SSC      = 0)
                   GO TO ACP-570
               ELSE
                   GO TO ACP-550
               END-IF
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND ADV
               GO TO ACP-580
           END-IF.
       ACP-585.
           IF  W-HPV > 1
               GO TO ACP-570
           END-IF
           IF  W-SKP = 6
               MOVE 0 TO W-SKP
           END-IF.
       ACP-600.
           MOVE ZERO TO CNT.
           MOVE 16 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE 7 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
       ACP-620.
           ADD 1 TO CNT.
           IF  CNT = 41
               GO TO ACP-700
           END-IF
           IF  CNT = 11 OR 21 OR 31
               ADD 1 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE 7 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
               IF  W-SKP = 7
                   MOVE 0 TO W-SKP
               END-IF
           END-IF
           ADD 5 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  CNT = 30 OR 39
               MOVE 0 TO W-S(CNT)
               GO TO ACP-620
           END-IF
           IF  W-SKP NOT = 0
               GO TO ACP-660
           END-IF.
       ACP-640.
           MOVE W-S(CNT) TO W-BS(CNT).
           CALL "SD_Accept" USING BY REFERENCE A-S "A-S" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "D-S" D-S "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-BS(CNT) TO W-S(CNT)
               CALL "SD_Output" USING "D-S" D-S "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF5
               GO TO ACP-082
           END-IF
           IF  ESTAT = PF4
               MOVE 4 TO W-SKP
               GO TO ACP-680
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = PF7
               MOVE 7 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-680
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND PF7 AND ADV
               GO TO ACP-640
           END-IF.
       ACP-660.
           IF  CNT = 40
               IF  W-S(CNT) > 1
                   MOVE 0 TO W-SKP
                   GO TO ACP-640
               END-IF
           END-IF
           CALL "SD_Output" USING "D-S" D-S "p" RETURNING RESU.
           IF  CNT = 31
               IF  W-S(31) NOT = 0
                   IF  W-S(27) NOT = 0
                       GO TO ACP-640
                   END-IF
               END-IF
           END-IF
           IF  CNT = 32
               IF  W-S(32) NOT = 0
                   IF  W-S(28) NOT = 0
                       GO TO ACP-640
                   END-IF
               END-IF
           END-IF
           IF  CNT = 33
               IF  W-S(33) NOT = 0
                   IF  W-S(29) NOT = 0
                       GO TO ACP-640
                   END-IF
               END-IF
           END-IF
           IF  W-ACT = 2
               IF  W-S(CNT) = 0
                   IF  W-OS(CNT) NOT = ZERO
                       PERFORM CHK-RTN THRU CHK-RTN
                   END-IF
               END-IF
           END-IF
           IF  W-ACP = 9
               MOVE 0 TO W-SKP
               GO TO ACP-640
           END-IF
           GO TO ACP-620.
       ACP-680.
           SUBTRACT 1 FROM CNT.
           IF  CNT = ZERO
               GO TO ACP-580
           END-IF
           IF  CNT = 10 OR 20 OR 30 OR 40
               SUBTRACT 1 FROM W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE 62 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
               IF  W-SKP = 4
                   MOVE 0 TO W-SKP
               END-IF
           END-IF
           SUBTRACT 5 FROM W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  CNT = 30 OR 39
               GO TO ACP-680
           END-IF
           IF  W-SKP = 4
               GO TO ACP-680
           END-IF
           GO TO ACP-640.
       ACP-700.
           IF  W-SSD = ZERO
               MOVE SPACE TO W-MSG
               MOVE "***  ª≤Ωﬁ ∂ﬁ  ≤Ø√≈≤  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 0 TO W-SKP
               GO TO ACP-600
           END-IF
           IF  W-SSC NOT = 0
               IF (W-SK(2) NOT = ZERO) OR (W-SK(3) NOT = ZERO) OR
                  (W-S(31) NOT = 0) OR (W-S(32) NOT = 0) OR
                  (W-S(33) NOT = 0) OR (W-S(34) NOT = 0) OR
                  (W-S(35) NOT = 0) OR (W-S(36) NOT = 0) OR
                  (W-S(37) NOT = 0) OR (W-S(38) NOT = 0) OR
                  (W-S(39) NOT = 0)
                   MOVE SPACE TO W-MSG
                   MOVE "***  ª≤Ωﬁ ¥◊∞  ***" TO W-MSG
                   CALL "SD_Output" USING
                    "E-ME" E-ME "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   MOVE 0 TO W-SKP
                   GO TO ACP-600
               END-IF
           END-IF
           IF  W-SSC = 0
               IF (W-S(01) NOT = 0) OR (W-S(02) NOT = 0) OR
                  (W-S(03) NOT = 0) OR (W-S(04) NOT = 0) OR
                  (W-S(05) NOT = 0) OR (W-S(06) NOT = 0) OR
                  (W-S(07) NOT = 0)
                   MOVE SPACE TO W-MSG
                   MOVE "***  ª≤Ωﬁ ¥◊∞  ***" TO W-MSG
                   CALL "SD_Output" USING
                    "E-ME" E-ME "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   MOVE 0 TO W-SKP
                   GO TO ACP-600
               END-IF
           END-IF
           IF  W-SKP = 6
               MOVE 0 TO W-SKP
           END-IF
           IF  W-SKP NOT = 0
               GO TO ACP-740
           END-IF.
       ACP-720.
           MOVE W-SNG TO W-BSNG.
           CALL "SD_Accept" USING BY REFERENCE A-SNG "A-SNG" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  W-SNG = ZERO
               CALL "SD_Output" USING "D-SNGC" D-SNGC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-SNG" A-SNG "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF5
               GO TO ACP-600
           END-IF
           IF  ESTAT = ADV
               MOVE W-BSNG TO W-SNG
               MOVE 1 TO W-SKP
               IF  W-SNG = ZERO
                   CALL "SD_Output" USING
                    "D-SNGC" D-SNGC "p" RETURNING RESU
               ELSE
                   CALL "SD_Output" USING
                    "A-SNG" A-SNG "p" RETURNING RESU
               END-IF
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               MOVE 41 TO CNT
               MOVE 20 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               GO TO ACP-680
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND ADV
               GO TO ACP-720
           END-IF.
       ACP-740.
           IF  W-SNG = ZERO
               CALL "SD_Output" USING "D-SNGC" D-SNGC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-SNG" A-SNG "p" RETURNING RESU
           END-IF
           IF  W-SKP NOT = 0
               GO TO ACP-780
           END-IF.
       ACP-760.
           MOVE W-ENG TO W-BENG.
           CALL "SD_Accept" USING BY REFERENCE A-ENG "A-ENG" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  W-ENG = ZERO
               CALL "SD_Output" USING "D-ENGC" D-ENGC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-ENG" A-ENG "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF5
               GO TO ACP-600
           END-IF
           IF  ESTAT = ADV
               MOVE W-BENG TO W-ENG
               MOVE 1 TO W-SKP
               IF  W-ENG = ZERO
                   CALL "SD_Output" USING
                    "D-ENGC" D-ENGC "p" RETURNING RESU
               ELSE
                   CALL "SD_Output" USING
                    "A-ENG" A-ENG "p" RETURNING RESU
               END-IF
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-720
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND ADV
               GO TO ACP-760
           END-IF.
       ACP-780.
           IF  W-ENG = ZERO
               CALL "SD_Output" USING "D-ENGC" D-ENGC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-ENG" A-ENG "p" RETURNING RESU
           END-IF
           IF  W-SKP NOT = 0
               GO TO ACP-820
           END-IF
      *
           IF  W-ACT NOT = 1
               GO TO ACP-800
           END-IF
           IF  W-TCD NOT = ZERO
               GO TO ACP-800
           END-IF
           IF  W-BCD1 = 322
               MOVE 5359 TO W-TCD
           END-IF
           IF  W-BC1 = 22 OR 23
               MOVE 0459 TO W-TCD
           END-IF
           IF  W-BC1 = 29
               MOVE 5349 TO W-TCD
           END-IF
           IF  W-BC3 = 30
               MOVE 5350 TO W-TCD
           END-IF
           IF  W-BC3 = 20
               MOVE 5358 TO W-TCD
           END-IF
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO ACP-800
           END-IF
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TCD" D-TCD "p" RETURNING RESU.
       ACP-800.
           MOVE W-TCD TO W-BTCD.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-BTCD TO W-TCD
               CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU
               MOVE 1 TO W-SKP
           END-IF
           IF  ESTAT = PF6
               MOVE 6 TO W-SKP
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-760
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND ADV
               GO TO ACP-800
           END-IF.
       ACP-820.
           IF  W-TCD = ZERO
               MOVE SPACE TO T-NAME
               CALL "SD_Output" USING "D-TCD" D-TCD "p" RETURNING RESU
               GO TO ACP-840
           END-IF
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO ACP-800
           END-IF
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TCD" D-TCD "p" RETURNING RESU.
           IF  T-BC NOT = 0
               MOVE SPACE TO W-MSG
               MOVE "***  Ãﬁ”› ∂ﬁ ¡∂ﬁ≥  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 0 TO W-SKP
               GO TO ACP-800
           END-IF.
       ACP-840.
           IF  W-SKP NOT = 0
               GO TO ACP-880
           END-IF.
       ACP-860.
           MOVE W-CS TO W-BCS.
           CALL "SD_Accept" USING BY REFERENCE A-CS "A-CS" "N" "20"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "A-CS" A-CS "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF5
               GO TO ACP-600
           END-IF
           IF  ESTAT = ADV
               MOVE W-BCS TO W-CS
               MOVE 1 TO W-SKP
               CALL "SD_Output" USING "A-CS" A-CS "p" RETURNING RESU
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-800
           END-IF
           IF  ESTAT = HTB OR SKP
               MOVE 0 TO W-SKP
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND PF6 AND ADV
               GO TO ACP-860
           END-IF.
       ACP-880.
           CALL "SD_Output" USING "A-CS" A-CS "p" RETURNING RESU.
           MOVE 0 TO W-SKP.
       ACP-980.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3 OR 5 OR 8
                   MOVE 9 TO W-ACP
                   GO TO ACP-EX
               ELSE
                   GO TO ACP-860
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-980
           END-IF
           IF  W-DMM = 9
               MOVE 9 TO W-ACP
               GO TO ACP-EX
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-980
           END-IF.
       ACP-EX.
           EXIT.
      ********************************
      *****Å@Å@Å@âÊñ Å@ï\é¶      *****
      ********************************
       CRT-RTN.
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "A-MHCD" A-MHCD "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NAME" A-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SMS" A-SMS "p" RETURNING RESU.
           CALL "SD_Output" USING "D-FT" D-FT "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SB" D-SB "p" RETURNING RESU.
           CALL "SD_Output" USING "D-YG" D-YG "p" RETURNING RESU.
           CALL "SD_Output" USING "D-YG2" D-YG2 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-ISU" D-ISU "p" RETURNING RESU.
           CALL "SD_Output" USING "A-CS" A-CS "p" RETURNING RESU.
           CALL "SD_Output" USING "A-HPV" A-HPV "p" RETURNING RESU.
           IF  W-KRC = 1
               MOVE "â¡" TO W-KRCN
           ELSE
               IF  W-KRC = 2
                   MOVE "îÒ" TO W-KRCN
               ELSE
                   MOVE SPACE TO W-KRCN
               END-IF
           END-IF
           CALL "SD_Output" USING "D-KR" D-KR "p" RETURNING RESU.
           IF  W-SCC = 0
               MOVE "Å@Å@" TO W-SCCN
           ELSE
               IF  W-SCC = 1
                   MOVE "îpî‘" TO W-SCCN
               ELSE
                   MOVE "ïsó«" TO W-SCCN
               END-IF
           END-IF
           CALL "SD_Output" USING "D-SC" D-SC "p" RETURNING RESU.
           MOVE SPACE TO W-SSCN.
           IF  W-SSC = 0
               MOVE "æ›¡" TO W-SSCN
           ELSE
               IF  W-SSC = 1
                   MOVE "SML" TO W-SSCN
               ELSE
                   IF  W-SSC = 2
                       MOVE "¿ﬁ≤" TO W-SSCN
                   ELSE
                       IF  W-SSC = 3
                           MOVE "≈º " TO W-SSCN
                       END-IF
                   END-IF
               END-IF
           END-IF
           CALL "SD_Output" USING "D-SSCM" D-SSCM "p" RETURNING RESU.
           IF  W-HKB = 0
               MOVE SPACE TO W-HKBN
           ELSE
               MOVE " ∞Ã" TO W-HKBN
           END-IF
           CALL "SD_Output" USING "D-HK" D-HK "p" RETURNING RESU.
           MOVE SPACE TO W-BMCN.
           IF  W-BMC = 22
               MOVE "çëÅ@ì‡" TO W-BMCN
           END-IF
           IF  W-BMC = 23
               MOVE "ÉèÅ[ÉN" TO W-BMCN
           END-IF
           IF  W-BMC = 24
               MOVE "ã≥Å@àÁ" TO W-BMCN
           END-IF
           IF  W-BMC = 26
               MOVE "è„Å@äC" TO W-BMCN
           END-IF
           IF  W-BMC = 29
               MOVE "édÅ@ì¸" TO W-BMCN
           END-IF
           CALL "SD_Output" USING "D-BM" D-BM "p" RETURNING RESU.
           IF  W-SNG NOT = ZERO
               CALL "SD_Output" USING "A-SNG" A-SNG "p" RETURNING RESU
           END-IF
           IF  W-ENG NOT = ZERO
               CALL "SD_Output" USING "A-ENG" A-ENG "p" RETURNING RESU
           END-IF
      *
           IF  W-MHCD = W-KEY
               CALL "SD_Output" USING "D-CHNA" D-CHNA "p" RETURNING RESU
               GO TO CRT-020
           END-IF
           MOVE W-MHCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
           END-IF
           CALL "SD_Output" USING "D-MHNA" D-MHNA "p" RETURNING RESU.
           MOVE W-KEY TO HI-KEY.
      *           READ HI-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  DATA ≈º ?  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-ACP
               GO TO CRT-EX
           END-IF.
       CRT-020.
           MOVE SPACE TO HKB-KEY.
           MOVE 11 TO HKB-NO.
           MOVE W-BC1 TO HKB-BR1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BRN1
           END-IF
           CALL "SD_Output" USING "A-BCD1" A-BCD1 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BCN1" D-BCN1 "p" RETURNING RESU.
           MOVE SPACE TO W-BCN21.
           PERFORM BC21-RTN THRU BC21-EX.
           MOVE SPACE TO HKB-KEY.
           MOVE 13 TO HKB-NO.
           MOVE W-BC22 TO HKB-BR22.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BRN22
           END-IF
           CALL "SD_Output" USING "A-BC22" A-BC22 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BCN22" D-BCN22 "p" RETURNING RESU.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE 14 TO HKB-NO.
           MOVE W-BC3 TO HKB-BR3.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BRN3
           END-IF
           CALL "SD_Output" USING "A-BC3" A-BC3 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BCN3" D-BCN3 "p" RETURNING RESU.
      *
           MOVE SPACE TO W-BCN4.
           IF  W-BC3 = 20
               IF  W-BC4 = 0
                   MOVE "ÉèÅ[ÉN" TO W-BCN4
               ELSE
                   IF  W-BC4 = 1
                       MOVE "ê~Å@ñ[" TO W-BCN4
                   ELSE
                       IF  W-BC4 = 2
                           MOVE "âÓÅ@åÏ" TO W-BCN4
                       END-IF
                   END-IF
               END-IF
           END-IF
           CALL "SD_Output" USING "A-BC4" A-BC4 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BCN4" D-BCN4 "p" RETURNING RESU.
       CRT-030.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
           END-IF
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TCD" D-TCD "p" RETURNING RESU.
      *
           MOVE ZERO TO CNT.
           MOVE 16 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE 7 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
       CRT-040.
           ADD 1 TO CNT.
           IF  CNT = 41
               GO TO CRT-EX
           END-IF
           IF  CNT = 11 OR 21 OR 31
               ADD 1 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE 7 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
           END-IF
           ADD 5 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  CNT NOT = 30 AND 39
               CALL "SD_Output" USING "D-S" D-S "p" RETURNING RESU
           END-IF
           GO TO CRT-040.
       CRT-EX.
           EXIT.
      ************************************
      *****Å@Å@ÇaÇbÇQÇPÅ@ÇcÇhÇrÇo    *****
      ************************************
       BC21-RTN.
           IF  W-BC1 NOT = 48 AND 58
               GO TO BC21-020
           END-IF
           IF  W-BC21 = 1
               MOVE "çïÅ@Å@Å@" TO W-BCN21
           END-IF
           IF  W-BC21 = 2
               MOVE "ÉJÉâÅ[Å@" TO W-BCN21
           END-IF
           IF  W-BC21 = 3
               MOVE "ÉCÉìÉWÉF" TO W-BCN21
           END-IF.
       BC21-020.
           IF  W-BC1 NOT = 26
               GO TO BC21-030
           END-IF
           IF  W-BC21 = 1
               MOVE "íËÅ@î‘Å@" TO W-BCN21
           END-IF
           IF  W-BC21 = 2
               MOVE "îÒíËî‘Å@" TO W-BCN21
           END-IF
           IF  W-BC21 = 3
               MOVE "édì¸ïiÅ@" TO W-BCN21
           END-IF.
       BC21-030.
           IF  W-BC1 NOT = 32 AND 51
               GO TO BC21-090
           END-IF
           IF  W-BC21 = 1
               MOVE "àÍÅ@Å@î " TO W-BCN21
           END-IF
           IF  W-BC21 = 2
               MOVE "ÉîÉBÉîÉF" TO W-BCN21
           END-IF.
       BC21-090.
           CALL "SD_Output" USING "D-BCN21" D-BCN21 "p" RETURNING RESU.
       BC21-EX.
           EXIT.
      ************************************
      *****Å@Å@ÇgÇjÇaÇlÅ@ÇqÇdÇ`Çc    *****
      ************************************
       HKB-RTN.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  Ãﬁ›Ÿ≤ ≈º  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 9 TO W-INV
           END-IF.
       HKB-EX.
           EXIT.
      ***************************************
      *****    ÇgÇtÇgÇlÅ@ÇvÇqÇhÇsÇd     *****
      ***************************************
       HU1-RTN.
           INITIALIZE HUH-R.
           MOVE W-KEY TO HUH-KEY.
           MOVE W-NG TO HUH-NG.
           MOVE W-BCD1 TO HUH-BCD1.
           MOVE W-BC22 TO HUH-BC22.
           MOVE W-BC3 TO HUH-BC3.
           MOVE W-BMC TO HUH-BMC.
           MOVE W-BMNO TO HUH-BMNO.
           MOVE W-BC4 TO HUH-BC4.
      *           WRITE HUH-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HUH-M_PNAME1 HUH-M_LNAME HUH-R RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  WRITE ¥◊∞  ***" TO W-MSG
               MOVE "HUHM" TO W-FILE
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HUH" E-HUH "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO HU1-020
           END-IF
           GO TO HU1-EX.
       HU1-020.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO HU1-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-M_IDLST HUH-M_PNAME1.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" HUH-M_PNAME1 "SHARED" BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
           GO TO HU1-RTN.
       HU1-EX.
           EXIT.
      ***************************************
      *****    ÇgÇgÇsÇeÅ@ÇvÇqÇhÇsÇd     *****
      ***************************************
       HH1-RTN.
           INITIALIZE HHT-R.
           MOVE W-MHCD TO HHT-MHCD.
           MOVE W-KEY TO HHT-HCD.
           MOVE CNT TO HHT-SIZ.
           MOVE W-BCD1 TO HHT-BCD1.
           MOVE W-BC22 TO HHT-BC22.
           MOVE W-BC3 TO HHT-BC3.
           MOVE W-BMNO TO HHT-BMNO.
           MOVE W-BC4 TO HHT-BC4.
      *           WRITE HHT-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HHTF_PNAME1 HHTF_LNAME HHT-R RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  WRITE ¥◊∞  ***" TO W-MSG
               MOVE "HHTF" TO W-FILE
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HHT" E-HHT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO HH1-020
           END-IF
           GO TO HH1-EX.
       HH1-020.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO HH1-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" HHTF_PNAME1 "SHARED" BY REFERENCE HHTF_IDLST "2"
            "HHT-KEY" BY REFERENCE HHT-KEY "HHT-KEY2" BY REFERENCE
            HHT-KEY2.
           GO TO HH1-RTN.
       HH1-EX.
           EXIT.
      ****************************************
      *****Å@Å@èCê≥éûÉTÉCÉYÅ@É`ÉFÉbÉN    *****
      ****************************************
       CHK-RTN.
           MOVE 0 TO W-ACP.
           MOVE SPACE TO HHT-KEY.
           MOVE W-HCD TO HHT-HCD.
      *///////////////add koyama 20170303
           CALL "DB_Select" USING
            HHTF_PNAME1 "WHERE"
            W-HCD "=" "HHT-HCD"
            RETURNING RET.
      *           START HHTF KEY NOT < HHT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HHTF_PNAME1 "HHT-KEY" " NOT < " HHT-KEY RETURNING RET.
           IF  RET = 1
               GO TO CHK-EX
           END-IF.
       CHK-020.
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO CHK-EX
           END-IF
      *           IF  W-HCD NOT = HHT-HCD
      *               GO TO CHK-EX
      *           END-IF
           IF  CNT > 00 AND < 11
               IF  HHT-SIZ NOT = 1
                   GO TO CHK-020
               ELSE
                   MOVE CNT TO CNTD
               END-IF
           END-IF
           IF  CNT > 10 AND < 21
               IF  HHT-SIZ NOT = 2
                   GO TO CHK-020
               ELSE
                   COMPUTE CNTD = CNT - 10
               END-IF
           END-IF
           IF  CNT > 20 AND < 31
               IF  HHT-SIZ NOT = 3
                   GO TO CHK-020
               ELSE
                   COMPUTE CNTD = CNT - 20
               END-IF
           END-IF
           IF  CNT > 30 AND < 41
               IF  HHT-SIZ NOT = 4
                   GO TO CHK-020
               ELSE
                   COMPUTE CNTD = CNT - 30
               END-IF
           END-IF
           IF ZERO NOT = HHT-ZSU(CNTD) OR HHT-NSU(CNTD) OR HHT-USU(CNTD)
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  DATA ∂ﬁ ZERO √ﬁ≈≤  ***" TO W-MSG
               MOVE "HHTF" TO W-FILE
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HHT" E-HHT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-ACP
           END-IF.
       CHK-EX.
           EXIT.
      ****************************
      *****    ÇvÇqÇhÇsÇdÅ@Å@*****
      ****************************
       WRI-RTN.
           INITIALIZE HI-R.
           MOVE W-R TO HI-R.
      *           WRITE HI-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HI-M_PNAME1 HI-M_LNAME HI-R RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  WRITE ¥◊∞  ***" TO W-MSG
               MOVE "HIM" TO W-FILE
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HI" E-HI "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRI-020
           END-IF
           GO TO WRI-040.
       WRI-020.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO WRI-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           GO TO WRI-RTN.
       WRI-040.
           PERFORM HU1-RTN THRU HU1-EX.
           IF  W-END = 9
               GO TO WRI-EX
           END-IF
      *
           MOVE ZERO TO CNT.
       WRI-060.
           ADD 1 TO CNT.
           IF  CNT = 5
               GO TO WRI-EX
           END-IF
           IF  W-SK(CNT) = ZERO
               GO TO WRI-060
           END-IF
           PERFORM HH1-RTN THRU HH1-EX.
           IF  W-END = 9
               GO TO WRI-EX
           END-IF
           GO TO WRI-060.
       WRI-EX.
           EXIT.
      ********************************
      *****    ÇqÇdÇvÇqÇhÇsÇdÅ@Å@*****
      ********************************
       REW-RTN.
           INITIALIZE HI-R.
           MOVE W-R TO HI-R.
      *           REWRITE HI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HI-M_PNAME1 HI-M_LNAME HI-R RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  REWRITE ¥◊∞  ***" TO W-MSG
               MOVE "HIM" TO W-FILE
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HI" E-HI "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO REW-EX
           END-IF
      *
           MOVE ZERO TO W-DCDD.
           IF (W-OSKD(1) NOT = ZERO) AND (W-SKD(1) = ZERO)
               MOVE 1 TO W-DC(1)
           END-IF
           IF (W-OSKD(2) NOT = ZERO) AND (W-SKD(2) = ZERO)
               MOVE 1 TO W-DC(2)
           END-IF
           IF (W-OSKD(3) NOT = ZERO) AND (W-SKD(3) = ZERO)
               MOVE 1 TO W-DC(3)
           END-IF
           IF (W-OSKD(4) NOT = ZERO) AND (W-SKD(4) = ZERO)
               MOVE 1 TO W-DC(4)
           END-IF
           IF (W-OSKD(1) = ZERO) AND (W-SKD(1) NOT = ZERO)
               MOVE 2 TO W-DC(1)
           END-IF
           IF (W-OSKD(2) = ZERO) AND (W-SKD(2) NOT = ZERO)
               MOVE 2 TO W-DC(2)
           END-IF
           IF (W-OSKD(3) = ZERO) AND (W-SKD(3) NOT = ZERO)
               MOVE 2 TO W-DC(3)
           END-IF
           IF (W-OSKD(4) = ZERO) AND (W-SKD(4) NOT = ZERO)
               MOVE 2 TO W-DC(4)
           END-IF
           IF  W-DCDD = ZERO
               GO TO REW-100
           END-IF
      *
           MOVE 0 TO CHK.
       REW-020.
           ADD 1 TO CHK.
           IF  CHK = 5
               GO TO REW-100
           END-IF
           IF  W-DC(CHK) = 0
               GO TO REW-020
           END-IF
           IF  W-DC(CHK) = 2
               GO TO REW-060
           END-IF
           MOVE W-KEY TO HHT-HCD.
           MOVE CHK TO HHT-SIZ.
      *           READ HHTF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HHTF_PNAME1 BY REFERENCE HHT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO REW-020
           END-IF
           IF  ZERO = HHT-ZSU(01) AND HHT-ZSU(02) AND HHT-ZSU(03) AND
                      HHT-ZSU(04) AND HHT-ZSU(05) AND HHT-ZSU(06) AND
                      HHT-ZSU(07) AND HHT-ZSU(08) AND HHT-ZSU(09) AND
                      HHT-ZSU(10) AND
                      HHT-NSU(01) AND HHT-NSU(02) AND HHT-NSU(03) AND
                      HHT-NSU(04) AND HHT-NSU(05) AND HHT-NSU(06) AND
                      HHT-NSU(07) AND HHT-NSU(08) AND HHT-NSU(09) AND
                      HHT-NSU(10) AND
                      HHT-USU(01) AND HHT-USU(02) AND HHT-USU(03) AND
                      HHT-USU(04) AND HHT-USU(05) AND HHT-USU(06) AND
                      HHT-USU(07) AND HHT-USU(08) AND HHT-USU(09) AND
                      HHT-USU(10)
               GO TO REW-040
           END-IF
           MOVE SPACE TO W-MSG W-FILE.
           MOVE "***  DATA ∂ﬁ ZERO √ﬁ≈≤  ***" TO W-MSG.
           MOVE "HHTF" TO W-FILE.
           CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU.
           CALL "SD_Output" USING "E-FILE" E-FILE "p" RETURNING RESU.
           CALL "SD_Output" USING "E-HHT" E-HHT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           MOVE 9 TO W-END.
           GO TO REW-EX.
       REW-040.
      *           DELETE HHTF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HHTF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  DELETE ¥◊∞  ***" TO W-MSG
               MOVE "HHTF" TO W-FILE
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HHT" E-HHT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO REW-EX
           END-IF
           GO TO REW-020.
       REW-060.
           MOVE CHK TO CNT.
           PERFORM HH1-RTN THRU HH1-EX.
           IF  W-END = 9
               GO TO REW-EX
           END-IF
           GO TO REW-020.
      *-----------------------------------------------------------------
       REW-100.
           MOVE 0 TO W-RC.
           IF (W-BCD1 = W-OBCD1) AND (W-BC22 = W-OBC22) AND
              (W-BC3 = W-OBC3) AND (W-BMC = W-OBMC) AND (W-BC4 = W-OBC4)
               GO TO REW-120
           END-IF
      *
           MOVE W-KEY TO HUH-KEY.
      *           READ HUH-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO REW-120
           END-IF
      *
           MOVE W-BCD1 TO HUH-BCD1.
           MOVE W-BC22 TO HUH-BC22.
           MOVE W-BC3 TO HUH-BC3.
           MOVE W-BMC TO HUH-BMC.
           MOVE W-BMNO TO HUH-BMNO.
           MOVE W-BC4 TO HUH-BC4.
      *           REWRITE HUH-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HUH-M_PNAME1 HUH-M_LNAME HUH-R RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  REWRITE ¥◊∞  ***" TO W-MSG
               MOVE "HUHM" TO W-FILE
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HUH" E-HUH "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO REW-EX
           END-IF
           IF  COMPLETION_CODE = 000
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 050
           END-IF
           IF  COMPLETION_CODE = 100
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 150
           END-IF
           IF  W-RC = 0
               MOVE 1 TO W-RC
           END-IF.
       REW-120.
           IF (W-BCD1 = W-OBCD1) AND (W-BC22 = W-OBC22)
               AND (W-BC3 = W-OBC3) AND (W-MHCD = W-OMHCD)
               AND (W-BMC = W-OBMC) AND (W-BC4 = W-OBC4)
               GO TO REW-EX
           END-IF
           MOVE SPACE TO HHT-KEY.
           MOVE W-KEY TO HHT-HCD.
      *           START HHTF KEY NOT < HHT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HHTF_PNAME1 "HHT-KEY" " NOT < " HHT-KEY RETURNING RET.
           IF  RET = 1
               GO TO REW-200
           END-IF.
       REW-140.
      *           READ HHTF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO REW-200
           END-IF
           IF  W-KEY NOT = HHT-HCD
               GO TO REW-200
           END-IF
      *
           MOVE W-MHCD TO HHT-MHCD.
           MOVE W-BCD1 TO HHT-BCD1.
           MOVE W-BC22 TO HHT-BC22.
           MOVE W-BC3 TO HHT-BC3.
           MOVE W-BMNO TO HHT-BMNO.
           MOVE W-BC4 TO HHT-BC4.
      *           REWRITE HHT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HHTF_PNAME1 HHTF_LNAME HHT-R RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  REWRITE ¥◊∞  ***" TO W-MSG
               MOVE "HHTF" TO W-FILE
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HHT" E-HHT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO REW-EX
           END-IF
           GO TO REW-140.
       REW-200.
           IF  W-RC = 0
               GO TO REW-EX
           END-IF
           CALL "DB_F_Open" USING
            "I-O" M-DATE_PNAME1 "SHARED" BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
           MOVE "01" TO DATE-KEY.
      *           READ M-DATE INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" M-DATE_PNAME1 BY REFERENCE DATE-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE M-DATE_IDLST M-DATE_PNAME1
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  DATA ≈º  ***" TO W-MSG
               MOVE "DATEM" TO W-FILE
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO REW-EX
           END-IF
           IF  W-BCD1 NOT = W-OBCD1
               MOVE 1 TO DATE-HBC1
           END-IF
           IF (W-BC22 NOT = W-OBC22) OR (W-BC4 NOT = W-OBC4)
               MOVE 1 TO DATE-HBC2
           END-IF
           IF  W-BC3 NOT = W-OBC3
               MOVE 1 TO DATE-HBC3
           END-IF
      *           REWRITE DATE-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            M-DATE_PNAME1 M-DATE_LNAME DATE-R RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  REWRITE ¥◊∞  ***" TO W-MSG
               MOVE "DATEM" TO W-FILE
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
       REW-EX.
           EXIT.
      ******************************
      *****    ÇcÇdÇkÇdÇsÇdÅ@Å@*****
      ******************************
       DEL-RTN.
           IF  W-ACT NOT = 8
               GO TO DEL-040
           END-IF
           MOVE ZERO TO W-DDD W-DCNT.
           MOVE SPACE TO HI-KEY.
           MOVE W-HCD1 TO HI-HCD1.
      *///////////////add koyama 20170303
           CALL "DB_Select" USING
            HI-M_PNAME1 "WHERE"
            W-HCD1 "=" "HI-HCD1"
            RETURNING RET.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               GO TO DEL-EX
           END-IF.
       DEL-020.
      *           READ HI-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DEL-EX
           END-IF
      *           IF  W-HCD1 NOT = HI-HCD1
      *               GO TO DEL-EX
      *           END-IF
           MOVE HI-KEY TO W-HCD.
           IF  HI-MHCD = HI-HCD
               ADD 1 TO W-DCNT
               IF  W-DCNT < 21
                   MOVE HI-MHCD TO W-DHCD(W-DCNT)
               ELSE
                   MOVE SPACE TO W-MSG
                   MOVE "***  ≤€ ∂ﬁ µµΩ∑ﬁŸ (ø≥º›F)  ***" TO W-MSG
                   CALL "SD_Output" USING
                    "E-ME" E-ME "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-HI" E-HI "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               END-IF
           END-IF.
       DEL-040.
      *           DELETE HI-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HI-M_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  DELETE ¥◊∞  ***" TO W-MSG
               MOVE "HIM" TO W-FILE
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HI" E-HI "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO DEL-EX
           END-IF
           IF  COMPLETION_CODE = 000
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 100
           END-IF
           IF  COMPLETION_CODE = 050
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 150
           END-IF.
      *
       DEL-060.
           MOVE W-HCD TO HUH-KEY.
      *           READ HUH-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DEL-080
           END-IF
      *
      *           DELETE HUH-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HUH-M_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  DELETE ¥◊∞  ***" TO W-MSG
               MOVE "HUHM" TO W-FILE
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HUH" E-HUH "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO DEL-EX
           END-IF.
       DEL-080.
           MOVE SPACE TO HHT-KEY.
           MOVE W-HCD TO HHT-HCD.
      *///////////////add koyama 20170303
           CALL "DB_Select" USING
            HHTF_PNAME1 "WHERE"
            W-HCD "=" "HHT-HCD"
            RETURNING RET.
      *           START HHTF KEY NOT < HHT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HHTF_PNAME1 "HHT-KEY" " NOT < " HHT-KEY RETURNING RET.
           IF  RET = 1
               GO TO DEL-120
           END-IF.
       DEL-100.
      *           READ HHTF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DEL-120
           END-IF
      *           IF  W-HCD NOT = HHT-HCD
      *               GO TO DEL-120
      *           END-IF
      *
      *           DELETE HHTF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HHTF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  DELETE ¥◊∞  ***" TO W-MSG
               MOVE "HHTF" TO W-FILE
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HHT" E-HHT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO DEL-EX
           END-IF
           GO TO DEL-100.
       DEL-120.
           CALL "DB_F_Open" USING
            "I-O" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
           MOVE SPACE TO THT-KEY2.
           MOVE W-HCD TO THT-HCD2.
      *///////////////add koyama 20170303
           CALL "DB_Select" USING
            THTM_PNAME1 "WHERE"
            "THT-KEY2" "=" W-HCD
            RETURNING RET.
      *           START THTM KEY NOT < THT-KEY2 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            THTM_PNAME1 "THT-KEY2" " NOT < " THT-KEY2 RETURNING RET.
           IF  RET = 1
               GO TO DEL-160
           END-IF.
       DEL-140.
      *           READ THTM NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" THTM_PNAME1 BY REFERENCE THT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DEL-160
           END-IF
           IF  THT-KEY2 NOT = W-HCD
               GO TO DEL-160
           END-IF
      *           DELETE THTM INVALID KEY
      *///////////////
           CALL "DB_Delete" USING THTM_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG W-FILE
               MOVE "***  DELETE ¥◊∞  ***" TO W-MSG
               MOVE "THTM" TO W-FILE
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-THT" E-THT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO DEL-160
           END-IF
           GO TO DEL-140.
       DEL-160.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           IF  W-ACT = 8
               GO TO DEL-020
           END-IF.
       DEL-EX.
           EXIT.
      ****************************
      *****Å@Å@çÏÅ@Å@Å@ï\    *****
      ****************************
       PRI-RTN.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-PM" C-PM "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
       PRI-020.
           CALL "SD_Accept" USING BY REFERENCE A-PMC "A-PMC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PRI-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO PRI-020
           END-IF
           IF  W-PMC NOT = 0 AND 5
               GO TO PRI-020
           END-IF.
       PRI-060.
           CALL "SD_Accept" USING BY REFERENCE A-SKEY "A-SKEY" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PRI-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO PRI-060
           END-IF.
       PRI-080.
           CALL "SD_Accept" USING BY REFERENCE A-EKEY "A-EKEY" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PRI-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO PRI-080
           END-IF
           IF  W-SKEY > W-EKEY
               GO TO PRI-060
           END-IF.
       PRI-100.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PRI-080
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO PRI-100
           END-IF
           IF  W-DMM = 9
               GO TO PRI-EX
           END-IF
           IF  W-DMM NOT = 1
               GO TO PRI-100
           END-IF
      *
           MOVE W-SKEY TO HI-HCD.
      *///////////////add koyama 20170303
           CALL "DB_Select" USING
            HI-M_PNAME1 "WHERE"
            "HI-KEY" ">" HI-KEY "AND" "HI-HCD" "<" W-EKEY
            RETURNING RET.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  DATA ≈º  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO PRI-060
           END-IF
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  DATA ≈º  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO PRI-060
           END-IF
      *           IF  HI-HCD > W-EKEY
      *               MOVE SPACE TO W-MSG
      *               MOVE "***  DATA ≈º  ***" TO W-MSG
      *               CALL "SD_Output" USING
      *                "E-ME" E-ME "p" RETURNING RESU
      *               CALL "SD_Output" USING
      *                "E-ME98" E-ME98 "p" RETURNING RESU
      *               GO TO PRI-060
      *           END-IF
           PERFORM LST-RTN THRU LST-EX.
       PRI-EX.
           EXIT.
      ****************************
      *****Å@Å@ñæç◊Å@àÛéö    *****
      ****************************
       LST-RTN.
           IF  W-POC = 0
               MOVE 9 TO W-POC
               CALL "PR_Open" RETURNING RESP
               PERFORM MID-020 THRU MID-EX
           END-IF
           MOVE SPACE TO W-P1.
           MOVE HI-HCD TO P-HCD.
           MOVE HI-NAME TO P-NAME.
           MOVE HI-SMS TO P-SMS.
           MOVE HI-BC1 TO P-BC1.
           MOVE HI-BC21 TO P-BC21.
           MOVE HI-BC22 TO P-BC22.
           MOVE HI-BC3 TO P-BC3.
           MOVE HI-BMC TO P-BMC.
           IF  HI-BC3 = 20
               MOVE HI-BC4 TO P-BC4
           END-IF
           MOVE HI-SB TO P-SB.
           MOVE HI-FT TO P-FT.
           MOVE HI-YG TO P-YG.
           MOVE HI-ISU TO P-ISU.
           MOVE HI-KRC TO P-KRC.
           MOVE HI-SCC TO P-SCC.
           MOVE HI-SSC TO P-SSC.
           MOVE HI-HKB TO P-HKB.
           MOVE HI-HPV TO P-HPV.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-CS.
           IF  HI-MHCD NOT = HI-HCD
               MOVE HI-MHCD TO P-MHCD
               MOVE ")" TO P-C
           END-IF
           MOVE HI-CS TO P-CS.
           IF  HI-TCD NOT = ZERO
               MOVE HI-TCD TO P-TCD
           END-IF
           IF  HI-SNG NOT = ZERO
               MOVE HI-SNG TO P-SNG
           END-IF
           IF  HI-ENG NOT = ZERO
               MOVE HI-ENG TO P-ENG
           END-IF
           MOVE ZERO TO CHK.
       LST-020.
           ADD 1 TO CHK.
           IF  CHK = 5
               GO TO LST-060
           END-IF
           IF  HI-SS(CHK) = ZERO
               GO TO LST-020
           END-IF
           MOVE CHK TO P-SIZ.
           MOVE ZERO TO CNT.
       LST-040.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               MOVE HI-S(CHK,CNT) TO P-SC(CNT)
               GO TO LST-040
           END-IF
      *
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-CS.
           GO TO LST-020.
       LST-060.
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO LST-EX
           END-IF
           IF  HI-HCD > W-EKEY
               GO TO LST-EX
           END-IF
           GO TO LST-RTN.
       LST-EX.
           EXIT.
      ******************************
      *****Å@Å@å©èoÇµÅ@àÛéö    *****
      ******************************
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-020.
           IF  W-PMC = 5
               MOVE 0 TO W-PMC
               MOVE SPACE TO SP-R
               CALL "PR_LineFeed" USING "24" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE HEAD01 TO SP-R
               CALL "PR_LineFeed" USING "3" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD02 TO SP-R
               CALL "PR_LineFeed" USING "10" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
       MID-EX.
           EXIT.

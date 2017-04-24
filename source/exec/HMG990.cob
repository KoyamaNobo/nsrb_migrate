       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG990.
      *********************************************************
      *    PROGRAM         : 　出荷集計ワーク作成（年間用）他 *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  96/12/17                        *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=月次更新  ,  1=月次作表(HGGET)*
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-FILE             PIC  X(013).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-DATE         PIC  9(008).
           02  W-NGP   REDEFINES W-DATE.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-NGL   REDEFINES W-NG.
               04  F          PIC  9(002).
               04  W-NGS      PIC  9(004).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-DATE.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-RD.
             03  W-SU         PIC S9(005).
             03  W-UKIN       PIC S9(008).
             03  W-GKIN       PIC S9(008).
           02  W-KIN          PIC S9(009).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LITTM.
      *FD  SNTR-F
       01  SNTR-F_HMG990.
           02  SNTR-F_PNAME1  PIC  X(005) VALUE "SNTRF".
           02  F              PIC  X(001).
           02  SNTR-F_LNAME   PIC  X(013) VALUE "SNTR-F_HMG990".
           02  F              PIC  X(001).
           02  SNTR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  SNTR-F_SORT    PIC  X(100) VALUE SPACE.
           02  SNTR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  SNTR-F_RES     USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DNO       PIC  9(006).
           02  SNTR-GNO       PIC  9(001).
           02  SNTR-DATE.
             03  SNTR-NG      PIC  9(006).
             03  SNTR-PEY     PIC  9(002).
           02  SNTR-TCD       PIC  9(004).
           02  SNTR-HCD       PIC  9(006).
           02  F              PIC  X(031).
           02  SNTR-SU        PIC S9(005).
           02  SNTR-BT        PIC S9(005).
           02  SNTR-UKIN      PIC S9(008).
           02  F              PIC  X(001).
           02  SNTR-DC        PIC  9(001).
           02  SNTR-FT        PIC  9(005).
           02  F              PIC  X(003).
           02  SNTR-BC.
             03  SNTR-BC1     PIC  9(002).
             03  SNTR-BC2     PIC  9(002).
             03  SNTR-BC3     PIC  9(002).
           02  SNTR-BCD   REDEFINES SNTR-BC.
             03  SNTR-BCD1    PIC  9(003).
             03  F            PIC  9(003).
           02  F              PIC  X(001).
           02  SNTR-TNC       PIC  9(002).
           02  SNTR-FKC       PIC  9(002).
           02  F              PIC  X(027).
           02  SNTR-BMC       PIC  9(002).
           02  SNTR-BMNO      PIC  9(001).
           02  F              PIC  X(002).
           02  SNTR-SNC       PIC  9(001).
       77  F                  PIC  X(001).
      *FD  SSR-F
       01  SSR-F_HMG990.
           02  SSR-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SSR-F_LNAME    PIC  X(012) VALUE "SSR-F_HMG990".
           02  F              PIC  X(001).
           02  SSR-F_KEY1     PIC  X(100) VALUE SPACE.
           02  SSR-F_SORT     PIC  X(100) VALUE SPACE.
           02  SSR-F_IDLST    PIC  X(100) VALUE SPACE.
           02  SSR-F_RES      USAGE  POINTER.
       01  SSR-R.
           02  SSR-TCD        PIC  9(004).
           02  SSR-HCD        PIC  9(006).
           02  SSR-SU         PIC S9(007).
           02  SSR-UKIN       PIC S9(010).
           02  SSR-GKIN       PIC S9(010).
           02  SSR-TKC        PIC  9(002).
           02  SSR-TNC        PIC  9(002).
           02  SSR-BC.
             03  SSR-BC1      PIC  9(002).
             03  SSR-BC2      PIC  9(002).
             03  SSR-BC3      PIC  9(002).
           02  SSR-BMC        PIC  9(002).
           02  SSR-BMNO       PIC  9(001).
           02  SSR-FKC        PIC  9(002).
           02  SSR-NG         PIC  9(006).
           02  F              PIC  X(006).
       77  F                  PIC  X(001).
      *FD  TZNT-M
       01  TZNT-M_HMG990.
           02  TZNT-M_PNAME1  PIC  X(005) VALUE "TZNTM".
           02  F              PIC  X(001).
           02  TZNT-M_LNAME   PIC  X(013) VALUE "TZNT-M_HMG990".
           02  F              PIC  X(001).
           02  TZNT-M_KEY1    PIC  X(100) VALUE SPACE.
           02  TZNT-M_KEY2    PIC  X(100) VALUE SPACE.
           02  TZNT-M_SORT    PIC  X(100) VALUE SPACE.
           02  TZNT-M_IDLST   PIC  X(100) VALUE SPACE.
           02  TZNT-M_RES     USAGE  POINTER.
       01  TZNT-R.
           02  TZNT-KEY.
             03  TZNT-TCD     PIC  9(004).
             03  TZNT-IKC     PIC  9(001).
           02  TZNT-OUD       PIC  X(108).
           02  TZNT-NUD.
             03  TZNT-NU05    PIC S9(009).
             03  TZNT-NU06    PIC S9(009).
             03  TZNT-NU07    PIC S9(009).
             03  TZNT-NU08    PIC S9(009).
             03  TZNT-NU09    PIC S9(009).
             03  TZNT-NU10    PIC S9(009).
             03  TZNT-NU11    PIC S9(009).
             03  TZNT-NU12    PIC S9(009).
             03  TZNT-NU01    PIC S9(009).
             03  TZNT-NU02    PIC S9(009).
             03  TZNT-NU03    PIC S9(009).
             03  TZNT-NU04    PIC S9(009).
           02  TZNT-TUD.
             03  TZNT-OTU     PIC S9(010).
             03  TZNT-NTU     PIC S9(010).
           02  TZNT-OAD       PIC  X(108).
           02  TZNT-NAD.
             03  TZNT-NA05    PIC S9(009).
             03  TZNT-NA06    PIC S9(009).
             03  TZNT-NA07    PIC S9(009).
             03  TZNT-NA08    PIC S9(009).
             03  TZNT-NA09    PIC S9(009).
             03  TZNT-NA10    PIC S9(009).
             03  TZNT-NA11    PIC S9(009).
             03  TZNT-NA12    PIC S9(009).
             03  TZNT-NA01    PIC S9(009).
             03  TZNT-NA02    PIC S9(009).
             03  TZNT-NA03    PIC S9(009).
             03  TZNT-NA04    PIC S9(009).
           02  TZNT-TAD.
             03  TZNT-OTA     PIC S9(010).
             03  TZNT-NTA     PIC S9(010).
           02  TZNT-NG.
             03  TZNT-NEN     PIC  9(004).
             03  TZNT-GET     PIC  9(002).
           02  TZNT-TNC       PIC  9(002).
           02  TZNT-BC        PIC  9(001).
           02  F              PIC  X(026).
       77  F                  PIC  X(001).
      *FD  TZNTP-M
       01  TZNTP-M_HMG990.
           02  TZNTP-M_PNAME1 PIC  X(006) VALUE "TZNTPM".
           02  F              PIC  X(001).
           02  TZNTP-M_LNAME  PIC  X(014) VALUE "TZNTP-M_HMG990".
           02  F              PIC  X(001).
           02  TZNTP-M_KEY1   PIC  X(100) VALUE SPACE.
           02  TZNTP-M_KEY2   PIC  X(100) VALUE SPACE.
           02  TZNTP-M_SORT   PIC  X(100) VALUE SPACE.
           02  TZNTP-M_IDLST  PIC  X(100) VALUE SPACE.
           02  TZNTP-M_RES    USAGE  POINTER.
       01  TZNTP-R.
           02  TZNTP-KEY.
             03  TZNTP-TCD    PIC  9(004).
             03  TZNTP-IKC    PIC  9(001).
           02  TZNTP-OUD      PIC  X(108).
           02  TZNTP-NUD.
             03  TZNTP-NU05   PIC S9(009).
             03  TZNTP-NU06   PIC S9(009).
             03  TZNTP-NU07   PIC S9(009).
             03  TZNTP-NU08   PIC S9(009).
             03  TZNTP-NU09   PIC S9(009).
             03  TZNTP-NU10   PIC S9(009).
             03  TZNTP-NU11   PIC S9(009).
             03  TZNTP-NU12   PIC S9(009).
             03  TZNTP-NU01   PIC S9(009).
             03  TZNTP-NU02   PIC S9(009).
             03  TZNTP-NU03   PIC S9(009).
             03  TZNTP-NU04   PIC S9(009).
           02  TZNTP-TUD.
             03  TZNTP-OTU    PIC S9(010).
             03  TZNTP-NTU    PIC S9(010).
           02  TZNTP-OAD      PIC  X(108).
           02  TZNTP-NAD.
             03  TZNTP-NA05   PIC S9(009).
             03  TZNTP-NA06   PIC S9(009).
             03  TZNTP-NA07   PIC S9(009).
             03  TZNTP-NA08   PIC S9(009).
             03  TZNTP-NA09   PIC S9(009).
             03  TZNTP-NA10   PIC S9(009).
             03  TZNTP-NA11   PIC S9(009).
             03  TZNTP-NA12   PIC S9(009).
             03  TZNTP-NA01   PIC S9(009).
             03  TZNTP-NA02   PIC S9(009).
             03  TZNTP-NA03   PIC S9(009).
             03  TZNTP-NA04   PIC S9(009).
           02  TZNTP-TAD.
             03  TZNTP-OTA    PIC S9(010).
             03  TZNTP-NTA    PIC S9(010).
           02  TZNTP-NG.
             03  TZNTP-NEN    PIC  9(004).
             03  TZNTP-GET    PIC  9(002).
           02  TZNTP-TNC      PIC  9(002).
           02  TZNTP-BC       PIC  9(001).
           02  F              PIC  X(026).
       77  F                  PIC  X(001).
      *FD  TTMYR
       01  TTMYR_HMG990.
           02  TTMYR_PNAME1   PIC  X(005) VALUE "TTMYR".
           02  F              PIC  X(001).
           02  TTMYR_LNAME    PIC  X(012) VALUE "TTMYR_HMG990".
           02  F              PIC  X(001).
           02  TTMYR_KEY1     PIC  X(100) VALUE SPACE.
           02  TTMYR_SORT     PIC  X(100) VALUE SPACE.
           02  TTMYR_IDLST    PIC  X(100) VALUE SPACE.
           02  TTMYR_RES      USAGE  POINTER.
       01  TTMYR-R.
           02  TTMYR-KEY.
             03  TTMYR-TCD    PIC  9(004).
           02  TTMYR-TD.
             03  TTMYR-TZZ    PIC S9(009).
             03  TTMYR-TZZZ   PIC S9(007).
             03  TTMYR-TUZ    PIC S9(009).
             03  TTMYR-TUZZ   PIC S9(007).
             03  TTMYR-TUA    PIC S9(009).
             03  TTMYR-TUAZ   PIC S9(007).
             03  TTMYR-TNB    PIC S9(008).
             03  TTMYR-TNBZ   PIC S9(006).
             03  TTMYR-TNK    PIC S9(009).
             03  TTMYR-TNKZ   PIC S9(007).
             03  TTMYR-TUG    PIC S9(009).
           02  TTMYR-TKC      PIC  9(002).
           02  TTMYR-TNC      PIC  9(002).
           02  TTMYR-FKC      PIC  9(002).
           02  TTMYR-BC       PIC  9(001).
           02  TTMYR-DCN      PIC  9(003).
           02  F              PIC  X(021).
           02  TTMYR-DNG      PIC  9(006).
       77  F                  PIC  X(001).
      *FD  ZD-F
       01  ZD-F_HMG990.
           02  ZD-F_PNAME1    PIC  X(003) VALUE "ZDF".
           02  F              PIC  X(001).
           02  ZD-F_LNAME     PIC  X(011) VALUE "ZD-F_HMG990".
           02  F              PIC  X(001).
           02  ZD-F_KEY1      PIC  X(100) VALUE SPACE.
           02  ZD-F_SORT      PIC  X(100) VALUE SPACE.
           02  ZD-F_IDLST     PIC  X(100) VALUE SPACE.
           02  ZD-F_RES       USAGE  POINTER.
       01  ZD-R.
           02  ZD-NO          PIC  9(002).
           02  ZD-KEY         PIC  9(004).
           02  ZD-KIN         PIC S9(010).
           02  ZD-NG          PIC  9(004).
           02  F              PIC  X(001).
       77  F                  PIC  X(001).
      *FD  TZNTMYR
       01  TZNTMYR_HMG990.
           02  TZNTMYR_PNAME1 PIC  X(007) VALUE "TZNTMYR".
           02  F              PIC  X(001).
           02  TZNTMYR_LNAME  PIC  X(014) VALUE "TZNTMYR_HMG990".
           02  F              PIC  X(001).
           02  TZNTMYR_KEY1   PIC  X(100) VALUE SPACE.
           02  TZNTMYR_SORT   PIC  X(100) VALUE SPACE.
           02  TZNTMYR_IDLST  PIC  X(100) VALUE SPACE.
           02  TZNTMYR_RES    USAGE  POINTER.
       01  TZNTY-R.
           02  F              PIC  X(506).
           02  TZNTY-NG       PIC  9(006).
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
       01  C-MID0.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　出荷集計ワーク作成　（年間用）　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　得意先年間ファイル　累積・更新　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-MID1.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　得意先年間ファイル　更新　（作表）　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME2   PIC  X(025) VALUE
                  "***  TZNTM WRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(027) VALUE
                  "***  TZNTM REWRITE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(026) VALUE
                  "***  TZNTM DELETE ｴﾗｰ  ***".
             03  E-ME5   PIC  X(026) VALUE
                  "***  TZNTPM WRITE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(028) VALUE
                  "***  TZNTPM REWRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(027) VALUE
                  "***  TZNTPM DELETE ｴﾗｰ  ***".
             03  E-ME10  PIC  X(025) VALUE
                  "***  TTMYR WRITE ｴﾗｰ  ***".
             03  E-ME11  PIC  X(023) VALUE
                  "***  ZDF WRITE ｴﾗｰ  ***".
             03  E-ME12  PIC  X(027) VALUE
                  "***  TZNTMYR WRITE ｴﾗｰ  ***".
             03  E-KEY   PIC  X(005).
             03  E-KEYP  PIC  X(005).
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
            "C-MID0" " " "0" "0" "400" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID0" "N" "3" "10" "50" " " "C-MID0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID0" "N" "4" "10" "50" "01C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID0" "N" "5" "10" "50" "02C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID0" "N" "6" "10" "50" "03C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID0" "N" "7" "10" "50" "04C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID0" "N" "8" "10" "50" "05C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID0" "N" "9" "10" "50" "06C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID0" "N" "10" "10" "50" "07C-MID0" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "350" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "N" "3" "10" "50" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID1" "N" "4" "10" "50" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID1" "N" "5" "10" "50" "02C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID1" "N" "6" "10" "50" "03C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID1" "N" "7" "10" "50" "04C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID1" "N" "8" "10" "50" "05C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID1" "N" "9" "10" "50" "06C-MID1" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "244" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "244" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "25" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "27" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "26" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "26" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "28" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "27" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "25" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "23" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "27" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "50" "5" "E-ME12" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE TZNT-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEYP" "X" "24" "50" "5" "E-KEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEYP" BY REFERENCE TZNTP-KEY "5" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING
                "C-MID1" C-MID1 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "C-MID0" C-MID0 "p" RETURNING RESU
           END-IF
      *
           MOVE ZERO TO W-DATE.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
      *
           IF  JS-SIGN = 1
               CALL "DB_F_Open" USING
                "I-O" TZNTP-M_PNAME1 " " BY REFERENCE TZNTP-M_IDLST "1"
                "TZNTP-KEY" BY REFERENCE TZNTP-KEY
           ELSE
               CALL "DB_F_Open" USING
                "I-O" TZNT-M_PNAME1 "SHARED" BY REFERENCE TZNT-M_IDLST
                "1" "TZNT-KEY" BY REFERENCE TZNT-KEY
           END-IF
           IF  W-GET = 5
               PERFORM MZR-RTN THRU MZR-EX
           END-IF
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" SNTR-F_PNAME1 " " BY REFERENCE SNTR-F_IDLST "0".
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" TT-M_PNAME1 
            " " BY REFERENCE TT-M_IDLST "1"
            "TT-KEY" BY REFERENCE TT-KEY.
           IF  JS-SIGN = 1
               GO TO M-10
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SSR-F_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" SSR-F_PNAME1 " " BY REFERENCE SSR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" TTMYR_PNAME1 " " BY REFERENCE TTMYR_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" ZD-F_PNAME1 " " BY REFERENCE ZD-F_IDLST "0".
       M-10.
      *           READ SNTR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF
           IF  SNTR-GNO = 9
               GO TO M-10
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-10
           END-IF
           PERFORM DST-RTN THRU DST-EX.
           IF  ZERO = W-SU AND W-UKIN AND W-GKIN
               GO TO M-10
           END-IF
           IF  JS-SIGN = 1
               GO TO M-15
           END-IF
           MOVE ZERO      TO SSR-R.
           MOVE SNTR-TCD  TO SSR-TCD.
           MOVE SNTR-HCD  TO SSR-HCD.
           MOVE W-SU   TO SSR-SU.
           MOVE W-UKIN TO SSR-UKIN.
           MOVE W-GKIN TO SSR-GKIN.
           MOVE SNTR-TNC  TO SSR-TNC.
           MOVE SNTR-BC   TO SSR-BC.
           MOVE SNTR-BMC  TO SSR-BMC.
           MOVE SNTR-BMNO TO SSR-BMNO.
           MOVE SNTR-FKC  TO SSR-FKC.
           MOVE SNTR-NG TO SSR-NG.
      *           WRITE SSR-R.
      *//////////////
           CALL "DB_Insert" USING
            SSR-F_PNAME1 SSR-F_LNAME SSR-R RETURNING RET.
       M-15.
           COMPUTE W-KIN = W-UKIN - W-GKIN.
           IF  ZERO = W-UKIN AND W-KIN
               GO TO M-10
           END-IF
           PERFORM MDR-RTN THRU MDR-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF
           GO TO M-10.
       M-20.
      *           READ TT-M AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" TT-M_PNAME1 BY REFERENCE TT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JS-SIGN = 1
               GO TO M-25
           END-IF
      *
           PERFORM TYR-RTN THRU TYR-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF
      *
           PERFORM ZDW-RTN THRU ZDW-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF.
       M-25.
           IF  TT-BC = 0
               GO TO M-20
           END-IF
           COMPUTE W-UKIN = TT-TUA - TT-TNB.
           COMPUTE W-KIN = W-UKIN - TT-TUG.
           IF  ZERO = W-UKIN AND W-KIN
               GO TO M-20
           END-IF
           PERFORM KDR-RTN THRU KDR-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF
           GO TO M-20.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1.
           IF  JS-SIGN = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TZNTP-M_IDLST TZNTP-M_PNAME1
               GO TO M-95
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE SSR-F_IDLST SSR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TZNT-M_IDLST TZNT-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TTMYR_IDLST TTMYR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE ZD-F_IDLST ZD-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       DST-RTN.
           MOVE ZERO TO W-RD.
           IF (SNTR-SNC = 0) AND (SNTR-DC NOT = 2)
               COMPUTE W-GKIN = SNTR-SU * SNTR-FT
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-SU = SNTR-SU * -1
               COMPUTE W-GKIN = W-GKIN * -1
               COMPUTE W-UKIN = SNTR-UKIN * -1
           ELSE
               MOVE SNTR-SU TO W-SU
               MOVE SNTR-UKIN TO W-UKIN
           END-IF
           IF (SNTR-HCD > 999899) OR (SNTR-SNC = 1) OR (SNTR-DC = 2)
               MOVE ZERO TO W-SU
           END-IF.
       DST-EX.
           EXIT.
       MZR-RTN.
           IF  JS-SIGN = 1
               GO TO MZR-060
           END-IF
           CALL "DB_F_Open" USING
            "EXTEND" TZNTMYR_PNAME1 " " BY REFERENCE TZNTMYR_IDLST "0".
       MZR-010.
      *           READ TZNT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TZNT-M_PNAME1 BY REFERENCE TZNT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TZNTMYR_IDLST TZNTMYR_PNAME1
               GO TO MZR-EX
           END-IF.
       MZR-020.
           MOVE ZERO TO TZNTY-R.
           MOVE TZNT-R TO TZNTY-R.
           MOVE W-NG TO TZNTY-NG.
      *           WRITE TZNTY-R.
      *//////////////
           CALL "DB_Insert" USING
            TZNTMYR_PNAME1 TZNTMYR_LNAME TZNTY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO MZR-030
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME12" E-ME12 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "DB_F_Close" USING
                BY REFERENCE TZNTMYR_IDLST TZNTMYR_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO MZR-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TZNTMYR_IDLST TZNTMYR_PNAME1.
           MOVE "TZNTMYR      " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" TZNTMYR_PNAME1 " " BY REFERENCE TZNTMYR_IDLST "0".
           GO TO MZR-020.
       MZR-030.
           IF  ZERO = TZNT-NU05 AND TZNT-NU06 AND TZNT-NU07 AND
                      TZNT-NU08 AND TZNT-NU09 AND TZNT-NU10 AND
                      TZNT-NU11 AND TZNT-NU12 AND TZNT-NU01 AND
                      TZNT-NU02 AND TZNT-NU03 AND TZNT-NU04 AND
                      TZNT-NA05 AND TZNT-NA06 AND TZNT-NA07 AND
                      TZNT-NA08 AND TZNT-NA09 AND TZNT-NA10 AND
                      TZNT-NA11 AND TZNT-NA12 AND TZNT-NA01 AND
                      TZNT-NA02 AND TZNT-NA03 AND TZNT-NA04
               GO TO MZR-040
           END-IF
           MOVE TZNT-NUD TO TZNT-OUD.
           MOVE TZNT-NTU TO TZNT-OTU.
           MOVE TZNT-NAD TO TZNT-OAD.
           MOVE TZNT-NTA TO TZNT-OTA.
           MOVE ZERO TO TZNT-NUD TZNT-NTU TZNT-NAD TZNT-NTA.
           MOVE W-NG TO TZNT-NG.
      *
      *           REWRITE TZNT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TZNT-M_PNAME1 TZNT-M_LNAME TZNT-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TZNT-M_IDLST TZNT-M_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-CL" E-CL "p" RETURNING RESU
               GO TO MZR-EX
           END-IF
           GO TO MZR-010.
       MZR-040.
      *           DELETE TZNT-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TZNT-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TZNT-M_IDLST TZNT-M_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-CL" E-CL "p" RETURNING RESU
               GO TO MZR-EX
           END-IF
           GO TO MZR-010.
       MZR-060.
      *           READ TZNTP-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TZNTP-M_PNAME1 BY REFERENCE TZNTP-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO MZR-EX
           END-IF
           IF  ZERO = TZNTP-NU05 AND TZNTP-NU06 AND TZNTP-NU07 AND
                      TZNTP-NU08 AND TZNTP-NU09 AND TZNTP-NU10 AND
                      TZNTP-NU11 AND TZNTP-NU12 AND TZNTP-NU01 AND
                      TZNTP-NU02 AND TZNTP-NU03 AND TZNTP-NU04 AND
                      TZNTP-NA05 AND TZNTP-NA06 AND TZNTP-NA07 AND
                      TZNTP-NA08 AND TZNTP-NA09 AND TZNTP-NA10 AND
                      TZNTP-NA11 AND TZNTP-NA12 AND TZNTP-NA01 AND
                      TZNTP-NA02 AND TZNTP-NA03 AND TZNTP-NA04
               GO TO MZR-070
           END-IF
           MOVE TZNTP-NUD TO TZNTP-OUD.
           MOVE TZNTP-NTU TO TZNTP-OTU.
           MOVE TZNTP-NAD TO TZNTP-OAD.
           MOVE TZNTP-NTA TO TZNTP-OTA.
           MOVE ZERO TO TZNTP-NUD TZNTP-NTU TZNTP-NAD TZNTP-NTA.
           MOVE W-NG TO TZNTP-NG.
      *
      *           REWRITE TZNTP-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TZNTP-M_PNAME1 TZNTP-M_LNAME TZNTP-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TZNTP-M_IDLST TZNTP-M_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEYP" E-KEYP "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-CL" E-CL "p" RETURNING RESU
               GO TO MZR-EX
           END-IF
           GO TO MZR-060.
       MZR-070.
      *           DELETE TZNTP-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TZNTP-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TZNTP-M_IDLST TZNTP-M_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEYP" E-KEYP "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-CL" E-CL "p" RETURNING RESU
               GO TO MZR-EX
           END-IF
           GO TO MZR-060.
       MZR-EX.
           EXIT.
       MDR-RTN.
           IF  JS-SIGN = 1
               GO TO MDR-060
           END-IF
           MOVE ZERO TO TZNT-R.
           MOVE SNTR-TCD TO TZNT-TCD.
           IF (SNTR-BCD1 NOT = 322) OR (SNTR-BC3 NOT = 30)
               MOVE 1 TO TZNT-IKC
           END-IF
           IF  SNTR-BC3 = 30
               MOVE 2 TO TZNT-IKC
           END-IF
           IF  SNTR-BCD1 = 322
               MOVE 3 TO TZNT-IKC
           END-IF
      *           READ TZNT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TZNT-M_PNAME1 BY REFERENCE TZNT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO MDR-010
           END-IF
           PERFORM SET-RTN THRU SET-EX.
           MOVE SNTR-TNC TO TZNT-TNC.
           MOVE 0 TO TZNT-BC.
      *           REWRITE TZNT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TZNT-M_PNAME1 TZNT-M_LNAME TZNT-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-CL" E-CL "p" RETURNING RESU
           END-IF
           GO TO MDR-EX.
       MDR-010.
           PERFORM SET-RTN THRU SET-EX.
           MOVE SNTR-TNC TO TZNT-TNC.
           MOVE 0 TO TZNT-BC.
      *           WRITE TZNT-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TZNT-M_PNAME1 TZNT-M_LNAME TZNT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO MDR-020
           END-IF
           GO TO MDR-EX.
       MDR-020.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO MDR-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TZNT-M_IDLST TZNT-M_PNAME1.
           MOVE "TZNTM        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TZNT-M_PNAME1 "SHARED" BY REFERENCE TZNT-M_IDLST "1"
            "TZNT-KEY" BY REFERENCE TZNT-KEY.
           MOVE ZERO TO TZNT-R.
           MOVE SNTR-TCD TO TZNT-TCD.
           IF (SNTR-BCD1 NOT = 322) OR (SNTR-BC3 NOT = 30)
               MOVE 1 TO TZNT-IKC
           END-IF
           IF  SNTR-BC3 = 30
               MOVE 2 TO TZNT-IKC
           END-IF
           IF  SNTR-BCD1 = 322
               MOVE 3 TO TZNT-IKC
           END-IF
           GO TO MDR-010.
       MDR-060.
           MOVE ZERO TO TZNTP-R.
           MOVE SNTR-TCD TO TZNTP-TCD.
           IF (SNTR-BCD1 NOT = 322) OR (SNTR-BC3 NOT = 30)
               MOVE 1 TO TZNTP-IKC
           END-IF
           IF  SNTR-BC3 = 30
               MOVE 2 TO TZNTP-IKC
           END-IF
           IF  SNTR-BCD1 = 322
               MOVE 3 TO TZNTP-IKC
           END-IF
      *           READ TZNTP-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TZNTP-M_PNAME1 BY REFERENCE TZNTP-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO MDR-070
           END-IF
           PERFORM SET-RTN THRU SET-EX.
           MOVE SNTR-TNC TO TZNTP-TNC.
           MOVE 0 TO TZNTP-BC.
      *           REWRITE TZNTP-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TZNTP-M_PNAME1 TZNTP-M_LNAME TZNTP-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-CL" E-CL "p" RETURNING RESU
           END-IF
           GO TO MDR-EX.
       MDR-070.
           PERFORM SET-RTN THRU SET-EX.
           MOVE SNTR-TNC TO TZNTP-TNC.
           MOVE 0 TO TZNTP-BC.
      *           WRITE TZNTP-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TZNTP-M_PNAME1 TZNTP-M_LNAME TZNTP-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO MDR-080
           END-IF
           GO TO MDR-EX.
       MDR-080.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO MDR-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TZNTP-M_IDLST TZNTP-M_PNAME1.
           MOVE "TZNTPM       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TZNTP-M_PNAME1 " " BY REFERENCE TZNTP-M_IDLST "1"
            "TZNTP-KEY" BY REFERENCE TZNTP-KEY.
           MOVE ZERO TO TZNTP-R.
           MOVE SNTR-TCD TO TZNTP-TCD.
           IF (SNTR-BCD1 NOT = 322) OR (SNTR-BC3 NOT = 30)
               MOVE 1 TO TZNTP-IKC
           END-IF
           IF  SNTR-BC3 = 30
               MOVE 2 TO TZNTP-IKC
           END-IF
           IF  SNTR-BCD1 = 322
               MOVE 3 TO TZNTP-IKC
           END-IF
           GO TO MDR-070.
       MDR-EX.
           EXIT.
       KDR-RTN.
           IF  JS-SIGN = 1
               GO TO KDR-060
           END-IF
           MOVE ZERO TO TZNT-R.
           MOVE TT-TCD TO TZNT-TCD.
      *           READ TZNT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TZNT-M_PNAME1 BY REFERENCE TZNT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO KDR-010
           END-IF
           PERFORM SET-RTN THRU SET-EX.
           MOVE TT-TNC TO TZNT-TNC.
           MOVE TT-BC TO TZNT-BC.
      *           REWRITE TZNT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TZNT-M_PNAME1 TZNT-M_LNAME TZNT-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-CL" E-CL "p" RETURNING RESU
           END-IF
           GO TO KDR-EX.
       KDR-010.
           PERFORM SET-RTN THRU SET-EX.
           MOVE TT-TNC TO TZNT-TNC.
           MOVE TT-BC TO TZNT-BC.
      *           WRITE TZNT-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TZNT-M_PNAME1 TZNT-M_LNAME TZNT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO KDR-020
           END-IF
           GO TO KDR-EX.
       KDR-020.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO KDR-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TZNT-M_IDLST TZNT-M_PNAME1.
           MOVE "TZNTM        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TZNT-M_PNAME1 "SHARED" BY REFERENCE TZNT-M_IDLST "1"
            "TZNT-KEY" BY REFERENCE TZNT-KEY.
           MOVE ZERO TO TZNT-R.
           MOVE TT-TCD TO TZNT-TCD.
           GO TO KDR-010.
       KDR-060.
           MOVE ZERO TO TZNTP-R.
           MOVE TT-TCD TO TZNTP-TCD.
      *           READ TZNTP-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TZNTP-M_PNAME1 BY REFERENCE TZNTP-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO KDR-070
           END-IF
           PERFORM SET-RTN THRU SET-EX.
           MOVE TT-TNC TO TZNTP-TNC.
           MOVE TT-BC TO TZNTP-BC.
      *           REWRITE TZNTP-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TZNTP-M_PNAME1 TZNTP-M_LNAME TZNTP-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-CL" E-CL "p" RETURNING RESU
           END-IF
           GO TO KDR-EX.
       KDR-070.
           PERFORM SET-RTN THRU SET-EX.
           MOVE TT-TNC TO TZNTP-TNC.
           MOVE TT-BC TO TZNTP-BC.
      *           WRITE TZNTP-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TZNTP-M_PNAME1 TZNTP-M_LNAME TZNTP-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO KDR-080
           END-IF
           GO TO KDR-EX.
       KDR-080.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO KDR-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TZNTP-M_IDLST TZNTP-M_PNAME1.
           MOVE "TZNTPM       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TZNTP-M_PNAME1 " " BY REFERENCE TZNTP-M_IDLST "1"
            "TZNTP-KEY" BY REFERENCE TZNTP-KEY.
           MOVE ZERO TO TZNTP-R.
           MOVE TT-TCD TO TZNTP-TCD.
           GO TO KDR-070.
       KDR-EX.
           EXIT.
       SET-RTN.
           IF  JS-SIGN = 1
               GO TO SET-060
           END-IF
           IF  W-GET = 5
               ADD W-UKIN TO TZNT-NU05 TZNT-NTU
               ADD W-KIN TO TZNT-NA05 TZNT-NTA
           END-IF
           IF  W-GET = 6
               ADD W-UKIN TO TZNT-NU06 TZNT-NTU
               ADD W-KIN TO TZNT-NA06 TZNT-NTA
           END-IF
           IF  W-GET = 7
               ADD W-UKIN TO TZNT-NU07 TZNT-NTU
               ADD W-KIN TO TZNT-NA07 TZNT-NTA
           END-IF
           IF  W-GET = 8
               ADD W-UKIN TO TZNT-NU08 TZNT-NTU
               ADD W-KIN TO TZNT-NA08 TZNT-NTA
           END-IF
           IF  W-GET = 9
               ADD W-UKIN TO TZNT-NU09 TZNT-NTU
               ADD W-KIN TO TZNT-NA09 TZNT-NTA
           END-IF
           IF  W-GET = 10
               ADD W-UKIN TO TZNT-NU10 TZNT-NTU
               ADD W-KIN TO TZNT-NA10 TZNT-NTA
           END-IF
           IF  W-GET = 11
               ADD W-UKIN TO TZNT-NU11 TZNT-NTU
               ADD W-KIN TO TZNT-NA11 TZNT-NTA
           END-IF
           IF  W-GET = 12
               ADD W-UKIN TO TZNT-NU12 TZNT-NTU
               ADD W-KIN TO TZNT-NA12 TZNT-NTA
           END-IF
           IF  W-GET = 1
               ADD W-UKIN TO TZNT-NU01 TZNT-NTU
               ADD W-KIN TO TZNT-NA01 TZNT-NTA
           END-IF
           IF  W-GET = 2
               ADD W-UKIN TO TZNT-NU02 TZNT-NTU
               ADD W-KIN TO TZNT-NA02 TZNT-NTA
           END-IF
           IF  W-GET = 3
               ADD W-UKIN TO TZNT-NU03 TZNT-NTU
               ADD W-KIN TO TZNT-NA03 TZNT-NTA
           END-IF
           IF  W-GET = 4
               ADD W-UKIN TO TZNT-NU04 TZNT-NTU
               ADD W-KIN TO TZNT-NA04 TZNT-NTA
           END-IF
           MOVE W-NG TO TZNT-NG.
           GO TO SET-EX.
       SET-060.
           IF  W-GET = 5
               ADD W-UKIN TO TZNTP-NU05 TZNTP-NTU
               ADD W-KIN TO TZNTP-NA05 TZNTP-NTA
           END-IF
           IF  W-GET = 6
               ADD W-UKIN TO TZNTP-NU06 TZNTP-NTU
               ADD W-KIN TO TZNTP-NA06 TZNTP-NTA
           END-IF
           IF  W-GET = 7
               ADD W-UKIN TO TZNTP-NU07 TZNTP-NTU
               ADD W-KIN TO TZNTP-NA07 TZNTP-NTA
           END-IF
           IF  W-GET = 8
               ADD W-UKIN TO TZNTP-NU08 TZNTP-NTU
               ADD W-KIN TO TZNTP-NA08 TZNTP-NTA
           END-IF
           IF  W-GET = 9
               ADD W-UKIN TO TZNTP-NU09 TZNTP-NTU
               ADD W-KIN TO TZNTP-NA09 TZNTP-NTA
           END-IF
           IF  W-GET = 10
               ADD W-UKIN TO TZNTP-NU10 TZNTP-NTU
               ADD W-KIN TO TZNTP-NA10 TZNTP-NTA
           END-IF
           IF  W-GET = 11
               ADD W-UKIN TO TZNTP-NU11 TZNTP-NTU
               ADD W-KIN TO TZNTP-NA11 TZNTP-NTA
           END-IF
           IF  W-GET = 12
               ADD W-UKIN TO TZNTP-NU12 TZNTP-NTU
               ADD W-KIN TO TZNTP-NA12 TZNTP-NTA
           END-IF
           IF  W-GET = 1
               ADD W-UKIN TO TZNTP-NU01 TZNTP-NTU
               ADD W-KIN TO TZNTP-NA01 TZNTP-NTA
           END-IF
           IF  W-GET = 2
               ADD W-UKIN TO TZNTP-NU02 TZNTP-NTU
               ADD W-KIN TO TZNTP-NA02 TZNTP-NTA
           END-IF
           IF  W-GET = 3
               ADD W-UKIN TO TZNTP-NU03 TZNTP-NTU
               ADD W-KIN TO TZNTP-NA03 TZNTP-NTA
           END-IF
           IF  W-GET = 4
               ADD W-UKIN TO TZNTP-NU04 TZNTP-NTU
               ADD W-KIN TO TZNTP-NA04 TZNTP-NTA
           END-IF
           MOVE W-NG TO TZNTP-NG.
       SET-EX.
           EXIT.
       TYR-RTN.
           MOVE ZERO TO TTMYR-R.
           MOVE TT-R TO TTMYR-R.
           MOVE W-NG TO TTMYR-DNG.
      *           WRITE TTMYR-R.
      *//////////////
           CALL "DB_Insert" USING
            TTMYR_PNAME1 TTMYR_LNAME TTMYR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO TYR-EX
           END-IF
      *
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME10" E-ME10 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TYR-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TTMYR_IDLST TTMYR_PNAME1.
           MOVE "TTMYR        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" TTMYR_PNAME1 " " BY REFERENCE TTMYR_IDLST "0".
           GO TO TYR-RTN.
       TYR-EX.
           EXIT.
       ZDW-RTN.
           IF  TT-TUZ = ZERO
               GO TO ZDW-010
           END-IF
           MOVE ZERO TO ZD-R.
           MOVE 11 TO ZD-NO.
           MOVE TT-KEY TO ZD-KEY.
           MOVE TT-TUZ TO ZD-KIN.
           MOVE W-NGS TO ZD-NG.
      *           WRITE ZD-R.
      *//////////////
           CALL "DB_Insert" USING
            ZD-F_PNAME1 ZD-F_LNAME ZD-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO ZDW-010
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME11" E-ME11 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO ZDW-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE ZD-F_IDLST ZD-F_PNAME1.
           MOVE "ZDF          " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" ZD-F_PNAME1 " " BY REFERENCE ZD-F_IDLST "0".
           GO TO ZDW-RTN.
       ZDW-010.
           IF  TT-TUZZ = ZERO
               GO TO ZDW-EX
           END-IF.
       ZDW-020.
           MOVE ZERO TO ZD-R.
           MOVE 12 TO ZD-NO.
           MOVE TT-KEY TO ZD-KEY.
           MOVE TT-TUZZ TO ZD-KIN.
           MOVE W-NGS TO ZD-NG.
      *           WRITE ZD-R.
      *//////////////
           CALL "DB_Insert" USING
            ZD-F_PNAME1 ZD-F_LNAME ZD-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO ZDW-EX
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME11" E-ME11 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO ZDW-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE ZD-F_IDLST ZD-F_PNAME1.
           MOVE "ZDF          " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" ZD-F_PNAME1 " " BY REFERENCE ZD-F_IDLST "0".
           GO TO ZDW-020.
       ZDW-EX.
           EXIT.

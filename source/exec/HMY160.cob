       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         HMY160.
      **************************************************************
      *    PROGRAM         :  年間担当得意先月別　売上・粗利対比表 *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  ______                               *
      *        変更　　　  :  62/05/12                             *
      *    COMPILE TYPE    :  COBOL                                *
      *    W-JS            :  締め後=0 前=1                        *
      *    JS-SIGN         :  全体=0 一般=1 教育=2 ＶＩＶ=3        *
      *    W-JS9           :  作表=0 エクセル=1                    *
      **************************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  W-JS               PIC  9(001).
       77  JS-SIGN            PIC  9(001).
       77  W-JS9              PIC  9(001).
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0512".
           02  W-FID2         PIC  X(003).
       01  HEAD01.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  X(037) VALUE SPACE.
           02  H-UAM          PIC  N(002).
           02  F              PIC  N(013) VALUE
                 "　担当者得意先　月別対比表".
           02  F              PIC  X(039) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068212078".
       01  HEAD02.
           02  F              PIC  X(051) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  H-IKMD         PIC  N(011).
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  X(053) VALUE SPACE.
       01  HEAD03.
           02  F              PIC  X(048) VALUE SPACE.
           02  F              PIC  N(002) VALUE "（’".
           02  H-SNEN         PIC  N(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-SGET         PIC  N(002).
           02  F              PIC  N(005) VALUE "月　～　’".
           02  H-ENEN         PIC  N(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-EGET         PIC  N(002).
           02  F              PIC  N(002) VALUE "月）".
           02  F              PIC  X(050) VALUE SPACE.
       01  HEAD11.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  H-IKM          PIC  N(011).
           02  F              PIC  X(014) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-UAM1         PIC  N(002).
           02  F              PIC  N(018) VALUE
                 "　担当者得意先　月別対比表　　＊＊＊".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE1        PIC  99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE1        PIC  Z9.
       01  HEAD12.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(038) VALUE SPACE.
           02  F              PIC  N(014) VALUE
                 "＊＊＊　　年間　部門・月別　".
           02  H-UAM2         PIC  N(002).
           02  F              PIC  N(008) VALUE "対比表　　＊＊＊".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE2        PIC  99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE2        PIC  Z9.
       01  HEAD2.
           02  H-HM1          PIC  X(032).
           02  H-ANGD.
             03  H-X1         PIC  X(001).
             03  H-NGD   OCCURS   6.
               04  F          PIC  X(004).
               04  H-N        PIC  9(002).
               04  H-NM       PIC  N(001).
               04  H-G        PIC  9(002).
               04  H-GM       PIC  N(001).
           02  H-HM2          PIC  X(010).
           02  H-HM3          PIC  N(003).
           02  H-HM4          PIC  X(014).
           02  H-X2           PIC  X(001).
       01  H-HMD.
           02  H-HMD1         PIC  X(032) VALUE
                   "担当 ｺｰﾄﾞ 得　　意　　先　　名  ".
           02  H-HMD2         PIC  X(010) VALUE " :        ".
           02  H-HMD3A        PIC  N(003) VALUE "上期計".
           02  H-HMD3B        PIC  N(003) VALUE "下期計".
           02  H-HMD4A        PIC  X(014) VALUE "     <利益率%>".
           02  H-HMD4B        PIC  X(014) VALUE "        合　計".
       01  W-P1.
           02  F              PIC  X(001).
           02  P-TC1          PIC  9(002).
           02  F              PIC  X(002).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(026).
           02  F              PIC  X(075).
       01  W-P2.
           02  P-TMD          PIC  N(016).
           02  P-TMW   REDEFINES  P-TMD.
             03  F            PIC  X(001).
             03  P-TC2        PIC  9(002).
             03  F            PIC  X(001).
             03  P-TM         PIC  N(014).
           02  P-X1           PIC  X(001).
           02  P-UK.
             03  P-U     OCCURS   6  PIC  ----,---,--9.
           02  F              PIC  X(001).
           02  P-M            PIC  X(001).
           02  P-STU          PIC  --,---,---,--9.
           02  P-ATU          PIC  --,---,---,--9.
           02  P-RRD   REDEFINES  P-ATU.
             03  F            PIC  X(004).
             03  P-RF         PIC  X(001).
             03  P-RD         PIC  ---9.99.
             03  P-RR         PIC  X(002).
           02  P-X2           PIC  X(001).
       01  W-DATA.
           02  W-TC.
             03  W-TC1        PIC  9(001).
             03  W-TC2        PIC  9(001).
           02  CNT.
             03  CNT1         PIC  9(002).
             03  CNT2         PIC  9(002).
           02  W-UACD         PIC  9(001).
           02  W-UAC          PIC  9(001).
           02  W-C            PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-MC.
             03  W-MC1        PIC  9(001).
             03  W-MC2        PIC  9(001).
           02  W-NGD.
             03  W-NGD1.
               04  W-NG1   OCCURS   6  PIC  9(006).
             03  W-NGD2.
               04  W-NG2   OCCURS   6  PIC  9(006).
             03  W-NGD3.
               04  W-NG3   OCCURS   6  PIC  9(006).
             03  W-NGD4.
               04  W-NG4   OCCURS   6  PIC  9(006).
           02  W-YMD   REDEFINES  W-NGD.
             03  W-YM1   OCCURS  12.
               04  W-Y1       PIC  9(004).
               04  W-YD1   REDEFINES W-Y1.
                 05  W-Y11    PIC  9(002).
                 05  W-Y12    PIC  9(002).
               04  W-M1       PIC  9(002).
             03  W-YM2   OCCURS  12.
               04  W-Y2       PIC  9(004).
               04  W-YD2   REDEFINES W-Y2.
                 05  W-Y21    PIC  9(002).
                 05  W-Y22    PIC  9(002).
               04  W-M2       PIC  9(002).
           02  W-ADD.
             03  W-DD    OCCURS   6.
               04  W-DD1      PIC  9(004).
               04  W-DDD1  REDEFINES W-DD1.
                 05  W-DD11   PIC  9(002).
                 05  W-DD12   PIC  9(002).
               04  W-DD2      PIC  9(002).
           02  W-NG.
             03  W-N          PIC  9(004).
             03  W-ND    REDEFINES W-N.
               04  W-N1       PIC  9(002).
               04  W-N2       PIC  9(002).
             03  W-G          PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-UC           PIC  9(001).
           02  W-SETC.
             03  W-STC        PIC  9(002).
             03  W-ETC        PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-KIC          PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-ALL          PIC S9(003)V9(05).
           02  W-TUA.
             03  W-TAD.
               04  W-AOTAD    PIC S9(010).
               04  W-ANTAD    PIC S9(010).
             03  W-TUD.
               04  W-AOTUD    PIC S9(010).
               04  W-ANTUD    PIC S9(010).
           02  W-RD.
             03  W-ORD        PIC S9(003)V9(02).
             03  W-NRD        PIC S9(003)V9(02).
           02  W-D.
             03  W-UK.
               04  W-OUK1.
                 05  W-OU1   OCCURS   6  PIC  S9(009).
               04  W-OUK2.
                 05  W-OU2   OCCURS   6  PIC  S9(009).
               04  W-NUK1.
                 05  W-NU1   OCCURS   6  PIC  S9(009).
               04  W-NUK2.
                 05  W-NU2   OCCURS   6  PIC  S9(009).
             03  W-U     REDEFINES  W-UK.
               04  W-UKD   OCCURS  24  PIC S9(009).
             03  W-TU.
               04  W-AOTU     PIC S9(010).
               04  W-ANTU     PIC S9(010).
       01  WN-D.
           02  WN-SU1         PIC S9(010).
           02  WN-SU2         PIC S9(010).
           02  WN-SU3         PIC S9(010).
           02  WN-SU4         PIC S9(010).
       01  WT-D.
           02  WT-U1    OCCURS   6  PIC S9(009).
           02  WT-SU1         PIC S9(010).
           02  WT-U2    OCCURS   6  PIC S9(009).
           02  WT-SU2         PIC S9(010).
           02  WT-U3    OCCURS   6  PIC S9(009).
           02  WT-SU3         PIC S9(010).
           02  WT-U4    OCCURS   6  PIC S9(009).
           02  WT-SU4         PIC S9(010).
           02  WT-TUA.
             03  WT-TAD.
               04  WT-AOTU    PIC S9(010).
               04  WT-ANTU    PIC S9(010).
             03  WT-TUD.
               04  WT-AOTUD   PIC S9(010).
               04  WT-ANTUD   PIC S9(010).
       01  WS-D.
           02  WS-U1    OCCURS   6  PIC S9(009).
           02  WS-SU1         PIC S9(010).
           02  WS-U2    OCCURS   6  PIC S9(009).
           02  WS-SU2         PIC S9(010).
           02  WS-U3    OCCURS   6  PIC S9(009).
           02  WS-SU3         PIC S9(010).
           02  WS-U4    OCCURS   6  PIC S9(009).
           02  WS-SU4         PIC S9(010).
           02  WS-TUA.
             03  WS-TAD.
               04  WS-AOTU    PIC S9(010).
               04  WS-ANTU    PIC S9(010).
             03  WS-TUD.
               04  WS-AOTUD   PIC S9(010).
               04  WS-ANTUD   PIC S9(010).
       01  WA-D.
           02  WA-U1    OCCURS   6  PIC S9(009).
           02  WA-SU1         PIC S9(010).
           02  WA-U2    OCCURS   6  PIC S9(009).
           02  WA-SU2         PIC S9(010).
           02  WA-U3    OCCURS   6  PIC S9(009).
           02  WA-SU3         PIC S9(010).
           02  WA-U4    OCCURS   6  PIC S9(009).
           02  WA-SU4         PIC S9(010).
           02  WA-TUA.
             03  WA-TAD.
               04  WA-AOTU    PIC S9(010).
               04  WA-ANTU    PIC S9(010).
             03  WA-TUD.
               04  WA-AOTUD   PIC S9(010).
               04  WA-ANTUD   PIC S9(010).
       01  WB-D.
           02  WH-D.
             03  WH-U1    OCCURS   6  PIC S9(009).
             03  WH-SU1       PIC S9(010).
             03  WH-U2    OCCURS   6  PIC S9(009).
             03  WH-SU2       PIC S9(010).
             03  WH-U3    OCCURS   6  PIC S9(009).
             03  WH-SU3       PIC S9(010).
             03  WH-U4    OCCURS   6  PIC S9(009).
             03  WH-SU4       PIC S9(010).
             03  WH-AOTU      PIC S9(010).
             03  WH-ANTU      PIC S9(010).
             03  WH-AOTUD     PIC S9(010).
             03  WH-ANTUD     PIC S9(010).
           02  WK-D.
             03  WK-U1    OCCURS   6  PIC S9(009).
             03  WK-SU1       PIC S9(010).
             03  WK-U2    OCCURS   6  PIC S9(009).
             03  WK-SU2       PIC S9(010).
             03  WK-U3    OCCURS   6  PIC S9(009).
             03  WK-SU3       PIC S9(010).
             03  WK-U4    OCCURS   6  PIC S9(009).
             03  WK-SU4       PIC S9(010).
             03  WK-AOTU      PIC S9(010).
             03  WK-ANTU      PIC S9(010).
             03  WK-AOTUD     PIC S9(010).
             03  WK-ANTUD     PIC S9(010).
           02  WZ-D.
             03  WZ-U1    OCCURS   6  PIC S9(009).
             03  WZ-SU1       PIC S9(010).
             03  WZ-U2    OCCURS   6  PIC S9(009).
             03  WZ-SU2       PIC S9(010).
             03  WZ-U3    OCCURS   6  PIC S9(009).
             03  WZ-SU3       PIC S9(010).
             03  WZ-U4    OCCURS   6  PIC S9(009).
             03  WZ-SU4       PIC S9(010).
             03  WZ-AOTU      PIC S9(010).
             03  WZ-ANTU      PIC S9(010).
             03  WZ-AOTUD     PIC S9(010).
             03  WZ-ANTUD     PIC S9(010).
       01  W-TMD              PIC  N(016).
       01  W-UAM              PIC  N(002).
       01  W-IKM              PIC  N(011).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LSTAT.
           COPY LIBFDD.
           COPY LITM.
           COPY LSPF.
      *FD  THY-M
       01  THY-M_HMY160.
           02  THY-M_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  THY-M_LNAME    PIC  X(012) VALUE "THY-M_HMY160".
           02  F              PIC  X(001).
           02  THY-M_KEY1     PIC  X(100) VALUE SPACE.
           02  THY-M_KEY2     PIC  X(100) VALUE SPACE.
           02  THY-M_SORT     PIC  X(100) VALUE SPACE.
           02  THY-M_IDLST    PIC  X(100) VALUE SPACE.
           02  THY-M_RES      USAGE  POINTER.
       01  TH-R.
           02  TH-KEY         PIC  9(004).
           02  TH-IKC         PIC  9(001).
           02  TH-UD.
             03  TH-UK.
               04  TH-OUK1.
                 05  TH-OU1   OCCURS   6  PIC  9(009).
               04  TH-OUK2.
                 05  TH-OU2   OCCURS   6  PIC  9(009).
               04  TH-NUK1.
                 05  TH-NU1   OCCURS   6  PIC  9(009).
               04  TH-NUK2.
                 05  TH-NU2   OCCURS   6  PIC  9(009).
             03  TH-U     REDEFINES TH-UK.
               04  TH-UKD   OCCURS  24  PIC S9(009).
             03  TH-TU.
               04  TH-AOTU    PIC S9(010).
               04  TH-ANTU    PIC S9(010).
           02  TH-AD.
             03  TH-AK.
               04  TH-OAK1.
                 05  TH-OA1   OCCURS   6  PIC  9(009).
               04  TH-OAK2.
                 05  TH-OA2   OCCURS   6  PIC  9(009).
               04  TH-NAK1.
                 05  TH-NA1   OCCURS   6  PIC  9(009).
               04  TH-NAK2.
                 05  TH-NA2   OCCURS   6  PIC  9(009).
             03  TH-A  REDEFINES TH-AK.
               04  TH-AKD   OCCURS  24  PIC S9(009).
             03  TH-TA.
               04  TH-AOTA    PIC S9(010).
               04  TH-ANTA    PIC S9(010).
           02  TH-NG.
             03  TH-N         PIC  9(004).
             03  TH-G         PIC  9(002).
           02  TH-TC.
             03  TH-TC1       PIC  9(001).
             03  TH-TC2       PIC  9(001).
           02  TH-BC          PIC  9(001).
           02  F              PIC  X(026).
       77  F                  PIC  X(001).
      *FD  EXL-F
       01  EXL-F_HMY160.
           02  EXL-F_PNAME1   PIC  X(009) VALUE "WK0256000".
           02  F              PIC  X(001).
           02  EXL-F_LNAME    PIC  X(012) VALUE "EXL-F_HMY160".
           02  F              PIC  X(001).
           02  EXL-F_KEY1     PIC  X(100) VALUE SPACE.
           02  EXL-F_KEY2     PIC  X(100) VALUE SPACE.
           02  EXL-F_SORT     PIC  X(100) VALUE SPACE.
           02  EXL-F_IDLST    PIC  X(100) VALUE SPACE.
           02  EXL-F_RES      USAGE  POINTER.
       01  EXL-R.
           02  EXL-TNC        PIC  9(002).
           02  EXL-TCD        PIC  9(004).
           02  EXL-NAME       PIC  N(026).
           02  EXL-TKIN.
             03  EXL-KIND  OCCURS   6.
               04  EXL-KIN    PIC S9(009).
           02  EXL-SKIN       PIC S9(010).
           02  EXL-AKIN       PIC S9(010).
           02  F              PIC  X(124).
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
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊　　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊　　売上・粗利　担当得意先月別対比表　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊　　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(027) VALUE
                "売　上 = 1   粗　利 = 5    ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-UAC   PIC  9(001).
           02  A-UC    PIC  9(001).
           02  FILLER.
             03  A-STC   PIC  9(002).
             03  A-ETC   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-DPM0.
               04  FILLER  PIC  X(039) VALUE
                    "履　物 = 0   工品他 = 1   合　計 = 2   ".
               04  FILLER  PIC  X(039) VALUE
                    "担当計 = 3   終　了 = 9    ".
             03  D-DPM1.
               04  FILLER  PIC  N(011).
               04  FILLER  PIC  X(040) VALUE
                    "得意先 = 0   担当計 = 3   終　了 = 9    ".
           02  FILLER.
             03  D-TCM   PIC  X(020) VALUE
                  "担当者ｺｰﾄﾞ  00 ～ 99".
             03  D-TCMC  PIC  X(020) VALUE
                  "                    ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                   "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY  LIBSCR.
       PROCEDURE           DIVISION.
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
            "C-MID" " " "0" "0" "413" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "52" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "52" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "52" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "52" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "52" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "52" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "52" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "13" "18" "27" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "22" "25" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UAC" "9" "13" "44" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UAC" BY REFERENCE W-UAC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UC" "9" "16" "68" "1" "A-UAC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UC" BY REFERENCE W-UC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "18" "0" "4" "A-UC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STC" "9" "18" "51" "2" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STC" BY REFERENCE W-STC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ETC" "9" "18" "57" "2" "A-STC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ETC" BY REFERENCE W-ETC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "42" "1" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "180" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "16" "0" "140" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DPM0" " " "16" "0" "78" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DPM0" "X" "16" "3" "39" " " "D-DPM0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DPM0" "X" "16" "42" "39" "01D-DPM0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DPM1" " " "16" "0" "62" "D-DPM0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DPM1" "bN" "16" "5" "22" " " "D-DPM1" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DPM1" BY REFERENCE W-IKM "22" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DPM1" "X" "16" "29" "40" "01D-DPM1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "18" "0" "40" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TCM" "X" "18" "39" "20" " " "02C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TCMC" "X" "18" "39" "20" "D-TCM" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "27" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "27" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
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
           ACCEPT  W-JS FROM ARGUMENT-VALUE.
           IF  W-JS > 1
               GO TO M-900
           END-IF
           ACCEPT  JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 3
               GO TO M-900
           END-IF
           ACCEPT  W-JS9 FROM ARGUMENT-VALUE.
           IF  W-JS9 > 1
               GO TO M-900
           END-IF
           MOVE SPACE TO W-IKM.
           IF  JS-SIGN = ZERO
               MOVE "【　　全　　　体　　】" TO W-IKM
           END-IF
           IF  JS-SIGN = 1
               MOVE "【　　一　　　般　　】" TO W-IKM
           END-IF
           IF  JS-SIGN = 2
               MOVE "【　　教　　　育　　】" TO W-IKM
           END-IF
           IF  JS-SIGN = 3
               MOVE "【　ヴィヴェンディ　】" TO W-IKM
           END-IF
           MOVE W-IKM TO H-IKM.
           MOVE ZERO TO W-DATA.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0512ID.
           MOVE WK0512ID TO THY-M_PNAME1.
           COPY LIBCPR.
           PERFORM NGS-RTN THRU NGS-EX.
       M-060.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = ZERO
               CALL "SD_Output" USING "D-DPM0" D-DPM0 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-DPM1" D-DPM1 "p" RETURNING RESU
           END-IF
           PERFORM ACP-RTN THRU ACP-EX.
           IF  ESTAT = PF9
               GO TO M-900
           END-IF
      *
           IF  W-UC = 9
               GO TO M-900
           END-IF
           IF  W-JS9 = 1
               GO TO M-130
           END-IF
           IF  W-UAC = 1
               MOVE "売上" TO W-UAM
           END-IF
           IF  W-UAC = 5
               MOVE "粗利" TO W-UAM
           END-IF
           MOVE W-UAM TO H-UAM H-UAM1 H-UAM2.
           MOVE ZERO TO W-PAGE.
       M-130.
           CALL "DB_F_Open" USING
            "INPUT" THY-M_PNAME1 " " BY REFERENCE THY-M_IDLST "0".
       M-140.
      *           READ THY-M AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" THY-M_PNAME1 BY REFERENCE TH-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE THY-M_IDLST THY-M_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-060
           END-IF
           IF  W-UC = ZERO
               IF  TH-BC NOT = ZERO
                   GO TO M-140
               END-IF
           END-IF
           IF  W-UC = 1
               IF  TH-BC = ZERO
                   GO TO M-140
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  W-UC = 0
                   IF  TH-TC < W-STC
                       GO TO M-140
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  W-UC = 0
                   IF  TH-TC > W-ETC
                       CALL "DB_F_Close" USING
                        BY REFERENCE THY-M_IDLST THY-M_PNAME1
                       CALL "SD_Output" USING
                        "E-ME1" E-ME1 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME99" E-ME99 "p" RETURNING RESU
                       GO TO M-060
                   END-IF
               END-IF
           END-IF
           PERFORM CHK-RTN THRU CHK-EX.
           IF  CHK = ZERO
               GO TO M-140
           END-IF
           MOVE ZERO TO WA-D WB-D.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           IF  W-JS9 = 1
               CALL "DB_F_Open" USING
                "OUTPUT" EXL-F_PNAME1 " " BY REFERENCE EXL-F_IDLST "0"
           ELSE
               CALL "PR_Open" RETURNING RESP
               PERFORM MID-040 THRU MID-EX
           END-IF
           IF  W-UC = 2
               GO TO M-220
           END-IF
           IF  W-UC NOT = 3
               IF  W-JS9 NOT = 1
                   MOVE SPACE TO W-P1
               END-IF
           END-IF.
       M-160.
           MOVE ZERO TO WS-D.
           MOVE TH-TC1 TO W-TC1.
       M-180.
           MOVE ZERO TO WT-D.
           IF  W-JS9 = 1
               MOVE ZERO TO W-MC
           END-IF
           MOVE TH-TC2 TO W-TC2.
           IF  W-UC NOT = 3
               IF  W-JS9 NOT = 1
                   MOVE W-TC TO P-TC1
               END-IF
           END-IF.
       M-200.
           IF  W-JS9 = 1
               MOVE 0 TO W-MC2
           END-IF
           IF  W-UC = 3
               GO TO M-220
           END-IF
           MOVE TH-KEY TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　" TO T-NAME
           END-IF
           IF  W-JS9 = 1
               GO TO M-220
           END-IF
           MOVE TH-KEY TO P-TCD.
           MOVE T-NAME TO P-NAME.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE W-TC TO P-TC1
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P1.
       M-220.
           MOVE ZERO TO CNT1 W-D.
           IF  W-UAC = 1
               MOVE TH-UD TO W-D
               GO TO M-240
           END-IF
           MOVE TH-AD TO W-D.
           IF  W-UC NOT = 2
               MOVE ZERO TO W-TUA
               MOVE TH-TA TO W-TAD
               MOVE TH-TU TO W-TUD
               PERFORM RRK-RTN THRU RRK-EX
           END-IF.
       M-240.
           ADD 1 TO CNT1.
           IF  CNT1 = 5
               GO TO M-360
           END-IF
           MOVE ZERO TO WN-D CNT2.
           IF  W-UC NOT = 2 AND 3
               IF  W-JS9 = 1
                   INITIALIZE EXL-R
                   MOVE SPACE TO EXL-NAME
               ELSE
                   MOVE SPACE TO W-P2
               END-IF
           END-IF
           IF  W-JS9 = 1
               IF  W-MC1 = 0
                   MOVE 1 TO W-MC1
                   MOVE W-TC TO EXL-TNC
               END-IF
           END-IF
           IF  W-JS9 = 1
               IF  W-MC2 = 0
                   MOVE 1 TO W-MC2
                   MOVE T-KEY TO EXL-TCD
                   MOVE T-NAME TO EXL-NAME
               END-IF
           END-IF.
       M-260.
           ADD 1 TO CNT2.
           IF  CNT2 = 7
               GO TO M-320
           END-IF
           IF  W-UC = 2
               GO TO M-280
           END-IF
           IF  CNT1 = 1
               ADD W-OU1(CNT2) TO WN-SU1 WT-U1(CNT2) WT-SU1
               IF  W-UC NOT = 3
                   IF  W-JS9 = 1
                       MOVE W-OU1(CNT2) TO EXL-KIN(CNT2)
                   ELSE
                       MOVE "(" TO P-X1
                       MOVE ")" TO P-X2
                       MOVE W-OU1(CNT2) TO P-U(CNT2)
                   END-IF
               END-IF
           END-IF
           IF  CNT1 = 2
               ADD W-OU2(CNT2) TO WN-SU2 WT-U2(CNT2) WT-SU2
               IF  W-UC NOT = 3
                   IF  W-JS9 = 1
                       MOVE W-OU2(CNT2) TO EXL-KIN(CNT2)
                   ELSE
                       MOVE "(" TO P-X1
                       MOVE ")" TO P-X2
                       MOVE W-OU2(CNT2) TO P-U(CNT2)
                   END-IF
               END-IF
           END-IF
           IF  CNT1 = 3
               ADD W-NU1(CNT2) TO WN-SU3 WT-U3(CNT2) WT-SU3
               IF  W-UC NOT = 3
                   IF  W-JS9 = 1
                       MOVE W-NU1(CNT2) TO EXL-KIN(CNT2)
                   ELSE
                       MOVE W-NU1(CNT2) TO P-U(CNT2)
                   END-IF
               END-IF
           END-IF
           IF  CNT1 = 4
               ADD W-NU2(CNT2) TO WN-SU4 WT-U4(CNT2) WT-SU4
               IF  W-UC NOT = 3
                   IF  W-JS9 = 1
                       MOVE W-NU2(CNT2) TO EXL-KIN(CNT2)
                   ELSE
                       MOVE W-NU2(CNT2) TO P-U(CNT2)
                   END-IF
               END-IF
           END-IF
           GO TO M-260.
       M-280.
           PERFORM TOT-RTN THRU TOT-EX.
           GO TO M-260.
       M-320.
           IF  W-UC = 2
               GO TO M-340
           END-IF
           IF  W-UC = 3
               GO TO M-330
           END-IF
           IF  W-JS9 NOT = 1
               MOVE ":" TO P-M
           END-IF
           IF  CNT1 = 1
               IF  W-JS9 = 1
                   MOVE WN-SU1 TO EXL-SKIN
               ELSE
                   MOVE WN-SU1 TO P-STU
                   IF  W-UAC = 5
                       MOVE "<" TO P-RF
                       MOVE W-ORD TO P-RD
                       MOVE "%>" TO P-RR
                       ADD W-AOTUD TO WT-AOTUD
                   END-IF
               END-IF
           END-IF
           IF  CNT1 = 2
               ADD W-AOTU TO WT-AOTU
               IF  W-JS9 = 1
                   MOVE WN-SU2 TO EXL-SKIN
                   MOVE W-AOTU TO EXL-AKIN
               ELSE
                   MOVE WN-SU2 TO P-STU
                   MOVE W-AOTU TO P-ATU
               END-IF
           END-IF
           IF  CNT1 = 3
               IF  W-JS9 = 1
                   MOVE WN-SU3 TO EXL-SKIN
               ELSE
                   MOVE WN-SU3 TO P-STU
                   IF  W-UAC = 5
                       MOVE "<" TO P-RF
                       MOVE W-NRD TO P-RD
                       MOVE "%>" TO P-RR
                       ADD W-ANTUD TO WT-ANTUD
                   END-IF
               END-IF
           END-IF
           IF  CNT1 = 4
               ADD W-ANTU TO WT-ANTU
               IF  W-JS9 = 1
                   MOVE WN-SU4 TO EXL-SKIN
                   MOVE W-ANTU TO EXL-AKIN
               ELSE
                   MOVE WN-SU4 TO P-STU
                   MOVE W-ANTU TO P-ATU
               END-IF
           END-IF
           IF  W-JS9 = 1
      *               WRITE EXL-R
      *///////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO M-240
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO M-240.
       M-330.
           IF  CNT1 = 1
               IF  W-UAC = 5
                   ADD W-AOTUD TO WT-AOTUD
               END-IF
           END-IF
           IF  CNT1 = 2
               ADD W-AOTU TO WT-AOTU
           END-IF
           IF  CNT1 = 3
               IF  W-UAC = 5
                   ADD W-ANTUD TO WT-ANTUD
               END-IF
           END-IF
           IF  CNT1 = 4
               ADD W-ANTU TO WT-ANTU
           END-IF
           GO TO M-240.
      ******************     合計　合計　集計     ********************
       M-340.
           IF  TH-BC = ZERO
               IF  CNT1 = 2
                   ADD W-AOTU TO WH-AOTU
                   IF  W-UAC = 5
                       ADD TH-AOTU TO WH-AOTUD
                   END-IF
               END-IF
           END-IF
           IF  TH-BC = ZERO
               IF  CNT1 = 4
                   ADD W-ANTU TO WH-ANTU
                   IF  W-UAC = 5
                       ADD TH-ANTU TO WH-ANTUD
                   END-IF
               END-IF
           END-IF
           IF  TH-BC = 1
               IF  CNT1 = 2
                   ADD W-AOTU TO WK-AOTU
                   IF  W-UAC = 5
                       ADD TH-AOTU TO WK-AOTUD
                   END-IF
               END-IF
           END-IF
           IF  TH-BC = 1
               IF  CNT1 = 4
                   ADD W-ANTU TO WK-ANTU
                   IF  W-UAC = 5
                       ADD TH-ANTU TO WK-ANTUD
                   END-IF
               END-IF
           END-IF
           IF  TH-BC = 3
               IF  CNT1 = 2
                   ADD W-AOTU TO WZ-AOTU
                   IF  W-UAC = 5
                       ADD TH-AOTU TO WZ-AOTUD
                   END-IF
               END-IF
           END-IF
           IF  TH-BC = 3
               IF  CNT1 = 4
                   ADD W-ANTU TO WZ-ANTU
                   IF  W-UAC = 5
                       ADD TH-ANTU TO WZ-ANTUD
                   END-IF
               END-IF
           END-IF
           GO TO M-240.
       M-360.
      *           READ THY-M AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" THY-M_PNAME1 BY REFERENCE TH-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-400
           END-IF
           IF  W-UC = ZERO
               IF  TH-BC NOT = ZERO
                   GO TO M-360
               END-IF
           END-IF
           IF  W-UC = 1
               IF  TH-BC = ZERO
                   GO TO M-360
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  W-UC = 0
                   IF  TH-TC > W-ETC
                       GO  TO  M-400
                   END-IF
               END-IF
           END-IF
           PERFORM CHK-RTN THRU CHK-EX.
           IF  CHK = ZERO
               GO TO M-360
           END-IF
           IF  W-UC = 2
               GO TO M-220
           END-IF
           IF  TH-TC1 NOT = W-TC1
               GO TO M-380
           END-IF
           IF  TH-TC2 = W-TC2
               GO TO M-200
           END-IF
           PERFORM TPR-RTN THRU TPR-EX.
           IF  W-JS9 NOT = 1
               IF  W-UC NOT = 3
                   PERFORM MID-RTN THRU MID-EX
               END-IF
           END-IF
           GO TO M-180.
       M-380.
           PERFORM TPR-RTN THRU TPR-EX.
           PERFORM SPR-RTN THRU SPR-EX.
           IF  W-JS9 NOT = 1
               IF  W-UC NOT = 3
                   PERFORM MID-RTN THRU MID-EX
               END-IF
           END-IF
           GO TO M-160.
       M-400.
           IF  W-UC = 2
               GO TO M-420
           END-IF
           PERFORM TPR-RTN THRU TPR-EX.
           PERFORM SPR-RTN THRU SPR-EX.
           PERFORM APR-RTN THRU APR-EX.
           GO TO M-440.
      ******************     合計　作表     **************************
       M-420.
           MOVE "　　履　　物　　合　　計　　　　" TO W-TMD.
           MOVE ZERO TO WS-D.
           MOVE WH-D TO WS-D.
           PERFORM SPR-RTN THRU SPR-EX.
           IF  W-JS9 NOT = 1
               MOVE SPACE TO SP-R
               CALL "PR_LineFeed" USING "4" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
           MOVE "　　工　　品　　合　　計　　　　" TO W-TMD.
           MOVE ZERO TO WS-D.
           MOVE WK-D TO WS-D.
           PERFORM SPR-RTN THRU SPR-EX.
           IF  W-JS9 NOT = 1
               MOVE SPACE TO SP-R
               CALL "PR_LineFeed" USING "4" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
           MOVE "　　【　　合　　計　　】　　　　" TO W-TMD.
           PERFORM APR-RTN THRU APR-EX.
           IF  W-JS9 NOT = 1
               MOVE SPACE TO SP-R
               CALL "PR_LineFeed" USING "6" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
           MOVE "　　材　　料　　合　　計　　　　" TO W-TMD.
           MOVE ZERO TO WS-D.
           MOVE WZ-D TO WS-D.
           PERFORM SPR-RTN THRU SPR-EX.
       M-440.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE THY-M_IDLST THY-M_PNAME1.
           IF  W-JS9 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE EXL-F_IDLST EXL-F_PNAME1
           ELSE
               CALL "PR_Close" RETURNING RESP
           END-IF
           GO TO M-060.
       M-900.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *********************     入　　　力　    ***********************
       ACP-RTN.
           IF  W-JS9 = 1
               MOVE 1 TO W-UAC
               CALL "SD_Output" USING "A-UAC" A-UAC "p" RETURNING RESU
               GO TO ACP-020
           END-IF.
       ACP-010.
           CALL "SD_Accept" USING BY REFERENCE A-UAC "A-UAC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-010
           END-IF
           IF  W-UAC NOT = 1 AND 5
               GO TO ACP-010
           END-IF.
       ACP-020.
           CALL "SD_Accept" USING BY REFERENCE A-UC "A-UC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               IF  W-JS9 NOT = 1
                   GO TO ACP-010
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-020
           END-IF
           IF  W-UC NOT = ZERO AND 1 AND 2 AND 3 AND 9
               GO TO ACP-020
           END-IF
           IF  JS-SIGN NOT = ZERO
               IF  W-UC NOT = ZERO AND 3 AND 9
                   GO TO ACP-020
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  W-UC = 0
                   CALL "SD_Output" USING
                    "D-TCM" D-TCM "p" RETURNING RESU
                   MOVE 99 TO W-ETC
                   GO TO ACP-040
               END-IF
           END-IF
           CALL "SD_Output" USING "D-TCMC" D-TCMC "p" RETURNING RESU.
           GO TO ACP-080.
       ACP-040.
           CALL "SD_Accept" USING BY REFERENCE A-STC "A-STC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-040
           END-IF.
       ACP-060.
           CALL "SD_Accept" USING BY REFERENCE A-ETC "A-ETC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-060
           END-IF
           IF  W-STC > W-ETC
               GO TO ACP-060
           END-IF.
       ACP-080.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  JS-SIGN NOT = 1
                   GO TO ACP-020
                ELSE
                   IF  W-UC = 0
                       GO TO ACP-060
                   ELSE
                       GO TO ACP-020
                   END-IF
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-080
           END-IF
           IF  W-DMM = 9
               GO TO ACP-RTN
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-080
           END-IF.
       ACP-EX.
           EXIT.
      *********************     年月　セット     ***********************
       NGS-RTN.
           MOVE DATE-02R TO H-DATE1 H-DATE2.
           MOVE ZERO TO W-NG.
           MOVE D-NHNG TO W-NGS.
           IF  W-N2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-N
           END-IF
           IF  W-N2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-N
           END-IF
           IF  W-JS = 0
               SUBTRACT 1 FROM W-G
           END-IF
           IF  W-G = ZERO
               MOVE 12 TO W-G
               SUBTRACT 1 FROM W-N
           END-IF
           MOVE W-N2 TO H-ENEN.
           MOVE W-G TO H-EGET.
           IF  W-G < 5
               SUBTRACT 1 FROM W-N
           END-IF
           MOVE 4 TO W-G.
           MOVE ZERO TO CNT1.
           MOVE 1 TO CNT2.
       NGS-020.
           IF  CNT1 = ZERO
               MOVE W-NG TO W-YM2(CNT2)
           ELSE
               MOVE W-YM2(CNT1) TO W-YM2(CNT2)
           END-IF
           ADD 1 TO W-M2(CNT2).
           IF  W-M2(CNT2) = 13
               ADD 1 TO W-Y2(CNT2)
               MOVE 1 TO W-M2(CNT2)
           END-IF
           MOVE W-YM2(CNT2) TO W-YM1(CNT2).
           SUBTRACT 1 FROM W-Y1(CNT2).
           ADD 1 TO CNT1 CNT2.
           IF  CNT1 NOT = 12
               GO TO NGS-020
           END-IF
           MOVE W-Y12(1) TO H-SNEN.
           MOVE W-M1(1) TO H-SGET.
       NGS-EX.
           EXIT.
      *********************     見出し　作表     ***********************
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-040.
           IF  JS-SIGN = 1
               IF  W-UC = 0
                   IF (W-STC NOT = 00) OR (W-ETC NOT = 99)
                       GO TO MID-050
                   END-IF
               END-IF
           END-IF
           IF  W-UC NOT = 1 AND 2 AND 3
               IF  W-UAC NOT = W-UACD
                   MOVE W-UAC TO W-UACD
                   MOVE SPACE TO SP-R
                   CALL "PR_LineFeed" USING "20" RETURNING RESP
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   MOVE HEAD01 TO SP-R
                   CALL "PR_LineFeed" USING "2" RETURNING RESP
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   MOVE SPACE TO SP-R
                   MOVE W-IKM TO H-IKMD
                   MOVE HEAD02 TO SP-R
                   CALL "PR_LineFeed" USING "4" RETURNING RESP
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   MOVE SPACE TO SP-R
                   MOVE HEAD03 TO SP-R
                   CALL "PR_LineFeed" USING "4" RETURNING RESP
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   MOVE SPACE TO SP-R
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   CALL "PR_NewPage" RETURNING RESP
               END-IF
           END-IF.
       MID-050.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE1 H-PAGE2.
           MOVE SPACE TO SP-R.
           IF  W-UC NOT = 2
               MOVE HEAD11 TO SP-R
           ELSE
               MOVE HEAD12 TO SP-R
           END-IF
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO W-C.
       MID-060.
           ADD 1 TO W-C.
           IF  W-C = 5
               GO TO MID-120
           END-IF
           MOVE SPACE TO HEAD2.
           MOVE H-HMD2 TO H-HM2.
           MOVE ZERO TO W-ADD.
           IF  W-C NOT = 1
               GO TO MID-080
           END-IF
           MOVE "(" TO H-X1.
           MOVE ")" TO H-X2.
           MOVE H-HMD3A TO H-HM3.
           MOVE W-NGD1 TO W-ADD.
           IF  W-UC NOT = 2
               MOVE H-HMD1 TO H-HM1
           END-IF
           IF  W-UAC = 5
               MOVE H-HMD4A TO H-HM4
           END-IF.
       MID-080.
           IF  W-C = 2
               MOVE "(" TO H-X1
               MOVE ")" TO H-X2
               MOVE H-HMD3B TO H-HM3
               MOVE H-HMD4B TO H-HM4
               MOVE W-NGD2 TO W-ADD
           END-IF
           IF  W-C = 3
               MOVE H-HMD3A TO H-HM3
               MOVE W-NGD3 TO W-ADD
               IF  W-UAC = 5
                   MOVE H-HMD4A TO H-HM4
               END-IF
           END-IF
           IF  W-C = 4
               MOVE H-HMD3B TO H-HM3
               MOVE H-HMD4B TO H-HM4
               MOVE W-NGD4 TO W-ADD
           END-IF
           MOVE ZERO TO CHK.
       MID-100.
           ADD 1 TO CHK.
           IF  CHK NOT = 7
               MOVE W-DD12(CHK) TO H-N(CHK)
               MOVE W-DD2(CHK) TO H-G(CHK)
               MOVE "年" TO H-NM(CHK)
               MOVE "月" TO H-GM(CHK)
               GO TO MID-100
           END-IF
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           IF  W-C = 1
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           ELSE
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
           GO TO MID-060.
       MID-120.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       MID-EX.
           EXIT.
      **************     ＤＡＴＡ　ＺＥＲＯ　チェック     **************
       CHK-RTN.
           MOVE ZERO TO CNT CHK.
       CHK-020.
           ADD 1 TO CNT1.
           IF  CNT1 = 25
               GO TO CHK-EX
           END-IF
           IF  W-UAC = 1
               IF  TH-UKD(CNT1) = ZERO
                   GO TO CHK-020
               END-IF
           END-IF
           IF  W-UAC = 5
               IF  TH-AKD(CNT1) = ZERO
                   GO TO CHK-020
               END-IF
           END-IF
           MOVE 5 TO CHK.
       CHK-EX.
           EXIT.
      ***************     担当コード別　合計　作表     *****************
       TPR-RTN.
           MOVE ZERO TO CNT1 W-TUA.
           IF  W-UAC = 5
               MOVE WT-TUA TO W-TUA
               PERFORM RRK-RTN THRU RRK-EX
           END-IF.
       TPR-020.
           ADD 1 TO CNT1.
           IF  CNT1 = 5
               GO TO TPR-EX
           END-IF
           IF  W-JS9 = 1
               INITIALIZE EXL-R
               MOVE SPACE TO EXL-NAME
           ELSE
               MOVE SPACE TO W-P2
               MOVE SPACE TO P-TM
               MOVE ":" TO P-M
           END-IF
           IF  CNT1 = 1
               IF  W-UC = 3
                   IF  W-JS9 = 1
                       MOVE W-TC TO EXL-TNC
                   ELSE
                       MOVE W-TC TO P-TC2
                   END-IF
               END-IF
           END-IF
           IF  CNT1 = 1
               ADD WT-SU1 TO WS-SU1
               IF  W-JS9 = 1
                   MOVE "　　　（　ＴＯＴＡＬ　）　　" TO EXL-NAME
                   MOVE WT-SU1 TO EXL-SKIN
               ELSE
                   MOVE "　　　（　ＴＯＴＡＬ　）　　" TO P-TM
                   MOVE "(" TO P-X1
                   MOVE ")" TO P-X2
                   MOVE WT-SU1 TO P-STU
                   IF  W-UAC = 5
                       MOVE "<" TO P-RF
                       MOVE W-ORD TO P-RD
                       MOVE "%>" TO P-RR
                       ADD WT-AOTUD TO WS-AOTUD
                   END-IF
               END-IF
           END-IF
           IF  CNT1 = 2
               ADD  WT-SU2 TO WS-SU2
               ADD WT-AOTU TO WS-AOTU
               IF  W-JS9 = 1
                   MOVE WT-SU2 TO EXL-SKIN
                   MOVE WT-AOTU TO EXL-AKIN
               ELSE
                   MOVE "(" TO P-X1
                   MOVE ")" TO P-X2
                   MOVE WT-SU2 TO P-STU
                   MOVE WT-AOTU TO P-ATU
               END-IF
           END-IF
           IF  CNT1 = 3
               ADD WT-SU3 TO WS-SU3
               IF  W-JS9 = 1
                   MOVE WT-SU3 TO EXL-SKIN
               ELSE
                   MOVE WT-SU3 TO P-STU
                   IF  W-UAC = 5
                       MOVE "<" TO P-RF
                       MOVE W-NRD TO P-RD
                       MOVE "%>" TO P-RR
                       ADD WT-ANTUD TO WS-ANTUD
                   END-IF
               END-IF
           END-IF
           IF  CNT1 = 4
               ADD WT-SU4 TO WS-SU4
               ADD WT-ANTU TO WS-ANTU
               IF  W-JS9 = 1
                   MOVE WT-SU4 TO EXL-SKIN
                   MOVE WT-ANTU TO EXL-AKIN
               ELSE
                   MOVE WT-SU4 TO P-STU
                   MOVE WT-ANTU TO P-ATU
               END-IF
           END-IF
           MOVE ZERO TO CNT2.
       TPR-040.
           ADD 1 TO CNT2.
           IF  CNT2 = 7
               GO TO TPR-060
           END-IF
           IF  CNT1 = 1
               ADD WT-U1(CNT2) TO WS-U1(CNT2)
               IF  W-JS9 NOT = 1
                   MOVE WT-U1(CNT2) TO P-U(CNT2)
               ELSE
                   MOVE WT-U1(CNT2) TO EXL-KIN(CNT2)
               END-IF
           END-IF
           IF  CNT1 = 2
               ADD WT-U2(CNT2) TO WS-U2(CNT2)
               IF  W-JS9 NOT = 1
                   MOVE WT-U2(CNT2) TO P-U(CNT2)
               ELSE
                   MOVE WT-U2(CNT2) TO EXL-KIN(CNT2)
               END-IF
           END-IF
           IF  CNT1 = 3
               ADD WT-U3(CNT2) TO WS-U3(CNT2)
               IF  W-JS9 NOT = 1
                   MOVE WT-U3(CNT2) TO P-U(CNT2)
               ELSE
                   MOVE WT-U3(CNT2) TO EXL-KIN(CNT2)
               END-IF
           END-IF
           IF  CNT1 = 4
               ADD WT-U4(CNT2) TO WS-U4(CNT2)
               IF  W-JS9 NOT = 1
                   MOVE WT-U4(CNT2) TO P-U(CNT2)
               ELSE
                   MOVE WT-U4(CNT2) TO EXL-KIN(CNT2)
               END-IF
           END-IF
           GO TO TPR-040.
       TPR-060.
           IF  W-JS9 = 1
      *               WRITE EXL-R
      *//////////////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO TPR-020
           END-IF
           IF  CNT1 = 1
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
               IF  LINAGECOUNTER > 62
                   MOVE W-TC TO P-TC2
                   PERFORM MID-RTN THRU MID-EX
               END-IF
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO TPR-020.
       TPR-EX.
           EXIT.
      ***************     担当者別　合計　作表     *********************
       SPR-RTN.
           IF (W-UC NOT = 2) AND (W-KIC = 0)
               MOVE "　　［　ＳＵＢ　ＴＯＴＡＬ　］　" TO W-TMD
           END-IF
           MOVE ZERO TO CNT1 W-TUA.
           IF  W-UAC = 5
               MOVE WS-TUA TO W-TUA
               PERFORM RRK-RTN THRU RRK-EX
           END-IF.
       SPR-020.
           ADD 1 TO CNT1.
           IF  CNT1 = 5
               MOVE ZERO TO W-KIC
               GO TO SPR-EX
           END-IF
           MOVE ZERO TO CNT2.
           IF  W-JS9 = 1
               INITIALIZE EXL-R
               MOVE SPACE TO EXL-NAME
           ELSE
               MOVE SPACE TO W-P2
               MOVE SPACE TO P-TMD
               MOVE ":" TO P-M
           END-IF
           IF  CNT1 NOT = 1
               GO TO SPR-040
           END-IF
           ADD WS-SU1 TO WA-SU1.
           IF  W-JS9 = 1
               MOVE W-TMD TO EXL-NAME
               MOVE WS-SU1 TO EXL-SKIN
           ELSE
               MOVE W-TMD TO P-TMD
               MOVE "(" TO P-X1
               MOVE ")" TO P-X2
               MOVE WS-SU1 TO P-STU
               IF  W-UAC = 5
                   MOVE "<" TO P-RF
                   MOVE W-ORD TO P-RD
                   MOVE "%>" TO P-RR
                   ADD WS-AOTUD TO WA-AOTUD
               END-IF
           END-IF
           GO TO SPR-100.
       SPR-040.
           IF  CNT1 NOT = 2
               GO TO SPR-060
           END-IF
           ADD WS-SU2 TO WA-SU2.
           ADD WS-AOTU TO WA-AOTU.
           IF  W-JS9 = 1
               MOVE WS-SU2 TO EXL-SKIN
               MOVE WS-AOTU TO EXL-AKIN
           ELSE
               MOVE "(" TO P-X1
               MOVE ")" TO P-X2
               MOVE WS-SU2 TO P-STU
               MOVE WS-AOTU TO P-ATU
           END-IF
           GO TO SPR-100.
       SPR-060.
           IF  CNT1 NOT = 3
               GO TO SPR-080
           END-IF
           ADD WS-SU3 TO WA-SU3.
           IF  W-JS9 = 1
               MOVE WS-SU3 TO EXL-SKIN
           ELSE
               MOVE WS-SU3 TO P-STU
               IF  W-UAC = 5
                   MOVE "<" TO P-RF
                   MOVE W-NRD TO P-RD
                   MOVE "%>" TO P-RR
                   ADD WS-ANTUD TO WA-ANTUD
               END-IF
           END-IF
           GO TO SPR-100.
       SPR-080.
           ADD WS-SU4 TO WA-SU4.
           ADD WS-ANTU TO WA-ANTU.
           IF  W-JS9 = 1
               MOVE WS-SU4 TO EXL-SKIN
               MOVE WS-ANTU TO EXL-AKIN
           ELSE
               MOVE WS-SU4 TO P-STU
               MOVE WS-ANTU TO P-ATU
           END-IF.
       SPR-100.
           ADD 1 TO CNT2.
           IF  CNT2 = 7
               GO TO SPR-120
           END-IF
           IF  CNT1 = 1
               ADD WS-U1(CNT2) TO WA-U1(CNT2)
               IF  W-JS9 NOT = 1
                   MOVE WS-U1(CNT2) TO P-U(CNT2)
               ELSE
                   MOVE WS-U1(CNT2) TO EXL-KIN(CNT2)
               END-IF
           END-IF
           IF  CNT1 = 2
               ADD WS-U2(CNT2) TO WA-U2(CNT2)
               IF  W-JS9 NOT = 1
                   MOVE WS-U2(CNT2) TO P-U(CNT2)
               ELSE
                   MOVE WS-U2(CNT2) TO EXL-KIN(CNT2)
               END-IF
           END-IF
           IF  CNT1 = 3
               ADD WS-U3(CNT2) TO WA-U3(CNT2)
               IF  W-JS9 NOT = 1
                   MOVE WS-U3(CNT2) TO P-U(CNT2)
               ELSE
                   MOVE WS-U3(CNT2) TO EXL-KIN(CNT2)
               END-IF
           END-IF
           IF  CNT1 = 4
               ADD WS-U4(CNT2) TO WA-U4(CNT2)
               IF  W-JS9 NOT = 1
                   MOVE WS-U4(CNT2) TO P-U(CNT2)
               ELSE
                   MOVE WS-U4(CNT2) TO EXL-KIN(CNT2)
               END-IF
           END-IF
           GO TO SPR-100.
       SPR-120.
           IF  W-JS9 = 1
      *               WRITE EXL-R
      *//////////////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO SPR-020
           END-IF
           IF  CNT1 = 1
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
               IF  LINAGECOUNTER > 62
                   PERFORM MID-RTN THRU MID-EX
               END-IF
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO SPR-020.
       SPR-EX.
           EXIT.
      ********************     総合計　作表     ************************
       APR-RTN.
           IF  W-UC NOT = 2
               MOVE "　【　ＡＬＬ　ＴＯＴＡＬ　】　　" TO W-TMD
           END-IF
           MOVE ZERO TO CNT1 W-TUA.
           IF  W-UAC = 5
               MOVE WA-TUA TO W-TUA
               PERFORM RRK-RTN THRU RRK-EX
           END-IF.
       APR-020.
           ADD 1 TO CNT1.
           IF  CNT1 = 5
               GO TO APR-EX
           END-IF
           IF  W-JS9 = 1
               INITIALIZE EXL-R
               MOVE SPACE TO EXL-NAME
           ELSE
               MOVE SPACE TO W-P2
               MOVE SPACE TO P-TMD
               MOVE ":" TO P-M
           END-IF
           IF  CNT1 = 1
               IF  W-JS9 = 1
                   MOVE W-TMD TO EXL-NAME
                   MOVE WA-SU1 TO EXL-SKIN
               ELSE
                   MOVE W-TMD TO P-TMD
                   MOVE "(" TO P-X1
                   MOVE ")" TO P-X2
                   MOVE WA-SU1 TO P-STU
                   IF  W-UAC = 5
                       MOVE "<" TO P-RF
                       MOVE W-ORD TO P-RD
                       MOVE "%>" TO P-RR
                   END-IF
               END-IF
           END-IF
           IF  CNT1 = 2
               IF  W-JS9 = 1
                   MOVE WA-SU2 TO EXL-SKIN
                   MOVE WA-AOTU TO EXL-AKIN
               ELSE
                   MOVE "(" TO P-X1
                   MOVE ")" TO P-X2
                   MOVE WA-SU2 TO P-STU
                   MOVE WA-AOTU TO P-ATU
               END-IF
           END-IF
           IF  CNT1 = 3
               IF  W-JS9 = 1
                   MOVE WA-SU3 TO EXL-SKIN
               ELSE
                   MOVE WA-SU3 TO P-STU
                   IF  W-UAC = 5
                       MOVE "<" TO P-RF
                       MOVE W-NRD TO P-RD
                       MOVE "%>" TO P-RR
                   END-IF
               END-IF
           END-IF
           IF  CNT1 = 4
               IF  W-JS9 = 1
                   MOVE WA-SU4 TO EXL-SKIN
                   MOVE WA-ANTU TO EXL-AKIN
               ELSE
                   MOVE WA-SU4 TO P-STU
                   MOVE WA-ANTU TO P-ATU
               END-IF
           END-IF
           MOVE ZERO TO CNT2.
       APR-040.
           ADD 1 TO CNT2.
           IF  CNT2 = 7
               GO TO APR-060
           END-IF
           IF  CNT1 = 1
               IF  W-JS9 = 1
                   MOVE WA-U1(CNT2) TO EXL-KIN(CNT2)
               ELSE
                   MOVE WA-U1(CNT2) TO P-U(CNT2)
               END-IF
           END-IF
           IF  CNT1 = 2
               IF  W-JS9 = 1
                   MOVE WA-U2(CNT2) TO EXL-KIN(CNT2)
               ELSE
                   MOVE WA-U2(CNT2) TO P-U(CNT2)
               END-IF
           END-IF
           IF  CNT1 = 3
               IF  W-JS9 = 1
                   MOVE WA-U3(CNT2) TO EXL-KIN(CNT2)
               ELSE
                   MOVE WA-U3(CNT2) TO P-U(CNT2)
               END-IF
           END-IF
           IF  CNT1 = 4
               IF  W-JS9 = 1
                   MOVE WA-U4(CNT2) TO EXL-KIN(CNT2)
               ELSE
                   MOVE WA-U4(CNT2) TO P-U(CNT2)
               END-IF
           END-IF
           GO TO APR-040.
       APR-060.
           IF  W-JS9 = 1
      *               WRITE EXL-R
      *//////////////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO APR-020
           END-IF
           IF  CNT1 = 1
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
               IF  LINAGECOUNTER > 62
                   PERFORM MID-RTN THRU MID-EX
               END-IF
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO APR-020.
       APR-EX.
           EXIT.
      ******************     合計　集計     ****************************
       TOT-RTN.
           IF  TH-BC NOT = ZERO
               GO TO TOT-020
           END-IF
           IF  CNT1 = 1
               ADD W-OU1(CNT2) TO WH-U1(CNT2) WH-SU1
           END-IF
           IF  CNT1 = 2
               ADD W-OU2(CNT2) TO WH-U2(CNT2) WH-SU2
           END-IF
           IF  CNT1 = 3
               ADD W-NU1(CNT2) TO WH-U3(CNT2) WH-SU3
           END-IF
           IF  CNT1 = 4
               ADD W-NU2(CNT2) TO WH-U4(CNT2) WH-SU4
           END-IF
           GO TO TOT-EX.
       TOT-020.
           IF  TH-BC NOT = 1
               GO TO TOT-040
           END-IF
           IF  CNT1 = 1
               ADD W-OU1(CNT2) TO WK-U1(CNT2) WK-SU1
           END-IF
           IF  CNT1 = 2
               ADD W-OU2(CNT2) TO WK-U2(CNT2) WK-SU2
           END-IF
           IF  CNT1 = 3
               ADD W-NU1(CNT2) TO WK-U3(CNT2) WK-SU3
           END-IF
           IF  CNT1 = 4
               ADD W-NU2(CNT2) TO WK-U4(CNT2) WK-SU4
           END-IF
           GO TO TOT-EX.
       TOT-040.
           IF  CNT1 = 1
               ADD W-OU1(CNT2) TO WZ-U1(CNT2) WZ-SU1
           END-IF
           IF  CNT1 = 2
               ADD W-OU2(CNT2) TO WZ-U2(CNT2) WZ-SU2
           END-IF
           IF  CNT1 = 3
               ADD W-NU1(CNT2) TO WZ-U3(CNT2) WZ-SU3
           END-IF
           IF  CNT1 = 4
               ADD W-NU2(CNT2) TO WZ-U4(CNT2) WZ-SU4
           END-IF.
       TOT-EX.
           EXIT.
      ******************     利益率　計算     **************************
       RRK-RTN.
           MOVE ZERO TO W-RD.
           IF  W-AOTAD = ZERO
               GO TO RRK-040
           END-IF
           IF  W-AOTUD NOT = ZERO
               GO TO RRK-020
           END-IF
           IF  W-AOTAD < ZERO
               MOVE -100 TO W-ORD
           ELSE
               MOVE 100 TO W-ORD
           END-IF
           GO TO RRK-040.
       RRK-020.
           COMPUTE W-ALL = (W-AOTAD / W-AOTUD) * 100.
           IF  W-AOTUD < ZERO
               COMPUTE W-ORD ROUNDED = W-ALL * -1
           ELSE
               COMPUTE W-ORD ROUNDED = W-ALL * 1
           END-IF.
       RRK-040.
           IF  W-ANTAD = ZERO
               GO TO RRK-EX
           END-IF
           IF  W-ANTUD NOT = ZERO
               GO TO RRK-060
           END-IF
           IF  W-ANTAD < ZERO
               MOVE -100 TO W-NRD
           ELSE
               MOVE 100 TO W-NRD
           END-IF
           GO TO RRK-EX.
       RRK-060.
           COMPUTE W-ALL = (W-ANTAD / W-ANTUD) * 100.
           IF  W-ANTUD < ZERO
               COMPUTE W-NRD ROUNDED = W-ALL * -1
           ELSE
               COMPUTE W-NRD ROUNDED = W-ALL * 1
           END-IF.
       RRK-EX.
           EXIT.

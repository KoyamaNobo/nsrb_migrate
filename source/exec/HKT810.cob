       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         HKT810.
      **************************************************************
      *    PROGRAM         :  得意先別　売上･粗利前年対比　問合せ  *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  SCHK81                               *
      *    COMPILE TYPE    :  COBOL                                *
      **************************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0512".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-TCD          PIC  9(004).
           02  CNT.
             03  CNT1         PIC  9(002).
             03  CNT2         PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-NG.
             03  W-N          PIC  9(004).
             03  W-ND    REDEFINES W-N.
               04  W-N1       PIC  9(002).
               04  W-N2       PIC  9(002).
             03  W-G          PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-YMD.
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
           02  W-HD.
             03  W-UOHF       PIC S9(010).
             03  W-UOHR       PIC S9(010).
             03  W-UNHF       PIC S9(010).
             03  W-UNHR       PIC S9(010).
             03  W-AOHF       PIC S9(009).
             03  W-AOHR       PIC S9(009).
             03  W-ANHF       PIC S9(009).
             03  W-ANHR       PIC S9(009).
       01  W-SENM             PIC  N(007).
       01  ERR-STAT           PIC  X(002).
           COPY  LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
      *FD  THY-M
       01  THY-M_HKT810.
           02  THY-M_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  THY-M_LNAME    PIC  X(012) VALUE "THY-M_HKT810".
           02  F              PIC  X(001).
           02  THY-M_KEY1     PIC  X(100) VALUE SPACE.
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
           02  F              PIC  X(025).
           02  TH-SEN         PIC  9(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-TCD   PIC  9(004).
       01  C-DSP.
           02  D-SENM  PIC  N(007).
           02  D-NAME  PIC  N(026).
           02  D-TNC   PIC  9(002).
           02  D-NG.
             03  FILLER.
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
             03  FILLER.
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
             03  FILLER.
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
             03  FILLER.
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
             03  FILLER.
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
             03  FILLER.
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
             03  FILLER.
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
             03  FILLER.
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
             03  FILLER.
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
             03  FILLER.
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
             03  FILLER.
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
             03  FILLER.
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
               04  FILLER  PIC  9(002).
               04  FILLER  PIC Z9 .
           02  D-KIN.
             03  FILLER.
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
             03  FILLER.
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
             03  FILLER.
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
             03  FILLER.
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
             03  FILLER.
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
             03  FILLER.
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
             03  FILLER.
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
             03  FILLER.
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
             03  FILLER.
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
             03  FILLER.
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
             03  FILLER.
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
             03  FILLER.
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
             03  FILLER.
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
             03  FILLER.
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
             03  FILLER.
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
               04  FILLER  PIC --,---,---,--- .
               04  FILLER  PIC ----,---,--- .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                   "***  ﾄｸｲｻｷ ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                   "***  ﾃﾞｰﾀ ﾅｼ  *** ".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY  LIBSCR.
       PROCEDURE           DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "4" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-TCD" "9" "4" "11" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "944" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SENM" "N" "2" "6" "14" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SENM" BY REFERENCE W-SENM "14" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "4" "25" "52" "D-SENM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNC" "9" "5" "78" "2" "D-NAME" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNC" BY REFERENCE T-TNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NG" " " "0" "0" "96" "D-TNC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NG" " " "8" "0" "8" " " "D-NG" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-NG" "9" "8" "6" "2" " " "01D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "0101D-NG" BY REFERENCE W-Y12(1) "2" "1" "01" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-NG" "Z9" "8" "9" "2" "0101D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-NG" BY REFERENCE W-M1(1) "2" "1" "01" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0301D-NG" "9" "8" "41" "2" "0201D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0301D-NG" BY REFERENCE W-Y22(1) "2" "1" "01" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0401D-NG" "Z9" "8" "44" "2" "0301D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0401D-NG" BY REFERENCE W-M2(1) "2" "1" "01" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NG" " " "9" "0" "8" "01D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-NG" "9" "9" "6" "2" " " "02D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "0102D-NG" BY REFERENCE W-Y12(1) "2" "1" "02" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-NG" "Z9" "9" "9" "2" "0102D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0202D-NG" BY REFERENCE W-M1(1) "2" "1" "02" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-NG" "9" "9" "41" "2" "0202D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0302D-NG" BY REFERENCE W-Y22(1) "2" "1" "02" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402D-NG" "Z9" "9" "44" "2" "0302D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0402D-NG" BY REFERENCE W-M2(1) "2" "1" "02" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-NG" " " "10" "0" "8" "02D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103D-NG" "9" "10" "6" "2" " " "03D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "0103D-NG" BY REFERENCE W-Y12(1) "2" "1" "03" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0203D-NG" "Z9" "10" "9" "2" "0103D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0203D-NG" BY REFERENCE W-M1(1) "2" "1" "03" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0303D-NG" "9" "10" "41" "2" "0203D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0303D-NG" BY REFERENCE W-Y22(1) "2" "1" "03" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0403D-NG" "Z9" "10" "44" "2" "0303D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0403D-NG" BY REFERENCE W-M2(1) "2" "1" "03" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-NG" " " "11" "0" "8" "03D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0104D-NG" "9" "11" "6" "2" " " "04D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "0104D-NG" BY REFERENCE W-Y12(1) "2" "1" "04" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0204D-NG" "Z9" "11" "9" "2" "0104D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0204D-NG" BY REFERENCE W-M1(1) "2" "1" "04" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0304D-NG" "9" "11" "41" "2" "0204D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0304D-NG" BY REFERENCE W-Y22(1) "2" "1" "04" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0404D-NG" "Z9" "11" "44" "2" "0304D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0404D-NG" BY REFERENCE W-M2(1) "2" "1" "04" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-NG" " " "12" "0" "8" "04D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0105D-NG" "9" "12" "6" "2" " " "05D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "0105D-NG" BY REFERENCE W-Y12(1) "2" "1" "05" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0205D-NG" "Z9" "12" "9" "2" "0105D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0205D-NG" BY REFERENCE W-M1(1) "2" "1" "05" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0305D-NG" "9" "12" "41" "2" "0205D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0305D-NG" BY REFERENCE W-Y22(1) "2" "1" "05" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0405D-NG" "Z9" "12" "44" "2" "0305D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0405D-NG" BY REFERENCE W-M2(1) "2" "1" "05" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-NG" " " "13" "0" "8" "05D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0106D-NG" "9" "13" "6" "2" " " "06D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "0106D-NG" BY REFERENCE W-Y12(1) "2" "1" "06" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0206D-NG" "Z9" "13" "9" "2" "0106D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0206D-NG" BY REFERENCE W-M1(1) "2" "1" "06" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0306D-NG" "9" "13" "41" "2" "0206D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0306D-NG" BY REFERENCE W-Y22(1) "2" "1" "06" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0406D-NG" "Z9" "13" "44" "2" "0306D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0406D-NG" BY REFERENCE W-M2(1) "2" "1" "06" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-NG" " " "15" "0" "8" "06D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0107D-NG" "9" "15" "6" "2" " " "07D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "0107D-NG" BY REFERENCE W-Y12(1) "2" "1" "07" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0207D-NG" "Z9" "15" "9" "2" "0107D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0207D-NG" BY REFERENCE W-M1(1) "2" "1" "07" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0307D-NG" "9" "15" "41" "2" "0207D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0307D-NG" BY REFERENCE W-Y22(1) "2" "1" "07" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0407D-NG" "Z9" "15" "44" "2" "0307D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0407D-NG" BY REFERENCE W-M2(1) "2" "1" "07" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-NG" " " "16" "0" "8" "07D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0108D-NG" "9" "16" "6" "2" " " "08D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "0108D-NG" BY REFERENCE W-Y12(1) "2" "1" "08" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0208D-NG" "Z9" "16" "9" "2" "0108D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0208D-NG" BY REFERENCE W-M1(1) "2" "1" "08" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0308D-NG" "9" "16" "41" "2" "0208D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0308D-NG" BY REFERENCE W-Y22(1) "2" "1" "08" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0408D-NG" "Z9" "16" "44" "2" "0308D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0408D-NG" BY REFERENCE W-M2(1) "2" "1" "08" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09D-NG" " " "17" "0" "8" "08D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0109D-NG" "9" "17" "6" "2" " " "09D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "0109D-NG" BY REFERENCE W-Y12(1) "2" "1" "09" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0209D-NG" "Z9" "17" "9" "2" "0109D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0209D-NG" BY REFERENCE W-M1(1) "2" "1" "09" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0309D-NG" "9" "17" "41" "2" "0209D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0309D-NG" BY REFERENCE W-Y22(1) "2" "1" "09" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0409D-NG" "Z9" "17" "44" "2" "0309D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0409D-NG" BY REFERENCE W-M2(1) "2" "1" "09" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10D-NG" " " "18" "0" "8" "09D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0110D-NG" "9" "18" "6" "2" " " "10D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "0110D-NG" BY REFERENCE W-Y12(1) "2" "1" "10" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0210D-NG" "Z9" "18" "9" "2" "0110D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0210D-NG" BY REFERENCE W-M1(1) "2" "1" "10" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0310D-NG" "9" "18" "41" "2" "0210D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0310D-NG" BY REFERENCE W-Y22(1) "2" "1" "10" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0410D-NG" "Z9" "18" "44" "2" "0310D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0410D-NG" BY REFERENCE W-M2(1) "2" "1" "10" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "11D-NG" " " "19" "0" "8" "10D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0111D-NG" "9" "19" "6" "2" " " "11D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "0111D-NG" BY REFERENCE W-Y12(1) "2" "1" "11" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0211D-NG" "Z9" "19" "9" "2" "0111D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0211D-NG" BY REFERENCE W-M1(1) "2" "1" "11" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0311D-NG" "9" "19" "41" "2" "0211D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0311D-NG" BY REFERENCE W-Y22(1) "2" "1" "11" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0411D-NG" "Z9" "19" "44" "2" "0311D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0411D-NG" BY REFERENCE W-M2(1) "2" "1" "11" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "12D-NG" " " "20" "0" "8" "11D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0112D-NG" "9" "20" "6" "2" " " "12D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "0112D-NG" BY REFERENCE W-Y12(1) "2" "1" "12" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0212D-NG" "Z9" "20" "9" "2" "0112D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0212D-NG" BY REFERENCE W-M1(1) "2" "1" "12" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0312D-NG" "9" "20" "41" "2" "0212D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0312D-NG" BY REFERENCE W-Y22(1) "2" "1" "12" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0412D-NG" "Z9" "20" "44" "2" "0312D-NG" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0412D-NG" BY REFERENCE W-M2(1) "2" "1" "12" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN" " " "0" "0" "780" "D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-KIN" " " "8" "0" "52" " " "D-KIN" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-KIN" "--,---,---,---" "8" "12" "14" " " "01D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0101D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "01" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-KIN" "----,---,---" "8" "27" "12" "0101D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "01" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0301D-KIN" "--,---,---,---" "8" "47" "14" "0201D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0301D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "13" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0401D-KIN" "----,---,---" "8" "64" "12" "0301D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0401D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "13" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-KIN" " " "9" "0" "52" "01D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-KIN" "--,---,---,---" "9" "12" "14" " " "02D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0102D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "02" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-KIN" "----,---,---" "9" "27" "12" "0102D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0202D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "02" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-KIN" "--,---,---,---" "9" "47" "14" "0202D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0302D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "14" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402D-KIN" "----,---,---" "9" "64" "12" "0302D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0402D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "14" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-KIN" " " "10" "0" "52" "02D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103D-KIN" "--,---,---,---" "10" "12" "14" " " "03D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0103D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "03" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0203D-KIN" "----,---,---" "10" "27" "12" "0103D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0203D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "03" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0303D-KIN" "--,---,---,---" "10" "47" "14" "0203D-KIN"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0303D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "15" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0403D-KIN" "----,---,---" "10" "64" "12" "0303D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0403D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "15" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-KIN" " " "11" "0" "52" "03D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0104D-KIN" "--,---,---,---" "11" "12" "14" " " "04D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0104D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "04" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0204D-KIN" "----,---,---" "11" "27" "12" "0104D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0204D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "04" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0304D-KIN" "--,---,---,---" "11" "47" "14" "0204D-KIN"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0304D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "16" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0404D-KIN" "----,---,---" "11" "64" "12" "0304D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0404D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "16" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-KIN" " " "12" "0" "52" "04D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0105D-KIN" "--,---,---,---" "12" "12" "14" " " "05D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0105D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "05" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0205D-KIN" "----,---,---" "12" "27" "12" "0105D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0205D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "05" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0305D-KIN" "--,---,---,---" "12" "47" "14" "0205D-KIN"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0305D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "17" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0405D-KIN" "----,---,---" "12" "64" "12" "0305D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0405D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "17" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-KIN" " " "13" "0" "52" "05D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0106D-KIN" "--,---,---,---" "13" "12" "14" " " "06D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0106D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "06" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0206D-KIN" "----,---,---" "13" "27" "12" "0106D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0206D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "06" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0306D-KIN" "--,---,---,---" "13" "47" "14" "0206D-KIN"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0306D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "18" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0406D-KIN" "----,---,---" "13" "64" "12" "0306D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0406D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "18" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-KIN" " " "14" "0" "52" "06D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0107D-KIN" "--,---,---,---" "14" "12" "14" " " "07D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0107D-KIN" BY REFERENCE W-UOHF "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0207D-KIN" "----,---,---" "14" "27" "12" "0107D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0207D-KIN" BY REFERENCE W-AOHF "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0307D-KIN" "--,---,---,---" "14" "47" "14" "0207D-KIN"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0307D-KIN" BY REFERENCE W-UNHF "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0407D-KIN" "----,---,---" "14" "64" "12" "0307D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0407D-KIN" BY REFERENCE W-ANHF "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-KIN" " " "15" "0" "52" "07D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0108D-KIN" "--,---,---,---" "15" "12" "14" " " "08D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0108D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "07" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0208D-KIN" "----,---,---" "15" "27" "12" "0108D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0208D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "07" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0308D-KIN" "--,---,---,---" "15" "47" "14" "0208D-KIN"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0308D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "19" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0408D-KIN" "----,---,---" "15" "64" "12" "0308D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0408D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "19" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09D-KIN" " " "16" "0" "52" "08D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0109D-KIN" "--,---,---,---" "16" "12" "14" " " "09D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0109D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "08" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0209D-KIN" "----,---,---" "16" "27" "12" "0109D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0209D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "08" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0309D-KIN" "--,---,---,---" "16" "47" "14" "0209D-KIN"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0309D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "20" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0409D-KIN" "----,---,---" "16" "64" "12" "0309D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0409D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "20" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10D-KIN" " " "17" "0" "52" "09D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0110D-KIN" "--,---,---,---" "17" "12" "14" " " "10D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0110D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "09" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0210D-KIN" "----,---,---" "17" "27" "12" "0110D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0210D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "09" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0310D-KIN" "--,---,---,---" "17" "47" "14" "0210D-KIN"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0310D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "21" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0410D-KIN" "----,---,---" "17" "64" "12" "0310D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0410D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "21" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "11D-KIN" " " "18" "0" "52" "10D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0111D-KIN" "--,---,---,---" "18" "12" "14" " " "11D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0111D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "10" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0211D-KIN" "----,---,---" "18" "27" "12" "0111D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0211D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "10" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0311D-KIN" "--,---,---,---" "18" "47" "14" "0211D-KIN"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0311D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "22" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0411D-KIN" "----,---,---" "18" "64" "12" "0311D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0411D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "22" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "12D-KIN" " " "19" "0" "52" "11D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0112D-KIN" "--,---,---,---" "19" "12" "14" " " "12D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0112D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "11" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0212D-KIN" "----,---,---" "19" "27" "12" "0112D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0212D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "11" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0312D-KIN" "--,---,---,---" "19" "47" "14" "0212D-KIN"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0312D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "23" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0412D-KIN" "----,---,---" "19" "64" "12" "0312D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0412D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "23" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "13D-KIN" " " "20" "0" "52" "12D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0113D-KIN" "--,---,---,---" "20" "12" "14" " " "13D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0113D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "12" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0213D-KIN" "----,---,---" "20" "27" "12" "0113D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0213D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "12" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0313D-KIN" "--,---,---,---" "20" "47" "14" "0213D-KIN"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0313D-KIN" BY REFERENCE TH-UKD(1) "9" "1" "24" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0413D-KIN" "----,---,---" "20" "64" "12" "0313D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0413D-KIN" BY REFERENCE TH-AKD(1) "9" "1" "24" 9
            RETURNING RESU.
       CALL "SD_Init" USING 
            "14D-KIN" " " "21" "0" "52" "13D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0114D-KIN" "--,---,---,---" "21" "12" "14" " " "14D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0114D-KIN" BY REFERENCE W-UOHR "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0214D-KIN" "----,---,---" "21" "27" "12" "0114D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0214D-KIN" BY REFERENCE W-AOHR "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0314D-KIN" "--,---,---,---" "21" "47" "14" "0214D-KIN"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0314D-KIN" BY REFERENCE W-UNHR "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0414D-KIN" "----,---,---" "21" "64" "12" "0314D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0414D-KIN" BY REFERENCE W-ANHR "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "15D-KIN" " " "22" "6" "52" "14D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0115D-KIN" "--,---,---,---" "22" "12" "14" " " "15D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0115D-KIN" BY REFERENCE TH-AOTU "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0215D-KIN" "----,---,---" "22" "27" "12" "0115D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0215D-KIN" BY REFERENCE TH-AOTA "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0315D-KIN" "--,---,---,---" "22" "47" "14" "0215D-KIN"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0315D-KIN" BY REFERENCE TH-ANTU "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0415D-KIN" "----,---,---" "22" "64" "12" "0315D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0415D-KIN" BY REFERENCE TH-ANTA "10" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "96" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "96" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
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
       M-05.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE ZERO TO W-NG.
           MOVE D-NHNG TO W-NGS.
           IF  W-N2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-N
           END-IF
           IF  W-N2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-N
           END-IF
           SUBTRACT 1 FROM W-G.
           IF  W-G = ZERO
               MOVE 12 TO W-G
               SUBTRACT 1 FROM W-N
           END-IF
           IF  W-G < 5
               SUBTRACT 1 FROM W-N
           END-IF
           MOVE 4 TO W-G.
           MOVE ZERO TO CNT1.
           MOVE 1 TO CNT2.
       M-10.
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
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0512ID.
           MOVE WK0512ID TO THY-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" THY-M_PNAME1 " " BY REFERENCE THY-M_IDLST "0".
      *           READ THY-M AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" THY-M_PNAME1 BY REFERENCE TH-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  TH-SEN = 0
               MOVE "【全　　体】　" TO W-SENM
           ELSE
               IF  TH-SEN = 1
                   MOVE "【一般ワーク】" TO W-SENM
               ELSE
                   IF  TH-SEN = 2
                       MOVE "【教　　育】　" TO W-SENM
                   ELSE
                       IF  TH-SEN = 3
                           MOVE "【ヴィヴェン】" TO W-SENM
                       ELSE
                           MOVE "　　　　　　　" TO W-SENM
                       END-IF
                   END-IF
               END-IF
           END-IF.
       M-15.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHK81" RETURNING RESU.
           CALL "SD_Output" USING "D-SENM" D-SENM "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE 0 TO CHK.
           IF  ESTAT = ADV
               MOVE 1 TO CHK
               GO TO M-30
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE SPACE TO T-R
               MOVE ZERO TO T-TNC
               MOVE ZERO TO TH-R
               CALL "SD_Output" USING
                "D-NAME" D-NAME "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-TNC" D-TNC "p" RETURNING RESU
               GO TO M-35
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNC" D-TNC "p" RETURNING RESU.
       M-25.
           CALL "DB_F_Close" USING
            BY REFERENCE THY-M_IDLST THY-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" THY-M_PNAME1 " " BY REFERENCE THY-M_IDLST "0".
       M-30.
      *           READ THY-M AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" THY-M_PNAME1 BY REFERENCE TH-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           IF  CHK = 0
               IF  W-TCD NOT = TH-KEY
                   GO TO M-30
               ELSE
                   GO TO M-35
               END-IF
           END-IF
           IF  CHK = 1
               IF  ZERO = TH-UKD(01) AND TH-UKD(02) AND TH-UKD(03) AND
                         TH-UKD(04) AND TH-UKD(05) AND TH-UKD(06) AND
                         TH-UKD(07) AND TH-UKD(08) AND TH-UKD(09) AND
                         TH-UKD(10) AND TH-UKD(11) AND TH-UKD(12) AND
                         TH-UKD(13) AND TH-UKD(14) AND TH-UKD(15) AND
                         TH-UKD(16) AND TH-UKD(17) AND TH-UKD(18) AND
                         TH-UKD(19) AND TH-UKD(20) AND TH-UKD(21) AND
                         TH-UKD(22) AND TH-UKD(23) AND TH-UKD(24)
                   GO TO M-30
               END-IF
           END-IF
           MOVE TH-KEY TO W-TCD.
           MOVE TH-KEY TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO T-TNC
               MOVE SPACE TO T-NAME
               MOVE "＊　得意先マスター　なし　＊" TO T-NAME
           END-IF
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNC" D-TNC "p" RETURNING RESU.
       M-35.
           COMPUTE W-UOHF = TH-UKD(01) + TH-UKD(02) + TH-UKD(03)
                          + TH-UKD(04) + TH-UKD(05) + TH-UKD(06).
           COMPUTE W-UOHR = TH-UKD(07) + TH-UKD(08) + TH-UKD(09)
                          + TH-UKD(10) + TH-UKD(11) + TH-UKD(12).
           COMPUTE W-UNHF = TH-UKD(13) + TH-UKD(14) + TH-UKD(15)
                          + TH-UKD(16) + TH-UKD(17) + TH-UKD(18).
           COMPUTE W-UNHR = TH-UKD(19) + TH-UKD(20) + TH-UKD(21)
                          + TH-UKD(22) + TH-UKD(23) + TH-UKD(24).
           COMPUTE W-AOHF = TH-AKD(01) + TH-AKD(02) + TH-AKD(03)
                          + TH-AKD(04) + TH-AKD(05) + TH-AKD(06).
           COMPUTE W-AOHR = TH-AKD(07) + TH-AKD(08) + TH-AKD(09)
                          + TH-AKD(10) + TH-AKD(11) + TH-AKD(12).
           COMPUTE W-ANHF = TH-AKD(13) + TH-AKD(14) + TH-AKD(15)
                          + TH-AKD(16) + TH-AKD(17) + TH-AKD(18).
           COMPUTE W-ANHR = TH-AKD(19) + TH-AKD(20) + TH-AKD(21)
                          + TH-AKD(22) + TH-AKD(23) + TH-AKD(24).
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           GO TO M-20.
       M-40.
           IF  CHK = 0
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE THY-M_IDLST THY-M_PNAME1
               CALL "DB_F_Open" USING
                "INPUT" THY-M_PNAME1 " " BY REFERENCE THY-M_IDLST "0"
               GO TO M-20
           END-IF
           GO TO M-25.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE THY-M_IDLST THY-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

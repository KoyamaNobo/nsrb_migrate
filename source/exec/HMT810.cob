       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         HMT810.
      **************************************************************
      *    PROGRAM         :  品名別　売上前年対比　問合せ         *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  SCHT81                               *
      *    COMPILE TYPE    :  COBOL                                *
      *    JS-SIGN         :  色あり(6ｹﾀ)=0 , 色なし(4ｹﾀ)=1        *
      **************************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0512".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-HCD          PIC  9(006).
           02  W-HCDD  REDEFINES  W-HCD.
             03  W-HCD1       PIC  9(004).
             03  W-HCD2       PIC  9(002).
           02  CNT.
             03  CNT1         PIC  9(002).
             03  CNT2         PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-INV          PIC  9(001).
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
           02  W-TD.
             03  W-ZFD.
               04  W-ZFSU     PIC S9(008).
               04  W-ZFKN     PIC S9(010).
             03  W-ZRD.
               04  W-ZRSU     PIC S9(008).
               04  W-ZRKN     PIC S9(010).
             03  W-ZAD.
               04  W-ZASU     PIC S9(008).
               04  W-ZAKN     PIC S9(010).
             03  W-KFD.
               04  W-KFSU     PIC S9(008).
               04  W-KFKN     PIC S9(010).
             03  W-KRD.
               04  W-KRSU     PIC S9(008).
               04  W-KRKN     PIC S9(010).
             03  W-KAD.
               04  W-KASU     PIC S9(008).
               04  W-KAKN     PIC S9(010).
           02  W-NAME         PIC  N(024).
           02  W-NAMED REDEFINES  W-NAME.
             03  W-NAD   OCCURS  24.
               04  W-NA       PIC  N(001).
           02  W-HNAM         PIC  N(024).
           02  W-HNAA  REDEFINES  W-HNAM.
             03  W-HNAD  OCCURS  24.
               04  W-HNA      PIC  N(001).
       01  ERR-STAT           PIC  X(002).
           COPY  LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
      *FD  HY-F
       01  HY-F_HMT810.
           02  HY-F_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HY-F_LNAME     PIC  X(011) VALUE "HY-F_HMT810".
           02  F              PIC  X(001).
           02  HY-F_KEY1      PIC  X(100) VALUE SPACE.
           02  HY-F_SORT      PIC  X(100) VALUE SPACE.
           02  HY-F_IDLST     PIC  X(100) VALUE SPACE.
           02  HY-F_RES       USAGE  POINTER.
       01  HY-R.
           02  HY-HCD         PIC  9(006).
           02  HY-HCDD  REDEFINES  HY-HCD.
             03  HY-HCD1      PIC  9(004).
             03  HY-HCD2      PIC  9(002).
           02  HY-ZAD.
             03  HY-ZD    OCCURS  12.
               04  HY-ZSU     PIC S9(007).
               04  HY-ZKN     PIC S9(010).
           02  HY-KAD.
             03  HY-KD    OCCURS  12.
               04  HY-KSU     PIC S9(007).
               04  HY-KKN     PIC S9(010).
           02  HY-BC.
             03  HY-BC1       PIC  9(002).
             03  HY-BC2       PIC  9(002).
             03  HY-BC3       PIC  9(002).
           02  HY-BMC         PIC  9(002).
           02  HY-BMNO        PIC  9(001).
           02  F              PIC  X(089).
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
       01  C-ACP.
           02  A-HCD   PIC  9(006).
           02  A-HCD1  PIC  9(004).
       01  C-DSP.
           02  D-NAME  PIC  N(024).
           02  D-NG.
             03  FILLER.
               04  02D-NG  PIC  9(002).
               04  03D-NG  PIC Z9 .
               04  04D-NG  PIC  9(002).
               04  05D-NG  PIC Z9 .
             03  FILLER.
               04  07D-NG  PIC  9(002).
               04  08D-NG  PIC Z9 .
               04  09D-NG  PIC  9(002).
               04  10D-NG  PIC Z9 .
             03  FILLER.
               04  12D-NG  PIC  9(002).
               04  13D-NG  PIC Z9 .
               04  14D-NG  PIC  9(002).
               04  15D-NG  PIC Z9 .
             03  FILLER.
               04  17D-NG  PIC  9(002).
               04  18D-NG  PIC Z9 .
               04  19D-NG  PIC  9(002).
               04  20D-NG  PIC Z9 .
             03  FILLER.
               04  22D-NG  PIC  9(002).
               04  23D-NG  PIC Z9 .
               04  24D-NG  PIC  9(002).
               04  25D-NG  PIC Z9 .
             03  FILLER.
               04  27D-NG  PIC  9(002).
               04  28D-NG  PIC Z9 .
               04  29D-NG  PIC  9(002).
               04  30D-NG  PIC Z9 .
             03  FILLER.
               04  32D-NG  PIC  9(002).
               04  33D-NG  PIC Z9 .
               04  34D-NG  PIC  9(002).
               04  35D-NG  PIC Z9 .
             03  FILLER.
               04  37D-NG  PIC  9(002).
               04  38D-NG  PIC Z9 .
               04  39D-NG  PIC  9(002).
               04  40D-NG  PIC Z9 .
             03  FILLER.
               04  42D-NG  PIC  9(002).
               04  43D-NG  PIC Z9 .
               04  44D-NG  PIC  9(002).
               04  45D-NG  PIC Z9 .
             03  FILLER.
               04  47D-NG  PIC  9(002).
               04  48D-NG  PIC Z9 .
               04  49D-NG  PIC  9(002).
               04  50D-NG  PIC Z9 .
             03  FILLER.
               04  52D-NG  PIC  9(002).
               04  53D-NG  PIC Z9 .
               04  54D-NG  PIC  9(002).
               04  55D-NG  PIC Z9 .
             03  FILLER.
               04  57D-NG  PIC  9(002).
               04  58D-NG  PIC Z9 .
               04  59D-NG  PIC  9(002).
               04  60D-NG  PIC Z9 .
           02  D-MD.
             03  FILLER.
               04  02D-MD  PIC ---,---,--- .
               04  03D-MD  PIC --,---,---,--- .
               04  04D-MD  PIC ---,---,--- .
               04  05D-MD  PIC --,---,---,--- .
             03  FILLER.
               04  07D-MD  PIC ---,---,--- .
               04  08D-MD  PIC --,---,---,--- .
               04  09D-MD  PIC ---,---,--- .
               04  10D-MD  PIC --,---,---,--- .
             03  FILLER.
               04  12D-MD  PIC ---,---,--- .
               04  13D-MD  PIC --,---,---,--- .
               04  14D-MD  PIC ---,---,--- .
               04  15D-MD  PIC --,---,---,--- .
             03  FILLER.
               04  17D-MD  PIC ---,---,--- .
               04  18D-MD  PIC --,---,---,--- .
               04  19D-MD  PIC ---,---,--- .
               04  20D-MD  PIC --,---,---,--- .
             03  FILLER.
               04  22D-MD  PIC ---,---,--- .
               04  23D-MD  PIC --,---,---,--- .
               04  24D-MD  PIC ---,---,--- .
               04  25D-MD  PIC --,---,---,--- .
             03  FILLER.
               04  27D-MD  PIC ---,---,--- .
               04  28D-MD  PIC --,---,---,--- .
               04  29D-MD  PIC ---,---,--- .
               04  30D-MD  PIC --,---,---,--- .
             03  FILLER.
               04  32D-MD  PIC ---,---,--- .
               04  33D-MD  PIC --,---,---,--- .
               04  34D-MD  PIC ---,---,--- .
               04  35D-MD  PIC --,---,---,--- .
             03  FILLER.
               04  37D-MD  PIC ---,---,--- .
               04  38D-MD  PIC --,---,---,--- .
               04  39D-MD  PIC ---,---,--- .
               04  40D-MD  PIC --,---,---,--- .
             03  FILLER.
               04  42D-MD  PIC ---,---,--- .
               04  43D-MD  PIC --,---,---,--- .
               04  44D-MD  PIC ---,---,--- .
               04  45D-MD  PIC --,---,---,--- .
             03  FILLER.
               04  47D-MD  PIC ---,---,--- .
               04  48D-MD  PIC --,---,---,--- .
               04  49D-MD  PIC ---,---,--- .
               04  50D-MD  PIC --,---,---,--- .
             03  FILLER.
               04  52D-MD  PIC ---,---,--- .
               04  53D-MD  PIC --,---,---,--- .
               04  54D-MD  PIC ---,---,--- .
               04  55D-MD  PIC --,---,---,--- .
             03  FILLER.
               04  57D-MD  PIC ---,---,--- .
               04  58D-MD  PIC --,---,---,--- .
               04  59D-MD  PIC ---,---,--- .
               04  60D-MD  PIC --,---,---,--- .
             03  FILLER.
               04  62D-MD  PIC ---,---,--- .
               04  63D-MD  PIC --,---,---,--- .
               04  64D-MD  PIC ---,---,--- .
               04  65D-MD  PIC --,---,---,--- .
             03  FILLER.
               04  67D-MD  PIC ---,---,--- .
               04  68D-MD  PIC --,---,---,--- .
               04  69D-MD  PIC ---,---,--- .
               04  70D-MD  PIC --,---,---,--- .
             03  FILLER.
               04  72D-MD  PIC ---,---,--- .
               04  73D-MD  PIC --,---,---,--- .
               04  74D-MD  PIC ---,---,--- .
               04  75D-MD  PIC --,---,---,--- .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                   "***  ﾋﾝﾒｲ ﾅｼ  ***".
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
           "C-ACP" " " "0" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-HCD" "9" "4" "11" "6" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-HCD1" "9" "4" "11" "4" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-HCD1" BY REFERENCE W-HCD1 "4" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "894" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-NAME" "N" "4" "24" "48" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING
           "D-NAME" BY REFERENCE W-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-NG" " " "0" "0" "96" "D-TNC" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-NG" " " "8" "0" "8" " " "D-NG" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-NG" "9" "8" "6" "2" " " "01D-NG" RETURNING RESU.
       CALL "SD_From" USING
           "02D-NG" BY REFERENCE W-Y12(1) "2" "1" "01" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "03D-NG" "Z9" "8" "9" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "03D-NG" BY REFERENCE W-M1(1) "2" "1" "01" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "04D-NG" "9" "8" "40" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "04D-NG" BY REFERENCE W-Y22(1) "2" "1" "01" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "05D-NG" "Z9" "8" "43" "2" "04D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "05D-NG" BY REFERENCE W-M2(1) "2" "1" "01" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "06D-NG" " " "9" "0" "8" "01D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07D-NG" "9" "9" "6" "2" " " "06D-NG" RETURNING RESU.
       CALL "SD_From" USING
           "07D-NG" BY REFERENCE W-Y12(1) "2" "1" "02" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "08D-NG" "Z9" "9" "9" "2" "07D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "08D-NG" BY REFERENCE W-M1(1) "2" "1" "02" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "09D-NG" "9" "9" "40" "2" "08D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "09D-NG" BY REFERENCE W-Y22(1) "2" "1" "02" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "10D-NG" "Z9" "9" "43" "2" "09D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "10D-NG" BY REFERENCE W-M2(1) "2" "1" "02" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "11D-NG" " " "10" "0" "8" "06D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING
           "12D-NG" "9" "10" "6" "2" " " "11D-NG" RETURNING RESU.
       CALL "SD_From" USING
           "12D-NG" BY REFERENCE W-Y12(1) "2" "1" "03" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "13D-NG" "Z9" "10" "9" "2" "12D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "13D-NG" BY REFERENCE W-M1(1) "2" "1" "03" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "14D-NG" "9" "10" "40" "2" "13D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "14D-NG" BY REFERENCE W-Y22(1) "2" "1" "03" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "15D-NG" "Z9" "10" "43" "2" "14D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "15D-NG" BY REFERENCE W-M2(1) "2" "1" "03" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "16D-NG" " " "11" "0" "8" "11D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING
           "17D-NG" "9" "11" "6" "2" " " "16D-NG" RETURNING RESU.
       CALL "SD_From" USING
           "17D-NG" BY REFERENCE W-Y12(1) "2" "1" "04" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "18D-NG" "Z9" "11" "9" "2" "17D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "18D-NG" BY REFERENCE W-M1(1) "2" "1" "04" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "19D-NG" "9" "11" "40" "2" "18D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "19D-NG" BY REFERENCE W-Y22(1) "2" "1" "04" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "20D-NG" "Z9" "11" "43" "2" "19D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "20D-NG" BY REFERENCE W-M2(1) "2" "1" "04" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "21D-NG" " " "12" "0" "8" "16D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING
           "22D-NG" "9" "12" "6" "2" " " "21D-NG" RETURNING RESU.
       CALL "SD_From" USING
           "22D-NG" BY REFERENCE W-Y12(1) "2" "1" "05" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "23D-NG" "Z9" "12" "9" "2" "22D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "23D-NG" BY REFERENCE W-M1(1) "2" "1" "05" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "24D-NG" "9" "12" "40" "2" "23D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "24D-NG" BY REFERENCE W-Y22(1) "2" "1" "05" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "25D-NG" "Z9" "12" "43" "2" "24D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "25D-NG" BY REFERENCE W-M2(1) "2" "1" "05" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "26D-NG" " " "13" "0" "8" "21D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING
           "27D-NG" "9" "13" "6" "2" " " "26D-NG" RETURNING RESU.
       CALL "SD_From" USING
           "27D-NG" BY REFERENCE W-Y12(1) "2" "1" "06" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "28D-NG" "Z9" "13" "9" "2" "27D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "28D-NG" BY REFERENCE W-M1(1) "2" "1" "06" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "29D-NG" "9" "13" "40" "2" "28D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "29D-NG" BY REFERENCE W-Y22(1) "2" "1" "06" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "30D-NG" "Z9" "13" "43" "2" "29D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "30D-NG" BY REFERENCE W-M2(1) "2" "1" "06" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "31D-NG" " " "15" "0" "8" "26D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING
           "32D-NG" "9" "15" "6" "2" " " "31D-NG" RETURNING RESU.
       CALL "SD_From" USING
           "32D-NG" BY REFERENCE W-Y12(1) "2" "1" "07" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "33D-NG" "Z9" "15" "9" "2" "32D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "33D-NG" BY REFERENCE W-M1(1) "2" "1" "07" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "34D-NG" "9" "15" "40" "2" "33D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "34D-NG" BY REFERENCE W-Y22(1) "2" "1" "07" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "35D-NG" "Z9" "15" "43" "2" "34D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "35D-NG" BY REFERENCE W-M2(1) "2" "1" "07" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "36D-NG" " " "16" "0" "8" "31D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING
           "37D-NG" "9" "16" "6" "2" " " "36D-NG" RETURNING RESU.
       CALL "SD_From" USING
           "37D-NG" BY REFERENCE W-Y12(1) "2" "1" "08" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "38D-NG" "Z9" "16" "9" "2" "37D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "38D-NG" BY REFERENCE W-M1(1) "2" "1" "08" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "39D-NG" "9" "16" "40" "2" "38D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "39D-NG" BY REFERENCE W-Y22(1) "2" "1" "08" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "40D-NG" "Z9" "16" "43" "2" "39D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "40D-NG" BY REFERENCE W-M2(1) "2" "1" "08" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "41D-NG" " " "17" "0" "8" "36D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING
           "42D-NG" "9" "17" "6" "2" " " "41D-NG" RETURNING RESU.
       CALL "SD_From" USING
           "42D-NG" BY REFERENCE W-Y12(1) "2" "1" "09" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "43D-NG" "Z9" "17" "9" "2" "42D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "43D-NG" BY REFERENCE W-M1(1) "2" "1" "09" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "44D-NG" "9" "17" "40" "2" "43D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "44D-NG" BY REFERENCE W-Y22(1) "2" "1" "09" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "45D-NG" "Z9" "17" "43" "2" "44D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "45D-NG" BY REFERENCE W-M2(1) "2" "1" "09" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "46D-NG" " " "18" "0" "8" "41D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING
           "47D-NG" "9" "18" "6" "2" " " "46D-NG" RETURNING RESU.
       CALL "SD_From" USING
           "47D-NG" BY REFERENCE W-Y12(1) "2" "1" "10" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "48D-NG" "Z9" "18" "9" "2" "47D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "48D-NG" BY REFERENCE W-M1(1) "2" "1" "10" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "49D-NG" "9" "18" "40" "2" "48D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "49D-NG" BY REFERENCE W-Y22(1) "2" "1" "10" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "50D-NG" "Z9" "18" "43" "2" "49D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "50D-NG" BY REFERENCE W-M2(1) "2" "1" "10" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "51D-NG" " " "19" "0" "8" "46D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING
           "52D-NG" "9" "19" "6" "2" " " "51D-NG" RETURNING RESU.
       CALL "SD_From" USING
           "52D-NG" BY REFERENCE W-Y12(1) "2" "1" "11" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "53D-NG" "Z9" "19" "9" "2" "52D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "53D-NG" BY REFERENCE W-M1(1) "2" "1" "11" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "54D-NG" "9" "19" "40" "2" "53D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "54D-NG" BY REFERENCE W-Y22(1) "2" "1" "11" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "55D-NG" "Z9" "19" "43" "2" "54D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "55D-NG" BY REFERENCE W-M2(1) "2" "1" "11" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "56D-NG" " " "20" "0" "8" "51D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING
           "57D-NG" "9" "20" "6" "2" " " "56D-NG" RETURNING RESU.
       CALL "SD_From" USING
           "57D-NG" BY REFERENCE W-Y12(1) "2" "1" "12" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "58D-NG" "Z9" "20" "9" "2" "57D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "58D-NG" BY REFERENCE W-M1(1) "2" "1" "12" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "59D-NG" "9" "20" "40" "2" "58D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "59D-NG" BY REFERENCE W-Y22(1) "2" "1" "12" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "60D-NG" "Z9" "20" "43" "2" "59D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "60D-NG" BY REFERENCE W-M2(1) "2" "1" "12" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "D-MD" " " "0" "0" "750" "D-NG" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-MD" " " "8" "0" "50" " " "D-MD" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-MD" "---,---,---" "8" "12" "11" " " "01D-MD"
            RETURNING RESU.
       CALL "SD_From" USING
           "02D-MD" BY REFERENCE HY-ZSU(1) "7" "1" "01" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "03D-MD" "--,---,---,---" "8" "24" "14" "02D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "03D-MD" BY REFERENCE HY-ZKN(1) "10" "1" "01" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "04D-MD" "---,---,---" "8" "46" "11" "03D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "04D-MD" BY REFERENCE HY-KSU(1) "7" "1" "01" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "05D-MD" "--,---,---,---" "8" "58" "14" "04D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "05D-MD" BY REFERENCE HY-KKN(1) "10" "1" "01" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "06D-MD" " " "9" "0" "50" "01D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07D-MD" "---,---,---" "9" "12" "11" " " "06D-MD"
            RETURNING RESU.
       CALL "SD_From" USING
           "07D-MD" BY REFERENCE HY-ZSU(1) "7" "1" "02" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "08D-MD" "--,---,---,---" "9" "24" "14" "07D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "08D-MD" BY REFERENCE HY-ZKN(1) "10" "1" "02" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "09D-MD" "---,---,---" "9" "46" "11" "08D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "09D-MD" BY REFERENCE HY-KSU(1) "7" "1" "02" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "10D-MD" "--,---,---,---" "9" "58" "14" "09D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "10D-MD" BY REFERENCE HY-KKN(1) "10" "1" "02" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "11D-MD" " " "10" "0" "50" "06D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "12D-MD" "---,---,---" "10" "12" "11" " " "11D-MD"
            RETURNING RESU.
       CALL "SD_From" USING
           "12D-MD" BY REFERENCE HY-ZSU(1) "7" "1" "03" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "13D-MD" "--,---,---,---" "10" "24" "14" "12D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "13D-MD" BY REFERENCE HY-ZKN(1) "10" "1" "03" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "14D-MD" "---,---,---" "10" "46" "11" "13D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "14D-MD" BY REFERENCE HY-KSU(1) "7" "1" "03" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "15D-MD" "--,---,---,---" "10" "58" "14" "14D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "15D-MD" BY REFERENCE HY-KKN(1) "10" "1" "03" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "16D-MD" " " "11" "0" "50" "11D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "17D-MD" "---,---,---" "11" "12" "11" " " "16D-MD"
            RETURNING RESU.
       CALL "SD_From" USING
           "17D-MD" BY REFERENCE HY-ZSU(1) "7" "1" "04" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "18D-MD" "--,---,---,---" "11" "24" "14" "17D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "18D-MD" BY REFERENCE HY-ZKN(1) "10" "1" "04" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "19D-MD" "---,---,---" "11" "46" "11" "18D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "19D-MD" BY REFERENCE HY-KSU(1) "7" "1" "04" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "20D-MD" "--,---,---,---" "11" "58" "14" "19D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "20D-MD" BY REFERENCE HY-KKN(1) "10" "1" "04" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "21D-MD" " " "12" "0" "50" "16D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "22D-MD" "---,---,---" "12" "12" "11" " " "21D-MD"
            RETURNING RESU.
       CALL "SD_From" USING
           "22D-MD" BY REFERENCE HY-ZSU(1) "7" "1" "05" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "23D-MD" "--,---,---,---" "12" "24" "12" "22D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "23D-MD" BY REFERENCE HY-ZKN(1) "10" "1" "05" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "24D-MD" "---,---,---" "12" "46" "11" "23D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "24D-MD" BY REFERENCE HY-KSU(1) "7" "1" "05" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "25D-MD" "--,---,---,---" "12" "58" "14" "24D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "25D-MD" BY REFERENCE HY-KKN(1) "10" "1" "05" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "26D-MD" " " "13" "0" "50" "21D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "27D-MD" "---,---,---" "13" "12" "11" " " "26D-MD"
            RETURNING RESU.
       CALL "SD_From" USING
           "27D-MD" BY REFERENCE HY-ZSU(1) "7" "1" "06" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "28D-MD" "--,---,---,---" "13" "24" "14" "27D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "28D-MD" BY REFERENCE HY-ZKN(1) "10" "1" "06" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "29D-MD" "---,---,---" "13" "46" "11" "28D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "29D-MD" BY REFERENCE HY-KSU(1) "7" "1" "06" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "30D-MD" "--,---,---,---" "13" "58" "14" "29D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "30D-MD" BY REFERENCE HY-KKN(1) "10" "1" "06" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "31D-MD" " " "14" "0" "50" "26D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "32D-MD" "---,---,---" "14" "12" "11" " " "31D-MD"
            RETURNING RESU.
       CALL "SD_From" USING
           "32D-MD" BY REFERENCE W-ZFSU "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "33D-MD" "--,---,---,---" "14" "24" "14" "32D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "33D-MD" BY REFERENCE W-ZFKN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "34D-MD" "---,---,---" "14" "46" "11" "33D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "34D-MD" BY REFERENCE W-KFSU "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "35D-MD" "--,---,---,---" "14" "58" "14" "34D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "35D-MD" BY REFERENCE W-KFKN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "36D-MD" " " "15" "0" "50" "31D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "37D-MD" "---,---,---" "15" "12" "11" " " "36D-MD"
            RETURNING RESU.
       CALL "SD_From" USING
           "37D-MD" BY REFERENCE HY-ZSU(1) "7" "1" "07" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "38D-MD" "--,---,---,---" "15" "24" "14" "37D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "38D-MD" BY REFERENCE HY-ZKN(1) "10" "1" "07" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "39D-MD" "---,---,---" "15" "46" "11" "38D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "39D-MD" BY REFERENCE HY-KSU(1) "7" "1" "07" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "40D-MD" "--,---,---,---" "15" "58" "14" "39D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "40D-MD" BY REFERENCE HY-KKN(1) "10" "1" "07" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "41D-MD" " " "16" "0" "50" "36D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "42D-MD" "---,---,---" "16" "12" "11" " " "41D-MD"
            RETURNING RESU.
       CALL "SD_From" USING
           "42D-MD" BY REFERENCE HY-ZSU(1) "7" "1" "08" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "43D-MD" "--,---,---,---" "16" "24" "14" "42D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "43D-MD" BY REFERENCE HY-ZKN(1) "10" "1" "08" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "44D-MD" "---,---,---" "16" "46" "11" "43D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "44D-MD" BY REFERENCE HY-KSU(1) "7" "1" "08" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "45D-MD" "--,---,---,---" "16" "58" "14" "44D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "45D-MD" BY REFERENCE HY-KKN(1) "10" "1" "08" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "46D-MD" " " "17" "0" "50" "41D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "47D-MD" "---,---,---" "17" "12" "11" " " "46D-MD"
            RETURNING RESU.
       CALL "SD_From" USING
           "47D-MD" BY REFERENCE HY-ZSU(1) "7" "1" "09" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "48D-MD" "--,---,---,---" "17" "24" "14" "47D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "48D-MD" BY REFERENCE HY-ZKN(1) "10" "1" "09" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "49D-MD" "---,---,---" "17" "46" "11" "48D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "49D-MD" BY REFERENCE HY-KSU(1) "7" "1" "09" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "50D-MD" "--,---,---,---" "17" "58" "14" "49D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "50D-MD" BY REFERENCE HY-KKN(1) "10" "1" "09" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "51D-MD" " " "18" "0" "50" "46D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "52D-MD" "---,---,---" "18" "12" "11" " " "51D-MD"
            RETURNING RESU.
       CALL "SD_From" USING
           "52D-MD" BY REFERENCE HY-ZSU(1) "7" "1" "10" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "53D-MD" "--,---,---,---" "18" "24" "14" "52D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "53D-MD" BY REFERENCE HY-ZKN(1) "10" "1" "10" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "54D-MD" "---,---,---" "18" "46" "11" "53D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "54D-MD" BY REFERENCE HY-KSU(1) "7" "1" "10" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "55D-MD" "--,---,---,---" "18" "58" "14" "54D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "55D-MD" BY REFERENCE HY-KKN(1) "10" "1" "10" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "56D-MD" " " "19" "0" "50" "51D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "57D-MD" "---,---,---" "19" "12" "11" " " "56D-MD"
            RETURNING RESU.
       CALL "SD_From" USING
           "57D-MD" BY REFERENCE HY-ZSU(1) "7" "1" "11" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "58D-MD" "--,---,---,---" "19" "24" "14" "57D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "58D-MD" BY REFERENCE HY-ZKN(1) "10" "1" "11" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "59D-MD" "---,---,---" "19" "46" "11" "58D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "59D-MD" BY REFERENCE HY-KSU(1) "7" "1" "11" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "60D-MD" "--,---,---,---" "19" "58" "14" "59D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "60D-MD" BY REFERENCE HY-KKN(1) "10" "1" "11" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "61D-MD" " " "20" "0" "50" "56D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "62D-MD" "---,---,---" "20" "12" "11" " " "61D-MD"
            RETURNING RESU.
       CALL "SD_From" USING
           "62D-MD" BY REFERENCE HY-ZSU(1) "7" "1" "12" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "63D-MD" "--,---,---,---" "20" "24" "14" "62D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "63D-MD" BY REFERENCE HY-ZKN(1) "10" "1" "12" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "64D-MD" "---,---,---" "20" "46" "11" "63D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "64D-MD" BY REFERENCE HY-KSU(1) "7" "1" "12" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "65D-MD" "--,---,---,---" "20" "58" "14" "64D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "65D-MD" BY REFERENCE HY-KKN(1) "10" "1" "12" 17
            RETURNING RESU.
       CALL "SD_Init" USING
           "66D-MD" " " "21" "0" "50" "61D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "67D-MD" "---,---,---" "21" "12" "11" " " "66D-MD"
            RETURNING RESU.
       CALL "SD_From" USING
           "67D-MD" BY REFERENCE W-ZRSU "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "68D-MD" "--,---,---,---" "21" "24" "14" "67D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "68D-MD" BY REFERENCE W-ZRKN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "69D-MD" "---,---,---" "21" "46" "11" "68D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "69D-MD" BY REFERENCE W-KRSU "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "70D-MD" "--,---,---,---" "21" "58" "14" "69D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "70D-MD" BY REFERENCE W-KRKN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "71D-MD" " " "22" "0" "50" "66D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "72D-MD" "---,---,---" "22" "12" "11" " " "71D-MD"
            RETURNING RESU.
       CALL "SD_From" USING
           "72D-MD" BY REFERENCE W-ZASU "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "73D-MD" "--,---,---,---" "22" "24" "14" "72D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "73D-MD" BY REFERENCE W-ZAKN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "74D-MD" "---,---,---" "22" "46" "11" "73D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "74D-MD" BY REFERENCE W-KASU "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "75D-MD" "--,---,---,---" "22" "58" "14" "74D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "75D-MD" BY REFERENCE W-KAKN "10" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "95" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "95" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
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
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               GO TO M-95
           END-IF
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
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0512ID.
           MOVE WK0512ID TO HY-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HY-F_PNAME1 " " BY REFERENCE HY-F_IDLST "0".
       M-15.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHT81" RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
       M-20.
           IF  JS-SIGN = 0
               CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD"
                "9" "4" BY REFERENCE ESTAT RETURNING RESU
           ELSE
               IF  JS-SIGN = 1
                   CALL "SD_Accept" USING BY REFERENCE A-HCD1 "A-HCD1"
                    "9" "4" BY REFERENCE ESTAT RETURNING RESU
               END-IF
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           MOVE 0 TO CHK.
           IF  ESTAT = ADV
               MOVE 1 TO CHK
               GO TO M-30
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
      *
           PERFORM HNA-RTN THRU HNA-EX.
           IF  W-INV = 1
               GO TO M-20
           END-IF.
       M-25.
           CALL "DB_F_Close" USING BY REFERENCE HY-F_IDLST HY-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HY-F_PNAME1 " " BY REFERENCE HY-F_IDLST "0".
       M-30.
      *           READ HY-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HY-F_PNAME1 BY REFERENCE HY-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF
           IF  CHK = 1
               GO TO M-35
           END-IF
           IF  JS-SIGN = 0
               IF  W-HCD > HY-HCD
                   GO TO M-30
               ELSE
                   IF  W-HCD = HY-HCD
                       GO TO M-40
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  W-HCD1 > HY-HCD1
                   GO TO M-30
               ELSE
                   IF  W-HCD1 = HY-HCD1
                       GO TO M-40
                   END-IF
               END-IF
           END-IF
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           GO TO M-20.
       M-35.
           IF  JS-SIGN = 0
               MOVE HY-HCD TO W-HCD
           ELSE
               MOVE HY-HCD1 TO W-HCD1
           END-IF
           PERFORM HNA-RTN THRU HNA-EX.
       M-40.
           COMPUTE W-ZFSU = HY-ZSU(01) + HY-ZSU(02) + HY-ZSU(03)
                          + HY-ZSU(04) + HY-ZSU(05) + HY-ZSU(06).
           COMPUTE W-ZFKN = HY-ZKN(01) + HY-ZKN(02) + HY-ZKN(03)
                          + HY-ZKN(04) + HY-ZKN(05) + HY-ZKN(06).
           COMPUTE W-ZRSU = HY-ZSU(07) + HY-ZSU(08) + HY-ZSU(09)
                          + HY-ZSU(10) + HY-ZSU(11) + HY-ZSU(12).
           COMPUTE W-ZRKN = HY-ZKN(07) + HY-ZKN(08) + HY-ZKN(09)
                          + HY-ZKN(10) + HY-ZKN(11) + HY-ZKN(12).
           COMPUTE W-KFSU = HY-KSU(01) + HY-KSU(02) + HY-KSU(03)
                          + HY-KSU(04) + HY-KSU(05) + HY-KSU(06).
           COMPUTE W-KFKN = HY-KKN(01) + HY-KKN(02) + HY-KKN(03)
                          + HY-KKN(04) + HY-KKN(05) + HY-KKN(06).
           COMPUTE W-KRSU = HY-KSU(07) + HY-KSU(08) + HY-KSU(09)
                          + HY-KSU(10) + HY-KSU(11) + HY-KSU(12).
           COMPUTE W-KRKN = HY-KKN(07) + HY-KKN(08) + HY-KKN(09)
                          + HY-KKN(10) + HY-KKN(11) + HY-KKN(12).
           COMPUTE W-ZASU = W-ZFSU + W-ZRSU.
           COMPUTE W-ZAKN = W-ZFKN + W-ZRKN.
           COMPUTE W-KASU = W-KFSU + W-KRSU.
           COMPUTE W-KAKN = W-KFKN + W-KRKN.
           CALL "SD_Output" USING "D-MD" D-MD "p" RETURNING RESU.
           GO TO M-20.
       M-45.
           IF  CHK = 0
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-20
           END-IF
           GO TO M-25.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HY-F_IDLST HY-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       HNA-RTN.
           MOVE 0 TO W-INV.
           IF  JS-SIGN = 1
               GO TO HNA-10
           END-IF
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE SPACE TO W-NAME
               CALL "SD_Output" USING
                "D-NAME" D-NAME "p" RETURNING RESU
               GO TO HNA-EX
           END-IF
           MOVE HI-NAME TO W-NAME.
           GO TO HNA-30.
       HNA-10.
           MOVE ZERO TO HI-KEY.
           MOVE W-HCD1 TO HI-HCD1.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE SPACE TO W-NAME
               CALL "SD_Output" USING
                "D-NAME" D-NAME "p" RETURNING RESU
               GO TO HNA-EX
           END-IF
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE SPACE TO W-NAME
               CALL "SD_Output" USING
                "D-NAME" D-NAME "p" RETURNING RESU
               GO TO HNA-EX
           END-IF
           MOVE SPACE TO W-NAME W-HNAM.
           MOVE HI-NAME TO W-HNAM.
           MOVE ZERO TO CNT1.
       HNA-20.
           ADD 1 TO CNT1.
           IF  CNT1 > 24
               GO TO HNA-30
           END-IF
           MOVE W-HNA(CNT1) TO W-NA(CNT1).
           IF  W-HNA(CNT1) NOT = SPACE
               GO TO HNA-20
           END-IF
           ADD 1 TO CNT1.
           IF  CNT1 > 24
               GO TO HNA-30
           END-IF
           MOVE W-HNA(CNT1) TO W-NA(CNT1).
           IF  W-HNA(CNT1) NOT = SPACE
               GO TO HNA-20
           END-IF.
       HNA-30.
           IF  JS-SIGN = 0
               CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "A-HCD1" A-HCD1 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
       HNA-EX.
           EXIT.

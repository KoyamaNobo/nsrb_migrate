       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD530.
       AUTHOR. S-NAKAO.
       DATE-WRITTEN. 1974-05-03.
      *********************************************************
      *    PROGRAM         :  履物累積ファイル　作成　　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/17                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       01  W-DATA.
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NEND  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-HSDD         PIC  9(008).
           02  W-HSDM  REDEFINES W-HSDD.
             03  W-SNEN       PIC  9(004).
             03  W-SNEND REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-HSDS  REDEFINES W-HSDD.
             03  F            PIC  9(002).
             03  W-HSD        PIC  9(006).
           02  W-HNDD         PIC  9(008).
           02  W-HNDM  REDEFINES W-HNDD.
             03  W-NNEN       PIC  9(004).
             03  W-NNEND REDEFINES W-NNEN.
               04  W-NNEN1    PIC  9(002).
               04  W-NNEN2    PIC  9(002).
             03  W-NGET       PIC  9(002).
             03  W-NPEY       PIC  9(002).
           02  W-HNDS  REDEFINES W-HNDD.
             03  F            PIC  9(002).
             03  W-HND        PIC  9(006).
           02  W-HKC          PIC  9(001).
           02  W-TTD.
             03  W-TUA        PIC S9(009).
             03  W-TUAZ       PIC S9(007).
             03  W-TNB        PIC S9(008).
             03  W-TNBZ       PIC S9(006).
             03  W-TNK        PIC S9(009).
             03  W-TNKZ       PIC S9(007).
             03  W-TUG        PIC S9(009).
           02  W-NKD.
             03  WN-NO        PIC  9(006).
             03  WN-DATE      PIC  9(008).
             03  WN-TCD       PIC  9(004).
             03  WN-TNC       PIC  9(002).
             03  WN-BC        PIC  9(001).
             03  WN-DCC       PIC  9(001).
             03  WN-SKD       PIC  9(008).
             03  WN-KIN       PIC S9(009).
             03  WN-SHZ       PIC S9(007).
           02  W-FD.
             03  W-FC         PIC  9(001).
             03  W-FDC        PIC  9(001).
             03  W-FTCD       PIC  9(004).
             03  W-FHCD       PIC  9(006).
             03  W-FNG        PIC  9(006).
             03  W-FNGD       PIC  9(006).
           02  W-KEY.
             03  WK-TCD       PIC  9(004).
             03  F            PIC  X(009).
             03  WK-DNO       PIC  9(006).
             03  F            PIC  X(001).
           02  W-DTC          PIC  9(001).
           02  W-KSU          PIC  9(003).
           02  W-TUK.
             03  W-TCD        PIC  9(004).
             03  W-DATE       PIC  9(008).
             03  W-DC         PIC  9(001).
             03  W-KIN        PIC S9(009).
             03  W-SHZ        PIC S9(007).
             03  W-DNO        PIC  9(006).
             03  W-GNO        PIC  9(001).
           02  WT-D.
             03  WT-TZZ       PIC S9(009).
             03  WT-TUA       PIC S9(009).
             03  WT-TNB       PIC S9(009).
             03  WT-TNK       PIC S9(009).
             03  WT-TUZ       PIC S9(009).
             03  WT-TUZW      PIC S9(009).
       01  ERR-STAT           PIC  X(002).
           COPY LDAIW.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LITTM.
           COPY LIHIM.
           COPY LSNYUR.
           COPY LUTRAN.
           COPY LIHKBM.
           COPY LITUKF.
           COPY LISKDF.
           COPY LISKDK.
      *FD  STRAN
       01  STRAN_HMD530.
           02  STRAN_PNAME1   PIC  X(005) VALUE "STRAN".
           02  F              PIC  X(001).
           02  STRAN_LNAME    PIC  X(012) VALUE "STRAN_HMD530".
           02  F              PIC  X(001).
           02  STRAN_KEY1     PIC  X(100) VALUE SPACE.
           02  STRAN_KEY2     PIC  X(100) VALUE SPACE.
           02  STRAN_SORT     PIC  X(100) VALUE SPACE.
           02  STRAN_IDLST    PIC  X(100) VALUE SPACE.
           02  STRAN_RES      USAGE  POINTER.
       01  STRAN-R.
           02  ST-DNO         PIC  9(006).
           02  ST-GNO         PIC  9(001).
           02  ST-DATE        PIC  9(008).
           02  ST-TCD         PIC  9(004).
           02  ST-D1.
             03  ST-HCD       PIC  9(006).
             03  F            PIC  X(031).
             03  ST-SU        PIC S9(005).
             03  ST-T         PIC S9(005).
             03  ST-KIN       PIC S9(008).
             03  ST-CSC       PIC  9(001).
             03  ST-DC        PIC  9(001).
             03  ST-FT        PIC  9(005).
             03  ST-CCD       PIC  9(003).
             03  F            PIC  X(007).
             03  ST-TNC       PIC  9(002).
             03  F            PIC  X(003).
             03  ST-KSU       PIC  9(003).
             03  F            PIC  X(001).
             03  ST-TCD2      PIC  9(004).
             03  F            PIC  X(010).
             03  ST-SKD       PIC  9(008).
             03  F            PIC  X(005).
           02  ST-D2    REDEFINES ST-D1.
             03  ST-BI        PIC  N(024).
             03  ST-HNO       PIC  9(006).
             03  F            PIC  X(030).
             03  ST-SHZ       PIC S9(007).
             03  F            PIC  X(017).
           02  ST-SNC         PIC  9(001).
       77  F                  PIC  X(001).
      *FD  NYU-F
       01  NYU-F_HMD530.
           02  NYU-F_PNAME1   PIC  X(004) VALUE "NYUF".
           02  F              PIC  X(001).
           02  NYU-F_LNAME    PIC  X(012) VALUE "NYU-F_HMD530".
           02  F              PIC  X(001).
           02  NYU-F_KEY1     PIC  X(100) VALUE SPACE.
           02  NYU-F_SORT     PIC  X(100) VALUE SPACE.
           02  NYU-F_IDLST    PIC  X(100) VALUE SPACE.
           02  NYU-F_RES      USAGE  POINTER.
       01  NYU-R.
           02  NYU-RD.
             03  N-DATE       PIC  9(008).
             03  N-TCD        PIC  9(004).
             03  N-KIN        PIC S9(008).
             03  N-NC.
               04  N-NC1      PIC  9(001).
               04  N-NC2      PIC  9(001).
             03  F            PIC  X(015).
             03  N-BC         PIC  9(001).
             03  N-TNC        PIC  9(002).
             03  F            PIC  X(003).
             03  N-KEY.
               04  N-NO       PIC  9(006).
               04  N-GNO      PIC  9(001).
             03  F            PIC  X(008).
             03  N-SKD        PIC  9(008).
             03  N-DCC        PIC  9(001).
             03  F            PIC  X(014).
             03  N-RSC        PIC  9(001).
             03  N-DPC        PIC  9(001).
             03  F            PIC  X(002).
           02  F              PIC  X(017).
       77  F                  PIC  X(001).
      *FD  SNTR-F
       01  SNTR-F_HMD530.
           02  SNTR-F_PNAME1  PIC  X(005) VALUE "SNTRF".
           02  F              PIC  X(001).
           02  SNTR-F_LNAME   PIC  X(013) VALUE "SNTR-F_HMD530".
           02  F              PIC  X(001).
           02  SNTR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  SNTR-F_SORT    PIC  X(100) VALUE SPACE.
           02  SNTR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  SNTR-F_RES     USAGE  POINTER.
       01  SNTR-R.
           02  F              PIC  X(007).
           02  SNTR-DATE      PIC  9(008).
           02  SNTR-NGP   REDEFINES SNTR-DATE.
             03  SNTR-NG      PIC  9(006).
             03  F            PIC  9(002).
           02  SNTR-TCD       PIC  9(004).
           02  SNTR-HCD       PIC  9(006).
           02  F              PIC  X(050).
           02  SNTR-DC        PIC  9(001).
           02  F              PIC  X(052).
       77  F                  PIC  X(001).
      *FD  UTR-F
       01  UTR-F_HMD530.
           02  UTR-F_PNAME1   PIC  X(004) VALUE "UTRF".
           02  F              PIC  X(001).
           02  UTR-F_LNAME    PIC  X(013) VALUE "UTR-F_HMD530".
           02  F              PIC  X(001).
           02  UTR-F_KEY1     PIC  X(100) VALUE SPACE.
           02  UTR-F_SORT     PIC  X(100) VALUE SPACE.
           02  UTR-F_IDLST    PIC  X(100) VALUE SPACE.
           02  UTR-F_RES      USAGE  POINTER.
       01  UTR-R.
           02  UTR-NO         PIC  9(007).
           02  UTR-DATE       PIC  9(008).
           02  UTR-NGPD  REDEFINES UTR-DATE.
             03  UTR-NG       PIC  9(006).
             03  F            PIC  9(002).
           02  UTR-HCD        PIC  9(006).
           02  UTR-SIZ        PIC  9(001).
           02  UTR-SUD.
             03  UTR-SU       PIC S9(004)  OCCURS  10.
           02  UTR-SUT        PIC S9(005).
           02  UTR-BKIN       PIC S9(008).
           02  UTR-FKIN       PIC S9(008).
           02  UTR-NRC        PIC  9(001).
           02  UTR-SSC        PIC  9(001).
           02  UTR-HPC        PIC  9(001).
           02  UTR-SKC        PIC  9(001).
           02  UTR-BC.
             03  UTR-BC1      PIC  9(002).
             03  UTR-BC2      PIC  9(002).
             03  UTR-BC3      PIC  9(002).
           02  UTR-BMC        PIC  9(002).
           02  UTR-BMNO       PIC  9(001).
           02  F              PIC  X(031).
           02  UTR-PRC        PIC  9(001).
       77  F                  PIC  X(001).
      *FD  NYURYR
       01  NYURYR_HMD530.
           02  NYURYR_PNAME1  PIC  X(006) VALUE "NYURYR".
           02  F              PIC  X(001).
           02  NYURYR_LNAME   PIC  X(013) VALUE "NYURYR_HMD530".
           02  F              PIC  X(001).
           02  NYURYR_KEY1    PIC  X(100) VALUE SPACE.
           02  NYURYR_SORT    PIC  X(100) VALUE SPACE.
           02  NYURYR_IDLST   PIC  X(100) VALUE SPACE.
           02  NYURYR_RES     USAGE  POINTER.
       01  NYURY-R            PIC  X(102).
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
       01  C-MID.
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　履物　累積ファイル　作成　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-DSP.
           02  D-ZM.
             03  FILLER.
               04  FILLER      PIC  X(004) VALUE "ｺｰﾄﾞ".
               04  0201D-ZMC   PIC  9(004).
             03  FILLERs.
               04  FILLER      PIC  N(006) VALUE "　　　前月残".
               04  FILLER      PIC  N(006) VALUE "　　　売　上".
               04  FILLER      PIC  N(006) VALUE "　　　値　引".
               04  FILLER      PIC  N(006) VALUE "　　　入　金".
               04  FILLER      PIC  N(006) VALUE "　　　売掛残".
             03  FILLER.
               04  FILLER      PIC  N(003) VALUE "本　体".
               04  0203D-ZMC   PIC ----,---,--9 .
               04  0303D-ZMC   PIC ----,---,--9 .
               04  0403D-ZMC   PIC ----,---,--9 .
               04  0503D-ZMC   PIC ----,---,--9 .
               04  0603D-ZMC   PIC ----,---,--9 .
             03  FILLER.
               04  FILLER      PIC  N(003) VALUE "消費税".
               04  0204D-ZMC   PIC ----,---,--9 .
               04  0304D-ZMC   PIC ----,---,--9 .
               04  0404D-ZMC   PIC ----,---,--9 .
               04  0504D-ZMC   PIC ----,---,--9 .
               04  0604D-ZMC   PIC ----,---,--9 .
             03  FILLER.
               04  FILLER      PIC  N(003) VALUE "合　計".
               04  0205D-ZMC   PIC ----,---,--9 .
               04  0305D-ZMC   PIC ----,---,--9 .
               04  0405D-ZMC   PIC ----,---,--9 .
               04  0505D-ZMC   PIC ----,---,--9 .
               04  0605D-ZMC   PIC ----,---,--9 .
           02  D-ZMC.
             03  FILLER.
               04  FILLER      PIC  X(004) VALUE "    ".
               04  FILLER      PIC  X(004) VALUE "    ".
             03  FILLER.
               04  FILLER      PIC  X(012) VALUE "            ".
               04  FILLER      PIC  X(012) VALUE "            ".
               04  FILLER      PIC  X(012) VALUE "            ".
               04  FILLER      PIC  X(012) VALUE "            ".
               04  FILLER      PIC  X(012) VALUE "            ".
             03  FILLER.
               04  FILLER      PIC  X(012) VALUE "            ".
               04  FILLER      PIC  X(012) VALUE "            ".
               04  FILLER      PIC  X(012) VALUE "            ".
               04  FILLER      PIC  X(012) VALUE "            ".
               04  FILLER      PIC  X(012) VALUE "            ".
             03  FILLER.
               04  FILLER      PIC  X(012) VALUE "            ".
               04  FILLER      PIC  X(012) VALUE "            ".
               04  FILLER      PIC  X(012) VALUE "            ".
               04  FILLER      PIC  X(012) VALUE "            ".
               04  FILLER      PIC  X(012) VALUE "            ".
             03  FILLER.
               04  FILLER      PIC  X(012) VALUE "            ".
               04  FILLER      PIC  X(012) VALUE "            ".
               04  FILLER      PIC  X(012) VALUE "            ".
               04  FILLER      PIC  X(012) VALUE "            ".
               04  FILLER      PIC  X(012) VALUE "            ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(025) VALUE
                  "***  SNTRF WRITE ｴﾗｰ  ***".
             03  E-ME2   PIC  X(025) VALUE
                  "***  NYURF WRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(024) VALUE
                  "***  UTRF WRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(026) VALUE
                  "***  NYURYR WRITE ｴﾗｰ  ***".
             03  E-ME8   PIC  X(026) VALUE
                  "***  NYUF REWRITE ｴﾗｰ  ***".
             03  E-ME9   PIC  X(018) VALUE
                  "***  DATEM ﾅｼ  ***".
             03  E-ME10  PIC  X(027) VALUE
                  "***  DATEM REWRITE ｴﾗｰ  ***".
             03  E-ME19  PIC  X(024) VALUE
                  "***  HKBM WRITE ｴﾗｰ  ***".
             03  E-ME20  PIC  X(026) VALUE
                  "***  HKBM REWRITE ｴﾗｰ  ***".
             03  E-ME21  PIC  X(017) VALUE
                  "***  HKBM ﾅｼ  ***".
             03  E-ME22  PIC  X(016) VALUE
                  "***  TTM ﾅｼ  ***".
             03  E-ME25  PIC  X(024) VALUE
                  "***  TM REWRITE ｴﾗｰ  ***".
             03  E-ME26  PIC  X(025) VALUE
                  "***  HIM REWRITE ｴﾗｰ  ***".
             03  E-ME27  PIC  X(025) VALUE
                  "***  TTM REWRITE ｴﾗｰ  ***".
             03  E-ME28  PIC  X(026) VALUE
                  "***   TUKF WRITE ｴﾗｰ   ***".
             03  E-ME29  PIC  X(026) VALUE
                  "***   SKDF WRITE ｴﾗｰ   ***".
             03  E-ME30  PIC  X(026) VALUE
                  "***  SKDF REWRITE ｴﾗｰ  ***".
             03  E-ME31.
               04  FILLER    PIC  X(039) VALUE
                    "***  ﾃﾞﾝﾋﾟｮｳNO ｴﾗｰ (      ･      )  ***".
               04  02E-ME31  PIC  9(006).
               04  03E-ME31  PIC  9(006).
             03  E-ME32  PIC  X(027) VALUE
                  "***   SKDKF WRITE ｴﾗｰ   ***".
             03  E-ME40  PIC  X(026) VALUE
                  "***   ﾀﾞｲﾁｮｳNO ｵｰﾊﾞｰ   ***".
             03  E-TCD   PIC  9(004).
           COPY LSSEM.
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
            "C-MID" " " "0" "0" "280" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "40" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "40" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "40" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "40" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "40" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "40" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "40" "06C-MID" " "  RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "1028" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZM" " " "0" "0" "514" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-ZMC" " " "14" "0" "16" " " "D-ZMC"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-ZMC" "X" "14" "8" "4" " " "01D-ZMC" RETURNING RESU.
       CALL "SD_Init" USING 
           "0201D-ZMC" "9" "14" "13" "4" "0101D-ZMC" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-ZMC" BY REFERENCE TT-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-ZMC" " " "15" "0" "120" "LINE" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-ZMC" "N" "15" "14" "12" " " "02D-ZMC" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-ZMC" "N" "15" "26" "12" "0102D-ZMC" " "
             RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-ZMC" "N" "15" "38" "12" "0202D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402D-ZMC" "N" "15" "50" "12" "0302D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0502D-ZMC" "N" "15" "62" "12" "0402D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-ZMC" " " "16" "0" "126" "LINE" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0103D-ZMC" "N" "16" "8" "6" " " "03D-ZMC" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203D-ZMC" "----,---,--9" "16" "14" "12" "0103D-ZMC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0203D-ZMC" BY REFERENCE TT-TZZ "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0303D-ZMC" "----,---,--9" "16" "26" "12" "0203D-ZMC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0303D-ZMC" BY REFERENCE TT-TUA "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0403D-ZMC" "----,---,--9" "16" "38" "12" "0303D-ZMC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0403D-ZMC" BY REFERENCE TT-TNB "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0503D-ZMC" "----,---,--9" "16" "50" "12" "0403D-ZMC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0503D-ZMC" BY REFERENCE TT-TNK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0603D-ZMC" "----,---,--9" "16" "62" "12" "0503D-ZMC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0603D-ZMC" BY REFERENCE TT-TUZ "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-ZMC" " " "17" "0" "126" "LINE" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0104D-ZMC" "N" "17" "8" "6" " " "04D-ZMC"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0204D-ZMC" "----,---,--9" "17" "14" "12" "0104D-ZMC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0204D-ZMC" BY REFERENCE TT-TZZZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0304D-ZMC" "----,---,--9" "17" "26" "12" "0204D-ZMC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0304D-ZMC" BY REFERENCE TT-TUAZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0404D-ZMC" "----,---,--9" "17" "38" "12" "0304D-ZMC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0404D-ZMC" BY REFERENCE TT-TNBZ "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0504D-ZMC" "----,---,--9" "17" "50" "12" "0404D-ZMC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0504D-ZMC" BY REFERENCE TT-TNKZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0604D-ZMC" "----,---,--9" "17" "62" "12" "0504D-ZMC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0604D-ZMC" BY REFERENCE TT-TUZZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-ZMC" " " "18" "0" "126" "LINE" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0105D-ZMC" "N" "18" "8" "6" " " "05D-ZMC" RETURNING RESU.
       CALL "SD_Init" USING 
            "0205D-ZMC" "----,---,--9" "18" "14" "12" "0105D-ZMC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0205D-ZMC" BY REFERENCE WT-TZZ "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0305D-ZMC" "----,---,--9" "18" "26" "12" "0205D-ZMC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0305D-ZMC" BY REFERENCE WT-TUA "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0405D-ZMC" "----,---,--9" "18" "38" "12" "0305D-ZMC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0405D-ZMC" BY REFERENCE WT-TNB "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0505D-ZMC" "----,---,--9" "18" "50" "12" "0405D-ZMC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0505D-ZMC" BY REFERENCE WT-TNK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0605D-ZMC" "----,---,--9" "18" "62" "12" "0505D-ZMC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0605D-ZMC" BY REFERENCE WT-TUZ "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZMC" " " "0" "0" "514" "D-ZM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-ZMC" " " "14" "0" "16" " " "D-ZMC"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-ZMC" "X" "14" "8" "4" " " "01D-ZMC" RETURNING RESU.
       CALL "SD_Init" USING 
           "0201D-ZMC" "X" "14" "13" "4" "0101D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-ZMC" " " "15" "0" "120" "01D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01LINE" "X" "15" "14" "12" " " "LINE"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02LINE" "X" "15" "26" "12" "COLUMN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03LINE" "X" "15" "38" "12" "COLUMN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04LINE" "X" "15" "50" "12" "COLUMN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05LINE" "X" "15" "62" "12" "COLUMN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-ZMC" " " "16" "0" "126" "02D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01LINE" "X" "16" "14" "12" " " "LINE"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02LINE" "X" "16" "26" "12" "COLUMN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03LINE" "X" "16" "38" "12" "COLUMN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04LINE" "X" "16" "50" "12" "COLUMN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05LINE" "X" "16" "62" "12" "COLUMN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-ZMC" " " "17" "0" "126" "03D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01LINE" "X" "17" "14" "12" " " "LINE"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02LINE" "X" "17" "26" "12" "COLUMN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03LINE" "X" "17" "38" "12" "COLUMN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04LINE" "X" "17" "50" "12" "COLUMN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05LINE" "X" "17" "62" "12" "COLUMN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-ZMC" " " "18" "0" "126" "04D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0105D-ZMC" "X" "18" "14" "12" " " "05D-ZMC" RETURNING RESU.
       CALL "SD_Init" USING 
            "0205D-ZMC" "X" "18" "26" "12" "0105D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0305D-ZMC" "X" "18" "38" "12" "0205D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0405D-ZMC" "X" "18" "50" "12" "0305D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0505D-ZMC" "X" "18" "62" "12" "0405D-ZMC" " "
            RETURNING RESU.
      *
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "514" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "514" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "25" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "25" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "24" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "26" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "26" "E-ME7" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "18" "E-ME8" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "27" "E-ME9" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME19" "X" "24" "15" "24" "E-ME10" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME20" "X" "24" "15" "26" "E-ME19" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME21" "X" "24" "15" "17" "E-ME20" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME22" "X" "24" "15" "16" "E-ME21" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME25" "X" "24" "15" "24" "E-ME22" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME26" "X" "24" "15" "25" "E-ME25" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME27" "X" "24" "15" "25" "E-ME26" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME28" "X" "24" "15" "26" "E-ME27" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME29" "X" "24" "15" "26" "E-ME28" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME30" "X" "24" "15" "26" "E-ME29" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME31" " " "24" "0" "51" "E-ME30" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME31" "X" "24" "15" "39" " " "E-ME31" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME31" "9" "24" "35" "6" "01E-ME31" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME31" BY REFERENCE ST-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03E-ME31" "9" "24" "42" "6" "02E-ME31" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03E-ME31" BY REFERENCE WK-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME32" "X" "24" "15" "27" "E-ME31" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME40" "X" "24" "15" "26" "E-ME32" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TCD" "9" "24" "45" "4" "E-ME40" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE WK-TCD "4" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           MOVE 9 TO W-HKC.
      *
           CALL "DB_F_Open" USING
            "I-O" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "EXTEND" SNTR-F_PNAME1 " " BY REFERENCE SNTR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
           CALL "DB_F_Open" USING
            "I-O" SKDKF_PNAME1 "SHARED" BY REFERENCE SKDKF_IDLST "1"
            "SKDK-KEY" BY REFERENCE SKDK-KEY.
           CALL "DB_F_Open" USING
            "I-O" TT-M_PNAME1 "SHARED" BY REFERENCE TT-M_IDLST "1"
            "TT-KEY" BY REFERENCE TT-KEY.
           CALL "DB_F_Open" USING
            "I-O" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           CALL "DB_F_Open" USING
            "I-O" TUKF_PNAME1 "SHARED" BY REFERENCE TUKF_IDLST "2"
            "TUK-KEY" BY REFERENCE TUK-KEY "TUK-KEY2" BY REFERENCE
            TUK-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" STRAN_PNAME1 " " BY REFERENCE STRAN_IDLST "0".
       M-040.
      *           READ STRAN AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" STRAN_PNAME1 BY REFERENCE STRAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-180
           END-IF
           PERFORM SNTR-RTN THRU SNTR-EX.
           IF  W-END = 9
               GO TO M-160
           END-IF
           IF  ST-GNO = 9
               GO TO M-060
           END-IF
      *-----------------------------------------------------------------
           IF  ST-SNC = 1
               ADD ST-KIN TO W-TNB
           ELSE
               IF  ST-DC = 0 OR 3 OR 7
                   ADD ST-KIN TO W-TUA
                   COMPUTE W-TUG = W-TUG + (ST-SU * ST-FT)
               ELSE
                   IF  ST-DC = 1 OR 2 OR 5
                       SUBTRACT ST-KIN FROM W-TUA
                       COMPUTE W-TUG = W-TUG - (ST-SU * ST-FT)
                   END-IF
               END-IF
           END-IF
      *-----------------------------------------------------------------
           MOVE ST-KSU TO W-KSU.
           MOVE 0 TO W-DTC.
           PERFORM SKDW-RTN THRU SKDW-EX.
           IF  W-END = 9
               GO TO M-160
           END-IF
           IF  W-HKC NOT = 1
               MOVE 1 TO W-HKC
           END-IF
           IF  W-HSDD < ST-DATE
               MOVE ST-DATE TO W-HSDD
           END-IF
      *
           MOVE ZERO TO W-FD.
           MOVE 1 TO W-FC.
           MOVE SNTR-TCD TO W-FTCD.
           MOVE SNTR-HCD TO W-FHCD.
           MOVE SNTR-NG TO W-FNG.
           IF  SNTR-DC NOT = 5
               MOVE SNTR-NG TO W-FNGD
           END-IF
           PERFORM DNG-RTN THRU DNG-EX.
           GO TO M-040.
       M-060.
           IF  ST-DNO NOT = WK-DNO
               CALL "SD_Output" USING
                "E-ME31" E-ME31 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-160
           END-IF
      *-----------------------------------------------------------------
           MOVE ST-TCD TO TT-KEY.
      *           READ TT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TT-M_PNAME1 BY REFERENCE TT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME22" E-ME22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-160
           END-IF
           ADD W-TUA TO TT-TUA.
           ADD W-TNB TO TT-TNB.
           ADD W-TUG TO TT-TUG.
           COMPUTE TT-TUZ = TT-TUZ + W-TUA - W-TNB.
           IF  ST-SNC = 1
               SUBTRACT ST-SHZ FROM TT-TUZZ
               ADD ST-SHZ TO TT-TNBZ
           ELSE
               ADD ST-SHZ TO TT-TUZZ TT-TUAZ
           END-IF
           PERFORM TTMC-RTN THRU TTMC-EX.
      *           REWRITE TT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TT-M_PNAME1 TT-M_LNAME TT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME27" E-ME27 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-160
           END-IF
           PERFORM DNO2-RTN THRU DNO2-EX.
           MOVE ST-TCD TO W-TCD.
           MOVE ST-DATE TO W-DATE.
           IF  ST-SNC NOT = 0
               MOVE 2 TO W-DC
               MOVE W-TNB TO W-KIN
               MOVE ST-SHZ TO W-SHZ
           ELSE
               MOVE 1 TO W-DC
               MOVE W-TUA TO W-KIN
               MOVE ST-SHZ TO W-SHZ
           END-IF
           MOVE ST-DNO TO W-DNO.
           MOVE 0 TO W-GNO.
           PERFORM TUK-RTN THRU TUK-EX.
           MOVE ZERO TO W-TTD W-TUK.
      *-----------------------------------------------------------------
           MOVE W-KEY TO SKD-KEY.
      *           READ SKDF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" SKDF_PNAME1 BY REFERENCE SKD-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME31" E-ME31 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-160
           END-IF.
       M-080.
           PERFORM SKDR-RTN THRU SKDR-EX.
           IF  W-END = 9
               GO TO M-160
           END-IF
           IF  SKD-SKD NOT = 99999999
               GO TO M-100
           END-IF
           PERFORM SKKW-RTN THRU SKKW-EX.
           IF  W-END = 9
               GO TO M-160
           END-IF.
       M-100.
      *           READ SKDF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-040
           END-IF
           IF  SKD-DNO = WK-DNO
               GO TO M-080
           END-IF
           GO TO M-040.
       M-160.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "DB_F_Close" USING
            BY REFERENCE STRAN_IDLST STRAN_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SKDKF_IDLST SKDKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TUKF_IDLST TUKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           GO TO M-980.
       M-180.
           CALL "DB_F_Close" USING
            BY REFERENCE STRAN_IDLST STRAN_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SKDKF_IDLST SKDKF_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" NYU-F_PNAME1 " " BY REFERENCE NYU-F_IDLST "1"
            "N-KEY" BY REFERENCE N-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" NYUR-F_PNAME1 " " BY REFERENCE NYUR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" NYURYR_PNAME1 " " BY REFERENCE NYURYR_IDLST "0".
           MOVE ZERO TO W-NKD.
       M-200.
      *           READ NYU-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-300
           END-IF
           IF  N-BC NOT = 0
               GO TO M-200
           END-IF
           IF  N-DPC = 0
               GO TO M-200
           END-IF.
       M-220.
           MOVE ZERO TO NYUR-R.
           MOVE NYU-R TO NYUR-R.
      *           WRITE NYUR-R.
      *//////////////
           CALL "DB_Insert" USING
            NYUR-F_PNAME1 NYUR-F_LNAME NYUR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-240
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-360
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1.
           MOVE "NYURF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" NYUR-F_PNAME1 " " BY REFERENCE NYUR-F_IDLST "0".
           GO TO M-220.
       M-240.
           MOVE ZERO TO NYURY-R.
           MOVE NYU-R TO NYURY-R.
      *           WRITE NYURY-R.
      *//////////////
           CALL "DB_Insert" USING
            NYURYR_PNAME1 NYURYR_LNAME NYURY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-260
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME7" E-ME7 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-360
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NYURYR_IDLST NYURYR_PNAME1.
           MOVE "NYURYR       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" NYURYR_PNAME1 " " BY REFERENCE NYURYR_IDLST "0".
           GO TO M-240.
       M-260.
           IF  WN-TCD = ZERO
               MOVE N-DATE TO WN-DATE
               MOVE N-TCD TO WN-TCD
               MOVE N-NO TO WN-NO
               MOVE N-TNC TO WN-TNC
               MOVE N-BC TO WN-BC
               MOVE N-DCC TO WN-DCC
               MOVE N-SKD TO WN-SKD
           ELSE
               IF N-NO NOT = WN-NO
                   GO TO M-280
               END-IF
           END-IF
           IF  N-NC2 > 7
               ADD N-KIN TO WN-SHZ
           ELSE
               ADD N-KIN TO WN-KIN
           END-IF
           MOVE 1 TO N-RSC.
      *           REWRITE NYU-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            NYU-F_PNAME1 NYU-F_LNAME NYU-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-360
           END-IF
      *
           MOVE ZERO TO W-FD.
           MOVE 2 TO W-FC.
           MOVE NUR-TCD TO W-FTCD.
           MOVE NUR-NG TO W-FNG.
           PERFORM DNG-RTN THRU DNG-EX.
           GO TO M-200.
       M-280.
      *-----------------------------------------------------------------
           MOVE WN-TCD TO TT-KEY.
      *           READ TT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TT-M_PNAME1 BY REFERENCE TT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME22" E-ME22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-360
           END-IF
           ADD WN-KIN TO TT-TNK.
           ADD WN-SHZ TO TT-TNKZ.
           SUBTRACT WN-KIN FROM TT-TUZ.
           SUBTRACT WN-SHZ FROM TT-TUZZ.
           PERFORM TTMC-RTN THRU TTMC-EX.
      *           REWRITE TT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TT-M_PNAME1 TT-M_LNAME TT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME27" E-ME27 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-360
           END-IF
      *-----------------------------------------------------------------
           MOVE 3 TO W-DTC.
           PERFORM SKDW-RTN THRU SKDW-EX.
           IF  W-END = 9
               GO TO M-360
           END-IF
           PERFORM DNO2-RTN THRU DNO2-EX.
           MOVE ZERO TO W-TUK.
           MOVE WN-TCD TO W-TCD.
           MOVE WN-DATE TO W-DATE.
           MOVE 3 TO W-DC.
           MOVE WN-KIN TO W-KIN.
           MOVE WN-SHZ TO W-SHZ.
           MOVE WN-NO TO W-DNO.
           MOVE 0 TO W-GNO.
           PERFORM TUK-RTN THRU TUK-EX.
           MOVE ZERO TO W-NKD.
           GO TO M-260.
       M-300.
           IF WN-TCD = ZERO
               GO TO M-380
           END-IF
      *-----------------------------------------------------------------
           MOVE WN-TCD TO TT-KEY.
      *           READ TT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TT-M_PNAME1 BY REFERENCE TT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME22" E-ME22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-360
           END-IF
           ADD WN-KIN TO TT-TNK.
           ADD WN-SHZ TO TT-TNKZ.
           SUBTRACT WN-KIN FROM TT-TUZ.
           SUBTRACT WN-SHZ FROM TT-TUZZ.
           PERFORM TTMC-RTN THRU TTMC-EX.
      *           REWRITE TT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TT-M_PNAME1 TT-M_LNAME TT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME27" E-ME27 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-360
           END-IF
      *-----------------------------------------------------------------
           MOVE 3 TO W-DTC.
           PERFORM SKDW-RTN THRU SKDW-EX.
           IF  W-END = 9
               GO TO M-360
           END-IF
           PERFORM DNO2-RTN THRU DNO2-EX.
           MOVE ZERO TO W-TUK.
           MOVE WN-TCD TO W-TCD.
           MOVE WN-DATE TO W-DATE.
           MOVE 3 TO W-DC.
           MOVE WN-KIN TO W-KIN.
           MOVE WN-SHZ TO W-SHZ.
           MOVE WN-NO TO W-DNO.
           MOVE 0 TO W-GNO.
           PERFORM TUK-RTN THRU TUK-EX.
           GO TO M-380.
       M-360.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TUKF_IDLST TUKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NYURYR_IDLST NYURYR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
           GO TO M-980.
       M-380.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TUKF_IDLST TUKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NYURYR_IDLST NYURYR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" UTRAN_PNAME1 " " BY REFERENCE UTRAN_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" UTR-F_PNAME1 " " BY REFERENCE UTR-F_IDLST "0".
       M-400.
      *           READ UTRAN AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" UTRAN_PNAME1 BY REFERENCE UTRAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-460
           END-IF.
       M-420.
           MOVE ZERO TO UTR-R.
           MOVE UTRAN-R TO UTR-R.
      *           WRITE UTR-R.
      *//////////////
           CALL "DB_Insert" USING
            UTR-F_PNAME1 UTR-F_LNAME UTR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-440
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME3" E-ME3 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE UTRAN_IDLST UTRAN_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE UTR-F_IDLST UTR-F_PNAME1
               GO TO M-980
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE UTR-F_IDLST UTR-F_PNAME1.
           MOVE "UTRF         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" UTR-F_PNAME1 " " BY REFERENCE UTR-F_IDLST "0".
           GO TO M-420.
       M-440.
           IF  W-HKC NOT = 1
               MOVE 1 TO W-HKC
           END-IF
           IF  W-HNDD < UTRAN-DATE
               MOVE UTRAN-DATE TO W-HNDD
           END-IF
      *
           MOVE ZERO TO W-FD.
           MOVE 3 TO W-FC.
           MOVE UTR-HCD TO W-FHCD.
           MOVE UTR-NG TO W-FNG.
           IF  UTR-NRC NOT = 4 AND 5
               MOVE UTR-NG TO W-FNGD
           END-IF
           PERFORM DNG-RTN THRU DNG-EX.
           GO TO M-400.
       M-460.
           CALL "DB_F_Close" USING
            BY REFERENCE UTRAN_IDLST UTRAN_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UTR-F_IDLST UTR-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
      *
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
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE M-DATE_IDLST M-DATE_PNAME1
               GO TO M-980
           END-IF
      *
           MOVE ZERO TO W-NGP.
           MOVE D-HSD TO W-NGPS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF (W-NGP < W-HSDD) OR (ZERO = W-GET AND W-PEY)
               MOVE W-HSD TO D-HSD
           END-IF
      *
           MOVE ZERO TO W-NGP.
           MOVE D-HND TO W-NGPS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF (W-NGP < W-HNDD) OR (ZERO = W-GET AND W-PEY)
               MOVE W-HND TO D-HND
           END-IF
      *
           MOVE W-HKC TO D-HKC.
      *           REWRITE DATE-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            M-DATE_PNAME1 M-DATE_LNAME DATE-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       SNTR-RTN.
           MOVE ZERO TO SNTR-R.
           MOVE STRAN-R TO SNTR-R.
      *           WRITE SNTR-R.
      *//////////////
           CALL "DB_Insert" USING
            SNTR-F_PNAME1 SNTR-F_LNAME SNTR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO SNTR-EX
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 9 TO W-END
               GO TO SNTR-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1.
           MOVE "SNTRF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" SNTR-F_PNAME1 " " BY REFERENCE SNTR-F_IDLST "0".
           GO TO SNTR-RTN.
       SNTR-EX.
           EXIT.
       SKDW-RTN.
           IF  W-DTC = 3
               GO TO SKDW-010
           END-IF
           MOVE ST-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO T-BC T-DCC
           END-IF.
       SKDW-010.
           INITIALIZE SKD-R.
           MOVE ZERO TO SKD-HNO.
           MOVE SPACE TO SKD-BI.
           IF  W-DTC = 3
               MOVE WN-TCD TO SKD-TCD
               MOVE WN-DATE TO SKD-DATE
               MOVE 3 TO SKD-DTC
               MOVE WN-NO TO SKD-DNO
               MOVE WN-KIN TO SKD-KIN
               MOVE WN-SKD TO SKD-SKD
               MOVE WN-TNC TO SKD-TNC
               MOVE WN-BC TO SKD-BMC
               MOVE WN-DCC TO SKD-DCC
               MOVE WN-SHZ TO SKD-SHZ
               GO TO SKDW-020
           END-IF
           MOVE ST-TCD TO SKD-TCD.
           MOVE ST-DATE TO SKD-DATE.
           MOVE ST-SNC TO SKD-DTC.
           MOVE ST-DNO TO SKD-DNO.
           MOVE ST-GNO TO SKD-GNO.
           MOVE ST-HCD TO SKD-HCD.
           MOVE ST-SU TO SKD-SU.
           MOVE ST-T TO SKD-T.
           MOVE ST-KIN TO SKD-KIN.
           MOVE ST-DC TO SKD-DC.
           MOVE ST-CSC TO SKD-CSC.
           MOVE ST-SKD TO SKD-SKD.
           MOVE ST-TNC TO SKD-TNC.
           MOVE T-BC TO SKD-BMC.
           MOVE T-DCC TO SKD-DCC.
           MOVE ST-CCD TO SKD-CCD.
           MOVE ST-TCD2 TO SKD-TCD2.
       SKDW-020.
      *           WRITE SKD-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            SKDF_PNAME1 SKDF_LNAME SKD-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME29" E-ME29 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SKDW-030
           END-IF
           IF  SKD-DNO NOT = WK-DNO
               MOVE SKD-KEY TO W-KEY
           END-IF
           GO TO SKDW-EX.
       SKDW-030.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 9 TO W-END
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SKDW-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           MOVE "SKDF         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
           GO TO SKDW-010.
       SKDW-EX.
           EXIT.
       SKDR-RTN.
           MOVE ST-BI TO SKD-BI.
           MOVE ST-HNO TO SKD-HNO.
           MOVE ST-SHZ TO SKD-SHZ.
           MOVE W-KSU TO SKD-KSU.
      *           REWRITE SKD-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SKDF_PNAME1 SKDF_LNAME SKD-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME30" E-ME30 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 9 TO W-END
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       SKDR-EX.
           EXIT.
       SKKW-RTN.
           INITIALIZE SKDK-R.
           MOVE SKD-R TO SKDK-R.
      *           WRITE SKDK-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            SKDKF_PNAME1 SKDKF_LNAME SKDK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME32" E-ME32 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SKKW-020
           END-IF
           GO TO SKKW-EX.
       SKKW-020.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 9 TO W-END
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SKKW-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE SKDKF_IDLST SKDKF_PNAME1.
           MOVE "SKDKF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" SKDKF_PNAME1 "SHARED" BY REFERENCE SKDKF_IDLST "1"
            "SKDK-KEY" BY REFERENCE SKDK-KEY.
           GO TO SKKW-RTN.
       SKKW-EX.
           EXIT.
       DNO2-RTN.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
      *           READ HKBM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO DNO2-020
           END-IF
           MOVE HKB-DAI TO W-DAI.
           GO TO DNO2-EX.
       DNO2-020.
           INITIALIZE HKB-R.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
      *           WRITE HKB-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME19" E-ME19 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           INITIALIZE W-DAI.
       DNO2-EX.
           EXIT.
       TUK-RTN.
           COPY LDAIP.
           IF  W-DAI1 = "999"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME40" E-ME40 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           MOVE W-DAI TO HKB-DAI.
      *           REWRITE HKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME20" E-ME20 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 8 TO W-END
               GO TO TUK-EX
           END-IF.
       TUK-010.
           INITIALIZE TUK-R.
           MOVE W-TCD TO TUK-TCD TUK-TCD2.
           MOVE W-DAI TO TUK-DAI.
           MOVE W-DATE TO TUK-DATE.
           MOVE W-DC TO TUK-DC.
           MOVE W-KIN TO TUK-KIN.
           MOVE W-SHZ TO TUK-SHZ.
           MOVE W-DNO TO TUK-DNO.
           MOVE W-GNO TO TUK-GNO.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO T-TNC T-DCC
           END-IF
           MOVE T-TNC TO TUK-TNC.
           MOVE T-DCC TO TUK-DCC.
           MOVE 0 TO TUK-BMC.
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
               GO TO TUK-020
           END-IF
           GO TO TUK-EX.
       TUK-020.
           IF  ERR-STAT NOT = "24"
               MOVE 8 TO W-END
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TUK-EX
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
           GO TO TUK-010.
       TUK-EX.
           EXIT.
       DNG-RTN.
           IF  W-FTCD = ZERO
               GO TO DNG-010
           END-IF
           MOVE W-FTCD TO T-KEY.
      *           READ T-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DNG-010
           END-IF
           IF  W-FNG NOT > T-DNG
               GO TO DNG-010
           END-IF
           MOVE W-FNG TO T-DNG.
           IF  T-ENG NOT = ZERO
               MOVE ZERO TO T-ENG
           END-IF
      *           REWRITE T-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            T-M_PNAME1 T-M_LNAME T-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME25" E-ME25 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       DNG-010.
           IF  W-FHCD = ZERO
               GO TO DNG-EX
           END-IF
           MOVE W-FHCD TO HI-KEY.
      *           READ HI-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DNG-EX
           END-IF
           IF  W-FNG > HI-DNG
               MOVE 1 TO W-FDC
               MOVE W-FNG TO HI-DNG
           END-IF
           IF  W-FC = 1
               IF  W-FNGD > HI-UNG
                   MOVE 1 TO W-FDC
                   MOVE W-FNGD TO HI-UNG
               END-IF
           END-IF
           IF  W-FC = 3
               IF  W-FNGD > HI-NNG
                   MOVE 1 TO W-FDC
                   MOVE W-FNGD TO HI-NNG
               END-IF
           END-IF
           IF  W-FDC = 0
               GO TO DNG-EX
           END-IF
      *           REWRITE HI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HI-M_PNAME1 HI-M_LNAME HI-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME26" E-ME26 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       DNG-EX.
           EXIT.
       TTMC-RTN.
           COMPUTE WT-TZZ = TT-TZZ + TT-TZZZ.
           COMPUTE WT-TUA = TT-TUA + TT-TUAZ.
           COMPUTE WT-TNB = TT-TNB + TT-TNBZ.
           COMPUTE WT-TNK = TT-TNK + TT-TNKZ.
           COMPUTE WT-TUZ = TT-TUZ + TT-TUZZ.
           COMPUTE WT-TUZW = WT-TZZ + WT-TUA - WT-TNB - WT-TNK.
           IF  WT-TUZ NOT = WT-TUZW
               CALL "SD_Output" USING "D-ZM" D-ZM "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "D-ZMC" D-ZMC "p" RETURNING RESU
           END-IF.
       TTMC-EX.
           EXIT.

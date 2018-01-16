       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD530.
       AUTHOR. S-NAKAO.
       DATE-WRITTEN. 1974-05-03.
      *********************************************************
      *    PROGRAM         :  入金　累積・更新・クリア　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
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
             03  W-FTCD       PIC  9(004).
             03  W-FNG        PIC  9(006).
           02  W-KEY.
             03  WK-TCD       PIC  9(004).
             03  F            PIC  X(009).
             03  WK-DNO       PIC  9(006).
             03  F            PIC  X(001).
           02  W-DMM          PIC  9(001).
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
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LITTM.
           COPY LSNYUR.
           COPY LIHKBM.
           COPY LITUKF.
           COPY LISKDF.
      *FD  NYU-F
       01  NYU-F_HMD535.
           02  NYU-F_PNAME1   PIC  X(004) VALUE "NYUF".
           02  F              PIC  X(001).
           02  NYU-F_LNAME    PIC  X(012) VALUE "NYU-F_HMD535".
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
      *FD  NYURYR
       01  NYURYR_HMD535.
           02  NYURYR_PNAME1  PIC  X(006) VALUE "NYURYR".
           02  F              PIC  X(001).
           02  NYURYR_LNAME   PIC  X(013) VALUE "NYURYR_HMD535".
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　入金　累積・更新・クリア　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-ZM.
             03  FILLER.
               04  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
               04  FILLER  PIC  9(004).
             03  FILLER.
               04  FILLER  PIC  N(006) VALUE "　　　前月残".
               04  FILLER  PIC  N(006) VALUE "　　　売　上".
               04  FILLER  PIC  N(006) VALUE "　　　値　引".
               04  FILLER  PIC  N(006) VALUE "　　　入　金".
               04  FILLER  PIC  N(006) VALUE "　　　売掛残".
             03  FILLER.
               04  FILLER  PIC  N(003) VALUE "本　体".
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ----,---,--9 .
             03  FILLER.
               04  FILLER  PIC  N(003) VALUE "消費税".
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ----,---,--9 .
             03  FILLER.
               04  FILLER  PIC  N(003) VALUE "合　計".
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ----,---,--9 .
           02  D-ZMC.
             03  FILLER.
               04  FILLER  PIC  X(004) VALUE "    ".
               04  FILLER  PIC  X(004) VALUE "    ".
             03  FILLER.
               04  FILLER  PIC  X(012) VALUE "            ".
               04  FILLER  PIC  X(012) VALUE "            ".
               04  FILLER  PIC  X(012) VALUE "            ".
               04  FILLER  PIC  X(012) VALUE "            ".
               04  FILLER  PIC  X(012) VALUE "            ".
             03  FILLER.
               04  FILLER  PIC  X(012) VALUE "            ".
               04  FILLER  PIC  X(012) VALUE "            ".
               04  FILLER  PIC  X(012) VALUE "            ".
               04  FILLER  PIC  X(012) VALUE "            ".
               04  FILLER  PIC  X(012) VALUE "            ".
             03  FILLER.
               04  FILLER  PIC  X(012) VALUE "            ".
               04  FILLER  PIC  X(012) VALUE "            ".
               04  FILLER  PIC  X(012) VALUE "            ".
               04  FILLER  PIC  X(012) VALUE "            ".
               04  FILLER  PIC  X(012) VALUE "            ".
             03  FILLER.
               04  FILLER  PIC  X(012) VALUE "            ".
               04  FILLER  PIC  X(012) VALUE "            ".
               04  FILLER  PIC  X(012) VALUE "            ".
               04  FILLER  PIC  X(012) VALUE "            ".
               04  FILLER  PIC  X(012) VALUE "            ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1.
               04  FILLER  PIC  X(036) VALUE
                    "***  NYUF DELETE ｴﾗｰ (      - )  ***".
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  9(001).
             03  E-ME2   PIC  X(025) VALUE
                  "***  NYURF WRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(026) VALUE
                  "***  NYURYR WRITE ｴﾗｰ  ***".
             03  E-ME8   PIC  X(026) VALUE
                  "***  NYUF REWRITE ｴﾗｰ  ***".
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
             03  E-ME27  PIC  X(025) VALUE
                  "***  TTM REWRITE ｴﾗｰ  ***".
             03  E-ME28  PIC  X(026) VALUE
                  "***   TUKF WRITE ｴﾗｰ   ***".
             03  E-ME29  PIC  X(026) VALUE
                  "***   SKDF WRITE ｴﾗｰ   ***".
             03  E-ME30  PIC  X(026) VALUE
                  "***  SKDF REWRITE ｴﾗｰ  ***".
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
            "C-MID" " " "0" "0" "330" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING 
            "08C-MID" "X" "20" "30" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "47" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "514" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZM" " " "0" "0" "266" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-ZM" " " "14" "0" "8" " " "D-ZM" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-ZM" "X" "14" "8" "4" " " "01D-ZM" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-ZM" "9" "14" "13" "4" "0101D-ZM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-ZM" BY REFERENCE TT-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-ZM" " " "15" "0" "60" "01D-ZM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-ZM" "N" "15" "14" "12" " " "02D-ZM" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-ZM" "N" "15" "26" "12" "0102D-ZM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-ZM" "N" "15" "38" "12" "0202D-ZM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0402D-ZM" "N" "15" "50" "12" "0302D-ZM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0502D-ZM" "N" "15" "62" "12" "0402D-ZM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-ZM" " " "16" "0" "66" "02D-ZM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103D-ZM" "N" "16" "8" "6" " " "03D-ZM" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203D-ZM" "----,---,--9" "16" "14" "12" "0103D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0203D-ZM" BY REFERENCE TT-TZZ "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0303D-ZM" "----,---,--9" "16" "26" "12" "0203D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0303D-ZM" BY REFERENCE TT-TUA "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0403D-ZM" "----,---,--9" "16" "38" "12" "0303D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0403D-ZM" BY REFERENCE TT-TNB "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0503D-ZM" "----,---,--9" "16" "50" "12" "0403D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0503D-ZM" BY REFERENCE TT-TNK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0603D-ZM" "----,---,--9" "16" "62" "12" "0503D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0603D-ZM" BY REFERENCE TT-TUZ "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-ZM" " " "17" "0" "66" "03D-ZM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0104D-ZM" "N" "17" "8" "6" " " "04D-ZM" RETURNING RESU.
       CALL "SD_Init" USING 
            "0204D-ZM" "----,---,--9" "17" "14" "12" "0104D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0204D-ZM" BY REFERENCE TT-TZZZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0304D-ZM" "----,---,--9" "17" "26" "12" "0204D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0304D-ZM" BY REFERENCE TT-TUAZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0404D-ZM" "----,---,--9" "17" "38" "12" "0304D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0404D-ZM" BY REFERENCE TT-TNBZ "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0504D-ZM" "----,---,--9" "17" "50" "12" "0404D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0504D-ZM" BY REFERENCE TT-TNKZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0604D-ZM" "----,---,--9" "17" "62" "12" "0504D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0604D-ZM" BY REFERENCE TT-TUZZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-ZM" " " "18" "0" "66" "04D-ZM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0105D-ZM" "N" "18" "8" "6" " " "05D-ZM" RETURNING RESU.
       CALL "SD_Init" USING 
            "0205D-ZM" "----,---,--9" "18" "14" "12" "0105D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0205D-ZM" BY REFERENCE WT-TZZ "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0305D-ZM" "----,---,--9" "18" "26" "12" "0205D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0305D-ZM" BY REFERENCE WT-TUA "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0405D-ZM" "----,---,--9" "18" "38" "12" "0305D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0405D-ZM" BY REFERENCE WT-TNB "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0505D-ZM" "----,---,--9" "18" "50" "12" "0405D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0505D-ZM" BY REFERENCE WT-TNK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0605D-ZM" "----,---,--9" "18" "62" "12" "0505D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0605D-ZM" BY REFERENCE WT-TUZ "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZMC" " " "0" "0" "248" "D-ZM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-ZMC" " " "14" "0" "8" " " "D-ZMC" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-ZMC" "X" "14" "8" "4" " " "01D-ZMC" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-ZMC" "X" "14" "13" "4" "0101D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-ZMC" " " "15" "0" "60" "01D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-ZMC" "X" "15" "14" "12" " " "02D-ZMC" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-ZMC" "X" "15" "26" "12" "0102D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-ZMC" "X" "15" "38" "12" "0202D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402D-ZMC" "X" "15" "50" "12" "0302D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0502D-ZMC" "X" "15" "62" "12" "0402D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-ZMC" " " "16" "0" "60" "02D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103D-ZMC" "X" "16" "14" "12" " " "03D-ZMC" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203D-ZMC" "X" "16" "26" "12" "0103D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0303D-ZMC" "X" "16" "38" "12" "0203D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0403D-ZMC" "X" "16" "50" "12" "0303D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0503D-ZMC" "X" "16" "62" "12" "0403D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-ZMC" " " "17" "0" "60" "03D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0104D-ZMC" "X" "17" "14" "12" " " "04D-ZMC" RETURNING RESU.
       CALL "SD_Init" USING 
            "0204D-ZMC" "X" "17" "26" "12" "0104D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0304D-ZMC" "X" "17" "38" "12" "0204D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0404D-ZMC" "X" "17" "50" "12" "0304D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0504D-ZMC" "X" "17" "62" "12" "0404D-ZMC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-ZMC" " " "18" "0" "60" "04D-ZMC" " " RETURNING RESU.
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
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "360" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "360" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" " " "24" "0" "43" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME1" "X" "24" "15" "36" " " "E-ME1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME1" "9" "24" "37" "6" "01E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME1" BY REFERENCE N-NO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03E-ME1" "9" "24" "44" "1" "02E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03E-ME1" BY REFERENCE N-GNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "25" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "26" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "26" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME19" "X" "24" "15" "24" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME20" "X" "24" "15" "26" "E-ME19" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME21" "X" "24" "15" "17" "E-ME20" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME22" "X" "24" "15" "16" "E-ME21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME25" "X" "24" "15" "24" "E-ME22" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME27" "X" "24" "15" "25" "E-ME25" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME28" "X" "24" "15" "26" "E-ME27" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME29" "X" "24" "15" "26" "E-ME28" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME30" "X" "24" "15" "26" "E-ME29" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME40" "X" "24" "15" "26" "E-ME30" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TCD" "9" "24" "45" "4" "E-ME40" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE WK-TCD "4" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "I-O" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
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
            "I-O" NYU-F_PNAME1 " " BY REFERENCE NYU-F_IDLST "1"
            "N-KEY" BY REFERENCE N-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" NYUR-F_PNAME1 "SHARED" BY REFERENCE
            NYUR-F_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" NYURYR_PNAME1 " " BY REFERENCE NYURYR_IDLST "0".
           MOVE ZERO TO W-NKD.
       M-15.
      *           READ NYU-F NEXT RECORD AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           IF  N-DPC = 0
               GO TO M-15
           END-IF.
       M-20.
           MOVE ZERO TO NYUR-R.
           MOVE NYU-R TO NYUR-R.
      *           WRITE NYUR-R.
      *//////////////
           CALL "DB_Insert" USING
            NYUR-F_PNAME1 NYUR-F_LNAME NYUR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-45
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
            "EXTEND" NYUR-F_PNAME1 "SHARED" BY REFERENCE
            NYUR-F_IDLST "0".
           GO TO M-20.
       M-25.
           MOVE ZERO TO NYURY-R.
           MOVE NYU-R TO NYURY-R.
      *           WRITE NYURY-R.
      *//////////////
           CALL "DB_Insert" USING
            NYURYR_PNAME1 NYURYR_LNAME NYURY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-30
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME7" E-ME7 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-45
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
           GO TO M-25.
       M-30.
           IF  WN-TCD = ZERO
               MOVE N-DATE TO WN-DATE
               MOVE N-TCD TO WN-TCD
               MOVE N-NO TO WN-NO
               MOVE N-TNC TO WN-TNC
               MOVE N-BC TO WN-BC
               MOVE N-DCC TO WN-DCC
               MOVE N-SKD TO WN-SKD
           ELSE
               IF  N-NO NOT = WN-NO
                   GO TO M-35
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
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
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
               GO TO M-45
           END-IF
      *
           MOVE ZERO TO W-FD.
           MOVE 2 TO W-FC.
           MOVE NUR-TCD TO W-FTCD.
           MOVE NUR-NG TO W-FNG.
           PERFORM DNG-RTN THRU DNG-EX.
           GO TO M-15.
       M-35.
           MOVE WN-TCD TO TT-KEY.
      *           READ TT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TT-M_PNAME1 BY REFERENCE TT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME22" E-ME22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-45
           END-IF
           ADD WN-KIN TO TT-TNK.
           ADD WN-SHZ TO TT-TNKZ.
           SUBTRACT WN-KIN FROM TT-TUZ.
           SUBTRACT WN-SHZ FROM TT-TUZZ.
           COMPUTE WT-TZZ = TT-TZZ + TT-TZZZ.
           COMPUTE WT-TUA = TT-TUA + TT-TUAZ.
           COMPUTE WT-TNB = TT-TNB + TT-TNBZ.
           COMPUTE WT-TNK = TT-TNK + TT-TNKZ.
           COMPUTE WT-TUZ = TT-TUZ + TT-TUZZ.
           COMPUTE WT-TUZW = WT-TZZ + WT-TUA - WT-TNB - WT-TNK.
           IF  WT-TUZ NOT = WT-TUZW
               CALL "SD_Output" USING "D-ZM" D-ZM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "D-ZMC" D-ZMC "p" RETURNING RESU
           END-IF
      *           REWRITE TT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TT-M_PNAME1 TT-M_LNAME TT-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME27" E-ME27 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-45
           END-IF
      *-------------------------------------------------------------------------
           PERFORM SKDW-RTN THRU SKDW-EX.
           IF  W-END = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-45
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
           GO TO M-30.
       M-40.
           IF  WN-TCD = ZERO
               GO TO M-45
           END-IF
           MOVE WN-TCD TO TT-KEY.
      *           READ TT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TT-M_PNAME1 BY REFERENCE TT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME22" E-ME22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-45
           END-IF
           ADD WN-KIN TO TT-TNK.
           ADD WN-SHZ TO TT-TNKZ.
           SUBTRACT WN-KIN FROM TT-TUZ.
           SUBTRACT WN-SHZ FROM TT-TUZZ.
           COMPUTE WT-TZZ = TT-TZZ + TT-TZZZ.
           COMPUTE WT-TUA = TT-TUA + TT-TUAZ.
           COMPUTE WT-TNB = TT-TNB + TT-TNBZ.
           COMPUTE WT-TNK = TT-TNK + TT-TNKZ.
           COMPUTE WT-TUZ = TT-TUZ + TT-TUZZ.
           COMPUTE WT-TUZW = WT-TZZ + WT-TUA - WT-TNB - WT-TNK.
           IF  WT-TUZ NOT = WT-TUZW
               CALL "SD_Output" USING "D-ZM" D-ZM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "D-ZMC" D-ZMC "p" RETURNING RESU
           END-IF
      *           REWRITE TT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TT-M_PNAME1 TT-M_LNAME TT-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME27" E-ME27 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-45
           END-IF
           PERFORM SKDW-RTN THRU SKDW-EX.
           IF  W-END = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-45
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
       M-45.
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
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
           CALL "DB_F_Open" USING
            "I-O" NYU-F_PNAME1 " " BY REFERENCE NYU-F_IDLST "1"
            "N-KEY" BY REFERENCE N-KEY.
       M-50.
      *           READ NYU-F NEXT RECORD AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           IF  N-RSC NOT = 1
               GO TO M-50
           END-IF
      *           DELETE NYU-F INVALID KEY
      *///////////////
           CALL "DB_Delete" USING NYU-F_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-50.
       M-55.
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       SKDW-RTN.
           INITIALIZE SKD-R.
           MOVE ZERO TO SKD-HNO.
           MOVE SPACE TO SKD-BI.
           MOVE WN-TCD TO SKD-TCD.
           MOVE WN-DATE TO SKD-DATE.
           MOVE 3 TO SKD-DTC.
           MOVE WN-NO TO SKD-DNO.
           MOVE WN-KIN TO SKD-KIN.
           MOVE WN-SKD TO SKD-SKD.
           MOVE WN-TNC TO SKD-TNC.
           MOVE WN-BC TO SKD-BMC.
           MOVE WN-DCC TO SKD-DCC.
           MOVE WN-SHZ TO SKD-SHZ.
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
                USER_ID BY REFERENCE COMPLETION_CODE 255
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
           GO TO SKDW-RTN.
       SKDW-EX.
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
           MOVE W-FTCD TO T-KEY.
      *           READ T-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DNG-EX
           END-IF
           IF  W-FNG NOT > T-DNG
               GO TO DNG-EX
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
       DNG-EX.
           EXIT.

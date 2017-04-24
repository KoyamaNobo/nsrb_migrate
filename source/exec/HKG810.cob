       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG810.
      ******************************************************************
      *    PROGRAM         :  請求書データ　請求日修正                 *
      *    PRINTER TYPE    :  JIPS                                     *
      *    SCREEN          :  ******                                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-SEN          PIC  9(001).
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGD   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-DATE         PIC  9(008).
           02  W-ONGP.
             03  W-ONG.
               04  W-ONEN     PIC  9(004).
               04  W-OGET     PIC  9(002).
             03  W-OPEY       PIC  9(002).
           02  W-NNGP.
             03  W-NNG.
               04  W-NNEN     PIC  9(004).
               04  W-NGET     PIC  9(002).
             03  W-NPEY       PIC  9(002).
           02  W-TCD          PIC  9(004).
           02  W-DNO          PIC  9(006).
           02  W-MD.
             03  W-SU         PIC S9(006)V9(02).
             03  W-T          PIC S9(006)V9(02).
             03  W-KIN        PIC S9(009).
           02  W-SHZ          PIC S9(007).
           02  W-TKIN         PIC S9(009).
           02  W-DMM          PIC  9(001).
           02  W-L.
             03  W-L1         PIC  9(002).
             03  W-L2         PIC  9(002).
           02  CNT            PIC  9(004).
           02  W-EC           PIC  9(001).
           02  W-NAME         PIC  N(024).
           02  W-DTC          PIC  9(001).
           02  W-DC           PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LISKDF.
           COPY LISKDK.
           COPY LITSKF.
           COPY LITUKF.
           COPY LITM.
           COPY LIHIM.
           COPY LIKHM.
           COPY LIJM.
      *FD  TAZM
       01  TAZM_HKG810.
           02  TAZM_PNAME1    PIC  X(004) VALUE "TAZM".
           02  F              PIC  X(001).
           02  TAZM_LNAME     PIC  X(011) VALUE "TAZM_HKG810".
           02  F              PIC  X(001).
           02  TAZM_KEY1      PIC  X(100) VALUE SPACE.
           02  TAZM_KEY2      PIC  X(100) VALUE SPACE.
           02  TAZM_SORT      PIC  X(100) VALUE SPACE.
           02  TAZM_IDLST     PIC  X(100) VALUE SPACE.
           02  TAZM_RES       USAGE  POINTER.
       01  TAZ-R.
           02  TAZ-KEY.
             03  TAZ-TCD      PIC  9(004).
             03  TAZ-HCD      PIC  9(006).
           02  TAZ-AZS        PIC S9(005).
           02  TAZ-AAS        PIC S9(005).
           02  TAZ-SZS        PIC S9(005).
           02  TAZ-SAS        PIC S9(005).
           02  F              PIC  X(006).
           02  TAZ-NG         PIC  9(006).
       77  F                  PIC  X(001).
      *FD  RSTRANYR
       01  RSTRANYR_HKG810.
           02  RSTRANYR_PNAME1  PIC  X(010) VALUE "R-STRANYRK".
           02  F                PIC  X(001).
           02  RSTRANYR_LNAME   PIC  X(015) VALUE "RSTRANYR_HKG810".
           02  F                PIC  X(001).
           02  RSTRANYR_KEY1    PIC  X(100) VALUE SPACE.
           02  RSTRANYR_KEY2    PIC  X(100) VALUE SPACE.
           02  RSTRANYR_KEY3    PIC  X(100) VALUE SPACE.
           02  RSTRANYR_KEY4    PIC  X(100) VALUE SPACE.
           02  RSTRANYR_SORT    PIC  X(100) VALUE SPACE.
           02  RSTRANYR_IDLST   PIC  X(100) VALUE SPACE.
           02  RSTRANYR_RES     USAGE  POINTER.
       01  RSTRANY-R.
           02  RSTRANY-KEY.
             03  RSTRANY-DNO  PIC  9(006).
             03  RSTRANY-GNO  PIC  9(001).
             03  RSTRANY-DATE PIC  9(008).
             03  RSTRANY-TCD  PIC  9(004).
           02  F              PIC  X(095).
           02  RSTRANY-SKD    PIC  9(008).
           02  F              PIC  X(006).
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
           02  FILLER  PIC  N(018) VALUE
                "【　　請求書データ　請求日修正　　】".
           02  FILLER.
             03  FILLER  PIC  X(050) VALUE
                  "一括修正 = 1  ,  得意先別修正 = 2  ,  個別修正 = 3".
             03  FILLER  PIC  X(008) VALUE " .....  ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  A-TCD   PIC  9(004).
           02  A-DNO   PIC  9(006).
           02  FILLER.
             03  A-ONEN  PIC  9(004).
             03  A-OGET  PIC  9(002).
             03  A-OPEY  PIC  9(002).
             03  A-NNEN  PIC  9(004).
             03  A-NGET  PIC  9(002).
             03  A-NPEY  PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MID1.
             03  FILLER.
               04  FILLER  PIC  X(045) VALUE
                    "                                             ".
               04  FILLER  PIC  X(013) VALUE "             ".
             03  FILLER.
               04  FILLER  PIC  N(003) VALUE "請求日".
               04  FILLER  PIC  X(038) VALUE
                    "    年   月   日  →      年   月   日".
           02  D-EM.
             03  01D-EM  PIC Z,ZZ9 .
             03  FILLER  PIC  N(003) VALUE "枚修正".
           02  D-MID2.
             03  FILLER.
               04  FILLER  PIC  X(045) VALUE
                    "                                             ".
               04  FILLER  PIC  X(013) VALUE "             ".
             03  FILLER.
               04  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
               04  FILLER  PIC  N(004) VALUE "得意先名".
             03  FILLER  PIC  X(009) VALUE "終了=ｆ･9".
             03  FILLER.
               04  FILLER  PIC  N(003) VALUE "請求日".
               04  FILLER  PIC  X(038) VALUE
                    "    年   月   日  →      年   月   日".
           02  D-MID3.
             03  FILLER.
               04  FILLER  PIC  N(003) VALUE "伝票№".
               04  FILLER  PIC  N(002) VALUE "日付".
             03  FILLER.
               04  FILLER  PIC  N(002) VALUE "伝区".
               04  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
               04  FILLER  PIC  N(005) VALUE "品　　　名".
               04  FILLER  PIC  N(002) VALUE "数量".
               04  FILLER  PIC  N(002) VALUE "単価".
               04  FILLER  PIC  N(002) VALUE "金額".
             03  FILLER.
               04  FILLER  PIC  X(007) VALUE "消費税:".
               04  FILLER  PIC  X(005) VALUE "合計:".
           02  D-MD1.
             03  FILLER.
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  9(002).
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  9(002).
           02  D-TNA   PIC  N(026).  
           02  D-MD21.
             03  FILLER.
               04  FILLER  PIC 9999/99/99 .
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  9(002).
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  9(002).
           02  D-MD22.
             03  FILLER.
               04  FILLER  PIC  9(001).
               04  FILLER  PIC  X(006).
               04  FILLER  PIC  N(024).
             03  FILLER.
               04  FILLER  PIC --,---,--9.99 .
               04  FILLER  PIC ----,--9.99.
               04  FILLER  PIC ----,---,--9 .
           02  D-TD.
             03  FILLER  PIC --,---,--9 .
             03  FILLER  PIC ----,---,--9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(026) VALUE
                  "***  SKDF REWRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  ﾄｸｲｻｷ ﾅｼ  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME5.
               04  FILLER   PIC  X(025) VALUE
                    "***  ｾｲｷｭｳｼｮ ﾊｯｺｳｽﾞﾐ  ***".
               04  02E-ME5  PIC  9999/99/99 .
             03  E-ME6   PIC  X(026) VALUE
                  "***  TUKF REWRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(025) VALUE
                  "***  SKDKF WRITE ｴﾗｰ  ***".
             03  E-ME8   PIC  X(026) VALUE
                  "***  SKDKF DELETE ｴﾗｰ  ***".
             03  E-ME9   PIC  X(018) VALUE
                  "***  SKDKF ﾅｼ  ***".
             03  E-ME10  PIC  X(018) VALUE
                  "***  ﾋﾂﾞｹ ｴﾗｰ  ***".
             03  E-ME11  PIC  X(024) VALUE
                  "***  TAZM WRITE ｴﾗｰ  ***".
             03  E-ME12  PIC  X(026) VALUE
                  "***  TAZM REWRITE ｴﾗｰ  ***".
             03  E-ME13  PIC  X(017) VALUE
                  "***  TAZM ﾅｼ  ***".
             03  E-ME14  PIC  X(032) VALUE
                  "***  R-STRANYRK REWRITE ｴﾗｰ  ***".
             03  E-ME15  PIC  X(023) VALUE
                  "***  R-STRANYRK ﾅｼ  ***".
             03  E-KEY   PIC  X(020).
             03  E-TCD   PIC  9(004).
             03  E-TUK   PIC  X(014).
             03  E-TAZ   PIC  X(010).
             03  E-STR   PIC  X(019).
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
            "C-MID" " " "0" "0" "116" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "22" "36" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" " " "4" "0" "58" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID" "X" "4" "11" "50" " " "02C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID" "X" "4" "61" "8" "0102C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "22" "29" "22" "02C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "28" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "4" "68" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "3" "6" "4" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DNO" "9" "5" "8" "6" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DNO" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "5" "0" "16" "A-DNO" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ONEN" "9" "5" "42" "4" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ONEN" BY REFERENCE W-ONEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-OGET" "9" "5" "49" "2" "A-ONEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-OGET" BY REFERENCE W-OGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-OPEY" "9" "5" "54" "2" "A-OGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-OPEY" BY REFERENCE W-OPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NNEN" "9" "5" "64" "4" "A-OPEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NNEN" BY REFERENCE W-NNEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NGET" "9" "5" "71" "2" "A-NNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NGET" BY REFERENCE W-NGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NPEY" "9" "5" "76" "2" "A-NGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NPEY" BY REFERENCE W-NPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "46" "1" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "487" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID1" " " "0" "0" "102" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MID1" " " "4" "0" "58" " " "D-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
           "0101D-MID1" "X" "4" "11" "45" " " "01D-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-MID1" "X" "4" "56" "13" "0101D-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MID1" " " "5" "0" "44" "01D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-MID1" "N" "5" "34" "6" " " "02D-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-MID1" "X" "5" "42" "38" "0102D-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-EM" " " "15" "0" "11" "D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-EM" "Z,ZZ9" "15" "35" "5" " " "D-EM" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-EM" BY REFERENCE CNT "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-EM" "N" "15" "40" "6" "01D-EM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID2" " " "0" "0" "123" "D-EM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MID2" " " "4" "0" "58" " " "D-MID2" RETURNING RESU.
       CALL "SD_Init" USING 
           "0101D-MID2" "X" "4" "11" "45" " " "01D-MID2" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-MID2" "X" "4" "56" "13" "0101D-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MID2" " " "3" "0" "12" "01D-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-MID2" "X" "3" "1" "4" " " "02D-MID2" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-MID2" "N" "3" "12" "8" "0102D-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MID2" "X" "4" "6" "9" "02D-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-MID2" " " "5" "0" "44" "03D-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0104D-MID2" "N" "5" "34" "6" " " "04D-MID2" RETURNING RESU.
       CALL "SD_Init" USING 
            "0204D-MID2" "X" "5" "42" "38" "0104D-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID3" " " "0" "0" "52" "D-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MID3" " " "5" "0" "10" " " "D-MID3" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-MID3" "N" "5" "1" "6" " " "01D-MID3" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-MID3" "N" "5" "16" "4" "0101D-MID3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MID3" " " "7" "0" "30" "01D-MID3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-MID3" "N" "7" "1" "4" " " "02D-MID3" RETURNING RESU.
       CALL "SD_Init" USING 
           "0202D-MID3" "X" "7" "7" "4" "0102D-MID3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-MID3" "N" "7" "13" "10" "0202D-MID3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402D-MID3" "N" "7" "51" "4" "0302D-MID3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0502D-MID3" "N" "7" "62" "4" "0402D-MID3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0602D-MID3" "N" "7" "77" "4" "0502D-MID3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MID3" " " "20" "0" "12" "02D-MID3" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "0103D-MID3" "X" "20" "45" "7" " " "03D-MID3" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203D-MID3" "X" "20" "64" "5" "0103D-MID3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MD1" " " "0" "0" "12" "D-MID3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MD1" " " "5" "0" "12" " " "D-MD1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-MD1" "9" "5" "42" "4" " " "01D-MD1" RETURNING RESU.
       CALL "SD_From" USING 
            "0101D-MD1" BY REFERENCE W-ONEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-MD1" "9" "5" "49" "2" "0101D-MD1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-MD1" BY REFERENCE W-OGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0301D-MD1" "9" "5" "64" "4" "0201D-MD1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0301D-MD1" BY REFERENCE W-NNEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0401D-MD1" "9" "5" "71" "2" "0301D-MD1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0401D-MD1" BY REFERENCE W-NGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "3" "21" "52" "D-MD1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MD21" " " "0" "0" "22" "D-TNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MD21" " " "5" "0" "22" " " "D-MD21" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-MD21" "9999/99/99" "5" "21" "10" " " "01D-MD21"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0101D-MD21" BY REFERENCE SKD-DATE "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-MD21" "9" "5" "42" "4" "0101D-MD21" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-MD21" BY REFERENCE W-ONEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0301D-MD21" "9" "5" "49" "2" "0201D-MD21" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0301D-MD21" BY REFERENCE W-OGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0401D-MD21" "9" "5" "64" "4" "0301D-MD21" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0401D-MD21" BY REFERENCE W-NNEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0501D-MD21" "9" "5" "71" "2" "0401D-MD21" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0501D-MD21" BY REFERENCE W-NGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MD22" " " "0" "0" "91" "D-MD21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MD22" " " "W-L1" "0" "55" " " "D-MD22" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-MD22" "9" "W-L1" "3" "1" " " "01D-MD22"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0101D-MD22" BY REFERENCE SKD-DC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-MD22" "X" "W-L1" "6" "6" "0101D-MD22" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-MD22" BY REFERENCE SKD-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0301D-MD22" "N" "W-L1" "13" "48" "0201D-MD22" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0301D-MD22" BY REFERENCE W-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MD22" " " "W-L1" "0" "36" "01D-MD22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-MD22" "--,---,--9.99" "W-L1" "45" "13" " "
            "02D-MD22" RETURNING RESU.
       CALL "SD_From" USING 
            "0102D-MD22" BY REFERENCE W-SU "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-MD22" "----,--9.99" "W-L1" "58" "11" "0102D-MD22"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0202D-MD22" BY REFERENCE W-T "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-MD22" "----,---,--9" "W-L1" "69" "12" "0202D-MD22"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0302D-MD22" BY REFERENCE W-KIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TD" " " "20" "0" "22" "D-MD22" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TD" "--,---,--9" "20" "52" "10" " " "D-TD"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-TD" BY REFERENCE W-SHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TD" "----,---,--9" "20" "69" "12" "01D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-TD" BY REFERENCE W-TKIN "9" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "416" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "416" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "26" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "18" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" " " "24" "0" "35" "E-ME4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME5" "X" "0" "15" "25" " " "E-ME5"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME5" "9999/99/99" "0" "45" "10" "01E-ME5" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME5" BY REFERENCE W-DATE "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "26" "E-ME5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "25" "E-ME6" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "26" "E-ME7" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "18" "E-ME8" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "18" "E-ME9" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "24" "E-ME10" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "26" "E-ME11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "17" "E-ME12" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME14" "X" "24" "15" "32" "E-ME13" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME15" "X" "24" "15" "23" "E-ME14" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "45" "20" "E-ME15" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE SKD-KEY "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TCD" "9" "24" "45" "4" "E-KEY" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE SKD-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TUK" "X" "24" "45" "14" "E-TCD" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-TUK" BY REFERENCE TUK-KEY "14" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TAZ" "X" "24" "45" "10" "E-TUK" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-TAZ" BY REFERENCE TAZ-KEY "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STR" "X" "24" "50" "19" "E-TAZ" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STR" BY REFERENCE RSTRANY-KEY "19" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SEN < 1 OR > 3
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           IF  W-SEN > 1
               GO TO M-20
           END-IF
           PERFORM IKS-RTN THRU IKS-EX.
           IF  ESTAT = BTB
               GO TO M-05
           END-IF
           GO TO M-95.
       M-20.
           IF  W-SEN = 2
               PERFORM TBS-RTN THRU TBS-EX
           ELSE
               PERFORM KBS-RTN THRU KBS-EX
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *=================================================================
       IKS-RTN.
           CALL "SD_Output" USING "D-MID1" D-MID1 "p" RETURNING RESU.
           MOVE W-NG TO W-ONG W-NNG.
           CALL "SD_Output" USING "A-ONEN" A-ONEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-OGET" A-OGET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NNEN" A-NNEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NGET" A-NGET "p" RETURNING RESU.
       IKS-020.
           CALL "SD_Accept" USING BY REFERENCE A-ONEN "A-ONEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO IKS-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO IKS-020
           END-IF.
       IKS-040.
           CALL "SD_Accept" USING BY REFERENCE A-OGET "A-OGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO IKS-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO IKS-040
           END-IF
           IF  W-OGET < 1 OR > 12
               GO TO IKS-040
           END-IF.
       IKS-060.
           CALL "SD_Accept" USING BY REFERENCE A-OPEY "A-OPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO IKS-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO IKS-060
           END-IF
           IF  W-OPEY < 1 OR > 31
               GO TO IKS-060
           END-IF.
       IKS-080.
           CALL "SD_Accept" USING BY REFERENCE A-NNEN "A-NNEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO IKS-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO IKS-080
           END-IF.
       IKS-100.
           CALL "SD_Accept" USING BY REFERENCE A-NGET "A-NGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO IKS-080
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO IKS-100
           END-IF
           IF  W-NGET < 1 OR > 12
               GO TO IKS-100
           END-IF
           IF  W-NG > W-NNG
               GO TO IKS-100
           END-IF.
       IKS-120.
           CALL "SD_Accept" USING BY REFERENCE A-NPEY "A-NPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO IKS-100
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO IKS-120
           END-IF
           IF  W-NPEY < 1 OR > 31
               GO TO IKS-120
           END-IF
           IF  W-ONGP = W-NNGP
               GO TO IKS-120
           END-IF.
       IKS-140.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO IKS-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO IKS-140
           END-IF
           IF  W-DMM = 9
               GO TO IKS-020
           END-IF
           IF  W-DMM NOT = 1
               GO TO IKS-140
           END-IF
      *
           MOVE ZERO TO W-TCD W-DNO CNT W-EC.
           CALL "DB_F_Open" USING
            "INPUT" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
           CALL "DB_F_Open" USING
            "I-O" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
           CALL "DB_F_Open" USING
            "I-O" RSTRANYR_PNAME1 "SHARED" BY REFERENCE 
            RSTRANYR_IDLST "1" "RSTRANY-KEY" BY REFERENCE RSTRANY-KEY.
       IKS-160.
      *           READ SKDF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO IKS-220
           END-IF
           IF  SKD-SNO NOT = ZERO
               GO TO IKS-160
           END-IF
           IF  SKD-SKD NOT = W-ONGP
               GO TO IKS-160
           END-IF
           IF  SKD-TCD = W-TCD
               IF  W-EC = 0
                   GO TO IKS-200
               ELSE
                   GO TO IKS-160
               END-IF
           END-IF
      *
           MOVE ZERO TO W-EC.
           MOVE SKD-TCD TO TSK-TCD W-TCD.
      *           READ TSKF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO IKS-200
           END-IF
           IF  TSK-ZNGP(4) = ZERO
               GO TO IKS-200
           END-IF
           IF  TSK-ZNGP(5) = ZERO
               IF  TSK-ZNGP(4) < W-ONGP AND < W-NNGP
                   GO TO IKS-200
               END-IF
           END-IF
           MOVE 1 TO W-EC.
           GO TO IKS-160.
       IKS-200.
           MOVE W-NNGP TO SKD-SKD.
      *           REWRITE SKD-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SKDF_PNAME1 SKDF_LNAME SKD-R RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO IKS-220
           END-IF
           PERFORM STR-RTN THRU STR-EX.
           IF  SKD-DNO = W-DNO
               GO TO IKS-160
           END-IF
           ADD 1 TO CNT.
           MOVE SKD-DNO TO W-DNO.
      *
           IF  ZERO = W-ONGP OR W-NNGP
               PERFORM TUK-RTN THRU TUK-EX
           END-IF
           GO TO IKS-160.
       IKS-220.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           IF  W-END = 0
               IF  CNT = ZERO
                   CALL "SD_Output" USING
                    "E-ME1" E-ME1 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
               ELSE
                   CALL "SD_Output" USING
                    "D-EM" D-EM "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
               END-IF
           END-IF.
       IKS-EX.
           EXIT.
      *-----------------------------------------------------------------
       TBS-RTN.
           CALL "DB_F_Open" USING
            "I-O" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
           CALL "DB_F_Open" USING
            "I-O" RSTRANYR_PNAME1 "SHARED" BY REFERENCE 
            RSTRANYR_IDLST "1" "RSTRANY-KEY" BY REFERENCE RSTRANY-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
       TBS-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-MID2" D-MID2 "p" RETURNING RESU.
       TBS-040.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO TBS-900
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO TBS-040
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
               GO TO TBS-040
           END-IF
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
      *
           MOVE W-TCD TO TSK-KEY.
      *           READ TSKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO TSK-R
           END-IF
           IF  TSK-ZNGP(4) = ZERO
               MOVE ZERO TO W-DATE
           ELSE
               MOVE TSK-ZNGP(4) TO W-DATE
           END-IF
           IF  TSK-ZNGP(5) NOT = ZERO
               MOVE TSK-ZNGP(5) TO W-DATE
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               GO TO TBS-040
           END-IF
      *
           MOVE SPACE TO SKD-KEY.
           MOVE W-TCD TO SKD-TCD.
      *           START SKDF KEY NOT < SKD-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SKDF_PNAME1 "SKD-KEY" " NOT < " SKD-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO TBS-040
           END-IF.
      *
      *           READ SKDF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO TBS-040
           END-IF
           IF  SKD-TCD NOT = W-TCD
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO TBS-040
           END-IF
      *
           MOVE W-NG TO W-ONG W-NNG.
           CALL "SD_Output" USING "A-ONEN" A-ONEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-OGET" A-OGET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NNEN" A-NNEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NGET" A-NGET "p" RETURNING RESU.
       TBS-060.
           CALL "SD_Accept" USING BY REFERENCE A-ONEN "A-ONEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO TBS-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO TBS-060
           END-IF.
       TBS-080.
           CALL "SD_Accept" USING BY REFERENCE A-OGET "A-OGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO TBS-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO TBS-080
           END-IF
           IF  W-OGET < 1 OR > 12
               GO TO TBS-080
           END-IF
           IF  W-NG > W-ONG
               GO TO TBS-080
           END-IF.
       TBS-100.
           CALL "SD_Accept" USING BY REFERENCE A-OPEY "A-OPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO TBS-080
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO TBS-100
           END-IF
           IF  W-OPEY < 1 OR > 31
               GO TO TBS-100
           END-IF.
      *
           IF  W-DATE NOT = ZERO
               IF  W-DATE >= W-ONGP
                   CALL "SD_Output" USING
                    "E-ME5" E-ME5 "p" RETURNING RESU
                   GO TO TBS-100
               END-IF
           END-IF.
       TBS-160.
           CALL "SD_Accept" USING BY REFERENCE A-NNEN "A-NNEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO TBS-100
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO TBS-160
           END-IF.
       TBS-180.
           CALL "SD_Accept" USING BY REFERENCE A-NGET "A-NGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO TBS-160
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO TBS-180
           END-IF
           IF  W-NNEN NOT = ZERO
               IF  W-NGET < 1 OR > 12
                   GO TO TBS-180
               END-IF
           END-IF
           IF  W-NNEN NOT = ZERO
               IF  W-NNG < W-NG
                   GO TO TBS-180
               END-IF
           END-IF
           IF  W-NNEN = ZERO
               IF  W-NGET NOT = ZERO
                   GO TO TBS-180
               END-IF
           END-IF.
       TBS-200.
           CALL "SD_Accept" USING BY REFERENCE A-NPEY "A-NPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO TBS-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO TBS-200
           END-IF
           IF  W-NNEN NOT = ZERO
               IF  W-NPEY < 1 OR > 31
                   GO TO TBS-200
               END-IF
           END-IF
           IF  W-NNEN = ZERO
               IF  W-NPEY NOT = ZERO
                   GO TO TBS-200
               END-IF
           END-IF
           IF  W-NNGP = W-ONGP
               GO TO TBS-200
           END-IF
      *
           IF  W-NNGP NOT = ZERO
               IF  W-DATE >= W-NNGP
                   CALL "SD_Output" USING
                    "E-ME5" E-ME5 "p" RETURNING RESU
                   GO TO TBS-200
               END-IF
           END-IF.
       TBS-220.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO TBS-200
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO TBS-220
           END-IF
           IF  W-DMM = 9
               GO TO TBS-040
           END-IF
           IF  W-DMM NOT = 1
               GO TO TBS-220
           END-IF
      *
           MOVE ZERO TO W-DNO CNT.
           MOVE SPACE TO SKD-KEY.
           MOVE W-TCD TO SKD-TCD.
      *           START SKDF KEY NOT < SKD-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SKDF_PNAME1 "SKD-KEY" " NOT < " SKD-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO TBS-900
           END-IF.
       TBS-240.
      *           READ SKDF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO TBS-500
           END-IF
           IF  SKD-TCD NOT = W-TCD
               GO TO TBS-500
           END-IF
           IF  SKD-SNO NOT = ZERO
               GO TO TBS-240
           END-IF
           IF  SKD-SKD NOT = W-ONGP
               GO TO TBS-240
           END-IF
      *
           MOVE W-NNGP TO SKD-SKD.
      *           REWRITE SKD-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SKDF_PNAME1 SKDF_LNAME SKD-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TBS-900
           END-IF
           PERFORM STR-RTN THRU STR-EX.
           IF  SKD-DNO = W-DNO
               GO TO TBS-240
           END-IF
           ADD 1 TO CNT.
           MOVE SKD-DNO TO W-DNO.
      *
           IF  ZERO = W-ONGP OR W-NNGP
               PERFORM TUK-RTN THRU TUK-EX
           END-IF
           GO TO TBS-240.
       TBS-500.
           IF  CNT = ZERO
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "D-EM" D-EM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO TBS-020.
       TBS-900.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
       TBS-EX.
           EXIT.
      *-----------------------------------------------------------------
       KBS-RTN.
           CALL "DB_F_Open" USING
            "I-O" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
           CALL "DB_F_Open" USING
            "I-O" SKDKF_PNAME1 "SHARED" BY REFERENCE SKDKF_IDLST "1"
            "SKDK-KEY" BY REFERENCE SKDK-KEY.
           CALL "DB_F_Open" USING
            "I-O" TAZM_PNAME1 "SHARED" BY REFERENCE TAZM_IDLST "1"
            "TAZ-KEY" BY REFERENCE TAZ-KEY.
           CALL "DB_F_Open" USING
            "I-O" RSTRANYR_PNAME1 "SHARED" BY REFERENCE 
            RSTRANYR_IDLST "1" "RSTRANY-KEY" BY REFERENCE RSTRANY-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
       KBS-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-MID2" D-MID2 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-MID3" D-MID3 "p" RETURNING RESU.
       KBS-040.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO KBS-900
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO KBS-040
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
               GO TO KBS-040
           END-IF
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
      *
           MOVE W-TCD TO TSK-KEY.
      *           READ TSKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO TSK-R
           END-IF
           IF  TSK-ZNGP(4) = ZERO
               MOVE ZERO TO W-DATE
           ELSE
               MOVE TSK-ZNGP(4) TO W-DATE
           END-IF
           IF  TSK-ZNGP(5) NOT = ZERO
               MOVE TSK-ZNGP(5) TO W-DATE
           END-IF.
       KBS-050.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-MID2" D-MID2 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-MID3" D-MID3 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
       KBS-060.
           CALL "SD_Accept" USING BY REFERENCE A-DNO "A-DNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO KBS-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO KBS-060
           END-IF
      *
           MOVE SPACE TO SKD-KEY.
           MOVE W-TCD TO SKD-TCD.
      *           START SKDF KEY NOT < SKD-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SKDF_PNAME1 "SKD-KEY" " NOT < " SKD-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO KBS-060
           END-IF.
       KBS-080.
      *           READ SKDF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO KBS-060
           END-IF
           IF  SKD-TCD NOT = W-TCD
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO KBS-060
           END-IF
           IF  SKD-DNO NOT = W-DNO
               GO TO KBS-080
           END-IF
           IF  SKD-SNO NOT = ZERO
               MOVE SKD-SKD TO W-DATE
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO KBS-060
           END-IF
      *
           MOVE SKD-SKD TO W-ONGP W-NNGP.
           MOVE SKD-DTC TO W-DTC.
           MOVE SKD-DC TO W-DC.
           MOVE ZERO TO W-TKIN.
           CALL "SD_Output" USING "D-MD21" D-MD21 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-OPEY" A-OPEY "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NPEY" A-NPEY "p" RETURNING RESU.
           MOVE 6 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 7 TO W-L2.
       KBS-100.
           ADD 2 TO W-L1 W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           IF  W-L1 > 19
               GO TO KBS-240
           END-IF
           MOVE SPACE TO W-NAME.
           IF  SKD-DTC = 3
               MOVE "入　金" TO W-NAME
               GO TO KBS-200
           END-IF
           IF  SKD-DTC = 5
               MOVE "前月繰越" TO W-NAME
               GO TO KBS-200
           END-IF
           IF  SKD-BMC NOT = 0
               GO TO KBS-120
           END-IF
           MOVE SKD-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "品名マスター　なし" TO HI-NAME
           END-IF
           MOVE HI-NAME TO W-NAME.
           GO TO KBS-200.
       KBS-120.
           IF  SKD-BMC NOT = 1
               GO TO KBS-140
           END-IF
           MOVE SKD-KCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "ｺｳﾋﾝﾏｽﾀｰ ﾅｼ         " TO KH-NAME
           END-IF
           MOVE KH-NAME TO W-NAME.
           GO TO KBS-200.
       KBS-140.
           IF  SKD-BMC NOT = 3
               GO TO KBS-200
           END-IF
           MOVE SKD-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO J-NAME
               MOVE "材料マスター　なし" TO J-NAME
           END-IF
           MOVE J-NAME TO W-NAME.
       KBS-200.
           MOVE ZERO TO W-MD.
           IF  SKD-BMC = 0
               IF  SKD-DTC = 1
                   MOVE SKD-SU TO W-SU
                   COMPUTE W-T = SKD-T * -1
                   COMPUTE W-KIN = SKD-KIN * -1
                   COMPUTE W-SHZ = SKD-SHZ * -1
               ELSE
                   IF  SKD-DC = 1 OR 2 OR 5
                       COMPUTE W-SU = SKD-SU * -1
                       MOVE SKD-T TO W-T
                       COMPUTE W-KIN = SKD-KIN * -1
                       MOVE SKD-SHZ TO W-SHZ
                   ELSE
                       MOVE SKD-SU TO W-SU
                       MOVE SKD-T TO W-T
                       MOVE SKD-KIN TO W-KIN
                       MOVE SKD-SHZ TO W-SHZ
                   END-IF
               END-IF
           END-IF
           IF  SKD-BMC NOT = 0
               IF  SKD-DTC = 0 OR 3
                   MOVE SKD-SU TO W-SU
                   MOVE SKD-T TO W-T
                   MOVE SKD-KIN TO W-KIN
                   MOVE SKD-SHZ TO W-SHZ
               ELSE
                   COMPUTE W-KIN = SKD-KIN * -1
                   COMPUTE W-SHZ = SKD-SHZ * -1
               END-IF
           END-IF
           ADD W-KIN TO W-TKIN.
           CALL "SD_Output" USING "D-MD22" D-MD22 "p" RETURNING RESU.
       KBS-220.
      *           READ SKDF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO KBS-240
           END-IF
           IF  SKD-TCD NOT = W-TCD
               GO TO KBS-240
           END-IF
           IF  SKD-DNO = W-DNO
               GO TO KBS-100
           END-IF.
       KBS-240.
           CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU.
           IF  W-DATE NOT = ZERO
               IF  W-ONGP NOT = ZERO
                   IF  W-DATE >= W-ONGP
                       CALL "SD_Output" USING
                        "E-ME5" E-ME5 "p" RETURNING RESU
                       GO TO KBS-060
                   END-IF
               END-IF
           END-IF.
       KBS-260.
           CALL "SD_Accept" USING BY REFERENCE A-NNEN "A-NNEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO KBS-050
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO KBS-260
           END-IF.
       KBS-280.
           CALL "SD_Accept" USING BY REFERENCE A-NGET "A-NGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO KBS-260
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO KBS-280
           END-IF
           IF  W-NNEN NOT = ZERO AND 9999
               IF  W-NGET < 1 OR > 12
                   GO TO KBS-280
               END-IF
           END-IF
           IF  W-NNEN NOT = ZERO AND 9999
               IF  W-NNG < W-NG
                   GO TO KBS-280
               END-IF
           END-IF
           IF  W-NNEN = ZERO
               IF  W-NGET NOT = ZERO
                   GO TO KBS-280
               END-IF
           END-IF
           IF  W-NNEN = 9999
               IF  W-NGET NOT = 99
                   GO TO KBS-280
               END-IF
           END-IF.
       KBS-300.
           CALL "SD_Accept" USING BY REFERENCE A-NPEY "A-NPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO KBS-280
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO KBS-300
           END-IF
           IF  W-NNEN NOT = ZERO AND 9999
               IF  W-NPEY < 1 OR > 31
                   GO TO KBS-300
               END-IF
           END-IF
           IF  W-NNEN = ZERO
               IF  W-NPEY NOT = ZERO
                   GO TO KBS-300
               END-IF
           END-IF
           IF  W-NNEN = 9999
               IF  W-NPEY NOT = 99
                   GO TO KBS-300
               END-IF
           END-IF
           IF  W-ONGP = W-NNGP
               GO TO KBS-300
           END-IF
           IF  W-NNGP = 99999999
               IF (W-DTC NOT = 0) OR (W-DC NOT = 3)
                   CALL "SD_Output" USING
                    "E-ME10" E-ME10 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO KBS-260
               END-IF
           END-IF
      *
           IF  W-NNGP NOT = ZERO AND 99999999
               IF  W-DATE >= W-NNGP
                   CALL "SD_Output" USING
                    "E-ME5" E-ME5 "p" RETURNING RESU
                   GO TO KBS-300
               END-IF
           END-IF.
       KBS-320.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO KBS-300
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO KBS-320
           END-IF
           IF  W-DMM = 9
               GO TO KBS-260
           END-IF
           IF  W-DMM NOT = 1
               GO TO KBS-320
           END-IF
      *
           MOVE SPACE TO SKD-KEY.
           MOVE W-TCD TO SKD-TCD.
      *           START SKDF KEY NOT < SKD-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SKDF_PNAME1 "SKD-KEY" " NOT < " SKD-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO KBS-900
           END-IF.
       KBS-360.
      *           READ SKDF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO KBS-500
           END-IF
           IF  SKD-TCD NOT = W-TCD
               GO TO KBS-500
           END-IF
           IF  SKD-SNO NOT = ZERO
               GO TO KBS-360
           END-IF
           IF  SKD-DNO NOT = W-DNO
               GO TO KBS-360
           END-IF
      *
           MOVE W-NNGP TO SKD-SKD.
      *           REWRITE SKD-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SKDF_PNAME1 SKDF_LNAME SKD-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO KBS-900
           END-IF
           PERFORM STR-RTN THRU STR-EX.
           IF  W-NNGP NOT = 99999999
               GO TO KBS-420
           END-IF.
       KBS-380.
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
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO KBS-400
           END-IF
           GO TO KBS-410.
       KBS-400.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO KBS-900
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
           GO TO KBS-380.
       KBS-410.
           PERFORM TAZ-RTN THRU TAZ-EX.
           IF  W-END NOT = 0
               GO TO KBS-900
           END-IF.
       KBS-420.
           IF  W-ONGP NOT = 99999999
               GO TO KBS-360
           END-IF
           MOVE SKD-KEY TO SKDK-KEY.
      *           READ SKDKF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" SKDKF_PNAME1 BY REFERENCE SKDK-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO KBS-430
           END-IF
      *           DELETE SKDKF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING SKDKF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO KBS-900
           END-IF.
       KBS-430.
           PERFORM TAZ-RTN THRU TAZ-EX.
           IF  W-END NOT = 0
               GO TO KBS-900
           END-IF
           GO TO KBS-360.
       KBS-500.
           IF (ZERO = W-ONGP OR W-NNGP) OR
              (99999999 = W-ONGP OR W-NNGP)
               PERFORM TUK-RTN THRU TUK-EX
           END-IF
           GO TO KBS-020.
       KBS-900.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SKDKF_IDLST SKDKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TAZM_IDLST TAZM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
       KBS-EX.
           EXIT.
      *-----------------------------------------------------------------
       TUK-RTN.
           CALL "DB_F_Open" USING
            "I-O" TUKF_PNAME1 "SHARED" BY REFERENCE TUKF_IDLST "2"
            "TUK-KEY" BY REFERENCE TUK-KEY "TUK-KEY2" BY REFERENCE
            TUK-KEY2.
           MOVE SPACE TO TUK-KEY.
           MOVE W-TCD TO TUK-TCD.
      *           START TUKF KEY NOT < TUK-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TUKF_PNAME1 "TUK-KEY" " NOT < " TUK-KEY RETURNING RET.
           IF  RET = 1
               GO TO TUK-900
           END-IF.
       TUK-020.
      *           READ TUKF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TUKF_PNAME1 BY REFERENCE TUK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO TUK-900
           END-IF
           IF  W-TCD NOT = TUK-TCD
               GO TO TUK-900
           END-IF
           IF  W-DNO NOT = TUK-DNO
               GO TO TUK-020
           END-IF
           IF  W-ONGP = ZERO
               MOVE 00000000 TO TUK-SKD
           END-IF
           IF  W-NNGP = ZERO OR 99999999
               MOVE 99999999 TO TUK-SKD
           END-IF
      *           REWRITE TUK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TUKF_PNAME1 TUKF_LNAME TUK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TUK" E-TUK "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       TUK-900.
           CALL "DB_F_Close" USING BY REFERENCE TUKF_IDLST TUKF_PNAME1.
       TUK-EX.
           EXIT.
       TAZ-RTN.
           MOVE SKD-TCD TO TAZ-TCD.
           MOVE SKD-HCD TO TAZ-HCD.
      *           READ TAZM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TAZM_PNAME1 BY REFERENCE TAZ-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TAZ" E-TAZ "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO TAZ-010
           END-IF
           IF  W-ONGP = 99999999
               ADD SKD-SU TO TAZ-SAS
           ELSE
               SUBTRACT SKD-SU FROM TAZ-SAS
           END-IF
      *           REWRITE TAZ-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TAZM_PNAME1 TAZM_LNAME TAZ-R RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TAZ" E-TAZ "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TAZ-EX
           END-IF
           GO TO TAZ-EX.
       TAZ-010.
           MOVE ZERO TO TAZ-R.
           MOVE SKD-TCD TO TAZ-TCD.
           MOVE SKD-HCD TO TAZ-HCD.
           IF  W-ONGP = 99999999
               ADD SKD-SU TO TAZ-SAS
           ELSE
               SUBTRACT SKD-SU FROM TAZ-SAS
           END-IF
      *           WRITE TAZ-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TAZM_PNAME1 TAZM_LNAME TAZ-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TAZ" E-TAZ "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TAZ-020
           END-IF
           GO TO TAZ-EX.
       TAZ-020.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TAZ-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE TAZM_IDLST TAZM_PNAME1.
           MOVE "TAZM         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TAZM_PNAME1 "SHARED" BY REFERENCE TAZM_IDLST "1"
            "TAZ-KEY" BY REFERENCE TAZ-KEY.
           GO TO TAZ-010.
       TAZ-EX.
           EXIT.
       STR-RTN.
           IF  SKD-BMC NOT = 0
               GO TO STR-EX
           END-IF
           IF  SKD-DTC NOT = 0
               GO TO STR-EX
           END-IF
           MOVE SKD-DATE TO RSTRANY-DATE.
           MOVE SKD-DNO TO RSTRANY-DNO.
           MOVE SKD-GNO TO RSTRANY-GNO.
           MOVE SKD-TCD TO RSTRANY-TCD.
      *           READ RSTRANYR INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" RSTRANYR_PNAME1 BY REFERENCE RSTRANY-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STR" E-STR "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO STR-EX
           END-IF
           MOVE W-NNGP TO RSTRANY-SKD.
      *           REWRITE RSTRANY-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            RSTRANYR_PNAME1 RSTRANYR_LNAME RSTRANY-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STR" E-STR "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       STR-EX.
           EXIT.

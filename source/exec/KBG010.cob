       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG010.
      *********************************************************
      *    PROGRAM         :  日付別買掛残高明細表　　　　    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/05                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(042) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-NEN          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-GET          PIC Z9.
           02  F              PIC  N(016) VALUE
                "月分　買掛残高　明細表　　＊＊＊".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(023) VALUE
                "   I-----------------  ".
           02  F              PIC  N(004) VALUE "仕　　入".
           02  F              PIC  X(040) VALUE
                "  ----------------I I-----------------  ".
           02  F              PIC  N(004) VALUE "支　　払".
           02  F              PIC  X(034) VALUE
                "  ----------------I I-----------  ".
           02  F              PIC  N(004) VALUE "買掛残高".
           02  F              PIC  X(013) VALUE "  ----------I".
       01  HEAD3.
           02  F              PIC  N(001) VALUE "日".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(003) VALUE "材　料".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE "製　品".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE "合　計".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(003) VALUE "材　料".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE "製　品".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE "合　計".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(003) VALUE "仕　入".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE "合　計".
       01  W-P.
           02  P-PEY          PIC Z9.
           02  P-TM    REDEFINES P-PEY   PIC  N(001).
           02  P-SZI          PIC -----,---,--9.
           02  P-SSS          PIC ----,---,--9.
           02  P-STK          PIC ----,---,--9.
           02  P-SSZ          PIC ---,---,--9.
           02  P-HZI          PIC -----,---,--9.
           02  P-HSS          PIC ----,---,--9.
           02  P-HTK          PIC ----,---,--9.
           02  P-HSZ          PIC ---,---,--9.
           02  P-KM    REDEFINES P-HSZ   PIC  X(011).
           02  P-KSI          PIC -----,---,--9.
           02  P-KSZ          PIC ---,---,--9.
           02  P-KTK          PIC ----,---,--9.
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-NG.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
       01  W-D.
           02  W-PEY          PIC  9(002).
           02  W-SZI          PIC S9(009).
           02  W-SSS          PIC S9(009).
           02  W-STK          PIC S9(009).
           02  W-SSZ          PIC S9(008).
           02  W-HZI          PIC S9(009).
           02  W-HSS          PIC S9(009).
           02  W-HTK          PIC S9(009).
           02  W-HSZ          PIC S9(008).
       01  WT-D.
           02  WT-SZI         PIC S9(009).
           02  WT-SSS         PIC S9(009).
           02  WT-STK         PIC S9(009).
           02  WT-SSZ         PIC S9(008).
           02  WT-HZI         PIC S9(009).
           02  WT-HSS         PIC S9(009).
           02  WT-HTK         PIC S9(009).
           02  WT-HSZ         PIC S9(008).
       01  W-KZD.
           02  W-KSI          PIC S9(009).
           02  W-KSZ          PIC S9(008).
           02  W-KTK          PIC S9(009).
           02  W-ZKZ          PIC S9(009).
           02  W-ZKZZ         PIC S9(008).
           02  W-KZ           PIC S9(009).
           02  W-KZZ          PIC S9(008).
           02  W-TSK          PIC S9(009).
           02  W-TSKZ         PIC S9(008).
           02  W-THK          PIC S9(009).
           02  W-THKZ         PIC S9(008).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LISTM.
           COPY LSJSSW.
           COPY LSPF.
      *FD  TDT-M
       01  TDT-M_KBG010.
           02  TDT-M_PNAME1   PIC  X(004) VALUE "TDTM".
           02  F              PIC  X(001).
           02  TDT-M_LNAME    PIC  X(012) VALUE "TDT-M_KBG010".
           02  F              PIC  X(001).
           02  TDT-M_KEY1     PIC  X(100) VALUE SPACE.
           02  TDT-M_SORT     PIC  X(100) VALUE SPACE.
           02  TDT-M_IDLST    PIC  X(100) VALUE SPACE.
           02  TDT-M_RES      USAGE  POINTER.
       01  TDT-R.
           02  TD-KEY.
             03  TD-TKC       PIC  9(002).
             03  TD-TNO       PIC  9(004).
           02  TD-TCD         PIC  9(004).
           02  TD-DATE        PIC  9(006).
           02  TD-NGP   REDEFINES TD-DATE.
             03  F            PIC  9(002).
             03  TD-GET       PIC  9(002).
             03  F            PIC  9(002).
           02  TD-MND         PIC  9(006).
           02  TD-KIN         PIC S9(010).
           02  TD-BKC         PIC  9(004).
           02  TD-FRN         PIC  N(024).
           02  TD-SAD.
             03  TD-S     OCCURS   7  PIC S9(008).
           02  TD-ZSHZ        PIC S9(007).
           02  TD-SSHZ        PIC S9(007).
           02  F              PIC  X(006).
           02  TD-SNEN        PIC  9(004).
           02  TD-SNEND REDEFINES TD-SNEN.
             03  TD-SNEN1     PIC  9(002).
             03  TD-SNEN2     PIC  9(002).
           02  TD-HCR         PIC  9(001).
           02  TD-HCT         PIC  9(001).
           02  TD-HCK         PIC  9(001).
           02  TD-HCZ         PIC  9(001).
           02  TD-PC          PIC  9(001).
           02  TD-RSC         PIC  9(001).
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
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　日付別　買掛残高明細表　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-DMM   PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
             03  D-DMMC  PIC  X(022) VALUE
                  "                      ".
           02  D-EME.
             03  FILLER  PIC  X(044) VALUE
                  "(仕入先マスター)      (仕入支払累積ファイル)".
             03  FILLER.
               04  FILLER  PIC  N(005) VALUE "材料・製品".
               04  FILLER  PIC  N(003) VALUE "消費税".
               04  FILLER  PIC  N(005) VALUE "材料・製品".
               04  FILLER  PIC  N(003) VALUE "消費税".
             03  FILLER.
               04  FILLER  PIC  N(003) VALUE "前　残".
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ---,---,--9 .
             03  FILLER.
               04  FILLER  PIC  N(003) VALUE "仕　入".
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ---,---,--9 .
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ---,---,--9 .
             03  FILLER.
               04  FILLER  PIC  N(003) VALUE "支　払".
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ---,---,--9 .
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ---,---,--9 .
             03  FILLER.
               04  FILLER  PIC  N(003) VALUE "買掛残".
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ---,---,--9 .
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ---,---,--9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME4   PIC  X(032) VALUE
                  "***  支払未変換データ　有り  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "294" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "2" "17" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "3" "17" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "4" "17" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "5" "17" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "6" "17" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "7" "17" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "8" "17" "42" "06C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "44" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "305" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "20" "0" "44" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DMM" "X" "20" "27" "22" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DMMC" "X" "20" "27" "22" "D-DMM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-EME" " " "0" "0" "261" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-EME" "X" "12" "22" "44" " " "D-EME" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-EME" " " "14" "0" "32" "01D-EME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-EME" "N" "14" "20" "10" " " "02D-EME" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-EME" "N" "14" "36" "6" "0102D-EME" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-EME" "N" "14" "45" "10" "0202D-EME" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
           "0402D-EME" "N" "14" "61" "6" "0302D-EME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-EME" " " "16" "0" "29" "02D-EME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103D-EME" "N" "16" "11" "6" " " "03D-EME" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203D-EME" "----,---,--9" "16" "18" "12" "0103D-EME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0203D-EME" BY REFERENCE W-ZKZ "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0303D-EME" "---,---,--9" "16" "31" "11" "0203D-EME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0303D-EME" BY REFERENCE W-ZKZZ "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-EME" " " "18" "0" "52" "03D-EME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0104D-EME" "N" "18" "11" "6" " " "04D-EME" RETURNING RESU.
       CALL "SD_Init" USING 
            "0204D-EME" "----,---,--9" "18" "18" "12" "0104D-EME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0204D-EME" BY REFERENCE W-TSK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0304D-EME" "---,---,--9" "18" "31" "11" "0204D-EME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0304D-EME" BY REFERENCE W-TSKZ "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0404D-EME" "----,---,--9" "18" "43" "12" "0304D-EME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0404D-EME" BY REFERENCE WT-STK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0504D-EME" "---,---,--9" "18" "56" "11" "0404D-EME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0504D-EME" BY REFERENCE WT-SSZ "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-EME" " " "20" "0" "52" "04D-EME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0105D-EME" "N" "20" "11" "6" " " "05D-EME" RETURNING RESU.
       CALL "SD_Init" USING 
            "0205D-EME" "----,---,--9" "20" "18" "12" "0105D-EME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0205D-EME" BY REFERENCE W-THK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0305D-EME" "---,---,--9" "20" "31" "11" "0205D-EME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0305D-EME" BY REFERENCE W-THKZ "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0405D-EME" "----,---,--9" "20" "43" "12" "0305D-EME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0405D-EME" BY REFERENCE WT-HTK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0505D-EME" "---,---,--9" "20" "56" "11" "0405D-EME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0505D-EME" BY REFERENCE WT-HSZ "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-EME" " " "22" "0" "52" "05D-EME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0106D-EME" "N" "22" "11" "6" " " "06D-EME" RETURNING RESU.
       CALL "SD_Init" USING 
            "0206D-EME" "----,---,--9" "22" "18" "12" "0106D-EME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0206D-EME" BY REFERENCE W-KZ "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0306D-EME" "---,---,--9" "22" "31" "11" "0206D-EME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0306D-EME" BY REFERENCE W-KZZ "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0406D-EME" "----,---,--9" "22" "43" "12" "0306D-EME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0406D-EME" BY REFERENCE W-KSI "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0506D-EME" "---,---,--9" "22" "56" "11" "0406D-EME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0506D-EME" BY REFERENCE W-KSZ "8" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "77" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "77" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "32" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           MOVE DATE-05R TO H-DATE.
           MOVE D-NBN TO H-NEN.
           MOVE D-NBG TO H-GET.
           MOVE D-NBNG TO W-NG.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           PERFORM S-15 THRU S-30.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" ST-M_PNAME1 
            "SHARED" BY REFERENCE ST-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
           MOVE ZERO TO W-KZD CHK.
       M-10.
      *           READ ST-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" ST-M_PNAME1 BY REFERENCE ST-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE ST-M_IDLST ST-M_PNAME1
               GO TO M-15
           END-IF
           ADD ST-ZKZ TO W-KSI W-KTK W-ZKZ.
           ADD ST-ZKZZ TO W-KSZ W-KTK W-ZKZZ.
           ADD ST-KZ TO W-KZ.
           ADD ST-KZZ TO W-KZZ.
           ADD ST-TSK TO W-TSK.
           ADD ST-TSKZ TO W-TSKZ.
           ADD ST-THK TO W-THK.
           ADD ST-THKZ TO W-THKZ.
           GO TO M-10.
       M-15.
           IF  CHK NOT = 5
               GO TO M-20
           END-IF
           CALL "SD_Output" USING "D-DMM" D-DMM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-15
           END-IF.
       M-20.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JSSR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "PR_Open" RETURNING RESP.
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
           MOVE SPACE TO SP-R W-P.
           MOVE "-- 繰越 --" TO P-KM.
           MOVE W-KSI TO P-KSI.
           MOVE W-KSZ TO P-KSZ.
           MOVE W-KTK TO P-KTK.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO WT-D.
       M-25.
           MOVE ZERO TO W-D.
           MOVE JR-PEY TO W-PEY.
       M-30.
           IF  JR-DC1 NOT = 3
               IF  JR-SCD1 < 5
                   ADD JR-KIN TO W-SZI W-STK
                   ADD JR-SHZ TO W-SSZ
               ELSE
                   ADD JR-KIN TO W-SSS W-STK
                   ADD JR-SHZ TO W-SSZ
               END-IF
           END-IF
           IF  JR-DC1 = 3
               IF  JR-SCD1 < 5
                   ADD JR-KIN TO W-HZI W-HTK
                   ADD JR-SHZ TO W-HSZ
               ELSE
                   ADD JR-KIN TO W-HSS W-HTK
                   ADD JR-SHZ TO W-HSZ
               END-IF
           END-IF.
       M-35.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  W-PEY = JR-PEY
               GO TO M-30
           END-IF
           PERFORM S-05 THRU S-10.
           GO TO M-25.
       M-90.
           PERFORM S-05 THRU S-10.
           MOVE SPACE TO W-P.
           MOVE "計" TO P-TM.
           MOVE WT-SZI TO P-SZI.
           MOVE WT-SSS TO P-SSS.
           MOVE WT-STK TO P-STK.
           MOVE WT-SSZ TO P-SSZ.
           MOVE WT-HZI TO P-HZI.
           MOVE WT-HSS TO P-HSS.
           MOVE WT-HTK TO P-HTK.
           MOVE WT-HSZ TO P-HSZ.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               MOVE SPACE TO SP-R
               MOVE HEAD1 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD2 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD3 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           IF (WT-STK NOT = W-TSK) OR (WT-SSZ NOT = W-TSKZ) OR
                     (WT-HTK NOT = W-THK) OR (WT-HSZ NOT = W-THKZ) OR
                              (W-KSI NOT = W-KZ) OR (W-KSZ NOT = W-KZZ)
               CALL "SD_Output" USING "D-DMMC" D-DMMC "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING "D-EME" D-EME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           COMPUTE W-KSI = W-KSI + W-STK - W-HTK.
           COMPUTE W-KSZ = W-KSZ + W-SSZ - W-HSZ.
           COMPUTE W-KTK = W-KSI + W-KSZ.
           MOVE SPACE TO W-P.
           MOVE W-PEY TO P-PEY.
           MOVE W-SZI TO P-SZI.
           MOVE W-SSS TO P-SSS.
           MOVE W-STK TO P-STK.
           MOVE W-SSZ TO P-SSZ.
           MOVE W-HZI TO P-HZI.
           MOVE W-HSS TO P-HSS.
           MOVE W-HTK TO P-HTK.
           MOVE W-HSZ TO P-HSZ.
           MOVE W-KSI TO P-KSI.
           MOVE W-KSZ TO P-KSZ.
           MOVE W-KTK TO P-KTK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               MOVE SPACE TO SP-R
               MOVE HEAD1 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD2 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD3 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD W-SZI TO WT-SZI.
           ADD W-SSS TO WT-SSS.
           ADD W-STK TO WT-STK.
           ADD W-SSZ TO WT-SSZ.
           ADD W-HZI TO WT-HZI.
           ADD W-HSS TO WT-HSS.
           ADD W-HTK TO WT-HTK.
           ADD W-HSZ TO WT-HSZ.
       S-10.
           EXIT.
       S-15.
           CALL "DB_F_Open" USING
            "INPUT" TDT-M_PNAME1 "SHARED" BY REFERENCE TDT-M_IDLST "1"
            "TD-KEY" BY REFERENCE TD-KEY.
       S-20.
      *           READ TDT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDT-M_PNAME1 BY REFERENCE TDT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-25
           END-IF
           IF  TD-HCK NOT = 0
               GO TO S-20
           END-IF
           IF (TD-SNEN2 NOT = W-NEN) OR (TD-GET NOT = W-GET)
               GO TO S-20
           END-IF
           CALL "SD_Output" USING "E-ME4" E-ME4 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
       S-25.
           CALL "DB_F_Close" USING
            BY REFERENCE TDT-M_IDLST TDT-M_PNAME1.
       S-30.
           EXIT.

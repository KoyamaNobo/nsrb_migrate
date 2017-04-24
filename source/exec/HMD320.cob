       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD320.
      *********************************************************
      *    PROGRAM         :  倉庫間移動入力　　　　　　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHD32                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       77  ERR-STAT           PIC  X(002).
       77  W-POC              PIC  9(001) VALUE 0.
       77  W-IPC              PIC  9(001) VALUE 0.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(043) VALUE SPACE.
           02  F              PIC  N(021) VALUE
                "＊＊＊　　倉庫間移動　入力リスト　　＊＊＊".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "日　　付".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "倉　　庫".
           02  F              PIC  X(120) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　伝票№".
           02  F              PIC  X(001) VALUE "-".
           02  F              PIC  N(002) VALUE "行　".
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(001) VALUE "1".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "３号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "２号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "１号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "０号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　中".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　大".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "特大".
           02  F              PIC  X(021) VALUE
                "   28.0   29.0   30.0".
           02  F              PIC  X(008) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(057) VALUE SPACE.
           02  F              PIC  X(039) VALUE
                "2   12.5   13.0   13.5   14.0   15.0   ".
           02  F              PIC  X(032) VALUE
                "16.0   17.0   18.0   19.0   20.0".
           02  F              PIC  X(008) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(057) VALUE SPACE.
           02  F              PIC  X(039) VALUE
                "3   21.0   21.5   22.0   22.5   23.0   ".
           02  F              PIC  X(032) VALUE
                "23.5   24.0   24.5   25.0       ".
           02  F              PIC  X(008) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(057) VALUE SPACE.
           02  F              PIC  X(039) VALUE
                "4   24.0   24.5   25.0   25.5   26.0   ".
           02  F              PIC  X(032) VALUE
                "26.5   27.0   27.5              ".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(002) VALUE "合計".
       01  W-P1.
           02  P-DATE         PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-SKC          PIC  9(001).
           02  F              PIC  X(001).
           02  P-SKN          PIC  N(006).
           02  F              PIC  X(115).
       01  W-P2.
           02  F              PIC  X(004).
           02  P-UNO          PIC  9(006).
           02  P-V            PIC  X(001).
           02  P-GYO          PIC  9(001).
           02  F              PIC  X(001).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  9(001).
           02  P-SU           PIC ---,---       OCCURS  10.
           02  P-TSU          PIC ----,--9.
       01  W-ID.
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPSD REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-IDM          PIC  9(001).
           02  W-IDS          PIC  9(001).
           02  W-AMEI.
             03  W-MEID  OCCURS   6.
               04  W-HCD      PIC  9(006).
               04  W-SIZ      PIC  9(001).
               04  W-ASUD.
                 05  W-SUD   OCCURS  10.
                   06  W-SU   PIC S9(005).
               04  W-GSU      PIC S9(006).
           02  W-SUT          PIC S9(006).
       01  W-DATA.
           02  W-LC.
             03  W-L.
               04  W-L1       PIC  9(002).
               04  W-L2       PIC  9(002).
             03  W-C          PIC S9(002).
           02  W-UNO1         PIC  9(006).
           02  W-UNO2         PIC  9(006).
           02  W-GN           PIC  9(001).
           02  W-SIZD         PIC  9(001).
           02  CNT            PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-SEN          PIC  9(001).
           02  W-SNGP         PIC  9(008).
           02  W-SDD   REDEFINES W-SNGP.
             03  F            PIC  9(002).
             03  W-SDAT       PIC  9(006).
           02  W-ENGP         PIC  9(008).
           02  W-EDD   REDEFINES W-ENGP.
             03  F            PIC  9(002).
             03  W-EDAT       PIC  9(006).
           02  W-SHCD         PIC  9(006).
           02  W-EHCD         PIC  9(006).
           02  W-DMM          PIC  9(001).
           02  W-SNG          PIC  9(006).
           02  W-ENG          PIC  9(006).
           02  W-TSU          PIC S9(006).
           02  W-PAGE         PIC  9(002).
           COPY LSTAT.
           COPY LWMSG.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY L-JCON.
           COPY LSPF.
      *FD  HSKIF
       01  HSKIF_HMD320.
           02  HSKIF_PNAME1   PIC  X(005) VALUE "HSKIF".
           02  F              PIC  X(001).
           02  HSKIF_LNAME    PIC  X(012) VALUE "HSKIF_HMD320".
           02  F              PIC  X(001).
           02  HSKIF_KEY1     PIC  X(100) VALUE SPACE.
           02  HSKIF_SORT     PIC  X(100) VALUE SPACE.
           02  HSKIF_IDLST    PIC  X(100) VALUE SPACE.
           02  HSKIF_RES      USAGE  POINTER.
       01  HSKI-R.
           02  HSKI-NO        PIC  9(007).
           02  HSKI-NOD   REDEFINES HSKI-NO.
             03  HSKI-UNO     PIC  9(006).
             03  HSKI-GYO     PIC  9(001).
           02  HSKI-DATE      PIC  9(008).
           02  HSKI-NGPD  REDEFINES HSKI-DATE.
             03  HSKI-NG      PIC  9(006).
             03  F            PIC  9(002).
           02  HSKI-NGP   REDEFINES HSKI-DATE.
             03  F            PIC  9(002).
             03  HSKI-NGPS    PIC  9(006).
           02  HSKI-HCD       PIC  9(006).
           02  HSKI-SIZ       PIC  9(001).
           02  HSKI-SUD.
             03  HSKI-SU      PIC S9(005)  OCCURS  10.
           02  HSKI-SKC       PIC  9(001).
           02  HSKI-IDC       PIC  9(001).
           02  F              PIC  X(009).
           02  HSKI-PRN       PIC  9(001).
           02  HSKI-UPD       PIC  9(001).
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  FILLER.
             03  A-NGPS  PIC  9(006).
             03  A-IDM   PIC  9(001).
             03  A-IDS   PIC  9(001).
           02  FILLER.
             03  A-HCD   PIC  9(006).
           02  FILLER.
             03  A-SIZ   PIC  9(001).
             03  A-SU    PIC S9(005).
           02  A-SEN   PIC  9(001).
           02  FILLER.
             03  A-SDAT  PIC  9(006).
             03  A-EDAT  PIC  9(006).
           02  FILLER.
             03  A-SHCD  PIC  9(006).
             03  A-EHCD  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-IDMN  PIC  N(006).
             03  D-IDSN  PIC  N(006).
           02  D-NAME  PIC  N(024).
           02  FILLER.
             03  D-SU    PIC ZZZZZ- .
             03  D-GSU   PIC ZZZZZZ- .
           02  D-SUT   PIC ZZZZZZ- .
           02  D-PRN.
             03  FILLER  PIC  N(021) VALUE
                  "＊＊＊　　倉庫間移動　入力リスト　　＊＊＊".
             03  FILLER  PIC  X(045) VALUE
                  "未印字分  YES=1 NO=9  1 ﾘﾀｰﾝ        終了=ｆ･9".
             03  FILLER  PIC  X(026) VALUE
                  "日　付    000000 ～ 999999".
             03  FILLER  PIC  X(026) VALUE
                  "品名ｺｰﾄﾞ  000000 ～ 999999".
             03  FILLER  PIC  X(025) VALUE
                  "確認(OK=1,NO=9)-->   ﾘﾀｰﾝ".
       01  C-SPC.
           02  C-SPACE.
             03  C-HCD.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
             03  C-MEI.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME2   PIC  X(021) VALUE
                  "***  PROGRAM ｴﾗｰ  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  日付 ｴﾗｰ  ***".
             03  E-ME6   PIC  N(011) VALUE
                  "コントロールＦ　未登録".
             03  E-ME7   PIC  N(007) VALUE
                  "倉庫名　未登録".
             03  E-ME8   PIC  N(009) VALUE
                  "未更新データ　有り".
             03  E-ME9   PIC  N(008) VALUE
                  "印刷データ　なし".
             03  E-ME11  PIC  X(019) VALUE
                  "***  WRITE ｴﾗｰ  ***".
             03  E-ME12  PIC  X(027) VALUE
                  "***  JCON ﾃﾞﾝﾋﾟｮｳNO ﾅｼ  ***".
             03  E-ME13  PIC  X(026) VALUE
                  "***  JCON REWRITE ｴﾗｰ  ***".
           COPY LIBSCR.
           COPY LSSEM.
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "46" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "3" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NGPS" "9" "3" "7" "6" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NGPS" BY REFERENCE W-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-IDM" "9" "3" "26" "1" "A-NGPS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-IDM" BY REFERENCE W-IDM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-IDS" "9" "3" "49" "1" "A-IDM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-IDS" BY REFERENCE W-IDS "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "W-L1" "0" "6" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "W-L1" "2" "6" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD(1) "6" "1" BY REFERENCE W-GN 63
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "W-L2" "0" "6" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SIZ" "9" "W-L2" "2" "1" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SIZ" BY REFERENCE W-SIZ(1) "1" "1" BY REFERENCE W-GN 63
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SU" "S9" "W-L2" "W-C" "5" "A-SIZ" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SU" BY REFERENCE W-SU(1,1) "5" "2" BY REFERENCE W-GN 63
            BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "9" "49" "1" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-ACP" " " "12" "0" "12" "A-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SDAT" "9" "12" "37" "6" " " "05C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SDAT" BY REFERENCE W-SDAT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EDAT" "9" "12" "47" "6" "A-SDAT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EDAT" BY REFERENCE W-EDAT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-ACP" " " "14" "0" "12" "05C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHCD" "9" "14" "37" "6" " " "06C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHCD" BY REFERENCE W-SHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EHCD" "9" "14" "47" "6" "A-SHCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EHCD" BY REFERENCE W-EHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "60" "1" "06C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "256" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "3" "0" "24" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-IDMN" "N" "3" "28" "12" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-IDMN" BY REFERENCE JCON3-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-IDSN" "N" "3" "51" "12" "D-IDMN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-IDSN" BY REFERENCE JCON3-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "W-L1" "9" "48" "01C-DSP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-DSP" " " "W-L2" "0" "13" "D-NAME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU" "ZZZZZ-" "W-L2" "W-C" "6" " " "03C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-SU" BY REFERENCE W-SU(1,1) "5" "2" BY REFERENCE W-GN 63
            BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GSU" "ZZZZZZ-" "W-L2" "74" "7" "D-SU" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-GSU" BY REFERENCE W-GSU(1) "6" "1" BY REFERENCE W-GN 63
            RETURNING RESU.
       CALL "SD_Init" USING 
           "D-SUT" "ZZZZZZ-" "21" "74" "7" "03C-DSP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SUT" BY REFERENCE W-SUT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PRN" " " "0" "0" "164" "D-SUT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-PRN" "N" "1" "19" "42" " " "D-PRN" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-PRN" "X" "9" "27" "45" "01D-PRN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-PRN" "X" "12" "27" "26" "02D-PRN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-PRN" "X" "14" "27" "26" "03D-PRN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-PRN" "X" "23" "41" "25" "04D-PRN" " " RETURNING RESU.
      *C-SPC
       CALL "SD_Init" USING 
            "C-SPC" " " "0" "0" "160" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-SPACE" " " "0" "0" "160" " " "C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-HCD" " " "W-L1" "0" "80" " " "C-SPACE" RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-HCD" "X" "W-L1" "1" "40" " " "C-HCD" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-HCD" "X" "W-L1" "41" "40" "01C-HCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-MEI" " " "W-L2" "0" "80" "C-HCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MEI" "X" "W-L2" "1" "40" " " "C-MEI" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MEI" "X" "W-L2" "41" "40" "01C-MEI" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "197" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "197" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "16" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "21" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "N" "24" "15" "22" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "N" "24" "15" "14" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "N" "24" "15" "18" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "N" "24" "15" "16" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "19" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "27" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "26" "E-ME12" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHD32" RETURNING RESU.
      *
           MOVE ZERO TO W-ID W-DATA.
           MOVE 1 TO W-SEN.
           MOVE 999999 TO W-EDAT W-EHCD.
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON3-KEY" BY REFERENCE JCON3-KEY.
           MOVE "6 " TO JCON6-KEY.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE JCON6-03 TO W-ENG.
           MOVE W-ENG TO W-NG.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           MOVE W-NG TO W-SNG.
      *
           MOVE "18" TO JCON5-KEY.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "EXTEND" HSKIF_PNAME1 " " BY REFERENCE HSKIF_IDLST "0".
           COPY LIBCPR.
       M-220.
           CALL "SD_Accept" USING BY REFERENCE A-NGPS "A-NGPS" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-600
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-220
           END-IF
           IF  W-NGPS = ZERO
               ACCEPT W-NGPS FROM DATE
               CALL "SD_Output" USING "A-NGPS" A-NGPS "p" RETURNING RESU
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-220
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-220
           END-IF
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-NG NOT = W-SNG AND W-ENG
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-220
           END-IF.
       M-240.
           CALL "SD_Accept" USING BY REFERENCE A-IDM "A-IDM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-220
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-240
           END-IF
           MOVE 3 TO JCON3-01.
           MOVE W-IDM TO JCON3-02.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               GO TO M-240
           END-IF
           CALL "SD_Output" USING "D-IDMN" D-IDMN "p" RETURNING RESU.
       M-245.
           CALL "SD_Accept" USING BY REFERENCE A-IDS "A-IDS" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-240
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-245
           END-IF
           MOVE 3 TO JCON3-01.
           MOVE W-IDS TO JCON3-02.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               GO TO M-245
           END-IF
           CALL "SD_Output" USING "D-IDSN" D-IDSN "p" RETURNING RESU.
           IF  W-IDM = W-IDS
               GO TO M-245
           END-IF.
       M-260.
           MOVE 0 TO W-GN.
           MOVE 7 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 8 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
       M-270.
           ADD 1 TO W-GN.
           ADD 2 TO W-L1 W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF W-GN = 7
               GO TO M-460
           END-IF.
       M-280.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-440
           END-IF
           IF  ESTAT = ADV
               GO TO M-460
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-280
           END-IF
           MOVE W-HCD(W-GN) TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               GO TO M-280
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           MOVE 0 TO HI-S(4,10).
       M-320.
           CALL "SD_Accept" USING BY REFERENCE A-SIZ "A-SIZ" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-280
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-320
           END-IF
           IF  W-SIZ(W-GN) < 1 OR > 4
               GO TO M-320
           END-IF
           MOVE W-SIZ(W-GN) TO W-SIZD.
           IF  HI-SS(W-SIZD) = ZERO
               GO TO M-320
           END-IF
      *
           MOVE 0 TO CNT.
           MOVE -3 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
       M-340.
           ADD 1 TO CNT.
           ADD 7 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  CNT = 11
               GO TO M-420
           END-IF
           MOVE W-SIZ(W-GN) TO W-SIZD.
           IF  HI-S(W-SIZD,CNT) = ZERO
               MOVE ZERO TO W-SU(W-GN,CNT)
               CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU
               GO TO M-340
           END-IF.
       M-380.
           CALL "SD_Accept" USING BY REFERENCE A-SU "A-SU" "S9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-400
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND ADV
               GO TO M-380
           END-IF
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
           GO TO M-340.
       M-400.
           SUBTRACT 1 FROM CNT.
           SUBTRACT 7 FROM W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  CNT = ZERO
               GO TO M-320
           END-IF
           MOVE W-SIZ(W-GN) TO W-SIZD.
           IF  HI-S(W-SIZD,CNT) = ZERO
               MOVE ZERO TO W-SU(W-GN,CNT)
               CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU
               GO TO M-400
           END-IF
           GO TO M-380.
       M-420.
           COMPUTE W-GSU(W-GN) = W-SU(W-GN,01) + W-SU(W-GN,02)
                               + W-SU(W-GN,03) + W-SU(W-GN,04)
                               + W-SU(W-GN,05) + W-SU(W-GN,06)
                               + W-SU(W-GN,07) + W-SU(W-GN,08)
                               + W-SU(W-GN,09) + W-SU(W-GN,10).
           CALL "SD_Output" USING "D-GSU" D-GSU "p" RETURNING RESU.
           IF  ZERO = W-SU(W-GN,01) AND W-SU(W-GN,02) AND W-SU(W-GN,03)
                 AND W-SU(W-GN,04) AND W-SU(W-GN,05) AND W-SU(W-GN,06)
                 AND W-SU(W-GN,07) AND W-SU(W-GN,08) AND W-SU(W-GN,09)
                 AND W-SU(W-GN,10)
               GO TO M-400
           END-IF
           GO TO M-270.
       M-440.
           SUBTRACT 1 FROM W-GN.
           SUBTRACT 2 FROM W-L1 W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-GN = ZERO
               GO TO M-245
           END-IF
           IF  W-HCD(W-GN) = ZERO
               GO TO M-440
           END-IF
           MOVE W-HCD(W-GN) TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
           END-IF
           MOVE 0 TO HI-S(4,10).
           MOVE 11 TO CNT.
           MOVE 74 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           GO TO M-400.
       M-460.
           IF  W-GN = 1
               GO TO M-280
           END-IF
           IF  W-GN < 7
               MOVE ZERO TO W-MEID(W-GN)
               CALL "SD_Output" USING
                "C-SPACE" C-SPACE "p" RETURNING RESU
               ADD 1 TO W-GN
               ADD 2 TO W-L1 W-L2
               CALL "SD_Arg_Match_Line" USING
                "W-L1" "2" W-L1 RETURNING RESU
               CALL "SD_Arg_Match_Line" USING
                "W-L2" "2" W-L2 RETURNING RESU
               GO TO M-460
           END-IF
           COMPUTE W-SUT = W-GSU(1) + W-GSU(2) + W-GSU(3) + W-GSU(4)
                                               + W-GSU(5) + W-GSU(6).
           CALL "SD_Output" USING "D-SUT" D-SUT "p" RETURNING RESU.
       M-500.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-440
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-500
           END-IF
           IF  W-DMM = 9
               GO TO M-260
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-500
           END-IF
      *
           PERFORM WRI-RTN THRU WRI-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-980
           END-IF
      *
           MOVE ZERO TO W-AMEI W-SUT W-GN.
           MOVE 7 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 8 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
       M-520.
           ADD 1 TO W-GN.
           ADD 2 TO W-L1 W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-GN < 7
               CALL "SD_Output" USING
                "C-SPACE" C-SPACE "p" RETURNING RESU
               GO TO M-520
           END-IF
           GO TO M-260.
       M-600.
           CALL "DB_F_Close" USING
            BY REFERENCE HSKIF_IDLST HSKIF_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" HSKIF_PNAME1 " " BY REFERENCE HSKIF_IDLST "0".
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "D-PRN" D-PRN "p" RETURNING RESU.
       M-620.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-980
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-620
           END-IF
           IF  W-SEN = 1
               MOVE ZERO TO W-SNGP W-SHCD
               MOVE 99999999 TO W-ENGP
               MOVE 999999 TO W-EHCD
               CALL "SD_Output" USING "A-SDAT" A-SDAT "p" RETURNING RESU
               CALL "SD_Output" USING "A-EDAT" A-EDAT "p" RETURNING RESU
               CALL "SD_Output" USING "A-SHCD" A-SHCD "p" RETURNING RESU
               CALL "SD_Output" USING "A-EHCD" A-EHCD "p" RETURNING RESU
               GO TO M-720
           END-IF
           IF  W-SEN NOT = 9
               GO TO M-620
           END-IF.
       M-640.
           CALL "SD_Accept" USING BY REFERENCE A-SDAT "A-SDAT" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-620
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-640
           END-IF
           IF  W-SDAT = ZERO
               MOVE ZERO TO W-SNGP
               GO TO M-660
           END-IF
           MOVE ZERO TO W-NEN1.
           MOVE W-SDAT TO W-NGPS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NGP TO W-SNGP.
       M-660.
           CALL "SD_Accept" USING BY REFERENCE A-EDAT "A-EDAT" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-640
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-660
           END-IF
           IF  W-EDAT = 999999
               MOVE 99999999 TO W-ENGP
               GO TO M-680
           END-IF
           MOVE ZERO TO W-NEN1.
           MOVE W-EDAT TO W-NGPS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NGP TO W-ENGP.
           IF  W-SNGP > W-ENGP
               GO TO M-660
           END-IF.
       M-680.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD "A-SHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-660
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-680
           END-IF.
       M-700.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD "A-EHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-680
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-700
           END-IF
           IF  W-SHCD > W-EHCD
               GO TO M-700
           END-IF.
       M-720.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-SEN = 1
                   GO TO M-620
               ELSE
                   GO TO M-700
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-720
           END-IF
           IF  W-DMM = 9
               GO TO M-620
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-720
           END-IF.
       M-800.
      *           READ HSKIF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HSKIF_PNAME1 BY REFERENCE HSKI-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           IF  HSKI-DATE < W-SNGP OR > W-ENGP
               GO TO M-800
           END-IF
           IF  HSKI-HCD < W-SHCD OR > W-EHCD
               GO TO M-800
           END-IF
           IF  W-SEN = 1
               IF  HSKI-UPD NOT = 0
                   GO TO M-800
               END-IF
           END-IF.
       M-820.
           MOVE 0 TO CHK.
           IF  W-SEN = 1
               MOVE ZERO TO W-SUT
           END-IF
           MOVE HSKI-UNO TO W-UNO1.
       M-840.
           PERFORM MEI-RTN THRU MEI-EX.
       M-860.
      *           READ HSKIF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HSKIF_PNAME1 BY REFERENCE HSKI-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-980
           END-IF
           IF  HSKI-DATE < W-SNGP OR > W-ENGP
               GO TO M-860
           END-IF
           IF  HSKI-HCD < W-SHCD OR > W-EHCD
               GO TO M-860
           END-IF
           IF  W-SEN = 1
               IF  HSKI-UPD NOT = 0
                   GO TO M-860
               END-IF
           END-IF
           IF  HSKI-UNO = W-UNO1
               GO TO M-840
           END-IF
           IF  W-SEN = 1
               PERFORM KEI-RTN THRU KEI-EX
           END-IF
           GO TO M-820.
       M-980.
           IF  W-IPC = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
           END-IF
           IF  W-POC NOT = ZERO
               IF  W-SEN = 9
                   CALL "PR_Close" RETURNING RESP
               ELSE
                   PERFORM KEI-RTN THRU KEI-EX
                   CALL "PR_Close" RETURNING RESP
               END-IF
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSKIF_IDLST HSKIF_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       WRI-RTN.
           MOVE ZERO TO W-UNO1 W-UNO2.
           MOVE "18" TO JCON5-KEY  ERR-K.
      *           READ JCON INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRI-EX
           END-IF
           ADD 1 TO JCON5-03.
           MOVE JCON5-03 TO W-UNO1.
           ADD 1 TO JCON5-03.
           MOVE JCON5-03 TO W-UNO2.
      *           REWRITE JCON5-R  INVALID
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON5-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRI-EX
           END-IF
           MOVE ZERO TO CHK.
       WRI-020.
           ADD 1 TO CHK.
           IF  CHK = 3
               GO TO WRI-EX
           END-IF
           MOVE ZERO TO W-GN.
       WRI-040.
           ADD 1 TO W-GN.
           IF  W-GN = 7
               GO TO WRI-020
           END-IF
           IF  W-HCD(W-GN) = ZERO
               GO TO WRI-020
           END-IF.
       WRI-060.
           MOVE ZERO TO HSKI-R.
           MOVE W-GN TO HSKI-GYO.
           MOVE W-NGP TO HSKI-DATE.
           MOVE W-HCD(W-GN) TO HSKI-HCD.
           MOVE W-SIZ(W-GN) TO HSKI-SIZ.
           IF  CHK = 1
               MOVE W-UNO1 TO HSKI-UNO
               MOVE W-SU(W-GN,01) TO HSKI-SU(01)
               MOVE W-SU(W-GN,02) TO HSKI-SU(02)
               MOVE W-SU(W-GN,03) TO HSKI-SU(03)
               MOVE W-SU(W-GN,04) TO HSKI-SU(04)
               MOVE W-SU(W-GN,05) TO HSKI-SU(05)
               MOVE W-SU(W-GN,06) TO HSKI-SU(06)
               MOVE W-SU(W-GN,07) TO HSKI-SU(07)
               MOVE W-SU(W-GN,08) TO HSKI-SU(08)
               MOVE W-SU(W-GN,09) TO HSKI-SU(09)
               MOVE W-SU(W-GN,10) TO HSKI-SU(10)
               MOVE W-IDS TO HSKI-SKC
               MOVE 1 TO HSKI-IDC
           ELSE
               MOVE W-UNO2 TO HSKI-UNO
               COMPUTE HSKI-SU(01) = -1 * W-SU(W-GN,01)
               COMPUTE HSKI-SU(02) = -1 * W-SU(W-GN,02)
               COMPUTE HSKI-SU(03) = -1 * W-SU(W-GN,03)
               COMPUTE HSKI-SU(04) = -1 * W-SU(W-GN,04)
               COMPUTE HSKI-SU(05) = -1 * W-SU(W-GN,05)
               COMPUTE HSKI-SU(06) = -1 * W-SU(W-GN,06)
               COMPUTE HSKI-SU(07) = -1 * W-SU(W-GN,07)
               COMPUTE HSKI-SU(08) = -1 * W-SU(W-GN,08)
               COMPUTE HSKI-SU(09) = -1 * W-SU(W-GN,09)
               COMPUTE HSKI-SU(10) = -1 * W-SU(W-GN,10)
               MOVE W-IDM TO HSKI-SKC
           END-IF
      *           WRITE HSKI-R.
      *//////////////
           CALL "DB_Insert" USING
            HSKIF_PNAME1 HSKIF_LNAME HSKI-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRI-100
           END-IF
           IF  W-IPC = 0
               MOVE 1 TO W-IPC
           END-IF
           GO TO WRI-040.
       WRI-100.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRI-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE HSKIF_IDLST HSKIF_PNAME1.
           MOVE "HSKIF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" HSKIF_PNAME1 " " BY REFERENCE HSKIF_IDLST "0".
           GO TO WRI-060.
       WRI-EX.
           EXIT.
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
       MID-EX.
           EXIT.
       MEI-RTN.
           IF  W-POC = ZERO
               MOVE 5 TO W-POC
               CALL "PR_Open" RETURNING RESP
               ACCEPT H-DATE FROM DATE
               PERFORM MID-010 THRU MID-EX
           END-IF
           IF  CHK NOT = 0
               GO TO MEI-020
           END-IF
           MOVE 1 TO CHK.
           MOVE 3 TO JCON3-01.
           MOVE HSKI-SKC TO JCON3-02.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO JCON3-03
           END-IF
      *
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-SKN.
           MOVE HSKI-NGPS TO P-DATE.
           MOVE HSKI-SKC TO P-SKC.
           MOVE JCON3-03 TO P-SKN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MEI-020.
           MOVE HSKI-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
           END-IF
           COMPUTE W-TSU = HSKI-SU(01) + HSKI-SU(02) + HSKI-SU(03)
                         + HSKI-SU(04) + HSKI-SU(05) + HSKI-SU(06)
                         + HSKI-SU(07) + HSKI-SU(08) + HSKI-SU(09)
                         + HSKI-SU(10).
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-NAME.
           IF  CHK = 1
               MOVE 2 TO CHK
               MOVE HSKI-UNO TO P-UNO
           END-IF
           MOVE "-" TO P-V.
           MOVE HSKI-GYO TO P-GYO.
           MOVE HSKI-HCD TO P-HCD.
           MOVE HI-NAME TO P-NAME.
           MOVE HSKI-SIZ TO P-SIZ.
           MOVE HSKI-SU(01) TO P-SU(01).
           MOVE HSKI-SU(02) TO P-SU(02).
           MOVE HSKI-SU(03) TO P-SU(03).
           MOVE HSKI-SU(04) TO P-SU(04).
           MOVE HSKI-SU(05) TO P-SU(05).
           MOVE HSKI-SU(06) TO P-SU(06).
           MOVE HSKI-SU(07) TO P-SU(07).
           MOVE HSKI-SU(08) TO P-SU(08).
           MOVE HSKI-SU(09) TO P-SU(09).
           MOVE HSKI-SU(10) TO P-SU(10).
           MOVE W-TSU TO P-TSU.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE HSKI-UNO TO P-UNO
               PERFORM MID-RTN THRU MID-EX
               MOVE SPACE TO SP-R
               MOVE W-P1 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  W-SEN = 1
               ADD W-TSU TO W-SUT
           END-IF
      *
           MOVE 1 TO HSKI-PRN.
      *           REWRITE HSKI-R.
      *///////////////
           CALL "DB_Update" USING
            HSKIF_PNAME1 HSKIF_LNAME HSKI-R RETURNING RET.
       MEI-EX.
           EXIT.
       KEI-RTN.
           IF  CHK NOT = 2
               GO TO KEI-EX
           END-IF
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-NAME.
           MOVE "　　　　　　　　　　　　　［　合　計　］" TO P-NAME.
           MOVE W-SUT TO P-TSU.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
               MOVE SPACE TO SP-R
               MOVE W-P1 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       KEI-EX.
           EXIT.

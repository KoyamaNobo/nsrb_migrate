       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMT050.
      *********************************************************
      *    PROGRAM         :  履物部門（担当）日付別売上問合せ*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHT05                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-SEN              PIC  9(001).
       01  W-DATA.
           02  W-NGP.
             03  W-NG.
               04  W-NEN.
                 05  F        PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-KIN          PIC S9(008).
           02  W-DMM          PIC  9(001).
           02  W-NEND         PIC  9(004).
           02  W-TC           PIC  9(002).
           02  CNT            PIC  9(002).
           02  CNT1           PIC  9(001).
           02  CNT2           PIC  9(001).
           02  CNT3           PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-C1           PIC  9(002).
           02  W-C2           PIC  9(002).
           02  W-C3           PIC  9(002).
       01  W-D.
           02  W-U10   OCCURS  31  PIC S9(008).
           02  W-U20   OCCURS  31  PIC S9(008).
           02  W-U30   OCCURS  31  PIC S9(008).
       01  WT-D.
           02  WT-U10         PIC S9(009).
           02  WT-U20         PIC S9(009).
           02  WT-U30         PIC S9(009).
       01  WK-D.
           02  WK-U10         PIC S9(009).
           02  WK-U20         PIC S9(009).
           02  WK-U30         PIC S9(009).
       01  W-ATTD.
           02  W-ATT   OCCURS   9.
             03  W-TTD   OCCURS  31.
               04  W-TT       PIC S9(008).
             03  W-TTT        PIC S9(009).
             03  W-KTT        PIC S9(009).
       01  W-ATND.
           02  W-ATN.
             03  W-TND   OCCURS   9.
               04  W-TN       PIC  N(003).
           02  W-TNA.
             03  F            PIC  N(024) VALUE
                  "小　林山　崎渡　邉妹　尾青　井坂　田福　嶋三　宅".
             03  F            PIC  N(003) VALUE "合　計".
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
      *FD  SNTR-F
       01  SNTR-F_HMT050.
           02  SNTR-F_PNAME1  PIC  X(005) VALUE "SNTRF".
           02  F              PIC  X(001).
           02  SNTR-F_LNAME   PIC  X(013) VALUE "SNTR-F_HMT050".
           02  F              PIC  X(001).
           02  SNTR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  SNTR-F_SORT    PIC  X(100) VALUE SPACE.
           02  SNTR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  SNTR-F_RES     USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DNO       PIC  9(006).
           02  SNTR-GNO       PIC  9(001).
           02  SNTR-NG        PIC  9(006).
           02  SNTR-PEY       PIC  9(002).
           02  SNTR-D1.
             03  SNTR-TCD     PIC  9(004).
             03  SNTR-HCD     PIC  9(006).
             03  F            PIC  X(041).
             03  SNTR-KIN     PIC S9(008).
             03  F            PIC  X(001).
             03  SNTR-DC      PIC  9(001).
             03  F            PIC  X(012).
             03  SNTR-BC3     PIC  9(002).
             03  F            PIC  X(001).
             03  SNTR-TNC.
               04  SNTR-TNC1  PIC  9(001).
               04  F          PIC  9(001).
             03  F            PIC  X(034).
           02  SNTR-D2    REDEFINES SNTR-D1.
             03  F            PIC  X(088).
             03  SNTR-SHZ     PIC S9(007).
             03  F            PIC  X(017).
           02  SNTR-SNC       PIC  9(001).
       77  F                  PIC  X(001).
      *FD  HBHKF
       01  HBHKF_HMT050.
           02  HBHKF_PNAME1   PIC  X(005) VALUE "HBHKF".
           02  F              PIC  X(001).
           02  HBHKF_LNAME    PIC  X(012) VALUE "HBHKF_HMT050".
           02  F              PIC  X(001).
           02  HBHKF_KEY1     PIC  X(100) VALUE SPACE.
           02  HBHKF_SORT     PIC  X(100) VALUE SPACE.
           02  HBHKF_IDLST    PIC  X(100) VALUE SPACE.
           02  HBHKF_RES      USAGE  POINTER.
       01  HBHK-R.
           02  HBHK-KEY.
             03  HBHK-NG      PIC  9(006).
             03  HBHK-BC.
               04  HBHK-BC3   PIC  9(002).
               04  HBHK-BMNO  PIC  9(001).
               04  HBHK-BC1   PIC  9(002).
           02  HBHK-BMN       PIC  9(002).
           02  HBHK-KKIN      PIC S9(010).
           02  HBHK-JKIN      PIC S9(010).
           02  F              PIC  X(009).
       77  F                  PIC  X(001).
      *FD  HTHKF
       01  HTHKF_HMT050.
           02  HTHKF_PNAME1   PIC  X(005) VALUE "HTHKF".
           02  F              PIC  X(001).
           02  HTHKF_LNAME    PIC  X(012) VALUE "HTHKF_HMT050".
           02  F              PIC  X(001).
           02  HTHKF_KEY1     PIC  X(100) VALUE SPACE.
           02  HTHKF_SORT     PIC  X(100) VALUE SPACE.
           02  HTHKF_IDLST    PIC  X(100) VALUE SPACE.
           02  HTHKF_RES      USAGE  POINTER.
       01  HTHK-R.
           02  HTHK-KEY.
             03  HTHK-NEN     PIC  9(004).
             03  HTHK-TNC     PIC  9(001).
           02  HTHK-ATKG.
             03  HTHK-TKGD  OCCURS  12.
               04  HTHK-KG.
                 05  HTHK-KEI PIC S9(010).
                 05  HTHK-ZIT PIC S9(010).
           02  F              PIC  X(011).
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
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　履物部門・担当日付別売上問合せ　　＊＊＊".
           02  FILLER  PIC  X(010) VALUE
                  "部門別 = 0".
           02  FILLER  PIC  X(016) VALUE
                  "担当別 = 1 ...  ".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-DSP.
           02  D-MID.
             03  FILLER  PIC  N(002) VALUE "担当".
             03  FILLER  PIC  X(030) VALUE
                  "前進=F･5 , 後退=F･4 , 終了=F･9".
           02  D-NG.
             03  01D-NG  PIC  9(002).
             03  02D-NG  PIC Z9.
           02  D-MEI.
             03  01D-MEI PIC ----,---,---.
             03  02D-MEI PIC ----,---,---.
             03  03D-MEI PIC ----,---,---.
           02  D-TOT.
             03  01D-TOT PIC ----,---,--9.
             03  02D-TOT PIC ----,---,--9.
             03  03D-TOT PIC ----,---,--9.
           02  D-KEI.
             03  FILLER  PIC  N(002)       VALUE "計画".
             03  02D-KEI PIC ----,---,--9.
             03  03D-KEI PIC ----,---,--9.
             03  04D-KEI PIC ----,---,--9.
      *
           02  D-TN.
             03  01D-TN  PIC  N(003).
             03  02D-TN  PIC  N(003).
             03  03D-TN  PIC  N(003).
             03  04D-TN  PIC  N(003).
             03  05D-TN  PIC  N(003).
             03  06D-TN  PIC  N(003).
           02  D-TT.
             03  01D-TT  PIC ----,---,---.
             03  02D-TT  PIC ----,---,---.
             03  03D-TT  PIC ----,---,---.
           02  D-TTT.
             03  01D-TTT PIC ----,---,--9.
             03  02D-TTT PIC ----,---,--9.
             03  03D-TTT PIC ----,---,--9.
           02  D-KTT.
             03  FILLER  PIC  N(002)       VALUE "計画".
             03  02D-KTT PIC ----,---,--9.
             03  03D-KTT PIC ----,---,--9.
             03  04D-KTT PIC ----,---,--9.
       01  A-ACP.
           02  A-SEN   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  ﾃﾞｰﾀ ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  ﾀﾝﾄｳ ｴﾗｰ  ***".
             03  E-ME3   PIC  X(019) VALUE
                  "***  ﾌﾞﾝﾙｲ ｴﾗｰ  ***".
             03  E-HCD   PIC  9(006).
             03  E-TCD   PIC  9(006).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
           "C-MID" " " "0" "0" "98" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "1" "10" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "X" "10" "20" "10" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "X" "12" "20" "16" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "X" "23" "52" "22" "03C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "298" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-MID" " " "0" "0" "34" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "01D-MID" "N" "1" "31" "4" " " "D-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-MID" "X" "23" "5" "30" "01D-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-NG" " " "2" "0" "4" "D-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-NG" "9" "2" "2" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING
           "01D-NG" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-NG" "Z9" "2" "6" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING
           "02D-NG" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-MEI" " " "W-L" "0" "36" "D-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-MEI" "----,---,---" "W-L" "W-C1" "12" " " "D-MEI"
            RETURNING RESU.
       CALL "SD_From" USING
           "01D-MEI" BY REFERENCE W-U10(1) "8" "1" BY REFERENCE W-PEY 8
            RETURNING RESU.
       CALL "SD_Init" USING
           "02D-MEI" "----,---,---" "W-L" "W-C2" "12" "01D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "02D-MEI" BY REFERENCE W-U20(1) "8" "1" BY REFERENCE W-PEY 8
            RETURNING RESU.
       CALL "SD_Init" USING
           "03D-MEI" "----,---,---" "W-L" "W-C3" "12" "02D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "03D-MEI" BY REFERENCE W-U30(1) "8" "1" BY REFERENCE W-PEY 8
            RETURNING RESU.
       CALL "SD_Init" USING
           "D-TOT" " " "21" "0" "36" "D-MEI" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-TOT" "----,---,--9" "21" "45" "12" " " "D-TOT"
            RETURNING RESU.
       CALL "SD_From" USING
           "01D-TOT" BY REFERENCE WT-U10 "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-TOT" "----,---,--9" "21" "57" "12" "01D-TOT" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "02D-TOT" BY REFERENCE WT-U20 "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-TOT" "----,---,--9" "21" "69" "12" "02D-TOT" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "03D-TOT" BY REFERENCE WT-U30 "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-KEI" " " "22" "0" "40" "D-TOT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-KEI" "N" "22" "40" "4" " " "D-KEI" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-KEI" "----,---,--9" "22" "45" "12" "01D-KEI" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "02D-KEI" BY REFERENCE WK-U10 "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-KEI" "----,---,--9" "22" "57" "12" "02D-KEI" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "03D-KEI" BY REFERENCE WK-U20 "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "04D-KEI" "----,---,--9" "22" "69" "12" "03D-KEI" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "04D-KEI" BY REFERENCE WT-U30 "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-TN" " " "4" "0" "36" "D-KEI" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-TN" "N" "4" "10" "6" " " "D-TN" RETURNING RESU.
       CALL "SD_From" USING
           "01D-TN" BY REFERENCE W-TN(1) "6" "1" BY REFERENCE CNT1 6
            RETURNING RESU.
       CALL "SD_Init" USING
           "02D-TN" "N" "4" "22" "6" "01D-TN" " " RETURNING RESU.
       CALL "SD_From" USING
           "02D-TN" BY REFERENCE W-TN(1) "6" "1" BY REFERENCE CNT2 6
            RETURNING RESU.
       CALL "SD_Init" USING
           "03D-TN" "N" "4" "34" "6" "02D-TN" " " RETURNING RESU.
       CALL "SD_From" USING
           "03D-TN" BY REFERENCE W-TN(1) "6" "1" BY REFERENCE CNT3 6
            RETURNING RESU.
       CALL "SD_Init" USING
           "04D-TN" "N" "4" "51" "6" "03D-TN" " " RETURNING RESU.
       CALL "SD_From" USING
           "04D-TN" BY REFERENCE W-TN(1) "6" "1" BY REFERENCE CNT1 6
            RETURNING RESU.
       CALL "SD_Init" USING
           "05D-TN" "N" "4" "63" "6" "04D-TN" " " RETURNING RESU.
       CALL "SD_From" USING
           "05D-TN" BY REFERENCE W-TN(1) "6" "1" BY REFERENCE CNT2 6
            RETURNING RESU.
       CALL "SD_Init" USING
           "06D-TN" "N" "4" "75" "6" "05D-TN" " " RETURNING RESU.
       CALL "SD_From" USING
           "06D-TN" BY REFERENCE W-TN(1) "6" "1" BY REFERENCE CNT3 6
            RETURNING RESU.
       CALL "SD_Init" USING
           "D-TT" " " "W-L" "0" "36" "D-TN" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-TT" "----,---,---" "W-L" "W-C1" "12" " " "D-TT"
            RETURNING RESU.
       CALL "SD_From" USING
           "01D-TT" BY REFERENCE W-TT(1,1) "8" "2" BY REFERENCE CNT1 266
            BY REFERENCE W-PEY 8 RETURNING RESU.
       CALL "SD_Init" USING
           "02D-TT" "----,---,---" "W-L" "W-C2" "12" "01D-TT" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "02D-TT" BY REFERENCE W-TT(1,1) "8" "2" BY REFERENCE CNT2 266
            BY REFERENCE W-PEY 8 RETURNING RESU.
       CALL "SD_Init" USING
           "03D-TT" "----,---,---" "W-L" "W-C3" "12" "02D-TT" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "03D-TT" BY REFERENCE W-TT(1,1) "8" "2" BY REFERENCE CNT3 266
            BY REFERENCE W-PEY 8 RETURNING RESU.
       CALL "SD_Init" USING
           "D-TTT" " " "21" "0" "36" "D-TT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-TTT" "----,---,--9" "21" "45" "12" " " "D-TTT"
            RETURNING RESU.
       CALL "SD_From" USING
           "01D-TTT" BY REFERENCE W-TTT(1) "9" "1" BY REFERENCE CNT1 266
            RETURNING RESU.
       CALL "SD_Init" USING
           "02D-TTT" "----,---,--9" "21" "57" "12" "01D-TTT" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "02D-TTT" BY REFERENCE W-TTT(1) "9" "1" BY REFERENCE CNT2 266
            RETURNING RESU.
       CALL "SD_Init" USING
           "03D-TTT" "----,---,--9" "21" "69" "12" "02D-TTT" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "03D-TTT" BY REFERENCE W-TTT(1) "9" "1" BY REFERENCE CNT3 266
            RETURNING RESU.
       CALL "SD_Init" USING
           "D-KTT" " " "22" "0" "40" "D-TTT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-KTT" "N" "22" "40" "4" " " "D-KTT" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-KTT" "----,---,--9" "22" "45" "12" "01D-KTT" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "02D-KTT" BY REFERENCE W-KTT(1) "9" "1" BY REFERENCE CNT1 266
            RETURNING RESU.
       CALL "SD_Init" USING
           "03D-KTT" "----,---,--9" "22" "57" "12" "02D-KTT" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "03D-KTT" BY REFERENCE W-KTT(1) "9" "1" BY REFERENCE CNT2 266
            RETURNING RESU.
       CALL "SD_Init" USING
           "04D-KTT" "----,---,--9" "22" "69" "12" "03D-KTT" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "04D-KTT" BY REFERENCE W-KTT(1) "9" "1" BY REFERENCE CNT3 266
            RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SEN" "9" "12" "35" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "69" "1" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "126" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "126" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" "X" "24" "15" "19" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-HCD" "9" "24" "36" "6" "E-ME3" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-HCD" BY REFERENCE SNTR-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-TCD" "9" "24" "43" "6" "E-HCD" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-TCD" BY REFERENCE SNTR-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-TCD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SEN > 1
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
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
               GO TO M-15
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHT05" RETURNING RESU.
           IF  W-SEN = 1
               MOVE W-TNA TO W-ATN
               MOVE 1 TO CNT1
               MOVE 2 TO CNT2
               MOVE 3 TO CNT3
               CALL "SD_Output" USING
                "D-MID" D-MID "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-TN" D-TN "p" RETURNING RESU
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" SNTR-F_PNAME1 "SHARED" BY REFERENCE
            SNTR-F_IDLST "0".
      *           READ SNTR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE SNTR-NG TO W-NG.
           IF  W-SEN = 1
               MOVE W-NEN TO W-NEND
               IF  W-GET < 5
                   SUBTRACT 1 FROM W-NEND
               END-IF
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1.
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
           MOVE ZERO TO W-D WT-D WK-D W-ATTD.
           CALL "DB_F_Open" USING
            "INPUT" SNTR-F_PNAME1 "SHARED" BY REFERENCE
            SNTR-F_IDLST "0".
       M-20.
      *           READ SNTR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           IF  SNTR-GNO = 9
               GO TO M-20
           END-IF
           IF (SNTR-KIN = ZERO) OR (SNTR-DC = 4 OR 8)
               GO TO M-20
           END-IF
           IF  SNTR-BC3 NOT = 10 AND 20 AND 30
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-KIN = -1 * SNTR-KIN
           ELSE
               MOVE SNTR-KIN TO W-KIN
           END-IF
           IF  SNTR-BC3 = 10
               ADD W-KIN TO W-U10(SNTR-PEY) WT-U10
           END-IF
           IF  SNTR-BC3 = 20
               ADD W-KIN TO W-U20(SNTR-PEY) WT-U20
           END-IF
           IF  SNTR-BC3 = 30
               ADD W-KIN TO W-U30(SNTR-PEY) WT-U30
           END-IF
      *
           IF  SNTR-BC3 = 30
               GO TO M-20
           END-IF
           IF  SNTR-TNC = 00
               GO TO M-20
           END-IF
           COMPUTE CNT = SNTR-TNC1 + 1.
           IF  CNT = 9
               MOVE 3 TO CNT
           END-IF
           IF  CNT > 0 AND < 9
               ADD W-KIN TO W-TT(CNT,SNTR-PEY) W-TTT(CNT)
                            W-TT(9,SNTR-PEY) W-TTT(9)
           END-IF
           GO TO M-20.
       M-25.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1.
           IF  W-SEN = 1
               GO TO M-40
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HBHKF_PNAME1 "SHARED" BY REFERENCE HBHKF_IDLST "1"
            "HBHK-KEY" BY REFERENCE HBHK-KEY.
           MOVE SPACE TO HBHK-KEY.
           MOVE W-NG TO HBHK-NG.
      *           START HBHKF KEY NOT < HBHK-KEY INVALID
      *///////////////
           CALL "DB_Start" USING
            HBHKF_PNAME1 "HBHK-KEY" "NOT < " HBHK-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
      *           READ HBHKF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HBHKF_PNAME1 BY REFERENCE HBHK-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           IF  HBHK-NG NOT = W-NG
               GO TO M-35
           END-IF.
       M-30.
           IF  HBHK-BC3 = 10
               ADD HBHK-KKIN TO WK-U10
           END-IF
           IF  HBHK-BC3 = 20
               ADD HBHK-KKIN TO WK-U20
           END-IF
           IF  HBHK-BC3 = 30
               ADD HBHK-KKIN TO WK-U30
           END-IF
      *           READ HBHKF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HBHKF_PNAME1 BY REFERENCE HBHK-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "D-KEI" D-KEI "p" RETURNING RESU
               GO TO M-35
           END-IF
           IF  HBHK-NG NOT = W-NG
               CALL "SD_Output" USING
                "D-KEI" D-KEI "p" RETURNING RESU
               GO TO M-35
           END-IF
           GO TO M-30.
       M-35.
           CALL "DB_F_Close" USING
            BY REFERENCE HBHKF_IDLST HBHKF_PNAME1.
           GO TO M-55.
       M-40.
           CALL "DB_F_Open" USING
            "INPUT" HTHKF_PNAME1 "SHARED" BY REFERENCE HTHKF_IDLST "1"
            "HTHK-KEY" BY REFERENCE HTHK-KEY.
           MOVE SPACE TO HTHK-KEY.
           MOVE W-NEND TO HTHK-NEN.
      *           START HTHKF KEY NOT < HTHK-KEY INVALID
      *///////////////
           CALL "DB_Start" USING
            HTHKF_PNAME1 "HTHK-KEY" "NOT < " HTHK-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
      *           READ HTHKF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HTHKF_PNAME1 BY REFERENCE HTHK-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  HTHK-NEN NOT = W-NEND
               GO TO M-50
           END-IF
           IF  W-GET < 5
               COMPUTE W-TC = W-GET + 8
           ELSE
               COMPUTE W-TC = W-GET - 4
           END-IF.
       M-45.
           COMPUTE CNT = HTHK-TNC + 1.
           MOVE HTHK-KEI(W-TC) TO W-KTT(CNT).
      *
      *           READ HTHKF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HTHKF_PNAME1 BY REFERENCE HTHK-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  HTHK-NEN NOT = W-NEND
               GO TO M-50
           END-IF
           GO TO M-45.
       M-50.
           CALL "DB_F_Close" USING
            BY REFERENCE HTHKF_IDLST HTHKF_PNAME1.
      *
           MOVE W-TNA TO W-ATN.
           COMPUTE W-KTT(9) = W-KTT(1) + W-KTT(2) + W-KTT(3) + W-KTT(4)
                            + W-KTT(5) + W-KTT(6) + W-KTT(7) + W-KTT(8).
       M-55.
           MOVE ZERO TO W-PEY.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE 4 TO W-C1.
           CALL "SD_Arg_Match_Col" USING "W-C1" "2" W-C1 RETURNING RESU.
           MOVE 16 TO W-C2.
           CALL "SD_Arg_Match_Col" USING "W-C2" "2" W-C2 RETURNING RESU.
           MOVE 28 TO W-C3.
           CALL "SD_Arg_Match_Col" USING "W-C3" "2" W-C3 RETURNING RESU.
       M-60.
           ADD 1 TO W-PEY.
           IF  W-PEY = 32
               GO TO M-65
           END-IF
           IF  W-PEY = 16
               MOVE 4 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE 45 TO W-C1
               CALL "SD_Arg_Match_Col" USING
                "W-C1" "2" W-C1 RETURNING RESU
               MOVE 57 TO W-C2
               CALL "SD_Arg_Match_Col" USING
                "W-C2" "2" W-C2 RETURNING RESU
               MOVE 69 TO W-C3
               CALL "SD_Arg_Match_Col" USING
                "W-C3" "2" W-C3 RETURNING RESU
           END-IF
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-SEN = 1
               CALL "SD_Output" USING "D-TT" D-TT "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-MEI" D-MEI "p" RETURNING RESU
           END-IF
           GO TO M-60.
       M-65.
           IF  W-SEN = 1
               CALL "SD_Output" USING "D-TTT" D-TTT "p" RETURNING RESU
               CALL "SD_Output" USING "D-KTT" D-KTT "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-TOT" D-TOT "p" RETURNING RESU
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  W-SEN = 0
               GO TO M-95
           END-IF
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = PF4 AND PF5
               GO TO M-65
           END-IF
           IF  ESTAT = PF4
               IF  CNT1 = 1
                   GO TO M-65
               ELSE
                   SUBTRACT 3 FROM CNT1 CNT2 CNT3
               END-IF
           END-IF
           IF  ESTAT = PF5
               IF  CNT1 = 7
                   GO TO M-65
               ELSE
                   ADD 3 TO CNT1 CNT2 CNT3
               END-IF
           END-IF
           CALL "SD_Screen_Output" USING "SCHT05" RETURNING RESU.
           CALL "SD_Output" USING "D-MID" D-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TN" D-TN "p" RETURNING RESU.
           GO TO M-55.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG230.
      ******************************************************************
      *    PROGRAM         :  担当者別　請求チェックリスト             *
      *    PRINTER TYPE    :  JIPS                                     *
      *    SCREEN          :  ******                                   *
      *    JS-SIGN         :  0 = 請求  ,  1 = 非請求                  *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  H-MID          PIC  X(114).
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD11.
           02  F              PIC  N(001) VALUE "【".
           02  H-NEN          PIC  9(004).
           02  F              PIC  N(001) VALUE "年".
           02  H-GET          PIC  9(002).
           02  F              PIC  N(001) VALUE "月".
           02  H-PEY          PIC  9(002).
           02  F              PIC  N(004) VALUE "日予定】".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-SEM          PIC  X(014) VALUE SPACE.
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　担当者別　請求チェックリスト　　＊＊＊".
           02  F              PIC  X(027) VALUE SPACE.
       01  HEAD12.
           02  F              PIC  X(039) VALUE SPACE.
           02  F              PIC  N(025) VALUE
                "＊＊＊　　担当者別　非請求チェックリスト　　＊＊＊".
           02  F              PIC  X(025) VALUE SPACE.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "担当".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(008) VALUE "得　意　先　名　".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(008) VALUE "直　送　先　名　".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(002) VALUE "個数".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(008) VALUE "摘　　　　　要　".
           02  F              PIC  X(024) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  N(002) VALUE "月日".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "伝票番号".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "区分".
           02  F              PIC  X(008) VALUE "  ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　単　価".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
       01  W-P1.
           02  F              PIC  X(001).
           02  P-TNC          PIC  9(002).
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(001).
           02  P-CCD          PIC  9(003).
           02  F              PIC  X(001).
           02  P-CNA          PIC  N(026).
           02  F              PIC  X(001).
           02  P-KSU          PIC ZZ9.
           02  P-KSM          PIC  N(002).
           02  F              PIC  X(001).
           02  P-BI           PIC  N(024).
       01  W-P2.
           02  F              PIC  X(030).
           02  P-GP           PIC 99/99.
           02  F              PIC  X(001).
           02  P-DNO          PIC  9(006).
           02  F              PIC  X(001).
           02  P-KBN          PIC  N(002).
           02  F              PIC  X(001).
           02  P-HCD          PIC  X(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  P-SUD          PIC --,---,--9.99.
           02  P-SD    REDEFINES P-SUD.
             03  P-SU         PIC --,---,--9.
             03  F            PIC  X(003).
           02  P-TND          PIC ----,--9.99.
           02  P-TD   REDEFINES P-TND.
             03  P-TN         PIC ----,--9.
             03  F            PIC  X(003).
           02  P-KIN          PIC ----,---,--9.
           02  P-SHZ          PIC --,---,--9.
       01  W-MAD.
           02  W-TNC.
             03  W-TNC1       PIC  9(001).
             03  W-TNC2       PIC  9(001).
           02  W-DNO          PIC  9(006).                              伝票№
           02  W-GP           PIC  9(004).                              日付
           02  W-CSC          PIC  9(001).
           02  W-TCD          PIC  9(004).                              得意先C
           02  W-CCD          PIC  9(003).                              直送№
           02  W-KSU          PIC  9(003).                              個数
           02  W-BI           PIC  N(024).                              備考
           02  W-SHZ          PIC S9(007).                              消費税
           02  W-TKIN         PIC S9(009).
           02  W-BMC          PIC  9(001).
           02  W-MD.
             03  W-M     OCCURS  6.
               04  W-DTC      PIC  9(001).                              区分
               04  W-DC       PIC  9(001).                              伝区
               04  W-HCD      PIC  X(006).
               04  W-HCDD  REDEFINES W-HCD.
                 05  W-KCD    PIC  X(005).
                 05  F        PIC  X(001).
               04  W-SU       PIC S9(006)V9(02).                        数量
               04  W-TN       PIC S9(006)V9(02).                        単価
               04  W-KIN      PIC S9(009).                              金額
       01  W-DATA.
           02  W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-TNCD.
             03  W-TNCD1      PIC  9(001).
             03  W-TNCD2      PIC  9(001).
           02  W-TCDD         PIC  9(004).
           02  W-PAGE         PIC  9(002).
           02  CNT            PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-PD.
             03  W-SUD        PIC S9(006)V9(02).                        数量
             03  W-TND        PIC S9(006)V9(02).                        単価
             03  W-KIND       PIC S9(009).                              金額
             03  W-SHZD       PIC S9(007).                              消費税
           02  W-NAME         PIC  N(024).
           02  W-FRD.
             03  W-FNGP.
               04  W-FNEN     PIC  9(004).
               04  W-FGET     PIC  9(002).
               04  W-FPEY     PIC  9(002).
             03  W-RNGP.
               04  W-RNEN     PIC  9(004).
               04  W-RGET     PIC  9(002).
               04  W-RPEY     PIC  9(002).
       01  W-SEM.
           02  F              PIC  X(001) VALUE "(".
           02  W-FG           PIC Z9.
           02  F              PIC  X(001) VALUE "/".
           02  W-FP           PIC Z9.
           02  F              PIC  N(001) VALUE "～".
           02  W-RG           PIC Z9.
           02  F              PIC  X(001) VALUE "/".
           02  W-RP           PIC Z9.
           02  F              PIC  X(001) VALUE ")".
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LITCM.
           COPY LIHIM.
           COPY LIKHM.
           COPY LIJM.
           COPY LSPF.
      *FD  SKCF
       01  SKCF_HKG230.
           02  SKCF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SKCF_LNAME     PIC  X(011) VALUE "SKCF_HKG230".
           02  F              PIC  X(001).
           02  SKCF_KEY1      PIC  X(100) VALUE SPACE.
           02  SKCF_SORT      PIC  X(100) VALUE SPACE.
           02  SKCF_IDLST     PIC  X(100) VALUE SPACE.
           02  SKCF_RES       USAGE  POINTER.
       01  SKC-R.
           02  SKC-KEY.                                                 KEY
             03  SKC-TCD      PIC  9(004).                              得意先C
             03  SKC-DATE     PIC  9(008).                              日付
             03  SKC-NGP   REDEFINES SKC-DATE.
               04  SKC-NEN    PIC  9(004).
               04  SKC-GP.
                 05  SKC-GET  PIC  9(002).
                 05  SKC-PEY  PIC  9(002).
             03  SKC-DTC      PIC  9(001).                              区分
             03  SKC-DNO      PIC  9(006).                              伝票№
             03  SKC-GNO      PIC  X(001).                              　行№
           02  SKC-HCD        PIC  9(006).                              品名Ｃ
           02  SKC-HCDD  REDEFINES SKC-HCD.
             03  SKC-KCD      PIC  X(005).
             03  F            PIC  X(001).
           02  SKC-SU         PIC S9(006)V9(02).                        数量
           02  SKC-TN         PIC S9(006)V9(02).                        単価
           02  SKC-KIN        PIC S9(009).                              金額
           02  SKC-DC         PIC  9(001).                              伝区
           02  SKC-CSC        PIC  9(001).
           02  SKC-SKD        PIC  9(008).                              請求日
           02  SKC-TNC        PIC  9(002).                              担当Ｃ
           02  SKC-BMC        PIC  9(001).                              部門C
           02  SKC-DCN        PIC  9(003).                              台帳№
           02  SKC-TCD2       PIC  9(004).
           02  SKC-CCD        PIC  9(003).                              直送№
           02  SKC-BI         PIC  N(024).                              備考
           02  SKC-HNO        PIC  9(006).
           02  F              PIC  X(030).
           02  SKC-SHZ        PIC S9(007).                              消費税
           02  SKC-KSU        PIC  9(003).                              個数
           02  SKC-JCD        PIC  9(006).
           02  F              PIC  X(012).
           02  SKD-SNO        PIC  9(006).
           02  F              PIC  X(048).
           02  SKD-SNGP       PIC  9(008).
           02  SKD-ENGP       PIC  9(008).
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　担当別　　請求チェックリスト　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-DSP.
           02  D-HMID  PIC  N(001) VALUE "非".
           02  D-NGP.
             03  01D-NGP  PIC  9(004).
             03  FILLER   PIC  N(001) VALUE "年".
             03  03D-NGP  PIC Z9 .
             03  FILLER   PIC  N(001) VALUE "月".
             03  05D-NGP  PIC Z9 .
             03  FILLER   PIC  N(001) VALUE "日".
             03  FILLER   PIC  N(001) VALUE "分".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
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
            "C-MID" " " "0" "0" "336" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "48" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "48" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "48" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "48" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "48" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "48" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "48" "06C-MID" " "  RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "18" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HMID" "N" "6" "28" "2" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NGP" " " "11" "0" "16" "D-HMID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NGP" "9" "11" "24" "4" " " "D-NGP"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NGP" BY REFERENCE W-NEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NGP" "N" "11" "28" "2" "01D-NGP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-NGP" "Z9" "11" "31" "2" "02D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NGP" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-NGP" "N" "11" "33" "2" "03D-NGP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-NGP" "Z9" "11" "36" "2" "04D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "05D-NGP" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-NGP" "N" "11" "38" "2" "05D-NGP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-NGP" "N" "11" "41" "2" "06D-NGP" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "45" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "45" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN  FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "D-HMID" D-HMID "p" RETURNING RESU
           END-IF
           MOVE ZERO TO W-DATA.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO SKCF_PNAME1.
           IF  JS-SIGN = 1
               GO TO M-10
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" SKCF_PNAME1 " " BY REFERENCE SKCF_IDLST "0".
      *           READ SKCF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SKCF_PNAME1 BY REFERENCE SKC-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SKCF_IDLST SKCF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE SKC-SKD TO W-NGP.
           CALL "SD_Output" USING "D-NGP" D-NGP "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE SKCF_IDLST SKCF_PNAME1.
       M-10.
           PERFORM S-80 THRU S-95.
           CALL "DB_F_Open" USING
            "INPUT" SKCF_PNAME1 " " BY REFERENCE SKCF_IDLST "0".
       M-65.
      *           READ SKCF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SKCF_PNAME1 BY REFERENCE SKC-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SKCF_IDLST SKCF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           IF  JS-SIGN = 0
               MOVE W-NEN TO H-NEN
               MOVE W-GET TO H-GET
               MOVE W-PEY TO H-PEY
               IF  W-FNGP NOT = 99999999
                   MOVE W-FGET TO W-FG
                   MOVE W-FPEY TO W-FP
                   MOVE W-RGET TO W-RG
                   MOVE W-RPEY TO W-RP
                   MOVE W-SEM TO H-SEM
                   MOVE HEAD11 TO H-MID
               ELSE
                   MOVE HEAD11 TO H-MID
               END-IF
           END-IF
           IF  JS-SIGN = 1
               MOVE HEAD12 TO H-MID
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
       M-70.
           MOVE ZERO TO W-MAD CNT.
           MOVE SPACE TO W-BI.
           MOVE SKC-DNO TO W-DNO.
           MOVE SKC-GP TO W-GP.
           MOVE SKC-CSC TO W-CSC.
           MOVE SKC-TNC TO W-TNC.
           MOVE SKC-TCD TO W-TCD.
           MOVE SKC-CCD TO W-CCD.
           MOVE SKC-BMC TO W-BMC.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "＊＊　得意先　なし　＊＊" TO T-NAME
           END-IF
           IF  W-CCD = ZERO OR 001
               GO TO M-75
           END-IF
           MOVE SKC-TCD2 TO TC-TCD.
           MOVE W-CCD TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
               MOVE "＊＊　直送先　なし　＊＊" TO TC-NAME
           END-IF.
       M-75.
           ADD 1 TO CNT.
           IF  CNT > 6
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE SKC-DTC TO W-DTC(CNT).
           MOVE SKC-DC TO W-DC(CNT).
           IF  W-BMC = 3
               MOVE SKC-JCD TO W-HCD(CNT)
           ELSE
               MOVE SKC-HCD TO W-HCD(CNT)                               品名
           END-IF
           MOVE ZERO TO W-PD.
           IF  W-BMC = 0
               IF  SKC-DTC = 1
                   MOVE SKC-SU TO W-SUD
                   COMPUTE W-TND = SKC-TN * -1
                   COMPUTE W-KIND = SKC-KIN * -1
                   COMPUTE W-SHZD = SKC-SHZ * -1
               ELSE
                   IF  SKC-DC = 1 OR 2 OR 5
                       COMPUTE W-SUD = SKC-SU * -1
                       MOVE SKC-TN TO W-TND
                       COMPUTE W-KIND = SKC-KIN * -1
                       MOVE SKC-SHZ TO W-SHZD
                   ELSE
                       MOVE SKC-SU TO W-SUD
                       MOVE SKC-TN TO W-TND
                       MOVE SKC-KIN TO W-KIND
                       MOVE SKC-SHZ TO W-SHZD
                   END-IF
               END-IF
           END-IF
           IF  W-BMC NOT = 0
               IF  SKC-DTC = 0
                   MOVE SKC-SU TO W-SUD
                   MOVE SKC-TN TO W-TND
                   MOVE SKC-KIN TO W-KIND
                   MOVE SKC-SHZ TO W-SHZD
               ELSE
                   COMPUTE W-KIND = SKC-KIN * -1
                   COMPUTE W-SHZD = SKC-SHZ * -1
               END-IF
           END-IF
           MOVE W-SUD TO W-SU(CNT).                                     数量
           MOVE W-TND TO W-TN(CNT).                                     単価
           MOVE W-KIND TO W-KIN(CNT).                                   金額
           MOVE SKC-KSU TO W-KSU.                                       個数
           MOVE SKC-BI TO W-BI.                                         備考
           MOVE W-SHZD TO W-SHZ.                                        消費税
           IF  W-DC(CNT) NOT = 4 AND 8
               ADD W-KIND TO W-TKIN
           END-IF.
       M-80.
      *           READ SKCF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SKCF_PNAME1 BY REFERENCE SKC-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SKC-DNO = W-DNO
               GO TO M-75
           END-IF
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-75.
           GO TO M-70.
       M-90.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-75.
           CALL "DB_F_Close" USING BY REFERENCE SKCF_IDLST SKCF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
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
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-TNA P-CNA P-KSM P-BI.
           IF  W-TNCD = W-TNC
               GO TO S-25
           END-IF
           IF  W-TNCD1 NOT = W-TNC1
               IF  W-TNCD NOT = ZERO
                   PERFORM S-05 THRU S-15
               END-IF
           END-IF
           MOVE W-TNC TO W-TNCD P-TNC.
       S-25.
           IF  W-TCDD NOT = W-TCD
               MOVE W-TCD TO W-TCDD P-TCD
               MOVE T-NAME TO P-TNA
           END-IF
           IF  W-CCD NOT = ZERO AND 001
               MOVE W-CCD TO P-CCD
               MOVE TC-NAME TO P-CNA
           END-IF
           IF  CHK = 0
               MOVE W-BI TO P-BI
               IF  W-KSU NOT = ZERO
                   MOVE W-KSU TO P-KSU
                   MOVE "個口" TO P-KSM
               END-IF
           END-IF
           IF  CHK = 1
               MOVE 0 TO CHK
               MOVE W-TNC TO P-TNC
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE W-TNC TO P-TNC
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-30.
           EXIT.
       S-35.
           MOVE ZERO TO CNT.
       S-40.
           ADD 1 TO CNT.
           IF  CNT > 6
               GO TO S-65
           END-IF
           IF  W-M(CNT) = ZERO
               GO TO S-65
           END-IF
           IF  W-HCD(CNT) = ZERO
               GO TO S-55
           END-IF
           MOVE SPACE TO W-NAME.
           IF  W-BMC NOT = 0
               GO TO S-45
           END-IF
           MOVE W-HCD(CNT) TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "＊＊　品名　なし　＊＊　０" TO HI-NAME
           END-IF
           MOVE HI-NAME TO W-NAME.
           GO TO S-55.
       S-45.
           IF  W-BMC NOT = 1
               GO TO S-50
           END-IF
           MOVE W-KCD(CNT) TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO KH-NAME
               MOVE "**  ﾋﾝﾒｲ ﾅｼ  ** 1" TO KH-NAME
           END-IF
           MOVE KH-NAME TO W-NAME.
           GO TO S-55.
       S-50.
           MOVE W-HCD(CNT) TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO J-NAME
               MOVE "＊＊　品名　なし　＊＊　３" TO J-NAME
           END-IF
           MOVE J-NAME TO W-NAME.
       S-55.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-KBN P-HNA.
           IF  CNT = 1
               MOVE W-GP TO P-GP
               MOVE W-DNO TO P-DNO
           END-IF
           IF  W-DTC(CNT) = 0
               IF  W-CSC = 0
                   MOVE "売上" TO P-KBN
               ELSE
                   MOVE "売調" TO P-KBN
               END-IF
           END-IF
           IF  W-DTC(CNT) = 1
               IF  W-CSC = 0
                   MOVE "値引" TO P-KBN
               ELSE
                   MOVE "値調" TO P-KBN
               END-IF
           END-IF
           IF  W-DC(CNT) = 1
               MOVE "返品" TO P-KBN
           END-IF
           IF  W-DC(CNT) = 2
               MOVE "不良" TO P-KBN
           END-IF
           IF  W-DC(CNT) = 3
               MOVE "預売" TO P-KBN
           END-IF
           IF  W-DC(CNT) = 4
               MOVE "預出" TO P-KBN
           END-IF
           IF  W-HCD(CNT) NOT = ZERO
               MOVE W-HCD(CNT) TO P-HCD
               MOVE W-NAME TO P-HNA
               IF  W-BMC = 0
                   MOVE W-SU(CNT) TO P-SU
               ELSE
                   IF  W-SU(CNT) NOT = ZERO
                       MOVE W-SU(CNT) TO P-SUD
                   END-IF
               END-IF
           END-IF
           IF  W-DC(CNT) NOT = 4 AND 8
               MOVE W-KIN(CNT) TO P-KIN
               IF  W-HCD(CNT) NOT = ZERO
                   IF  W-BMC = 0
                       MOVE W-TN(CNT) TO P-TN
                   ELSE
                       IF  W-TN(CNT) NOT = ZERO
                           MOVE W-TN(CNT) TO P-TND
                       END-IF
                   END-IF
               END-IF
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER NOT > 62
               GO TO S-60
           END-IF
           MOVE W-GP TO P-GP.
           IF  W-BMC = 0
               MOVE W-DNO TO P-DNO
           END-IF
           PERFORM S-05 THRU S-15.
           MOVE 1 TO CHK.
           PERFORM S-20 THRU S-30.
       S-60.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO S-40.
       S-65.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-KBN P-HNA.
           MOVE "　　　　　　　　　　　　　　（　合計　）" TO P-HNA.
           MOVE W-SHZ TO P-SHZ.
           MOVE W-TKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER NOT > 62
               GO TO S-70
           END-IF
           MOVE W-GP TO P-GP.
           IF  W-BMC = 0
               MOVE W-DNO TO P-DNO
           END-IF
           PERFORM S-05 THRU S-15.
           MOVE 1 TO CHK.
           PERFORM S-20 THRU S-30.
       S-70.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-75.
           EXIT.
       S-80.
           MOVE 99999999 TO W-FNGP.
           MOVE ZERO TO W-RNGP.
           CALL "DB_F_Open" USING
            "INPUT" SKCF_PNAME1 " " BY REFERENCE SKCF_IDLST "0".
       S-85.
      *           READ SKCF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SKCF_PNAME1 BY REFERENCE SKC-R " " RETURNING RET.
           IF  RET = 1
               GO TO S-90
           END-IF
           IF  SKC-DATE < W-FNGP
               MOVE SKC-DATE TO W-FNGP
           END-IF
           IF  SKC-DATE > W-RNGP
               MOVE SKC-DATE TO W-RNGP
           END-IF
           GO TO S-85.
       S-90.
           CALL "DB_F_Close" USING BY REFERENCE SKCF_IDLST SKCF_PNAME1.
       S-95.
           EXIT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN210.
      *********************************************************
      *    PROGRAM         :  品名別棚卸チェックリスト        *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :                                  *
      *        変更　　　  :  94/04/05                        *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=通常 , 1=Ｗチェック           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-END              PIC  9(001) VALUE 0.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  H-MID          PIC  N(002) VALUE SPACE.
           02  F              PIC  X(034) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　品名別　棚卸　チェックリスト　　＊＊＊".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(001) VALUE "(".
           02  F              PIC  N(002) VALUE "入数".
           02  F              PIC  X(001) VALUE ")".
           02  F              PIC  X(087) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  N(002) VALUE "倉庫".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　伝票№".
           02  F              PIC  X(010) VALUE SPACE.
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
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "28.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "29.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "30.0".
           02  F              PIC  X(028) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  X(001) VALUE "2".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "12.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "14.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "15.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "16.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "17.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "18.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "19.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "20.0".
           02  F              PIC  X(028) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  X(001) VALUE "3".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(035) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  X(001) VALUE "4".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.5".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  N(004) VALUE "ケース計".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "足数合計".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　総合計".
       01  W-P1.
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(001).
           02  P-F            PIC  X(001).
           02  P-ISU          PIC ZZ9.
           02  P-R            PIC  X(001).
           02  F              PIC  X(002).
           02  P-BC3          PIC  9(002).
           02  P-VER          PIC  X(001).
           02  P-BC1          PIC  9(002).
           02  F              PIC  X(042).
           02  P-ZM           PIC  N(010).
           02  F              PIC  X(023).
       01  W-P2.
           02  P-TM           PIC  N(012).
           02  P-SNO          PIC  9(001).
           02  F              PIC  X(001).
           02  P-DNO1         PIC  9(002).
           02  P-V            PIC  X(001).
           02  P-DNO2         PIC  X(004).
           02  F              PIC  X(001).
           02  P-SM           PIC  N(006).
           02  P-SIZ          PIC  9(001).
           02  P-SUD.
             03  P-SU         PIC ---,--9  OCCURS  10.
           02  P-KST          PIC ----,--9.
           02  P-SST          PIC --,---,--9.
           02  P-AST          PIC --,---,--9.
       01  W-ASU.
           02  W-SUD1.
             03  W-SU1        PIC S9(006)  OCCURS  10.
           02  W-SUD2.
             03  W-SU2        PIC S9(006)  OCCURS  10.
           02  W-SUD3.
             03  W-SU3        PIC S9(006)  OCCURS  10.
           02  W-SUD4.
             03  W-SU4        PIC S9(006)  OCCURS  10.
           02  W-SUD5.
             03  W-SU5        PIC S9(006)  OCCURS  10.
           02  W-SUD6.
             03  W-SU6        PIC S9(006)  OCCURS  10.
           02  W-SUD7.
             03  W-SU7        PIC S9(006)  OCCURS  10.
           02  W-SUD8.
             03  W-SU8        PIC S9(006)  OCCURS  10.
       01  W-SUD.
           02  W-SU           PIC S9(006)  OCCURS  10.
       01  W-DATA.
           02  W-SNO          PIC  9(001).
           02  W-DNO          PIC  X(006).
           02  W-DNOD  REDEFINES W-DNO.
             03  W-DNO1       PIC  9(002).
             03  W-DNO2       PIC  X(004).
           02  W-HCD          PIC  9(006) VALUE ZERO.
           02  W-PAGE         PIC  9(003) VALUE ZERO.
           02  CNT            PIC  9(002).
           02  W-C            PIC  9(001).
           02  W-SC           PIC  9(001).
           02  W-ZCD.
             03  W-ZC         PIC  9(001) OCCURS  8.
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
             03  CHK3         PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-PC           PIC  9(001).
           02  W-SEN          PIC  9(001).
           02  W-SE.
             03  W-SBC3       PIC  9(002).
             03  W-EBC3       PIC  9(002) VALUE 99.
             03  W-SHCD       PIC  9(006).
             03  W-EHCD       PIC  9(006) VALUE 999999.
           02  W-TC           PIC  9(001).
           02  W-TCD          PIC  9(001).
           02  W-ISU          PIC  9(003).
           02  W-T.
             03  W-KST        PIC S9(006).
             03  W-SST        PIC S9(007).
             03  W-HST        PIC S9(007).
             03  W-AST        PIC S9(007).
           02  WS-T.
             03  WS-KST       PIC S9(006).
             03  WS-SST       PIC S9(007).
             03  WS-HST       PIC S9(007).
             03  WS-AST       PIC S9(007).
           02  WA-AST         PIC S9(007).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHTIM.
           COPY LIHIM.
           COPY LSPF.
      *FD  HTIW-M
       01  HTIW-M_HMN210.
           02  HTIW-M_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HTIW-M_LNAME   PIC  X(013) VALUE "HTIW-M_HMN210".
           02  F              PIC  X(001).
           02  HTIW-M_KEY1    PIC  X(100) VALUE SPACE.
           02  HTIW-M_SORT    PIC  X(100) VALUE SPACE.
           02  HTIW-M_IDLST   PIC  X(100) VALUE SPACE.
           02  HTIW-M_RES     USAGE  POINTER.
       01  HTIW-R.
           02  HTIW-KEY.
             03  HTIW-DNO.
               04  HTIW-DNO1  PIC  9(005).
               04  HTIW-DNO2  PIC  X(001).
             03  HTIW-GNO     PIC  9(001).
           02  HTIW-SNO       PIC  9(001).
           02  HTIW-HCD       PIC  9(006).
           02  HTIW-SIZ       PIC  9(001).
           02  HTIW-SUD.
             03  HTIW-SU      PIC S9(006)  OCCURS  10.
           02  HTIW-BC.
             03  HTIW-BC1     PIC  9(002).
             03  HTIW-BC2     PIC  9(002).
             03  HTIW-BC3     PIC  9(002).
           02  HTIW-ISU       PIC  9(003).
           02  F              PIC  X(044).
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
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　品名別　棚卸チェックリスト　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(032) VALUE
                  "通常=0 , チェック=1 (玉島) ...  ".
           02  FILLER.
             03  FILLER  PIC  X(022) VALUE
                  "分類ｺｰﾄﾞ3     00 ～ 99".
             03  FILLER  PIC  X(031) VALUE
                  "(一　般=10,ワーク=20,教　育=30)".
           02  FILLER  PIC  X(026) VALUE
                "品名ｺｰﾄﾞ  000000 ～ 999999".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  FILLER.
             03  A-SBC3  PIC  9(002).
             03  A-EBC3  PIC  9(002).
           02  FILLER.
             03  A-SHCD  PIC  9(006).
             03  A-EHCD  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(025) VALUE
                  "***  DATA ﾅｼ  ***        ".
             03  E-ME2   PIC  X(025) VALUE
                  "***  HTIM ﾅｼ  ***        ".
             03  E-ME3   PIC  X(025) VALUE
                  "***  HTIM REWRITE ｴﾗｰ  **".
             03  E-ME5   PIC  X(025) VALUE
                  "***  ｲﾘｽｳ ZERO  ***      ".
             03  E-ME6   PIC  X(025) VALUE
                  "***  ﾀﾝｶ ZERO  ***       ".
             03  E-KEY   PIC  X(007).
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "363" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "X" "10" "20" "32" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" " " "13" "0" "53" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "13" "20" "22" " " "07C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "13" "49" "31" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "10C-MID" "X" "15" "20" "26" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "11C-MID" "X" "22" "29" "22" "10C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "18" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SEN" "9" "10" "51" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "13" "0" "4" "A-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SBC3" "9" "13" "34" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SBC3" BY REFERENCE W-SBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EBC3" "9" "13" "40" "2" "A-SBC3" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EBC3" BY REFERENCE W-EBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-ACP" " " "15" "0" "12" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SHCD" "9" "15" "30" "6" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SHCD" BY REFERENCE W-SHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EHCD" "9" "15" "40" "6" "A-SHCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EHCD" BY REFERENCE W-EHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "22" "46" "1" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "228" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "228" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "25" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "25" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" "X" "24" "15" "25" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME5" "X" "24" "15" "25" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME6" "X" "24" "15" "25" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "X" "24" "45" "7" "E-ME6" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE HTI-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME78" "N" "24" "5" "4" "E-KEY" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME78" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" " " "24" "0" "80" "E-ME99" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01E-CL" "X" "24" "1" "40" " " "E-CL" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-CL" "X" "24" "41" "40" "01E-CL" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
      *
           PERFORM ACP-RTN THRU ACP-EX.
           IF  W-END NOT = 0
               GO TO M-95
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO HTIW-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HTIW-M_PNAME1 " " BY REFERENCE HTIW-M_IDLST "0".
       M-10.
      *           READ HTIW-M AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HTIW-M_PNAME1 BY REFERENCE HTIW-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HTIW-M_IDLST HTIW-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-SEN = 1
               IF  HTIW-SNO NOT = 6
                   GO TO M-10
               ELSE
                   IF  HTIW-BC3 = 30
                       GO TO M-10
                   END-IF
               END-IF
           END-IF
           IF  HTIW-HCD < W-SHCD
               GO TO M-10
           END-IF
           IF  HTIW-HCD > W-EHCD
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HTIW-M_IDLST HTIW-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  HTIW-BC3 < W-SBC3 OR > W-EBC3
               GO TO M-10
           END-IF
           MOVE ZERO TO WA-AST.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           IF  JS-SIGN = 1
               MOVE "Ｃ２" TO H-MID
           ELSE
               CALL "DB_F_Open" USING
                "I-O" HTI-M_PNAME1 "SHARED" BY REFERENCE HTI-M_IDLST "1"
                "HTI-KEY" BY REFERENCE HTI-KEY
               IF  W-SEN = 1
                   MOVE "Ｃ１" TO H-MID
               END-IF
           END-IF
           CALL "PR_Open" RETURNING RESP.
           PERFORM MID-010 THRU MID-EX.
       M-15.
           MOVE HTIW-HCD TO W-HCD.
           MOVE HTIW-ISU TO W-ISU.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO HI-FT HI-ISU HI-BC1 HI-BC3
               MOVE SPACE TO HI-NAME
               MOVE "　＊＊　マスター　なし　＊＊　　　" TO HI-NAME
           END-IF
           IF  JS-SIGN = 0
               IF  W-ISU = ZERO
                   IF  HI-ISU NOT = ZERO
                       PERFORM REW-RTN THRU REW-EX
                   END-IF
               END-IF
           END-IF
           PERFORM ME1-RTN THRU ME1-EX.
           MOVE ZERO TO W-ASU WS-T W-ZCD CHK W-C.
       M-20.
           MOVE HTIW-SNO TO W-SNO.
           MOVE ZERO TO CHK2.
       M-25.
           MOVE HTIW-DNO TO W-DNO.
           MOVE ZERO TO W-T CHK3.
       M-30.
           IF  HTIW-GNO < 5
               MOVE 1 TO W-TC
           ELSE
               MOVE 2 TO W-TC
           END-IF
           MOVE 0 TO W-TCD.
       M-35.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-TM P-SM.
           IF  CHK2 = 0
               MOVE 5 TO CHK2
               MOVE W-SNO TO P-SNO
           END-IF
           IF  CHK3 = 0
               MOVE 5 TO CHK3
               MOVE W-DNO1 TO P-DNO1
               MOVE "-" TO P-V
               MOVE W-DNO2 TO P-DNO2
           END-IF
           IF  W-TC NOT = W-TCD
               IF  W-TC = 1
                   MOVE "［ケース］　" TO P-SM
               ELSE
                   MOVE "［端　数］　" TO P-SM
               END-IF
           END-IF
           MOVE HTIW-SIZ TO P-SIZ.
           IF  W-ZC(HTIW-GNO) = 0
               MOVE 1 TO W-ZC(HTIW-GNO)
           END-IF
           MOVE ZERO TO CNT.
       M-40.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-45
           END-IF
           IF  HTIW-GNO = 2 OR 6
               IF  CNT > 9
                   GO TO M-40
               END-IF
           END-IF
           IF  HTIW-GNO = 3 OR 7
               IF  CNT > 8
                   GO TO M-40
               END-IF
           END-IF
           MOVE HTIW-SU(CNT) TO P-SU(CNT).
           IF  HTIW-GNO = 1
               ADD HTIW-SU(CNT) TO W-SU1(CNT) W-KST
           END-IF
           IF  HTIW-GNO = 2
               ADD HTIW-SU(CNT) TO W-SU2(CNT) W-KST
           END-IF
           IF  HTIW-GNO = 3
               ADD HTIW-SU(CNT) TO W-SU3(CNT) W-KST
           END-IF
           IF  HTIW-GNO = 4
               ADD HTIW-SU(CNT) TO W-SU4(CNT) W-KST
           END-IF
           IF  HTIW-GNO = 5
               ADD HTIW-SU(CNT) TO W-SU5(CNT) W-HST
           END-IF
           IF  HTIW-GNO = 6
               ADD HTIW-SU(CNT) TO W-SU6(CNT) W-HST
           END-IF
           IF  HTIW-GNO = 7
               ADD HTIW-SU(CNT) TO W-SU7(CNT) W-HST
           END-IF
           IF  HTIW-GNO = 8
               ADD HTIW-SU(CNT) TO W-SU8(CNT) W-HST
           END-IF
           GO TO M-40.
       M-45.
      *           READ HTIW-M AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HTIW-M_PNAME1 BY REFERENCE HTIW-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  W-SEN = 1
               IF  HTIW-SNO NOT = 6
                   GO TO M-45
               ELSE
                   IF  HTIW-BC3 = 30
                       GO TO M-45
                   END-IF
               END-IF
           END-IF
           IF  HTIW-HCD > W-EHCD
               GO TO M-90
           END-IF
           IF  HTIW-BC3 < W-SBC3 OR > W-EBC3
               GO TO M-45
           END-IF
           IF  HTIW-GNO < 5
               MOVE 1 TO W-TCD
           ELSE
               MOVE 2 TO W-TCD
           END-IF
           IF  HTIW-DNO NOT = W-DNO
               GO TO M-50
           END-IF
           IF  W-TC = W-TCD
               GO TO M-55
           END-IF
           IF  W-TC = 1
               COMPUTE W-SST = W-KST * W-ISU
               ADD W-KST TO WS-KST
               ADD W-SST TO WS-SST
               MOVE W-KST TO P-KST
               MOVE W-SST TO P-SST
           ELSE
               ADD W-HST TO WS-HST
               MOVE W-HST TO P-SST
           END-IF
           GO TO M-55.
       M-50.
           IF  W-TC = 1
               COMPUTE W-SST = W-KST * W-ISU
               ADD W-KST TO WS-KST
               ADD W-SST TO WS-SST
               MOVE W-KST TO P-KST
               MOVE W-SST TO P-SST
           ELSE
               ADD W-HST TO WS-HST
               MOVE W-HST TO P-SST
           END-IF
           COMPUTE W-AST = W-SST + W-HST.
           ADD W-AST TO WS-AST WA-AST.
           MOVE W-AST TO P-AST.
           IF  W-C = 5
               MOVE 9 TO W-C
           END-IF
           IF  W-C = 0
               MOVE 5 TO W-C
           END-IF.
       M-55.
           PERFORM ME2-RTN THRU ME2-EX.
           IF  HTIW-HCD NOT = W-HCD
               GO TO M-60
           END-IF
           IF  HTIW-SNO NOT = W-SNO
               GO TO M-20
           END-IF
           IF  HTIW-DNO NOT = W-DNO
               GO TO M-25
           END-IF
           IF  W-TC NOT = W-TCD
               GO TO M-30
           END-IF
           GO TO M-35.
       M-60.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  W-C = 9
               PERFORM SUB-RTN THRU SUB-EX
           END-IF
           GO TO M-15.
       M-90.
           IF  W-TC = 1
               COMPUTE W-SST = W-KST * W-ISU
               ADD W-KST TO WS-KST
               ADD W-SST TO WS-SST
               MOVE W-KST TO P-KST
               MOVE W-SST TO P-SST
           ELSE
               ADD W-HST TO WS-HST
               MOVE W-HST TO P-SST
           END-IF
           COMPUTE W-AST = W-SST + W-HST.
           ADD W-AST TO WS-AST WA-AST.
           MOVE W-AST TO P-AST.
           IF  W-C = 5
               MOVE 9 TO W-C
           END-IF
           IF  W-C = 0
               MOVE 5 TO W-C
           END-IF
           PERFORM ME2-RTN THRU ME2-EX.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  W-C = 9
               PERFORM SUB-RTN THRU SUB-EX
           END-IF
           PERFORM ALL-RTN THRU ALL-EX.
           IF  JS-SIGN = 0
               CALL "DB_F_Close" USING
                BY REFERENCE HTIW-M_IDLST HTIW-M_PNAME1
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HTIW-M_IDLST HTIW-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           IF  JS-SIGN = 1
               MOVE 1 TO W-SEN
               CALL "SD_Output" USING "A-SEN" A-SEN "p" RETURNING RESU
               GO TO ACP-020
           END-IF.
       ACP-010.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               MOVE 1 TO W-END
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-010
           END-IF
           IF  W-SEN > 1
               GO TO ACP-010
           END-IF.
       ACP-020.
           CALL "SD_Accept" USING BY REFERENCE A-SBC3 "A-SBC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               MOVE 1 TO W-END
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               IF  JS-SIGN = 0
                   GO TO ACP-010
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-020
           END-IF.
       ACP-030.
           CALL "SD_Accept" USING BY REFERENCE A-EBC3 "A-EBC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-030
           END-IF
           IF  W-SBC3 > W-EBC3
               GO TO ACP-030
           END-IF.
       ACP-040.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD "A-SHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               MOVE 1 TO W-END
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-030
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-040
           END-IF.
       ACP-050.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD "A-EHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-050
           END-IF
           IF  W-SHCD > W-EHCD
               GO TO ACP-050
           END-IF.
       ACP-090.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-050
           END-IF
           IF  ESTAT = HTB AND SKP
               GO TO ACP-090
           END-IF
           IF  W-DMM = 9
               GO TO ACP-RTN
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-090
           END-IF.
       ACP-EX.
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
       ME1-RTN.
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-NAME P-ZM.
           MOVE W-HCD TO P-HCD.
           MOVE HI-NAME TO P-NAME.
           MOVE "(" TO P-F.
           MOVE W-ISU TO P-ISU.
           MOVE ")" TO P-R.
           MOVE HI-BC3 TO P-BC3.
           MOVE "-" TO P-VER.
           MOVE HI-BC1 TO P-BC1.
           IF  ZERO = HI-FT AND W-ISU
               MOVE "　（単価０，入数０）" TO P-ZM
           END-IF
           IF  HI-FT = ZERO
               IF  W-ISU NOT = ZERO
                   MOVE "　　　　　（単価０）" TO P-ZM
               END-IF
           END-IF
           IF  W-ISU = ZERO
               IF  HI-FT NOT = ZERO
                   MOVE "　　　　　（入数０）" TO P-ZM
               END-IF
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       ME1-EX.
           EXIT.
       ME2-RTN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER NOT > 62
               GO TO ME2-010
           END-IF
           PERFORM MID-RTN THRU MID-EX.
           PERFORM ME1-RTN THRU ME1-EX.
           MOVE W-SNO TO P-SNO.
           MOVE W-DNO1 TO P-DNO1.
           MOVE "-" TO P-V.
           MOVE W-DNO2 TO P-DNO2.
           IF  W-TC = 1
               MOVE "［ケース］　" TO P-SM
           ELSE
               MOVE "［端　数］　" TO P-SM
           END-IF.
       ME2-010.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       ME2-EX.
           EXIT.
       SUB-RTN.
           MOVE ZERO TO W-SC CHK.
       SUB-010.
           ADD 1 TO W-SC.
           IF  W-SC = 9
               GO TO SUB-EX
           END-IF
           IF  W-ZC(W-SC) = 0
               GO TO SUB-010
           END-IF
      *
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-TM P-SM.
           IF  CHK1 = 0
               MOVE 5 TO CHK1
               MOVE "　　　　　　［　合計　］" TO P-TM
           END-IF
           MOVE ZERO TO W-SUD.
           IF  W-SC = 1
               MOVE W-SUD1 TO W-SUD
               MOVE 2 TO P-SIZ
               MOVE "［ケース］　" TO P-SM
           END-IF
           IF  W-SC = 2
               MOVE W-SUD2 TO W-SUD
               MOVE 3 TO P-SIZ
               IF  ZERO = W-ZC(1)
                   MOVE "［ケース］　" TO P-SM
               END-IF
           END-IF
           IF  W-SC = 3
               MOVE W-SUD3 TO W-SUD
               MOVE 4 TO P-SIZ
               IF  ZERO = W-ZC(1) AND W-ZC(2)
                   MOVE "［ケース］　" TO P-SM
               END-IF
           END-IF
           IF  W-SC = 4
               MOVE W-SUD4 TO W-SUD
               MOVE 1 TO P-SIZ
               IF  ZERO = W-ZC(1) AND W-ZC(2) AND W-ZC(3)
                   MOVE "［ケース］　" TO P-SM
               END-IF
           END-IF
           IF  W-SC = 5
               MOVE W-SUD5 TO W-SUD
               MOVE 2 TO P-SIZ
               MOVE "［端　数］　" TO P-SM
           END-IF
           IF  W-SC = 6
               MOVE W-SUD6 TO W-SUD
               MOVE 3 TO P-SIZ
               IF  ZERO = W-ZC(5)
                   MOVE "［端　数］　" TO P-SM
               END-IF
           END-IF
           IF  W-SC = 7
               MOVE W-SUD7 TO W-SUD
               MOVE 4 TO P-SIZ
               IF  ZERO = W-ZC(5) AND W-ZC(6)
                   MOVE "［端　数］　" TO P-SM
               END-IF
           END-IF
           IF  W-SC = 8
               MOVE W-SUD8 TO W-SUD
               MOVE 1 TO P-SIZ
               IF  ZERO = W-ZC(5) AND W-ZC(6) AND W-ZC(7)
                   MOVE "［端　数］　" TO P-SM
               END-IF
           END-IF
           MOVE ZERO TO CNT.
       SUB-020.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO SUB-030
           END-IF
           IF  W-SC = 2 OR 6
               IF  CNT > 9
                   GO TO SUB-020
               END-IF
           END-IF
           IF  W-SC = 3 OR 7
               IF  CNT > 8
                   GO TO SUB-020
               END-IF
           END-IF
           MOVE W-SU(CNT) TO P-SU(CNT).
           GO TO SUB-020.
       SUB-030.
           IF  W-SC = 1
               IF  ZERO = W-ZC(2) AND W-ZC(3) AND W-ZC(4)
                   MOVE WS-KST TO P-KST
                   MOVE WS-SST TO P-SST
                   IF  ZERO = W-ZC(5) AND W-ZC(6) AND W-ZC(7)
                                                 AND W-ZC(8)
                       MOVE WS-AST TO P-AST
                   END-IF
               END-IF
           END-IF
           IF  W-SC = 2
               IF  ZERO = W-ZC(3) AND W-ZC(4)
                   MOVE WS-KST TO P-KST
                   MOVE WS-SST TO P-SST
                   IF  ZERO = W-ZC(5) AND W-ZC(6) AND W-ZC(7)
                                                 AND W-ZC(8)
                       MOVE WS-AST TO P-AST
                   END-IF
               END-IF
           END-IF
           IF  W-SC = 3
               IF  ZERO = W-ZC(4)
                   MOVE WS-KST TO P-KST
                   MOVE WS-SST TO P-SST
                   IF  ZERO = W-ZC(5) AND W-ZC(6) AND W-ZC(7)
                                                 AND W-ZC(8)
                       MOVE WS-AST TO P-AST
                   END-IF
               END-IF
           END-IF
           IF  W-SC = 4
               MOVE WS-KST TO P-KST
               MOVE WS-SST TO P-SST
               IF  ZERO = W-ZC(5) AND W-ZC(6) AND W-ZC(7) AND W-ZC(8)
                   MOVE WS-AST TO P-AST
               END-IF
           END-IF
           IF  W-SC = 5
               IF  ZERO = W-ZC(6) AND W-ZC(7) AND W-ZC(8)
                   MOVE WS-HST TO P-SST
                   MOVE WS-AST TO P-AST
               END-IF
           END-IF
           IF  W-SC = 6
               IF  ZERO = W-ZC(7) AND W-ZC(8)
                   MOVE WS-HST TO P-SST
                   MOVE WS-AST TO P-AST
               END-IF
           END-IF
           IF  W-SC = 7
               IF  ZERO = W-ZC(8)
                   MOVE WS-HST TO P-SST
                   MOVE WS-AST TO P-AST
               END-IF
           END-IF
           IF  W-SC = 8
               MOVE WS-HST TO P-SST
               MOVE WS-AST TO P-AST
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
               PERFORM ME1-RTN THRU ME1-EX
               MOVE "　　　　　　［　合計　］" TO P-TM
               IF  W-SC < 5
                   MOVE "［ケース］　" TO P-SM
               ELSE
                   MOVE "［端　数］　" TO P-SM
               END-IF
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO SUB-010.
       SUB-EX.
           EXIT.
       ALL-RTN.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-TM P-SM.
           MOVE "　　【　総　合　計　】　" TO P-TM.
           MOVE WA-AST TO P-AST.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       ALL-EX.
           EXIT.
       REW-RTN.
           MOVE HTIW-KEY TO HTI-KEY.
      *           READ HTI-M INVALID KEY
      *//////////////////////     
           CALL "DB_Read" USING
            "INVALID KEY" HTI-M_PNAME1 BY REFERENCE HTI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO REW-EX
           END-IF
           MOVE HI-ISU TO HTI-ISU W-ISU.
      *           REWRITE HTI-R INVALID KEY
      *//////////////////////
           CALL "DB_Update" USING
            HTI-M_PNAME1 HTI-M_LNAME HTI-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       REW-EX.
           EXIT.

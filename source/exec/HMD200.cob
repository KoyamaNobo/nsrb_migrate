       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD200.
      *********************************************************
      *    PROGRAM         :  履物出荷指図ＴＲＡＮ　自動変換  *
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
       77  CHK                PIC  9(001) VALUE 0.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-TCD          PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  W-SNO          PIC  9(006).
           02  W-DNO          PIC  9(006).
           02  W-GNO          PIC  9(001).
           02  W-BCHK         PIC  9(001).
           02  W-NG           PIC  9(006).
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NGD.
               04  W-NEN      PIC  9(004).
               04  W-NEND  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPSD REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
             03  F            PIC  9(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LIHSMS.
           COPY LIHKBM.
      *FD  HSMSW
       01  HSMSW_HMD200.
           02  HSMSW_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HSMSW_LNAME    PIC  X(012) VALUE "HSMSW_HMD200".
           02  HSMSW_KEY1     PIC  X(100) VALUE SPACE.
           02  HSMSW_SORT     PIC  X(100) VALUE SPACE.
           02  HSMSW_IDLST    PIC  X(100) VALUE SPACE.
           02  HSMSW_RES      USAGE  POINTER.
       01  HSMSW-R1.
           02  HSMSW-KEY.                                               KEY
             03  HSMSW-01     PIC 9(6).                                 ｼｭｯｶｼｽﾞ
             03  HSMSW-02     PIC 9(1).                                 ｷﾞｮｳ
           02  HSMSW-03       PIC 9(1).                                 ﾃﾞﾝｸ
           02  HSMSW-05.                                                ｼｭｯｶﾋﾞｼﾞ
             03  HSMSW-051    PIC 9(4).
             03  HSMSW-052    PIC 9(2).                                 ﾂｷ
             03  HSMSW-053    PIC 9(2).                                 ﾋ
           02  HSMSW-06.                                                ﾁｮｸｿｳ CD
             03  HSMSW-061    PIC 9(4).                                 ﾄｸｲｺｰﾄﾞ
             03  HSMSW-062    PIC 9(3).                                 ﾁｮｸ NO
           02  HSMSW-07       PIC 9(1).                                 ｸﾗ ｺｰﾄﾞ
           02  HSMSW-09       PIC 9(6).                                 ﾋﾝｺｰﾄﾞ
           02  HSMSW-10       PIC 9(1).                                 ｻｲｽﾞｸﾌﾞﾝ
           02  HSMSW-12.                                                ｼｭｯｶｼﾞﾂ
             03  HSMSW-121    OCCURS  10.                               ｻｲｽﾞﾍﾞﾂ
               04  HSMSW-1211 PIC S9(4).
             03  HSMSW-122    PIC S9(6).
           02  HSMSW-13       PIC 9(1).                                 ｱｽﾞｶﾘ KB
           02  HSMSW-14       PIC S9(03).                               個数
           02  HSMSW-21       PIC 9(01).                                ｲﾝｼﾞｸﾌﾞﾝ
           02  HSMSW-20       PIC 9(02).                                ﾀﾝﾄｳ
           02  HSMSW-16       PIC 9(02).                                ﾌﾞﾝﾙｲ2
           02  HSMSW-17       PIC 9(05).                                ﾀﾝｶ
           02  HSMSW-18       PIC 9(08).
           02  HSMSW-18D  REDEFINES HSMSW-18.
             03  F            PIC 9(02).
             03  HSMSW-NG     PIC 9(04).
             03  F            PIC 9(02).
           02  HSMSW-22       PIC X(10).
           02  HSMSW-23       PIC 9(01).                                ﾍﾝｶﾝｸﾌﾞﾝ
           02  HSMSW-24       PIC 9(01).
           02  FILLER         PIC X(14).
           02  HSMSW-26       PIC 9(01).
           02  HSMSW-25       PIC 9(01).
           02  HSMSW-19       PIC 9(01).                                ｼﾖｳｶｲｽｳ
       01  HSMSW-R2.
           02  HSMSW-KEYB.                                              KEY
             03  HSMSW-01B    PIC 9(6).                                 ｼｭｯｶｼｽﾞ
             03  HSMSW-02B    PIC 9(1).                                 ｷﾞｮｳ
           02  HSMSW-03B      PIC 9(1).
           02  HSMSW-05B.
             03  HSMSW-051B   PIC 9(4).
             03  HSMSW-052B   PIC 9(2).
             03  HSMSW-053B   PIC 9(2).
           02  HSMSW-06B.
             03  HSMSW-061B   PIC 9(4).
             03  HSMSW-062B   PIC 9(3).
           02  HSMSW-07B      PIC 9(1).
           02  HSMSW-15       PIC N(24).                                摘要
           02  FILLER         PIC X(37).
           02  HSMSW-23B      PIC 9(01).
           02  HSMSW-24B      PIC 9(01).
           02  FILLER         PIC X(14).
           02  HSMSW-26B      PIC 9(01).
           02  HSMSW-25B      PIC 9(01).
           02  HSMSW-19B      PIC 9(01).                                ｼﾖｳｶｲｽｳ
       77  F                  PIC  X(001).
      *FD  S-TRAN
       01  S-TRAN_HMD200.
           02  S-TRAN_PNAME1  PIC  X(005) VALUE "STRAN".
           02  F              PIC  X(001).
           02  S-TRAN_LNAME   PIC  X(013) VALUE "S-TRAN_HMD200".
           02  S-TRAN_KEY1    PIC  X(100) VALUE SPACE.
           02  S-TRAN_SORT    PIC  X(100) VALUE SPACE.
           02  S-TRAN_IDLST   PIC  X(100) VALUE SPACE.
           02  S-TRAN_RES     USAGE  POINTER.
       01  S-R.
           02  S-DNO          PIC  9(006).
           02  S-GNO          PIC  9(001).
           02  S-DATE         PIC  9(008).
           02  S-TCD          PIC  9(004).
           02  S-DT1          PIC  X(107).
           02  S-DT2   REDEFINES S-DT1.
             03  S-ZD1.
               04  S-HCD      PIC  9(006).
               04  S-SIZ      PIC  9(001).
               04  S-SUS.
                 05  S-SUSD  OCCURS  10.
                   06  S-SU   PIC S9(004)  COMP-3.
               04  S-SUT      PIC S9(005).
               04  S-BT       PIC S9(005).
             03  S-KIN        PIC S9(008).
             03  S-CSC        PIC  9(001).
             03  S-DC         PIC  9(001).
             03  S-FT         PIC  9(005).
             03  S-CCD        PIC  9(003).
             03  S-BC         PIC  9(006).
             03  S-SOK        PIC  9(001).
             03  S-TNC        PIC  9(002).
             03  S-FKC        PIC  9(002).
             03  S-HSC        PIC  9(001).
             03  S-KOSU       PIC  9(003).
             03  S-FRC        PIC  9(001).
             03  S-TCD2       PIC  9(004).
             03  S-BIK        PIC  X(010).
             03  S-SDT        PIC  9(008).
             03  S-BMC        PIC  9(002).
             03  S-BMNO       PIC  9(001).
             03  S-USC        PIC  9(001).
           02  S-DT3   REDEFINES S-DT1.
             03  S-BI         PIC  N(024).
             03  S-HA         PIC  9(006).
             03  F            PIC  X(030).
             03  S-TAX        PIC S9(007).
           02  S-DHC          PIC  9(001).
           02  S-SNC          PIC  9(001).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　履物　売上データ　変換　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-CHK   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-ME1   PIC  N(007) VALUE
                  "【ワークマン】".
             03  D-ME2   PIC  N(007) VALUE
                  "　【ナフコ】　".
           02  D-NGP.
             03  01D-NGP  PIC  9(004).
             03  FILLER   PIC  N(001) VALUE "年".
             03  03D-NGP  PIC Z9 .
             03  FILLER   PIC  N(001) VALUE "月".
             03  05D-NGP  PIC Z9 .
             03  FILLER   PIC  N(002) VALUE "日分".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME4   PIC  X(020) VALUE
                  "***  摘要エラー  ***".
             03  E-ME5   PIC  X(022) VALUE
                  "***  得意先エラー  ***".
             03  E-ME6.
               04  FILLER   PIC  X(016) VALUE
                    "***  HIM ﾅｼ  ***".
               04  02E-ME6  PIC  9(006).
             03  E-ME7.
               04  FILLER   PIC  X(022) VALUE
                    "***  振替単価無し  ***".
               04  02E-ME7  PIC  9(006).
             03  E-ME10  PIC  X(018) VALUE
                  "***  HSMSF ﾅｼ  ***".
             03  E-ME12  PIC  X(020) VALUE
                  "***  日付エラー ***".
             03  E-ME16  PIC  X(025) VALUE
                  "***  STRAN WRITE ｴﾗｰ  ***".
             03  E-ME17  PIC  X(027) VALUE
                  "***  STRAN REWRITE ｴﾗｰ  ***".
             03  E-ME19  PIC  X(028) VALUE
                  "***  未更新データ　有り  ***".
             03  E-ME20  PIC  X(041) VALUE
                  "伝票未発行データ有り   消す  OK=1 NO=5   ".
             03  E-ME21  PIC  X(017) VALUE
                  "***  ﾃﾞｰﾀ ﾅｼ  ***".
             03  E-ME22  PIC  X(027) VALUE
                  "***  HSMSF REWRITE ｴﾗｰ  ***".
             03  E-ME23  PIC  X(017) VALUE
                  "***  HKBM ﾅｼ  ***".
             03  E-ME24  PIC  X(026) VALUE
                  "***  HKBM REWRITE ｴﾗｰ  ***".
             03  E-KEY   PIC  X(007).
             03  E-SNO   PIC  9(006).
             03  E-TCD   PIC  9(004).
             03  E-HCD   PIC  9(006).
             03  E-NGP   PIC  9(008).
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "316" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "15" "42" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "15" "42" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "15" "42" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "15" "42" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "15" "42" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "15" "42" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "15" "42" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "20" "26" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "2" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "A-CHK" "9" "24" "55" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING
            "A-CHK" BY REFERENCE CHK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "43" "1" "A-CHK" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "44" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-DSP" " " "12" "0" "28" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING
            "D-ME1" "N" "12" "29" "14" " " "01C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING
            "D-ME2" "N" "12" "29" "14" "D-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "D-NGP" " " "15" "0" "16" "01C-DSP" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NGP" "9" "15" "28" "4" " " "D-NGP"  RETURNING RESU.
       CALL "SD_From" USING
            "01D-NGP" BY REFERENCE W-NEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NGP" "N" "15" "32" "2" "01D-NGP" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NGP" "Z9" "15" "34" "2" "02D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING
            "03D-NGP" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NGP" "N" "15" "36" "2" "03D-NGP" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "05D-NGP" "Z9" "15" "38" "2" "04D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING
            "05D-NGP" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "06D-NGP" "N" "15" "40" "4" "05D-NGP" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "402" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "402" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "15" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME4" "X" "24" "15" "20" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME5" "X" "24" "15" "22" "E-ME4" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME6" " " "24" "0" "22" "E-ME5" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME6" "X" "24" "15" "16" " " "E-ME6"  RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME6" "9" "24" "34" "6" "01E-ME6" " "  RETURNING RESU.
       CALL "SD_From" USING
            "02E-ME6" BY REFERENCE HSMS-09 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME7" " " "24" "0" "28" "E-ME6" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME7" "X" "24" "15" "22" " " "E-ME7"  RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME7" "9" "24" "40" "6" "01E-ME7" " "  RETURNING RESU.
       CALL "SD_From" USING
            "02E-ME7" BY REFERENCE HSMS-09 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME10" "X" "24" "15" "18" "E-ME7" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME12" "X" "24" "15" "20" "E-ME10" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME16" "X" "24" "15" "25" "E-ME12" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME17" "X" "24" "15" "27" "E-ME16" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME19" "X" "24" "15" "28" "E-ME17" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME20" "X" "24" "15" "41" "E-ME19" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME21" "X" "24" "15" "17" "E-ME20" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME22" "X" "24" "15" "27" "E-ME21" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME23" "X" "24" "15" "17" "E-ME22" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME24" "X" "24" "15" "26" "E-ME23" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-KEY" "X" "24" "40" "7" "E-ME24" " "  RETURNING RESU.
       CALL "SD_From" USING
            "E-KEY" BY REFERENCE HSMSW-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-SNO" "9" "24" "40" "6" "E-KEY" " "  RETURNING RESU.
       CALL "SD_From" USING
            "E-SNO" BY REFERENCE W-SNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-TCD" "9" "24" "40" "4" "E-SNO" " "  RETURNING RESU.
       CALL "SD_From" USING
            "E-TCD" BY REFERENCE HSMSW-061 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-HCD" "9" "24" "40" "6" "E-TCD" " "  RETURNING RESU.
       CALL "SD_From" USING
            "E-HCD" BY REFERENCE HSMSW-09 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-NGP" "9" "24" "40" "8" "E-HCD" " "  RETURNING RESU.
       CALL "SD_From" USING
            "E-NGP" BY REFERENCE W-NGP "8" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           MOVE ZERO TO W-NGP.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NGD TO W-NG.
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO HSMSW_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HSMSW_PNAME1 " " BY REFERENCE HSMSW_IDLST "0".
      *
      *           READ HSMSW AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HSMSW_PNAME1 BY REFERENCE HSMSW-R1 " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HSMSW_IDLST HSMSW_PNAME1
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  HSMSW-061 = 9850
               CALL "SD_Output" USING "D-ME1" D-ME1 "p" RETURNING RESU
           ELSE
               IF  HSMSW-061 = 5000
                   CALL "SD_Output" USING
                    "D-ME2" D-ME2 "p" RETURNING RESU
               END-IF
           END-IF
           MOVE HSMSW-061 TO W-TCD.
           MOVE HSMSW-05 TO W-NGP.
           CALL "DB_F_Close" USING
            BY REFERENCE HSMSW_IDLST HSMSW_PNAME1.
           CALL "SD_Output" USING "D-NGP" D-NGP "p" RETURNING RESU.
           IF  W-NGD NOT = W-NG
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
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
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" S-TRAN_PNAME1 " " BY REFERENCE S-TRAN_IDLST "0".
      *           READ S-TRAN AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" S-TRAN_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF.
       M-20.
           IF  S-DHC NOT = 0
               CALL "DB_F_Close" USING
                BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME19" E-ME19 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *           READ S-TRAN AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" S-TRAN_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME20" E-ME20 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-25
           END-IF
           GO TO M-20.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-CHK "A-CHK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  CHK = 1
               GO TO M-30
           END-IF
           IF  CHK = 5
               CALL "DB_F_Close" USING
                BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           GO TO M-25.
       M-30.
           CALL "DB_F_Close" USING
            BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME23" E-ME23 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE HKB-UNN TO W-DNO.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HSMSW_PNAME1 " " BY REFERENCE HSMSW_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" HSMSF_PNAME1 "SHARED" BY REFERENCE HSMSF_IDLST "1"
            "HSMS-KEY" BY REFERENCE HSMS-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" S-TRAN_PNAME1 " " BY REFERENCE S-TRAN_IDLST "0".
      *
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-35.
      *           READ HSMSW AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HSMSW_PNAME1 BY REFERENCE HSMSW-R1 " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  HSMSW-02 = 7
               GO TO M-35
           END-IF
           IF  HSMSW-061 NOT = W-TCD
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  HSMSW-05 NOT = W-NGP
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE ZERO TO W-SNO.
       M-40.
           IF  W-SNO NOT = ZERO
               IF  W-BCHK = 0
                   CALL "SD_Output" USING
                    "E-ME4" E-ME4 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-SNO" E-SNO "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   MOVE ZERO TO S-R
                   MOVE SPACE TO S-BI
                   MOVE W-DNO TO S-DNO
                   MOVE 9 TO S-GNO
                   MOVE W-NGP TO S-DATE
                   MOVE W-TCD TO S-TCD
                   MOVE W-SNO TO S-HA
      *                   WRITE S-R.
      *//////////////
                   CALL "DB_Insert" USING
                    S-TRAN_PNAME1 S-TRAN_LNAME S-R RETURNING RET
               END-IF
           END-IF
           ADD 1 TO W-DNO.
           IF  W-DNO = ZERO
               MOVE 1 TO W-DNO
           END-IF
           MOVE HSMSW-01 TO W-SNO.
           MOVE ZERO TO W-GNO W-BCHK.
       M-45.
           ADD 1 TO W-GNO.
           IF  W-GNO > 6
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SNO" E-SNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-85
           END-IF
           MOVE HSMSW-09 TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-85
           END-IF
      *
           MOVE ZERO TO S-R.
           MOVE SPACE TO S-BIK.
           MOVE W-DNO TO S-DNO.
           MOVE W-GNO TO S-GNO.
           MOVE W-NGP TO S-DATE.
           MOVE W-TCD TO S-TCD S-TCD2.
           MOVE HSMSW-09 TO S-HCD.
           MOVE HSMSW-10 TO S-SIZ.
           MOVE HSMSW-1211(01) TO S-SU(01).
           MOVE HSMSW-1211(02) TO S-SU(02).
           MOVE HSMSW-1211(03) TO S-SU(03).
           MOVE HSMSW-1211(04) TO S-SU(04).
           MOVE HSMSW-1211(05) TO S-SU(05).
           MOVE HSMSW-1211(06) TO S-SU(06).
           MOVE HSMSW-1211(07) TO S-SU(07).
           MOVE HSMSW-1211(08) TO S-SU(08).
           MOVE HSMSW-1211(09) TO S-SU(09).
           MOVE HSMSW-1211(10) TO S-SU(10).
           MOVE HSMSW-122 TO S-SUT.
           MOVE HSMSW-17 TO S-BT.
           COMPUTE S-KIN = S-SUT * S-BT.
           MOVE HSMSW-03 TO S-DC.
           IF  HSMSW-13 = 4 OR 5
               MOVE 4 TO S-DC
           END-IF
           IF  HSMSW-09 < 999900
               IF  HI-FT = ZERO
                   CALL "SD_Output" USING
                    "E-ME7" E-ME7 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-HCD" E-HCD "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               END-IF
           END-IF
           IF  S-DC NOT = 0 AND 3 AND 4 AND 5 AND 7
               IF  HSMSW-09 < 999900
                   IF  HI-FT = 1
                       CALL "SD_Output" USING
                        "E-ME7" E-ME7 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-HCD" E-HCD "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME99" E-ME99 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-CL" E-CL "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF
           IF  S-DC = 2
               MOVE ZERO TO S-FT
           ELSE
               MOVE HI-FT TO S-FT
           END-IF
           MOVE HSMSW-062 TO S-CCD.
           MOVE HI-BC TO S-BC.
           MOVE HSMSW-07 TO S-SOK.
           MOVE T-TNC TO S-TNC.
           MOVE T-FKC TO S-FKC.
           IF  W-NG < 1404
               MOVE 0 TO S-HSC
           ELSE
               MOVE 8 TO S-HSC
           END-IF
           IF  W-TCD NOT = 9850
               MOVE HSMSW-14 TO S-KOSU
           END-IF
           MOVE HSMSW-22 TO S-BIK.
           MOVE HSMSW-18 TO S-SDT.
           MOVE HI-BMC TO S-BMC.
           MOVE HI-BMNO TO S-BMNO.
           MOVE HSMSW-24 TO S-USC.
      *           WRITE S-R.
      *//////////////
           CALL "DB_Insert" USING
            S-TRAN_PNAME1 S-TRAN_LNAME S-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-50
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME16" E-ME16 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           PERFORM SD-RTN THRU SD-EX.
           GO TO M-90.
       M-50.
           IF  W-GNO = 1
               PERFORM HKB-RTN THRU HKB-EX
           END-IF.
       M-55.
      *           READ HSMSW AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HSMSW_PNAME1 BY REFERENCE HSMSW-R1 " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HSMSW-01 = W-SNO
               IF  HSMSW-02 NOT = 7
                   GO TO M-45
               ELSE
                   GO TO M-60
               END-IF
           END-IF
           IF  HSMSW-26 NOT = 1
               GO TO M-55
           END-IF
           IF  HSMSW-061 NOT = W-TCD
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  HSMSW-05 NOT = W-NGP
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  HSMSW-02 = 7
               GO TO M-55
           END-IF
           GO TO M-40.
       M-60.
           MOVE 1 TO W-BCHK.
           MOVE ZERO TO S-R.
           MOVE SPACE TO S-BI.
           MOVE W-DNO TO S-DNO.
           MOVE 9 TO S-GNO.
           MOVE W-NGP TO S-DATE.
           MOVE W-TCD TO S-TCD.
           MOVE HSMSW-15 TO S-BI.
           MOVE W-SNO TO S-HA.
      *           WRITE S-R.
      *//////////////
           CALL "DB_Insert" USING
            S-TRAN_PNAME1 S-TRAN_LNAME S-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME16" E-ME16 "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
      *
           MOVE SPACE TO HSMS-KEY.
           MOVE W-SNO TO HSMS-01.
      *           START HSMSF KEY NOT < HSMS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HSMSF_PNAME1 "HSMS-KEY" " NOT < " HSMS-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SNO" E-SNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-65.
      *           READ HSMSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSMSF_PNAME1 BY REFERENCE HSMS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           IF  W-SNO NOT = HSMS-01
               GO TO M-55
           END-IF
           ADD 1 TO HSMS-19.
           MOVE 1 TO HSMS-23.
      *           REWRITE HSMS-R1 INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HSMSF_PNAME1 HSMSF_LNAME HSMS-R1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME22" E-ME22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           GO TO M-65.
       M-85.
           PERFORM SD-RTN THRU SD-EX.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSMSF_IDLST HSMSF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSMSW_IDLST HSMSW_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *------------------------------------------------------------------------
       HKB-RTN.
           CALL "DB_F_Open" USING
            "I-O" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
      *           READ HKBM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME23" E-ME23 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO HKB-EX
           END-IF
           MOVE W-DNO TO HKB-UNN.
      *           REWRITE HKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME24" E-ME24 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO HKB-EX
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
       HKB-EX.
           EXIT.
       SD-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" S-TRAN_PNAME1 " " BY REFERENCE S-TRAN_IDLST "0".
       SD-020.
      *           READ S-TRAN AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" S-TRAN_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               GO TO SD-EX
           END-IF
           IF W-DNO NOT = S-DNO
               GO TO SD-020
           END-IF
           MOVE X"FF" TO S-R.
      *           REWRITE S-R.
      *///////////////
           CALL "DB_Update" USING
            S-TRAN_PNAME1 S-TRAN_LNAME S-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME17" E-ME17 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO SD-020.
       SD-EX.
           EXIT.

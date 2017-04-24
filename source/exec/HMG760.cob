       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG760.
      *********************************************************
      *    PROGRAM         :  履物在庫入庫日表                *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=作表 , 1=ＰＣ変換             *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0064".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0128".
           02  W-FID22        PIC  X(003).
       01  HEAD1.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "　＊＊＊　　履物在庫　入庫日明細表　　＊＊＊".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(006) VALUE "品　　　　名".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　在庫数".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単価".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "入庫年度".
       01  W-P.
           02  P-HCD1         PIC  9(004).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  P-ZSU          PIC --,---,--9.
           02  P-T            PIC ZZZ,ZZ9.
           02  P-ZKIN         PIC ----,---,--9.
           02  F              PIC  X(005).
           02  P-NEN          PIC  9(002).
       01  W-DATA.
           02  W-BC3          PIC  9(002).
           02  W-BMC          PIC  9(002).
           02  W-BC1          PIC  9(002).
           02  W-HCD.
             03  W-HCD1       PIC  9(004).
             03  F            PIC  9(002).
           02  W-PAGE         PIC  9(002).
           02  CNT            PIC  9(002).
           02  CHK            PIC  9(001).
           02  CHKD           PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-TC           PIC  9(002).
           02  W-MC           PIC  9(001).
           02  W-EC           PIC  9(002).
           02  W-SHCD         PIC  9(004).
           02  W-EHCD         PIC  9(004).
           02  W-SBC3         PIC  9(002).
           02  W-EBC3         PIC  9(002) VALUE 99.
           02  W-SBMNO        PIC  9(001).
           02  W-EBMNO        PIC  9(001) VALUE 9.
           02  W-SBCD1        PIC  9(003).
           02  W-EBCD1        PIC  9(003) VALUE 999.
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NEND  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-ZSU          PIC S9(007).
           02  W-ZKIN         PIC S9(009).
           02  WN-ZSU         PIC S9(007).
           02  WN-ZKIN        PIC S9(009).
           02  WT-ZSU         PIC S9(007).
           02  WT-ZKIN        PIC S9(009).
           02  WS-ZSU         PIC S9(007).
           02  WS-ZKIN        PIC S9(009).
           02  WA-ZSU         PIC S9(007).
           02  WA-ZKIN        PIC S9(009).
      *
           02  W-NAME         PIC  N(024).
           02  W-ANM   REDEFINES W-NAME.
             03  W-NM    OCCURS  24  PIC  N(001).
           02  W-NAMED        PIC  N(024).
           02  W-ANMD  REDEFINES W-NAMED.
             03  W-NMD   OCCURS  24  PIC  N(001).
      *
           02  W-TM           PIC  N(012).
           02  W-TMDD  REDEFINES W-TM.
             03  W-TMD   OCCURS  12  PIC  N(001).
           02  W-MN           PIC  N(008).
           02  W-MNDD  REDEFINES W-MN.
             03  W-MND   OCCURS   8  PIC  N(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LIHKBM.
           COPY LSPF.
      *FD  HZN-F
       01  HZN-F_HMG760.
           02  HZN-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HZN-F_LNAME    PIC  X(012) VALUE "HZN-F_HMG760".
           02  F              PIC  X(001).
           02  HZN-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HZN-F_SORT     PIC  X(100) VALUE SPACE.
           02  HZN-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HZN-F_RES      USAGE  POINTER.
       01  HZN-R.
           02  HZN-HCD.
             03  HZN-HCD1     PIC  9(004).
             03  HZN-HCD2     PIC  9(002).
           02  HZN-ZSU        PIC S9(006).
           02  HZN-ZKIN       PIC S9(009).
           02  HZN-NG         PIC  9(006).
           02  HZN-BCD1.
             03  HZN-BC1      PIC  9(002).
             03  HZN-BC21     PIC  9(001).
           02  HZN-BC22       PIC  9(001).
           02  HZN-BC3        PIC  9(002).
           02  HZN-BMC        PIC  9(002).
           02  HZN-BMNO       PIC  9(001).
           02  FILLER         PIC  X(028).
       77  F                  PIC  X(001).
      *FD  HZNPC-F
       01  HZNPC-F_HMG760.
           02  HZNPC-F_PNAME1 PIC  X(006) VALUE "HZNPCF".
           02  F              PIC  X(001).
           02  HZNPC-F_LNAME  PIC  X(014) VALUE "HZNPC-F_HMG760".
           02  F              PIC  X(001).
           02  HZNPC-F_KEY1   PIC  X(100) VALUE SPACE.
           02  HZNPC-F_SORT   PIC  X(100) VALUE SPACE.
           02  HZNPC-F_IDLST  PIC  X(100) VALUE SPACE.
           02  HZNPC-F_RES    USAGE  POINTER.
       01  HZNPC-R.
           02  HZNPC-HCD1     PIC  X(004).
           02  HZNPC-HNA      PIC  N(024).
           02  HZNPC-ZSU      PIC S9(007).
           02  HZNPC-T        PIC  9(005).
           02  HZNPC-ZKIN     PIC S9(009).
           02  HZNPC-NEN      PIC  X(002).
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
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　履物在庫　入庫日明細表　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(049) VALUE
                "分類③  00 ～ 99    カジュ=10,ワーク=20,教　育=30".
           02  FILLER.
             03  FILLER  PIC  X(029) VALUE
                  "部門№   0 ～ 9     国  内=1,".
             03  FILLER  PIC  N(003) VALUE "上　海".
             03  FILLER  PIC  X(011) VALUE "=2,仕  入=3".
           02  FILLER.
             03  FILLER  PIC  N(003) VALUE "ワーク".
             03  FILLER  PIC  X(011) VALUE
                  "=4,教　育=5".
           02  FILLER  PIC  X(017) VALUE
                "分類① 000 ～ 999".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SBC3  PIC  9(002).
             03  A-EBC3  PIC  9(002).
           02  FILLER.
             03  A-SBMNO PIC  9(001).
             03  A-EBMNO PIC  9(001).
           02  FILLER.
             03  A-SBC1  PIC  9(003).
             03  A-EBC1  PIC  9(003).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER  PIC  N(010) VALUE
                "ＰＣ用ファイルへ変換".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
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
           "C-MID" " " "0" "0" "445" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "12" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "12" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "12" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "12" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "12" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "12" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "12" "42" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "15" "19" "49" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" " " "17" "0" "46" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "10C-MID" "X" "17" "19" "29" " " "09C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "11C-MID" "N" "17" "48" "6" "10C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "12C-MID" "X" "17" "54" "11" "11C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "13C-MID" " " "18" "0" "17" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "14C-MID" "N" "18" "39" "6" " " "13C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "15C-MID" "X" "18" "45" "11" "14C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "16C-MID" "X" "19" "19" "17" "13C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "17C-MID" "X" "22" "24" "22" "16C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "15" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-SBC3" "9" "15" "27" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SBC3" BY REFERENCE W-SBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EBC3" "9" "15" "33" "2" "A-SBC3" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EBC3" BY REFERENCE W-EBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-ACP" " " "17" "0" "2" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SBMNO" "9" "17" "28" "1" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SBMNO" BY REFERENCE W-SBMNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EBMNO" "9" "17" "33" "1" "A-SBMNO" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EBMNO" BY REFERENCE W-EBMNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03C-ACP" " " "19" "0" "6" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SBC1" "9" "19" "26" "3" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SBC1" BY REFERENCE W-SBCD1 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EBC1" "9" "19" "33" "3" "A-SBC1" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EBC1" BY REFERENCE W-EBCD1 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "22" "41" "1" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "20" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-DSP" "N" "7" "22" "20" " " "C-DSP" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "29" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "29" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-980
           END-IF
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "C-DSP" C-DSP "p" RETURNING RESU
           END-IF.
       M-100.
           CALL "SD_Accept" USING BY REFERENCE A-SBC3 "A-SBC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-100
           END-IF.
       M-120.
           CALL "SD_Accept" USING BY REFERENCE A-EBC3 "A-EBC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-100
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-120
           END-IF
           IF  W-SBC3 > W-EBC3
               GO TO M-120
           END-IF.
       M-140.
           CALL "SD_Accept" USING BY REFERENCE A-SBMNO "A-SBMNO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-140
           END-IF.
       M-160.
           CALL "SD_Accept" USING BY REFERENCE A-EBMNO "A-EBMNO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-140
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-160
           END-IF
           IF  W-SBMNO > W-EBMNO
               GO TO M-160
           END-IF.
       M-180.
           CALL "SD_Accept" USING BY REFERENCE A-SBC1 "A-SBC1" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-160
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-180
           END-IF.
       M-200.
           CALL "SD_Accept" USING BY REFERENCE A-EBC1 "A-EBC1" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-200
           END-IF
           IF  W-SBCD1 > W-EBCD1
               GO TO M-200
           END-IF.
       M-220.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-200
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-220
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-220
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12.
           MOVE W-FID1 TO WK0064ID.
           MOVE WK0064ID TO HZN-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HZN-F_PNAME1 " " BY REFERENCE HZN-F_IDLST "0".
       M-300.
      *           READ HZN-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HZN-F_PNAME1 BY REFERENCE HZN-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE HZN-F_IDLST HZN-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  HZN-BC3 < W-SBC3 OR > W-EBC3
               GO TO M-300
           END-IF
           IF  HZN-BMNO < W-SBMNO OR > W-EBMNO
               GO TO M-300
           END-IF
           IF  HZN-BCD1 < W-SBCD1 OR > W-EBCD1
               GO TO M-300
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           IF JS-SIGN = 1
               MOVE STN-NO2 TO W-FID22
               MOVE W-FID2 TO WK0128ID
               MOVE WK0128ID TO HZNPC-F_PNAME1
               CALL "DB_F_Open" USING
                "OUTPUT" HZNPC-F_PNAME1 " " BY REFERENCE
                HZNPC-F_IDLST "0"
           ELSE
               CALL "PR_Open" RETURNING RESP
               MOVE DATE-02R TO H-DATE
               MOVE ZERO TO W-PAGE
               PERFORM MID-020 THRU MID-EX
           END-IF
           MOVE ZERO TO WA-ZSU WA-ZKIN.
       M-320.
           MOVE ZERO TO WS-ZSU WS-ZKIN.
           MOVE HZN-BC3 TO W-BC3.
       M-340.
           MOVE ZERO TO WT-ZSU WT-ZKIN.
           MOVE HZN-BMC TO W-BMC.
       M-360.
           MOVE ZERO TO WN-ZSU WN-ZKIN.
           MOVE HZN-BC1 TO W-BC1.
       M-400.
           MOVE HZN-HCD1 TO W-HCD1.
           MOVE ZERO TO W-ZSU W-ZKIN W-NG.
           PERFORM HMS-RTN THRU HMS-EX.
       M-420.
           ADD HZN-ZSU TO W-ZSU.
           ADD HZN-ZKIN TO W-ZKIN.
           IF HZN-NG > W-NG
               MOVE HZN-NG TO W-NG
           END-IF.
       M-440.
      *           READ HZN-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HZN-F_PNAME1 BY REFERENCE HZN-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-920
           END-IF
           IF  HZN-BC3 < W-SBC3 OR > W-EBC3
               GO TO M-440
           END-IF
           IF  HZN-BMNO < W-SBMNO OR > W-EBMNO
               GO TO M-440
           END-IF
           IF  HZN-BCD1 < W-SBCD1 OR > W-EBCD1
               GO TO M-440
           END-IF
           IF  HZN-BC3 NOT = W-BC3
               GO TO M-580
           END-IF
           IF  HZN-BMC NOT = W-BMC
               GO TO M-540
           END-IF
           IF  HZN-BC1 NOT = W-BC1
               GO TO M-500
           END-IF
           IF  HZN-HCD1 = W-HCD1
               GO TO M-420
           END-IF
           PERFORM MEI-RTN THRU MEI-EX.
           GO TO M-400.
       M-500.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM KEI1-RTN THRU KEI1-EX.
           GO TO M-360.
       M-540.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM KEI1-RTN THRU KEI1-EX.
           PERFORM KEI2-RTN THRU KEI2-EX.
           GO TO M-340.
       M-580.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM KEI1-RTN THRU KEI1-EX.
           PERFORM KEI2-RTN THRU KEI2-EX.
           PERFORM KEI3-RTN THRU KEI3-EX.
           GO TO M-320.
       M-920.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM KEI1-RTN THRU KEI1-EX.
           PERFORM KEI2-RTN THRU KEI2-EX.
           PERFORM KEI3-RTN THRU KEI3-EX.
           PERFORM KEI4-RTN THRU KEI4-EX.
           CALL "DB_F_Close" USING
            BY REFERENCE HZN-F_IDLST HZN-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           IF  JS-SIGN = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HZNPC-F_IDLST HZNPC-F_PNAME1
           ELSE
               CALL "PR_Close" RETURNING RESP
           END-IF.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       HMS-RTN.
           MOVE SPACE TO W-NAME W-NAMED.
           MOVE ZERO TO HI-KEY.
           MOVE W-HCD1 TO HI-HCD1.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　　　　" TO W-NAME
               GO TO HMS-EX
           END-IF
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　　　　" TO W-NAME
               GO TO HMS-EX
           END-IF
           IF  W-HCD1 NOT = HI-HCD1
               MOVE "　＊＊　マスター　なし　＊＊　　　　" TO W-NAME
               GO TO HMS-EX
           END-IF
      *
           MOVE HI-NAME TO W-NAMED.
           MOVE SPACE TO W-NAME.
           MOVE ZERO TO CNT.
       HMS-020.
           ADD 1 TO CNT.
           IF  CNT = 24
               GO TO HMS-EX
           END-IF
           MOVE W-NMD(CNT) TO W-NM(CNT).
           IF  W-NMD(CNT) NOT = SPACE
               GO TO HMS-020
           END-IF
           ADD 1 TO CNT.
           IF  CNT = 24
               GO TO HMS-EX
           END-IF
           MOVE W-NMD(CNT) TO W-NM(CNT).
           IF  W-NMD(CNT) NOT = SPACE
               GO TO HMS-020
           END-IF.
       HMS-EX.
           EXIT.
       MEI-RTN.
           IF  JS-SIGN = 1
               GO TO MEI-110
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-HNA.
           MOVE W-HCD1 TO P-HCD1.
           MOVE W-NAME TO P-HNA.
           MOVE W-ZSU TO P-ZSU.
           MOVE HI-FT TO P-T.
           MOVE W-ZKIN TO P-ZKIN.
           IF  W-GET > 0 AND < 5
               SUBTRACT 1 FROM W-NEN
           END-IF
           IF  W-NG NOT = ZERO
               MOVE W-NEN2 TO P-NEN
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO MEI-210.
       MEI-110.
           INITIALIZE HZNPC-R.
           MOVE W-HCD1 TO HZNPC-HCD1.
           MOVE W-NAME TO HZNPC-HNA.
           MOVE W-ZSU TO HZNPC-ZSU.
           MOVE HI-FT TO HZNPC-T.
           MOVE W-ZKIN TO HZNPC-ZKIN.
           IF  W-GET > 0 AND < 5
               SUBTRACT 1 FROM W-NEN
           END-IF
           IF  W-NG NOT = ZERO
               MOVE W-NEN2 TO HZNPC-NEN
           END-IF
      *           WRITE HZNPC-R.
      *//////////////
           CALL "DB_Insert" USING
            HZNPC-F_PNAME1 HZNPC-F_LNAME HZNPC-R RETURNING RET.
       MEI-210.
           ADD W-ZSU TO WN-ZSU.
           ADD W-ZKIN TO WN-ZKIN.
       MEI-EX.
           EXIT.
       KEI1-RTN.
           MOVE SPACE TO HKB-KEY.
           MOVE "11" TO HKB-NO.
           MOVE W-BC1 TO HKB-BR1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BRN1
           END-IF
           MOVE SPACE TO W-TM W-MN.
           MOVE HKB-BRN1 TO W-MN.
           MOVE 9 TO W-EC.
       KEI1-010.
           SUBTRACT 1 FROM W-EC.
           IF  W-EC NOT = 0
               IF  W-MND(W-EC) = SPACE
                   GO TO KEI1-010
               END-IF
           END-IF
      *
           ADD 1 TO W-EC.
           MOVE "（" TO W-TMD(4).
           MOVE 4 TO W-TC.
           MOVE 0 TO W-MC.
       KEI1-020.
           ADD 1 TO W-TC W-MC.
           IF  W-MC NOT = W-EC
               MOVE W-MND(W-MC) TO W-TMD(W-TC)
               GO TO KEI1-020
           END-IF
           MOVE "）" TO W-TMD(W-TC).
           IF  JS-SIGN = 1
               GO TO KEI1-110
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-HNA.
           MOVE W-TM TO P-HNA.
           MOVE WN-ZSU TO P-ZSU.
           MOVE WN-ZKIN TO P-ZKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO KEI1-210.
       KEI1-110.
           INITIALIZE HZNPC-R.
           MOVE W-TM TO HZNPC-HNA.
           MOVE WN-ZSU TO HZNPC-ZSU.
           MOVE WN-ZKIN TO HZNPC-ZKIN.
      *           WRITE HZNPC-R.
      *//////////////
           CALL "DB_Insert" USING
            HZNPC-F_PNAME1 HZNPC-F_LNAME HZNPC-R RETURNING RET.
       KEI1-210.
           ADD WN-ZSU TO WT-ZSU.
           ADD WN-ZKIN TO WT-ZKIN.
       KEI1-EX.
           EXIT.
       KEI2-RTN.
           MOVE SPACE TO HKB-KEY.
           MOVE "16" TO HKB-NO.
           MOVE W-BMC TO HKB-BMC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BMN
           END-IF
           MOVE SPACE TO W-TM W-MN.
           MOVE HKB-BMN TO W-TM.
           MOVE ZERO TO W-MC W-EC.
       KEI2-010.
           ADD 1 TO W-MC.
           IF  W-MC = 5
               GO TO KEI2-020
           END-IF
           IF  W-TMD(W-MC) NOT = SPACE
               ADD 1 TO W-EC
               MOVE W-TMD(W-MC) TO W-MND(W-EC)
           END-IF
           GO TO KEI2-010.
       KEI2-020.
           ADD 1 TO W-EC.
           MOVE "計" TO W-MND(W-EC).
           ADD 2 TO W-EC.
           MOVE SPACE TO W-TM.
           MOVE "＜" TO W-TMD(3).
           MOVE 4 TO W-TC.
           MOVE 0 TO W-MC.
       KEI2-030.
           ADD 1 TO W-TC W-MC.
           IF  W-MC NOT = W-EC
               MOVE W-MND(W-MC) TO W-TMD(W-TC)
               GO TO KEI2-030
           END-IF
           MOVE "＞" TO W-TMD(W-TC).
           IF  JS-SIGN = 1
               GO TO KEI2-110
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-HNA.
           MOVE W-TM TO P-HNA.
           MOVE WT-ZSU TO P-ZSU.
           MOVE WT-ZKIN TO P-ZKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO KEI2-210.
       KEI2-110.
           INITIALIZE HZNPC-R.
           MOVE W-TM TO HZNPC-HNA.
           MOVE WT-ZSU TO HZNPC-ZSU.
           MOVE WT-ZKIN TO HZNPC-ZKIN.
      *           WRITE HZNPC-R.
      *//////////////
           CALL "DB_Insert" USING
            HZNPC-F_PNAME1 HZNPC-F_LNAME HZNPC-R RETURNING RET.
       KEI2-210.
           ADD WT-ZSU TO WS-ZSU.
           ADD WT-ZKIN TO WS-ZKIN.
       KEI2-EX.
           EXIT.
       KEI3-RTN.
           MOVE SPACE TO HKB-KEY.
           MOVE "14" TO HKB-NO.
           MOVE W-BC3 TO HKB-BR3.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BRN3
           END-IF
           MOVE SPACE TO W-TM W-MN.
           MOVE HKB-BRN3 TO W-TM.
           MOVE ZERO TO W-MC W-EC.
       KEI3-010.
           ADD 1 TO W-MC.
           IF  W-MC = 5
               GO TO KEI3-020
           END-IF
           IF  W-TMD(W-MC) NOT = SPACE
               ADD 1 TO W-EC
               MOVE W-TMD(W-MC) TO W-MND(W-EC)
           END-IF
           GO TO KEI3-010.
       KEI3-020.
           ADD 1 TO W-EC.
           MOVE "合" TO W-MND(W-EC).
           ADD 1 TO W-EC.
           MOVE "計" TO W-MND(W-EC).
           ADD 2 TO W-EC.
           MOVE SPACE TO W-TM.
           MOVE "［" TO W-TMD(2).
           MOVE 3 TO W-TC.
           MOVE 0 TO W-MC.
       KEI3-030.
           ADD 1 TO W-TC W-MC.
           IF  W-MC NOT = W-EC
               MOVE W-MND(W-MC) TO W-TMD(W-TC)
               GO TO KEI3-030
           END-IF
           MOVE "］" TO W-TMD(W-TC).
           IF  JS-SIGN = 1
               GO TO KEI3-110
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-HNA.
           MOVE W-TM TO P-HNA.
           MOVE WS-ZKIN TO P-ZKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO KEI3-210.
       KEI3-110.
           INITIALIZE HZNPC-R.
           MOVE W-TM TO HZNPC-HNA.
           MOVE WS-ZKIN TO HZNPC-ZKIN.
      *           WRITE HZNPC-R.
      *//////////////
           CALL "DB_Insert" USING
            HZNPC-F_PNAME1 HZNPC-F_LNAME HZNPC-R RETURNING RET.
       KEI3-210.
           ADD WS-ZSU TO WA-ZSU.
           ADD WS-ZKIN TO WA-ZKIN.
       KEI3-EX.
           EXIT.
       KEI4-RTN.
           IF  JS-SIGN = 1
               GO TO KEI4-110
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-HNA.
           MOVE "【　総　合　計　】　　　" TO P-HNA.
           MOVE WA-ZSU TO P-ZSU.
           MOVE WA-ZKIN TO P-ZKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO KEI4-EX.
       KEI4-110.
           INITIALIZE HZNPC-R.
           MOVE "【　総　合　計　】　　　" TO HZNPC-HNA.
           MOVE WA-ZSU TO HZNPC-ZSU.
           MOVE WA-ZKIN TO HZNPC-ZKIN.
      *           WRITE HZNPC-R.
      *//////////////
           CALL "DB_Insert" USING
            HZNPC-F_PNAME1 HZNPC-F_LNAME HZNPC-R RETURNING RET.
       KEI4-EX.
           EXIT.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-020.
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
       MID-EX.
           EXIT.

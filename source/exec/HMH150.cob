       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMH150.
      *********************************************************
      *    PROGRAM         :  履物品種別在庫廃棄明細表        *
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
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(032) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "＊＊＊　　履物製品在庫・廃棄明細表　　＊＊＊".
           02  F              PIC  X(021) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(045) VALUE SPACE.
           02  F              PIC  X(008) VALUE "I-----  ".
           02  F              PIC  N(003) VALUE "在　庫".
           02  F              PIC  X(019) VALUE "  ------I  I-----  ".
           02  F              PIC  N(003) VALUE "廃　棄".
           02  F              PIC  X(019) VALUE "  ------I  I-----  ".
           02  F              PIC  N(003) VALUE "残　高".
           02  F              PIC  X(009) VALUE "  ------I".
       01  HEAD3.
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(005) VALUE "品　　　名".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
       01  W-P.
           02  P-15K          PIC  X(005).                              1.5ﾊﾞｲﾄ
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(024).
           02  P-CSU          PIC ---,---,--9.
           02  P-CKIN         PIC --,---,---,--9.
           02  P-JSU          PIC ---,---,--9.
           02  P-JKIN         PIC --,---,---,--9.
           02  P-KSU          PIC ---,---,--9.
           02  P-KKIN         PIC --,---,---,--9.
       01  W-DATA.
           02  W-BC3          PIC  9(002).
           02  W-BMC          PIC  9(002).
           02  W-BC1          PIC  9(002).
           02  W-HCD.
             03  W-HCD1       PIC  9(004).
             03  W-HCD2       PIC  9(002).
           02  W-TC           PIC  9(001).
           02  W-D.
             03  W-FT         PIC  9(005).
             03  W-CSU        PIC S9(006).
             03  W-CKIN       PIC S9(009).
             03  W-JSU        PIC S9(006).
             03  W-JKIN       PIC S9(009).
             03  W-KSU        PIC S9(007).
             03  W-KKIN       PIC S9(009).
           02  CNT            PIC  9(002).
           02  W-EC           PIC  9(002).
           02  W-MC           PIC  9(001).
           02  W-MN           PIC  N(008).
           02  W-MNDD  REDEFINES W-MN.
             03  W-MND   OCCURS   8  PIC  N(001).
           02  W-NAME         PIC  N(024).
           02  W-NAMED REDEFINES W-NAME.
             03  W-NA         PIC  N(001)  OCCURS 24.
           02  W-SEBC.
             03  W-SBC3       PIC  9(002).
             03  W-EBC3       PIC  9(002) VALUE 99.
             03  W-SBMNO      PIC  9(001).
             03  W-EBMNO      PIC  9(001) VALUE 9. 
             03  W-SBC1       PIC  9(002).
             03  W-EBC1       PIC  9(002) VALUE 99.
           02  W-DMM          PIC  9(001).
       01  W-ND.
           02  WN-CSU         PIC S9(007).
           02  WN-CKIN        PIC S9(010).
           02  WN-JSU         PIC S9(007).
           02  WN-JKIN        PIC S9(010).
           02  WN-KSU         PIC S9(007).
           02  WN-KKIN        PIC S9(010).
       01  W-TD.
           02  WT-CSU         PIC S9(007).
           02  WT-CKIN        PIC S9(010).
           02  WT-JSU         PIC S9(007).
           02  WT-JKIN        PIC S9(010).
           02  WT-KSU         PIC S9(007).
           02  WT-KKIN        PIC S9(010).
       01  W-GD.
           02  WG-CSU         PIC S9(007).
           02  WG-CKIN        PIC S9(010).
           02  WG-JSU         PIC S9(007).
           02  WG-JKIN        PIC S9(010).
           02  WG-KSU         PIC S9(007).
           02  WG-KKIN        PIC S9(010).
       01  W-SD.
           02  WS-CSU         PIC S9(007).
           02  WS-CKIN        PIC S9(010).
           02  WS-JSU         PIC S9(007).
           02  WS-JKIN        PIC S9(010).
           02  WS-KSU         PIC S9(007).
           02  WS-KKIN        PIC S9(010).
       01  W-AD.
           02  WA-CSU         PIC S9(007).
           02  WA-CKIN        PIC S9(010).
           02  WA-JSU         PIC S9(007).
           02  WA-JKIN        PIC S9(010).
           02  WA-KSU         PIC S9(007).
           02  WA-KKIN        PIC S9(010).
       01  W-PAGE             PIC  9(002) VALUE ZERO.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LIHKBM.
           COPY LSPF.
      *FD  TSW-F
       01  TSW-F_HMH150.
           02  TSW-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TSW-F_LNAME    PIC  X(012) VALUE "TSW-F_HMH150".
           02  F              PIC  X(001).
           02  TSW-F_KEY1     PIC  X(100) VALUE SPACE.
           02  TSW-F_SORT     PIC  X(100) VALUE SPACE.
           02  TSW-F_IDLST    PIC  X(100) VALUE SPACE.
           02  TSW-F_RES      USAGE  POINTER.
       01  TSW-R.
           02  TSW-HCD.
             03  TSW-HCD1     PIC  9(004).
             03  TSW-HCD2     PIC  9(002).
           02  TSW-CSU        PIC S9(006).
           02  TSW-JSU        PIC S9(006).
           02  TSW-FT         PIC  9(005).
           02  TSW-BC1        PIC  9(002).
           02  TSW-BC2.
             03  TSW-BC21     PIC  9(001).
             03  TSW-BC22     PIC  9(001).
           02  TSW-BC3        PIC  9(002).
           02  TSW-BMC        PIC  9(002).
           02  TSW-BMNO       PIC  9(001).
           02  F              PIC  X(096).
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　履物製品　在庫廃棄明細表　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(049) VALUE
                "分類3  00 ～ 99   (一　般=10,ワーク=20,教　育=30)".
           02  FILLER  PIC  X(049) VALUE
                "部門    0 ～ 9    (国内=1,上海=2,ワーク=3,教育=4)".
           02  FILLER  PIC  X(015) VALUE
                "分類1  00 ～ 99".
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
             03  A-SBC1  PIC  9(002).
             03  A-EBC1  PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-KEY   PIC  9(006).
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
           "C-MID" " " "0" "0" "443" " " " " RETURNING RESU.
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
           "08C-MID" "X" "14" "19" "49" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "15" "19" "49" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "10C-MID" "X" "16" "19" "15" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "11C-MID" "X" "20" "23" "22" "10C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "11" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "14" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SBC3" "9" "14" "26" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SBC3" BY REFERENCE W-SBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EBC3" "9" "14" "32" "2" "A-SBC3" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EBC3" BY REFERENCE W-EBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "15" "0" "2" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SBMNO" "9" "15" "27" "1" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SBMNO" BY REFERENCE W-SBMNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EBMNO" "9" "15" "32" "1" "A-SBMNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EBMNO" BY REFERENCE W-EBMNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "16" "0" "4" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SBC1" "9" "16" "26" "2" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SBC1" BY REFERENCE W-SBC1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EBC1" "9" "16" "32" "2" "A-SBC1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EBC1" BY REFERENCE W-EBC1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "40" "1" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "84" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "84" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "16" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "9" "24" "45" "6" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE TSW-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" "E-KEY" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           PERFORM ACP-RTN THRU ACP-EX.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO TSW-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TSW-F_PNAME1 " " BY REFERENCE TSW-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO W-AD.
       M-10.
      *           READ TSW-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TSW-F_PNAME1 BY REFERENCE TSW-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  TSW-BC3 < W-SBC3 OR > W-EBC3
               GO TO M-10
           END-IF
           IF  TSW-BMNO < W-SBMNO OR > W-EBMNO
               GO TO M-10
           END-IF
           IF  TSW-BC1 < W-SBC1 OR > W-EBC1
               GO TO M-10
           END-IF
           IF  ZERO = TSW-CSU AND TSW-JSU
               GO TO M-10
           END-IF
           PERFORM MID-020 THRU MID-EX.
       M-15.
           MOVE TSW-BC3 TO W-BC3.
           MOVE ZERO TO W-SD.
       M-20.
           MOVE TSW-BMC TO W-BMC.
           MOVE ZERO TO W-GD.
       M-25.
           MOVE TSW-BC1 TO W-BC1.
           MOVE ZERO TO W-TD.
       M-30.
           MOVE TSW-HCD1 TO W-HCD1.
           MOVE ZERO TO W-ND W-TC.
       M-35.
           MOVE ZERO TO W-D.
           MOVE TSW-HCD TO W-HCD.
           MOVE TSW-FT TO W-FT.
       M-40.
           ADD TSW-CSU TO W-CSU.
           ADD TSW-JSU TO W-JSU.
       M-45.
      *           READ TSW-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TSW-F_PNAME1 BY REFERENCE TSW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  TSW-BC3 < W-SBC3 OR > W-EBC3
               GO TO M-45
           END-IF
           IF  TSW-BMNO < W-SBMNO OR > W-EBMNO
               GO TO M-45
           END-IF
           IF  TSW-BC1 < W-SBC1 OR > W-EBC1
               GO TO M-45
           END-IF
           IF  ZERO = TSW-CSU AND TSW-JSU
               GO TO M-45
           END-IF
           IF  TSW-BC3 NOT = W-BC3
               GO TO M-65
           END-IF
           IF  TSW-BMC NOT = W-BMC
               GO TO M-60
           END-IF
           IF  TSW-BC1 NOT = W-BC1
               GO TO M-55
           END-IF
           IF  TSW-HCD1 NOT = W-HCD1
               GO TO M-50
           END-IF
      *
           IF (TSW-HCD = W-HCD) AND (TSW-FT = W-FT)
               GO TO M-40
           END-IF
           IF (W-CSU NOT = ZERO) OR (W-JSU NOT = ZERO)
               PERFORM MEI-RTN THRU MEI-EX
           END-IF
           GO TO M-35.
       M-50.
           IF (W-CSU NOT = ZERO) OR (W-JSU NOT = ZERO)
               PERFORM MEI-RTN THRU MEI-EX
           END-IF
           PERFORM KEI1-RTN THRU KEI1-EX.
           GO TO M-30.
       M-55.
           IF (W-CSU NOT = ZERO) OR (W-JSU NOT = ZERO)
               PERFORM MEI-RTN THRU MEI-EX
           END-IF
           PERFORM KEI1-RTN THRU KEI1-EX.
           PERFORM KEI2-RTN THRU KEI2-EX.
           GO TO M-25.
       M-60.
           IF (W-CSU NOT = ZERO) OR (W-JSU NOT = ZERO)
               PERFORM MEI-RTN THRU MEI-EX
           END-IF
           PERFORM KEI1-RTN THRU KEI1-EX.
           PERFORM KEI2-RTN THRU KEI2-EX.
           PERFORM KEI3-RTN THRU KEI3-EX.
           GO TO M-20.
       M-65.
           IF (W-CSU NOT = ZERO) OR (W-JSU NOT = ZERO)
               PERFORM MEI-RTN THRU MEI-EX
           END-IF
           PERFORM KEI1-RTN THRU KEI1-EX.
           PERFORM KEI2-RTN THRU KEI2-EX.
           PERFORM KEI3-RTN THRU KEI3-EX.
           PERFORM KEI4-RTN THRU KEI4-EX.
           GO TO M-15.
       M-85.
           IF (W-CSU NOT = ZERO) OR (W-JSU NOT = ZERO)
               PERFORM MEI-RTN THRU MEI-EX
           END-IF
           PERFORM KEI1-RTN THRU KEI1-EX.
           PERFORM KEI2-RTN THRU KEI2-EX.
           PERFORM KEI3-RTN THRU KEI3-EX.
           PERFORM KEI4-RTN THRU KEI4-EX.
      *
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R W-P.
           MOVE ALL "　" TO P-NAME.
           MOVE W-15K TO P-15K.
           MOVE "　　　【　　総　合　計　　】　" TO P-NAME.
           MOVE WA-CSU TO P-CSU.
           MOVE WA-CKIN TO P-CKIN.
           MOVE WA-JSU TO P-JSU.
           MOVE WA-JKIN TO P-JKIN.
           MOVE WA-KSU TO P-KSU.
           MOVE WA-KKIN TO P-KKIN.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TSW-F_IDLST TSW-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-SBC3 "A-SBC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-RTN
           END-IF.
       ACP-010.
           CALL "SD_Accept" USING BY REFERENCE A-EBC3 "A-EBC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-010
           END-IF
           IF  W-SBC3 > W-EBC3
               GO TO ACP-010
           END-IF.
       ACP-020.
           CALL "SD_Accept" USING BY REFERENCE A-SBMNO "A-SBMNO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-010
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-RTN
           END-IF.
       ACP-030.
           CALL "SD_Accept" USING BY REFERENCE A-EBMNO "A-EBMNO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-030
           END-IF
           IF  W-SBMNO > W-EBMNO
               GO TO ACP-030
           END-IF.
       ACP-040.
           CALL "SD_Accept" USING BY REFERENCE A-SBC1 "A-SBC1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-030
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-040
           END-IF.
       ACP-050.
           CALL "SD_Accept" USING BY REFERENCE A-EBC1 "A-EBC1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-050
           END-IF
           IF  W-SBC1 > W-EBC1
               GO TO ACP-050
           END-IF.
       ACP-060.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO ACP-EX
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-050
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-060
           END-IF
           IF  W-DMM = 9
               GO TO ACP-RTN
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-060
           END-IF.
       ACP-EX.
           EXIT.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-020.
           MOVE SPACE TO SP-R.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
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
       MID-EX.
           EXIT.
       MEI-RTN.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　　＊＊　" TO HI-NAME
           END-IF
           COMPUTE W-CKIN = W-CSU * W-FT.
           COMPUTE W-JKIN = W-JSU * W-FT.
           COMPUTE W-KSU = W-CSU - W-JSU.
           COMPUTE W-KKIN = W-CKIN - W-JKIN.
      *
           MOVE SPACE TO W-P.
           MOVE ALL "　" TO P-NAME.
           MOVE W-15K TO P-15K.
           MOVE W-HCD TO P-HCD.
           MOVE HI-NAME TO P-NAME.
           MOVE W-CSU TO P-CSU.
           MOVE W-CKIN TO P-CKIN.
           MOVE W-JSU TO P-JSU.
           MOVE W-JKIN TO P-JKIN.
           MOVE W-KSU TO P-KSU.
           MOVE W-KKIN TO P-KKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-CSU TO WN-CSU.
           ADD W-CKIN TO WN-CKIN.
           ADD W-JSU TO WN-JSU.
           ADD W-JKIN TO WN-JKIN.
           ADD W-KSU TO WN-KSU.
           ADD W-KKIN TO WN-KKIN.
           IF  W-TC = 5
               MOVE 9 TO W-TC
           END-IF
           IF  W-TC = 0
               MOVE 5 TO W-TC
           END-IF.
       MEI-EX.
           EXIT.
       KEI1-RTN.
           IF  ZERO = WN-CSU AND WN-CKIN AND WN-JSU AND WN-JKIN
                                        AND WN-KSU AND WN-KKIN
               GO TO KEI1-EX
           END-IF
           IF  W-TC NOT = 9
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               GO TO KEI1-020
           END-IF
           MOVE SPACE TO W-P.
           MOVE ALL "　" TO P-NAME.
           MOVE W-15K TO P-15K.
           MOVE "　　　　　　　　　　　　　　　（　計　）" TO P-NAME.
           MOVE WN-CSU TO P-CSU.
           MOVE WN-CKIN TO P-CKIN.
           MOVE WN-JSU TO P-JSU.
           MOVE WN-JKIN TO P-JKIN.
           MOVE WN-KSU TO P-KSU.
           MOVE WN-KKIN TO P-KKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       KEI1-020.
           MOVE SPACE TO SP-R.
           ADD WN-CSU TO WT-CSU.
           ADD WN-CKIN TO WT-CKIN.
           ADD WN-JSU TO WT-JSU.
           ADD WN-JKIN TO WT-JKIN.
           ADD WN-KSU TO WT-KSU.
           ADD WN-KKIN TO WT-KKIN.
       KEI1-EX.
           EXIT.
       KEI2-RTN.
           IF  ZERO = WT-CSU AND WT-CKIN AND WT-JSU AND WT-JKIN
                                        AND WT-KSU AND WT-KKIN
               GO TO KEI2-EX
           END-IF
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
           MOVE SPACE TO W-NAME W-MN.
           MOVE HKB-BRN1 TO W-MN.
           MOVE 9 TO W-EC.
       KEI2-020.
           SUBTRACT 1 FROM W-EC.
           IF  W-EC NOT = 0
               IF  W-MND(W-EC) = SPACE
                   GO TO KEI2-020
               END-IF
           END-IF
           ADD 1 TO W-EC.
           MOVE "＜" TO W-NA(13).
           MOVE 13 TO CNT.
           MOVE 0 TO W-MC.
       KEI2-040.
           ADD 1 TO CNT W-MC.
           IF  W-MC NOT = W-EC
               MOVE W-MND(W-MC) TO W-NA(CNT)
               GO TO KEI2-040
           END-IF
           MOVE "＞" TO W-NA(CNT).
           MOVE SPACE TO W-P.
           MOVE ALL "　" TO P-NAME.
           MOVE W-15K TO P-15K.
           MOVE W-NAME TO P-NAME.
           MOVE WT-CSU TO P-CSU.
           MOVE WT-CKIN TO P-CKIN.
           MOVE WT-JSU TO P-JSU.
           MOVE WT-JKIN TO P-JKIN.
           MOVE WT-KSU TO P-KSU.
           MOVE WT-KKIN TO P-KKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD WT-CSU TO WG-CSU.
           ADD WT-CKIN TO WG-CKIN.
           ADD WT-JSU TO WG-JSU.
           ADD WT-JKIN TO WG-JKIN.
           ADD WT-KSU TO WG-KSU.
           ADD WT-KKIN TO WG-KKIN.
       KEI2-EX.
           EXIT.
       KEI3-RTN.
           IF  ZERO = WG-CSU AND WG-CKIN AND WG-JSU AND WG-JKIN
                                        AND WG-KSU AND WG-KKIN
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               GO TO KEI3-EX
           END-IF
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
           MOVE SPACE TO W-NAME W-MN.
           MOVE HKB-BMN TO W-NAME.
           MOVE ZERO TO W-MC W-EC.
       KEI3-020.
           ADD 1 TO W-MC.
           IF  W-MC = 5
               GO TO KEI3-040
           END-IF
           IF  W-NA(W-MC) NOT = SPACE
               ADD 1 TO W-EC
               MOVE W-NA(W-MC) TO W-MND(W-EC)
           END-IF
           GO TO KEI3-020.
       KEI3-040.
           ADD 1 TO W-EC.
           MOVE "計" TO W-MND(W-EC).
           ADD 2 TO W-EC.
           MOVE SPACE TO W-NAME.
           MOVE "｛" TO W-NA(10).
           MOVE 11 TO CNT.
           MOVE 0 TO W-MC.
       KEI3-060.
           ADD 1 TO CNT W-MC.
           IF  W-MC NOT = W-EC
               MOVE W-MND(W-MC) TO W-NA(CNT)
               GO TO KEI3-060
           END-IF
           MOVE "｝" TO W-NA(CNT).
      *
           MOVE SPACE TO W-P.
           MOVE ALL "　" TO P-NAME.
           MOVE W-15K TO P-15K.
           MOVE W-NAME TO P-NAME.
           MOVE WG-CSU TO P-CSU.
           MOVE WG-CKIN TO P-CKIN.
           MOVE WG-JSU TO P-JSU.
           MOVE WG-JKIN TO P-JKIN.
           MOVE WG-KSU TO P-KSU.
           MOVE WG-KKIN TO P-KKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WG-CSU TO WS-CSU.
           ADD WG-CKIN TO WS-CKIN.
           ADD WG-JSU TO WS-JSU.
           ADD WG-JKIN TO WS-JKIN.
           ADD WG-KSU TO WS-KSU.
           ADD WG-KKIN TO WS-KKIN.
       KEI3-EX.
           EXIT.
       KEI4-RTN.
           IF  ZERO = WS-CSU AND WS-CKIN AND WS-JSU AND WS-JKIN
                                        AND WS-KSU AND WS-KKIN
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               GO TO KEI4-EX
           END-IF
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
           MOVE SPACE TO W-NAME W-MN.
           MOVE HKB-BRN3 TO W-NAME.
           MOVE ZERO TO W-MC W-EC.
       KEI4-020.
           ADD 1 TO W-MC.
           IF  W-MC = 5
               GO TO KEI4-040
           END-IF
           IF  W-NA(W-MC) NOT = SPACE
               ADD 1 TO W-EC
               MOVE W-NA(W-MC) TO W-MND(W-EC)
           END-IF
           GO TO KEI4-020.
       KEI4-040.
           ADD 1 TO W-EC.
           MOVE "合" TO W-MND(W-EC).
           ADD 1 TO W-EC.
           MOVE "計" TO W-MND(W-EC).
           ADD 2 TO W-EC.
           MOVE SPACE TO W-NAME.
           MOVE "［" TO W-NA(7).
           MOVE 8 TO CNT.
           MOVE 0 TO W-MC.
       KEI4-060.
           ADD 1 TO CNT W-MC.
           IF  W-MC NOT = W-EC
               MOVE W-MND(W-MC) TO W-NA(CNT)
               GO TO KEI4-060
           END-IF
           MOVE "］" TO W-NA(CNT).
      *
           MOVE SPACE TO W-P.
           MOVE ALL "　" TO P-NAME.
           MOVE W-15K TO P-15K.
           MOVE W-NAME TO P-NAME.
           MOVE WS-CSU TO P-CSU.
           MOVE WS-CKIN TO P-CKIN.
           MOVE WS-JSU TO P-JSU.
           MOVE WS-JKIN TO P-JKIN.
           MOVE WS-KSU TO P-KSU.
           MOVE WS-KKIN TO P-KKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WS-CSU TO WA-CSU.
           ADD WS-CKIN TO WA-CKIN.
           ADD WS-JSU TO WA-JSU.
           ADD WS-JKIN TO WA-JKIN.
           ADD WS-KSU TO WA-KSU.
           ADD WS-KKIN TO WA-KKIN.
       KEI4-EX.
           EXIT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRD360.
      *********************************************************
      *    入金販売変換 (WK0128___→NYUF)   　　　　　　　　  *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-PAGE             PIC  9(002) VALUE ZERO.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  ERR-STAT           PIC  X(002).
       01  W-FILE             PIC  X(013).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(043) VALUE SPACE.
           02  H-MID          PIC  N(020).
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(004) VALUE "ACT ".
           02  F              PIC  N(004) VALUE "日　　付".
           02  F              PIC  X(007) VALUE "  ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  N(004) VALUE "入金№　".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "区分".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "相殺".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "手形期日".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  N(004) VALUE "　請求日".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　消費税入金".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上入金".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(006) VALUE "振替伝票№　".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "請求書　".
       01  HEAD9.
           02  F              PIC  X(050) VALUE
                "==================================================".
           02  F              PIC  X(050) VALUE
                "==================================================".
           02  F              PIC  X(036) VALUE
                "====================================".
       01  W-P.
           02  P-15K          PIC  X(005).
           02  F              PIC  X(001).
           02  P-ACT          PIC  9(001).
           02  F              PIC  X(001).
           02  P-DATE         PIC 99/99/99.
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(001).
           02  P-NO           PIC  9(006).
           02  P-V1           PIC  X(001).
           02  P-GNO          PIC  9(001).
           02  F              PIC  X(001).
           02  P-NC           PIC  9(002).
           02  F              PIC  X(002).
           02  P-NSC          PIC  Z(001).
           02  F              PIC  X(001).
           02  P-TD           PIC 99/99/99.
           02  P-KIN          PIC ---,---,--9.
           02  P-SS           PIC B99/99.
           02  P-SHZ          PIC --,---,--9.
           02  P-TKIN         PIC ---,---,--9.
           02  F              PIC  X(001).
           02  P-FNO          PIC  9(006).
           02  P-V2           PIC  X(001).
           02  P-FGNO         PIC  9(001).
           02  F              PIC  X(001).
           02  P-SKD          PIC 99/99/99.
           02  P-20K          PIC  X(005).
       01  W-DATA.
           02  W-TCD          PIC  9(004).
           02  W-DATE         PIC  9(008).
           02  W-BC           PIC  9(001).
           02  CNT            PIC  9(001).
           02  W-TD.
             03  W-GKIN       PIC S9(009).
             03  W-TKIN       PIC S9(009).
             03  W-SHZ        PIC S9(007).
           02  W-AD.
             03  WA-GKIN      PIC S9(009).
             03  WA-TKIN      PIC S9(009).
             03  WA-SHZ       PIC S9(007).
           02  W-DNO          PIC  9(006).
           02  W-PC           PIC  9(001).
           02  W-INV          PIC  9(001).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
       01  W-MID.
           02  W-MID0         PIC  N(020).
           02  W-MID1         PIC  N(020) VALUE
                "＊＊＊　　入金伝票　変換リスト　　＊＊＊".
           02  W-MID2         PIC  N(020) VALUE
                "＊＊＊　　入金　無変換　リスト　　＊＊＊".
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHKBM.
           COPY SIWAIW.
           COPY LSPF.
      *FD  NYU-F
       01  NYU-F_PRD360.
           02  NYU-F_PNAME1   PIC  X(004) VALUE "NYUF".
           02  F              PIC  X(001).
           02  NYU-F_LNAME    PIC  X(012) VALUE "NYU-F_PRD360".
           02  F              PIC  X(001).
           02  NYU-F_KEY1     PIC  X(100) VALUE SPACE.
           02  NYU-F_SORT     PIC  X(100) VALUE SPACE.
           02  NYU-F_IDLST    PIC  X(100) VALUE SPACE.
           02  NYU-F_RES      USAGE  POINTER.
       01  NYU-R.
           02  N-DATE.
             03  F            PIC  9(002).
             03  N-DATES      PIC  9(006).
           02  N-TCD          PIC  9(004).
           02  N-KIN          PIC S9(008).
           02  N-NC.
             03  N-NC1        PIC  9(001).
             03  N-NC2        PIC  9(001).
           02  N-NSC          PIC  9(001).
           02  N-TD.
             03  F            PIC  9(002).
             03  N-TNGPS      PIC  9(006).
           02  N-SS.
             03  F            PIC  9(002).
             03  N-SNGS       PIC  9(004).
           02  N-BC           PIC  9(001).
           02  N-TC           PIC  9(002).
           02  F              PIC  X(003).
           02  N-KEY.
             03  N-NO         PIC  9(006).
             03  N-GNO        PIC  9(001).
           02  N-FDNO.
             03  N-FNO        PIC  9(006).
             03  N-FGNO       PIC  9(002).
           02  N-SKD          PIC  9(008).
           02  N-SKDL  REDEFINES N-SKD.
             03  F            PIC  9(002).
             03  N-SKDS       PIC  9(006).
           02  N-DCC          PIC  9(001).
           02  F              PIC  X(013).
           02  N-DC           PIC  9(001).
           02  F              PIC  X(002).
           02  N-ACT          PIC  9(001).
           02  N-PRC          PIC  9(001).
           02  F              PIC  X(017).
       77  F                  PIC  X(001).
      *FD  NYUW-F
       01  NYUW-F_PRD360.
           02  NYUW-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  NYUW-F_LNAME   PIC  X(013) VALUE "NYUW-F_PRD360".
           02  F              PIC  X(001).
           02  NYUW-F_KEY1    PIC  X(100) VALUE SPACE.
           02  NYUW-F_SORT    PIC  X(100) VALUE SPACE.
           02  NYUW-F_IDLST   PIC  X(100) VALUE SPACE.
           02  NYUW-F_RES     USAGE  POINTER.
       01  NYUW-R.
           02  NYUW-RD.
             03  NW-DATE.
               04  F          PIC  9(002).
               04  NW-DATES   PIC  9(006).
             03  NW-TCD       PIC  9(004).
             03  NW-KIN       PIC S9(008).
             03  NW-NC        PIC  9(002).
             03  NW-NSC       PIC  9(001).
             03  NW-TD.
               04  F          PIC  9(002).
               04  NW-TNGPS   PIC  9(006).
             03  NW-SS.
               04  F          PIC  9(002).
               04  NW-SNGS    PIC  9(004).
             03  NW-BC        PIC  9(001).
             03  NW-TC        PIC  9(002).
             03  F            PIC  X(003).
             03  NW-KEY.
               04  NW-NO      PIC  9(006).
               04  NW-GNO     PIC  9(001).
             03  NW-FDNO.
               04  NW-FNO     PIC  9(006).
               04  NW-FGNO    PIC  9(002).
             03  NW-SKD       PIC  9(008).
             03  NW-SKDL  REDEFINES NW-SKD.
               04  F          PIC  9(002).
               04  NW-SKDS    PIC  9(006).
             03  NW-DCC       PIC  9(001).
             03  F            PIC  X(016).
             03  NW-ACT       PIC  9(001).
             03  NW-PRC       PIC  9(001).
             03  F            PIC  X(017).
           02  F              PIC  X(025).
           02  NW-HHC         PIC  9(001).
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　入金販売変換（ＮＹＵＦ作成）　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-DSP.
           02  D-PRN   PIC  N(020).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME9   PIC  X(024) VALUE
                  "***  HKBM WRITE ｴﾗｰ  ***".
             03  E-ME10  PIC  X(026) VALUE
                  "***  HKBM REWRITE ｴﾗｰ  ***".
             03  E-ME11  PIC  X(017) VALUE
                  "***  HKBM ﾅｼ  ***".
             03  E-ME12  PIC  X(024) VALUE
                  "***  NYUF WRITE ｴﾗｰ  ***".
             03  E-ME13  PIC  X(031) VALUE
                  "***  SIWAKE-IW REWRITE ｴﾗｰ  ***".
             03  E-ME14  PIC  X(022) VALUE
                  "***  SIWAKE-IW ﾅｼ  ***".
             03  E-ME15  PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-SDW   PIC  X(008).
             03  E-TCD   PIC  X(004).
           COPY LSSEM.
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
           "C-MID" " " "0" "0" "336" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "48" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "48" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "48" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "48" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "48" "06C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "40" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PRN" "N" "1" "20" "40" " " "C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-PRN" BY REFERENCE W-MID0 "40" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "191" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "191" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "24" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "26" "E-ME9" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "17" "E-ME10" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "24" "E-ME11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "31" "E-ME12" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME14" "X" "24" "15" "22" "E-ME13" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME15" "X" "24" "15" "17" "E-ME14" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-SDW" "X" "24" "50" "8" "E-ME15" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-SDW" BY REFERENCE SDW-KEY "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TCD" "X" "24" "35" "4" "E-SDW" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE NW-TCD "4" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO NYUW-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NYUW-F_PNAME1 " " BY REFERENCE NYUW-F_IDLST "0".
      *           READ NYUW-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NYUW-F_PNAME1 BY REFERENCE NYUW-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE NYUW-F_IDLST NYUW-F_PNAME1
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE ZERO TO W-DATA.
      *
           CALL "DB_F_Open" USING
            "I-O" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
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
                BY REFERENCE NYUW-F_IDLST NYUW-F_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE HKB-NKN TO W-DNO.
      *
           CALL "DB_F_Open" USING
            "I-O" SDW_PNAME1 " " BY REFERENCE SDW_IDLST "1"
            "SDW-KEY" BY REFERENCE SDW-KEY.
           CALL "DB_F_Open" USING
            "I-O" NYU-F_PNAME1 " " BY REFERENCE NYU-F_IDLST "1"
            "N-KEY" BY REFERENCE N-KEY.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
       M-10.
           IF  NW-HHC NOT = 0
               MOVE ZERO TO W-TCD W-DATE
               GO TO M-30
           END-IF
           MOVE NW-TCD TO W-TCD.
           MOVE NW-DATE TO W-DATE.
           MOVE NW-BC TO W-BC.
           ADD 1 TO W-DNO.
           MOVE ZERO TO CNT.
       M-15.
           IF  NW-HHC NOT = 0
               GO TO M-30
           END-IF
           ADD 1 TO CNT.
           IF  CNT = 9
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-55
           END-IF.
       M-20.
           MOVE ZERO TO NYU-R.
           MOVE NYUW-RD TO NYU-R.
           MOVE W-DNO TO N-NO.
           MOVE CNT TO N-GNO.
           MOVE 1 TO N-DC.
      *           WRITE NYU-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            NYU-F_PNAME1 NYU-F_LNAME NYU-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-25
           END-IF
           GO TO M-35.
       M-25.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-55
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
           MOVE "NYUF         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" NYU-F_PNAME1 " " BY REFERENCE NYU-F_IDLST "1"
            "N-KEY" BY REFERENCE N-KEY.
           GO TO M-20.
       M-30.
           PERFORM NON-RTN THRU NON-EX.
       M-35.
           MOVE NW-FDNO TO SDW-KEY.
      *           READ SDW INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" SDW_PNAME1 BY REFERENCE SDW-REC " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SDW" E-SDW "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-55
           END-IF
           IF  NW-HHC = 0
               MOVE 1 TO SDWHHC
           END-IF
           IF  NW-HHC = 8
               MOVE 9 TO SDWHHC
           END-IF
      *           REWRITE SDW-REC INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SDW_PNAME1 SDW_LNAME SDW-REC RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SDW" E-SDW "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-55
           END-IF.
       M-40.
      *           READ NYUW-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NYUW-F_PNAME1 BY REFERENCE NYUW-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           IF  ZERO = W-TCD AND W-DATE
               GO TO M-10
           END-IF
           IF  NW-TCD = W-TCD
               IF  NW-DATE = W-DATE
                   IF  NW-BC = W-BC
                       IF  CNT NOT = 8
                           GO TO M-15
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           PERFORM HKB-RTN THRU HKB-EX.
           IF  W-INV = 0
               GO TO M-10
           END-IF.
       M-55.
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDW_IDLST SDW_PNAME1.
           PERFORM HKB-RTN THRU HKB-EX.
           IF  W-PC NOT = 0
               MOVE SPACE TO SP-R
               MOVE HEAD9 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "D-PRN" D-PRN "p" RETURNING RESU.
           COPY LIBCPR.
           CALL "DB_F_Open" USING
            "I-O" NYU-F_PNAME1 " " BY REFERENCE NYU-F_IDLST "1"
            "N-KEY" BY REFERENCE N-KEY.
       M-60.
      *           READ NYU-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  N-PRC = 9
               GO TO M-60
           END-IF
           MOVE W-MID1 TO H-MID.
       M-65.
           MOVE N-DATE TO W-DATE.
           MOVE ZERO TO W-AD CHK.
       M-70.
           MOVE ZERO TO W-TD CHK2.
           MOVE N-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "　＊＊　マスター　なし　＊＊　" TO T-NAME
           END-IF.
       M-75.
           IF  W-PC = 0
               MOVE 9 TO W-PC
               CALL "PR_Open" RETURNING RESP
               MOVE DATE-02R TO H-DATE
               PERFORM MID-020 THRU MID-EX
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           IF  N-GNO = 1
               MOVE N-ACT TO P-ACT
           END-IF
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               MOVE N-DATES TO P-DATE
           END-IF
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
           END-IF
           IF  N-GNO = 1
               MOVE N-NO TO P-NO
           END-IF
           MOVE "-" TO P-V1.
           MOVE N-GNO TO P-GNO.
           MOVE N-NC TO P-NC.
           MOVE N-NSC TO P-NSC.
           IF  N-TD NOT = ZERO
               MOVE N-TNGPS TO P-TD
           END-IF
           MOVE N-KIN TO P-KIN.
           IF  N-SS NOT = ZERO
               MOVE N-SNGS TO P-SS
           END-IF
           MOVE N-FNO TO P-FNO.
           MOVE "-" TO P-V2.
           MOVE N-FGNO TO P-FGNO.
           IF  N-SKD NOT = ZERO
               MOVE N-SKDS TO P-SKD
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE N-ACT TO P-ACT
               MOVE N-DATES TO P-DATE
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
               MOVE N-NO TO P-NO
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD N-KIN TO W-GKIN.
           IF  N-NC2 > 7
               ADD N-KIN TO W-SHZ
           ELSE
               ADD N-KIN TO W-TKIN
           END-IF
      *
           MOVE 9 TO N-PRC.
      *           REWRITE NYU-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            NYU-F_PNAME1 NYU-F_LNAME NYU-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       M-80.
      *           READ NYU-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  N-PRC = 9
               GO TO M-80
           END-IF
           IF  N-DATE NOT = W-DATE
               GO TO M-85
           END-IF
           IF  N-TCD = W-TCD
               GO TO M-75
           END-IF
           PERFORM KEI-RTN THRU KEI-EX.
           GO TO M-70.
       M-85.
           PERFORM KEI-RTN THRU KEI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
           GO TO M-65.
       M-90.
           PERFORM KEI-RTN THRU KEI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
           IF  W-PC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *-------------  計　印字　----------------------------------------
       KEI-RTN.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE "　　　　　　　　　　　　（　　計　　）　" TO P-TNA.
           MOVE W-GKIN TO P-KIN.
           MOVE W-SHZ TO P-SHZ.
           MOVE W-TKIN TO P-TKIN.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-GKIN TO WA-GKIN.
           ADD W-SHZ TO WA-SHZ.
           ADD W-TKIN TO WA-TKIN.
       KEI-EX.
           EXIT.
      *-------------  合計　印字　--------------------------------------
       TOT-RTN.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE "　　＜　　合　計　　＞　　　　　　　　　" TO P-TNA.
           MOVE WA-GKIN TO P-KIN.
           MOVE WA-SHZ TO P-SHZ.
           MOVE WA-TKIN TO P-TKIN.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       TOT-EX.
           EXIT.
      *-------------  見出し　印字　------------------------------------
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-020.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE  TO H-PAGE.
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
      *-------------  伝票№　更新　------------------------------------
       HKB-RTN.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
      *           READ HKBM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO HKB-010
           END-IF
           MOVE W-DNO TO HKB-NKN.
      *           REWRITE HKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO HKB-EX.
       HKB-010.
           INITIALIZE HKB-R.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
           MOVE W-DNO TO HKB-NKN.
      *           WRITE HKB-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       HKB-EX.
           EXIT.
      *-------------  入金　非変換　リスト　----------------------------
       NON-RTN.
           IF  W-PC = 0
               MOVE 9 TO W-PC
               CALL "PR_Close" RETURNING RESP
               MOVE DATE-02R TO H-DATE
               MOVE W-MID2 TO H-MID
               PERFORM MID-020 THRU MID-EX
           END-IF
           MOVE NW-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "　＊＊　マスター　なし　＊＊　" TO T-NAME
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE NW-DATES TO P-DATE.
           MOVE NW-TCD TO P-TCD.
           MOVE T-NAME TO P-TNA.
           MOVE NW-NC TO P-NC.
           MOVE NW-NSC TO P-NSC.
           IF  NW-TD NOT = ZERO
               MOVE NW-TNGPS TO P-TD
           END-IF
           MOVE NW-KIN TO P-KIN.
           IF  NW-SS NOT = ZERO
               MOVE NW-SNGS TO P-SS
           END-IF
           MOVE NW-FNO TO P-FNO.
           MOVE "-" TO P-V2.
           MOVE NW-FGNO TO P-FGNO.
           IF  NW-SKD NOT = ZERO
               MOVE NW-SKDS TO P-SKD
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE NW-DATES TO P-DATE
               MOVE NW-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       NON-EX.
           EXIT.

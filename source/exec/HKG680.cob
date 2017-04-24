       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG680.
      ******************************************************************
      *    PROGRAM     :  ワークマン・ナフコ直送先日付別　請求明細表   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(043) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "＊＊＊　　直送先日付別　請求明細表　　＊＊＊".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(008) VALUE
                "得　意　先　名　".
           02  F              PIC  X(119) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  N(008) VALUE "直　送　先　名　".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　日　付".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　伝票№".
           02  F              PIC  X(001) VALUE "-".
           02  F              PIC  N(002) VALUE "行　".
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　単　価".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
       01  W-P1.
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(092).
       01  W-P2.
           02  F              PIC  X(003).
           02  P-CCD          PIC  9(003).
           02  F              PIC  X(001).
           02  P-CNA          PIC  N(026).
           02  F              PIC  X(001).
           02  P-NGP          PIC  99/99/99.
           02  F              PIC  X(001).
           02  P-DNO          PIC  9(006).
           02  P-V            PIC  X(001).
           02  P-GNO          PIC  9(001).
           02  F              PIC  X(001).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  P-SU           PIC --,---,--9.
           02  P-T            PIC ---,--9.
           02  P-KIN          PIC ---,---,--9.
       01  W-DATA.
           02  W-POC          PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-TCD          PIC  9(004).
           02  W-DNO          PIC  9(006).
           02  W-CCD          PIC  9(003).
           02  W-BID          PIC  N(024).
           02  W-BIDD  REDEFINES W-BID.
             03  F            PIC  N(004).
             03  W-BI         PIC  N(004).
             03  F            PIC  N(016).
           02  W-DATE         PIC  9(008).
           02  W-NGP          PIC  9(008).
           02  W-NGPM  REDEFINES W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-DMM          PIC  9(001).
           02  W-ND.
             03  W-NSU        PIC S9(007).
             03  W-NKIN       PIC S9(009).
           02  W-TD.
             03  W-TSU        PIC S9(007).
             03  W-TKIN       PIC S9(009).
           02  W-SD.
             03  W-SSU        PIC S9(007).
             03  W-SKIN       PIC S9(009).
           02  W-AD.
             03  W-ASU        PIC S9(007).
             03  W-AKIN       PIC S9(009).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  W-C            PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LITCM.
           COPY LIHIM.
           COPY LSPF.
      *FD  SKDF
       01  SKDF_HKG680.
           02  SKDF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SKDF_LNAME     PIC  X(011) VALUE "SKDF_HKG680".
           02  F              PIC  X(001).
           02  SKDF_KEY1      PIC  X(100) VALUE SPACE.
           02  SKDF_SORT      PIC  X(100) VALUE SPACE.
           02  SKDF_IDLST     PIC  X(100) VALUE SPACE.
           02  SKDF_RES       USAGE  POINTER.
       01  SKD-R.
           02  SKD-KEY.
             03  SKD-TCD      PIC  9(004).
             03  SKD-DATE     PIC  9(008).
             03  SKD-NGP   REDEFINES SKD-DATE.
               04  SKD-NG.
                 05  SKD-NEN  PIC  9(004).
                 05  SKD-GET  PIC  9(002).
               04  SKD-PEY    PIC  9(002).
             03  SKD-NGPL  REDEFINES SKD-DATE.
               04  F          PIC  9(002).
               04  SKD-NGPS   PIC  9(006).
             03  SKD-DTC      PIC  9(001).
             03  SKD-DNO      PIC  9(006).
             03  SKD-GNO      PIC  9(001).
           02  SKD-HCD        PIC  9(006).
           02  SKD-SU         PIC S9(006)V9(02).
           02  SKD-T          PIC S9(006)V9(02).
           02  SKD-KIN        PIC S9(009).
           02  SKD-DC         PIC  9(001).
           02  SKD-CSC        PIC  9(001).
           02  SKD-SKD        PIC  9(008).
           02  SKD-TNC.
             03  SKD-TNC1     PIC  9(001).
             03  SKD-TNC2     PIC  9(001).
           02  SKD-BMC        PIC  9(001).
           02  SKD-DCC        PIC  9(001).
           02  F              PIC  X(002).
           02  SKD-TCD2       PIC  9(004).
           02  SKD-CCD        PIC  9(003).
           02  SKD-BID        PIC  N(024).
           02  SKD-BIDD  REDEFINES SKD-BID.
             03  F            PIC  N(004).
             03  SKD-BI       PIC  N(004).
             03  F            PIC  N(016).
           02  SKD-HNO.
             03  SKD-HNO1     PIC  9(006).
             03  SKD-HNO2     PIC  9(006).
             03  SKD-HNO3     PIC  9(006).
             03  SKD-HNO4     PIC  9(006).
             03  SKD-HNO5     PIC  9(006).
             03  SKD-HNO6     PIC  9(006).
           02  SKD-SHZ        PIC S9(007).
           02  SKD-KSU        PIC  9(003).
           02  SKD-JCD        PIC  9(006).
           02  F              PIC  X(012).
           02  SKD-SNO        PIC  9(006).
           02  F              PIC  X(064).
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　直送先日付別　請求明細表　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(020) VALUE
                "'00年 00月 00日 請求".
           02  FILLER  PIC  X(018) VALUE
                "得意先      ".
           02  FILLER  PIC  X(009) VALUE
                "終了=Ｆ９".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-NEN   PIC  9(002).
             03  A-GET   PIC  9(002).
             03  A-PEY   PIC  9(002).
           02  A-TCD   PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-TNA   PIC  N(026).
             03  D-TNAC.
               04  FILLER  PIC  X(030) VALUE
                    "                              ".
               04  FILLER  PIC  X(022) VALUE
                    "                      ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  ﾄｸｲｻｷ ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "377" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "44" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "44" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "44" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "44" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "44" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "44" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "44" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "13" "22" "20" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "15" "10" "18" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "16" "23" "9" "09C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-MID" "X" "22" "24" "22" "10C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "11" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "13" "0" "6" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "13" "23" "2" " " "01C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "13" "28" "2" "A-NEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PEY" "9" "13" "33" "2" "A-GET" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "15" "18" "4" "01C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "41" "1" "A-TCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "104" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "15" "0" "104" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "15" "23" "52" " " "01C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNAC" " " "15" "0" "52" "D-TNA" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TNAC" "X" "0" "23" "30" " " "D-TNAC"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TNAC" "X" "0" "53" "22" "01D-TNAC" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "95" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "95" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           MOVE SPACE TO W-BID.
           COPY LIBCPR.
           ACCEPT W-NGPS FROM DATE.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NGP TO W-DATE.
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
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO SKDF_PNAME1.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-PEY "A-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-20
           END-IF
      *
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-DATE <= W-NGP
               GO TO M-10
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
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
                "E-ME2" E-ME2 "p" RETURNING RESU
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-30
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" SKDF_PNAME1 " " BY REFERENCE SKDF_IDLST "0".
       M-35.
      *           READ SKDF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" SKDF_PNAME1 BY REFERENCE SKD-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SKDF_IDLST SKDF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               GO TO M-05
           END-IF
           IF  SKD-TCD NOT = W-TCD
               GO TO M-35
           END-IF
           IF  SKD-SKD NOT = W-NGP
               GO TO M-35
           END-IF
      *
           IF  W-POC = 0
               MOVE 1 TO W-POC
               CALL "PR_Open" RETURNING RESP
               MOVE W-NGPS TO H-DATE
               PERFORM HED-010 THRU HED-EX
           END-IF
           MOVE SPACE TO W-P1.
           MOVE SKD-TCD TO P-TCD.
           MOVE T-NAME TO P-TNA.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO W-AD.
       M-40.
           MOVE ZERO TO W-SD CHK.
           IF  W-TCD = 9850
               MOVE SKD-BID TO W-BID
               GO TO M-45
           END-IF
           MOVE SKD-CCD TO W-CCD.
           MOVE SKD-TCD TO TC-TCD.
           MOVE SKD-CCD TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
               MOVE "　＊＊　　直送先　なし　　＊＊" TO TC-NAME
           END-IF.
       M-45.
           MOVE ZERO TO W-TD CHK2 W-C.
           MOVE SKD-DNO TO W-DNO.
       M-50.
           MOVE SKD-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "　＊＊　　品名　なし　　＊＊　" TO HI-NAME
           END-IF
           MOVE SKD-SU TO W-NSU.
           MOVE SKD-KIN TO W-NKIN.
           IF  SKD-DTC = 1
               COMPUTE W-NSU = SKD-SU * -1
               COMPUTE W-NKIN = SKD-KIN * -1
           ELSE
               IF  SKD-DTC = 0
                   IF  SKD-DC = 1 OR 2 OR 5
                       COMPUTE W-NSU = SKD-SU * -1
                       COMPUTE W-NKIN = SKD-KIN * -1
                   END-IF
               END-IF
           END-IF
      *
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-CNA P-HNA.
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               IF  W-TCD = 9850
                   MOVE W-BID TO P-CNA
               ELSE
                   MOVE W-CCD TO P-CCD
                   MOVE TC-NAME TO P-CNA
               END-IF
           END-IF
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               MOVE SKD-NGPS TO P-NGP
               MOVE SKD-DNO TO P-DNO
           END-IF
           MOVE "-" TO P-V.
           MOVE SKD-GNO TO P-GNO.
           MOVE SKD-HCD TO P-HCD.
           MOVE HI-NAME TO P-HNA.
           MOVE W-NSU TO P-SU.
           MOVE SKD-T TO P-T.
           MOVE W-NKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM HED-RTN THRU HED-EX
               MOVE SPACE TO SP-R
               MOVE W-P1 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE SKD-NGPS TO P-NGP
               MOVE SKD-DNO TO P-DNO
               IF  W-TCD = 9850
                   MOVE W-BID TO P-CNA
               ELSE
                   MOVE W-CCD TO P-CCD
                   MOVE TC-NAME TO P-CNA
               END-IF
           END-IF
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  W-C = 1
               MOVE 2 TO W-C
           END-IF
           IF  W-C = 0
               MOVE 1 TO W-C
           END-IF
      *
           ADD W-NSU TO W-TSU.
           ADD W-NKIN TO W-TKIN.
       M-55.
      *           READ SKDF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" SKDF_PNAME1 BY REFERENCE SKD-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-65
           END-IF
           IF  SKD-TCD NOT = W-TCD
               GO TO M-65
           END-IF
           IF  SKD-SKD NOT = W-NGP
               GO TO M-55
           END-IF
           IF  W-TCD = 9850
               IF  SKD-BI NOT = W-BI
                   GO TO M-60
               END-IF
           END-IF
           IF  W-TCD NOT = 9850
               IF  SKD-CCD NOT = W-CCD
                   GO TO M-60
               END-IF
           END-IF
           IF  SKD-DNO = W-DNO
               GO TO M-50
           END-IF
      *
           PERFORM PRT-RTN THRU PRT-EX.
           GO TO M-45.
       M-60.
           PERFORM PRT-RTN THRU PRT-EX.
           PERFORM PRS-RTN THRU PRS-EX.
           GO TO M-40.
       M-65.
           PERFORM PRT-RTN THRU PRT-EX.
           PERFORM PRS-RTN THRU PRS-EX.
           PERFORM PRA-RTN THRU PRA-EX.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           GO TO M-25.
       M-90.
           IF W-POC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       HED-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       HED-010.
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
       HED-EX.
           EXIT.
       PRT-RTN.
           IF  W-C NOT = 2
               GO TO PRT-010
           END-IF
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-CNA P-HNA.
           MOVE "　　　　　　　　　　　　　　　（　計　）" TO P-HNA.
           MOVE W-TSU TO P-SU.
           MOVE W-TKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM HED-RTN THRU HED-EX
               MOVE SPACE TO SP-R
               MOVE W-P1 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               IF  W-TCD = 9850
                   MOVE W-BID TO P-CNA
               ELSE
                   MOVE W-CCD TO P-CCD
                   MOVE TC-NAME TO P-CNA
               END-IF
           END-IF
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       PRT-010.
           ADD W-TSU TO W-SSU.
           ADD W-TKIN TO W-SKIN.
       PRT-EX.
           EXIT.
       PRS-RTN.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-CNA P-HNA.
           MOVE "　　　［　合　計　］　" TO P-HNA.
           MOVE W-SSU TO P-SU.
           MOVE W-SKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM HED-RTN THRU HED-EX
               MOVE SPACE TO SP-R
               MOVE W-P1 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               IF  W-TCD = 9850
                   MOVE W-BID TO P-CNA
               ELSE
                   MOVE W-CCD TO P-CCD
                   MOVE TC-NAME TO P-CNA
               END-IF
           END-IF
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-SSU TO W-ASU.
           ADD W-SKIN TO W-AKIN.
       PRS-EX.
           EXIT.
       PRA-RTN.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-CNA P-HNA.
           MOVE "　　　　　　　　　【　総　合　計　】" TO P-CNA.
           MOVE W-ASU TO P-SU.
           MOVE W-AKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM HED-RTN THRU HED-EX
               MOVE SPACE TO SP-R
               MOVE W-P1 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       PRA-EX.
           EXIT.

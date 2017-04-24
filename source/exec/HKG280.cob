       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG280.
      ******************************************************************
      *    PROGRAM         :  担当得意先品種別　非請求明細表　         *
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
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  N(025) VALUE
                "＊＊＊　　担当得意先品種別　非請求明細表　　＊＊＊".
           02  F              PIC  X(021) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "担当".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　日　付".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　単　価".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
       01  W-P.
           02  F              PIC  X(001).
           02  P-TNC          PIC  9(002).
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(001).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-NGP          PIC 99/99/99.
           02  P-SU           PIC --,---,--9.
           02  P-T            PIC ---,--9.
           02  P-KIN          PIC ----,---,--9.
       01  W-DATA.
           02  W-END          PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-NGPD.
             03  W-SNGP       PIC  9(008).
             03  W-SNGPD REDEFINES W-SNGP.
               04  W-SNEN     PIC  9(004).
               04  W-SNENL  REDEFINES W-SNEN.
                 05  W-SNEN1  PIC  9(002).
                 05  W-SNEN2  PIC  9(002).
               04  W-SGET     PIC  9(002).
               04  W-SPEY     PIC  9(002).
             03  W-ENGP       PIC  9(008).
             03  W-ENGPD REDEFINES W-ENGP.
               04  W-ENEN     PIC  9(004).
               04  W-ENENL  REDEFINES W-ENEN.
                 05  W-ENEN1  PIC  9(002).
                 05  W-ENEN2  PIC  9(002).
               04  W-EGET     PIC  9(002).
               04  W-EPEY     PIC  9(002).
           02  W-TNCD.
             03  W-STNC       PIC  9(002).
             03  W-ETNC       PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-TNC.
             03  W-TNC1       PIC  9(001).
             03  W-TNC2       PIC  9(001).
           02  W-TCD          PIC  9(004).
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
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LSPF.
      *FD  SKDKF
       01  SKDKF_HKG280.
           02  SKDKF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SKDKF_LNAME    PIC  X(012) VALUE "SKDKF_HKG280".
           02  F              PIC  X(001).
           02  SKDKF_KEY1     PIC  X(100) VALUE SPACE.
           02  SKDKF_KEY2     PIC  X(100) VALUE SPACE.
           02  SKDKF_SORT     PIC  X(100) VALUE SPACE.
           02  SKDKF_IDLST    PIC  X(100) VALUE SPACE.
           02  SKDKF_RES      USAGE  POINTER.
       01  SKDK-R.
           02  SKDK-KEY.
             03  SKDK-TCD     PIC  9(004).
             03  SKDK-DATE    PIC  9(008).
             03  SKDK-NGP   REDEFINES SKDK-DATE.
               04  SKDK-NG.
                 05  SKDK-NEN PIC  9(004).
                 05  SKDK-GET PIC  9(002).
               04  SKDK-PEY   PIC  9(002).
             03  SKDK-NGPL  REDEFINES SKDK-DATE.
               04  F          PIC  9(002).
               04  SKDK-NGPS  PIC  9(006).
             03  SKDK-DTC     PIC  9(001).
             03  SKDK-DNO     PIC  9(006).
             03  SKDK-GNO     PIC  9(001).
           02  SKDK-HCD       PIC  9(006).
           02  SKDK-SU        PIC S9(006)V9(02).
           02  SKDK-T         PIC S9(006)V9(02).
           02  SKDK-KIN       PIC S9(009).
           02  SKDK-DC        PIC  9(001).
           02  SKDK-CSC       PIC  9(001).
           02  SKDK-SKD       PIC  9(008).
           02  SKDK-TNC.
             03  SKDK-TNC1    PIC  9(001).
             03  SKDK-TNC2    PIC  9(001).
           02  SKDK-BMC       PIC  9(001).
           02  SKDK-DCC       PIC  9(001).
           02  F              PIC  X(002).
           02  SKDK-TCD2      PIC  9(004).
           02  SKDK-CCD       PIC  9(003).
           02  SKDK-BI        PIC  N(024).
           02  SKDK-HNO.
             03  SKDK-HNO1    PIC  9(006).
             03  SKDK-HNO2    PIC  9(006).
             03  SKDK-HNO3    PIC  9(006).
             03  SKDK-HNO4    PIC  9(006).
             03  SKDK-HNO5    PIC  9(006).
             03  SKDK-HNO6    PIC  9(006).
           02  SKDK-SHZ       PIC S9(007).
           02  SKDK-KSU       PIC  9(003).
           02  SKDK-JCD       PIC  9(006).
           02  F              PIC  X(012).
           02  SKDK-SNO       PIC  9(006).
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
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　担当得意先品種別　非請求明細表　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(018) VALUE
                "担当ｺｰﾄﾞ  00 ～ 99".
           02  FILLER  PIC  X(036) VALUE
                "'00年 00月 00日　～　'99年 99月 99日".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-STNC  PIC  9(002).
             03  A-ETNC  PIC  9(002).
           02  FILLER.
             03  A-SNEN  PIC  9(002).
             03  A-SGET  PIC  9(002).
             03  A-SPEY  PIC  9(002).
             03  A-ENEN  PIC  9(002).
             03  A-EGET  PIC  9(002).
             03  A-EPEY  PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
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
            "C-MID" " " "0" "0" "426" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "50" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "50" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "50" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "50" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "50" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "50" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "50" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "13" "26" "18" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "15" "17" "36" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "22" "24" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "17" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "13" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STNC" "9" "13" "36" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STNC" BY REFERENCE W-STNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ETNC" "9" "13" "42" "2" "A-STNC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ETNC" BY REFERENCE W-ETNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "15" "0" "12" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNEN" "9" "15" "18" "2" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNEN" BY REFERENCE W-SNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SGET" "9" "15" "23" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SPEY" "9" "15" "28" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENEN" "9" "15" "39" "2" "A-SPEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENEN" BY REFERENCE W-ENEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EGET" "9" "15" "44" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EPEY" "9" "15" "49" "2" "A-EGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "41" "1" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "27" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "27" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           MOVE 99 TO W-ETNC W-ENEN2 W-EGET W-EPEY.
           COPY LIBCPR.
      *
           PERFORM ACP-RTN THRU ACP-EX.
           IF  W-END NOT = 0
               GO TO M-95
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO SKDKF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SKDKF_PNAME1 " " BY REFERENCE SKDKF_IDLST "0".
       M-10.
      *           READ SKDKF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SKDKF_PNAME1 BY REFERENCE SKDK-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SKDKF_IDLST SKDKF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  SKDK-TNC < W-STNC
               GO TO M-10
           END-IF
           IF  SKDK-TNC > W-ETNC
               CALL "DB_F_Close" USING
                BY REFERENCE SKDKF_IDLST SKDKF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  SKDK-DATE < W-SNGP OR > W-ENGP
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           PERFORM HED-010 THRU HED-EX.
       M-15.
           MOVE ZERO TO W-SD.
           MOVE SKDK-TNC1 TO W-TNC1.
       M-20.
           MOVE ZERO TO CHK.
           MOVE SKDK-TNC2 TO W-TNC2.
       M-25.
           MOVE ZERO TO W-TD CHK2.
           MOVE SKDK-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "　＊＊　　得意先　なし　　＊＊　" TO T-NAME
           END-IF.
       M-30.
           MOVE SKDK-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "　＊＊　　品名　なし　　＊＊　" TO HI-NAME
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA P-HNA.
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               MOVE W-TNC TO P-TNC
           END-IF
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
           END-IF
           MOVE SKDK-HCD TO P-HCD.
           MOVE HI-NAME TO P-HNA.
           MOVE SKDK-NGPS TO P-NGP.
           MOVE SKDK-SU TO P-SU.
           MOVE SKDK-T TO P-T.
           MOVE SKDK-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TNC TO P-TNC
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
               PERFORM HED-RTN THRU HED-EX
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD SKDK-SU TO W-TSU.
           ADD SKDK-KIN TO W-TKIN.
       M-35.
      *           READ SKDKF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SKDKF_PNAME1 BY REFERENCE SKDK-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SKDK-TNC > W-ETNC
               GO TO M-90
           END-IF
           IF  SKDK-DATE < W-SNGP OR > W-ENGP
               GO TO M-35
           END-IF
           IF  SKDK-TNC NOT = W-TNC
               GO TO M-40
           END-IF
           IF  SKDK-TCD = W-TCD
               GO TO M-30
           END-IF
           PERFORM PRT-RTN THRU PRT-EX.
           GO TO M-25.
       M-40.
           PERFORM PRT-RTN THRU PRT-EX.
           IF  SKDK-TNC1 = W-TNC1
               GO TO M-20
           END-IF
           PERFORM PRS-RTN THRU PRS-EX.
           GO TO M-15.
       M-90.
           PERFORM PRT-RTN THRU PRT-EX.
           PERFORM PRS-RTN THRU PRS-EX.
           PERFORM PRA-RTN THRU PRA-EX.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING
            BY REFERENCE SKDKF_IDLST SKDKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-STNC "A-STNC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               MOVE 1 TO W-END
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-RTN
           END-IF.
       ACP-010.
           CALL "SD_Accept" USING BY REFERENCE A-ETNC "A-ETNC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-010
           END-IF
           IF  W-STNC > W-ETNC
               GO TO ACP-010
           END-IF.
       ACP-020.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-010
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-020
           END-IF
           MOVE ZERO TO W-SNEN1.
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
       ACP-030.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-030
           END-IF.
       ACP-035.
           CALL "SD_Accept" USING BY REFERENCE A-SPEY "A-SPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-030
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-035
           END-IF
           IF  ZERO = W-SGET AND W-SPEY
               IF  W-SNEN2 = ZERO
                   MOVE ZERO TO W-SNEN1
               END-IF
           END-IF.
       ACP-040.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-035
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-040
           END-IF
           MOVE ZERO TO W-ENEN1.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
       ACP-050.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-050
           END-IF.
       ACP-060.
           CALL "SD_Accept" USING BY REFERENCE A-EPEY "A-EPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-050
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-060
           END-IF
           IF  99 = W-EGET AND W-EPEY
               IF  W-ENEN2 = 99
                   MOVE 99 TO W-ENEN1
               END-IF
           END-IF
           IF W-SNGP > W-ENGP
               GO TO ACP-060
           END-IF.
       ACP-070.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-070
           END-IF
           IF  W-DMM = 9
               GO TO ACP-RTN
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-070
           END-IF.
       ACP-EX.
           EXIT.
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
       HED-EX.
           EXIT.
       PRT-RTN.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA P-HNA.
           MOVE "　　　　　　　　　　　　　　　（　計　）" TO P-HNA.
           MOVE W-TSU TO P-SU.
           MOVE W-TKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TNC TO P-TNC
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
               PERFORM HED-RTN THRU HED-EX
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-TSU TO W-SSU.
           ADD W-TKIN TO W-SKIN.
       PRT-EX.
           EXIT.
       PRS-RTN.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA P-HNA.
           MOVE "　　　　　　　　　　［　合　計　］" TO P-HNA.
           MOVE W-SSU TO P-SU.
           MOVE W-SKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM HED-RTN THRU HED-EX
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD W-SSU TO W-ASU.
           ADD W-SKIN TO W-AKIN.
       PRS-EX.
           EXIT.
       PRA-RTN.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA P-HNA.
           MOVE "　　　　【　総　合　計　】" TO P-HNA.
           MOVE W-ASU TO P-SU.
           MOVE W-AKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM HED-RTN THRU HED-EX
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       PRA-EX.
           EXIT.

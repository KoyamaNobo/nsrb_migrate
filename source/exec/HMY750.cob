       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY750.
      *********************************************************
      *    PROGRAM         :  履物　年間　生産明細表          *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  04/02/28                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-SN           PIC 99.
           02  F              PIC  N(001) VALUE "年".
           02  H-SG           PIC Z9.
           02  F              PIC  X(005) VALUE "月 - ".
           02  H-EN           PIC 99.
           02  F              PIC  N(001) VALUE "年".
           02  H-EG           PIC Z9.
           02  F              PIC  N(017) VALUE
                "月　履物年間　生産明細表　　＊＊＊".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  N(004) VALUE "生産区分".
           02  F              PIC  X(008) VALUE "  ｺｰﾄﾞ  ".
           02  F              PIC  N(005) VALUE "品　　　名".
           02  F              PIC  X(045) VALUE SPACE.
           02  F              PIC  N(002) VALUE "数量".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単価".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
       01  W-P.
           02  P-SCM          PIC  N(004).
           02  F              PIC  X(002).
           02  P-HCD1         PIC  9(004).
           02  F              PIC  X(002).
           02  P-NAME         PIC  N(024).
           02  P-SU           PIC ---,---,--9.
           02  P-TN           PIC ---,--9.
           02  P-SK           PIC ---,---,---,--9.
       01  W-KEY.
           02  W-SC           PIC  9(001).
           02  W-HCD1         PIC  9(004).
       01  W-ND.
           02  WN-SU          PIC S9(008).
           02  WN-SK          PIC S9(010).
       01  W-SD.
           02  WS-SU          PIC S9(008).
           02  WS-SK          PIC S9(010).
       01  W-AD.
           02  WA-SU          PIC S9(008).
           02  WA-SK          PIC S9(010).
       01  W-DATA.
           02  W-TD           PIC  9(004).
           02  CHK            PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-PAGE         PIC  9(002).
           02  W-NAMET.
             03  W-NAME  OCCURS  24  PIC  N(001).
           02  W-NAMED  REDEFINES W-NAMET  PIC  N(024).
           02  W-NAT.
             03  W-NA    OCCURS  24  PIC  N(001).
           02  W-NAD    REDEFINES W-NAT    PIC  N(024).
           02  W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SND   REDEFINES W-SNEN.
               04  W-SN1      PIC  9(002).
               04  W-SN2      PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-ENG.
             03  W-ENEN       PIC  9(004).
             03  W-END   REDEFINES W-ENEN.
               04  W-EN1      PIC  9(002).
               04  W-EN2      PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-ENGL  REDEFINES W-ENG.
             03  F            PIC  9(002).
             03  W-ENGS       PIC  9(004).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LIHIM.
           COPY LIHKBM.
           COPY LSPF.
       01  SUSR-F_HMY750.
           02  SUSR-F_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F               PIC  X(001).
           02  SUSR-F_LNAME    PIC  X(013)  VALUE "SUSR-F_HMY750".
           02  F               PIC  X(001).
           02  SUSR-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  SUSR-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  SUSR-F_SORT     PIC  X(100)  VALUE SPACE.
           02  SUSR-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  SUSR-F_RES      USAGE  POINTER.
       01  SUSR-R.
           02  UR-HCD.
             03  UR-HCD1      PIC  9(004).
             03  UR-HCD2      PIC  9(002).
           02  UR-SU          PIC S9(007).
           02  UR-SK          PIC S9(009).
           02  F              PIC  X(009).
           02  UR-SC          PIC  9(001).
           02  F              PIC  X(009).
           02  UR-NRC         PIC  9(001).
           02  F              PIC  X(003).
           02  UR-NG          PIC  9(006).
           02  F              PIC  X(013).
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
           02  C-CL  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　履物　年間　生産明細表　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(026) VALUE
                "'  年   月  ～  '  年   月".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM     PIC  9(001).
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC 99.
             03  FILLER  PIC Z9.
             03  FILLER  PIC Z9.
             03  FILLER  PIC Z9.
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2     PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
             03  E-HCD     PIC  9(004).
           COPY LIBSCR.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "342" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "42" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "14" "18" "26" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "20" "20" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "37" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "14" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "99" "14" "19" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-SN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "Z9" "14" "24" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NG" "Z9" "14" "35" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE W-EN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NG" "Z9" "14" "40" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NG" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "47" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "47" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "16" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-HCD" "X" "24" "36" "4" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HCD" BY REFERENCE W-HCD1 "4" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO W-SNG W-ENG.
           MOVE D-SPNG TO W-SNGS.
           MOVE D-EPNG TO W-ENGS.
           IF  W-SN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF.
           IF  W-SN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
           IF  W-EN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF.
           IF  W-EN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" 
                                         RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
           IF  W-DMM = 9
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF.
           MOVE W-SN2 TO H-SN.
           MOVE W-SGET TO H-SG.
           MOVE W-EN2 TO H-EN.
           MOVE W-EGET TO H-EG.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SUSR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SUSR-F_PNAME1 " " BY REFERENCE SUSR-F_IDLST "0".
       M-15.
      *           READ SUSR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SUSR-F_PNAME1 BY REFERENCE SUSR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                         RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SUSR-F_IDLST SUSR-F_PNAME1
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  UR-NG < W-SNG OR > W-ENG
               GO TO M-15
           END-IF.
           IF  UR-SC = ZERO
               GO TO M-15
           END-IF.
           IF  ZERO = UR-SU AND UR-SK
               GO TO M-15
           END-IF.
           IF  UR-HCD > 979999
               GO TO M-15
           END-IF.
           IF  UR-NRC = 2
               GO TO M-15
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO W-PAGE.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO W-AD.
       M-20.
           MOVE UR-SC TO W-SC.
           MOVE ZERO TO W-SD CHK.
           MOVE SPACE TO HKB-KEY.
           MOVE "42" TO HKB-NO.
           MOVE W-SC TO HKB-SSC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-SSNA
           END-IF.
       M-25.
           MOVE UR-HCD1 TO W-HCD1.
           MOVE ZERO TO W-ND.
       M-30.
           IF  UR-NRC = 5
               SUBTRACT UR-SU FROM WN-SU
               SUBTRACT UR-SK FROM WN-SK
           ELSE
               ADD UR-SU TO WN-SU
               ADD UR-SK TO WN-SK
           END-IF.
       M-35.
      *           READ SUSR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SUSR-F_PNAME1 BY REFERENCE SUSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-60
           END-IF.
           IF  UR-NG < W-SNG OR > W-ENG
               GO TO M-35
           END-IF.
           IF  UR-SC = ZERO
               GO TO M-35
           END-IF.
           IF  ZERO = UR-SU AND UR-SK
               GO TO M-35
           END-IF.
           IF  UR-HCD > 979999
               GO TO M-35
           END-IF.
           IF  UR-NRC = 2
               GO TO M-35
           END-IF.
           IF  W-SC NOT = UR-SC
               GO TO M-40
           END-IF.
           IF  W-HCD1 = UR-HCD1
               GO TO M-30
           END-IF.
           PERFORM S-20 THRU S-45.
           GO TO M-25.
       M-40.
           PERFORM S-20 THRU S-45.
           PERFORM S-50 THRU S-55.
           GO TO M-20.
       M-60.
           PERFORM S-20 THRU S-45.
           PERFORM S-50 THRU S-55.
           MOVE SPACE TO W-P.
           MOVE "　【　ＡＬＬ　ＴＯＴＡＬ　】　" TO P-NAME.
           MOVE WA-SU TO P-SU.
           MOVE WA-SK TO P-SK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 58
               PERFORM S-05 THRU S-10
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE SUSR-F_IDLST SUSR-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
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
       S-15.
           EXIT.
       S-20.
           IF  ZERO = WN-SU AND WN-SK
               GO TO S-45
           END-IF.
           MOVE SPACE TO W-NAMED W-NAD.
           MOVE ZERO TO CNT.
           MOVE "　＊＊＊　マスター　ナシ　＊＊＊" TO W-NAMED.
           MOVE ZERO TO HI-KEY.
           MOVE W-HCD1 TO HI-HCD1.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               GO TO S-30
           END-IF
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO S-30
           END-IF.
           IF  W-HCD1 NOT = HI-HCD1
               GO TO S-30
           END-IF.
           MOVE HI-NAME TO W-NAMED.
       S-30.
           ADD 1 TO CNT.
           IF  CNT = 25
               GO TO S-35
           END-IF.
           MOVE W-NAME(CNT) TO W-NA(CNT).
           IF  W-NAME(CNT) NOT = SPACE
               GO TO S-30
           END-IF.
           ADD 1 TO CNT.
           IF  CNT = 25
               GO TO S-35
           END-IF.
           MOVE W-NAME(CNT) TO W-NA(CNT).
           IF  W-NAME(CNT) NOT = SPACE
               GO TO S-30
           END-IF.
       S-35.
           MOVE ZERO TO W-TD.
           IF  ZERO NOT = WN-SU AND WN-SK
               COMPUTE W-TD = WN-SK / WN-SU
           END-IF.
           MOVE SPACE TO SP-R W-P.
           IF  CHK = ZERO
               MOVE 5 TO CHK
               MOVE HKB-SSNA TO P-SCM
           END-IF.
           MOVE W-HCD1 TO P-HCD1.
           MOVE W-NAD TO P-NAME.
           MOVE WN-SU TO P-SU.
           MOVE W-TD TO P-TN.
           MOVE WN-SK TO P-SK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 58
               MOVE HKB-SSNA TO P-SCM
               PERFORM S-05 THRU S-10
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD WN-SU TO WS-SU.
           ADD WN-SK TO WS-SK.
       S-45.
           EXIT.
       S-50.
           MOVE SPACE TO W-P.
           MOVE "　［　ＳＵＢ　ＴＯＴＡＬ　］　" TO P-NAME.
           MOVE WS-SU TO P-SU.
           MOVE WS-SK TO P-SK.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  LINAGECOUNTER > 58
               MOVE HKB-SSNA TO P-SCM
               PERFORM S-05 THRU S-10
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-SU TO WA-SU.
           ADD WS-SK TO WA-SK.
       S-55.
           EXIT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.  HKG760.
      *********************************************************
      *    PROGRAM         :  得意先元帳総括表　　　　　      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    FORM            :  FHG760                          *
      *        変更　　　  :  62/05/29                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
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
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(049) VALUE SPACE.
           02  W-30K          PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  N(008) VALUE "得意先元帳総括票".
           02  W-35K          PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　№".
           02  F              PIC  X(001) VALUE SPACE.
           02  P-DNO          PIC  N(004).
       01  HEAD2.
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(011) VALUE SPACE.
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(065) VALUE SPACE.
       01  HEAD8.
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  N(004) VALUE "日　　付".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売　　上".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "入　　金".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売掛残高".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　消費税残高".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "残高合計".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "備　考　".
       01  W-P1.
           02  F              PIC  X(062).
           02  P-UM           PIC  N(002).
           02  P-UNO          PIC  X(008).
           02  F              PIC  X(001).
           02  P-JSU          PIC  N(020).
           02  F              PIC  X(004).
       01  W-P2.
           02  F              PIC  X(013).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-NAME         PIC  N(026).
           02  F              PIC  X(020).
           02  P-JSS          PIC  N(020).
       01  W-P3.
           02  F              PIC  X(058).
           02  P-F            PIC  X(001).
           02  P-BMC          PIC  9(001).
           02  P-R            PIC  X(001).
           02  F              PIC  X(009).
           02  P-TM           PIC  X(003).
           02  F              PIC  X(001).
           02  P-TEL          PIC  X(014).
           02  F              PIC  X(002).
           02  P-FM           PIC  X(003).
           02  F              PIC  X(001).
           02  P-FAX          PIC  X(014).
       01  W-P4.
           02  F              PIC  X(013).
           02  P-DNM          PIC  N(008).
           02  F              PIC  X(083).
       01  W-P5.
           02  F              PIC  X(013).
           02  P-MSM          PIC  N(008).
           02  P-UK           PIC -----,---,---.
           02  P-UKZ          PIC ---,---,---.
           02  P-NK           PIC -----,---,---.
           02  P-UZ           PIC -----,---,---.
           02  P-UZZ          PIC ---,---,---.
           02  P-UZG          PIC -----,---,---.
           02  F              PIC  X(009).
       01  W-DATA.
           02  W-TCD          PIC  9(004).
           02  W-DATE         PIC  9(008).
           02  WP-D.
             03  WP-NGP.
               04  WP-NEN     PIC 99.
               04  WP-GET     PIC Z9.
               04  WP-PEY     PIC Z9.
             03  WP-TGP.
               04  WP-TG      PIC Z9.
               04  WP-TP      PIC Z9.
           02  W-DNMD.
             03  W-NEN        PIC  N(002).
             03  F            PIC  N(001) VALUE "．".
             03  W-GET        PIC  N(002).
             03  F            PIC  N(001) VALUE "．".
             03  W-PEY        PIC  N(002).
           02  W-DNM   REDEFINES W-DNMD  PIC  N(008).
           02  W-MSM          PIC  N(008).
           02  W-DNO          PIC  N(004).
           02  W-DNOD  REDEFINES W-DNO.
             03  F            PIC  N(001).
             03  W-TNC1       PIC  N(001).
             03  W-V          PIC  N(001).
             03  W-DCC        PIC  N(001).
           02  W-NK           PIC S9(009).
           02  W-UZ           PIC S9(009).
           02  W-UZZ          PIC S9(007).
           02  W-UZG          PIC S9(009).
           02  W-TD.
             03  W-TUK        PIC S9(009).
             03  W-TUKZ       PIC S9(007).
             03  W-TNK        PIC S9(009).
           02  W-DMM          PIC  9(001).
           02  W-TST          PIC  9(001).
           02  W-PC           PIC  9(001) VALUE 0.
           02  W-OC           PIC  9(001) VALUE 0.
           02  CNT            PIC  9(002) VALUE ZERO.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LITM.
      *FD  TMS-F
       01  TMS-F_HKG760.
           02  TMS-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TMS-F_LNAME    PIC  X(012) VALUE "TMS-F_HKG760".
           02  F              PIC  X(001).
           02  TMS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  TMS-F_SORT     PIC  X(100) VALUE SPACE.
           02  TMS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  TMS-F_RES      USAGE  POINTER.
       01  TMS-R.
           02  TMS-TCD        PIC  9(004).
           02  TMS-DATE       PIC  9(008).
           02  TMS-NGP   REDEFINES TMS-DATE.
             03  F            PIC  9(002).
             03  TMS-NEN      PIC  9(002).
             03  TMS-GET      PIC  9(002).
             03  TMS-PEY      PIC  9(002).
           02  TMS-KIN        PIC S9(009).
           02  TMS-SHZ        PIC S9(007).
           02  TMS-KBN        PIC  9(001).
           02  TMS-BC         PIC  9(001).
           02  TMS-TNC.
             03  TMS-TNC1     PIC  9(001).
             03  TMS-TNC2     PIC  9(001).
           02  TMS-DCC        PIC  9(001).
           02  F              PIC  X(031).
       77  F                  PIC  X(001).
      *
       77  SP-R               PIC  X(170).
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
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　得意先元帳　総括表　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(040) VALUE
                "<  テスト印字　しない=1 する=9   ﾘﾀｰﾝ  >".
           02  FILLER  PIC  X(021) VALUE
                "全件=1 , 1件=5   ﾘﾀｰﾝ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-CM.
           02  FILLER  PIC  X(030) VALUE
                "(  ｺｰﾄﾞ       )    　終了=ｆ･9".
       01  C-ACP.
           02  A-TST   PIC  9(001).
           02  A-PC    PIC  9(001).
           02  A-TCD   PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NAME  PIC  N(026).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-TCD   PIC  9(004).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999-FHG760" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "349" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "38" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "38" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "38" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "38" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "38" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "38" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "38" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "14" "9" "40" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "16" "18" "21" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "10C-MID" "X" "23" "15" "22" "09C-MID" " " RETURNING RESU.
      *C-CM
       CALL "SD_Init" USING
           "C-CM" " " "0" "0" "30" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CM" "X" "18" "10" "30" " " "C-CM" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-TST" "9" "14" "41" "1" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-TST" BY REFERENCE W-TST "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-PC" "9" "16" "34" "1" "A-TST" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-PC" BY REFERENCE W-PC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-TCD" "9" "18" "18" "4" "A-PC" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "32" "1" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "52" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-NAME" "N" "19" "18" "52" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING
           "D-NAME" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "96" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "96" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "15" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-TCD" "9" "24" "40" "4" "E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-TCD" BY REFERENCE TMS-TCD "4" "0" RETURNING RESU.
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
           CALL "SD_Accept" USING BY REFERENCE A-TST "A-TST" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-TST = 1
               GO TO M-15
           END-IF
           IF  W-TST NOT = 9
               GO TO M-10
           END-IF
      *
           PERFORM TST-RTN THRU TST-EX.
      *
           GO TO M-10.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-PC "A-PC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-PC NOT = 1 AND 5
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
      *
           IF  W-OC = 0
               MOVE 9 TO W-OC
               CALL "PR_Open" RETURNING RESP
           END-IF
           IF  CNT NOT = ZERO
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO TMS-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TMS-F_PNAME1 " " BY REFERENCE TMS-F_IDLST "0".
           IF  W-PC NOT = 5
               GO TO M-55
           END-IF
           CALL "SD_Output" USING "C-CM" C-CM "p" RETURNING RESU.
      *----    一  件　　-----------------------------------------------
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           MOVE ZERO TO CNT.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
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
               GO TO M-25
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-30
           END-IF
      *
           PERFORM MDM-RTN THRU MDM-EX.
           PERFORM MDP-RTN THRU MDP-EX.
           CALL "DB_F_Close" USING
            BY REFERENCE TMS-F_IDLST TMS-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TMS-F_PNAME1 " " BY REFERENCE TMS-F_IDLST "0".
       M-35.
      *           READ TMS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TMS-F_PNAME1 BY REFERENCE TMS-R " " RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               GO TO M-25
           END-IF
           IF  ZERO = TMS-KIN AND TMS-SHZ
               GO TO M-35
           END-IF
           IF  W-TCD NOT = TMS-TCD
               GO TO M-35
           END-IF
           PERFORM KKM-RTN THRU KKM-EX.
           IF  TMS-KBN = 0
               GO TO M-45
           END-IF.
       M-40.
           PERFORM MSM-RTN THRU MSM-EX.
       M-45.
      *           READ TMS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TMS-F_PNAME1 BY REFERENCE TMS-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  ZERO = TMS-KIN AND TMS-SHZ
               GO TO M-45
           END-IF
           IF  W-TCD = TMS-TCD
               GO TO M-40
           END-IF.
       M-50.
           PERFORM KEI-RTN THRU KEI-EX.
           GO TO M-25.
      *----    Ａ　Ｌ　Ｌ    -------------------------------------------
       M-55.
      *           READ TMS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TMS-F_PNAME1 BY REFERENCE TMS-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  ZERO = TMS-KIN AND TMS-SHZ
               GO TO M-55
           END-IF.
       M-60.
           MOVE ZERO TO W-DATE CNT.
           MOVE TMS-TCD TO W-TCD.
           MOVE TMS-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               INITIALIZE T-R
           END-IF
           PERFORM MDM-RTN THRU MDM-EX.
           PERFORM MDP-RTN THRU MDP-EX.
           PERFORM KKM-RTN THRU KKM-EX.
           IF  TMS-KBN = 0
               GO TO M-70
           END-IF.
       M-65.
           PERFORM MSM-RTN THRU MSM-EX.
       M-70.
      *           READ TMS-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TMS-F_PNAME1 BY REFERENCE TMS-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  ZERO = TMS-KIN AND TMS-SHZ
               GO TO M-70
           END-IF
           IF  W-TCD NOT = TMS-TCD
               GO TO M-75
           END-IF
           GO TO M-65.
       M-75.
           PERFORM KEI-RTN THRU KEI-EX.
           GO TO M-60.
       M-85.
           PERFORM KEI-RTN THRU KEI-EX.
      *-----------------------------------------------------------------
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TMS-F_IDLST TMS-F_PNAME1.
       M-95.
           IF  W-OC = 9
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *-----------------------------------------------------------------
       TST-RTN.
           IF  W-OC = 0
               MOVE 9 TO W-OC
               MOVE SPACE TO W-P1 W-P2 W-P3 W-P5
               MOVE SPACE TO P-MSM
               MOVE "　９－９" TO P-DNO
               MOVE "　〒" TO P-UM
               MOVE "TEL" TO P-TM
               MOVE "FAX" TO P-FM
               MOVE ALL "X" TO P-UNO P-TEL P-FAX
               MOVE 9999 TO P-TCD
               MOVE ALL "Ｘ" TO P-NAME P-JSU P-JSS P-MSM
               MOVE 999999999 TO P-UK P-NK P-UZ P-UZG
               MOVE 9999999 TO P-UKZ P-UZZ
               CALL "PR_Open" RETURNING RESP
           END-IF
      *
           IF  CNT = ZERO
               MOVE SPACE TO SP-R
               MOVE HEAD1 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD2 TO SP-R
               CALL "PR_LineFeed" USING "3" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE W-P1 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE W-P2 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE W-P3 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD8 TO SP-R
               CALL "PR_LineFeed" USING "3" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P5 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD 1 TO CNT.
           IF  CNT = 11
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               MOVE ZERO TO CNT
           END-IF.
       TST-EX.
           EXIT.
       MDM-RTN.
           MOVE SPACE TO W-P1 W-P2 W-P3.
           MOVE SPACE TO P-DNO P-UM P-JSU P-NAME P-JSS.
           MOVE SPACE TO W-DNO.
           MOVE T-TNC1 TO W-TNC1.
           MOVE "－" TO W-V.
           MOVE T-DCC TO W-DCC.
           MOVE W-DNO TO P-DNO.
           MOVE "　〒" TO P-UM.
           MOVE "TEL" TO P-TM.
           MOVE "FAX" TO P-FM.
           MOVE T-TCD TO P-TCD.
           MOVE T-NAME TO P-NAME.
           MOVE "<" TO P-F.
           MOVE ">" TO P-R.
           MOVE T-BC TO P-BMC.
           MOVE T-JSU TO P-JSU.
           MOVE T-JSS TO P-JSS.
           MOVE T-UNO TO P-UNO.
           MOVE T-TEL TO P-TEL.
           MOVE T-FAX TO P-FAX.
           MOVE ZERO TO W-UZ W-UZZ W-UZG W-TD.
      *
       MDM-EX.
           EXIT.
       MDP-RTN.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD8 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MDP-EX.
           EXIT.
       KKM-RTN.
           MOVE SPACE TO W-P4 W-P5.
           MOVE SPACE TO P-DNM P-MSM.
           IF (TMS-KBN NOT = 0) OR (CNT = 11)
               GO TO KKM-020
           END-IF
           MOVE TMS-NEN TO WP-NEN.
           MOVE TMS-GET TO WP-GET.
           MOVE TMS-PEY TO WP-PEY.
           MOVE WP-NEN TO W-NEN.
           MOVE WP-GET TO W-GET.
           MOVE WP-PEY TO W-PEY.
           MOVE W-DNM TO P-DNM.
           MOVE "　　　　　　繰越" TO P-MSM.
           MOVE TMS-KIN TO W-UZ.
           MOVE TMS-SHZ TO W-UZZ.
           COMPUTE W-UZG = W-UZ + W-UZZ.
           MOVE W-UZ TO P-UZ.
           MOVE W-UZZ TO P-UZZ.
           MOVE W-UZG TO P-UZG.
       KKM-020.
           MOVE SPACE TO SP-R.
           MOVE W-P4 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE 1 TO CNT.
       KKM-EX.
           EXIT.
       MSM-RTN.
           MOVE SPACE TO W-P4 W-P5.
           MOVE SPACE TO P-DNM P-MSM.
           IF  TMS-DATE NOT = W-DATE
               MOVE TMS-DATE TO W-DATE
               MOVE TMS-NEN TO WP-NEN
               MOVE TMS-GET TO WP-GET
               MOVE TMS-PEY TO WP-PEY
               MOVE WP-NEN TO W-NEN
               MOVE WP-GET TO W-GET
               MOVE WP-PEY TO W-PEY
               MOVE W-DNM TO P-DNM
           END-IF
           IF  TMS-KBN NOT = 1
               GO TO MSM-040
           END-IF
           MOVE "　　　　　　まで" TO P-MSM.
           ADD TMS-KIN TO W-TUK W-UZ.
           ADD TMS-SHZ TO W-TUKZ W-UZZ.
           COMPUTE W-UZG = W-UZ + W-UZZ.
           MOVE TMS-KIN TO P-UK.
           MOVE TMS-SHZ TO P-UKZ.
           MOVE W-UZ TO P-UZ.
           MOVE W-UZZ TO P-UZZ.
           MOVE W-UZG TO P-UZG.
           GO TO MSM-100.
       MSM-040.
           IF  TMS-KBN NOT = 2
               GO TO MSM-060
           END-IF
           MOVE "　　　　　　入金" TO P-MSM.
           SUBTRACT TMS-KIN FROM W-UZ.
           SUBTRACT TMS-SHZ FROM W-UZZ.
           COMPUTE W-NK = TMS-KIN + TMS-SHZ.
           COMPUTE W-UZG = W-UZ + W-UZZ.
           MOVE W-NK TO P-NK.
           MOVE W-UZ TO P-UZ.
           MOVE W-UZZ TO P-UZZ.
           MOVE W-UZG TO P-UZG.
           ADD W-NK TO W-TNK.
           GO TO MSM-100.
       MSM-060.
           MOVE "　　　　　　請求" TO P-MSM.
           COMPUTE W-UZG = TMS-KIN + TMS-SHZ.
           MOVE TMS-KIN TO P-UZ.
           MOVE TMS-SHZ TO P-UZZ.
           MOVE W-UZG TO P-UZG.
       MSM-100.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               GO TO MSM-120
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
           MOVE ZERO TO W-DATE CNT.
           PERFORM MDP-RTN THRU MDP-EX.
           PERFORM KKM-RTN THRU KKM-EX.
           MOVE TMS-NEN TO WP-NEN.
           MOVE TMS-GET TO WP-GET.
           MOVE TMS-PEY TO WP-PEY.
           MOVE WP-NEN TO W-NEN.
           MOVE WP-GET TO W-GET.
           MOVE WP-PEY TO W-PEY.
           MOVE W-DNM TO P-DNM.
           IF  TMS-KBN = 1
               SUBTRACT TMS-KIN FROM W-TUK W-UZ
               SUBTRACT TMS-SHZ FROM W-TUKZ W-UZZ
           ELSE
               IF  TMS-KBN = 2
                   ADD TMS-KIN TO W-UZ
                   ADD TMS-SHZ TO W-UZZ
                   SUBTRACT TMS-KIN FROM W-TNK
                   SUBTRACT TMS-SHZ FROM W-TNK
               END-IF
           END-IF
           GO TO MSM-RTN.
       MSM-120.
           MOVE SPACE TO SP-R.
           MOVE W-P4 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MSM-EX.
           EXIT.
       KEI-RTN.
           IF  ZERO = W-TUK AND W-TUKZ AND W-TNK
               GO TO KEI-020
           END-IF
           MOVE SPACE TO W-P5.
           MOVE SPACE TO P-MSM.
           MOVE "合　　　計　　　" TO P-MSM.
           MOVE W-TUK TO P-UK.
           MOVE W-TUKZ TO P-UKZ.
           MOVE W-TNK TO P-NK.
           COMPUTE CNT = 22 - (2 * CNT).
           IF  CNT = ZERO
               MOVE 22 TO CNT
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P5 TO SP-R.
           CALL "PR_LineFeed" USING CNT RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       KEI-020.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       KEI-EX.
           EXIT.

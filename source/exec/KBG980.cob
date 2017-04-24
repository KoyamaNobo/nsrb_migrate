       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG980.
      **************************************************
      *    購買関係未変換チェック                      *
      *    JS-SIGN  :  0=買掛支払未変換リスト          *
      *             :  1=月末締め未変換チェック        *
      **************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  N(021) VALUE
                "＊＊＊　　買掛支払　未変換リスト　　＊＊＊".
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "日　　付".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "種類".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "手形他№".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                "仕　　入　　先　　名".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  N(004) VALUE "材　　料".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "製品仕入".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
       01  W-P.
           02  P-NEN          PIC  9(002).
           02  P-V1           PIC  X(001).
           02  P-GET          PIC Z9.
           02  P-V2           PIC  X(001).
           02  P-PEY          PIC Z9.
           02  F              PIC  X(002).
           02  P-KBN          PIC  9(002).
           02  F              PIC  X(002).
           02  P-NO           PIC  9(004).
           02  F              PIC  X(002).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(024).
           02  P-ZR           PIC ---,---,--9.
           02  P-ZSHZ         PIC --,---,--9.
           02  P-SS           PIC ---,---,--9.
           02  P-SSHZ         PIC --,---,--9.
           02  P-HC           PIC  N(002).
       01  W-DATA.
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-GET        PIC  9(002).
           02  W-NGD.
             03  W-NEND       PIC  9(004).
             03  W-NENL  REDEFINES W-NEND.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GETD       PIC  9(002).
           02  W-NGL   REDEFINES W-NGD.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  W-ACT          PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LISM.
           COPY LIHSSF.
           COPY LSPF.
      *FD  TDT-M
       01  TDT-M_KBG980.
           02  TDT-M_PNAME1   PIC  X(004) VALUE "TDTM".
           02  F              PIC  X(001).
           02  TDT-M_LNAME    PIC  X(012) VALUE "TDT-M_KBG980".
           02  F              PIC  X(001).
           02  TDT-M_KEY1     PIC  X(100) VALUE SPACE.
           02  TDT-M_SORT     PIC  X(100) VALUE SPACE.
           02  TDT-M_IDLST    PIC  X(100) VALUE SPACE.
           02  TDT-M_RES      USAGE  POINTER.
       01  TDT-R.
           02  TD-KEY.
             03  TD-KBN       PIC  X(002).
             03  TD-NO        PIC  X(004).
           02  TD-TCD         PIC  9(004).
           02  TD-DAT         PIC  9(006).
           02  TD-NGP   REDEFINES TD-DAT.
             03  TD-NEN       PIC  9(002).
             03  TD-GET       PIC  9(002).
             03  TD-PEY       PIC  9(002).
           02  TD-MAN         PIC  9(006).
           02  TD-KIN         PIC S9(010).
           02  TD-BK          PIC  9(004).
           02  TD-HAC         PIC  N(024).
           02  TD-ZR          PIC S9(008).
           02  TD-SS          PIC S9(008).
           02  TD-SB          PIC  9(008).
           02  TD-GC          PIC  9(008).
           02  TD-SZ          PIC  9(008).
           02  TD-EG          PIC  9(008).
           02  TD-ST          PIC  9(008).
           02  TD-ZSHZ        PIC S9(007).
           02  TD-SSHZ        PIC S9(007).
           02  F              PIC  X(006).
           02  TD-SNEN.
             03  TD-SNEN1     PIC  9(002).
             03  TD-SNEN2     PIC  9(002).
           02  TD-HCR         PIC  9(001).
           02  TD-HCT         PIC  9(001).
           02  TD-HCK         PIC  9(001).
           02  TD-HCZ         PIC  9(001).
           02  TD-PCHK        PIC  9(001).
           02  TD-RSC         PIC  9(001).
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
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID0.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　買掛支払　未変換リスト　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(027) VALUE
                "全件作表=1  未変換分=5 ... ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　購買月末締め　未変換チェック　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME11  PIC  N(013) VALUE
                  "製品仕入　未変換データ有り".
             03  E-ME12  PIC  N(016) VALUE
                  "製品仕入・支払　未変換データ有り".
             03  E-ME13  PIC  N(011) VALUE
                  "支払　未変換データ有り".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID0
       CALL "SD_Init" USING 
            "C-MID0" " " "0" "0" "343" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID0" "N" "3" "10" "42" " " "C-MID0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID0" "N" "4" "10" "42" "01C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID0" "N" "5" "10" "42" "02C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID0" "N" "6" "10" "42" "03C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID0" "N" "7" "10" "42" "04C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID0" "N" "8" "10" "42" "05C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID0" "N" "9" "10" "42" "06C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID0" "X" "13" "17" "27" "07C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID0" "X" "20" "25" "22" "08C-MID0" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "336" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "N" "3" "10" "48" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID1" "N" "4" "10" "48" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID1" "N" "5" "10" "48" "02C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID1" "N" "6" "10" "48" "03C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID1" "N" "7" "10" "48" "04C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID1" "N" "8" "10" "48" "05C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID1" "N" "9" "10" "48" "06C-MID1" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "13" "43" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "42" "1" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "159" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "159" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "N" "24" "15" "26" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "N" "24" "15" "32" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "N" "24" "15" "22" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Output" USING "C-MID0" C-MID0 "p" RETURNING RESU
               ACCEPT H-DATE FROM DATE
           ELSE
               CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU
           END-IF
           MOVE ZERO TO W-NGD.
           COPY LIBCPR.
           MOVE D-NBNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEND
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEND
           END-IF
           IF  JS-SIGN = 1
               GO TO M-50
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-ACT NOT = 1 AND 5
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-15
           END-IF
           IF  W-ACT = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" TDT-M_PNAME1 "SHARED" BY REFERENCE TDT-M_IDLST "1"
            "TD-KEY" BY REFERENCE TD-KEY.
       M-20.
      *           READ TDT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDT-M_PNAME1 BY REFERENCE TDT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE TDT-M_IDLST TDT-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  TD-KBN < 20
               GO TO M-20
           END-IF
           IF  ZERO = TD-ZR AND TD-SS AND TD-ZSHZ AND TD-SSHZ
               GO TO M-20
           END-IF
           IF  W-ACT = 5
               IF  TD-HCK NOT = 0
                   GO TO M-20
               END-IF
           END-IF
           MOVE TD-SNEN TO W-NEN.
           MOVE TD-GET TO W-GET.
           IF  W-NG > W-NGD
               GO TO M-20
           END-IF
      *
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
       M-25.
           MOVE TD-TCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　　仕入先　なし　　＊＊" TO S-NAME
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA P-HC.
           MOVE TD-SNEN2 TO P-NEN.
           MOVE TD-GET TO P-GET.
           MOVE TD-PEY TO P-PEY.
           MOVE "/" TO P-V1 P-V2.
           MOVE TD-KBN TO P-KBN.
           MOVE TD-NO TO P-NO.
           MOVE TD-TCD TO P-TCD.
           MOVE S-NAME TO P-TNA.
           MOVE TD-ZR TO P-ZR.
           MOVE TD-ZSHZ TO P-ZSHZ.
           MOVE TD-SS TO P-SS.
           MOVE TD-SSHZ TO P-SSHZ.
           IF  TD-HCK = 0
               MOVE "　未" TO P-HC
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       M-30.
      *           READ TDT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDT-M_PNAME1 BY REFERENCE TDT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  TD-KBN < 20
               GO TO M-30
           END-IF
           IF  ZERO = TD-ZR AND TD-SS AND TD-ZSHZ AND TD-SSHZ
               GO TO M-30
           END-IF
           IF  W-ACT = 5
               IF  TD-HCK NOT = 0
                   GO TO M-30
               END-IF
           END-IF
           MOVE TD-SNEN TO W-NEN.
           MOVE TD-GET TO W-GET.
           IF  W-NG > W-NGD
               GO TO M-30
           END-IF
           GO TO M-25.
       M-50.
           MOVE ZERO TO CHK.
           CALL "DB_F_Open" USING
            "INPUT" TDT-M_PNAME1 "SHARED" BY REFERENCE TDT-M_IDLST "1"
            "TD-KEY" BY REFERENCE TD-KEY.
       M-55.
      *           READ TDT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDT-M_PNAME1 BY REFERENCE TDT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-60
           END-IF
           IF  TD-KBN < 20
               GO TO M-55
           END-IF
           IF  ZERO = TD-ZR AND TD-SS AND TD-ZSHZ AND TD-SSHZ
               GO TO M-55
           END-IF
           IF  TD-HCK NOT = 0
               GO TO M-55
           END-IF
           MOVE TD-SNEN TO W-NEN.
           MOVE TD-GET TO W-GET.
           IF  W-NG NOT = W-NGD
               GO TO M-55
           END-IF
           MOVE 1 TO CHK1.
       M-60.
           CALL "DB_F_Close" USING
            BY REFERENCE TDT-M_IDLST TDT-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HSS-F_PNAME1 "SHARED" BY REFERENCE HSS-F_IDLST "1"
            "HSS-KEY" BY REFERENCE HSS-KEY.
       M-65.
      *           READ HSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSS-F_PNAME1 BY REFERENCE HSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-70
           END-IF
           IF  HSS-BHC NOT = 0
               GO TO M-65
           END-IF
           IF  HSS-NG NOT = W-NGD
               GO TO M-65
           END-IF
           MOVE 1 TO CHK2.
       M-70.
           CALL "DB_F_Close" USING
            BY REFERENCE HSS-F_IDLST HSS-F_PNAME1.
      *
           IF (CHK1 = 0) AND (CHK2 = 1)
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           IF (CHK1 = 1) AND (CHK2 = 1)
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           IF (CHK1 = 1) AND (CHK2 = 0)
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-95.
       M-90.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDT-M_IDLST TDT-M_PNAME1.
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
       S-15.
           EXIT.

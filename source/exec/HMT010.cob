       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMT010.
      *********************************************************
      *    PROGRAM         :  得意先別　預り在庫問合せ　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHT01                          *
      *        変更　　　  :  62/05/11                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(040) VALUE SPACE.
           02  F              PIC  N(020) VALUE
                "＊＊＊　　取引先別　預り残高表　　＊＊＊".
           02  F              PIC  X(022) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(019) VALUE "ｺｰﾄﾞ 得　意　先　名".
           02  F              PIC  X(042) VALUE SPACE.
           02  F              PIC  X(014) VALUE "ｺｰﾄﾞ  品　　名".
           02  F              PIC  X(043) VALUE SPACE.
           02  F              PIC  N(003) VALUE "預り数".
       01  W-P.
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(003).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-AZS          PIC ----,--9.
       01  W-D.
           02  W-TD.
             03  W-HCD        PIC  9(006).
             03  W-HNA        PIC  N(024).
             03  W-AZS        PIC S9(005).
           02 W-AA.
             03  W-AAD   OCCURS 100  PIC  X(059).
           02  W-TCD          PIC  9(004).
           02  CNT            PIC  9(003).
           02  W-DC           PIC  9(003).
           02  W-L            PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-PC           PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-DATE         PIC  9(006).
           02  W-DATED REDEFINES W-DATE.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-GOK          PIC S9(005).
           02  O-HCD          PIC  9(006).
           02  O-HNA          PIC  N(024).
           02  O-TCD          PIC  9(004).
           02  O-TNA          PIC  N(026).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LSPF.
      *FD  TAZ-M
       01  TAZ-M_HMT010.
           02  TAZ-M_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TAZ-M_LNAME    PIC  X(012) VALUE "TAZ-M_HMT010".
           02  F              PIC  X(001).
           02  TAZ-M_KEY1     PIC  X(100) VALUE SPACE.
           02  TAZ-M_SORT     PIC  X(100) VALUE SPACE.
           02  TAZ-M_IDLST    PIC  X(100) VALUE SPACE.
           02  TAZ-M_RES      USAGE  POINTER.
       01  TAZ-R.
           02  TAZ-KEY.
             03  TAZ-TCD      PIC  9(004).
             03  TAZ-HCD      PIC  9(006).
           02  F              PIC  X(010).
           02  TAZ-ZSU        PIC S9(005).
           02  TAZ-AZS        PIC S9(005).
           02  F              PIC  X(006).
           02  TAZ-NG         PIC  9(006).
           02  F              PIC  X(022).
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
                "＊＊＊　　取引先別　預り残高問合せ　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ACP.
           02  A-TCD   PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-GET   PIC  Z(002).
             03  D-PEY   PIC  Z(002).
           02  D-TNA   PIC  N(026).
           02  FILLER.
             03  D-HCD   PIC  9(006).
             03  D-HNA   PIC  N(024).
             03  D-AZS   PIC ----,--9 .
             03  D-HNK   PIC  N(005)    VALUE
                  "【品名計】".
             03  D-GOK   PIC ----,--9 .
             03  D-OHCD  PIC  9(006).
             03  D-OHNA  PIC  N(024).
           02  FILLER.
             03  D-NM    PIC  X(040) VALUE
                  "<   N E X T  P A G E   O K  ?       >   ".
             03  D-PM    PIC  X(040) VALUE
                  "　　< 　打出しする=5 しない=0   ﾘﾀｰﾝ   >".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  ｱｽﾞｶﾘ ﾅｼ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  DATEM ﾅｼ  ***".
             03  E-ME4   PIC  X(023) VALUE
                  "***  DATA ｵｰﾊﾞｰﾌﾛｰ  ***".
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
           "C-MID" " " "0" "0" "308" " " " " RETURNING RESU.
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
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-TCD" "9" "2" "9" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "53" "1" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "270" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-DSP" " " "1" "0" "4" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "D-GET" "Z" "1" "63" "2" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING
           "D-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-PEY" "Z" "1" "67" "2" "D-GET" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-TNA" "N" "2" "23" "52" "01C-DSP" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-TNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-DSP" " " "W-L" "0" "134" "D-TNA" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-HCD" "9" "W-L" "7" "6" " " "02C-DSP" RETURNING RESU.
       CALL "SD_From" USING
           "D-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-HNA" "N" "W-L" "14" "48" "D-HCD" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-HNA" BY REFERENCE W-HNA "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-AZS" "----,--9" "W-L" "63" "8" "D-HNA" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-AZS" BY REFERENCE W-AZS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-HNK" "N" "W-L" "53" "10" "D-AZS" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-GOK" "----,--9" "W-L" "63" "8" "D-HNK" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-GOK" BY REFERENCE W-GOK "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-OHCD" "9" "W-L" "7" "6" "D-GOK" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-OHCD" BY REFERENCE O-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-OHNA" "N" "W-L" "14" "48" "D-OHCD" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-OHNA" BY REFERENCE O-HNA "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03C-DSP" " " "23" "0" "80" "02C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-NM" "X" "23" "22" "40" " " "03C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "D-PM" "X" "23" "22" "40" "D-NM" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "74" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "74" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "15" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME4" "X" "24" "15" "23" "E-ME3" " " RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO TAZ-M_PNAME1.
           MOVE ZERO TO W-DATE.
           COPY LIBCPR.
           MOVE D-HSD TO W-DATE.
           MOVE DATE-02R TO H-DATE.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TAZ-M_PNAME1 " " BY REFERENCE TAZ-M_IDLST "0".
           MOVE ZERO TO W-PC.
       M-10.
           CALL "DB_F_Close" USING
            BY REFERENCE TAZ-M_IDLST TAZ-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TAZ-M_PNAME1 " " BY REFERENCE TAZ-M_IDLST "0".
           CALL "SD_Screen_Output" USING "SCHT01" RETURNING RESU.
           CALL "SD_Output" USING "D-GET" D-GET "p" RETURNING RESU.
           CALL "SD_Output" USING "D-PEY" D-PEY "p" RETURNING RESU.
           MOVE ZERO TO W-TD CNT.
       M-15.
           ADD 1 TO CNT.
           IF  CNT NOT = 101
               MOVE ZERO TO W-AAD(CNT)
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
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
               GO TO M-20
           END-IF
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           IF  T-BC NOT = ZERO
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-20
           END-IF
           MOVE ZERO TO W-DC.
       M-25.
      *           READ TAZ-M AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TAZ-M_PNAME1 BY REFERENCE TAZ-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
           IF  TAZ-TCD < W-TCD
               GO TO M-25
           END-IF
           IF  TAZ-TCD > W-TCD
               GO TO M-30
           END-IF
           IF  TAZ-AZS = ZERO
               GO TO M-25
           END-IF
           MOVE ZERO TO W-TD.
           MOVE TAZ-HCD TO W-HCD.
           MOVE TAZ-AZS TO W-AZS.
           MOVE TAZ-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊　マスター　なし　＊　" TO HI-NAME
           END-IF
           MOVE HI-NAME TO W-HNA.
           ADD 1 TO W-DC.
           IF  W-DC < 101
               MOVE W-TD TO W-AAD(W-DC)
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "E-ME4" E-ME4 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
       M-30.
           IF  W-DC = ZERO
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-10
           END-IF
           MOVE ZERO TO CNT.
           MOVE ZERO TO O-HCD W-GOK CHK.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-35.
           ADD 1 TO CNT.
           MOVE W-AAD(CNT) TO W-TD.
           IF  CNT  =  1
               MOVE W-HCD  TO O-HCD
           END-IF
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L NOT = 23
               GO TO M-45
           END-IF.
       M-40.
           CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHT01" RETURNING RESU.
           CALL "SD_Output" USING "D-GET" D-GET "p" RETURNING RESU.
           CALL "SD_Output" USING "D-PEY" D-PEY "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-45.
           IF  W-HCD  NOT  =  O-HCD
               GO TO M-50
           END-IF
           IF  W-L = 4
               CALL "SD_Output" USING
                "D-HCD" D-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-HNA" D-HNA "p" RETURNING RESU
           END-IF
           GO  TO  M-55.
       M-50.
           IF  CHK > 1
               CALL "SD_Output" USING
                "D-HNK" D-HNK "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-GOK" D-GOK "p" RETURNING RESU
               IF  W-L  =  4
                   CALL "SD_Output" USING
                    "D-OHCD" D-OHCD "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-OHNA" D-OHNA "p" RETURNING RESU
               END-IF
           END-IF
           MOVE  ZERO   TO  W-GOK CHK.
           MOVE  W-HCD  TO  O-HCD.
           ADD   1      TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L  =  23
               GO  TO  M-40
           END-IF
           CALL "SD_Output" USING "D-HCD" D-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
       M-55.
           CALL "SD_Output" USING "D-AZS" D-AZS "p" RETURNING RESU.
           ADD  W-AZS   TO  W-GOK.
           IF  CHK = 1
               MOVE 2 TO CHK
           END-IF
           IF  CHK = 0
               MOVE 1 TO CHK
           END-IF
           MOVE W-HCD   TO  O-HCD.
           MOVE W-HNA   TO  O-HNA.
           IF  W-DC NOT = CNT
               GO TO M-35
           END-IF.
       M-60.
           IF  W-L = 23
               CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU
               CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9"
                "1" BY REFERENCE ESTAT RETURNING RESU
               CALL "SD_Screen_Output" USING "SCHT01" RETURNING RESU
               CALL "SD_Output" USING "D-GET" D-GET "p" RETURNING RESU
               CALL "SD_Output" USING "D-PEY" D-PEY "p" RETURNING RESU
               CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU
               CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU
               MOVE 4 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               CALL "SD_Output" USING "D-HCD" D-HCD "p" RETURNING RESU
               CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU
           END-IF
           ADD 1  TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CHK > 1
               CALL "SD_Output" USING
                "D-HNK" D-HNK "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-GOK" D-GOK "p" RETURNING RESU
           END-IF
           MOVE ZERO  TO W-GOK CHK.
           CALL "SD_Output" USING "D-PM" D-PM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-60
           END-IF
           IF  W-DMM = ZERO
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 5
               GO TO M-60
           END-IF
           IF  W-PC = ZERO
               MOVE 5 TO W-PC
               CALL "PR_Open" RETURNING RESP
               PERFORM S-10 THRU S-15
           END-IF
           MOVE ZERO TO CNT.
           MOVE ZERO TO O-HCD W-GOK CHK.
       M-65.
           ADD 1 TO CNT.
           MOVE W-AAD(CNT) TO W-TD.
           MOVE SPACE TO W-P.
           IF  CNT = 1
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
               GO  TO  M-70
           END-IF
           IF  W-HCD   =  O-HCD
               GO  TO  M-75
           END-IF
           PERFORM S-20 THRU S-30.
       M-70.
           MOVE W-HCD TO P-HCD.
           MOVE W-HNA TO P-HNA.
       M-75.
           MOVE W-AZS TO P-AZS.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD  W-AZS    TO  W-GOK.
           IF  CHK = 1
               MOVE 2 TO CHK
           END-IF
           IF  CHK = 0
               MOVE 1 TO CHK
           END-IF
           MOVE W-HCD    TO  O-HCD.
           MOVE W-HNA    TO  O-HNA.
           MOVE W-TCD    TO  O-TCD.
           MOVE T-NAME   TO  O-TNA.
           IF  W-DC NOT = CNT
               GO TO M-65
           END-IF
           PERFORM S-20 THRU S-30.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO M-10.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE TAZ-M_IDLST TAZ-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           IF  W-PC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF
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
       S-20.
           IF  CHK < 2
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               GO TO S-25
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  60
               MOVE  SPACE    TO  W-P
               MOVE  O-TCD    TO  P-TCD
               MOVE  O-TNA    TO  P-TNA
               MOVE  O-HCD    TO  P-HCD
               MOVE  O-HNA    TO  P-HNA
               PERFORM  S-05  THRU  S-15
               MOVE  SPACE    TO  SP-R
               MOVE  W-P      TO  SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE  SPACE    TO  SP-R
           END-IF
           MOVE SPACE TO W-P.
           MOVE  "　　　　　　　　　　【　合　計　】　"  TO  P-HNA.
           MOVE  W-GOK           TO  P-AZS.
           MOVE  SPACE    TO  SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-25.
           MOVE  SPACE    TO  SP-R.
           MOVE  ZERO     TO  W-GOK CHK.
       S-30.
           EXIT.

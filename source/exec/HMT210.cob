       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMT210.
      *********************************************************
      *    PROGRAM         :  品種別　預り在庫　問合せ　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHT21                          *
      *        変更　　　  :  62/05/11                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  PCNT               PIC  9(002) VALUE ZERO.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(003) VALUE "＊＊＊".
           02  F              PIC  X(004) VALUE SPACE.
           02  H-GET          PIC Z9.
           02  F              PIC  N(001) VALUE "月".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-PEY          PIC Z9.
           02  F              PIC  N(001) VALUE "日".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(020) VALUE
                "現在　品種得意先別　預り残高表　　＊＊＊".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PCNT         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(008) VALUE "得　意　先　名　".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　預り数".
       01  W-P.
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  P-AZS          PIC --,---,--9.
       01  W-ARD.
           02  W-RD    OCCURS  98  PIC  X(116).
       01  W-R.
           02  W-HCD          PIC  9(006).
           02  W-R1.
             03  W-HNA        PIC  N(024).
             03  W-TCD        PIC  9(004).
             03  W-TNA        PIC  N(026).
             03  W-AZS        PIC S9(006).
       01  W-DATA.
           02  O-TCD          PIC  9(004).
           02  WT-AZS         PIC S9(006).
           02  W-C            PIC  9(003).
           02  CNT            PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-PC           PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-DATED        PIC  9(006).
           02  W-DATE  REDEFINES W-DATED.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LSPF.
      *FD  WAZ-F
       01  WAZ-F_HMT210.
           02  WAZ-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  WAZ-F_LNAME    PIC  X(012) VALUE "WAZ-F_HMT210".
           02  F              PIC  X(001).
           02  WAZ-F_KEY1     PIC  X(100) VALUE SPACE.
           02  WAZ-F_SORT     PIC  X(100) VALUE SPACE.
           02  WAZ-F_IDLST    PIC  X(100) VALUE SPACE.
           02  WAZ-F_RES      USAGE  POINTER.
       01  TAZ-R.
           02  AZ-KEY.
             03  AZ-TCD       PIC  9(004).
             03  AZ-HCD       PIC  9(006).
           02  F              PIC  X(010).
           02  AZ-ZSU         PIC S9(005).
           02  AZ-AZS         PIC S9(005).
           02  F              PIC  X(006).
           02  AZ-NG          PIC  9(006).
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
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　品種別　預り残高問合せ　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ACP.
           02  A-HCD   PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-GP.
             03  01D-GP  PIC  Z(002).
             03  02D-GP  PIC  Z(002).
           02  D-HNA   PIC  N(024).
           02  FILLER.
             03  D-D.
               04  01D-D   PIC  9(004).
               04  02D-D   PIC  N(026).
             03  D-D2.
               04  01D-D2  PIC ----,--9 .
             03  D-TD.
               04  FILLER  PIC  N(013)   VALUE
                      "＊＊＊　ＴＯＴＡＬ　＊＊＊".
               04  02D-TD  PIC ----,--9 .
           02  FILLER.
             03  D-NM    PIC  X(034) VALUE
                  "      <  NEXT PAGE  >             ".
             03  D-EM    PIC  X(034) VALUE
                  "<  打出しする=5 しない=0   ﾘﾀｰﾝ  >".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME2   PIC  X(021) VALUE
                  "***  PROGRAM ｴﾗｰ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  ｱｽﾞｶﾘ ﾅｼ  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  DATEM ﾅｼ  ***".
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
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "294" " " " " RETURNING RESU.
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
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-HCD" "9" "3" "12" "6" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "53" "1" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "218" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-GP" " " "2" "0" "4" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "01D-GP" "Z" "2" "62" "2" " " "D-GP" RETURNING RESU.
       CALL "SD_From" USING
           "01D-GP" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-GP" "Z" "2" "66" "2" "01D-GP" " " RETURNING RESU.
       CALL "SD_From" USING
           "02D-GP" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-HNA" "N" "3" "19" "48" "D-GP" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "01C-DSP" " " "W-L" "0" "98" "D-HNA" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-D" " " "W-L" "0" "56" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "01D-D" "9" "W-L" "10" "4" " " "D-D" RETURNING RESU.
       CALL "SD_From" USING
           "01D-D" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-D" "N" "W-L" "14" "52" "01D-D" " " RETURNING RESU.
       CALL "SD_From" USING
           "02D-D" BY REFERENCE W-TNA "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-D2" " " "W-L" "0" "8" "D-D" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-D2" "----,--9" "W-L" "68" "8" " " "D-D2" RETURNING RESU.
       CALL "SD_From" USING
           "01D-D2" BY REFERENCE W-AZS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-TD" " " "W-L" "0" "64" "D-D2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-TD" "N" "W-L" "27" "26" " " "D-TD" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-TD" "----,--9" "W-L" "68" "8" "01D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "02D-TD" BY REFERENCE WT-AZS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-DSP" " " "23" "0" "68" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-NM" "X" "23" "27" "34" " " "02C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "D-EM" "X" "23" "27" "34" "D-NM" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "133" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "133" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "16" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "21" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME4" "X" "24" "15" "18" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATED.
           COPY LIBCPR.
           MOVE D-HSD TO W-DATED.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO WAZ-F_PNAME1.
       M-15.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" WAZ-F_PNAME1 "EXCLUSIVE" BY REFERENCE
            WAZ-F_IDLST "0".
           MOVE W-GET TO H-GET.
           MOVE W-PEY TO H-PEY.
           ACCEPT H-DATE FROM DATE.
           MOVE ZERO TO W-PC.
       M-20.
           CALL "DB_F_Close" USING
            BY REFERENCE WAZ-F_IDLST WAZ-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" WAZ-F_PNAME1 "EXCLUSIVE" BY REFERENCE
            WAZ-F_IDLST "0".
           CALL "SD_Screen_Output" USING "SCHT21" RETURNING RESU.
           CALL "SD_Output" USING "D-GP" D-GP "p" RETURNING RESU.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  W-HCD = ZERO
               GO TO M-25
           END-IF
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           MOVE ZERO TO W-C.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE ZERO TO WT-AZS CNT CHK.
           MOVE ZERO TO O-TCD.
       M-30.
           ADD 1 TO CNT.
           IF  CNT NOT = 99
               MOVE ZERO TO W-RD(CNT)
               GO TO M-30
           END-IF
           MOVE ZERO TO CNT.
       M-35.
      *           READ WAZ-F WITH UNLOCK AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" WAZ-F_PNAME1 BY REFERENCE TAZ-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  AZ-HCD > W-HCD
               GO TO M-50
           END-IF
           IF  AZ-HCD NOT = W-HCD
               GO TO M-35
           END-IF
           IF  AZ-AZS = ZERO
               GO TO M-35
           END-IF.
       M-40.
           MOVE 5 TO W-C.
           MOVE AZ-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊　マスター　なし　＊　　　" TO T-NAME
           END-IF
           MOVE T-NAME TO W-TNA.
           MOVE ZERO TO W-R1.
           MOVE HI-NAME TO W-HNA.
           MOVE AZ-TCD TO W-TCD.
           MOVE T-NAME TO W-TNA.
           MOVE AZ-AZS TO W-AZS.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT  =  ZERO
               CALL "SD_Output" USING "D-D" D-D "p" RETURNING RESU
               GO  TO  M-47
           END-IF
           IF  W-L = 23
               PERFORM S-20 THRU S-25
               CALL "SD_Output" USING "D-D" D-D "p" RETURNING RESU
           END-IF
           IF  W-TCD  =  O-TCD
               GO  TO  M-47
           END-IF
           IF  W-L  =  5
               ADD  1    TO  W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
           END-IF
           IF  CHK > 1
               CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU
           END-IF
           MOVE  ZERO  TO  WT-AZS CHK.
           ADD   1     TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 23
               PERFORM S-20 THRU S-25
           END-IF
           CALL "SD_Output" USING "D-D" D-D "p" RETURNING RESU.
       M-47.
           CALL "SD_Output" USING "D-D2" D-D2 "p" RETURNING RESU.
           ADD 1 TO CNT.
           IF  CNT = 99
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE W-TCD TO O-TCD.
           MOVE W-R TO W-RD(CNT).
           ADD AZ-AZS TO WT-AZS.
           IF  CHK = 1
               MOVE 2 TO CHK
           END-IF
           IF  CHK = 0
               MOVE 1 TO CHK
           END-IF
           GO TO M-35.
       M-50.
           IF  W-C = ZERO
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-20
           END-IF
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 23
               PERFORM S-20 THRU S-25
               CALL "SD_Output" USING "D-D" D-D "p" RETURNING RESU
               ADD 1  TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
           END-IF
           IF  CHK > 1
               CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-EM" D-EM "p" RETURNING RESU.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-55
           END-IF
           IF  W-DMM = ZERO
               GO TO M-20
           END-IF
           IF  W-DMM NOT = 5
               GO TO M-55
           END-IF
           MOVE ZERO TO CNT.
           MOVE ZERO TO O-TCD.
           MOVE ZERO TO WT-AZS CHK.
           IF  W-PC = 9
               GO TO M-60
           END-IF
           MOVE 9 TO W-PC.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
       M-60.
           ADD 1 TO CNT.
           IF  CNT = 99
               GO TO M-65
           END-IF
           MOVE ZERO TO W-R.
           MOVE W-RD(CNT) TO W-R.
           IF  W-HCD = ZERO
               GO TO M-65
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-HNA P-TNA.
           IF  CNT = 1
               MOVE W-HCD TO P-HCD
               MOVE W-HNA TO P-HNA
               MOVE W-TCD TO P-TCD
               MOVE W-TNA TO P-TNA
               GO TO M-63
           END-IF
           IF  W-TCD  =  O-TCD
               GO TO M-63
           END-IF
           IF  CHK > 1
               PERFORM S-30 THRU S-35
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE ZERO   TO WT-AZS CHK.
           MOVE W-TCD TO P-TCD.
           MOVE W-TNA TO P-TNA.
       M-63.
           MOVE W-AZS TO P-AZS.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-HCD TO P-HCD
               MOVE W-HNA TO P-HNA
               MOVE W-TCD TO P-TCD
               MOVE W-TNA TO P-TNA
               PERFORM S-05 THRU S-15
           END-IF
           ADD  W-AZS  TO WT-AZS.
           IF  CHK = 1
               MOVE 2 TO CHK
           END-IF
           IF  CHK = 0
               MOVE 1 TO CHK
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-TCD TO O-TCD.
           GO TO M-60.
       M-65.
           IF  CHK > 1
               PERFORM S-30 THRU S-35
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO M-20.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE WAZ-F_IDLST WAZ-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           IF  W-PC = 9
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
           ADD    1   TO PCNT.
           MOVE SPACE TO SP-R.
           MOVE PCNT  TO H-PCNT.
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
           CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHT21" RETURNING RESU.
           CALL "SD_Output" USING "D-GP" D-GP "p" RETURNING RESU.
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       S-25.
           EXIT.
       S-30.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-HNA P-TNA.
           MOVE "　　　　　　　　　　　　　【　合　計　】　" TO P-TNA.
           MOVE WT-AZS TO P-AZS.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-35.
           EXIT.

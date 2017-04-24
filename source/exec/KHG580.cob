       IDENTIFICATION DIVISION.
       PROGRAM-ID.  KHG580.
      *********************************************************
      *    PROGRAM         :  工品受払表　　　　　　　　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/08                        *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=防振他  1=マット他            *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(041) VALUE SPACE.
           02  H-MID          PIC  N(022) VALUE
                "　　　＊＊＊　　工　品　受　払　表　　＊＊＊".
           02  F              PIC  X(012) VALUE SPACE.
           02  F              PIC  X(001) VALUE "'".
           02  H-NEN          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-GET          PIC  Z(002).
           02  F              PIC  N(002) VALUE "月分".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(006) VALUE "品　　　名　".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(002) VALUE "日付".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　加硫数".
           02  F              PIC  N(004) VALUE "　廃却数".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　仕入数".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　出荷数".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　在庫数".
           02  F              PIC  X(003) VALUE " : ".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(006) VALUE "品　　　名　".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(002) VALUE "日付".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　加硫数".
           02  F              PIC  N(004) VALUE "　廃却数".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　仕入数".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　出荷数".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　在庫数".
           02  F              PIC  X(005) VALUE X"1A24212474".
       01  W-P.
           02  W-PD    OCCURS  58.
             03  P-HCD1       PIC  X(005).
             03  F            PIC  X(001).
             03  P-NAME1      PIC  X(019).
             03  F            PIC  X(001).
             03  P-PEY1       PIC Z9.
             03  P-YD11  REDEFINES P-PEY1.
               04  F          PIC  X(001).
               04  P-F11      PIC  X(001).
             03  P-KS1        PIC ----,--9.
             03  P-HS1        PIC  -(006).
             03  P-YD12  REDEFINES P-HS1.
               04  P-R11      PIC  X(001).
               04  F          PIC  X(005).
             03  P-IS1        PIC ----,--9.
             03  P-YD13  REDEFINES P-IS1.
               04  F          PIC  X(007).
               04  P-F12      PIC  X(001).
             03  P-SS1        PIC ----,--9.
             03  P-ZS1        PIC ----,--9.
             03  P-YD14  REDEFINES P-ZS1.
               04  P-R12      PIC  X(001).
               04  F          PIC  X(007).
             03  F            PIC  X(001).
             03  P-X          PIC  X(001).
             03  F            PIC  X(001).
             03  P-HCD2       PIC  X(005).
             03  F            PIC  X(001).
             03  P-NAME2      PIC  X(019).
             03  F            PIC  X(001).
             03  P-PEY2       PIC Z9.
             03  P-YD21  REDEFINES P-PEY2.
               04  F          PIC  X(001).
               04  P-F21      PIC  X(001).
             03  P-KS2        PIC ----,--9.
             03  P-HS2        PIC  -(006).
             03  P-YD22  REDEFINES P-HS2.
               04  P-R21      PIC  X(001).
               04  F          PIC  X(005).
             03  P-IS2        PIC ----,--9.
             03  P-YD23  REDEFINES P-IS2.
               04  F          PIC  X(007).
               04  P-F22      PIC  X(001).
             03  P-SS2        PIC ----,--9.
             03  P-ZS2        PIC ----,--9.
             03  P-YD24  REDEFINES P-ZS2.
               04  P-R22      PIC  X(001).
               04  F          PIC  X(007).
       01  W-DATA.
           02  W-HCD          PIC  X(005).
           02  W-PEY          PIC  9(002).
           02  W-D.
             03  W-KS         PIC S9(006).
             03  W-HS         PIC S9(006).
             03  W-IS         PIC S9(006).
             03  W-SS         PIC S9(006).
             03  W-ZS         PIC S9(006).
           02  W-TD.
             03  W-TKS        PIC S9(006).
             03  W-THS        PIC S9(006).
             03  W-TIS        PIC S9(006).
             03  W-TSS        PIC S9(006).
           02  W-LD           PIC  9(002).
           02  W-CD           PIC  9(001).
           02  CHK            PIC  9(001).
           02  CNT            PIC  9(003).
           02  W-PC           PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-NG.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIKHM.
           COPY LIKHT1.
           COPY LSPF.
      *FD  KUH-F
       01  KUH-F_KHG580.
           02  KUH-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  KUH-F_LNAME    PIC  X(012) VALUE "KUH-F_KHG580".
           02  F              PIC  X(001).
           02  KUH-F_KEY1     PIC  X(100) VALUE SPACE.
           02  KUH-F_SORT     PIC  X(100) VALUE SPACE.
           02  KUH-F_IDLST    PIC  X(100) VALUE SPACE.
           02  KUH-F_RES      USAGE  POINTER.
       01  K-R.
           02  K-HCD          PIC  X(005).
           02  K-NGP.
             03  K-NEN        PIC  9(002).
             03  K-GET        PIC  9(002).
             03  K-PEY        PIC  9(002).
           02  K-KS           PIC S9(006).
           02  K-HS           PIC S9(006).
           02  K-IS           PIC S9(006).
           02  K-SS           PIC S9(006).
           02  K-ZS           PIC S9(006).
           02  K-YC           PIC  9(002).
           02  F              PIC  X(021).
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
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　工　品　受　払　表　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-MID1.
           02  FILLER  PIC  N(006) VALUE
                "（マット他）".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  KHTM ﾅｼ  ***".
             03  E-KEY   PIC  X(005).
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
            "C-MID" " " "0" "0" "266" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "38" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "38" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "38" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "38" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "38" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "38" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "38" "06C-MID" " "  RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "12" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "N" "7" "22" "12" " " "C-MID1"  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "99" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "99" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "40" "5" "E-ME2" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE K-HCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-KEY" " "  RETURNING RESU.
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
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           COPY LIBCPR.
           MOVE DATE-03R TO H-DATE.
           MOVE D-NKNG TO W-NG.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO KUH-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KUH-F_PNAME1 " " BY REFERENCE KUH-F_IDLST "0".
       M-10.
      *           READ KUH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KUH-F_PNAME1 BY REFERENCE K-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KUH-F_IDLST KUH-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  JS-SIGN = 0
               IF  K-YC NOT = 10 AND 11
                   GO TO M-10
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  K-YC = 00 OR 10 OR 11 OR 25
                   GO TO M-10
               END-IF
           END-IF
           IF  ZERO = K-KS AND K-HS AND K-IS AND K-SS AND K-ZS
               GO TO M-10
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KHT-M_PNAME1 "SHARED" BY REFERENCE KHT-M_IDLST "1"
            "KHT-KEY" BY REFERENCE KHT-KEY.
           IF  JS-SIGN = 1
                MOVE "＊＊＊　　マット他　日付別　受払表　　＊＊＊"
                                                            TO H-MID
           END-IF
           MOVE SPACE TO W-P.
           MOVE ZERO TO W-LD W-CD W-PC.
       M-15.
           MOVE K-HCD TO W-HCD.
           MOVE K-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "    **  KHM ﾅｼ  **  " TO KH-NAME
           END-IF
           MOVE ZERO TO W-D W-TD W-PEY CHK CNT.
           MOVE K-HCD TO KHT-KEY.
      *           READ KHT-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KHT-M_PNAME1 BY REFERENCE KHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO KHT-R
           END-IF
           MOVE K-ZS TO W-ZS.
       M-20.
           MOVE K-PEY TO W-PEY.
           MOVE ZERO TO W-KS W-HS W-IS W-SS.
       M-25.
           ADD K-KS TO W-KS.
           ADD K-HS TO W-HS.
           ADD K-IS TO W-IS.
           ADD K-SS TO W-SS.
       M-30.
      *           READ KUH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KUH-F_PNAME1 BY REFERENCE K-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JS-SIGN = 0
               IF  K-YC NOT = 10 AND 11
                   GO TO M-30
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  K-YC = 00 OR 10 OR 11 OR 25
                   GO TO M-30
               END-IF
           END-IF
           IF  ZERO = K-KS AND K-HS AND K-IS AND K-SS AND K-ZS
               GO TO M-30
           END-IF
           IF  K-HCD NOT = W-HCD
               GO TO M-40
           END-IF
           IF  K-PEY NOT = W-PEY
               GO TO M-35
           END-IF
           GO TO M-25.
       M-35.
           PERFORM S-20 THRU S-30.
           GO TO M-20.
       M-40.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-60.
           GO TO M-15.
       M-90.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-60.
           PERFORM S-80 THRU S-90.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE KUH-F_IDLST KUH-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KHT-M_IDLST KHT-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
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
       S-15.
           EXIT.
       S-20.
           PERFORM S-70 THRU S-75.
           IF  W-PEY NOT = ZERO
               ADD 1 TO CNT
           END-IF
           ADD W-KS TO W-TKS.
           ADD W-HS TO W-THS.
           ADD W-IS TO W-TIS.
           ADD W-SS TO W-TSS.
           IF  W-CD = 5
               GO TO S-25
           END-IF
           MOVE ":" TO P-X(W-LD).
           IF  CHK = 0
               MOVE 5 TO CHK
               MOVE W-HCD TO P-HCD1(W-LD)
               MOVE KH-NAME TO P-NAME1(W-LD)
           END-IF
           IF  W-PEY = ZERO
               MOVE W-ZS TO P-ZS1(W-LD)
               GO TO S-30
           END-IF
           MOVE W-PEY TO P-PEY1(W-LD).
           IF  W-KS NOT = ZERO
               MOVE W-KS TO P-KS1(W-LD)
           END-IF
           IF  W-HS NOT = ZERO
               MOVE W-HS TO P-HS1(W-LD)
           END-IF
           IF  W-IS NOT = ZERO
               MOVE W-IS TO P-IS1(W-LD)
           END-IF
           IF  W-SS NOT = ZERO
               MOVE W-SS TO P-SS1(W-LD)
           END-IF
           COMPUTE W-ZS = W-ZS + W-KS - W-HS + W-IS - W-SS.
           MOVE W-ZS TO P-ZS1(W-LD).
           GO TO S-30.
       S-25.
           IF  CHK = 0
               MOVE 5 TO CHK
               MOVE W-HCD TO P-HCD2(W-LD)
               MOVE KH-NAME TO P-NAME2(W-LD)
           END-IF
           IF  W-PEY = ZERO
               MOVE W-ZS TO P-ZS2(W-LD)
               GO TO S-30
           END-IF
           MOVE W-PEY TO P-PEY2(W-LD).
           IF  W-KS NOT = ZERO
               MOVE W-KS TO P-KS2(W-LD)
           END-IF
           IF  W-HS NOT = ZERO
               MOVE W-HS TO P-HS2(W-LD)
           END-IF
           IF  W-IS NOT = ZERO
               MOVE W-IS TO P-IS2(W-LD)
           END-IF
           IF  W-SS NOT = ZERO
               MOVE W-SS TO P-SS2(W-LD)
           END-IF
           COMPUTE W-ZS = W-ZS + W-KS - W-HS + W-IS - W-SS.
           MOVE W-ZS TO P-ZS2(W-LD).
       S-30.
           EXIT.
       S-35.
           IF  CNT < 2
               GO TO S-55
           END-IF
           PERFORM S-70 THRU S-75.
           IF  W-CD NOT = 0
               GO TO S-40
           END-IF
           MOVE ":" TO P-X(W-LD).
           IF  CHK = 0
               MOVE 5 TO CHK
               MOVE W-HCD TO P-HCD1(W-LD)
           END-IF
           MOVE "  ［　合　計　］   " TO P-NAME1(W-LD).
           MOVE "[" TO P-F11(W-LD).
           MOVE "]" TO P-R11(W-LD).
           MOVE "[" TO P-F12(W-LD).
           MOVE "]" TO P-R12(W-LD).
           MOVE W-TKS TO P-KS1(W-LD).
           MOVE W-THS TO P-HS1(W-LD).
           MOVE W-TIS TO P-IS1(W-LD).
           MOVE W-TSS TO P-SS1(W-LD).
           GO TO S-55.
       S-40.
           IF  CHK = 0
               MOVE 5 TO CHK
               MOVE W-HCD TO P-HCD2(W-LD)
           END-IF
           MOVE "  ［　合　計　］   " TO P-NAME2(W-LD).
           MOVE "[" TO P-F21(W-LD).
           MOVE "]" TO P-R21(W-LD).
           MOVE "[" TO P-F22(W-LD).
           MOVE "]" TO P-R22(W-LD).
           MOVE W-TKS TO P-KS2(W-LD).
           MOVE W-THS TO P-HS2(W-LD).
           MOVE W-TIS TO P-IS2(W-LD).
           MOVE W-TSS TO P-SS2(W-LD).
       S-55.
           PERFORM S-70 THRU S-75.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF.
       S-60.
           EXIT.
       S-70.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               GO TO S-75
           END-IF
           IF  W-CD = 0
               MOVE 5 TO W-CD
               MOVE ZERO TO W-LD CHK
               GO TO S-70
           END-IF
           PERFORM S-80 THRU S-90.
           MOVE SPACE TO W-P.
           MOVE ZERO TO W-LD W-CD CHK.
           GO TO S-70.
       S-75.
           EXIT.
       S-80.
           IF  W-PC = 0
               MOVE 5 TO W-PC
               CALL "PR_Open" RETURNING RESP
               MOVE W-NEN TO H-NEN
               MOVE W-GET TO H-GET
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE ZERO TO W-LD.
       S-85.
           ADD 1 TO W-LD.
           IF  W-LD = 59
               GO TO S-90
           END-IF
           IF  P-X(W-LD) = SPACE
               GO TO S-90
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-PD(W-LD) TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO S-85.
       S-90.
           EXIT.

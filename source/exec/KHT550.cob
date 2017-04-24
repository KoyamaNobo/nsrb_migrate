       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHT510.
      *********************************************************
      *    PROGRAM         :  用途区分別　販売実績問合せ　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCKT55                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-PEY.
             03  W-BP         PIC  9(002).
             03  W-MP         PIC  9(002).
           02  CNT            PIC  9(002).
           02  CNTD           PIC  9(002).
           02  W-L            PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-ND.
             03  WN-YC        PIC  9(002).
             03  WN-SSU       PIC S9(006)V9(02).
             03  WN-UKIN      PIC S9(008).
             03  WN-GKIN      PIC S9(008).
             03  WN-AR        PIC S9(008).
           02  W-DD.
             03  W-DDD   OCCURS  42.
               04  WD-YCN     PIC  N(014).
               04  WD-SSU     PIC S9(006)V9(02).
               04  WD-UKIN    PIC S9(008).
               04  WD-GKIN    PIC S9(008).
               04  WD-AR      PIC S9(008).
           02  W-AD.
             03  WA-UKIN      PIC S9(008).
             03  WA-GKIN      PIC S9(008).
             03  WA-AR        PIC S9(008).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIKHT2.
           COPY LIKKBM.
      *FD  URIR-F
       01  URIR-F_KHT550.
           02  URIR-F_PNAME1  PIC  X(005) VALUE "URIRF".
           02  F              PIC  X(001).
           02  URIR-F_LNAME   PIC  X(013) VALUE "URIR-F_KHT550".
           02  F              PIC  X(001).
           02  URIR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  URIR-F_SORT    PIC  X(100) VALUE SPACE.
           02  URIR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  URIR-F_RES     USAGE  POINTER.
       01  URIR-R.
           02  F              PIC  X(007).
           02  UR-PEY         PIC  9(002).
           02  F              PIC  X(033).
           02  UR-YC          PIC  9(002).
           02  F              PIC  X(084).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-PEY.
             03  01D-PEY  PIC  Z(002).
             03  02D-PEY  PIC  Z(002).
           02  D-DATA.
             03  01D-DATA  PIC  N(014) .
             03  02D-DATA  PIC -----,--9.99 .
             03  03D-DATA  PIC ---,---,--9 .
             03  04D-DATA  PIC ---,---,--9 .
             03  05D-DATA  PIC ---,---,--9 .
           02  D-TOTAL.
             03  01D-TOTAL  PIC ---,---,--9 .
             03  02D-TOTAL  PIC ---,---,--9 .
             03  03D-TOTAL  PIC ---,---,--9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-KEY   PIC  X(005).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "26" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "110" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PEY" " " "1" "0" "4" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-PEY" "Z" "1" "63" "2" " " "D-PEY"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-PEY" BY REFERENCE W-BP "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-PEY" "Z" "1" "75" "2" "01D-PEY" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "02D-PEY" BY REFERENCE W-MP "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATA" " " "W-L" "0" "73" "D-PEY" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DATA" "N" "W-L" "2" "28" " " "D-DATA" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DATA" BY REFERENCE WD-YCN(1) "28" "1" BY REFERENCE
             CNT 60 RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DATA" "-----,--9.99" "W-L" "31" "12" "01D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DATA" BY REFERENCE WD-SSU(1) "8" "1" 
            BY REFERENCE CNT 60 RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-DATA" "---,---,--9" "W-L" "44" "11" "02D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-DATA" BY REFERENCE WD-UKIN(1) "8" "1" BY REFERENCE
             CNT 60 RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-DATA" "---,---,--9" "W-L" "56" "11" "03D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-DATA" BY REFERENCE WD-GKIN(1) "8" "1" BY REFERENCE
             CNT 60 RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-DATA" "---,---,--9" "W-L" "68" "11" "04D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-DATA" BY REFERENCE WD-AR(1) "8" "1" BY REFERENCE CNT 60
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TOTAL" " " "23" "0" "33" "D-DATA" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TOTAL" "---,---,--9" "23" "44" "11" " " "D-TOTAL"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-TOTAL" BY REFERENCE WA-UKIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TOTAL" "---,---,--9" "23" "56" "11" "01D-TOTAL" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-TOTAL" BY REFERENCE WA-GKIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-TOTAL" "---,---,--9" "23" "68" "11" "02D-TOTAL" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-TOTAL" BY REFERENCE WA-AR "8" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "50" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "50" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "35" "5" "E-ME3" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE KHT-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-KEY" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCKT55" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" URIR-F_PNAME1 "SHARED" BY REFERENCE
            URIR-F_IDLST "0".
           MOVE ZERO TO W-PEY.
       M-10.
      *           READ URIR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URIR-F_PNAME1 BY REFERENCE URIR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF
           IF  UR-YC NOT = 10 AND 11
               GO TO M-15
           END-IF
           IF  UR-PEY > W-BP
               MOVE UR-PEY TO W-BP
           END-IF
           GO TO M-10.
       M-15.
           IF  UR-PEY > W-MP
               MOVE UR-PEY TO W-MP
           END-IF
           GO TO M-10.
       M-20.
           CALL "DB_F_Close" USING
            BY REFERENCE URIR-F_IDLST URIR-F_PNAME1.
           CALL "SD_Output" USING "D-PEY" D-PEY "p" RETURNING RESU.
           MOVE ZERO TO CNT.
           CALL "DB_F_Open" USING
            "INPUT" KHT-M_PNAME1 "SHARED" BY REFERENCE KHT-M_IDLST "1"
            "KHT-KEYD" BY REFERENCE KHT-KEYD.
           CALL "DB_F_Open" USING
            "INPUT" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
       M-25.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  KHT-YC = ZERO
               GO TO M-25
           END-IF
           MOVE ZERO TO W-AD W-DD.
       M-30.
           MOVE ZERO TO W-ND.
           MOVE KHT-YC TO WN-YC.
           MOVE SPACE TO KKB-KEY.
           MOVE 01 TO KKB-NO.
           MOVE WN-YC TO KKB-YC.
      *           READ KKB-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO KKB-YCN
           END-IF.
       M-35.
           ADD KHT-SSU TO WN-SSU.
           ADD KHT-UKIN TO WN-UKIN WA-UKIN.
           SUBTRACT KHT-NKIN FROM WN-UKIN WA-UKIN.
           ADD KHT-GKIN TO WN-GKIN WA-GKIN.
       M-40.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF
           IF  KHT-YC = ZERO
               GO TO M-40
           END-IF
           IF  KHT-YC = WN-YC
               GO TO M-35
           END-IF
           IF  ZERO = WN-SSU AND WN-UKIN AND WN-GKIN
               GO TO M-30
           END-IF
           COMPUTE WN-AR = WN-UKIN - WN-GKIN.
           ADD 1 TO CNT.
           IF  CNT > 40
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE KKB-YCN TO WD-YCN(CNT).
           MOVE WN-SSU TO WD-SSU(CNT).
           MOVE WN-UKIN TO WD-UKIN(CNT).
           MOVE WN-GKIN TO WD-GKIN(CNT).
           MOVE WN-AR TO WD-AR(CNT).
           GO TO M-30.
       M-45.
           IF  ZERO = WN-SSU AND WN-UKIN AND WN-GKIN
               GO TO M-50
           END-IF
           COMPUTE WN-AR = WN-UKIN - WN-GKIN.
           ADD 1 TO CNT.
           IF  CNT > 40
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE KKB-YCN TO WD-YCN(CNT).
           MOVE WN-SSU TO WD-SSU(CNT).
           MOVE WN-UKIN TO WD-UKIN(CNT).
           MOVE WN-GKIN TO WD-GKIN(CNT).
           MOVE WN-AR TO WD-AR(CNT).
       M-50.
           CALL "DB_F_Close" USING
            BY REFERENCE KHT-M_IDLST KHT-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
      *
           COMPUTE WA-AR = WA-UKIN - WA-GKIN.
           COMPUTE CNTD = CNT + 1.
           PERFORM S-05 THRU S-20.
           GO TO M-95.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE KHT-M_IDLST KHT-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE 2 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE ZERO TO CNT.
       S-10.
           ADD 1 TO CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT > 40
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-20
           END-IF
           IF  CNT = CNTD
               CALL "SD_Output" USING
                "D-TOTAL" D-TOTAL "p" RETURNING RESU
               GO TO S-15
           END-IF
           IF  CNT NOT = 21
               CALL "SD_Output" USING
                "D-DATA" D-DATA "p" RETURNING RESU
               GO TO S-10
           END-IF.
       S-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO S-20
           END-IF
           IF  CNT = CNTD
               GO TO S-20
           END-IF
           IF  ESTAT = PF5
               IF  CNT = 21
                   CALL "SD_Output" USING
                    "C-CLEAR" C-CLEAR "p" RETURNING RESU
                   CALL "SD_Screen_Output" USING "SCKT55" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-PEY" D-PEY "p" RETURNING RESU
                   MOVE 3 TO W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
                   CALL "SD_Output" USING
                    "D-DATA" D-DATA "p" RETURNING RESU
                   GO TO S-10
               END-IF
           END-IF
           IF  ESTAT = PF6
               IF  CNT > 21
                   CALL "SD_Output" USING
                    "C-CLEAR" C-CLEAR "p" RETURNING RESU
                   CALL "SD_Screen_Output" USING "SCKT55" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-PEY" D-PEY "p" RETURNING RESU
                   GO TO S-05
               END-IF
           END-IF
           GO TO S-15.
       S-20.
           EXIT.

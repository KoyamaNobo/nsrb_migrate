       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMK070.
      *********************************************************
      *    PROGRAM         :  教育出荷集計　追加入力　　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHK07                          *
      *        変更　　　  :  62/04/15                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-D.
           02  W-TCD          PIC  9(004).
           02  W-HCD1         PIC  9(004).
           02  W-SU           PIC S9(006).
           02  W-KIN          PIC S9(008).
           02  W-ADR          PIC  9(001).
           02  W-T            PIC  9(005).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-HNAD.
             03  W-HNA   OCCURS  24  PIC  N(001).
           02  W-NAD.
             03  W-NA    OCCURS  24  PIC  N(001).
           02  W-HNAME REDEFINES W-NAD   PIC  N(024).
           02  CNT            PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-BC.
             03  W-BC1        PIC  9(001).
             03  W-BC2        PIC  9(001).
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NEND  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGD   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LIHKBM.
      *FD  HKS-F
       01  HKS-F_HMK070.
           02  HKS-F_PNAME1   PIC  X(005) VALUE "HKSRF".
           02  F              PIC  X(001).
           02  HKS-F_LNAME    PIC  X(012) VALUE "HKS-F_HMK070".
           02  F              PIC  X(001).
           02  HKS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HKS-F_SORT     PIC  X(100) VALUE SPACE.
           02  HKS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HKS-F_RES      USAGE  POINTER.
       01  HKS-R.
           02  K-TCD          PIC  9(004).
           02  K-HCD1         PIC  9(004).
           02  K-SU           PIC S9(006).
           02  K-KIN          PIC S9(008).
           02  K-ADR          PIC  9(001).
           02  K-BC           PIC  9(001).
           02  F              PIC  X(002).
           02  K-NG           PIC  9(006).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  FILLER.
             03  A-NEN   PIC  9(002).
             03  A-GET   PIC  9(002).
           02  A-TCD   PIC  9(004).
           02  A-HCD   PIC  9(004).
           02  A-SU    PIC S9(006).
           02  A-T     PIC  9(005).
           02  A-KIN   PIC S9(008).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-TNA   PIC  N(024).
           02  D-HNA   PIC  N(024).
           02  D-SU    PIC ZZZZZ9- .
           02  D-T     PIC ZZZZ9 .
           02  D-KIN   PIC ZZZZZZZ9- .
       01  C-SPC.
           02  S-SU    PIC  X(007) VALUE
                "       ".
           02  S-T     PIC  X(5) VALUE "     ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME2   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME3   PIC  X(024) VALUE
                  "***  ｷｮｳｲｸ ｸﾌﾞﾝ ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "32" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "3" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "3" "64" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "3" "69" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "5" "11" "4" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "6" "11" "4" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SU" "S9" "8" "27" "6" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SU" BY REFERENCE W-SU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-T" "9" "9" "28" "5" "A-SU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-T" BY REFERENCE W-T "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KIN" "S9" "10" "25" "8" "A-T" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KIN" BY REFERENCE W-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "15" "41" "1" "A-KIN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "117" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "5" "25" "48" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNA" "N" "6" "25" "48" "D-TNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-HNA" BY REFERENCE W-HNAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU" "ZZZZZ9-" "8" "27" "7" "D-HNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SU" BY REFERENCE W-SU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-T" "ZZZZ9" "9" "28" "5" "D-SU" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-T" BY REFERENCE W-T "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN" "ZZZZZZZ9-" "10" "25" "9" "D-T" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-KIN" BY REFERENCE W-KIN "8" "0" RETURNING RESU.
      *C-SPC
       CALL "SD_Init" USING 
            "C-SPC" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-SU" "X" "8" "27" "7" " " "C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-T" "X" "9" "28" "5" "S-SU" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "117" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "117" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "16" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "24" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           MOVE ZERO TO W-NG.
           MOVE D-NING TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" HKS-F_PNAME1 " " BY REFERENCE HKS-F_IDLST "0".
           MOVE ZERO TO W-D.
       M-25.
           CALL "SD_Screen_Output" USING "SCHK07" RETURNING RESU.
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU.
           IF  W-TCD NOT = ZERO
               CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU
               CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU
           END-IF
           MOVE ZERO TO W-SU W-T W-KIN.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
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
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE ZERO TO T-FKC
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE 01 TO HKB-NO.
           MOVE T-FKC TO HKB-TDFK.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 0 TO HKB-KTKCD
           END-IF
           MOVE HKB-KTKCD TO W-ADR.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
      *
           MOVE ZERO TO HI-KEY.
           MOVE W-HCD1 TO HI-HCD1.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-30
           END-IF
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-30
           END-IF
           IF  W-HCD1 NOT = HI-HCD1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-30
           END-IF
           IF  W-HCD1 = 9999
               GO TO M-35
           END-IF
           IF  HI-BC3 NOT = 30
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-30
           END-IF.
       M-35.
           MOVE SPACE TO W-HNAD W-HNAME.
           MOVE HI-NAME TO W-HNAD.
           MOVE ZERO TO CHK CNT.
       M-40.
           ADD 1 TO CNT.
           IF  CNT = 24
               GO TO M-50
           END-IF
           IF  CHK NOT = ZERO
               GO TO M-45
           END-IF
           MOVE W-HNA(CNT) TO W-NA(CNT).
           IF  W-HNA(CNT) = SPACE
               MOVE 5 TO CHK
           END-IF
           GO TO M-40.
       M-45.
           MOVE W-HNA(CNT) TO W-NA(CNT).
           IF  W-HNA(CNT) NOT = SPACE
               MOVE ZERO TO CHK
               GO TO M-40
           END-IF.
       M-50.
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-SU "A-SU" "S9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-30
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-55
           END-IF
           IF  W-SU = ZERO
               CALL "SD_Output" USING "S-SU" S-SU "p" RETURNING RESU
               CALL "SD_Output" USING "S-T" S-T "p" RETURNING RESU
               GO TO M-65
           END-IF
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
       M-60.
           CALL "SD_Accept" USING BY REFERENCE A-T "A-T" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-55
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-60
           END-IF
           CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU.
           COMPUTE W-KIN = W-SU * W-T.
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           GO TO M-70.
       M-65.
           CALL "SD_Accept" USING BY REFERENCE A-KIN "A-KIN" "S9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-55
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-65
           END-IF
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
       M-70.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-SU = ZERO
                   GO TO M-65
               ELSE
                   GO TO M-60
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-70
           END-IF
           IF  W-DMM = 9
               GO TO M-55
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-70
           END-IF
      *
           MOVE HI-BC2 TO W-BC.
           MOVE ZERO TO HKS-R.
           MOVE W-TCD TO K-TCD.
           MOVE W-HCD1 TO K-HCD1.
           MOVE W-SU TO K-SU.
           MOVE W-KIN TO K-KIN.
           MOVE W-ADR TO K-ADR.
           MOVE W-BC2 TO K-BC.
           MOVE W-NG TO K-NG.
      *           WRITE HKS-R.
      *//////////////
           CALL "DB_Insert" USING
            HKS-F_PNAME1 HKS-F_LNAME HKS-R RETURNING RET.
           GO TO M-25.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HKS-F_IDLST HKS-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

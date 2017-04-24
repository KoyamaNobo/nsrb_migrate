       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKT310.
      *********************************************************
      *    PROGRAM         :  請求明細　問合せ　　　　　      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHK31                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-MSG              PIC  X(030).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-S            PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-SK           PIC S9(009).
           02  W-SKZ          PIC S9(007).
           02  W-UR           PIC S9(009).
           02  W-URZ          PIC S9(007).
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-NGPD.
             03  W-NEND       PIC  9(004).
             03  W-GPD.
               04  W-GETD     PIC  9(002).
               04  W-PEYD     PIC  9(002).
           02  W-SHM          PIC  N(001).
           02  W-SGR          PIC  9(001)V9(01).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
      *FD  SM-F
       01  SM-F_HKT310.
           02  SM-F_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SM-F_LNAME     PIC  X(011) VALUE "SM-F_HMG240".
           02  F              PIC  X(001).
           02  SM-F_KEY1      PIC  X(100) VALUE SPACE.
           02  SM-F_SORT      PIC  X(100) VALUE SPACE.
           02  SM-F_IDLST     PIC  X(100) VALUE SPACE.
           02  SM-F_RES       USAGE  POINTER.
       01  SM-R.
           02  SM-TCD         PIC  9(004).
           02  SM-DATE        PIC  9(008).
           02  SM-NGP   REDEFINES SM-DATE.
             03  SM-NEN       PIC  9(004).
             03  SM-GP        PIC  9(004).
           02  SM-ZS          PIC S9(009).
           02  SM-ZSZ         PIC S9(007).
           02  SM-UR          PIC S9(009).
           02  SM-URZ         PIC S9(007).
           02  SM-TS          PIC S9(007).
           02  SM-TSZ         PIC S9(005).
           02  SM-NK          PIC S9(009).
           02  SM-NKZ         PIC S9(007).
           02  SM-NNGP.
             03  SM-NY        PIC  9(002).
             03  SM-NMD       PIC  9(004).
           02  SM-SIT         PIC  9(003).
           02  SM-SU          PIC  9(001).
           02  SM-DNO         PIC  9(006).
           02  F              PIC  X(004).
           02  SM-TNC         PIC  9(002).
           02  F              PIC  X(007).
           02  SM-PC          PIC  9(001).
           02  F              PIC  X(026).
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
           02  A-TCD   PIC  9(004).
           02  A-GP    PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NAME  PIC  N(026).
           02  D-SSM.
             03  01D-SSM PIC  Z(003).
             03  FILLER  PIC  X(001) VALUE ",".
             03  03D-SSM PIC  N(001).
             03  FILLER  PIC  X(001) VALUE ",".
             03  05D-SSM PIC Z.Z .
             03  FILLER  PIC  X(001) VALUE "%".
           02  FILLER.
             03  D-MD.
               04  01D-MD  PIC  9(004).
               04  02D-MD  PIC ----,---,--9 .
               04  03D-MD  PIC -----,--9 .
               04  04D-MD  PIC ----,---,--9 .
               04  05D-MD  PIC -----,--9 .
               04  06D-MD  PIC ----,---,--9 .
               04  07D-MD  PIC -----,--9 .
             03  D-NMD   PIC  9(004).
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(030).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
           "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-TCD" "9" "5" "7" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-GP" "9" "9" "2" "4" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-GP" BY REFERENCE W-GPD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "22" "79" "1" "A-GP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "134" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "5" "21" "52" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SSM" " " "6" "0" "11" "D-NAME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SSM" "Z" "6" "26" "3" " " "D-SSM" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-SSM" BY REFERENCE T-NKY "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SSM" "X" "6" "30" "1" "01D-SSM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SSM" "N" "6" "32" "2" "02D-SSM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-SSM" BY REFERENCE W-SHM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-SSM" "X" "6" "35" "1" "03D-SSM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-SSM" "Z.Z" "6" "37" "3" "04D-SSM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "05D-SSM" BY REFERENCE W-SGR "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-SSM" "X" "6" "40" "1" "05D-SSM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-DSP" " " "W-L" "0" "71" "D-SSM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MD" " " "W-L" "21" "67" " " "03C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MD" "9" "W-L" "2" "4" " " "D-MD" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MD" BY REFERENCE SM-GP "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MD" "----,---,--9" "W-L" "7" "12" "01D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MD" BY REFERENCE W-UR "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MD" "-----,--9" "W-L" "20" "9" "02D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-MD" BY REFERENCE W-URZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-MD" "----,---,--9" "W-L" "30" "12" "03D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-MD" BY REFERENCE SM-NK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-MD" "-----,--9" "W-L" "43" "9" "04D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-MD" BY REFERENCE SM-NKZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-MD" "----,---,--9" "W-L" "53" "12" "05D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06D-MD" BY REFERENCE W-SK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-MD" "-----,--9" "W-L" "66" "9" "06D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07D-MD" BY REFERENCE W-SKZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NMD" "9" "W-L" "76" "4" "D-MD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NMD" BY REFERENCE SM-NMD "4" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "90" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "90" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME" "X" "24" "15" "30" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-ME" BY REFERENCE W-MSG "30" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME" " " RETURNING RESU.
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
           COPY LIBCPR.
           MOVE ZERO TO W-DATA.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO SM-F_PNAME1.
       M-10.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHK31" RETURNING RESU.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  ﾄｸｲｻｷ ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  T-SHC1 = 1
               MOVE "現" TO W-SHM
           ELSE
               IF  T-SHC1 = 2
                   MOVE "小" TO W-SHM
               ELSE
                   IF  T-SHC1 = 3
                       MOVE "手" TO W-SHM
                   ELSE
                       MOVE "　" TO W-SHM
                   END-IF
               END-IF
           END-IF
           COMPUTE W-SGR = T-SGR + T-STR.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SSM" D-SSM "p" RETURNING RESU.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-GP "A-GP" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-GETD < 1 OR > 12
               GO TO M-20
           END-IF
           IF  W-PEYD < 1 OR > 31
               GO TO M-20
           END-IF
           IF  W-GET < W-GETD
               COMPUTE W-NEND = W-NEN - 1
           ELSE
               MOVE W-NEN TO W-NEND
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" SM-F_PNAME1 " " BY REFERENCE SM-F_IDLST "0".
       M-25.
      *           READ SM-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SM-F_PNAME1 BY REFERENCE SM-R " " RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-S
               GO TO M-40
           END-IF
           IF  SM-PC NOT = 0
               GO TO M-25
           END-IF
           IF  SM-TCD < W-TCD
               GO TO M-25
           END-IF
           IF  SM-TCD > W-TCD
               MOVE 9 TO W-S
               GO TO M-40
           END-IF
           IF  SM-DATE < W-NGPD
               GO TO M-25
           END-IF
      *
           MOVE 8 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-30.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 21
               MOVE 1 TO W-S
               GO TO M-40
           END-IF
           COMPUTE W-UR = SM-UR + SM-TS.
           COMPUTE W-URZ = SM-URZ + SM-TSZ.
           COMPUTE W-SK = SM-ZS + W-UR - SM-NK.
           COMPUTE W-SKZ = SM-ZSZ + W-URZ - SM-NKZ.
           CALL "SD_Output" USING "D-MD" D-MD "p" RETURNING RESU.
           IF  SM-NMD NOT = ZERO
               CALL "SD_Output" USING "D-NMD" D-NMD "p" RETURNING RESU
           END-IF.
       M-35.
      *           READ SM-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SM-F_PNAME1 BY REFERENCE SM-R " " RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-S
               GO TO M-40
           END-IF
           IF  SM-PC NOT = 0
               GO TO M-35
           END-IF
           IF  SM-TCD > W-TCD
               MOVE 9 TO W-S
               GO TO M-40
           END-IF
           GO TO M-30.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE SM-F_IDLST SM-F_PNAME1
               GO TO M-90
           END-IF
           IF  W-S = 1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Screen_Output" USING "SCHK31" RETURNING RESU
               CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-NAME" D-NAME "p" RETURNING RESU
               MOVE 8 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               GO TO M-30
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE SM-F_IDLST SM-F_PNAME1.
           GO TO M-10.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

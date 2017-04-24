       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKT410.
      *********************************************************
      *    PROGRAM         :  得意先別入金明細問合せ　　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHK41                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  ERR-STAT           PIC  X(002).
       01  W-FILE             PIC  X(013).
       01  W-MSG              PIC  X(030).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-S            PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-NO           PIC  9(006).
           02  W-NOC          PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-KBN          PIC  N(006).
           02  W-KIN          PIC S9(009).
           02  W-SHZ          PIC S9(007).
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
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHKBM.
      *FD  NYU-F
       01  NYU-F_HKT410.
           02  NYU-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  NYU-F_LNAME    PIC  X(012) VALUE "NYU-F_HKT410".
           02  F              PIC  X(001).
           02  NYU-F_KEY1     PIC  X(100) VALUE SPACE.
           02  NYU-F_SORT     PIC  X(100) VALUE SPACE.
           02  NYU-F_IDLST    PIC  X(100) VALUE SPACE.
           02  NYU-F_RES      USAGE  POINTER.
       01  NYU-R.
           02  N-DATE.
             03  N-NEN        PIC  9(004).
             03  N-GP         PIC  9(004).
           02  N-TCD          PIC  9(004).
           02  N-KIN          PIC S9(008).
           02  N-NC.
             03  N-NC1        PIC  9(001).
             03  N-NC2        PIC  9(001).
           02  N-NSC          PIC  9(001).
           02  F              PIC  9(002).
           02  N-TD           PIC  9(006).
           02  F              PIC  9(002).
           02  N-SS           PIC  9(004).
           02  N-BC           PIC  9(001).
           02  N-TC           PIC  9(002).
           02  F              PIC  X(003).
           02  N-KEY.
             03  N-NO         PIC  9(006).
             03  N-GNO        PIC  9(001).
           02  N-FNO          PIC  9(008).
           02  N-SKD          PIC  9(008).
           02  N-DCC          PIC  9(001).
           02  F              PIC  X(013).
           02  N-DC           PIC  9(001).
           02  F              PIC  X(001).
           02  N-DPC          PIC  9(001).
           02  N-ACT          PIC  9(001).
           02  N-PRC          PIC  9(001).
           02  F              PIC  X(043).
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
           02  FILLER.
             03  D-GP    PIC  9(004).
             03  D-KIN   PIC ---,---,--9 .
             03  D-SRI   PIC  N(006).
             03  D-TD    PIC 99/99/99 .
             03  D-SS    PIC 99/99 .
             03  D-TKIN  PIC ----,---,--9 .
             03  D-TSHZ  PIC --,---,--9 .
             03  D-NO    PIC  9(006).
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(030).
           COPY LSSEM.
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
           "A-TCD" "9" "3" "7" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-GP" "9" "6" "2" "4" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-GP" BY REFERENCE W-GPD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "77" "1" "A-GP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "120" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-NAME" "N" "3" "21" "52" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING
           "D-NAME" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "01C-DSP" " " "W-L" "0" "68" "D-NAME" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-GP" "9" "W-L" "2" "4" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING
           "D-GP" BY REFERENCE N-GP "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-KIN" "---,---,--9" "W-L" "7" "11" "D-GP" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "D-KIN" BY REFERENCE N-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-SRI" "N" "W-L" "19" "12" "D-KIN" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-SRI" BY REFERENCE W-KBN "12" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-TD" "99/99/99" "W-L" "32" "8" "D-SRI" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-TD" BY REFERENCE N-TD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-SS" "99/99" "W-L" "42" "5" "D-TD" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-SS" BY REFERENCE N-SS "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-TKIN" "----,---,--9" "W-L" "48" "12" "D-SS" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "D-TKIN" BY REFERENCE W-KIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-TSHZ" "--,---,--9" "W-L" "61" "10" "D-TKIN" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "D-TSHZ" BY REFERENCE W-SHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-NO" "9" "W-L" "72" "6" "D-TSHZ" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-NO" BY REFERENCE W-NO "6" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "30" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "30" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME" "X" "24" "15" "30" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-ME" BY REFERENCE W-MSG "30" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           MOVE ZERO TO W-DATA.
           MOVE D-NHNG TO W-NGS.
           IF W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO NYU-F_PNAME1.
       M-10.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHK41" RETURNING RESU.
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
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
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
           MOVE 0 TO W-S.
           CALL "DB_F_Open" USING
            "INPUT" NYU-F_PNAME1 " " BY REFERENCE NYU-F_IDLST "0".
       M-25.
      *           READ NYU-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " " RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-S
               MOVE SPACE TO W-MSG
               MOVE "***  DATA ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               GO TO M-40
           END-IF
           IF  N-TCD < W-TCD
               GO TO M-25
           END-IF
           IF  N-TCD > W-TCD
               MOVE 9 TO W-S
               MOVE SPACE TO W-MSG
               MOVE "***  DATA ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               GO TO M-40
           END-IF
           IF  N-DATE < W-NGPD
               GO TO M-25
           END-IF
      *
           MOVE ZERO TO W-NO W-KIN W-SHZ.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-30.
           IF  W-NO = ZERO
               MOVE N-NO TO W-NO
               MOVE 0 TO W-NOC
               GO TO M-35
           END-IF
           IF  N-NO NOT = W-NO
               CALL "SD_Output" USING
                "D-TKIN" D-TKIN "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-TSHZ" D-TSHZ "p" RETURNING RESU
               MOVE ZERO TO W-KIN W-SHZ
               MOVE N-NO TO W-NO
               MOVE 0 TO W-NOC
           END-IF.
       M-35.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 23
               GO TO M-40
           END-IF
           PERFORM KBN-RTN THRU KBN-EX.
           IF  W-NOC = 0
               MOVE 1 TO W-NOC
               CALL "SD_Output" USING "D-NO" D-NO "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-GP" D-GP "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SRI" D-SRI "p" RETURNING RESU.
           IF  N-TD NOT = ZERO
               CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU
           END-IF
           IF  N-SS NOT = ZERO
               CALL "SD_Output" USING "D-SS" D-SS "p" RETURNING RESU
           END-IF
      *
      *           READ NYU-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "D-TKIN" D-TKIN "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-TSHZ" D-TSHZ "p" RETURNING RESU
               MOVE 9 TO W-S
               GO TO M-40
           END-IF
           IF  N-TCD NOT = W-TCD
               CALL "SD_Output" USING
                "D-TKIN" D-TKIN "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-TSHZ" D-TSHZ "p" RETURNING RESU
               MOVE 9 TO W-S
               GO TO M-40
           END-IF
           GO TO M-30.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE NYU-F_IDLST NYU-F_PNAME1
               GO TO M-90
           END-IF
           IF  W-DMM = 9
               CALL "DB_F_Close" USING
                BY REFERENCE NYU-F_IDLST NYU-F_PNAME1
               GO TO M-10
           END-IF
           IF  W-S NOT = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Screen_Output" USING "SCHK41" RETURNING RESU
               CALL "SD_Output" USING
                "A-TCD" A-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-NAME" D-NAME "p" RETURNING RESU
               MOVE 5 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE 0 TO W-NOC
               GO TO M-35
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
           GO TO M-10.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       KBN-RTN.
           MOVE SPACE TO W-KBN.
           IF  N-NC2 = 8
               MOVE "消費税　　　" TO W-KBN
               ADD N-KIN TO W-SHZ
               GO TO KBN-EX
           END-IF
           IF  N-NC2 = 9
               MOVE "消費税取消し" TO W-KBN
               ADD N-KIN TO W-SHZ
               GO TO KBN-EX
           END-IF
           ADD N-KIN TO W-KIN.
           IF  N-NC1 = 6
               GO TO KBN-010
           END-IF
           MOVE SPACE TO HKB-KEY.
           MOVE "31" TO HKB-NO.
           MOVE N-NC1 TO HKB-NKC1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-NKNA
           END-IF
           MOVE HKB-NKNA TO W-KBN.
           GO TO KBN-EX.
       KBN-010.
           MOVE SPACE TO HKB-KEY.
           MOVE "32" TO HKB-NO.
           MOVE N-NSC TO HKB-NSC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-NSNA
           END-IF
           MOVE HKB-NSNA TO W-KBN.
       KBN-EX.
           EXIT.

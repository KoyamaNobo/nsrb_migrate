       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS02U.
      **********************************************************
      *    ワークマン受信データ　チェック                      *
      **********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT         PIC  X(002).
       01  W-DATA.
           02  W-DMM        PIC  9(001).
           02  W-ERR        PIC  9(001).
           02  W-SU         PIC  9(005).
           COPY LSTAT.
      *
           COPY LSWMJC.
      *FD  WK-F
       01  WK-F_JHS02U.
           02  WK-F_PNAME1    PIC  X(006) VALUE "WK1301".
           02  F              PIC  X(001).
           02  WK-F_LNAME     PIC  X(011) VALUE "WK-F_JHS02U".
           02  F              PIC  X(001).
           02  WK-F_KEY1      PIC  X(100) VALUE SPACE.
           02  WK-F_SORT      PIC  X(100) VALUE SPACE.
           02  WK-F_IDLST     PIC  X(100) VALUE SPACE.
           02  WK-F_RES       USAGE  POINTER.
       01  WK-R.
           02  WK-001          PIC  X(003).
           02  WK-002          PIC  X(013).
           02  WK-003          PIC  X(004).
           02  WK-004          PIC  X(013).
           02  WK-005          PIC  X(004).
           02  WK-006          PIC  X(012).
           02  WK-007          PIC  X(004).
           02  WK-008          PIC  X(080).
           02  WK-009          PIC  X(255).
           02  WK-010          PIC  X(005).
           02  WK-011          PIC  X(014).
           02  WK-012          PIC  X(013).
           02  WK-013          PIC  X(004).
           02  WK-014          PIC  X(001).
           02  WK-015          PIC  X(013).
           02  WK-016          PIC  X(013).
           02  WK-017          PIC  X(013).
           02  WK-018          PIC  X(080).
           02  WK-019          PIC  X(008).
           02  WK-020          PIC  X(008).
           02  WK-021          PIC  X(008).
           02  WK-022          PIC  X(001).
           02  WK-023          PIC  X(001).
           02  WK-024          PIC  X(001).
           02  WK-025          PIC  X(003).
           02  WK-026          PIC  X(003).
           02  WK-027          PIC  X(001).
           02  WK-028          PIC  X(001).
           02  WK-029          PIC  9(004).
           02  WK-030          PIC  X(001).
           02  WK-031          PIC  X(001).
           02  WK-032          PIC  X(001).
           02  WK-033          PIC  9(004).
           02  WK-034          PIC  X(001).
           02  WK-035          PIC  N(020).
           02  WK-036          PIC  X(020).
           02  WK-037          PIC  9(009).
           02  WK-038          PIC  9(001).
           02  WK-039          PIC  9(005).
           02  WK-040          PIC  X(001).
           02  WK-041          PIC  N(020).
           02  WK-042          PIC  X(020).
           02  WK-043        PIC  9(005).
           02  WK-044        PIC  X(001).
           02  WK-045        PIC  N(020).
           02  WK-046        PIC  X(020).
           02  WK-047        PIC  9(005).
           02  WK-048        PIC  X(001).
           02  WK-049        PIC  X(001).
           02  WK-050        PIC  X(001).
           02  WK-051        PIC  X(001).
           02  WK-052        PIC  X(001).
           02  WK-053        PIC  X(001).
           02  WK-054        PIC  9(006).
           02  WK-055        PIC  X(001).
           02  WK-056        PIC  N(020).
           02  WK-057        PIC  X(020).
           02  WK-058        PIC  9(006).
           02  WK-059        PIC  X(001).
           02  WK-060        PIC  N(020).
           02  WK-061        PIC  X(020).
           02  WK-062        PIC  X(001).
           02  WK-063        PIC  X(001).
           02  WK-064        PIC  X(001).
           02  WK-065        PIC  9(002).
           02  WK-066        PIC  9(002).
           02  WK-067        PIC  X(001).
           02  WK-068        PIC  9(002).
           02  WK-069        PIC  X(001).
           02  WK-070        PIC  X(001).
           02  WK-071        PIC  X(001).
           02  WK-072        PIC  X(001).
           02  WK-073        PIC  X(001).
           02  WK-074        PIC  X(001).
           02  WK-075        PIC  X(001).
           02  WK-076        PIC  X(001).
           02  WK-077        PIC  X(003).
           02  WK-078        PIC  X(001).
           02  WK-079        PIC  X(010).
           02  WK-080        PIC  X(010).
           02  WK-081        PIC  X(010).
           02  WK-082        PIC  X(001).
           02  WK-083        PIC  X(001).
           02  WK-084        PIC  X(001).
           02  WK-085        PIC  X(001).
           02  WK-086        PIC  9(002).
           02  WK-087        PIC  9(002).
           02  WK-088        PIC  9(002).
           02  WK-089        PIC  X(001).
           02  WK-090        PIC  X(001).
           02  WK-091        PIC  X(001).
           02  WK-092        PIC  9(002).
           02  WK-093        PIC  9(002).
           02  WK-094        PIC  X(001).
           02  WK-095        PIC  X(001).
           02  WK-096        PIC  X(001).
           02  WK-097        PIC  9(002).
           02  WK-098        PIC  X(001).
           02  WK-099        PIC  X(001).
           02  WK-100        PIC  X(001).
           02  WK-101        PIC  X(001).
           02  WK-102        PIC  X(001).
           02  WK-103        PIC  X(001).
           02  WK-104        PIC  X(001).
           02  WK-105        PIC  X(001).
           02  WK-106        PIC  X(001).
           02  WK-107        PIC  9(002).
           02  WK-108        PIC  9(001).
           02  WK-109        PIC  X(001).
           02  WK-110        PIC  X(001).
           02  WK-111        PIC  X(003).
           02  WK-112        PIC  X(006).
           02  WK-113        PIC  X(001).
           02  WK-114        PIC  X(001).
           02  WK-115        PIC  X(001).
           02  WK-116        PIC  X(001).
           02  WK-117        PIC  X(014).
           02  WK-118        PIC  X(014).
           02  WK-119        PIC  X(003).
           02  WK-120        PIC  X(005).
           02  WK-121        PIC  N(025).
           02  WK-122        PIC  X(025).
           02  WK-123        PIC  X(016).
           02  WK-124        PIC  X(001).
           02  WK-125        PIC  X(004).
           02  WK-126        PIC  N(008).
           02  WK-127        PIC  X(008).
           02  WK-128        PIC  X(004).
           02  WK-129        PIC  N(008).
           02  WK-130        PIC  X(008).
           02  WK-131        PIC  X(001).
           02  WK-132        PIC  X(001).
           02  WK-133        PIC  X(001).
           02  WK-134        PIC  X(001).
           02  WK-135        PIC  X(001).
           02  WK-136        PIC  X(001).
           02  WK-137        PIC  X(001).
           02  WK-138        PIC  X(001).
           02  WK-139        PIC  X(001).
           02  WK-140        PIC  X(001).
           02  WK-141        PIC  X(001).
           02  WK-142        PIC  X(001).
           02  WK-143        PIC  X(001).
           02  WK-144        PIC  X(001).
           02  WK-145        PIC  X(001).
           02  WK-146        PIC  X(001).
           02  WK-147        PIC  X(001).
           02  WK-148        PIC  X(001).
           02  WK-149        PIC  X(001).
           02  WK-150        PIC  9(010).
           02  WK-151        PIC  9(007)V99.
           02  WK-152        PIC  9(010).
           02  WK-153        PIC  9(007).
           02  WK-154        PIC  9(001).
           02  WK-155        PIC  9(004).
           02  WK-156        PIC  X(002).
           02  WK-157        PIC  9(002).
           02  WK-158        PIC  9(005)V9.
           02  WK-159        PIC  9(004).
           02  WK-160        PIC  X(001).
           02  WK-161        PIC  X(001).
           02  WK-162        PIC  X(001).
           02  WK-163        PIC  X(001).
       77  F                 PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　ワークマン受信データ　チェック　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SU    PIC ZZ,ZZ9 .
           02  D-SUM.
             03  FILLER  PIC  X(018) VALUE
                  "受注件数          ".
             03  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  N(010) VALUE
                  "受注以外のデータ有り".
             03  E-ME2   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "350" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "50" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "50" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "50" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "50" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "50" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "50" "06C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "40" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "46" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SU" "ZZ,ZZ9" "16" "30" "6" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING
            "D-SU" BY REFERENCE W-SU "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-SUM" " " "0" "0" "40" "D-SU" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-SUM" "X" "16" "20" "18" " " "D-SUM" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-SUM" "X" "20" "23" "22" "01D-SUM" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "99" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "99" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "N" "24" "15" "20" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
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
           CALL "DB_F_Open" USING
            "INPUT" WK-F_PNAME1 " " BY REFERENCE WK-F_IDLST "0".
           MOVE 0 TO W-ERR.
       M-10.
      *           READ WK-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" WK-F_PNAME1 BY REFERENCE WK-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE WK-F_IDLST WK-F_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF
           IF  WK-158 = 0
               IF  W-ERR = 0
                   CALL "SD_Output" USING
                    "E-ME1" E-ME1 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-CL" E-CL "p" RETURNING RESU
                   MOVE 1 TO W-ERR
                   GO TO M-10
               ELSE
                   GO TO M-10
               END-IF
           END-IF
           CALL "DB_F_Open" USING
            "OUTPUT" WMJCF_PNAME1 " " BY REFERENCE WMJCF_IDLST "0".
           MOVE ZERO TO W-SU.
       M-15.
           INITIALIZE WMJC-R.
           MOVE WK-R TO WMJC-R.
      *           WRITE WMJC-R.
      *//////////////
           CALL "DB_Insert" USING
            WMJCF_PNAME1 WMJCF_LNAME WMJC-R RETURNING RET.
           ADD 1 TO W-SU.
       M-20.
      *           READ WK-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" WK-F_PNAME1 BY REFERENCE WK-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  WK-158 = 0
               IF  W-ERR = 0
                   CALL "SD_Output" USING
                    "E-ME1" E-ME1 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   MOVE 1 TO W-ERR
                   GO TO M-20
               ELSE
                   GO TO M-20
               END-IF
           END-IF
           GO TO M-15.
       M-50.
           CALL "DB_F_Close" USING
            BY REFERENCE WK-F_IDLST WK-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WMJCF_IDLST WMJCF_PNAME1.
           CALL "SD_Output" USING "D-SUM" D-SUM "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-55
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-55
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

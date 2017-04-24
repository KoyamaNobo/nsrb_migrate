       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG960.
      *    PROGRAM         :  履物月次　マスター更新・クリア　　 *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-GET        PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-NGD.
             03  W-NEN1       PIC  9(002).
             03  W-NGS.
               04  W-NEN2     PIC  9(002).
               04  W-GETS     PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
      *
      *FD  HIYF
       01  HIYF_HMM765.
           02  HIYF_PNAME1    PIC  X(004) VALUE "HIYF".
           02  F              PIC  X(001).
           02  HIYF_LNAME     PIC  X(011) VALUE "HIYF_HMM765".
           02  F              PIC  X(001).
           02  HIYF_KEY1      PIC  X(100) VALUE SPACE.
           02  HIYF_SORT      PIC  X(100) VALUE SPACE.
           02  HIYF_IDLST     PIC  X(100) VALUE SPACE.
           02  HIYF_RES       USAGE  POINTER.
       01  HIY-R.
           02  F              PIC  X(006).
           02  HIY-NGD.
             03  HIY-NEN      PIC  9(004).
             03  HIY-GET      PIC  9(002).
           02  HIY-NG    REDEFINES HIY-NGD  PIC 9(006).
           02  F              PIC  X(090).
       77  F                  PIC  X(001).
      *FD  HUH-M
       01  HUH-M_HMM765.
           02  HUH-M_PNAME1   PIC  X(006) VALUE "T-HUHM".
           02  F              PIC  X(001).
           02  HUH-M_LNAME    PIC  X(012) VALUE "HUH-M_HMM765".
           02  F              PIC  X(001).
           02  HUH-M_KEY1     PIC  X(100) VALUE SPACE.
           02  HUH-M_SORT     PIC  X(100) VALUE SPACE.
           02  HUH-M_IDLST    PIC  X(100) VALUE SPACE.
           02  HUH-M_RES      USAGE  POINTER.
       01  HUH-R.
           02  HUH-KEY.
             03  HUH-HCD      PIC  9(006).
             03  HUH-HCDD  REDEFINES HUH-HCD.
               04  HUH-HCD1   PIC  9(004).
               04  HUH-HCD2   PIC  9(002).
           02  HUH-NGD.
             03  HUH-NEN      PIC  9(004).
             03  HUH-GET      PIC  9(002).
           02  HUH-NG    REDEFINES HUH-NGD  PIC 9(006).
           02  HUH-D.
             03  HUH-ZS       PIC S9(006).
             03  HUH-ZK       PIC S9(009).
             03  HUH-NS       PIC S9(007).
             03  HUH-NK       PIC S9(010).
             03  HUH-SS       PIC S9(008).
             03  HUH-SK       PIC S9(010).
             03  HUH-YS       PIC S9(006).
             03  HUH-YK       PIC S9(009).
             03  HUH-UG       PIC S9(010).
           02  HUH-BCD12.
             03  HUH-BCD1     PIC  9(003).
             03  HUH-BCW1  REDEFINES HUH-BCD1.
               04  HUH-BC1    PIC  9(002).
               04  HUH-BC21   PIC  9(001).
             03  HUH-BC22     PIC  9(001).
           02  HUH-BCW12 REDEFINES HUH-BCD12.
             03  F            PIC  9(002).
             03  HUH-BC2      PIC  9(002).
           02  HUH-BC3        PIC  9(002).
           02  HUH-BCD3  REDEFINES HUH-BC3.
             03  HUH-BC31     PIC  9(001).
             03  HUH-BC32     PIC  9(001).
           02  HUH-BMC        PIC  9(002).
           02  HUH-BMNO       PIC  9(001).
           02  HUH-BC4        PIC  9(001).
           02  F              PIC  X(005).
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
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　ＨＩＹＦ　Ｔ－ＨＵＨＭ　変換　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　（評価替え）　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(014) VALUE
                  "    年   月 分".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-NEN   PIC 9(004).
             03  A-GET   PIC 9(002).
           02  A-DMM   PIC 9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(022) VALUE
                  "***  HIYF DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(026) VALUE
                  "***  T-HUHM WRITE ｴﾗｰ  ***".
             03  E-HUHM  PIC  X(006).
           COPY LIBSCR.
           COPY LSSEM.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "386" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "18" "14" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "22" "18" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "12" "0" "6" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "12" "18" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "12" "25" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "35" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "54" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "54" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "22" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "26" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HUHM" "X" "24" "50" "6" "E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HUHM" BY REFERENCE HUH-KEY "6" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NGS.
           MOVE 20 TO W-NEN1.
           SUBTRACT 1 FROM W-GETS.
           IF  W-GETS = ZERO
               MOVE 12 TO W-GETS
               SUBTRACT 1 FROM W-NEN2
           END-IF
           MOVE W-NGD TO W-NG.
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-NEN < 2010
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-GET < 1 OR > 12
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
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
      *
           CALL "DB_F_Open" USING
            "OUTPUT" HUH-M_PNAME1 " " BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-M_IDLST HUH-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HIYF_PNAME1 " " BY REFERENCE HIYF_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" HUH-M_PNAME1 " " BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
       M-25.
      *           READ HIYF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HIYF_PNAME1 BY REFERENCE HIY-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HIY-NG NOT = W-NG
               GO TO M-25
           END-IF
           MOVE HIY-R TO HUH-R.
      *           WRITE HUH-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HUH-M_PNAME1 HUH-M_LNAME HUH-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HUHM" E-HUHM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF
           GO TO M-25.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-M_IDLST HUH-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HIYF_IDLST HIYF_PNAME1.
           IF  W-DC = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

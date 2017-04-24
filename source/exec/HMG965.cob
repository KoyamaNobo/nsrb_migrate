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
           02  CNT            PIC  9(002).
       01  ERR-STAT           PIC  X(002).
      *
      *FD  HI-M
       01  HI-M_HMG965.
           02  HI-M_PNAME1    PIC  X(006) VALUE "T-HIM1".
           02  F              PIC  X(001).
           02  HI-M_LNAME     PIC  X(011) VALUE "HI-M_HMG965".
           02  F              PIC  X(001).
           02  HI-M_KEY1      PIC  X(100) VALUE SPACE.
           02  HI-M_SORT      PIC  X(100) VALUE SPACE.
           02  HI-M_IDLST     PIC  X(100) VALUE SPACE.
           02  HI-M_RES       USAGE  POINTER.
       01  HI-R.
           02  HI-KEY2.
             03  HI-MHCD      PIC  9(006).
             03  HI-MHCDD REDEFINES HI-MHCD.
               04  HI-MHCD1   PIC  9(004).
               04  HI-MHCD2   PIC  9(002).
             03  HI-HCD       PIC  9(006).
             03  HI-KEY   REDEFINES HI-HCD.
               04  HI-HCD1    PIC  9(004).
               04  HI-HCD2    PIC  9(002).
           02  HI-NAME        PIC  N(024).
           02  HI-BC.
             03  HI-BCD12.
               04  HI-BCD1    PIC  9(003).
               04  HI-BCW1 REDEFINES HI-BCD1.
                 05  HI-BC1   PIC  9(002).
                 05  HI-BC21  PIC  9(001).
               04  HI-BC22    PIC  9(001).
             03  HI-BCW12 REDEFINES HI-BCD12.
               04  F          PIC  9(002).
               04  HI-BC2     PIC  9(002).
             03  HI-BC3       PIC  9(002).
           02  HI-ASSD.
             03  HI-SSD   OCCURS  4.
               04  HI-SS      PIC  9(010).
           02  HI-ASKD  REDEFINES HI-ASSD.
             03  HI-SKD   OCCURS  4.
               04  HI-SK    OCCURS 10.
                 05  HI-S     PIC  9(001).
           02  HI-AHSD  REDEFINES HI-ASSD.
             03  HI-HSD.
               04  HI-SS1     PIC  9(010).
               04  HI-SD1   REDEFINES HI-SS1.
                 05  HI-S1    OCCURS  10  PIC  9(001).
               04  HI-SS2     PIC  9(010).
               04  HI-SD2    REDEFINES HI-SS2.
                 05  HI-S2    OCCURS  10  PIC  9(001).
               04  HI-SS3     PIC  9(010).
               04  HI-SD3    REDEFINES HI-SS3.
                 05  HI-S3    OCCURS  10  PIC  9(001).
               04  HI-SS4     PIC  9(010).
               04  HI-SD4    REDEFINES HI-SS4.
                 05  HI-S4    OCCURS  10  PIC  9(001).
           02  HI-SB          PIC  9(005).
           02  HI-FT          PIC  9(005).
           02  F              PIC  X(019).
           02  HI-KT          PIC  9(005).
           02  HI-TCD         PIC  9(004).
           02  HI-ISU         PIC  9(003).
           02  HI-KRC         PIC  9(001).
           02  HI-SCC         PIC  9(001).
           02  HI-BMC         PIC  9(002).
           02  HI-BMNO        PIC  9(001).
           02  HI-YG          PIC  9(005).
           02  HI-HKB         PIC  9(001).
           02  HI-HPV         PIC  9(001).
           02  HI-BC4         PIC  9(001).
           02  HI-SSC         PIC  9(001).
           02  F              PIC  X(005).
           02  HI-YG2         PIC  9(005).
           02  HI-SMS         PIC  N(016).
           02  HI-UNG         PIC  9(006).
           02  HI-NNG         PIC  9(006).
           02  HI-OL          PIC  X(001).
           02  HI-CS          PIC  N(010).
           02  HI-RNG.
             03  F            PIC  X(005).
             03  HI-DELC      PIC  9(001).
           02  HI-DNG         PIC  9(006).
           02  HI-SNG         PIC  9(004).
           02  HI-SNGD    REDEFINES HI-SNG.
             03  HI-SNEN      PIC  9(002).
             03  HI-SGET      PIC  9(002).
           02  HI-ENG         PIC  9(004).
           02  HI-ENGD    REDEFINES HI-ENG.
             03  HI-ENEN      PIC  9(002).
             03  HI-EGET      PIC  9(002).
       77  F                  PIC  X(001).
      *FD  HUH-M
       01  HUH-M_HMG965.
           02  HUH-M_PNAME1   PIC  X(006) VALUE "T-HUHM".
           02  F              PIC  X(001).
           02  HUH-M_LNAME    PIC  X(012) VALUE "HUH-M_HMG965".
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
                "＊＊＊　　履物月次　マスター更新・クリア　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　（評価替え）　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME5   PIC  X(026) VALUE
                  "***  HUHM REWRITE ｴﾗｰ  ***".
             03  E-ME8   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-HUHM  PIC  X(006).
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
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "48" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "48" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "26" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "16" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HUHM" "X" "24" "50" "6" "E-ME8" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HUHM" BY REFERENCE HUH-KEY "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "I-O" HUH-M_PNAME1 " " BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
       M-55.
      *           READ HUH-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HUH-M_PNAME1 BY REFERENCE HUH-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           MOVE HUH-YS TO HUH-ZS.
           MOVE HUH-YK TO HUH-ZK.
           MOVE ZERO TO HUH-NS HUH-NK HUH-SS HUH-SK HUH-UG.
           MOVE HUH-KEY TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HUHM" E-HUHM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-60
           END-IF
           COMPUTE HUH-YK = HUH-YS * HI-FT.
       M-60.
      *           REWRITE HUH-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HUH-M_PNAME1 HUH-M_LNAME HUH-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HUHM" E-HUHM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-55.
       M-95.
           PERFORM END-RTN  THRU  END-EX.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-M_IDLST HUH-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       END-EX.
           EXIT.

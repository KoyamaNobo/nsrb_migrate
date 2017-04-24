       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      HMM090.
      *********************************************************
      *    PROGRAM         :  履物品名マスター年間使用チェック*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *    DATA WRITTN     :  00/06/22                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  CHK            PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-NGM          PIC  9(006).
           02  W-NG           PIC  9(006).
           02  W-NGD   REDEFINES W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LIHUHM.
      *FD  HIYF
       01  HIYF_HMM091.
           02  HIYF_PNAME1    PIC  X(004) VALUE "HIYF".
           02  F              PIC  X(001).
           02  HIYF_LNAME     PIC  X(011) VALUE "HIYF_HMM091".
           02  F              PIC  X(001).
           02  HIYF_KEY1      PIC  X(100) VALUE SPACE.
           02  HIYF_KEY2      PIC  X(100) VALUE SPACE.
           02  HIYF_SORT      PIC  X(100) VALUE SPACE.
           02  HIYF_IDLST     PIC  X(100) VALUE SPACE.
           02  HIYF_RES       USAGE  POINTER.
       01  HIY-R.
           02  HIY-KEY.
             03  HIY-HCD      PIC  9(006).                              品名ｺｰﾄﾞ
           02  HIY-NG.
             03  HIY-NEN      PIC  9(004).
             03  HIY-GET      PIC  9(002).
           02  HIY-D.
             03  HIY-ZS       PIC S9(006).                              前繰数
             03  HIY-ZK       PIC S9(009).                              前繰額
             03  HIY-NS       PIC S9(007).                              入庫数
             03  HIY-NK       PIC S9(010).                              入庫額
             03  HIY-SS       PIC S9(008).                              出荷数
             03  HIY-SK       PIC S9(010).                              出荷額
             03  HIY-YS       PIC S9(006).                              翌繰数
             03  HIY-YK       PIC S9(009).                              翌繰額
             03  HIY-UG       PIC S9(010).                              売上原価
           02  HIY-BCD12.
             03  HIY-BCD1     PIC  9(003).
             03  HIY-BC22     PIC  9(001).
           02  HIY-BC3        PIC  9(002).                              分類CD3
           02  HIY-BMC        PIC  9(002).
           02  HIY-BMNO       PIC  9(001).
           02  F              PIC  X(006).
       77  F                  PIC  X(001).
      *FD  RSTRANYR
       01  RSTRANYR_HMM091.
           02  RSTRANYR_PNAME1 PIC  X(011) VALUE "STRANYR-RDB".
           02  F               PIC  X(001).
           02  RSTRANYR_LNAME  PIC  X(015) VALUE "RSTRANYR_HMM091".
           02  F               PIC  X(001).
           02  RSTRANYR_KEY1   PIC  X(100) VALUE SPACE.
           02  RSTRANYR_KEY2   PIC  X(100) VALUE SPACE.
           02  RSTRANYR_SORT   PIC  X(100) VALUE SPACE.
           02  RSTRANYR_IDLST  PIC  X(100) VALUE SPACE.
           02  RSTRANYR_RES    USAGE  POINTER.
       01  STR-R.
           02  F              PIC  X(019).
           02  STR-HCD        PIC  9(006).                              品名ｺｰﾄﾞ
           02  F              PIC  X(103).
       77  F                  PIC  X(001).
      *FD  UTRYR
       01  UTRYR_HMM091.
           02  UTRYR_PNAME1   PIC  X(009) VALUE "UTRYR-RDB".
           02  F              PIC  X(001).
           02  UTRYR_LNAME    PIC  X(012) VALUE "UTRYR_HMM091".
           02  F              PIC  X(001).
           02  UTRYR_KEY1     PIC  X(100) VALUE SPACE.
           02  UTRYR_KEY2     PIC  X(100) VALUE SPACE.
           02  UTRYR_SORT     PIC  X(100) VALUE SPACE.
           02  UTRYR_IDLST    PIC  X(100) VALUE SPACE.
           02  UTRYR_RES      USAGE  POINTER.
       01  UTR-R.
           02  F              PIC  X(015).
           02  UTR-HCD        PIC  9(006).                              品名ｺｰﾄﾞ
           02  F              PIC  X(107).
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
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊　　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊　　履物品名マスター年間使用チェック　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊　　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
               "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  N(013) VALUE
                  "＊　　ＤＡＴＡ　なし　　＊".
             03  E-ME2   PIC  N(013) VALUE
                  "＊　　ＨＩＭ　なし　　＊　".
             03  E-ME3   PIC  N(013) VALUE
                  "＊ＲＥＷＲＩＴＥエラー＊　".
             03  E-ME4   PIC  N(013) VALUE
                  "＊　　ＨＵＨＭ　なし　　＊".
             03  E-KEY   PIC  X(006).
           COPY LIBSCR.
           COPY LSSEM.
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
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "364" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "52" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "52" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "52" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "52" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "52" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "52" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "52" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "110" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "110" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "N" "24" "15" "26" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "N" "24" "15" "26" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" "N" "24" "15" "26" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME4" "N" "24" "15" "26" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "X" "24" "55" "6" "E-ME4" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE HI-KEY "6" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           MOVE W-NG TO W-NGM.
      *
           CALL "DB_F_Open" USING
            "I-O" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HUH-M_PNAME1 "SHARED" BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
       M-10.
      *           READ HI-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF
           MOVE ZERO TO W-NGD.
           MOVE HI-SNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-NG > W-NGM
               MOVE 1 TO HI-DELC
               GO TO M-15
           END-IF
           MOVE HI-KEY TO HUH-KEY.
      *           READ HUH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO HUH-R
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           IF  ZERO = HUH-ZS AND HUH-ZK AND HUH-NS AND HUH-NK AND
                     HUH-SS AND HUH-SK AND HUH-YS AND HUH-YK AND HUH-UG
               MOVE 0 TO HI-DELC
           ELSE
               MOVE 1 TO HI-DELC
           END-IF.
       M-15.
      *           REWRITE HI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HI-M_PNAME1 HI-M_LNAME HI-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-10.
       M-20.
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-M_IDLST HUH-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HIYF_PNAME1 " " BY REFERENCE HIYF_IDLST "0".
       M-25.
      *           READ HIYF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HIYF_PNAME1 BY REFERENCE HIY-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
           IF  ZERO = HIY-ZS AND HIY-ZK AND HIY-NS AND HIY-NK AND
                     HIY-SS AND HIY-SK AND HIY-YS AND HIY-YK AND HIY-UG
               GO TO M-25
           END-IF
           MOVE HIY-HCD TO HI-KEY.
      *           READ HI-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-25
           END-IF
           IF  HI-DELC = 1
               GO TO M-25
           END-IF
           MOVE 1 TO HI-DELC.
      *           REWRITE HI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HI-M_PNAME1 HI-M_LNAME HI-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HIYF_IDLST HIYF_PNAME1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-25.
       M-30.
           CALL "DB_F_Close" USING BY REFERENCE HIYF_IDLST HIYF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" RSTRANYR_PNAME1 " " BY REFERENCE RSTRANYR_IDLST "0".
       M-35.
      *           READ RSTRANYR AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" RSTRANYR_PNAME1 BY REFERENCE STR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           IF  STR-HCD = ZERO
               GO TO M-35
           END-IF
           MOVE STR-HCD TO HI-KEY.
      *           READ HI-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-35
           END-IF
           IF  HI-DELC NOT = 0
               GO TO M-35
           END-IF
           MOVE 1 TO HI-DELC.
      *           REWRITE HI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HI-M_PNAME1 HI-M_LNAME HI-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE RSTRANYR_IDLST RSTRANYR_PNAME1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-35.
       M-40.
           CALL "DB_F_Close" USING
            BY REFERENCE RSTRANYR_IDLST RSTRANYR_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" UTRYR_PNAME1 " " BY REFERENCE UTRYR_IDLST "0".
       M-45.
      *           READ UTRYR AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" UTRYR_PNAME1 BY REFERENCE UTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           MOVE UTR-HCD TO HI-KEY.
      *           READ HI-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-45
           END-IF
           IF  HI-DELC NOT = 0
               GO TO M-45
           END-IF
           MOVE 1 TO HI-DELC.
      *           REWRITE HI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HI-M_PNAME1 HI-M_LNAME HI-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE UTRYR_IDLST UTRYR_PNAME1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-45.
       M-50.
           CALL "DB_F_Close" USING
            BY REFERENCE UTRYR_IDLST UTRYR_PNAME1.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

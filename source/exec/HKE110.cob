       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKE010.
       AUTHOR.     S-NAKAO.
      *********************************************************
      *    PROGRAM         :  担当・分類　年間ファイル更新    *
      *    PRINTER TYPE    :  *****                           *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-SEN          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-NG           PIC  9(006).
           02  W-NGD   REDEFINES W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NEND  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  CHK            PIC  9(001).
       01  W-FIL              PIC  X(009).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
      *FD  NYURYR
       01  NYURYR_HKE110.
           02  NYURYR_PNAME1  PIC  X(006) VALUE "NYURYR".
           02  F              PIC  X(001).
           02  NYURYR_LNAME   PIC  X(013) VALUE "NYURYR_HKE110".
           02  F              PIC  X(001).
           02  NYURYR_KEY1    PIC  X(100) VALUE SPACE.
           02  NYURYR_KEY2    PIC  X(100) VALUE SPACE.
           02  NYURYR_SORT    PIC  X(100) VALUE SPACE.
           02  NYURYR_IDLST   PIC  X(100) VALUE SPACE.
           02  NYURYR_RES     USAGE  POINTER.
       01  NYURY-R.
           02  NYURY-NG       PIC  9(006).
           02  F              PIC  9(002).
           02  NYURY-TCD      PIC  9(004).
           02  F              PIC  X(026).
           02  NYURY-TNC      PIC  9(002).
           02  F              PIC  X(062).
       77  F                  PIC  X(001).
      *FD  STRANYR
       01  STRANYR_HKE110.
           02  STRANYR_PNAME1 PIC  X(007) VALUE "STRANYR".
           02  F              PIC  X(001).
           02  STRANYR_LNAME  PIC  X(014) VALUE "STRANYR_HKE110".
           02  F              PIC  X(001).
           02  STRANYR_KEY1   PIC  X(100) VALUE SPACE.
           02  STRANYR_KEY2   PIC  X(100) VALUE SPACE.
           02  STRANYR_SORT   PIC  X(100) VALUE SPACE.
           02  STRANYR_IDLST  PIC  X(100) VALUE SPACE.
           02  STRANYR_RES    USAGE  POINTER.
       01  STRANY-R.
           02  F              PIC  X(006).
           02  STRANY-GNO     PIC  9(001).
           02  STRANY-NG      PIC  9(006).
           02  F              PIC  9(002).
           02  STRANY-TCD     PIC  9(004).
           02  STRANY-HCD     PIC  9(006).
           02  F              PIC  X(059).
           02  STRANY-BCD     PIC  9(006).
           02  F              PIC  X(001).
           02  STRANY-TNC     PIC  9(002).
           02  F              PIC  X(029).
           02  STRANY-BMC     PIC  9(002).
           02  STRANY-BMNO    PIC  9(001).
           02  F              PIC  X(003).
       77  F                  PIC  X(001).
      *FD  RSTRANYR
       01  RSTRANYR_HKE110.
           02  RSTRANYR_PNAME1 PIC  X(011) VALUE "STRANYR-RDB".
           02  F               PIC  X(001).
           02  RSTRANYR_LNAME  PIC  X(015) VALUE "RSTRANYR_HKE110".
           02  F               PIC  X(001).
           02  RSTRANYR_KEY1   PIC  X(100) VALUE SPACE.
           02  RSTRANYR_KEY2   PIC  X(100) VALUE SPACE.
           02  RSTRANYR_SORT   PIC  X(100) VALUE SPACE.
           02  RSTRANYR_IDLST  PIC  X(100) VALUE SPACE.
           02  RSTRANYR_RES    USAGE  POINTER.
       01  RSTRANY-R.
           02  F              PIC  X(007).
           02  RSTRANY-NG     PIC  9(006).
           02  F              PIC  9(002).
           02  RSTRANY-TCD    PIC  9(004).
           02  RSTRANY-HCD    PIC  9(006).
           02  F              PIC  X(059).
           02  RSTRANY-BCD    PIC  9(006).
           02  F              PIC  X(001).
           02  RSTRANY-TNC    PIC  9(002).
           02  F              PIC  X(029).
           02  RSTRANY-BMC    PIC  9(002).
           02  RSTRANY-BMNO   PIC  9(001).
           02  F              PIC  X(003).
       77  F                  PIC  X(001).
      *FD  UTRYR
       01  UTRYR_HKE110.
           02  UTRYR_PNAME1   PIC  X(009) VALUE "UTRYR-RDB".
           02  F              PIC  X(001).
           02  UTRYR_LNAME    PIC  X(012) VALUE "UTRYR_HKE110".
           02  F              PIC  X(001).
           02  UTRYR_KEY1     PIC  X(100) VALUE SPACE.
           02  UTRYR_KEY2     PIC  X(100) VALUE SPACE.
           02  UTRYR_SORT     PIC  X(100) VALUE SPACE.
           02  UTRYR_IDLST    PIC  X(100) VALUE SPACE.
           02  UTRYR_RES      USAGE  POINTER.
       01  UTRY-R.
           02  F              PIC  X(007).
           02  UTRY-NG        PIC  9(006).
           02  F              PIC  9(002).
           02  UTRY-HCD       PIC  9(006).
           02  F              PIC  X(066).
           02  UTRY-BCD       PIC  9(006).
           02  UTRY-BMC       PIC  9(002).
           02  UTRY-BMNO      PIC  9(001).
           02  F              PIC  X(032).
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
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　担当・分類　年間累積Ｆ変換　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　（　前期まで　）　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(036) VALUE
                "担当=1 , 分類=2 , 担当･分類=3   ﾘﾀｰﾝ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-FIL.
             03  FILLER  PIC  X(001) VALUE "(".
             03  02D-FIL PIC  X(009).
             03  FILLER  PIC  X(001) VALUE ")".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME2   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-TCD   PIC  9(004).
             03  E-HCD   PIC  9(006).
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
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "380" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "46" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "14" "15" "36" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "20" "25" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SEN" "9" "14" "46" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "42" "1" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "11" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-FIL" " " "11" "0" "11" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "01D-FIL" "X" "11" "29" "1" " " "D-FIL" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-FIL" "X" "11" "31" "9" "01D-FIL" " " RETURNING RESU.
       CALL "SD_From" USING
           "02D-FIL" BY REFERENCE W-FIL "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-FIL" "X" "11" "41" "1" "02D-FIL" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "53" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "53" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "15" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "16" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-TCD" "9" "24" "40" "4" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-TCD" BY REFERENCE T-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-HCD" "9" "24" "45" "6" "E-TCD" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-HCD" BY REFERENCE HI-HCD "6" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SEN < 1 OR > 3
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-15
           END-IF
      *
           COPY LIBCPR.
           MOVE ZERO TO W-NG.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-GET < 5
               SUBTRACT 2 FROM W-NEN
           ELSE
               SUBTRACT 1 FROM W-NEN
           END-IF
           MOVE 5 TO W-GET.
           IF  W-SEN NOT = 2
               CALL "DB_F_Open" USING
                "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
                "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2
           END-IF
      *
           IF  W-SEN = 2
               GO TO M-30
           END-IF
           CALL "DB_F_Open" USING
            "I-O" NYURYR_PNAME1 " " BY REFERENCE NYURYR_IDLST "0".
           MOVE " NYURYR  " TO W-FIL.
           CALL "SD_Output" USING "D-FIL" D-FIL "p" RETURNING RESU.
       M-20.
      *           READ NYURYR AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NYURYR_PNAME1 BY REFERENCE NYURY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           IF NYURY-NG < W-NG
               GO TO M-20
           END-IF
           MOVE NYURY-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-20
           END-IF
           IF  T-TNC = NYURY-TNC
               GO TO M-20
           END-IF
           MOVE T-TNC TO NYURY-TNC.
      *           REWRITE NYURY-R.
      *///////////////
           CALL "DB_Update" USING
            NYURYR_PNAME1 NYURYR_LNAME NYURY-R RETURNING RET.
           GO TO M-20.
       M-25.
           CALL "DB_F_Close" USING
            BY REFERENCE NYURYR_IDLST NYURYR_PNAME1.
       M-30.
           IF  W-SEN NOT = 1
               CALL "DB_F_Open" USING
                "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
                "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE
                HI-KEY2
           END-IF
           CALL "DB_F_Open" USING
            "I-O" STRANYR_PNAME1 " " BY REFERENCE STRANYR_IDLST "0".
           MOVE " STRANYR " TO W-FIL.
           CALL "SD_Output" USING "D-FIL" D-FIL "p" RETURNING RESU.
       M-35.
      *           READ STRANYR AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" STRANYR_PNAME1 BY REFERENCE STRANY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  STRANY-NG < W-NG
               GO TO M-35
           END-IF
           IF  STRANY-GNO = 9
               GO TO M-35
           END-IF
           MOVE 0 TO CHK.
           IF  W-SEN = 2
               GO TO M-40
           END-IF
      *
           MOVE STRANY-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-40
           END-IF
           IF  T-TNC = STRANY-TNC
               GO TO M-40
           END-IF
           MOVE T-TNC TO STRANY-TNC.
           MOVE 1 TO CHK.
       M-40.
           IF  W-SEN = 1
               GO TO M-45
           END-IF
           IF  STRANY-HCD = ZERO
               GO TO M-45
           END-IF
      *
           MOVE STRANY-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-45
           END-IF
           IF (HI-BC = STRANY-BCD) AND (HI-BMC = STRANY-BMC) AND
                                        (HI-BMNO = STRANY-BMNO)
               GO TO M-45
           END-IF
           MOVE HI-BC TO STRANY-BCD.
           MOVE HI-BMC TO STRANY-BMC.
           MOVE HI-BMNO TO STRANY-BMNO.
           MOVE 1 TO CHK.
       M-45.
           IF CHK = 1
      *               REWRITE STRANY-R.
      *///////////////
               CALL "DB_Update" USING
                STRANYR_PNAME1 STRANYR_LNAME STRANY-R RETURNING RET
           END-IF
           GO TO M-35.
       M-50.
           CALL "DB_F_Close" USING
            BY REFERENCE STRANYR_IDLST STRANYR_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" RSTRANYR_PNAME1 " " BY REFERENCE RSTRANYR_IDLST "0".
           MOVE "R-STRANYR" TO W-FIL.
           CALL "SD_Output" USING "D-FIL" D-FIL "p" RETURNING RESU.
       M-55.
      *           READ RSTRANYR AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" RSTRANYR_PNAME1 BY REFERENCE RSTRANY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-70
           END-IF
           IF  RSTRANY-NG < W-NG
               GO TO M-55
           END-IF
           MOVE 0 TO CHK.
           IF  W-SEN = 2
               GO TO M-60
           END-IF
      *
           MOVE RSTRANY-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-60
           END-IF
           IF  T-TNC = RSTRANY-TNC
               GO TO M-60
           END-IF
           MOVE T-TNC TO RSTRANY-TNC.
           MOVE 1 TO CHK.
       M-60.
           IF  W-SEN = 1
               GO TO M-65
           END-IF
           IF  RSTRANY-HCD = ZERO
               GO TO M-65
           END-IF
      *
           MOVE RSTRANY-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-65
           END-IF
           IF (HI-BC = RSTRANY-BCD) AND (HI-BMC = RSTRANY-BMC) AND
                                         (HI-BMNO = RSTRANY-BMNO)
               GO TO M-65
           END-IF
           MOVE HI-BC TO RSTRANY-BCD.
           MOVE HI-BMC TO RSTRANY-BMC.
           MOVE HI-BMNO TO RSTRANY-BMNO.
           MOVE 1 TO CHK.
       M-65.
           IF CHK = 1
      *               REWRITE RSTRANY-R.
      *///////////////
               CALL "DB_Update" USING
                RSTRANYR_PNAME1 RSTRANYR_LNAME RSTRANY-R RETURNING RET
           END-IF
           GO TO M-55.
       M-70.
           CALL "DB_F_Close" USING
            BY REFERENCE RSTRANYR_IDLST RSTRANYR_PNAME1.
           IF  W-SEN NOT = 2
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
           END-IF
      *
           IF  W-SEN = 1
               GO TO M-95
           END-IF
           CALL "DB_F_Open" USING
            "I-O" UTRYR_PNAME1 " " BY REFERENCE UTRYR_IDLST "0".
           MOVE "UTRYR    " TO W-FIL.
           CALL "SD_Output" USING "D-FIL" D-FIL "p" RETURNING RESU.
       M-75.
      *           READ UTRYR AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" UTRYR_PNAME1 BY REFERENCE UTRY-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  UTRY-NG < W-NG
               GO TO M-75
           END-IF
           IF  UTRY-HCD = ZERO
               GO TO M-75
           END-IF
      *
           MOVE UTRY-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-75
           END-IF
           IF (HI-BC = UTRY-BCD) AND (HI-BMC = UTRY-BMC) AND
                                      (HI-BMNO = UTRY-BMNO)
               GO TO M-75
           END-IF
           MOVE HI-BC TO UTRY-BCD.
           MOVE HI-BMC TO UTRY-BMC.
           MOVE HI-BMNO TO UTRY-BMNO.
      *
      *           REWRITE UTRY-R.
      *///////////////
           CALL "DB_Update" USING
            UTRYR_PNAME1 UTRYR_LNAME UTRY-R RETURNING RET.
           GO TO M-75.
       M-80.
           CALL "DB_F_Close" USING
            BY REFERENCE UTRYR_IDLST UTRYR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

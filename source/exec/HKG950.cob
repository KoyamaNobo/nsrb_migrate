       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG950.
      *********************************************************
      *    PROGRAM         :  各年間ファイル　最新区分　変換  *
      *    PRINTER TYPE    :  ****                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-DC.
             03  W-DC1        PIC  9(001).
             03  W-DC2        PIC  9(001).
           02  W-NG           PIC  9(006).
           02  W-NGD   REDEFINES  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
       01  SSRY-F_HKG950.
           02  SSRY-F_PNAME1   PIC  X(005)  VALUE "SSRYF".
           02  F               PIC  X(001).
           02  SSRY-F_LNAME    PIC  X(013)  VALUE "SSRY-F_HKG950".
           02  F               PIC  X(001).
           02  SSRY-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  SSRY-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  SSRY-F_SORT     PIC  X(100)  VALUE SPACE.
           02  SSRY-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  SSRY-F_RES      USAGE  POINTER.
       01  SSRY-R.
           02  SSRY-TCD       PIC  9(004).
           02  SSRY-HCD       PIC  9(006).
           02  SSRY-SU        PIC S9(007).
           02  SSRY-UK        PIC S9(010).
           02  SSRY-GK        PIC S9(010).
           02  F              PIC  9(002).
           02  SSRY-TNC       PIC  9(002).
           02  SSRY-BC.
             03  SSRY-BC1     PIC  9(002).
             03  SSRY-BC2     PIC  9(002).
             03  SSRY-BC3     PIC  9(002).
           02  SSRY-BMC       PIC  9(002).
           02  SSRY-BMNO      PIC  9(001).
           02  SSRY-FKC       PIC  9(002).
           02  SSRY-NG        PIC  9(006).
           02  F              PIC  X(006).
       77  F                  PIC  X(001).
       01  SUSRY-F_HKG950.
           02  SUSRY-F_PNAME1   PIC  X(006)  VALUE "SUSRYF".
           02  F                PIC  X(001).
           02  SUSRY-F_LNAME    PIC  X(014)  VALUE "SUSRY-F_HKG950".
           02  F                PIC  X(001).
           02  SUSRY-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  SUSRY-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  SUSRY-F_SORT     PIC  X(100)  VALUE SPACE.
           02  SUSRY-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  SUSRY-F_RES      USAGE  POINTER.
       01  SUSRY-R.
           02  SUSRY-HCD      PIC  9(006).
           02  SUSRY-SU       PIC S9(007).
           02  SUSRY-UK       PIC S9(009).
           02  SUSRY-FK       PIC S9(009).
           02  SUSRY-SC       PIC  9(001).
           02  SUSRY-BC.
             03  SUSRY-BC1    PIC  9(002).
             03  SUSRY-BC2    PIC  9(002).
             03  SUSRY-BC3    PIC  9(002).
           02  SUSRY-BMC      PIC  9(002).
           02  SUSRY-BMNO     PIC  9(001).
           02  SUSRY-NC       PIC  9(001).
           02  F              PIC  X(003).
           02  SUSRY-NG       PIC  9(006).
       77  F                  PIC  X(001).
       01  HPYR-F_HKG950.
           02  HPYR-F_PNAME1   PIC  X(005)  VALUE "HPYRF".
           02  F               PIC  X(001).
           02  HPYR-F_LNAME    PIC  X(013)  VALUE "HPYR-F_HKG950".
           02  F               PIC  X(001).
           02  HPYR-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  HPYR-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  HPYR-F_SORT     PIC  X(100)  VALUE SPACE.
           02  HPYR-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  HPYR-F_RES      USAGE  POINTER.
       01  HPYR-R.
           02  HPYR-TCD       PIC  9(004).
           02  HPYR-HCD       PIC  9(006).
           02  HPYR-SU        PIC S9(007).
           02  HPYR-KIN       PIC S9(010).
           02  HPYR-TNC       PIC  9(002).
           02  HPYR-BC.
             03  HPYR-BC1     PIC  9(002).
             03  HPYR-BC2     PIC  9(002).
             03  HPYR-BC3     PIC  9(002).
           02  F              PIC  X(001).
           02  HPYR-NG        PIC  9(006).
       77  F                  PIC  X(001).
       01  HIY-F_HKG950.
           02  HIY-F_PNAME1   PIC  X(004)  VALUE "HIYF".
           02  F              PIC  X(001).
           02  HIY-F_LNAME    PIC  X(012)  VALUE "HIY-F_HKG950".
           02  F              PIC  X(001).
           02  HIY-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  HIY-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  HIY-F_SORT     PIC  X(100)  VALUE SPACE.
           02  HIY-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  HIY-F_RES      USAGE  POINTER.
       01  HIY-R.
           02  HIY-HCD        PIC  9(006).
           02  HIY-NG         PIC  9(006).
           02  HIY-ZSU        PIC S9(006).
           02  HIY-ZKIN       PIC S9(009).
           02  HIY-SSU        PIC S9(007).
           02  HIY-SKIN       PIC S9(010).
           02  HIY-USU        PIC S9(008).
           02  HIY-UKIN       PIC S9(010).
           02  HIY-YSU        PIC S9(006).
           02  HIY-YKIN       PIC S9(009).
           02  HIY-GKIN       PIC S9(010).
           02  HIY-BC.
             03  HIY-BC1      PIC  9(002).
             03  HIY-BC2      PIC  9(002).
             03  HIY-BC3      PIC  9(002).
           02  HIY-BMC        PIC  9(002).
           02  HIY-BMNO       PIC  9(001).
           02  F              PIC  X(006).
       77  F                  PIC  X(001).
       01  TMY-F_HKG950.
           02  TMY-F_PNAME1   PIC  X(005)  VALUE "TTMYR".
           02  F              PIC  X(001).
           02  TMY-F_LNAME    PIC  X(012)  VALUE "TMY-F_HKG950".
           02  F              PIC  X(001).
           02  TMY-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  TMY-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  TMY-F_SORT     PIC  X(100)  VALUE SPACE.
           02  TMY-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  TMY-F_RES      USAGE  POINTER.
       01  TMY-R.
           02  TMY-TCD        PIC  9(004).
           02  TMY-ZKIN       PIC S9(009).
           02  TMY-ZSHZ       PIC S9(007).
           02  TMY-GKIN       PIC S9(009).
           02  TMY-GSHZ       PIC S9(007).
           02  TMY-UKIN       PIC S9(009).
           02  TMY-USHZ       PIC S9(007).
           02  TMY-BKIN       PIC S9(008).
           02  TMY-BSHZ       PIC S9(006).
           02  TMY-NKIN       PIC S9(009).
           02  TMY-NSHZ       PIC S9(007).
           02  TMY-UGEN       PIC S9(009).
           02  F              PIC  9(002).
           02  TMY-TNC        PIC  9(002).
           02  TMY-FKC        PIC  9(002).
           02  TMY-BMC        PIC  9(001).
           02  F              PIC  X(024).
           02  TMY-NG         PIC  9(006).
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
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　年間ファイル　区分変換　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM     PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(018) VALUE
                  "***  DATEM ﾅｼ  ***".
             03  E-ME2     PIC  X(027) VALUE
                  "***  DATEM REWRITE ｴﾗｰ  ***".
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
             03  E-STAT    PIC  X(002).
           COPY LIBSCR.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "316" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "42" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "20" "20" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "37" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "57" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "57" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "27" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 0 AND 1
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  JS-SIGN = 1
               MOVE 1 TO DATE-HBC DATE-HTNC
                         DATE-HBC1 DATE-HBC2 DATE-HBC3
           END-IF.
           MOVE ZERO TO W-DC.
           IF  DATE-TM NOT = ZERO
               MOVE 1 TO W-DC1
           END-IF.
           IF  DATE-HM NOT = ZERO
               MOVE 1 TO W-DC2
           END-IF.
           IF  W-DC = ZERO
               CALL "DB_Close"
               STOP RUN
           END-IF.
       M-10.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                         RETURNING RESU.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
           IF  W-DMM = 9
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-15
           END-IF.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "A-DMM" A-DMM "p" 
                                         RETURNING RESU.
           MOVE ZERO TO W-NG.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF.
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
           IF  W-GET < 5
               SUBTRACT 4 FROM W-NEN
           ELSE
               SUBTRACT 3 FROM W-NEN
           END-IF.
           MOVE 5 TO W-GET.
           IF  W-DC1 = 1
               CALL "DB_F_Open" USING
                "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
                "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2
           END-IF.
           IF  W-DC2 = 1
               CALL "DB_F_Open" USING
                "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
                "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE
                HI-KEY2
           END-IF.
           PERFORM SSR-RTN THRU SSR-EX.
           PERFORM HPY-RTN THRU HPY-EX.
           IF  W-DC1 = 1
               PERFORM TMY-RTN THRU TMY-EX
           END-IF.
           IF  W-DC2 = 1
               PERFORM SUS-RTN THRU SUS-EX
           END-IF.
           IF  W-DC2 = 1
               PERFORM HIY-RTN THRU HIY-EX
           END-IF.
           IF  W-DC1 = 1
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
           END-IF.
           IF  W-DC2 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
           END-IF.
       M-40.
           CALL "DB_F_Open" USING
            "I-O" M-DATE_PNAME1 " " BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
           MOVE "01" TO DATE-KEY.
      *           READ M-DATE INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" M-DATE_PNAME1 BY REFERENCE DATE-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE M-DATE_IDLST M-DATE_PNAME1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                         RETURNING RESU
               GO TO M-95
           END-IF.
           MOVE ZERO TO DATE-TM DATE-HM.
      *           REWRITE DATE-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            M-DATE_PNAME1 M-DATE_LNAME DATE-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                         RETURNING RESU
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" 
                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                         RETURNING RESU
           END-IF.
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       SSR-RTN.
           CALL "DB_F_Open" USING
            "I-O" SSRY-F_PNAME1 " " BY REFERENCE SSRY-F_IDLST "0".
       SSR-020.
      *           READ SSRY-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSRY-F_PNAME1 BY REFERENCE SSRY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO SSR-080
           END-IF.
           IF  SSRY-NG < W-NG
               GO TO SSR-020
           END-IF.
           MOVE 0 TO CHK.
           IF  W-DC1 = 0
               GO TO SSR-040
           END-IF.
           IF  0 = DATE-HTNC
               GO TO SSR-040
           END-IF.
           MOVE SSRY-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO SSR-040
           END-IF.
           IF (T-TNC NOT = SSRY-TNC) OR (T-FKC NOT = SSRY-FKC)
               MOVE 1 TO CHK
               MOVE T-FKC TO SSRY-FKC
               MOVE T-TNC TO SSRY-TNC
           END-IF.
       SSR-040.
           IF  W-DC2 = 0
               GO TO SSR-060
           END-IF.
           MOVE SSRY-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO SSR-060
           END-IF.
           IF (HI-BC NOT = SSRY-BC) OR (HI-BMC NOT = SSRY-BMC)
               MOVE 1 TO CHK
               MOVE HI-BC TO SSRY-BC
               MOVE HI-BMC TO SSRY-BMC
               MOVE HI-BMNO TO SSRY-BMNO
           END-IF.
       SSR-060.
           IF  CHK = 1
      *               REWRITE SSRY-R.
      *///////////////
               CALL "DB_Update" USING
                SSRY-F_PNAME1 SSRY-F_LNAME SSRY-R RETURNING RET
           END-IF.
           GO TO SSR-020.
       SSR-080.
           CALL "DB_F_Close" USING
            BY REFERENCE SSRY-F_IDLST SSRY-F_PNAME1.
       SSR-EX.
           EXIT.
       HPY-RTN.
           CALL "DB_F_Open" USING
            "I-O" HPYR-F_PNAME1 " " BY REFERENCE HPYR-F_IDLST "0".
       HPY-020.
      *           READ HPYR-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HPYR-F_PNAME1 BY REFERENCE HPYR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO HPY-080
           END-IF.
           IF  HPYR-NG < W-NG
               GO TO HPY-020
           END-IF.
           MOVE 0 TO CHK.
           IF  W-DC1 = 0
               GO TO HPY-040
           END-IF.
           IF  0 = DATE-HTNC
               GO TO HPY-040
           END-IF.
           MOVE HPYR-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO HPY-040
           END-IF.
           IF  T-TNC NOT = HPYR-TNC
               MOVE 1 TO CHK
               MOVE T-TNC TO HPYR-TNC
           END-IF.
       HPY-040.
           IF  W-DC2 = 0
               GO TO HPY-060
           END-IF.
           MOVE HPYR-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO HPY-060
           END-IF.
           IF  HI-BC NOT = HPYR-BC
               MOVE 1 TO CHK
               MOVE HI-BC TO HPYR-BC
           END-IF.
       HPY-060.
           IF  CHK = 1
      *               REWRITE HPYR-R.
      *///////////////
               CALL "DB_Update" USING
                HPYR-F_PNAME1 HPYR-F_LNAME HPYR-R RETURNING RET
           END-IF.
           GO TO HPY-020.
       HPY-080.
           CALL "DB_F_Close" USING
            BY REFERENCE HPYR-F_IDLST HPYR-F_PNAME1.
       HPY-EX.
           EXIT.
       TMY-RTN.
           CALL "DB_F_Open" USING
            "I-O" TMY-F_PNAME1 " " BY REFERENCE TMY-F_IDLST "0".
       TMY-020.
      *           READ TMY-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TMY-F_PNAME1 BY REFERENCE TMY-R " " RETURNING RET.
           IF  RET = 1
               GO TO TMY-040
           END-IF.
           IF  TMY-NG < W-NG
               GO TO TMY-020
           END-IF.
           MOVE TMY-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO TMY-020
           END-IF.
           IF (T-TNC NOT = TMY-TNC) OR
              (T-FKC NOT = TMY-FKC) OR (T-BC  NOT = TMY-BMC)
               MOVE T-TNC TO TMY-TNC
               MOVE T-FKC TO TMY-FKC
               MOVE T-BC TO TMY-BMC
      *               REWRITE TMY-R.
      *///////////////
               CALL "DB_Update" USING
                TMY-F_PNAME1 TMY-F_LNAME TMY-R RETURNING RET
           END-IF.
           GO TO TMY-020.
       TMY-040.
           CALL "DB_F_Close" USING
            BY REFERENCE TMY-F_IDLST TMY-F_PNAME1.
       TMY-EX.
           EXIT.
       SUS-RTN.
           CALL "DB_F_Open" USING
            "I-O" SUSRY-F_PNAME1 " " BY REFERENCE SUSRY-F_IDLST "0".
       SUS-020.
      *           READ SUSRY-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SUSRY-F_PNAME1 BY REFERENCE SUSRY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO SUS-040
           END-IF.
           IF  SUSRY-NG < W-NG
               GO TO SUS-020
           END-IF.
           MOVE SUSRY-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO SUS-020
           END-IF.
           IF (HI-BC NOT = SUSRY-BC) OR (HI-BMC NOT = SUSRY-BMC)
               MOVE HI-BC TO SUSRY-BC
               MOVE HI-BMC TO SUSRY-BMC
               MOVE HI-BMNO TO SUSRY-BMNO
      *               REWRITE SUSRY-R.
      *///////////////
               CALL "DB_Update" USING
                SUSRY-F_PNAME1 SUSRY-F_LNAME SUSRY-R RETURNING RET
           END-IF.
           GO TO SUS-020.
       SUS-040.
           CALL "DB_F_Close" USING
            BY REFERENCE SUSRY-F_IDLST SUSRY-F_PNAME1.
       SUS-EX.
           EXIT.
       HIY-RTN.
           CALL "DB_F_Open" USING
            "I-O" HIY-F_PNAME1 " " BY REFERENCE HIY-F_IDLST "0".
       HIY-020.
      *           READ HIY-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HIY-F_PNAME1 BY REFERENCE HIY-R " " RETURNING RET.
           IF  RET = 1
               GO TO HIY-040
           END-IF.
           IF  HIY-NG < W-NG
               GO TO HIY-020
           END-IF.
           MOVE HIY-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  HIY-020
           END-IF.
           IF (HI-BC NOT = HIY-BC) OR (HI-BMC NOT = HIY-BMC)
               MOVE HI-BC TO HIY-BC
               MOVE HI-BMC TO HIY-BMC
               MOVE HI-BMNO TO HIY-BMNO
      *               REWRITE HIY-R.
      *///////////////
               CALL "DB_Update" USING
                HIY-F_PNAME1 HIY-F_LNAME HIY-R RETURNING RET
           END-IF.
           GO TO HIY-020.
       HIY-040.
           CALL "DB_F_Close" USING
            BY REFERENCE HIY-F_IDLST HIY-F_PNAME1.
       HIY-EX.
           EXIT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG920.
      *********************************************************
      *    PROGRAM         :  —š•¨ŒŽŽŸ”NŠÔ—ÝÏ                *
      *    PRINTER TYPE    :  *****                           *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  1=SUSRYF , 2=HIYF , 3=SSRYF ,   *
      *                    :  4=HPYRF                         *
      *                    :  7=TAZMYR , 8=TMYR , 9=HIMYR     *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-FILE             PIC  X(013).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-MSG          PIC  N(008) VALUE SPACE.
           02  W-END          PIC  9(001) VALUE 0.
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
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHUHM.
           COPY LITM.
           COPY LIHIM.
      *FD  SUSRYF
       01  SUSRYF_HMG920.
           02  SUSRYF_PNAME1  PIC  X(006) VALUE "SUSRYF".
           02  F              PIC  X(001).
           02  SUSRYF_LNAME   PIC  X(013) VALUE "SUSRYF_HMG920".
           02  F              PIC  X(001).
           02  SUSRYF_KEY1    PIC  X(100) VALUE SPACE.
           02  SUSRYF_KEY2    PIC  X(100) VALUE SPACE.
           02  SUSRYF_SORT    PIC  X(100) VALUE SPACE.
           02  SUSRYF_IDLST   PIC  X(100) VALUE SPACE.
           02  SUSRYF_RES     USAGE  POINTER.
       01  SUSRY-R            PIC  X(051).
       77  F                  PIC  X(001).
      *FD  HIYF
       01  HIYF_HMG920.
           02  HIYF_PNAME1    PIC  X(004) VALUE "HIYF".
           02  F              PIC  X(001).
           02  HIYF_LNAME     PIC  X(011) VALUE "HIYF_HMG920".
           02  F              PIC  X(001).
           02  HIYF_KEY1      PIC  X(100) VALUE SPACE.
           02  HIYF_KEY2      PIC  X(100) VALUE SPACE.
           02  HIYF_SORT      PIC  X(100) VALUE SPACE.
           02  HIYF_IDLST     PIC  X(100) VALUE SPACE.
           02  HIYF_RES       USAGE  POINTER.
       01  HIY-R              PIC  X(102).
       77  F                  PIC  X(001).
      *FD  SSRYF
       01  SSRYF_HMG920.
           02  SSRYF_PNAME1   PIC  X(005) VALUE "SSRYF".
           02  F              PIC  X(001).
           02  SSRYF_LNAME    PIC  X(012) VALUE "SSRYF_HMG920".
           02  F              PIC  X(001).
           02  SSRYF_KEY1     PIC  X(100) VALUE SPACE.
           02  SSRYF_SORT     PIC  X(100) VALUE SPACE.
           02  SSRYF_IDLST    PIC  X(100) VALUE SPACE.
           02  SSRYF_RES      USAGE  POINTER.
       01  SSRY-R             PIC  X(064).
       77  F                  PIC  X(001).
      *FD  HPYRF
       01  HPYRF_HMG920.
           02  HPYRF_PNAME1   PIC  X(005) VALUE "HPYRF".
           02  F              PIC  X(001).
           02  HPYRF_LNAME    PIC  X(012) VALUE "HPYRF_HMG920".
           02  F              PIC  X(001).
           02  HPYRF_KEY1     PIC  X(100) VALUE SPACE.
           02  HPYRF_SORT     PIC  X(100) VALUE SPACE.
           02  HPYRF_IDLST    PIC  X(100) VALUE SPACE.
           02  HPYRF_RES      USAGE  POINTER.
       01  HPYR-R             PIC  X(042).
       77  F                  PIC  X(001).
      *FD  DATA-F
       01  DATA-F_HMG920.
           02  DATA-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  DATA-F_LNAME   PIC  X(013) VALUE "DATA-F_HMG920".
           02  F              PIC  X(001).
           02  DATA-F_KEY1    PIC  X(100) VALUE SPACE.
           02  DATA-F_KEY2    PIC  X(100) VALUE SPACE.
           02  DATA-F_SORT    PIC  X(100) VALUE SPACE.
           02  DATA-F_IDLST   PIC  X(100) VALUE SPACE.
           02  DATA-F_RES     USAGE  POINTER.
       01  DATA-R.
           02  DATA-D         PIC  X(064).
           02  DATA-42D  REDEFINES DATA-D.
             03  DATA-42      PIC  X(042).
             03  F            PIC  X(022).
           02  DATA-51D  REDEFINES DATA-D.
             03  DATA-51      PIC  X(051).
             03  F            PIC  X(013).
           02  DATA-64D  REDEFINES DATA-D.
             03  DATA-64      PIC  X(064).
       77  F                  PIC  X(001).
      *FD  TAZ-M
       01  TAZ-M_HMG920.
           02  TAZ-M_PNAME1   PIC  X(004) VALUE "TAZM".
           02  F              PIC  X(001).
           02  TAZ-M_LNAME    PIC  X(012) VALUE "TAZ-M_HMG920".
           02  F              PIC  X(001).
           02  TAZ-M_KEY1     PIC  X(100) VALUE SPACE.
           02  TAZ-M_SORT     PIC  X(100) VALUE SPACE.
           02  TAZ-M_IDLST    PIC  X(100) VALUE SPACE.
           02  TAZ-M_RES      USAGE  POINTER.
       01  TAZ-R.
           02  TAZ-KEY        PIC  X(010).
           02  F              PIC  X(032).
       77  F                  PIC  X(001).
      *FD  TAZMYR
       01  TAZMYR_HMG920.
           02  TAZMYR_PNAME1  PIC  X(006) VALUE "TAZMYR".
           02  F              PIC  X(001).
           02  TAZMYR_LNAME   PIC  X(013) VALUE "TAZMYR_HMG920".
           02  F              PIC  X(001).
           02  TAZMYR_KEY1    PIC  X(100) VALUE SPACE.
           02  TAZMYR_KEY2    PIC  X(100) VALUE SPACE.
           02  TAZMYR_SORT    PIC  X(100) VALUE SPACE.
           02  TAZMYR_IDLST   PIC  X(100) VALUE SPACE.
           02  TAZMYR_RES     USAGE  POINTER.
       01  TAZMY-R            PIC  X(042).
       77  F                  PIC  X(001).
      *FD  TMYR
       01  TMYR_HMG920.
           02  TMYR_PNAME1    PIC  X(004) VALUE "TMYR".
           02  F              PIC  X(001).
           02  TMYR_LNAME     PIC  X(012) VALUE "TMYR_HMG920".
           02  F              PIC  X(001).
           02  TMYR_KEY1      PIC  X(100) VALUE SPACE.
           02  TMYR_SORT      PIC  X(100) VALUE SPACE.
           02  TMYR_IDLST     PIC  X(100) VALUE SPACE.
           02  TMYR_RES       USAGE  POINTER.
       01  TMY-R.
           02  F              PIC  X(456).
           02  TMY-NG         PIC  9(006).
           02  F              PIC  X(050).
       77  F                  PIC  X(001).
      *FD  HIMYR
       01  HIMYR_HMG920.
           02  HIMYR_PNAME1   PIC  X(005) VALUE "HIMYR".
           02  F              PIC  X(001).
           02  HIMYR_LNAME    PIC  X(012) VALUE "HIMYR_HMG920".
           02  F              PIC  X(001).
           02  HIMYR_KEY1     PIC  X(100) VALUE SPACE.
           02  HIMYR_SORT     PIC  X(100) VALUE SPACE.
           02  HIMYR_IDLST    PIC  X(100) VALUE SPACE.
           02  HIMYR_RES      USAGE  POINTER.
       01  HIMY-R.
           02  F              PIC  X(236).
           02  HIMY-NG        PIC  9(006).
           02  F              PIC  X(014).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(019) VALUE
                "–––––––––––––––––––".
           02  FILLER  PIC  N(019) VALUE
                "–––––––––––––––––––".
           02  FILLER  PIC  N(019) VALUE
                "–––@@@@@@@@@@@@@–––".
           02  FILLER  PIC  N(019) VALUE
                "–––@@—š•¨ŒŽŽŸ@”NŠÔ—ÝÏ@@–––".
           02  FILLER  PIC  N(019) VALUE
                "–––@@@@@@@@@@@@@–––".
           02  FILLER  PIC  N(019) VALUE
                "–––––––––––––––––––".
           02  FILLER  PIC  N(019) VALUE
                "–––––––––––––––––––".
       01  C-DSP.
           02  D-MSG   PIC  N(008).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(030) VALUE
                  "***  SUSRYF WRITE ´×°  ***    ".
             03  E-ME2   PIC  X(030) VALUE
                  "***  HIYF WRITE ´×°  ***      ".
             03  E-ME3   PIC  X(030) VALUE
                  "***  SSRYF WRITE ´×°  ***     ".
             03  E-ME4   PIC  X(030) VALUE
                  "***  HPYRF WRITE ´×°  ***     ".
             03  E-ME7   PIC  X(030) VALUE
                  "***  TAZMYR WRITE ´×°  ***    ".
             03  E-ME8   PIC  X(030) VALUE
                  "***  TMYR WRITE ´×°  ***      ".
             03  E-ME9   PIC  X(030) VALUE
                  "***  HIMYR WRITE ´×°  ***     ".
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
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "266" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "38" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "38" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "38" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "38" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "38" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "38" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "38" "06C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "16" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MSG" "N" "7" "22" "16" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-MSG" BY REFERENCE W-MSG "16" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "210" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "210" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "30" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "30" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "30" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "30" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "30" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "30" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "30" "E-ME8" " " RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 1 AND 2 AND 3 AND 4 AND 7 AND 8 AND 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "DB_F_Open" USING
                "EXTEND" SUSRYF_PNAME1 " " BY REFERENCE SUSRYF_IDLST "0"
               MOVE "i‚r‚t‚r‚q‚x‚ej" TO W-MSG
           END-IF
           IF  JS-SIGN = 2
               CALL "DB_F_Open" USING
                "INPUT" HUH-M_PNAME1 " " BY REFERENCE HUH-M_IDLST "1"
                "HUH-KEY" BY REFERENCE HUH-KEY
               CALL "DB_F_Open" USING
                "EXTEND" HIYF_PNAME1 " " BY REFERENCE HIYF_IDLST "0"
               MOVE "i‚g‚h‚x‚ej@@" TO W-MSG
           END-IF
           IF  JS-SIGN = 3
               CALL "DB_F_Open" USING
                "EXTEND" SSRYF_PNAME1 " " BY REFERENCE SSRYF_IDLST "0"
               MOVE "i‚r‚r‚q‚x‚ej@" TO W-MSG
           END-IF
           IF  JS-SIGN = 4
               CALL "DB_F_Open" USING
                "EXTEND" HPYRF_PNAME1 " " BY REFERENCE HPYRF_IDLST "0"
               MOVE "i‚g‚o‚x‚q‚ej@" TO W-MSG
           END-IF
           IF  JS-SIGN = 7
               CALL "DB_F_Open" USING
                "INPUT" TAZ-M_PNAME1 " " BY REFERENCE TAZ-M_IDLST "1"
                "TAZ-KEY" BY REFERENCE TAZ-KEY
               CALL "DB_F_Open" USING
                "EXTEND" TAZMYR_PNAME1 " " BY REFERENCE TAZMYR_IDLST "0"
               MOVE "i‚s‚`‚y‚l‚x‚qj" TO W-MSG
           END-IF
           IF  JS-SIGN = 8
               CALL "DB_F_Open" USING
                "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
                "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2
               CALL "DB_F_Open" USING
                "EXTEND" TMYR_PNAME1 " " BY REFERENCE TMYR_IDLST "0"
               MOVE "i‚s‚l‚x‚qj@@" TO W-MSG
           END-IF
           IF  JS-SIGN = 9
               CALL "DB_F_Open" USING
                "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
                "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE
                HI-KEY2
               CALL "DB_F_Open" USING
                "EXTEND" HIMYR_PNAME1 " " BY REFERENCE HIMYR_IDLST "0"
               MOVE "i‚g‚h‚l‚x‚qj@" TO W-MSG
           END-IF
           CALL "SD_Output" USING "D-MSG" D-MSG "p" RETURNING RESU.
           IF  JS-SIGN = 2
               GO TO M-20
           END-IF
           IF  JS-SIGN = 7
               GO TO M-30
           END-IF
           IF  JS-SIGN = 8 OR 9
               GO TO M-40
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO DATA-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" DATA-F_PNAME1 " " BY REFERENCE DATA-F_IDLST "0".
       M-10.
      *           READ DATA-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" DATA-F_PNAME1 BY REFERENCE DATA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JS-SIGN = 1
               PERFORM SUS-RTN THRU SUS-EX
           END-IF
           IF  JS-SIGN = 3
               PERFORM SSR-RTN THRU SSR-EX
           END-IF
           IF  JS-SIGN = 4
               PERFORM HPY-RTN THRU HPY-EX
           END-IF
           IF  W-END = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-90
           END-IF
           GO TO M-10.
       M-20.
      *           READ HUH-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HUH-M_PNAME1 BY REFERENCE HUH-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           PERFORM HIY-RTN THRU HIY-EX.
           IF  W-END = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-90
           END-IF
           GO TO M-20.
       M-30.
      *           READ TAZ-M NEXT RECORD AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TAZ-M_PNAME1 BY REFERENCE TAZ-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           PERFORM TAZ-RTN THRU TAZ-EX.
           IF  W-END = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-90
           END-IF
           GO TO M-30.
       M-40.
           MOVE ZERO TO W-NG.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  JS-SIGN NOT = 8
               GO TO M-60
           END-IF.
       M-50.
      *           READ T-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" T-M_PNAME1 BY REFERENCE T-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           PERFORM TM-RTN THRU TM-EX.
           IF  W-END = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-90
           END-IF
           GO TO M-50.
       M-60.
      *           READ HI-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           PERFORM HIM-RTN THRU HIM-EX.
           IF  W-END = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-90
           END-IF
           GO TO M-60.
       M-90.
           IF  JS-SIGN = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SUSRYF_IDLST SUSRYF_PNAME1
           END-IF
           IF  JS-SIGN = 2
               CALL "DB_F_Close" USING
                BY REFERENCE HUH-M_IDLST HUH-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HIYF_IDLST HIYF_PNAME1
           END-IF
           IF  JS-SIGN = 3
               CALL "DB_F_Close" USING
                BY REFERENCE SSRYF_IDLST SSRYF_PNAME1
           END-IF
           IF  JS-SIGN = 4
               CALL "DB_F_Close" USING
                BY REFERENCE HPYRF_IDLST HPYRF_PNAME1
           END-IF
           IF  JS-SIGN = 7
               CALL "DB_F_Close" USING
                BY REFERENCE TAZ-M_IDLST TAZ-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TAZMYR_IDLST TAZMYR_PNAME1
           END-IF
           IF  JS-SIGN = 8
               CALL "DB_F_Close" USING
                BY REFERENCE T-M_IDLST T-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TMYR_IDLST TMYR_PNAME1
           END-IF
           IF  JS-SIGN = 9
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HIMYR_IDLST HIMYR_PNAME1
           END-IF
           IF  JS-SIGN = 1 OR 3 OR 4
               CALL "DB_F_Close" USING
                BY REFERENCE DATA-F_IDLST DATA-F_PNAME1
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       SUS-RTN.
           INITIALIZE SUSRY-R.
           MOVE DATA-51 TO SUSRY-R.
      *           WRITE SUSRY-R.
      *//////////////
           CALL "DB_Insert" USING
            SUSRYF_PNAME1 SUSRYF_LNAME SUSRY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO SUS-EX
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO SUS-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE SUSRYF_IDLST SUSRYF_PNAME1.
           MOVE "SUSRYF       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" SUSRYF_PNAME1 " " BY REFERENCE SUSRYF_IDLST "0".
           GO TO SUS-RTN.
       SUS-EX.
           EXIT.
       SSR-RTN.
           INITIALIZE SSRY-R.
           MOVE DATA-64 TO SSRY-R.
      *           WRITE SSRY-R.
      *//////////////
           CALL "DB_Insert" USING
            SSRYF_PNAME1 SSRYF_LNAME SSRY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO SSR-EX
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME3" E-ME3 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO SSR-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE SSRYF_IDLST SSRYF_PNAME1.
           MOVE "SSRYF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" SSRYF_PNAME1 " " BY REFERENCE SSRYF_IDLST "0".
           GO TO SSR-RTN.
       SSR-EX.
           EXIT.
       HPY-RTN.
           INITIALIZE HPYR-R.
           MOVE DATA-42 TO HPYR-R.
      *           WRITE HPYR-R.
      *//////////////
           CALL "DB_Insert" USING
            HPYRF_PNAME1 HPYRF_LNAME HPYR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO HPY-EX
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME4" E-ME4 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO HPY-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE HPYRF_IDLST HPYRF_PNAME1.
           MOVE "HPYRF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" HPYRF_PNAME1 " " BY REFERENCE HPYRF_IDLST "0".
           GO TO HPY-RTN.
       HPY-EX.
           EXIT.
       HIY-RTN.
           INITIALIZE HIY-R.
           MOVE HUH-R TO HIY-R.
      *           WRITE HIY-R.
      *//////////////
           CALL "DB_Insert" USING
            HIYF_PNAME1 HIYF_LNAME HIY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO HIY-EX
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO HIY-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE HIYF_IDLST HIYF_PNAME1.
           MOVE "HIYF         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" HIYF_PNAME1 " " BY REFERENCE HIYF_IDLST "0".
           GO TO HIY-RTN.
       HIY-EX.
           EXIT.
       TAZ-RTN.
           INITIALIZE TAZMY-R.
           MOVE TAZ-R TO TAZMY-R.
      *           WRITE TAZMY-R.
      *//////////////
           CALL "DB_Insert" USING
            TAZMYR_PNAME1 TAZMYR_LNAME TAZMY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO TAZ-EX
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME7" E-ME7 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO TAZ-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TAZMYR_IDLST TAZMYR_PNAME1.
           MOVE "TAZMYR       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" TAZMYR_PNAME1 " " BY REFERENCE TAZMYR_IDLST "0".
           GO TO TAZ-RTN.
       TAZ-EX.
           EXIT.
       TM-RTN.
           INITIALIZE TMY-R.
           MOVE T-R TO TMY-R.
           MOVE W-NG TO TMY-NG.
      *           WRITE TMY-R.
      *//////////////
           CALL "DB_Insert" USING
            TMYR_PNAME1 TMYR_LNAME TMY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO TM-EX
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME8" E-ME8 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO TM-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE TMYR_IDLST TMYR_PNAME1.
           MOVE "TMYR         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" TMYR_PNAME1 " " BY REFERENCE TMYR_IDLST "0".
           GO TO TM-RTN.
       TM-EX.
           EXIT.
       HIM-RTN.
           INITIALIZE HIMY-R.
           MOVE HI-R TO HIMY-R.
           MOVE W-NG TO HIMY-NG.
      *           WRITE HIMY-R.
      *//////////////
           CALL "DB_Insert" USING
            HIMYR_PNAME1 HIMYR_LNAME HIMY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO HIM-EX
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME9" E-ME9 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO HIM-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE HIMYR_IDLST HIMYR_PNAME1.
           MOVE "HIMYR        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" HIMYR_PNAME1 " " BY REFERENCE HIMYR_IDLST "0".
           GO TO HIM-RTN.
       HIM-EX.
           EXIT.

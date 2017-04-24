       IDENTIFICATION DIVISION.
       PROGRAM-ID. HN013M.
      ***************************************************************
      *    PROGRAM         :  ìæà”êÊïiñºíPâøÉ}ÉXÉ^Å@íäèo            *
      *                       ÇsÇgÇsÇlÇPÅ@Å®Å@ÇvÇjÇOÇOÇUÇSÇOÇOÇO    *
      ***************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM7200.
       OBJECT-COMPUTER. SYSTEM7200.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-TCD          PIC  9(004).
           02  W-ATCDD.
             03  W-ATCD  OCCURS   16.
               04  W-TCDF     PIC  9(004).
               04  W-TCDT     PIC  9(004).
           02  W-HCD          PIC  9(004).
           02  W-AHCDD.
             03  W-AHCD  OCCURS   16.
               04  W-HCDF     PIC  9(004).
               04  W-HCDT     PIC  9(004).
           02  W-L            PIC  9(002).
           02  W-C1           PIC  9(002).
           02  W-C2           PIC  9(002).
           02  W-C3           PIC  9(002).
           02  W-C4           PIC  9(002).
           02  CNT            PIC  9(002).
           02  W-EC           PIC  9(001).
           02  W-AC           PIC  9(001).
           02  CNTD           PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-GC           PIC  9(002).
           02  W-NC           PIC  9(001).
           02  W-TC           PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-SC           PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY    LITHTM.
      *FD  HENKAN
       01  HENKAN_HN013M.
           02  HENKAN_PNAME1  PIC  X(009) VALUE "WK0064000".
           02  F              PIC  X(001).
           02  HENKAN_LNAME   PIC  X(013) VALUE "HENKAN_HN013M".
           02  F              PIC  X(001).
           02  HENKAN_KEY1    PIC  X(100) VALUE SPACE.
           02  HENKAN_SORT    PIC  X(100) VALUE SPACE.
           02  HENKAN_IDLST   PIC  X(100) VALUE SPACE.
           02  HENKAN_RES     USAGE  POINTER.
       01  HENKAN-R.
           02  HENKAN-KEY.
             03  HENKAN-TCD     PIC  X(004).
             03  HENKAN-HCD     PIC  X(006).
             03  HENKAN-HCDD  REDEFINES HENKAN-HCD.
               04  HENKAN-HCD1  PIC  X(004).
               04  HENKAN-HCD2  PIC  X(002).
             03  HENKAN-SIZ     PIC  X(001).
           02  HENKAN-NEWT      PIC  9(005).
           02  F                PIC  X(048).
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
                "ÅñÅñÅñÅ@Å@ìæà”êÊïiñºï íPâøÉtÉ@ÉCÉãÅ@íäèoÅ@Å@ÅñÅñÅñ".
           02  FILLER.
             03  FILLER  PIC  N(003) VALUE "ìæà”êÊ".
             03  FILLER  PIC  N(003) VALUE "ïiÅ@ñº".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(002) VALUE "èIóπ".
             03  FILLER  PIC  X(004) VALUE ": F9".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(001) VALUE "Å`".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(004) VALUE "à»â∫Ç»Çµ".
             03  FILLER  PIC  X(005) VALUE ": F10".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(001) VALUE "Å`".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(001) VALUE "Å`".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(001) VALUE "Å`".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(001) VALUE "Å`".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(001) VALUE "Å`".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(001) VALUE "Å`".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(001) VALUE "Å`".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(001) VALUE "Å`".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(001) VALUE "Å`".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(001) VALUE "Å`".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(001) VALUE "Å`".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(001) VALUE "Å`".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  N(001) VALUE "Å`".
           02  FILLER  PIC  X(022) VALUE
                "ämîF  OK=1 NO=9   ÿ¿∞›".
       01  C-ACP.
           02  FILLER.
             03  A-TCDF  PIC  9(004).
             03  A-TCDT  PIC  9(004).
             03  A-HCDF  PIC  9(004).
             03  A-HCDT  PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  S-TCDF  PIC  X(004) VALUE "    ".
             03  S-TCDT  PIC  X(004) VALUE "    ".
             03  S-HCDF  PIC  X(004) VALUE "    ".
             03  S-HCDT  PIC  X(004) VALUE "    ".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ≈º  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ¥◊∞  ***".
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "169" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "1" "10" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" " " "4" "0" "12" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "4" "22" "6" " " "02C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "4" "42" "6" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" " " "5" "0" "12" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "5" "24" "2" " " "05C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "5" "44" "2" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "N" "5" "60" "4" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "5" "64" "4" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "10C-MID" " " "6" "0" "4" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "11C-MID" "N" "6" "24" "2" " " "10C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "12C-MID" "N" "6" "44" "2" "11C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "13C-MID" " " "7" "0" "17" "10C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "14C-MID" "N" "7" "24" "2" " " "13C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "15C-MID" "N" "7" "44" "2" "14C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "16C-MID" "N" "7" "60" "8" "15C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "17C-MID" "X" "7" "68" "5" "16C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "18C-MID" " " "8" "0" "4" "13C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "19C-MID" "N" "8" "24" "2" " " "18C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "20C-MID" "N" "8" "44" "2" "19C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "21C-MID" " " "9" "0" "4" "18C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "22C-MID" "N" "9" "24" "2" " " "21C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "23C-MID" "N" "9" "44" "2" "22C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "24C-MID" " " "10" "0" "4" "21C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "25C-MID" "N" "10" "24" "2" " " "24C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "26C-MID" "N" "10" "44" "2" "25C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "27C-MID" " " "11" "0" "4" "24C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "28C-MID" "N" "11" "24" "2" " " "27C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "29C-MID" "N" "11" "44" "2" "28C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "30C-MID" " " "12" "0" "4" "27C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "31C-MID" "N" "12" "24" "2" " " "30C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "32C-MID" "N" "12" "44" "2" "31C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "33C-MID" " " "13" "0" "4" "30C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "34C-MID" "N" "13" "24" "2" " " "33C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "35C-MID" "N" "13" "44" "2" "34C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "36C-MID" " " "14" "0" "4" "33C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "37C-MID" "N" "14" "24" "2" " " "36C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "38C-MID" "N" "14" "44" "2" "37C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "39C-MID" " " "15" "0" "4" "36C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "40C-MID" "N" "15" "24" "2" " " "39C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "41C-MID" "N" "15" "44" "2" "40C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "42C-MID" " " "16" "0" "4" "39C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "43C-MID" "N" "16" "24" "2" " " "42C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "44C-MID" "N" "16" "44" "2" "43C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "45C-MID" " " "17" "0" "4" "42C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "46C-MID" "N" "17" "24" "2" " " "45C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "47C-MID" "N" "17" "44" "2" "46C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "48C-MID" " " "18" "0" "4" "45C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "49C-MID" "N" "18" "24" "2" " " "48C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "50C-MID" "N" "18" "44" "2" "49C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "51C-MID" " " "19" "0" "4" "48C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "52C-MID" "N" "19" "24" "2" " " "51C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "53C-MID" "N" "19" "44" "2" "52C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "54C-MID" " " "20" "0" "4" "51C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "55C-MID" "N" "20" "24" "2" " " "54C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "56C-MID" "N" "20" "44" "2" "55C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "57C-MID" "X" "23" "40" "22" "54C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "17" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "W-L" "0" "16" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-TCDF" "9" "W-L" "W-C1" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-TCDF" BY REFERENCE W-TCDF(1) "4" "1" BY REFERENCE CNT 8
            RETURNING RESU.
       CALL "SD_Init" USING
           "A-TCDT" "9" "W-L" "W-C2" "4" "A-TCDF" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-TCDT" BY REFERENCE W-TCDT(1) "4" "1" BY REFERENCE CNT 8
            RETURNING RESU.
       CALL "SD_Init" USING
           "A-HCDF" "9" "W-L" "W-C3" "4" "A-TCDT" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-HCDF" BY REFERENCE W-HCDF(1) "4" "1" BY REFERENCE CNT 8
            RETURNING RESU.
       CALL "SD_Init" USING
           "A-HCDT" "9" "W-L" "W-C4" "4" "A-HCDF" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-HCDT" BY REFERENCE W-HCDT(1) "4" "1" BY REFERENCE CNT 8
            RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "57" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "16" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-DSP" " " "W-L" "0" "16" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "S-TCDF" "X" "W-L" "W-C1" "4" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "S-TCDT" "X" "W-L" "W-C2" "4" "S-TCDF" " " RETURNING RESU.
       CALL "SD_Init" USING
           "S-HCDF" "X" "W-L" "W-C3" "4" "S-TCDT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "S-HCDT" "X" "W-L" "W-C4" "4" "S-HCDF" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "97" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "97" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
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
           PERFORM CR0-RTN THRU CR0-EX
           IF  W-EC NOT = 0
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
           CALL "DB_F_Open" USING
            "OUTPUT" HENKAN_PNAME1 " " BY REFERENCE HENKAN_IDLST "0".
           MOVE SPACE TO THT-KEY.
           MOVE W-TCDF(1) TO THT-TCD.
      *           START THTM KEY NOT < THT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            THTM_PNAME1 "THT-KEY" " NOT < " THT-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-10.
      *           READ THTM NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           PERFORM CHK-RTN THRU CHK-EX.
           IF  CHK = 0
               GO TO M-10
           END-IF.
       M-15.
           MOVE SPACE TO HENKAN-R.
           MOVE THT-KEY TO HENKAN-KEY.
           MOVE THT-T TO HENKAN-NEWT.
      *           WRITE HENKAN-R.
      *//////////////
           CALL "DB_Insert" USING
            HENKAN_PNAME1 HENKAN_LNAME HENKAN-R RETURNING RET.
       M-20.
      *           READ THTM NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           PERFORM CHK-RTN THRU CHK-EX.
           IF  CHK = 0
               GO TO M-20
           END-IF
           GO TO M-15.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HENKAN_IDLST HENKAN_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       CHK-RTN.
           MOVE ZERO TO CNT CHK.
       CHK-010.
           ADD 1 TO CNT.
           IF  CNT = 17
               GO TO CHK-EX
           END-IF
           IF  W-TCDT(CNT) NOT = ZERO
               IF  THT-TCD >= W-TCDF(CNT) AND <= W-TCDT(CNT)
                   GO TO CHK-020
               END-IF
           END-IF
           GO TO CHK-010.
       CHK-020.
           MOVE ZERO TO CNT.
       CHK-030.
           ADD 1 TO CNT.
           IF  CNT = 17
               GO TO CHK-EX
           END-IF
           IF  W-HCDT(CNT) NOT = ZERO
               IF  THT-HCDF >= W-HCDF(CNT) AND <= W-HCDT(CNT)
                   MOVE 1 TO CHK
                   GO TO CHK-EX
               END-IF
           END-IF
           GO TO CHK-030.
       CHK-EX.
           EXIT.
       CR0-RTN.
           MOVE 0 TO W-EC.
           MOVE 19 TO W-C1.
           CALL "SD_Arg_Match_Col" USING "W-C1" "2" W-C1 RETURNING RESU.
           MOVE 27 TO W-C2.
           CALL "SD_Arg_Match_Col" USING "W-C2" "2" W-C2 RETURNING RESU.
           MOVE 39 TO W-C3.
           CALL "SD_Arg_Match_Col" USING "W-C3" "2" W-C3 RETURNING RESU.
           MOVE 47 TO W-C4.
           CALL "SD_Arg_Match_Col" USING "W-C4" "2" W-C4 RETURNING RESU.
       CR0-010.
           MOVE ZERO TO CNT W-AC.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       CR0-020.
           ADD 1 TO CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT > 16
               GO TO CR0-120
           END-IF.
       CR0-040.
           IF  W-AC = 1
               MOVE ZERO TO W-TCDF(CNT)
               CALL "SD_Output" USING
                "S-TCDF" S-TCDF "p" RETURNING RESU
               GO TO CR0-060
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-TCDF "A-TCDF" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               MOVE 1 TO W-EC
               GO TO CR0-EX
           END-IF
           IF  ESTAT = ADV
               MOVE 1 TO W-AC
               MOVE ZERO TO W-TCDF(CNT)
               CALL "SD_Output" USING
                "S-TCDF" S-TCDF "p" RETURNING RESU
               GO TO CR0-060
           END-IF
           IF  ESTAT = BTB
               SUBTRACT 1 FROM CNT W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               IF  CNT = ZERO
                   GO TO CR0-020
               ELSE
                   GO TO CR0-060
               END-IF
           END-IF
           IF ESTAT NOT = HTB AND SKP
               GO TO CR0-040
           END-IF
           IF (CNT NOT = 1) AND (W-TCDF(CNT) = 9999)
               MOVE 1 TO W-AC
               MOVE ZERO TO W-TCDF(CNT)
               CALL "SD_Output" USING
                "S-TCDF" S-TCDF "p" RETURNING RESU
               GO TO CR0-060
           END-IF
           IF (CNT NOT = 1) AND (W-TCDF(CNT) NOT = ZERO)
               COMPUTE CNTD = CNT - 1
               IF  W-TCDT(CNTD) >= W-TCDF(CNT)
                   GO TO CR0-040
               END-IF
           END-IF.
       CR0-060.
           IF  W-AC = 1
               MOVE ZERO TO W-TCDT(CNT)
               CALL "SD_Output" USING
                "S-TCDT" S-TCDT "p" RETURNING RESU
               GO TO CR0-020
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-TCDT "A-TCDT" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO CR0-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO CR0-060
           END-IF
           IF  W-TCDF(CNT) > W-TCDT(CNT)
               GO TO CR0-060
           END-IF
           IF  CNT = 1
               IF  W-TCDT(1) = ZERO
                   GO TO CR0-060
               END-IF
           END-IF
           GO TO CR0-020.
       CR0-080.
           MOVE 17 TO CNT.
           MOVE 21 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE ZERO TO W-AC.
       CR0-100.
           SUBTRACT 1 FROM CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT = ZERO
               GO TO CR0-010
           END-IF
           IF  W-TCDT(CNT) = ZERO
               GO TO CR0-100
           END-IF
           GO TO CR0-060.
       CR0-120.
           MOVE ZERO TO CNT W-AC.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       CR0-140.
           ADD 1 TO CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT > 16
               GO TO CR0-300
           END-IF.
       CR0-160.
           IF  W-AC = 1
               MOVE ZERO TO W-HCDF(CNT)
               CALL "SD_Output" USING
                "S-HCDF" S-HCDF "p" RETURNING RESU
               GO TO CR0-180
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-HCDF "A-HCDF" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               MOVE 1 TO W-EC
               GO TO CR0-EX
           END-IF
           IF  ESTAT = ADV
               MOVE 1 TO W-AC
               MOVE ZERO TO W-HCDF(CNT)
               CALL "SD_Output" USING
                "S-HCDF" S-HCDF "p" RETURNING RESU
               GO TO CR0-180
           END-IF
           IF  ESTAT = BTB
               SUBTRACT 1 FROM CNT W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               IF  CNT = ZERO
                   GO TO CR0-020
               ELSE
                   GO TO CR0-180
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO CR0-160
           END-IF
           IF (CNT NOT = 1) AND (W-HCDF(CNT) = 9999)
               MOVE 1 TO W-AC
               MOVE ZERO TO W-HCDF(CNT)
               CALL "SD_Output" USING
                "S-HCDF" S-HCDF "p" RETURNING RESU
               GO TO CR0-180
           END-IF
           IF (CNT NOT = 1) AND (W-HCDF(CNT) NOT = ZERO)
               COMPUTE CNTD = CNT - 1
               IF  W-HCDT(CNTD) >= W-HCDF(CNT)
                   GO TO CR0-160
               END-IF
           END-IF.
       CR0-180.
           IF  W-AC = 1
               MOVE ZERO TO W-HCDT(CNT)
               CALL "SD_Output" USING
                "S-HCDT" S-HCDT "p" RETURNING RESU
               GO TO CR0-140
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-HCDT "A-HCDT" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO CR0-160
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO CR0-180
           END-IF
           IF  W-HCDF(CNT) > W-HCDT(CNT)
               GO TO CR0-180
           END-IF
           IF  CNT = 1
               IF  W-HCDT(1) = ZERO
                   GO TO CR0-180
               END-IF
           END-IF
           GO TO CR0-140.
       CR0-300.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = BTB
               GO TO CR0-340
           END-IF
           MOVE ZERO TO W-AC.
       CR0-320.
           SUBTRACT 1 FROM CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT = ZERO
               GO TO CR0-120
           END-IF
           IF  W-HCDT(CNT) = ZERO
               GO TO CR0-320
           END-IF
           GO TO CR0-180.
       CR0-340.
           IF  ESTAT NOT = HTB AND SKP
               GO TO CR0-300
           END-IF
           IF  W-DMM = 9
               GO TO CR0-010
           END-IF
           IF  W-DMM NOT = 1
               GO TO CR0-300
           END-IF.
       CR0-EX.
           EXIT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS99U.
      **********************************************
      *****    ÇiÇ`ÇmÉ}ÉXÉ^Å@ÇdÇòÇÉÇÖÇåïœä∑    *****
      **********************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-DATE.
             03  W-NEN1       PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-TIME.
             03  W-TIME1      PIC  9(006).
             03  W-TIME2      PIC  9(002).
           02  W-S            PIC  9(001).
       01  W-SET.
           02  W-ADSM.
             03  F            PIC  X(040) VALUE
                  "        SS  S   M   L   LL  XL  XXL    ".
             03  F            PIC  X(040) VALUE
                  "                            28.029.030.0".
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.0    ".
             03  F            PIC  X(040) VALUE
                  "24.024.525.025.526.026.527.027.5        ".
           02  W-ADSD.
             03  W-ADS   OCCURS   5.
               04  W-DSD   OCCURS  10.
                 05  W-DS     PIC  X(004).
           COPY LSTAT.
      *
           COPY LICODE.
           COPY LIHIM.
      *FD  JANF
       01  JANF_JHS99U.
           02  JANF_PNAME1    PIC  X(004) VALUE "JANF".
           02  F              PIC  X(001).
           02  JANF_LNAME     PIC  X(011) VALUE "JANF_JHS99U".
           02  F              PIC  X(001).
           02  JANF_KEY1      PIC  X(100) VALUE SPACE.
           02  JANF_KEY2      PIC  X(100) VALUE SPACE.
           02  JANF_SORT      PIC  X(100) VALUE SPACE.
           02  JANF_IDLST     PIC  X(100) VALUE SPACE.
           02  JANF_RES       USAGE  POINTER.
       01  JAN-R.
           02  JAN-KEY.
             03  JAN-ITF      PIC  X(016).
             03  JAN-ITFD  REDEFINES  JAN-ITF.
               04  JAN-ITF1   PIC  X(013).
               04  JAN-ITF2   PIC  X(003).
           02  JAN-KBN        PIC  X(001).
           02  JAN-JAN        PIC  X(013).
           02  JAN-HCD        PIC  X(006).
           02  JAN-NAME       PIC  N(024).
           02  JAN-SIZ        PIC  X(004).
           02  JAN-ISU        PIC  X(003).
           02  JAN-DATE       PIC  X(008).
           02  JAN-TIME       PIC  X(006).
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
           02  FILLER  PIC  N(022) VALUE
               "ÅñÅñÅñÅ@Å@ÇiÇ`ÇmÉ}ÉXÉ^Å@ÇbÇrÇuïœä∑Å@Å@ÅñÅñÅñ".
       01  C-ERR.
           02  FILLER.
             03  E-ME2   PIC  X(022) VALUE
                  "***  ¿›º≠∏À›“≤ ≈º  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  À›“≤ ≈º  ***".
             03  E-ME5   PIC  X(017) VALUE
                  "***  ª≤Ωﬁ ≈º  ***".
             03  E-ME9   PIC  X(019) VALUE
                  "***  WRITE ¥◊∞  ***".
             03  E-JAN   PIC  X(013).
             03  E-HCD   PIC  9(006).
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "44" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "15" "44" " " "C-MID" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "94" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "94" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "22" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "17" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "19" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JAN" "X" "24" "40" "13" "E-ME9" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JAN" BY REFERENCE CODE-JAN "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HCD" "9" "24" "56" "6" "E-JAN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HCD" BY REFERENCE CODE-HCD "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           ACCEPT W-NGPS FROM DATE.
           ACCEPT W-TIME FROM TIME.
           MOVE 20 TO W-NEN1.
           MOVE W-ADSM TO W-ADSD.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "OUTPUT" JANF_PNAME1 " " BY REFERENCE JANF_IDLST "1"
            "JAN-KEY" BY REFERENCE JAN-KEY.
           CALL "DB_F_Close" USING BY REFERENCE JANF_IDLST JANF_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" JANF_PNAME1 " " BY REFERENCE JANF_IDLST "1"
            "JAN-KEY" BY REFERENCE JAN-KEY.
       M-10.
      *           READ CODEF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  CODE-TCD NOT = ZERO
               GO TO M-10
           END-IF
           IF  CODE-HCD = 999999
               GO TO M-10
           END-IF
           MOVE CODE-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-10
           END-IF
           IF  CODE-ISU = ZERO
               GO TO M-20
           END-IF
           MOVE CODE-ITF TO JAN-ITF.
      *           READ JANF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JANF_PNAME1 BY REFERENCE JAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           GO TO M-20.
       M-15.
           INITIALIZE JAN-R.
           MOVE SPACE TO JAN-ITF JAN-JAN JAN-NAME JAN-SIZ.
           MOVE ZERO TO JAN-KBN JAN-HCD JAN-ISU.
           MOVE CODE-ITF TO JAN-ITF.
           MOVE 1 TO JAN-KBN.
           MOVE CODE-JAN TO JAN-JAN.
           MOVE CODE-HCD TO JAN-HCD.
           IF  HI-SMS NOT = SPACE
               MOVE HI-SMS TO JAN-NAME
           ELSE
               MOVE HI-NAME TO JAN-NAME
           END-IF
           MOVE CODE-SIZ TO W-S.
           IF  HI-SSC = 0
               ADD 1 TO W-S
           END-IF
           IF  W-DS(W-S,CODE-SNO) = SPACE
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-10
           END-IF
           MOVE W-DS(W-S,CODE-SNO) TO JAN-SIZ.
           MOVE HI-ISU TO JAN-ISU.
           MOVE W-DATE TO JAN-DATE.
           MOVE W-TIME1 TO JAN-TIME.
      *           WRITE JAN-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            JANF_PNAME1 JANF_LNAME JAN-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-20.
           MOVE CODE-JAN TO JAN-ITF1.
           MOVE SPACE TO JAN-ITF2.
      *           READ JANF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JANF_PNAME1 BY REFERENCE JAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           GO TO M-10.
       M-25.
           INITIALIZE JAN-R.
           MOVE SPACE TO JAN-ITF JAN-JAN JAN-NAME JAN-SIZ.
           MOVE ZERO TO JAN-KBN JAN-HCD JAN-ISU.
           MOVE CODE-JAN TO JAN-ITF1.
           MOVE SPACE TO JAN-ITF2.
           MOVE 0 TO JAN-KBN.
           MOVE CODE-JAN TO JAN-JAN.
           MOVE CODE-HCD TO JAN-HCD.
           IF  HI-SMS NOT = SPACE
               MOVE HI-SMS TO JAN-NAME
           ELSE
               MOVE HI-NAME TO JAN-NAME
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           MOVE CODE-SIZ TO W-S.
           IF  HI-SSC = 0
               ADD 1 TO W-S
           END-IF
           IF  W-DS(W-S,CODE-SNO) = SPACE
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-10
           END-IF
           MOVE W-DS(W-S,CODE-SNO) TO JAN-SIZ.
           MOVE 1 TO JAN-ISU.
           MOVE W-DATE TO JAN-DATE.
           MOVE W-TIME1 TO JAN-TIME.
      *           WRITE JAN-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            JANF_PNAME1 JANF_LNAME JAN-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-10.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JANF_IDLST JANF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

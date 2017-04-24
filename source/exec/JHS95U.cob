       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS95U.
      ******************************************
      *****    ÇbÇnÇcÇdÇeÅ@ÇhÇsÇeÉZÉbÉg    *****
      ******************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-ITF.
               03  W-D1         PIC 9.
               03  W-D2         PIC 9.
               03  W-D3         PIC 9.
               03  W-D4         PIC 9.
               03  W-D5         PIC 9.
               03  W-D6         PIC 9.
               03  W-D7         PIC 9.
               03  W-D8         PIC 9.
               03  W-D9         PIC 9.
               03  W-D10        PIC 9.
               03  W-D11        PIC 9.
               03  W-D12        PIC 9.
               03  W-D13        PIC 9.
               03  W-D14        PIC 9.
               03  W-D15        PIC 9.
               03  W-D16        PIC 9.
       01  W-KDATA.
           02  W-K1             PIC 9(3).
           02  W-K2             PIC 9(3).
           02  W-K3             PIC 9.
           02  KEISAND          PIC 9(3).
           02  KEISAN   REDEFINES  KEISAND.
               03  K-D1         PIC 9(2).
               03  K-D2         PIC 9(1).
           COPY LSTAT.
      *
           COPY LICODE.
           COPY LIHIM.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "ÅñÅñÅñÅ@Å@ÇbÇnÇcÇdÇeÅ@ÇhÇsÇeÉZÉbÉgÅ@Å@ÅñÅñÅñ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME4   PIC  X(017) VALUE
                  "***  À›“≤ ≈º  ***".
             03  E-ME12  PIC  X(021) VALUE
                  "***  REWRITE ¥◊∞  ***".
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
            "01C-MID" "N" "2" "10" "44" " " "C-MID" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "65" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "57" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "57" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "21" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JAN" "X" "24" "40" "13" "E-ME12" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JAN" BY REFERENCE CODE-JAN "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HCD" "9" "24" "55" "6" "E-JAN" " " RETURNING RESU.
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
           CALL "DB_F_Open" USING
            "I-O" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-10.
      *           READ CODEF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  CODE-TCD NOT = ZERO
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
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-10
           END-IF
           MOVE CODE-JAN TO CODE-JAND.
           MOVE HI-ISU TO CODE-ISU.
           PERFORM S-05 THRU S-10.
      *           REWRITE CODE-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            CODEF_PNAME1 CODEF_LNAME CODE-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           GO TO M-10.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE CODE-ITF TO W-ITF.
           COMPUTE W-K1 = W-D1 + W-D3 + W-D5 + W-D7 + W-D9 + W-D11
                                                   + W-D13 + W-D15.
           COMPUTE W-K1 = W-K1 * 3.
           COMPUTE W-K2 = W-D2 + W-D4 + W-D6 + W-D8 + W-D10 + W-D12
                                                           + W-D14.
           COMPUTE KEISAND = W-K1 + W-K2.
           COMPUTE W-K3 = 10 - K-D2.
           MOVE W-K3 TO W-D16.
           MOVE W-ITF TO CODE-ITF.
       S-10.
           EXIT.

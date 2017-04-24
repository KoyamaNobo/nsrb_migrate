       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JV710U.
      ************************************************************
      *    PROGRAM         :  ’I‰µ•ÏŠ·—pƒ[ƒNì¬(ÊÞ°º°ÄÞÅ¼)    *
      *    PRINTER TYPE    :  JIPS                               *
      *    SCREEN          :  ______                             *
      *    COMPILE TYPE    :  COBOL                              *
      ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0128".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  W-DMM          PIC  X(01).
           02  W-SC           PIC  9(001).
           02  CNT            PIC  9(02).
           02  W-SCD          PIC  9(001).
           02  W-ASID.
             03  W-ASI   OCCURS   5.
               04  W-SID   OCCURS  10.
                 05  W-SI     PIC  X(004).
           02  W-MSI.
             03  F            PIC  X(040) VALUE
                  "0000      SS   S   M   L  LL  XL XXLXXXL".
             03  F            PIC  X(040) VALUE
                  "                            28.029.030.0".
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.0    ".
             03  F            PIC  X(040) VALUE
                  "24.024.525.025.526.026.527.027.5        ".
           02  W-SOK          PIC  9(001).
           02  W-SOM          PIC  N(006).
           COPY LSTAT.
      *
           COPY LIHIM.
           COPY L-JCON.
           COPY LICODE.
      *FD  WTANA
       01  WTANA_JV710U.
           02  WTANA_PNAME1   PIC  X(006) VALUE "TTANAF".
           02  F              PIC  X(001).
           02  WTANA_LNAME    PIC  X(012) VALUE "WTANA_JV710U".
           02  F              PIC  X(001).
           02  WTANA_KEY1     PIC  X(100) VALUE SPACE.
           02  WTANA_SORT     PIC  X(100) VALUE SPACE.
           02  WTANA_IDLST    PIC  X(100) VALUE SPACE.
           02  WTANA_RES      USAGE  POINTER.
       01  WTANA-R.
           02  WTANA-SOK      PIC 9(1).
           02  WTANA-HCD      PIC 9(6).                                 •i–¼C
           02  WTANA-NAME     PIC N(24).                                •i–¼
           02  WTANA-SIZ      PIC X(4).                                 »²½Þ
           02  WTANA-KES      PIC S9(5).                                ¹°½”
           02  WTANA-ISU      PIC 9(3).                                 “ü”
           02  WTANA-KSU      PIC S9(6).                                ¹°½‘«”
           02  WTANA-HSU      PIC S9(6).                                ’[”
           02  WTANA-KEI      PIC S9(6).                                ‡Œv
           02  WTANA-ZAI      PIC S9(6).                                ÝŒÉ”
           02  WTANA-SAG      PIC S9(6).                                ·Šz
           02  F              PIC X(5).
       77  F                  PIC X(1).
      *FD  TANAO                                                        ÆÌÀÞÄ×Ý
       01  TANAO_JV710U.
           02  TANAO_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TANAO_LNAME    PIC  X(012) VALUE "TANAO_JV710U".
           02  F              PIC  X(001).
           02  TANAO_KEY1     PIC  X(100) VALUE SPACE.
           02  TANAO_SORT     PIC  X(100) VALUE SPACE.
           02  TANAO_IDLST    PIC  X(100) VALUE SPACE.
           02  TANAO_RES      USAGE  POINTER.
       01  TANAO-R.
           02  F              PIC X(6).
           02  TANAO-SOK      PIC 9(1).                                 ‘qŒÉC
           02  TANAO-BAS      PIC 9(6).                                 êŠNO
           02  TANAO-HCD      PIC 9(6).                                 •i–¼C
           02  TANAO-SC       PIC 9(1).                                 »²½Þ
           02  TANAO-ASU.                                               ”—Ê
             03  TANAO-SUD   OCCURS  10.                                ”—Ê
               04  TANAO-SU   PIC S9(6).                                ”—Ê
           02  TANAO-GC       PIC 9(1).                                 sC
           02  TANAO-ISU      PIC 9(3).                                 “ü”
           02  TANAO-BC       PIC 9(06).
           02  F              PIC X(38).
       77  F                  PIC X(01).
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
           02  FILLER  PIC  N(020) VALUE
                "–––@@’I‰µ•ÏŠ·—pƒ[ƒNì¬@@–––".
           02  FILLER  PIC  N(010) VALUE
                "iƒo[ƒR[ƒh‚È‚µ•ªj".
           02  FILLER  PIC  N(018) VALUE
                "‚s‚s‚`‚m‚`‚e@¨@‚v‚j‚O‚P‚Q‚W‚Ž‚Ž‚Ž".
           02  FILLER.
               03  FILLER  PIC  N(003) VALUE  "Šm”Fi".
               03  FILLER  PIC  X(009) VALUE  "OK=1,NO=9".
               03  FILLER  PIC  N(001) VALUE  "j".
               03  FILLER  PIC  X(009) VALUE  "---> ØÀ°Ý".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SOM   PIC  N(006).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA Å¼  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ´×°  ***".
             03  E-ME3   PIC  X(016) VALUE
                  "***  HIM Å¼  ***".
             03  E-ME8   PIC  X(017) VALUE
                  "***  »²½Þ Å¼  ***".
             03  E-ME9   PIC  X(017) VALUE
                  "***  ²Ø½³ Å¼  ***".
             03  E-ME10  PIC  X(016) VALUE
                  "***  ¿³º Å¼  ***".
             03  E-ME11  PIC  N(010) VALUE
                  "–•¡”‚Ì‘qŒÉ‚ª‚ ‚é–".
             03  E-HCD   PIC  9(006).
             03  E-SIZ   PIC  X(004).
             03  E-SOK   PIC  9(006).
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
           "C-MID" " " "0" "0" "122" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "1" "16" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "5" "26" "20" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "7" "18" "36" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" " " "23" "0" "26" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "23" "41" "6" " " "04C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "X" "23" "47" "9" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "23" "56" "2" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "23" "58" "9" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "62" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-SOM" "N" "12" "29" "12" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING
           "D-SOM" BY REFERENCE JCON3-03 "12" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "137" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "137" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" "X" "24" "15" "16" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME8" "X" "24" "15" "17" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME9" "X" "24" "15" "17" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME10" "X" "24" "15" "16" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME11" "N" "24" "15" "20" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-HCD" "9" "24" "45" "6" "E-ME11" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-HCD" BY REFERENCE WTANA-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-SIZ" "X" "24" "53" "4" "E-HCD" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-SIZ" BY REFERENCE WTANA-SIZ "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-SOK" "9" "24" "35" "6" "E-SIZ" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-SOK" BY REFERENCE WTANA-SOK "1" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           PERFORM S-05 THRU S-20.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           MOVE W-MSI TO W-ASID.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID22.
           MOVE W-FID2 TO WK0128ID.
           MOVE WK0128ID TO TANAO_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" WTANA_PNAME1 " " BY REFERENCE WTANA_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "OUTPUT" TANAO_PNAME1 " " BY REFERENCE TANAO_IDLST "0".
      *           READ WTANA AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" WTANA_PNAME1 BY REFERENCE WTANA-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-15.
           MOVE WTANA-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-35
           END-IF
           IF  WTANA-SIZ = SPACE
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SIZ" E-SIZ "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-35
           END-IF
           MOVE 0 TO W-SC.
       M-20.
           ADD 1 TO W-SC.
           IF  W-SC = 6
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SIZ" E-SIZ "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-35
           END-IF
           IF  W-SC > 1
               COMPUTE W-SCD = W-SC - 1
           ELSE
               MOVE W-SC TO W-SCD
           END-IF
           MOVE ZERO TO CNT.
       M-25.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-20
           END-IF
           IF  WTANA-SIZ NOT = W-SI(W-SC,CNT)
               GO TO M-25
           END-IF
           IF  HI-S(W-SCD,CNT) = 0
               GO TO M-25
           END-IF
      *
           IF  WTANA-KES = ZERO
               GO TO M-30
           END-IF
           INITIALIZE TANAO-R.
           MOVE ZERO TO TANAO-ASU.
           MOVE WTANA-SOK TO TANAO-SOK.
           MOVE WTANA-HCD TO TANAO-HCD.
           MOVE W-SCD TO TANAO-SC.
           MOVE WTANA-KES TO TANAO-SU(CNT).
           IF  W-SCD = 1
               MOVE 4 TO TANAO-GC
           ELSE
               IF  W-SCD = 2
                   MOVE 1 TO TANAO-GC
               ELSE
                   IF  W-SCD = 3
                       MOVE 2 TO TANAO-GC
                   ELSE
                       IF  W-SCD = 4
                           MOVE 3 TO TANAO-GC
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE HI-ISU TO TANAO-ISU.
           MOVE HI-BC TO TANAO-BC.
      *           WRITE TANAO-R.
      *//////////////
           CALL "DB_Insert" USING
            TANAO_PNAME1 TANAO_LNAME TANAO-R RETURNING RET.
       M-30.
           IF  WTANA-HSU = ZERO
               GO TO M-35
           END-IF
           INITIALIZE TANAO-R.
           MOVE ZERO TO TANAO-ASU.
           MOVE WTANA-SOK TO TANAO-SOK.
           MOVE WTANA-HCD TO TANAO-HCD.
           MOVE W-SCD TO TANAO-SC.
           MOVE WTANA-HSU TO TANAO-SU(CNT).
           IF  W-SCD = 1
               MOVE 8 TO TANAO-GC
           ELSE
               IF  W-SCD = 2
                   MOVE 5 TO TANAO-GC
               ELSE
                   IF  W-SCD = 3
                       MOVE 6 TO TANAO-GC
                   ELSE
                       IF  W-SCD = 4
                           MOVE 7 TO TANAO-GC
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE HI-ISU TO TANAO-ISU.
           MOVE HI-BC TO TANAO-BC.
      *           WRITE TANAO-R.
      *//////////////
           CALL "DB_Insert" USING
            TANAO_PNAME1 TANAO_LNAME TANAO-R RETURNING RET.
       M-35.
      *           READ WTANA AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" WTANA_PNAME1 BY REFERENCE WTANA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           GO TO M-15.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE WTANA_IDLST WTANA_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TANAO_IDLST TANAO_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           CALL "DB_F_Open" USING
            "INPUT" WTANA_PNAME1 " " BY REFERENCE WTANA_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON3-KEY" BY REFERENCE JCON3-KEY.
      *           READ WTANA AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" WTANA_PNAME1 BY REFERENCE WTANA-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-15
           END-IF
           MOVE WTANA-SOK TO W-SOK.
           MOVE "3" TO JCON3-01.
           MOVE W-SOK TO JCON3-02.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SOK" E-SOK "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-15
           END-IF
           CALL "SD_Output" USING "D-SOM" D-SOM "p" RETURNING RESU.
       S-10.
      *           READ WTANA AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" WTANA_PNAME1 BY REFERENCE WTANA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-15
           END-IF
           IF  WTANA-SOK = W-SOK
               GO TO S-10
           END-IF
           CALL "SD_Output" USING "E-ME11" E-ME11 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
       S-15.
           CALL "DB_F_Close" USING
            BY REFERENCE WTANA_IDLST WTANA_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
       S-20.
           EXIT.

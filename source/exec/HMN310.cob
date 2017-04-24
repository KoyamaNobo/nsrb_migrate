       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN310.
      *********************************************************
      *    PROGRAM         :  品名統計マスター　棚卸セット    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :                                  *
      *        変更　　　  :  62/05/16                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-NG.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-HCD          PIC  9(006).
           02  W-AS           PIC S9(006).
           02  W-SU           PIC S9(006).
           02  CNT            PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LIHHTF.
           COPY LIHTIM.
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
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　品名別　棚卸数セット　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(026) VALUE
                  "***  HHTF REWRITE ｴﾗｰ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  HHTF ﾅｼ  ***".
             03  E-ME3   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME4   PIC  X(026) VALUE
                  "***  HTIM REWRITE ｴﾗｰ  ***".
             03  E-ME5   PIC  X(018) VALUE
                  "***  ｲﾘｽｳ ｴﾗｰ  ***".
             03  E-ME6   PIC  X(019) VALUE
                  "***  ｲﾘｽｳ ZERO  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-KEY   PIC  9(007).
             03  E-HTI   PIC  X(007).
             03  E-HIM   PIC  9(006).
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
           "C-MID" " " "0" "0" "302" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "40" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "40" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "40" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "40" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "40" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "40" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "20" "23" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "40" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "204" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "204" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "26" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" "X" "24" "15" "16" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME4" "X" "24" "15" "26" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME5" "X" "24" "15" "18" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME6" "X" "24" "15" "19" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "9" "24" "45" "7" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE HHT-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-HTI" "X" "24" "45" "7" "E-KEY" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-HTI" BY REFERENCE HTI-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-HIM" "9" "24" "45" "6" "E-HTI" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-HIM" BY REFERENCE HI-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" "E-HIM" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NG.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" HHTF_PNAME1 "SHARED" BY REFERENCE HHTF_IDLST "2"
            "HHT-KEY" BY REFERENCE HHT-KEY "HHT-KEY2" BY REFERENCE
            HHT-KEY2.
       M-15.
      *           READ HHTF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
      *
           MOVE ZERO TO CNT.
       M-20.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-25
           END-IF
           IF (HHT-SIZ = 4) AND (CNT = 10)
               IF  W-GET = 5 OR 11
                   GO TO M-20
               END-IF
           END-IF
           MOVE ZERO TO HHT-TSU(CNT).
           GO TO M-20.
       M-25.
      *           REWRITE HHT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HHTF_PNAME1 HHTF_LNAME HHT-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HHTF_IDLST HHTF_PNAME1
               GO TO M-90
           END-IF
           GO TO M-15.
       M-30.
           CALL "DB_F_Open" USING
            "I-O" HTI-M_PNAME1 "SHARED" BY REFERENCE HTI-M_IDLST "1"
            "HTI-KEY" BY REFERENCE HTI-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-35.
      *           READ HTI-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HTI-M_PNAME1 BY REFERENCE HTI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  HTI-SUD = ZERO
               GO TO M-35
           END-IF
           MOVE HTI-HCD TO HHT-HCD.
           MOVE HTI-SIZ TO HHT-SIZ.
      *           READ HHTF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HHTF_PNAME1 BY REFERENCE HHT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  HTI-GNO > 4
               GO TO M-37
           END-IF
           MOVE HTI-HCD TO HI-KEY.
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
                "E-HIM" E-HIM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  ZERO = HTI-ISU AND HI-ISU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HIM" E-HIM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  HTI-ISU = HI-ISU
               GO TO M-37
           END-IF
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "SD_Output" USING "E-ME5" E-ME5 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-HIM" E-HIM "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  HI-ISU NOT = ZERO
               MOVE HI-ISU TO HTI-ISU
      *               REWRITE HTI-R INVALID KEY
      *//////////////////////
               CALL "DB_Update" USING
                HTI-M_PNAME1 HTI-M_LNAME HTI-R RETURNING RET
               IF  RET = 1
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "E-STAT" E-STAT "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME4" E-ME4 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-HTI" E-HTI "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-CL" E-CL "p" RETURNING RESU
               END-IF
           END-IF.
       M-37.
           MOVE ZERO TO CNT.
       M-40.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-45
           END-IF
           IF  HTI-SU(CNT) NOT = ZERO
               IF  HTI-GNO > 4
                   ADD HTI-SU(CNT) TO HHT-TSU(CNT)
               ELSE
                   COMPUTE HHT-TSU(CNT) = HHT-TSU(CNT) +
                                         (HTI-SU(CNT) * HTI-ISU)
               END-IF
           END-IF
           GO TO M-40.
       M-45.
      *           REWRITE HHT-R INVALID KEY
      *//////////////////////
           CALL "DB_Update" USING
            HHTF_PNAME1 HHTF_LNAME HHT-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-35.
       M-50.
           IF  W-GET NOT = 4 AND 10
               GO TO M-90
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" HHTF_PNAME1 "SHARED" BY REFERENCE HHTF_IDLST "2"
            "HHT-KEY" BY REFERENCE HHT-KEY "HHT-KEY2" BY REFERENCE
            HHT-KEY2.
       M-52.
      *           READ HHTF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
       M-55.
           MOVE HHT-HCD TO W-HCD.
           MOVE ZERO TO W-SU W-AS.
       M-60.
           COMPUTE W-AS = HHT-ASS(01) + HHT-ASS(02) + HHT-ASS(03)
                        + HHT-ASS(04) + HHT-ASS(05) + HHT-ASS(06)
                        + HHT-ASS(07) + HHT-ASS(08) + HHT-ASS(09)
                        + HHT-ASS(10) + W-AS.
           IF  HHT-SIZ = 4
               COMPUTE W-SU = HHT-ZSU(10) + W-AS - HHT-USU(10)
               IF  W-SU NOT = ZERO
                   MOVE W-SU TO HHT-TSU(10)
      *                   REWRITE HHT-R INVALID KEY
      *//////////////////////
                   CALL "DB_Update" USING
                    HHTF_PNAME1 HHTF_LNAME HHT-R RETURNING RET
                   IF  RET = 1
                       CALL "C3_Set_Jrcode" USING 
                        USER_ID BY REFERENCE COMPLETION_CODE  255
                       CALL "SD_Output" USING
                        "E-STAT" E-STAT "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME1" E-ME1 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-KEY" E-KEY "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME99" E-ME99 "p" RETURNING RESU
                       GO TO M-90
                   END-IF
               END-IF
           END-IF.
       M-65.
      *           READ HHTF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HHT-HCD = W-HCD
               GO TO M-60
           END-IF
           GO TO M-55.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HTI-M_IDLST HTI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

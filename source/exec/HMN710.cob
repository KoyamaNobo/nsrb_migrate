       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN710.
      *********************************************************
      *    PROGRAM         :  履物　棚卸　前月残高　更新      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
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
           02  W-DMM          PIC  9(001).
           02  W-D.
             03  W-HCD        PIC  9(006).
             03  W-ZS         PIC S9(006).
             03  W-ZK         PIC S9(009).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LIHHTF.
           COPY LIHUHM.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID1.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　履物　棚卸　前月残高　更新　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(025) VALUE
                  "***  ｼﾞｯｺｳ ﾈﾝｹﾞﾂ ｴﾗｰ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  HHTF ﾅｼ  ***".
             03  E-ME3   PIC  X(026) VALUE
                  "***  HHTF REWRITE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  HUH-M ﾅｼ  ***".
             03  E-ME5   PIC  X(027) VALUE
                  "***  HUH-M REWRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-KEY   PIC  9(007).
             03  E-HCD   PIC  9(006).
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
      *C-MID1
       CALL "SD_Init" USING
           "C-MID1" " " "0" "0" "344" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID1" "N" "3" "10" "46" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID1" "N" "4" "10" "46" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID1" "N" "5" "10" "46" "02C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID1" "N" "6" "10" "46" "03C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID1" "N" "7" "10" "46" "04C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID1" "N" "8" "10" "46" "05C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID1" "N" "9" "10" "46" "06C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID1" "X" "20" "40" "22" "07C-MID1" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "57" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "204" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "204" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "25" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" "X" "24" "15" "26" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME4" "X" "24" "15" "18" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME5" "X" "24" "15" "27" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME7" "X" "24" "15" "16" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "9" "24" "45" "7" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE HHT-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-HCD" "9" "24" "45" "6" "E-KEY" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" "E-HCD" " " RETURNING RESU.
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
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
           COPY LIBCPR.
           IF  D-NHG NOT = 5 AND 11
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
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
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "I-O" HHTF_PNAME1 " " BY REFERENCE HHTF_IDLST "2"
            "HHT-KEY" BY REFERENCE HHT-KEY "HHT-KEY2" BY REFERENCE
            HHT-KEY2.
           CALL "DB_F_Open" USING
            "I-O" HUH-M_PNAME1 " " BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
      *           READ HHTF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-30.
           MOVE ZERO TO W-D.
           MOVE HHT-HCD TO W-HCD.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO HI-FT
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       M-35.
           MOVE HHT-TSU(01) TO HHT-ZSU(01).
           MOVE HHT-TSU(02) TO HHT-ZSU(02).
           MOVE HHT-TSU(03) TO HHT-ZSU(03).
           MOVE HHT-TSU(04) TO HHT-ZSU(04).
           MOVE HHT-TSU(05) TO HHT-ZSU(05).
           MOVE HHT-TSU(06) TO HHT-ZSU(06).
           MOVE HHT-TSU(07) TO HHT-ZSU(07).
           MOVE HHT-TSU(08) TO HHT-ZSU(08).
           MOVE HHT-TSU(09) TO HHT-ZSU(09).
           MOVE HHT-TSU(10) TO HHT-ZSU(10).
      *           REWRITE HHT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HHTF_PNAME1 HHTF_LNAME HHT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           COMPUTE W-ZS = HHT-TSU(01) + HHT-TSU(02) + HHT-TSU(03)
                        + HHT-TSU(04) + HHT-TSU(05) + HHT-TSU(06)
                        + HHT-TSU(07) + HHT-TSU(08) + HHT-TSU(09)
                        + HHT-TSU(10) + W-ZS.
      *           READ HHTF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  W-HCD = HHT-HCD
               GO TO M-35
           END-IF
           PERFORM S-05 THRU S-25.
           GO TO M-30.
       M-90.
           PERFORM S-05 THRU S-25.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-M_IDLST HUH-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           COMPUTE W-ZK = W-ZS * HI-FT.
           MOVE W-HCD TO HUH-KEY.
      *           READ HUH-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-25
           END-IF
           MOVE W-ZS TO HUH-ZS.
           MOVE W-ZK TO HUH-ZK.
           COMPUTE HUH-YS = HUH-ZS + HUH-NS - HUH-SS.
           COMPUTE HUH-YK = HUH-YS * HI-FT.
      *           REWRITE HUH-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HHTF_PNAME1 HHTF_LNAME HHT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       S-25.
           EXIT.

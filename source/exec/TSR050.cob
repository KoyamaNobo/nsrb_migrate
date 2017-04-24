       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSR050.
      ************************************************
      *****     領収書ファイル　メンテナンス     *****
      *****          ( SCREEN : SCTR05 )         *****
      ************************************************
       AUTHOR. S-NAKAO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-R.
           02  F            PIC  X(012).
           02  W-DATE       PIC  9(006).
           02  W-DATED REDEFINES W-DATE.
             03  W-NEN      PIC  9(002).
             03  W-GET      PIC  9(002).
             03  W-PEY      PIC  9(002).
           02  W-TCD        PIC  9(004).
           02  W-KIN        PIC S9(009).
           02  W-INS        PIC  9(005).
           02  W-SOC        PIC  9(001).
           02  W-SHC        PIC  9(001).
           02  F            PIC  X(023).
           02  W-SEQ        PIC  9(003).
       01  W-DATA.
           02  W-KEY        PIC  9(003).
           02  W-NO         PIC  9(003).
           02  W-GKIN       PIC S9(009).
           02  W-ACT        PIC  9(001).
           02  W-DMM        PIC  9(001).
           02  CNT          PIC  9(002).
           02  CHK          PIC  9(001).
           02  W-SOM        PIC  N(003).
           02  W-SHM        PIC  N(003).
           02  W-NAME       PIC  N(016).
       01  W-TBLD.
           02  W-ARTB.
             03  F          PIC  X(050) VALUE
                  "20000100000600004000020000100000600004000020000000".
           02  W-RTBD  REDEFINES W-ARTB.
             03  W-RTB   OCCURS 10  PIC  9(005).
           02  W-AKTB.
             03  F          PIC  X(045) VALUE
                  "100000000050000000030000000020000000010000000".
             03  F          PIC  X(045) VALUE
                  "005000000003000000002000000001000000000049999".
           02  W-KTBD  REDEFINES W-AKTB.
             03  W-KTB   OCCURS 10  PIC  9(009).
       01  ERR-STAT         PIC  X(002).
           COPY LSTAT.
      *
           COPY LITM.
      *FD  RS-F
       01  RS-F_TSR050.
           02  RS-F_PNAME1  PIC  X(003) VALUE "RSF".
           02  F            PIC  X(001).
           02  RS-F_LNAME   PIC  X(011) VALUE "RS-F_TSR050".
           02  F            PIC  X(001).
           02  RS-F_KEY1    PIC  X(100) VALUE SPACE.
           02  RS-F_SORT    PIC  X(100) VALUE SPACE.
           02  RS-F_IDLST   PIC  X(100) VALUE SPACE.
           02  RS-F_RES     USAGE  POINTER.
       01  RS-R.
           02  F            PIC  X(012).
           02  RS-DATE      PIC  9(006).
           02  RS-TCD       PIC  9(004).
           02  RS-KIN       PIC S9(009).
           02  RS-INS       PIC  9(005).
           02  RS-SOC       PIC  9(001).
           02  RS-SHC       PIC  9(001).
           02  F            PIC  X(023).
           02  RS-SEQ       PIC  9(003).
       77  F                PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  A-KEY   PIC  9(003).
           02  A-DATE  PIC  9(006).
           02  A-TCD   PIC  9(004).
           02  A-KIN   PIC  9(009).
           02  A-SOC   PIC  9(001).
           02  A-SHC   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NAME  PIC  N(016).
           02  D-KIN   PIC ZZZZZZZZ9 .
           02  D-INS   PIC ZZ,ZZ9 .
           02  D-SOM   PIC  N(003).
           02  D-SHM   PIC  N(003).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  ﾄｳﾛｸ ｽﾞﾐ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  ﾌｧｲﾙ ﾅｼ  ***".
             03  E-ME3   PIC  X(017) VALUE
                  "***  ﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME9   PIC  X(021) VALUE
                  "***  PROGRAM ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "26" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "46" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "9" "5" "14" "3" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DATE" "9" "7" "20" "6" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DATE" BY REFERENCE W-DATE "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "8" "20" "4" "A-DATE" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KIN" "9" "9" "20" "9" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KIN" BY REFERENCE W-KIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SOC" "9" "10" "20" "1" "A-KIN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SOC" BY REFERENCE W-SOC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHC" "9" "12" "20" "1" "A-SOC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHC" BY REFERENCE W-SHC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "40" "1" "A-SHC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "59" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "8" "25" "32" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE W-NAME "32" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN" "ZZZZZZZZ9" "9" "20" "9" "D-NAME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-KIN" BY REFERENCE W-KIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-INS" "ZZ,ZZ9" "11" "20" "6" "D-KIN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-INS" BY REFERENCE W-INS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SOM" "N" "10" "23" "6" "D-INS" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SOM" BY REFERENCE W-SOM "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHM" "N" "12" "23" "6" "D-SOM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SHM" BY REFERENCE W-SHM "6" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "135" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "135" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "17" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "21" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           MOVE 5 TO CHK.
           CALL "DB_F_Open" USING
            "INPUT" RS-F_PNAME1 " " BY REFERENCE RS-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           MOVE ZERO TO W-NO.
       M-10.
      *           READ RS-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" RS-F_PNAME1 BY REFERENCE RS-R " " RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO CHK
               CALL "DB_F_Close" USING
                BY REFERENCE RS-F_IDLST RS-F_PNAME1
               GO TO M-15
           END-IF
           IF  RS-SEQ > W-NO
               MOVE RS-SEQ TO W-NO
           END-IF
           GO TO M-10.
       M-15.
           CALL "SD_Screen_Output" USING "SCTR05" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-ACT = 9
               GO TO M-95
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Screen_Output" USING "SCTR05" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           IF  W-ACT NOT = 1
               GO TO M-25
           END-IF
           COMPUTE W-KEY = W-NO + 1.
           CALL "SD_Output" USING "A-KEY" A-KEY "p" RETURNING RESU.
           MOVE 5 TO CHK.
           CALL "DB_F_Open" USING
            "EXTEND" RS-F_PNAME1 " " BY REFERENCE RS-F_IDLST "0".
           INITIALIZE W-R.
           MOVE W-KEY TO W-SEQ.
           GO TO M-35.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           MOVE 5 TO CHK.
           CALL "DB_F_Open" USING
            "I-O" RS-F_PNAME1 " " BY REFERENCE RS-F_IDLST "0".
       M-30.
      *           READ RS-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" RS-F_PNAME1 BY REFERENCE RS-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE ZERO TO CHK
               CALL "DB_F_Close" USING
                BY REFERENCE RS-F_IDLST RS-F_PNAME1
               GO TO M-25
           END-IF
           IF  W-KEY NOT = RS-SEQ
               GO TO M-30
           END-IF
           MOVE RS-R TO W-R.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　　＊＊　マスター　なし　＊＊　" TO T-TNA
           END-IF
           IF  T-TNA = SPACE
               MOVE T-NAME TO W-NAME
           ELSE
               MOVE T-TNA TO W-NAME
           END-IF
           MOVE SPACE TO W-SOM W-SHM.
           IF  W-SOC = 5
               MOVE "相　殺" TO W-SOM
           END-IF
           IF  W-SOC = 8
               MOVE "消費税" TO W-SOM
           END-IF
           IF  W-SHC = 5
               MOVE "再発行" TO W-SHM
           END-IF
           CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-INS" D-INS "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SOC" A-SOC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SOM" D-SOM "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SHC" A-SHC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SHM" D-SHM "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO M-70
           END-IF
           GO TO M-35.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-DATE "A-DATE" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = BTB
               GO TO M-40
           END-IF
           MOVE ZERO TO CHK.
           CALL "DB_F_Close" USING BY REFERENCE RS-F_IDLST RS-F_PNAME1.
           IF  W-ACT = 2 OR 3
               GO TO M-25
           END-IF
           GO TO M-15.
       M-40.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-35
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-35
           END-IF.
       M-45.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-35
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-45
           END-IF
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-45
           END-IF
           IF  T-TNA = SPACE
               MOVE T-NAME TO W-NAME
           ELSE
               MOVE T-TNA TO W-NAME
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
       M-50.
           CALL "SD_Accept" USING BY REFERENCE A-KIN "A-KIN" "9" "9"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-45
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-50
           END-IF
           IF  W-KIN = ZERO
               GO TO M-50
           END-IF
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-SOC "A-SOC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-50
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-55
           END-IF
           IF  W-SOC NOT = 0 AND 5 AND 8
               GO TO M-55
           END-IF
           MOVE SPACE TO W-SOM.
           IF  W-SOC = 5
               MOVE "相　殺" TO W-SOM
           END-IF
           IF  W-SOC = 8
               MOVE "消費税" TO W-SOM
           END-IF
           CALL "SD_Output" USING "D-SOM" D-SOM "p" RETURNING RESU.
           IF  W-SOC = 5 OR 8
               MOVE ZERO TO W-INS
               CALL "SD_Output" USING "D-INS" D-INS "p" RETURNING RESU
               GO TO M-65
           END-IF
           MOVE 11 TO CNT.
       M-60.
           SUBTRACT 1 FROM CNT.
           IF  CNT = ZERO
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  W-KIN > W-KTB(CNT)
               GO TO M-60
           END-IF
           MOVE W-RTB(CNT) TO W-INS.
           CALL "SD_Output" USING "D-INS" D-INS "p" RETURNING RESU.
       M-65.
           CALL "SD_Accept" USING BY REFERENCE A-SHC "A-SHC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-55
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-65
           END-IF
           IF  W-SHC NOT = 0 AND 5
               GO TO M-65
           END-IF
           MOVE SPACE TO W-SHM.
           IF  W-SHC = 5
               MOVE "再発行" TO W-SHM
           END-IF
           CALL "SD_Output" USING "D-SHM" D-SHM "p" RETURNING RESU.
       M-70.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = BTB
               GO TO M-75
           END-IF
           IF  W-ACT = 3
               MOVE ZERO TO CHK
               CALL "DB_F_Close" USING
                BY REFERENCE RS-F_IDLST RS-F_PNAME1
               GO TO M-20
           END-IF
           GO TO M-65.
       M-75.
           IF  ESTAT = HTB AND SKP
               GO TO M-70
           END-IF
           IF  W-DMM = 9
               MOVE ZERO TO CHK
               CALL "DB_F_Close" USING
                BY REFERENCE RS-F_IDLST RS-F_PNAME1
               GO TO M-25
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-70
           END-IF
           IF  W-ACT = 3
               GO TO M-85
           END-IF
           INITIALIZE RS-R.
           MOVE W-R TO RS-R.
           IF  W-ACT NOT = 1
               GO TO M-80
           END-IF
      *           WRITE RS-R.
      *//////////////
           CALL "DB_Insert" USING
            RS-F_PNAME1 RS-F_LNAME RS-R RETURNING RET.
           MOVE W-SEQ TO W-NO.
           GO TO M-90.
       M-80.
      *           REWRITE RS-R.
      *///////////////
           CALL "DB_Update" USING
            RS-F_PNAME1 RS-F_LNAME RS-R RETURNING RET.
           GO TO M-90.
       M-85.
           MOVE X"FF" TO RS-R.
      *           REWRITE RS-R.
      *///////////////
           CALL "DB_Update" USING
            RS-F_PNAME1 RS-F_LNAME RS-R RETURNING RET.
       M-90.
           MOVE ZERO TO CHK.
           CALL "DB_F_Close" USING BY REFERENCE RS-F_IDLST RS-F_PNAME1.
           GO TO M-20.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           IF  CHK = 5
               CALL "DB_F_Close" USING
                BY REFERENCE RS-F_IDLST RS-F_PNAME1
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. JT012I.
      *********************************************************
      *    PROGRAM         :  受注マスタ摘要修正入力          *
      *    PRINTER TYPE    :  JIPS                            *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-JNO          PIC  9(006).
           02  W-KBN          PIC  N(003).
           02  W-TEKI.
             03  W-HTS        PIC  N(009).
             03  W-TEK        PIC  N(023).
           02  W-DMM          PIC  9(001).
           02  W-CHK          PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-SU           PIC S9(006).
           02  CNT            PIC  9(002).
           02  W-DCHK.
             03  W-DCD   OCCURS   6.
               04  W-DC       PIC  9(001).
           02  W-C            PIC  9(002).
           COPY LSTAT.
      *
           COPY LITM.
           COPY LITCM.
           COPY LIHIM.
           COPY LJMSTD.
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　受注マスタ　摘要修正入力　　＊＊＊".
           02  FILLER  PIC  N(003) VALUE "受注№".
           02  FILLER  PIC  X(007) VALUE "終了:F9".
           02  FILLER  PIC  N(003) VALUE "受注日".
           02  FILLER.
             03  FILLER  PIC  X(006) VALUE "ｺ ｰ ﾄﾞ".
             03  FILLER  PIC  N(003) VALUE "得意先".
           02  FILLER.
             03  FILLER  PIC  X(006) VALUE "ｺ ｰ ﾄﾞ".
             03  FILLER  PIC  N(003) VALUE "直送先".
           02  FILLER.
             03  FILLER  PIC  X(006) VALUE " ｺｰﾄﾞ ".
             03  FILLER  PIC  N(003) VALUE "品　名".
             03  FILLER  PIC  N(003) VALUE "残数量".
           02  FILLER  PIC  N(003) VALUE "配　達".
           02  FILLER  PIC  N(003) VALUE "摘　要".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-JNO   PIC  9(006).
           02  A-TEK   PIC  N(023).
           02  A-DMM    PIC  9(001).
       01  C-DSP.
           02  D-HED.
             03  FILLER  PIC  N(003).
             03  FILLER  PIC 99/99/99 .
             03  FILLER.
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  N(026).
             03  FILLER.
               04  FILLER  PIC  9(003).
               04  FILLER  PIC  N(026).
             03  FILLER  PIC  N(009).
           02  D-MEI.
             03  FILLER  PIC  9(006).
             03  FILLER  PIC  N(024).
             03  FILLER  PIC ----,--9 .
           02  D-MEIC.
             03  FILLER  PIC  X(006)  VALUE "      ".
             03  FILLER  PIC  X(048)  VALUE
                  "                                                ".
             03  FILLER  PIC  X(008) VALUE "        ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  ﾄﾞｳｷ ｴﾗｰ  ***".
             03  E-ME3   PIC  X(021) VALUE
                  "***  PROGRAM ｴﾗｰ  ***".
             03  E-ME5   PIC  X(021) VALUE
                  "***  REWRITE ｴﾗｰ  ***".
           COPY LSSEM.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "139" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "18" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "6" "6" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "4" "28" "7" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "6" "6" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" " " "7" "0" "12" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0105C-MID" "X" "7" "6" "6" " " "05C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0205C-MID" "N" "7" "20" "6" "0105C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" " " "8" "0" "12" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0106C-MID" "X" "8" "6" "6" " " "06C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0206C-MID" "N" "8" "20" "6" "0106C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" " " "9" "0" "18" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0107C-MID" "X" "9" "7" "6" " " "07C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0207C-MID" "N" "9" "14" "6" "0107C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0307C-MID" "N" "9" "65" "6" "0207C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "N" "16" "6" "6" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "N" "17" "6" "6" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "20" "39" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "53" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JNO" "9" "4" "13" "6" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JNO" BY REFERENCE W-JNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TEK" "N" "17" "13" "46" "A-JNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TEK" BY REFERENCE W-TEK "46" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "56" "1" "A-TEK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "267" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HED" " " "0" "0" "143" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-HED" "N" "4" "20" "6" " " "D-HED" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-HED" BY REFERENCE W-KBN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-HED" "99/99/99" "6" "13" "8" "01D-HED" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-HED" BY REFERENCE JMSTD-02S "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-HED" " " "7" "0" "56" "02D-HED" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103D-HED" "9" "7" "13" "4" " " "03D-HED" RETURNING RESU.
       CALL "SD_From" USING 
            "0103D-HED" BY REFERENCE JMSTD-04 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203D-HED" "N" "7" "27" "52" "0103D-HED" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0203D-HED" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-HED" " " "8" "0" "55" "03D-HED" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0104D-HED" "9" "8" "14" "3" " " "04D-HED" RETURNING RESU.
       CALL "SD_From" USING 
            "0104D-HED" BY REFERENCE JMSTD-10 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0204D-HED" "N" "8" "27" "52" "0104D-HED" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0204D-HED" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-HED" "N" "16" "13" "18" "04D-HED" " " RETURNING RESU.
       CALL "SD_From" USING 
            "05D-HED" BY REFERENCE W-HTS "18" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEI" " " "W-L" "0" "62" "D-HED" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MEI" "9" "W-L" "7" "6" " " "D-MEI" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MEI" BY REFERENCE JMSTD-03 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MEI" "N" "W-L" "14" "48" "01D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MEI" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MEI" "----,--9" "W-L" "63" "8" "02D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-MEI" BY REFERENCE W-SU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEIC" " " "W-L" "0" "62" "D-MEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MEIC" "X" "W-L" "7" "6" " " "D-MEIC" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MEIC" "X" "W-L" "14" "48" "01D-MEIC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MEIC" "X" "W-L" "63" "8" "02D-MEIC" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "77" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "77" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "21" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "21" "E-ME3" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
            CALL "DB_F_Open" USING
            "I-O" JMSTD_PNAME1 "SHARED" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2" BY
            REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE JMSTD-KEY3.
       M-10.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-JNO "A-JNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           PERFORM CHK-RTN THRU CHK-EX.
           IF  W-DCHK = ZERO
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-10
           END-IF
           PERFORM DSP-RTN THRU DSP-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-TEK "A-TEK" "N" "46"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-DMM = 9
               GO TO M-15
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-25
           END-IF
      *
           MOVE ZERO TO CNT W-CHK.
       M-30.
           ADD 1 TO CNT.
           IF  CNT > 6
               GO TO M-10
           END-IF
           IF  W-DC(CNT) = 0
               GO TO M-30
           END-IF
           MOVE W-JNO TO JMSTD-07.
           MOVE CNT TO JMSTD-08.
      *           READ JMSTD INVALID KEY
      *//////////////////////
           CALL "DB_Read" USING
            "INVALID KEY" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE W-TEKI TO JMSTD-13.
      *           REWRITE JMSTD-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JMSTD_PNAME1 JMSTD_LNAME JMSTD-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-30.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       CHK-RTN.
           MOVE ZERO TO W-DCHK CNT.
       CHK-010.
           ADD 1 TO CNT.
           IF  CNT > 6
               GO TO CHK-EX
           END-IF
           MOVE W-JNO TO JMSTD-07.
           MOVE CNT TO JMSTD-08.
      *           READ JMSTD WITH UNLOCK INVALID KEY
      *//////////////////////
           CALL "DB_Read" USING
            "INVALID KEY" JMSTD_PNAME1 BY REFERENCE JMSTD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO CHK-010
           END-IF
           MOVE 1 TO W-DC(CNT).
           GO TO CHK-010.
       CHK-EX.
           EXIT.
       DSP-RTN.
           MOVE ZERO TO CNT W-CHK.
       DSP-010.
           ADD 1 TO CNT.
           IF  CNT > 6
               GO TO DSP-090
           END-IF
           IF  W-DC(CNT) = 0
               COMPUTE W-L = CNT + 9
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               CALL "SD_Output" USING "D-MEIC" D-MEIC "p" RETURNING RESU
               GO TO DSP-010
           END-IF
           MOVE W-JNO TO JMSTD-07.
           MOVE CNT TO JMSTD-08.
      *           READ JMSTD WITH UNLOCK INVALID KEY
      *//////////////////////
           CALL "DB_Read" USING
            "INVALID KEY" JMSTD_PNAME1 BY REFERENCE JMSTD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO DSP-090
           END-IF
           IF  W-CHK NOT = 0
               GO TO DSP-020
           END-IF
           MOVE 1 TO W-CHK.
      *
           IF  JMSTD-01 = 0
               MOVE "受　注" TO W-KBN
           ELSE
               IF  JMSTD-01 = 5
                   MOVE "預　り" TO W-KBN
               ELSE
                   IF  JMSTD-01 = 6
                       MOVE "取よけ" TO W-KBN
                   ELSE
                       MOVE "　　　" TO W-KBN
                   END-IF
               END-IF
           END-IF
      *
           MOVE SPACE TO W-HTS W-TEK.
           MOVE JMSTD-13 TO W-TEKI.
      *
           MOVE JMSTD-04 TO T-TCD.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
           END-IF
      *
           MOVE JMSTD-04 TO TC-TCD.
           MOVE JMSTD-10 TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
           END-IF
      *
           CALL "SD_Output" USING "D-HED" D-HED "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TEK" A-TEK "p" RETURNING RESU.
       DSP-020.
           MOVE JMSTD-03 TO HI-HCD.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
           END-IF
           MOVE ZERO TO W-C W-SU.
       DSP-030.
           ADD 1 TO W-C.
           IF  W-C NOT = 11
               COMPUTE W-SU = JMSTD-1111(W-C) - JMSTD-1211(W-C)
                            - JMSTD-141(W-C)  - JMSTD-151(W-C) + W-SU
               GO TO DSP-030
           END-IF
           COMPUTE W-L = JMSTD-08 + 9.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L < 9 OR > 15
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO DSP-EX
           END-IF
           CALL "SD_Output" USING "D-MEI" D-MEI "p" RETURNING RESU.
           GO TO DSP-010.
       DSP-090.
           IF  W-CHK = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       DSP-EX.
           EXIT.

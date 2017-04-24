       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 PR225U.
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       SOURCE-COMPUTER.            SYSTEM-100.
       OBJECT-COMPUTER.            SYSTEM-100.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
       77  ERR-STAT            PIC X(02).
       77  ACT                 PIC 9(01).
       01  W-DATE.
           02  F               PIC X(02).
           02  W-YMD           PIC 9(06).
      *
       COPY  LWMSG_PR.
       COPY  SIWAKE.
       COPY  LGYM.
      *
       77  USER_ID             PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE     PIC  X(003) VALUE ZERO.
       77  ESTAT               PIC  X(002).
       77  RESU                PIC  9(001).
       77  RET                 PIC  9(001) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER          PIC  X(12)  VALUE "CLEAR SCREEN".
       01  DSP-AREA.
           03  FILLER     PIC   N(11) VALUE  "åªóaã‡É}ÉXÉ^ñ¢ìoò^".
           03  FILLER     PIC   N(04) VALUE  "ì`ï[ì˙ït".
           03  FILLER     PIC   X(01) VALUE  "=".
           03  DSP-TRDATE PIC   9(06).
           03  FILLER     PIC   N(04) VALUE  "ì`ï[î‘çÜ".
           03  FILLER     PIC   X(01) VALUE  "=".
           03  DSP-JUNLNO PIC   9(06).
           03  FILLER     PIC   X(01) VALUE  "-".
           03  DSP-LINENO PIC   9(02).
           03  FILLER     PIC   N(04) VALUE  "ë›éÿãÊï™".
           03  FILLER     PIC   X(01) VALUE  "=".
           03  DSP-DR-CR  PIC   9(01).
           03  FILLER     PIC   N(02) VALUE  "ämîF".
           03  FILLER     PIC   X(03) VALUE  "( )".
       01  ACP-AREA.
           03  ACP-010    PIC   X(1).
       01  DSP-AREA2.
           02  FILLER     PIC   N(007) VALUE  "É}ÉXÉ^çXêVÅ@áB".
       COPY  LSMSG_PR.
       PROCEDURE                   DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-CLR
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "24" "0" "72" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA" "N" "24" "1" "22" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-AREA" "N" "24" "24" "8" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-AREA" "X" "24" "32" "1" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TRDATE" "9" "24" "33" "6" "03DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TRDATE" BY REFERENCE W-YMD "6" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "04DSP-AREA" "N" "24" "40" "8" "DSP-TRDATE" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05DSP-AREA" "X" "24" "48" "1" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-JUNLNO" "9" "24" "49" "6" "05DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-JUNLNO" BY REFERENCE JUNLNO "6" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "06DSP-AREA" "X" "24" "55" "1" "DSP-JUNLNO" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-LINENO" "9" "24" "56" "2" "06DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-LINENO" BY REFERENCE LINENO "2" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "07DSP-AREA" "N" "24" "59" "8" "DSP-LINENO" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "08DSP-AREA" "X" "24" "67" "1" "07DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-DR-CR" "9" "24" "68" "1" "08DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-DR-CR" BY REFERENCE DR-CR OF SD-REC "2" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "09DSP-AREA" "N" "24" "71" "4" "DSP-DR-CR" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "10DSP-AREA" "X" "24" "76" "3" "09DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-010" "X" "24" "77" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-010" BY REFERENCE ACT "1" "0" RETURNING RESU.
      *DSP-AREA2
       CALL "SD_Init" USING
            "DSP-AREA2" " " "0" "0" "14" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA2" "RN" "1" "35" "14" " " "DSP-AREA2"
            RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       ST.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-AREA2" DSP-AREA2 "p"
                                  RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" SSD_PNAME1 "EXCLUSIVE"BY REFERENCE SSD_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" GYM_PNAME1 "EXCLUSIVE"BY REFERENCE GYM_IDLST "1"
            "GYM-KEY" BY REFERENCE GYM-KEY.
       MAIN00.
      *           READ        SSD         AT  END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSD_PNAME1 BY REFERENCE SD-REC " " RETURNING RET.
           IF  RET = 1
               GO  TO      STOP-RTN
           END-IF.
           IF  TEG-BAN     =       0
               GO  TO      MAIN00
           END-IF.
           IF  TEG-BAN     <       13
               GO  TO      MAIN01
           END-IF.
           IF  TEG-BAN     =       30
               GO  TO      MAIN01
           END-IF.
           GO          TO          MAIN00.
       MAIN01.
           MOVE        ACCNTCD     TO      GYM-011.
           IF  TEG-BAN  =  30
               MOVE    ZERO        TO      HOACCNT
           END-IF.
           MOVE        HOACCNT     TO      GYM-012.
      *           READ        GYM         INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" GYM_PNAME1 BY REFERENCE GYM-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO      E-STOP
           END-IF.
           IF  DR-CR       =       1
               ADD         AMOUNT  TO  GYM-041
           ELSE
               ADD         AMOUNT  TO  GYM-042
           END-IF.
           MOVE        GYM-KEY     TO      ERR-K.
      *           REWRITE     GYM-R       INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            GYM_PNAME1 GYM_LNAME GYM-R RETURNING RET.
           IF  RET = 1
               MOVE  "GYM"    TO   ERR-F
               MOVE  "R"      TO   ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
           GO          TO          MAIN00.
       E-STOP.
           MOVE    TRDATE      TO  W-DATE.
           CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
                                           RETURNING RESU.
           CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                             RETURNING RESU.
           CALL "SD_Accept" USING
               BY REFERENCE ACP-010 "ACP-010" "X" "1"
               BY REFERENCE ESTAT RETURNING RESU.
           GO TO       MAIN00.
       STOP-RTN.
           PERFORM CLSE-ENT  THRU  CLSE-EXT.
           CALL "DB_Close".
           STOP        RUN.
      *****
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE SSD_IDLST SSD_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE GYM_IDLST GYM_PNAME1.
       CLSE-EXT.
           EXIT.
      *****
       COPY LPMSG_PR.

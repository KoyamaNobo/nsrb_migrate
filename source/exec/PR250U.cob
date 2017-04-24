       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 PR250U.
      *>==============================================================<*
      *>                                                              <*
      *>             USER     NAME ¥¥¥¥¥¥¥                            <*
      *>             PROGRAM  NAME ¥¥¥¥¥¥¥ PR250U                     <*
      *>             PROGRAM  TITLE ¥¥¥¥¥¥ ÌÞ»ÞÝ Ï½À  º³¼Ý            <*
      *>             AUTHOR   ¥¥¥¥¥¥¥¥¥¥¥¥                            <*
      *>             DATE     WRITTEN ¥¥¥¥ 90/12/19                   <*
      *>                                                              <*
      *>==============================================================<*
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       SOURCE-COMPUTER.            SYSTEM100.
       OBJECT-COMPUTER.            SYSTEM100.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
       77  ERR-STAT               PIC X(02).
       77  INV-SW                 PIC 9(01).
       01  W-AREA.
           02  CHK                PIC X(01).
           02  END-SW             PIC 9(01).
           02  W-FILE             PIC N(11).
           02  I                  PIC 9(02).
       01  W-DATE.
           02  F                  PIC 9(02).
           02  W-YMD              PIC 9(06).
      *
       COPY  LWMSG_PR.
       COPY  SIWAKE.
       COPY  ACCUNT.
       COPY  LBUZAN.
       COPY  FCTL.
      *
       77  USER_ID             PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE     PIC  X(003) VALUE ZERO.
       77  ESTAT               PIC  X(002).
       77  RESU                PIC  9(001).
       77  RET                 PIC  9(001) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER          PIC   X(12)  VALUE "CLEAR SCREEN".
       01  DSP-AREA.
           03  DSP-FILE        PIC   N(11).
           03  FILLER          PIC   N(04) VALUE  "“`•[“ú•t".
           03  FILLER          PIC   X(01) VALUE  "=".
           03  DSP-TRDATE      PIC   9(06).
           03  FILLER          PIC   N(04) VALUE  "“`•[”Ô†".
           03  FILLER          PIC   X(01) VALUE  "=".
           03  DSP-JUNLNO      PIC   9(06).
           03  FILLER          PIC   X(01) VALUE  "-".
           03  DSP-LINENO      PIC   9(02).
           03  FILLER          PIC   N(04) VALUE  "‘ÝŽØ‹æ•ª".
           03  FILLER          PIC   X(01) VALUE  "=".
           03  DSP-DR-CR       PIC   9(01).
           03  FILLER          PIC   N(02) VALUE  "Šm”F".
           03  FILLER          PIC   X(03) VALUE  "( )".
       01  ACP-AREA.
           03  ACP-010         PIC   X(1).
       01  DSP-AREA2.
           02  FILLER          PIC   N(007) VALUE  "ƒ}ƒXƒ^XV@‡F".
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
            "DSP-FILE" "N" "24" "1" "22" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_From" USING
            "DSP-FILE" BY REFERENCE W-FILE "22" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA" "N" "24" "24" "8" "DSP-FILE" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-AREA" "X" "24" "32" "1" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TRDATE" "9" "24" "33" "6" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TRDATE" BY REFERENCE W-YMD "6" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-AREA" "N" "24" "40" "8" "DSP-TRDATE" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04DSP-AREA" "X" "24" "48" "1" "03DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-JUNLNO" "9" "24" "49" "6" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-JUNLNO" BY REFERENCE JUNLNO "6" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "05DSP-AREA" "X" "24" "55" "1" "DSP-JUNLNO" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-LINENO" "9" "24" "56" "2" "05DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-LINENO" BY REFERENCE LINENO "2" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "06DSP-AREA" "N" "24" "59" "8" "DSP-LINENO" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "07DSP-AREA" "X" "24" "67" "1" "06DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-DR-CR" "9" "24" "68" "1" "07DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-DR-CR" BY REFERENCE DR-CR OF SD-REC "2" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "08DSP-AREA" "N" "24" "71" "4" "DSP-DR-CR" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "09DSP-AREA" "X" "24" "76" "3" "08DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-010" "X" "24" "77" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-010" BY REFERENCE CHK "1" "0" RETURNING RESU.
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
       HAJIME.
           PERFORM     INIT-RTN    THRU    INIT-EX.
           PERFORM     MAIN-RTN    THRU    MAIN-EX
                       UNTIL       END-SW  =  1.
           PERFORM     CLSE-ENT    THRU    CLSE-EXT.
       OWARI.
           CALL "DB_Close".
           STOP  RUN.
       INIT-RTN.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-AREA2" DSP-AREA2 "p"
                                  RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY1" BY REFERENCE FCTL-KEY1.
           MOVE        "SUB   "    TO      FCTL-KEY1.
      *           READ        FCTL-F      WITH  UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "INV-CON" INV-CON "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                             RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           IF  FCTL-SUB5 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               CALL "DB_Close"
               STOP RUN
           END-IF.
           MOVE        "DATE  "    TO      FCTL-KEY1.
      *           READ        FCTL-F      WITH  UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "INV-CON" INV-CON "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                             RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           MOVE        FCTL-REC1   TO      Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SSD_PNAME1 "EXCLUSIVE" BY REFERENCE SSD_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "I-O" BZM-F_PNAME1 "SHARED" BY REFERENCE BZM-F_IDLST "1"
            "BZM-KEY" BY REFERENCE BZM-KEY.
           INITIALIZE  W-AREA.
       INIT-EX.
           EXIT.
       MAIN-RTN.
      *           READ        SSD         AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSD_PNAME1 BY REFERENCE SD-REC " " RETURNING RET.
           IF  RET = 1
               MOVE  1     TO    END-SW
               GO  TO   MAIN-EX
           END-IF.
           MOVE        ACCNTCD     TO        AM-KEY.
      *           READ        AM  WITH  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  "‰È–Úƒ}ƒXƒ^–¢“o˜^"   TO   W-FILE
               MOVE  TRDATE       TO  W-DATE
               CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
                                               RETURNING RESU
               CALL "SD_Accept" USING
                     BY REFERENCE ACP-010 "ACP-010" "X" "1"
                     BY REFERENCE ESTAT RETURNING RESU
               GO  TO  MAIN-RTN
           END-IF.
           IF  BS-PL       =         0
               GO  TO      MAIN-RTN
           END-IF.
           MOVE        TRDATE      TO      ZYMD.
           PERFORM     Z-RTN       THRU    Z-EXT.
           IF  ZI > 15
               GO TO MAIN-RTN
           END-IF.
           MOVE        ZI          TO      I.
           MOVE 0     TO INV-SW.
           MOVE        SECTCD      TO      BZM-BMON.
           MOVE        ZERO        TO      BZM-YOBI.
           MOVE        ACCNTCD     TO      BZM-KMCD.
      *           READ        BZM-F       INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" BZM-F_PNAME1 BY REFERENCE BZM-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1     TO INV-SW
               INITIALIZE BZM-REC
               MOVE SECTCD     TO BZM-BMON
               MOVE ZERO       TO BZM-YOBI
               MOVE ACCNTCD    TO BZM-KMCD
           END-IF.
           IF  DR-CR OF SD-REC     =       1
               ADD   AMOUNT     TO      BZM-TJKR(I)
           END-IF.
           IF  DR-CR OF SD-REC     =       2
               ADD   AMOUNT     TO      BZM-TJKS(I)
           END-IF.
           MOVE        BZM-KEY     TO     ERR-K.
           IF  INV-SW = 0
               GO TO MAIN-999
           END-IF
      *           WRITE BZM-REC INVALID
      *///////////////
           CALL "DB_Insert" USING
            BZM-F_PNAME1 BZM-F_LNAME BZM-REC RETURNING RET.
           IF  RET = 1
               MOVE "BZM-F"     TO ERR-F
               MOVE "W"         TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
           GO TO MAIN-EX.
       MAIN-999.
      *           REWRITE     BZM-REC     INVALID
      *///////////////
           CALL "DB_Update" USING
            BZM-F_PNAME1 BZM-F_LNAME BZM-REC RETURNING RET.
           IF  RET = 1
               MOVE  "BZM-F"    TO    ERR-F
               MOVE  "R"        TO    ERR-M
               PERFORM ERR-ENT    THRU  ERR-EXT
           END-IF.
       MAIN-EX.
           EXIT.
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE SSD_IDLST SSD_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BZM-F_IDLST BZM-F_PNAME1.
       CLSE-EXT.
           EXIT.
       COPY  LPMSG_PR.

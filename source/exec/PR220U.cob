       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PR220U.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM100.
       OBJECT-COMPUTER.    NEAC-SYSTEM100.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       77  INV-SW          PIC 9(1).
       77  ERR-STAT        PIC X(02).
       01  I               PIC 9(02).
       01  W-ACT           PIC X(01).
       01  W-FILE          PIC N(11).
       01  W-DATE.
           02  F           PIC 9(02).
           02  W-YMD       PIC 9(06).
      *
       COPY    LWMSG_PR.
       COPY    SIWAKE.
       COPY    FCTL.
       COPY    LKAZAN.
       COPY    ACCUNT.
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
           03  DSP-FILE PIC   N(11).
           03  FILLER   PIC   N(04) VALUE  "“`•[“ú•t".
           03  FILLER   PIC   X(01) VALUE  "=".
           03  DSP-TRDATE     PIC   9(06).
           03  FILLER   PIC   N(04) VALUE  "“`•[”Ô†".
           03  FILLER   PIC   X(01) VALUE  "=".
           03  DSP-JUNLNO     PIC   9(06).
           03  FILLER   PIC   X(01) VALUE  "-".
           03  DSP-LINENO     PIC   9(02).
           03  FILLER   PIC   N(04) VALUE  "‘ÝŽØ‹æ•ª".
           03  FILLER   PIC   X(01) VALUE  "=".
           03  DSP-DR-CR      PIC   9(01).
           03  FILLER   PIC   N(02) VALUE  "Šm”F".
           03  FILLER   PIC   X(03) VALUE  "( )".
       01  ACP-AREA.
           03  ACP-010  PIC   X(1).
       01  DSP-AREA2.
           02  FILLER   PIC   N(007) VALUE "ƒ}ƒXƒ^XV@‡A".
       COPY    LSMSG_PR.
       PROCEDURE       DIVISION.
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
            "ACP-010" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
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
            "INPUT" SSD_PNAME1 "EXCLUSIVE" BY REFERENCE SSD_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "EXCLUSIVE" BY REFERENCE FCTL-F_IDLST
            "1" "FCTL-KEY1" BY REFERENCE FCTL-KEY1.
           CALL "DB_F_Open" USING
            "I-O" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "I-O" KZM-F_PNAME1 "EXCLUSIVE" BY REFERENCE KZM-F_IDLST "1"
            "KZM-KEY" BY REFERENCE KZM-KEY.
           MOVE  "DATE  "         TO      FCTL-KEY1.
      *           READ  FCTL-F           UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING BY REFERENCE SSD_IDLST SSD_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE KZM-F_IDLST KZM-F_PNAME1
               CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "INV-CON" INV-CON "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                       RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           MOVE  FCTL-REC1        TO      Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
       ST-10.
      *           READ       SSD           AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSD_PNAME1 BY REFERENCE SD-REC " " RETURNING RET.
           IF  RET = 1
               PERFORM  CLSE-ENT     THRU    CLSE-EXT
               CALL "DB_Close"
               STOP     RUN
           END-IF.
           MOVE       ACCNTCD OF SD-REC TO    AM-KEY OF  AM-REC.
      *           READ       AM           INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AM_PNAME1 BY REFERENCE AM-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  "‰È–Úƒ}ƒXƒ^–¢“o˜^"    TO      W-FILE
               MOVE  TRDATE        TO  W-DATE
               CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
                              RETURNING RESU
               CALL "SD_Output" USING "ACP-010" ACP-010 "p"
                                           RETURNING RESU
               CALL "SD_Accept" USING
                   BY REFERENCE ACP-010 "ACP-010" "X" "1"
                   BY REFERENCE ESTAT RETURNING RESU
               GO TO    ST-10
           END-IF.
           IF  DR-CR  OF SD-REC =      1
               ADD      AMOUNT OF SD-REC TO     DDR
           END-IF.
           IF  DR-CR  OF SD-REC =      2
               ADD      AMOUNT OF SD-REC TO     DCR
           END-IF.
           MOVE       AM-KEY       TO      ERR-K.
      *           REWRITE    AM-REC       INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            AM_PNAME1 AM_LNAME AM-REC RETURNING RET.
           IF  RET = 1
               MOVE  "AM"   TO      ERR-F
               MOVE  "R"    TO      ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
           MOVE       TRDATE         TO     ZYMD.
           PERFORM    Z-RTN        THRU     Z-EXT.
           MOVE       ZI             TO     I.
           IF  I  >  15
               GO TO    ST-10
           END-IF.
           MOVE 0     TO INV-SW.
           MOVE       ACCNTCD OF SD-REC TO    KZM-KEY.
      *           READ       KZM-F        INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KZM-F_PNAME1 BY REFERENCE KZM-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1     TO INV-SW
               INITIALIZE KZM-R
               MOVE ACCNTCD     TO KZM-KEY
           END-IF.
           IF  DR-CR OF SD-REC  =  1
               ADD    AMOUNT         TO     KZM-TJKR(I)
           ELSE
               ADD    AMOUNT         TO     KZM-TJKS(I)
           END-IF.
           MOVE       KZM-KEY        TO     ERR-K.
           IF  INV-SW = 0
               GO TO ST-999
           END-IF
      *           WRITE KZM-R INVALID
      *///////////////
           CALL "DB_Insert" USING
            KZM-F_PNAME1 KZM-F_LNAME KZM-R RETURNING RET.
           IF  RET = 1
               MOVE  "KZM-F"  TO     ERR-F
               MOVE  "W"      TO     ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
           GO TO ST-10.
       ST-999.
      *           REWRITE    KZM-R          INVALID
      *///////////////
           CALL "DB_Insert" USING
            KZM-F_PNAME1 KZM-F_LNAME KZM-R RETURNING RET.
           IF  RET = 1
               MOVE  "KZM-F"  TO     ERR-F
               MOVE  "R"      TO     ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
           GO TO      ST-10.
       ED.
           EXIT.
      ******
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE SSD_IDLST SSD_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KZM-F_IDLST KZM-F_PNAME1.
       CLSE-EXT.
           EXIT.
      ******
       COPY  LPMSG_PR.

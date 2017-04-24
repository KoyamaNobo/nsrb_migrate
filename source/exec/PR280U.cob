       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PR280U.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM100.
       OBJECT-COMPUTER.    NEAC-SYSTEM100.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT             PIC  X(02).
       01  W-AREA.
           02  W-OKC            PIC  X(01).
      *
       COPY    LWMSG_PR.
       COPY    ACCUNT.
      *
       77  USER_ID              PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE      PIC  X(003) VALUE ZERO.
       77  ESTAT                PIC  X(002).
       77  RESU                 PIC  9(001).
       77  RET                  PIC  9(001) VALUE ZERO.
      *
       01  DSP-CLR.
           03  FILLER           PIC X(12)  VALUE "CLEAR SCREEN".
       01  DSP-AREA.
           02  FILLER           PIC  X(10)  VALUE  " “úŽŸŒJ‰z ".
           02  FILLER           PIC  N(02)  VALUE  "Šm”F".
           02  FILLER           PIC  X(13)  VALUE  "OK=1,NO=9 ( )".
       01  ACP-AREA.
           02  ACP-OKC          PIC  X(01).
       COPY  LSMSG_PR.
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
            "DSP-AREA" " " "0" "0" "27" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA" "RX" "1" "33" "10" " " "DSP-AREA"
             RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-AREA" "N" "24" "61" "4" "01DSP-AREA" " "
             RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-AREA" "X" "24" "66" "13" "02DSP-AREA" " "
             RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "24" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-OKC" "X" "24" "77" "1" " " "ACP-AREA"
             RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-OKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       ST.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
                                  RETURNING RESU.
           PERFORM    ACP-RTN      THRU   ACP-EX.
           CALL "DB_F_Open" USING
            "I-O SEQUENTIAL" AM_PNAME1 " " BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
       ST-10.
      *           READ       AM           AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" AM_PNAME1 BY REFERENCE AM-REC " " RETURNING RET.
           IF  RET = 1
               PERFORM CLSE-ENT    THRU  CLSE-EXT
               GO  TO  ED
           END-IF.
           MOVE       0            TO     DDR.
           MOVE       0            TO     DCR.
           MOVE       AM-KEY       TO     ERR-K.
      *           REWRITE    AM-REC       INVALID KEY
      *//////////////////////
           CALL "DB_Update" USING
            AM_PNAME1 AM_LNAME AM-REC RETURNING RET.
           IF  RET = 1
               MOVE  "AM"   TO     ERR-F
               MOVE  "R"    TO     ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
           GO TO      ST-10.
       ED.
           CALL "DB_Close".
           STOP  RUN.
       ACP-RTN.
           CALL "SD_Accept" USING
               BY REFERENCE ACP-OKC "ACP-OKC" "X" "1"
               BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                        "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP    RUN
           END-IF.
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  ACP-RTN
           END-IF.
           IF  W-OKC  NOT =  "1"  AND  "9"
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                               RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                               RETURNING RESU
               GO  TO  ACP-RTN
           END-IF.
           IF  W-OKC  =  "9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP    RUN
           END-IF.
       ACP-EX.
           EXIT.
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
       CLSE-EXT.
           EXIT.
       COPY  LPMSG_PR.

       IDENTIFICATION         DIVISION.
       PROGRAM-ID.            JT005U.
      *********************************************************
      *    PROGRAM         :  受注マスタ　担当区分セット      *
      *    PRINTER TYPE    :  ****                            *
      *    SCREEN          :  SJ010I                          *
      *    DATE      　　  :  03/09/10                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT            DIVISION.
       CONFIGURATION          SECTION.
       SOURCE-COMPUTER.       NEAC-SYSTEM100.
       OBJECT-COMPUTER.       NEAC-SYSTEM100.
       DATA                   DIVISION.
       WORKING-STORAGE        SECTION.
       77  ERR-STAT           PIC  X(02).
       77  OKC                PIC  9(01).
           COPY   LWMSG.
      *
           COPY   LJMSTD.
           COPY   LITM.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  ACT-AREA.
           02  A-OKC   PIC  9(01).
       01  DSP-AREA.
           02  FILLER  PIC X(32) VALUE
                 "                                ".
           02  FILLER  PIC N(15) VALUE
               "受注マスタ　担当者区分　セット".
           02  FILLER  PIC X(30) VALUE
                 "確認 (OK=1,終了=PF9) -->  ﾘﾀｰﾝ".
           COPY LSMSG.
       PROCEDURE              DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-CLEAR
       CALL "SD_Init" USING
           "DSP-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
          "01DSP-CLEAR" "X" "1" "0" "12" " " "DSP-CLEAR" RETURNING RESU.
      *ACT-AREA
       CALL "SD_Init" USING 
            "ACT-AREA" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-OKC" "9" "24" "60" "1" " " "ACT-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "92" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" "RX" "1" "19" "32" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" "N" "1" "20" "30" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" "X" "24" "35" "30" "02DSP-AREA" " "
            RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
      *************************************************
      *    ＭＡＩＮ－ＲＴＮ　 （主処理）              *
      *************************************************
       MAIN-RTN.
           PERFORM  INIT-RTN       THRU  INIT-EX.
           PERFORM  UPD-RTN        THRU  UPD-EX.
           PERFORM  END-RTN        THRU  END-EX.
           CALL "DB_Close".
           STOP     RUN.
           COPY   LPMSG.
      *
       INIT-RTN.
           CALL "SD_Output" USING
            "DSP-CLEAR" DSP-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
       INIT-010.
           CALL "SD_Accept" USING BY REFERENCE A-OKC "A-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT         =   "P9"
               CALL "SD_Output" USING
                "DSP-CLEAR" DSP-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP    RUN
           END-IF
           IF  ESTAT    NOT  =   "01"  AND "06"
               GO  TO  INIT-010
           END-IF
           IF  OKC      NOT  =    1
               GO  TO  INIT-010
           END-IF
      *
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-OKC" A-OKC "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "I-O" JMSTD_PNAME1 "SHARED" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2"
            BY REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE
            JMSTD-KEY3.
       INIT-EX.
           EXIT.
      *
       UPD-RTN.
      *           READ    JMSTD     NEXT  RECORD   AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-EX
           END-IF
           MOVE    JMSTD-04  TO    T-KEY.
      *           READ    T-M       UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-RTN
           END-IF
           IF  JMSTD-91      =   T-TNC
               GO  TO  UPD-RTN
           END-IF
           MOVE    T-TNC     TO    JMSTD-91.
      *           REWRITE  JMSTD-R               INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JMSTD_PNAME1 JMSTD_LNAME JMSTD-R RETURNING RET.
           IF  RET = 1
               MOVE  "R"         TO  ERR-M
               MOVE  "JMSTD"     TO  ERR-F
               MOVE  JMSTD-KEY1  TO  ERR-K
               PERFORM     ERR-RTN   THRU      ERR-EX
           END-IF
           GO  TO  UPD-RTN.
       UPD-EX.
           EXIT.
      *
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
       END-EX.
           EXIT.

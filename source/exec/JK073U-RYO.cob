       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       JK073U.
      ****************************************************************
      *    出　荷　実　績　送　信　後　処　理                        *
      *                              90/ 5/ 7           IKUMI.N      *
      ****************************************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       DATA              DIVISION.
       WORKING-STORAGE   SECTION.
      *----< ﾜｰｸ ｴﾘｱ >----*
       77  ERR-STAT      PIC X(02).
           COPY    LWMSG.
      *
           COPY    LJOLJF-RYO.
           COPY    L-JSTR-RYO.
      *
       77  END-STS            PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  DSP-HYOJI.
           02  FILLER  PIC N(15)  VALUE
                   "＊＊　出荷実績送信後処理　＊＊".
           COPY    LSERR.
      *
      *==============================================================*
       PROCEDURE         DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-CLR
       CALL "SD_Init" USING
           "DSP-CLR" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-CLR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *DSP-HYOJI
       CALL "SD_Init" USING 
            "DSP-HYOJI" " " "1" "0" "30" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-HYOJI" "N" "1" "20" "30" " " "DSP-HYOJI"
            RETURNING RESU.
      *
           COPY LSERR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
      *--------------------------------------------------------------*
      *    ﾒ ｲ ﾝ  ﾙ ｰ ﾁ ﾝ                                            *
      *--------------------------------------------------------------*
       MAINLINE-ROUTINE.
           PERFORM  INIT-RTN  THRU  INIT-RTN-EXIT.
           PERFORM  OPEN-RTN  THRU  OPEN-RTN-EXIT.
           PERFORM  PROC-RTN  THRU  PROC-RTN-EXIT.
           PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT.
      *
       MAINLINE-END.
           CALL "DB_Close".
           STOP RUN.
      *--------------------------------------------------------------*
      *    P R O C  -  R T N                                         *
      *--------------------------------------------------------------*
       PROC-RTN.
      *           READ    JOLJF     NEXT   AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JOLJF_PNAME1 BY REFERENCE JOLJF11-REC " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  PROC-RTN-EXIT
           END-IF
           MOVE    JOLJF11-02       TO  JSTR-01.
           MOVE    JOLJF11-03       TO  JSTR-02.
      *           READ    JSTR      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JSTR_PNAME1 BY REFERENCE JSTR-R " " RETURNING RET.
           IF  RET = 1
               GO  TO  PROC-RTN
           END-IF
           MOVE    1                TO  JSTR-17.
      *
      *           REWRITE  JSTR-R   INVALID
      *///////////////
           CALL "DB_Update" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               GO  TO  PROC-RTN
           END-IF
           GO  TO   PROC-RTN.
       PROC-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    I N I T  -  R T N                                         *
      *--------------------------------------------------------------*
       INIT-RTN.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-HYOJI" DSP-HYOJI "p" RETURNING RESU.
      *
       INIT-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    O P E N  -  R T N                                         *
      *--------------------------------------------------------------*
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "INPUT" JOLJF_PNAME1 " " BY REFERENCE JOLJF_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
       OPEN-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    C L S E  -  R T N                                         *
      *--------------------------------------------------------------*
       CLSE-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JOLJF_IDLST JOLJF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
       CLSE-RTN-EXIT.
           EXIT.
      ******
       COPY    LPERR.
      ******

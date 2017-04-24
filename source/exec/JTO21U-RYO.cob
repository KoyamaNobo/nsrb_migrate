       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       JTO21U.
      ****************************************************************
      *    Ｏ／Ｌ　送　信　後　処　理                                *
      *                              89/ 8/16   NO.175  N.HARA       *
      *    JS-SIGN  :  旧（送信）= 0  ,  新 = 1                      *
      ****************************************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       DATA              DIVISION.
       WORKING-STORAGE   SECTION.
      *----< ﾜｰｸ ｴﾘｱ >----*
       77  JS-SIGN       PIC 9(01).
       77  NN            PIC 9(02).
       77  NN-W          PIC 9(02).
       77  ERR-STAT      PIC X(02).
       77  END-SW        PIC 9(01)   VALUE 0.
       01  W-BIT         PIC 9(04).
       01  W-NO          PIC 9(02) VALUE 0.
           COPY    LWMSG.
      *
           COPY    L-JOSF-RYO.
           COPY    L-JSTR.
           COPY    L-JNIF.
           COPY    LOKJF.
           COPY    LITDNW.
           COPY    LITDNN.
           COPY    L-TDIF.
           COPY    LITDNA.
           COPY    L-JOSR.
           COPY    L-JOJF.
      *
       77  END-STS            PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-HYOJI.
           02  FILLER  PIC N(14)  VALUE
                 "＊＊　Ｏ／Ｌ送信後処理　＊＊".
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
           "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *DSP-HYOJI
       CALL "SD_Init" USING 
            "DSP-HYOJI" " " "1" "0" "28" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-HYOJI" "N" "1" "20" "28" " " "DSP-HYOJI"
            RETURNING RESU.
      *
           COPY LSERR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *--------------------------------------------------------------*
      *    ﾒ ｲ ﾝ  ﾙ ｰ ﾁ ﾝ                                            *
      *--------------------------------------------------------------*
       MAINLINE-ROUTINE.
           PERFORM  INIT-RTN  THRU  INIT-RTN-EXIT.
           PERFORM  OPEN-RTN  THRU  OPEN-RTN-EXIT.
           PERFORM  READ-RTN  THRU  READ-RTN-EXIT.
           PERFORM  PROC-RTN  THRU  PROC-RTN-EXIT
               UNTIL  END-SW  =  9.
           PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT.
           PERFORM  ENDR-RTN  THRU  ENDR-RTN-EXIT.
      *
       MAINLINE-END.
           CALL "DB_Close".
           STOP RUN.
      *--------------------------------------------------------------*
      *    P R O C  -  R T N                                         *
      *--------------------------------------------------------------*
       PROC-RTN.
           IF  JOLSF11-01  =  11
      *----< ｼｭｯｶｻｼｽﾞﾄﾗﾝ ｺｳｼﾝｼｮﾘ >----*
               MOVE  JOLSF11-02  TO  JSTR-01
               MOVE  JOLSF11-03  TO  JSTR-02
               PERFORM  UPD2-RTN  THRU  UPD2-RTN-EXIT
           END-IF
           IF  JOLSF12-01  =  12
      *----< ﾆﾌﾀﾞﾄﾗﾝ ｺｳｼﾝｼｮﾘ >----*
               MOVE  JOLSF121-01  TO  JNIF1-01
               MOVE  JOLSF121-02  TO  JNIF1-02
               PERFORM  UPD3-RTN  THRU  UPD3-RTN-EXIT
               IF  JOLSF121-21  NOT =  SPACE
                   MOVE  JOLSF121-21  TO  JNIF2-01
                   MOVE  JOLSF121-22  TO  JNIF2-02
                   PERFORM  UPD4-RTN  THRU  UPD4-RTN-EXIT
               END-IF
           END-IF
           IF  JOLSF13-01  =  13
      *----< ｵｸﾘｼﾞｮｳﾌｧｲﾙ ｺｳｼﾝｼｮﾘ >----*
               PERFORM  UPD5-RTN  THRU  UPD5-RTN-EXIT
                   VARYING  NN  FROM  1  BY  1  UNTIL  NN  >  4
           END-IF
           IF  JOLSF14-01  =  14
               MOVE  JOLSF141-KEY TO  TDNW1-KEY
               PERFORM  UPD6-RTN  THRU  UPD6-RTN-EXIT
           END-IF
           IF  JOLSF15-01  =  15
               MOVE  JOLSF151-KEY TO  TDNN1-KEY
               PERFORM  UPD7-RTN  THRU  UPD7-RTN-EXIT
           END-IF
           IF  JOLSF16-01  =  16
               MOVE  JOLSF16-KEY  TO  TDI-KEY
               PERFORM  UPD8-RTN  THRU  UPD8-RTN-EXIT
           END-IF
           IF  JOLSF17-01  =  17
               MOVE  JOLSF17-KEY  TO  TDNA-KEY
               PERFORM  UPD9-RTN  THRU  UPD9-RTN-EXIT
           END-IF
           PERFORM  READ-RTN  THRU  READ-RTN-EXIT.
       PROC-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    I N I T  -  R T N                                         *
      *--------------------------------------------------------------*
       INIT-RTN.
           ACCEPT   JS-SIGN FROM ARGUMENT-VALUE.
           ACCEPT   W-BIT FROM ARGUMENT-VALUE.
      **
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-HYOJI" DSP-HYOJI "p" RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "INPUT" JOJF_PNAME1 "SHARED" BY REFERENCE JOJF_IDLST "1"
            "JOJF-KEY" BY REFERENCE JOJF-KEY.
       INIT-010.
      *           READ JOJF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JOJF_PNAME1 BY REFERENCE JOJF-REC
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO INIT-090
           END-IF
           IF  JOJF-061 = 1
               ADD 1 TO W-NO
           END-IF
           GO TO INIT-010.
       INIT-090.
           CALL "DB_F_Close" USING BY REFERENCE JOJF_IDLST JOJF_PNAME1.
       INIT-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    R E A D  -  R T N                                         *
      *--------------------------------------------------------------*
       READ-RTN.
      *           READ    JOLSF     AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JOLSF_PNAME1 BY REFERENCE JOLSF-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  9  TO  END-SW
           END-IF.
      *
       READ-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    ﾌ ｧ ｲ ﾙ  ｺ ｳ ｼ ﾝ  ｼ ｮ ﾘ                                   *
      *--------------------------------------------------------------*
      *====<  U P D 2  -  R T N  >====*
       UPD2-RTN.
      *           READ   JSTR     INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO UPD2-RTN-EXIT
           END-IF
           MOVE   1   TO   JSTR-158.
           MOVE  "1"  TO   JSTR-19.
      *           REWRITE  JSTR-R  INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE  "JSTR"       TO    ERR-F
               MOVE  "R"          TO    ERR-M
               MOVE   JSTR-KEY    TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP   RUN
           END-IF.
       UPD2-RTN-EXIT.
           EXIT.
      *====<  U P D 3  -  R T N  >====*
       UPD3-RTN.
      *           READ   JNIF     INVALID  KEY
      *//////////////////////     
           CALL "DB_Read" USING
            "INVALID KEY" JNIF_PNAME1 BY REFERENCE JNIF-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO UPD3-RTN-EXIT
           END-IF
           MOVE   1   TO   JNIF1-10.
      *           REWRITE   JNIF1-R   INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            JNIF_PNAME1 JNIF_LNAME JNIF1-R RETURNING RET.
           IF  RET = 1
               MOVE  "JNIF"       TO    ERR-F
               MOVE  "R"          TO    ERR-M
               MOVE   JNIF1-KEY   TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP   RUN
           END-IF.
       UPD3-RTN-EXIT.
           EXIT.
      *====<  U P D 4  -  R T N  >====*
       UPD4-RTN.
      *           READ   JNIF     INVALID  KEY
      *//////////////////////     
           CALL "DB_Read" USING
            "INVALID KEY" JNIF_PNAME1 BY REFERENCE JNIF-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO UPD4-RTN-EXIT
           END-IF
           MOVE   1   TO   JNIF2-04.
      *           REWRITE  JNIF2-R  INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            JNIF_PNAME1 JNIF_LNAME JNIF2-R RETURNING RET.
           IF  RET = 1
               MOVE  "JNIF"       TO    ERR-F
               MOVE  "R"          TO    ERR-M
               MOVE   JNIF2-KEY   TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP   RUN
           END-IF.
       UPD4-RTN-EXIT.
           EXIT.
      *====<  U P D 5  -  R T N  >====*
       UPD5-RTN.
           MOVE   JOLSF13-02(NN)  TO  OKJF-01.
      *           READ   OKJF     INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" OKJF_PNAME1 BY REFERENCE OKJF-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO UPD5-RTN-EXIT
           END-IF
           MOVE   1   TO   OKJF-08.
      *           REWRITE  OKJF-R  INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            OKJF_PNAME1 OKJF_LNAME OKJF-R RETURNING RET.
           IF  RET = 1
               MOVE  "OKJF"       TO    ERR-F
               MOVE  "R"          TO    ERR-M
               MOVE   OKJF-KEY    TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP   RUN
           END-IF
           COMPUTE  NN-W  =  NN + 1.
           IF  NN   NOT =  4
               IF  JOLSF13-KEY(NN-W)  =  SPACE
                   MOVE  4  TO  NN
               END-IF
           END-IF.
       UPD5-RTN-EXIT.
           EXIT.
      *====<  U P D 6  -  R T N  >====*
       UPD6-RTN.
      *           READ   TDNWF    INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TDNWF_PNAME1 BY REFERENCE TDNW-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO UPD6-RTN-EXIT
           END-IF
           MOVE   9   TO   TDNW1-PC.
      *           REWRITE  TDNW-R1 INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            TDNWF_PNAME1 TDNWF_LNAME TDNW-R1 RETURNING RET.
           IF  RET = 1
               MOVE  "TDNWF"      TO    ERR-F
               MOVE  "R"          TO    ERR-M
               MOVE   TDNW1-KEY   TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP   RUN
           END-IF.
       UPD6-RTN-EXIT.
           EXIT.
      *====<  U P D 7  -  R T N  >====*
       UPD7-RTN.
      *           READ   TDNNF    INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TDNNF_PNAME1 BY REFERENCE TDNN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO UPD7-RTN-EXIT
           END-IF
           MOVE   9   TO   TDNN1-PC.
      *           REWRITE  TDNN-R1 INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            TDNNF_PNAME1 TDNNF_LNAME TDNN-R1 RETURNING RET.
           IF  RET = 1
               MOVE  "TDNNF"      TO    ERR-F
               MOVE  "R"          TO    ERR-M
               MOVE   TDNN1-KEY   TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP   RUN
           END-IF.
       UPD7-RTN-EXIT.
           EXIT.
      *====<  U P D 8  -  R T N  >====*
       UPD8-RTN.
      *           READ   TDIF     INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TDIF_PNAME1 BY REFERENCE TDI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO UPD8-RTN-EXIT
           END-IF
           MOVE   9   TO   TDI-UPC.
      *           REWRITE  TDI-R   INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            TDIF_PNAME1 TDIF_LNAME TDI-R RETURNING RET.
           IF  RET = 1
               MOVE  "TDIF"       TO    ERR-F
               MOVE  "R"          TO    ERR-M
               MOVE   TDI-KEY     TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP   RUN
           END-IF.
       UPD8-RTN-EXIT.
           EXIT.
      *====<  U P D 9  -  R T N  >====*
       UPD9-RTN.
      *           READ   TDNAF    INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TDNAF_PNAME1 BY REFERENCE TDNA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO UPD9-RTN-EXIT
           END-IF
           MOVE   9   TO   TDNA-PC.
      *           REWRITE  TDNA-R  INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            TDNAF_PNAME1 TDNAF_LNAME TDNA-R RETURNING RET.
           IF  RET = 1
               MOVE  "TDNAF"      TO    ERR-F
               MOVE  "R"          TO    ERR-M
               MOVE   TDNA-KEY    TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP   RUN
           END-IF.
       UPD9-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    E N D R  -  R T N                                         *
      *--------------------------------------------------------------*
       ENDR-RTN.
           CALL "DB_F_Open" USING
            "INPUT" JOLSF_PNAME1 " " BY REFERENCE JOLSF_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" JOLSR_PNAME1 " " BY REFERENCE JOLSR_IDLST "0".
       ENDR-010.
      *           READ  JOLSF       AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JOLSF_PNAME1 BY REFERENCE JOLSF-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO ENDR-100
           END-IF
           INITIALIZE JOLSR1-REC.
           MOVE  JOLSF1-REC  TO  JOLSR1-REC.
           MOVE W-NO TO JOLSR1-NO.
      *           WRITE  JOLSR1-REC.
      *//////////////
           CALL "DB_Insert" USING
            JOLSR_PNAME1 JOLSR_LNAME JOLSR1-REC RETURNING RET.
           IF  ERR-STAT  = "22" OR "24"
               MOVE  "JOLSF"      TO    ERR-F
               MOVE  "W"          TO    ERR-M
               MOVE   JOLSR1-01   TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP  RUN
           END-IF
           GO TO ENDR-010.
       ENDR-100.
           CALL "DB_F_Close" USING
            BY REFERENCE JOLSF_IDLST JOLSF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JOLSR_IDLST JOLSR_PNAME1.
           IF  JS-SIGN        =   0
               CALL "DB_F_Open" USING
                "OUTPUT" JOLSF_PNAME1 " " BY REFERENCE JOLSF_IDLST "0"
               CALL "DB_F_Close" USING
                BY REFERENCE JOLSF_IDLST JOLSF_PNAME1
           END-IF.
      *
       ENDR-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    O P E N  -  R T N                                         *
      *--------------------------------------------------------------*
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "INPUT" JOLSF_PNAME1 " " BY REFERENCE JOLSF_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "I-O" JNIF_PNAME1 "SHARED" BY REFERENCE JNIF_IDLST "1"
            "JNIF1-KEY" BY REFERENCE JNIF1-KEY.
           CALL "DB_F_Open" USING
            "I-O" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
           CALL "DB_F_Open" USING
            "I-O" TDNWF_PNAME1 "SHARED" BY REFERENCE TDNWF_IDLST "1"
            "TDNW1-KEY" BY REFERENCE TDNW1-KEY.
           CALL "DB_F_Open" USING
            "I-O" TDNNF_PNAME1 "SHARED" BY REFERENCE TDNNF_IDLST "1"
            "TDNN1-KEY" BY REFERENCE TDNN1-KEY.
           CALL "DB_F_Open" USING
            "I-O" TDIF_PNAME1 "SHARED" BY REFERENCE TDIF_IDLST "1"
            "TDI-KEY" BY REFERENCE TDI-KEY.
           CALL "DB_F_Open" USING
            "I-O" TDNAF_PNAME1 "SHARED" BY REFERENCE TDNAF_IDLST "1"
            "TDNA-KEY" BY REFERENCE TDNA-KEY.
       OPEN-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    C L S E  -  R T N                                         *
      *--------------------------------------------------------------*
       CLSE-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JOLSF_IDLST JOLSF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JNIF_IDLST JNIF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNWF_IDLST TDNWF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNNF_IDLST TDNNF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TDIF_IDLST TDIF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNAF_IDLST TDNAF_PNAME1.
       CLSE-RTN-EXIT.
           EXIT.
      *
           COPY    LPERR.

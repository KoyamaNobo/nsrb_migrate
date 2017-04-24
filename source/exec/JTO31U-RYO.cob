       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       JTO31U.
      ****************************************************************
      *    Ｏ／Ｌ受信データ（更新）更新                              *
      *                              90/ 5/ 8           IKUMI.N      *
      ****************************************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       DATA              DIVISION.
       WORKING-STORAGE   SECTION.
      *----< ﾜｰｸ ｴﾘｱ >----*
       77  ERR-STAT      PIC X(02).
       77  END-SW        PIC 9(01)   VALUE  0.
       77  WRI-SW        PIC 9(01).
       01  WK-NO         PIC 9(06)   VALUE  0.
       01  ERR-SW        PIC 9(01)   VALUE  0.
      *-------------------*
           COPY    LWMSG.
      *
           COPY    LJOLJF-RYO.
           COPY    L-JSJD.
           COPY    L-JSTR.
           COPY    LNJZAI.
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
           02  FILLER  PIC N(20)  VALUE
                 "＊＊　Ｏ／Ｌ受信データ（実績）更新　＊＊".
       01  EMSG-AREA.
           02  EMSG-01.
               03  FILLER    PIC  N(14)
                              VALUE  "出荷実績二重更新　指図№＝".
               03  EMSG-011  PIC  9(06).
               03  FILLER    PIC  N(11)
                              VALUE  "リセットにて続行します".
      *-------------------*
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
            "DSP-HYOJI" " " "0" "0" "40" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-HYOJI" "N" "1" "20" "40" " " "DSP-HYOJI"
            RETURNING RESU.
      *EMSG-AREA
       CALL "SD_Init" USING 
            "EMSG-AREA" " " "0" "0" "56" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-01" " " "24" "0" "56" " " "EMSG-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01EMSG-01" "N" "24" "1" "28" " " "EMSG-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-011" "9" "24" "27" "6" "01EMSG-01" " " RETURNING RESU.
       CALL "SD_From" USING 
            "EMSG-011" BY REFERENCE JSJD-03 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "03EMSG-01" "N" "24" "35" "22" "EMSG-011" " " RETURNING RESU.
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
           PERFORM  READ-RTN  THRU  READ-RTN-EXIT.
           PERFORM  PROC-RTN  THRU  PROC-RTN-EXIT
               UNTIL  END-SW  =  9.
           PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT.
           PERFORM  ENDR-RTN  THRU  ENDR-RTN-EXIT.
       MAINLINE-END.
           CALL "DB_Close".
           STOP RUN.
      *--------------------------------------------------------------*
      *    P R O C  -  R T N                                         *
      *--------------------------------------------------------------*
       PROC-RTN.
           IF  JOLJF11-01  NOT =  11
               GO  TO  PROC-090
           END-IF
           IF  ERR-SW          =  1    AND    JOLJF11-02  =  WK-NO
               GO  TO  PROC-090
           END-IF
           IF  JOLJF11-02  NOT =  WK-NO
               PERFORM  CHK-RTN  THRU  CHK-EX
               IF  ERR-SW    NOT =  0
                   GO  TO  PROC-080
               END-IF
           END-IF
           MOVE  JOLJF11-08    TO  JSJD-01.
           MOVE  JOLJF11-07    TO  JSJD-02.
           MOVE  JOLJF11-02    TO  JSJD-03.
           MOVE  JOLJF11-03    TO  JSJD-04.
      *           READ  JSJD   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JSJD_PNAME1 BY REFERENCE JSJD-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE         TO  JSJD-REC
               INITIALIZE              JSJD-REC
               GO  TO  PROC-010
           END-IF
           GO    TO    PROC-090.
       PROC-010.
           MOVE  JOLJF11-08    TO  JSJD-01.
           MOVE  JOLJF11-07    TO  JSJD-02.
           MOVE  JOLJF11-02    TO  JSJD-03.
           MOVE  JOLJF11-03    TO  JSJD-04.
           MOVE  JOLJF11-04    TO  JSJD-05.
           MOVE  JOLJF11-05    TO  JSJD-06.
           MOVE  JOLJF11-06    TO  JSJD-07.
           MOVE  JOLJF11-09    TO  JSJD-08.
           MOVE  JOLJF11-10    TO  JSJD-09.
           MOVE  JOLJF11-11    TO  JSJD-10.
           MOVE  JOLJF11-12    TO  JSJD-11.
           MOVE  JOLJF11-13    TO  JSJD-12.
           MOVE  JOLJF11-14    TO  JSJD-13.
           MOVE  JOLJF11-15    TO  JSJD-14.
           MOVE  JOLJF11-15A   TO  JSJD-14A.
           MOVE  JOLJF11-15B   TO  JSJD-14B.
           MOVE  JOLJF11-15C   TO  JSJD-14C.
           MOVE  JOLJF11-15D   TO  JSJD-14D.
           MOVE  JOLJF11-16    TO  JSJD-15.
           MOVE  JOLJF11-16A   TO  JSJD-15A.
           MOVE  JOLJF11-20    TO  JSJD-20.
           MOVE  JOLJF11-19    TO  JSJD-19.
           MOVE  0             TO  JSJD-158.
           IF    (JOLJF11-1311(01) = ZERO)
             AND (JOLJF11-1311(02) = ZERO)
             AND (JOLJF11-1311(03) = ZERO)
             AND (JOLJF11-1311(04) = ZERO)
             AND (JOLJF11-1311(05) = ZERO)
             AND (JOLJF11-1311(06) = ZERO)
             AND (JOLJF11-1311(07) = ZERO)
             AND (JOLJF11-1311(08) = ZERO)
             AND (JOLJF11-1311(09) = ZERO)
             AND (JOLJF11-1311(10) = ZERO)
               MOVE  1      TO  JSJD-158
           END-IF
           MOVE  JOLJF11-17    TO  JSJD-16.
           MOVE  9             TO  JSJD-17.
      *           WRITE  JSJD-REC  INVALID
      *//////////////
           CALL "DB_Insert" USING
            JSJD_PNAME1 JSJD_LNAME JSJD-REC RETURNING RET.
           IF  RET = 1
               MOVE   "JSJD"    TO   ERR-F
               MOVE   "W"       TO   ERR-M
               MOVE   JSJD-KEY  TO   ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
      ****
           MOVE  JOLJF11-KEYW  TO  JSTR-KEY.
      *           READ  JSTR   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JSTR_PNAME1 BY REFERENCE JSTR-R " " RETURNING RET.
           IF  RET = 1
               MOVE  SPACE         TO  JSTR-R
               INITIALIZE              JSTR-R
               MOVE  LOW-VALUE     TO  JSTR-KEY
           END-IF
      *
           MOVE  JOLJF11-04    TO  JSTR-03.
           MOVE  JOLJF11-05    TO  JSTR-04.
           MOVE  JOLJF11-06    TO  JSTR-05.
           MOVE  JOLJF11-07    TO  JSTR-06.
           MOVE  JOLJF11-08    TO  JSTR-07.
           MOVE  JOLJF11-09    TO  JSTR-08.
           MOVE  JOLJF11-10    TO  JSTR-09.
           MOVE  JOLJF11-11    TO  JSTR-10.
           MOVE  JOLJF11-12    TO  JSTR-11.
           MOVE  JOLJF11-13    TO  JSTR-12.
           MOVE  JOLJF11-14    TO  JSTR-13.
           MOVE  JOLJF11-15    TO  JSTR-14.
           MOVE  JOLJF11-15A   TO  JSTR-14A.
           MOVE  JOLJF11-15B   TO  JSTR-14B.
           MOVE  JOLJF11-15C   TO  JSTR-14C.
           MOVE  JOLJF11-15D   TO  JSTR-14D.
           MOVE  JOLJF11-16    TO  JSTR-15.
           MOVE  JOLJF11-16A   TO  JSTR-15A.
           MOVE  JOLJF11-20    TO  JSTR-20.
           MOVE  JOLJF11-19    TO  JSTR-19.
           MOVE  JOLJF11-168   TO  JSTR-158.
           MOVE  JOLJF11-17    TO  JSTR-16.
           MOVE  9             TO  JSTR-17.
           IF  JSTR-KEY     NOT =  LOW-VALUE
               GO  TO  PROC-070
           END-IF
           MOVE  JOLJF11-KEYW      TO   JSTR-KEY.
      *           WRITE  JSTR-R    INVALID
      *//////////////
           CALL "DB_Insert" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE   "JSTR"    TO   ERR-F
               MOVE   "W"       TO   ERR-M
               MOVE   JSTR-KEY  TO   ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           GO  TO  PROC-075.
       PROC-070.
      *           REWRITE  JSTR-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE   "JSTR"    TO   ERR-F
               MOVE   "R"       TO   ERR-M
               MOVE   JSTR-KEY  TO   ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
      *****
       PROC-075.
           IF  (JSTR-111(01)  =  JSTR-121(01))  AND
               (JSTR-111(02)  =  JSTR-121(02))  AND
               (JSTR-111(03)  =  JSTR-121(03))  AND
               (JSTR-111(04)  =  JSTR-121(04))  AND
               (JSTR-111(05)  =  JSTR-121(05))  AND
               (JSTR-111(06)  =  JSTR-121(06))  AND
               (JSTR-111(07)  =  JSTR-121(07))  AND
               (JSTR-111(08)  =  JSTR-121(08))  AND
               (JSTR-111(09)  =  JSTR-121(09))  AND
               (JSTR-111(10)  =  JSTR-121(10))
               GO  TO  PROC-080
           END-IF.
      *
       PROC-076.
           MOVE  JSTR-07       TO  NJZAI-01.
           MOVE  JSTR-09       TO  NJZAI-02.
           MOVE  JSTR-10       TO  NJZAI-03.
      *           READ  NJZAI  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE         TO  NJZAI-R
               INITIALIZE              NJZAI-R
               MOVE  LOW-VALUE     TO  NJZAI-KEY
           END-IF
      *
           PERFORM  NJS-RTN   THRU  NJS-EX.
      *
           IF  NJZAI-KEY    NOT =  LOW-VALUE
               GO  TO  PROC-077
           END-IF
           MOVE  JSTR-07       TO  NJZAI-01.
           MOVE  JSTR-09       TO  NJZAI-02.
           MOVE  JSTR-10       TO  NJZAI-03.
           PERFORM  NJW-RTN   THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  PROC-076
           END-IF
           GO  TO  PROC-078.
       PROC-077.
           PERFORM  NJR-RTN   THRU  NJR-EX.
       PROC-078.
           MOVE  9             TO  NJZAI-01.
           MOVE  JSTR-09       TO  NJZAI-02.
           MOVE  JSTR-10       TO  NJZAI-03.
      *           READ  NJZAI  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE         TO  NJZAI-R
               INITIALIZE              NJZAI-R
               MOVE  LOW-VALUE     TO  NJZAI-KEY
           END-IF
      *
           PERFORM  NJS-RTN   THRU  NJS-EX.
      *
           IF  NJZAI-KEY    NOT =  LOW-VALUE
               GO  TO  PROC-079
           END-IF
           MOVE  9             TO  NJZAI-01.
           MOVE  JSTR-09       TO  NJZAI-02.
           MOVE  JSTR-10       TO  NJZAI-03.
           PERFORM  NJW-RTN   THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  PROC-078
           END-IF
           GO  TO  PROC-080.
       PROC-079.
           PERFORM  NJR-RTN   THRU  NJR-EX.
      *****
       PROC-080.
           MOVE  JOLJF11-02   TO  WK-NO.
       PROC-090.
           PERFORM  READ-RTN  THRU  READ-RTN-EXIT.
       PROC-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    I N I T  -  R T N                                         *
      *--------------------------------------------------------------*
       INIT-RTN.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-HYOJI" DSP-HYOJI "p" RETURNING RESU.
      ** ﾌｧｲﾙ OPEN **
           CALL "DB_F_Open" USING
            "INPUT" JOLJF_PNAME1 " " BY REFERENCE JOLJF_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" JSJD_PNAME1 "SHARED" BY REFERENCE JSJD_IDLST "2"
            "JSJD-KEY" BY REFERENCE JSJD-KEY "JSJD-KEY2" BY REFERENCE
            JSJD-KEY2.
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
       INIT-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    R E A D  -  R T N                                         *
      *--------------------------------------------------------------*
       READ-RTN.
      *           READ    JOLJF     AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JOLJF_PNAME1 BY REFERENCE JOLJF11-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  9  TO  END-SW
           END-IF.
       READ-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    E N D R  -  R T N                                         *
      *--------------------------------------------------------------*
       ENDR-RTN.
           CALL "DB_F_Open" USING
            "OUTPUT" JOLJF_PNAME1 " " BY REFERENCE JOLJF_IDLST "0".
           CALL "DB_F_Close" USING
            BY REFERENCE JOLJF_IDLST JOLJF_PNAME1.
       ENDR-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    C L S E  -  R T N                                         *
      *--------------------------------------------------------------*
       CLSE-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JOLJF_IDLST JOLJF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JSJD_IDLST JSJD_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
       CLSE-RTN-EXIT.
           EXIT.
      *----------------------------*
      *    ＣＨＫ－ＲＴＮ          *
      *----------------------------*
       CHK-RTN.
           MOVE  0             TO  ERR-SW.
           MOVE  JOLJF11-02    TO  JSJD-03.
           MOVE  0             TO  JSJD-04.
      *           START  JSJD    KEY   NOT <  JSJD-KEY2 INVALID
      *///////////////
           CALL "DB_Start" USING
            JSJD_PNAME1 "JSJD-KEY2" " NOT < " JSJD-KEY2
            RETURNING RET.
           IF  RET = 1
               GO  TO  CHK-EX
           END-IF.
       CHK-010.
      *           READ  JSJD    NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSJD_PNAME1 BY REFERENCE JSJD-REC " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  CHK-EX
           END-IF
           IF  JOLJF11-02   NOT =  JSJD-03
               GO  TO  CHK-EX
           END-IF
           CALL "SD_Output" USING "EMSG-01" EMSG-01 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           MOVE  1             TO  ERR-SW.
       CHK-EX.
           EXIT.
      *----------------------------*
      *    NJZAI ｽｳﾘｮｳ ｹｲｻﾝ        *
      *----------------------------*
       NJS-RTN.
           COMPUTE  NJZAI-0911(01)  =  NJZAI-0911(01)  -  JSTR-1111(01)
                                                       +  JSTR-1211(01).
           COMPUTE  NJZAI-0911(02)  =  NJZAI-0911(02)  -  JSTR-1111(02)
                                                       +  JSTR-1211(02).
           COMPUTE  NJZAI-0911(03)  =  NJZAI-0911(03)  -  JSTR-1111(03)
                                                       +  JSTR-1211(03).
           COMPUTE  NJZAI-0911(04)  =  NJZAI-0911(04)  -  JSTR-1111(04)
                                                       +  JSTR-1211(04).
           COMPUTE  NJZAI-0911(05)  =  NJZAI-0911(05)  -  JSTR-1111(05)
                                                       +  JSTR-1211(05).
           COMPUTE  NJZAI-0911(06)  =  NJZAI-0911(06)  -  JSTR-1111(06)
                                                       +  JSTR-1211(06).
           COMPUTE  NJZAI-0911(07)  =  NJZAI-0911(07)  -  JSTR-1111(07)
                                                       +  JSTR-1211(07).
           COMPUTE  NJZAI-0911(08)  =  NJZAI-0911(08)  -  JSTR-1111(08)
                                                       +  JSTR-1211(08).
           COMPUTE  NJZAI-0911(09)  =  NJZAI-0911(09)  -  JSTR-1111(09)
                                                       +  JSTR-1211(09).
           COMPUTE  NJZAI-0911(10)  =  NJZAI-0911(10)  -  JSTR-1111(10)
                                                       +  JSTR-1211(10).
       NJS-EX.
           EXIT.
      *----------------------------*
      *    NJZAI WRITE RTN         *
      *----------------------------*
       NJW-RTN.
           MOVE     0            TO   WRI-SW.
      *           WRITE    NJZAI-R      INVALID
      *//////////////
           CALL "DB_Insert" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               GO  TO  NJW-010
           END-IF
           GO  TO  NJW-EX.
       NJW-010.
           MOVE    "W"           TO   ERR-M.
           MOVE    "NJZAI"       TO   ERR-F.
           MOVE     NJZAI-KEY    TO   ERR-K.
           IF  ERR-STAT          =  "24"
               MOVE     ERR-STAT    TO   ERR-FLG
               CALL "SD_Output" USING
                "ERR-DIS" ERR-DIS "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE NJZAI_IDLST NJZAI_PNAME1
               CALL "SD_Output" USING
                " " "ｴﾘｱ ｶｸﾁｮｳｺﾞ､ｻｲｶｲｷｰ ｦ ｵｽ!" "STOP" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               CALL "DB_F_Open" USING
                "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
                "NJZAI-KEY" BY REFERENCE NJZAI-KEY
               MOVE     1           TO   WRI-SW
               GO  TO  NJW-EX
           END-IF
           IF  ERR-STAT     NOT  =  "00"
               MOVE     2           TO   WRI-SW
               PERFORM  ERR-RTN   THRU  ERR-EX
           END-IF.
       NJW-EX.
           EXIT.
      *------------------------------*
      *    NJZAI REWRITE RTN         *
      *------------------------------*
       NJR-RTN.
      *           REWRITE  NJZAI-R      INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"           TO   ERR-M
               MOVE    "NJZAI"       TO   ERR-F
               MOVE     NJZAI-KEY    TO   ERR-K
               PERFORM  ERR-RTN   THRU  ERR-EX
           END-IF.
       NJR-EX.
           EXIT.
      *--------------------------------------------------------------*
           COPY    LPERR.

       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       JK020U.
      ******************************************************************
      *    éÛêMÉfÅ[É^çXêV                                              *
      *    ÇQÇOÇOÇTÅ^ÇOÇXÅ^ÇOÇP                                        *
      *    T.ISHISHITA                                                 *
      *    JS-SIGN  :  0=ã ìá , 1=ëÅìá                                 *
      ******************************************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       DATA              DIVISION.
       WORKING-STORAGE   SECTION.
      *----< ‹∞∏ ¥ÿ± >----*
       01  END-STS-WK.
         02  PF9         PIC X(02)   VALUE "P9".
         02  HTAB        PIC X(02)   VALUE "01".
       77  KAKU-W        PIC X(01).
       77  NN            PIC 9(02).
       77  NN-W          PIC 9(02).
       77  ERR-STAT      PIC X(02).
       77  END-SW        PIC 9(01)   VALUE  0.
       01  NC-WK.
         02  NC-01       PIC N(08)   VALUE    "éÛêMÉfÅ[É^Å@ÉiÉV".
         02  NC-02       PIC N(03)   VALUE    "éÛêMíÜ".
       01  ERR-WK        PIC N(20).
       01  JS-SIGN       PIC 9(01).
       01  W-SKM         PIC N(06).
       01  W-DATA.
           03  W-TIM               PIC  9(08).
           03  W-TIMD  REDEFINES W-TIM.
             04  W-TIME            PIC  9(04).
             04  F                 PIC  9(04).
      *-------------------*
           COPY    LWMSG.
      *
           COPY    L-JOSF.
           COPY    L-JSTR.
           COPY    L-JNIF.
           COPY    LOKJF.
           COPY    LITDNW.
           COPY    LITDNN.
           COPY    LITDIF.
           COPY    LITDNA.
      *FD  CHKF
       01  CHKF_JK020U.
           02  CHKF_PNAME1  PIC  X(006) VALUE "T-CHKF".
           02  F            PIC  X(001).
           02  CHKF_LNAME   PIC  X(011) VALUE "CHKF_JK020U".
           02  F            PIC  X(001).
           02  CHKF_KEY1    PIC  X(100) VALUE SPACE.
           02  CHKF_SORT    PIC  X(100) VALUE SPACE.
           02  CHKF_IDLST   PIC  X(100) VALUE SPACE.
           02  CHKF_RES     USAGE  POINTER.
       01  CHKF-R.
           02  CHKF-DATE    PIC 9(06).
           02  CHKF-TIME    PIC 9(04).
           02  CHKF-SIGN    PIC 9(01).
           02  CHKF-SEN     PIC 9(01).
           02  CHKF-DMM     PIC 9(01).
           02  CHKF-PRG     PIC X(03).
       77  F                PIC X(01).
      *
       77  END-STS          PIC  X(002).
       77  RESU             PIC  9(001).
       77  RET              PIC  9(001) VALUE ZERO.
       77  USER_ID          PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE  PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
         02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  DSP-HYOJI.
         02  FILLER  PIC N(15)  VALUE
                   "ÅñÅñÅñÅ@éÛêMÉfÅ[É^çXêVÅ@ÅñÅñÅñ".
         02  FILLER.
           03  FILLER  PIC N(02)  VALUE   "ämîF".
           03  FILLER  PIC X(15)  VALUE "OK=ÿ¿∞› , NO=F9".
           03  FILLER  PIC X(01)  VALUE "(".
           03  FILLER  PIC X(01)  VALUE ")".
       01  ACP-KAKU.
         02  01ACP-KAKU  PIC X(01).
       01  DSP-SKM.
         02  FILLER      PIC N(06).
       01  DSP-ERRMSG.
         02  FILLER  PIC N(20).
         02  FILLER  PIC X(05)  VALUE X"1B4202".
         02  FILLER  PIC X(40)  VALUE " ".
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
           "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-CLR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *DSP-HYOJI
       CALL "SD_Init" USING
            "DSP-HYOJI" " " "0" "0" "51" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-HYOJI" "N" "1" "20" "30" " " "DSP-HYOJI"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-HYOJI" " " "24" "0" "21" "01DSP-HYOJI" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0102DSP-HYOJI" "N" "24" "29" "4" " " "02DSP-HYOJI"
            RETURNING RESU.
       CALL "SD_Init" USING
            "0202DSP-HYOJI" "X" "24" "35" "15" "0102DSP-HYOJI" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0302DSP-HYOJI" "X" "24" "52" "1" "0202DSP-HYOJI" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0402DSP-HYOJI" "X" "24" "54" "1" "0302DSP-HYOJI" " "
            RETURNING RESU.
      *ACP-KAKU
       CALL "SD_Init" USING
            "ACP-KAKU" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01ACP-KAKU" "X" "24" "53" "1" " " "ACP-KAKU" RETURNING RESU.
       CALL "SD_Into" USING
            "01ACP-KAKU" BY REFERENCE KAKU-W "1" "0" RETURNING RESU.
      *DSP-SKM
       CALL "SD_Init" USING
            "DSP-SKM" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-SKM" "N" "12" "30" "12" " " "DSP-SKM" RETURNING RESU.
       CALL "SD_Into" USING
            "01DSP-SKM" BY REFERENCE W-SKM "12" "0" RETURNING RESU.
      *DSP-ERRMSG
       CALL "SD_Init" USING
            "DSP-ERRMSG" " " "24" "0" "85" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-ERRMSG" "N" "24" "1" "40" " " "DSP-ERRMSG"
            RETURNING RESU.
       CALL "SD_From" USING
            "01DSP-ERRMSG" BY REFERENCE ERR-WK "40" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-ERRMSG" "X" "24" "1" "5" "01DSP-ERRMSG" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-ERRMSG" "X" "24" "1" "40" "02DSP-ERRMSG" " "
            RETURNING RESU.
      *
           COPY LSERR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
      *--------------------------------------------------------------*
      *    “ ≤ ›  Ÿ ∞ ¡ ›                                            *
      *--------------------------------------------------------------*
       MAINLINE-ROUTINE.
           PERFORM  INIT-RTN  THRU  INIT-RTN-EXIT.
           PERFORM  OPEN-RTN  THRU  OPEN-RTN-EXIT.
           PERFORM  READ-RTN  THRU  READ-RTN-EXIT.
           PERFORM  PROC-RTN  THRU  PROC-RTN-EXIT
               UNTIL  END-SW  =  9.
      *
           PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT.
           PERFORM  ENDR-RTN  THRU  ENDR-RTN-EXIT.
      *
       MAINLINE-END.
           STOP RUN.
      *--------------------------------------------------------------*
      *    I N I T  -  R T N                                         *
      *--------------------------------------------------------------*
       INIT-RTN.
           ACCEPT  JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN      >  1
               CALL "DB_Close"
               STOP  RUN
           END-IF
      *
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-HYOJI" DSP-HYOJI "p" RETURNING RESU.
           IF  JS-SIGN      =  0
               MOVE    "Åyã Å@ìáÅzÅ@"  TO  W-SKM
           ELSE
               MOVE    "ÅyëÅÅ@ìáÅzÅ@"  TO  W-SKM
           END-IF
           CALL "SD_Output" USING "DSP-SKM" DSP-SKM "p" RETURNING RESU.
       INIT-010.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-KAKU "01ACP-KAKU"
            "X" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS      =  PF9
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  END-STS  NOT =  HTAB
               GO TO INIT-010
           END-IF
           CALL "DB_F_Open" USING
            "EXTEND" CHKF_PNAME1 "SHARED" BY REFERENCE CHKF_IDLST "0".
           ACCEPT  W-TIM      FROM  TIME.
           INITIALIZE  CHKF-R.
           ACCEPT  CHKF-DATE  FROM  DATE.
           MOVE    W-TIME     TO    CHKF-TIME.
           MOVE    JS-SIGN    TO    CHKF-SIGN.
           MOVE    8          TO    CHKF-SEN.
           MOVE    "20U"      TO    CHKF-PRG.
      *           WRITE   CHKF-R.
      *//////////////
           CALL "DB_Insert" USING
            CHKF_PNAME1 CHKF_LNAME CHKF-R RETURNING RET.
           CALL "DB_F_Close" USING BY REFERENCE CHKF_IDLST CHKF_PNAME1.
       INIT-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    P R O C  -  R T N                                         *
      *--------------------------------------------------------------*
       PROC-RTN.
       PROC-010.
           IF  JOLSF11-01  =  11
      *----< º≠Ø∂ªºΩﬁT ∫≥º›ºÆÿ >----*
               MOVE  JOLSF11-02  TO  JSTR-01
               MOVE  JOLSF11-03  TO  JSTR-02
               PERFORM  UPD5-RTN  THRU  UPD5-RTN-EXIT
           END-IF
           IF  JOLSF11-01  =  12
      *----< ∆Ã¿ﬁT ∫≥º›ºÆÿ >----*
               MOVE  JOLSF121-01  TO  JNIF1-01
               MOVE  JOLSF121-02  TO  JNIF1-02
               PERFORM  UPD6-RTN  THRU  UPD6-RTN-EXIT
           END-IF
           IF  JOLSF13-01  =  13
      *----< µ∏ÿºﬁÆ≥F ∫≥º›ºÆÿ >----*
               PERFORM  UPD7-RTN  THRU  UPD7-RTN-EXIT
                   VARYING  NN  FROM  1  BY  1  UNTIL  NN  >  4
           END-IF
           IF  JOLSF14-01  =  14
      *----< ƒ≥≤¬√ﬁ›ÀﬂÆ≥(‹∞∏œ›)>----*
               MOVE  JOLSF141-KEY TO  TDNW1-KEY
               PERFORM  UPD8-RTN  THRU  UPD8-RTN-EXIT
           END-IF
           IF  JOLSF15-01  =  15
      *----< ƒ≥≤¬√ﬁ›ÀﬂÆ≥(≈Ã∫)>----*
               MOVE  JOLSF151-KEY TO  TDNN1-KEY
               PERFORM  UPD9-RTN  THRU  UPD9-RTN-EXIT
           END-IF
           IF  JOLSF16-01  =  16
      *----< ƒ≥≤¬√ﬁ›ÀﬂÆ≥(ƒ◊Ω∫)>----*
               MOVE  JOLSF16-KEY  TO  TDI-KEY
               PERFORM  UPD10-RTN  THRU  UPD10-RTN-EXIT
           END-IF
           IF  JOLSF16-01  =  17
      *----< ƒ≥≤¬√ﬁ›ÀﬂÆ≥(±∂¡¨›Œ›Œﬂ)>----*
               MOVE  JOLSF17-KEY  TO  TDNA-KEY
               PERFORM  UPD11-RTN  THRU  UPD11-RTN-EXIT
           END-IF
      *
           PERFORM  READ-RTN  THRU  READ-RTN-EXIT.
       PROC-RTN-EXIT.
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
      *    Ã ß ≤ Ÿ  ∫ ≥ º ›  º Æ ÿ                                   *
      *--------------------------------------------------------------*
      *====<  U P D 5  -  R T N  >====*
       UPD5-RTN.
      *           READ   JSTR     INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               PERFORM  WRT5-RTN  THRU  WRT5-RTN-EXIT
           END-IF.
       UPD5-RTN-EXIT.
           EXIT.
      *====<  U P D 6  -  R T N  >====*
       UPD6-RTN.
      *           READ   JNIF     INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JNIF_PNAME1 BY REFERENCE JNIF-R " "
            RETURNING RET.
           IF  RET = 1
               PERFORM  WRT6-RTN  THRU  WRT6-RTN-EXIT
           END-IF
           IF  JOLSF121-21  =  ZERO  OR  SPACE
               GO TO UPD6-RTN-EXIT
           END-IF
           MOVE  JOLSF121-21  TO  JNIF1-01.
           MOVE  JOLSF121-22  TO  JNIF1-02.
      *           READ   JNIF     INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JNIF_PNAME1 BY REFERENCE JNIF-R " "
            RETURNING RET.
           IF  RET = 1
               PERFORM  WRT6-RTN2 THRU  WRT6-RTN2-EXIT
           END-IF.
       UPD6-RTN-EXIT.
           EXIT.
      *====<  U P D 7  -  R T N  >====*
       UPD7-RTN.
           IF  JOLSF13-02(NN)  =  ZERO  OR  SPACE
               GO TO UPD7-RTN-EXIT
           END-IF
           MOVE   JOLSF13-02(NN)  TO  OKJF-01.
      *           READ   OKJF     INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" OKJF_PNAME1 BY REFERENCE OKJF-R " "
            RETURNING RET.
           IF  RET = 1
               PERFORM  WRT7-RTN  THRU  WRT7-RTN-EXIT
           END-IF.
       UPD7-RTN-EXIT.
           EXIT.
      *====<  U P D 8  -  R T N  >====*
       UPD8-RTN.
      *           READ   TDNWF    INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TDNWF_PNAME1 BY REFERENCE TDNW-R " "
            RETURNING RET.
           IF  RET = 1
               PERFORM  WRT8-RTN  THRU  WRT8-RTN-EXIT
           END-IF.
       UPD8-RTN-EXIT.
           EXIT.
      *====<  U P D 9  -  R T N  >====*
       UPD9-RTN.
      *           READ   TDNNF    INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TDNNF_PNAME1 BY REFERENCE TDNN-R " "
            RETURNING RET.
           IF  RET = 1
               PERFORM  WRT9-RTN  THRU  WRT9-RTN-EXIT
           END-IF.
       UPD9-RTN-EXIT.
           EXIT.
      *====<  U P D 10 -  R T N  >====*
       UPD10-RTN.
      *           READ   TDIF     INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TDIF_PNAME1 BY REFERENCE TDI-R " "
            RETURNING RET.
           IF  RET = 1
               PERFORM  WRT10-RTN THRU  WRT10-RTN-EXIT
           END-IF.
       UPD10-RTN-EXIT.
           EXIT.
      *====<  U P D 11 -  R T N  >====*
       UPD11-RTN.
      *           READ   TDNAF    INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TDNAF_PNAME1 BY REFERENCE TDNA-R " "
            RETURNING RET.
           IF  RET = 1
               PERFORM  WRT11-RTN THRU  WRT11-RTN-EXIT
           END-IF.
       UPD11-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       WRT5-RTN.
           MOVE  SPACE       TO  JSTR-R.
           INITIALIZE            JSTR-R.
           MOVE  JOLSF11-02   TO  JSTR-01.
           MOVE  JOLSF11-03   TO  JSTR-02.
           MOVE  JOLSF11-04   TO  JSTR-03.
           MOVE  JOLSF11-05   TO  JSTR-04.
           MOVE  JOLSF11-06   TO  JSTR-05.
           MOVE  JOLSF11-07   TO  JSTR-06.
           MOVE  JOLSF11-08   TO  JSTR-07.
           MOVE  JOLSF11-09   TO  JSTR-08.
           MOVE  JOLSF11-10   TO  JSTR-09.
           MOVE  JOLSF11-11   TO  JSTR-10.
           MOVE  JOLSF11-12   TO  JSTR-11.
           MOVE  JOLSF11-13   TO  JSTR-12.
           MOVE  JOLSF11-14   TO  JSTR-13.
           MOVE  JOLSF11-15   TO  JSTR-14.
           MOVE  JOLSF11-15A  TO  JSTR-14A.
           MOVE  JOLSF11-15B  TO  JSTR-14B.
           MOVE  JOLSF11-15C  TO  JSTR-14C.
           MOVE  JOLSF11-15D  TO  JSTR-14D.
           MOVE  JOLSF11-16   TO  JSTR-15.
           MOVE  JOLSF11-20   TO  JSTR-20.
           MOVE  JOLSF11-16A  TO  JSTR-15A.
           MOVE  JOLSF11-19   TO  JSTR-19.
           MOVE  JOLSF11-168  TO  JSTR-158.
           MOVE  JOLSF11-17   TO  JSTR-16.
           MOVE  JOLSF11-18   TO  JSTR-17.
      *           WRITE   JSTR-R     INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE  "JSTR"       TO    ERR-F
               MOVE  "W"          TO    ERR-M
               MOVE   JSTR-KEY    TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       WRT5-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       WRT6-RTN.
           IF  JNIF1-02  =  7
               GO TO WRT6-010
           END-IF
           MOVE  SPACE        TO  JNIF1-R.
           INITIALIZE             JNIF1-R.
           MOVE  JOLSF121-01  TO  JNIF1-01.
           MOVE  JOLSF121-02  TO  JNIF1-02.
           MOVE  JOLSF121-03  TO  JNIF1-03.
           MOVE  JOLSF121-04  TO  JNIF1-04.
           MOVE  JOLSF121-05  TO  JNIF1-05.
           MOVE  JOLSF121-06  TO  JNIF1-06.
           MOVE  JOLSF121-07  TO  JNIF1-07.
           MOVE  JOLSF121-08  TO  JNIF1-08.
           PERFORM  MV61-RTN  THRU  MV61-RTN-EXIT
               VARYING  NN  FROM  1  BY  1  UNTIL  NN  >  27.
           MOVE  JOLSF121-10  TO  JNIF1-10.
           MOVE  JOLSF121-11  TO  JNIF1-11.
           MOVE  JOLSF121-12  TO  JNIF1-12.
           MOVE  JOLSF121-13  TO  JNIF1-13.
           MOVE  JOLSF121-14  TO  JNIF1-14.
      *           WRITE   JNIF1-R   INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            JNIF_PNAME1 JNIF_LNAME JNIF1-R RETURNING RET.
           IF  RET = 1
               MOVE  "JNIF"       TO    ERR-F
               MOVE  "W"          TO    ERR-M
               MOVE   JNIF1-KEY   TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
           GO TO WRT6-RTN-EXIT.
       WRT6-010.
           MOVE  SPACE        TO  JNIF2-R.
           INITIALIZE             JNIF2-R.
           MOVE  JOLSF122-01  TO  JNIF2-01.
           MOVE  JOLSF122-02  TO  JNIF2-02.
           MOVE  JOLSF122-02A TO  JNIF2-02A.
           MOVE  JOLSF122-03  TO  JNIF2-03.
           MOVE  JOLSF122-04  TO  JNIF2-04.
           MOVE  JOLSF122-05  TO  JNIF2-05.
           MOVE  JOLSF122-06  TO  JNIF2-06.
           MOVE  JOLSF122-07  TO  JNIF2-07.
           MOVE  JOLSF122-08  TO  JNIF2-08.
      *           WRITE   JNIF2-R   INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            JNIF_PNAME1 JNIF_LNAME JNIF2-R RETURNING RET.
           IF  RET = 1
               MOVE  "JNIF"       TO    ERR-F
               MOVE  "W"          TO    ERR-M
               MOVE   JNIF2-KEY   TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       WRT6-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       WRT6-RTN2.
           IF  JNIF1-02  =  7
               GO TO WRT62-010
           END-IF
           MOVE  SPACE        TO  JNIF1-R.
           INITIALIZE             JNIF1-R.
           MOVE  JOLSF121-21  TO  JNIF1-01.
           MOVE  JOLSF121-22  TO  JNIF1-02.
           MOVE  JOLSF121-23  TO  JNIF1-03.
           MOVE  JOLSF121-24  TO  JNIF1-04.
           MOVE  JOLSF121-25  TO  JNIF1-05.
           MOVE  JOLSF121-26  TO  JNIF1-06.
           MOVE  JOLSF121-27  TO  JNIF1-07.
           MOVE  JOLSF121-28  TO  JNIF1-08.
           PERFORM  MV61-RTN2 THRU  MV61-RTN2-EXIT
               VARYING  NN  FROM  1  BY  1  UNTIL  NN  >  27.
           MOVE  JOLSF121-30  TO  JNIF1-10.
           MOVE  JOLSF121-31  TO  JNIF1-11.
           MOVE  JOLSF121-32  TO  JNIF1-12.
           MOVE  JOLSF121-33  TO  JNIF1-13.
           MOVE  JOLSF121-34  TO  JNIF1-14.
      *           WRITE   JNIF1-R   INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            JNIF_PNAME1 JNIF_LNAME JNIF1-R RETURNING RET.
           IF  RET = 1
               MOVE  "JNIF"       TO    ERR-F
               MOVE  "W"          TO    ERR-M
               MOVE   JNIF1-KEY   TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
           GO TO WRT6-RTN2-EXIT.
       WRT62-010.
           MOVE  SPACE        TO  JNIF2-R.
           INITIALIZE             JNIF2-R.
           MOVE  JOLSF122-21  TO  JNIF2-01.
           MOVE  JOLSF122-22  TO  JNIF2-02.
           MOVE  JOLSF122-22A TO  JNIF2-02A.
           MOVE  JOLSF122-23  TO  JNIF2-03.
           MOVE  JOLSF122-24  TO  JNIF2-04.
           MOVE  JOLSF122-25  TO  JNIF2-05.
           MOVE  JOLSF122-26  TO  JNIF2-06.
           MOVE  JOLSF122-27  TO  JNIF2-07.
           MOVE  JOLSF122-28  TO  JNIF2-08.
      *           WRITE   JNIF2-R   INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            JNIF_PNAME1 JNIF_LNAME JNIF2-R RETURNING RET.
           IF  RET = 1
               MOVE  "JNIF"       TO    ERR-F
               MOVE  "W"          TO    ERR-M
               MOVE   JNIF2-KEY   TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       WRT6-RTN2-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       WRT7-RTN.
           MOVE  SPACE           TO  OKJF-R.
           INITIALIZE                OKJF-R.
           MOVE  JOLSF13-02(NN)  TO  OKJF-01.
           MOVE  JOLSF13-03(NN)  TO  OKJF-02.
           MOVE  JOLSF13-04(NN)  TO  OKJF-03.
           MOVE  JOLSF13-05(NN)  TO  OKJF-04.
           MOVE  JOLSF13-06(NN)  TO  OKJF-05.
           MOVE  JOLSF13-07(NN)  TO  OKJF-06.
           MOVE  JOLSF13-08(NN)  TO  OKJF-07.
           MOVE  JOLSF13-09(NN)  TO  OKJF-08.
           MOVE  JOLSF13-10(NN)  TO  OKJF-09.
           MOVE  JOLSF13-11(NN)  TO  OKJF-10.
           MOVE  JOLSF13-12(NN)  TO  OKJF-11.
           MOVE  JOLSF13-13(NN)  TO  OKJF-12.
      *           WRITE   OKJF-R    INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            OKJF_PNAME1 OKJF_LNAME OKJF-R RETURNING RET.
           IF  RET = 1
               MOVE  "OKJF"       TO    ERR-F
               MOVE  "W"          TO    ERR-M
               MOVE   OKJF-KEY    TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       WRT7-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       WRT8-RTN.
           IF  JOLSF141-DGN   =  ZERO
               MOVE  SPACE           TO  TDNW-R1
               INITIALIZE                TDNW-R1
               MOVE  JOLSF141-KEY    TO  TDNW1-KEY
               MOVE  JOLSF141-BC     TO  TDNW1-BC
               MOVE  JOLSF141-SHC    TO  TDNW1-SHC
               MOVE  JOLSF141-DPC    TO  TDNW1-DPC
               MOVE  JOLSF141-HNGP   TO  TDNW1-HNGP
               MOVE  JOLSF141-NNGP   TO  TDNW1-NNGP
               MOVE  JOLSF141-THC    TO  TDNW1-THC
               MOVE  JOLSF141-MHC    TO  TDNW1-MHC
               MOVE  JOLSF141-SNA    TO  TDNW1-SNA
               MOVE  JOLSF141-TNA    TO  TDNW1-TNA
               MOVE  JOLSF141-HCC    TO  TDNW1-HCC
               MOVE  JOLSF141-HSP    TO  TDNW1-HSP
               MOVE  JOLSF141-DHC    TO  TDNW1-DHC
               MOVE  JOLSF141-KHC    TO  TDNW1-KHC
               MOVE  JOLSF141-KCC    TO  TDNW1-KCC
               MOVE  JOLSF141-UBC    TO  TDNW1-UBC
               MOVE  JOLSF141-NCC    TO  TDNW1-NCC
               MOVE  JOLSF141-EDI    TO  TDNW1-EDI
               MOVE  JOLSF141-NKC    TO  TDNW1-NKC
               MOVE  JOLSF141-ZAC    TO  TDNW1-ZAC
               MOVE  9               TO  TDNW1-PC
           ELSE
               MOVE  SPACE           TO  TDNW-R2
               INITIALIZE                TDNW-R2
               MOVE  JOLSF142-KEY    TO  TDNW2-KEY
               MOVE  JOLSF142-HCD    TO  TDNW2-HCD
               MOVE  JOLSF142-ISU    TO  TDNW2-ISU
               MOVE  JOLSF142-KSU    TO  TDNW2-KSU
               MOVE  JOLSF142-HTC    TO  TDNW2-HTC
               MOVE  JOLSF142-SU     TO  TDNW2-SU
               MOVE  JOLSF142-GTN    TO  TDNW2-GTN
               MOVE  JOLSF142-UTN    TO  TDNW2-UTN
               MOVE  JOLSF142-GKIN   TO  TDNW2-GKIN
               MOVE  JOLSF142-UKIN   TO  TDNW2-UKIN
               MOVE  JOLSF142-GCN    TO  TDNW2-GCN
               MOVE  JOLSF142-CCD    TO  TDNW2-CCD
               MOVE  JOLSF142-SHN    TO  TDNW2-SHN
               MOVE  JOLSF142-JAN    TO  TDNW2-JAN
               MOVE  JOLSF142-TSH    TO  TDNW2-TSH
               MOVE  JOLSF142-TKC    TO  TDNW2-TKC
               MOVE  9               TO  TDNW2-PC
           END-IF
      *           WRITE   TDNW-R1   INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            TDNWF_PNAME1 TDNWF_LNAME TDNW-R1 RETURNING RET.
           IF  RET = 1
               MOVE  "TDNWF"      TO    ERR-F
               MOVE  "W"          TO    ERR-M
               MOVE   TDNW1-KEY   TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       WRT8-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       WRT9-RTN.
           IF  JOLSF151-DGN   =  ZERO
               MOVE  SPACE        TO TDNN-R1
               INITIALIZE            TDNN-R1
               MOVE JOLSF151-KEY  TO TDNN1-KEY
               MOVE JOLSF151-BCD  TO TDNN1-BCD
               MOVE JOLSF151-DPC  TO TDNN1-DPC
               MOVE JOLSF151-HNGP TO TDNN1-HNGP
               MOVE JOLSF151-NNGP TO TDNN1-NNGP
               MOVE JOLSF151-THC  TO TDNN1-THC
               MOVE JOLSF151-STA  TO TDNN1-STA
               MOVE JOLSF151-SNA  TO TDNN1-SNA
               MOVE JOLSF151-TNA  TO TDNN1-TNA
               MOVE JOLSF151-TSN  TO TDNN1-TSN
               MOVE JOLSF151-TST  TO TDNN1-TST
               MOVE JOLSF151-HCC  TO TDNN1-HCC
               MOVE JOLSF151-AR   TO TDNN1-AR
               MOVE JOLSF151-DUR  TO TDNN1-DUR
               MOVE JOLSF151-DSHR TO TDNN1-DSHR
               MOVE JOLSF151-DSMR TO TDNN1-DSMR
               MOVE JOLSF151-ER   TO TDNN1-ER
               MOVE JOLSF151-FSR  TO TDNN1-FSR
               MOVE JOLSF151-FUR  TO TDNN1-FUR
               MOVE JOLSF151-LCR  TO TDNN1-LCR
               MOVE JOLSF151-LUR  TO TDNN1-LUR
               MOVE JOLSF151-LSR  TO TDNN1-LSR
               MOVE 9             TO TDNN1-PC
           ELSE
               MOVE  SPACE        TO TDNN-R2
               INITIALIZE            TDNN-R2
               MOVE JOLSF152-KEY  TO TDNN2-KEY
               MOVE JOLSF152-JAN  TO TDNN2-JAN
               MOVE JOLSF152-GAR  TO TDNN2-GAR
               MOVE JOLSF152-TNI  TO TDNN2-TNI
               MOVE JOLSF152-SU   TO TDNN2-SU
               MOVE JOLSF152-GTN  TO TDNN2-GTN
               MOVE JOLSF152-UTN  TO TDNN2-UTN
               MOVE JOLSF152-GKIN TO TDNN2-GKIN
               MOVE JOLSF152-UKIN TO TDNN2-UKIN
               MOVE JOLSF152-SHN  TO TDNN2-SHN
               MOVE JOLSF152-HSC  TO TDNN2-HSC
               MOVE JOLSF152-COR  TO TDNN2-COR
               MOVE JOLSF152-SIZ  TO TDNN2-SIZ
               MOVE JOLSF152-KKK  TO TDNN2-KKK
               MOVE JOLSF152-PCH  TO TDNN2-PCH
               MOVE JOLSF152-PSI  TO TDNN2-PSI
               MOVE JOLSF152-PBM  TO TDNN2-PBM
               MOVE JOLSF152-PJAN TO TDNN2-PJAN
               MOVE JOLSF152-PSHN TO TDNN2-PSHN
               MOVE JOLSF152-PKKK TO TDNN2-PKKK
               MOVE JOLSF152-PUTN TO TDNN2-PUTN
               MOVE JOLSF152-PMS  TO TDNN2-PMS
               MOVE 9             TO TDNN2-PC
           END-IF
      *           WRITE   TDNN-R1   INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            TDNNF_PNAME1 TDNNF_LNAME TDNN-R1 RETURNING RET.
           IF  RET = 1
               MOVE  "TDNNF"      TO    ERR-F
               MOVE  "W"          TO    ERR-M
               MOVE   TDNN1-KEY   TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       WRT9-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       WRT10-RTN.
           MOVE  SPACE       TO  TDI-R.
           INITIALIZE            TDI-R.
           MOVE  JOLSF16-DNO  TO  TDI-DNO.
           MOVE  JOLSF16-GNO  TO  TDI-GNO.
           MOVE  JOLSF16-DATE TO  TDI-DATE.
           MOVE  JOLSF16-TCD  TO  TDI-TCD.
           MOVE  JOLSF16-CCD  TO  TDI-CCD.
           MOVE  JOLSF16-TPC  TO  TDI-TPC.
           MOVE  JOLSF16-HCD  TO  TDI-HCD.
           MOVE  JOLSF16-SIZ  TO  TDI-SIZ.
           MOVE  JOLSF16-SKB  TO  TDI-SKB.
           MOVE  JOLSF16-SNO  TO  TDI-SNO.
           MOVE  JOLSF16-SU   TO  TDI-SU.
           MOVE  JOLSF16-GT   TO  TDI-GT.
           MOVE  JOLSF16-UT   TO  TDI-UT.
           MOVE  JOLSF16-GKIN TO  TDI-GKIN.
           MOVE  JOLSF16-UKIN TO  TDI-UKIN.
           MOVE  JOLSF16-JNO  TO  TDI-JNO.
           MOVE  JOLSF16-JGN  TO  TDI-JGN.
           MOVE  JOLSF16-SOK  TO  TDI-SOK.
           MOVE  JOLSF16-UNS  TO  TDI-UNS.
           MOVE  JOLSF16-ISU  TO  TDI-ISU.
           MOVE  JOLSF16-HNO  TO  TDI-HNO.
           MOVE  JOLSF16-THT  TO  TDI-THT.
           MOVE  JOLSF16-TTE  TO  TDI-TTE.
           MOVE  JOLSF16-TRN  TO  TDI-TRN.
           IF  JOLSF16-TCD  =  5000
               MOVE  9            TO  TDI-PRC
           ELSE
               MOVE  JOLSF16-PRC  TO  TDI-PRC
           END-IF
           MOVE  JOLSF16-UPC  TO  TDI-UPC.
           MOVE  JOLSF16-JAN  TO  TDI-JAN.
      *           WRITE   TDI-R      INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            TDIF_PNAME1 TDIF_LNAME TDI-R RETURNING RET.
           IF  RET = 1
               MOVE  "TDIF"       TO    ERR-F
               MOVE  "W"          TO    ERR-M
               MOVE   TDI-KEY     TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       WRT10-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       WRT11-RTN.
           MOVE  SPACE       TO  TDNA-R.
           INITIALIZE            TDNA-R.
           MOVE  JOLSF17-KEY  TO  TDNA-KEY.
           MOVE  JOLSF17-JAN  TO  TDNA-JAN.
           MOVE  JOLSF17-SU   TO  TDNA-SU.
           MOVE  JOLSF17-GTN  TO  TDNA-GTN.
           MOVE  JOLSF17-UTN  TO  TDNA-UTN.
           MOVE  JOLSF17-GKIN TO  TDNA-GKIN.
           MOVE  JOLSF17-UKIN TO  TDNA-UKIN.
           MOVE  JOLSF17-DPM  TO  TDNA-DPM.
           MOVE  JOLSF17-CLS  TO  TDNA-CLS.
           MOVE  JOLSF17-SHM  TO  TDNA-SHM.
           MOVE  JOLSF17-MKH  TO  TDNA-MKH.
           MOVE  JOLSF17-MSB  TO  TDNA-MSB.
           MOVE  JOLSF17-TY   TO  TDNA-TY.
           MOVE  JOLSF17-HCD  TO  TDNA-HCD.
           MOVE  JOLSF17-COR  TO  TDNA-COR.
           MOVE  JOLSF17-SIZ  TO  TDNA-SIZ.
           MOVE  JOLSF17-NSU  TO  TDNA-NSU.
           MOVE  JOLSF17-TSC  TO  TDNA-TSC.
           MOVE  JOLSF17-CCD  TO  TDNA-CCD.
           MOVE  JOLSF17-TNA  TO  TDNA-TNA.
           MOVE  JOLSF17-HNO  TO  TDNA-HNO.
           MOVE  JOLSF17-HNGP TO  TDNA-HNGP.
           MOVE  JOLSF17-NNGP TO  TDNA-NNGP.
           MOVE  JOLSF17-THC  TO  TDNA-THC.
           MOVE  JOLSF17-BI   TO  TDNA-BI.
           MOVE  JOLSF17-SNGP TO  TDNA-SNGP.
           MOVE  JOLSF17-HNA  TO  TDNA-HNA.
           MOVE  JOLSF17-ZON  TO  TDNA-ZON.
           MOVE  JOLSF17-DC   TO  TDNA-DC.
           MOVE  JOLSF17-DNGP TO  TDNA-DNGP.
           MOVE  JOLSF17-NRC  TO  TDNA-NRC.
           MOVE  JOLSF17-PC   TO  TDNA-PC.
           MOVE  JOLSF17-RC   TO  TDNA-RC.
      *           WRITE   TDNA-R     INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            TDNAF_PNAME1 TDNAF_LNAME TDNA-R RETURNING RET.
           IF  RET = 1
               MOVE  "TDNAF"      TO    ERR-F
               MOVE  "W"          TO    ERR-M
               MOVE   TDNA-KEY    TO    ERR-K
               MOVE   0           TO    ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       WRT11-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       MV61-RTN.
           MOVE  JOLSF121-091(NN)  TO  JNIF1-091(NN).
       MV61-RTN-EXIT.
           EXIT.
       MV61-RTN2.
           MOVE  JOLSF121-291(NN)  TO  JNIF1-091(NN).
       MV61-RTN2-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    E N D R  -  R T N                                         *
      *--------------------------------------------------------------*
       ENDR-RTN.
           CALL "DB_F_Open" USING
            "OUTPUT" JOLSF_PNAME1 " " BY REFERENCE JOLSF_IDLST "0".
           CALL "DB_F_Close" USING
            BY REFERENCE JOLSF_IDLST JOLSF_PNAME1.
      *
       ENDR-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    O P E N  -  R T N                                         *
      *--------------------------------------------------------------*
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "INPUT" JOLSF_PNAME1 " " BY REFERENCE JOLSF_IDLST "0".
      **
           CALL "DB_F_Open" USING
            "I-O" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
      **
           CALL "DB_F_Open" USING
            "I-O" JNIF_PNAME1 "SHARED" BY REFERENCE JNIF_IDLST "1"
            "JNIF1-KEY" BY REFERENCE JNIF1-KEY.
      **
           IF  JS-SIGN      =  0
               CALL "DB_F_Open" USING
                "I-O" TDNNF_PNAME1 "SHARED" BY REFERENCE TDNNF_IDLST "1"
                "TDNN1-KEY" BY REFERENCE TDNN1-KEY
               CALL "DB_F_Open" USING
                "I-O" TDNWF_PNAME1 "SHARED" BY REFERENCE TDNWF_IDLST "1"
                "TDNW1-KEY" BY REFERENCE TDNW1-KEY
               CALL "DB_F_Open" USING
                "I-O" TDIF_PNAME1 "SHARED" BY REFERENCE TDIF_IDLST "1"
                "TDI-KEY" BY REFERENCE TDI-KEY
           ELSE
               CALL "DB_F_Open" USING
                "I-O" TDNAF_PNAME1 "SHARED" BY REFERENCE TDNAF_IDLST "1"
                "TDNA-KEY" BY REFERENCE TDNA-KEY
           END-IF
      **
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
            BY REFERENCE JOLSF_IDLST JOLSF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JNIF_IDLST JNIF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
           IF  JS-SIGN      =  0
               CALL "DB_F_Close" USING
                BY REFERENCE TDNWF_IDLST TDNWF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TDNNF_IDLST TDNNF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TDIF_IDLST TDIF_PNAME1
           END-IF
           IF  JS-SIGN      =  1
               CALL "DB_F_Close" USING
                BY REFERENCE TDNAF_IDLST TDNAF_PNAME1
           END-IF
           CALL "DB_F_Open" USING
            "EXTEND" CHKF_PNAME1 "SHARED" BY REFERENCE CHKF_IDLST "0".
           ACCEPT  W-TIM      FROM  TIME.
           INITIALIZE  CHKF-R.
           ACCEPT  CHKF-DATE  FROM  DATE.
           MOVE    W-TIME     TO    CHKF-TIME.
           MOVE    JS-SIGN    TO    CHKF-SIGN.
           MOVE    9          TO    CHKF-SEN.
           MOVE    "20U"      TO    CHKF-PRG.
      *           WRITE   CHKF-R.
      *//////////////
           CALL "DB_Insert" USING
            CHKF_PNAME1 CHKF_LNAME CHKF-R RETURNING RET.
           CALL "DB_F_Close" USING BY REFERENCE CHKF_IDLST CHKF_PNAME1.
       CLSE-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
           COPY    LPERR.

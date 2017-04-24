       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         JT033U.
       AUTHOR.             I.NAKANISHI.
      ***********************************************************
      *    PROGRAM      :  ëóÇËèÛÉfÅ[É^ÉtÉ@ÉCÉãê∂ê¨ÇR           *
      *    DATA WRITTEN :  63/09/29                             *
      *    SCREEN USED  :  UNUSED                               *
      *    FORM   USED  :  UNUSED                               *
      *    PRINTER TYPE :  UNUSED                               *
      *    COMPILE TYPE :  COBOL                                *
      ***********************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT                PIC X(02)   VALUE  SPACE.
       77  END-SW                  PIC 9(01)   VALUE  0.
       77  ERR-SW                  PIC 9(01)   VALUE  0.
       01  WK-AREA.
           03  OKC                 PIC 9(01).
           03  O-KEY.
               04  O-01            PIC 9(06).
               04  O-02            PIC 9(02).
           03  W-HIN               PIC 9(06).
           03  W-REN               PIC 9(03).
           03  W-RENNO             PIC 9(03).
           03  G-ZAN               PIC S9(04).
           03  W-ZAN               PIC S9(04).
           03  W-BUN.
               04  W-BUN1          PIC S9(04).
               04  W-BUN2          PIC S9(04).
           03  CNT                 PIC 9(01).
           03  SV-OCD              PIC 9(06).
           03  SV-02.
               04  SV-021          PIC 9(02).
               04  SV-022          PIC 9(03).
               04  SV-022R   REDEFINES   SV-022.
                   05  SV-0221         PIC 9(01).
                   05  SV-0222         PIC 9(02).
           03  W-SET               PIC 9(03).
      *
       01  WW2-R.
           02   WW-01              PIC 9(06).
           02   WW-02.
                03   WW-021        PIC 9(02).
                03   WW-022        PIC 9(03).
           02   WW-03              PIC 9(01).
           02   WW-04              PIC 9(06).
           02   WW-05              PIC 9(03).
           02   WW-06              PIC 9(03).
           02   WW-07              PIC 9(06).
           02   WW-08              PIC 9(03).
           02   WW-09              PIC S9(04).
           02   F                  PIC X(05).
       01  NF-WK1-ID               PIC X(17).
       01  NF-WK1-IDR  REDEFINES  NF-WK1-ID.
           02  W-ID1               PIC X(07).
       01  NF-WK2-ID               PIC X(17).
       01  NF-WK2-IDR  REDEFINES  NF-WK2-ID.
           02  W-ID2               PIC X(07).
       01  KBN                     PIC N(03).
      *
       COPY     LWMSG.
      *
           COPY    LNFWK2.
           COPY    LNFWK1.
           COPY    LOKJF.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER  PIC X(12) VALUE "CLEAR SCREEN".
       01  DSP-INI.
           02  FILLER  PIC X(28) VALUE " ".
           02  FILLER  PIC X(26) VALUE "ëóÇËèÛÉfÅ[É^ÉtÉ@ÉCÉãê∂ê¨ÇR".
           02  FILLER  PIC X(02) VALUE "Åi".
           02  DSP-01  PIC  N(03).
           02  FILLER  PIC X(02) VALUE "Åj".
       01  DSP-END.
           02  FILLER  PIC X(12) VALUE "CLEAR SCREEN".
      *
       COPY  LSMSG.
      *
       PROCEDURE           DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *DSP-INI
       CALL "SD_Init" USING 
            "DSP-INI" " " "1" "0" "64" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-INI" "RX" "1" "20" "28" " " "DSP-INI"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-INI" "X" "1" "21" "26" "01DSP-INI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-INI" "X" "1" "49" "2" "02DSP-INI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" "N" "1" "51" "6" "03DSP-INI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-01" BY REFERENCE KBN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-INI" "X" "1" "57" "2" "DSP-01" " " RETURNING RESU.
      *DSP-END
       CALL "SD_Init" USING
           "DSP-END" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-END" "X" "1" "0" "12" " " "DSP-END" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *************************
      *    ÉÅÉCÉìÅ@ÉãÅ[É`Éì   *
      *************************
       MAIN.
           PERFORM  INI-RTN    THRU  INI-EX.
           PERFORM  UPD-RTN    THRU  UPD-EX.
           PERFORM  END-RTN    THRU  END-EX.
           CALL "SD_Output" USING "DSP-END" DSP-END "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *----------------------*
      *    ÇhÇmÇhÅ|ÇqÇsÇm    *
      *----------------------*
       INI-RTN.
           PERFORM  JS-ACP-RTN     THRU  JS-ACP-EX.
           IF  IPN-KYO-KBN  =  0
               MOVE  "NF-WK1 "     TO  W-ID1
               MOVE  W-ID1         TO  NF-WK1_PNAME1
               MOVE  "NF-WK2 "     TO  W-ID2
               MOVE  W-ID2         TO  NF-WK2_PNAME1
               MOVE  "ã≥Å@àÁ"    TO  KBN
           END-IF
           IF  IPN-KYO-KBN  =  1
               MOVE  "NF-WK1I"     TO  W-ID1
               MOVE  W-ID1         TO  NF-WK1_PNAME1
               MOVE  "NF-WK2I"     TO  W-ID2
               MOVE  W-ID2         TO  NF-WK2_PNAME1
               MOVE  "àÍÅ@î "    TO  KBN
           END-IF
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-INI" DSP-INI "p" RETURNING RESU.
      *
           INITIALIZE       WK-AREA.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" NF-WK2_PNAME1 
            "EXCLUSIVE" BY REFERENCE NF-WK2_IDLST
            "1" "WK2-KEY" BY REFERENCE WK2-KEY.
           CALL "DB_F_Open" USING "OUTPUT SEQUENTIAL" NF-WK1_PNAME1 
            "EXCLUSIVE" BY REFERENCE NF-WK1_IDLST
            "1" "WK1-KEY" BY REFERENCE WK1-KEY.
           CALL "DB_F_Open" USING
            "I-O" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
       INI-020.
      *           READ     OKJF     NEXT    AT    END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" OKJF_PNAME1 BY REFERENCE OKJF-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  INI-EX
           END-IF
           IF  IPN-KYO-KBN   NOT =   OKJF-09
               GO  TO  INI-020
           END-IF
           IF  OKJF-10  NOT =   0
               GO  TO  INI-020
           END-IF
           MOVE     ZERO     TO      OKJF-07.
      *           REWRITE  OKJF-R   INVALID
      *///////////////
           CALL "DB_Update" USING
            OKJF_PNAME1 OKJF_LNAME OKJF-R RETURNING RET.
           IF  RET = 1
               MOVE   "OKJF"     TO   ERR-F
               MOVE    OKJF-KEY  TO   ERR-K
               MOVE   "R"        TO   ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
               PERFORM  END-RTN  THRU  END-RTN
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP    RUN
           END-IF
           GO  TO   INI-020.
       INI-EX.
           EXIT.
      *----------------------*
      *    ÇdÇmÇcÅ|ÇqÇsÇm    *
      *----------------------*
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE NF-WK2_IDLST NF-WK2_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NF-WK1_IDLST NF-WK1_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
       END-EX.
           EXIT.
      *--------------------------------*
      *    ÉfÅ[É^Å@Å@íäèoÅ@            *
      *--------------------------------*
       UPD-RTN.
      *           READ     NF-WK2   NEXT  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" NF-WK2_PNAME1 BY REFERENCE WK2-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE     1     TO     END-SW
               GO  TO   UPD-020
           END-IF
           IF  SV-OCD   =        ZERO
               MOVE     WK2-01  TO   SV-OCD
               IF  WK2-06   >       1
                   MOVE     WK2-06  TO   W-SET
               END-IF
           END-IF
           IF  WK2-01   NOT =    SV-OCD
               IF  SV-OCD   =       O-01
                   GO  TO  UPD-020
               ELSE
                   GO  TO  UPD-070
               END-IF
           END-IF.
       UPD-001.
           IF  WK2-06   >        1
               IF  WK2-021  NOT =     SV-021
                   ADD      WK2-06    TO      W-SET
                   GO  TO   UPD-005
               ELSE
                   GO  TO   UPD-005
               END-IF
           END-IF
           IF  WK2-022  =        900
               GO  TO  UPD-010
           END-IF
           IF  SV-0221  =        9
               GO  TO  UPD-020
           END-IF
           IF  WK2-06   =        1
               MOVE     1        TO      W-SET
               GO       TO       UPD-005
           END-IF
           IF  WK2-02   NOT =    SV-02
               ADD      1       TO   W-SET
           END-IF.
       UPD-004.
           MOVE     WK2-02   TO       SV-02.
       UPD-005.
           IF  WK2-022  =        900
               GO  TO  UPD-010
           END-IF
           PERFORM  MOV1-RTN  THRU    MOV1-EX.
           IF  ERR-SW   =        1
               GO  TO   UPD-EX
           END-IF
           MOVE     WK2-02   TO       SV-02.
           GO  TO   UPD-RTN.
       UPD-010.
           IF  O-KEY    NOT =    ZERO
               GO  TO  UPD-020
           END-IF
           MOVE     WK2-01   TO       O-01.
           MOVE     WK2-021  TO       O-02.
           MOVE     WK2-04   TO       W-HIN.
           MOVE     WK2-R    TO       WW2-R.
           MOVE     900      TO       W-REN  W-RENNO.
           MOVE     1        TO       CNT.
           GO  TO   UPD-RTN.
       UPD-020.
           IF  END-SW   =        1
               IF  O-KEY    =       ZERO
                   GO  TO  UPD-070
               END-IF
           END-IF
           COMPUTE  G-ZAN    =        G-ZAN   +    WW-09.
           IF  WW-08    >        G-ZAN
               GO  TO  UPD-030
           END-IF
           MOVE     G-ZAN    TO       W-ZAN.
           MOVE     ZERO     TO       G-ZAN.
           COMPUTE  W-REN    =        W-RENNO +    1.
           IF  WW-08    =        W-ZAN
               GO  TO  UPD-030
           END-IF
           COMPUTE  W-BUN2   =        W-ZAN   -    WW-08.
           COMPUTE  W-BUN1   =        WW-09   -    W-BUN2.
           MOVE     W-BUN2   TO       G-ZAN.
       UPD-030.
           PERFORM  MOV2-RTN THRU     MOV2-EX.
           IF  ERR-SW   =        1
               GO  TO   UPD-EX
           END-IF
           MOVE     W-REN    TO       W-RENNO.
           IF  W-BUN2   =        ZERO
               GO  TO  UPD-040
           END-IF
           MOVE     W-BUN2   TO       W-BUN1.
           MOVE     ZERO     TO       W-BUN2.
           GO  TO   UPD-030.
       UPD-040.
           IF  END-SW   =        1
               GO  TO   UPD-070
           END-IF
           IF  WK2-01   NOT =    O-01
               GO  TO  UPD-050
           END-IF
           IF  WK2-021  NOT =    O-02
               GO  TO  UPD-050
           END-IF
           IF  WK2-04   NOT =    W-HIN
               ADD      1      TO    CNT
           END-IF
           IF  CNT      >        6
               ADD      1      TO    W-RENNO  W-REN
               MOVE     1      TO    CNT
               MOVE     ZERO   TO    G-ZAN
           END-IF
           GO  TO   UPD-060.
       UPD-050.
           MOVE     ZERO     TO       G-ZAN.
           MOVE     1        TO       CNT.
           MOVE     ZERO     TO       O-KEY.
           IF  WK2-01   NOT =    SV-OCD
               GO  TO   UPD-070
           END-IF
           IF  WK2-022  NOT =    900
               MOVE     ZERO   TO     SV-02
               GO  TO   UPD-001
           END-IF
           MOVE     WK2-022  TO       W-REN  W-RENNO.
       UPD-060.
           MOVE     WK2-01   TO       O-01.
           MOVE     WK2-021  TO       O-02.
           MOVE     WK2-04   TO       W-HIN.
           MOVE     WK2-R    TO       WW2-R.
           GO  TO   UPD-RTN.
       UPD-070.
           IF  SV-OCD   =        ZERO
               GO  TO  UPD-080
           END-IF
           MOVE     SV-OCD   TO       OKJF-KEY.
      *           READ     OKJF     INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" OKJF_PNAME1 BY REFERENCE OKJF-R " " RETURNING RET.
           IF  RET = 1
               MOVE   "OKJF"     TO   ERR-F
               MOVE    OKJF-KEY  TO   ERR-K
               MOVE   "A"        TO   ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO   UPD-EX
           END-IF
           MOVE     W-SET    TO       OKJF-07.
      *           REWRITE  OKJF-R   INVALID
      *///////////////
           CALL "DB_Update" USING
            OKJF_PNAME1 OKJF_LNAME OKJF-R RETURNING RET.
           IF  RET = 1
               MOVE    "OKJF"     TO    ERR-F
               MOVE    OKJF-KEY   TO    ERR-K
               MOVE    "R"        TO    ERR-F
               PERFORM  ERR-RTN  THRU   ERR-EX
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO   UPD-EX
           END-IF.
       UPD-080.
           IF  END-SW   =        1
               GO  TO  UPD-EX
           END-IF
           MOVE     WK2-01   TO       SV-OCD.
           MOVE     ZERO     TO       W-SET  SV-02.
           IF  WK2-06   >       1
               MOVE     WK2-06  TO   W-SET
           END-IF
           GO  TO   UPD-001.
       UPD-EX.
           EXIT.
      *****************************************************
      *    çÄñ⁄ ÇlÇnÇuÇd ÉãÅ[É`Éì   (é}î‘   ÅÇ  ÇXÇO)     *
      *****************************************************
       MOV1-RTN.
           MOVE     SPACE     TO    WK1-R.
           INITIALIZE               WK1-R.
           MOVE     WK2-01    TO    WK1-01.
           MOVE     WK2-021   TO    WK1-021.
           MOVE     WK2-022   TO    WK1-022.
           MOVE     WK2-03    TO    WK1-03.
           MOVE     WK2-04    TO    WK1-04.
           MOVE     WK2-05    TO    WK1-05.
           MOVE     WK2-06    TO    WK1-06.
           MOVE     ZERO      TO    WK1-07.
           MOVE     WK2-07    TO    WK1-08.
           MOVE     WK2-08    TO    WK1-09.
           MOVE     ZERO      TO    WK1-10.
           MOVE     WK2-09    TO    WK1-11.
           PERFORM  WRI-RTN  THRU  WRI-EX.
       MOV1-EX.
           EXIT.
      *****************************************************
      *    çÄñ⁄ ÇlÇnÇuÇd ÉãÅ[É`Éì   (é}î‘   ÅÅ  ÇXÇO)     *
      *****************************************************
       MOV2-RTN.
           MOVE     SPACE     TO    WK1-R.
           INITIALIZE               WK1-R.
           MOVE     WW-01     TO    WK1-01.
           MOVE     WW-021    TO    WK1-021.
           MOVE     W-RENNO   TO    WK1-022.
           MOVE     WW-03     TO    WK1-03.
           MOVE     WW-04     TO    WK1-04.
           MOVE     WW-05     TO    WK1-05.
           MOVE     WW-06     TO    WK1-06.
           MOVE     ZERO      TO    WK1-07.
           MOVE     WW-07     TO    WK1-08.
           MOVE     WW-08     TO    WK1-09.
           MOVE     ZERO      TO    WK1-10.
           IF  W-BUN1    =     ZERO
               MOVE      WW-09     TO     WK1-11
           ELSE
               MOVE      W-BUN1    TO     WK1-11
           END-IF
           MOVE     ZERO      TO    W-BUN1.
           PERFORM  WRI-RTN  THRU  WRI-EX.
           IF  WK1-02    NOT =     SV-02
               ADD   1    TO    W-SET
           END-IF
           MOVE     WK1-02    TO   SV-02.
       MOV2-EX.
           EXIT.
      *****************************************************
      *    ÇvÇqÇhÇsÇdÅ@  ÉãÅ[É`Éì                         *
      *****************************************************
       WRI-RTN.
      *           WRITE    WK1-R    INVALID
      *//////////////
           CALL "DB_Insert" USING
            NF-WK1_PNAME1 NF-WK1_LNAME WK1-R RETURNING RET.
           IF  RET = 1
               MOVE  "NF-WK1"   TO   ERR-F
               MOVE   WK2-KEY   TO   ERR-K
               MOVE  "W"        TO   ERR-M
               PERFORM  ERR-RTN THRU  ERR-EX
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE   1    TO   ERR-SW
               GO  TO  WRI-EX
           END-IF.
       WRI-EX.
           EXIT.
      ***
      *****************************
      *    ¥◊∞ DISPLAY (“≤›)      *
      *****************************
       ERR-RTN.
           MOVE    ERR-STAT  TO  ERR-FLG.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
       ERR-EX.
           EXIT.
       COPY  LPACPT.

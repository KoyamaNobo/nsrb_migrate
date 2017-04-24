       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         JT032U.
       AUTHOR.             I.NAKANISHI.
      ***********************************************************
      *    PROGRAM      :  ëóÇËèÛÉfÅ[É^ÉtÉ@ÉCÉãê∂ê¨ÇQ           *
      *    DATA WRITTEN :  63/09/27                             *
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
       77  STA-SW                  PIC 9(01)   VALUE  0.
       77  SET-SW                  PIC 9(01)   VALUE  0.
       77  HIKU-SW                 PIC 9(01)   VALUE  0.
       01  WK-AREA.
           03  W-KEY.
               04  W-K1            PIC 9(06).
               04  W-K2            PIC 9(02).
               04  W-HIN           PIC 9(06).
           03  W-REN1              PIC 9(03).
           03  O-KEY.
               04  O-01            PIC 9(06).
               04  O-02            PIC 9(02).
               04  O-03            PIC 9(01).
           03  OLD-KEY.
               04  OL-01           PIC 9(06).
               04  OL-021          PIC 9(02).
               04  OL-022          PIC 9(03).
               04  OL-03           PIC 9(01).
               04  OL-04           PIC 9(06).
           03  W-REN               PIC 9(03).
           03  W-RENNO             PIC 9(03).
           03  UP-REN              PIC 9(03).
           03  SV-RENNO            PIC 9(03).
           03  G-ZAN               PIC S9(04).
           03  W-ZAN               PIC S9(04).
           03  W-BUN.
               04  W-BUN1          PIC S9(04).
               04  W-BUN2          PIC S9(04).
           03  W-Z                 PIC S9(04).
           03  W-GOK               PIC S9(04).
           03  WW-GOK              PIC S9(04).
           03  P                   PIC 9(04).
           03  I                   PIC 9(04).
           03  CNT                 PIC 9(01).
           03  W-C                 PIC 9(01).
      *
       01  WW1-R.
           02   WW-01                 PIC 9(06).
           02   WW-02.
                03   WW-021           PIC 9(02).
                03   WW-022           PIC 9(03).
           02   WW-03                 PIC 9(01).
           02   WW-04                 PIC 9(06).
           02   WW-05                 PIC 9(03).
           02   WW-06                 PIC 9(03).
           02   WW-07                 PIC S9(04).
           02   WW-08                 PIC 9(06).
           02   WW-09                 PIC 9(03).
           02   WW-10                 PIC S9(04).
           02   WW-11                 PIC S9(04).
           02   F                     PIC X(06).
       01  NF-WK1-ID                  PIC X(17).
       01  NF-WK1-IDR    REDEFINES    NF-WK1-ID.
           02  W-ID1                  PIC X(07).
       01  NF-WK2-ID                  PIC X(17).
       01  NF-WK2-IDR    REDEFINES    NF-WK2-ID.
           02  W-ID2                  PIC X(07).
       01  NF-WK3-ID                  PIC X(17).
       01  NF-WK3-IDR    REDEFINES    NF-WK3-ID.
           02  W-ID3                  PIC X(07).
       01  KBN                        PIC N(03).
      ***
       COPY  LWMSG.
      *
           COPY    LNFWK1.
           COPY    LNFWK3.
           COPY    LNFWK2.
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
           02  FILLER  PIC X(26) VALUE "ëóÇËèÛÉfÅ[É^ÉtÉ@ÉCÉãê∂ê¨ÇQ".
           02  FILLER  PIC X(02) VALUE "Åi".
           02  DSP-01  PIC  N(03).
           02  FILLER  PIC X(02) VALUE "Åj".
       01  DSP-END.
           02  FILLER  PIC X(12) VALUE "CLEAR SCREEN".
       01  DSP-ERR.
           02  ERR-NIF PIC X(28)
                        VALUE    "ÅñÅ@ÇPâ◊éDÇ≈ÇUåÖÇí¥Ç¶ÇÈÅ@Åñ".
      *
       COPY  LSMSG.
      *
       PROCEDURE           DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-CLR
       CALL "SD_Init" USING
           "DSP-CLR" " " "1" "0" "12" " " " " RETURNING RESU.
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
           "DSP-END" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-END" "X" "1" "0" "12" " " "DSP-END" RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "28" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-NIF" "X" "24" "1" "28" " " "DSP-ERR" RETURNING RESU.
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
               MOVE  "NF-WK3 "     TO  W-ID3
               MOVE  W-ID3         TO  NF-WK3_PNAME1
               MOVE  "ã≥Å@àÁ"    TO  KBN
           END-IF
           IF  IPN-KYO-KBN  =  1
               MOVE  "NF-WK1I"     TO  W-ID1
               MOVE  W-ID1         TO  NF-WK1_PNAME1
               MOVE  "NF-WK2I"     TO  W-ID2
               MOVE  W-ID2         TO  NF-WK2_PNAME1
               MOVE  "NF-WK3I"     TO  W-ID3
               MOVE  W-ID3         TO  NF-WK3_PNAME1
               MOVE  "àÍÅ@î "    TO  KBN
           END-IF
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-INI" DSP-INI "p" RETURNING RESU.
      *
           INITIALIZE       WK-AREA.
      **
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" NF-WK1_PNAME1 
            "EXCLUSIVE" BY REFERENCE NF-WK1_IDLST
            "1" "WK1-KEY" BY REFERENCE WK1-KEY.
           CALL "DB_F_Open" USING
            "INPUT" NF-WK3_PNAME1 "EXCLUSIVE" BY REFERENCE NF-WK3_IDLST
            "1" "WK3-KEY" BY REFERENCE WK3-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" NF-WK2_PNAME1 "EXCLUSIVE" BY REFERENCE NF-WK2_IDLST
            "1" "WK2-KEY" BY REFERENCE WK2-KEY.
      **
       INI-EX.
           EXIT.
      *----------------------*
      *    ÇdÇmÇcÅ|ÇqÇsÇm    *
      *----------------------*
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE NF-WK1_IDLST NF-WK1_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NF-WK3_IDLST NF-WK3_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NF-WK2_IDLST NF-WK2_PNAME1.
       END-EX.
           EXIT.
      *--------------------------------*
      *    ÉfÅ[É^Å@Å@íäèoÅ@            *
      *--------------------------------*
       UPD-RTN.
      *           READ     NF-WK1   NEXT  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" NF-WK1_PNAME1 BY REFERENCE WK1-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE     1     TO     END-SW
           END-IF
      **
           IF  END-SW   =   1
               IF  SET-SW   =   1
                   GO  TO  UPD-050
               ELSE
                   GO  TO  UPD-EX
               END-IF
           END-IF
      **
           IF  WK1-06   =        ZERO
               GO  TO  UPD-030
           END-IF
           IF  WK1-01   NOT =    W-K1
               GO  TO  UPD-020
           END-IF
           IF  WK1-021  NOT =    W-K2
               GO  TO  UPD-020
           END-IF.
       UPD-010.
           IF  WK1-04   NOT =    W-HIN
               ADD      1     TO     W-C
           END-IF
           IF  W-C      >        6
               ADD      1     TO     W-REN1
               MOVE     1     TO     W-C
               GO       TO    UPD-024
           END-IF
           GO  TO   UPD-024.
       UPD-020.
           MOVE     1        TO       W-REN1       W-C.
       UPD-024.
           MOVE     WK1-01   TO       W-K1.
           MOVE     WK1-021  TO       W-K2.
           MOVE     WK1-04   TO       W-HIN.
       UPD-025.
           PERFORM  MOV1-RTN  THRU    MOV1-EX.
           IF  ERR-SW   =        1
               GO  TO   UPD-EX
           END-IF
           GO  TO   UPD-RTN.
       UPD-030.
           IF  O-KEY    NOT =    ZERO
               GO  TO  UPD-050
           END-IF
           MOVE     WK1-01   TO       O-01.
           MOVE     WK1-021  TO       O-02.
           MOVE     WK1-03   TO       O-03.
           MOVE     WK1-R    TO       WW1-R.
           MOVE     1        TO       SET-SW.
           MOVE     1        TO       W-REN  W-RENNO  P.
           MOVE     1        TO       STA-SW.
           GO  TO   UPD-RTN.
       UPD-050.
           IF  WW-10    =        ZERO
               GO  TO  UPD-060
           END-IF
           MOVE     1        TO       I.
           ADD      UP-REN   TO       W-REN.
       UPD-055.
           IF  I        >        WW-10
               GO  TO  UPD-056
           END-IF
           IF  STA-SW   =        1
               MOVE     0       TO   W-REN   P
           END-IF
           COMPUTE  W-RENNO  =        W-REN   +    I.
           PERFORM  MOV2-RTN THRU     MOV2-EX.
           IF  ERR-SW   =        1
               GO  TO   UPD-EX
           END-IF
           ADD      1        TO       I.
           GO  TO   UPD-055.
       UPD-056.
           SUBTRACT UP-REN   FROM     W-REN.
           ADD      WW-10    TO       P.
           IF  G-ZAN    NOT =    ZERO
               MOVE     W-REN    TO       W-RENNO
               ADD      WW-10    TO       UP-REN
           ELSE
               MOVE     W-RENNO  TO       W-REN
               IF  WW-11    NOT =  ZERO
                   ADD      1   TO       W-RENNO  W-REN  P
               END-IF
           END-IF
           MOVE     ZERO     TO       WW-10.
       UPD-060.
           MOVE     WW-01    TO       WK3-01.
           MOVE     WW-021   TO       WK3-02.
           MOVE     WW-03    TO       WK3-03.
      *           READ     NF-WK3   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NF-WK3_PNAME1 BY REFERENCE WK3-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE    "NF-WK3" TO   ERR-F
               MOVE     WK3-KEY TO   ERR-K
               MOVE    "A"      TO   ERR-M
               PERFORM  ERR-RTN THRU  ERR-EX
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  UPD-EX
           END-IF
           IF  WK3-06   =        ZERO
               GO  TO  UPD-100
           END-IF
           IF  WW-11    =        ZERO
               GO  TO  UPD-080
           END-IF
           IF  WK3-08   =        WK3-06
               GO  TO  UPD-100
           END-IF
           COMPUTE  W-GOK    =        WK3-08  *    WK3-05.
           COMPUTE  W-GOK    =        WK3-04  -    W-GOK.
           ADD      WW-11    TO       WW-GOK.
           COMPUTE  W-Z      =        W-GOK   -    WW-GOK.
           COMPUTE  G-ZAN    =        G-ZAN   +    WW-11.
           IF  WK3-05   >        G-ZAN
               IF  UP-REN     =      0
                   IF  P        >       WK3-06
                       GO  TO  UPD-100
                   ELSE
                       IF  WK3-07   NOT <    W-Z
                           GO  TO   UPD-100
                       ELSE
                           GO  TO   UPD-070
                       END-IF
                   END-IF
               ELSE
                   IF  WK3-07   NOT <    W-Z
                       GO  TO   UPD-100
                   ELSE
                       GO  TO   UPD-070
                   END-IF
               END-IF
           END-IF
           MOVE     G-ZAN    TO       W-ZAN.
           MOVE     ZERO     TO       G-ZAN.
           COMPUTE  W-REN    =        W-RENNO +    1.
           ADD      UP-REN   TO       W-REN.
           MOVE     0        TO       UP-REN.
           ADD      1        TO       P.
           IF  WK3-05   =        W-ZAN
               GO      TO       UPD-070
           END-IF
           COMPUTE  W-BUN2   =        W-ZAN   -    WK3-05.
           COMPUTE  W-BUN1   =        WW-11   -    W-BUN2.
           MOVE     W-BUN2   TO       G-ZAN.
       UPD-070.
           PERFORM  MOV2-RTN THRU     MOV2-EX.
           IF  ERR-SW   =        1
               GO  TO   UPD-EX
           END-IF
           MOVE     W-REN    TO       W-RENNO.
           IF  W-BUN2   =        ZERO
               GO  TO  UPD-080
           END-IF
           MOVE     W-BUN2   TO       W-BUN1.
           MOVE     ZERO     TO       W-BUN2.
           IF  WK3-07   NOT <    W-Z
               GO  TO   UPD-100
           END-IF
           IF  P        NOT >    WK3-06
               GO  TO   UPD-070
           ELSE
               GO  TO   UPD-100
           END-IF.
       UPD-080.
           IF  END-SW   =        1
               GO  TO  UPD-110
           END-IF
           MOVE     0        TO       STA-SW.
      ***
           IF  WW-11    =        ZERO
               IF  G-ZAN    =        ZERO
                   IF  WK1-11   NOT =    ZERO
                       IF  WK1-10   =        ZERO
                           ADD      1        TO    W-RENNO  W-REN  P
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           MOVE     0        TO       HIKU-SW.
           IF  WW-11    NOT =    ZERO
               IF  G-ZAN    =        ZERO
                   IF  WK1-11   =        ZERO
                       IF  WK1-10   NOT =    ZERO
                           SUBTRACT     1    FROM  W-REN  P
                           MOVE     1   TO   HIKU-SW
                       ELSE
                           GO  TO   UPD-08A
                       END-IF
                   ELSE
                       IF  WK3-08   =   WK3-06
                           IF  WK1-10   NOT =    ZERO
                               SUBTRACT     1    FROM  W-REN  P
                               MOVE     1   TO   HIKU-SW
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.
       UPD-08A.
           IF  WK1-01   NOT =    O-01
               GO  TO  UPD-081
           END-IF
           IF  WK1-021  NOT =    O-02
               GO  TO  UPD-081
           END-IF
           IF  WK1-03   NOT =    O-03
               GO  TO  UPD-085
           END-IF
           GO  TO   UPD-086.
       UPD-081.
           MOVE     1        TO       STA-SW.
           MOVE     1        TO       W-REN  W-RENNO.
           MOVE     ZERO     TO       UP-REN.
       UPD-085.
           IF  WK1-10       =    0
               MOVE     1        TO       P
               GO   TO   UPD-085A
           END-IF
           MOVE     0        TO       P.
           IF  HIKU-SW      =    1
               GO  TO  UPD-085A
           END-IF
           IF  UP-REN  =    0
               IF  WW-11    NOT =  ZERO
                   IF  WK3-06  NOT =  WK3-08
                       SUBTRACT   1      FROM     W-REN
                   END-IF
               END-IF
           END-IF.
       UPD-085A.
           MOVE     ZERO     TO       G-ZAN  WW-GOK  W-GOK  W-Z.
       UPD-086.
           MOVE     WK1-01   TO       O-01.
           MOVE     WK1-021  TO       O-02.
           MOVE     WK1-03   TO       O-03.
           MOVE     WK1-R    TO       WW1-R.
           MOVE     1        TO       SET-SW.
           GO  TO   UPD-RTN.
       UPD-090.
           IF  P        =        0
               GO  TO   UPD-110
           END-IF
           IF  P        NOT >    WK3-06
               GO  TO   UPD-050
           END-IF
           IF  WW-11    NOT =    ZERO
               GO  TO  UPD-100
           END-IF
           GO  TO   UPD-110.
       UPD-100.
           MOVE     W-RENNO  TO       SV-RENNO.
           MOVE     900      TO       W-RENNO.
           PERFORM  MOV2-RTN THRU     MOV2-EX.
           IF  ERR-SW   =        1
               GO  TO   UPD-EX
           END-IF
           MOVE     SV-RENNO  TO      W-RENNO.
       UPD-110.
           IF  END-SW   =        1
               GO  TO   UPD-EX
           END-IF
           GO  TO   UPD-080.
       UPD-EX.
           EXIT.
      *****************************************************
      *    çÄñ⁄ ÇlÇnÇuÇd ÉãÅ[É`Éì   (æØƒêî  ÅÇ  ZERO)     *
      *****************************************************
       MOV1-RTN.
           MOVE     SPACE     TO    WK2-R.
           INITIALIZE               WK2-R.
           MOVE     WK1-01    TO    WK2-01.
           MOVE     WK1-021   TO    WK2-021.
           MOVE     W-REN1    TO    WK2-022.
           MOVE     0         TO    WK2-03.
           MOVE     WK1-04    TO    WK2-04.
           MOVE     WK1-05    TO    WK2-05.
           MOVE     WK1-06    TO    WK2-06.
           MOVE     WK1-08    TO    WK2-07.
           MOVE     WK1-09    TO    WK2-08.
           MOVE     WK1-11    TO    WK2-09.
           PERFORM  WRI-RTN  THRU  WRI-EX.
       MOV1-EX.
           EXIT.
      *****************************************************
      *    çÄñ⁄ ÇlÇnÇuÇd ÉãÅ[É`Éì   (æØƒêî  ÅÅ  ZERO)     *
      *****************************************************
       MOV2-RTN.
           MOVE     0        TO    SET-SW.
      **
           MOVE     SPACE    TO    WK2-R.
           INITIALIZE              WK2-R.
           MOVE     WW-01    TO    WK2-01.
           MOVE     WW-021   TO    WK2-021.
           MOVE     W-RENNO  TO    WK2-022.
           IF  W-RENNO  =     900
               MOVE      9    TO    WK2-03
           ELSE
               MOVE     WW-03     TO   WK2-03
           END-IF
           MOVE     WW-04    TO    WK2-04.
           MOVE     WW-05    TO    WK2-05.
           MOVE     WW-06    TO    WK2-06.
           MOVE     WW-08    TO    WK2-07.
           MOVE     WW-09    TO    WK2-08.
           IF  WW-10    NOT =     ZERO
               MOVE  WW-09    TO     WK2-09
           ELSE
               IF  W-BUN1   =     ZERO
                   MOVE  WW-11    TO     WK2-09
               ELSE
                   MOVE  W-BUN1   TO   WK2-09
               END-IF
           END-IF
           MOVE     ZERO     TO    W-BUN1.
           IF  WK2-03   =     9
               GO  TO  MOV2-020
           END-IF
           IF  WK2-01   NOT =     OL-01
               GO  TO  MOV2-010
           END-IF
           IF  WK2-021  NOT =     OL-021
               GO  TO  MOV2-010
           END-IF
           IF  WK2-022  NOT =     OL-022
               GO  TO  MOV2-010
           END-IF
           IF  WK2-03   NOT =     OL-03
               GO  TO  MOV2-010
           END-IF
           IF  WK2-04   NOT =     OL-04
               ADD   1   TO   CNT
           END-IF
           IF  CNT      >     6
               CALL "SD_Output" USING
                "ERR-NIF" ERR-NIF "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE  1   TO   ERR-SW
               GO  TO  MOV2-EX
           END-IF
           GO  TO   MOV2-020.
       MOV2-010.
           MOVE     1        TO    CNT.
       MOV2-020.
           PERFORM  WRI-RTN  THRU  WRI-EX.
           IF  ERR-SW   =     1
               GO  TO  MOV2-EX
           END-IF.
       MOV2-030.
           IF  WK2-03   =     9
               GO  TO  MOV2-EX
           END-IF
           MOVE     WK2-01   TO    OL-01.
           MOVE     WK2-021  TO    OL-021.
           MOVE     WK2-022  TO    OL-022.
           MOVE     WK2-03   TO    OL-03.
           MOVE     WK2-04   TO    OL-04.
       MOV2-EX.
           EXIT.
      *****************************************************
      *    ÇvÇqÇhÇsÇdÅ@  ÉãÅ[É`Éì                         *
      *****************************************************
       WRI-RTN.
      *           WRITE    WK2-R    INVALID
      *//////////////
           CALL "DB_Insert" USING
            NF-WK2_PNAME1 NF-WK2_LNAME WK2-R RETURNING RET.
           IF  RET = 1
               MOVE  "NF-WK2"   TO   ERR-F
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
      *
      *

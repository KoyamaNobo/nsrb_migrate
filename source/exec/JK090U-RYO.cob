       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JKO90U.
      *================================================================*
      *    ì˙éüåJâzèàóùÅiäOïîÅj ÉvÉçÉOÉâÉÄ                             *
      *            ÇWÇXÅ^Å@ÇWÅ^  ÇPÇO   ÇaÇxÅ@ÇrÅDÇrÇ`ÇjÇhÇxÇ`ÇlÇ`     *
      *================================================================*
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM3100.
       OBJECT-COMPUTER.               SYSTEM3100.
       INPUT-OUTPUT                   SECTION.
       DATA                           DIVISION.
       WORKING-STORAGE                SECTION.
      *
       77  END-SW     PIC 9(1).
       77  ERR-STAT   PIC X(2).
       77  JNIF1-OLD  PIC 9(6).
       01  TDNW-OLD.
           02  TDNW-OSTC     PIC X(09).
           02  TDNW-ODNO     PIC X(09).
       01  TDNN-OLD.
           02  TDNN-OSTC     PIC X(09).
           02  TDNN-ODNO     PIC X(09).
       01  TDNA-OLD.
           02  TDNA-OSTC     PIC X(07).
           02  TDNA-ODNO     PIC X(07).
       01  WORK-AREA.
           02  SYS-DATE      PIC  9(06).
           02  HIZ1-W        PIC  9(06).
           02  HIZ1-WR       REDEFINES  HIZ1-W.
               03  YY1-W     PIC  9(02).
               03  MM1-W     PIC  9(02).
               03  DD1-W     PIC  9(02).
           02  KAKU-W        PIC  X(01).
           02  HIZ2-W        PIC  9(08).
           02  HIZ2-WR       REDEFINES  HIZ2-W.
               03  F         PIC  9(02).
               03  HIZ21-W   PIC  9(06).
       COPY LWMSG.
      *
           COPY  L-JSTR-RYO.
           COPY  L-JNIF-RYO.
           COPY  LOKJF-RYO.
           COPY  LITDNW-RYO.
           COPY  LITDNN-RYO.
           COPY  L-TDIF-RYO.
           COPY  LITDNA-RYO.
      *
       77  END-STS            PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-GAMEN.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
           02  FILLER  PIC N(12) VALUE
                 "ÅñÅñÅ@ì˙éüåJâzèàóùÅ@ÅñÅñ".
           02  FILLER  PIC X(24) VALUE
                 "ì˙  ït   '  îN   åé   ì˙".
           02  FILLER  PIC X(13) VALUE
                 "ämîF      ( )".
       01  HIZUKE.
           02  ACP-YY1  PIC 9(02).
           02  ACP-MM1  PIC 9(02).
           02  ACP-DD1  PIC 9(02).
       01  ACP-KAKU.
           02       01ACP-KAKU  PIC X(01).
       01  DSP-HIZ.
           02           FILLER  PIC 9(02).
           02           FILLER  PIC Z9 .
           02           FILLER  PIC Z9 .
       COPY LSERR.
      *================================================================*
       PROCEDURE                      DIVISION.
      *================================================================*
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-GAMEN
       CALL "SD_Init" USING 
            "DSP-GAMEN" " " "0" "0" "73" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-GAMEN" "X" "1" "0" "12" " " "DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-GAMEN" "N" "1" "20" "24" "01DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-GAMEN" "X" "5" "20" "24" "02DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-GAMEN" "X" "24" "40" "13" "03DSP-GAMEN" " "
            RETURNING RESU.
      *HIZUKE
       CALL "SD_Init" USING 
            "HIZUKE" " " "5" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-YY1" "9" "5" "30" "2" " " "HIZUKE" RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-YY1" BY REFERENCE YY1-W "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-MM1" "9" "5" "35" "2" "ACP-YY1" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-MM1" BY REFERENCE MM1-W "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-DD1" "9" "5" "40" "2" "ACP-MM1" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-DD1" BY REFERENCE DD1-W "2" "0" RETURNING RESU.
      *ACP-KAKU
       CALL "SD_Init" USING 
            "ACP-KAKU" " " "24" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-KAKU" "X" "24" "51" "1" " " "ACP-KAKU" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-KAKU" BY REFERENCE KAKU-W "1" "0" RETURNING RESU.
      *DSP-HIZ
       CALL "SD_Init" USING 
            "DSP-HIZ" " " "5" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-HIZ" "9" "5" "30" "2" " " "DSP-HIZ" RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-HIZ" BY REFERENCE YY1-W "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "02DSP-HIZ" "Z9" "5" "35" "2" "01DSP-HIZ" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-HIZ" BY REFERENCE MM1-W "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "03DSP-HIZ" "Z9" "5" "40" "2" "02DSP-HIZ" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-HIZ" BY REFERENCE DD1-W "2" "0" RETURNING RESU.
      *
           COPY LSERR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MAIN-RTN.
           PERFORM  INIT-RTN  THRU  INIT-RTN-EXIT.
           IF  END-SW       =  9
               CALL "DB_Close"
               STOP  RUN
           END-IF
           PERFORM  SLCT-RTN  THRU  SLCT-RTN-EXIT.
           IF  END-STS      =  "P9"
               CALL "DB_Close"
               STOP  RUN
           END-IF
           PERFORM  OPEN-RTN  THRU  OPEN-RTN-EXIT.
           PERFORM  PROC-RTN  THRU  PROC-RTN-EXIT
               UNTIL  END-SW = 9.
           PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT.
       MAIN-END.
           CALL "DB_Close".
           STOP  RUN.
       COPY LPERR.
      *================================================================*
      *    èâä˙ê›íËèàóùÅiÇhÇmÇhÇsÅ|ÇqÇsÇmÅj                            *
      *================================================================*
       INIT-RTN.
           MOVE  ZERO      TO  END-SW.
           MOVE  ZERO      TO  JNIF1-OLD.
           INITIALIZE          WORK-AREA.
       INIT-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÉÅÉCÉìèàóùÅiÇoÇqÇnÇbÅ|ÇqÇsÇmÅj                              *
      *================================================================*
       PROC-RTN.
           PERFORM JSTR-RTN THRU JSTR-RTN-EXIT.
           PERFORM JNIF-RTN THRU JNIF-RTN-EXIT.
           PERFORM OKJF-RTN THRU OKJF-RTN-EXIT.
           PERFORM TDNW-RTN THRU TDNW-RTN-EXIT.
           PERFORM TDNN-RTN THRU TDNN-RTN-EXIT.
           PERFORM TDI-RTN  THRU TDI-RTN-EXIT.
           PERFORM TDNA-RTN THRU TDNA-RTN-EXIT.
       PROC-RTN-EXIT.
           EXIT.
      *================================================================*
      *    èoâ◊éwê}ÉgÉâÉìèàóùÅiÇiÇrÇsÇqÅ|ÇqÇsÇmÅj                      *
      *================================================================*
       JSTR-RTN.
      *           READ JSTR    NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO END-SW
               GO TO JSTR-RTN-EXIT
           END-IF
           IF  JSTR-17  NOT = 1
               GO TO JSTR-RTN
           END-IF
           MOVE JSTR-05     TO HIZ2-W.
           IF  HIZ1-W       <  HIZ21-W
               GO TO JSTR-RTN
           END-IF
      *           DELETE  JSTR    INVALID
      *///////////////
           CALL "DB_Delete" USING JSTR_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE "JSTR"      TO ERR-F
               MOVE "D"         TO ERR-M
               MOVE JSTR-KEY    TO ERR-K
               PERFORM ERR-RTN THRU ERR-EX
               MOVE 9           TO END-SW
               GO               TO JSTR-RTN-EXIT
           END-IF
           GO TO JSTR-RTN.
       JSTR-RTN-EXIT.
           EXIT.
      *================================================================*
      *    â◊éDÉgÉâÉìèàóùÅiÇiÇmÇhÇeÅ|ÇqÇsÇmÅj                          *
      *================================================================*
       JNIF-RTN.
      *           READ JNIF    NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JNIF_PNAME1 BY REFERENCE JNIF-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO END-SW
               GO TO JNIF-RTN-EXIT
           END-IF
           IF  JNIF1-01     = JNIF1-OLD
               GO TO JNIF-010
           END-IF
           IF  JNIF1-02     = 7
               GO TO JNIF-RTN
           END-IF
           IF  JNIF1-10 NOT = 1
               GO TO JNIF-RTN
           END-IF
           IF  JNIF1-02 NOT = 1
               GO TO JNIF-RTN
           END-IF
           IF  HIZ1-W       <  JNIF1-04
               GO TO JNIF-RTN
           END-IF.
       JNIF-010.
      *           DELETE  JNIF    INVALID
      *///////////////
           CALL "DB_Delete" USING JNIF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE "JNIF"      TO ERR-F
               MOVE "D"         TO ERR-M
               MOVE JNIF1-KEY   TO ERR-K
               PERFORM ERR-RTN THRU ERR-EX
               MOVE 9           TO END-SW
               GO               TO JNIF-RTN-EXIT
           END-IF
           MOVE JNIF1-01    TO JNIF1-OLD.
           GO TO JNIF-RTN.
       JNIF-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ëóÇËèÛÇeèàóùÅiÇnÇjÇiÇeÅ|ÇqÇsÇmÅj                            *
      *================================================================*
       OKJF-RTN.
      *           READ OKJF    NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" OKJF_PNAME1 BY REFERENCE OKJF-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO END-SW
               GO TO OKJF-RTN-EXIT
           END-IF
           IF  OKJF-08 NOT = 1
               GO TO OKJF-RTN
           END-IF
           IF  HIZ1-W       <  OKJF-03
               GO TO OKJF-RTN
           END-IF
      *           DELETE  OKJF    INVALID
      *///////////////
           CALL "DB_Delete" USING OKJF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE "OKJF"      TO ERR-F
               MOVE "D"         TO ERR-M
               MOVE OKJF-KEY    TO ERR-K
               PERFORM ERR-RTN THRU ERR-EX
               MOVE 9           TO END-SW
               GO               TO OKJF-RTN-EXIT
           END-IF
           GO TO OKJF-RTN.
       OKJF-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ìùàÍì`ï[ÇeÅiÉèÅ[ÉNÉ}ÉìÅjèàóùÅiÇsÇcÇmÇvÅ|ÇqÇsÇmÅj            *
      *================================================================*
       TDNW-RTN.
      *           READ TDNWF   NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" TDNWF_PNAME1 BY REFERENCE TDNW-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO END-SW
               GO TO TDNW-RTN-EXIT
           END-IF
           IF  TDNW1-STC     = TDNW-OSTC
               IF  TDNW1-DNO     = TDNW-ODNO
                   GO TO TDNW-010
               END-IF
           END-IF
           IF  TDNW1-DGN NOT = ZERO
               GO TO TDNW-RTN
           END-IF
           IF  TDNW1-PC  NOT = 9
               GO TO TDNW-RTN
           END-IF
           IF  HIZ1-W        < TDNW1-HNGP
               GO TO TDNW-RTN
           END-IF.
       TDNW-010.
           MOVE TDNW1-STC   TO TDNW-OSTC.
           MOVE TDNW1-DNO   TO TDNW-ODNO.
      *           DELETE  TDNWF   INVALID
      *///////////////
           CALL "DB_Delete" USING TDNWF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE "TDNWF"     TO ERR-F
               MOVE "D"         TO ERR-M
               MOVE TDNW1-KEY   TO ERR-K
               PERFORM ERR-RTN THRU ERR-EX
               MOVE 9           TO END-SW
               GO               TO TDNW-RTN-EXIT
           END-IF
           GO TO TDNW-RTN.
       TDNW-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ìùàÍì`ï[ÇeÅiÉiÉtÉRÅjèàóùÅ@Å@ÅiÇsÇcÇmÇmÅ|ÇqÇsÇmÅj            *
      *================================================================*
       TDNN-RTN.
      *           READ TDNNF   NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO END-SW
               GO TO TDNN-RTN-EXIT
           END-IF
           IF  TDNN1-STC     = TDNN-OSTC
               IF  TDNN1-DNO     = TDNN-ODNO
                   GO TO TDNN-010
               END-IF
           END-IF
           IF  TDNN1-DGN NOT = ZERO
               GO TO TDNN-RTN
           END-IF
           IF  TDNN1-PC  NOT = 9
               GO TO TDNN-RTN
           END-IF
           IF  HIZ1-W        < TDNN1-HNGP
               GO TO TDNN-RTN
           END-IF.
       TDNN-010.
           MOVE TDNN1-STC   TO TDNN-OSTC.
           MOVE TDNN1-DNO   TO TDNN-ODNO.
      *           DELETE  TDNNF   INVALID
      *///////////////
           CALL "DB_Delete" USING TDNNF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE "TDNNF"     TO ERR-F
               MOVE "D"         TO ERR-M
               MOVE TDNN1-KEY   TO ERR-K
               PERFORM ERR-RTN THRU ERR-EX
               MOVE 9           TO END-SW
               GO               TO TDNN-RTN-EXIT
           END-IF
           GO TO TDNN-RTN.
       TDNN-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ìùàÍì`ï[ÇeÅiÉgÉâÉXÉRëºì¸óÕï™ÅjèàóùÅiÇsÇcÇhÇeÅ|ÇqÇsÇmÅj      *
      *================================================================*
       TDI-RTN.
      *           READ TDIF     NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" TDIF_PNAME1 BY REFERENCE TDI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO END-SW
               GO TO TDI-RTN-EXIT
           END-IF
           IF  TDI-PRC NOT = 9
               GO TO TDI-RTN
           END-IF
           IF  HIZ1-W        < TDI-DATE
               GO TO TDI-RTN
           END-IF
      *           DELETE  TDIF     INVALID
      *///////////////
           CALL "DB_Delete" USING TDIF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE "TDIF"      TO ERR-F
               MOVE "D"         TO ERR-M
               MOVE TDI-KEY     TO ERR-K
               PERFORM ERR-RTN THRU ERR-EX
               MOVE 9           TO END-SW
               GO               TO TDI-RTN-EXIT
           END-IF
           GO TO TDI-RTN.
       TDI-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ìùàÍì`ï[ÇeÅiê‘ÇøÇ·ÇÒñ{ï‹ÅjèàóùÅ@Å@ÅiÇsÇcÇmÇ`Å|ÇqÇsÇmÅj      *
      *================================================================*
       TDNA-RTN.
      *           READ TDNAF   NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO END-SW
               GO TO TDNA-RTN-EXIT
           END-IF
           IF  TDNA-STC      = TDNA-OSTC
               IF  TDNA-DNO      = TDNA-ODNO
                   GO TO TDNA-010
               END-IF
           END-IF
           IF  TDNA-DGN  NOT = 01
               GO TO TDNA-RTN
           END-IF
           IF  TDNA-PC   NOT = 9
               GO TO TDNA-RTN
           END-IF
           MOVE TDNA-DNGP   TO HIZ2-W.
           IF  HIZ1-W        < HIZ21-W
               GO TO TDNA-RTN
           END-IF.
       TDNA-010.
           MOVE TDNA-STC    TO TDNA-OSTC.
           MOVE TDNA-DNO    TO TDNA-ODNO.
      *           DELETE  TDNAF   INVALID
      *///////////////
           CALL "DB_Delete" USING TDNAF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE "TDNAF"     TO ERR-F
               MOVE "D"         TO ERR-M
               MOVE TDNA-KEY    TO ERR-K
               PERFORM ERR-RTN THRU ERR-EX
               MOVE 9           TO END-SW
               GO               TO TDNA-RTN-EXIT
           END-IF
           GO TO TDNA-RTN.
       TDNA-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ëIëèàóùÅiÇrÇkÇbÇsÅ|ÇqÇsÇmÅj                                *
      *================================================================*
       SLCT-RTN.
           CALL "SD_Output" USING
            "DSP-GAMEN" DSP-GAMEN "p" RETURNING RESU.
           ACCEPT SYS-DATE FROM DATE.
           MOVE SYS-DATE    TO HIZ1-W.
           CALL "SD_Output" USING "DSP-HIZ" DSP-HIZ "p" RETURNING RESU.
       SLCT-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-YY1 "ACP-YY1" "9" "2"
            BY REFERENCE END-STS RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  END-STS      =  "P9"
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  YY1-W    NOT =  ZERO
               GO  TO  SLCT-020
           END-IF
           MOVE SYS-DATE    TO HIZ1-W.
           GO  TO  SLCT-040.
       SLCT-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-MM1 "ACP-MM1" "9" "2"
            BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS      =  "09"
               GO  TO  SLCT-010
           END-IF
           IF  MM1-W        <  1  OR  >  12
               GO  TO  SLCT-020
           END-IF.
       SLCT-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-DD1 "ACP-DD1" "9" "2"
            BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS      =  "09"
               GO  TO  SLCT-020
           END-IF
           IF  END-STS  NOT =  "01"
               GO  TO  SLCT-030
           END-IF
           IF  DD1-W        <  1  OR  >  31
               GO  TO  SLCT-030
           END-IF.
       SLCT-040.
           CALL "SD_Output" USING "DSP-HIZ" DSP-HIZ "p" RETURNING RESU.
       SLCT-050.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-KAKU "01ACP-KAKU"
            "X" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS      =  "09"
               GO  TO  SLCT-030
           END-IF
           IF  END-STS      =  "P9"
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS  NOT =  "01"
               GO  TO  SLCT-050
           END-IF.
       SLCT-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÇnÇoÇdÇmèàóùÅiÇnÇoÇdÇmÅ|ÇqÇsÇmÅj                            *
      *================================================================*
       OPEN-RTN.
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
      *================================================================*
      *    ÇbÇkÇnÇrÇdèàóùÅiÇbÇkÇrÇdÅ|ÇqÇsÇmÅj                          *
      *================================================================*
       CLSE-RTN.
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
      *
       CLSE-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÇdÇmÇcèàóùÅiÇdÇmÇcÅ|ÇqÇsÇmÅj                                *
      *================================================================*
       END-RTN.
           PERFORM CLSE-RTN THRU CLSE-RTN-EXIT.
       END-EX.
           EXIT.

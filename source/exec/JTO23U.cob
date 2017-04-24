       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JTO23U.
      *================================================================*
      *    ÇnÅ^ÇkÉfÅ[É^ì˙éüåJâz   Å@ÉvÉçÉOÉâÉÄ                         *
      *            ÇWÇXÅ^Å@ÇWÅ^  ÇX   ÇaÇxÅ@ÇrÅDÇrÇ`ÇjÇhÇxÇ`ÇlÇ`       *
      *================================================================*
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM3100.
       OBJECT-COMPUTER.               SYSTEM3100.
       INPUT-OUTPUT                   SECTION.
       DATA                           DIVISION.
       WORKING-STORAGE                SECTION.
      *
       77  END-SW                 PIC 9(1).
       77  ERR-STAT               PIC X(2).
       77  W-FILE                 PIC X(13).
       01  W-DATA.
           02  W-NGP              PIC 9(8).
           02  W-NGPL  REDEFINES W-NGP.
               03  W-NEN1         PIC 9(2).
               03  W-NGPS.
                   04  W-NEN2     PIC 9(2).
                   04  W-GET      PIC 9(2).
                   04  W-PEY      PIC 9(2).
       COPY LWMSG.
      *
           COPY  L-JOJF.
           COPY  L-JOSR.
           COPY  L-JSJD.
      *FD  JSJDRF
       01  JSJDRF_JTO23U.
           02  JSJDRF_PNAME1      PIC  X(006) VALUE "JSJDRF".
           02  F                  PIC  X(001).
           02  JSJDRF_LNAME       PIC  X(013) VALUE "JSJDRF_JTO23U".
           02  F                  PIC  X(001).
           02  JSJDRF_KEY1        PIC  X(100) VALUE SPACE.
           02  JSJDRF_SORT        PIC  X(100) VALUE SPACE.
           02  JSJDRF_IDLST       PIC  X(100) VALUE SPACE.
           02  JSJDRF_RES         USAGE  POINTER.
       01  JSJDR-R.
           02  F                  PIC X(244).
           02  JSJDR-NGP          PIC 9(08).
           02  F                  PIC X(04).
       77  F                      PIC X(01).
      *
      *================================================================*
      *    ÉXÉNÉäÅ[ÉìÅ@ÉZÉNÉVÉáÉì                                      *
      *================================================================*
      *
       77  END-STS            PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-GAMEN.
           02  FILLER  PIC X(12) VALUE "CLEAR SCREEN".
           02  FILLER  PIC N(16) VALUE
               "ÅñÅñÅ@ÇnÅ^ÇkÉfÅ[É^ì˙éüåJâzÅ@ÅñÅñ".
       01  DSP-ERR1.
           02  FILLER  PIC N(15) VALUE
               "ÇiÇrÇiÇcÇqÇeÅ@ÇvÇqÇhÇsÇdÉGÉâÅ[".
       COPY LSERR.
       COPY LSSEM.
      *================================================================*
       PROCEDURE                      DIVISION.
      *================================================================*
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-GAMEN
       CALL "SD_Init" USING 
            "DSP-GAMEN" " " "0" "0" "44" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-GAMEN" "X" "1" "0" "12" " " "DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-GAMEN" "N" "1" "20" "32" "01DSP-GAMEN" " "
            RETURNING RESU.
      *DSP-ERR1
       CALL "SD_Init" USING 
            "DSP-ERR1" " " "0" "0" "30" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-ERR1" "N" "24" "15" "30" " " "DSP-ERR1"
            RETURNING RESU.
      *
           COPY LSERR_P.
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       MAIN-RTN.
           PERFORM  INIT-RTN  THRU  INIT-RTN-EXIT.
           PERFORM  SLCT-RTN  THRU  SLCT-RTN-EXIT.
           PERFORM  OPEN-RTN  THRU  OPEN-RTN-EXIT.
           PERFORM  PROC-RTN  THRU  PROC-RTN-EXIT
                    UNTIL  END-SW = 9.
           PERFORM  JSJD-RTN  THRU  JSJD-EX.
           PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT.
       MAIN-END.
           CALL "DB_Close".
           STOP  RUN.
       COPY LPERR.
      *================================================================*
      *    èâä˙ê›íËèàóùÅiÇhÇmÇhÇsÅ|ÇqÇsÇmÅj                            *
      *================================================================*
       INIT-RTN.
           MOVE  0         TO  END-SW.
           MOVE  ZERO      TO  W-NGP.
           ACCEPT  W-NGPS  FROM  DATE.
           MOVE  20        TO  W-NEN1.
       INIT-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÉÅÉCÉìèàóùÅiÇoÇqÇnÇbÅ|ÇqÇsÇmÅj                              *
      *================================================================*
       PROC-RTN.
      *ÇnÅ^ÇkèÛãµÇeÅ@ÇqÇdÇ`Çc
      *           READ JOJF    NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JOJF_PNAME1 BY REFERENCE JOJF-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO END-SW
               GO TO PROC-RTN-EXIT
           END-IF
           IF  JOJF-061     = ZERO  AND  6
               GO TO PROC-RTN
           END-IF
      *           DELETE  JOJF    INVALID
      *///////////////
           CALL "DB_Delete" USING JOJF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE "JOJF"      TO ERR-F
               MOVE "D"         TO ERR-M
               MOVE JOJF-KEY    TO ERR-K
               PERFORM ERR-RTN THRU ERR-EX
               MOVE 9           TO END-SW
               GO               TO PROC-RTN-EXIT
           END-IF
           GO TO PROC-RTN.
       PROC-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ëIëèàóùÅiÇrÇkÇbÇsÅ|ÇqÇsÇmÅj                                *
      *================================================================*
       SLCT-RTN.
           CALL "SD_Output" USING
            "DSP-GAMEN" DSP-GAMEN "p" RETURNING RESU.
       SLCT-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÇnÇoÇdÇmèàóùÅiÇnÇoÇdÇmÅ|ÇqÇsÇmÅj                            *
      *================================================================*
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "I-O" JOJF_PNAME1 "PROTECTED" BY REFERENCE JOJF_IDLST "1"
            "JOJF-KEY" BY REFERENCE JOJF-KEY.
           CALL "DB_F_Open" USING
            "I-O" JSJD_PNAME1 "PROTECTED" BY REFERENCE JSJD_IDLST "2"
            "JSJD-KEY" BY REFERENCE JSJD-KEY "JSJD-KEY2" BY REFERENCE
            JSJD-KEY2.
           CALL "DB_F_Open" USING
            "OUTPUT" JOLSR_PNAME1 " " BY REFERENCE JOLSR_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" JSJDRF_PNAME1 " " BY REFERENCE JSJDRF_IDLST "0".
       OPEN-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÇbÇkÇnÇrÇdèàóùÅiÇbÇkÇrÇdÅ|ÇqÇsÇmÅj                          *
      *================================================================*
       CLSE-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JOJF_IDLST JOJF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JOLSR_IDLST JOLSR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JSJD_IDLST JSJD_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSJDRF_IDLST JSJDRF_PNAME1.
       CLSE-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ÇdÇmÇcèàóùÅiÇdÇmÇcÅ|ÇqÇsÇmÅj                                *
      *================================================================*
       END-RTN.
           PERFORM CLSE-RTN THRU CLSE-RTN-EXIT.
       END-EX.
           EXIT.
      *================================================================*
      *    èoâ◊é¿ê—ÉtÉ@ÉCÉãÅ@çXêVÉãÅ[É`Éì         (ADD : 90.04.10)     *
      *================================================================*
       JSJD-RTN.
      *           READ  JSJD  NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSJD_PNAME1 BY REFERENCE JSJD-REC " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  JSJD-100
           END-IF
           IF  JSJD-17  =  1   AND   JSJD-158    =  1
               GO  TO  JSJD-010
           END-IF
           GO  TO    JSJD-RTN.
       JSJD-010.
           MOVE  SPACE     TO  JSJDR-R.
           MOVE  JSJD-REC  TO  JSJDR-R.
           MOVE  W-NGP     TO  JSJDR-NGP.
      *           WRITE   JSJDR-R.
      *//////////////
           CALL "DB_Insert" USING
            JSJDRF_PNAME1 JSJDRF_LNAME JSJDR-R RETURNING RET.
           IF  ERR-STAT     =  "00"
               GO TO JSJD-020
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-ERR1" DSP-ERR1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT =  "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9           TO END-SW
               GO               TO JSJD-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE JSJDRF_IDLST JSJDRF_PNAME1.
           MOVE "JSJDRF       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" JSJDRF_PNAME1 " " BY REFERENCE JSJDRF_IDLST "0".
           GO TO JSJD-010.
       JSJD-020.
      *           DELETE  JSJD   INVALID
      *///////////////
           CALL "DB_Delete" USING JSJD_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE "JSJD"      TO ERR-F
               MOVE "D"         TO ERR-M
               MOVE JSJD-KEY    TO ERR-K
               PERFORM ERR-RTN THRU ERR-EX
               GO  TO  JSJD-EX
           END-IF
           GO  TO  JSJD-RTN.
       JSJD-100.
           CALL "DB_F_Close" USING
            BY REFERENCE JSJDRF_IDLST JSJDRF_PNAME1.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET    =  ZERO
               MOVE  12          TO  W-GET
               SUBTRACT   1    FROM  W-NEN2
           END-IF
           CALL "DB_F_Open" USING
            "I-O" JSJDRF_PNAME1 " " BY REFERENCE JSJDRF_IDLST "0".
       JSJD-110.
      *           READ  JSJDRF      AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSJDRF_PNAME1 BY REFERENCE JSJDR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  JSJD-EX
           END-IF
           IF  JSJDR-NGP   <   W-NGP
               MOVE X"FF"     TO  JSJDR-R
      *               REWRITE  JSJDR-R.
      *///////////////
               CALL "DB_Update" USING
                JSJDRF_PNAME1 JSJDRF_LNAME JSJDR-R RETURNING RET
           END-IF
           GO  TO  JSJD-110.
       JSJD-EX.
           EXIT.

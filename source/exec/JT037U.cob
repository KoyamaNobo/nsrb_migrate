       IDENTIFICATION                   DIVISION.
       PROGRAM-ID.                      JT037U.
      **************************************************
      **************************************************
      **                                              **
      **   ëóÅ@ÇËÅ@èÛÅ@Å@é¿Å@ê—Å@çX  êV               **
      **                                              **
      **        USER  NAME : ì˙êiÉSÉÄ.                **
      **        DATE       : 1988•09•22               **
      **        TYPE       : COBOL                    **
      **        PROGRAM-ID : JT037U                   **
      **        SCREEN-ID  : ------.                  **
      **        AUTHOR     : HAJIME  MIZUNO           **
      **************************************************
      **************************************************
       ENVIRONMENT                      DIVISION.
       CONFIGURATION                    SECTION.
       SOURCE-COMPUTER.                 SYSTEM150.
       OBJECT-COMPUTER.                 SYSTEM150.
      ******************************************************************
      *                 DATA              DIVISION                     *
      ******************************************************************
       DATA                             DIVISION.
      ******************************************************************
      *            WORKING     STORAGE     SECTION                     *
      ******************************************************************
       WORKING-STORAGE SECTION.
      *
       77  ERR-STAT               PIC  X(02).
       01  JS-SIGN                PIC  9(01).
       01  WORK-AREA.
           03  ENDFLG             PIC  X(03).
           03  LCNT               PIC     99.
           03  DDWK               PIC  9(06).
           03  SIZEX              PIC  9(01).
           03  I                  PIC  9(02).
           03  SW                 PIC  9.
           03  TOTAL              PIC S9(07).
           03  KEY-WORK.
               05  NEW-01         PIC  9(06).
               05  OLD-01         PIC  9(06).
           03  M44                PIC  9(06).
      *
      *
           COPY     LWMSG.
      *
           COPY     L-JSTR.
           COPY     L-JCON.
           COPY     LOKJF.
      *
      ******************************************************************
      *            SCREEN                  SECTION                     *
      ******************************************************************
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
      *************************
      *    DISPLAY SECTION    *
      *************************
       01  DSIP-AREA.
           03  CRT-CLR PIC  X(12) VALUE "CLEAR SCREEN".
           03  CRT-01.
               05  FILLER  PIC  X(12) VALUE  "ëóèÛé¿ê—çXêV".
           03  MSG-04.
               05  FILLER  PIC  N(12) VALUE
                                   "ëóÇËèÛÉtÉ@ÉCÉãÅ@ÉfÅ[É^ñ≥".
               05  FILLER  PIC  X(01)  VALUE     " ".
               05  FILLER  PIC  X(04)  VALUE     "KEY=".
               05  FILLER  PIC  9(06).
       01  DSP.
           03  DSP-01 PIC N(05) VALUE "Åiã≥Å@àÁÅj".
           03  DSP-02 PIC N(05) VALUE "ÅiàÍÅ@î Åj".
           03  DSP-05 PIC N(11) VALUE
               "ÉRÉìÉgÉçÅ[ÉãÇeÅ@ñ¢ìoò^".
           COPY     LSMSG.
      ******************************************************************
      *                 PROCEDURE         DIVISION                     *
      ******************************************************************
       PROCEDURE        DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSIP-AREA
       CALL "SD_Init" USING 
            "DSIP-AREA" " " "0" "0" "59" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CRT-CLR" "X" "1" "0" "12" " " "DSIP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "CRT-01" " " "1" "0" "12" "CRT-CLR" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CRT-01" "RX" "1" "30" "12" " " "CRT-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "MSG-04" " " "24" "0" "35" "CRT-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01MSG-04" "N" "24" "1" "24" " " "MSG-04" RETURNING RESU.
       CALL "SD_Init" USING 
            "02MSG-04" "X" "24" "25" "1" "01MSG-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03MSG-04" "X" "24" "26" "4" "02MSG-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04MSG-04" "9" "24" "30" "6" "03MSG-04" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04MSG-04" BY REFERENCE M44 "6" "0" RETURNING RESU.
      *DSP
       CALL "SD_Init" USING 
            "DSP" " " "0" "0" "42" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" "N" "1" "43" "10" " " "DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-02" "N" "1" "43" "10" "DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-05" "N" "24" "1" "22" "DSP-02" " " RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       PROGRM-START.
           PERFORM   INITIAL-RTN   THRU   INITIAL-EXT.
       PROGRAM-MAIN.
      *           READ         JSTR       NEXT       AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO      TO    PROGRAM-ENDING
           END-IF
      **
           IF  (JSTR-03         = 0 OR 7   )    AND
               (JSTR-05         =      ZERO)    AND
               (JSTR-04S        =      DDWK)    AND
               (JSTR-16         = JS-SIGN  )    AND
               (JSTR-17         =      9)       AND
               (JSTR-4012  NOT  =      0)
               GO  TO  PROGRAM-010
           END-IF
           IF  (JSTR-03         =      3)       AND
               (JSTR-16         = JS-SIGN  )    AND
               (JSTR-17         =      9)       AND
               (JSTR-4012  NOT  =      0)
               GO  TO  PROGRAM-010
           END-IF
           GO  TO  PROGRAM-MAIN.
       PROGRAM-010.
           IF  JSTR-03  =  0 OR 7
               MOVE  JSTR-11  TO  JSTR-12
           END-IF.
       PROGRAM-015.
           PERFORM      OKJF-RTN   THRU       OKJF-EXT.
       PROGRAM-020.
           PERFORM      JSTRM-RTN  THRU       JSTRM-EXT.
           GO   TO      PROGRAM-MAIN.
       PROGRAM-ENDING.
           PERFORM      JCON-RTN   THRU       JCON-EXT.
           PERFORM       END-RTN   THRU         END-EX.
           CALL "DB_Close".
           STOP     RUN.
      ******************************************************************
      *    INITIAL            èâä˙èàóù
      ******************************************************************
       INITIAL-RTN.
           ACCEPT   JS-SIGN FROM ARGUMENT-VALUE.
           CALL "SD_Output" USING "CRT-CLR" CRT-CLR "p" RETURNING RESU.
           CALL "SD_Output" USING "CRT-01" CRT-01 "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Output" USING "DSP-01" DSP-01 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "DSP-02" DSP-02 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "I-O" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
           MOVE     SPACE  TO    WORK-AREA.
           INITIALIZE            WORK-AREA.
           MOVE     SPACE  TO     KEY-WORK.
      *
           MOVE     SPACE  TO    JCON7-KEY.
           MOVE     7      TO    JCON7-01.
      *           READ     JCON         UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               PERFORM      END-RTN   THRU   END-EX
               CALL "SD_Output" USING
                "DSP-05" DSP-05 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "DB_Close"
               STOP         RUN
           END-IF
           IF  JS-SIGN  =  0
               MOVE  JCON7-05  TO  DDWK
           ELSE
               MOVE  JCON7-07  TO  DDWK
           END-IF.
       INITIAL-EXT.
           EXIT.
      ******************************************************************
      *    END-RTN            èIóπèàóù
      ******************************************************************
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
       END-EX.
           EXIT.
      ******************************************************************
      *    ÇiÇrÇsÇqÇlÅ|ÇqÇsÇmÅD
      ******************************************************************
       JSTRM-RTN.
           IF  JSTR-03  =  0 OR 7
               MOVE  JSTR-04  TO  JSTR-05
               MOVE  JSTR-11  TO  JSTR-12
           END-IF
           MOVE  9  TO  JSTR-17.
       JSTRM-03.
      *           REWRITE   JSTR-R            INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE     "JSTR"     TO  ERR-F
               MOVE     "R"         TO  ERR-M
               MOVE     JSTR-KEY    TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           MOVE      NEW-01   TO   OLD-01.
       JSTRM-EXT.
           EXIT.
      ******************************************************************
      *    ÇnÇjÇiÇeÅ|ÇqÇsÇmÅD
      ******************************************************************
       OKJF-RTN.
           IF  JSTR-03  NOT =  0 AND 7
               GO  TO  OKJF-EXT
           END-IF
           IF  JSTR-16  NOT =  JS-SIGN
               GO  TO  OKJF-EXT
           END-IF
           IF  JSTR-14      =  9
               GO  TO  OKJF-EXT
           END-IF
           MOVE  JSTR-14B TO  OKJF-01.
      *           READ     OKJF            INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" OKJF_PNAME1 BY REFERENCE OKJF-R " " RETURNING RET.
           IF  RET = 1
               MOVE  JSTR-14B  TO  M44
               CALL "SD_Output" USING
                "MSG-04" MSG-04 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               GO   TO    OKJF-EXT
           END-IF
      *
           IF  DDWK  =  OKJF-03
               MOVE    1   TO   OKJF-10
           ELSE
               GO     TO        OKJF-EXT
           END-IF
      *           REWRITE   OKJF-R            INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            OKJF_PNAME1 OKJF_LNAME OKJF-R RETURNING RET.
           IF  RET = 1
               MOVE     "OKJF"      TO  ERR-F
               MOVE     "R"         TO  ERR-M
               MOVE     OKJF-KEY    TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       OKJF-EXT.
           EXIT.
      *
      ******************************************************************
      *    ÉRÉìÉgÉçÅ[ÉãÇe                                   A:890904
      ******************************************************************
       JCON-RTN.
           MOVE     SPACE  TO    JCON7-KEY.
           MOVE     7      TO    JCON7-01.
      *           READ     JCON         INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "DSP-05" DSP-05 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               GO   TO   JCON-EXT
           END-IF
           IF  JS-SIGN  =  0
               MOVE  0  TO  JCON7-06
           ELSE
               MOVE  0  TO  JCON7-08
           END-IF
      *           REWRITE  JCON1-R      INVALID
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               MOVE   "JCON"     TO   ERR-F
               MOVE    JCON1-KEY TO   ERR-K
               MOVE   "R"        TO   ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       JCON-EXT.
           EXIT.
           COPY     LPMSG.
      *******************    E N D    O F    P R O G R A M    **********

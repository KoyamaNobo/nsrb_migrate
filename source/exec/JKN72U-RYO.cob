       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JK071U.
      *================================================================*
      *    o‰×ŽÀÑ‘—M‚e¶¬                                          *
      *                       ‚X‚O^@‚S^‚Q‚U   BY. IKUMI.N           *
      *    UPDATE  :  91/10/25                   BY. IKUMI.N           *
      *================================================================*
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM3100.
       OBJECT-COMPUTER.               SYSTEM3100.
       DATA                           DIVISION.
       WORKING-STORAGE                SECTION.
           COPY  LWMSG.
      *
       01  WORK-AREA.
           02  WORK-01.
               03  HIZ1-W             PIC  9(08).
               03  HIZ1-WR            REDEFINES  HIZ1-W.
                   04  YY1-W          PIC  9(04).
                   04  MM1-W          PIC  9(02).
                   04  DD1-W          PIC  9(02).
               03  HIZ1-WRL           REDEFINES  HIZ1-W.
                   04  F              PIC  9(02).
                   04  HIZ1-WRS       PIC  9(06).
               03  KAKU-W             PIC  X(01).
               03  ERR-CD             PIC  9(01).
               03  ERR-STAT           PIC  X(02).
               03  W-SNM              PIC  N(06).
               03  WYMD               PIC  9(08).
               03  WYMDR              REDEFINES  WYMD.
                   04  W-YY           PIC  9(04).
                   04  W-MMDD         PIC  9(04).
               03  WYMDRL             REDEFINES  WYMD.
                   04  F              PIC  9(02).
                   04  WYMDRS         PIC  9(06).
               03  W-DCHK             PIC  9(01).
               03  W-DNO              PIC  9(06).
               03  W-DCNT             PIC  9(05).
       01  STR-TMP.
           02  STR-01                 PIC  9(02).
           02  STR-02.
               03  STR-020            PIC  9(07).
               03  STR-021            PIC  X(243).
               03  STR-021A           PIC  X(01).
               03  STR-022            PIC  9(01).
               03  STR-023            PIC  9(01).
               03  STR-024            PIC  9(01).
       01  DEFINE-WORK.
           02  FIL-DF                 PIC  X(10).
           02    M-DF                 PIC  X(01)  OCCURS  4.
           02  KBN-DF                 PIC  9(02).
           02  HTAB                   PIC  X(02)  VALUE  "01".
           02  BSKIP                  PIC  X(02)  VALUE  "09".
           02  PF6                    PIC  X(02)  VALUE  "P6".
           02  PF9                    PIC  X(02)  VALUE  "P9".
       01  KEY-WORK.
           02  KEY-01                 PIC  9(02).
           02  KEY-02                 PIC  9(07).
      *
           COPY  L-JSTR-RYO.
           COPY  LJOLJF-RYO.
           COPY  L-JCON.
      *
       77  END-STS            PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLR-GMN.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  DSP-GMN.
           02  FILLER  PIC  N(13)  VALUE
               "–@o‰×ŽÀÑ‘—M‚e¶¬@–".
           02  FILLER  PIC  X(33)  VALUE
               "o‰×“ú      ”N  ŒŽ  “ú i¼—ïj".
           02  FILLER  PIC  X(09)  VALUE
               "‘—Mæ ".
           02  FILLER  PIC  X(13)  VALUE
               "Šm”F      ( )".
       01  HIZUKE.
           02  ACP-YY1   PIC  9(04).
           02  ACP-MM1   PIC  9(02).
           02  ACP-DD1   PIC  9(02).
       01  SOUSIN.
           02  ACP-SNM   PIC  N(06).
       01  SOUSIN2.
           02  DSP-HIZ.
               03        FILLER  PIC  9(04).
               03        FILLER  PIC  Z9 .
               03        FILLER  PIC  Z9 .
           02  CLR-HIZ.
               03        FILLER  PIC  X(04)  VALUE  " ".
               03        FILLER  PIC  X(02)  VALUE  " ".
               03        FILLER  PIC  X(02)  VALUE  " ".
       01  MESSEGE.
           02  DSP-MSG.
               03        FILLER  PIC  N(18)  VALUE
                   "–¢ˆ—ƒf[ƒ^—L‚èAˆ—‚ð’†’f‚µ‚Ü‚·B".
       01  MESSEGE1.
           02  DSP-MSG2.
               03        FILLER  PIC  N(12)  VALUE
                   "‘—Mƒf[ƒ^‚ª—L‚è‚Ü‚¹‚ñI".
       01  MESSEGE9.
           02  DSP-MSG9.
               03        FILLER  PIC  N(04)  VALUE
                   "“`•[–‡”".
               03        FILLER  PIC  ZZ,ZZ9 .
               03        FILLER  PIC  N(01)  VALUE
                   "–‡".
               03        FILLER  PIC  X(03)  VALUE
                   "ESC".
       01  ACP-KAKU.
           02        01ACP-KAKU  PIC  X(01).
           COPY  LSERR.
      *================================================================*
       PROCEDURE                      DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLR-GMN
       CALL "SD_Init" USING
           "CLR-GMN" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLR-GMN" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *DSP-GMN
       CALL "SD_Init" USING 
            "DSP-GMN" " " "0" "0" "81" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-GMN" "N" "1" "20" "26" " " "DSP-GMN" RETURNING RESU.
       CALL "SD_Init" USING 
           "02DSP-GMN" "X" "5" "18" "33" "01DSP-GMN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-GMN" "X" "7" "20" "9" "02DSP-GMN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-GMN" "X" "24" "40" "13" "03DSP-GMN" " "
            RETURNING RESU.
      *HIZUKE
       CALL "SD_Init" USING 
            "HIZUKE" " " "5" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-YY1" "9" "5" "28" "4" " " "HIZUKE" RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-YY1" BY REFERENCE YY1-W "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-MM1" "9" "5" "34" "2" "ACP-YY1" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-MM1" BY REFERENCE MM1-W "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-DD1" "9" "5" "38" "2" "ACP-MM1" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-DD1" BY REFERENCE DD1-W "2" "0" RETURNING RESU.
      *SOUSIN
       CALL "SD_Init" USING 
            "SOUSIN" " " "7" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SNM" "N" "7" "30" "12" " " "SOUSIN" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SNM" BY REFERENCE W-SNM "12" "0" RETURNING RESU.
      *SOUSIN2
       CALL "SD_Init" USING 
            "SOUSIN2" " " "5" "0" "16" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HIZ" " " "5" "0" "8" " " "SOUSIN2" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-HIZ" "9" "5" "28" "4" " " "DSP-HIZ" RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-HIZ" BY REFERENCE YY1-W "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "02DSP-HIZ" "Z9" "5" "34" "2" "01DSP-HIZ" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-HIZ" BY REFERENCE MM1-W "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "03DSP-HIZ" "Z9" "5" "38" "2" "02DSP-HIZ" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-HIZ" BY REFERENCE DD1-W "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-HIZ" " " "5" "0" "8" "DSP-HIZ" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-HIZ" "X" "5" "28" "4" " " "CLR-HIZ" RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLR-HIZ" "X" "5" "34" "2" "01CLR-HIZ" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLR-HIZ" "X" "5" "38" "2" "02CLR-HIZ" " " RETURNING RESU.
      *MESSEGE
       CALL "SD_Init" USING 
            "MESSEGE" " " "12" "0" "36" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MSG" " " "12" "0" "36" " " "MESSEGE" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MSG" "N" "12" "10" "36" " " "DSP-MSG" RETURNING RESU.
      *MESSEGE1
       CALL "SD_Init" USING 
            "MESSEGE1" " " "12" "0" "24" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MSG2" " " "12" "0" "24" " " "MESSEGE1" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MSG2" "N" "12" "10" "24" " " "DSP-MSG2"
            RETURNING RESU.
      *MESSEGE9
       CALL "SD_Init" USING 
            "MESSEGE9" " " "16" "0" "19" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MSG9" " " "16" "0" "19" " " "MESSEGE9" RETURNING RESU.
       CALL "SD_Init" USING 
           "01DSP-MSG9" "N" "16" "10" "8" " " "DSP-MSG9" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-MSG9" "ZZ,ZZ9" "16" "21" "6" "01DSP-MSG9" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-MSG9" BY REFERENCE W-DCNT "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-MSG9" "N" "16" "27" "2" "02DSP-MSG9" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-MSG9" "X" "16" "40" "3" "03DSP-MSG9" " "
            RETURNING RESU.
      *ACP-KAKU
       CALL "SD_Init" USING 
            "ACP-KAKU" " " "24" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-KAKU" "X" "24" "51" "1" " " "ACP-KAKU" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-KAKU" BY REFERENCE KAKU-W "1" "0" RETURNING RESU.
      *
           COPY LSERR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MAINLINE-ROUTINE.
           PERFORM  INIT-RTN  THRU  INIT-RTN-EXIT.
           PERFORM  SLCT-RTN  THRU  SLCT-RTN-EXIT.
           IF  END-STS     =  PF9
               CALL "DB_Close"
               STOP  RUN
           END-IF
           PERFORM  OPEN-RTN  THRU  OPEN-RTN-EXIT.
           PERFORM  PROC-RTN  THRU  PROC-RTN-EXIT.
           PERFORM  ENDR-RTN  THRU  ENDR-RTN-EXIT.
           PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT.
      *
       MAINLINE-END.
           CALL "DB_Close".
           STOP  RUN.
      *
       PROC-RTN.
           PERFORM  JSTR-RTN  THRU  JSTR-RTN-EXIT.
       PROC-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ‰ŠúÝ’èˆ—                                                *
      *================================================================*
       INIT-RTN.
           MOVE  SPACE     TO  KEY-WORK
                               STR-TMP.
           MOVE  ZERO      TO  ERR-CD.
           MOVE  10        TO  ERR-LIN.
           CALL "SD_Arg_Match_Line" USING
            "ERR-LIN" "2" ERR-LIN RETURNING RESU.
           MOVE  "JOLJF"   TO  FIL-DF.
           MOVE  "W"       TO  M-DF(1)  M-DF(4).
           MOVE  "R"       TO  M-DF(2).
           MOVE  "A"       TO  M-DF(3).
           MOVE  11        TO  KBN-DF.
       INIT-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ‚n‚o‚d‚mˆ—                                                *
      *================================================================*
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" JOLJF_PNAME1 " " BY REFERENCE JOLJF_IDLST "0".
       OPEN-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ‘I‘ðˆ—@@@@               (SLCT-RTN)                   *
      *================================================================*
       SLCT-RTN.
           CALL "SD_Output" USING "CLR-GMN" CLR-GMN "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-GMN" DSP-GMN "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" JOLJF_PNAME1 " " BY REFERENCE JOLJF_IDLST "0".
      *           READ  JOLJF      AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" JOLJF_PNAME1 BY REFERENCE JOLJF11-REC " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JOLJF_IDLST JOLJF_PNAME1
               GO TO  SLCT-005
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE JOLJF_IDLST JOLJF_PNAME1.
           CALL "SD_Output" USING "DSP-MSG" DSP-MSG "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  100.
           MOVE   PF9       TO    END-STS.
           GO  TO  SLCT-RTN-EXIT.
       SLCT-005.
           MOVE   ZERO      TO    WYMD.
           ACCEPT   WYMDRS  FROM  DATE.
           IF  W-YY        >  90
               ADD    1900    TO  W-YY
           ELSE
               ADD    2000    TO  W-YY
           END-IF
           MOVE   WYMD      TO    HIZ1-W.
           CALL "SD_Output" USING "DSP-HIZ" DSP-HIZ "p" RETURNING RESU.
           GO  TO  SLCT-040.
       SLCT-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-YY1 "ACP-YY1" "9" "4"
            BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS     =  PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  100
               GO TO  SLCT-RTN-EXIT
           END-IF
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  YY1-W   NOT =  ZERO
               IF  YY1-W   <  2009
                   GO TO  SLCT-010
               ELSE
                   GO TO  SLCT-020
               END-IF
           END-IF
           MOVE   WYMD      TO    HIZ1-W.
           GO TO  SLCT-040.
       SLCT-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-MM1 "ACP-MM1" "9" "2"
            BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS     =  BSKIP
               GO TO  SLCT-010
           END-IF.
       SLCT-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-DD1 "ACP-DD1" "9" "2"
            BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS     =  BSKIP
               GO TO  SLCT-020
           END-IF
           IF  END-STS NOT =  HTAB
               GO TO  SLCT-030
           END-IF
           IF  (MM1-W  <  1  OR  MM1-W  >  12)  OR
               (DD1-W  <  1  OR  DD1-W  >  31)
               CALL "SD_Output" USING
                "CLR-HIZ" CLR-HIZ "p" RETURNING RESU
               GO TO  SLCT-010
           END-IF
           IF  HIZ1-W        >  WYMD
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO    SLCT-010
           END-IF.
       SLCT-040.
           CALL "SD_Output" USING "DSP-HIZ" DSP-HIZ "p" RETURNING RESU.
       SLCT-045.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON4-KEY" BY REFERENCE JCON4-KEY.
           MOVE    4          TO  JCON4-01.
           MOVE    ZERO       TO  JCON4-02.
      *           READ    JCON       UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE    TO    JCON4-03
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           MOVE    JCON4-03   TO  W-SNM.
           CALL "SD_Output" USING "ACP-SNM" ACP-SNM "p" RETURNING RESU.
       SLCT-050.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-KAKU "01ACP-KAKU"
            "X" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS     =  PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  100
               GO TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS     =  BSKIP
               GO TO  SLCT-010
           END-IF
           IF  END-STS     =  PF6
               GO TO  SLCT-RTN
           END-IF
           IF  END-STS NOT =  HTAB
               GO TO  SLCT-050
           END-IF.
       SLCT-RTN-EXIT.
           EXIT.
      *================================================================*
      *    o‰×Žw}ƒgƒ‰ƒ“ˆ—             (JSTR-RTN)                   *
      *================================================================*
       JSTR-RTN.
           MOVE  ZERO      TO  W-DCNT  W-DNO.
           MOVE  KBN-DF    TO  STR-01.
       JSTR-010.
      *----/o‰×Žw}ƒgƒ‰ƒ“@‚q‚d‚`‚c/----*
      *           READ  JSTR   NEXT   AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO  JSTR-RTN-EXIT
           END-IF
           IF  JSTR-17      =  1
               GO TO  JSTR-010
           END-IF
           IF  JSTR-05      >  HIZ1-W
               GO TO  JSTR-010
           END-IF
           IF  JSTR-158 NOT =  1
               GO TO  JSTR-010
           END-IF
           MOVE  0         TO  W-DCHK.
           IF  JSTR-05      <  HIZ1-W
               MOVE  1         TO  W-DCHK
               MOVE  HIZ1-W    TO  JSTR-05
           END-IF
      *
           MOVE  JSTR-R    TO  STR-02.
           MOVE  JSTR-19   TO  STR-021A.
           MOVE  JSTR-158  TO  STR-022.
           MOVE  JSTR-16   TO  STR-023.
           MOVE  JSTR-17   TO  STR-024.
           MOVE  STR-TMP   TO  JOLJF11-REC.
      *----/‚n^‚k‘—M‚e@‚v‚q‚h‚s‚d/----*
      *           WRITE  JOLJF11-REC.
      *//////////////
           CALL "DB_Insert" USING
            JOLJF_PNAME1 JOLJF_LNAME JOLJF11-REC RETURNING RET.
           IF  ERR-STAT NOT =  "00"
               MOVE  KBN-DF     TO  KEY-01
               MOVE  STR-020    TO  KEY-02
               MOVE  04         TO  ERR-CD
               PERFORM  ESUB-RTN  THRU  ESUB-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  JSTR-01 NOT =  W-DNO
               ADD   1          TO  W-DCNT
               MOVE  JSTR-01    TO  W-DNO
           END-IF
           IF  W-DCHK      =  0
               GO  TO  JSTR-010
           END-IF.
       JSTR-999.
           MOVE  JSTR-KEY  TO  ERR-K.
      ***
      *           REWRITE    JSTR-R   INVALID
      *///////////////
           CALL "DB_Update" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE  "R"     TO  ERR-M
               MOVE  "JSTR"  TO  ERR-F
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           GO  TO  JSTR-010.
       JSTR-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ‚d‚q‚q‚n‚qˆ—                 (ESUB-RTN)                   *
      *================================================================*
       ESUB-RTN.
           MOVE  FIL-DF          TO  ERR-F.
           MOVE    M-DF(ERR-CD)  TO  ERR-M.
           IF  ERR-CD      = 4
               MOVE  KEY-WORK  TO  ERR-K
           ELSE
               MOVE  "0001"    TO  ERR-K
           END-IF
           PERFORM  ERR-RTN  THRU  ERR-EX.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "DB_F_Close" USING
            BY REFERENCE JOLJF_IDLST JOLJF_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" JOLJF_PNAME1 " " BY REFERENCE JOLJF_IDLST "0".
           PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT.
      *
       ESUB-RTN-EXIT.
           EXIT.
      *================================================================*
      *    ‚b‚k‚n‚r‚dˆ—                                              *
      *================================================================*
       CLSE-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JOLJF_IDLST JOLJF_PNAME1.
           IF  W-DCNT    =  ZERO
               CALL "SD_Output" USING
                "DSP-MSG2" DSP-MSG2 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING
            "DSP-MSG9" DSP-MSG9 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
       CLSE-RTN-EXIT.
           EXIT.
      *================================================================*
      *    I—¹ˆ—                                                    *
      *================================================================*
       ENDR-RTN.
       ENDR-RTN-EXIT.
           EXIT.
      *================================================================
           COPY  LPERR.

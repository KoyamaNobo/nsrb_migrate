       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 JTN00U.
      **************************************************************************
      *    PROGRAM  :  ‚i‚b‚n‚m@ŽóMXVE“ˆê“`•[“ü—Í‹æ•ª@XV              *
      *    JS-SIGN  :  ‘—MXVi“¡“cj=1 , i‘“‡j=2 , “`•[“ü—Í=5            *
      *             :  ŽóMXVi“¡“cj=6 , i‘“‡j=7                         *
      **************************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       SOURCE-COMPUTER.            SYSTEM3100.
       OBJECT-COMPUTER.            SYSTEM3100.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
       01  JS-SIGN                 PIC  9(01).
       01  ERR-STAT                PIC  X(02).
       01  W-FILE                  PIC  X(13).
       01  W-DATA.
           03  W-SEN               PIC  9(01).
           03  W-DMM               PIC  9(01).
           03  W-BS                PIC  N(04).
           03  W-TIM               PIC  9(08).
           03  W-TIMD  REDEFINES W-TIM.
             04  W-TIME            PIC  9(04).
             04  F                 PIC  9(04).
      *
           COPY    L-JCON.
      *FD  CHKF
       01  CHKF_JTN00U.
           02  CHKF_PNAME1  PIC  X(006) VALUE "T-CHKF".
           02  F            PIC  X(001).
           02  CHKF_LNAME   PIC  X(011) VALUE "CHKF_JTN00U".
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
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID1.
           03  FILLER.
             05  FILLER  PIC X(36) VALUE
                  "                                    ".
             05  FILLER  PIC N(17) VALUE
                 "‚i‚b‚n‚m@XV’†‹æ•ªi“¡“cj@XV".
           03  FILLER  PIC X(28) VALUE
                       "‰ð@œ=0 , XV’†=1  .....  ".
           03  FILLER  PIC N(15) VALUE
                 "ƒvƒƒOƒ‰ƒ€•úŠüE‹Æ–±•úŠü“™•s‰Â".
           03  FILLER  PIC N(11) VALUE
                 "ƒVƒXƒeƒ€‚ªŽ~‚Ü‚è‚Ü‚·B".
       01  C-MID2.
           03  FILLER.
             05  FILLER  PIC X(36) VALUE
                  "                                    ".
             05  FILLER  PIC N(17) VALUE
                 "‚i‚b‚n‚m@XV’†‹æ•ªi‘“‡j@XV".
           03  FILLER  PIC X(28) VALUE
                       "‰ð@œ=0 , XV’†=1  .....  ".
           03  FILLER  PIC N(15) VALUE
                 "ƒvƒƒOƒ‰ƒ€•úŠüE‹Æ–±•úŠü“™•s‰Â".
           03  FILLER  PIC N(11) VALUE
                 "ƒVƒXƒeƒ€‚ªŽ~‚Ü‚è‚Ü‚·B".
       01  C-MID5.
           03  FILLER.
             05  FILLER  PIC X(34) VALUE
                  "                                  ".
             05  FILLER  PIC N(16) VALUE
                 "‚i‚b‚n‚m@“ˆê“`•[“ü—Í‹æ•ª@XV".
           03  FILLER  PIC X(37) VALUE
                       "‰ð@œ=0 , “ü—Í’†=1 , •ÏŠ·’†=2  ...  ".
           03  FILLER  PIC N(15) VALUE
                 "ƒvƒƒOƒ‰ƒ€•úŠüE‹Æ–±•úŠü“™•s‰Â".
           03  FILLER  PIC N(11) VALUE
                 "ƒVƒXƒeƒ€‚ªŽ~‚Ü‚è‚Ü‚·B".
       01  C-MID9.
           03  FILLER  PIC X(22) VALUE
                       "Šm”F  OK=1 NO=9   ØÀ°Ý".
       01  C-ACP.
           03  A-SEN   PIC 9(1).
           03  A-DMM   PIC 9(1).
       01  C-DSP.
           02  D-BS    PIC  N(004).
           02  FILLER.
             03  D-MSG0   PIC  N(05) VALUE
                 "y‰ðœÏz".
             03  D-MSG1   PIC  N(07) VALUE
                 "yXVˆ—’†z".
             03  D-MSG2   PIC  N(07) VALUE
                 "y“ü—Íˆ—’†z".
             03  D-MSG3   PIC  N(07) VALUE
                 "y•ÏŠ·ˆ—’†z".
             03  D-MSG4   PIC  N(07) VALUE
                 "y“`‘—ˆ—’†z".
             03  D-MSG5   PIC  N(07) VALUE
                 "y¶¬ˆ—’†z".
             03  D-MSG9   PIC  N(06) VALUE
                 "I—¹F‚d‚“‚ƒ".
       01  DSP-ERR.
           02  ERR-MSG1   PIC  N(07) VALUE
               "‚i‚b‚n‚m@‚È‚µ".
           02  ERR-MSG2   PIC  N(15) VALUE
               "‚i‚b‚n‚m@‚q‚d‚v‚q‚h‚s‚dƒGƒ‰[".
           02  ERR-MSG3   PIC  N(10) VALUE
               "‚i‚r|‚r‚h‚f‚mƒGƒ‰[".
           02  ERR-MSG4   PIC  N(05) VALUE
               "ƒLƒƒƒ“ƒZƒ‹".
           02  ERR-MSG9   PIC  N(17) VALUE
               "‚P`‚Q•ª‚µ‚ÄAÄ“xŽÀs‚µ‚Ä‰º‚³‚¢B".
           02  ERR-MSG10  PIC  N(12) VALUE
               "ŠÇ—ŽÒ‚É˜A—‚µ‚Ä‰º‚³‚¢B".
           COPY LSSEM.
       PROCEDURE                   DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "150" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" " " "1" "0" "70" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-MID1" "RX" "1" "15" "36" " " "01C-MID1"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201C-MID1" "N" "1" "16" "34" "0101C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID1" "X" "10" "16" "28" "01C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID1" "bN" "15" "16" "30" "02C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID1" "bN" "16" "16" "22" "03C-MID1" " "
            RETURNING RESU.
      *C-MID2
       CALL "SD_Init" USING 
            "C-MID2" " " "0" "0" "150" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID2" " " "1" "0" "70" " " "C-MID2" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-MID2" "RX" "1" "15" "36" " " "01C-MID2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201C-MID2" "N" "1" "16" "34" "0101C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID2" "X" "10" "16" "28" "01C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID2" "bN" "15" "16" "30" "02C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID2" "bN" "16" "16" "22" "03C-MID2" " "
            RETURNING RESU.
      *C-MID5
       CALL "SD_Init" USING 
            "C-MID5" " " "0" "0" "155" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID5" " " "1" "0" "66" " " "C-MID5" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-MID5" "RX" "1" "15" "34" " " "01C-MID5"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201C-MID5" "N" "1" "16" "32" "0101C-MID5" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID5" "X" "10" "7" "37" "01C-MID5" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID5" "bN" "15" "16" "30" "02C-MID5" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID5" "bN" "16" "16" "22" "03C-MID5" " "
            RETURNING RESU.
      *C-MID9
       CALL "SD_Init" USING 
            "C-MID9" " " "0" "0" "22" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID9" "X" "22" "30" "22" " " "C-MID9" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "10" "43" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "47" "1" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "100" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BS" "N" "1" "1" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-BS" BY REFERENCE W-BS "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "15" "0" "92" "D-BS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MSG0" "N" "15" "15" "10" " " "02C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MSG1" "N" "15" "15" "14" "D-MSG0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MSG2" "N" "15" "15" "14" "D-MSG1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MSG3" "N" "15" "15" "14" "D-MSG2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MSG4" "N" "15" "15" "14" "D-MSG3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MSG5" "N" "15" "15" "14" "D-MSG4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MSG9" "N" "15" "40" "12" "D-MSG5" " " RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "24" "0" "132" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG1" "N" "24" "15" "14" " " "DSP-ERR"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG2" "N" "24" "15" "30" "ERR-MSG1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG3" "N" "24" "15" "20" "ERR-MSG2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG4" "N" "24" "15" "10" "ERR-MSG3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG9" "N" "24" "15" "34" "ERR-MSG4" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG10" "N" "24" "55" "24" "ERR-MSG9" " "
            RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           ACCEPT    JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN    NOT  =  1   AND  2   AND  5   AND  6   AND  7
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "ERR-MSG3" ERR-MSG3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "ERR-MSG10" ERR-MSG10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-95
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN         =  1   OR  6
               CALL "SD_Output" USING
                "C-MID1" C-MID1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-MID9" C-MID9 "p" RETURNING RESU
           END-IF
           IF  JS-SIGN         =  2   OR  7
               CALL "SD_Output" USING
                "C-MID2" C-MID2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-MID9" C-MID9 "p" RETURNING RESU
           END-IF
           IF  JS-SIGN         =  5
               CALL "SD_Output" USING
                "C-MID5" C-MID5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-MID9" C-MID9 "p" RETURNING RESU
           END-IF
           MOVE SPACE TO W-BS.
           IF  JS-SIGN = 1 OR 2
               MOVE "y–{ŽÐz" TO W-BS
           END-IF
           IF  JS-SIGN = 6 OR 7
               MOVE "y‘qŒÉz" TO W-BS
           END-IF
           CALL "SD_Output" USING "D-BS" D-BS "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT           =  "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "ERR-MSG4" ERR-MSG4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "ERR-MSG10" ERR-MSG10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-95
           END-IF
           IF  ESTAT      NOT  =  "01"  AND  "06"
               GO  TO  M-10
           END-IF
           IF  JS-SIGN         =  1   OR  2   OR  6   OR  7
               IF  W-SEN           >  1
                   GO  TO  M-10
               END-IF
           END-IF
           IF  JS-SIGN         =  5
               IF  W-SEN           >  2
                   GO  TO  M-10
               END-IF
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT           =  "09"
               GO  TO  M-10
           END-IF
           IF  ESTAT      NOT  =  "01"  AND  "06"
               GO  TO  M-15
           END-IF
           IF  W-DMM           =  9
               GO  TO  M-10
           END-IF
           IF  W-DMM      NOT  =  1
               GO  TO  M-15
           END-IF
           CALL "DB_F_Open" USING
            "EXTEND" CHKF_PNAME1 "SHARED" BY REFERENCE CHKF_IDLST "0".
           ACCEPT  W-TIM      FROM  TIME.
           INITIALIZE  CHKF-R.
           ACCEPT  CHKF-DATE  FROM  DATE.
           MOVE    W-TIME     TO    CHKF-TIME.
           MOVE    JS-SIGN    TO    CHKF-SIGN.
           MOVE    W-SEN      TO    CHKF-SEN.
           MOVE    "00U"      TO    CHKF-PRG.
      *           WRITE   CHKF-R.
      *//////////////
           CALL "DB_Insert" USING
            CHKF_PNAME1 CHKF_LNAME CHKF-R RETURNING RET.
           CALL "DB_F_Close" USING BY REFERENCE CHKF_IDLST CHKF_PNAME1.
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN         =  1  OR  6
               CALL "SD_Output" USING
                "C-MID1" C-MID1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-MID9" C-MID9 "p" RETURNING RESU
           END-IF
           IF  JS-SIGN         =  2  OR  7
               CALL "SD_Output" USING
                "C-MID2" C-MID2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-MID9" C-MID9 "p" RETURNING RESU
           END-IF
           IF  JS-SIGN         =  5
               CALL "SD_Output" USING
                "C-MID5" C-MID5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-MID9" C-MID9 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-BS" D-BS "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SEN" A-SEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DMM" A-DMM "p" RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON8-KEY" BY REFERENCE JCON8-KEY.
           MOVE    SPACE          TO  JCON8-KEY.
           MOVE    8              TO  JCON8-01.
      *           READ    JCON       INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "ERR-MSG1" ERR-MSG1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "ERR-MSG10" ERR-MSG10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-90
           END-IF
           IF  JS-SIGN    NOT  =  5
               GO  TO  M-20
           END-IF
           IF  W-SEN           =  0
               IF  JCON8-06        =  0
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "D-MSG0" D-MSG0 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-MSG9" D-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ERR-MSG10" ERR-MSG10 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO  TO  M-90
               END-IF
           END-IF
           IF  W-SEN      NOT  =  0
               IF  JCON8-06        =  1
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "D-MSG2" D-MSG2 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-MSG9" D-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ERR-MSG9" ERR-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO  TO  M-90
               END-IF
           END-IF
           IF  W-SEN      NOT  =  0
               IF  JCON8-06        =  2
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "D-MSG3" D-MSG3 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-MSG9" D-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ERR-MSG9" ERR-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO  TO  M-90
               END-IF
           END-IF
           MOVE    W-SEN          TO  JCON8-06.
           GO  TO  M-50.
       M-20.
           IF  W-SEN      NOT  =  0
               IF  ((JS-SIGN = 1 OR 2) AND (JCON8-04 = 2)) OR
                   ((JS-SIGN = 6 OR 7) AND (JCON8-04 = 1))
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "D-MSG5" D-MSG5 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-MSG9" D-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ERR-MSG9" ERR-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO  TO  M-90
               END-IF
           END-IF
           IF  JS-SIGN    NOT  =  1
               GO  TO  M-25
           END-IF
           IF  W-SEN           =  0
               IF  JCON8-051       =  0
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "D-MSG0" D-MSG0 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-MSG9" D-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ERR-MSG10" ERR-MSG10 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO  TO  M-90
               END-IF
           END-IF
           IF  W-SEN      NOT  =  0
               IF  JCON8-051       =  1
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "D-MSG1" D-MSG1 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-MSG9" D-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ERR-MSG9" ERR-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO  TO  M-90
               END-IF
           END-IF
           MOVE    W-SEN          TO  JCON8-051.
           GO  TO  M-50.
       M-25.
           IF  JS-SIGN    NOT  =  2
               GO  TO  M-30
           END-IF
           IF  W-SEN           =  0
               IF  JCON8-052       =  0
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "D-MSG0" D-MSG0 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-MSG9" D-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ERR-MSG10" ERR-MSG10 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO  TO  M-90
               END-IF
           END-IF
           IF  W-SEN      NOT  =  0
               IF  JCON8-052       =  1
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "D-MSG1" D-MSG1 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-MSG9" D-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ERR-MSG9" ERR-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO  TO  M-90
               END-IF
           END-IF
           MOVE    W-SEN          TO  JCON8-052.
           GO  TO  M-50.
       M-30.
           IF  JS-SIGN    NOT  =  6
               GO  TO  M-35
           END-IF
           IF  W-SEN           =  0
               IF  JCON8-053       =  0
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "D-MSG0" D-MSG0 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-MSG9" D-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ERR-MSG10" ERR-MSG10 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO  TO  M-90
               END-IF
           END-IF
           IF  W-SEN      NOT  =  0
               IF  JCON8-053       =  1
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "D-MSG4" D-MSG4 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-MSG9" D-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ERR-MSG9" ERR-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO  TO  M-90
               END-IF
           END-IF
           MOVE    W-SEN          TO  JCON8-053.
           GO  TO  M-50.
       M-35.
           IF  JS-SIGN    NOT  =  7
               GO  TO  M-90
           END-IF
           IF  W-SEN           =  0
               IF  JCON8-054       =  0
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "D-MSG0" D-MSG0 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-MSG9" D-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ERR-MSG10" ERR-MSG10 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO  TO  M-90
               END-IF
           END-IF
           IF  W-SEN      NOT  =  0
               IF  JCON8-054       =  1
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "D-MSG4" D-MSG4 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-MSG9" D-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ERR-MSG9" ERR-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO  TO  M-90
               END-IF
           END-IF
           MOVE    W-SEN          TO  JCON8-054.
       M-50.
      *           REWRITE   JCON8-R INVALID
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON8-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "ERR-MSG2" ERR-MSG2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "ERR-MSG10" ERR-MSG10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Open" USING
            "EXTEND" CHKF_PNAME1 "SHARED" BY REFERENCE CHKF_IDLST "0".
           ACCEPT  W-TIM      FROM  TIME.
           INITIALIZE  CHKF-R.
           ACCEPT  CHKF-DATE  FROM  DATE.
           MOVE    W-TIME     TO    CHKF-TIME.
           MOVE    JS-SIGN    TO    CHKF-SIGN.
           MOVE    W-SEN      TO    CHKF-SEN.
           MOVE    "00U"      TO    CHKF-PRG.
      *           WRITE   CHKF-R.
      *//////////////
           CALL "DB_Insert" USING
            CHKF_PNAME1 CHKF_LNAME CHKF-R RETURNING RET.
           CALL "DB_F_Close" USING BY REFERENCE CHKF_IDLST CHKF_PNAME1.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.

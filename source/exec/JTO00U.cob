       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 JTO00U.
      **************************************************************************
      *    PROGRAM  :  ‚i‚b‚n‚m@“`‘—’†‹æ•ª@XV                              *
      **************************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       SOURCE-COMPUTER.            SYSTEM3100.
       OBJECT-COMPUTER.            SYSTEM3100.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
       01  ERR-STAT                PIC  X(02).
       01  W-FILE                  PIC  X(13).
       01  W-DATA.
           03  W-SEN               PIC  9(01).
           03  W-DMM               PIC  9(01).
      *
           COPY    L-JCON.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           03  FILLER.
             05  FILLER  PIC X(28) VALUE
                           "                            ".
             05  FILLER  PIC N(13) VALUE
                          "‚i‚b‚n‚m@“`‘—’†‹æ•ª@XV".
           03  FILLER  PIC X(38) VALUE
                       "‰ð@œ=0 , “`‘—’†=1 , ¶¬’†=2 .....  ".
           03  FILLER  PIC X(22) VALUE
                       "Šm”F  OK=1 NO=9   ØÀ°Ý".
       01  C-ACP.
           03  A-SEN   PIC 9(1).
           03  A-DMM   PIC 9(1).
       01  C-DSP.
           02  D-MSG3   PIC  N(07) VALUE
               "y“`‘—ˆ—’†z".
           02  D-MSG4   PIC  N(05) VALUE
               "y‰ðœÏz".
           02  D-MSG5   PIC  N(07) VALUE
               "y¶¬ˆ—’†z".
           02  D-MSG9   PIC  N(06) VALUE
               "I—¹F‚d‚“‚ƒ".
       01  DSP-ERR.
           02  ERR-MSG1   PIC  N(07) VALUE
               "‚i‚b‚n‚m@‚È‚µ".
           02  ERR-MSG2   PIC  N(15) VALUE
               "‚i‚b‚n‚m@‚q‚d‚v‚q‚h‚s‚dƒGƒ‰[".
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
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "114" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" " " "1" "0" "54" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-MID" "RX" "1" "15" "28" " " "01C-MID"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201C-MID" "N" "1" "16" "26" "0101C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "10" "16" "38" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "22" "30" "22" "02C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "10" "53" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "47" "1" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "15" "0" "50" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MSG3" "N" "15" "15" "14" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MSG4" "N" "15" "15" "10" "D-MSG3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MSG5" "N" "15" "15" "14" "D-MSG4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MSG9" "N" "15" "40" "12" "D-MSG5" " " RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "24" "0" "102" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG1" "N" "24" "15" "14" " " "DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG2" "N" "24" "15" "30" "ERR-MSG1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG9" "N" "24" "15" "34" "ERR-MSG2" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "ERR-MSG10" "N" "24" "15" "24" "ERR-MSG9" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT           =  "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  M-95
           END-IF
           IF  ESTAT      NOT  =  "01"  AND  "06"
               GO  TO  M-10
           END-IF
           IF  W-SEN           >  2
               GO  TO  M-10
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
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
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
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "ERR-MSG1" ERR-MSG1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-90
           END-IF
           IF  W-SEN      NOT  =  0
               IF  JCON8-04        =  1
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
           IF  W-SEN      NOT  =  0
               IF  JCON8-04        =  2
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
           IF  W-SEN           =  0
               IF  JCON8-04        =  0
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "D-MSG4" D-MSG4 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-MSG9" D-MSG9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ERR-MSG10" ERR-MSG10 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO  TO  M-90
               END-IF
           END-IF
           IF  W-SEN           =  1
               IF (JCON8-053   NOT =  0)  OR  (JCON8-054   NOT = 0)
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
           IF  W-SEN           =  2
               IF (JCON8-051   NOT =  0)  OR  (JCON8-052   NOT = 0)
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
           MOVE    W-SEN          TO  JCON8-04.
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
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.

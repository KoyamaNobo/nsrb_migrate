       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         JT055R.
       AUTHOR.             1996-10-02.
      ****************************************
      ******    荷札・入日記　問合せ    ******
      ******    JS-SIGN  :  本社=1 , 倉庫=9   ******
      ****************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT                PIC X(02)    VALUE  SPACE.
       77  JS-SIGN                 PIC 9(01).
       01  WORK-AREA.
           02  W-NO                PIC 9(06).
           02  W-SK                PIC 9(01).
           02  W-HK                PIC N(01).
           02  W-L                 PIC 9(02).
           02  W-DMM               PIC 9(01).
           02  W-END               PIC 9(01).
       01  W-STAT.
           02  HTB                 PIC X(02)    VALUE  "01".
           02  SKP                 PIC X(02)    VALUE  "06".
           02  BTB                 PIC X(02)    VALUE  "09".
           02  PF9                 PIC X(02)    VALUE  "P9".
       COPY  LWMSG.
      *
           COPY  L-JNIF-RYO.
           COPY  LITCM.
           COPY  L-JCON.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  ACP-AREA.
           02  A-SK   PIC 9(01).
           02  A-DMM  PIC 9(01).
       01  DSP-AREA.
           02  D-SK.
               03  FILLER  PIC 9(01).
               03  FILLER  PIC N(06).
           02  D-DATA.
               03  FILLER  PIC 9(06).
               03  FILLER  PIC 9(02).
               03  FILLER  PIC X(01)    VALUE  "/".
               03  FILLER  PIC 9(02).
               03  FILLER  PIC N(21).
               03  FILLER  PIC N(06).
               03  FILLER  PIC 9(06).
               03  FILLER  PIC N(01).
       01  ERR-MSG-AREA.
           02  FILLER.
               03  E-STAT   PIC X(10).
               03  E-ME1    PIC X(18)   VALUE
                     "***  ｶﾞｲﾄｳ ﾅｼ  ***".
               03  E-ME2    PIC X(21)   VALUE
                     "***  ﾃﾞｰﾀ ｼｭｳﾘｮｳ  ***".
               03  E-ME98   PIC X(05)   VALUE X"1B4A05".
               03  E-ME99   PIC X(05)   VALUE X"1B4205".
               03  E-CL     PIC X(40)   VALUE
                     "                                        ".
       01  DATA-CLEAR-AREA.
           02  D-CL1  PIC X(14)    VALUE
                 "              ".
           02  D-CL2.
               03  FILLER  PIC X(40)    VALUE
                     "                                        ".
               03  FILLER  PIC X(40)    VALUE
                     "                                        ".
       PROCEDURE           DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SK" "9" "3" "6" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SK" BY REFERENCE W-SK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "24" "65" "1" "A-SK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "86" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SK" " " "3" "0" "13" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SK" "9" "3" "6" "1" " " "D-SK" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-SK" BY REFERENCE W-SK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SK" "N" "3" "8" "12" "01D-SK" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SK" BY REFERENCE JCON3-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATA" " " "W-L" "0" "73" "D-SK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DATA" "9" "W-L" "1" "6" " " "D-DATA" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DATA" BY REFERENCE JNIF1-01 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DATA" "9" "W-L" "8" "2" "01D-DATA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DATA" BY REFERENCE JNIF1-042 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-DATA" "X" "W-L" "10" "1" "02D-DATA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-DATA" "9" "W-L" "11" "2" "03D-DATA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-DATA" BY REFERENCE JNIF1-043 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "05D-DATA" "N" "W-L" "14" "42" "04D-DATA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "05D-DATA" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-DATA" "N" "W-L" "57" "12" "05D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06D-DATA" BY REFERENCE JCON2-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-DATA" "9" "W-L" "70" "6" "06D-DATA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "07D-DATA" BY REFERENCE JNIF1-14 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-DATA" "N" "W-L" "78" "2" "07D-DATA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "08D-DATA" BY REFERENCE W-HK "2" "0" RETURNING RESU.
      *ERR-MSG-AREA
       CALL "SD_Init" USING 
            "ERR-MSG-AREA" " " "0" "0" "99" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-MSG-AREA" " " "24" "0" "99" " " "ERR-MSG-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "10" " " "01ERR-MSG-AREA"
            RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "3" "18" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "3" "21" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "1" "40" "E-ME99" " " RETURNING RESU.
      *DATA-CLEAR-AREA
       CALL "SD_Init" USING 
            "DATA-CLEAR-AREA" " " "0" "0" "94" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CL1" "X" "3" "6" "14" " " "DATA-CLEAR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CL2" " " "W-L" "0" "80" "D-CL1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-CL2" "X" "W-L" "1" "40" " " "D-CL2" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-CL2" "X" "W-L" "41" "40" "01D-CL2" " "
            RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT  JS-SIGN FROM ARGUMENT-VALUE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ055R" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" JNIF_PNAME1 "SHARED" BY REFERENCE JNIF_IDLST "1"
            "JNIF1-KEY" BY REFERENCE JNIF1-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON2-KEY" BY REFERENCE JCON2-KEY.
           IF  JS-SIGN = 3
               MOVE  6  TO  W-SK
               GO  TO  M-15
           END-IF
           IF  JS-SIGN = 4
               MOVE  7  TO  W-SK
               GO  TO  M-15
           END-IF.
       M-10.
           IF  JS-SIGN  NOT = 1 AND 9
               GO  TO  M-99
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-SK "A-SK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-30
           END-IF
           IF  ESTAT = PF9
               GO  TO  M-99
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-10
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ055R" RETURNING RESU.
           CALL "SD_Output" USING "A-SK" A-SK "p" RETURNING RESU.
       M-15.
           MOVE  3      TO  JCON3-01.
           MOVE  W-SK   TO  JCON3-02.
      *           READ  JCON  WITH  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-10
           END-IF
           CALL "SD_Output" USING "D-SK" D-SK "p" RETURNING RESU.
           MOVE  ZERO  TO  W-NO  W-END.
       M-20.
           MOVE  5     TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-25.
           PERFORM  S-05  THRU  S-10.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-30
           END-IF
           IF  W-DMM = 9
               IF  JS-SIGN  NOT = 1
                   GO  TO  M-99
               ELSE
                   CALL "DB_F_Close" USING
                    BY REFERENCE JNIF_IDLST JNIF_PNAME1
                   CALL "DB_F_Open" USING
                    "INPUT" JNIF_PNAME1 "SHARED" BY REFERENCE JNIF_IDLST
                    "1" "JNIF1-KEY" BY REFERENCE JNIF1-KEY
                   CALL "SD_Output" USING
                    "D-CL1" D-CL1 "p" RETURNING RESU
                   PERFORM  S-15  THRU  S-25
                   GO  TO  M-10
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO  TO  M-30
           END-IF
      *
           IF  W-END        = 1
               IF  JS-SIGN  NOT = 1
                   GO  TO  M-99
                 ELSE
                   CALL "DB_F_Close" USING
                    BY REFERENCE JNIF_IDLST JNIF_PNAME1
                   CALL "DB_F_Open" USING
                    "INPUT" JNIF_PNAME1 "SHARED" BY REFERENCE JNIF_IDLST
                    "1" "JNIF1-KEY" BY REFERENCE JNIF1-KEY
                   CALL "SD_Output" USING
                    "D-CL1" D-CL1 "p" RETURNING RESU
                   PERFORM  S-15  THRU  S-25
                   GO TO M-10
               END-IF
           END-IF
           PERFORM  S-15  THRU  S-25.
           GO  TO  M-20.
       M-99.
           CALL "DB_F_Close" USING BY REFERENCE JNIF_IDLST JNIF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.
       S-05.
      *           READ  JNIF  NEXT  RECORD  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JNIF_PNAME1 BY REFERENCE JNIF-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JNIF_IDLST JNIF_PNAME1
               CALL "DB_F_Open" USING
                "INPUT" JNIF_PNAME1 "SHARED" BY REFERENCE JNIF_IDLST "1"
                "JNIF1-KEY" BY REFERENCE JNIF1-KEY
               GO  TO  S-10
           END-IF
           IF  JNIF1-02 = 7
               GO  TO  S-05
           END-IF
           IF  JNIF1-07 NOT = W-SK
               GO  TO  S-05
           END-IF
           IF  JNIF1-01 = W-NO
               GO  TO  S-05
           END-IF
           MOVE  JNIF1-01  TO  W-NO.
           IF  JNIF1-10 = ZERO
               MOVE  "未"  TO  W-HK
           END-IF
           IF  JNIF1-10 = 1
               MOVE  "済"  TO  W-HK
           END-IF
           MOVE  JNIF1-05  TO  TC-KEY.
      *           READ  TC-M  WITH  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  JNIF1-05  TO  TC-NAME
           END-IF
           MOVE  2  TO  JCON2-01.
           MOVE  JNIF1-06  TO  JCON2-02.
      *           READ  JCON  WITH  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  JNIF1-06  TO  JCON2-03
           END-IF
           CALL "SD_Output" USING "D-DATA" D-DATA "p" RETURNING RESU.
           ADD  1  TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L < 24
               GO  TO  S-05
           END-IF.
       S-10.
           EXIT.
       S-15.
           MOVE  5  TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       S-20.
           CALL "SD_Output" USING "D-CL2" D-CL2 "p" RETURNING RESU.
           ADD  1  TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L < 24
               GO  TO  S-20
           END-IF.
       S-25.
           EXIT.

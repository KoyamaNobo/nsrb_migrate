       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT795U.
       AUTHOR.                        I.N.
      ***************************************************
      *    PROGRAM        : éÛíçÅEóaÇËÅEéÊÇÊÇØë‰í†íäèo  *
      *                   : JS-SIGN  0=ìñåé  1=ëOåé     *
      *    DATA WRITTEN   : 95/10/03                    *
      *    SCREEN USED    : UNUSED                      *
      *    FORM   USED    : UNUSED                      *
      *    PRINTER TYPE   : UNUSED                      *
      *    COMPILE TYPE   : COBOL                       *
      ***************************************************
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM150.
       OBJECT-COMPUTER.               SYSTEM150.
       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77  ERR-STAT                  PIC X(02)    VALUE SPACE.
       01  JS-SIGN                   PIC X(01).
       01  WK0128ID                  PIC X(09).
       01  STN-NO.
           02  STN-NO1               PIC X(03).
           02  STN-NO2               PIC X(03).
       01  W-FID.
           02  W-FID1                PIC X(06)    VALUE "WK0128".
           02  W-FID2                PIC X(03).
       01  S-AREA.
           03  S-TUKI                PIC 9(02).
           03  S-OKC                 PIC 9(01).
       01  W-AREA.
           03  END-SW                PIC 9(01).
           03  I                     PIC 9(02).
           03  W-YMD                 PIC 9(06).
           03  W-YMDD  REDEFINES W-YMD.
               04  W-YY              PIC 9(02).
               04  W-MM              PIC 9(02).
               04  W-DD              PIC 9(02).
           03  W-KEI                 PIC S9(06).
       01  WJMST-R.
           02   WJMST-01                  PIC 9(1).
           02   WJMST-02.
                03   WJMST-021            PIC 9(4).
                03   WJMST-022            PIC 9(2).
                03   WJMST-023            PIC 9(2).
           02   WJMST-03                  PIC 9(6).
           02   WJMST-04                  PIC 9(4).
           02   WJMST-05                  PIC 9(6).
           02   WJMST-06.
                03   WJMST-061            PIC 9(4).
                03   WJMST-062            PIC 9(2).
                03   WJMST-063            PIC 9(2).
           02   WJMST-KEY1.
                03   WJMST-07             PIC 9(6).
                03   WJMST-08             PIC 9(1).
           02   WJMST-09                  PIC 9(1).
           02   WJMST-10                  PIC 9(3).
           02   WJMST-11.
                03  WJMST-111            OCCURS  10.
                    04  WJMST-1111       PIC S9(6)   COMP-3.
           02   WJMST-12.
                03  WJMST-121            OCCURS  10.
                    04  WJMST-1211       PIC S9(6)   COMP-3.
           02   WJMST-14.
                03  WJMST-141  OCCURS 10 PIC S9(06)  COMP-3.
           02   WJMST-15.
                03  WJMST-151  OCCURS 10 PIC S9(06)  COMP-3.
           02   WJMST-16                 PIC S9(03).
           02   F                        PIC X(04).
           02   WJMST-18                 PIC 9(01).
           02   F                        PIC X(07).
           02   WJMST-20                 PIC 9(03).
           02   WJMST-13                 PIC N(32).
           02   WJMST-21                 PIC 9(01).
           02   WJMST-17                 PIC 9(05).
           02   FILLER                   PIC X(46).
           02   WJMST-90                 PIC 9(01).
           02   WJMST-91                 PIC 9(02).
       01  WJNSR-R.
           02  WJNSR-KEY1.
             03  WJNSR-01             PIC  9(06)  COMP-3.
             03  WJNSR-02             PIC  9(08)  COMP-3.
             03  WJNSR-03             PIC  9(02).
             03  WJNSR-04             PIC  9(06)  COMP-3.
             03  WJNSR-05             PIC  9(01).
           02  WJNSR-06               PIC  9(01).
           02  WJNSR-07               PIC  9(01).
           02  WJNSR-08.
             03  WJNSR-081            PIC S9(04)  COMP-3    OCCURS  10.
           02  WJNSR-09               PIC  9(01).
           02  WJNSR-10               PIC  9(01).
           02  WJNSR-11.
             03  WJNSR-111            PIC  9(04).
             03  WJNSR-112            PIC  9(03).
           02  WJNSR-12               PIC  9(06)  COMP-3.
           02  WJNSR-13               PIC  9(01).
           02  WJNSR-14               PIC  9(01).
           02  WJNSR-KEY2.
             03  WJNSR-15.
               04  WJNSR-151          PIC  9(06)  COMP-3.
               04  WJNSR-152          PIC  9(01).
             03  WJNSR-16             PIC  9(08)  COMP-3.
             03  WJNSR-17             PIC  9(01).
             03  WJNSR-18.
               04  WJNSR-181          PIC  9(06)  COMP-3.
               04  WJNSR-182          PIC  9(01).
           02  WJNSR-KEY3.
             03  WJNSR-19             PIC  9(04).
             03  WJNSR-20             PIC  9(08)  COMP-3.
             03  WJNSR-21             PIC  9(01).
             03  WJNSR-22.
               04  WJNSR-221          PIC  9(06)  COMP-3.
               04  WJNSR-222          PIC  9(01).
           02  WJNSR-23               PIC  N(09).
           02  WJNSR-24               PIC  N(23).
           02  FILLER                 PIC  X(08).
           02  WJNSR-90               PIC  9(01).
           02  WJNSR-91               PIC  9(01).
           02  WJNSR-92               PIC  9(02).
       COPY    LWMSG.
      *
           COPY  LJMST1.
           COPY  L-JNSR.
      ***  ëOåééÛíçÉ}ÉXÉ^  ÅiÇjÇdÇxÇPÅj
      *FD  DJMST
       01  DJMST_JT795U.
           02  DJMST_PNAME1           PIC  X(008) VALUE "BU-JMSTD".
           02  F                      PIC  X(001).
           02  DJMST_LNAME            PIC  X(012) VALUE "DJMST_JT795U".
           02  F                      PIC  X(001).
           02  DJMST_KEY1             PIC  X(100) VALUE SPACE.
           02  DJMST_KEY2             PIC  X(100) VALUE SPACE.
           02  DJMST_SORT             PIC  X(100) VALUE SPACE.
           02  DJMST_IDLST            PIC  X(100) VALUE SPACE.
           02  DJMST_RES              USAGE  POINTER.
       01  DJMST-R.
           02   DJMST-01                  PIC 9(1).
           02   DJMST-02.
                03   DJMST-021            PIC 9(4).
                03   DJMST-022            PIC 9(2).
                03   DJMST-023            PIC 9(2).
           02   DJMST-03                  PIC 9(6).
           02   DJMST-04                  PIC 9(4).
           02   DJMST-05                  PIC 9(6).
           02   DJMST-06.
                03   DJMST-061            PIC 9(4).
                03   DJMST-062            PIC 9(2).
                03   DJMST-063            PIC 9(2).
           02   DJMST-KEY1.
                03   DJMST-07             PIC 9(6).
                03   DJMST-08             PIC 9(1).
           02   DJMST-09                  PIC 9(1).
           02   DJMST-10                  PIC 9(3).
           02   DJMST-11.
                03  DJMST-111            OCCURS  10.
                    04  DJMST-1111       PIC S9(6)   COMP-3.
           02   DJMST-12.
                03  DJMST-121            OCCURS  10.
                    04  DJMST-1211       PIC S9(6)   COMP-3.
           02   DJMST-14.
                03  DJMST-141  OCCURS 10 PIC S9(06)  COMP-3.
           02   DJMST-15.
                03  DJMST-151  OCCURS 10 PIC S9(06)  COMP-3.
           02   DJMST-16                 PIC S9(03).
           02   F                        PIC X(04).
           02   DJMST-18                 PIC 9(01).
           02   F                        PIC X(07).
           02   DJMST-20                 PIC 9(03).
           02   DJMST-13                 PIC N(32).
           02   DJMST-21                 PIC 9(01).
           02   DJMST-17                 PIC 9(05).
           02   FILLER                   PIC X(46).
           02   DJMST-90                 PIC 9(01).
           02   DJMST-91                 PIC 9(02).
       77  F                             PIC  X(001).
      *FD  DJNSR
       01  DJNSR_JT795U.
           02  DJNSR_PNAME1           PIC  X(007) VALUE "BU-JNSR".
           02  F                      PIC  X(001).
           02  DJNSR_LNAME            PIC  X(012) VALUE "DJNSR_JT795U".
           02  F                      PIC  X(001).
           02  DJNSR_KEY1             PIC  X(100) VALUE SPACE.
           02  DJNSR_SORT             PIC  X(100) VALUE SPACE.
           02  DJNSR_IDLST            PIC  X(100) VALUE SPACE.
           02  DJNSR_RES              USAGE  POINTER.
       01  DJNSR-R.
           02  DJNSR-KEY1.
             03  DJNSR-01             PIC  9(06)  COMP-3.
             03  DJNSR-02             PIC  9(08)  COMP-3.
             03  DJNSR-03             PIC  9(02).
             03  DJNSR-04             PIC  9(06)  COMP-3.
             03  DJNSR-05             PIC  9(01).
           02  DJNSR-06               PIC  9(01).
           02  DJNSR-07               PIC  9(01).
           02  DJNSR-08.
             03  DJNSR-081            PIC S9(04)  COMP-3    OCCURS  10.
           02  DJNSR-09               PIC  9(01).
           02  DJNSR-10               PIC  9(01).
           02  DJNSR-11.
             03  DJNSR-111            PIC  9(04).
             03  DJNSR-112            PIC  9(03).
           02  DJNSR-12               PIC  9(06)  COMP-3.
           02  DJNSR-13               PIC  9(01).
           02  DJNSR-14               PIC  9(01).
           02  DJNSR-KEY2.
             03  DJNSR-15.
               04  DJNSR-151          PIC  9(06)  COMP-3.
               04  DJNSR-152          PIC  9(01).
             03  DJNSR-16             PIC  9(08)  COMP-3.
             03  DJNSR-17             PIC  9(01).
             03  DJNSR-18.
               04  DJNSR-181          PIC  9(06)  COMP-3.
               04  DJNSR-182          PIC  9(01).
           02  DJNSR-KEY3.
             03  DJNSR-19             PIC  9(04).
             03  DJNSR-20             PIC  9(08)  COMP-3.
             03  DJNSR-21             PIC  9(01).
             03  DJNSR-22.
               04  DJNSR-221          PIC  9(06)  COMP-3.
               04  DJNSR-222          PIC  9(01).
           02  DJNSR-23               PIC  N(09).
           02  DJNSR-24               PIC  N(23).
           02  FILLER                 PIC  X(08).
           02  DJNSR-90               PIC  9(01).
           02  DJNSR-91               PIC  9(01).
           02  DJNSR-92               PIC  9(02).
       77  F                          PIC  X(001).
           COPY  LTWK05.
           COPY  L-JCON.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLE-01.
           02  FILLER   PIC X(12) VALUE "CLEAR SCREEN".
       01  DSP-AREA.
           02  FILLER   PIC X(28) VALUE
                 "                            ".
           02  FILLER   PIC N(13) VALUE
               "éÛíçÅEóaÇËÅEéÊÇÊÇØë‰í†íäèo".
           02  FILLER   PIC X(04) VALUE  "åéìx".
           02  FILLER   PIC X(20) VALUE  "ämîFÅiOK=1,NO=9Åj-->".
           02  FILLER   PIC X(04) VALUE  "ÿ¿∞›".
       01  ACP-AREA.
           02  DSP-TUKI  PIC Z9 .
           02  ACP-OKC   PIC 9(01).
       01  CLR-AREA.
           02  FILLER   PIC  X(02)  VALUE  "  ".
           02  FILLER   PIC  X(01)  VALUE  " ".
       01  DSP-ERR.
           02  ERR-MSG1    PIC  N(07) VALUE
               "ÇiÇbÇnÇmÅ@Ç»Çµ".
      *
       COPY    LSMSG.
      *
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLE-01
       CALL "SD_Init" USING
           "CLE-01" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLE-01" "X" "1" "0" "12" " " "CLE-01" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "82" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" "RX" "1" "20" "28" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" "N" "1" "21" "26" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" "X" "6" "34" "4" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-AREA" "X" "23" "41" "20" "03DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-AREA" "X" "23" "62" "4" "04DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "3" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TUKI" "Z9" "6" "31" "2" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TUKI" BY REFERENCE S-TUKI "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "23" "61" "1" "DSP-TUKI" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-OKC" BY REFERENCE S-OKC "1" "0" RETURNING RESU.
      *CLR-AREA
       CALL "SD_Init" USING 
            "CLR-AREA" " " "0" "0" "3" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-AREA" "X" "6" "31" "2" " " "CLR-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLR-AREA" "X" "23" "61" "1" "01CLR-AREA" " "
            RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "24" "0" "14" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG1" "N" "24" "1" "14" " " "DSP-ERR" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ************************************
      *    ÉÅÉCÉìÅ@ÉãÅ[É`Éì              *
      ************************************
       PRO-010.
           PERFORM   INT-RTN   THRU  INT-EX.
           IF  END-SW           =  1
               GO  TO  PRO-900
           END-IF
           PERFORM   MAIN-RTN  THRU  MAIN-EX.
           PERFORM     END-RTN    THRU   END-EX.
       PRO-900.
           CALL "DB_Close".
           STOP  RUN.
      *********************************************
      *    ÇhÇmÇsÅ|ÇqÇsÇm                         *
      *********************************************
       INT-RTN.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           ACCEPT    JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN  NOT =  "0"  AND  "1"
               GO  TO  INT-RTN
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON6-KEY" BY REFERENCE JCON6-KEY.
           MOVE    SPACE          TO  JCON6-KEY.
           MOVE    6              TO  JCON6-01.
      *           READ    JCON       UNLOCK   INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "ERR-MSG1" ERR-MSG1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               MOVE    1           TO   END-SW
               GO  TO  INT-EX
           END-IF
           MOVE    JCON6-032          TO  W-MM.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           IF  JS-SIGN  NOT =  0
               SUBTRACT    1     FROM         W-MM
               IF  W-MM         =  ZERO
                   MOVE  12        TO  W-MM
               END-IF
           END-IF
      ***  âÊñ èàóù
           PERFORM   INP-RTN   THRU  INP-EX.
           IF  END-SW       =  1
               GO  TO  INT-EX
           END-IF
      **
           CALL  "CBLSTNNO" USING STN-NO USER_ID.
           MOVE  STN-NO2          TO  W-FID2.
           MOVE  W-FID            TO  WK0128ID.
           MOVE  WK0128ID         TO  JT-WK05_PNAME1.
           IF  JS-SIGN      =  0
               CALL "DB_F_Open" USING
                "INPUT" JMST1_PNAME1 "SHARED" BY REFERENCE JMST1_IDLST
                "1" "JMST1-KEY1" BY REFERENCE JMST1-KEY1
               CALL "DB_F_Open" USING
                "INPUT" JNSR_PNAME1 "SHARED" BY REFERENCE JNSR_IDLST "3"
                "JNSR-KEY1" BY REFERENCE JNSR-KEY1 "JNSR-KEY2"
                BY REFERENCE JNSR-KEY2 "JNSR-KEY3" BY REFERENCE
                JNSR-KEY3
           ELSE
               CALL "DB_F_Open" USING
                "INPUT" DJMST_PNAME1 "SHARED" BY REFERENCE DJMST_IDLST
                "1" "DJMST-KEY1" BY REFERENCE DJMST-KEY1
               CALL "DB_F_Open" USING
                "INPUT" DJNSR_PNAME1 "SHARED" BY REFERENCE DJNSR_IDLST
                "1" "DJNSR-KEY2" BY REFERENCE DJNSR-KEY2
           END-IF
           CALL "DB_F_Open" USING
            "OUTPUT" JT-WK05_PNAME1 "EXCLUSIVE" BY REFERENCE
            JT-WK05_IDLST "0".
       INT-EX.
           EXIT.
      *********************************************
      *    âÊñ èàóù       ÇhÇmÇoÅ|ÇqÇsÇm          *
      *********************************************
       INP-RTN.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           MOVE  W-MM         TO  S-TUKI.
           CALL "SD_Output" USING
            "DSP-TUKI" DSP-TUKI "p" RETURNING RESU.
       INP-070.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT            =   "P9"
               MOVE  1              TO  END-SW
               GO  TO  INP-EX
           END-IF
           IF  ESTAT        NOT =   "01"  AND  "06"
               GO  TO  INP-070
           END-IF
           IF  S-OKC            =  9
               MOVE  1              TO  END-SW
               GO  TO  INP-EX
           END-IF
           IF  S-OKC        NOT =  1
               GO  TO  INP-070
           END-IF.
       INP-EX.
           EXIT.
      *********************************************
      *    ÇdÇmÇcÅ|ÇqÇsÇm                         *
      *********************************************
       END-RTN.
           IF  JS-SIGN      =  0
               CALL "DB_F_Close" USING
                BY REFERENCE JMST1_IDLST JMST1_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JNSR_IDLST JNSR_PNAME1
           ELSE
               CALL "DB_F_Close" USING
                BY REFERENCE DJMST_IDLST DJMST_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE DJNSR_IDLST DJNSR_PNAME1
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK05_IDLST JT-WK05_PNAME1.
       END-EX.
           EXIT.
      *********************************************
      *    ÉÅÉCÉìèàóùÅ@Å@Å@                       *
      *********************************************
       MAIN-RTN.
           PERFORM  ZEN-RTN    THRU  ZEN-EX.
      *
           PERFORM  MEI-RTN    THRU  MEI-EX.
       MAIN-EX.
           EXIT.
      *********************************************
      *    ëOécÉåÉRÅ[Éhíäèoèàóù                   *
      *********************************************
       ZEN-RTN.
           IF  JS-SIGN          =  1
                 GO  TO  ZEN-020
           END-IF.
       ZEN-010.
      *           READ  JMST1  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST1_PNAME1 BY REFERENCE JMST1-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  ZEN-EX
           END-IF
           IF  JMST1-01     NOT =  0  AND  5  AND  6
               GO  TO  ZEN-010
           END-IF
           INITIALIZE          WJMST-R.
           MOVE  JMST1-R   TO  WJMST-R.
           GO  TO  ZEN-030.
       ZEN-020.
      *           READ  DJMST  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" DJMST_PNAME1 BY REFERENCE DJMST-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  ZEN-EX
           END-IF
           IF  DJMST-01     NOT =  0  AND  5  AND  6
               GO  TO  ZEN-020
           END-IF
           INITIALIZE          WJMST-R.
           MOVE  DJMST-R   TO  WJMST-R.
       ZEN-030.
           PERFORM  SET1-RTN   THRU  SET1-EX.
           PERFORM  WRT-RTN    THRU  WRT-EX.
      *
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  000.
           GO  TO  ZEN-RTN.
       ZEN-EX.
           EXIT.
      *********************************************
      *    ñæç◊ÉåÉRÅ[Éhíäèoèàóù                   *
      *********************************************
       MEI-RTN.
           IF  JS-SIGN          =  1
               GO  TO  MEI-020
           END-IF.
       MEI-010.
      *           READ  JNSR    NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JNSR_PNAME1 BY REFERENCE JNSR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  MEI-EX
           END-IF
           IF  JNSR-151         =  ZERO
               GO  TO  MEI-010
           END-IF
           IF  JNSR-92     NOT  =  SPACE  AND  ZERO
               GO  TO  MEI-010
           END-IF
           IF  JNSR-13      NOT =  0  AND  5  AND  6
               GO  TO  MEI-010
           END-IF
           MOVE  JNSR-151       TO  JMST1-07.
           MOVE  JNSR-152       TO  JMST1-08.
      *           READ  JMST1  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JMST1_PNAME1 BY REFERENCE JMST1-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  MEI-010
           END-IF
           INITIALIZE          WJNSR-R.
           MOVE  JNSR-R    TO  WJNSR-R.
           INITIALIZE          WJMST-R.
           MOVE  JMST1-R   TO  WJMST-R.
           GO  TO  MEI-030.
       MEI-020.
      *           READ  DJNSR   NEXT RECORD   AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" DJNSR_PNAME1 BY REFERENCE DJNSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  MEI-EX
           END-IF
           IF  DJNSR-151        =  ZERO
               GO  TO  MEI-020
           END-IF
           IF  DJNSR-92    NOT  =  SPACE  AND  ZERO
               GO  TO  MEI-020
           END-IF
           IF  DJNSR-13      NOT =  0  AND  5  AND  6
               GO  TO  MEI-020
           END-IF
           MOVE  DJNSR-151      TO  DJMST-07.
           MOVE  DJNSR-152      TO  DJMST-08.
      *           READ  DJMST  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" DJMST_PNAME1 BY REFERENCE DJMST-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  MEI-020
           END-IF
           INITIALIZE          WJNSR-R.
           MOVE  DJNSR-R   TO  WJNSR-R.
           INITIALIZE          WJMST-R.
           MOVE  DJMST-R   TO  WJMST-R.
       MEI-030.
           PERFORM  SET2-RTN   THRU  SET2-EX.
           PERFORM  WRT-RTN    THRU  WRT-EX.
      *
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  000.
           GO  TO  MEI-RTN.
       MEI-EX.
           EXIT.
      **********************************************
      *    ÇrÇdÇsÇPÅ|ÇqÇsÇm                        *
      **********************************************
       SET1-RTN.
           MOVE  SPACE         TO  WK05-R.
           INITIALIZE              WK05-R.
           MOVE  WJMST-01      TO  WK05-01.
           MOVE  WJMST-07      TO  WK05-061.
           MOVE  WJMST-08      TO  WK05-062.
           MOVE  ZERO          TO  WK05-03.
           MOVE  ZERO          TO  WK05-02.
           MOVE  WJMST-04      TO  WK05-041.
           MOVE  WJMST-10      TO  WK05-042.
           MOVE  WJMST-03      TO  WK05-07.
           MOVE  WJMST-09      TO  WK05-08.
           PERFORM  SUR-RTN  THRU  SUR-EX
                 VARYING  I    FROM  1  BY  1
                 UNTIL    I     >  10.
           MOVE  ZERO          TO  WK05-10.
           MOVE  WJMST-03      TO  WK05-95.
           MOVE  WJMST-02      TO  WK05-96.
           MOVE  WJMST-17      TO  WK05-97.
           IF (WJMST-16    NOT =  ZERO)  AND  (WK05-092  NOT = ZERO)
               COMPUTE  W-KEI  =  WJMST-1111(01)  +  WJMST-1111(02)  +
                                  WJMST-1111(03)  +  WJMST-1111(04)  +
                                  WJMST-1111(05)  +  WJMST-1111(06)  +
                                  WJMST-1111(07)  +  WJMST-1111(08)  +
                                  WJMST-1111(09)  +  WJMST-1111(10)
               IF  W-KEI       NOT =  ZERO
                   COMPUTE  W-KEI     =  W-KEI    /  WJMST-16
                   IF  W-KEI       NOT =  ZERO
                       COMPUTE  WK05-98   =  WK05-092 /  W-KEI
                   END-IF
               END-IF
           END-IF
           MOVE  S-TUKI        TO  WK05-99.
       SET1-EX.
           EXIT.
      **********************************************
      *    ÇrÇdÇsÇQÅ|ÇqÇsÇm                        *
      **********************************************
       SET2-RTN.
           MOVE  SPACE         TO  WK05-R.
           INITIALIZE              WK05-R.
           MOVE  WJNSR-13      TO  WK05-01.
           MOVE  WJMST-07      TO  WK05-061.
           MOVE  WJMST-08      TO  WK05-062.
           MOVE  WJNSR-16      TO  WK05-030.
           MOVE  WJNSR-181     TO  WK05-021.
           MOVE  WJNSR-182     TO  WK05-022.
           MOVE  WJNSR-11      TO  WK05-04.
           MOVE  WJNSR-01      TO  WK05-07.
           MOVE  WJNSR-07      TO  WK05-08.
           MOVE  WJNSR-081(01) TO  WK05-0911(01).
           MOVE  WJNSR-081(02) TO  WK05-0911(02).
           MOVE  WJNSR-081(03) TO  WK05-0911(03).
           MOVE  WJNSR-081(04) TO  WK05-0911(04).
           MOVE  WJNSR-081(05) TO  WK05-0911(05).
           MOVE  WJNSR-081(06) TO  WK05-0911(06).
           MOVE  WJNSR-081(07) TO  WK05-0911(07).
           MOVE  WJNSR-081(08) TO  WK05-0911(08).
           MOVE  WJNSR-081(09) TO  WK05-0911(09).
           MOVE  WJNSR-081(10) TO  WK05-0911(10).
           COMPUTE  WK05-092    =   WJNSR-081(01)  +  WJNSR-081(02)  +
                                    WJNSR-081(03)  +  WJNSR-081(04)  +
                                    WJNSR-081(05)  +  WJNSR-081(06)  +
                                    WJNSR-081(07)  +  WJNSR-081(08)  +
                                    WJNSR-081(09)  +  WJNSR-081(10).
           MOVE  WJNSR-10      TO  WK05-10.
           MOVE  WJNSR-152     TO  WK05-94.
           MOVE  WJMST-03      TO  WK05-95.
           MOVE  WJMST-02      TO  WK05-96.
           MOVE  WJMST-17      TO  WK05-97.
           MOVE  WJMST-16      TO  WK05-98.
           MOVE  S-TUKI        TO  WK05-99.
       SET2-EX.
           EXIT.
      **********************************************
      *    ÇvÇqÇsÅ|ÇqÇsÇmÅ@                        *
      **********************************************
       WRT-RTN.
      *           WRITE  WK05-R.
      *//////////////
           CALL "DB_Insert" USING
            JT-WK05_PNAME1 JT-WK05_LNAME WK05-R RETURNING RET.
           IF  ERR-STAT   NOT   =   "00"
               MOVE    "WK0256 "  TO  ERR-F
               MOVE    "W"        TO  ERR-M
               PERFORM    ERR-RTN    THRU    ERR-EX
           END-IF
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  000.
       WRT-EX.
           EXIT.
      **************************************************
      *    èoâ◊êîÉZÉbÉgÅ@èàóù                          *
      **************************************************
       SUR-RTN.
           COMPUTE  WK05-0911(I)  =  WJMST-1111(I)  -  WJMST-141(I).
           ADD   WK05-0911(I)    TO  WK05-092.
       SUR-EX.
           EXIT.
      ***
       COPY    LPMSG.
      ***

       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         JT235U.
      ******************************************************************
      *    FUNCTION.......  ïiñºéÛíçì˙ï Å@éÛíçécÉäÉXÉg                 *
      *    RELEASE DATE...  99/ 9/ 3         (REV.001)                 *
      ******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       SOURCE-COMPUTER.        NEAC-SYSTEM100.
       OBJECT-COMPUTER.        NEAC-SYSTEM100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT             PIC  X(02).
       77  WK0256ID             PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1          PIC  X(003).
           02  STN-NO2          PIC  X(003).
       01  W-FID.
           02  W-FID1           PIC  X(006) VALUE "WK0256".
           02  W-FID2           PIC  X(003).
       01  WK-AREA.
           02  SOEJI-AREA.
             03  ZZ             PIC  9(02).
           02  GMN-AREA.
             03  GMN-FROM.
               04  GMN-FYMD.
                 05  GMN-FYY    PIC  9(04).
                 05  GMN-FYYL  REDEFINES  GMN-FYY.
                   06  GMN-FYY1 PIC  9(02).
                   06  GMN-FYY2 PIC  9(02).
                 05  GMN-FMM    PIC  9(02).
                 05  GMN-FDD    PIC  9(02).
               04  GMN-FYMDL  REDEFINES  GMN-FYMD.
                 05  F          PIC  9(02).
                 05  GMN-FYMDS  PIC  9(06).
               04  GMN-FHCD     PIC  9(06).
             03  GMN-TO.
               04  GMN-TYMD.
                 05  GMN-TYY    PIC  9(04).
                 05  GMN-TYYL  REDEFINES  GMN-TYY.
                   06  GMN-TYY1 PIC  9(02).
                   06  GMN-TYY2 PIC  9(02).
                 05  GMN-TMM    PIC  9(02).
                 05  GMN-TDD    PIC  9(02).
               04  GMN-TYMDL  REDEFINES  GMN-TYMD.
                 05  F          PIC  9(02).
                 05  GMN-TYMDS  PIC  9(06).
               04  GMN-THCD     PIC  9(06).
             03  GMN-SEN        PIC  9(01).
             03  GMN-TYU        PIC  9(01).
             03  GMN-KAK        PIC  9(01).
             03  GMN-90         PIC  9(01).
       COPY  LWMSG.
      *
           COPY  LIBFDD.
           COPY  LJMST3.
           COPY  LJWNOK.
      *
       77  ESTAT                    PIC  X(002).
       77  RESU                     PIC  9(001).
       77  RET                      PIC  9(001) VALUE ZERO.
       77  USER_ID                  PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE          PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLEAR.
           02  CLR-GMN     PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-MIDAS.
           02  FILLER      PIC  X(28)    VALUE
             " ïiñºéÛíçì˙ï Å@éÛíçécÉäÉXÉg ".
           02  FILLER      PIC  X(08)    VALUE  "ÇeÇqÇnÇl".
           02  FILLER      PIC  X(04)    VALUE  "ÇsÇn".
           02  FILLER      PIC  X(04)    VALUE  "ïiñº".
           02  FILLER      PIC  X(06)    VALUE  "éÛíçì˙".
           02  FILLER      PIC  X(01)    VALUE    "/".
           02  FILLER      PIC  X(01)    VALUE    "/".
           02  FILLER      PIC  X(01)    VALUE    "/".
           02  FILLER      PIC  X(01)    VALUE    "/".
           02  FILLER      PIC  X(01)    VALUE    "0".
           02  FILLER      PIC  X(06)    VALUE  "ã≥Å@àÁ".
           02  FILLER      PIC  X(01)    VALUE    "1".
           02  FILLER      PIC  X(06)    VALUE  "àÍÅ@î ".
           02  FILLER      PIC  X(01)    VALUE    "9".
           02  FILLER      PIC  X(06)    VALUE  "ëSÅ@åè".
           02  FILLER      PIC  X(04)    VALUE  "ëIë".
           02  FILLER      PIC  X(03)    VALUE    "[ ]".
           02  FILLER      PIC  X(46)    VALUE
             "éÛÅ@íç = 0  ,  óaÅ@ÇË = 5  ,  éÊÇÊÇØ = 6   [ ]".
           02  FILLER      PIC  X(25)    VALUE
             "ämîFÅiOK=1,NO=9Åj--> ÿ¿∞›".
       01  DSP-ACTION.
           02  DSP-FROM.
               03  DSP-FHCD  PIC  9(06).
               03  DSP-FYY   PIC  9(02).
               03  DSP-FMM   PIC  9(02).
               03  DSP-FDD   PIC  9(02).
           02  DSP-TO.
               03  DSP-THCD  PIC  9(06).
               03  DSP-TYY   PIC  9(02).
               03  DSP-TMM   PIC  9(02).
               03  DSP-TDD   PIC  9(02).
           02  DSP-SEN       PIC  9(01).
           02  DSP-TYU       PIC  9(01).
           02  DSP-KAK       PIC  9(01).
       01  DSP-CLR.
           02  FILLER      PIC  X(06)    VALUE "      ".
           02  FILLER      PIC  X(02)    VALUE "  ".
           02  FILLER      PIC  X(02)    VALUE "  ".
           02  FILLER      PIC  X(02)    VALUE "  ".
           02  FILLER      PIC  X(06)    VALUE "      ".
           02  FILLER      PIC  X(02)    VALUE "  ".
           02  FILLER      PIC  X(02)    VALUE "  ".
           02  FILLER      PIC  X(02)    VALUE "  ".
           02  FILLER      PIC  X(01)    VALUE " ".
           02  FILLER      PIC  X(01)    VALUE " ".
           COPY  LSMSG.
           COPY LIBSCR.
       PROCEDURE           DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-CLEAR
       CALL "SD_Init" USING
           "DSP-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLR-GMN" "X" "1" "0" "12" " " "DSP-CLEAR" RETURNING RESU.
      *DSP-MIDAS
       CALL "SD_Init" USING
            "DSP-MIDAS" " " "0" "0" "153" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-MIDAS" "RX" "1" "23" "28" " " "DSP-MIDAS"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-MIDAS" "X" "7" "15" "8" "01DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-MIDAS" "X" "9" "15" "4" "02DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04DSP-MIDAS" "X" "5" "27" "4" "03DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05DSP-MIDAS" "X" "5" "36" "6" "04DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06DSP-MIDAS" "X" "7" "37" "1" "05DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "07DSP-MIDAS" "X" "7" "40" "1" "06DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "08DSP-MIDAS" "X" "9" "37" "1" "07DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "09DSP-MIDAS" "X" "9" "40" "1" "08DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "10DSP-MIDAS" "X" "5" "47" "1" "09DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "11DSP-MIDAS" "X" "5" "49" "6" "10DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "12DSP-MIDAS" "X" "7" "47" "1" "11DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "13DSP-MIDAS" "X" "7" "49" "6" "12DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "14DSP-MIDAS" "X" "9" "47" "1" "13DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "15DSP-MIDAS" "X" "9" "49" "6" "14DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "16DSP-MIDAS" "X" "9" "58" "4" "15DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "17DSP-MIDAS" "X" "9" "62" "3" "16DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "18DSP-MIDAS" "X" "13" "15" "46" "17DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "19DSP-MIDAS" "X" "23" "41" "25" "18DSP-MIDAS" " "
            RETURNING RESU.
      *DSP-ACTION
       CALL "SD_Init" USING
            "DSP-ACTION" " " "0" "0" "27" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-FROM" " " "0" "0" "12" " " "DSP-ACTION" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-FHCD" "9" "7" "26" "6" " " "DSP-FROM" RETURNING RESU.
       CALL "SD_Using" USING
            "DSP-FHCD" BY REFERENCE GMN-FHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-FYY" "9" "7" "35" "2" "DSP-FHCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "DSP-FYY" BY REFERENCE GMN-FYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-FMM" "9" "7" "38" "2" "DSP-FYY" " " RETURNING RESU.
       CALL "SD_Using" USING
            "DSP-FMM" BY REFERENCE GMN-FMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-FDD" "9" "7" "41" "2" "DSP-FMM" " " RETURNING RESU.
       CALL "SD_Using" USING
            "DSP-FDD" BY REFERENCE GMN-FDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TO" " " "0" "0" "12" "DSP-FROM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-THCD" "9" "9" "26" "6" " " "DSP-TO" RETURNING RESU.
       CALL "SD_Using" USING
            "DSP-THCD" BY REFERENCE GMN-THCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TYY" "9" "9" "35" "2" "DSP-THCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "DSP-TYY" BY REFERENCE GMN-TYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TMM" "9" "9" "38" "2" "DSP-TYY" " " RETURNING RESU.
       CALL "SD_Using" USING
            "DSP-TMM" BY REFERENCE GMN-TMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TDD" "9" "9" "41" "2" "DSP-TMM" " " RETURNING RESU.
       CALL "SD_Using" USING
            "DSP-TDD" BY REFERENCE GMN-TDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-SEN" "9" "9" "63" "1" "DSP-TO" " " RETURNING RESU.
       CALL "SD_Using" USING
            "DSP-SEN" BY REFERENCE GMN-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TYU" "9" "13" "59" "1" "DSP-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "DSP-TYU" BY REFERENCE GMN-TYU "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-KAK" "9" "23" "61" "1" "DSP-TYU" " " RETURNING RESU.
       CALL "SD_Using" USING
            "DSP-KAK" BY REFERENCE GMN-KAK "1" "0" RETURNING RESU.
      *DSP-CLR
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "26" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "7" "26" "6" " " "DSP-CLR" RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-CLR" "X" "7" "35" "2" "01DSP-CLR" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-CLR" "X" "7" "38" "2" "02DSP-CLR" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04DSP-CLR" "X" "7" "41" "2" "03DSP-CLR" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05DSP-CLR" "X" "9" "26" "6" "04DSP-CLR" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06DSP-CLR" "X" "9" "35" "2" "05DSP-CLR" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07DSP-CLR" "X" "9" "38" "2" "06DSP-CLR" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08DSP-CLR" "X" "9" "41" "2" "07DSP-CLR" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09DSP-CLR" "X" "9" "63" "1" "08DSP-CLR" " " RETURNING RESU.
       CALL "SD_Init" USING
           "10DSP-CLR" "X" "23" "61" "1" "09DSP-CLR" " " RETURNING RESU.
      *
           COPY LSMSG_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ******************************************************************
      *                    ÇlÇ`ÇhÇmÅ|ÇqÇsÇm                            *
      ******************************************************************
       MAIN-RTN.
           CALL "C3_Set_Jrcode" USING
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "SD_Output" USING "CLR-GMN" CLR-GMN "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MIDAS" DSP-MIDAS "p" RETURNING RESU.
           COPY  LIBCPR.
       MAIN-FHCD.
           CALL "SD_Accept" USING BY REFERENCE DSP-FHCD "DSP-FHCD"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO    MAIN-END
           END-IF
           IF  ESTAT  NOT =   "01" AND "06" AND "00"
               GO  TO    MAIN-FHCD
           END-IF.
       MAIN-THCD.
           CALL "SD_Accept" USING BY REFERENCE DSP-THCD "DSP-THCD"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO    MAIN-FHCD
           END-IF
           IF  ESTAT  NOT =   "01" AND "06" AND "00"
               GO  TO    MAIN-THCD
           END-IF
           IF  GMN-FHCD  >   GMN-THCD
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO    MAIN-FHCD
           END-IF.
       MAIN-FYY.
           CALL "SD_Accept" USING BY REFERENCE DSP-FYY "DSP-FYY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO    MAIN-THCD
           END-IF
           IF  ESTAT  NOT =   "01" AND "06" AND "00"
               GO  TO    MAIN-FYY
           END-IF.
       MAIN-FMM.
           CALL "SD_Accept" USING BY REFERENCE DSP-FMM "DSP-FMM" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO    MAIN-FYY
           END-IF
           IF  ESTAT  NOT =   "01" AND "06" AND "00"
               GO  TO    MAIN-FMM
           END-IF
           IF  GMN-FMM    =   ZERO
               IF  GMN-FYY2      =   ZERO
                   GO  TO  MAIN-FDD
               END-IF
           END-IF
           IF  GMN-FMM    <  1   OR   >  12
               GO  TO  MAIN-FMM
           END-IF.
       MAIN-FDD.
           CALL "SD_Accept" USING BY REFERENCE DSP-FDD "DSP-FDD" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO    MAIN-FMM
           END-IF
           IF  ESTAT  NOT =   "01" AND "06" AND "00"
               GO  TO    MAIN-FDD
           END-IF
           IF  GMN-FYMDS     =   ZERO
               MOVE    ZERO      TO  GMN-FYMD
               GO  TO  MAIN-TYY
           END-IF
           IF  GMN-FDD    <  1   OR   >  31
               GO  TO  MAIN-FDD
           END-IF
           MOVE  ZERO       TO  GMN-FYY1.
           IF  GMN-FYY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO GMN-FYY
           END-IF
           IF  GMN-FYY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO GMN-FYY
           END-IF.
       MAIN-TYY.
           CALL "SD_Accept" USING BY REFERENCE DSP-TYY "DSP-TYY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO    MAIN-FDD
           END-IF
           IF  ESTAT  NOT =   "01" AND "06" AND "00"
               GO  TO    MAIN-TYY
           END-IF.
       MAIN-TMM.
           CALL "SD_Accept" USING BY REFERENCE DSP-TMM "DSP-TMM" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO    MAIN-TYY
           END-IF
           IF  ESTAT  NOT =   "01" AND "06" AND "00"
               GO  TO    MAIN-TMM
           END-IF
           IF  GMN-TMM    =   99
               IF  GMN-TYY2      =   99
                   GO  TO  MAIN-TDD
               END-IF
           END-IF
           IF  GMN-TMM    <  1   OR   >  12
               GO  TO  MAIN-TMM
           END-IF.
       MAIN-TDD.
           CALL "SD_Accept" USING BY REFERENCE DSP-TDD "DSP-TDD" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO    MAIN-TMM
           END-IF
           IF  ESTAT  NOT =   "01" AND "06" AND "00"
               GO  TO    MAIN-TDD
           END-IF
           IF  GMN-TYMDS     =   999999
               MOVE    99999999  TO  GMN-TYMD
               GO  TO  MAIN-SEN
           END-IF
           IF  GMN-TDD    <  1   OR   >  31
               GO  TO  MAIN-TDD
           END-IF
           MOVE  ZERO       TO  GMN-TYY1.
           IF  GMN-TYY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO GMN-TYY
           END-IF
           IF  GMN-TYY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO GMN-TYY
           END-IF
           IF  GMN-FYMD  >   GMN-TYMD
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO    MAIN-FYY
           END-IF.
       MAIN-SEN.
           CALL "SD_Accept" USING BY REFERENCE DSP-SEN "DSP-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO    MAIN-TDD
           END-IF
           IF  ESTAT  NOT =   "01"   AND   "06"
               GO  TO    MAIN-SEN
           END-IF
           IF  GMN-SEN  NOT =  0  AND  1  AND  9
               GO  TO    MAIN-SEN
           END-IF.
       MAIN-TYU.
           CALL "SD_Accept" USING BY REFERENCE DSP-TYU "DSP-TYU" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO    MAIN-SEN
           END-IF
           IF  ESTAT  NOT =   "01"   AND   "06"
               GO  TO    MAIN-TYU
           END-IF
           IF  GMN-TYU  NOT =  0  AND  5  AND  6
               GO  TO    MAIN-TYU
           END-IF.
       MAIN-KAK.
           CALL "SD_Accept" USING BY REFERENCE DSP-KAK "DSP-KAK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO    MAIN-TYU
           END-IF
           IF  ESTAT  NOT =   "01"   AND   "06"
               GO  TO    MAIN-KAK
           END-IF
           IF  GMN-KAK =   9
               CALL "SD_Output" USING
                "CAN-01" CAN-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DSP-CLR" DSP-CLR "p" RETURNING RESU
               GO  TO    MAIN-FYY
           END-IF
           IF  GMN-KAK NOT =  1
               GO  TO    MAIN-KAK
           END-IF
           PERFORM   OPEN-RTN     THRU         OPEN-EX.
           PERFORM   READ-RTN     THRU         READ-EX.
           PERFORM   END-RTN      THRU         END-EX.
       MAIN-END.
           CALL "DB_Close".
           STOP      RUN.
      ******************************************************************
      *                    ÇnÇoÇdÇmÅ|ÇqÇsÇm                            *
      ******************************************************************
       OPEN-RTN.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JWNOK_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JMST3_PNAME1 "SHARED" BY REFERENCE JMST3_IDLST "1"
            "JMST3-KEY" BY REFERENCE JMST3-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" JWNOK_PNAME1 "EXCLUSIVE" BY REFERENCE
            JWNOK_IDLST "0".
       OPEN-EX.
           EXIT.
      ******************************************************************
      *                    ÇqÇdÇ`ÇcÅ|ÇqÇsÇm                            *
      ******************************************************************
       READ-RTN.
           MOVE  SPACE     TO  JMST3-KEY.
           MOVE GMN-FHCD   TO  JMST3-03.
      *           START     JMST3      KEY  NOT  <  JMST3-KEY   INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            JMST3_PNAME1 "JMST3-KEY" " NOT < " JMST3-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  READ-EX
           END-IF.
       READ-NXT.
      *           READ      JMST3 NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST3_PNAME1 BY REFERENCE JMST3-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  READ-EX
           END-IF
      **
           IF  JMST3-01  =  1
               GO  TO  READ-NXT
           END-IF
           IF  JMST3-01  NOT =  GMN-TYU
               GO  TO  READ-NXT
           END-IF
           MOVE      JMST3-90     TO   GMN-90.
           IF  GMN-90           =  2
               MOVE      1            TO   GMN-90
           END-IF
           IF  GMN-SEN      NOT =  9
               IF  GMN-SEN  NOT =  GMN-90
                   GO  TO  READ-NXT
               END-IF
           END-IF
           IF  JMST3-03  >  GMN-THCD
               GO  TO  READ-EX
           END-IF
           IF  JMST3-02  <  GMN-FYMD
               GO  TO  READ-NXT
           END-IF
           IF  JMST3-02  >  GMN-TYMD
               GO  TO  READ-NXT
           END-IF
      **
           MOVE      JMST3-02     TO   JWNOK-01.
           MOVE      JMST3-04     TO   JWNOK-021.
           MOVE      JMST3-10     TO   JWNOK-022.
           MOVE      JMST3-03     TO   JWNOK-03.
           MOVE      JMST3-07     TO   JWNOK-04.
           MOVE      JMST3-09     TO   JWNOK-05.
           MOVE      JMST3-01     TO   JWNOK-06.
           MOVE      JMST3-08     TO   JWNOK-09.
           MOVE      1            TO   ZZ.
       READ-LOOP.
           COMPUTE  JWNOK-0711(ZZ) = JMST3-1111(ZZ)
                                   - JMST3-1211(ZZ)
                                   - JMST3-141 (ZZ).
           IF  ZZ   =    10
               GO   TO   READ-END
           END-IF
           ADD       1            TO     ZZ.
           GO    TO    READ-LOOP.
       READ-END.
           IF  ZERO         =   JWNOK-0711(01)   AND  JWNOK-0711(02)
                           AND  JWNOK-0711(03)   AND  JWNOK-0711(04)
                           AND  JWNOK-0711(05)   AND  JWNOK-0711(06)
                           AND  JWNOK-0711(07)   AND  JWNOK-0711(08)
                           AND  JWNOK-0711(09)   AND  JWNOK-0711(10)
               GO  TO  READ-NXT
           END-IF
      *           WRITE     JWNOK-R.
      *//////////////
           CALL "DB_Insert" USING
            JWNOK_PNAME1 JWNOK_LNAME JWNOK-R RETURNING RET.
           IF  ERR-STAT   NOT =     "00"
               MOVE  "W"           TO  ERR-M
               MOVE  "JWNOK"       TO  ERR-F
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
           CALL "C3_Set_Jrcode" USING
            USER_ID BY REFERENCE COMPLETION_CODE  000.
           GO    TO    READ-NXT.
       READ-EX.
           EXIT.
      ******************************************************************
      *                    ÇdÇmÇcÅ|ÇqÇsÇm                              *
      ******************************************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JMST3_IDLST JMST3_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JWNOK_IDLST JWNOK_PNAME1.
       END-EX.
           EXIT.
       COPY  LPMSG.

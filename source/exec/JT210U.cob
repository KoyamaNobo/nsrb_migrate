       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         JT210U.
      ******************************************************************
      *    <<REMARKS>>                                                 *
      *    FUNCTION.......  î[ä˙ï èoâ◊ó\íËï\                           *
      *    AUTHOR.........  Y.KATOH                                    *
      *    COMPILE MODE...  NORMAL                                     *
      *    SCREEN.........  XXXXX                                      *
      *    RELEASE DATE...  62/08/17         (REV.001)                 *
      ******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       SOURCE-COMPUTER.        NEAC-SYSTEM100.
       OBJECT-COMPUTER.        NEAC-SYSTEM100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT                  PIC  X(02).
       77  WK0256ID                  PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1               PIC  X(003).
           02  STN-NO2               PIC  X(003).
       01  W-FID.
           02  W-FID1                PIC  X(006) VALUE "WK0256".
           02  W-FID2                PIC  X(003).
       01  UPD-SW                    PIC  X(02).
       01  WK-AREA.
           02  SOEJI-AREA.
               03  ZZ                PIC  9(02).
           02  GMN-AREA.
               03  GMN-FYMD.
                   04  GMN-FYY       PIC  9(04).
                   04  GMN-FYYL  REDEFINES  GMN-FYY.
                       05  GMN-FYY1  PIC  9(02).
                       05  GMN-FYY2  PIC  9(02).
                   04  GMN-FMM       PIC  9(02).
                   04  GMN-FDD       PIC  9(02).
               03  GMN-FYMDL  REDEFINES  GMN-FYMD.
                   04  F             PIC  9(02).
                   04  GMN-FYMDS     PIC  9(06).
               03  GMN-TYMD.
                   04  GMN-TYY       PIC  9(04).
                   04  GMN-TYYL  REDEFINES  GMN-TYY.
                       05  GMN-TYY1  PIC  9(02).
                       05  GMN-TYY2  PIC  9(02).
                   04  GMN-TMM       PIC  9(02).
                   04  GMN-TDD       PIC  9(02).
               03  GMN-TYMDL  REDEFINES  GMN-TYMD.
                   04  F             PIC  9(02).
                   04  GMN-TYMDS     PIC  9(06).
               03  GMN-FTOK          PIC  9(04).
               03  GMN-TTOK          PIC  9(04).
               03  GMN-FHIN          PIC  9(06).
               03  GMN-THIN          PIC  9(06).
               03  GMN-SEN           PIC  9(01).
               03  GMN-KAK           PIC  9(01).
       01  W-YMD.
           02  W-YD             PIC  9(04).
           02  W-MDD            PIC  9(04).
       COPY  LWMSG.
      *
           COPY  LIBFDD.
           COPY  LJMST1.
           COPY  LJWNOK.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLEAR.
           02  CLR-GMN PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-MIDAS.
           02  FILLER  PIC  X(26) VALUE
             " éÛíçëºî[ä˙ï Å@èoâ◊ó\íËï\ ".
           02  FILLER  PIC  X(04) VALUE  "î[ä˙".
           02  FILLER  PIC  X(06) VALUE  "ìæà”êÊ".
           02  FILLER  PIC  X(04) VALUE    "∫∞ƒﬁ".
           02  FILLER  PIC  X(04) VALUE  "ïiñº".
           02  FILLER  PIC  X(04) VALUE    "∫∞ƒﬁ".
           02  FILLER  PIC  X(01) VALUE    "0".
           02  FILLER  PIC  X(06) VALUE  "ã≥Å@àÁ".
           02  FILLER  PIC  X(08) VALUE  "ÇeÇqÇnÇl".
           02  FILLER  PIC  X(01) VALUE    "/".
           02  FILLER  PIC  X(01) VALUE    "/".
           02  FILLER  PIC  X(01) VALUE    "1".
           02  FILLER  PIC  X(06) VALUE  "àÍÅ@î ".
           02  FILLER  PIC  X(04) VALUE  "ÇsÇn".
           02  FILLER  PIC  X(01) VALUE    "/".
           02  FILLER  PIC  X(01) VALUE    "/".
           02  FILLER  PIC  X(01) VALUE    "9".
           02  FILLER  PIC  X(06) VALUE  "ëSÅ@åè".
           02  FILLER  PIC  X(04) VALUE  "ëIë".
           02  FILLER  PIC  X(03) VALUE    "[ ]".
           02  FILLER  PIC  X(25) VALUE
             "ämîFÅiOK=1,NO=9Åj--> ÿ¿∞›".
       01  DSP-ACTION.
           02  DSP-FROM.
               03  DSP-FYY          PIC  9(02).
               03  DSP-FMM          PIC  9(02).
               03  DSP-FDD          PIC  9(02).
               03  DSP-FTOK         PIC  9(04).
               03  DSP-FHIN         PIC  9(06).
           02  DSP-TO.
               03  DSP-TYY          PIC  9(02).
               03  DSP-TMM          PIC  9(02).
               03  DSP-TDD          PIC  9(02).
               03  DSP-TTOK         PIC  9(04).
               03  DSP-THIN         PIC  9(06).
           02  DSP-SEN     PIC  9(01).
           02  DSP-KAK     PIC  9(01).
       01  DSP-CLR.
           02  FILLER.
               03  FILLER           PIC  X(02)  VALUE "  ".
               03  FILLER           PIC  X(02)  VALUE "  ".
               03  FILLER           PIC  X(02)  VALUE "  ".
               03  FILLER           PIC  X(04)  VALUE "    ".
               03  FILLER           PIC  X(06)  VALUE "      ".
           02  FILLER.
               03  FILLER           PIC  X(02)  VALUE "  ".
               03  FILLER           PIC  X(02)  VALUE "  ".
               03  FILLER           PIC  X(02)  VALUE "  ".
               03  FILLER           PIC  X(04)  VALUE "    ".
               03  FILLER           PIC  X(06)  VALUE "      ".
           02  FILLER      PIC  X(01)  VALUE " ".
           02  FILLER      PIC  X(01)  VALUE " ".
           COPY  LSMSG.
           COPY  LIBSCR.
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
            "DSP-MIDAS" " " "0" "0" "117" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MIDAS" "RX" "1" "27" "26" " " "DSP-MIDAS"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-MIDAS" "X" "5" "22" "4" "01DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-MIDAS" "X" "5" "28" "6" "02DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-MIDAS" "X" "5" "34" "4" "03DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-MIDAS" "X" "5" "39" "4" "04DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-MIDAS" "X" "5" "43" "4" "05DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-MIDAS" "X" "5" "53" "1" "06DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-MIDAS" "X" "5" "56" "6" "07DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-MIDAS" "X" "7" "10" "8" "08DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-MIDAS" "X" "7" "22" "1" "09DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "11DSP-MIDAS" "X" "7" "25" "1" "10DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "12DSP-MIDAS" "X" "7" "53" "1" "11DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "13DSP-MIDAS" "X" "7" "56" "6" "12DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "14DSP-MIDAS" "X" "9" "10" "4" "13DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "15DSP-MIDAS" "X" "9" "22" "1" "14DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "16DSP-MIDAS" "X" "9" "25" "1" "15DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "17DSP-MIDAS" "X" "9" "53" "1" "16DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "18DSP-MIDAS" "X" "9" "56" "6" "17DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "19DSP-MIDAS" "X" "9" "64" "4" "18DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "20DSP-MIDAS" "X" "9" "68" "3" "19DSP-MIDAS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "21DSP-MIDAS" "X" "23" "41" "25" "20DSP-MIDAS" " "
            RETURNING RESU.
      *DSP-ACTION
       CALL "SD_Init" USING 
            "DSP-ACTION" " " "0" "0" "34" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-FROM" " " "7" "0" "16" " " "DSP-ACTION"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-FYY" "9" "7" "20" "2" " " "DSP-FROM" RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-FYY" BY REFERENCE GMN-FYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-FMM" "9" "7" "23" "2" "DSP-FYY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-FMM" BY REFERENCE GMN-FMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-FDD" "9" "7" "26" "2" "DSP-FMM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-FDD" BY REFERENCE GMN-FDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-FTOK" "9" "7" "32" "4" "DSP-FDD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-FTOK" BY REFERENCE GMN-FTOK "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-FHIN" "9" "7" "40" "6" "DSP-FTOK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-FHIN" BY REFERENCE GMN-FHIN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TO" " " "9" "0" "16" "DSP-FROM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TYY" "9" "9" "20" "2" " " "DSP-TO" RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-TYY" BY REFERENCE GMN-TYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TMM" "9" "9" "23" "2" "DSP-TYY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-TMM" BY REFERENCE GMN-TMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TDD" "9" "9" "26" "2" "DSP-TMM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-TDD" BY REFERENCE GMN-TDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TTOK" "9" "9" "32" "4" "DSP-TDD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-TTOK" BY REFERENCE GMN-TTOK "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-THIN" "9" "9" "40" "6" "DSP-TTOK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-THIN" BY REFERENCE GMN-THIN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SEN" "9" "9" "69" "1" "DSP-TO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-SEN" BY REFERENCE GMN-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KAK" "9" "23" "61" "1" "DSP-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-KAK" BY REFERENCE GMN-KAK "1" "0" RETURNING RESU.
      *DSP-CLR
       CALL "SD_Init" USING 
            "DSP-CLR" " " "0" "0" "34" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-CLR" " " "7" "0" "16" " " "DSP-CLR" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101DSP-CLR" "X" "7" "20" "2" " " "01DSP-CLR"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201DSP-CLR" "X" "7" "23" "2" "0101DSP-CLR" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0301DSP-CLR" "X" "7" "26" "2" "0201DSP-CLR" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0401DSP-CLR" "X" "7" "32" "4" "0301DSP-CLR" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0501DSP-CLR" "X" "7" "40" "6" "0401DSP-CLR" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-CLR" " " "9" "0" "16" "01DSP-CLR" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-CLR" "X" "9" "20" "2" " " "02DSP-CLR"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DSP-CLR" "X" "9" "23" "2" "0102DSP-CLR" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302DSP-CLR" "X" "9" "26" "2" "0202DSP-CLR" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402DSP-CLR" "X" "9" "32" "4" "0302DSP-CLR" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0502DSP-CLR" "X" "9" "40" "6" "0402DSP-CLR" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-CLR" "X" "9" "69" "1" "02DSP-CLR" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-CLR" "X" "23" "61" "1" "03DSP-CLR" " "
            RETURNING RESU.
      *
           COPY LSMSG_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
      ******************************************************************
      *                    ÇlÇ`ÇhÇmÅ|ÇqÇsÇm                            *
      ******************************************************************
       MAIN-RTN.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           COPY  LIBCPR.
           CALL "SD_Output" USING "CLR-GMN" CLR-GMN "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MIDAS" DSP-MIDAS "p" RETURNING RESU.
       MAIN-FYY.
           CALL "SD_Accept" USING BY REFERENCE DSP-FYY "DSP-FYY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO    MAIN-END
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
               IF  GMN-FYY2    =  ZERO
                   GO  TO  MAIN-FDD
               END-IF
           END-IF
           IF  GMN-FMM    <   1   OR   >   12
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
           IF  GMN-FYMDS  =   ZERO
               MOVE  ZERO       TO   GMN-FYMD
               GO  TO  MAIN-TYY
           END-IF
           IF  GMN-FDD    <   1   OR   >   31
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
               IF  GMN-TYY2    =  99
                   GO  TO  MAIN-TDD
               END-IF
           END-IF
           IF  GMN-TMM    <   1   OR   >   12
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
           IF  GMN-TYMDS  =   999999
               MOVE  99999999   TO   GMN-TYMD
               GO  TO  MAIN-FTOK
           END-IF
           IF  GMN-TDD    <   1   OR   >   31
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
       MAIN-FTOK.
           CALL "SD_Accept" USING BY REFERENCE DSP-FTOK "DSP-FTOK"
            "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO    MAIN-TDD
           END-IF
           IF  ESTAT  NOT =   "01" AND "06" AND "00"
               GO  TO    MAIN-FTOK
           END-IF.
       MAIN-TTOK.
           CALL "SD_Accept" USING BY REFERENCE DSP-TTOK "DSP-TTOK"
            "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO    MAIN-FTOK
           END-IF
           IF  ESTAT  NOT =   "01" AND "06" AND "00"
               GO  TO    MAIN-TTOK
           END-IF
           IF  GMN-FTOK  >   GMN-TTOK
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO    MAIN-FTOK
           END-IF.
       MAIN-FHIN.
           CALL "SD_Accept" USING BY REFERENCE DSP-FHIN "DSP-FHIN"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO    MAIN-TTOK
           END-IF
           IF  ESTAT  NOT =   "01" AND "06" AND "00"
               GO  TO    MAIN-FHIN
           END-IF.
       MAIN-THIN.
           CALL "SD_Accept" USING BY REFERENCE DSP-THIN "DSP-THIN"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO    MAIN-FHIN
           END-IF
           IF  ESTAT  NOT =   "01" AND "06" AND "00"
               GO  TO    MAIN-THIN
           END-IF
           IF  GMN-FHIN  >   GMN-THIN
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO    MAIN-FHIN
           END-IF.
       MAIN-SEN.
           CALL "SD_Accept" USING BY REFERENCE DSP-SEN "DSP-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO    MAIN-THIN
           END-IF
           IF  ESTAT  NOT =   "01" AND "06" AND "00"
               GO  TO    MAIN-SEN
           END-IF
           IF  GMN-SEN    NOT =  0  AND  1  AND  9
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO    MAIN-SEN
           END-IF.
       MAIN-KAK.
           CALL "SD_Accept" USING BY REFERENCE DSP-KAK "DSP-KAK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO    MAIN-SEN
           END-IF
           IF  ESTAT  NOT =   "01"   AND   "06"
               GO  TO    MAIN-KAK
           END-IF
           IF  GMN-KAK =   9
               CALL "SD_Output" USING "CAN-01" CAN-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DSP-CLR" DSP-CLR "p" RETURNING RESU
               GO  TO    MAIN-FYY
           END-IF
           IF  GMN-KAK NOT =  1
               GO  TO    MAIN-KAK
           END-IF
      *
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
            "INPUT" JMST1_PNAME1 "SHARED" BY REFERENCE JMST1_IDLST "1"
            "JMST1-KEY1" BY REFERENCE JMST1-KEY1.
           CALL "DB_F_Open" USING
            "INPUT" JWNOK_PNAME1 "EXCLUSIVE" BY REFERENCE
            JWNOK_IDLST "0".
       OPEN-EX.
           EXIT.
      ******************************************************************
      *                    ÇqÇdÇ`ÇcÅ|ÇqÇsÇm                            *
      ******************************************************************
       READ-RTN.
      *           READ      JMST1 NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST1_PNAME1 BY REFERENCE JMST1-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  READ-EX
           END-IF
      **
           IF  JMST1-01  =  1
               GO  TO  READ-RTN
           END-IF
           IF  JMST1-06  =  ZERO
               GO  TO  READ-RTN
           END-IF
           IF  JMST1-06  <  GMN-FYMD
               GO  TO  READ-RTN
           END-IF
           IF  JMST1-06  >  GMN-TYMD
               GO  TO  READ-RTN
           END-IF
           IF  JMST1-04  <  GMN-FTOK  OR  >  GMN-TTOK
               GO  TO  READ-RTN
           END-IF
           IF  JMST1-05  <  GMN-FHIN  OR  >  GMN-THIN
               GO  TO  READ-RTN
           END-IF
           IF  GMN-SEN   =  0
               IF  JMST1-07   <   100000   OR  >  199999
                   GO  TO  READ-RTN
               END-IF
           END-IF
           IF  GMN-SEN   =  1
               IF  JMST1-07   <   300000   OR  >  599999
                   GO  TO  READ-RTN
               END-IF
           END-IF
      **
           MOVE      JMST1-06     TO   JWNOK-01.
           MOVE      JMST1-04     TO   JWNOK-021.
           MOVE      JMST1-10     TO   JWNOK-022.
           MOVE      JMST1-03     TO   JWNOK-03.
           MOVE      JMST1-07     TO   JWNOK-04.
           MOVE      JMST1-09     TO   JWNOK-05.
           MOVE      JMST1-01     TO   JWNOK-06.
           MOVE      JMST1-02     TO   W-YMD.
           MOVE      W-MDD        TO   JWNOK-08.
           MOVE      JMST1-08     TO   JWNOK-09.
           MOVE      1            TO   ZZ.
           MOVE      "OF"         TO   UPD-SW.
       READ-LOOP.
           COMPUTE  JWNOK-0711(ZZ) = JMST1-1111(ZZ)
                                   - JMST1-1211(ZZ)
                                   - JMST1-141 (ZZ)
                                   - JMST1-151 (ZZ).
           IF  JWNOK-0711(ZZ) > ZERO
               MOVE  "ON"  TO  UPD-SW
           END-IF
           IF  ZZ   =    10
               GO   TO   READ-END
           END-IF
           ADD       1            TO     ZZ.
           GO    TO    READ-LOOP.
       READ-END.
           IF  UPD-SW  =  "OF"
               GO  TO  READ-RTN
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
           GO    TO    READ-RTN.
       READ-EX.
           EXIT.
      ******************************************************************
      *                    ÇdÇmÇcÅ|ÇqÇsÇm                              *
      ******************************************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JMST1_IDLST JMST1_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JWNOK_IDLST JWNOK_PNAME1.
       END-EX.
           EXIT.
       COPY  LPMSG.

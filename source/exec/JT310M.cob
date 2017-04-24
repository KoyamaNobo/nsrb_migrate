       IDENTIFICATION       DIVISION.
       PROGRAM-ID.          JT310M.
       AUTHOR.              I.NAKANISHI
      ******************************************************************
      *    FUNCTION.......  ÉRÉìÉgÉçÅ[ÉãÉtÉ@ÉCÉãÉÅÉìÉeÉiÉìÉX           *
      *    COMPILE MODE...  COBOL                                      *
      *    PRINTER TYPE...  JIPS                                       *
      *    SCREEN.........  SJ310M                                     *
      *    RELEASE DATE...  62/08/06                                   *
      *    UPDATE      ...  91/11/20                                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM150.
       OBJECT-COMPUTER. SYSTEM150.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ACT                     PIC  9       VALUE   0.
       77  OKC                     PIC  9(01).
       77  ERR-STAT                PIC  X(02).
       01  N-06                    PIC  N(06)  VALUE  ALL "Å@".
       01  W-AREA.
           02  W-ID                PIC  9(01).
           02  W-KBN               PIC  9(01).
           02  W-DNO.
               03  W-DNO1          PIC  9(06).
               03  W-DNO2          PIC  9(06).
               03  W-DNO3          PIC  9(06).
           02  W-SYB.
               03  W-SYB1          PIC  9(06).
               03  W-SYB1R  REDEFINES   W-SYB1.
                   04  W-YY1       PIC  9(02).
                   04  W-MM1       PIC  9(02).
                   04  W-DD1       PIC  9(02).
               03  W-SYB2          PIC  9(06).
               03  W-SYB2R  REDEFINES   W-SYB2.
                   04  W-YY2       PIC  9(02).
                   04  W-MM2       PIC  9(02).
                   04  W-DD2       PIC  9(02).
               03  W-SYB3          PIC  9(06).
               03  W-SYB3R  REDEFINES   W-SYB3.
                   04  W-YY3       PIC  9(02).
                   04  W-MM3       PIC  9(02).
                   04  W-DD3       PIC  9(02).
           02  W-JKB.
               03  W-JKB1          PIC  9(01).
               03  W-JKB2          PIC  9(01).
               03  W-JKB3          PIC  9(01).
           02  W-CODE              PIC  9(01).
           02  W-MEI               PIC  N(06).
           02  W-TEL               PIC  X(13).
           02  W-KAN               PIC  9(01).
       COPY        LWMSG.
      **
           COPY    L-JCON.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLE-01.
           02  CLE-01A  PIC  X(12) VALUE "CLEAR SCREEN".
       01  CLE-02.
           02  CLE-021  PIC X(01) VALUE " ".
           02  CLE-03.
               03  FILLER   PIC  X(01)  VALUE  " ".
               03  CLE-031  PIC  X(06)  VALUE  " ".
               03  CLE-032  PIC  X(06)  VALUE  " ".
               03  CLE-032A PIC  X(06)  VALUE  " ".
           02  CLE-03A.
               03  CLE-033  PIC  X(06)  VALUE  " ".
               03  CLE-033A PIC  X(06)  VALUE  " ".
               03  CLE-033B PIC  X(06)  VALUE  " ".
               03  CLE-034  PIC  X(01)  VALUE  " ".
               03  CLE-034A PIC  X(01)  VALUE  " ".
               03  CLE-034B PIC  X(01)  VALUE  " ".
           02  CLE-04.
               03  FILLER   PIC  X(01)  VALUE  " ".
               03  FILLER   PIC  N(06).
               03  CLE-041  PIC  X(13)  VALUE  " ".
               03  CLE-042  PIC  X(01)  VALUE  " ".
       01  ACEP-AREA.
           02  ACEP-ACT   PIC  9 .
           02  ACEP-OKC   PIC  9 .
           02  ACEP-ID    PIC  9 .
           02  ACEP-KBN   PIC  9 .
           02  ACEP-DNO.
               03  ACEP-DNO1  PIC  9(06).
               03  ACEP-DNO2  PIC  9(06).
               03  ACEP-DNO3  PIC  9(06).
           02  ACEP-SYB.
               03  ACEP-SYB1  PIC  9(06).
               03  ACEP-SYB2  PIC  9(06).
               03  ACEP-SYB3  PIC  9(06).
           02  ACEP-JKB.
               03  ACEP-JKB1  PIC  9(01).
               03  ACEP-JKB2  PIC  9(01).
               03  ACEP-JKB3  PIC  9(01).
           02  ACEP-CODE  PIC  9(01).
           02  ACEP-MEI   PIC  N(06).
           02  ACEP-TEL   PIC  X(13).
           02  ACEP-KAN   PIC  9(01).
      *
       COPY        LSMSG.
      **
       PROCEDURE      DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLE-01
       CALL "SD_Init" USING
           "CLE-01" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLE-01A" "X" "1" "0" "12" " " "CLE-01" RETURNING RESU.
      *CLE-02
       CALL "SD_Init" USING 
            "CLE-02" " " "0" "0" "68" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-021" "X" "5" "7" "1" " " "CLE-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-03" " " "0" "0" "19" "CLE-021" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLE-03" "X" "5" "21" "1" " " "CLE-03" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-031" "X" "7" "38" "6" "01CLE-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-032" "X" "7" "45" "6" "CLE-031" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-032A" "X" "7" "52" "6" "CLE-032" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-03A" " " "0" "0" "21" "CLE-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-033" "X" "11" "38" "6" " " "CLE-03A" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-033A" "X" "11" "45" "6" "CLE-033" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-033B" "X" "11" "52" "6" "CLE-033A" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-034" "X" "11" "62" "1" "CLE-033B" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-034A" "X" "11" "69" "1" "CLE-034" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-034B" "X" "11" "76" "1" "CLE-034A" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-04" " " "0" "0" "27" "CLE-03A" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLE-04" "X" "15" "40" "1" " " "CLE-04" RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLE-04" "N" "15" "43" "12" "01CLE-04" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02CLE-04" BY REFERENCE N-06 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-041" "X" "15" "56" "13" "02CLE-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-042" "X" "15" "72" "1" "CLE-041" " " RETURNING RESU.
      *ACEP-AREA
       CALL "SD_Init" USING 
            "ACEP-AREA" " " "0" "0" "70" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-ACT" "9" "3" "50" "1" " " "ACEP-AREA" RETURNING RESU.
       CALL "SD_Into" USING 
            "ACEP-ACT" BY REFERENCE ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-OKC" "9" "23" "65" "1" "ACEP-ACT" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACEP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-ID" "9" "5" "7" "1" "ACEP-OKC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-ID" BY REFERENCE W-ID "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-KBN" "9" "5" "21" "1" "ACEP-ID" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-KBN" BY REFERENCE W-KBN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-DNO" " " "7" "0" "18" "ACEP-KBN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-DNO1" "9" "7" "38" "6" " " "ACEP-DNO" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-DNO1" BY REFERENCE W-DNO1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-DNO2" "9" "7" "45" "6" "ACEP-DNO1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-DNO2" BY REFERENCE W-DNO2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-DNO3" "9" "7" "52" "6" "ACEP-DNO2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-DNO3" BY REFERENCE W-DNO3 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-SYB" " " "11" "0" "18" "ACEP-DNO" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-SYB1" "9" "11" "38" "6" " " "ACEP-SYB" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-SYB1" BY REFERENCE W-SYB1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "ACEP-SYB2" "9" "11" "45" "6" "ACEP-SYB1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-SYB2" BY REFERENCE W-SYB2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "ACEP-SYB3" "9" "11" "52" "6" "ACEP-SYB2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-SYB3" BY REFERENCE W-SYB3 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-JKB" " " "11" "0" "3" "ACEP-SYB" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-JKB1" "9" "11" "62" "1" " " "ACEP-JKB" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-JKB1" BY REFERENCE W-JKB1 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "ACEP-JKB2" "9" "11" "69" "1" "ACEP-JKB1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-JKB2" BY REFERENCE W-JKB2 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "ACEP-JKB3" "9" "11" "76" "1" "ACEP-JKB2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-JKB3" BY REFERENCE W-JKB3 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "ACEP-CODE" "9" "15" "40" "1" "ACEP-JKB" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-CODE" BY REFERENCE W-CODE "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "ACEP-MEI" "N" "15" "43" "12" "ACEP-CODE" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-MEI" BY REFERENCE W-MEI "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-TEL" "X" "15" "56" "13" "ACEP-MEI" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-TEL" BY REFERENCE W-TEL "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-KAN" "9" "15" "72" "1" "ACEP-TEL" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-KAN" BY REFERENCE W-KAN "1" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       HAJIME.
           CALL "SD_Screen_Output" USING "SJ310M" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
       MAIN-RTN.
       MR010.
           CALL "SD_Accept" USING BY REFERENCE ACEP-ACT "ACEP-ACT"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ACT     =      9
               GO  TO   MR999
           END-IF
           IF  ESTAT  NOT  =  "01"
               GO  TO  MR010
           END-IF
           CALL "SD_Output" USING
            "ACEP-ACT" ACEP-ACT "p" RETURNING RESU.
           IF  (ACT   <  1)  OR  (ACT  >  3)
               GO  TO  MR010
           END-IF
           PERFORM   INP-RTN  THRU  INP-EX.
           CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLE-03" CLE-03 "p" RETURNING RESU.
           GO  TO  MR010.
       MR999.
           PERFORM     END-RTN   THRU   END-EX.
       OWARI.
           CALL "DB_Close".
           STOP  RUN.
       MAIN-EX.
           EXIT.
      **
      **
      **********************************
      *    âÊñ èàóùÅ@Å@Å@Å@Å@          *
      **********************************
       INP-RTN.
           MOVE  SPACE   TO  W-AREA.
           INITIALIZE        W-AREA.
           CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU.
       INP-010.
           CALL "SD_Accept" USING BY REFERENCE ACEP-ID "ACEP-ID"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INP-EX
           END-IF
           IF  ESTAT  NOT   =  "01"  AND  "06"
               GO  TO  INP-010
           END-IF
           IF  W-ID   NOT   =  1  AND  2  AND  3  AND  4  AND  5
               GO  TO  INP-010
           END-IF
           IF  W-ID         =  2
               CALL "SD_Output" USING "CLE-03" CLE-03 "p" RETURNING RESU
               CALL "SD_Output" USING "CLE-04" CLE-04 "p" RETURNING RESU
               GO  TO  INP-100
           END-IF
           IF  W-ID         >  2
               CALL "SD_Output" USING "CLE-03" CLE-03 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLE-03A" CLE-03A "p" RETURNING RESU
               GO  TO  INP-200
           END-IF
           CALL "SD_Output" USING "CLE-03A" CLE-03A "p" RETURNING RESU.
           CALL "SD_Output" USING "CLE-04" CLE-04 "p" RETURNING RESU.
       INP-015.
           CALL "SD_Accept" USING BY REFERENCE ACEP-KBN "ACEP-KBN"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INP-010
           END-IF
           IF  ESTAT  NOT   =  "01"  AND  "06"
               GO  TO  INP-015
           END-IF
           IF  W-KBN        <  1  OR  >  8
               GO  TO  INP-015
           END-IF.
       INP-020.
           MOVE  "1"    TO  JCON1-01.
           MOVE  W-KBN  TO  JCON1-02.
      *           READ  JCON    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               GO  TO   INP-021
           END-IF
           IF  ACT  =  1
               CALL "SD_Output" USING
                "NOR-M01" NOR-M01 "p" RETURNING RESU
               GO  TO  INP-015
           END-IF
           MOVE  JCON1-03      TO  W-DNO1.
           MOVE  JCON1-04      TO  W-DNO2.
           MOVE  JCON1-05      TO  W-DNO3.
           CALL "SD_Output" USING
            "ACEP-DNO1" ACEP-DNO1 "p" RETURNING RESU.
           IF  W-KBN        NOT =  8
               CALL "SD_Output" USING
                "ACEP-DNO2" ACEP-DNO2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "ACEP-DNO3" ACEP-DNO3 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "CLE-032" CLE-032 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLE-032A" CLE-032A "p" RETURNING RESU
           END-IF
           IF  ACT              =  3
               GO  TO  INP-OKC
           END-IF
           GO  TO  INP-030.
       INP-021.
           IF  ACT              =  2  OR  3
               CALL "SD_Output" USING
                "INV-M01" INV-M01 "p" RETURNING RESU
               GO  TO  INP-015
           END-IF
           MOVE  SPACE   TO  JCON1-R.
           INITIALIZE        JCON1-R.
           MOVE  "1"    TO  JCON1-01.
           MOVE  W-KBN  TO  JCON1-02.
       INP-030.
           CALL "SD_Accept" USING BY REFERENCE ACEP-DNO1 "ACEP-DNO1"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO TO INP-015
           END-IF
           IF  ESTAT  NOT  =    "01"  AND  "06"
               GO TO INP-030
           END-IF
           IF  W-KBN = 1
               IF  W-DNO1 < 100000 OR > 199999
                   GO TO INP-030
               END-IF
           END-IF
           IF  W-KBN = 2 OR 3 OR 4 OR 5 OR 6 OR 7
               IF  W-DNO1 > 099999
                   GO TO INP-030
               END-IF
           END-IF
           IF  W-KBN = 8
               IF  W-DNO1 < 200000 OR > 299999
                   GO TO INP-030
               END-IF
           END-IF
           IF  W-KBN = 8
               GO TO INP-OKC
           END-IF.
       INP-031.
           CALL "SD_Accept" USING BY REFERENCE ACEP-DNO2 "ACEP-DNO2"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INP-030
           END-IF
           IF  ESTAT  NOT  =    "01"  AND  "06"
               GO  TO  INP-031
           END-IF
           IF  W-KBN = 1
               IF  W-DNO2 < 300000 OR > 399999
                   GO TO INP-031
               END-IF
           END-IF
           IF  W-KBN = 2 OR 3 OR 4 OR 6 OR 7
               IF  W-DNO2 < 100000 OR > 199999
                   GO TO INP-031
               END-IF
           END-IF
           IF  W-KBN = 5
               IF  W-DNO2 < 200000 OR > 299999
                   GO TO INP-031
               END-IF
           END-IF.
       INP-032.
           CALL "SD_Accept" USING BY REFERENCE ACEP-DNO3 "ACEP-DNO3"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INP-031
           END-IF
           IF  ESTAT  NOT  =    "01"  AND  "06"
               GO  TO  INP-032
           END-IF
           IF  W-KBN = 1
               IF  W-DNO3 < 500000 OR > 599999
                   GO TO INP-032
               END-IF
           END-IF
           IF  W-KBN = 2 OR 3 OR 4 OR 6 OR 7
               IF  W-DNO3 < 200000 OR > 299999
                   GO TO INP-032
               END-IF
           END-IF
           IF  W-KBN = 5
               IF  W-DNO3 < 400000 OR > 499999
                   GO TO INP-032
               END-IF
           END-IF.
       INP-OKC.
           CALL "SD_Accept" USING BY REFERENCE ACEP-OKC "ACEP-OKC"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"  AND  ACT  =  3
               GO  TO  INP-015
           END-IF
           IF  ESTAT  =  "09"
               IF  W-KBN     =  8
                   GO  TO  INP-030
               ELSE
                   GO  TO  INP-032
               END-IF
           END-IF
           IF  ESTAT  NOT  =  "01"  AND  "06"
               GO  TO  INP-OKC
           END-IF
           IF  OKC  =  9
               CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU
               CALL "SD_Output" USING "CAN-01" CAN-01 "p" RETURNING RESU
               GO  TO  INP-RTN
           END-IF
           IF  OKC  NOT =  1
               GO  TO  INP-OKC
           END-IF
           PERFORM  UPD-RTN    THRU  UPD-EX.
           MOVE  ZERO  TO  W-KBN  W-DNO.
           CALL "SD_Output" USING "CLE-03" CLE-03 "p" RETURNING RESU.
           CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU.
           GO  TO  INP-015.
       INP-100.
           MOVE  SPACE  TO  JCON7-KEY.
           MOVE  "7"    TO  JCON7-01.
      *           READ  JCON    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               GO  TO   INP-101
           END-IF
           IF  ACT  =  1
               CALL "SD_Output" USING
                "NOR-M01" NOR-M01 "p" RETURNING RESU
               GO  TO  INP-010
           END-IF
           MOVE  JCON7-05      TO  W-SYB1.
           MOVE  JCON7-07      TO  W-SYB2.
           MOVE  JCON7-09      TO  W-SYB3.
           MOVE  JCON7-06      TO  W-JKB1.
           MOVE  JCON7-08      TO  W-JKB2.
           MOVE  JCON7-10      TO  W-JKB3.
           CALL "SD_Output" USING
            "ACEP-SYB" ACEP-SYB "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACEP-JKB" ACEP-JKB "p" RETURNING RESU.
           IF  ACT              =  3
               GO  TO  INP-OKC2
           END-IF
           GO  TO  INP-110.
       INP-101.
           IF  ACT              =  2  OR  3
               CALL "SD_Output" USING
                "INV-M01" INV-M01 "p" RETURNING RESU
               GO  TO  INP-010
           END-IF
           MOVE  SPACE   TO  JCON7-R.
           INITIALIZE        JCON7-R.
           MOVE  "7"    TO  JCON7-01.
           CALL "SD_Output" USING
            "ACEP-SYB" ACEP-SYB "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACEP-JKB" ACEP-JKB "p" RETURNING RESU.
       INP-110.
           CALL "SD_Accept" USING BY REFERENCE ACEP-SYB1 "ACEP-SYB1"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INP-010
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INP-110
           END-IF.
       INP-111.
           CALL "SD_Accept" USING BY REFERENCE ACEP-SYB2 "ACEP-SYB2"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INP-110
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INP-111
           END-IF.
       INP-112.
           CALL "SD_Accept" USING BY REFERENCE ACEP-SYB3 "ACEP-SYB3"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INP-111
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INP-112
           END-IF.
       INP-120.
           CALL "SD_Accept" USING BY REFERENCE ACEP-JKB1 "ACEP-JKB1"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INP-112
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INP-120
           END-IF
           IF  W-JKB1 NOT =  0  AND  1
               GO  TO  INP-120
           END-IF.
       INP-121.
           CALL "SD_Accept" USING BY REFERENCE ACEP-JKB2 "ACEP-JKB2"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INP-120
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INP-121
           END-IF
           IF  W-JKB2 NOT =  0  AND  1
               GO  TO  INP-121
           END-IF.
       INP-122.
           CALL "SD_Accept" USING BY REFERENCE ACEP-JKB3 "ACEP-JKB3"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INP-121
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INP-122
           END-IF
           IF  W-JKB3 NOT =  0  AND  1
               GO  TO  INP-122
           END-IF.
       INP-OKC2.
           CALL "SD_Accept" USING BY REFERENCE ACEP-OKC "ACEP-OKC"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"  AND  ACT  =  3
               GO  TO  INP-010
           END-IF
           IF  ESTAT  =  "09"
               GO  TO  INP-122
           END-IF
           IF  ESTAT  NOT  =  "01"  AND  "06"
               GO  TO  INP-OKC2
           END-IF
           IF  OKC  =  9
               CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU
               CALL "SD_Output" USING "CAN-01" CAN-01 "p" RETURNING RESU
               GO  TO  INP-RTN
           END-IF
           IF  OKC  NOT =  1
               GO  TO  INP-OKC2
           END-IF
           PERFORM  UPD-RTN    THRU  UPD-EX.
           MOVE  ZERO  TO  W-SYB  W-JKB.
           CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU.
           GO  TO  INP-RTN.
       INP-200.
           CALL "SD_Accept" USING BY REFERENCE ACEP-CODE "ACEP-CODE"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INP-010
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INP-200
           END-IF
           IF  W-ID       =  4
               IF  W-CODE  <  1  OR  >  8
                   GO  TO  INP-200
               END-IF
           END-IF
           CALL "SD_Output" USING "CLE-041" CLE-041 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLE-042" CLE-042 "p" RETURNING RESU.
      *
           COMPUTE  JCON2-01   =   W-ID   -   1.
           MOVE  W-CODE        TO  JCON2-02.
      *           READ  JCON          INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               GO  TO  INP-201
           END-IF
           IF  ACT              =  1
               CALL "SD_Output" USING
                "NOR-M01" NOR-M01 "p" RETURNING RESU
               GO  TO  INP-200
           END-IF
           MOVE  JCON2-03      TO  W-MEI.
           CALL "SD_Output" USING
            "ACEP-MEI" ACEP-MEI "p" RETURNING RESU.
           IF  W-ID             =  4
               MOVE  JCON3-04    TO  W-KAN
               CALL "SD_Output" USING
                "ACEP-KAN" ACEP-KAN "p" RETURNING RESU
           END-IF
           IF  W-ID             =  5
               MOVE  JCON4-04    TO  W-TEL
               CALL "SD_Output" USING
                "ACEP-TEL" ACEP-TEL "p" RETURNING RESU
           END-IF
           IF  ACT              =  3
               GO  TO  INP-OKC3
           END-IF
           GO  TO  INP-210.
       INP-201.
           IF  ACT              =  2  OR  3
               CALL "SD_Output" USING
                "INV-M01" INV-M01 "p" RETURNING RESU
               GO  TO  INP-200
           END-IF.
       INP-210.
           CALL "SD_Accept" USING BY REFERENCE ACEP-MEI "ACEP-MEI"
            "N" "12" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INP-200
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INP-210
           END-IF
           IF  W-ID             =  3
               GO  TO  INP-OKC3
           END-IF
           IF  W-ID             =  4
               GO  TO  INP-230
           END-IF.
       INP-220.
           CALL "SD_Accept" USING BY REFERENCE ACEP-TEL "ACEP-TEL"
            "X" "13" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INP-210
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INP-220
           END-IF
           GO  TO  INP-OKC3.
       INP-230.
           CALL "SD_Accept" USING BY REFERENCE ACEP-KAN "ACEP-KAN"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INP-210
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INP-230
           END-IF
           IF  W-KAN        NOT =  0  AND  1
               GO  TO  INP-230
           END-IF.
       INP-OKC3.
           CALL "SD_Accept" USING BY REFERENCE ACEP-OKC "ACEP-OKC"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"  AND  ACT  =  3
               GO  TO  INP-200
           END-IF
           IF  ESTAT  =  "09"  AND  W-ID  =  4
               GO  TO  INP-230
           END-IF
           IF  ESTAT  =  "09"  AND  W-ID  =  5
               GO  TO  INP-220
           END-IF
           IF  ESTAT  =  "09"  AND  W-ID  =  3
               GO  TO  INP-210
           END-IF
           IF  ESTAT  NOT  =  "01"  AND  "06"
               GO  TO  INP-OKC3
           END-IF
           IF  OKC  =  9
               CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU
               CALL "SD_Output" USING "CAN-01" CAN-01 "p" RETURNING RESU
               GO  TO  INP-RTN
           END-IF
           IF  OKC  NOT =  1
               GO  TO  INP-OKC3
           END-IF
           PERFORM  UPD-RTN    THRU  UPD-EX.
           MOVE  ZERO  TO  W-CODE  W-KAN.
           MOVE  SPACE TO  W-MEI  W-TEL.
           CALL "SD_Output" USING "CLE-04" CLE-04 "p" RETURNING RESU.
           CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU.
           GO  TO  INP-200.
       INP-EX.
           EXIT.
      ****************************************
      *    çXÅ@êVÅ@èàÅ@óùÅ@                  *
      ****************************************
       UPD-RTN.
           IF  ACT              =  3
               GO  TO  UPD-100
           END-IF
           IF  W-ID             =  1
               GO  TO  UPD-010
           END-IF
           IF  W-ID             =  2
               GO  TO  UPD-020
           END-IF
           MOVE  SPACE         TO  JCON2-R
           INITIALIZE              JCON2-R.
           COMPUTE  JCON2-01   =  W-ID    -  1.
           MOVE  W-CODE        TO  JCON2-02.
           MOVE  W-MEI         TO  JCON2-03.
           IF  W-ID             =  4
               MOVE  W-KAN         TO  JCON3-04
           END-IF
           IF  W-ID             =  5
               MOVE  W-TEL         TO  JCON4-04
           END-IF
           IF  ACT              =  1
               PERFORM  WRI-RTN    THRU  WRI-EX
           ELSE
               PERFORM  REW-RTN    THRU  REW-EX
           END-IF
           GO  TO  UPD-EX.
       UPD-010.
           MOVE  SPACE         TO  JCON1-R
           INITIALIZE              JCON1-R.
           MOVE  W-ID          TO  JCON1-01.
           MOVE  W-KBN         TO  JCON1-02.
           MOVE  W-DNO1        TO  JCON1-03.
           MOVE  W-DNO2        TO  JCON1-04.
           MOVE  W-DNO3        TO  JCON1-05.
           IF  ACT              =  1
               PERFORM  WRI-RTN    THRU  WRI-EX
           ELSE
               PERFORM  REW-RTN    THRU  REW-EX
           END-IF
           GO  TO  UPD-EX.
       UPD-020.
           MOVE  SPACE         TO  JCON7-R
           INITIALIZE              JCON7-R.
           MOVE  SPACE         TO  JCON7-KEY.
           MOVE  7             TO  JCON7-01.
           MOVE  W-SYB1        TO  JCON7-05.
           MOVE  W-JKB1        TO  JCON7-06.
           MOVE  W-SYB2        TO  JCON7-07.
           MOVE  W-JKB2        TO  JCON7-08.
           MOVE  W-SYB3        TO  JCON7-09.
           MOVE  W-JKB3        TO  JCON7-10.
           IF  ACT              =  1
               PERFORM  WRI-RTN    THRU  WRI-EX
           ELSE
               PERFORM  REW-RTN    THRU  REW-EX
           END-IF
           GO  TO  UPD-EX.
       UPD-100.
           MOVE  JCON1-KEY     TO  ERR-K.
      *           DELETE    JCON      INVALID
      *///////////////
           CALL "DB_Delete" USING JCON_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "JCON"      TO  ERR-F
               MOVE  "D"         TO  ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       UPD-EX.
           EXIT.
      *----------------------------*
      *    Å@ ÇvÇqÇhÅ|ÇqÇsÇm       *
      *----------------------------*
       WRI-RTN.
           MOVE  JCON1-KEY     TO  ERR-K.
      *           WRITE    JCON1-R    INVALID
      *//////////////
           CALL "DB_Insert" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               MOVE  "JCON"      TO  ERR-F
               MOVE  "W"         TO  ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       WRI-EX.
           EXIT.
      *----------------------------*
      *    Å@ ÇqÇdÇvÅ|ÇqÇsÇm       *
      *----------------------------*
       REW-RTN.
           MOVE  JCON1-KEY     TO  ERR-K.
      *           REWRITE  JCON1-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               MOVE  "JCON"      TO  ERR-F
               MOVE  "R"         TO  ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       REW-EX.
           EXIT.
      *----------------------------*
      ****  END-RTN  *******
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
       END-EX.
           EXIT.
       COPY     LPMSG.

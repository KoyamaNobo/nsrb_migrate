       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT367I.
       AUTHOR.                        -----------.
      ***************************************************
      *    PROGRAM        : éÛíçécè¡çûÇ›Åiìæà”êÊïiñºï Åj*
      *    DATA WRITTEN   : 92/10/30                    *
      *    SCREEN USED    : ------                      *
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
       01  ACT-WORK.
           02  W-OK                  PIC 9(01).
           02  CHK                   PIC 9(01).
           02  CNT                   PIC 9(02).
           02  W-SU                  PIC S9(06).
           02  W-AREA1.
             03  W-FROM.
               04  W-FTCD            PIC  9(04).
               04  W-FHCD            PIC  9(06).
               04  W-FNGP            PIC  9(08).
               04  W-FNGPD  REDEFINES  W-FNGP.
                 05  W-FNEN          PIC  9(04).
                 05  W-FNENL  REDEFINES  W-FNEN.
                   06  W-FNEN1       PIC  9(02).
                   06  W-FNEN2       PIC  9(02).
                 05  W-FGET          PIC  9(02).
                 05  W-FPEY          PIC  9(02).
               04  W-FNGPL  REDEFINES  W-FNGP.
                 05  F               PIC  9(02).
                 05  W-FNGPS         PIC  9(06).
             03  W-TO.
               04  W-TTCD            PIC  9(04).
               04  W-THCD            PIC  9(06).
               04  W-TNGP            PIC  9(08).
               04  W-TNGPD  REDEFINES  W-TNGP.
                 05  W-TNEN          PIC  9(04).
                 05  W-TNENL  REDEFINES  W-TNEN.
                   06  W-TNEN1       PIC  9(02).
                   06  W-TNEN2       PIC  9(02).
                 05  W-TGET          PIC  9(02).
                 05  W-TPEY          PIC  9(02).
               04  W-TNGPL  REDEFINES  W-TNGP.
                 05  F               PIC  9(02).
                 05  W-TNGPS         PIC  9(06).
       COPY    LWMSG.
      *
           COPY LIBFDD.
      ***  éÛíçÉ}ÉXÉ^
      *FD  JMSTD
       01  JMSTD_JT367I.
           02  JMSTD_PNAME1          PIC  X(005) VALUE "JMST1".
           02  F                     PIC  X(001).
           02  JMSTD_PNAME2          PIC  X(005) VALUE "JMST2".
           02  F                     PIC  X(001).
           02  JMSTD_PNAME3          PIC  X(005) VALUE "JMST3".
           02  F                     PIC  X(001).
           02  JMSTD_LNAME           PIC  X(012) VALUE "JMSTD_JT367I".
           02  F                     PIC  X(001).
           02  JMSTD_KEY1            PIC  X(100) VALUE SPACE.
           02  JMSTD_KEY2            PIC  X(100) VALUE SPACE.
           02  JMSTD_SORT            PIC  X(100) VALUE SPACE.
           02  JMSTD_IDLST           PIC  X(100) VALUE SPACE.
           02  JMSTD_RES             USAGE  POINTER.
      *
       01  JMSTD-R.
           02   JMSTD-01                 PIC 9(1).
           02   JMSTD-02.
                03  JMSTD-021            PIC 9(4).
                03  JMSTD-022            PIC 9(2).
                03  JMSTD-023            PIC 9(2).
           02   JMSTD-KEY3.
                03  JMSTD-03             PIC 9(6).
                03  JMSTD-KEY2.
                    04  JMSTD-04         PIC 9(4).
                    04  JMSTD-05         PIC 9(6).
                    04  JMSTD-06.
                        05  JMSTD-061    PIC 9(4).
                        05  JMSTD-062    PIC 9(2).
                        05  JMSTD-063    PIC 9(2).
                    04  JMSTD-KEY1.
                        05  JMSTD-07     PIC 9(6).
                        05  JMSTD-08     PIC 9(1).
           02   JMSTD-09                 PIC 9(1).
           02   JMSTD-10                 PIC 9(3).
           02   JMSTD-11.
                03  JMSTD-111            OCCURS  10.
                    04  JMSTD-1111       PIC S9(6)   COMP-3.
           02   JMSTD-12.
                03  JMSTD-121            OCCURS  10.
                    04  JMSTD-1211       PIC S9(6)   COMP-3.
           02   JMSTD-14.
                03  JMSTD-141  OCCURS 10 PIC S9(06)  COMP-3.
           02   JMSTD-15.
                03  JMSTD-151  OCCURS 10 PIC S9(06)  COMP-3.
           02   JMSTD-16                 PIC S9(03).
           02   FILLER                   PIC X(12).
           02   JMSTD-20                 PIC 9(03).
           02   JMSTD-13                 PIC N(32).
           02   JMSTD-21                 PIC 9(01).
           02   FILLER                   PIC X(54).
       77  F                             PIC X(01).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLE-01.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  CLE-AREA.
           02  CLE-02.
               03  FILLER  PIC  X(01) VALUE " ".
       01  DSP-AREA.
           02  DSP-01.
               03  FILLER  PIC  X(20) VALUE "éÛíçÅEéÊÇÊÇØécÅ@çÌèú".
           02  DSP-02.
               03  FILLER  PIC  X(28) VALUE
                     "ìæà”êÊ∫∞ƒﬁ ïiñº∫∞ƒﬁ   éÛíçì˙".
           02  DSP-03.
               03  FILLER  PIC  X(08) VALUE "ÇeÇqÇnÇl".
               03  FILLER  PIC  X(08) VALUE   "  /  /  ".
           02  DSP-04.
               03  FILLER  PIC  X(04) VALUE "ÇsÇn".
               03  FILLER  PIC  X(08) VALUE   "  /  /  ".
           02  DSP-12.
               03  FILLER  PIC  X(06) VALUE "ämîFÅi".
               03  FILLER  PIC  X(09) VALUE "OK=1,NO=9".
               03  FILLER  PIC  X(02) VALUE "Åj".
               03  FILLER  PIC  X(04) VALUE "--->".
               03  FILLER  PIC  X(04) VALUE "ÿ¿∞›".
       01  ACP-AREA.
           02  FILLER.
             03  ACP-FTCD    PIC  9(04).
             03  ACP-FHCD    PIC  9(06).
             03  ACP-FNEN2   PIC  9(02).
             03  ACP-FGET    PIC  9(02).
             03  ACP-FPEY    PIC  9(02).
           02  FILLER.
             03  ACP-TTCD    PIC  9(04).
             03  ACP-THCD    PIC  9(06).
             03  ACP-TNEN2   PIC  9(02).
             03  ACP-TGET    PIC  9(02).
             03  ACP-TPEY    PIC  9(02).
           02  ACP-OK      PIC  9(01).
       01  DSP-ERR.
           02  INV-01      PIC  X(20) VALUE
             "éÛíçÉ}ÉXÉ^Å[Å@ñ¢ìoò^".
           02  INV-04      PIC  X(20) VALUE
             "èoâ◊èàóùíÜÅ@è¡çûïsâ¬".
      *
           COPY LSMSG.
           COPY LIBSCR.
      *
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLE-01
       CALL "SD_Init" USING
           "CLE-01" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLE-01" "X" "1" "0" "12" " " "CLE-01" RETURNING RESU.
      *CLE-AREA
       CALL "SD_Init" USING 
            "CLE-AREA" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-02" " " "0" "0" "1" " " "CLE-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLE-02" "X" "23" "62" "1" " " "CLE-02" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "101" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "20" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "X" "1" "22" "20" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-02" " " "5" "0" "28" "DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-02" "X" "5" "21" "28" " " "DSP-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-03" " " "7" "0" "16" "DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-03" "X" "7" "14" "8" " " "DSP-03" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-03" "X" "7" "42" "8" "01DSP-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-04" " " "9" "0" "12" "DSP-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-04" "X" "9" "14" "4" " " "DSP-04" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-04" "X" "9" "42" "8" "01DSP-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-12" " " "23" "0" "25" "DSP-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-12" "X" "23" "41" "6" " " "DSP-12" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-12" "X" "23" "47" "9" "01DSP-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-12" "X" "23" "56" "2" "02DSP-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-12" "X" "23" "58" "4" "03DSP-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-12" "X" "23" "63" "4" "04DSP-12" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "33" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-AREA" " " "7" "0" "16" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FTCD" "9" "7" "24" "4" " " "01ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FTCD" BY REFERENCE W-FTCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FHCD" "9" "7" "33" "6" "ACP-FTCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FHCD" BY REFERENCE W-FHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FNEN2" "9" "7" "42" "2" "ACP-FHCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FNEN2" BY REFERENCE W-FNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FGET" "9" "7" "45" "2" "ACP-FNEN2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FGET" BY REFERENCE W-FGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FPEY" "9" "7" "48" "2" "ACP-FGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FPEY" BY REFERENCE W-FPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACP-AREA" " " "9" "0" "16" "01ACP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TTCD" "9" "9" "24" "4" " " "02ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TTCD" BY REFERENCE W-TTCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-THCD" "9" "9" "33" "6" "ACP-TTCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-THCD" BY REFERENCE W-THCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TNEN2" "9" "9" "42" "2" "ACP-THCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TNEN2" BY REFERENCE W-TNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TGET" "9" "9" "45" "2" "ACP-TNEN2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TGET" BY REFERENCE W-TGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TPEY" "9" "9" "48" "2" "ACP-TGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TPEY" BY REFERENCE W-TPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OK" "9" "23" "62" "1" "02ACP-AREA" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-OK" BY REFERENCE W-OK "1" "0" RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "40" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-01" "X" "24" "1" "20" " " "DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-04" "X" "24" "1" "20" "INV-01" " " RETURNING RESU.
      *
           COPY LSMSG_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
      ************************************
      *    ÉÅÉCÉìÅ@ÉãÅ[É`Éì              *
      ************************************
       HAJIME.
           PERFORM   INT-RTN   THRU  INT-EX.
       MR010.
           CALL "SD_Accept" USING BY REFERENCE ACP-FTCD "ACP-FTCD"
            "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  MR999
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR010
           END-IF.
       MR020.
           CALL "SD_Accept" USING BY REFERENCE ACP-TTCD "ACP-TTCD"
            "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR010
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR020
           END-IF
           IF  W-FTCD  >  W-TTCD
               GO  TO  MR020
           END-IF.
       MR021.
           CALL "SD_Accept" USING BY REFERENCE ACP-FHCD "ACP-FHCD"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR020
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR021
           END-IF.
       MR022.
           CALL "SD_Accept" USING BY REFERENCE ACP-THCD "ACP-THCD"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR021
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR022
           END-IF
           IF  W-FHCD  >  W-THCD
               GO  TO  MR022
           END-IF.
       MR023.
           CALL "SD_Accept" USING BY REFERENCE ACP-FNEN2 "ACP-FNEN2"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR022
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"   AND  "00"
               GO  TO  MR023
           END-IF.
       MR024.
           CALL "SD_Accept" USING BY REFERENCE ACP-FGET "ACP-FGET"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR023
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"   AND  "00"
               GO  TO  MR024
           END-IF
           IF  (W-FGET  =  ZERO)  AND  (W-FNEN2  =  ZERO)
               GO  TO  MR025
           END-IF
           IF   W-FGET  <   1   OR   >  12
               GO  TO  MR024
           END-IF.
       MR025.
           CALL "SD_Accept" USING BY REFERENCE ACP-FPEY "ACP-FPEY"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR024
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"   AND  "00"
               GO  TO  MR025
           END-IF
           IF  (W-FPEY  =  ZERO)  AND  (W-FGET   =  ZERO)
               MOVE  ZERO     TO  W-FNEN1
               GO  TO  MR026
           END-IF
           IF   W-FPEY  <   1   OR   >  31
               GO  TO  MR025
           END-IF
           MOVE  ZERO     TO  W-FNEN1.
           IF  W-FNEN2  >=  DATE-NF1  AND  DATE-NT1
               ADD  DATE-NC1     TO  W-FNEN
           END-IF
           IF  W-FNEN2  >=  DATE-NF2  AND  DATE-NT2
               ADD  DATE-NC2     TO  W-FNEN
           END-IF.
       MR026.
           CALL "SD_Accept" USING BY REFERENCE ACP-TNEN2 "ACP-TNEN2"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR025
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"   AND  "00"
               GO  TO  MR026
           END-IF.
       MR027.
           CALL "SD_Accept" USING BY REFERENCE ACP-TGET "ACP-TGET"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR026
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"   AND  "00"
               GO  TO  MR027
           END-IF
           IF  (W-TGET  =  99  )  AND  (W-TNEN2  =  99  )
               GO  TO  MR028
           END-IF
           IF   W-TGET  <   1   OR   >  12
               GO  TO  MR027
           END-IF.
       MR028.
           CALL "SD_Accept" USING BY REFERENCE ACP-TPEY "ACP-TPEY"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR027
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"   AND  "00"
               GO  TO  MR028
           END-IF
           IF  (W-TPEY  =  99  )  AND  (W-TGET   =  99  )
               MOVE  99       TO  W-TNEN1
               GO  TO  MR029
           END-IF
           IF  W-TPEY  <   1   OR   >  31
               GO  TO  MR028
           END-IF
           MOVE  ZERO     TO  W-TNEN1.
           IF  W-TNEN2  >=  DATE-NF1  AND  DATE-NT1
               ADD  DATE-NC1     TO  W-TNEN
           END-IF
           IF  W-TNEN2  >=  DATE-NF2  AND  DATE-NT2
               ADD  DATE-NC2     TO  W-TNEN
           END-IF.
       MR029.
           IF  W-FNGP   >  W-TNGP
               GO  TO  MR026
           END-IF.
       MR030.
           CALL "SD_Accept" USING BY REFERENCE ACP-OK "ACP-OK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  MR026
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR030
           END-IF
           IF  W-OK    =  9
               CALL "SD_Output" USING
                "CLE-AREA" CLE-AREA "p" RETURNING RESU
               GO  TO  MR010
           END-IF
           IF  W-OK   NOT  =  1
               GO  TO  MR030
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" JMSTD_PNAME1 "EXCLUSIVE" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY2" BY REFERENCE JMSTD-KEY2 "JMSTD-KEY1"
            BY REFERENCE JMSTD-KEY1 "JMSTD-KEY3" BY REFERENCE
            JMSTD-KEY3.
           MOVE    SPACE    TO  JMSTD-KEY2.
           MOVE    W-FTCD   TO  JMSTD-04.
           MOVE    W-FHCD   TO  JMSTD-05.
      *           START   JMSTD     KEY  NOT  <  JMSTD-KEY2  INVALID
      *///////////////
           CALL "DB_Start" USING
            JMSTD_PNAME1 "JMSTD-KEY2" " NOT < " JMSTD-KEY2
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "INV-01" INV-01 "p" RETURNING RESU
               GO  TO  MR010
           END-IF.
       MR040.
      *           READ    JMSTD     NEXT      AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  MR900
           END-IF
           IF  JMSTD-04  >  W-TTCD
               GO  TO  MR900
           END-IF
           IF  JMSTD-05  >  W-THCD
               GO  TO  MR900
           END-IF
           IF  JMSTD-02  <  W-FNGP  OR  >  W-TNGP
               GO  TO  MR040
           END-IF.
       MR050.
           PERFORM     DEL-RTN     THRU      DEL-EX.
           GO  TO  MR040.
       MR900.
           PERFORM     END-RTN    THRU   END-EX.
       MR999.
           CALL "DB_Close".
           STOP  RUN.
      *********************************************
      *    ÇhÇmÇsÅ|ÇqÇsÇm                         *
      *********************************************
       INT-RTN.
           INITIALIZE     ACT-WORK.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           COPY  LIBCPR.
       INT-EX.
           EXIT.
      *********************************************
      *    ÇdÇmÇcÅ|ÇqÇsÇm                         *
      *********************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
       END-EX.
           EXIT.
      *********************************************
      *    ÇcÇdÇkÅ|ÇqÇsÇm                         *
      *********************************************
       DEL-RTN.
      *           DELETE      JMSTD        INVALID
      *///////////////
           CALL "DB_Delete" USING JMSTD_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE    "JMSTD"   TO   ERR-F
               MOVE    "D"       TO   ERR-M
               MOVE   JMSTD-KEY1 TO   ERR-K
               PERFORM   ERR-RTN  THRU   ERR-EX
           END-IF.
       DEL-EX.
           EXIT.
      ***
       COPY LPMSG.

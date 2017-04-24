       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       JK038L.
      ******************************************************************
      *    ã ìáÅ@ì¸â◊ïiñºèWåvï\                                        *
      *****JS-SIGN : ëSïî=0 , ‹∞∏œ›=1 , ≈Ã∫=2                          *
      ******************************************************************
      *
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       DATA              DIVISION.
       WORKING-STORAGE   SECTION.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  END-SW                PIC 9      VALUE 0.
       01  RED-SW                PIC 9      VALUE 0.
       01  DAT-SW                PIC 9      VALUE 0.
       01  MID1-R.
           02  W-20K             PIC X(5)   VALUE X"1A24212474".
           02  M1-SAI            PIC N(1).
           02  F                 PIC X(02)  VALUE SPACE.
           02  M1-SNGP           PIC 99/99/99.
           02  F                 PIC N(1)   VALUE "Å`".
           02  M1-ENGP           PIC 99/99/99.
           02  F                 PIC X(13)  VALUE SPACE.
           02  F                 PIC N(20)  VALUE
                "ÅñÅñÅñÅ@Å@ã ìáÅ@ì¸â◊ïiñºèWåvï\Å@Å@ÅñÅñÅñ".
           02  F                 PIC X(16)  VALUE SPACE.
           02  F                 PIC X(5)   VALUE "DATE ".
           02  M1-DATE           PIC 99/99/99.
           02  F                 PIC X(7)   VALUE "     P.".
           02  M1-PAGE           PIC Z9.
       01  MID2-R.
           02  W-15K             PIC X(5)   VALUE X"1A24212078".
           02  F                 PIC X(7)   VALUE " ∫∞ƒﬁ  ".
           02  F                 PIC N(8)   VALUE
                "ïiÅ@Å@Å@Å@Å@ñºÅ@".
           02  F                 PIC X(25)  VALUE SPACE.
           02  F                 PIC X(1)   VALUE "1".
           02  F                 PIC X(16)  VALUE SPACE.
           02  F                 PIC X(2)   VALUE "SS".
           02  F                 PIC X(5)   VALUE SPACE.
           02  F                 PIC X(1)   VALUE "S".
           02  F                 PIC X(5)   VALUE SPACE.
           02  F                 PIC X(1)   VALUE "M".
           02  F                 PIC X(5)   VALUE SPACE.
           02  F                 PIC X(1)   VALUE "L".
           02  F                 PIC X(4)   VALUE SPACE.
           02  F                 PIC X(2)   VALUE "LL".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "28.0".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "29.0".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "30.0".
           02  F                 PIC X(8)   VALUE SPACE.
       01  MID3-R.
           02  F                 PIC X(44)  VALUE SPACE.
           02  F                 PIC X(1)   VALUE "2".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "12.5".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "13.0".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "13.5".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "14.0".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "15.0".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "16.0".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "17.0".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "18.0".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "19.0".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "20.0".
           02  F                 PIC X(8)   VALUE SPACE.
       01  MID4-R.
           02  F                 PIC X(44)  VALUE SPACE.
           02  F                 PIC X(1)   VALUE "3".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "21.0".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "21.5".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "22.0".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "22.5".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "23.0".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "23.5".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "24.0".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "24.5".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "25.0".
           02  F                 PIC X(14)  VALUE SPACE.
       01  MID5-R.
           02  F                 PIC X(44)  VALUE SPACE.
           02  F                 PIC X(1)   VALUE "4".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "24.0".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "24.5".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "25.0".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "25.5".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "26.0".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "26.5".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "27.0".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "27.5".
           02  F                 PIC X(17)  VALUE SPACE.
           02  F                 PIC N(2)   VALUE "çáåv".
       01  WR-R.
           02  WR-K1             PIC X(05).
           02  WR-HCD            PIC 9(06).
           02  F                 PIC X(01).
           02  WR-HNA            PIC N(24).
           02  F                 PIC X(01).
           02  WR-SIZ            PIC 9(01).
           02  WR-SU01           PIC -(06).
           02  WR-SU02           PIC -(06).
           02  WR-SU03           PIC -(06).
           02  WR-SU04           PIC -(06).
           02  WR-SU05           PIC -(06).
           02  WR-SU06           PIC -(06).
           02  WR-SU07           PIC -(06).
           02  WR-SU08           PIC -(06).
           02  WR-SU09           PIC -(06).
           02  WR-SU10           PIC -(06).
           02  WR-SUT            PIC ----,--9.
           02  WR-K2             PIC X(05).
       01  W-ASU.
           02  W-SUD       OCCURS   10.
             03  W-SU            PIC S9(5).
           02  W-SUT             PIC S9(6).
       01  WD-R.
           02   JNSW-01               PIC 9(6).
           02   JNSW-02               PIC 9(8).
           02   JNSW-06               PIC 9(1).
           02   JNSW-07               PIC 9(1).
           02   JNSW-08.
                03  JNSW-080    OCCURS  10.
                    04  JNSW-081      PIC S9(4).
       01  W-KEI                 PIC S9(6).
       01  LCNT                  PIC 9(2).
       01  PAGE-C                PIC 9(2).
       01  KAKU-W                PIC 9.
       01  ERR-STAT              PIC X(2).
       01  W-DATA.
           02  W-DATE            PIC 9(06).
           02  W-NGP    REDEFINES  W-DATE.
               03  W-NEN         PIC 9(02).
               03  W-GET         PIC 9(02).
               03  W-PEY         PIC 9(02).
           02  W-SDATE           PIC 9(08).
           02  W-EDATE           PIC 9(08).
           02  W-SNGP.
               03  W-SNEN        PIC 9(04).
               03  W-SNENL  REDEFINES W-SNEN.
                   04  W-SNEN1   PIC 9(02).
                   04  W-SNEN2   PIC 9(02).
               03  W-SGET        PIC 9(02).
               03  W-SPEY        PIC 9(02).
           02  W-SNGPD   REDEFINES W-SNGP.
             03  F               PIC 9(02).
             03  W-SNGPS         PIC 9(06).
           02  W-ENGP.
               03  W-ENEN        PIC 9(04).
               03  W-ENENL  REDEFINES W-ENEN.
                   04  W-ENEN1   PIC 9(02).
                   04  W-ENEN2   PIC 9(02).
               03  W-EGET        PIC 9(02).
               03  W-EPEY        PIC 9(02).
           02  W-ENGPD   REDEFINES W-ENGP.
             03  F               PIC 9(02).
             03  W-ENGPS         PIC 9(06).
           02  W-HCD.
             03  W-HCD1          PIC 9(4).
             03  W-HCD2          PIC 9(2).
           02  OLD-HCD.
             03  OLD-HCD1        PIC 9(4).
             03  OLD-HCD2        PIC 9(2).
           02  OLD-SIZ           PIC 9(1).
           COPY  LWMSG.
      *
           COPY  LIHIM2.
      *FD  JNSRF
       01  JNSRF_JK038L.
           02  JNSRF_PNAME1           PIC  X(009) VALUE SPACE.
           02  F                      PIC  X(001).
           02  JNSRF_LNAME            PIC  X(012) VALUE "JNSRF_JK038L".
           02  F                      PIC  X(001).
           02  JNSRF_KEY1             PIC  X(100) VALUE SPACE.
           02  JNSRF_SORT             PIC  X(100) VALUE SPACE.
           02  JNSRF_IDLST            PIC  X(100) VALUE SPACE.
           02  JNSRF_RES              USAGE  POINTER.
       01  JNSR-R.
           02   JNSR-01               PIC 9(6)  COMP-3.
           02   JNSR-02               PIC 9(8)  COMP-3.
           02   F                     PIC X(7).
           02   JNSR-06               PIC 9(1).
           02   JNSR-07               PIC 9(1).
           02   JNSR-08.
                03  JNSR-080    OCCURS  10.
                    04  JNSR-081      PIC S9(4)  COMP-3.
           02   F                     PIC X(208).
       77  F                          PIC X(001).
      *FD  PRN-F
       77  PRN-R                      PIC X(256).
      *
       77  END-STS            PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  ACP-NYURYOKU.
           02  ACP-SNGP.
               03  ACP-SNEN  PIC 9(02).
               03  ACP-SGET  PIC 9(02).
               03  ACP-SPEY  PIC 9(02).
           02  ACP-ENGP.
               03  ACP-ENEN  PIC 9(02).
               03  ACP-EGET  PIC 9(02).
               03  ACP-EPEY  PIC 9(02).
           02  ACP-KAKU.
               03  01ACP-KAKU  PIC 9 .
       01  DSP-GAMEN.
           02  FILLER  PIC  X(20) VALUE
                                "ã ìáÅ@ì¸â◊ïiñºèWåvï\".
           02  FILLER.
               03  FILLER  PIC  9(02).
               03  FILLER  PIC  N(01) VALUE "îN".
               03  FILLER  PIC  9(02).
               03  FILLER  PIC  N(01) VALUE "åé".
               03  FILLER  PIC  9(02).
               03  FILLER  PIC  N(01) VALUE "ì˙".
           02  FILLER  PIC  X(06) VALUE  "ì˙Å@ït".
           02  FILLER.
               03  FILLER  PIC  X(08) VALUE  "ÇeÇqÇnÇl".
               03  FILLER  PIC  X(08) VALUE  "  /  /  ".
           02  FILLER.
               03  FILLER  PIC  X(04) VALUE  "ÇsÇn".
               03  FILLER  PIC  X(08) VALUE  "  /  /  ".
           02  FILLER.
               03  FILLER  PIC  X(06) VALUE  "ämîFÅi".
               03  FILLER  PIC  X(09) VALUE    "OK=1,NO=9".
               03  FILLER  PIC  X(02) VALUE  "Åj".
               03  FILLER  PIC  X(10) VALUE    "--->  ÿ¿∞›".
       01  DSP-CLER.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-CLER2.
           02  FILLER  PIC X(6)  VALUE "      ".
           02  FILLER  PIC X(6)  VALUE "      ".
           COPY  LSERR.
       PROCEDURE         DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "PRF-999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *ACP-NYURYOKU
       CALL "SD_Init" USING 
            "ACP-NYURYOKU" " " "0" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SNGP" " " "17" "0" "6" " " "ACP-NYURYOKU"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SNEN" "9" "17" "31" "2" " " "ACP-SNGP" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SNEN" BY REFERENCE W-SNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SGET" "9" "17" "34" "2" "ACP-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SPEY" "9" "17" "37" "2" "ACP-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ENGP" " " "19" "0" "6" "ACP-SNGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ENEN" "9" "19" "31" "2" " " "ACP-ENGP" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-ENEN" BY REFERENCE W-ENEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-EGET" "9" "19" "34" "2" "ACP-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-EPEY" "9" "19" "37" "2" "ACP-EGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KAKU" " " "24" "0" "1" "ACP-ENGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-KAKU" "9" "24" "63" "1" " " "ACP-KAKU" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-KAKU" BY REFERENCE KAKU-W "1" "0" RETURNING RESU.
      *DSP-GAMEN
       CALL "SD_Init" USING 
            "DSP-GAMEN" " " "0" "0" "93" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-GAMEN" "RX" "1" "30" "20" " " "DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-GAMEN" " " "3" "0" "12" "01DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-GAMEN" "9" "3" "63" "2" " " "02DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0102DSP-GAMEN" BY REFERENCE W-NEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DSP-GAMEN" "N" "3" "65" "2" "0102DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302DSP-GAMEN" "9" "3" "67" "2" "0202DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0302DSP-GAMEN" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0402DSP-GAMEN" "N" "3" "69" "2" "0302DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0502DSP-GAMEN" "9" "3" "71" "2" "0402DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0502DSP-GAMEN" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0602DSP-GAMEN" "N" "3" "73" "2" "0502DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-GAMEN" "X" "15" "31" "6" "02DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-GAMEN" " " "17" "0" "16" "03DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0104DSP-GAMEN" "X" "17" "21" "8" " " "04DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0204DSP-GAMEN" "X" "17" "31" "8" "0104DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-GAMEN" " " "19" "0" "12" "04DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0105DSP-GAMEN" "X" "19" "21" "4" " " "05DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0205DSP-GAMEN" "X" "19" "31" "8" "0105DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-GAMEN" " " "24" "0" "27" "05DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0106DSP-GAMEN" "X" "24" "41" "6" " " "06DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0206DSP-GAMEN" "X" "24" "47" "9" "0106DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0306DSP-GAMEN" "X" "24" "56" "2" "0206DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0406DSP-GAMEN" "X" "24" "58" "10" "0306DSP-GAMEN" " "
            RETURNING RESU.
      *DSP-CLER
       CALL "SD_Init" USING
           "DSP-CLER" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-CLER" "X" "1" "0" "12" " " "DSP-CLER" RETURNING RESU.
      *DSP-CLER2
       CALL "SD_Init" USING 
            "DSP-CLER2" " " "0" "0" "24" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-CLER2" "X" "17" "41" "6" " " "DSP-CLER2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-CLER2" "X" "19" "41" "6" "01DSP-CLER2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-CLER2" "X" "17" "41" "6" "02DSP-CLER2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-CLER2" "X" "19" "41" "6" "03DSP-CLER2" " "
            RETURNING RESU.
      *
           COPY LSERR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MAINLINE-ROUTINE.
           PERFORM  INIT-RTN  THRU  INIT-RTN-EXIT.
           IF  END-STS  =  "P9"
               CALL "DB_Close"
               STOP  RUN
           END-IF
           PERFORM  SLCT-RTN  THRU  SLCT-RTN-EXIT.
           IF  END-STS  =  "P9"
               CALL "DB_Close"
               STOP  RUN
           END-IF
           MOVE  W-SNGPS TO  M1-SNGP.
           MOVE  W-ENGPS TO  M1-ENGP.
           PERFORM  OPEN-RTN  THRU  OPEN-RTN-EXIT.
           PERFORM  PROC-RTN  THRU  PROC-RTN-EXIT
                    UNTIL  NOT  (END-SW  NOT  =  9).
           PERFORM  GKEI-RTN  THRU  GKEI-RTN-EXIT
      *
           PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT.
       MAINLINE-END.
           CALL "DB_Close".
           STOP  RUN.
      ******************************************************************
      *    ÉÅÉCÉìèàóù  ÅiÇoÇqÇnÇbÅ|ÇqÇsÇmÅj                            *
      ******************************************************************
       PROC-RTN.
           PERFORM  READ1-RTN  THRU  READ1-RTN-EXIT.
           PERFORM  GENT-RTN   THRU  GENT-RTN-EXIT
                    UNTIL  NOT  (END-SW  =  0  AND  RED-SW  =  1).
       PROC-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÅiÇfÇdÇmÇsÅ|ÇqÇsÇmÅj                                        *
      ******************************************************************
       GENT-RTN.
           PERFORM  READ2-RTN  THRU  READ2-RTN-EXIT.
           PERFORM  DETL-RTN   THRU  DETL-RTN-EXIT.
           PERFORM  READ1-RTN  THRU  READ1-RTN-EXIT.
           PERFORM  TOTL-RTN   THRU  TOTL-RTN-EXIT.
       GENT-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    èâä˙ê›íËèàóù  ÅiÇhÇmÇhÇsÅ|ÇqÇsÇmÅj                          *
      ******************************************************************
       INIT-RTN.
      *
           MOVE  SPACE  TO  WR-R.
           MOVE  W-15K  TO  WR-K1.
           MOVE  W-20K  TO  WR-K2.
           MOVE  ZERO   TO  W-DATA  W-ASU  W-KEI  PAGE-C.
           MOVE  90     TO  LCNT.
           ACCEPT W-DATE FROM DATE.
           MOVE  W-DATE TO  M1-DATE.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JNSRF_PNAME1.
       INIT-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ëIëèàóù  ÅiÇrÇkÇbÇsÅ|ÇqÇsÇmÅj                              *
      ******************************************************************
       SLCT-RTN.
       SLCT-010.
           CALL "SD_Output" USING
            "DSP-CLER" DSP-CLER "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-GAMEN" DSP-GAMEN "p" RETURNING RESU.
           MOVE  W-DATE      TO  W-SNGPS.
           MOVE  20          TO  W-SNEN1.
           MOVE W-SNGP       TO  W-ENGP.
           CALL "SD_Output" USING
            "ACP-SNGP" ACP-SNGP "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACP-ENGP" ACP-ENGP "p" RETURNING RESU.
           GO  TO  SLCT-200.
       SLCT-052.
           CALL "SD_Accept" USING BY REFERENCE ACP-SNEN "ACP-SNEN"
            "9" "2" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "P9"
               MOVE  9  TO  END-SW
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS   NOT =  "00" AND "01" AND "06"
               GO  TO  SLCT-052
           END-IF
           MOVE  ZERO        TO  W-SNEN1.
           IF  W-SNEN2       >  90
               ADD  1900   TO  W-SNEN
           ELSE
               ADD  2000   TO  W-SNEN
           END-IF.
       SLCT-053.
           CALL "SD_Accept" USING BY REFERENCE ACP-SGET "ACP-SGET"
            "9" "2" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-052
           END-IF
           IF  END-STS   NOT =  "00" AND "01" AND "06"
               GO  TO  SLCT-053
           END-IF
           IF  W-SGET        <  1  OR  >  12
               GO  TO  SLCT-053
           END-IF.
       SLCT-054.
           CALL "SD_Accept" USING BY REFERENCE ACP-SPEY "ACP-SPEY"
            "9" "2" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-053
           END-IF
           IF  END-STS   NOT =  "00" AND "01" AND "06"
               GO  TO  SLCT-054
           END-IF
           IF  W-SPEY        <  1  OR  >  31
               GO  TO  SLCT-054
           END-IF.
       SLCT-056.
           CALL "SD_Accept" USING BY REFERENCE ACP-ENEN "ACP-ENEN"
            "9" "2" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-054
           END-IF
           IF  END-STS   NOT =  "00" AND "01" AND "06"
               GO  TO  SLCT-056
           END-IF
           MOVE  ZERO        TO  W-ENEN1.
           IF  W-ENEN2       >  90
               ADD  1900   TO  W-ENEN
           ELSE
               ADD  2000   TO  W-ENEN
           END-IF.
       SLCT-057.
           CALL "SD_Accept" USING BY REFERENCE ACP-EGET "ACP-EGET"
            "9" "2" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-056
           END-IF
           IF  END-STS   NOT =  "00" AND "01" AND "06"
               GO  TO  SLCT-057
           END-IF
           IF  W-EGET        <  1  OR  >  12
               GO  TO  SLCT-057
           END-IF.
       SLCT-058.
           CALL "SD_Accept" USING BY REFERENCE ACP-EPEY "ACP-EPEY"
            "9" "2" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-057
           END-IF
           IF  END-STS   NOT =  "00" AND "01" AND "06"
               GO  TO  SLCT-058
           END-IF
           IF  W-EPEY        <  1  OR  >  31
               GO  TO  SLCT-058
           END-IF
           IF  W-SNGP        >  W-ENGP
               GO  TO  SLCT-056
           END-IF.
       SLCT-200.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-KAKU "01ACP-KAKU"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "P9"
               MOVE  9  TO  END-SW
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS       =  "09"
               GO  TO  SLCT-052
           END-IF
           IF  END-STS  NOT  =  "01"
               GO  TO  SLCT-200
           END-IF
           IF  KAKU-W  =  1
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  KAKU-W  =  9
               GO  TO  SLCT-010
           ELSE
               GO  TO  SLCT-200
           END-IF.
       SLCT-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÇqÇdÇ`ÇcèàóùÅiÇPÅj  ÅiÇqÇdÇ`ÇcÇPÅ|ÇqÇsÇmÅj                  *
      ******************************************************************
       READ1-RTN.
       READ1-030.
      *    (º≠Ø∂•ªºΩﬁ•ƒ◊› … ÿ∞ƒﬁ (ª≤ Ø∫≥))
           IF  RED-SW  =  1
               GO  TO  READ1-040
           END-IF.
       READ1-040.
      *           READ  JNSRF        AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JNSRF_PNAME1 BY REFERENCE JNSR-R " " RETURNING RET.
           IF  RET = 1
               MOVE  9  TO  END-SW
               GO  TO  READ1-RTN-EXIT
           END-IF
           MOVE  ZERO     TO  WD-R.
           MOVE  JNSR-01  TO  JNSW-01.
           MOVE  JNSR-02  TO  JNSW-02.
           MOVE  JNSR-06  TO  JNSW-06.
           MOVE  JNSR-07  TO  JNSW-07.
           MOVE  JNSR-081(01) TO  JNSW-081(01).
           MOVE  JNSR-081(02) TO  JNSW-081(02).
           MOVE  JNSR-081(03) TO  JNSW-081(03).
           MOVE  JNSR-081(04) TO  JNSW-081(04).
           MOVE  JNSR-081(05) TO  JNSW-081(05).
           MOVE  JNSR-081(06) TO  JNSW-081(06).
           MOVE  JNSR-081(07) TO  JNSW-081(07).
           MOVE  JNSR-081(08) TO  JNSW-081(08).
           MOVE  JNSR-081(09) TO  JNSW-081(09).
           MOVE  JNSR-081(10) TO  JNSW-081(10).
           IF  JNSW-01        >  999899
               GO  TO  READ1-040
           END-IF
           IF  JNSW-02        <  W-SNGP  OR  > W-ENGP
               GO  TO  READ1-040
           END-IF
           MOVE  1  TO  RED-SW.
       READ1-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÇqÇdÇ`ÇcèàóùÅiÇQÅj  ÅiÇqÇdÇ`ÇcÇQÅ|ÇqÇsÇmÅj                  *
      ******************************************************************
       READ2-RTN.
           MOVE  JNSW-01  TO  WR-HCD.
           MOVE  JNSW-01  TO  HI-MHCD HI-HCD.
      *           READ  HI2-M  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE  TO  WR-HNA
               GO  TO  READ2-RTN-EXIT
           END-IF
           MOVE  HI-NAME TO  WR-HNA.
       READ2-RTN-EXIT.
           EXIT.
       DETL-RTN.
           IF  JNSW-01  NOT  =  OLD-HCD
               IF  OLD-HCD NOT = ZERO
                   MOVE  SPACE  TO  PRN-R
                   CALL "PR_LineFeed" USING "1" RETURNING RESP
                   CALL "PR_Write" USING PRN-R RETURNING RESP
                   ADD  1  TO  LCNT
               END-IF
           END-IF
           PERFORM  ADD-RTN   THRU  ADD-RTN-EXIT.
           MOVE  JNSW-01           TO  OLD-HCD.
           MOVE  JNSW-07           TO  OLD-SIZ.
       DETL-RTN-EXIT.
           EXIT.
       TOTL-RTN.
           IF  ZERO         =  W-SU(01)  AND  W-SU(02)  AND  W-SU(03)
                          AND  W-SU(04)  AND  W-SU(05)  AND  W-SU(06)
                          AND  W-SU(07)  AND  W-SU(08)  AND  W-SU(09)
                          AND  W-SU(10)
               GO  TO  TOTL-RTN-EXIT
           END-IF
           IF  END-SW  =  9
               PERFORM  MEI2-RTN  THRU  MEI2-RTN-EXIT
               GO  TO  TOTL-RTN-EXIT
           END-IF
           IF (JNSW-01  NOT  =  OLD-HCD)  OR
              (JNSW-07  NOT  =  OLD-SIZ)
               PERFORM  MEI2-RTN  THRU  MEI2-RTN-EXIT
           END-IF.
       TOTL-RTN-EXIT.
           EXIT.
       MID-RTN.
           IF  LCNT    NOT  =  90
               MOVE  SPACE  TO  PRN-R
               CALL "PR_Write" USING PRN-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           ADD   1         TO  PAGE-C.
           MOVE  PAGE-C          TO  M1-PAGE.
           MOVE  SPACE           TO  PRN-R.
           MOVE  MID1-R          TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE  SPACE           TO  PRN-R.
           MOVE  MID2-R          TO  PRN-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE  SPACE           TO  PRN-R.
           MOVE  MID3-R          TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE  SPACE           TO  PRN-R.
           MOVE  MID4-R          TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE  SPACE           TO  PRN-R.
           MOVE  MID5-R          TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE  SPACE           TO  PRN-R.
           MOVE  6  TO  LCNT.
       MID-RTN-EX.
           EXIT.
       ADD-RTN.
           ADD   JNSW-081(1)   TO  W-SU(01).
           ADD   JNSW-081(2)   TO  W-SU(02).
           ADD   JNSW-081(3)   TO  W-SU(03).
           ADD   JNSW-081(4)   TO  W-SU(04).
           ADD   JNSW-081(5)   TO  W-SU(05).
           ADD   JNSW-081(6)   TO  W-SU(06).
           ADD   JNSW-081(7)   TO  W-SU(07).
           ADD   JNSW-081(8)   TO  W-SU(08).
           ADD   JNSW-081(9)   TO  W-SU(09).
           ADD   JNSW-081(10)  TO  W-SU(10).
           COMPUTE  W-SUT   =  W-SUT  +  JNSW-081(1)  +  JNSW-081(2)
                                      +  JNSW-081(3)  +  JNSW-081(4)
                                      +  JNSW-081(5)  +  JNSW-081(6)
                                      +  JNSW-081(7)  +  JNSW-081(8)
                                      +  JNSW-081(9)  +  JNSW-081(10).
       ADD-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÇvÇqÇhÇsÇdèàóù  ÅiÇvÇqÇhÇsÇdÅ|ÇqÇsÇmÅj                      *
      ******************************************************************
       MEI2-RTN.
       MEI2-010.
           IF  LCNT    NOT  <  62
               PERFORM  MID-RTN    THRU  MID-RTN-EX
           END-IF
           MOVE  OLD-SIZ   TO  WR-SIZ.
           MOVE  W-SU(1)   TO  WR-SU01.
           MOVE  W-SU(2)   TO  WR-SU02.
           MOVE  W-SU(3)   TO  WR-SU03.
           MOVE  W-SU(4)   TO  WR-SU04.
           MOVE  W-SU(5)   TO  WR-SU05.
           MOVE  W-SU(6)   TO  WR-SU06.
           MOVE  W-SU(7)   TO  WR-SU07.
           MOVE  W-SU(8)   TO  WR-SU08.
           MOVE  W-SU(9)   TO  WR-SU09.
           MOVE  W-SU(10)  TO  WR-SU10.
           MOVE  W-SUT     TO  WR-SUT.
           MOVE  WR-R          TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           ADD   W-SUT       TO  W-KEI.
           MOVE  ZERO      TO  W-ASU.
           ADD  1  TO  LCNT.
       MEI2-RTN-EXIT.
           EXIT.
       GKEI-RTN.
       GKEI-010.
           IF  LCNT    NOT  <  62
               PERFORM  MID-RTN    THRU  MID-RTN-EX
           END-IF
           MOVE  SPACE  TO  WR-R.
           MOVE  W-15K  TO  WR-K1.
           MOVE  W-20K  TO  WR-K2.
           MOVE  "Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@ÅiÅ@çáÅ@åvÅ@ÅjÅ@Å@" TO  WR-HNA.
           MOVE  W-KEI     TO  WR-SUT.
           MOVE  WR-R          TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
       GKEI-RTN-EXIT.
           EXIT.
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "INPUT" JNSRF_PNAME1 " " BY REFERENCE JNSRF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
       OPEN-RTN-EXIT.
           EXIT.
       CLSE-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JNSRF_IDLST JNSRF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       CLSE-RTN-EXIT.
           EXIT.
           COPY  LPERR.

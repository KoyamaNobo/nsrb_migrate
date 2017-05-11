       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       JK036L.
      ******************************************************************
      *    ì°ìcÅ@ÉèÅ[ÉNÉ}Éìèoâ◊éwê}Å@ïiñºèWåvï\                        *
      *    JS-SIGN : ëSïî=0 , ‹∞∏œ›=1 , ≈Ã∫=2                          *
      ******************************************************************
      *
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       DATA              DIVISION.
       WORKING-STORAGE   SECTION.
       77  JS-SIGN            PIC  9(1).
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
           02  F                 PIC X(08)  VALUE SPACE.
           02  F                 PIC N(05)  VALUE "ÅñÅñÅñÅ@Å@".
           02  M1-MID            PIC N(05)  VALUE "ÉèÅ[ÉNÉ}Éì".
           02  F                 PIC N(15)  VALUE
                "èoâ◊éwê}Å@ïiñºèWåvï\Å@Å@ÅñÅñÅñ".
           02  F                 PIC X(11)  VALUE SPACE.
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
       01  W-KEI                 PIC S9(6).
       01  LCNT                  PIC 9(2).
       01  PAGE-C                PIC 9(2).
       01  SEN-W                 PIC 9.
       01  DEN-WF                PIC 9(6).
       01  DEN-WT                PIC 9(6).
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
      *FD  JHSSF
       01  JHSSF_JK036L.
           02  JHSSF_PNAME1           PIC  X(009) VALUE SPACE.
           02  F                      PIC  X(001).
           02  JHSSF_LNAME            PIC  X(012) VALUE "JHSSF_JK036L".
           02  F                      PIC  X(001).
           02  JHSSF_KEY1             PIC  X(100) VALUE SPACE.
           02  JHSSF_SORT             PIC  X(100) VALUE SPACE.
           02  JHSSF_IDLST            PIC  X(100) VALUE SPACE.
           02  JHSSF_RES              USAGE  POINTER.
       01  JHSS-R.
           02   JHSS-KEY.
                03   JHSS-01          PIC 9(6).
                03   JHSS-02          PIC 9(1).
           02   JHSS-03               PIC 9(1).
           02   JHSS-04.
                03  JHSS-041          PIC 9(4).
                03  JHSS-041L  REDEFINES  JHSS-041.
                    04  JHSS-0411     PIC 9(2).
                    04  JHSS-0412     PIC 9(2).
                03  JHSS-042          PIC 9(2).
                03  JHSS-043          PIC 9(2).
           02   JHSS-04L   REDEFINES  JHSS-04.
                03  F                 PIC 9(2).
                03  JHSS-04S          PIC 9(6).
           02   JHSS-05.
                03  JHSS-051          PIC 9(4).
                03  JHSS-051L  REDEFINES  JHSS-051.
                    04  JHSS-0511     PIC 9(2).
                    04  JHSS-0512     PIC 9(2).
                03  JHSS-052          PIC 9(2).
                03  JHSS-053          PIC 9(2).
           02   JHSS-05L   REDEFINES  JHSS-05.
                03  F                 PIC 9(2).
                03  JHSS-05S          PIC 9(6).
           02   JHSS-06.
                03  JHSS-061          PIC 9(4).
                03  JHSS-062          PIC 9(3).
           02   JHSS-07               PIC 9(1).
           02   JHSS-08.
                03  JHSS-081          PIC 9(6).
                03  JHSS-082          PIC 9(1).
           02   JHSS-09               PIC 9(6).
           02   JHSS-10               PIC 9(1).
           02   JHSS-11.
                03  JHSS-111    OCCURS  10.
                    04  JHSS-1111     PIC S9(4).
                03  JHSS-112          PIC S9(5).
           02   JHSS-12.
                03  JHSS-121    OCCURS  10.
                    04  JHSS-1211     PIC S9(4).
                03  JHSS-122          PIC S9(5).
           02  JHSS-13                PIC 9(1).
           02  JHSS-14                PIC 9(1).
           02  JHSS-14A               PIC 9(3).
           02  JHSS-14B               PIC 9(6).
           02  JHSS-14C               PIC 9(2).
           02  JHSS-14D               PIC N(9).
           02  JHSS-15                PIC N(23).
           02  JHSS-20                PIC X(10).
           02  JHSS-15A               PIC S9(03).
           02  JHSS-30                PIC 9(01).
           02  JHSS-40.
               03  JHSS-401.
                   04  JHSS-4011      PIC X(03).
                   04  JHSS-4012      PIC 9(01).
               03  JHSS-402.
                   04  JHSS-4021      PIC X(03).
                   04  JHSS-4022      PIC 9(01).
                   04  JHSS-4023      PIC 9(01).
           02  FILLER                 PIC X(16).
           02  JHSS-19                PIC X(01).
           02  JHSS-158               PIC 9(01).
           02  JHSS-16                PIC 9(01).
           02  JHSS-17                PIC 9(01).
       77  F                          PIC X(01).
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
           02  ACP-SEN.
               03  01ACP-SEN   PIC 9 .
           02  ACP-SNGP.
               03  ACP-SNEN    PIC 9(02).
               03  ACP-SGET    PIC 9(02).
               03  ACP-SPEY    PIC 9(02).
           02  ACP-ENGP.
               03  ACP-ENEN    PIC 9(02).
               03  ACP-EGET    PIC 9(02).
               03  ACP-EPEY    PIC 9(02).
           02  ACP-DENF.
               03  01ACP-DENF  PIC 9(6).
           02  ACP-DENT.
               03  01ACP-DENT  PIC 9(6).
           02  ACP-KAKU.
               03  01ACP-KAKU  PIC 9 .
       01  DSP-GAMEN.
           02  FILLER  PIC  X(20) VALUE
                                  "èoâ◊éwê}Å@ïiñºèWåvï\".
           02  FILLER.
               03  FILLER  PIC  9(02).
               03  FILLER  PIC  N(01) VALUE "îN".
               03  FILLER  PIC  9(02).
               03  FILLER  PIC  N(01) VALUE "åé".
               03  FILLER  PIC  9(02).
               03  FILLER  PIC  N(01) VALUE "ì˙".
           02  FILLER  PIC  X(16) VALUE  "ÇPÅ@éwê}ñ¢î≠çsï™".
           02  FILLER.
               03  FILLER  PIC  X(22) VALUE "ÇQÅ@Å@Å@î≠çsçœï™Å@ëIë".
               03  FILLER  PIC  X(01) VALUE   "[".
               03  FILLER  PIC  X(01) VALUE   "]".
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
       01  DSP-GAMEN0.
           02  FILLER  PIC  X(16) VALUE    "                ".
           02  FILLER.
               03  FILLER  PIC  X(22) VALUE   "                      ".
               03  FILLER  PIC  X(01) VALUE   " ".
               03  FILLER  PIC  X(01) VALUE   " ".
       01  DSP-GAMEN1.
           02  DSP-MID0   PIC  X(10) VALUE
               "Å@ì°Å@ìcÅ@".
           02  DSP-MID1   PIC  X(10) VALUE
               "ÉèÅ[ÉNÉ}Éì".
           02  DSP-MID2   PIC  X(10) VALUE
               "ÉiÅ@ÉtÅ@ÉR".
           02  DSP-DNOM   PIC  X(06) VALUE  "ì`ï[áÇ".
           02  DSP-DNOC   PIC  X(06) VALUE    "      ".
       01  DSP-CLER.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-CLER2.
           02  FILLER  PIC X(6)  VALUE "      ".
           02  FILLER  PIC X(6)  VALUE "      ".
       01  DSP-MI.
           02  FILLER  PIC  X(26) VALUE
               "ÅñÅñèoâ◊éwê}ÉgÉâÉìÅ@ñ¢ìoò^".
           COPY  LSERR.
       PROCEDURE         DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "PRF-999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *ACP-NYURYOKU
       CALL "SD_Init" USING 
            "ACP-NYURYOKU" " " "0" "0" "26" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SEN" " " "7" "0" "1" " " "ACP-NYURYOKU" RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-SEN" "9" "7" "47" "1" " " "ACP-SEN" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-SEN" BY REFERENCE SEN-W "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SNGP" " " "17" "0" "6" "ACP-SEN" " " RETURNING RESU.
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
            "ACP-DENF" " " "17" "0" "6" "ACP-ENGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-DENF" "9" "17" "41" "6" " " "ACP-DENF" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-DENF" BY REFERENCE DEN-WF "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-DENT" " " "19" "0" "6" "ACP-DENF" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-DENT" "9" "19" "41" "6" " " "ACP-DENT" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-DENT" BY REFERENCE DEN-WT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KAKU" " " "24" "0" "1" "ACP-DENT" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-KAKU" "9" "24" "63" "1" " " "ACP-KAKU" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-KAKU" BY REFERENCE KAKU-W "1" "0" RETURNING RESU.
      *DSP-GAMEN
       CALL "SD_Init" USING 
            "DSP-GAMEN" " " "0" "0" "133" " " " " RETURNING RESU.
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
            "03DSP-GAMEN" "X" "5" "23" "16" "02DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-GAMEN" " " "7" "0" "24" "03DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0104DSP-GAMEN" "X" "7" "23" "22" " " "04DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0204DSP-GAMEN" "X" "7" "46" "1" "0104DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0304DSP-GAMEN" "X" "7" "48" "1" "0204DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-GAMEN" "X" "15" "31" "6" "04DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-GAMEN" " " "17" "0" "16" "05DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0106DSP-GAMEN" "X" "17" "21" "8" " " "06DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0206DSP-GAMEN" "X" "17" "31" "8" "0106DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-GAMEN" " " "19" "0" "12" "06DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0107DSP-GAMEN" "X" "19" "21" "4" " " "07DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0207DSP-GAMEN" "X" "19" "31" "8" "0107DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-GAMEN" " " "24" "0" "27" "07DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0108DSP-GAMEN" "X" "24" "41" "6" " " "08DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0208DSP-GAMEN" "X" "24" "47" "9" "0108DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0308DSP-GAMEN" "X" "24" "56" "2" "0208DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0408DSP-GAMEN" "X" "24" "58" "10" "0308DSP-GAMEN" " "
            RETURNING RESU.
      *DSP-GAMEN0
       CALL "SD_Init" USING 
            "DSP-GAMEN0" " " "0" "0" "40" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-GAMEN0" "X" "5" "23" "16" " " "DSP-GAMEN0"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-GAMEN0" " " "7" "0" "24" "01DSP-GAMEN0" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-GAMEN0" "X" "7" "23" "22" " " "02DSP-GAMEN0"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DSP-GAMEN0" "X" "7" "46" "1" "0102DSP-GAMEN0" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302DSP-GAMEN0" "X" "7" "48" "1" "0202DSP-GAMEN0" " "
            RETURNING RESU.
      *DSP-GAMEN1
       CALL "SD_Init" USING 
            "DSP-GAMEN1" " " "0" "0" "42" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MID0" "RX" "1" "20" "10" " " "DSP-GAMEN1"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MID1" "RX" "1" "20" "10" "DSP-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MID2" "RX" "1" "20" "10" "DSP-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DNOM" "X" "15" "41" "6" "DSP-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DNOC" "X" "15" "41" "6" "DSP-DNOM" " " RETURNING RESU.
      *DSP-CLER
       CALL "SD_Init" USING
           "DSP-CLER" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-CLER" "X" "1" "0" "12" " " "DSP-CLER" RETURNING RESU.
      *DSP-CLER2
       CALL "SD_Init" USING 
            "DSP-CLER2" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-CLER2" "X" "17" "41" "6" " " "DSP-CLER2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-CLER2" "X" "19" "41" "6" "01DSP-CLER2" " "
            RETURNING RESU.
      *DSP-MI
       CALL "SD_Init" USING 
            "DSP-MI" " " "0" "0" "26" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MI" "X" "24" "1" "26" " " "DSP-MI" RETURNING RESU.
      *
           COPY LSERR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
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
           IF  SEN-W    =  2
               MOVE  "çƒ"     TO M1-SAI
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
           ACCEPT   JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN    NOT  =  0  AND  1 AND  2
               MOVE  "P9"   TO  END-STS
               GO TO INIT-RTN-EXIT
           END-IF
           IF  JS-SIGN       =  0
               MOVE  "ì°Å@ìcÅ@Å@"  TO  M1-MID
           ELSE
               IF  JS-SIGN       =  2
                   MOVE  "ÉiÅ@ÉtÅ@ÉR"  TO  M1-MID
               END-IF
           END-IF
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
           MOVE WK0256ID TO JHSSF_PNAME1.
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
           IF  JS-SIGN       =  0
               CALL "SD_Output" USING
                "DSP-MID0" DSP-MID0 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DSP-GAMEN0" DSP-GAMEN0 "p" RETURNING RESU
               IF  JS-SIGN       =  1
                   CALL "SD_Output" USING
                    "DSP-MID1" DSP-MID1 "p" RETURNING RESU
               ELSE
                   IF  JS-SIGN       =  2
                       CALL "SD_Output" USING
                        "DSP-MID2" DSP-MID2 "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN   NOT =  0
               GO  TO  SLCT-020
           END-IF
           MOVE  W-DATE      TO  W-SNGPS.
           MOVE  20          TO  W-SNEN1.
           MOVE W-SNGP       TO  W-ENGP.
           CALL "SD_Output" USING
            "ACP-SNGP" ACP-SNGP "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACP-ENGP" ACP-ENGP "p" RETURNING RESU.
           GO  TO  SLCT-200.
       SLCT-020.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-SEN "01ACP-SEN"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  END-STS       =  "P9"
               MOVE  9  TO  END-SW
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS  NOT  =  "01"
               GO  TO  SLCT-020
           END-IF
           IF  SEN-W    NOT  =  1  AND  2
               GO  TO  SLCT-020
           END-IF
           CALL "SD_Output" USING
            "DSP-CLER2" DSP-CLER2 "p" RETURNING RESU.
           PERFORM  SEL-RTN     THRU  SEL-EX.
           IF  W-SNGP        =  ZERO
               CALL "SD_Output" USING
                "ERR-02" ERR-02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  SLCT-020
           END-IF
           IF  SEN-W         =  2
               CALL "SD_Output" USING
                "DSP-DNOM" DSP-DNOM "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "DSP-DNOC" DSP-DNOC "p" RETURNING RESU
               CALL "SD_Output" USING
                "DSP-CLER2" DSP-CLER2 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING
            "ACP-SNGP" ACP-SNGP "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACP-ENGP" ACP-ENGP "p" RETURNING RESU.
       SLCT-052.
           CALL "SD_Accept" USING BY REFERENCE ACP-SNEN "ACP-SNEN"
            "9" "2" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "P9"
               MOVE  9  TO  END-SW
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  JS-SIGN   NOT =  0
               IF  END-STS       =  "09"
                   GO  TO  SLCT-020
               END-IF
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
           END-IF
           IF  JS-SIGN   NOT =  0
               IF  W-SNGP        <  W-SDATE  OR  >  W-EDATE
                   GO  TO  SLCT-052
               END-IF
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
           END-IF
           IF  JS-SIGN   NOT =  0
               IF  W-ENGP        <  W-SDATE  OR  >  W-EDATE
                   GO  TO  SLCT-056
               END-IF
           END-IF
           IF  JS-SIGN       =  0
               GO  TO  SLCT-200
           END-IF.
       SLCT-100.
           IF  SEN-W    =  1
               GO  TO  SLCT-200
           END-IF
           CALL "SD_Accept" USING BY REFERENCE 01ACP-DENF "01ACP-DENF"
            "9" "6" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               CALL "SD_Output" USING
                "DSP-CLER2" DSP-CLER2 "p" RETURNING RESU
               GO  TO  SLCT-058
           END-IF
           IF  END-STS  NOT  =  "01"
               GO  TO  SLCT-100
           END-IF.
       SLCT-110.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-DENT "01ACP-DENT"
            "9" "6" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-100
           END-IF
           IF  END-STS  NOT  =  "01"
               GO  TO  SLCT-110
           END-IF
           IF  DEN-WF  >  DEN-WT
               GO  TO  SLCT-110
           END-IF.
       SLCT-200.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-KAKU "01ACP-KAKU"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "P9"
               MOVE  9  TO  END-SW
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS       =  "09"
               IF  JS-SIGN  =  0
                   GO  TO  SLCT-052
               ELSE
                   IF  SEN-W  =  1
                       GO  TO  SLCT-058
                   ELSE
                       GO  TO  SLCT-110
                   END-IF
               END-IF
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
           IF  JS-SIGN       =  0
               GO  TO  READ1-030
           END-IF
           IF  SEN-W   =  1
               GO  TO  READ1-010
           ELSE
               GO  TO  READ1-030
           END-IF.
       READ1-010.
      *    (º≠Ø∂•ªºΩﬁ•ƒ◊› … ÿ∞ƒﬁ ( Ø∫≥))
           IF  RED-SW  =  1
               GO  TO  READ1-020
           END-IF
      *           READ  JHSSF        AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JHSSF_PNAME1 BY REFERENCE JHSS-R " " RETURNING RET.
           IF  RET = 1
               MOVE  24  TO  ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               CALL "SD_Output" USING
                "DSP-MI" DSP-MI "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  JHSS-158  NOT =  0
               GO  TO  READ1-020
           END-IF
           IF  JHSS-17   NOT =  9
               GO  TO  READ1-020
           END-IF
           IF  JS-SIGN       =  1
               IF  JHSS-061  NOT =  9850
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JS-SIGN       =  2
               IF  JHSS-061  NOT =  5000
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JHSS-09       =  000000
               GO  TO  READ1-020
           END-IF
           IF  JHSS-07  NOT =  5  AND  6
               GO  TO  READ1-020
           END-IF
           IF  JHSS-03   NOT  =  0
               GO  TO  READ1-020
           END-IF
           IF  JHSS-04        <  W-SNGP  OR  > W-ENGP
               GO  TO  READ1-020
           END-IF
           MOVE  1  TO  RED-SW.
           GO  TO  READ1-RTN-EXIT.
       READ1-020.
      *           READ  JHSSF        AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JHSSF_PNAME1 BY REFERENCE JHSS-R " " RETURNING RET.
           IF  RET = 1
               MOVE  9  TO  END-SW
               GO  TO  READ1-RTN-EXIT
           END-IF
           IF  JHSS-158  NOT =  0
               GO  TO  READ1-020
           END-IF
           IF  JHSS-17   NOT =  9
               GO  TO  READ1-020
           END-IF
           IF  JS-SIGN       =  1
               IF  JHSS-061  NOT =  9850
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JS-SIGN       =  2
               IF  JHSS-061  NOT =  5000
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JHSS-09       =  000000
               GO  TO  READ1-020
           END-IF
           IF  JHSS-07  NOT =  5  AND  6
               GO  TO  READ1-020
           END-IF
           IF  JHSS-03   NOT  =  0
               GO  TO  READ1-020
           END-IF
           IF  JHSS-04        <  W-SNGP  OR  > W-ENGP
               GO  TO  READ1-020
           END-IF
           MOVE  1  TO  RED-SW.
           GO  TO  READ1-RTN-EXIT.
       READ1-030.
      *    (º≠Ø∂•ªºΩﬁ•ƒ◊› … ÿ∞ƒﬁ (ª≤ Ø∫≥))
           IF  RED-SW  =  1
               GO  TO  READ1-040
           END-IF.
       READ1-040.
      *           READ  JHSSF        AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JHSSF_PNAME1 BY REFERENCE JHSS-R " " RETURNING RET.
           IF  RET = 1
               MOVE  9  TO  END-SW
               GO  TO  READ1-RTN-EXIT
           END-IF
           IF  JHSS-158  NOT =  1
               GO  TO  READ1-040
           END-IF
           IF  JHSS-17       =  0
               GO  TO  READ1-040
           END-IF
           IF  JS-SIGN       =  0
               IF  JHSS-061      =  2150  OR  2157  OR  2158  OR  2160
                                OR  2170  OR  2180  OR  5349  OR  5350
                                OR  5353  OR  5357  OR  5358
                   GO  TO  READ1-040
               END-IF
           END-IF
           IF  JS-SIGN       =  1
               IF  JHSS-061  NOT =  9850
                   GO  TO  READ1-040
               END-IF
           END-IF
           IF  JS-SIGN       =  2
               IF  JHSS-061  NOT =  5000
                   GO  TO  READ1-040
               END-IF
           END-IF
           IF  JHSS-09       =  000000
               GO  TO  READ1-040
           END-IF
           IF  JS-SIGN   NOT =  0
               IF  JHSS-01     < DEN-WF  OR    > DEN-WT
                   GO  TO  READ1-040
               END-IF
           END-IF
           IF  JHSS-07  NOT =  5  AND  6
               GO  TO  READ1-040
           END-IF
           IF  JHSS-03   NOT  =  0
               GO  TO  READ1-040
           END-IF
           IF  JS-SIGN   NOT =  0
               IF  JHSS-04        <  W-SNGP  OR  > W-ENGP
                   GO  TO  READ1-040
               END-IF
           END-IF
           IF  JS-SIGN       =  0
               IF  JHSS-09        >  999899
                   GO  TO  READ1-040
               END-IF
           END-IF
           IF  JS-SIGN       =  0
               IF  JHSS-05        <  W-SNGP  OR  > W-ENGP
                   GO  TO  READ1-040
               END-IF
           END-IF
           MOVE  1  TO  RED-SW.
       READ1-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÇqÇdÇ`ÇcèàóùÅiÇQÅj  ÅiÇqÇdÇ`ÇcÇQÅ|ÇqÇsÇmÅj                  *
      ******************************************************************
       READ2-RTN.
           MOVE  JHSS-09  TO  WR-HCD.
           MOVE  JHSS-09  TO  HI-MHCD HI-HCD.
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
           IF  JHSS-09  NOT  =  OLD-HCD
               IF  OLD-HCD NOT = ZERO
                   MOVE  SPACE  TO  PRN-R
                   CALL "PR_LineFeed" USING "1" RETURNING RESP
                   CALL "PR_Write" USING PRN-R RETURNING RESP
                   ADD  1  TO  LCNT
               END-IF
           END-IF
           PERFORM  ADD-RTN   THRU  ADD-RTN-EXIT.
           MOVE  JHSS-09           TO  OLD-HCD.
           MOVE  JHSS-10           TO  OLD-SIZ.
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
           IF (JHSS-09  NOT  =  OLD-HCD)  OR
              (JHSS-10  NOT  =  OLD-SIZ)
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
           IF  JS-SIGN      =  0
               ADD   JHSS-1211(1)  TO  W-SU(01)
               ADD   JHSS-1211(2)  TO  W-SU(02)
               ADD   JHSS-1211(3)  TO  W-SU(03)
               ADD   JHSS-1211(4)  TO  W-SU(04)
               ADD   JHSS-1211(5)  TO  W-SU(05)
               ADD   JHSS-1211(6)  TO  W-SU(06)
               ADD   JHSS-1211(7)  TO  W-SU(07)
               ADD   JHSS-1211(8)  TO  W-SU(08)
               ADD   JHSS-1211(9)  TO  W-SU(09)
               ADD   JHSS-1211(10) TO  W-SU(10)
               COMPUTE  W-SUT   =  W-SUT + JHSS-1211(1) + JHSS-1211(2)
                                         + JHSS-1211(3) + JHSS-1211(4)
                                         + JHSS-1211(5) + JHSS-1211(6)
                                         + JHSS-1211(7) + JHSS-1211(8)
                                         + JHSS-1211(9) + JHSS-1211(10)
               GO  TO  ADD-RTN-EXIT
           END-IF
           ADD   JHSS-1111(1)  TO  W-SU(01).
           ADD   JHSS-1111(2)  TO  W-SU(02).
           ADD   JHSS-1111(3)  TO  W-SU(03).
           ADD   JHSS-1111(4)  TO  W-SU(04).
           ADD   JHSS-1111(5)  TO  W-SU(05).
           ADD   JHSS-1111(6)  TO  W-SU(06).
           ADD   JHSS-1111(7)  TO  W-SU(07).
           ADD   JHSS-1111(8)  TO  W-SU(08).
           ADD   JHSS-1111(9)  TO  W-SU(09).
           ADD   JHSS-1111(10) TO  W-SU(10).
           COMPUTE  W-SUT   =  W-SUT  +  JHSS-1111(1)  +  JHSS-1111(2)
                                      +  JHSS-1111(3)  +  JHSS-1111(4)
                                      +  JHSS-1111(5)  +  JHSS-1111(6)
                                      +  JHSS-1111(7)  +  JHSS-1111(8)
                                      +  JHSS-1111(9)  +  JHSS-1111(10).
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
           MOVE  WR-R      TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
       GKEI-RTN-EXIT.
           EXIT.
       SEL-RTN.
           MOVE  ZERO    TO  W-SDATE  W-EDATE  W-SNGP  W-ENGP.
           CALL "DB_F_Open" USING
            "INPUT" JHSSF_PNAME1 " " BY REFERENCE JHSSF_IDLST "0".
       SEL-010.
      *           READ  JHSSF                 AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JHSSF_PNAME1 BY REFERENCE JHSS-R " " RETURNING RET.
           IF  RET = 1
               GO  TO  SEL-090
           END-IF
           IF  JHSS-17   NOT =  9
               GO  TO  SEL-010
           END-IF
           IF  JS-SIGN       =  1
               IF  JHSS-061  NOT =  9850
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  JS-SIGN       =  2
               IF  JHSS-061  NOT =  5000
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  JHSS-09       =  000000
               GO  TO  SEL-010
           END-IF
           IF  SEN-W         =  1
               IF  JHSS-158       =  1
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  SEN-W         =  2
               IF  JHSS-158       =  0
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  W-SDATE        =  ZERO
               MOVE  JHSS-04      TO  W-SDATE
           END-IF
           IF  W-SDATE        >  JHSS-04
               MOVE  JHSS-04      TO  W-SDATE
           END-IF
           IF  W-EDATE        <  JHSS-04
               MOVE  JHSS-04      TO  W-EDATE
           END-IF
           GO  TO  SEL-010.
       SEL-090.
           CALL "DB_F_Close" USING
            BY REFERENCE JHSSF_IDLST JHSSF_PNAME1.
           MOVE  W-SDATE TO  W-SNGP.
           MOVE  W-EDATE TO  W-ENGP.
       SEL-EX.
           EXIT.
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "INPUT" JHSSF_PNAME1 " " BY REFERENCE JHSSF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
       OPEN-RTN-EXIT.
           EXIT.
       CLSE-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JHSSF_IDLST JHSSF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       CLSE-RTN-EXIT.
           EXIT.
           COPY  LPERR.

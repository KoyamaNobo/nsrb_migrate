       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       JK035L.
      ******************************************************************
      *    äOïîëqå…  èoâ◊éwê}èëÅiã ìáÉèÅ[ÉNÉ}ÉìÅEÉiÉtÉRópîíéÜÅj        *
      ******************************************************************
      *
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       DATA              DIVISION.
       WORKING-STORAGE   SECTION.
       01  END-SW                PIC 9      VALUE 0.
       01  RED-SW                PIC 9      VALUE 0.
       01  TCM-SW                PIC 9      VALUE 0.
       01  DAT-SW                PIC 9      VALUE 0.
       01  MID1-R.
           02  W-20K             PIC X(5)   VALUE X"1A24212474".
           02  M1-SAI            PIC N(1).
           02  F                 PIC X(43)  VALUE SPACE.
           02  F                 PIC N(19)  VALUE
                  "ÅñÅñÅñÅ@Å@èoÅ@â◊Å@éwÅ@ê}Å@èëÅ@Å@ÅñÅñÅñ".
           02  F                 PIC X(30)  VALUE SPACE.
           02  F                 PIC X(5)   VALUE "DATE ".
           02  M1-DATE           PIC 99/99/99.
           02  F                 PIC X(7)   VALUE "     P.".
           02  M1-PAGE           PIC ZZ9.
       01  MID2-R.
           02  W-15K             PIC X(5)   VALUE X"1A24212078".
           02  F                 PIC N(2)   VALUE   "ì`ãÊ".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC N(4)   VALUE   "éwê}áÇÅ@".
           02  F                 PIC X(6)   VALUE " ∫∞ƒﬁ ".
           02  F                 PIC N(8)   VALUE
                  "ìæÅ@à”Å@êÊÅ@ñºÅ@".
           02  F                 PIC X(28)  VALUE SPACE.
           02  F                 PIC X(5)   VALUE "∫∞ƒﬁ ".
           02  F                 PIC N(8)   VALUE
                  "íºÅ@ëóÅ@êÊÅ@ñºÅ@".
           02  F                 PIC X(29)  VALUE SPACE.
           02  F                 PIC N(4)   VALUE   "ì˙Å@Å@ït".
           02  F                 PIC X(3)   VALUE SPACE.
           02  F                 PIC X(3)   VALUE "æØƒ".
           02  F                 PIC X(2)   VALUE SPACE.
           02  F                 PIC N(4)   VALUE   "ëóÇËèÛáÇ".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC N(4)   VALUE   "â^Å@Å@ëó".
           02  F                 PIC X(4)   VALUE SPACE.
           02  F                 PIC N(2)   VALUE   "å¬êî".
       01  MID3-R.
           02  F                 PIC X(23)  VALUE SPACE.
           02  F                 PIC X(6)   VALUE "∫∞ƒﬁ  ".
           02  F                 PIC N(8)   VALUE
                  "ïiÅ@Å@Å@Å@Å@ñºÅ@".
           02  F                 PIC X(25)  VALUE SPACE.
           02  F                 PIC X(1)   VALUE "1".
           02  F                 PIC X(13)  VALUE SPACE.
           02  F                 PIC X(2)   VALUE "SS".
           02  F                 PIC X(4)   VALUE SPACE.
           02  F                 PIC X(1)   VALUE "S".
           02  F                 PIC X(4)   VALUE SPACE.
           02  F                 PIC X(1)   VALUE "M".
           02  F                 PIC X(4)   VALUE SPACE.
           02  F                 PIC X(1)   VALUE "L".
           02  F                 PIC X(3)   VALUE SPACE.
           02  F                 PIC X(2)   VALUE "LL".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "28.0".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "29.0".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "30.0".
           02  F                 PIC X(9)   VALUE SPACE.
           02  F                 PIC N(4)   VALUE   "îıÅ@Å@çl".
           02  F                 PIC X(4)   VALUE SPACE.
       01  MID4-R.
           02  F                 PIC X(66)  VALUE SPACE.
           02  F                 PIC X(1)   VALUE "2".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "12.5".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "13.0".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "13.5".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "14.0".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "15.0".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "16.0".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "17.0".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "18.0".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "19.0".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "20.0".
           02  F                 PIC X(19)  VALUE SPACE.
       01  MID5-R.
           02  F                 PIC X(66)  VALUE SPACE.
           02  F                 PIC X(1)   VALUE "3".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "21.0".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "21.5".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "22.0".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "22.5".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "23.0".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "23.5".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "24.0".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "24.5".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "25.0".
           02  F                 PIC X(24)  VALUE SPACE.
       01  MID6-R.
           02  F                 PIC X(66)  VALUE SPACE.
           02  F                 PIC X(1)   VALUE "4".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "24.0".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "24.5".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "25.0".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "25.5".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "26.0".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "26.5".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "27.0".
           02  F                 PIC X(1)   VALUE SPACE.
           02  F                 PIC X(4)   VALUE "27.5".
           02  F                 PIC X(15)  VALUE SPACE.
           02  F                 PIC N(2)   VALUE   "Å@åv".
           02  F                 PIC X(11)  VALUE SPACE.
       01  MID9-R.
           02  F                 PIC X(50)  VALUE
                "••••••••••••••••••••••••••••••••••••••••••••••••••".
           02  F                 PIC X(50)  VALUE
                "••••••••••••••••••••••••••••••••••••••••••••••••••".
           02  F                 PIC X(36)  VALUE
                "••••••••••••••••••••••••••••••••••••".
       01  WR1-R.
           02  WR1-K1            PIC X(05).
           02  WR1-DCM           PIC N(02).
           02  F                 PIC X(01).
           02  WR1-SNO           PIC 9(06).
           02  F                 PIC X(01).
           02  WR1-TCD           PIC 9(04).
           02  F                 PIC X(01).
           02  WR1-TNA           PIC N(26).
           02  F                 PIC X(02).
           02  WR1-CCD           PIC 9(03).
           02  WR1-CCDC  REDEFINES WR1-CCD   PIC X(03).
           02  F                 PIC X(01).
           02  WR1-CNA           PIC N(26).
           02  F                 PIC X(01).
           02  WR1-DATE          PIC 99/99/99.
           02  F                 PIC X(02).
           02  WR1-SET           PIC ZZ9.
           02  F                 PIC X(02).
           02  WR1-ONO           PIC 9(06).
           02  F                 PIC X(01).
           02  WR1-UNA           PIC N(06).
           02  WR1-KSU           PIC -(4).
           02  WR1-KSUC  REDEFINES WR1-KSU   PIC X(4).
           02  WR1-K2            PIC X(05).
       01  WR2-R.
           02  WR2-K1            PIC X(05).
           02  F                 PIC X(22).
           02  WR2-HCD           PIC 9(06).
           02  F                 PIC X(01).
           02  WR2-HNA           PIC N(24).
           02  F                 PIC X(01).
           02  WR2-SIZ           PIC 9(01).
           02  WR2-SU01          PIC -(05).
           02  WR2-SU02          PIC -(05).
           02  WR2-SU03          PIC -(05).
           02  WR2-SU04          PIC -(05).
           02  WR2-SU05          PIC -(05).
           02  WR2-SU06          PIC -(05).
           02  WR2-SU07          PIC -(05).
           02  WR2-SU08          PIC -(05).
           02  WR2-SU09          PIC -(05).
           02  WR2-SU10          PIC -(05).
           02  WR2-SUT           PIC ----,--9.
           02  F                 PIC X(01).
           02  WR2-BI            PIC X(10).
           02  WR2-K2            PIC X(05).
       01  WR3-R.
           02  WR3-K1            PIC X(05).
           02  F                 PIC X(48).
           02  WR3-TEKM          PIC N(02).
           02  WR3-TEKC          PIC X(01).
           02  WR3-TEK1          PIC N(10).
           02  F                 PIC X(02).
           02  WR3-TEK2          PIC N(24).
           02  F                 PIC X(09).
           02  WR3-KEIM          PIC N(02).
           02  WR3-KEI           PIC ----,--9.
           02  F                 PIC X(11).
           02  WR3-K2            PIC X(05).
       01  W-SUT                 PIC S9(6).
       01  LCNT                  PIC 9(2).
       01  PAGE-C                PIC 9(3).
       01  GOKEI                 PIC S9(6).
       01  SEN-W                 PIC 9.
       01  DEN-W                 PIC 9(6).
       01  DEN-W1                PIC 9(6).
       01  DEN-W2                PIC 9(6).
       01  KAKU-W                PIC 9.
       01  ERR-STAT              PIC X(2).
       01  OLD-OKNO              PIC 9(6)  VALUE  0.
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
           02  W-ENGP.
               03  W-ENEN        PIC 9(04).
               03  W-ENENL  REDEFINES W-ENEN.
                   04  W-ENEN1   PIC 9(02).
                   04  W-ENEN2   PIC 9(02).
               03  W-EGET        PIC 9(02).
               03  W-EPEY        PIC 9(02).
           COPY  LWMSG.
      *
           COPY  L-JSTR.
           COPY  L-JCON.
           COPY  LITCM.
           COPY  LIHIM2.
      *FD  PRN-F
       77  PRN-R                 PIC X(256).
      *
       77  END-STS               PIC  X(002).
       77  RESU                  PIC  9(001).
       77  RESP                  PIC  9(001).
       77  RET                   PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER         PIC  9(003).
       77  USER_ID               PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE       PIC  X(003) VALUE ZERO.
      *
       01  ACP-NYURYOKU.
           02  ACP-SEN.
               03  01ACP-SEN PIC 9 .
           02  ACP-SNGP.
               03  ACP-SNEN  PIC 9(02).
               03  ACP-SGET  PIC 9(02).
               03  ACP-SPEY  PIC 9(02).
           02  ACP-ENGP.
               03  ACP-ENEN  PIC 9(02).
               03  ACP-EGET  PIC 9(02).
               03  ACP-EPEY  PIC 9(02).
           02  ACP-DEN1.
               03  01ACP-DEN1  PIC 9(6).
           02  ACP-DEN2.
               03  01ACP-DEN2  PIC 9(6).
           02  ACP-KAKU.
               03  01ACP-KAKU  PIC 9 .
       01  DSP-GAMEN.
           02  FILLER  PIC  X(018) VALUE
                 "èoÅ@â◊Å@éwÅ@ê}Å@èë".
           02  FILLER.
               03  FILLER  PIC  9(02).
               03  FILLER  PIC  N(01) VALUE   "îN".
               03  FILLER  PIC  9(02).
               03  FILLER  PIC  N(01) VALUE   "åé".
               03  FILLER  PIC  9(02).
               03  FILLER  PIC  N(01) VALUE   "ì˙".
           02  FILLER  PIC  X(010) VALUE    "ÇPÅ@î≠Å@çs".
           02  FILLER.
               03  FILLER  PIC  X(018) VALUE   "ÇQÅ@çƒî≠çsÅ@Å@ëIë".
               03  FILLER  PIC  X(001) VALUE   "[".
               03  FILLER  PIC  X(001) VALUE   "]".
           02  FILLER  PIC  X(008) VALUE    "ÇeÇqÇnÇl".
           02  FILLER  PIC  X(004) VALUE    "ÇsÇn".
           02  FILLER.
               03  FILLER  PIC  X(006) VALUE    "ämîFÅi".
               03  FILLER  PIC  X(009) VALUE    "OK=1,NO=9".
               03  FILLER  PIC  X(002) VALUE    "Åj".
               03  FILLER  PIC  X(010) VALUE    "--->  ÿ¿∞›".
       01  DSP-GAMEN1.
           02  DSP-DNOM   PIC  X(006) VALUE    "ì`ï[áÇ".
           02  DSP-DATM.
               03  FILLER  PIC  X(006) VALUE    "ì˙Å@ït".
               03  FILLER  PIC  X(008) VALUE  "  /  /  ".
               03  FILLER  PIC  X(008) VALUE  "  /  /  ".
       01  DSP-CLER.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  DSP-CLER2.
           02  FILLER  PIC  X(008) PIC X(8)  VALUE "        ".
           02  FILLER  PIC  X(008) PIC X(8)  VALUE "        ".
       01  DSP-MI.
           02  FILLER  PIC  X(026) VALUE
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
            "01ACP-SEN" "9" "7" "45" "1" " " "ACP-SEN" RETURNING RESU.
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
            "ACP-DEN1" " " "17" "0" "6" "ACP-ENGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-DEN1" "9" "17" "31" "6" " " "ACP-DEN1" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-DEN1" BY REFERENCE DEN-W1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-DEN2" " " "19" "0" "6" "ACP-DEN1" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-DEN2" "9" "19" "31" "6" " " "ACP-DEN2" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-DEN2" BY REFERENCE DEN-W2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KAKU" " " "24" "0" "1" "ACP-DEN2" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-KAKU" "9" "24" "63" "1" " " "ACP-KAKU" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-KAKU" BY REFERENCE KAKU-W "1" "0" RETURNING RESU.
      *DSP-GAMEN
       CALL "SD_Init" USING 
            "DSP-GAMEN" " " "0" "0" "99" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-GAMEN" "RX" "1" "26" "18" " " "DSP-GAMEN"
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
            "03DSP-GAMEN" "X" "5" "25" "10" "02DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-GAMEN" " " "7" "0" "20" "03DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0104DSP-GAMEN" "X" "7" "25" "18" " " "04DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0204DSP-GAMEN" "X" "7" "44" "1" "0104DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0304DSP-GAMEN" "X" "7" "46" "1" "0204DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-GAMEN" "X" "17" "21" "8" "04DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-GAMEN" "X" "19" "21" "4" "05DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-GAMEN" " " "24" "0" "27" "06DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0107DSP-GAMEN" "X" "24" "41" "6" " " "07DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0207DSP-GAMEN" "X" "24" "47" "9" "0107DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0307DSP-GAMEN" "X" "24" "56" "2" "0207DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0407DSP-GAMEN" "X" "24" "58" "10" "0307DSP-GAMEN" " "
            RETURNING RESU.
      *DSP-GAMEN1
       CALL "SD_Init" USING 
            "DSP-GAMEN1" " " "0" "0" "28" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DNOM" "X" "15" "31" "6" " " "DSP-GAMEN1"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DATM" " " "0" "0" "22" "DSP-DNOM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-DATM" "X" "15" "31" "6" " " "DSP-DATM"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-DATM" "X" "17" "31" "8" "01DSP-DATM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-DATM" "X" "19" "31" "8" "02DSP-DATM" " "
            RETURNING RESU.
      *DSP-CLER
       CALL "SD_Init" USING 
            "DSP-CLER" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-CLER" "X" "1" "0" "12" " " "DSP-CLER" RETURNING RESU.
      *DSP-CLER2
       CALL "SD_Init" USING 
            "DSP-CLER2" " " "0" "0" "16" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-CLER2" "X" "17" "31" "8" " " "DSP-CLER2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-CLER2" "X" "19" "31" "8" "01DSP-CLER2" " "
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
      *
       MAINLINE-ROUTINE.
           PERFORM  INIT-RTN  THRU  INIT-RTN-EXIT.
           PERFORM  SLCT-RTN  THRU  SLCT-RTN-EXIT.
           IF  END-STS  =  "P9"
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  SEN-W    =  2
               MOVE    "çƒ"     TO M1-SAI
           END-IF.
       MAINLINE-010.
           ADD   1    TO   DAT-SW.
           IF  DAT-SW   =  3
               CALL "DB_Close"
               STOP  RUN
           END-IF
           PERFORM  OPEN-RTN  THRU  OPEN-RTN-EXIT.
           PERFORM  PROC-RTN  THRU  PROC-RTN-EXIT
               UNTIL  NOT  (END-SW  NOT  =  9).
      *
           PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT.
           MOVE  0  TO  GOKEI  PAGE-C  END-SW  RED-SW  TCM-SW.
           MOVE  SPACE  TO  WR1-R  WR2-R  WR3-R.
           IF  LCNT NOT =  90
               MOVE  70   TO LCNT
           END-IF
           GO  TO  MAINLINE-010.
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
           MOVE 90  TO  LCNT.
           MOVE  0  TO  GOKEI  PAGE-C.
      *
           MOVE  SPACE  TO  WR1-R  WR2-R  WR3-R.
           MOVE  ZERO   TO  W-DATA.
           ACCEPT W-DATE FROM DATE.
           MOVE  W-DATE TO  M1-DATE.
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
           IF  SEN-W         =  2
               CALL "SD_Output" USING
                "DSP-DNOM" DSP-DNOM "p" RETURNING RESU
               GO  TO  SLCT-040
           END-IF
           PERFORM  SEL-RTN     THRU  SEL-EX.
           IF  W-SNGP        =  ZERO
               CALL "SD_Output" USING
                "ERR-02" ERR-02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  SLCT-020
           END-IF
           CALL "SD_Output" USING
            "DSP-DATM" DSP-DATM "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACP-SNGP" ACP-SNGP "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACP-ENGP" ACP-ENGP "p" RETURNING RESU.
       SLCT-040.
           IF  SEN-W    =  1
               GO  TO  SLCT-060
           END-IF
           CALL "SD_Accept" USING BY REFERENCE 01ACP-DEN1 "01ACP-DEN1"
            "9" "6" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               CALL "SD_Output" USING
                "DSP-CLER2" DSP-CLER2 "p" RETURNING RESU
               GO  TO  SLCT-020
           END-IF
           IF  END-STS  NOT  =  "01"
               GO  TO  SLCT-040
           END-IF.
       SLCT-050.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-DEN2 "01ACP-DEN2"
            "9" "6" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-040
           END-IF
           IF  END-STS  NOT  =  "01"
               GO  TO  SLCT-050
           END-IF
           IF  DEN-W1  >  DEN-W2
               GO  TO  SLCT-040
           END-IF
           GO  TO  SLCT-060.
       SLCT-052.
           CALL "SD_Accept" USING BY REFERENCE ACP-SNEN "ACP-SNEN"
            "9" "2" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-020
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
           IF  W-SNGP        <  W-SDATE  OR  >  W-EDATE
               GO  TO  SLCT-052
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
           IF  W-ENGP        <  W-SDATE  OR  >  W-EDATE
               GO  TO  SLCT-056
           END-IF.
       SLCT-060.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-KAKU "01ACP-KAKU"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               IF  SEN-W  =  1
                   GO  TO  SLCT-052
               ELSE
                   GO  TO  SLCT-040
               END-IF
           END-IF
           IF  END-STS  NOT  =  "01"
               GO  TO  SLCT-060
           END-IF
           IF  KAKU-W  =  1
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  KAKU-W  =  9
               GO  TO  SLCT-010
           ELSE
               GO  TO  SLCT-060
           END-IF.
       SLCT-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÇqÇdÇ`ÇcèàóùÅiÇPÅj  ÅiÇqÇdÇ`ÇcÇPÅ|ÇqÇsÇmÅj                  *
      ******************************************************************
       READ1-RTN.
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
      *           READ  JSTR  NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
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
           IF  DAT-SW   =  1
               IF  JSTR-061 NOT =  9850
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  DAT-SW   =  2
               IF  JSTR-061 NOT =  5000
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JSTR-17   NOT =  9
               GO  TO  READ1-020
           END-IF
           IF  JSTR-07  NOT =  5  AND  6
               GO  TO  READ1-020
           END-IF
           IF  JSTR-03   NOT  =  0
               GO  TO  READ1-020
           END-IF
           IF  JSTR-158  NOT  =  0
               GO  TO  READ1-020
           END-IF
           IF  JSTR-04        <  W-SNGP  OR  > W-ENGP
               GO  TO  READ1-020
           END-IF
           MOVE  1  TO  RED-SW.
           GO  TO  READ1-RTN-EXIT.
       READ1-020.
      *           READ  JSTR  NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  9  TO  END-SW
               GO  TO  READ1-RTN-EXIT
           END-IF
           IF  DAT-SW   =  1
               IF  JSTR-061 NOT =  9850
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  DAT-SW   =  2
               IF  JSTR-061 NOT =  5000
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JSTR-17   NOT =  9
               GO  TO  READ1-020
           END-IF
           IF  JSTR-07  NOT =  5  AND  6
               GO  TO  READ1-020
           END-IF
           IF  JSTR-03   NOT  =  0
               GO  TO  READ1-020
           END-IF
           IF  JSTR-158  NOT  =  0
               GO  TO  READ1-020
           END-IF
           IF  JSTR-04        <  W-SNGP  OR  > W-ENGP
               GO  TO  READ1-020
           END-IF
           MOVE  1  TO  RED-SW.
           GO  TO  READ1-RTN-EXIT.
       READ1-030.
      *    (º≠Ø∂•ªºΩﬁ•ƒ◊› … ÿ∞ƒﬁ (ª≤ Ø∫≥))
           IF  RED-SW  =  1
               GO  TO  READ1-040
           END-IF
           MOVE  DEN-W1  TO  JSTR-01.
           MOVE  0       TO  JSTR-02.
      *           START  JSTR  KEY  IS  NOT  <  JSTR-KEY  INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            JSTR_PNAME1 "JSTR-KEY" " NOT < " JSTR-KEY RETURNING RET.
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
           END-IF.
       READ1-040.
      *           READ  JSTR  NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  9  TO  END-SW
               GO  TO  READ1-RTN-EXIT
           END-IF
           IF  DEN-W2  <  JSTR-01  MOVE  9  TO  END-SW
               GO  TO  READ1-RTN-EXIT
           END-IF
           IF  DAT-SW   =  1
               IF  JSTR-061 NOT =  9850
                   GO  TO  READ1-040
               END-IF
           END-IF
           IF  DAT-SW   =  2
               IF  JSTR-061 NOT =  5000
                   GO  TO  READ1-040
               END-IF
           END-IF
           IF  JSTR-17       =  0
               GO  TO  READ1-040
           END-IF
           IF  JSTR-07  NOT =  5  AND  6
               GO  TO  READ1-040
           END-IF
           IF  JSTR-03   NOT  =  0
               GO  TO  READ1-040
           END-IF
           IF  JSTR-158  NOT  =  1
               GO  TO  READ1-040
           END-IF
           MOVE  1  TO  RED-SW.
       READ1-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÇqÇdÇ`ÇcèàóùÅiÇQÅj  ÅiÇqÇdÇ`ÇcÇQÅ|ÇqÇsÇmÅj                  *
      ******************************************************************
       READ2-RTN.
           MOVE  0         TO  TCM-SW.
       READ2-010.
      *    (¡Æ∏ø≥ª∑•œΩ¿ … ÿ∞ƒﬁ)
           MOVE  JSTR-061  TO  TC-TCD.
           MOVE  1         TO  TC-CCD.
      *           READ  TC-M  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  JSTR-061  TO  WR1-TCD
               MOVE  SPACE     TO  WR1-TNA
               GO  TO  READ2-020
           END-IF
           MOVE  JSTR-061  TO  WR1-TCD.
           MOVE  TC-NAME    TO  WR1-TNA.
       READ2-020.
           IF  JSTR-062  =  1
               MOVE  SPACE  TO  WR1-CCDC
               MOVE  SPACE  TO  WR1-CNA
               GO  TO  READ2-030
           ELSE
               MOVE  JSTR-061  TO  TC-TCD
               MOVE  JSTR-062  TO  TC-CCD
      *               READ  TC-M  UNLOCK  INVALID  KEY
      *///////////////
               CALL "DB_Read" USING
                "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
                RETURNING RET
               IF  RET = 1
                   MOVE  JSTR-062  TO  WR1-CCD
                   MOVE  SPACE     TO  WR1-CNA
                   GO  TO  READ2-030
               END-IF
           END-IF
           MOVE  JSTR-062  TO  WR1-CCD.
           MOVE  TC-NAME   TO  WR1-CNA.
       READ2-030.
           MOVE  JSTR-09  TO  HI-MHCD HI-HCD.
      *           READ  HI2-M  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE  TO  WR2-HNA
               GO  TO  READ2-040
           END-IF
           MOVE  HI-NAME TO  WR2-HNA.
       READ2-040.
           MOVE  2        TO  JCON2-01.
           MOVE  JSTR-14  TO  JCON2-02.
      *           READ  JCON  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE  TO  WR1-UNA
               GO  TO  READ2-RTN-EXIT
           END-IF
           MOVE  JCON2-03  TO  WR1-UNA.
       READ2-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ñæç◊èoóÕèàóù  ÅiÇcÇdÇsÇkÅ|ÇqÇsÇmÅj                          *
      ******************************************************************
       DETL-RTN.
           IF  TCM-SW  =  1
               GO  TO  DETL-010
           END-IF
           IF  DEN-W  NOT  =  JSTR-01
               PERFORM  MEI1-RTN  THRU  MEI1-RTN-EXIT
           END-IF
           PERFORM  MEI2-RTN  THRU  MEI2-RTN-EXIT.
       DETL-010.
           IF  SEN-W  =  1
               MOVE  1  TO  JSTR-158
      *               REWRITE  JSTR-R  INVALID  KEY
      *///////////////
               CALL "DB_Update" USING
                JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET
               IF  RET = 1
                   MOVE  JSTR-KEY  TO  ERR-K
                   MOVE  "JSTR"    TO  ERR-F
                   MOVE  "R"       TO  ERR-M
                   MOVE   0        TO  ERR-LIN
                   CALL "SD_Arg_Match_Line" USING
                    "ERR-LIN" "2" ERR-LIN RETURNING RESU
                   PERFORM  ERR-RTN   THRU  ERR-EX
                   PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
                   CALL "DB_Close"
                   STOP  RUN
               END-IF
           END-IF
           MOVE  JSTR-01           TO  DEN-W.
           IF  JSTR-09  <  999900
               ADD   W-SUT       TO  GOKEI
           END-IF.
       DETL-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    çáåvèoóÕèàóù  ÅiÇsÇnÇsÇkÅ|ÇqÇsÇmÅj                          *
      ******************************************************************
       TOTL-RTN.
           IF  TCM-SW  NOT  =  0
               GO  TO  TOTL-RTN-EXIT
           END-IF
           IF  END-SW  =  9
               MOVE     "çáåv"        TO  WR3-KEIM
               MOVE   GOKEI           TO  WR3-KEI
               MOVE   WR3-R           TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               MOVE   MID9-R          TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               ADD   2         TO  LCNT
               GO  TO  TOTL-RTN-EXIT
           END-IF
           IF  JSTR-01  NOT  =  DEN-W
               MOVE     "çáåv"        TO  WR3-KEIM
               MOVE   GOKEI           TO  WR3-KEI
               MOVE   WR3-R           TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               MOVE   MID9-R          TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               ADD   2         TO  LCNT
               MOVE  0  TO  GOKEI
           END-IF.
       TOTL-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    å©èoÇµèoóÕèàóù  ÅiÇlÇhÇcÅ|ÇqÇsÇmÅj                        *
      ******************************************************************
       MID-RTN.
           IF  LCNT    NOT  =  90
               MOVE  SPACE  TO  PRN-R
               CALL "PR_Write" USING PRN-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           ADD   1         TO  PAGE-C.
           MOVE  PAGE-C          TO  M1-PAGE.
           MOVE  SPACE           TO  PRN-R.
           MOVE   MID1-R         TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE  SPACE           TO  PRN-R.
           MOVE   MID2-R         TO  PRN-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE  SPACE           TO  PRN-R.
           MOVE   MID3-R         TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE  SPACE           TO  PRN-R.
           MOVE   MID4-R         TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE  SPACE           TO  PRN-R.
           MOVE   MID5-R         TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE  SPACE           TO  PRN-R.
           MOVE   MID6-R         TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE  SPACE           TO  PRN-R.
           MOVE  7  TO  LCNT.
       MID-RTN-EX.
           EXIT.
       MEI1-RTN.
           IF  LCNT    NOT  <  61
               PERFORM  MID-RTN    THRU  MID-RTN-EX
           END-IF
           MOVE  W-15K           TO  WR1-K1  WR2-K1  WR3-K1.
           MOVE  W-20K           TO  WR1-K2  WR2-K2  WR3-K2.
           MOVE  JSTR-01         TO  WR1-SNO.
       MEI1-010.
           MOVE  JSTR-05S        TO  WR1-DATE.
           IF  JSTR-03  =  0
               MOVE    "èoâ◊"  TO  WR1-DCM
           END-IF
           IF  JSTR-03  =  7
               MOVE    "ÉTèo"  TO  WR1-DCM
           END-IF
           MOVE  JSTR-14B  TO  WR1-ONO.
           MOVE  JSTR-15A  TO  WR1-KSU.
           MOVE   WR1-R    TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE    "ìEóv"      TO  WR3-TEKM.
           MOVE  ":"           TO  WR3-TEKC.
           MOVE  JSTR-15   TO  WR3-TEK2.
           MOVE  JSTR-14D  TO  WR3-TEK1.
           MOVE  JSTR-14B  TO  OLD-OKNO.
           ADD   1         TO  LCNT.
       MEI1-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÇvÇqÇhÇsÇdèàóù  ÅiÇvÇqÇhÇsÇdÅ|ÇqÇsÇmÅj                      *
      ******************************************************************
       MEI2-RTN.
       MEI2-010.
           IF  LCNT    NOT  <  62
               PERFORM  MID-RTN    THRU  MID-RTN-EX
           END-IF
           MOVE  JSTR-09       TO  WR2-HCD.
           MOVE  JSTR-10       TO  WR2-SIZ.
           MOVE  JSTR-1111(1)   TO  WR2-SU01.
           MOVE  JSTR-1111(2)   TO  WR2-SU02.
           MOVE  JSTR-1111(3)   TO  WR2-SU03.
           MOVE  JSTR-1111(4)   TO  WR2-SU04.
           MOVE  JSTR-1111(5)   TO  WR2-SU05.
           MOVE  JSTR-1111(6)   TO  WR2-SU06.
           MOVE  JSTR-1111(7)   TO  WR2-SU07.
           MOVE  JSTR-1111(8)   TO  WR2-SU08.
           MOVE  JSTR-1111(9)   TO  WR2-SU09.
           MOVE  JSTR-1111(10)  TO  WR2-SU10.
           COMPUTE  W-SUT    =  JSTR-1111(1)  +  JSTR-1111(2)
                             +  JSTR-1111(3)  +  JSTR-1111(4)
                             +  JSTR-1111(5)  +  JSTR-1111(6)
                             +  JSTR-1111(7)  +  JSTR-1111(8)
                             +  JSTR-1111(9)  +  JSTR-1111(10).
       MEI2-020.
           MOVE  W-SUT     TO  WR2-SUT.
           MOVE  JSTR-20       TO  WR2-BI.
           MOVE   WR2-R    TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           ADD  1  TO  LCNT.
       MEI2-RTN-EXIT.
           EXIT.
       SEL-RTN.
           MOVE  ZERO    TO  W-SDATE  W-EDATE  W-SNGP  W-ENGP.
           CALL "DB_F_Open" USING
            "INPUT" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
       SEL-010.
      *           READ  JSTR    NEXT RECORD  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  SEL-090
           END-IF
           IF  JSTR-061 NOT =  9850  AND  5000
               GO  TO  SEL-010
           END-IF
           IF  JSTR-158   NOT =  0
               GO  TO  SEL-010
           END-IF
           IF  W-SDATE        =  ZERO
               MOVE  JSTR-04      TO  W-SDATE
           END-IF
           IF  W-SDATE        >  JSTR-04
               MOVE  JSTR-04      TO  W-SDATE
           END-IF
           IF  W-EDATE        <  JSTR-04
               MOVE  JSTR-04      TO  W-EDATE
           END-IF
           GO  TO  SEL-010.
       SEL-090.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           MOVE  W-SDATE TO  W-SNGP.
           MOVE  W-EDATE TO  W-ENGP.
       SEL-EX.
           EXIT.
      ******************************************************************
      *    ÇnÇoÇdÇmèàóù  ÅiÇnÇoÇdÇmÅ|ÇqÇsÇmÅj                          *
      ******************************************************************
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON2-KEY" BY REFERENCE JCON2-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
       OPEN-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÇbÇkÇnÇrÇdèàóù  ÅiÇbÇkÇrÇdÅ|ÇqÇsÇmÅj                        *
      ******************************************************************
       CLSE-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       CLSE-RTN-EXIT.
           EXIT.
           COPY  LPERR.

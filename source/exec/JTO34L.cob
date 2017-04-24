       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       JTO34L.
      ******************************************************************
      *    ÉiÉtÉRÅEÉèÅ[ÉNÉ}Éìèoâ◊éwê}èë  Åièoâ◊é¿ê—ÇeÅj                *
      *    JS-SIGN : ìñì˙=0 , à»ëO=1                                   *
      ******************************************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       DATA              DIVISION.
       WORKING-STORAGE   SECTION.
       77  JS-SIGN                PIC  9(001).
       77  WK0256ID               PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1            PIC  X(003).
           02  STN-NO2            PIC  X(003).
       01  W-FID.
           02  W-FID1             PIC  X(006) VALUE "WK0256".
           02  W-FID2             PIC  X(003).
       01  END-SW                PIC 9      VALUE 0.
       01  RED-SW                PIC 9      VALUE 0.
       01  MID1.
           02  F                  PIC  X(05)  VALUE X"1A24212474".
           02  F                  PIC  X(42)  VALUE  SPACE.
           02  F                  PIC  N(24)  VALUE
                "ÅñÅñÅñÅ@Å@èoâ◊éwê}é¿ê—ÉäÉXÉgÅiéÛêMï™ÅjÅ@Å@ÅñÅñÅñ".
           02  F                  PIC  X(18)  VALUE  SPACE.
           02  F                  PIC  X(05)  VALUE  "DATE.".
           02  M-YY               PIC  9(02).
           02  F                  PIC  X(01)  VALUE  "/".
           02  M-MM               PIC Z9.
           02  F                  PIC  X(01)  VALUE  "/".
           02  M-DD               PIC Z9.
           02  F                  PIC  X(10)  VALUE  SPACE.
           02  F                  PIC  X(02)  VALUE  "P.".
           02  WPCNT              PIC ZZ9.
       01  MID2.
           02  F                  PIC  X(05)  VALUE X"1A24212078".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  N(04)  VALUE "ì˙Å@ïtÅ@".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  N(04)  VALUE "éwê}áÇÅ@".
           02  F                  PIC  X(04)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "∫∞ƒﬁ".
           02  F                  PIC  X(03)  VALUE  SPACE.
           02  F                  PIC  N(10)  VALUE
                                              "ìæÅ@Å@à”Å@Å@êÊÅ@Å@ñº".
           02  F                  PIC  X(25)  VALUE  SPACE.
           02  F                  PIC  N(10)  VALUE
                                              "íºÅ@Å@ëóÅ@Å@êÊÅ@Å@ñº".
           02  F                  PIC  X(25)  VALUE  SPACE.
           02  F                  PIC  N(04)  VALUE "ëqÅ@Å@å…".
           02  F                  PIC  X(04)  VALUE  SPACE.
           02  F                  PIC  N(04)  VALUE "â^Å@Å@ëó".
           02  F                  PIC  X(04)  VALUE  SPACE.
           02  F                  PIC  N(04)  VALUE "ëóÇËèÛáÇ".
           02  F                  PIC  X(03)  VALUE  SPACE.
       01  MID3.
           02  F                  PIC  X(10)  VALUE  SPACE.
           02  F                  PIC  X(01)  VALUE  "-".
           02  F                  PIC  N(02)  VALUE "çsÅ@".
           02  F                  PIC  X(06)  VALUE  "∫∞ƒﬁ  ".
           02  F                  PIC  N(06)  VALUE "ïiÅ@Å@Å@ñºÅ@".
           02  F                  PIC  X(28)  VALUE  SPACE.
           02  F                  PIC  X(01)  VALUE  "1".
           02  F                  PIC  X(03)  VALUE  SPACE.
           02  F                  PIC  N(02)  VALUE "ÇRçÜ".
           02  F                  PIC  X(03)  VALUE  SPACE.
           02  F                  PIC  N(02)  VALUE "ÇQçÜ".
           02  F                  PIC  X(04)  VALUE  SPACE.
           02  F                  PIC  X(02)  VALUE  "SS".
           02  F                  PIC  X(05)  VALUE  SPACE.
           02  F                  PIC  X(01)  VALUE   "S".
           02  F                  PIC  X(05)  VALUE  SPACE.
           02  F                  PIC  X(01)  VALUE   "M".
           02  F                  PIC  X(05)  VALUE  SPACE.
           02  F                  PIC  X(01)  VALUE   "L".
           02  F                  PIC  X(04)  VALUE  SPACE.
           02  F                  PIC  X(02)  VALUE   "LL".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "28.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "29.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "30.0".
           02  F                  PIC  X(18)  VALUE  SPACE.
       01  MID4.
           02  F                  PIC  X(57)  VALUE  SPACE.
           02  F                  PIC  X(01)  VALUE  "2".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "12.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "13.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "13.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "14.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "15.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "16.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "17.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "18.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "19.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "20.0".
           02  F                  PIC  X(18)  VALUE  SPACE.
       01  MID5.
           02  F                  PIC  X(57)  VALUE  SPACE.
           02  F                  PIC  X(01)  VALUE  "3".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "21.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "21.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "22.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "22.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "23.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "23.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "24.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "24.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "25.0".
           02  F                  PIC  X(24)  VALUE  SPACE.
       01  MID6.
           02  F                  PIC  X(57)  VALUE  SPACE.
           02  F                  PIC  X(01)  VALUE  "4".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "24.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "24.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "25.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "25.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "26.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "26.5".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "27.0".
           02  F                  PIC  X(02)  VALUE  SPACE.
           02  F                  PIC  X(04)  VALUE  "27.5".
           02  F                  PIC  X(16)  VALUE  SPACE.
           02  F                  PIC  N(02)  VALUE "Å@åv".
           02  F                  PIC  X(01)  VALUE  SPACE.
           02  F                  PIC  N(04)  VALUE "îıÅ@çlÅ@".
           02  F                  PIC  X(04)  VALUE  SPACE.
       01  MID9.
           02  F                  PIC  X(50)  VALUE
                "--------------------------------------------------".
           02  F                  PIC  X(50)  VALUE
                "--------------------------------------------------".
           02  F                  PIC  X(36)  VALUE
                "------------------------------------".
       01  W-PR1.
           02  P1-15B             PIC X(05).
           02  P1-0712            PIC 9(02).
           02  P1-A               PIC X(01).
           02  P1-072             PIC Z9.
           02  P1-B               PIC X(01).
           02  P1-073             PIC Z9.
           02  F                  PIC X(02).
           02  P1-03              PIC 9(06).
           02  F                  PIC X(02).
           02  P1-021             PIC 9(04).
           02  P1-C               PIC X(01).
           02  P1-022             PIC X(03).
           02  F                  PIC X(01).
           02  P1-021N            PIC N(26).
           02  F                  PIC X(01).
           02  P1-022N            PIC N(26).
           02  F                  PIC X(01).
           02  P1-01N             PIC N(06).
           02  F                  PIC X(01).
           02  P1-14N             PIC N(06).
           02  F                  PIC X(01).
           02  P1-14B             PIC 9(06).
           02  F                  PIC X(03).
       01  W-PR2.
           02  F                  PIC X(10).
           02  P2-D               PIC X(01).
           02  P2-04              PIC 9(01).
           02  F                  PIC X(01).
           02  P2-09              PIC 9(06).
           02  F                  PIC X(01).
           02  P2-09N             PIC N(24).
           02  F                  PIC X(01).
           02  P2-10              PIC 9(01).
           02  P2-12101           PIC --,---.
           02  P2-12102           PIC --,---.
           02  P2-12103           PIC --,---.
           02  P2-12104           PIC --,---.
           02  P2-12105           PIC --,---.
           02  P2-12106           PIC --,---.
           02  P2-12107           PIC --,---.
           02  P2-12108           PIC --,---.
           02  P2-12109           PIC --,---.
           02  P2-12110           PIC --,---.
           02  P2-122             PIC ---,---.
           02  P2-122D            PIC X(01).
           02  P2-20              PIC X(10).
       01  W-PR3.
           02  F                  PIC X(02).
           02  P3-E               PIC X(01).
           02  P3-062             PIC Z9.
           02  P3-F               PIC X(01).
           02  P3-033             PIC Z9.
           02  P3-G               PIC X(01).
           02  F                  PIC X(42).
           02  P3-14DM            PIC N(02).
           02  P3-H               PIC X(01).
           02  P3-14D             PIC N(10).
           02  F                  PIC X(01).
           02  P3-15M             PIC N(02).
           02  P3-I               PIC X(01).
           02  P3-15              PIC N(24).
           02  F                  PIC X(03).
           02  P3-TOM             PIC N(02).
           02  P3-J               PIC X(01).
           02  P3-TO              PIC ---,---.
           02  F                  PIC X(02).
           02  P3-15AM            PIC N(02).
           02  P3-K               PIC X(01).
           02  P3-15A             PIC Z,ZZ9.
           02  P3-15AC  REDEFINES P3-15A   PIC X(05).
           02  P3-2B              PIC X(05).
       01  W-PR8.
           02  P8-15B             PIC X(05).
           02  F                  PIC X(118).
           02  P8-TOM             PIC N(06).
           02  P8-J               PIC X(01).
           02  P8-TO              PIC ----,---.
           02  P8-2B              PIC X(05).
       01  WORK-92               PIC S9(6).
       01  I                     PIC 9.
       01  J                     PIC 9(2).
       01  PCNT                  PIC 9(3).
       01  PAGE-C                PIC 9.
       01  GOKEI                 PIC S9(6).
       01  SEN-W                 PIC 9.
       01  W-DATE.
           02  W-NGP             PIC 9(8).
           02  W-NGPD   REDEFINES  W-NGP.
               03  W-NEN1        PIC 9(2).
               03  W-NEN2        PIC 9(2).
               03  W-GET         PIC 9(2).
               03  W-PEY         PIC 9(2).
       01  DEN-W                 PIC 9(6).
       01  W-TOK                 PIC 9(4).
       01  W-TOKT                PIC S9(6).
       01  FROM-WK.
           02  KURA-W1           PIC 9(1).
           02  TKU-W1.
               03  TKU-W11       PIC 9(4).
               03  TKU-W12       PIC 9(3).
           02  DEN-W1            PIC 9(6).
       01  TO-WK.
           02  KURA-W2           PIC 9(1).
           02  TKU-W2.
               03  TKU-W21       PIC 9(4).
               03  TKU-W22       PIC 9(3).
           02  DEN-W2            PIC 9(6).
       01  KAKU-W                PIC 9.
       01  ERR-STAT              PIC X(2).
       01  WYMD.
           02  WYY               PIC 9(2).
           02  WMM               PIC 9(2).
           02  WDD               PIC 9(2).
       01  KEY-WK.
           02  KEY-W1            PIC 9(1).
           02  KEY-W2            PIC X(7).
           02  KEY-W3            PIC 9(6).
       01  OLD-OKNO              PIC 9(6)  VALUE 0.
      **********************
           COPY  LWMSG.
      **********************
           COPY  L-JSJD.
           COPY  L-JCON.
           COPY  LITCM.
           COPY  LIHIM2.
           COPY  LITM.
           COPY  LITHTM.
      *FD  JSJW
       01  JSJW_JTO34L.
           02  JSJW_PNAME1        PIC  X(009) VALUE SPACE.
           02  F                  PIC  X(001).
           02  JSJW_LNAME         PIC  X(011) VALUE "JSJW_JTO34L".
           02  F                  PIC  X(001).
           02  JSJW_KEY1          PIC  X(100) VALUE SPACE.
           02  JSJW_SORT          PIC  X(100) VALUE SPACE.
           02  JSJW_IDLST         PIC  X(100) VALUE SPACE.
           02  JSJW_RES           USAGE  POINTER.
       01  JSJW-REC.
           02  JSJW-KEY.
               03  JSJW-01        PIC 9(01).
               03  JSJW-02.
                   04  JSJW-021   PIC 9(04).
                   04  JSJW-022   PIC 9(03).
               03  JSJW-KEY2.
                   04  JSJW-03    PIC 9(06).
                   04  JSJW-04    PIC 9(01).
           02  JSJW-05            PIC 9(01).
           02  JSJW-06.
               03  JSJW-061       PIC 9(04).
               03  JSJW-061L  REDEFINES  JSJW-061.
                   04  JSJW-061   PIC 9(02).
                   04  JSJW-062   PIC 9(02).
               03  JSJW-062       PIC 9(02).
               03  JSJW-063       PIC 9(02).
           02  JSJW-06L   REDEFINES  JSJW-06.
               03  F              PIC 9(02).
               03  JSJW-06S       PIC 9(06).
           02  JSJW-07.
               03  JSJW-071       PIC 9(04).
               03  JSJW-071L  REDEFINES  JSJW-071.
                   04  JSJW-0711  PIC 9(02).
                   04  JSJW-0712  PIC 9(02).
               03  JSJW-072       PIC 9(02).
               03  JSJW-073       PIC 9(02).
           02  JSJW-07L   REDEFINES  JSJW-07.
               03  F              PIC 9(02).
               03  JSJW-07S       PIC 9(06).
           02  JSJW-08.
               03  JSJW-081       PIC 9(06).
               03  JSJW-082       PIC 9(01).
           02  JSJW-09            PIC 9(06).
           02  JSJW-10            PIC 9(01).
           02  JSJW-11.
               03  JSJW-111   OCCURS  10.
                   04  JSJW-1111      PIC S9(04).
               03  JSJW-112       PIC S9(05).
           02  JSJW-12.
               03  JSJW-121   OCCURS  10.
                   04  JSJW-1211      PIC S9(04).
               03  JSJW-122       PIC S9(05).
           02  JSJW-13            PIC 9(01).
           02  JSJW-14            PIC 9(01).
           02  JSJW-14A           PIC 9(03).
           02  JSJW-14B           PIC 9(06).
           02  JSJW-14C           PIC 9(02).
           02  JSJW-14D           PIC N(09).
           02  JSJW-15            PIC N(23).
           02  JSJW-20            PIC X(10).
           02  JSJW-15A           PIC S9(03).
           02  FILLER             PIC X(18).
           02  JSJW-NGP           PIC X(08).
           02  JSJW-19            PIC X(01).
           02  JSJW-158           PIC 9(01).
           02  JSJW-16            PIC 9(01).
           02  JSJW-17            PIC 9(01).
       77  F                      PIC X(01).
      *FD  SP-F
       77  SP-R                   PIC  X(180).
      **********************
       77  END-STS                PIC  X(002).
       77  RESU                   PIC  9(001).
       77  RESP                   PIC  9(001).
       77  RET                    PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER          PIC  9(003).
       77  USER_ID                PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE        PIC  X(003) VALUE ZERO.
      *
       01  ACP-NYURYOKU.
           02  FILLER.
               03  ACP-NEN2     PIC 9(2).
               03  ACP-GET      PIC 9(2).
               03  ACP-PEY      PIC 9(2).
           02  ACP-SEN.
               03  01ACP-SEN    PIC 9 .
           02  ACP-KURA1.
               03  01ACP-KURA1  PIC 9(1).
           02  ACP-KURA2.
               03  01ACP-KURA2  PIC 9(1).
           02  ACP-TKU1.
               03  ACP-TKU11    PIC 9(4).
               03  ACP-TKU12    PIC 9(3).
           02  ACP-TKU2.
               03  ACP-TKU21    PIC 9(4).
               03  ACP-TKU22    PIC 9(3).
           02  ACP-DEN1.
               03  01ACP-DEN1   PIC 9(6).
           02  ACP-DEN2.
               03  01ACP-DEN2   PIC 9(6).
           02  ACP-KAKU.
               03  01ACP-KAKU   PIC 9 .
       01  DSP-GAMEN.
           02  FILLER    PIC  X(10) VALUE  "ÅyîíéÜópÅz".
           02  FILLER    PIC  X(40) VALUE
                 " ÉiÉtÉRÅEÉèÅ[ÉNÉ}ÉìÅ@èoâ◊éwê}èëÅié¿ê—Åj ".
           02  FILLER    PIC  X(06) VALUE    "ëq∫∞ƒﬁ".
           02  FILLER    PIC  X(06) VALUE  "íºëóêÊ".
           02  FILLER    PIC  X(06) VALUE  "ì`ï[áÇ".
           02  FILLER    PIC  X(08) VALUE  "ÇeÇqÇnÇl".
           02  FILLER    PIC  X(01) VALUE    "-".
           02  FILLER    PIC  X(04) VALUE  "ÇsÇn".
           02  FILLER    PIC  X(01) VALUE    "-".
           02  FILLER.
               03  FILLER    PIC  X(06) VALUE  "ämîFÅi".
               03  FILLER    PIC  X(09) VALUE    "OK=1,NO=9".
               03  FILLER    PIC  X(02) VALUE  "Åj".
               03  FILLER    PIC  X(10) VALUE    "--->  ÿ¿∞›".
       01  DSP-GAMEN1.
           02  FILLER    PIC  X(10) VALUE  "ÇPÅ@î≠Å@çs".
           02  FILLER.
               03  FILLER    PIC  X(18) VALUE "ÇQÅ@çƒî≠çsÅ@Å@ëIë".
               03  FILLER    PIC  X(01) VALUE   "[".
               03  FILLER    PIC  X(01) VALUE   "]".
       01  DSP-GAMEN2.
           02  FILLER    PIC  X(08) VALUE "ìñì˙à»ëO".
           02  FILLER    PIC  X(18) VALUE "'  îN   åé   ì˙ ï™".
       01  DSP-CLER.
           02  FILLER    PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-CLER2.
           02  FILLER    PIC X(23) VALUE "  ".
           02  FILLER    PIC X(01) VALUE    "-".
           02  FILLER    PIC X(23) VALUE "  ".
           02  FILLER    PIC X(01) VALUE    "-".
           COPY  LSERR.
      *
       PROCEDURE         DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *ACP-NYURYOKU
       CALL "SD_Init" USING 
            "ACP-NYURYOKU" " " "0" "0" "36" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-NYURYOKU" " " "8" "0" "6" " " "ACP-NYURYOKU"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-NEN2" "9" "8" "24" "2" " " "01ACP-NYURYOKU"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-NEN2" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-GET" "9" "8" "29" "2" "ACP-NEN2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-PEY" "9" "8" "34" "2" "ACP-GET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SEN" " " "8" "0" "1" "01ACP-NYURYOKU" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-SEN" "9" "8" "45" "1" " " "ACP-SEN" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-SEN" BY REFERENCE SEN-W "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KURA1" " " "16" "0" "1" "ACP-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-KURA1" "9" "16" "33" "1" " " "ACP-KURA1"
            RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-KURA1" BY REFERENCE KURA-W1 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KURA2" " " "17" "0" "1" "ACP-KURA1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-KURA2" "9" "17" "33" "1" " " "ACP-KURA2"
            RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-KURA2" BY REFERENCE KURA-W2 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TKU1" " " "16" "0" "7" "ACP-KURA2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TKU11" "9" "16" "39" "4" " " "ACP-TKU1" RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-TKU11" BY REFERENCE TKU-W11 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TKU12" "9" "16" "44" "3" "ACP-TKU11" " "
            RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-TKU12" BY REFERENCE TKU-W12 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TKU2" " " "17" "0" "7" "ACP-TKU1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TKU21" "9" "17" "39" "4" " " "ACP-TKU2" RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-TKU21" BY REFERENCE TKU-W21 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TKU22" "9" "17" "44" "3" "ACP-TKU21" " "
            RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-TKU22" BY REFERENCE TKU-W22 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-DEN1" " " "16" "0" "6" "ACP-TKU2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-DEN1" "9" "16" "50" "6" " " "ACP-DEN1"
            RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-DEN1" BY REFERENCE DEN-W1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-DEN2" " " "17" "0" "6" "ACP-DEN1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-DEN2" "9" "17" "50" "6" " " "ACP-DEN2"
            RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-DEN2" BY REFERENCE DEN-W2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KAKU" " " "24" "0" "1" "ACP-DEN2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-KAKU" "9" "24" "63" "1" " " "ACP-KAKU"
            RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-KAKU" BY REFERENCE KAKU-W "1" "0" RETURNING RESU.
      *DSP-GAMEN
       CALL "SD_Init" USING 
            "DSP-GAMEN" " " "0" "0" "109" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-GAMEN" "X" "1" "1" "10" " " "DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-GAMEN" "RX" "1" "16" "40" "01DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-GAMEN" "X" "15" "31" "6" "02DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-GAMEN" "X" "15" "39" "6" "03DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-GAMEN" "X" "15" "50" "6" "04DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-GAMEN" "X" "16" "21" "8" "05DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-GAMEN" "X" "16" "43" "1" "06DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-GAMEN" "X" "17" "21" "4" "07DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-GAMEN" "X" "17" "43" "1" "08DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-GAMEN" " " "24" "0" "27" "09DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0110DSP-GAMEN" "X" "24" "41" "6" " " "10DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0210DSP-GAMEN" "X" "24" "47" "9" "0110DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0310DSP-GAMEN" "X" "24" "56" "2" "0210DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0410DSP-GAMEN" "X" "24" "58" "10" "0310DSP-GAMEN" " "
            RETURNING RESU.
      *DSP-GAMEN1
       CALL "SD_Init" USING 
            "DSP-GAMEN1" " " "0" "0" "30" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-GAMEN1" "X" "7" "25" "10" " " "DSP-GAMEN1"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-GAMEN1" " " "8" "0" "20" "01DSP-GAMEN1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-GAMEN1" "X" "8" "25" "18" " " "02DSP-GAMEN1"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DSP-GAMEN1" "X" "8" "44" "1" "0102DSP-GAMEN1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302DSP-GAMEN1" "X" "8" "46" "1" "0202DSP-GAMEN1" " "
            RETURNING RESU.
      *DSP-GAMEN2
       CALL "SD_Init" USING 
            "DSP-GAMEN2" " " "0" "0" "26" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-GAMEN2" "X" "8" "13" "8" " " "DSP-GAMEN2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-GAMEN2" "X" "8" "23" "18" "01DSP-GAMEN2" " "
            RETURNING RESU.
      *DSP-CLER
       CALL "SD_Init" USING
           "DSP-CLER" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-CLER" "X" "1" "0" "12" " " "DSP-CLER" RETURNING RESU.
      *DSP-CLER2
       CALL "SD_Init" USING 
            "DSP-CLER2" " " "0" "0" "48" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-CLER2" "X" "16" "33" "23" " " "DSP-CLER2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-CLER2" "X" "16" "43" "1" "01DSP-CLER2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-CLER2" "X" "17" "33" "23" "02DSP-CLER2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-CLER2" "X" "17" "43" "1" "03DSP-CLER2" " "
            RETURNING RESU.
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
           PERFORM  OPEN-RTN  THRU  OPEN-RTN-EXIT.
           PERFORM  PROC-RTN  THRU  PROC-RTN-EXIT
                    UNTIL  NOT  (END-SW  NOT  =  9).
           PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT.
       MAINLINE-END.
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
           ACCEPT   WYMD    FROM    DATE.
           MOVE  WYY  TO  M-YY.
           MOVE  WMM  TO  M-MM.
           MOVE  WDD  TO  M-DD.
           MOVE  ZERO TO  W-TOK.
           MOVE  0  TO  PCNT.
           MOVE  0  TO  GOKEI  W-TOKT.
       INIT-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ëIëèàóù  ÅiÇrÇkÇbÇsÅ|ÇqÇsÇmÅj                              *
      ******************************************************************
       SLCT-RTN.
       SLCT-010.
           ACCEPT   JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN   NOT =   0  AND  1
               MOVE  9  TO  END-SW
               GO  TO  SLCT-RTN-EXIT
           END-IF
           CALL "SD_Output" USING
            "DSP-CLER" DSP-CLER "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-GAMEN" DSP-GAMEN "p" RETURNING RESU.
           IF  JS-SIGN       =   1
               CALL "SD_Output" USING
                "DSP-GAMEN2" DSP-GAMEN2 "p" RETURNING RESU
               MOVE  2  TO  SEN-W
               GO  TO  SLCT-030
           END-IF
           CALL "SD_Output" USING
            "DSP-GAMEN1" DSP-GAMEN1 "p" RETURNING RESU.
       SLCT-020.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-SEN "01ACP-SEN"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "P9"
               MOVE  9  TO  END-SW
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS  NOT  =  "01"
               GO  TO  SLCT-020
           END-IF
           IF  SEN-W   =  1  OR  2
               GO  TO  SLCT-040
           ELSE
               GO  TO  SLCT-020
           END-IF.
       SLCT-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-NEN2 "ACP-NEN2"
            "9" "2" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "P9"
               MOVE  9  TO  END-SW
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS  NOT  =  "00"  AND  "01"  AND  "06"
               GO  TO  SLCT-030
           END-IF
           MOVE  20       TO  W-NEN1.
       SLCT-032.
           CALL "SD_Accept" USING BY REFERENCE ACP-GET "ACP-GET" "9" "2"
            BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-030
           END-IF
           IF  END-STS  NOT  =  "00"  AND  "01"  AND  "06"
               GO  TO  SLCT-032
           END-IF
           IF  W-GET         <   1  OR  >  12
               GO  TO  SLCT-032
           END-IF.
       SLCT-034.
           CALL "SD_Accept" USING BY REFERENCE ACP-PEY "ACP-PEY" "9" "2"
            BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-032
           END-IF
           IF  END-STS  NOT  =  "01"  AND  "06"
               GO  TO  SLCT-034
           END-IF
           IF  W-PEY         <   1  OR  >  31
               GO  TO  SLCT-034
           END-IF.
       SLCT-040.
           IF  SEN-W    =  1
               GO  TO  SLCT-100
           END-IF
           CALL "SD_Accept" USING BY REFERENCE 01ACP-KURA1 "01ACP-KURA1"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               CALL "SD_Output" USING
                "DSP-CLER2" DSP-CLER2 "p" RETURNING RESU
           END-IF
           IF  END-STS       =  "09"
               GO  TO  SLCT-020
           END-IF
           IF  END-STS  NOT  =  "01"  AND  "06"
               GO  TO  SLCT-040
           END-IF.
       SLCT-050.
           CALL "SD_Accept" USING BY REFERENCE ACP-TKU11 "ACP-TKU11"
            "9" "4" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-040
           END-IF.
       SLCT-055.
           CALL "SD_Accept" USING BY REFERENCE ACP-TKU12 "ACP-TKU12"
            "9" "3" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-050
           END-IF
           IF  END-STS  NOT  =  "01"  AND  "06"
               GO  TO  SLCT-055
           END-IF.
       SLCT-060.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-DEN1 "01ACP-DEN1"
            "9" "6" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-050
           END-IF
           IF  END-STS  NOT  =  "01"  AND  "06"
               GO  TO  SLCT-060
           END-IF.
       SLCT-070.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-KURA2 "01ACP-KURA2"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-060
           END-IF
           IF  END-STS  NOT  =  "01"  AND  "06"
               GO  TO  SLCT-070
           END-IF.
       SLCT-080.
           CALL "SD_Accept" USING BY REFERENCE ACP-TKU21 "ACP-TKU21"
            "9" "4" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-070
           END-IF.
       SLCT-085.
           CALL "SD_Accept" USING BY REFERENCE ACP-TKU22 "ACP-TKU22"
            "9" "3" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-080
           END-IF
           IF  END-STS  NOT  =  "01"  AND  "06"
               GO  TO  SLCT-085
           END-IF.
       SLCT-090.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-DEN2 "01ACP-DEN2"
            "9" "6" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-080
           END-IF
           IF  END-STS  NOT  =  "01"  AND  "06"
               GO  TO  SLCT-090
           END-IF
           IF  FROM-WK >  TO-WK
               GO  TO  SLCT-040
           END-IF.
       SLCT-100.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-KAKU "01ACP-KAKU"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               IF  SEN-W  =  1
                   CALL "SD_Output" USING
                    "DSP-CLER2" DSP-CLER2 "p" RETURNING RESU
                   GO  TO  SLCT-020
               ELSE
                   GO  TO  SLCT-090
               END-IF
           END-IF
           IF  END-STS  NOT  =  "01"  AND  "06"
               GO  TO  SLCT-100
           END-IF
           IF  KAKU-W  =  1
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  KAKU-W  =  9
               GO  TO  SLCT-010
           ELSE
               GO  TO  SLCT-100
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
      *           READ  JSJW        AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSJW_PNAME1 BY REFERENCE JSJW-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE   9  TO  END-SW
               GO  TO  READ1-RTN-EXIT
           END-IF
           IF  JSJW-021 NOT = 5000 AND  9850
               GO  TO  READ1-010
           END-IF
           IF  JSJW-158  NOT  =  0
               GO  TO  READ1-020
           END-IF
           IF  JSJW-1211(1)  =  ZERO  AND  JSJW-1211(2)  =  ZERO  AND
               JSJW-1211(3)  =  ZERO  AND  JSJW-1211(4)  =  ZERO  AND
               JSJW-1211(5)  =  ZERO  AND  JSJW-1211(6)  =  ZERO  AND
               JSJW-1211(7)  =  ZERO  AND  JSJW-1211(8)  =  ZERO  AND
               JSJW-1211(9)  =  ZERO  AND  JSJW-1211(10) =  ZERO
               GO  TO  READ1-010
           END-IF
           MOVE  1  TO  RED-SW.
           GO  TO  READ1-RTN-EXIT.
       READ1-020.
      *           READ  JSJW        AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSJW_PNAME1 BY REFERENCE JSJW-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  9  TO  END-SW
               GO  TO  READ1-RTN-EXIT
           END-IF
           IF  JSJW-021 NOT = 5000 AND  9850
               GO  TO  READ1-010
           END-IF
           IF  JSJW-158  NOT  =  0
               GO  TO  READ1-020
           END-IF
           IF  JSJW-1211(1)  =  ZERO  AND  JSJW-1211(2)  =  ZERO  AND
               JSJW-1211(3)  =  ZERO  AND  JSJW-1211(4)  =  ZERO  AND
               JSJW-1211(5)  =  ZERO  AND  JSJW-1211(6)  =  ZERO  AND
               JSJW-1211(7)  =  ZERO  AND  JSJW-1211(8)  =  ZERO  AND
               JSJW-1211(9)  =  ZERO  AND  JSJW-1211(10) =  ZERO
               GO  TO  READ1-020
           END-IF
           MOVE  1  TO  RED-SW.
           GO  TO  READ1-RTN-EXIT.
       READ1-030.
      *    (º≠Ø∂•ªºΩﬁ•ƒ◊› … ÿ∞ƒﬁ (ª≤ Ø∫≥))
       READ1-040.
      *           READ  JSJW        AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSJW_PNAME1 BY REFERENCE JSJW-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  9  TO  END-SW
               GO  TO  READ1-RTN-EXIT
           END-IF
           IF  JS-SIGN  =  1
               IF  JSJW-NGP    NOT =  W-NGP
                   GO  TO  READ1-040
               END-IF
           END-IF
           MOVE  JSJW-01  TO  KEY-W1.
           MOVE  JSJW-02  TO  KEY-W2.
           MOVE  JSJW-03  TO  KEY-W3.
           IF  FROM-WK  >  KEY-WK
               GO  TO  READ1-040
           END-IF
           IF  TO-WK    <  KEY-WK
               MOVE  9  TO  END-SW
               GO  TO  READ1-RTN-EXIT
           END-IF
           IF  JSJW-021 NOT = 5000 AND  9850
               GO  TO  READ1-040
           END-IF
           IF  JSJW-158  NOT  =  1
               GO  TO  READ1-040
           END-IF
           IF  JSJW-1211(1)  =  ZERO  AND  JSJW-1211(2)  =  ZERO  AND
               JSJW-1211(3)  =  ZERO  AND  JSJW-1211(4)  =  ZERO  AND
               JSJW-1211(5)  =  ZERO  AND  JSJW-1211(6)  =  ZERO  AND
               JSJW-1211(7)  =  ZERO  AND  JSJW-1211(8)  =  ZERO  AND
               JSJW-1211(9)  =  ZERO  AND  JSJW-1211(10) =  ZERO
               GO  TO  READ1-040
           END-IF
           MOVE  1  TO  RED-SW.
       READ1-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÇqÇdÇ`ÇcèàóùÅiÇQÅj  ÅiÇqÇdÇ`ÇcÇQÅ|ÇqÇsÇmÅj                  *
      ******************************************************************
       READ2-RTN.
       READ2-010.
      *    (¡Æ∏ø≥ª∑•œΩ¿ … ÿ∞ƒﬁ)
           MOVE  JSJW-021  TO  TC-TCD.
           MOVE  1         TO  TC-CCD.
      *           READ  TC-M  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  JSJW-021  TO  P1-021
               MOVE  SPACE     TO  P1-021N
               GO  TO  READ2-020
           END-IF
           MOVE  JSJW-021  TO  P1-021.
           MOVE  TC-NAME   TO  P1-021N.
       READ2-020.
           IF  JSJW-022  =  1
               MOVE  SPACE  TO  P1-022
               MOVE  SPACE  TO  P1-022N
               GO  TO  READ2-030
           ELSE
               MOVE  JSJW-021  TO  TC-TCD
               MOVE  JSJW-022  TO  TC-CCD
      *               READ  TC-M  UNLOCK  INVALID  KEY
      *///////////////
               CALL "DB_Read" USING
                "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
                RETURNING RET
               IF  RET = 1
                   MOVE  JSJW-022  TO  P1-022
                   MOVE  SPACE     TO  P1-022N
                   GO  TO  READ2-030
               END-IF
           END-IF
           MOVE  "-"       TO  P1-C.
           MOVE  JSJW-022  TO  P1-022.
           MOVE  TC-NAME   TO  P1-022N.
       READ2-030.
           MOVE  2        TO  JCON2-01.
           MOVE  JSJW-14  TO  JCON2-02.
      *           READ  JCON  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE  TO  P1-14N
               GO  TO  READ2-035
           END-IF
           MOVE  JCON2-03  TO  P1-14N.
       READ2-035.
           MOVE  3         TO  JCON3-01.
           MOVE  JSJW-01   TO  JCON3-02.
      *           READ  JCON      UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE  TO  P1-01N
               GO  TO  READ2-RTN-EXIT
           END-IF
           MOVE  JCON3-03  TO  P1-01N.
       READ2-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ñæç◊èoóÕèàóù  ÅiÇcÇdÇsÇkÅ|ÇqÇsÇmÅj                          *
      ******************************************************************
       DETL-RTN.
           IF  DEN-W  NOT  =  JSJW-03
               PERFORM  HEAD-RTN  THRU  HEAD-RTN-EXIT
           END-IF
           PERFORM  WRITE-RTN  THRU  WRITE-RTN-EXIT.
       DETL-010.
           IF  SEN-W  NOT  =  1
               GO  TO  DETL-020
           END-IF
           MOVE  JSJW-KEY    TO  JSJD-KEY.
      *           READ  JSJD         INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JSJD_PNAME1 BY REFERENCE JSJD-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  JSJW-KEY  TO  ERR-K
               MOVE  "WK0256"  TO  ERR-F
               MOVE  "I"       TO  ERR-M
               MOVE   0        TO  ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
           MOVE  1  TO  JSJD-158.
      *           REWRITE  JSJD-REC  INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            JSJD_PNAME1 JSJD_LNAME JSJD-REC RETURNING RET.
           IF  RET = 1
               MOVE  JSJD-KEY  TO  ERR-K
               MOVE  "JSJD"    TO  ERR-F
               MOVE  "R"       TO  ERR-M
               MOVE   0        TO  ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
               PERFORM  ERR-RTN   THRU  ERR-EX
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       DETL-020.
           MOVE  JSJW-03           TO  DEN-W.
           IF  W-TOK         =  ZERO
               MOVE  JSJW-021    TO  W-TOK
           END-IF
           IF  JSJW-09  <  999900
               ADD   WORK-92           TO  GOKEI
           END-IF.
       DETL-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    çáåvèoóÕèàóù  ÅiÇsÇnÇsÇkÅ|ÇqÇsÇmÅj                          *
      ******************************************************************
       TOTL-RTN.
           IF  END-SW  NOT =  9
               GO  TO  TOTL-010
           END-IF
           MOVE   "çáåv"        TO  P3-TOM.
           MOVE     ":"           TO  P3-J.
           MOVE   GOKEI           TO  P3-TO.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  64
               MOVE  SPACE       TO    SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               PERFORM  MID-RTN  THRU  MID-EX
           END-IF
           MOVE W-PR3 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD   GOKEI  TO  W-TOKT.
           GO  TO  TOTL-020.
       TOTL-010.
           IF  JSJW-03       =  DEN-W
               GO  TO  TOTL-RTN-EXIT
           END-IF
           MOVE   "çáåv"        TO  P3-TOM.
           MOVE     ":"           TO  P3-J.
           MOVE   GOKEI           TO  P3-TO.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  64
               MOVE  SPACE       TO    SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               PERFORM  MID-RTN  THRU  MID-EX
           END-IF
           MOVE W-PR3 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD   GOKEI  TO  W-TOKT.
           MOVE  0  TO  GOKEI.
           IF  JSJW-021      =  W-TOK
               GO  TO  TOTL-RTN-EXIT
           END-IF.
       TOTL-020.
           MOVE   SPACE           TO  W-PR8.
           MOVE  X"1A24212078"  TO  P8-15B.
           MOVE  X"1A24212474"  TO  P8-2B.
           MOVE  "Å@ÅyëççáåvÅz" TO  P8-TOM.
           MOVE     ":"           TO  P8-J.
           MOVE   W-TOKT          TO  P8-TO.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  64
               MOVE  SPACE       TO    SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               PERFORM  MID-RTN  THRU  MID-EX
           END-IF
           MOVE W-PR8 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE MID9 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  END-SW  NOT =  9
               MOVE  0  TO  W-TOKT
           END-IF
           MOVE  JSJW-021    TO  W-TOK.
       TOTL-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    å©èoÇµèoóÕèàóù  ÅiÇgÇdÇ`ÇcÅ|ÇqÇsÇmÅj                        *
      ******************************************************************
       HEAD-RTN.
           MOVE  X"1A24212078"  TO  P1-15B.
           MOVE  X"1A24212474"  TO  P3-2B.
           MOVE  JSJW-03         TO  P1-03.
           MOVE  JSJW-0712       TO  P1-0712.
           MOVE  JSJW-072        TO  P1-072.
           MOVE  JSJW-073        TO  P1-073.
           MOVE  "/"             TO  P1-A  P1-B.
           MOVE  JSJW-14B  TO  P1-14B.
           IF  PCNT            =  0
               PERFORM  MID-RTN  THRU  MID-EX
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  63
               MOVE  SPACE       TO    SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               PERFORM  MID-RTN  THRU  MID-EX
           END-IF
           MOVE W-PR1 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  JSJW-14B  =  OLD-OKNO
               MOVE  SPACE     TO  P3-15AM  P3-K  P3-15AC
           ELSE
               MOVE  "å¬êî"  TO  P3-15AM
               MOVE  ":"       TO  P3-K
               MOVE  JSJW-15A  TO  P3-15A
           END-IF
           IF  JSJW-15   =  SPACE
               MOVE  SPACE     TO  P3-15M  P3-I  P3-15
           ELSE
               MOVE  "ìEóv"  TO  P3-15M
               MOVE  ":"       TO  P3-I
               MOVE  JSJW-15   TO  P3-15
           END-IF
           IF  JSJW-14D  =  SPACE
               MOVE  SPACE     TO  P3-14DM P3-H  P3-14D
           ELSE
               MOVE  "îzíB"  TO  P3-14DM
               MOVE  ":"       TO  P3-H
               MOVE  JSJW-14D  TO  P3-14D
           END-IF
           MOVE  JSJW-14B  TO  OLD-OKNO.
       HEAD-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÇvÇqÇhÇsÇdèàóù  ÅiÇvÇqÇhÇsÇdÅ|ÇqÇsÇmÅj                      *
      ******************************************************************
       WRITE-RTN.
           MOVE  ZERO            TO  WORK-92.
       WRITE-010.
           MOVE  "-"            TO  P2-D.
           MOVE  JSJW-04        TO  P2-04.
           MOVE  JSJW-09        TO  P2-09.
           MOVE  JSJW-10        TO  P2-10.
           MOVE  JSJW-1211(1)   TO  P2-12101.
           MOVE  JSJW-1211(2)   TO  P2-12102.
           MOVE  JSJW-1211(3)   TO  P2-12103.
           MOVE  JSJW-1211(4)   TO  P2-12104.
           MOVE  JSJW-1211(5)   TO  P2-12105.
           MOVE  JSJW-1211(6)   TO  P2-12106.
           MOVE  JSJW-1211(7)   TO  P2-12107.
           MOVE  JSJW-1211(8)   TO  P2-12108.
           MOVE  JSJW-1211(9)   TO  P2-12109.
           MOVE  JSJW-1211(10)  TO  P2-12110.
           COMPUTE  WORK-92  =  JSJW-1211(1)  +  JSJW-1211(2)
                             +  JSJW-1211(3)  +  JSJW-1211(4)
                             +  JSJW-1211(5)  +  JSJW-1211(6)
                             +  JSJW-1211(7)  +  JSJW-1211(8)
                             +  JSJW-1211(9)  +  JSJW-1211(10).
           MOVE  JSJW-09  TO  HI-MHCD HI-HCD.
      *           READ  HI2-M  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE  TO  P2-09N
               GO  TO  WRITE-020
           END-IF
           MOVE  HI-NAME TO  P2-09N.
       WRITE-020.
           IF  JSJW-13  =  5 OR 4
               MOVE  WORK-92   TO  P2-122
               MOVE  ")"       TO  P2-122D
           ELSE
               MOVE  WORK-92   TO  P2-122
               MOVE  SPACE     TO  P2-122D
           END-IF
           IF  JSJW-20    NOT =  SPACE
               MOVE  JSJW-20       TO  P2-20
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  64
               MOVE  SPACE       TO    SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               PERFORM  MID-RTN  THRU  MID-EX
           END-IF
           MOVE W-PR2 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       WRITE-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÇnÇoÇdÇmèàóù  ÅiÇnÇoÇdÇmÅ|ÇqÇsÇmÅj                        *
      ******************************************************************
       OPEN-RTN.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JSJW_PNAME1.
           IF  SEN-W     =  1
               CALL "DB_F_Open" USING
                "I-O" JSJD_PNAME1 "SHARED" BY REFERENCE JSJD_IDLST "2"
                "JSJD-KEY" BY REFERENCE JSJD-KEY "JSJD-KEY2"
                BY REFERENCE JSJD-KEY2
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON2-KEY" BY REFERENCE JCON2-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JSJW_PNAME1 " " BY REFERENCE JSJW_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
           CALL "PR_Open" RETURNING RESP.
       OPEN-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÇbÇkÇnÇrÇdèàóù  ÅiÇbÇkÇrÇdÅ|ÇqÇsÇmÅj                        *
      ******************************************************************
       CLSE-RTN.
           IF  SEN-W     =  1
               CALL "DB_F_Close" USING
                BY REFERENCE JSJD_IDLST JSJD_PNAME1
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE JSJW_IDLST JSJW_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       CLSE-RTN-EXIT.
           EXIT.
       MID-RTN.
           ADD   1   TO    PCNT.
           MOVE  PCNT  TO  WPCNT.
           MOVE   MID1    TO    SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE    TO     SP-R.
           MOVE   MID2    TO    SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE   TO     SP-R.
           MOVE   MID3    TO    SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE    TO     SP-R.
           MOVE   MID4    TO    SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE    TO     SP-R.
           MOVE   MID5    TO    SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE    TO     SP-R.
           MOVE   MID6    TO    SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE    TO     SP-R.
      *
       MID-EX.
           EXIT.
           COPY  LPERR.
           COPY  LPACPT.

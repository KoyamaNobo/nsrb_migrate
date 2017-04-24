       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       JTN30L.
      ******************************************************************
      *    ì˙êi  èoâ◊éwê}èëÅ@Åiï‘ïiì`ï[Åj                              *
      *    ÇmÇnÅDÇPÇXÇS  î~ä_        ÇgÇOÇPÅDÇOÇWÅDÇPÇU                *
      *    JS-W     : 0=ñ{é– , 1=ëIë , 2=ã ìá , 3=ëÅìá , 9=ï‘ïi       *
      *****JS-SIGN3 : 0=éwê} , 1=ï‘ïi                                  *
      ******************************************************************

       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       DATA              DIVISION.
       WORKING-STORAGE   SECTION.
       01  END-SW                PIC 9      VALUE 0.
       01  RED-SW                PIC 9      VALUE 0.
       01  WORK-1.
           02  F                 PIC X(93).
           02  ST-CD0            PIC X(5).
           02  WORK-10           PIC N(2).
           02  ED-CD0            PIC X(5).
           02  F                 PIC X(7).
           02  WORK-11           PIC 9(6).
       01  WORK-1A.
           02  F                 PIC X(85)  VALUE  SPACE.
           02  F                 PIC X(5)   VALUE  X"1A24212078".
           02  F                 PIC X(8)   VALUE  X"1A26212068222176".
           02  WORK-NA           PIC N(8)   VALUE  SPACE.
           02  F                 PIC X(8)   VALUE  X"1A26212068212078".
           02  F                 PIC X(5)   VALUE  X"1A24212474".
       01  WORK-1B.
           02  F                 PIC X(87)  VALUE  SPACE.
           02  F                 PIC N(1)   VALUE  "Åß".
           02  WORK-UNO          PIC X(8)   VALUE  SPACE.
       01  WORK-1C.
           02  F                 PIC X(87)  VALUE  SPACE.
           02  F                 PIC X(5)   VALUE  X"1A24212078".
           02  WORK-JS           PIC N(14)  VALUE  SPACE.
           02  F                 PIC X(5)   VALUE  X"1A24212474".
       01  WORK-2.
           02  F                 PIC X(38).
           02  WORK-M2           PIC X(01).
           02  WORK-21.
               03  WORK-211      PIC 9(2).
               03  F             PIC X(4).
               03  WORK-212      PIC ZZ.
               03  F             PIC X(4).
               03  WORK-213      PIC ZZ.
           02  F                 PIC X(09).
           02  WORK-22           PIC N(2).
           02  F                 PIC X(4).
           02  WORK-23           PIC N(06).
           02  F                 PIC X(5).
           02  WORK-TM           PIC X(3).
           02  F                 PIC X(1).
           02  WORK-TEL          PIC X(14).
       01  WORK-3.
           02  F                 PIC X(61).
           02  WORK-31           PIC 9(6).
           02  F                 PIC X(20).
           02  WORK-FM           PIC X(3).
           02  F                 PIC X(1).
           02  WORK-FAX          PIC X(14).
      *----  äOïîëqå…óp  -----------------------------------------------
       01  WORK-1AG.
           02  F                 PIC X(81)  VALUE  SPACE.
           02  F                 PIC X(5)   VALUE  X"1A24212078".
           02  WORK-NAG          PIC N(18)  VALUE  SPACE.
           02  F                 PIC X(5)   VALUE  X"1A24212474".
       01  WORK-1BG.
           02  F                 PIC X(79)  VALUE  SPACE.
           02  F                 PIC N(2)   VALUE  "Å@Åß".
           02  WORK-UNOG         PIC X(8)   VALUE  SPACE.
       01  WORK-1CG.
           02  F                 PIC X(81)  VALUE  SPACE.
           02  F                 PIC X(5)   VALUE  X"1A24212078".
           02  WORK-JSG          PIC N(20)  VALUE  SPACE.
           02  F                 PIC X(5)   VALUE  X"1A24212474".
       01  WORK-2G.
           02  F                 PIC X(38).
           02  WORK-M2G          PIC X(01).
           02  WORK-21G.
               03  WORK-211G     PIC 9(2).
               03  F             PIC X(4).
               03  WORK-212G     PIC ZZ.
               03  F             PIC X(4).
               03  WORK-213G     PIC ZZ.
           02  F                 PIC X(09).
           02  WORK-22G          PIC N(2).
           02  F                 PIC X(4).
           02  WORK-K1G          PIC X(05).
           02  WORK-23G          PIC N(06).
           02  F                 PIC X(05).
           02  WORK-JSSG         PIC N(16).
           02  WORK-K2G          PIC X(05).
       01  WORK-3G.
           02  F                 PIC X(61).
           02  WORK-31G          PIC 9(6).
           02  F                 PIC X(14).
           02  WORK-TMG          PIC X(3).
           02  F                 PIC X(1).
           02  WORK-TELG         PIC X(14).
      *-----------------------------------------------------------------
       01  WORK-3A.
           02  F                 PIC X(5).
           02  WORK-33A.
             03  WORK-33F        PIC X(1).
             03  WORK-331        PIC 9(1).
             03  WORK-332        PIC X(1).
             03  WORK-333        PIC 9(1).
             03  WORK-33R        PIC X(1).
           02  F                 PIC X(94).
           02  WORK-32           PIC 9(6).
       01  WORK-4.
           02  F                 PIC X(06).
           02  WORK-41           PIC 9(4).
           02  F                 PIC X(4).
           02  ST-CD1            PIC X(5).
           02  WORK-42           PIC N(26).
           02  F                 PIC X(5).
           02  WORK-43           PIC X(3).
           02  F                 PIC X(4).
           02  WORK-44           PIC N(26).
           02  ED-CD1            PIC X(5).
       01  WORK-5.
           02  F                 PIC X(06).
           02  ED-CD2            PIC X(5).
           02  F                 PIC X(18).
           02  WORK-NGP          PIC 99/99/99.
           02  WORK-NGPD  REDEFINES  WORK-NGP.
               03  F             PIC X(08).
           02  F                 PIC X(05).
           02  WORK-51F          PIC X(01).
           02  WORK-51           PIC X(10).
           02  WORK-51R          PIC X(01).
           02  F                 PIC X(02).
           02  WORK-52           OCCURS  10.
               03  WORK-521      PIC ----B.
           02  WORK-53           PIC ----.
           02  WORK-54           PIC X(01).
           02  WORK-55           PIC ZZZ.
       01  WORK-6.
           02  F                 PIC X(06).
           02  WORK-61           PIC 9(6).
           02  F                 PIC X.
           02  ST-CD3            PIC X(5).
           02  WORK-62           PIC N(24).
           02  ED-CD3            PIC X(5).
           02  F                 PIC X.
           02  WORK-63           PIC 9.
           02  F                 PIC X.
           02  WORK-64           OCCURS  10.
               03  WORK-641      PIC -----.
           02  WORK-65           PIC -------.
           02  WORK-65R  REDEFINES  WORK-65.
               03  WORK-651      PIC X(01).
               03  WORK-652      PIC ------.
           02  WORK-66           PIC X(01).
           02  WORK-67           PIC X.
       01  WORK-6A.
           02  F                 PIC X(101).
           02  WORK-61A          PIC ----.
           02  F                 PIC X(01).
           02  WORK-62A          PIC X(03).
       01  WORK-7.
           02  F                 PIC X(23).
           02  WORK-70           PIC ZZZ.
           02  WORK-70R  REDEFINES   WORK-70  PIC X(03).
           02  F                 PIC X(09).
           02  ST-CD4            PIC X(5).
           02  WORK-71           PIC N(6).
           02  F                 PIC X(4).
           02  WORK-72.
               03  WORK-721      PIC N(23).
               03  WORK-722      PIC N(9).
           02  ED-CD4            PIC X(5).
           02  F                 PIC X(6).
           02  WORK-73           PIC -------.
       01  WORK-8.
           02  WORK-81           OCCURS  10.
               03  WORK-811      PIC S9(2).
           02  WORK-82           PIC S9(6).
       01  WORK-9.
           02  WORK-91           OCCURS  10.
               03  WORK-911      PIC S9(4).
           02  WORK-92           PIC S9(6).
       01  I                     PIC 9.
       01  J                     PIC 9(2).
       01  PAGE-C                PIC 9(3).
       01  GOKEI                 PIC S9(6).
       01  SEN-W                 PIC 9.
       01  TEST-W                PIC 9.
       01  DEN-W                 PIC 9(6).
       01  DEN-W1                PIC 9(6).
       01  DEN-W2                PIC 9(6).
       01  KAKU-W                PIC 9.
       01  ERR-STAT              PIC X(2).
       01  W-JS                  PIC 9(1).
       01  JS-SIGN               PIC 9(1).
       01  JS-W                  PIC 9(1).
       01  OLD-OKNO              PIC 9(6)  VALUE  0.
       01  SETSU                 PIC S9(3).
       01  W-DEN                 PIC N(9).
       01  W-SOUKO               PIC N(4).
       01  W-TCD                 PIC 9(4).
       01  W-TAN                 PIC 9(5).
       01  W-DATA.
           02  W-DATE            PIC 9(06).
           02  W-NGP           REDEFINES  W-DATE.
             03  W-NEN           PIC 9(02).
             03  W-GET           PIC 9(02).
             03  W-PEY           PIC 9(02).
           02  W-SDATE           PIC 9(08).
           02  W-SNGP.
             03  W-SNEN          PIC 9(04).
             03  W-SNENL       REDEFINES  W-SNEN.
               04  W-SNEN1       PIC 9(02).
               04  W-SNEN2       PIC 9(02).
             03  W-SGET          PIC 9(02).
             03  W-SPEY          PIC 9(02).
           02  W-EDATE           PIC 9(08).
           02  W-ENGP.
             03  W-ENEN          PIC 9(04).
             03  W-ENENL       REDEFINES  W-ENEN.
               04  W-ENEN1       PIC 9(02).
               04  W-ENEN2       PIC 9(02).
             03  W-EGET          PIC 9(02).
             03  W-EPEY          PIC 9(02).
       01  W-HND.
           02  W-NAME            PIC N(24).
           02  W-NAMED REDEFINES W-NAME.
             03  W-NAD   OCCURS  24.
               04  W-NA          PIC N(01).
           COPY  LWMSG.
      *
           COPY  L-JSTR.
           COPY  L-JCON.
           COPY  LITCM.
           COPY  LIHIM2.
           COPY  LITM.
           COPY  LITHTM.
           COPY  LJMSTD.
      *FD  PRN-F
       77  PRN-R                 PIC X(180).
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
           02  ACP-SIGN.
               03  01ACP-SIGN  PIC 9.
           02  ACP-JS.
               03  01ACP-JS    PIC 9.
           02  ACP-SEN.
               03  01ACP-SEN   PIC 9.
           02  ACP-TEST.
               03  01ACP-TEST  PIC 9.
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
               03  01ACP-KAKU  PIC 9.
       01  DSP-GAMEN.
           02  FILLER  PIC N(09).
           02  DSP-SOUKO  PIC N(4).
           02  FILLER.
             03  FILLER  PIC 9(02).
             03  FILLER  PIC N(01)    VALUE  "îN".
             03  FILLER  PIC 9(02).
             03  FILLER  PIC N(01)    VALUE  "åé".
             03  FILLER  PIC 9(02).
             03  FILLER  PIC N(01)    VALUE  "ì˙".
           02  FILLER.
             03  FILLER  PIC X(36)    VALUE
                  "ã≥Å@àÁ=0 , àÍÅ@î =1 , Ç`ÇkÇk=9 ...  ".
           02  FILLER  PIC X(10)    VALUE  "ÇPÅ@î≠Å@çs".
           02  FILLER.
               03  FILLER  PIC X(18)    VALUE "ÇQÅ@çƒî≠çsÅ@Å@ëIë".
               03  FILLER  PIC X(01)    VALUE   "[".
               03  FILLER  PIC X(01)    VALUE   "]".
           02  FILLER  PIC X(18)    VALUE  "ÉeÉXÉgÉvÉäÉìÉgàÛéö".
           02  FILLER.
               03  FILLER  PIC X(14)    VALUE   "(YES=1,NO=2) [".
               03  FILLER  PIC X(01)    VALUE   "]".
           02  FILLER  PIC X(08)    VALUE  "ÇeÇqÇnÇl".
           02  FILLER  PIC X(04)    VALUE  "ÇsÇn".
           02  FILLER.
               03  FILLER  PIC X(06)    VALUE  "ämîFÅi".
               03  FILLER  PIC X(09)    VALUE    "OK=1,NO=9".
               03  FILLER  PIC X(02)    VALUE  "Åj".
               03  FILLER  PIC X(10)    VALUE    "--->  ÿ¿∞›".
       01  DSP-GAMEN1.
           02  DSP-SOK.
             03  FILLER  PIC X(39)    VALUE
                  "ñ{é–=0 , ã ìá=1 , í√éR=2 , ëÅìá=3 ...  ".
           02  DSP-DNOM  PIC X(06)    VALUE  "ì`ï[áÇ".
           02  DSP-DATM.
             03  FILLER  PIC X(06)    VALUE  "ì˙Å@ït".
             03  FILLER  PIC X(08)    VALUE  "  /  /  ".
             03  FILLER  PIC X(08)    VALUE  "  /  /  ".
       01  DSP-CLER.
           02  FILLER  PIC X(12)   VALUE "CLEAR SCREEN".
       01  DSP-CLER2.
           02  FILLER  PIC X(08)   VALUE "        ".
           02  FILLER  PIC X(08)   VALUE "        ".
       01  DSP-MI.
           02  FILLER  PIC X(26)   VALUE
               "ÅñÅñèoâ◊éwê}ÉgÉâÉìÅ@ñ¢ìoò^".
           COPY  LSERR.
      *
       PROCEDURE         DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "PRF-999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *ACP-NYURYOKU
       CALL "SD_Init" USING 
            "ACP-NYURYOKU" " " "0" "0" "29" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SIGN" " " "4" "0" "1" " " "ACP-NYURYOKU"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-SIGN" "9" "4" "59" "1" " " "ACP-SIGN" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-SIGN" BY REFERENCE JS-SIGN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-JS" " " "7" "0" "1" "ACP-SIGN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-JS" "9" "7" "56" "1" " " "ACP-JS" RETURNING RESU.
       CALL "SD_Using" USING 
            "01ACP-JS" BY REFERENCE W-JS "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SEN" " " "11" "0" "1" "ACP-JS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-SEN" "9" "11" "45" "1" " " "ACP-SEN" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-SEN" BY REFERENCE SEN-W "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TEST" " " "15" "0" "1" "ACP-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-TEST" "9" "15" "45" "1" " " "ACP-TEST" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-TEST" BY REFERENCE TEST-W "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SNGP" " " "19" "0" "6" "ACP-TEST" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SNEN" "9" "19" "31" "2" " " "ACP-SNGP" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SNEN" BY REFERENCE W-SNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SGET" "9" "19" "34" "2" "ACP-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SPEY" "9" "19" "37" "2" "ACP-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ENGP" " " "20" "0" "6" "ACP-SNGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ENEN" "9" "20" "31" "2" " " "ACP-ENGP" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-ENEN" BY REFERENCE W-ENEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-EGET" "9" "20" "34" "2" "ACP-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-EPEY" "9" "20" "37" "2" "ACP-EGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-DEN1" " " "19" "0" "6" "ACP-ENGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-DEN1" "9" "19" "31" "6" " " "ACP-DEN1" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-DEN1" BY REFERENCE DEN-W1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-DEN2" " " "20" "0" "6" "ACP-DEN1" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-DEN2" "9" "20" "31" "6" " " "ACP-DEN2" RETURNING RESU.
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
            "DSP-GAMEN" " " "0" "0" "176" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-GAMEN" "RN" "1" "26" "18" " " "DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-GAMEN" BY REFERENCE W-DEN "18" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SOUKO" "N" "1" "55" "8" "01DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-SOUKO" BY REFERENCE W-SOUKO "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-GAMEN" " " "2" "0" "12" "DSP-SOUKO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0103DSP-GAMEN" "9" "2" "63" "2" " " "03DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0103DSP-GAMEN" BY REFERENCE W-NEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203DSP-GAMEN" "N" "2" "65" "2" "0103DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0303DSP-GAMEN" "9" "2" "67" "2" "0203DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0303DSP-GAMEN" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0403DSP-GAMEN" "N" "2" "69" "2" "0303DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0503DSP-GAMEN" "9" "2" "71" "2" "0403DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0503DSP-GAMEN" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0603DSP-GAMEN" "N" "2" "73" "2" "0503DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-GAMEN" " " "7" "0" "36" "03DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0104DSP-GAMEN" "X" "7" "21" "36" " " "04DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-GAMEN" "X" "10" "25" "10" "04DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-GAMEN" " " "11" "0" "20" "05DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0106DSP-GAMEN" "X" "11" "25" "18" " " "06DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0206DSP-GAMEN" "X" "11" "44" "1" "0106DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0306DSP-GAMEN" "X" "11" "46" "1" "0206DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-GAMEN" "X" "14" "21" "18" "06DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-GAMEN" " " "15" "0" "15" "07DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0108DSP-GAMEN" "X" "15" "31" "14" " " "08DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0208DSP-GAMEN" "X" "15" "46" "1" "0108DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-GAMEN" "X" "19" "21" "8" "08DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-GAMEN" "X" "20" "21" "4" "09DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "11DSP-GAMEN" " " "24" "0" "27" "10DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0111DSP-GAMEN" "X" "24" "41" "6" " " "11DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0211DSP-GAMEN" "X" "24" "47" "9" "0111DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0311DSP-GAMEN" "X" "24" "56" "2" "0211DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0411DSP-GAMEN" "X" "24" "58" "10" "0311DSP-GAMEN" " "
            RETURNING RESU.
      *DSP-GAMEN1
       CALL "SD_Init" USING 
            "DSP-GAMEN1" " " "0" "0" "67" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SOK" " " "4" "0" "39" " " "DSP-GAMEN1"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-SOK" "X" "4" "21" "39" " " "DSP-SOK"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DNOM" "X" "18" "31" "6" "DSP-SOK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DATM" " " "0" "0" "22" "DSP-DNOM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-DATM" "X" "18" "31" "6" " " "DSP-DATM"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-DATM" "X" "19" "31" "8" "01DSP-DATM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-DATM" "X" "20" "31" "8" "02DSP-DATM" " "
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
            "01DSP-CLER2" "X" "19" "31" "8" " " "DSP-CLER2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-CLER2" "X" "20" "31" "8" "01DSP-CLER2" " "
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
           PERFORM  SLCT-RTN  THRU  SLCT-RTN-EXIT.
           IF  END-STS  =  "P9"
               CALL "DB_Close"
               STOP  RUN
           END-IF
           PERFORM  OPEN-RTN  THRU  OPEN-RTN-EXIT.
           PERFORM  PROC-RTN  THRU  PROC-RTN-EXIT
                    UNTIL  NOT  (END-SW  NOT  =  9).
           IF  PAGE-C  NOT =  1
               MOVE     SPACE     TO    PRN-R
               CALL "PR_Write" USING PRN-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
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
           ACCEPT   JS-W FROM ARGUMENT-VALUE.
           IF  JS-W          =  9
               MOVE  "Å@ï‘Å@ïiÅ@ì`Å@ï[Å@"  TO  W-DEN
               MOVE  9             TO  JS-SIGN
           ELSE
               MOVE  "èoÅ@â◊Å@éwÅ@ê}Å@èë"  TO  W-DEN
           END-IF
           MOVE  "Å@Å@Å@Å@"  TO  W-SOUKO.
           IF  JS-W          =  0
               MOVE  0             TO  JS-SIGN
           END-IF
           IF  JS-W          =  2
               MOVE  1             TO  JS-SIGN
           END-IF
           IF  JS-W          =  3
               MOVE  3             TO  JS-SIGN
           END-IF
           MOVE  1      TO  PAGE-C.
           MOVE  0      TO  GOKEI.
           MOVE  0      TO  SETSU.
           MOVE  SPACE  TO  WORK-1  WORK-2  WORK-3  WORK-3A
                            WORK-4  WORK-5  WORK-6  WORK-6A
                            WORK-7  WORK-8  WORK-9.
           MOVE  ZERO   TO  W-DATA.
           ACCEPT  W-DATE  FROM  DATE.
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
           IF  JS-W          =  9
               GO  TO  SLCT-015
           END-IF
           IF  JS-W      NOT =  1
               GO  TO  SLCT-013
           END-IF
           CALL "SD_Output" USING "DSP-SOK" DSP-SOK "p" RETURNING RESU.
       SLCT-012.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-SIGN "01ACP-SIGN"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  END-STS       =  "P9"
               MOVE  9  TO  END-SW
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS  NOT  =  "01"
               GO  TO  SLCT-012
           END-IF
           IF  JS-SIGN  NOT  = 0  AND  1  AND  2  AND  3
               GO  TO  SLCT-012
           END-IF.
       SLCT-013.
           IF  JS-SIGN       =  0
               MOVE  "ñ{Å@Å@é–"  TO  W-SOUKO
           END-IF
           IF  JS-SIGN       =  1
               MOVE  "ã Å@Å@ìá"  TO  W-SOUKO
           END-IF
           IF  JS-SIGN       =  2
               MOVE  "í√Å@Å@éR"  TO  W-SOUKO
           END-IF
           IF  JS-SIGN       =  3
               MOVE  "ëÅÅ@Å@ìá"  TO  W-SOUKO
           END-IF
           CALL "SD_Output" USING
            "DSP-SOUKO" DSP-SOUKO "p" RETURNING RESU.
           IF  JS-SIGN   NOT =  0
               MOVE  9        TO  W-JS
               CALL "SD_Output" USING "ACP-JS" ACP-JS "p" RETURNING RESU
               GO  TO  SLCT-020
           END-IF.
       SLCT-015.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-JS "01ACP-JS"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  END-STS       =  "09"
               IF  JS-W          =  1
                   GO  TO  SLCT-012
               END-IF
           END-IF
           IF  END-STS       =  "P9"
               MOVE  9  TO  END-SW
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS  NOT  =  "01"
               GO  TO  SLCT-015
           END-IF
           IF  W-JS  NOT = 0  AND  1  AND  9
               GO  TO  SLCT-015
           END-IF.
       SLCT-020.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-SEN "01ACP-SEN"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  END-STS       =  "P9"
               MOVE  9  TO  END-SW
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS       =  "09"
               IF (JS-W          =  0  OR  9)  OR
                 ((JS-W          =  1)  AND  (JS-SIGN       =  0))
                   GO  TO  SLCT-015
               ELSE
                   IF (JS-W          =  1)
                       GO  TO  SLCT-012
                   END-IF
               END-IF
           END-IF
           IF  END-STS  NOT  =  "01"
               GO  TO  SLCT-020
           END-IF
           IF  SEN-W NOT = 1  AND  2
               GO  TO  SLCT-020
           END-IF
           CALL "SD_Output" USING
            "DSP-CLER2" DSP-CLER2 "p" RETURNING RESU.
           IF  SEN-W = 2
               CALL "SD_Output" USING
                "DSP-DNOM" DSP-DNOM "p" RETURNING RESU
               GO  TO  SLCT-030
           END-IF
           PERFORM  SEL-RTN  THRU  SEL-EX.
           IF  W-SNGP = ZERO
               MOVE  24  TO  ERR-LIN
               CALL "SD_Arg_Match_Line" USING
                "ERR-LIN" "2" ERR-LIN RETURNING RESU
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
       SLCT-030.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-TEST "01ACP-TEST"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-020
           END-IF
           IF  END-STS  NOT  =  "01"
               GO  TO  SLCT-030
           END-IF
           IF  TEST-W  =  1
               PERFORM  TEST-RTN  THRU  TEST-RTN-EXIT
               GO  TO  SLCT-030
           END-IF
           IF  TEST-W  =  2
               GO  TO  SLCT-040
           ELSE
               GO  TO  SLCT-030
           END-IF.
       SLCT-040.
           IF  SEN-W    =  1
               GO  TO  SLCT-060
           END-IF
           CALL "SD_Accept" USING BY REFERENCE 01ACP-DEN1 "01ACP-DEN1"
            "9" "6" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               CALL "SD_Output" USING
                "DSP-CLER2" DSP-CLER2 "p" RETURNING RESU
               GO  TO  SLCT-030
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
           IF  END-STS = "09"
               GO  TO  SLCT-030
           END-IF
           IF  END-STS NOT = "00"  AND  "01"  AND  "06"
               GO  TO  SLCT-052
           END-IF
           MOVE  20    TO  W-SNEN1.
       SLCT-053.
           CALL "SD_Accept" USING BY REFERENCE ACP-SGET "ACP-SGET"
            "9" "2" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS = "09"
               GO  TO  SLCT-052
           END-IF
           IF  END-STS NOT = "00"  AND  "01"  AND  "06"
               GO  TO  SLCT-053
           END-IF
           IF  W-SGET < 1  OR  > 12
               GO  TO  SLCT-053
           END-IF.
       SLCT-054.
           CALL "SD_Accept" USING BY REFERENCE ACP-SPEY "ACP-SPEY"
            "9" "2" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS = "09"
               GO  TO  SLCT-053
           END-IF
           IF  END-STS NOT = "00"  AND  "01"  AND  "06"
               GO  TO  SLCT-054
           END-IF
           IF  W-SPEY < 1  OR  > 31
               GO  TO  SLCT-054
           END-IF
           IF  W-SNGP < W-SDATE  OR  > W-EDATE
               GO  TO  SLCT-052
           END-IF.
       SLCT-056.
           CALL "SD_Accept" USING BY REFERENCE ACP-ENEN "ACP-ENEN"
            "9" "2" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS = "09"
               GO  TO  SLCT-054
           END-IF
           IF  END-STS NOT = "00"  AND  "01"  AND  "06"
               GO  TO  SLCT-056
           END-IF
           MOVE  20    TO  W-ENEN1.
       SLCT-057.
           CALL "SD_Accept" USING BY REFERENCE ACP-EGET "ACP-EGET"
            "9" "2" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS = "09"
               GO  TO  SLCT-056
           END-IF
           IF  END-STS NOT = "00"  AND  "01"  AND  "06"
               GO  TO  SLCT-057
           END-IF
           IF  W-EGET < 1  OR  > 12
               GO  TO  SLCT-057
           END-IF.
       SLCT-058.
           CALL "SD_Accept" USING BY REFERENCE ACP-EPEY "ACP-EPEY"
            "9" "2" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS = "09"
               GO  TO  SLCT-057
           END-IF
           IF  END-STS NOT = "00"  AND  "01"  AND  "06"
               GO  TO  SLCT-058
           END-IF
           IF  W-EPEY < 1  OR  > 31
               GO  TO  SLCT-058
           END-IF
           IF  W-SNGP > W-ENGP
               GO  TO  SLCT-056
           END-IF
           IF  W-ENGP < W-SDATE  OR  > W-EDATE
               GO  TO  SLCT-056
           END-IF.
       SLCT-060.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-KAKU "01ACP-KAKU"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               IF  SEN-W = 1
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
               CALL "SD_Output" USING "DSP-MI" DSP-MI "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  W-JS      NOT =  9
               IF  JSTR-16   NOT =  W-JS
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JSTR-05       =  ZERO
               GO  TO  READ1-020
           END-IF
           IF  JSTR-17   NOT =  9
               GO  TO  READ1-020
           END-IF
           IF  JS-SIGN  =  0
               IF  JSTR-07  NOT =  1  AND  8
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JS-SIGN  =  1
               IF  JSTR-07  NOT =  6
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JS-SIGN  =  2
               IF  JSTR-07  NOT =  7
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JS-SIGN  =  3
               IF  JSTR-07  NOT =  4
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JS-SIGN       =  9
               IF  JSTR-03     NOT =  5  AND 6
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JS-SIGN   NOT =  9
               IF  JSTR-03     NOT =  0
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JS-SIGN       =  1
               IF  JSTR-061        =  9850   OR  5000
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JSTR-158  NOT  =  0
               GO  TO  READ1-020
           END-IF
           IF  JSTR-04 < W-SNGP  OR  > W-ENGP
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
           IF  W-JS      NOT =  9
               IF  JSTR-16   NOT =  W-JS
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JSTR-05       =  ZERO
               GO  TO  READ1-020
           END-IF
           IF  JSTR-17   NOT =  9
               GO  TO  READ1-020
           END-IF
           IF  JS-SIGN  =  0
               IF  JSTR-07  NOT =  1  AND  8
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JS-SIGN  =  1
               IF  JSTR-07  NOT =  6
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JS-SIGN  =  2
               IF  JSTR-07  NOT =  7
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JS-SIGN  =  3
               IF  JSTR-07  NOT =  4
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JS-SIGN       =  9
               IF  JSTR-03     NOT =  5  AND 6
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JS-SIGN   NOT =  9
               IF  JSTR-03     NOT =  0
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JS-SIGN       =  1
               IF  JSTR-061        =  9850   OR  5000
                   GO  TO  READ1-020
               END-IF
           END-IF
           IF  JSTR-158  NOT  =  0
               GO  TO  READ1-020
           END-IF
           IF  JSTR-04 < W-SNGP  OR  > W-ENGP
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
               CALL "SD_Output" USING "DSP-MI" DSP-MI "p" RETURNING RESU
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
           IF  DEN-W2  <  JSTR-01
               MOVE  9  TO  END-SW
               GO  TO  READ1-RTN-EXIT
           END-IF
           IF  W-JS      NOT =  9
               IF  JSTR-16   NOT =  W-JS
                   GO  TO  READ1-040
               END-IF
           END-IF
           IF  JSTR-17   NOT =  9 AND 8
               GO  TO  READ1-040
           END-IF
           IF  JS-SIGN  =  0
               IF  JSTR-07  NOT =  1  AND  8
                   GO  TO  READ1-040
               END-IF
           END-IF
           IF  JS-SIGN  =  1
               IF  JSTR-07  NOT =  6
                   GO  TO  READ1-040
               END-IF
           END-IF
           IF  JS-SIGN  =  2
               IF  JSTR-07  NOT =  7
                   GO  TO  READ1-040
               END-IF
           END-IF
           IF  JS-SIGN  =  3
               IF  JSTR-07  NOT =  4
                   GO  TO  READ1-040
               END-IF
           END-IF
           IF  JS-SIGN       =  9
               IF  JSTR-03     NOT =  5  AND 6
                   GO  TO  READ1-040
               END-IF
           END-IF
           IF  JS-SIGN   NOT =  9
               IF  JSTR-03     NOT =  0
                   GO  TO  READ1-040
               END-IF
           END-IF
           IF  JSTR-158       =  0
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
           MOVE  JSTR-061  TO  T-KEY.
      *           READ  T-M  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  0         TO  T-TNC1  T-DCC
           END-IF
      *    (¡Æ∏ø≥ª∑•œΩ¿ … ÿ∞ƒﬁ)
           MOVE  JSTR-061  TO  TC-TCD.
           MOVE  1         TO  TC-CCD.
      *           READ  TC-M  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  JSTR-061  TO  WORK-41
               MOVE  SPACE     TO  WORK-42
               GO  TO  READ2-020
           END-IF
           MOVE  JSTR-061  TO  WORK-41.
           MOVE  TC-NAME   TO  WORK-42.
       READ2-020.
           IF  JSTR-062  =  1
               MOVE  SPACE  TO  WORK-43
               MOVE  SPACE  TO  WORK-44
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
                   MOVE  JSTR-062  TO  WORK-43
                   MOVE  SPACE     TO  WORK-44
                   GO  TO  READ2-025
               END-IF
           END-IF
           MOVE  JSTR-062  TO  WORK-43.
           MOVE  TC-NAME   TO  WORK-44.
       READ2-025.
           IF  JSTR-061     =  2410
               MOVE  SPACE     TO  WORK-42
               MOVE  "ì˙ñ{ã≥àÁÉVÉÖÅ[ÉYÅ@ïüâ™åßéñã∆ñ{ïî" TO   WORK-42
           END-IF.
       READ2-030.
      *    (º≠Ø∂À›“≤•œΩ¿ … ÿ∞ƒﬁ)
           MOVE  JSTR-09  TO  HI-MHCD HI-HCD.
      *           READ  HI2-M    UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE  TO  WORK-62
               GO  TO  READ2-040
           END-IF
           IF  JSTR-061       =  0013  OR  0459  OR  0460
               MOVE  HI-NAME TO  W-NAME
               MOVE  SPACE   TO  W-NA(24)
               MOVE  W-NAME  TO  WORK-62
           ELSE
               MOVE  HI-NAME TO  WORK-62
           END-IF.
       READ2-040.
           MOVE  SPACE    TO  WORK-67.
           IF  JSTR-09     <  999900
               PERFORM  TAN-RTN  THRU  TAN-EX
           END-IF
      *    (∫›ƒ€∞Ÿ•Ãß≤Ÿ … ÿ∞ƒﬁ)
           MOVE  2        TO  JCON2-01.
           MOVE  JSTR-14  TO  JCON2-02.
      *           READ  JCON  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE  TO  WORK-71
               GO  TO  READ2-RTN-EXIT
           END-IF
           MOVE  JCON2-03  TO  WORK-71.
       READ2-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ñæç◊èoóÕèàóù  ÅiÇcÇdÇsÇkÅ|ÇqÇsÇmÅj                          *
      ******************************************************************
       DETL-RTN.
           IF  DEN-W  NOT  =  JSTR-01
               PERFORM  HEAD-RTN  THRU  HEAD-RTN-EXIT
           END-IF
           PERFORM  WRITE-RTN  THRU  WRITE-RTN-EXIT.
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
               ADD   WORK-92     TO  GOKEI
               ADD   WORK-82     TO  SETSU
           END-IF.
       DETL-RTN-EXIT.
           EXIT.

      ******************************************************************
      *    çáåvèoóÕèàóù  ÅiÇsÇnÇsÇkÅ|ÇqÇsÇmÅj                          *
      ******************************************************************
       TOTL-RTN.
           IF  END-SW  =  9
               COMPUTE  J  =  16  -  (I  *  2)
               MOVE   GOKEI          TO  WORK-73
               MOVE   X"1A24212078"  TO  ST-CD4
               MOVE   X"1A24212474"  TO  ED-CD4
               MOVE   WORK-7         TO  PRN-R
               CALL "PR_LineFeed" USING J RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               GO  TO  TOTL-RTN-EXIT
           END-IF
           IF  JSTR-01  NOT  =  DEN-W
               COMPUTE  J  =  16  -  (I  *  2)
               MOVE   GOKEI          TO  WORK-73
               MOVE   X"1A24212078"  TO  ST-CD4
               MOVE   X"1A24212474"  TO  ED-CD4
               MOVE   WORK-7         TO  PRN-R
               CALL "PR_LineFeed" USING J RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               MOVE  0  TO  GOKEI
           END-IF.
       TOTL-RTN-EXIT.
           EXIT.

      ******************************************************************
      *    ÉZÉbÉgêîèoóÕèàóù  ÅiÇsÇnÇsÇkÇPÅ|ÇqÇsÇmÅj                    *
      ******************************************************************
       TOTL1-RTN.
           COMPUTE  J  =  16  -  (I  *  2)  -  1.
           IF  SETSU  NOT  =  0
               MOVE  SETSU    TO  WORK-61A
               MOVE  "æØƒ"    TO  WORK-62A
               MOVE   WORK-6A TO  PRN-R
               CALL "PR_LineFeed" USING J RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
           ELSE
               MOVE  SPACE    TO  PRN-R
               CALL "PR_LineFeed" USING J RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
           END-IF
           MOVE  0    TO  SETSU.
       TOTL1-EX.
      ******************************************************************
      *    å©èoÇµèoóÕèàóù  ÅiÇgÇdÇ`ÇcÅ|ÇqÇsÇmÅj                        *
      ******************************************************************
       HEAD-RTN.
           IF  PAGE-C  NOT  =  1
               MOVE  SPACE  TO  PRN-R
               CALL "PR_Write" USING PRN-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           MOVE  X"1A24212078"  TO  ST-CD0  ST-CD1  WORK-K1G.
           MOVE  X"1A24212474"  TO  ED-CD0  ED-CD1  WORK-K2G.
           IF  SEN-W    =   1
               MOVE  SPACE     TO  WORK-10
           ELSE
               MOVE  "çƒÅ@"  TO  WORK-10
           END-IF
           MOVE  JSTR-01         TO  WORK-11.
           MOVE  "TEL" TO WORK-TM WORK-TMG.
           MOVE  "FAX" TO WORK-FM.
           IF  JS-SIGN       =  9
               GO  TO  HEAD-020
           END-IF
           IF  JSTR-061       =  0459  OR  0460
               IF  JSTR-062       =  001
                   GO  TO  HEAD-010
               END-IF
           END-IF
           IF  JSTR-061       =  0459
               MOVE  "äîéÆâÔé–è„éRõâã∆" TO  WORK-NA
               MOVE  "663-8033" TO  WORK-UNO
               MOVE "ï∫å…åßêºã{ésçÇñÿìåí¨ÇPÇTÅ|ÇP" TO WORK-JS
               MOVE  "0798-64-5111  " TO  WORK-TEL
               MOVE  "0798-64-3718  " TO  WORK-FAX
               GO  TO  HEAD-070
           ELSE
               IF  JSTR-061       =  0460
                   MOVE  "äîéÆâÔé–ÉEÉGÉÑÉ}" TO  WORK-NA
                   MOVE  "663-8033" TO  WORK-UNO
                   MOVE "ï∫å…åßêºã{ésçÇñÿìåí¨ÇPÇTÅ|ÇP" TO WORK-JS
                   MOVE  "0798-67-0810  " TO  WORK-TEL
                   MOVE  "0798-67-0139  " TO  WORK-FAX
                   GO  TO  HEAD-070
               END-IF
           END-IF
           IF  JSTR-061       =  3015
               MOVE  "äîéÆâÔé–ÉXÉMÉÑÉ}" TO  WORK-NA
               MOVE  "114-0034" TO  WORK-UNO
               MOVE "ìåãûìsñkãÊè„è\èÇSÅ|ÇPÅ|ÇTÅ@" TO WORK-JS
               MOVE  "03-3909-8883  " TO  WORK-TEL
               MOVE  "03-3909-8831  " TO  WORK-FAX
               GO  TO  HEAD-070
           END-IF
           IF  JSTR-061       =  4038  OR  2654  OR  2656  OR  2657
               GO  TO  HEAD-030
           END-IF.
       HEAD-010.
           IF  JSTR-07   NOT =  1 AND 8
               GO  TO  HEAD-030
           END-IF.
       HEAD-020.
           MOVE  "ì˙êiÉSÉÄäîéÆâÔé–" TO WORK-NA.
           MOVE  "700-0975" TO  WORK-UNO.
           MOVE "â™éRésñkãÊç°ÇWíöñ⁄ÇPÇUÅ|ÇPÇV" TO  WORK-JS.
           MOVE  "086-243-2456  " TO  WORK-TEL.
           MOVE  "086-242-0550  " TO  WORK-FAX.
           GO  TO  HEAD-070.
       HEAD-030.
           IF  JSTR-061       =  4038  OR  4054  OR  4056  OR  4057
               MOVE  "êÁã»éYã∆äîéÆâÔé–Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@"  TO
                                                            WORK-NAG
               MOVE  "542-0081"      TO  WORK-UNOG
               MOVE  "ëÂç„ésíÜâõãÊìÏëDèÍÇPíöñ⁄ÇPÇPÅ|ÇXÅ@Å@Å@Å@"  TO
                                                            WORK-JSG
               MOVE  "Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@" TO  WORK-JSSG
               MOVE  "06-6268-4561"                     TO  WORK-TELG
               GO  TO  HEAD-070
           END-IF
           IF  JSTR-061       =  2654  OR  2656  OR  2657
               MOVE  "äîéÆâÔé–êVì˙ñ{ã≥àÁÉVÉÖÅ[ÉYÅ@Å@Å@Å@Å@"  TO
                                                            WORK-NAG
               MOVE  "520-2132"      TO  WORK-UNOG
               MOVE  "é†âÍåßëÂí√ésê_óÃÇPÅ|ÇXÅ|ÇRÅ@Å@Å@Å@Å@Å@Å@"  TO
                                                            WORK-JSG
               MOVE  "Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@" TO  WORK-JSSG
               MOVE  "077-543-1331"                     TO  WORK-TELG
               GO  TO  HEAD-070
           END-IF
           IF  JSTR-07       =  6
               MOVE  "ì˙êiÉSÉÄäîéÆâÔé–Å@ã ìáï®ó¨ÉZÉìÉ^Å[Å@"  TO
                                                            WORK-NAG
               MOVE  "713-8103"      TO  WORK-UNOG
               MOVE  "â™éRåßëqï~ésã ìáâ≥ìáéöêVñ©ÇWÇQÇUÇQÅ|ÇPÅ@"  TO
                                                            WORK-JSG
               MOVE  "Å@Å@Å@Å@êÖìáç`çëç€ï®ó¨ÉZÉìÉ^Å[ì‡" TO  WORK-JSSG
               MOVE  "086-243-2456"                     TO  WORK-TELG
           ELSE
             IF  JSTR-07       =  7
               MOVE  "ì˙êiÉSÉÄäîéÆâÔé–Å@í√éRï®ó¨ÉZÉìÉ^Å[Å@"  TO
                                                            WORK-NAG
               MOVE  "709-3717"      TO  WORK-UNOG
               MOVE  "â™éRåßãvïƒåSî¸çÁí¨å¥ìcÇRÇQÇQÇVÅ|ÇPÅ@Å@Å@"  TO
                                                            WORK-JSG
               MOVE  "086-243-2456"                     TO  WORK-TEL
               MOVE  "086-242-0550"                     TO  WORK-FAX
           ELSE
             IF  JSTR-07       =  4
               MOVE  "ì˙êiÉSÉÄÅ@ëÅìáîzëóÉZÉìÉ^Å[Å@Å@Å@Å@Å@"  TO
                                                            WORK-NAG
               MOVE  "701-0304"      TO  WORK-UNOG
               MOVE  "â™éRåßìsåEåSëÅìáí¨ëÅìáÇSÇTÇOÇVÅ|ÇRÇVÅ@Å@"  TO
                                                            WORK-JSG
               MOVE  "Å@Å@óºîıÇgÇcíÜélçëï®ó¨ÉZÉìÉ^Å[ì‡" TO  WORK-JSSG
               MOVE  "086-243-2456"                     TO  WORK-TELG
           END-IF.
       HEAD-070.
           MOVE    "'"           TO  WORK-M2  WORK-M2G.
           MOVE  JSTR-0412       TO  WORK-211  WORK-211G.
           MOVE  JSTR-042        TO  WORK-212  WORK-212G.
           MOVE  JSTR-043        TO  WORK-213  WORK-213G.
           IF  JSTR-03  =  0
               MOVE  "èoâ◊"  TO  WORK-22  WORK-22G
           END-IF
           IF  JSTR-03  =  5
               MOVE  "ï‘ïi"  TO  WORK-22  WORK-22G
           END-IF
           IF  JSTR-03  =  6
               MOVE  "ïsó«"  TO  WORK-22  WORK-22G
           END-IF
           IF  JSTR-03  =  7
               MOVE  "ÉTèo"  TO  WORK-22  WORK-22G
           END-IF
           MOVE  3         TO  JCON3-01.
           MOVE  JSTR-07   TO  JCON3-02.
      *           READ  JCON      UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ALL "Å@"  TO  JCON3-03
           END-IF
           MOVE  JCON3-03  TO  WORK-23  WORK-23G.
           IF  JSTR-07       =  1
               IF  JSTR-061      =  2654  OR  2656  OR  2657  OR  4038
                                          OR  4054  OR  4056  OR  4057
                   MOVE  "ïxÅ@émÅ@Å@Å@"  TO  WORK-23  WORK-23G
               END-IF
           END-IF
           IF  JS-SIGN   NOT =  9
               MOVE  JSTR-14B  TO  WORK-31  WORK-31G
           END-IF.
       HEAD-090.
           MOVE  SPACE     TO  WORK-33A.
           MOVE  "("       TO  WORK-33F.
           MOVE  T-TNC1    TO  WORK-331.
           MOVE  "-"       TO  WORK-332.
           MOVE  T-DCC     TO  WORK-333.
           MOVE  ")"       TO  WORK-33R.
           MOVE  W-DATE    TO  WORK-32.
           MOVE  1         TO  I.
           ADD   1         TO  PAGE-C.
           MOVE   WORK-1   TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           IF  JSTR-061       =  4038  OR  2654  OR  2656  OR  2657
                                          OR  4054  OR  4056  OR  4057
               GO  TO  HEAD-100
           END-IF
           IF (JS-SIGN         =   9)  OR  (JSTR-07    =  1 OR 8 )  OR
             ((JSTR-062  NOT  = 001)  AND (JSTR-061  =  0459 OR 0460))
                                       OR  (JSTR-061      =  3015)
               MOVE   WORK-1A  TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               MOVE   WORK-1B  TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               MOVE   WORK-1C  TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               MOVE   WORK-2  TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               MOVE   WORK-3  TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               GO  TO  HEAD-120
           END-IF.
       HEAD-100.
           MOVE   WORK-1AG  TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE   WORK-1BG  TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE   WORK-1CG  TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           IF (JSTR-07       =  4 OR 6)  OR
              (JSTR-061      =  4038  OR  2654  OR  2656  OR  2657
                                          OR  4054  OR  4056  OR  4057)
               MOVE   WORK-2G  TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               MOVE   WORK-3G  TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
           ELSE
               MOVE   WORK-2  TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               MOVE   WORK-3  TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
           END-IF.
       HEAD-120.
           MOVE   WORK-3A  TO  PRN-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE   WORK-4  TO  PRN-R.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           IF  JS-SIGN       =  9
               MOVE  JSTR-15A  TO  WORK-70
           ELSE
               IF  JSTR-14B  =  OLD-OKNO
                   MOVE  SPACE     TO  WORK-70R
               ELSE
                   MOVE  JSTR-15A  TO  WORK-70
               END-IF
           END-IF
           MOVE  JSTR-15   TO  WORK-721.
           MOVE  JSTR-14D  TO  WORK-722.
           IF  JS-SIGN   NOT =  9
               MOVE  JSTR-14B  TO  OLD-OKNO
           END-IF.
       HEAD-RTN-EXIT.
           EXIT.

      ******************************************************************
      *    ÇvÇqÇhÇsÇdèàóù  ÅiÇvÇqÇhÇsÇdÅ|ÇqÇsÇmÅj                      *
      ******************************************************************
       WRITE-RTN.
           INITIALIZE  WORK-5  WORK-8  WORK-9.
           MOVE  X"1A24212078"  TO          ST-CD3.
           MOVE  X"1A24212474"  TO  ED-CD2  ED-CD3.
           MOVE  SPACE       TO  WORK-NGPD.
           IF (JSTR-14A  NOT  >  1)  AND  (JSTR-20    =  SPACE)
             AND  ((JSTR-01   >  099999)  AND  (JSTR-08    =  ZERO))
               IF  I  =  1
                   MOVE   WORK-5         TO  PRN-R
                   CALL "PR_LineFeed" USING "5" RETURNING RESP
                   CALL "PR_Write" USING PRN-R RETURNING RESP
                   GO  TO  WRITE-010
               ELSE
                   MOVE   WORK-5         TO  PRN-R
                   CALL "PR_LineFeed" USING "1" RETURNING RESP
                   CALL "PR_Write" USING PRN-R RETURNING RESP
                   GO  TO  WRITE-010
               END-IF
           END-IF
           IF  JSTR-14A       >  1
               COMPUTE WORK-811(1)  = JSTR-1111(1)  / JSTR-14A
               COMPUTE WORK-811(2)  = JSTR-1111(2)  / JSTR-14A
               COMPUTE WORK-811(3)  = JSTR-1111(3)  / JSTR-14A
               COMPUTE WORK-811(4)  = JSTR-1111(4)  / JSTR-14A
               COMPUTE WORK-811(5)  = JSTR-1111(5)  / JSTR-14A
               COMPUTE WORK-811(6)  = JSTR-1111(6)  / JSTR-14A
               COMPUTE WORK-811(7)  = JSTR-1111(7)  / JSTR-14A
               COMPUTE WORK-811(8)  = JSTR-1111(8)  / JSTR-14A
               COMPUTE WORK-811(9)  = JSTR-1111(9)  / JSTR-14A
               COMPUTE WORK-811(10) = JSTR-1111(10) / JSTR-14A
               COMPUTE WORK-82      = JSTR-1111(1)  + JSTR-1111(2)
                                    + JSTR-1111(3)  + JSTR-1111(4)
                                    + JSTR-1111(5)  + JSTR-1111(6)
                                    + JSTR-1111(7)  + JSTR-1111(8)
                                    + JSTR-1111(9)  + JSTR-1111(10)
               COMPUTE WORK-82      = WORK-82       / JSTR-14A
               MOVE  WORK-811(1)   TO  WORK-521(1)
               MOVE  WORK-811(2)   TO  WORK-521(2)
               MOVE  WORK-811(3)   TO  WORK-521(3)
               MOVE  WORK-811(4)   TO  WORK-521(4)
               MOVE  WORK-811(5)   TO  WORK-521(5)
               MOVE  WORK-811(6)   TO  WORK-521(6)
               MOVE  WORK-811(7)   TO  WORK-521(7)
               MOVE  WORK-811(8)   TO  WORK-521(8)
               MOVE  WORK-811(9)   TO  WORK-521(9)
               MOVE  WORK-811(10)  TO  WORK-521(10)
               MOVE  WORK-82       TO  WORK-53
               MOVE  "X"           TO  WORK-54
               MOVE  JSTR-14A      TO  WORK-55
           END-IF
           IF  JSTR-20    NOT =  SPACE
               MOVE  "["           TO  WORK-51F
               MOVE  JSTR-20       TO  WORK-51
               MOVE  "]"           TO  WORK-51R
           END-IF
           IF (JSTR-081   =  ZERO)  AND  (JSTR-082   =  ZERO)
               MOVE  SPACE         TO  WORK-NGPD
               GO  TO  WRITE-005
           END-IF
           MOVE  JSTR-08       TO  JMSTD-KEY1.
      *           READ  JMSTD  UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JMSTD_PNAME1 BY REFERENCE JMSTD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ZERO          TO  WORK-NGP
               GO  TO  WRITE-005
           END-IF
           IF  JMSTD-01   NOT =  6
               MOVE  JMSTD-02S     TO  WORK-NGP
           END-IF.
       WRITE-005.
           IF  I  =  1
               MOVE   WORK-5         TO  PRN-R
               CALL "PR_LineFeed" USING "5" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
           ELSE
               MOVE   WORK-5         TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
           END-IF.
       WRITE-010.
           MOVE  JSTR-09       TO  WORK-61.
           MOVE  JSTR-10       TO  WORK-63.
           MOVE  JSTR-1111(1)   TO  WORK-641(1).
           MOVE  JSTR-1111(2)   TO  WORK-641(2).
           MOVE  JSTR-1111(3)   TO  WORK-641(3).
           MOVE  JSTR-1111(4)   TO  WORK-641(4).
           MOVE  JSTR-1111(5)   TO  WORK-641(5).
           MOVE  JSTR-1111(6)   TO  WORK-641(6).
           MOVE  JSTR-1111(7)   TO  WORK-641(7).
           MOVE  JSTR-1111(8)   TO  WORK-641(8).
           MOVE  JSTR-1111(9)   TO  WORK-641(9).
           MOVE  JSTR-1111(10)  TO  WORK-641(10).
           COMPUTE  WORK-92  =  JSTR-1111(1)  +  JSTR-1111(2)
                             +  JSTR-1111(3)  +  JSTR-1111(4)
                             +  JSTR-1111(5)  +  JSTR-1111(6)
                             +  JSTR-1111(7)  +  JSTR-1111(8)
                             +  JSTR-1111(9)  +  JSTR-1111(10).
       WRITE-020.
           IF  JSTR-13  =  5 OR 4
               MOVE  "("       TO  WORK-651
               MOVE  WORK-92   TO  WORK-652
               MOVE  ")"       TO  WORK-66
           ELSE
               MOVE  WORK-92   TO  WORK-65
               MOVE  SPACE     TO  WORK-66
           END-IF
           MOVE   WORK-6  TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           ADD  1  TO  I.
       WRITE-RTN-EXIT.
           EXIT.

      ******************************************************************
      *    íPâøåüèoèàóù  ÅiÇsÇ`ÇmÅ|ÇqÇsÇmÅj                            *
      ******************************************************************
       TAN-RTN.
           MOVE JSTR-061 TO W-TCD.
           IF  W-TCD = 0460
               MOVE 0458 TO W-TCD
           END-IF
           MOVE 0 TO W-TAN.
           MOVE W-TCD TO THT-TCD.
           MOVE JSTR-09 TO THT-HCD.
           MOVE JSTR-10 TO THT-SIZ.
      *           READ THTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO THT-T
           END-IF
           MOVE THT-T TO W-TAN.
           IF  W-TAN NOT = ZERO
               GO TO TAN-EX
           END-IF
           MOVE W-TCD TO THT-TCD.
           MOVE JSTR-09 TO THT-HCD.
           MOVE 9 TO THT-SIZ.
      *           READ THTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO THT-T
           END-IF
           MOVE THT-T TO W-TAN.
           IF  W-TAN = ZERO
               MOVE  "•"      TO  WORK-67
           END-IF.
       TAN-EX.
           EXIT.
      ******************************************************************
      *    ÉeÉXÉgàÛéöèàóù  ÅiÇsÇdÇrÇsÅ|ÇqÇsÇmÅj                        *
      ******************************************************************
       TEST-RTN.
           CALL "PR_Open" RETURNING RESP.
           MOVE X"1A24212078"  TO  ST-CD0  ST-CD1.
           MOVE X"1A24212474"  TO  ED-CD0  ED-CD1.
           MOVE  SPACE         TO  WORK-10.
           MOVE  ALL    "9"    TO  WORK-11.
           MOVE  "'"           TO  WORK-M2.
           MOVE  ALL    "9"    TO  WORK-211  WORK-212  WORK-213.
           MOVE  ALL  "Çm"   TO  WORK-23.
           MOVE  ALL    "9"    TO  WORK-31  WORK-32.
           MOVE  ALL    "9"    TO  WORK-41  WORK-43.
           MOVE  ALL  "ÇX"   TO  WORK-42  WORK-44.
           IF  JS-SIGN     NOT =   9
               MOVE  ALL  "Çm"     TO  WORK-NA  WORK-JS
               MOVE  "TEL"         TO  WORK-TM
               MOVE  "FAX"         TO  WORK-FM
               MOVE  ALL    "X"    TO  WORK-UNO WORK-TEL WORK-FAX
           END-IF
           MOVE   WORK-1  TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           IF  JS-SIGN     NOT =   9
               MOVE   WORK-1A  TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               MOVE   WORK-1B  TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               MOVE   WORK-1C  TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               MOVE   WORK-2  TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
           ELSE
               MOVE   WORK-2  TO  PRN-R
               CALL "PR_LineFeed" USING "4" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
           END-IF
           MOVE   WORK-3  TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE   WORK-3A  TO  PRN-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE   WORK-4  TO  PRN-R.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE  1  TO  I.
       TEST-010.
           MOVE X"1A24212078"  TO          ST-CD3.
           MOVE X"1A24212474"  TO  ED-CD2  ED-CD3.
           MOVE         99     TO  WORK-521(1)  WORK-521(2)
                                   WORK-521(3)  WORK-521(4)
                                   WORK-521(5)  WORK-521(6)
                                   WORK-521(7)  WORK-521(8)
                                   WORK-521(9)  WORK-521(10).
           MOVE         999    TO  WORK-53.
           MOVE         "X"    TO  WORK-54.
           MOVE         999    TO  WORK-55.
           MOVE  ALL    "9"    TO  WORK-61.
           MOVE  ALL  "ÇX"     TO  WORK-62.
           MOVE  ALL    "9"    TO  WORK-63.
           MOVE  ALL    "9"    TO  WORK-641(1)  WORK-641(2)
                                   WORK-641(3)  WORK-641(4)
                                   WORK-641(5)  WORK-641(6)
                                   WORK-641(7)  WORK-641(8)
                                   WORK-641(9)  WORK-641(10).
           MOVE  ALL    "9"    TO  WORK-65.
           IF  I  =  1
               MOVE   WORK-5         TO  PRN-R
               CALL "PR_LineFeed" USING "5" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               MOVE   WORK-6         TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
           ELSE
               MOVE   WORK-5         TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               MOVE   WORK-6         TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
           END-IF
           IF  I  =  6
               GO  TO  TEST-020
           ELSE
               ADD  1  TO  I
               GO  TO  TEST-010
           END-IF.
       TEST-020.
           MOVE X"1A24212078"  TO  ST-CD4.
           MOVE X"1A24212474"  TO  ED-CD4.
           MOVE   WORK-6A  TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE  ALL  "ÇX"   TO  WORK-71
                                   WORK-721  WORK-722.
           MOVE  ALL    "9"    TO  WORK-73  WORK-70.
           MOVE   WORK-7  TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           CALL "PR_Close" RETURNING RESP.
       TEST-RTN-EXIT.
           EXIT.
       SEL-RTN.
           MOVE  ZERO  TO  W-SDATE  W-EDATE  W-SNGP  W-ENGP.
           CALL "DB_F_Open" USING
            "INPUT" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
       SEL-010.
      *           READ  JSTR  NEXT  RECORD  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  SEL-090
           END-IF
           IF  JSTR-158 NOT = 0
               GO  TO  SEL-010
           END-IF
           IF  JSTR-05       =  ZERO
               GO  TO  SEL-010
           END-IF
           IF  W-JS      NOT =  9
               IF  JSTR-16   NOT =  W-JS
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  JSTR-17   NOT =  9 AND 8
               GO  TO  SEL-010
           END-IF
           IF  JS-SIGN  =  0
               IF  JSTR-07  NOT =  1  AND  8
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  JS-SIGN  =  1
               IF  JSTR-07  NOT =  6
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  JS-SIGN  =  2
               IF  JSTR-07  NOT =  7
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  JS-SIGN  =  3
               IF  JSTR-07  NOT =  4
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  JS-SIGN       =  9
               IF  JSTR-03     NOT =  5  AND 6
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  JS-SIGN   NOT =  9
               IF  JSTR-03     NOT =  0
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  JS-SIGN       =  1
               IF  JSTR-061        =  9850   OR  5000
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  W-SDATE = ZERO
               MOVE  JSTR-04  TO  W-SDATE
           END-IF
           IF  W-SDATE > JSTR-04
               MOVE  JSTR-04  TO  W-SDATE
           END-IF
           IF  W-EDATE < JSTR-04
               MOVE  JSTR-04  TO  W-EDATE
           END-IF
           GO  TO  SEL-010.
       SEL-090.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           MOVE  W-SDATE  TO  W-SNGP.
           MOVE  W-EDATE  TO  W-ENGP.
       SEL-EX.
           EXIT.
      ******************************************************************
      *    ÇnÇoÇdÇmèàóù  ÅiÇnÇoÇdÇmÅ|ÇqÇsÇmÅj                        *
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
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" JMSTD_PNAME1 "SHARED" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2" BY
            REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE JMSTD-KEY3.
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
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       CLSE-RTN-EXIT.
           EXIT.
      ***
           COPY  LPERR.

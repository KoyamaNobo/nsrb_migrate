       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT287L.
       AUTHOR.                        E-SHIGIHARA.
      *************************************************************
      *    PROGRAM        : ïiñºíSìñìæà”êÊï éÛíçécí†ÅiâûópópéÜÅj  *
      *    DATA WRITTEN   : 87/08/28                              *
      *    SCREEN USED    : UNUSED                                *
      *    FORM   USED    : UNUSED                                *
      *    PRINTER TYPE   : JIPS                                  *
      *    COMPILE TYPE   : COBOL                                 *
      *************************************************************
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM150.
       OBJECT-COMPUTER.               SYSTEM150.
       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77  ERR-STAT                  PIC  X(002) VALUE SPACE.
       77  WK0256ID                  PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1               PIC  X(003).
           02  STN-NO2               PIC  X(003).
       01  W-FID.
           02  W-FID1                PIC  X(006) VALUE "WK0256".
           02  W-FID2                PIC  X(003).
       01  R1-R.
           02  R1-K1                 PIC X(05).
           02  R1-HCD                PIC 9(06).
           02  FILLER                PIC X(01).
           02  R1-HNA                PIC N(24).
           02  FILLER                PIC X(93).
           02  R1-K2                 PIC X(05).
       01  R2-R.
           02  R2-K1                 PIC X(05).
           02  FILLER                PIC X(06).
           02  R2-TCD                PIC 9(04).
           02  FILLER                PIC X(01).
           02  R2-TNA                PIC N(26).
           02  FILLER                PIC X(86).
           02  R2-K2                 PIC X(05).
       01  R3.
           02  R3-R        OCCURS  4.
               03  R3-K2             PIC X(5).
               03  FILLER            PIC X(2).
               03  R3-00A            PIC X(10).
               03  R3-00B     REDEFINES  R3-00A.
                   04  FILLER        PIC X(1).
                   04  R3-00         PIC 9(02).
                   04  FILLER        PIC X(7).
               03  FILLER            PIC X(1).
               03  R3-01             PIC X(01).
               03  R3-02             PIC ZZ.
               03  R3-03             PIC X(01).
               03  R3-04             PIC ZZ.
               03  R3-05             PIC X(01).
               03  R3-A              PIC X(01).
               03  R3-06             PIC ZZ.
               03  R3-B              PIC X(01).
               03  R3-07             PIC ZZ.
               03  R3-C              PIC X(01).
               03  R3-D              PIC X(01).
               03  R3-TD             PIC N(08).
               03  R3-MD      REDEFINES  R3-TD.
                   04  R3-08         PIC 9(06).
                   04  R3-E          PIC X(01).
                   04  R3-09         PIC 9(01).
                   04  R3-F          PIC X(01).
                   04  R3-G          PIC X(01).
                   04  R3-10         PIC ZZ,ZZZ.
               03  R3-H              PIC X(01).
               03  FILLER            PIC X(01).
               03  R3-16             PIC 9(01).
               03  R3-17        OCCURS   10.
                   04  R3-171        PIC ----,---.
               03  R3-18             PIC -----,--9.
       01  R4-R.
           02  R4-K1                 PIC X(05).
           02  FILLER                PIC X(10).
           02  R4-TEN                PIC 9(04).
           02  R4-TEND  REDEFINES R4-TEN.
               03  F                 PIC X(01).
               03  R4-CCD            PIC 9(03).
           02  FILLER                PIC X(01).
           02  R4-CNA                PIC N(26).
           02  FILLER                PIC X(82).
           02  R4-K2                 PIC X(05).
       01  MID1-R.
           02  M1-K2                 PIC X(05)  VALUE  X"1A24212474".
           02  FILLER                PIC X(47)    VALUE   SPACE.
           02  M1-MDS                PIC N(19).
           02  F                     PIC X(18)  VALUE SPACE.
           02  F                     PIC X(05)  VALUE "DATE ".
           02  M1-01                 PIC 9(02).
           02  FILLER                PIC X(01)    VALUE   ".".
           02  M1-02                 PIC Z9.
           02  FILLER                PIC X(01)    VALUE   ".".
           02  M1-03                 PIC Z9.
           02  FILLER                PIC X(15)    VALUE   SPACE.
           02  FILLER                PIC X(02)    VALUE   "P.".
           02  M1-04                 PIC ZZ9.
       01  MID2-R.
           02  M2-K1                 PIC X(05)  VALUE  X"1A24212078".
           02  FILLER                PIC X(07)  VALUE   " ∫∞ƒﬁ  ".
           02  FILLER                PIC N(08)  VALUE
                 "ïiÅ@Å@Å@Å@Å@ñºÅ@".
           02  FILLER                PIC X(27)  VALUE   SPACE.
           02  FILLER                PIC X(01)  VALUE   "1".
           02  FILLER                PIC X(05)    VALUE   SPACE.
           02  FILLER                PIC N(02)  VALUE  "ÇRçÜ".
           02  FILLER                PIC X(05)    VALUE   SPACE.
           02  FILLER                PIC N(02)  VALUE  "ÇQçÜ".
           02  FILLER                PIC X(05)    VALUE   SPACE.
           02  FILLER                PIC N(02)  VALUE  "ÇPçÜ".
           02  FILLER                PIC X(05)    VALUE   SPACE.
           02  FILLER                PIC N(02)  VALUE  "ÇOçÜ".
           02  FILLER                PIC X(05)    VALUE   SPACE.
           02  FILLER                PIC N(02)  VALUE  "Å@íÜ".
           02  FILLER                PIC X(05)    VALUE   SPACE.
           02  FILLER                PIC N(02)  VALUE  "Å@ëÂ".
           02  FILLER                PIC X(05)    VALUE   SPACE.
           02  FILLER                PIC N(02)  VALUE  "ì¡ëÂ".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "28.0".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "29.0".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "30.0".
           02  FILLER                PIC X(09)    VALUE   SPACE.
           02  F                     PIC X(05)  VALUE  X"1A24212474".
       01  MID3-R.
           02  M3-K1                 PIC X(05)  VALUE  X"1A24212078".
           02  FILLER                PIC X(06)    VALUE   SPACE.
           02  FILLER                PIC X(05)    VALUE "∫∞ƒﬁ ".
           02  FILLER                PIC N(10)    VALUE
                 "ìæÅ@Å@à”Å@Å@êÊÅ@Å@ñº".
           02  FILLER                PIC X(20)    VALUE   SPACE.
           02  FILLER                PIC X(01)    VALUE "2".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "12.5".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "13.0".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "13.5".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "14.0".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "15.0".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "16.0".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "17.0".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "18.0".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "19.0".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "20.0".
           02  FILLER                PIC X(09)    VALUE   SPACE.
           02  M3-K2                 PIC X(05)  VALUE  X"1A24212474".
       01  MID4-R.
           02  FILLER                PIC X(05)  VALUE  X"1A24212078".
           02  FILLER                PIC X(02)    VALUE   SPACE.
           02  FILLER                PIC N(02)  VALUE  "íSìñ".
           02  FILLER                PIC X(08)    VALUE   SPACE.
           02  FILLER                PIC X(02)  VALUE "( ".
           02  FILLER                PIC N(02)  VALUE  "éÛíç".
           02  FILLER                PIC X(04)  VALUE " )( ".
           02  FILLER                PIC N(02)  VALUE  "èoâ◊".
           02  FILLER                PIC X(04)  VALUE " )( ".
           02  FILLER                PIC N(04)  VALUE  "éÛíçáÇçs".
           02  FILLER                PIC X(05)  VALUE " )(  ".
           02  FILLER                PIC N(02)  VALUE  "íPâø".
           02  FILLER                PIC X(03)  VALUE " ) ".
           02  FILLER                PIC X(01)  VALUE "3".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "21.0".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "21.5".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "22.0".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "22.5".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "23.0".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "23.5".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "24.0".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "24.5".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "25.0".
           02  FILLER                PIC X(17)    VALUE   SPACE.
           02  M4-K2                 PIC X(05)  VALUE  X"1A24212474".
       01  MID5-R.
           02  F                     PIC X(05)  VALUE  X"1A24212078".
           02  FILLER                PIC X(11)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE "∫∞ƒﬁ".
           02  FILLER                PIC N(12)    VALUE
                 "íºÅ@ëóÅ@êÊÅ@ÅEÅ@ìXÅ@ñºÅ@".
           02  FILLER                PIC X(13)    VALUE   SPACE.
           02  FILLER                PIC X(01)    VALUE "4".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "24.0".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "24.5".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "25.0".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "25.5".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "26.0".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "26.5".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "27.0".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "27.5".
           02  FILLER                PIC X(19)    VALUE   SPACE.
           02  FILLER                PIC N(04)  VALUE  "Å@çáÅ@åv".
           02  M5-K2                 PIC X(05)  VALUE  X"1A24212474".
       01  ACT-WORK.
           02  W-OK                  PIC 9(01).
           02  I                     PIC 9(02).
           02  TOT                   PIC S9(07).
           02  OLD-1                 PIC 9(06).
           02  OLD-5                 PIC 9(02).
           02  OLD-2                 PIC 9(04).
           02  OLD-3                 PIC 9(06).
           02  OLD-10                PIC 9(03).
           02  OLD-23                PIC 9(04).
           02  NEW-1                 PIC 9(06).
           02  NEW-2                 PIC 9(04).
           02  NEW-10                PIC 9(03).
           02  NEW-23                PIC 9(04).
           02  CNT                   PIC 9(01).
           02  SW1                   PIC 9(01).
           02  SW2                   PIC 9(01).
           02  SW3                   PIC 9(01).
           02  W-ZAN                 PIC S9(06).
           02  0-CNT     OCCURS  4   PIC  9(02).
           02  W-SEN                 PIC  9(01).
           02  W-JYU.
               03  W-J1              PIC  9(01).
               03  W-J2              PIC  9(05).
           02  N                     PIC  9(01).
           02  CNT-M                 PIC  9(01).
           02  CNT-M1                PIC  9(01).
           02  W-NC                  PIC  9(01).
           02  O-SW1                 PIC  9(01).
           02  O-SW2                 PIC  9(01).
           02  O-SW3                 PIC  9(01).
           02  ASW                   PIC  9(01).
           02  J                     PIC  9(01).
           02  W-GC                  PIC  9(01).
           02  W-PSW1                PIC  9(01).
           02  W-PSW2                PIC  9(01).
           02  W-PSW3                PIC  9(01).
           02  W-DCC                 PIC  9(01).
           02  W-TCNC                PIC  9(01).
           02  W-CNA                 PIC  N(26).
       01  W-A.
           02  W-AA        OCCURS         4.
               03  W-AAA             PIC  S9(06)  OCCURS 10.
       01  W-B.
           02  W-BB        OCCURS         4.
               03  W-BBB             PIC  S9(06)  OCCURS 10.
       01  W-C.
           02  W-CC        OCCURS         4.
               03  W-CCC             PIC  S9(06)  OCCURS 10.
       01  W-AREA.
           02  PCNT                  PIC 9(03)   VALUE   0.
           02  LCNT                  PIC 9(02)   VALUE  90.
       01  WYMD.
           02  WYY                   PIC 9(02).
           02  WMM                   PIC 9(02).
           02  WDD                   PIC 9(02).
       01  K-1                       PIC X(05)  VALUE  X"1A24212078".
       01  K-2                       PIC X(05)  VALUE  X"1A24212474".
       COPY    LWMSG.
      *
           COPY  LITCM.
           COPY  LIHIM2.
           COPY  LTWK04.
           COPY  LWTNAF.
      *FD  P-F
       77  P-R                       PIC X(256).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLE-01.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  CLE-02.
           02  FILLER  PIC  X(06) VALUE "      ".
           02  FILLER  PIC  X(06) VALUE "      ".
           02  FILLER  PIC  X(01) VALUE " ".
       01  DSP-AREA1.
           02  DSP-00.
               03  FILLER  PIC  X(24) VALUE
                   "                        ".
               03  FILLER  PIC  X(22) VALUE
                   "ïiñºÅEíSìñï Å@éÛíçécí†".
       01  DSP-AREA2.
           02  DSP-01.
               03  FILLER  PIC  X(16) VALUE  "éwê}ä‹Ç‹Ç»Ç¢ = 0".
           02  DSP-02.
               03  FILLER  PIC  X(23) VALUE  "éwê}ä‹Çﬁ     = 1  ...  ".
           02  DSP-07.
               03  FILLER  PIC  X(06) VALUE  "ämîFÅi".
               03  FILLER  PIC  X(09) VALUE  "OK=1,NO=9".
               03  FILLER  PIC  X(02) VALUE  "Åj".
               03  FILLER  PIC  X(08) VALUE  "--> ÿ¿∞›".
       01  ACP-AREA.
           02  ACP-SEN     PIC 9(01).
           02  ACP-OK      PIC 9(01).
       COPY    LSMSG.
      *
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "P1-999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "CLE-01" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLE-01" "X" "1" "0" "12" " " "CLE-01" RETURNING RESU.
      *CLE-02
       CALL "SD_Init" USING 
            "CLE-02" " " "0" "0" "13" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLE-02" "X" "8" "33" "6" " " "CLE-02"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLE-02" "X" "10" "33" "6" "01CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLE-02" "X" "23" "61" "1" "02CLE-02" " " RETURNING RESU.
      *DSP-AREA1
       CALL "SD_Init" USING 
            "DSP-AREA1" " " "0" "0" "46" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-00" " " "1" "0" "46" " " "DSP-AREA1"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-00" "RX" "1" "23" "24" " " "DSP-00" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-00" "X" "1" "24" "22" "01DSP-00" " " RETURNING RESU.
      *DSP-AREA2
       CALL "SD_Init" USING 
            "DSP-AREA2" " " "0" "0" "64" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "7" "0" "16" " " "DSP-AREA2"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "X" "7" "24" "16" " " "DSP-01"  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-02" " " "9" "0" "23" "DSP-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-02" "X" "9" "24" "23" " " "DSP-02"  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-07" " " "23" "0" "25" "DSP-02" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-07" "X" "23" "41" "6" " " "DSP-07"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-07" "X" "23" "47" "9" "01DSP-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-07" "X" "23" "56" "2" "02DSP-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-07" "X" "23" "58" "8" "03DSP-07" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "2" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SEN" "9" "9" "46" "1" " " "ACP-AREA"  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OK" "9" "23" "61" "1" "ACP-SEN" " "  RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-OK" BY REFERENCE W-OK "1" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ************************************
      *    ÉÅÉCÉìÅ@ÉãÅ[É`Éì              *
      ************************************
       HAJIME.
           PERFORM   INT-RTN   THRU  INT-EX.
       MR010.
           CALL "SD_Accept" USING BY REFERENCE ACP-SEN "ACP-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  MR999
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR010
           END-IF
           CALL "SD_Output" USING "ACP-SEN" ACP-SEN "p" RETURNING RESU.
           IF  W-SEN  NOT  =  "0"   AND    "1"
               GO  TO  MR010
           END-IF.
       MR040.
           CALL "SD_Accept" USING BY REFERENCE ACP-OK "ACP-OK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR010
           END-IF
           IF  ESTAT  NOT  =  "01"
               GO  TO  MR040
           END-IF
           IF  W-OK   NOT  =  "1"   AND      "9"
               GO  TO  MR040
           END-IF
           IF  W-OK        =  "9"
               CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU
               INITIALIZE   ACT-WORK
               GO  TO  MR010
           END-IF
           IF  W-SEN       =   0
               MOVE  "ïiñºÅEíSìñÅ@éÛíçécí†Å@Åiéwê}ä‹Ç‹Ç»Ç¢Åj" TO  M1-MDS
           ELSE
               MOVE  "ïiñºÅEíSìñÅ@éÛíçécí†Å@Åiéwê}ä‹ÇﬁÅjÅ@Å@" TO  M1-MDS
           END-IF.
       MR050.
           MOVE      ZERO  TO   W-GC.
      *           READ      JT-WK04             AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-WK04_PNAME1 BY REFERENCE WK04-R " "
            RETURNING RET.
           IF  RET = 1
               PERFORM   MOV2-RTN  THRU   MOV2-EX
               PERFORM   GP1-RTN   THRU   GP1-EX
               PERFORM   GP3-RTN   THRU   GP3-EX
               PERFORM   GP2-RTN   THRU   GP2-EX
               GO   TO   MR999
           END-IF
           IF  LCNT  =   90
               GO  TO  MR060
           END-IF
           IF  (WK04-03       NOT = OLD-1) OR (CNT = 0)
               PERFORM   MOV2-RTN  THRU   MOV2-EX
               PERFORM   GP1-RTN   THRU   GP1-EX
               PERFORM     GP3-RTN     THRU     GP3-EX
               PERFORM     GP2-RTN     THRU     GP2-EX
               MOVE  1   TO   CNT-M1
               MOVE  1   TO   CNT-M
               GO   TO   MR060
           END-IF
           IF  WK04-91       NOT = OLD-5
               PERFORM   MOV2-RTN  THRU   MOV2-EX
               PERFORM   GP1-RTN   THRU   GP1-EX
               PERFORM     GP3-RTN     THRU     GP3-EX
               MOVE  1   TO   CNT-M1
               GO   TO   MR060
           END-IF
           IF  WK04-04       NOT = OLD-2
               PERFORM   MOV2-RTN  THRU   MOV2-EX
               PERFORM   GP1-RTN   THRU   GP1-EX
               MOVE  1   TO   CNT-M1
               GO   TO   MR060
           END-IF
           IF  WK04-07       NOT = OLD-3
               PERFORM   MOV2-RTN  THRU   MOV2-EX
           END-IF.
       MR060.
           PERFORM   MOV-RTN   THRU   MOV-EX.
           PERFORM   TOT-RTN   THRU   TOT-EX.
           GO   TO   MR050.
       MR999.
           PERFORM   END-RTN   THRU   END-EX.
           CALL "DB_Close".
           STOP      RUN.
      *********************************************
      *    ÇhÇmÇsÅ|ÇqÇsÇm                         *
      *********************************************
       INT-RTN.
           INITIALIZE     ACT-WORK    R3.
           MOVE     ZERO    TO   W-A   W-B  W-C.
           ACCEPT         WYMD        FROM     DATE.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-00" DSP-00 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA2" DSP-AREA2 "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JT-WK04_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-WK04_PNAME1 " " BY REFERENCE JT-WK04_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" WTNAF_PNAME1 "SHARED" BY REFERENCE WTNAF_IDLST "1"
            "WTNA-KEY" BY REFERENCE WTNA-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
       INT-EX.
           EXIT.
      *********************************************
      *    ÇdÇmÇcÅ|ÇqÇsÇm                         *
      *********************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK04_IDLST JT-WK04_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WTNAF_IDLST WTNAF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
       END-EX.
           EXIT.
      **********************************************
      *    ÇgÇdÇcÅ|ÇqÇsÇm                          *
      **********************************************
       HED-RTN.
           MOVE   SPACE     TO  P-R.
           IF  LCNT      NOT     =    90
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           ADD    1         TO  PCNT.
           MOVE   WYY       TO  M1-01.
           MOVE   WMM       TO  M1-02.
           MOVE   WDD       TO  M1-03.
           MOVE   PCNT      TO  M1-04.
           MOVE   MID1-R    TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           MOVE   MID2-R    TO  P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           MOVE   MID3-R    TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           MOVE   MID4-R    TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           MOVE   MID5-R    TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           MOVE   R1-R      TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           MOVE   8        TO  LCNT.
       HED-EX.
           EXIT.
      ***************************************************
      *    ÇsÇrÇsÅ|ÇqÇsÇm                               *
      ***************************************************
       HIM-RTN.
           MOVE  WK04-03      TO  HI-MHCD HI-HCD.
      *           READ  HI2-M     UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ALL "Å@"    TO  HI-NAME
           END-IF
           MOVE  SPACE        TO  R1-R.
           MOVE  K-1          TO  R1-K1.
           MOVE  K-2          TO  R1-K2.
           MOVE  WK04-03      TO  R1-HCD.
           MOVE  HI-NAME      TO  R1-HNA.
       HIM-EX.
           EXIT.
      ***************************************************
      *    ÇlÇnÇuÅ|ÇqÇsÇm                               *
      ***************************************************
       MOV-RTN.
           IF  N  =    0
               MOVE    1  TO  N
           END-IF
           MOVE  SPACE     TO  R3-R(N).
       MOV-020.
           MOVE   WK04-22     TO     R3-00A(N).
           MOVE     "("       TO     R3-01(N).
           MOVE   WK04-022    TO     R3-02(N).
           MOVE     "/"       TO     R3-03(N).
           MOVE   WK04-023    TO     R3-04(N).
           MOVE     ")"       TO     R3-05(N).
           MOVE     ")"       TO     R3-C(N)  R3-F(N)  R3-H(N).
           MOVE     "("       TO     R3-A(N)  R3-D(N)  R3-G(N).
           MOVE     "/"       TO     R3-B(N).
           MOVE     "-"       TO     R3-E(N).
           MOVE   WK04-062    TO     R3-06(N).
           MOVE   WK04-063    TO     R3-07(N).
           MOVE   WK04-07     TO     R3-08(N).
           MOVE   WK04-08     TO     R3-09(N).
           MOVE   WK04-17     TO     R3-10(N).
       MOV-030.
           MOVE   WK04-09   TO  R3-16(N).
           MOVE   0       TO  I.
       MOV-040.
           ADD    1       TO  I.
           IF  I       >   10
               GO  TO  MOV-050
           END-IF
           IF  W-SEN   =     0
               COMPUTE W-ZAN = WK04-1111(I)  - WK04-141(I)
                                        - WK04-1211(I)
           ELSE
               COMPUTE W-ZAN = WK04-1111(I)  - WK04-141(I)
                                        - WK04-1211(I)  -  WK04-151(I)
           END-IF
           MOVE    W-ZAN       TO   R3-171(N , I).
           ADD     W-ZAN       TO         TOT.
           IF  WK04-09      =        1
               ADD   W-ZAN   TO  W-AAA(1,I)  W-BBB(1,I)  W-CCC(1,I)
               IF  SW1        <        1
                   MOVE       1        TO       SW1
                   IF  SW3        <        1
                       MOVE       1        TO       SW3
                       IF  SW2        <        1
                           MOVE       1        TO       SW2
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  WK04-09      =        2
               ADD   W-ZAN   TO  W-AAA(2,I)  W-BBB(2,I)  W-CCC(2,I)
               IF  SW1        <        2
                   MOVE       2        TO       SW1
                   IF  SW3        <        2
                       MOVE       2        TO       SW3
                       IF  SW2        <        2
                           MOVE       2        TO       SW2
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  WK04-09      =        3
               ADD   W-ZAN   TO  W-AAA(3,I)  W-BBB(3,I)  W-CCC(3,I)
               IF  SW1        <        3
                   MOVE       3        TO       SW1
                   IF  SW3        <        3
                       MOVE       3        TO       SW3
                       IF  SW2        <        3
                           MOVE       3        TO       SW2
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  WK04-09      =        4
               ADD   W-ZAN   TO  W-AAA(4,I)  W-BBB(4,I)  W-CCC(4,I)
               MOVE       4        TO       SW1   SW2  SW3
           END-IF
           IF  W-ZAN        =        0
               ADD     1     TO    0-CNT(N)
           END-IF
           MOVE    0     TO   W-ZAN.
           GO  TO  MOV-040.
       MOV-050.
           MOVE   WK04-03    TO  OLD-1.
           MOVE   WK04-91    TO  OLD-5.
           MOVE   WK04-04    TO  OLD-2.
           MOVE   WK04-07    TO  OLD-3.
           MOVE   WK04-10    TO  OLD-10.
           MOVE   WK04-23    TO  OLD-23.
           IF  0-CNT(N)   =    10
               MOVE    0  TO   0-CNT(N)
               MOVE   O-SW1   TO   SW1
               MOVE   O-SW2   TO   SW2
               MOVE   O-SW3   TO   SW3
               GO  TO  MOV-EX
           END-IF
           MOVE     1        TO   CNT.
           ADD      1        TO   N.
           MOVE     SW1   TO   O-SW1.
           MOVE     SW2   TO   O-SW2.
           MOVE     SW3   TO   O-SW3.
           MOVE     1        TO   W-TCNC.
       MOV-EX.
           EXIT.
      ***************************************************
      *    ÇsÇnÇsÅ[ÇqÇsÇm                               *
      ***************************************************
       TOT-RTN.
           IF  CNT      =       0
               GO  TO  TOT-EX
           END-IF
           IF  W-GC    =  1
               IF  W-DCC    NOT  =     9
                   MOVE     SPACE      TO    P-R
                   GO  TO  TOT-080
               END-IF
           END-IF
           IF  N        =       1
               GO  TO  TOT-EX
           END-IF
           COMPUTE   N  =  N   -   1.
           IF  TOT      =       ZERO
               IF  R3-R(N)   =   SPACE
                   MOVE    0    TO  N
                   GO  TO  TOT-EX
               END-IF
           END-IF
           IF  (CNT-M       =  1)  OR  (LCNT  =  90)
               MOVE  WK04-03   TO  NEW-1
               MOVE  OLD-1    TO  WK04-03
               PERFORM  HIM-RTN  THRU  HIM-EX
               PERFORM  HED-RTN  THRU  HED-EX
               MOVE  NEW-1    TO  WK04-03
           END-IF
           IF  (CNT-M1      =  1)  OR  (LCNT  =  8)
               MOVE  WK04-03   TO  NEW-1
               MOVE  OLD-1     TO  WK04-03
               MOVE  WK04-04   TO  NEW-2
               MOVE  OLD-2  TO  WK04-04
               PERFORM  MOV1-RTN  THRU  MOV1-EX
               MOVE  NEW-1    TO  WK04-03
               MOVE  NEW-2    TO  WK04-04
           END-IF
           MOVE   N    TO     W-NC.
           MOVE   0    TO     N.
       TOT-010.
           ADD    1  TO    N.
           IF  N  =  W-NC
               GO  TO  TOT-020
           END-IF
           IF  0-CNT(N)  =   10
               MOVE  0  TO  0-CNT(N)
               GO  TO  TOT-010
           END-IF
           IF  W-GC     =    0
               IF  LCNT    NOT <  59
                   PERFORM     HED-RTN  THRU  HED-EX
                   MOVE  WK04-04   TO  NEW-2
                   MOVE  OLD-2  TO  WK04-04
                   PERFORM  MOV1-RTN  THRU  MOV1-EX
                   MOVE  0        TO  W-PSW1   W-PSW2   W-PSW3
                   MOVE  NEW-2    TO  WK04-04
               END-IF
           END-IF
           IF  W-GC     NOT =    0
               IF  LCNT    NOT <  60
                   PERFORM     HED-RTN  THRU  HED-EX
                   MOVE  WK04-04   TO  NEW-2
                   MOVE  OLD-2  TO  WK04-04
                   PERFORM  MOV1-RTN  THRU  MOV1-EX
                   MOVE  0        TO  W-PSW1   W-PSW2   W-PSW3
                   MOVE  NEW-2    TO  WK04-04
               END-IF
           END-IF
           MOVE SPACE  TO  P-R.
           MOVE    K-2     TO    R3-K2(N).
           IF  W-GC    =  1
               IF  W-PSW1    =    0
                   MOVE    5                     TO  W-PSW1
                   MOVE    "Å@Å@Å@Å@ÅiåvÅjÅ@"  TO  R3-TD(N)
               END-IF
           END-IF
           IF  W-GC    =  3
               IF  W-PSW3    =    0
                   MOVE    5                     TO  W-PSW3
                   MOVE    OLD-5                 TO  R3-00(N)
                   MOVE    "Å@ÅmÅ@è¨åvÅ@ÅnÅ@"  TO  R3-TD(N)
               END-IF
           END-IF
           IF  W-GC    =  2
               IF  W-PSW2    =    0
                   MOVE    5                     TO  W-PSW2
                   MOVE    "ÅyÅ@çáÅ@åvÅ@ÅzÅ@"  TO  R3-TD(N)
               END-IF
           END-IF
           MOVE    R3-R(N)  TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE TO  P-R.
           ADD     1       TO  LCNT.
           MOVE    0       TO  0-CNT(N).
           IF  W-DCC   =   5
               MOVE    9         TO  W-DCC
           END-IF
           IF  W-DCC   =   0
               MOVE    5         TO  W-DCC
           END-IF
           GO  TO  TOT-010.
       TOT-020.
           IF  W-GC     =    0
               IF  LCNT    NOT <  59
                   PERFORM     HED-RTN  THRU  HED-EX
                   MOVE  WK04-04  TO  NEW-2
                   MOVE  OLD-2  TO  WK04-04
                   PERFORM  MOV1-RTN  THRU  MOV1-EX
                   MOVE  0        TO  W-PSW1  W-PSW2  W-PSW3
                   MOVE  NEW-2    TO  WK04-04
               END-IF
           END-IF
           IF  W-GC     NOT  =    0
               IF  LCNT    NOT <  60
                   PERFORM     HED-RTN  THRU  HED-EX
                   MOVE  WK04-04  TO  NEW-2
                   MOVE  OLD-2  TO  WK04-04
                   PERFORM  MOV1-RTN  THRU  MOV1-EX
                   MOVE  0        TO  W-PSW1 W-PSW2  W-PSW3
                   MOVE  NEW-2    TO  WK04-04
               END-IF
           END-IF
           MOVE       TOT     TO    R3-18(N).
           MOVE     SPACE      TO    P-R.
           MOVE    K-2     TO    R3-K2(N).
           IF  W-GC    =  1
               IF  W-PSW1    =    0
                   MOVE    5                     TO  W-PSW1
                   MOVE    "Å@Å@Å@Å@ÅiåvÅj"    TO  R3-TD(N)
               END-IF
           END-IF
           IF  W-GC    =  3
               IF  W-PSW3    =    0
                   MOVE    5                     TO  W-PSW3
                   MOVE    OLD-5                 TO  R3-00(N)
                   MOVE    "Å@ÅmÅ@è¨åvÅ@Ån"    TO  R3-TD(N)
               END-IF
           END-IF
           IF  W-GC    =  2
               IF  W-PSW2    =    0
                   MOVE    5                     TO  W-PSW2
                   MOVE    "ÅyÅ@çáÅ@åvÅ@Åz"    TO  R3-TD(N)
               END-IF
           END-IF
           MOVE     R3-R(N)    TO    P-R.
           IF  W-DCC   =   5
               MOVE    9         TO  W-DCC
           END-IF
           IF  W-DCC   =   0
               MOVE    5         TO  W-DCC
           END-IF.
       TOT-080.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE     SPACE      TO    P-R.
           MOVE     WK04-03    TO    NEW-1.
           MOVE     WK04-04    TO    NEW-2.
           MOVE     WK04-10    TO    NEW-10.
           MOVE     WK04-23    TO    NEW-23.
           ADD      1          TO    LCNT.
           MOVE     0          TO    0-CNT(N).
           MOVE     0          TO    TOT     N   CNT-M  CNT-M1.
           MOVE     0          TO    W-PSW1  W-PSW2  W-PSW3.
           IF  W-GC    =  1
               MOVE  0     TO       W-DCC
           END-IF.
       TOT-EX.
      ***************************************************
      *    ÇfÇoÇPÅ|ÇqÇsÇm                               *
      ***************************************************
       GP1-RTN.
           IF  CNT      =       0
               GO  TO  GP1-120
           END-IF
           MOVE  SPACE        TO     R3.
           MOVE     1      TO  J  N.
           PERFORM  MZA-RTN  THRU  MZA-EX.
           IF  ASW    =   1
               GO  TO  GP1-030
           END-IF
           MOVE   1       TO     R3-16(N).
           MOVE   0       TO     I.
       GP1-010.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP1-020
           END-IF
           MOVE   W-AAA(1 , I)  TO     R3-171(N , I).
           ADD    W-AAA(1 , I)  TO     TOT.
           GO  TO  GP1-010.
       GP1-020.
           IF  SW1       =       1
               MOVE     1      TO  W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
               GO  TO    GP1-120
           END-IF.
       GP1-030.
           MOVE     2      TO  J.
           PERFORM  MZA-RTN  THRU  MZA-EX.
           IF  ASW    =   1
               GO  TO  GP1-060
           END-IF
           MOVE   2       TO     R3-16(N).
           MOVE   0       TO     I.
       GP1-040.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP1-050
           END-IF
           MOVE   W-AAA(2 , I)  TO     R3-171(N , I).
           ADD    W-AAA(2 , I)  TO     TOT.
           GO  TO  GP1-040.
       GP1-050.
           IF  SW1       =       2
               MOVE     1      TO  W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
               GO  TO    GP1-120
           END-IF.
       GP1-060.
           MOVE     3      TO  J.
           PERFORM  MZA-RTN  THRU  MZA-EX.
           IF  ASW    =   1
               GO  TO  GP1-090
           END-IF
           MOVE   3       TO     R3-16(N).
           MOVE   0       TO     I.
       GP1-070.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP1-080
           END-IF
           MOVE   W-AAA(3 , I)  TO     R3-171(N , I).
           ADD    W-AAA(3 , I)  TO     TOT.
           GO  TO  GP1-070.
       GP1-080.
           IF  SW1       =       3
               MOVE     1      TO  W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
               GO  TO    GP1-120
           END-IF.
       GP1-090.
           MOVE     4      TO  J.
           PERFORM  MZA-RTN  THRU  MZA-EX.
           IF  ASW    =   1
               GO  TO  GP1-120
           END-IF
           MOVE   4       TO     R3-16(N).
           MOVE   0       TO     I.
       GP1-100.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP1-110
           END-IF
           MOVE   W-AAA(4 , I)  TO     R3-171(N , I).
           ADD    W-AAA(4 , I)  TO     TOT.
           GO  TO  GP1-100.
       GP1-110.
           IF  SW1       =       4
               MOVE     1      TO  W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
           END-IF.
       GP1-120.
           MOVE   ZERO      TO   W-A.
           MOVE   ZERO      TO   SW1    N    O-SW1  W-GC.
       GP1-EX.
           EXIT.
      ***************************************************
      *    ÇfÇoÇQÅ|ÇqÇsÇm                               *
      ***************************************************
       GP2-RTN.
           IF  CNT      =       0
               GO  TO  GP2-120
           END-IF
           MOVE  SPACE        TO     R3.
           MOVE     1      TO  J  N.
           PERFORM  MZB-RTN  THRU  MZB-EX.
           IF  ASW    =   1
               GO  TO  GP2-030
           END-IF
           MOVE   1       TO     R3-16(N).
           MOVE   0       TO     I.
       GP2-010.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP2-020
           END-IF
           MOVE   W-BBB(1 , I)  TO     R3-171(N , I).
           ADD    W-BBB(1 , I)  TO     TOT.
           GO  TO  GP2-010.
       GP2-020.
           IF  SW2       =       1
               MOVE     2      TO  W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
               GO  TO    GP2-120
           END-IF.
       GP2-030.
           MOVE     2      TO  J.
           PERFORM  MZB-RTN  THRU  MZB-EX.
           IF  ASW    =   1
               GO  TO  GP2-060
           END-IF
           MOVE   2       TO     R3-16(N).
           MOVE   0       TO     I.
       GP2-040.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP2-050
           END-IF
           MOVE   W-BBB(2 , I)  TO     R3-171(N , I).
           ADD    W-BBB(2 , I)  TO     TOT.
           GO  TO  GP2-040.
       GP2-050.
           IF  SW2       =       2
               MOVE     2      TO  W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
               GO  TO    GP2-120
           END-IF.
       GP2-060.
           MOVE     3      TO  J.
           PERFORM  MZB-RTN  THRU  MZB-EX.
           IF  ASW    =   1
               GO  TO  GP2-090
           END-IF
           MOVE   3       TO     R3-16(N).
           MOVE   0       TO     I.
       GP2-070.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP2-080
           END-IF
           MOVE   W-BBB(3 , I)  TO     R3-171(N , I).
           ADD    W-BBB(3 , I)  TO     TOT.
           GO  TO  GP2-070.
       GP2-080.
           IF  SW2       =       3
               MOVE     2      TO  W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
               GO  TO    GP2-120
           END-IF.
       GP2-090.
           MOVE     4      TO  J.
           PERFORM  MZB-RTN  THRU  MZB-EX.
           IF  ASW    =   1
               GO  TO  GP2-120
           END-IF
           MOVE   4       TO     R3-16(N).
           MOVE   0       TO     I.
       GP2-100.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP2-110
           END-IF
           MOVE   W-BBB(4 , I)  TO     R3-171(N , I).
           ADD    W-BBB(4 , I)  TO     TOT.
           GO  TO  GP2-100.
       GP2-110.
           IF  SW2       =       4
               MOVE     2      TO  W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
           END-IF.
       GP2-120.
           MOVE   ZERO      TO   W-B.
           MOVE   ZERO      TO   SW2    N    O-SW2  W-GC.
           MOVE   0         TO   W-DCC.
       GP2-EX.
           EXIT.
      ***************************************************
      *    ÇfÇoÇRÅ|ÇqÇsÇm                               *
      ***************************************************
       GP3-RTN.
           IF  CNT      =       0
               GO  TO  GP3-120
           END-IF
           MOVE  SPACE        TO     R3.
           MOVE     1      TO  J  N.
           PERFORM  MZC-RTN  THRU  MZC-EX.
           IF  ASW    =   1
               GO  TO  GP3-030
           END-IF
           MOVE   1       TO     R3-16(N).
           MOVE   0       TO     I.
       GP3-010.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP3-020
           END-IF
           MOVE   W-CCC(1 , I)  TO     R3-171(N , I).
           ADD    W-CCC(1 , I)  TO     TOT.
           GO  TO  GP3-010.
       GP3-020.
           IF  SW3       =       1
               MOVE     3      TO  W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
               GO  TO    GP3-120
           END-IF.
       GP3-030.
           MOVE     2      TO  J.
           PERFORM  MZC-RTN  THRU  MZC-EX.
           IF  ASW    =   1
               GO  TO  GP3-060
           END-IF
           MOVE   2       TO     R3-16(N).
           MOVE   0       TO     I.
       GP3-040.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP3-050
           END-IF
           MOVE   W-CCC(2 , I)  TO     R3-171(N , I).
           ADD    W-CCC(2 , I)  TO     TOT.
           GO  TO  GP3-040.
       GP3-050.
           IF  SW3       =       2
               MOVE     3      TO  W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
               GO  TO    GP3-120
           END-IF.
       GP3-060.
           MOVE     3      TO  J.
           PERFORM  MZC-RTN  THRU  MZC-EX.
           IF  ASW    =   1
               GO  TO  GP3-090
           END-IF
           MOVE   3       TO     R3-16(N).
           MOVE   0       TO     I.
       GP3-070.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP3-080
           END-IF
           MOVE   W-CCC(3 , I)  TO     R3-171(N , I).
           ADD    W-CCC(3 , I)  TO     TOT.
           GO  TO  GP3-070.
       GP3-080.
           IF  SW3       =       3
               MOVE     3      TO  W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
               GO  TO    GP3-120
           END-IF.
       GP3-090.
           MOVE     4      TO  J.
           PERFORM  MZC-RTN  THRU  MZC-EX.
           IF  ASW    =   1
               GO  TO  GP3-120
           END-IF
           MOVE   4       TO     R3-16(N).
           MOVE   0       TO     I.
       GP3-100.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP3-110
           END-IF
           MOVE   W-CCC(4 , I)  TO     R3-171(N , I).
           ADD    W-CCC(4 , I)  TO     TOT.
           GO  TO  GP3-100.
       GP3-110.
           IF  SW3       =       4
               MOVE     3      TO  W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
           END-IF.
       GP3-120.
           MOVE   ZERO      TO   W-C.
           MOVE   ZERO      TO   SW3    N    O-SW3  W-GC.
           MOVE   0         TO   W-DCC.
       GP3-EX.
           EXIT.
      *******************************
      *       ÇlÇnÇuÇPÅ|ÇqÇsÇm      *
      *******************************
       MOV1-RTN.
           IF  LCNT  =  90
               GO  TO  MOV1-EX
           END-IF
           IF  W-GC    NOT =  0  AND  1
               GO          TO       MOV1-EX
           END-IF
           IF  LCNT    NOT <  58
               PERFORM  HIM-RTN  THRU  HIM-EX
               PERFORM     HED-RTN  THRU  HED-EX
           END-IF
           MOVE   WK04-04     TO     TC-TCD.
           MOVE   "001"       TO     TC-CCD.
      *           READ   TC-M     WITH UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ALL "Å@"  TO  TC-NAME
           END-IF
           MOVE   SPACE    TO  R2-R.
           MOVE   K-1      TO  R2-K1.
           MOVE   K-2      TO  R2-K2.
           MOVE   TC-TCD   TO  R2-TCD.
           MOVE   TC-NAME  TO  R2-TNA.
           MOVE   R2-R     TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           ADD      1      TO  LCNT.
       MOV1-EX.
           EXIT.
      *******************************
      *       ÇlÇnÇuÇQÅ|ÇqÇsÇm      *
      *******************************
       MOV2-RTN.
           IF  W-TCNC      =  0
               GO          TO       MOV2-EX
           END-IF
           MOVE     0      TO  W-TCNC.
           IF  W-GC    NOT =  0 AND 1
               GO          TO       MOV2-EX
           END-IF
           IF  OLD-2       =  9850
               IF  OLD-23  NOT =  ZERO
                   GO          TO       MOV2-05
               END-IF
           END-IF
           IF  OLD-10      =  001
               GO          TO       MOV2-EX
           END-IF
           MOVE   OLD-2       TO     TC-TCD.
           MOVE   OLD-10      TO     TC-CCD.
      *           READ   TC-M     WITH UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ALL "Å@"  TO  TC-NAME
           END-IF
           MOVE   TC-NAME  TO  W-CNA.
           MOVE   SPACE    TO  R4-R.
           MOVE   K-1      TO  R4-K1.
           MOVE   K-2      TO  R4-K2.
           MOVE   OLD-10   TO  R4-CCD.
           GO  TO  MOV2-10.
       MOV2-05.
           MOVE   OLD-23      TO     WTNA-KEY.
      *           READ   WTNAF    WITH UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" WTNAF_PNAME1 BY REFERENCE WTNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ALL "Å@"  TO  WTNA-NAME
           END-IF
           MOVE   WTNA-NAME TO  W-CNA.
           MOVE   SPACE    TO  R4-R.
           MOVE   K-1      TO  R4-K1.
           MOVE   K-2      TO  R4-K2.
           MOVE   OLD-23   TO  R4-TEN.
       MOV2-10.
           MOVE   W-CNA    TO  R4-CNA.
           MOVE   R4-R     TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           ADD      1      TO  LCNT.
       MOV2-EX.
           EXIT.
      *****************************
      *      ÇlÇyÇ`Å@Å|ÇqÇsÇm     Åñ
      *****************************
       MZA-RTN.
           MOVE    0    TO    I    ASW.
       MZA-001.
           ADD     1    TO    I.
           IF  I    >    10
               MOVE    1   TO   ASW
               GO  TO  MZA-EX
           END-IF
           IF  W-AAA(J , I)  NOT   =    0
               GO  TO  MZA-EX
           END-IF
           GO  TO  MZA-001.
       MZA-EX.
           EXIT.
      *****************************
      *      ÇlÇyÇaÅ@Å|ÇqÇsÇm     Åñ
      *****************************
       MZB-RTN.
           MOVE    0    TO    I    ASW.
       MZB-001.
           ADD     1    TO    I.
           IF  I    >    10
               MOVE    1   TO   ASW
               GO  TO  MZB-EX
           END-IF
           IF  W-BBB(J , I)  NOT   =    0
               GO  TO  MZB-EX
           END-IF
           GO  TO  MZB-001.
       MZB-EX.
           EXIT.
      *****************************
      *      ÇlÇyÇbÅ@Å|ÇqÇsÇm     Åñ
      *****************************
       MZC-RTN.
           MOVE    0    TO    I    ASW.
       MZC-001.
           ADD     1    TO    I.
           IF  I    >    10
               MOVE    1   TO   ASW
               GO  TO  MZC-EX
           END-IF
           IF  W-CCC(J , I)  NOT   =    0
               GO  TO  MZC-EX
           END-IF
           GO  TO  MZC-001.
       MZC-EX.
           EXIT.
      *
       COPY    LPMSG.

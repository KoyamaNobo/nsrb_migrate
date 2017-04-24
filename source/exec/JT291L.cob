       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT291L.
       AUTHOR.                        E-SHIGIHARA.
      ***************************************************
      *    PROGRAM        : ïiñºï éÛíçécí†ÅiâûópópéÜÅj  *
      *    DATA WRITTEN   : 87/08/28                    *
      *    SCREEN USED    : UNUSED                      *
      *    FORM   USED    : UNUSED                      *
      *    PRINTER TYPE   : JIPS                        *
      *    COMPILE TYPE   : COBOL                       *
      ***************************************************
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM150.
       OBJECT-COMPUTER.               SYSTEM150.
       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77  ERR-STAT                  PIC X(02)    VALUE SPACE.
       77  JS-CHK                    PIC 9(01).
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  P1-R.
           02  P1-K1                 PIC X(05).
           02  P1-HCD                PIC 9(06).
           02  F                     PIC X(01).
           02  P1-HNA                PIC N(24).
           02  F                     PIC X(93).
           02  P1-K2                 PIC X(05).
       01  P2-R.
           02  P2-K1                 PIC X(05).
           02  F                     PIC X(03).
           02  P2-TCD                PIC 9(04).
           02  F                     PIC X(01).
           02  P2-TNA                PIC N(26).
           02  F                     PIC X(89).
           02  P2-K2                 PIC X(05).
       01  P3.
           02  P3-R        OCCURS  4.
               03  P3-K2             PIC X(5).
               03  FILLER            PIC X(07).
      ***
               03  P3-GF             PIC X(01).
               03  P3-GGET           PIC ZZ.
               03  P3-GV             PIC X(01).
               03  P3-GPEY           PIC ZZ.
               03  P3-GR             PIC X(01).
               03  P3-SF             PIC X(01).
               03  P3-SGET           PIC ZZ.
               03  P3-SV             PIC X(01).
               03  P3-SPEY           PIC ZZ.
               03  P3-SR             PIC X(01).
               03  P3-NF             PIC X(01).
               03  P3-TD             PIC N(08).
               03  P3-MD      REDEFINES  P3-TD.
                   04  P3-DNO        PIC 9(06).
                   04  P3-DV         PIC X(01).
                   04  P3-GNO        PIC 9(01).
                   04  P3-DR         PIC X(01).
                   04  P3-TF         PIC X(01).
                   04  P3-TAN        PIC ZZ,ZZZ.
               03  P3-TR             PIC X(01).
               03  F                 PIC X(01).
               03  P3-SIZ            PIC 9(01).
               03  P3-SUD       OCCURS   10.
                   04  P3-SU         PIC -------.
               03  P3-SUT            PIC -----,--9.
               03  F                 PIC X(01).
               03  P3-K1             PIC X(5).
               03  P3-TEK1           PIC N(06).
               03  P3-TEK2           PIC N(02).
               03  P3-K21            PIC X(5).
       01  P4-R.
           02  P4-K1                 PIC X(05).
           02  F                     PIC X(07).
           02  P4-TEN                PIC 9(04).
           02  P4-TEND  REDEFINES P4-TEN.
               03  F                 PIC X(01).
               03  P4-CCD            PIC 9(03).
           02  F                     PIC X(01).
           02  P4-CNA                PIC N(26).
           02  F                     PIC X(85).
           02  P4-K2                 PIC X(05).
       01  MID1-R.
           02  F                     PIC X(05)    VALUE  X"1A24212474".
           02  FILLER                PIC X(55)    VALUE   SPACE.
           02  M2-MDS                PIC N(07).
           02  M2-00                 PIC X(24).
           02  F                     PIC X(10)    VALUE SPACE.
           02  FILLER                PIC X(05)    VALUE   "DATE ".
           02  M2-01                 PIC 9(02).
           02  FILLER                PIC X(01)    VALUE   ".".
           02  M2-02                 PIC Z9.
           02  FILLER                PIC X(01)    VALUE   ".".
           02  M2-03                 PIC Z9.
           02  FILLER                PIC X(17)    VALUE   SPACE.
           02  M2-04                 PIC ZZ9.
       01  MID2-R.
           02  F                     PIC X(05)    VALUE  X"1A24212078".
           02  FILLER                PIC X(07)    VALUE " ∫∞ƒﬁ  ".
           02  FILLER                PIC N(08)    VALUE
                 "ïiÅ@Å@Å@Å@Å@ñºÅ@".
           02  FILLER                PIC X(21)    VALUE   SPACE.
           02  FILLER                PIC X(01)    VALUE "1".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC N(02)  VALUE  "ÇRçÜ".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC N(02)  VALUE  "ÇQçÜ".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC N(02)  VALUE  "ÇPçÜ".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC N(02)  VALUE  "ÇOçÜ".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC N(02)  VALUE  "Å@íÜ".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC N(02)  VALUE  "Å@ëÂ".
           02  FILLER                PIC X(04)    VALUE   SPACE.
           02  FILLER                PIC N(02)  VALUE  "ì¡ëÂ".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "28.0".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "29.0".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "30.0".
           02  FILLER                PIC X(25)    VALUE   SPACE.
           02  F                     PIC X(05)  VALUE  X"1A24212474".
       01  MID3-R.
           02  F                     PIC X(05)  VALUE  X"1A24212078".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(05)    VALUE "∫∞ƒﬁ ".
           02  FILLER                PIC N(10)    VALUE
                 "ìæÅ@Å@à”Å@Å@êÊÅ@Å@ñº".
           02  FILLER                PIC X(17)    VALUE   SPACE.
           02  FILLER                PIC X(01)    VALUE "2".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "12.5".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "13.0".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "13.5".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "14.0".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "15.0".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "16.0".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "17.0".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "18.0".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "19.0".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "20.0".
           02  FILLER                PIC X(25)    VALUE   SPACE.
           02  F                     PIC X(05)  VALUE  X"1A24212474".
       01  MID4-R.
           02  F                     PIC X(05)  VALUE  X"1A24212078".
           02  FILLER                PIC X(07)    VALUE   SPACE.
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
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "21.0".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "21.5".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "22.0".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "22.5".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "23.0".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "23.5".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "24.0".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "24.5".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "25.0".
           02  FILLER                PIC X(32)    VALUE   SPACE.
           02  F                     PIC X(05)  VALUE  X"1A24212474".
       01  MID5-R.
           02  F                     PIC X(05)  VALUE  X"1A24212078".
           02  FILLER                PIC X(08)    VALUE   SPACE.
           02  FILLER                PIC X(04)    VALUE "∫∞ƒﬁ".
           02  FILLER                PIC N(12)    VALUE
                 "íºÅ@ëóÅ@êÊÅ@ÅEÅ@ìXÅ@ñºÅ@".
           02  FILLER                PIC X(10)    VALUE   SPACE.
           02  FILLER                PIC X(01)    VALUE "4".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "24.0".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "24.5".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "25.0".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "25.5".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "26.0".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "26.5".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "27.0".
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  FILLER                PIC X(04)  VALUE    "27.5".
           02  FILLER                PIC X(17)    VALUE   SPACE.
           02  FILLER                PIC N(04)  VALUE  "Å@çáÅ@åv".
           02  FILLER                PIC X(01)    VALUE   SPACE.
           02  FILLER                PIC N(02)  VALUE  "ìEóv".
           02  FILLER                PIC X(12)    VALUE   SPACE.
           02  F                     PIC X(05)  VALUE  X"1A24212474".
       01  MW-R1.
           02  FILLER                PIC X(06)  VALUE  SPACE.
           02  MW-SEN1               PIC N(05).
           02  FILLER                PIC X(08)    VALUE   SPACE.
       01  MW-P3.
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  MW-SEN2               PIC N(05).
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(01)  VALUE "(".
           02  MW-MDS                PIC N(03).
           02  F                     PIC X(01)  VALUE ")".
       01  ACT-WORK.
           02  W-KBN                 PIC 9(01).
           02  W-FROM                PIC 9(06).
           02  W-TO                  PIC 9(06).
           02  W-OK                  PIC 9(01).
           02  I                     PIC 9(02).
           02  TOT                   PIC S9(07).
           02  OLD-1                 PIC 9(06).
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
           02  ASW                   PIC  9(01).
           02  J                     PIC  9(01).
           02  W-GC                  PIC  9(01).
           02  W-PSW1                PIC  9(01).
           02  W-PSW2                PIC  9(01).
           02  W-DCC                 PIC  9(01).
           02  W-TCNC                PIC  9(01).
           02  W-TEK1                PIC  N(06).
           02  W-TEK2                PIC  N(02).
           02  W-CNA                 PIC  N(26).
       01  W-A.
           02  W-AA        OCCURS         4.
               03  W-AAA             PIC  S9(06)  OCCURS 10.
       01  W-B.
           02  W-BB        OCCURS         4.
               03  W-BBB             PIC  S9(06)  OCCURS 10.
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
           COPY  LTWK04.
           COPY  LITCM.
           COPY  LIHIM2.
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
       01  DSP-AREA1.
           02  DSP-00.
               03  FILLER  PIC  X(25) VALUE "                         ".
               03  FILLER  PIC  X(02) VALUE  "ïi".
               03  FILLER  PIC  X(02) VALUE  "ñº".
               03  FILLER  PIC  X(02) VALUE  "ï ".
               03  FILLER  PIC  X(02) VALUE  "éÛ".
               03  FILLER  PIC  X(02) VALUE  "íç".
               03  FILLER  PIC  X(02) VALUE  "éc".
               03  FILLER  PIC  X(02) VALUE  "í†".
               03  FILLER  PIC  X(02) VALUE  "ëº".
               03  FILLER  PIC  X(08) VALUE  "ÅiîíéÜÅj".
           02  DSP-01.
               03  FILLER  PIC  X(25) VALUE "                         ".
               03  FILLER  PIC  X(02) VALUE  "ïi".
               03  FILLER  PIC  X(02) VALUE  "ñº".
               03  FILLER  PIC  X(02) VALUE  "ï ".
               03  FILLER  PIC  X(02) VALUE  "éÛ".
               03  FILLER  PIC  X(02) VALUE  "íç".
               03  FILLER  PIC  X(02) VALUE  "êî".
               03  FILLER  PIC  X(02) VALUE  "í†".
               03  FILLER  PIC  X(02) VALUE  "ëº".
               03  FILLER  PIC  X(08) VALUE  "ÅiîíéÜÅj".
       COPY    LSMSG.
      *
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "P1-999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLE-01
       CALL "SD_Init" USING
           "CLE-01" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLE-01" "X" "1" "0" "12" " " "CLE-01" RETURNING RESU.
      *DSP-AREA1
       CALL "SD_Init" USING 
            "DSP-AREA1" " " "0" "0" "98" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-00" " " "1" "0" "49" " " "DSP-AREA1"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-00" "RX" "1" "23" "25" " " "DSP-00" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-00" "X" "1" "24" "2" "01DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-00" "X" "1" "27" "2" "02DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-00" "X" "1" "30" "2" "03DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-00" "X" "1" "33" "2" "04DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-00" "X" "1" "36" "2" "05DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-00" "X" "1" "39" "2" "06DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-00" "X" "1" "42" "2" "07DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-00" "X" "1" "45" "2" "08DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-00" "X" "1" "49" "8" "09DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "49" "DSP-00" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "RX" "1" "23" "25" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-01" "X" "1" "24" "2" "01DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-01" "X" "1" "27" "2" "02DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-01" "X" "1" "30" "2" "03DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-01" "X" "1" "33" "2" "04DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-01" "X" "1" "36" "2" "05DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-01" "X" "1" "39" "2" "06DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-01" "X" "1" "42" "2" "07DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-01" "X" "1" "45" "2" "08DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-01" "X" "1" "49" "8" "09DSP-01" " " RETURNING RESU.
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
       MR000.
      *           READ      JT-WK04           AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-WK04_PNAME1 BY REFERENCE WK04-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "ERR-02" ERR-02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               GO  TO  MR999
           END-IF
           MOVE  WK04-89    TO  W-KBN.
           MOVE  WK04-90    TO  W-SEN.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK04_IDLST JT-WK04_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-WK04_PNAME1 " " BY REFERENCE JT-WK04_IDLST "0".
           IF  W-SEN      =   0
               IF  JS-CHK      =   0   OR   2
                   MOVE  "Åyã≥Å@àÁÅz"   TO  MW-SEN1
               ELSE
                   MOVE  "Åyã≥Å@àÁÅz"   TO  MW-SEN2
               END-IF
           END-IF
           IF  W-SEN      =   1
               IF  JS-CHK      =   0   OR   2
                   MOVE  "ÅyàÍÅ@î Åz"   TO  MW-SEN1
               ELSE
                   MOVE  "ÅyàÍÅ@î Åz"   TO  MW-SEN2
               END-IF
           END-IF
           IF  W-SEN      =   9
               IF  JS-CHK      =   0   OR   2
                   MOVE  ALL  "Å@"    TO  MW-SEN1
               ELSE
                   MOVE  ALL  "Å@"    TO  MW-SEN2
               END-IF
           END-IF
           IF  JS-CHK     =   1
               MOVE  "éwê}éc"   TO  MW-MDS
           END-IF
           IF  W-KBN      =   0
               IF  JS-CHK     =   2
                   MOVE  "éÛÅ@íçÅ@êîÅ@í†"   TO  M2-MDS
               ELSE
                   MOVE  "éÛÅ@íçÅ@écÅ@í†"   TO  M2-MDS
               END-IF
           END-IF
           IF  W-KBN      =   5
               IF  JS-CHK     =   2
                   MOVE  "óaÅ@ÇËÅ@êîÅ@í†"   TO  M2-MDS
               ELSE
                   MOVE  "óaÅ@ÇËÅ@écÅ@í†"   TO  M2-MDS
               END-IF
           END-IF
           IF  W-KBN      =   6
               IF  JS-CHK     =   2
                   MOVE  "Å@éÊÇÊÇØêîí†Å@"   TO  M2-MDS
               ELSE
                   MOVE  "Å@éÊÇÊÇØécí†Å@"   TO  M2-MDS
               END-IF
           END-IF.
       MR050.
           MOVE      ZERO  TO   W-GC.
      *           READ      JT-WK04           AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-WK04_PNAME1 BY REFERENCE WK04-R " "
            RETURNING RET.
           IF  RET = 1
               PERFORM   MOV2-RTN  THRU   MOV2-EX
               PERFORM   GP1-RTN   THRU   GP1-EX
               PERFORM   GP2-RTN   THRU   GP2-EX
               GO   TO   MR999
           END-IF
           IF  LCNT  =   90
               GO  TO  MR060
           END-IF
           IF  (WK04-03       NOT = OLD-1) OR (CNT = 0)
               PERFORM   MOV2-RTN  THRU   MOV2-EX
               PERFORM   GP1-RTN   THRU   GP1-EX
               MOVE  1   TO   CNT-M1
               PERFORM     GP2-RTN     THRU     GP2-EX
               MOVE  1   TO   CNT-M
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
           ACCEPT JS-CHK FROM ARGUMENT-VALUE.
           IF  JS-CHK > 2
               GO TO INT-RTN
           END-IF
           INITIALIZE     ACT-WORK    P3.
           MOVE     ZERO    TO   W-A   W-B.
           ACCEPT         WYMD        FROM     DATE.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           IF  JS-CHK      NOT  =   2
               CALL "SD_Output" USING
                "DSP-00" DSP-00 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "DSP-01" DSP-01 "p" RETURNING RESU
           END-IF
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
           IF  JS-CHK     =   1
               MOVE  MW-P3            TO  M2-00
           ELSE
               MOVE  MW-R1            TO  M2-00
           END-IF
           MOVE   WYY       TO  M2-01.
           MOVE   WMM       TO  M2-02.
           MOVE   WDD       TO  M2-03.
           MOVE   PCNT      TO  M2-04.
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
           MOVE   P1-R      TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           MOVE   8        TO  LCNT.
       HED-EX.
           EXIT.
       HIM-RTN.
           MOVE   WK04-03   TO    HI-MHCD HI-HCD.
      *           READ   HI2-M  UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ALL "Å@"  TO    HI-NAME
           END-IF
           MOVE   SPACE     TO     P1-R.
           MOVE   K-1       TO    P1-K1.
           MOVE   K-2       TO    P1-K2.
           MOVE   WK04-03   TO    P1-HCD.
           MOVE   HI-NAME   TO    P1-HNA.
       HIM-EX.
           EXIT.
      ***************************************************
      *    ÇlÇnÇuÅ|ÇqÇsÇm                               *
      ***************************************************
       MOV-RTN.
           IF  N  =    0
               MOVE    1  TO  N
           END-IF
           MOVE  SPACE     TO  P3-R(N).
           MOVE SPACE  TO  P3-TEK1(N) P3-TEK2(N).
       MOV-020.
           MOVE     "("       TO     P3-GF(N).
           MOVE   WK04-022    TO     P3-GGET(N).
           MOVE     "/"       TO     P3-GV(N).
           MOVE   WK04-023    TO     P3-GPEY(N).
           MOVE     ")"       TO     P3-GR(N).
           MOVE     ")"       TO     P3-SR(N)  P3-DR(N)  P3-TR(N).
           MOVE     "("       TO     P3-SF(N)  P3-NF(N)  P3-TF(N).
           MOVE     "/"       TO     P3-SV(N).
           MOVE     "-"       TO     P3-DV(N).
           MOVE   WK04-062    TO     P3-SGET(N).
           MOVE   WK04-063    TO     P3-SPEY(N).
           MOVE   WK04-07     TO     P3-DNO(N).
           MOVE   WK04-08     TO     P3-GNO(N).
           MOVE   WK04-17     TO     P3-TAN(N).
           MOVE   WK04-801    TO     P3-TEK1(N).
           MOVE   WK04-803    TO     P3-TEK2(N).
       MOV-030.
           MOVE   WK04-09   TO  P3-SIZ(N).
           MOVE   0       TO  I.
       MOV-040.
           ADD    1       TO  I.
           IF  I       >   10
               GO  TO  MOV-050
           END-IF
           IF  JS-CHK      =   0
               COMPUTE W-ZAN = WK04-1111(I) - WK04-141(I)
                                            - WK04-1211(I)
           END-IF
           IF  JS-CHK      =   1
               COMPUTE W-ZAN = WK04-1111(I) - WK04-141(I)
                             - WK04-1211(I) - WK04-151(I)
           END-IF
           IF  JS-CHK      =   2
               MOVE    WK04-1111(I)     TO  W-ZAN
           END-IF
           MOVE    W-ZAN       TO   P3-SU(N , I).
           ADD     W-ZAN       TO         TOT.
           IF  WK04-09      =        1
               ADD   W-ZAN   TO    W-AAA(1 , I)   W-BBB(1 , I)
               IF  SW1        <        1
                   MOVE       1        TO       SW1
                   IF  SW2        <        1
                       MOVE       1        TO       SW2
                   END-IF
               END-IF
           END-IF
           IF  WK04-09      =        2
               ADD   W-ZAN   TO    W-AAA(2 , I)   W-BBB(2 , I)
               IF  SW1        <        2
                   MOVE       2        TO       SW1
                   IF  SW2        <        2
                       MOVE       2        TO       SW2
                   END-IF
               END-IF
           END-IF
           IF  WK04-09      =        3
               ADD   W-ZAN   TO    W-AAA(3 , I)   W-BBB(3 , I)
               IF  SW1        <        3
                   MOVE       3        TO       SW1
                   IF  SW2        <        3
                       MOVE       3        TO       SW2
                   END-IF
               END-IF
           END-IF
           IF  WK04-09      =        4
               ADD   W-ZAN   TO    W-AAA(4 , I)   W-BBB(4 , I)
               MOVE       4        TO       SW1   SW2
           END-IF
           IF  W-ZAN        =        0
               ADD     1     TO    0-CNT(N)
           END-IF
           MOVE    0     TO   W-ZAN.
           GO  TO  MOV-040.
       MOV-050.
           MOVE   WK04-03    TO  OLD-1.
           MOVE   WK04-04    TO  OLD-2.
           MOVE   WK04-07    TO  OLD-3.
           MOVE   WK04-10    TO  OLD-10.
           MOVE   WK04-23    TO  OLD-23.
           IF  0-CNT(N)   =    10
               MOVE    0  TO   0-CNT(N)
               MOVE   O-SW1   TO   SW1
               MOVE   O-SW2   TO   SW2
               GO  TO  MOV-EX
           END-IF
           MOVE     1        TO   CNT.
           ADD      1        TO   N.
           MOVE     SW1   TO   O-SW1.
           MOVE     SW2   TO   O-SW2.
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
               IF  P3-R(N)   =   SPACE
                   MOVE    0    TO  N
                   GO  TO  TOT-EX
               END-IF
           END-IF
           IF  (CNT-M       =  1)  OR  (LCNT  =  90)
               MOVE  WK04-03   TO  NEW-1
               MOVE  OLD-1  TO  WK04-03
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
               IF  LCNT    NOT <  60
                   PERFORM     HED-RTN  THRU  HED-EX
                   MOVE  WK04-04   TO  NEW-2
                   MOVE  OLD-2  TO  WK04-04
                   PERFORM  MOV1-RTN  THRU  MOV1-EX
                   MOVE  0        TO  W-PSW1   W-PSW2
                   MOVE  NEW-2    TO  WK04-04
               END-IF
           END-IF
           IF  W-GC     NOT =    0
               IF  LCNT    NOT <  61
                   PERFORM     HED-RTN  THRU  HED-EX
                   MOVE  WK04-04  TO  NEW-2
                   MOVE  OLD-2  TO  WK04-04
                   PERFORM  MOV1-RTN  THRU  MOV1-EX
                   MOVE  0        TO  W-PSW1   W-PSW2
                   MOVE  NEW-2    TO  WK04-04
               END-IF
           END-IF
           MOVE SPACE  TO  P-R.
           MOVE    K-1     TO    P3-K1(N).
           MOVE    K-2     TO    P3-K2(N).
           MOVE    K-2     TO    P3-K21(N).
           IF  W-GC    =  1
               IF  W-PSW1    =    0
                   MOVE    5                     TO  W-PSW1
                   MOVE    "Å@è¨Å@Å@Å@Å@åv"    TO  P3-TD(N)
               END-IF
           END-IF
           IF  W-GC    =  2
               IF  W-PSW2    =    0
                   MOVE    5                     TO  W-PSW2
                   MOVE    "çáÅ@Å@Å@Å@åvÅ@"    TO  P3-TD(N)
               END-IF
           END-IF
           MOVE    P3-R(N)  TO  P-R.
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
               IF  LCNT    NOT <  60
                   PERFORM     HED-RTN  THRU  HED-EX
                   MOVE  WK04-04  TO  NEW-2
                   MOVE  OLD-2  TO  WK04-04
                   PERFORM  MOV1-RTN  THRU  MOV1-EX
                   MOVE  0        TO  W-PSW1  W-PSW2
                   MOVE  NEW-2    TO  WK04-04
               END-IF
           END-IF
           IF  W-GC     NOT  =    0
               IF  LCNT    NOT <  61
                   PERFORM     HED-RTN  THRU  HED-EX
                   MOVE  WK04-04  TO  NEW-2
                   MOVE  OLD-2  TO  WK04-04
                   PERFORM  MOV1-RTN  THRU  MOV1-EX
                   MOVE  0        TO  W-PSW1 W-PSW2
                   MOVE  NEW-2    TO  WK04-04
               END-IF
           END-IF
           MOVE       TOT     TO    P3-SUT(N).
           MOVE     SPACE      TO    P-R.
           MOVE    K-2     TO    P3-K2(N).
           MOVE    K-1     TO    P3-K1(N).
           MOVE    K-2     TO    P3-K21(N).
           IF  W-GC    =  1
               IF  W-PSW1    =    0
                   MOVE    5                     TO  W-PSW1
                   MOVE    "Å@è¨Å@Å@Å@Å@åv"    TO  P3-TD(N)
               END-IF
           END-IF
           IF  W-GC    =  2
               IF  W-PSW2    =    0
                   MOVE    5                     TO  W-PSW2
                   MOVE    "çáÅ@Å@Å@Å@åvÅ@"    TO  P3-TD(N)
               END-IF
           END-IF
           MOVE     P3-R(N)    TO    P-R.
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
           MOVE     0          TO    W-PSW1  W-PSW2.
           IF  W-GC    =  1
               MOVE  0     TO       W-DCC
           END-IF.
       TOT-EX.
      ***************************************************
      *    ÇfÇoÇPÅ|ÇqÇsÇm                               *
      ***************************************************
       GP1-RTN.
           GO  TO  GP1-120.
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
           MOVE  SPACE        TO     P3.
           MOVE     1      TO  J  N.
           PERFORM  MZB-RTN  THRU  MZB-EX.
           IF  ASW    =   1
               GO  TO  GP2-030
           END-IF
           MOVE   1       TO     P3-SIZ(N).
           MOVE   0       TO     I.
       GP2-010.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP2-020
           END-IF
           MOVE   W-BBB(1 , I)  TO     P3-SU(N , I).
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
           MOVE   2       TO     P3-SIZ(N).
           MOVE   0       TO     I.
       GP2-040.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP2-050
           END-IF
           MOVE   W-BBB(2 , I)  TO     P3-SU(N , I).
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
           MOVE   3       TO     P3-SIZ(N).
           MOVE   0       TO     I.
       GP2-070.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP2-080
           END-IF
           MOVE   W-BBB(3 , I)  TO     P3-SU(N , I).
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
           MOVE   4       TO     P3-SIZ(N).
           MOVE   0       TO     I.
       GP2-100.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP2-110
           END-IF
           MOVE   W-BBB(4 , I)  TO     P3-SU(N , I).
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
           IF  LCNT    NOT <  59
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
           MOVE   SPACE     TO     P2-R.
           MOVE   K-1       TO    P2-K1.
           MOVE   K-2       TO    P2-K2.
           MOVE   TC-TCD   TO  P2-TCD.
           MOVE   TC-NAME  TO  P2-TNA.
           MOVE   P2-R     TO  P-R.
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
           MOVE   SPACE     TO     P4-R.
           MOVE   K-1       TO    P4-K1.
           MOVE   K-2       TO    P4-K2.
           MOVE   OLD-10   TO  P4-CCD.
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
           MOVE   SPACE     TO     P4-R.
           MOVE   K-1       TO    P4-K1.
           MOVE   K-2       TO    P4-K2.
           MOVE   OLD-23   TO  P4-TEN.
       MOV2-10.
           MOVE   W-CNA    TO  P4-CNA.
           MOVE   P4-R     TO  P-R.
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
       COPY    LPMSG.

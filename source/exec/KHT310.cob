       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHT310.
      *********************************************************
      *    PROGRAM         :  çHïiâ¡ó∞ÅEîpãpì`ï[Å@ñ‚çáÇπ    Å@*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCKT31                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-MSG              PIC  X(040).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-DATE         PIC  9(006).
           02  W-SDATE        PIC  9(006).
           02  W-EDATE        PIC  9(006).
           02  W-SNGP.
             03  W-SNEN       PIC  9(002).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-SNGPD REDEFINES W-SNGP.
             03  W-SNG        PIC  9(004).
             03  F            PIC  9(002).
           02  W-ENGP.
             03  W-ENEN       PIC  9(002).
             03  W-EGET       PIC  9(002).
             03  W-EPEY       PIC  9(002).
           02  W-ENGPD REDEFINES W-ENGP.
             03  W-ENG        PIC  9(004).
             03  F            PIC  9(002).
           02  W-NG.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-HCD          PIC  X(005).
           02  W-L            PIC  9(002).
           02  CNT            PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-INV          PIC  9(001).
           02  W-DCN          PIC  N(002).
           02  W-NRN          PIC  N(002).
           02  W-HNA          PIC  N(024).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKHM.
      *FD  KNH-F
       01  KNH-F_KHT310.
           02  KNH-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  KNH-F_LNAME    PIC  X(012) VALUE "KNH-F_KHT310".
           02  F              PIC  X(001).
           02  KNH-F_KEY1     PIC  X(100) VALUE SPACE.
           02  KNH-F_SORT     PIC  X(100) VALUE SPACE.
           02  KNH-F_IDLST    PIC  X(100) VALUE SPACE.
           02  KNH-F_RES      USAGE  POINTER.
       01  KNH-R.
           02  KNH-NHC        PIC  9(002).
           02  KNH-NGP        PIC  9(008).
           02  KNH-NGPD  REDEFINES KNH-NGP.
             03  F            PIC  9(002).
             03  KNH-NGPS     PIC  9(006).
           02  KNH-HCD        PIC  X(005).
           02  F              PIC  X(007).
           02  KNH-SU         PIC S9(006)V9(02).
           02  KNH-T          PIC S9(006)V9(02).
           02  KNH-KIN        PIC S9(008).
           02  F              PIC  X(016).
           02  KNH-NRC        PIC  9(001).
           02  F              PIC  X(001).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  FILLER.
             03  A-SNGP  PIC  9(006).
             03  A-ENGP  PIC  9(006).
           02  FILLER.
             03  A-HCD   PIC  X(005).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-HNA   PIC  N(024).
           02  D-MD.
             03  D-NGPS  PIC  9(006).
             03  D-DCN   PIC  N(002).
             03  D-SU    PIC ----,--9.99 .
             03  D-T     PIC ----,--9.99 .
             03  D-KIN   PIC ---,---,--9 .
             03  D-NRM   PIC  N(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(040).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "18" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "1" "0" "12" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Init" USING
            "A-SNGP" "9" "1" "57" "6" " " "01C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING
            "A-SNGP" BY REFERENCE W-SNGP "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ENGP" "9" "1" "65" "6" "A-SNGP" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-ENGP" BY REFERENCE W-ENGP "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-ACP" " " "2" "0" "5" "01C-ACP" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "A-HCD" "X" "2" "17" "5" " " "02C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING
            "A-HCD" BY REFERENCE W-HCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "23" "70" "1" "02C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "95" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "D-HNA" "N" "2" "23" "48" " " "C-DSP"  RETURNING RESU.
       CALL "SD_From" USING
            "D-HNA" BY REFERENCE W-HNA "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-MD" " " "W-L" "0" "47" "D-HNA" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "D-NGPS" "9" "W-L" "11" "6" " " "D-MD"  RETURNING RESU.
       CALL "SD_From" USING
            "D-NGPS" BY REFERENCE KNH-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-DCN" "N" "W-L" "18" "4" "D-NGPS" " "  RETURNING RESU.
       CALL "SD_From" USING
            "D-DCN" BY REFERENCE W-DCN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-SU" "----,--9.99" "W-L" "23" "11" "D-DCN" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "D-SU" BY REFERENCE KNH-SU "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-T" "----,--9.99" "W-L" "35" "11" "D-SU" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "D-T" BY REFERENCE KNH-T "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-KIN" "---,---,--9" "W-L" "48" "11" "D-T" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "D-KIN" BY REFERENCE KNH-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-NRM" "N" "W-L" "60" "4" "D-KIN" " "  RETURNING RESU.
       CALL "SD_From" USING
            "D-NRM" BY REFERENCE W-NRN "4" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "100" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "100" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME" "X" "24" "15" "40" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_From" USING
            "E-ME" BY REFERENCE W-MSG "40" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-CL" "X" "24" "10" "50" "E-ME99" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCKT31" RETURNING RESU.
      *    MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-NKNG TO W-SNG W-ENG.
           MOVE 31 TO W-EPEY.
           MOVE 1 TO W-SPEY.
           ADD 1 TO W-SGET.
           IF  W-SGET = 13
               MOVE 1 TO W-SGET
           ELSE
               SUBTRACT 1 FROM W-SNEN
           END-IF
           MOVE W-SNGP TO W-SDATE.
           MOVE W-ENGP TO W-EDATE.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO KNH-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SNGP "A-SNGP" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SNGP = ZERO
               MOVE W-SDATE TO W-SNGP
               GO TO M-15
           END-IF
           IF  W-SGET < 1 OR > 12
               GO TO M-10
           END-IF
           IF  W-SPEY < 1 OR > 31
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-ENGP "A-ENGP" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-ENGP = 999999
               MOVE W-EDATE TO W-ENGP
               GO TO M-17
           END-IF
           IF  W-EGET < 1 OR > 12
               GO TO M-15
           END-IF
           IF  W-EPEY < 1 OR > 31
               GO TO M-15
           END-IF.
       M-17.
           IF  W-SNGP > W-ENGP
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "X" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
      *
           MOVE W-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  À›“≤ ≈º  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               GO TO M-20
           END-IF
           MOVE SPACE TO W-HNA.
           IF  KH-KNA NOT = SPACE
               MOVE KH-KNA TO W-HNA
           ELSE
               MOVE KH-NAME TO W-HNA
           END-IF
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
       M-30.
           MOVE 0 TO W-DMM.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" KNH-F_PNAME1 "EXCLUSIVE" BY REFERENCE
            KNH-F_IDLST "0".
      *           SELECT KNH-F WHERE KNH-HCD = W-HCD
      *//////////////////////
           CALL "DB_Select" USING
            KNH-F_PNAME1 "WHERE"
            "KNH-HCD" "=" W-HCD RETURNING RET.
       M-35.
      *           READ KNH-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" KNH-F_PNAME1 BY REFERENCE KNH-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KNH-F_IDLST KNH-F_PNAME1
               MOVE SPACE TO W-MSG
               MOVE "***  DATA ≈º  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               GO TO M-70
           END-IF
           IF  KNH-NGPS < W-SNGP OR > W-ENGP
               GO TO M-35
           END-IF
           IF  KNH-HCD < W-HCD
               GO TO M-35
           END-IF
           IF  KNH-HCD > W-HCD
               CALL "DB_F_Close" USING
                BY REFERENCE KNH-F_IDLST KNH-F_PNAME1
               MOVE SPACE TO W-MSG
               MOVE "***  DATA ≈º  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               GO TO M-70
           END-IF.
       M-40.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE KNH-F_IDLST KNH-F_PNAME1
               GO TO M-95
           END-IF
           IF  W-DMM = 9
               CALL "DB_F_Close" USING
                BY REFERENCE KNH-F_IDLST KNH-F_PNAME1
               GO TO M-17
           END-IF.
       M-50.
      *           READ KNH-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" KNH-F_PNAME1 BY REFERENCE KNH-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KNH-F_IDLST KNH-F_PNAME1
               MOVE SPACE TO W-MSG
               MOVE "***  END DATA  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               GO TO M-70
           END-IF
           IF  KNH-NGPS < W-SNGP OR > W-ENGP
               GO TO M-50
           END-IF
           IF  KNH-HCD > W-HCD
               CALL "DB_F_Close" USING
                BY REFERENCE KNH-F_IDLST KNH-F_PNAME1
               MOVE SPACE TO W-MSG
               MOVE "***  END DATA  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               GO TO M-70
           END-IF
           GO TO M-40.
       M-70.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-70
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCKT31" RETURNING RESU.
           CALL "SD_Output" USING "A-SNGP" A-SNGP "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ENGP" A-ENGP "p" RETURNING RESU.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           GO TO M-17.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       MEI-RTN.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 23
               GO TO MEI-100
           END-IF
           MOVE SPACE TO W-DCN.
           IF  KNH-NHC NOT = ZERO
               MOVE "îpãp" TO W-DCN
           END-IF
           MOVE SPACE TO W-NRN.
           IF  KNH-NRC = 1
               MOVE "édì¸" TO W-NRN
           END-IF
           CALL "SD_Output" USING "D-NGPS" D-NGPS "p" RETURNING RESU.
           CALL "SD_Output" USING "D-DCN" D-DCN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NRM" D-NRM "p" RETURNING RESU.
           IF  KNH-SU NOT = ZERO
               CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU
           END-IF
           IF  KNH-T NOT = ZERO
               CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           GO TO MEI-EX.
       MEI-100.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO MEI-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO MEI-100
           END-IF
           IF  W-DMM NOT = 1 AND 9
               GO TO MEI-100
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCKT31" RETURNING RESU.
           CALL "SD_Output" USING "A-SNGP" A-SNGP "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ENGP" A-ENGP "p" RETURNING RESU.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF W-DMM = 9
               GO TO MEI-EX
           END-IF
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           GO TO MEI-RTN.
       MEI-EX.
           EXIT.

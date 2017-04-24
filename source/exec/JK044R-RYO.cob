       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JK044R.
      *********************************************************
      *    PROGRAM         :  ìùàÍì`ï[Å@ñ‚çáÇπÅiê‘ÇøÇ·ÇÒñ{ï‹Åj*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SJK044                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-SED.
             03  W-SSTC       PIC  9(007).
             03  W-ESTC       PIC  9(007).
             03  W-SDNO       PIC  9(007).
             03  W-EDNO       PIC  9(007).
           02  W-DMM          PIC  9(001).
           02  W-KEY.
             04  W-STC        PIC  9(007).
             04  W-DNO        PIC  9(007).
           02  W-DC           PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-NRC          PIC  N(002).
           02  W-PC           PIC  N(001).
           COPY LSTAT.
      *
           COPY LITDNA-RYO.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(016) VALUE
                "ê‘ÇøÇ·ÇÒñ{ï‹Å@ìùàÍì`ï[áÇÅ@ñ‚çáÇπ".
           02  FILLER  PIC  X(028) VALUE
                "ämîF (OK=1,NO=9) --->   ÿ¿∞›".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MEI.
             03  FILLER  PIC  9(007).
             03  FILLER  PIC  N(014).
             03  FILLER  PIC  9(007).
             03  FILLER  PIC  9(009).
             03  FILLER  PIC 99/99/99 .
             03  FILLER  PIC 99/99/99 .
             03  FILLER  PIC  N(002).
             03  FILLER  PIC  N(001).
           02  E-END   PIC  N(011) VALUE
                "ÇdÇmÇcÅ@ÇcÇ`ÇsÇ`".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ≈º  ***".
           COPY LSSEM.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "60" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "24" "32" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "23" "43" "28" "01C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "65" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "95" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEI" " " "W-L" "0" "73" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MEI" "9" "W-L" "1" "7" " " "D-MEI" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MEI" BY REFERENCE TDNA-STC "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MEI" "N" "W-L" "9" "28" "01D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MEI" BY REFERENCE TDNA-TNA "28" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MEI" "9" "W-L" "37" "7" "02D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-MEI" BY REFERENCE TDNA-DNO "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-MEI" "9" "W-L" "45" "9" "03D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-MEI" BY REFERENCE TDNA-HNO "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-MEI" "99/99/99" "W-L" "55" "8" "04D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-MEI" BY REFERENCE TDNA-HNGP "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-MEI" "99/99/99" "W-L" "64" "8" "05D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06D-MEI" BY REFERENCE TDNA-NNGP "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-MEI" "N" "W-L" "73" "4" "06D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "07D-MEI" BY REFERENCE W-NRC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-MEI" "N" "W-L" "78" "2" "07D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "08D-MEI" BY REFERENCE W-PC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-END" "N" "23" "41" "22" "D-MEI" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "17" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "17" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJK044" RETURNING RESU.
           MOVE 0 TO W-DC.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" TDNAF_PNAME1 "SHARED" BY REFERENCE TDNAF_IDLST "1"
            "TDNA-KEY" BY REFERENCE TDNA-KEY.
      *           READ TDNAF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
       M-50.
           MOVE TDNA-STC TO W-STC.
           MOVE TDNA-DNO TO W-DNO.
           MOVE SPACE TO W-NRC W-PC.
           IF  TDNA-NRC = 0
               MOVE "éÛêM" TO W-NRC
           END-IF
           IF  TDNA-NRC = 1
               MOVE "ì¸óÕ" TO W-NRC
           END-IF
           IF  TDNA-PC = 9
               MOVE "çœ" TO W-PC
           ELSE
               MOVE "ñ¢" TO W-PC
           END-IF
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF.
       M-55.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L NOT = 23
               CALL "SD_Output" USING "D-MEI" D-MEI "p" RETURNING RESU
               GO TO M-65
           END-IF.
       M-60.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-60
           END-IF
           IF  W-DMM = 9
               GO TO M-90
           END-IF
           IF  W-DMM = 1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Screen_Output" USING "SJK044" RETURNING RESU
               MOVE 3 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               GO TO M-55
           END-IF
           GO TO M-60.
       M-65.
      *           READ TDNAF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF (TDNA-STC = W-STC) AND (TDNA-DNO = W-DNO)
               GO TO M-65
           END-IF
           GO TO M-50.
       M-90.
           IF  W-DC = 0
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "E-END" E-END "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNAF_IDLST TDNAF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

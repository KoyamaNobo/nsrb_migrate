       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMT510.
      *********************************************************
      *    PROGRAM         :  履物　預り受払　問合せ　　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHT51                          *
      *        変更　　　  :  62/05/14                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  15K                PIC  X(005) VALUE X"1A24212078".
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-NGP.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-KEY.
             03  W-TCD        PIC  9(004).
             03  W-HCD        PIC  9(006).
           02  W-D.
             03  W-AS         PIC S9(006).
             03  W-SS         PIC S9(006).
           02  CNT            PIC  9(002).
           02  W-DC           PIC  9(002).
           02  W-AZ           PIC S9(006).
           02  W-TD.
             03  WT-AS        PIC S9(006).
             03  WT-SS        PIC S9(006).
           02  W-LC.
             03  W-L          PIC  9(002).
             03  W-C1         PIC  9(002).
             03  W-C2         PIC  9(002).
             03  W-C3         PIC  9(002).
             03  W-C4         PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-DMM          PIC  9(001).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
      *FD  TAZ-M
       01  TAZ-M_HMT510.
           02  TAZ-M_PNAME1   PIC  X(004) VALUE "TAZM".
           02  F              PIC  X(001).
           02  TAZ-M_LNAME    PIC  X(012) VALUE "TAZ-M_HMT510".
           02  F              PIC  X(001).
           02  TAZ-M_KEY1     PIC  X(100) VALUE SPACE.
           02  TAZ-M_KEY2     PIC  X(100) VALUE SPACE.
           02  TAZ-M_SORT     PIC  X(100) VALUE SPACE.
           02  TAZ-M_IDLST    PIC  X(100) VALUE SPACE.
           02  TAZ-M_RES      USAGE  POINTER.
       01  TAZ-R.
           02  TAZ-KEY.
             03  TAZ-TCD      PIC  9(004).
             03  TAZ-HCD      PIC  9(006).
           02  TAZ-ZS         PIC S9(005).
           02  TAZ-AS         PIC S9(005).
           02  F              PIC  X(016).
           02  TAZ-NG         PIC  9(006).
       77  F                  PIC  X(001).
      *FD  AUH-F
       01  AUH-F_HMT510.
           02  AUH-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  AUH-F_LNAME    PIC  X(012) VALUE "AUH-F_HMT510".
           02  F              PIC  X(001).
           02  AUH-F_KEY1     PIC  X(100) VALUE SPACE.
           02  AUH-F_SORT     PIC  X(100) VALUE SPACE.
           02  AUH-F_IDLST    PIC  X(100) VALUE SPACE.
           02  AUH-F_RES      USAGE  POINTER.
       01  AUH-R.
           02  UH-KEY.
             03  UH-TCD       PIC  9(004).
             03  UH-HCD       PIC  9(006).
           02  UH-DATE.
             03  UH-N         PIC  9(002).
             03  UH-G         PIC  9(002).
             03  UH-P         PIC  9(002).
           02  UH-AS          PIC S9(005).
           02  UH-SS          PIC S9(005).
           02  UH-AZ          PIC S9(005).
           02  F              PIC  X(033).
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
           02  A-TCD   PIC  9(004).
           02  A-HCD   PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  D-DSP.
           02  D-NG.
             03  01D-NG  PIC 99 .
             03  02D-NG  PIC Z9 .
           02  D-TNA   PIC  N(026).
           02  D-HNA   PIC  N(024).
           02  D-D.
             03  D-PEY   PIC  Z(002).
             03  D-AS    PIC ---,---,--- .
             03  D-SS    PIC ---,---,--- .
             03  D-AZ    PIC ---,---,--- .
           02  D-TD.
             03  01D-TD  PIC ---,---,--- .
             03  02D-TD  PIC ---,---,--- .
             03  03D-TD  PIC ---,---,--- .
           02  D-EM    PIC  X(040) VALUE
                "ｺｰﾄﾞ入力=ﾘﾀ-ﾝ  NEXT=ｆ･10  終了=ｆ･9    ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4A05".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
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
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "11" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-TCD" "9" "3" "12" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-HCD" "9" "4" "10" "6" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "56" "1" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *D-DSP
       CALL "SD_Init" USING 
            "D-DSP" " " "0" "0" "212" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NG" " " "1" "0" "4" " " "D-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NG" "99" "1" "25" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-NEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NG" "Z9" "1" "29" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "3" "17" "52" "D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNA" "N" "4" "17" "48" "D-TNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-D" " " "W-L" "0" "35" "D-HNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PEY" "Z" "W-L" "W-C1" "2" " " "D-D" RETURNING RESU.
       CALL "SD_From" USING 
            "D-PEY" BY REFERENCE W-DC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-AS" "---,---,---" "W-L" "W-C2" "11" "D-PEY" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-AS" BY REFERENCE W-AS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SS" "---,---,---" "W-L" "W-C3" "11" "D-AS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-SS" BY REFERENCE W-SS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-AZ" "---,---,---" "W-L" "W-C4" "11" "D-SS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-AZ" BY REFERENCE W-AZ "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TD" " " "22" "0" "33" "D-D" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TD" "---,---,---" "22" "45" "11" " " "D-TD"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-TD" BY REFERENCE WT-AS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TD" "---,---,---" "22" "57" "11" "01D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-TD" BY REFERENCE WT-SS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-TD" "---,---,---" "22" "69" "11" "02D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-TD" BY REFERENCE W-AZ "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-EM" "X" "23" "17" "40" "D-TD" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "77" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "77" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           MOVE D-HSD TO W-NGP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO AUH-F_PNAME1.
           MOVE ZERO TO W-TCD W-HCD.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TAZ-M_PNAME1 "SHARED" BY REFERENCE TAZ-M_IDLST "1"
            "TAZ-KEY" BY REFERENCE TAZ-KEY.
       M-10.
           CALL "SD_Screen_Output" USING "SCHT51" RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
           IF  W-TCD NOT = ZERO
               CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU
               CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU
           END-IF
           IF  W-HCD NOT = ZERO
               CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU
               CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND ADV AND SKP
               GO TO M-15
           END-IF
           IF  ESTAT = ADV
               ADD 1 TO W-TCD
           END-IF
           MOVE ZERO TO TAZ-KEY.
           MOVE W-TCD TO TAZ-TCD.
      *           START TAZ-M KEY NOT < TAZ-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TAZ-M_PNAME1 "TAZ-KEY" "NOT < " TAZ-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
      *           READ TAZ-M NEXT RECORD WITH UNLOCK AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TAZ-M_PNAME1 BY REFERENCE TAZ-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           MOVE TAZ-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　" TO T-NAME
           END-IF
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND ADV AND SKP
               GO TO M-20
           END-IF
           IF  ESTAT = ADV
               ADD 1 TO W-HCD
           END-IF
           MOVE W-TCD TO TAZ-TCD.
           MOVE W-HCD TO TAZ-HCD.
      *           START TAZ-M KEY NOT < TAZ-KEY INVALID KEY
      *//////////////////////     
           CALL "DB_Start" USING
            TAZ-M_PNAME1 "TAZ-KEY" "NOT < " TAZ-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-20
           END-IF
      *           READ TAZ-M NEXT RECORD WITH UNLOCK AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TAZ-M_PNAME1 BY REFERENCE TAZ-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-20
           END-IF
           IF  TAZ-TCD NOT = W-TCD
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-20
           END-IF.
       M-25.
           MOVE TAZ-HCD TO W-HCD.
           MOVE W-HCD TO HI-KEY
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　" TO HI-NAME
           END-IF
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           MOVE W-TCD TO TAZ-TCD.
           MOVE W-HCD TO TAZ-HCD.
      *           START TAZ-M KEY NOT < TAZ-KEY INVALID KEY
      *//////////////////////     
           CALL "DB_Start" USING
            TAZ-M_PNAME1 "TAZ-KEY" "NOT < " TAZ-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-20
           END-IF
      *           READ TAZ-M NEXT RECORD WITH UNLOCK AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TAZ-M_PNAME1 BY REFERENCE TAZ-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-20
           END-IF
           IF (TAZ-TCD NOT = W-TCD) OR (TAZ-HCD NOT = W-HCD)
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-20
           END-IF.
       M-40.
           MOVE ZERO TO W-DC W-TD CHK.
           MOVE TAZ-ZS TO W-AZ.
           MOVE 6 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE 02 TO W-C1.
           CALL "SD_Arg_Match_Col" USING "W-C1" "2" W-C1 RETURNING RESU.
           MOVE 05 TO W-C2.
           CALL "SD_Arg_Match_Col" USING "W-C2" "2" W-C2 RETURNING RESU.
           MOVE 17 TO W-C3.
           CALL "SD_Arg_Match_Col" USING "W-C3" "2" W-C3 RETURNING RESU.
           MOVE 29 TO W-C4.
           CALL "SD_Arg_Match_Col" USING "W-C4" "2" W-C4 RETURNING RESU.
           CALL "SD_Output" USING "D-AZ" D-AZ "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" AUH-F_PNAME1 "EXCLUSIVE" BY REFERENCE
            AUH-F_IDLST "0".
       M-45.
      *           READ AUH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" AUH-F_PNAME1 BY REFERENCE AUH-R " " RETURNING RET.
           IF  RET = 1
               MOVE 5 TO CHK
               GO TO M-50
           END-IF
           IF  W-KEY > UH-KEY
               GO TO M-45
           END-IF
           IF  UH-P = ZERO
               GO TO M-45
           END-IF
           IF  W-KEY < UH-KEY
               MOVE 5 TO CHK
           END-IF.
       M-50.
           MOVE ZERO TO W-D.
           ADD 1 TO W-DC W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-DC > W-PEY
               GO TO M-55
           END-IF
           IF  W-L = 22
               MOVE 6 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               ADD 40 TO W-C1 W-C2 W-C3 W-C4
               CALL "SD_Arg_Match_Col" USING
                "W-C1" "2" W-C1 RETURNING RESU
               CALL "SD_Arg_Match_Col" USING
                "W-C2" "2" W-C2 RETURNING RESU
               CALL "SD_Arg_Match_Col" USING
                "W-C3" "2" W-C3 RETURNING RESU
               CALL "SD_Arg_Match_Col" USING
                "W-C4" "2" W-C4 RETURNING RESU
           END-IF
           IF  CHK = 5
               CALL "SD_Output" USING "D-D" D-D "p" RETURNING RESU
               GO TO M-50
           END-IF
           IF  W-DC NOT = UH-P
               CALL "SD_Output" USING "D-D" D-D "p" RETURNING RESU
               GO TO M-50
           END-IF
           MOVE UH-AS TO W-AS.
           MOVE UH-SS TO W-SS.
           COMPUTE W-AZ = W-AZ + W-AS - W-SS.
           CALL "SD_Output" USING "D-D" D-D "p" RETURNING RESU.
           ADD W-AS TO WT-AS.
           ADD W-SS TO WT-SS.
           GO TO M-45.
       M-55.
           CALL "DB_F_Close" USING
            BY REFERENCE AUH-F_IDLST AUH-F_PNAME1.
           CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-EM" D-EM "p" RETURNING RESU.
       M-60.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT = HTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = ADV
               GO TO M-60
           END-IF
           CALL "SD_Screen_Output" USING "SCHT51" RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
      *           READ TAZ-M NEXT RECORD WITH UNLOCK AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TAZ-M_PNAME1 BY REFERENCE TAZ-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-10
           END-IF
           MOVE TAZ-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　" TO T-NAME
           END-IF
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           MOVE TAZ-HCD TO W-HCD.
           MOVE W-HCD TO HI-KEY
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　" TO HI-NAME
           END-IF
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           GO TO M-40.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TAZ-M_IDLST TAZ-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

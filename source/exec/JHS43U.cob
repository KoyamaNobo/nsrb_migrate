       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS43U.
      *********************************************************
      *    PROGRAM         :  ê‘ÇøÇ·ÇÒñ{ï‹ÇdÇnÇré©ìÆéwê}      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-ERR              PIC  9(001) VALUE 0.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-L            PIC  9(002).
           02  W-EC           PIC  9(001).
           02  W-OKC          PIC  9(001).
           02  CNT            PIC  9(001).
           02  W-STC          PIC  9(007).
           02  W-NGP.
             03  W-NG         PIC  9(006).
             03  F            PIC  9(002).
           COPY LSTAT.
      *
           COPY LITDNA.
           COPY LICODE.
           COPY LIHIM2.
           COPY LITCM.
           COPY LIAHNH.
      *FD  SHAW
       01  SHAW_JHS43U.
           02  SHAW_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SHAW_LNAME     PIC  X(011) VALUE "SHAW_JHS43U".
           02  F              PIC  X(001).
           02  SHAW_KEY1      PIC  X(100) VALUE SPACE.
           02  SHAW_SORT      PIC  X(100) VALUE SPACE.
           02  SHAW_IDLST     PIC  X(100) VALUE SPACE.
           02  SHAW_RES       USAGE  POINTER.
       01  SHAW-R.
           02  SHAW-DNGP      PIC  9(008).
           02  SHAW-CSC.
             03  SHAW-TCD     PIC  9(004).
             03  SHAW-CCD     PIC  9(003).
           02  SHAW-STC       PIC  9(007).
           02  SHAW-ISU       PIC  9(003).
           02  SHAW-HCD       PIC  9(006).
           02  SHAW-SIZ       PIC  9(001).
           02  SHAW-ASU.
             03  SHAW-SUD   OCCURS  10.
               04  SHAW-SU    PIC S9(004).
           02  SHAW-SNG       PIC  9(006).
           02  SHAW-SIZN      PIC  9(001).
           02  F              PIC  X(049).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "ÅñÅñÅñÅ@Å@ê‘ÇøÇ·ÇÒñ{ï‹éwê}ïœä∑ÉèÅ[ÉNçÏê¨Å@Å@ÅñÅñÅñ".
           02  FILLER  PIC  X(22)
                   VALUE "ämîF  OK=1 NO=9   ÿ¿∞›".
       01  ACP-AREA.
           02  ACP-OKC     PIC  9(01).
       01  DSP-AREA.
           02  FILLER.
             03  DSP-STC.
               04  FILLER  PIC  N(005) VALUE "î[ïiêÊÇ»Çµ".
               04  FILLER  PIC  9(007).
             03  DSP-CSC.
               04  FILLER  PIC  N(005) VALUE "íºëóêÊÇ»Çµ".
               04  FILLER  PIC  9(007).
             03  DSP-JAN.
               04  FILLER  PIC  N(005) VALUE "ÇiÇ`ÇmÇ»Çµ".
               04  FILLER  PIC  X(013).
             03  DSP-HCD.
               04  FILLER  PIC  N(004) VALUE "ïiñºÇ»Çµ".
               04  FILLER  PIC  X(013).
               04  FILLER  PIC  9(006).
             03  DSP-SIZ.
               04  FILLER  PIC  N(005) VALUE "ÉTÉCÉYÇ»Çµ".
               04  FILLER  PIC  X(013).
               04  FILLER  PIC  9(006).
             03  DSP-ISU.
               04  FILLER  PIC  N(004) VALUE "ì¸êîÇ»Çµ".
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  N(024).
             03  DSP-CLE.
               04  FILLER  PIC  X(042) VALUE
                    "                                        ".
               04  FILLER  PIC  X(034) VALUE
                    "                                  ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ≈º  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ¥◊∞  ***".
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
            "C-MID" " " "0" "0" "72" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "13" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "22" "43" "22" "01C-MID" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "22" "60" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "251" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" " " "W-L" "0" "251" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-STC" " " "W-L" "0" "17" " " "01DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-STC" "N" "W-L" "4" "10" " " "DSP-STC"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-STC" "9" "W-L" "16" "7" "01DSP-STC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-STC" BY REFERENCE TDNA-STC "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-CSC" " " "W-L" "0" "17" "DSP-STC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-CSC" "N" "W-L" "4" "10" " " "DSP-CSC"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-CSC" "9" "W-L" "16" "7" "01DSP-CSC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-CSC" BY REFERENCE AHNH-CSC "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-JAN" " " "W-L" "0" "23" "DSP-CSC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-JAN" "N" "W-L" "4" "10" " " "DSP-JAN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-JAN" "X" "W-L" "16" "13" "01DSP-JAN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-JAN" BY REFERENCE TDNA-JAN "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HCD" " " "W-L" "0" "27" "DSP-JAN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-HCD" "N" "W-L" "4" "8" " " "DSP-HCD"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-HCD" "X" "W-L" "13" "13" "01DSP-HCD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-HCD" BY REFERENCE TDNA-JAN "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-HCD" "9" "W-L" "28" "6" "02DSP-HCD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-HCD" BY REFERENCE HI-MHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SIZ" " " "W-L" "0" "29" "DSP-HCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-SIZ" "N" "W-L" "4" "10" " " "DSP-SIZ"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-SIZ" "X" "W-L" "16" "13" "01DSP-SIZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-SIZ" BY REFERENCE TDNA-JAN "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-SIZ" "9" "W-L" "30" "6" "02DSP-SIZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-SIZ" BY REFERENCE HI-MHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-ISU" " " "W-L" "0" "62" "DSP-SIZ" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-ISU" "N" "W-L" "4" "8" " " "DSP-ISU"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-ISU" "9" "W-L" "13" "6" "01DSP-ISU" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-ISU" BY REFERENCE HI-MHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-ISU" "N" "W-L" "20" "48" "02DSP-ISU" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-ISU" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-CLE" " " "W-L" "0" "76" "DSP-ISU" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-CLE" "X" "W-L" "2" "42" " " "DSP-CLE" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-CLE" "X" "W-L" "44" "34" "01DSP-CLE" " "
            RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "35" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "35" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-OKC = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  W-OKC NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO SHAW_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" SHAW_PNAME1 " " BY REFERENCE SHAW_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" TDNAF_PNAME1 "SHARED" BY REFERENCE TDNAF_IDLST "1"
            "TDNA-KEY" BY REFERENCE TDNA-KEY.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" AHNHF_PNAME1 "SHARED" BY REFERENCE AHNHF_IDLST "1"
            "AHNH-KEY" BY REFERENCE AHNH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-15.
      *           READ TDNAF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-20.
           MOVE TDNA-STC TO W-STC.
           MOVE W-STC TO AHNH-KEY.
      *           READ AHNHF UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" AHNHF_PNAME1 BY REFERENCE AHNH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 5 TO W-EC
               PERFORM MSG-RTN THRU MSG-EX
               GO TO M-30
           END-IF
           MOVE AHNH-CSC TO TC-KEY.
      *           READ TC-M UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 1 TO W-EC
               PERFORM MSG-RTN THRU MSG-EX
           END-IF.
       M-30.
           MOVE SPACE TO CODE-KEY.
           MOVE ZERO TO CODE-TCD.
           MOVE TDNA-JAN TO CODE-JAN.
      *           START CODEF KEY NOT < CODE-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CODEF_PNAME1 "CODE-KEY" " NOT < " CODE-KEY RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 2 TO W-EC
               PERFORM MSG-RTN THRU MSG-EX
               GO TO M-35
           END-IF
      *           READ CODEF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 2 TO W-EC
               PERFORM MSG-RTN THRU MSG-EX
               GO TO M-35
           END-IF
           IF (CODE-TCD NOT = ZERO) OR (TDNA-JAN NOT = CODE-JAN)
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 2 TO W-EC
               PERFORM MSG-RTN THRU MSG-EX
               GO TO M-35
           END-IF
           MOVE CODE-HCD TO HI-MHCD HI-HCD.
      *           READ HI2-M UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 6 TO W-EC
               PERFORM MSG-RTN THRU MSG-EX
               GO TO M-35
           END-IF
           IF  HI-ISU = ZERO
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 3 TO W-EC
               PERFORM MSG-RTN THRU MSG-EX
               GO TO M-35
           END-IF
           MOVE 0 TO HI-S(4,10).
           IF  HI-S(CODE-SIZ,CODE-SNO) = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 4 TO W-EC
               PERFORM MSG-RTN THRU MSG-EX
               GO TO M-35
           END-IF
      *
           IF  W-ERR NOT = 0
               GO TO M-35
           END-IF
           MOVE ZERO TO SHAW-R.
           MOVE TDNA-DNGP TO SHAW-DNGP.
           MOVE AHNH-CSC TO SHAW-CSC.
           MOVE AHNH-STC TO SHAW-STC.
           MOVE HI-ISU TO SHAW-ISU.
           MOVE CODE-HCD TO SHAW-HCD.
           MOVE CODE-SIZ TO SHAW-SIZ.
           MOVE TDNA-SU TO SHAW-SU(CODE-SNO).
           MOVE TDNA-SNGP TO W-NGP.
           MOVE W-NG TO SHAW-SNG.
           COMPUTE SHAW-SIZN = SHAW-SIZ - 1.
           IF  SHAW-SIZN = 0
               MOVE 4 TO SHAW-SIZN
           END-IF
      *           WRITE SHAW-R.
      *//////////////
           CALL "DB_Insert" USING
            SHAW_PNAME1 SHAW_LNAME SHAW-R RETURNING RET.
       M-35.
      *           READ TDNAF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  TDNA-STC NOT = W-STC
               GO TO M-20
           END-IF
           GO TO M-30.
       M-50.
           IF  W-EC = 0
               GO TO M-90
           END-IF
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-50
           END-IF
           IF  W-OKC NOT = 1
               GO TO M-50
           END-IF.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE SHAW_IDLST SHAW_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNAF_IDLST TDNAF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE AHNHF_IDLST AHNHF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       MSG-RTN.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 20
               GO TO MSG-010
           END-IF
           IF  W-EC = 1
               CALL "SD_Output" USING
                "DSP-CSC" DSP-CSC "p" RETURNING RESU
           END-IF
           IF  W-EC = 2
               CALL "SD_Output" USING
                "DSP-JAN" DSP-JAN "p" RETURNING RESU
           END-IF
           IF  W-EC = 3
               CALL "SD_Output" USING
                "DSP-ISU" DSP-ISU "p" RETURNING RESU
           END-IF
           IF  W-EC = 4
               CALL "SD_Output" USING
                "DSP-SIZ" DSP-SIZ "p" RETURNING RESU
           END-IF
           IF  W-EC = 5
               CALL "SD_Output" USING
                "DSP-STC" DSP-STC "p" RETURNING RESU
           END-IF
           IF  W-EC = 6
               CALL "SD_Output" USING
                "DSP-HCD" DSP-HCD "p" RETURNING RESU
           END-IF
           IF  W-EC = 0
               MOVE 1 TO W-EC
           END-IF
           GO TO MSG-EX.
       MSG-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO MSG-010
           END-IF
           IF  W-OKC NOT = 1
               GO TO MSG-010
           END-IF
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       MSG-020.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L < 21
               CALL "SD_Output" USING
                "DSP-CLE" DSP-CLE "p" RETURNING RESU
               GO TO MSG-020
           END-IF
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           GO TO MSG-RTN.
       MSG-EX.
           EXIT.

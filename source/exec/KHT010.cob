       IDENTIFICATION DIVISION.
       PROGRAM-ID.  KHT010.
      *********************************************************
      *    PROGRAM         :  工品　受払表　問合せ　　　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCKT01                          *
      *        変更　　　  :  62/04/03                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-AD.
             03  W-AD1   OCCURS  90  PIC  X(032).
           02  W-D.
             03  W-MD         PIC  9(002).
             03  W-KS         PIC S9(006).
             03  W-HS         PIC S9(006).
             03  W-IS         PIC S9(006).
             03  W-SS         PIC S9(006).
             03  W-ZS         PIC S9(006).
           02  W-ND.
             03  WN-MD        PIC  9(002).
             03  WN-KS        PIC S9(006).
             03  WN-HS        PIC S9(006).
             03  WN-IS        PIC S9(006).
             03  WN-SS        PIC S9(006).
             03  WN-ZS        PIC S9(006).
           02  W-M.
             03  W-KEY        PIC  X(005).
             03  W-NA         PIC  X(020).
           02  W-GD.
             03  W-GZS        PIC S9(006).
             03  W-GKS        PIC S9(006).
             03  W-GHS        PIC S9(006).
             03  W-GIS        PIC S9(006).
             03  W-GSS        PIC S9(006).
           02  W-DATE.
             03  W-DATE1      PIC  9(004).
             03  W-DATE2      PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-LC.
             03  W-L          PIC  9(002).
             03  W-C1         PIC  9(002).
             03  W-C2         PIC  9(002).
             03  W-C3         PIC  9(002).
             03  W-C4         PIC  9(002).
             03  W-C5         PIC  9(002).
             03  W-C6         PIC  9(002).
           02  CNT.
             03  CNT1         PIC  9(002).
             03  CNT2         PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-C            PIC  9(002).
           02  W-YMD.
             03  W-YMD1.
               04  W-YY1      PIC  9(002).
               04  W-MM1      PIC  9(002).
               04  W-DD1      PIC  9(002).
             03  W-YMD2.
               04  W-YY2      PIC  9(002).
               04  W-MM2      PIC  9(002).
               04  W-DD2      PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKHM.
           COPY LIKHT1.
      *FD  KUH-F
       01  KUH-F_KHT010.
           02  KUH-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  KUH-F_LNAME    PIC  X(012) VALUE "KUH-F_KHT010".
           02  F              PIC  X(001).
           02  KUH-F_KEY1     PIC  X(100) VALUE SPACE.
           02  KUH-F_KEY2     PIC  X(100) VALUE SPACE.
           02  KUH-F_SORT     PIC  X(100) VALUE SPACE.
           02  KUH-F_IDLST    PIC  X(100) VALUE SPACE.
           02  KUH-F_RES      USAGE  POINTER.
       01  K-R.
           02  K-KEY          PIC  X(005).
           02  K-DATE         PIC  9(006).
           02  K-KS           PIC S9(006).
           02  K-HS           PIC S9(006).
           02  K-IS           PIC S9(006).
           02  K-SS           PIC S9(006).
           02  K-ZS           PIC S9(006).
           02  F              PIC  X(023).
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
       01  C-MID.
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　工品　受払　問合せ　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ACP.
           02  A-KEY   PIC  X(005).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NSD.
             03  FILLER  PIC Z9 .
             03  FILLER  PIC Z9 .
             03  FILLER  PIC Z9 .
             03  FILLER  PIC Z9 .
           02  D-NA    PIC  X(020).
           02  D-MD    PIC  X(002) VALUE "  ".
           02  D-UHD.
             03  FILLER  PIC ZZ .
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(005).
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(007).
             03  FILLER  PIC  -(007).
           02  D-TD.
             03  D-GKSU  PIC ------9 .
             03  D-GHSU  PIC -----9 .
             03  D-GISU  PIC ------9 .
             03  D-GSSU  PIC -------9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(030) VALUE
                  "***  KHM ﾅｼ  ***              ".
             03  E-ME3   PIC  X(030) VALUE
                  "***  DATA ｴﾗｰ  ***            ".
             03  E-ME4   PIC  X(030) VALUE
                  "***  DATA ﾅｼ  ***             ".
             03  E-ME5   PIC  X(030) VALUE
                  "***  DATEM ﾅｼ  ***            ".
             03  E-ME6   PIC  X(030) VALUE
                  "***  KHTM ﾅｼ  ***             ".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "266" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "38" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "38" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "38" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "38" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "38" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "38" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "38" "06C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "X" "2" "7" "5" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "56" "1" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "91" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NSD" " " "2" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NSD" "Z9" "2" "60" "2" " " "D-NSD" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NSD" BY REFERENCE W-MM1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NSD" "Z9" "2" "63" "2" "01D-NSD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NSD" BY REFERENCE W-DD1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-NSD" "Z9" "2" "73" "2" "02D-NSD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NSD" BY REFERENCE W-MM2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-NSD" "Z9" "2" "76" "2" "03D-NSD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NSD" BY REFERENCE W-DD2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NA" "X" "2" "18" "20" "D-NSD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NA" BY REFERENCE KH-NAME "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MD" "X" "W-L" "W-C1" "2" "D-NA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UHD" " " "W-L" "0" "33" "D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-UHD" "ZZ" "W-L" "W-C1" "2" " " "D-UHD" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-UHD" BY REFERENCE WN-MD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-UHD" "------" "W-L" "W-C2" "6" "01D-UHD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-UHD" BY REFERENCE WN-KS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-UHD" "-----" "W-L" "W-C3" "5" "02D-UHD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-UHD" BY REFERENCE WN-HS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-UHD" "------" "W-L" "W-C4" "6" "03D-UHD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-UHD" BY REFERENCE WN-IS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-UHD" "-------" "W-L" "W-C5" "7" "04D-UHD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-UHD" BY REFERENCE WN-SS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-UHD" "-------" "W-L" "W-C6" "7" "05D-UHD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06D-UHD" BY REFERENCE WN-ZS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TD" " " "20" "0" "28" "D-UHD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GKSU" "------9" "20" "44" "7" " " "D-TD" RETURNING RESU.
       CALL "SD_From" USING 
            "D-GKSU" BY REFERENCE W-GKS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GHSU" "-----9" "20" "51" "6" "D-GKSU" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-GHSU" BY REFERENCE W-GHS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GISU" "------9" "20" "57" "7" "D-GHSU" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-GISU" BY REFERENCE W-GIS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GSSU" "-------9" "20" "64" "8" "D-GISU" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-GSSU" BY REFERENCE W-GSS "6" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "160" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "160" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "30" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "30" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "30" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "30" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "30" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" M-DATE_PNAME1 "SHARED" BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
           MOVE ZERO TO W-YMD.
       M-040.
           MOVE "01" TO DATE-KEY.
      *           READ M-DATE WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" M-DATE_PNAME1 BY REFERENCE DATE-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-040
           END-IF
           MOVE D-KKD TO W-YMD1.
           MOVE D-KUD TO W-YMD2.
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO KUH-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KHT-M_PNAME1 "SHARED" BY REFERENCE KHT-M_IDLST "1"
            "KHT-KEY" BY REFERENCE KHT-KEY.
       M-060.
           CALL "SD_Screen_Output" USING "SCKT01" RETURNING RESU.
           CALL "SD_Output" USING "D-NSD" D-NSD "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "X" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-900
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF
           MOVE W-KEY TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-060
           END-IF
           CALL "SD_Output" USING "D-NA" D-NA "p" RETURNING RESU.
           MOVE KH-NAME TO W-NA.
       M-080.
           MOVE KH-KEY TO KHT-KEY.
      *           READ KHT-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KHT-M_PNAME1 BY REFERENCE KHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-060
           END-IF.
       M-100.
           PERFORM S-20 THRU S-30.
           CALL "DB_F_Open" USING
            "INPUT" KUH-F_PNAME1 " " BY REFERENCE KUH-F_IDLST "0".
           MOVE ZERO TO W-GD CHK W-D W-ND W-DATE.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE  2 TO W-C1.
           CALL "SD_Arg_Match_Col" USING "W-C1" "2" W-C1 RETURNING RESU.
           MOVE  5 TO W-C2.
           CALL "SD_Arg_Match_Col" USING "W-C2" "2" W-C2 RETURNING RESU.
           MOVE 12 TO W-C3.
           CALL "SD_Arg_Match_Col" USING "W-C3" "2" W-C3 RETURNING RESU.
           MOVE 18 TO W-C4.
           CALL "SD_Arg_Match_Col" USING "W-C4" "2" W-C4 RETURNING RESU.
           MOVE 25 TO W-C5.
           CALL "SD_Arg_Match_Col" USING "W-C5" "2" W-C5 RETURNING RESU.
           MOVE 33 TO W-C6.
           CALL "SD_Arg_Match_Col" USING "W-C6" "2" W-C6 RETURNING RESU.
           MOVE ZERO TO CNT W-C.
      *
           IF  KHT-ZSU NOT = ZERO
               ADD 1 TO CNT1
               MOVE 99 TO W-MD
               MOVE KHT-ZSU TO W-GZS W-ZS
               MOVE W-D TO W-AD1(CNT1)
           END-IF.
       M-120.
      *           READ KUH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KUH-F_PNAME1 BY REFERENCE K-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-240
           END-IF
           IF  W-KEY NOT = K-KEY
               GO TO M-120
           END-IF
           IF  ZERO = K-KS AND K-HS AND K-IS AND K-SS
               GO TO M-120
           END-IF
           MOVE ZERO TO W-D.
       M-160.
           IF  CHK = ZERO
               GO TO M-200
           END-IF
           IF  K-DATE NOT = W-DATE
               MOVE ZERO TO CHK
               GO TO M-200
           END-IF
           COMPUTE W-GZS = W-GZS + K-KS - K-HS + K-IS - K-SS.
           ADD K-KS TO W-GKS W-KS.
           ADD K-HS TO W-GHS W-HS.
           ADD K-IS TO W-GIS W-IS.
           ADD K-SS TO W-GSS W-SS.
           MOVE W-GZS TO W-ZS.
           MOVE W-D TO W-AD1(CNT1).
           GO TO M-220.
       M-200.
           ADD 1 TO CNT1.
           IF  CNT1 = 91
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           MOVE ZERO TO W-D W-ND W-DATE.
           MOVE K-DATE TO W-DATE.
           MOVE W-DATE2 TO W-MD.
           MOVE K-KS TO W-KS.
           ADD K-KS TO W-GKS.
           MOVE K-HS TO W-HS
           ADD K-HS TO W-GHS.
           MOVE K-IS TO W-IS
           ADD K-IS TO W-GIS.
           MOVE K-SS TO W-SS.
           ADD K-SS TO W-GSS.
           COMPUTE W-GZS = W-GZS + K-KS - K-HS + K-IS - K-SS.
           MOVE W-GZS TO W-ZS.
           MOVE W-D TO W-AD1(CNT1).
       M-220.
           IF  W-SS = ZERO
               MOVE 5 TO CHK
           END-IF
      *           READ KUH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KUH-F_PNAME1 BY REFERENCE K-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-240
           END-IF
           IF  W-KEY NOT = K-KEY
               GO TO M-240
           END-IF
           IF  ZERO = K-KS AND K-HS AND K-IS AND K-SS
               GO TO M-220
           END-IF
           GO TO M-160.
       M-240.
           PERFORM S-35 THRU S-65.
       M-260.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-260
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE KUH-F_IDLST KUH-F_PNAME1.
           GO TO M-060.
       M-900.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KHT-M_IDLST KHT-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-20.
           MOVE ZERO TO CNT1.
       S-25.
           ADD 1 TO CNT1.
           IF  CNT1 NOT = 91
               MOVE ZERO TO W-AD1(CNT1)
               GO TO S-25
           END-IF.
       S-30.
           EXIT.
       S-35.
           MOVE 1 TO CNT2.
           MOVE W-AD1(CNT2) TO W-D.
       S-40.
           IF  W-MD = ZERO
               GO TO S-50
           END-IF
           MOVE W-D TO W-ND.
       S-45.
           ADD 1 TO CNT2.
           MOVE W-AD1(CNT2) TO W-D.
           IF  W-MD = WN-MD
               ADD W-SS TO WN-SS
               ADD W-KS TO WN-KS
               ADD W-HS TO WN-HS
               ADD W-IS TO WN-IS
               COMPUTE WN-ZS = WN-ZS + W-KS - W-HS + W-IS - W-SS
               GO TO S-45
           END-IF
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 20
               MOVE 4 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE 42 TO W-C1
               CALL "SD_Arg_Match_Col" USING
                "W-C1" "2" W-C1 RETURNING RESU
               MOVE 45 TO W-C2
               CALL "SD_Arg_Match_Col" USING
                "W-C2" "2" W-C2 RETURNING RESU
               MOVE 52 TO W-C3
               CALL "SD_Arg_Match_Col" USING
                "W-C3" "2" W-C3 RETURNING RESU
               MOVE 58 TO W-C4
               CALL "SD_Arg_Match_Col" USING
                "W-C4" "2" W-C4 RETURNING RESU
               MOVE 65 TO W-C5
               CALL "SD_Arg_Match_Col" USING
                "W-C5" "2" W-C5 RETURNING RESU
               MOVE 73 TO W-C6
               CALL "SD_Arg_Match_Col" USING
                "W-C6" "2" W-C6 RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-UHD" D-UHD "p" RETURNING RESU.
           IF  WN-MD = 99
               CALL "SD_Output" USING "D-MD" D-MD "p" RETURNING RESU
           END-IF
           GO TO S-40.
       S-50.
           CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU.
           IF  CNT2 = 1
               CALL "SD_Output" USING "E-ME4" E-ME4 "p" RETURNING RESU
           END-IF.
       S-65.
           EXIT.

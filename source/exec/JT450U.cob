       IDENTIFICATION DIVISION.
       PROGRAM-ID. JT450U.
      **************************************************
      *****     倉庫品名別出荷明細　選択           *****
      **************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  WK0256ID               PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1            PIC  X(003).
           02  STN-NO2            PIC  X(003).
       01  W-FID.
           02  W-FID1             PIC  X(006) VALUE "WK0256".
           02  W-FID2             PIC  X(003).
       01  W-DATA.
           02  W-SEN          PIC  9(001).
           02  W-SSOK         PIC  9(001).
           02  W-ESOK         PIC  9(001).
           02  W-SHCD         PIC  9(004).
           02  W-EHCD         PIC  9(004).
           02  W-SDAT         PIC  9(008).
           02  W-EDAT         PIC  9(008).
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-SNGP         PIC  9(008).
           02  W-SNGPD REDEFINES W-SNGP.
             03  W-SNEN       PIC  9(004).
             03  W-SNENL REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-ENGP         PIC  9(008).
           02  W-ENGPD REDEFINES W-ENGP.
             03  W-ENEN       PIC  9(004).
             03  W-ENENL REDEFINES W-ENEN.
               04  W-ENEN1    PIC  9(002).
               04  W-ENEN2    PIC  9(002).
             03  W-EGET       PIC  9(002).
             03  W-EPEY       PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-HCD.
             03  W-HCDD       PIC  9(004).
             03  F            PIC  9(002).
           02  W-DC           PIC  9(001).
           02  W-16           PIC  9(001).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LJSTRR.
           COPY LTWK03.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　倉庫品名別　出荷明細表　　＊＊＊".
           02  FILLER  PIC  X(028) VALUE
                "データは '  年   月   日より".
           02  FILLER  PIC  X(034) VALUE
                "'  年   月   日 ～ '  年   月   日".
           02  FILLER.
             03  FILLER  PIC  X(022) VALUE "教　育=0 , 一　般=1 , ".
             03  FILLER  PIC  X(015) VALUE "全  体=9  ...  ".
           02  FILLER  PIC  X(019) VALUE
                "倉庫ｺｰﾄﾞ     0 ～ 9".
           02  FILLER  PIC  X(022) VALUE
                "品種ｺｰﾄﾞ  0000 ～ 9999".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SNEN  PIC  9(002).
             03  A-SGET  PIC  9(002).
             03  A-SPEY  PIC  9(002).
             03  A-ENEN  PIC  9(002).
             03  A-EGET  PIC  9(002).
             03  A-EPEY  PIC  9(002).
           02  A-SEN   PIC  9(001).
           02  FILLER.
             03  A-SSOK  PIC  9(001).
             03  A-ESOK  PIC  9(001).
           02  FILLER.
             03  A-SHCD  PIC  9(004).
             03  A-EHCD  PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SNGP.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "204" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "RN" "1" "19" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "5" "20" "28" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "7" "20" "34" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" " " "10" "0" "37" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0104C-MID" "X" "10" "14" "22" " " "04C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0204C-MID" "X" "10" "36" "15" "0104C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "X" "12" "20" "19" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "X" "14" "20" "22" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "X" "20" "40" "22" "06C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "24" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "7" "0" "12" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNEN" "9" "7" "21" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNEN" BY REFERENCE W-SNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SGET" "9" "7" "26" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SPEY" "9" "7" "31" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENEN" "9" "7" "40" "2" "A-SPEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENEN" BY REFERENCE W-ENEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EGET" "9" "7" "45" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EPEY" "9" "7" "50" "2" "A-EGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "10" "50" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "12" "0" "2" "A-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SSOK" "9" "12" "33" "1" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SSOK" BY REFERENCE W-SSOK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ESOK" "9" "12" "38" "1" "A-SSOK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ESOK" BY REFERENCE W-ESOK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "14" "0" "8" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHCD" "9" "14" "30" "4" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHCD" BY REFERENCE W-SHCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EHCD" "9" "14" "38" "4" "A-SHCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EHCD" BY REFERENCE W-EHCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "57" "1" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SNGP" " " "5" "0" "6" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SNGP" "9" "5" "30" "2" " " "D-SNGP" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-SNGP" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SNGP" "9" "5" "35" "2" "01D-SNGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SNGP" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SNGP" "9" "5" "40" "2" "02D-SNGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-SNGP" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
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
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           MOVE 9 TO W-ESOK.
           MOVE 9999 TO W-EHCD.
           CALL "DB_F_Open" USING
            "INPUT" JSTRRF_PNAME1 "SHARED" BY REFERENCE
            JSTRRF_IDLST "0".
      *           READ JSTRRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSTRRF_PNAME1 BY REFERENCE JSTRR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JSTRRF_IDLST JSTRRF_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE JSTRR-90 TO W-SDAT.
           CALL "DB_F_Close" USING
            BY REFERENCE JSTRRF_IDLST JSTRRF_PNAME1.
      *
           COPY LIBCPR.
           ACCEPT W-NGPS FROM DATE.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NGP TO W-EDAT.
           MOVE W-SDAT TO W-NGP.
           CALL "SD_Output" USING "D-SNGP" D-SNGP "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = "00" AND "01" AND "06"
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-10
           END-IF
           IF  ESTAT NOT = "00" AND "01" AND "06"
               GO TO M-15
           END-IF
           IF  W-SGET < 1 OR > 12
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-SPEY "A-SPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-15
           END-IF
           IF  ESTAT NOT = "00" AND "01" AND "06"
               GO TO M-20
           END-IF
           IF  W-SPEY < 1 OR > 31
               GO TO M-20
           END-IF
      *
           MOVE ZERO TO W-SNEN1.
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF
           IF  W-SNGP < W-SDAT OR > W-EDAT
               GO TO M-10
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-20
           END-IF
           IF  ESTAT NOT = "00" AND "01" AND "06"
               GO TO M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-25
           END-IF
           IF  ESTAT NOT = "00" AND "01" AND "06"
               GO TO M-30
           END-IF
           IF  W-EGET < 1 OR > 12
               GO TO M-30
           END-IF.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-EPEY "A-EPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-30
           END-IF
           IF  ESTAT NOT = "00" AND "01" AND "06"
               GO TO M-35
           END-IF
           IF  W-EPEY < 1 OR > 31
               GO TO M-35
           END-IF
      *
           MOVE ZERO TO W-ENEN1.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF
           IF  W-ENGP < W-SNGP OR > W-EDAT
               GO TO M-25
           END-IF.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-35
           END-IF
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-40
           END-IF
           IF  W-SEN NOT = 0 AND 1 AND 9
               GO TO M-40
           END-IF.
       M-45.
           CALL "SD_Accept" USING BY REFERENCE A-SSOK "A-SSOK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-40
           END-IF
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-45
           END-IF.
       M-50.
           CALL "SD_Accept" USING BY REFERENCE A-ESOK "A-ESOK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-45
           END-IF
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-50
           END-IF
           IF  W-SSOK > W-ESOK
               GO TO M-50
           END-IF.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD "A-SHCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-50
           END-IF
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-55
           END-IF.
       M-60.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD "A-EHCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-55
           END-IF
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-60
           END-IF
           IF  W-SHCD > W-EHCD
               GO TO M-60
           END-IF.
       M-65.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-60
           END-IF
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-65
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-65
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JT-WK03_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" JT-WK03_PNAME1 " " BY REFERENCE JT-WK03_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" JSTRRF_PNAME1 "SHARED" BY REFERENCE
            JSTRRF_IDLST "0".
       M-70.
      *           READ  JSTRRF      AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSTRRF_PNAME1 BY REFERENCE JSTRR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-90
           END-IF
           IF  JSTRR-03  NOT  =   0  AND  3  AND 7
               GO  TO  M-70
           END-IF
           IF  JSTRR-17  NOT  =   1
               GO  TO  M-70
           END-IF
           IF (JSTRR-1211(01) =  0)  AND  (JSTRR-1211(02) =  0)  AND
              (JSTRR-1211(03) =  0)  AND  (JSTRR-1211(04) =  0)  AND
              (JSTRR-1211(05) =  0)  AND  (JSTRR-1211(06) =  0)  AND
              (JSTRR-1211(07) =  0)  AND  (JSTRR-1211(08) =  0)  AND
              (JSTRR-1211(09) =  0)  AND  (JSTRR-1211(10) =  0)
               GO  TO  M-70.
           MOVE  JSTRR-16  TO  W-16.
           IF  W-16         =  2
               MOVE  1         TO  W-16
           END-IF
           IF  W-SEN    NOT =  9
               IF  W-16      NOT  =  W-SEN
                   GO  TO  M-70
               END-IF
           END-IF
           IF  JSTRR-07      <  W-SSOK  OR  >  W-ESOK
               GO  TO  M-70
           END-IF
           MOVE JSTRR-09 TO W-HCD.
           IF  W-HCDD        <  W-SHCD  OR  >  W-EHCD
               GO  TO  M-70
           END-IF
           IF  JSTRR-05      <  W-SNGP  OR  >  W-ENGP
               GO  TO  M-70
           END-IF
           MOVE  SPACE     TO  W03-R.
           INITIALIZE          W03-R.
           MOVE  JSTRR-R   TO  W03-R.
      *           WRITE    W03-R.
      *//////////////
           CALL "DB_Insert" USING
            JT-WK03_PNAME1 JT-WK03_LNAME W03-R RETURNING RET.
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF
           GO  TO  M-70.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK03_IDLST JT-WK03_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSTRRF_IDLST JSTRRF_PNAME1.
           IF  W-DC = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

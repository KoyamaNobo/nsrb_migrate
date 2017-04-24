       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JTT01U.
      *********************************************************
      *    PROGRAM         :  ‘q•ÊÝŒÉƒ[ƒN@ì¬            *
      *                    :  (NJZAI¨WK0256)                 *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. NEAC-SYSTEM3100.
       OBJECT-COMPUTER. NEAC-SYSTEM3100.
       DATA    DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT                 PIC  X(2).
       77  W-NGP                    PIC  9(06).
       77  W-FILE                   PIC  X(13).
       77  WK0256ID                 PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1              PIC  X(003).
           02  STN-NO2              PIC  X(003).
       01  W-FID.
           02  W-FID1               PIC  X(006) VALUE "WK0256".
           02  W-FID2               PIC  X(003).
       01  W-DATA.
           02  W-SEN          PIC  9(01).
           02  W-SU           PIC S9(06).
           02  CNT            PIC  9(02).
           02  W-SC           PIC  9(01).
           02  W-ASID.
             03  W-ASI   OCCURS   5.
               04  W-SID   OCCURS  10.
                 05  W-SI     PIC  X(004).
           02  W-MSI.
             03  F            PIC  X(040) VALUE
                  "0000      SS   S   M   L  LL  XL XXLXXXL".
             03  F            PIC  X(040) VALUE
                  "                            28.029.030.0".
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.0    ".
             03  F            PIC  X(040) VALUE
                  "24.024.525.025.526.026.527.027.5        ".
       01  W-KEY.
           02  W-KURA               PIC  9(01).
           02  W-HCD                PIC  9(06).
           02  W-HCDD  REDEFINES W-HCD.
             03  W-HCD1             PIC  9(04).
             03  W-HCD2             PIC  9(02).
           02  W-MCD                PIC  9(06).
           02  W-MCDD  REDEFINES W-MCD.
             03  W-MCD1             PIC  9(04).
             03  W-MCD2             PIC  9(02).
       01  W-AREA.
           02  I                    PIC  9(02).
           02  W-FROM.
               03  W-FK             PIC  9(01).
               03  W-FH1            PIC  9(04).
           02  W-TO.
               03  W-TK             PIC  9(01).
               03  W-TH1            PIC  9(04).
           02  OKC                  PIC  9(01).
       COPY  LSTAT.
      *
           COPY   LNJZAI.
           COPY   LIHIM2.
           COPY   LICODE.
      *FD  ZAIKO
       01  ZAIKO_JTT01U.
           02  ZAIKO_PNAME1         PIC  X(009) VALUE SPACE.
           02  F                    PIC  X(001).
           02  ZAIKO_LNAME          PIC  X(012) VALUE "ZAIKO_JTT01U".
           02  F                    PIC  X(001).
           02  ZAIKO_KEY1           PIC  X(100) VALUE SPACE.
           02  ZAIKO_SORT           PIC  X(100) VALUE SPACE.
           02  ZAIKO_IDLST          PIC  X(100) VALUE SPACE.
           02  ZAIKO_RES            USAGE  POINTER.
       01  ZAIKO-R.
           02   ZAIKO-KURA          PIC 9(1).
           02   ZAIKO-HCD           PIC 9(6).
           02   ZAIKO-SMS           PIC N(16).
           02   ZAIKO-SIZ           PIC X(4).
           02   ZAIKO-ITF           PIC X(16).
           02   ZAIKO-SU            PIC S9(6).
           02   ZAIKO-ISU           PIC 9(03).
           02   ZAIKO-JAN           PIC X(13).
           02   ZAIKO-BC            PIC 9(06).
           02   ZAIKO-BMC           PIC 9(02).
           02   ZAIKO-BMNO          PIC 9(01).
           02   ZAIKO-NGP           PIC 9(06).
           02   FILLER              PIC X(32).
           02   FILLER              PIC X(128).
       77  F                        PIC X(001).
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
           02  FILLER  PIC  X(22) VALUE
                "                      ".
           02  FILLER  PIC  X(20) VALUE
               "‘q•ÊÝŒÉƒ[ƒN@’Šo".
           02  FILLER  PIC  X(43) VALUE
               "‚i‚`‚mº°ÄÞ  –³‚µ=1 , —L‚è=2 , ‘SŒ=3   ØÀ°Ý".
           02  FILLER  PIC  X(08) VALUE
               "‘q  •i–¼".
           02  FILLER  PIC  X(08) VALUE  "‚e‚q‚n‚l".
           02  FILLER  PIC  X(04) VALUE  "‚s‚n".
           02  FILLER  PIC  X(25) VALUE  "Šm”F(OK=1,NO=9)-->   ØÀ°Ý".
       01  C-ACP.
           02  ACP-SEN        PIC 9(01).
           02  ACP-FK         PIC 9(01).
           02  ACP-TK         PIC 9(01).
           02  ACP-FH1        PIC 9(04).
           02  ACP-TH1        PIC 9(04).
           02  ACP-OKC        PIC 9(01).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC X(17) VALUE
                  "***  ËÝÒ² Å¼  ***".
             03  E-ME2   PIC X(23) VALUE
                  "***  ËÝÒ² »²½Þ ´×°  ***".
             03  E-ME3   PIC X(22) VALUE
                  "***  ËÝÒ² »²½Þ Å¼  ***".
             03  E-KEY   PIC X(08).
       COPY  LSSEM.
       PROCEDURE   DIVISION.
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
            "C-MID" " " "0" "0" "130" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "RX" "1" "27" "22" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "1" "28" "20" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "11" "13" "43" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "14" "22" "8" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "X" "16" "13" "8" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "X" "18" "13" "4" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "X" "23" "41" "25" "06C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "12" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SEN" "9" "11" "51" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FK" "9" "16" "23" "1" "ACP-SEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FK" BY REFERENCE W-FK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TK" "9" "18" "23" "1" "ACP-FK" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TK" BY REFERENCE W-TK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FH1" "9" "16" "27" "4" "ACP-TK" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FH1" BY REFERENCE W-FH1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TH1" "9" "18" "27" "4" "ACP-FH1" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TH1" BY REFERENCE W-TH1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "23" "61" "1" "ACP-TH1" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "70" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "70" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "23" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "22" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "45" "8" "E-ME3" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE NJZAI-KEY "8" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       MR-RTN.
           PERFORM  INI-RTN     THRU   INI-EX.
           IF  ESTAT     =  "P9"
               CALL "DB_Close"
               STOP  RUN
           END-IF
           PERFORM  UPD1-RTN    THRU   UPD1-EX.
           PERFORM  END-RTN     THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
       INI-RTN.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           INITIALIZE  W-AREA.
       INI-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-SEN "ACP-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  INI-EX
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-010
           END-IF
           CALL "SD_Output" USING "ACP-SEN" ACP-SEN "p" RETURNING RESU.
           IF  W-SEN  NOT =  1  AND  2  AND  3
               GO  TO  INI-010
           END-IF.
       INI-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-FK "ACP-FK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  INI-EX
           END-IF
           IF  ESTAT  =  "09"
               GO  TO  INI-010
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-030
           END-IF
           CALL "SD_Output" USING "ACP-FK" ACP-FK "p" RETURNING RESU.
       INI-050.
           CALL "SD_Accept" USING BY REFERENCE ACP-TK "ACP-TK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-030
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-050
           END-IF
           CALL "SD_Output" USING "ACP-TK" ACP-TK "p" RETURNING RESU.
           IF  W-FK  >  W-TK
               GO  TO  INI-050
           END-IF.
       INI-055.
           CALL "SD_Accept" USING BY REFERENCE ACP-FH1 "ACP-FH1" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-050
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-055
           END-IF
           CALL "SD_Output" USING "ACP-FH1" ACP-FH1 "p" RETURNING RESU.
       INI-060.
           CALL "SD_Accept" USING BY REFERENCE ACP-TH1 "ACP-TH1" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-055
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-060
           END-IF
           CALL "SD_Output" USING "ACP-TH1" ACP-TH1 "p" RETURNING RESU.
           IF  W-FH1   >  W-TH1
               GO  TO  INI-060
           END-IF.
       INI-510.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO INI-060
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-510
           END-IF
           CALL "SD_Output" USING "ACP-OKC" ACP-OKC "p" RETURNING RESU.
           IF  OKC  NOT =  "1"  AND  "9"
               GO  TO  INI-510
           END-IF
           IF  OKC  =  "9"
               GO  TO  INI-RTN
           END-IF
      *
           ACCEPT W-NGP FROM DATE.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO ZAIKO_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
           CALL "DB_F_Open" USING
            "OUTPUT" ZAIKO_PNAME1 " " BY REFERENCE ZAIKO_IDLST "0".
       INI-EX.
            EXIT.
       UPD1-RTN.
           INITIALIZE                 NJZAI-KEY.
           MOVE  ZERO             TO  W-MCD.
           MOVE  W-FK             TO  NJZAI-01.
           MOVE  W-FH1            TO  W-MCD1.
           MOVE  W-MCD            TO  NJZAI-02.
      *           START  NJZAI  KEY  NOT <  NJZAI-KEY  INVALID
               GO  TO  UPD1-EX.
           MOVE  W-MSI            TO  W-ASID.
       UPD1-010.
      *           READ  NJZAI  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD1-EX
           END-IF
           MOVE  NJZAI-02         TO  W-HCD.
           IF  NJZAI-01  >  W-TK
               GO  TO  UPD1-EX
           END-IF
           IF  W-HCD1    <  W-FH1 OR    >   W-TH1
               GO  TO  UPD1-010
           END-IF
           MOVE  0                TO  CNT.
       UPD1-020.
           ADD   1                TO  CNT.
           IF  CNT      >  10
               GO  TO  UPD1-010
           END-IF
           COMPUTE  W-SU  =  NJZAI-0411(CNT)  -  NJZAI-0511(CNT)
                          +  NJZAI-0611(CNT)  +  NJZAI-0711(CNT)
                          -  NJZAI-0811(CNT)  +  NJZAI-1111(CNT).
           IF  W-SU     =  ZERO
               GO  TO  UPD1-020
           END-IF
      *
           MOVE  NJZAI-02     TO  HI-MHCD HI-HCD.
      *           READ  HI2-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO UPD1-EX
           END-IF
           MOVE  0            TO  HI-S4(10).
           IF  HI-SSC   NOT =  0
               IF (HI-SS(2) NOT = ZERO) OR (HI-SS(3) NOT = ZERO) OR
                  (HI-SS(4) NOT = ZERO)
                   CALL "SD_Output" USING
                    "E-ME2" E-ME2 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-KEY" E-KEY "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   GO TO UPD1-EX
               END-IF
           END-IF
           IF  HI-S(NJZAI-03,CNT) = 0
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO UPD1-EX
           END-IF
      *
           MOVE SPACE TO CODE-KEY2.
           MOVE NJZAI-02 TO CODE-HCD20.
           MOVE NJZAI-03 TO CODE-SIZ2.
           MOVE CNT TO CODE-SNO2.
      *           START CODEF KEY NOT < CODE-KEY2 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CODEF_PNAME1 "CODE-KEY2" " NOT < " CODE-KEY2 RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO CODE-JAN CODE-ITF
               GO TO UPD1-040
           END-IF.
       UPD1-030.
      *           READ CODEF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO CODE-JAN CODE-ITF
               GO TO UPD1-040
           END-IF
           IF  CODE-TCD NOT = ZERO
               GO TO UPD1-030
           END-IF
           IF (NJZAI-02 NOT = CODE-HCD20) OR
              (NJZAI-03 NOT = CODE-SIZ2) OR
              (CNT NOT = CODE-SNO2)
               MOVE ZERO TO CODE-JAN CODE-ITF
           END-IF.
       UPD1-040.
           IF  W-SEN    =  1
               IF  CODE-JAN  NOT  =  ZERO
                   GO TO UPD1-020
               END-IF
           END-IF
           IF  W-SEN    =  2
               IF  CODE-JAN       =  ZERO
                   GO TO UPD1-020
               END-IF
           END-IF
           MOVE  SPACE     TO  ZAIKO-R.
           INITIALIZE  ZAIKO-R.
           MOVE  NJZAI-01  TO  ZAIKO-KURA.
           MOVE  NJZAI-02  TO  ZAIKO-HCD.
           IF  HI-SMS = SPACE
               MOVE HI-NAME TO ZAIKO-SMS
           ELSE
               MOVE  HI-SMS    TO  ZAIKO-SMS
           END-IF
           MOVE  NJZAI-03  TO  W-SC.
           IF  HI-SSC   =  0
               ADD   1         TO  W-SC
           END-IF
           MOVE  W-SI(W-SC,CNT) TO  ZAIKO-SIZ.
           MOVE  W-SU      TO  ZAIKO-SU.
           MOVE  HI-ISU    TO  ZAIKO-ISU.
           MOVE  HI-BC     TO  ZAIKO-BC.
           MOVE  HI-BMC    TO  ZAIKO-BMC.
           MOVE  HI-BMNO   TO  ZAIKO-BMNO.
           IF  CODE-JAN  NOT  =  ZERO
               MOVE  CODE-JAN  TO  ZAIKO-JAN
           END-IF
           IF  CODE-ITF  NOT  =  ZERO
               MOVE  CODE-ITF  TO  ZAIKO-ITF
           END-IF
           MOVE  W-NGP     TO  ZAIKO-NGP.
      *           WRITE    ZAIKO-R.
      *//////////////
           CALL "DB_Insert" USING
            ZAIKO_PNAME1 ZAIKO_LNAME ZAIKO-R RETURNING RET.
           GO  TO  UPD1-020.
       UPD1-EX.
           EXIT.
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE ZAIKO_IDLST ZAIKO_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       END-EX.
           EXIT.

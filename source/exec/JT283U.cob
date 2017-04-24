       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JT283U.
       AUTHOR.          MAYUMI.I.
      *********************************************************
      *    PROGRAM         :  ‘q•ÊÝŒÉƒ[ƒN@ì¬            *
      *    PRINTER TYPE    :  JIPS                            *
      *    DATA WRITTN     :  96/03/06                        *
      *    COMPILE TYPE    :  CBL85 (74MODE)                  *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. NEAC-SYSTEM3100.
       OBJECT-COMPUTER. NEAC-SYSTEM3100.
       DATA    DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT                 PIC  X(2).
       77  WK0128ID                 PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1              PIC  X(003).
           02  STN-NO2              PIC  X(003).
       01  W-FID.
           02  W-FID1               PIC  X(006) VALUE "WK0128".
           02  W-FID2               PIC  X(003).
       01  W-SUD.
           02  W-SU                 PIC S9(06)  OCCURS  10.
       01  ZERO-SW                  PIC  9(01).
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
           02  W-ZCHK               PIC  9(01).
           02  W-SEN                PIC  9(01).
           02  W-FROM.
               03  W-FK             PIC  9(01).
               03  W-FH1            PIC  9(04).
               03  W-FH2            PIC  9(04).
               03  W-FH3            PIC  9(04).
               03  W-FH4            PIC  9(04).
               03  W-FH5            PIC  9(04).
               03  W-FH6            PIC  9(04).
           02  W-TO.
               03  W-TK             PIC  9(01).
               03  W-TH             PIC  9(04).
               03  W-TH1            PIC  9(04).
               03  W-TH2            PIC  9(04).
               03  W-TH3            PIC  9(04).
               03  W-TH4            PIC  9(04).
               03  W-TH5            PIC  9(04).
               03  W-TH6            PIC  9(04).
           02  OKC                  PIC  9(01).
      ***
       COPY  LWMSG.
      ***
      *
       COPY   LNJZAI.
       COPY   LJMST3.
       COPY   LJNYZ.
       COPY   LIHIM2.
      *FD  JT-YZAI
       01  JT-YZAI_JT283U.
           02  JT-YZAI_PNAME1       PIC  X(009) VALUE SPACE.
           02  F                    PIC  X(001).
           02  JT-YZAI_LNAME        PIC  X(014) VALUE "JT-YZAI_JT283U".
           02  F                    PIC  X(001).
           02  JT-YZAI_KEY1         PIC  X(100) VALUE SPACE.
           02  JT-YZAI_SORT         PIC  X(100) VALUE SPACE.
           02  JT-YZAI_IDLST        PIC  X(100) VALUE SPACE.
           02  JT-YZAI_RES          USAGE  POINTER.
      *
       01  YZAI-R.
           02   YZAI-01             PIC 9(1).
           02   YZAI-02             PIC 9(6).
           02   YZAI-03             PIC 9(1).
           02   YZAI-04.
                03   YZAI-04        OCCURS  10.
                    04   YZAI-0411  PIC S9(6).
           02   FILLER              PIC X(14).
           02   YZAI-91.
                03   YZAI-911       PIC 9(01).
                03   YZAI-912       PIC 9(01).
           02   YZAI-92             PIC 9(01).
           02   YZAI-93             PIC 9(01).
           02   FILLER              PIC X(42).
       77  F                        PIC X(01).
      *
       77  ESTAT                    PIC  X(002).
       77  RESU                     PIC  9(001).
       77  RET                      PIC  9(001) VALUE ZERO.
       77  USER_ID                  PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE          PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL      PIC  X(12) VALUE "CLEAR SCREEN".
       01  CLR-01.
           02  CLR-ZCHK  PIC  X(01) VALUE " ".
           02  CLR-SEN   PIC  X(01) VALUE " ".
           02  CLR-FK    PIC  X(01) VALUE " ".
           02  CLR-TK    PIC  X(01) VALUE " ".
           02  CLR-FH1   PIC  X(04) VALUE "    ".
           02  CLR-TH1   PIC  X(04) VALUE "    ".
           02  CLR-FH2   PIC  X(04) VALUE "    ".
           02  CLR-TH2   PIC  X(04) VALUE "    ".
           02  CLR-FH3   PIC  X(04) VALUE "    ".
           02  CLR-TH3   PIC  X(04) VALUE "    ".
           02  CLR-FH4   PIC  X(04) VALUE "    ".
           02  CLR-TH4   PIC  X(04) VALUE "    ".
           02  CLR-FH5   PIC  X(04) VALUE "    ".
           02  CLR-TH5   PIC  X(04) VALUE "    ".
           02  CLR-FH6   PIC  X(04) VALUE "    ".
           02  CLR-TH6   PIC  X(04) VALUE "    ".
      ***
       01  DSP-AREA.
           02  FILLER    PIC  X(22) VALUE
                "                      ".
           02  FILLER    PIC  X(20) VALUE
               "‘q•ÊÝŒÉƒ[ƒN@’Šo".
           02  FILLER    PIC  X(21) VALUE "ÝŒÉ”(Žw}ŠÜ)    = 1".
           02  FILLER    PIC  X(21) VALUE "      (Žw}–³)    = 2".
           02  FILLER    PIC  X(21) VALUE "—L Œø Ý ŒÉ ”    = 3".
           02  FILLER    PIC  X(27) VALUE "o‰×‰Â”\¥“üŒÉ—\’è = 4 ...  ".
           02  FILLER    PIC  X(14) VALUE "‹³@@ˆç@ = 0".
           02  FILLER    PIC  X(14) VALUE "ƒ@[@ƒN = 1".
           02  FILLER    PIC  X(14) VALUE "ˆê@@”Ê@ = 2".
           02  FILLER    PIC  X(20) VALUE "‘S@@Œ@ = 9 ...  ".
           02  FILLER    PIC  X(50) VALUE
               "‘q  •i–¼‚P      ‚Q      ‚R      ‚S      ‚T      ‚U".
           02  FILLER    PIC  X(08) VALUE  "‚e‚q‚n‚l".
           02  FILLER    PIC  X(04) VALUE  "‚s‚n".
           02  FILLER    PIC  X(25) VALUE  "Šm”F(OK=1,NO=9)-->   ØÀ°Ý".
       01  ACP-AREA.
           02  ACP-ZCHK  PIC 9(01).
           02  ACP-SEN   PIC 9(01).
           02  ACP-FK    PIC 9(01).
           02  ACP-TK    PIC 9(01).
           02  ACP-FH1   PIC 9(04).
           02  ACP-TH1   PIC 9(04).
           02  ACP-FH2   PIC 9(04).
           02  ACP-TH2   PIC 9(04).
           02  ACP-FH3   PIC 9(04).
           02  ACP-TH3   PIC 9(04).
           02  ACP-FH4   PIC 9(04).
           02  ACP-TH4   PIC 9(04).
           02  ACP-FH5   PIC 9(04).
           02  ACP-TH5   PIC 9(04).
           02  ACP-FH6   PIC 9(04).
           02  ACP-TH6   PIC 9(04).
           02  ACP-OKC   PIC 9(01).
      *
       01  DISP-MSG-SPACE1.
           02  FILLER    PIC X(40)     VALUE " ".
      ***
       COPY  LSMSG.
      ***
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
      *CLR-01
       CALL "SD_Init" USING 
            "CLR-01" " " "0" "0" "52" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-ZCHK" "X" "10" "36" "1" " " "CLR-01"  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-SEN" "X" "10" "54" "1" "CLR-ZCHK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-FK" "X" "16" "23" "1" "CLR-SEN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TK" "X" "18" "23" "1" "CLR-FK" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-FH1" "X" "16" "27" "4" "CLR-TK" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TH1" "X" "18" "27" "4" "CLR-FH1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-FH2" "X" "16" "35" "4" "CLR-TH1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TH2" "X" "18" "35" "4" "CLR-FH2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-FH3" "X" "16" "43" "4" "CLR-TH2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TH3" "X" "18" "43" "4" "CLR-FH3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-FH4" "X" "16" "51" "4" "CLR-TH3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TH4" "X" "18" "51" "4" "CLR-FH4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-FH5" "X" "16" "59" "4" "CLR-TH4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TH5" "X" "18" "59" "4" "CLR-FH5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-FH6" "X" "16" "67" "4" "CLR-TH5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TH6" "X" "18" "67" "4" "CLR-FH6" " "  RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "281" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" "RX" "1" "27" "22" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" "X" "1" "28" "20" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" "X" "4" "13" "21" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-AREA" "X" "6" "13" "21" "03DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-AREA" "X" "8" "13" "21" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-AREA" "X" "10" "13" "27" "05DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-AREA" "X" "4" "44" "14" "06DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-AREA" "X" "6" "44" "14" "07DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-AREA" "X" "8" "44" "14" "08DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-AREA" "X" "10" "44" "20" "09DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "11DSP-AREA" "X" "14" "22" "50" "10DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "12DSP-AREA" "X" "16" "13" "8" "11DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "13DSP-AREA" "X" "18" "13" "4" "12DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "14DSP-AREA" "X" "23" "41" "25" "13DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "53" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ZCHK" "9" "10" "39" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-ZCHK" BY REFERENCE W-ZCHK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SEN" "9" "10" "63" "1" "ACP-ZCHK" " " RETURNING RESU.
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
            "ACP-FH2" "9" "16" "35" "4" "ACP-TH1" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FH2" BY REFERENCE W-FH2 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TH2" "9" "18" "35" "4" "ACP-FH2" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TH2" BY REFERENCE W-TH2 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FH3" "9" "16" "43" "4" "ACP-TH2" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FH3" BY REFERENCE W-FH3 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TH3" "9" "18" "43" "4" "ACP-FH3" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TH3" BY REFERENCE W-TH3 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FH4" "9" "16" "51" "4" "ACP-TH3" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FH4" BY REFERENCE W-FH4 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TH4" "9" "18" "51" "4" "ACP-FH4" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TH4" BY REFERENCE W-TH4 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FH5" "9" "16" "59" "4" "ACP-TH4" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FH5" BY REFERENCE W-FH5 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TH5" "9" "18" "59" "4" "ACP-FH5" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TH5" BY REFERENCE W-TH5 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FH6" "9" "16" "67" "4" "ACP-TH5" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FH6" BY REFERENCE W-FH6 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TH6" "9" "18" "67" "4" "ACP-FH6" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TH6" BY REFERENCE W-TH6 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "23" "61" "1" "ACP-TH6" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *DISP-MSG-SPACE1
       CALL "SD_Init" USING 
            "DISP-MSG-SPACE1" " " "24" "0" "40" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-SPACE1" "X" "24" "1" "40" " " "DISP-MSG-SPACE1"
            RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ****************************
      ***  Ò ² Ý  R T N        ***
      ****************************
       MR-RTN.
           PERFORM  INI-RTN     THRU   INI-EX.
           IF  ESTAT     =  "P9"
               CALL "DB_Close"
               STOP  RUN
           END-IF
           PERFORM  UPD1-RTN    THRU   UPD1-EX.
           IF  W-ZCHK    =  3  OR  4
               PERFORM  UPD2-RTN    THRU   UPD2-EX
               PERFORM  UPD3-RTN    THRU   UPD3-EX
           END-IF
           PERFORM  END-RTN     THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
      *********************************
      ***   ²Æ¼¬Ù   R T N           ***
      *********************************
       INI-RTN.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           INITIALIZE  W-AREA.
       INI-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-ZCHK "ACP-ZCHK"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  INI-EX
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-010
           END-IF
           CALL "SD_Output" USING
            "ACP-ZCHK" ACP-ZCHK "p" RETURNING RESU.
           IF  W-ZCHK NOT =  1 AND 2 AND 3 AND 4
               GO  TO  INI-010
           END-IF.
       INI-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-SEN "ACP-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-010
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-020
           END-IF
           CALL "SD_Output" USING "ACP-SEN" ACP-SEN "p" RETURNING RESU.
           IF  W-SEN  NOT =  0  AND 1 AND 2  AND  9
               GO  TO  INI-020
           END-IF
           IF  W-ZCHK     =  3  OR 4
               MOVE   9        TO  W-FK  W-TK
               CALL "SD_Output" USING "CLR-FK" CLR-FK "p" RETURNING RESU
               CALL "SD_Output" USING "CLR-TK" CLR-TK "p" RETURNING RESU
               GO  TO  INI-055
           END-IF.
       INI-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-FK "ACP-FK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-020
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
               IF  W-ZCHK     =  3  OR 4
                   GO  TO  INI-020
               ELSE
                   GO  TO  INI-050
               END-IF
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
           END-IF
           IF  W-TH1 = 9999
               MOVE W-TH1 TO W-TH
               MOVE ZERO TO W-FH2 W-TH2 W-FH3 W-TH3 W-FH4 W-TH4
                            W-FH5 W-TH5 W-FH6 W-TH6
               CALL "SD_Output" USING
                "CLR-FH2" CLR-FH2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH2" CLR-TH2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH3" CLR-FH3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH3" CLR-TH3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH4" CLR-FH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH4" CLR-TH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH5" CLR-FH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH5" CLR-TH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH6" CLR-FH6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH6" CLR-TH6 "p" RETURNING RESU
               GO TO INI-510
           END-IF.
       INI-070.
           CALL "SD_Accept" USING BY REFERENCE ACP-FH2 "ACP-FH2" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-060
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-070
           END-IF
           IF  W-FH2 = ZERO
               MOVE W-TH1 TO W-TH
               MOVE ZERO TO W-FH2 W-TH2 W-FH3 W-TH3 W-FH4 W-TH4
                            W-FH5 W-TH5 W-FH6 W-TH6
               CALL "SD_Output" USING
                "CLR-FH2" CLR-FH2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH2" CLR-TH2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH3" CLR-FH3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH3" CLR-TH3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH4" CLR-FH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH4" CLR-TH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH5" CLR-FH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH5" CLR-TH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH6" CLR-FH6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH6" CLR-TH6 "p" RETURNING RESU
               GO TO INI-510
           END-IF
           CALL "SD_Output" USING "ACP-FH2" ACP-FH2 "p" RETURNING RESU.
           IF  W-FH2   <  W-TH1
               GO  TO  INI-070
           END-IF.
       INI-080.
           CALL "SD_Accept" USING BY REFERENCE ACP-TH2 "ACP-TH2" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-070
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-080
           END-IF
           CALL "SD_Output" USING "ACP-TH2" ACP-TH2 "p" RETURNING RESU.
           IF  W-FH2   >  W-TH2
               GO  TO  INI-080
           END-IF
           IF  W-TH2 = 9999
               MOVE W-TH2 TO W-TH
               MOVE ZERO TO W-FH3 W-TH3 W-FH4 W-TH4 W-FH5 W-TH5
                            W-FH6 W-TH6
               CALL "SD_Output" USING
                "CLR-FH3" CLR-FH3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH3" CLR-TH3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH4" CLR-FH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH4" CLR-TH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH5" CLR-FH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH5" CLR-TH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH6" CLR-FH6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH6" CLR-TH6 "p" RETURNING RESU
               GO TO INI-510
           END-IF.
       INI-090.
           CALL "SD_Accept" USING BY REFERENCE ACP-FH3 "ACP-FH3" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-080
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-090
           END-IF
           IF  W-FH3 = ZERO
               MOVE W-TH2 TO W-TH
               MOVE ZERO TO W-FH3 W-TH3 W-FH4 W-TH4 W-FH5 W-TH5
                            W-FH6 W-TH6
               CALL "SD_Output" USING
                "CLR-FH3" CLR-FH3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH3" CLR-TH3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH4" CLR-FH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH4" CLR-TH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH5" CLR-FH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH5" CLR-TH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH6" CLR-FH6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH6" CLR-TH6 "p" RETURNING RESU
               GO TO INI-510
           END-IF
           CALL "SD_Output" USING "ACP-FH3" ACP-FH3 "p" RETURNING RESU.
           IF  W-FH3   <  W-TH2
               GO  TO  INI-090
           END-IF.
       INI-100.
           CALL "SD_Accept" USING BY REFERENCE ACP-TH3 "ACP-TH3" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-090
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-100
           END-IF
           CALL "SD_Output" USING "ACP-TH3" ACP-TH3 "p" RETURNING RESU.
           IF  W-FH3   >  W-TH3
               GO  TO  INI-100
           END-IF
           IF  W-TH3 = 9999
               MOVE W-TH3 TO W-TH
               MOVE ZERO TO W-FH4 W-TH4 W-FH5 W-TH5 W-FH6 W-TH6
               CALL "SD_Output" USING
                "CLR-FH4" CLR-FH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH4" CLR-TH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH5" CLR-FH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH5" CLR-TH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH6" CLR-FH6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH6" CLR-TH6 "p" RETURNING RESU
               GO TO INI-510
           END-IF.
       INI-110.
           CALL "SD_Accept" USING BY REFERENCE ACP-FH4 "ACP-FH4" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-100
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-110
           END-IF
           IF  W-FH4 = ZERO
               MOVE W-TH3 TO W-TH
               MOVE ZERO TO W-FH4 W-TH4 W-FH5 W-TH5 W-FH6 W-TH6
               CALL "SD_Output" USING
                "CLR-FH4" CLR-FH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH4" CLR-TH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH5" CLR-FH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH5" CLR-TH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH6" CLR-FH6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH6" CLR-TH6 "p" RETURNING RESU
               GO TO INI-510
           END-IF
           CALL "SD_Output" USING "ACP-FH4" ACP-FH4 "p" RETURNING RESU.
           IF  W-FH4   <  W-TH3
               GO  TO  INI-110
           END-IF.
       INI-120.
           CALL "SD_Accept" USING BY REFERENCE ACP-TH4 "ACP-TH4" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-110
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-120
           END-IF
           CALL "SD_Output" USING "ACP-TH4" ACP-TH4 "p" RETURNING RESU.
           IF  W-FH4   >  W-TH4
               GO  TO  INI-120
           END-IF
           IF  W-TH4 = 9999
               MOVE W-TH4 TO W-TH
               MOVE ZERO TO W-FH5 W-TH5 W-FH6 W-TH6
               CALL "SD_Output" USING
                "CLR-FH5" CLR-FH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH5" CLR-TH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH6" CLR-FH6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH6" CLR-TH6 "p" RETURNING RESU
               GO TO INI-510
           END-IF.
       INI-130.
           CALL "SD_Accept" USING BY REFERENCE ACP-FH5 "ACP-FH5" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-120
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-130
           END-IF
           IF  W-FH5 = ZERO
               MOVE W-TH4 TO W-TH
               MOVE ZERO TO W-FH5 W-TH5 W-FH6 W-TH6
               CALL "SD_Output" USING
                "CLR-FH5" CLR-FH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH5" CLR-TH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH6" CLR-FH6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH6" CLR-TH6 "p" RETURNING RESU
               GO TO INI-510
           END-IF
           CALL "SD_Output" USING "ACP-FH5" ACP-FH5 "p" RETURNING RESU.
           IF  W-FH5   <  W-TH4
               GO  TO  INI-130
           END-IF.
       INI-140.
           CALL "SD_Accept" USING BY REFERENCE ACP-TH5 "ACP-TH5" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-130
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-140
           END-IF
           CALL "SD_Output" USING "ACP-TH5" ACP-TH5 "p" RETURNING RESU.
           IF  W-FH5   >  W-TH5
               GO  TO  INI-140
           END-IF
           IF  W-TH5 = 9999
               MOVE W-TH5 TO W-TH
               MOVE ZERO TO W-FH6 W-TH6
               CALL "SD_Output" USING
                "CLR-FH6" CLR-FH6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH6" CLR-TH6 "p" RETURNING RESU
               GO TO INI-510
           END-IF.
       INI-150.
           CALL "SD_Accept" USING BY REFERENCE ACP-FH6 "ACP-FH6" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-140
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-150
           END-IF
           IF  W-FH6 = ZERO
               MOVE W-TH5 TO W-TH
               MOVE ZERO TO W-FH6 W-TH6
               CALL "SD_Output" USING
                "CLR-FH6" CLR-FH6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH6" CLR-TH6 "p" RETURNING RESU
               GO TO INI-510
           END-IF
           CALL "SD_Output" USING "ACP-FH6" ACP-FH6 "p" RETURNING RESU.
           IF  W-FH6   <  W-TH5
               GO  TO  INI-150
           END-IF.
       INI-160.
           CALL "SD_Accept" USING BY REFERENCE ACP-TH6 "ACP-TH6" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-150
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-160
           END-IF
           CALL "SD_Output" USING "ACP-TH6" ACP-TH6 "p" RETURNING RESU.
           IF  W-FH6   >  W-TH6
               GO  TO  INI-160
           END-IF
           MOVE W-TH6 TO W-TH.
       INI-510.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  NOT =  "09"
               GO TO INI-520
           END-IF
           IF  W-FH2 = ZERO
               GO  TO  INI-060
           END-IF
           IF  W-FH3 = ZERO
               GO  TO  INI-080
           END-IF
           IF  W-FH4 = ZERO
               GO  TO  INI-100
           END-IF
           IF  W-FH5 = ZERO
               GO  TO  INI-120
           END-IF
           IF  W-FH6 = ZERO
               GO  TO  INI-140
           END-IF
           GO  TO  INI-160.
       INI-520.
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
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JT-YZAI_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           IF  W-ZCHK     =  3 OR 4
               CALL "DB_F_Open" USING
                "INPUT" JMST3_PNAME1 "SHARED" BY REFERENCE JMST3_IDLST
                "1" "JMST3-KEY" BY REFERENCE JMST3-KEY
               CALL "DB_F_Open" USING
                "INPUT" JNYZ_PNAME1 "SHARED" BY REFERENCE JNYZ_IDLST "1"
                "JNYZ-KEY" BY REFERENCE JNYZ-KEY
           END-IF
           CALL "DB_F_Open" USING
            "OUTPUT" JT-YZAI_PNAME1 "EXCLUSIVE" BY REFERENCE
            JT-YZAI_IDLST "0".
       INI-EX.
            EXIT.
      ******************************
      ***   UPD1  RTN            ***
      ******************************
       UPD1-RTN.
      *
           INITIALIZE                 NJZAI-KEY.
           MOVE  ZERO             TO  W-MCD.
           MOVE  W-FK             TO  NJZAI-01.
           MOVE  W-FH1            TO  W-MCD1.
           MOVE  W-MCD            TO  NJZAI-02.
      *
      *           START  NJZAI  KEY  NOT <  NJZAI-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            NJZAI_PNAME1 "NJZAI-KEY" " NOT < " NJZAI-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  UPD1-EX
           END-IF.
       UPD1-010.
      ***  ‘q•ÊÝŒÉƒ}ƒXƒ^@‚q‚d‚`‚c
      *           READ  NJZAI  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD1-EX
           END-IF
      *
           MOVE  NJZAI-01         TO  W-KURA.
           MOVE  NJZAI-02         TO  W-HCD.
           IF  W-KURA    >  W-TK
               GO  TO  UPD1-EX
           END-IF
           IF  W-HCD1    >  W-TH
               GO  TO  UPD1-EX
           END-IF
           IF  W-HCD1    >= W-FH1 AND   <=  W-TH1
               GO  TO  UPD1-020
           END-IF
           IF  W-HCD1    >= W-FH2 AND   <=  W-TH2
               GO  TO  UPD1-020
           END-IF
           IF  W-HCD1    >= W-FH3 AND   <=  W-TH3
               GO  TO  UPD1-020
           END-IF
           IF  W-HCD1    >= W-FH4 AND   <=  W-TH4
               GO  TO  UPD1-020
           END-IF
           IF  W-HCD1    >= W-FH5 AND   <=  W-TH5
               GO  TO  UPD1-020
           END-IF
           IF  W-HCD1    >= W-FH6 AND   <=  W-TH6
               GO  TO  UPD1-020
           END-IF
           GO  TO  UPD1-010.
      *
       UPD1-020.
           PERFORM  ZC1-RTN      THRU  ZC1-EX.
           IF  ZERO-SW  =  0
               GO  TO  UPD1-010
           END-IF
      ***  •i–¼ƒ}ƒXƒ^@‚q‚d‚`‚c
           MOVE  NJZAI-02     TO  HI-MHCD HI-HCD.
      *           READ  HI2-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD1-010
           END-IF
           IF  W-SEN  =  0
               IF  HI-BC3   <  30  OR   >  39
                   GO  TO  UPD1-010
               END-IF
           END-IF
           IF  W-SEN  =  1
               IF  HI-BC3   <  20  OR   >  29
                   GO  TO  UPD1-010
               END-IF
           END-IF
           IF  W-SEN  =  2
               IF  HI-BC3   > 19
                   GO  TO  UPD1-010
               END-IF
           END-IF
      *
           MOVE  SPACE     TO  YZAI-R.
           INITIALIZE  YZAI-R.
           MOVE  NJZAI-01  TO  YZAI-01.
           MOVE  NJZAI-02  TO  YZAI-02.
           MOVE  NJZAI-03  TO  YZAI-03.
           PERFORM  MOV-RTN       THRU  MOV-EX.
           MOVE  HI-BC3    TO  YZAI-91.
           MOVE  W-ZCHK    TO  YZAI-92.
           PERFORM  WRI-RTN       THRU  WRI-EX.
           GO  TO  UPD1-010.
       UPD1-EX.
           EXIT.
      *
      ******************************
      ***   UPD2  RTN            ***
      ******************************
       UPD2-RTN.
      *
           INITIALIZE                 JMST3-KEY.
           MOVE  ZERO             TO  W-MCD.
           MOVE  W-FH1            TO  W-MCD1.
           MOVE  W-MCD            TO  JMST3-03.
      *
      *           START  JMST3  KEY  NOT <  JMST3-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            JMST3_PNAME1 "JMST3-KEY" " NOT < " JMST3-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  UPD2-EX
           END-IF.
       UPD2-010.
      ***  Žó’ƒ}ƒXƒ^@‚q‚d‚`‚c
      *           READ  JMST3  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST3_PNAME1 BY REFERENCE JMST3-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD2-EX
           END-IF
      *
           IF  JMST3-01  NOT =  5  AND  6
               GO  TO  UPD2-010
           END-IF
           MOVE  JMST3-03         TO  W-HCD.
           IF  W-HCD1    >  W-TH
               GO  TO  UPD2-EX
           END-IF
           IF  W-HCD1    >= W-FH1 AND   <=  W-TH1
               GO  TO  UPD2-020
           END-IF
           IF  W-HCD1    >= W-FH2 AND   <=  W-TH2
               GO  TO  UPD2-020
           END-IF
           IF  W-HCD1    >= W-FH3 AND   <=  W-TH3
               GO  TO  UPD2-020
           END-IF
           IF  W-HCD1    >= W-FH4 AND   <=  W-TH4
               GO  TO  UPD2-020
           END-IF
           IF  W-HCD1    >= W-FH5 AND   <=  W-TH5
               GO  TO  UPD2-020
           END-IF
           IF  W-HCD1    >= W-FH6 AND   <=  W-TH6
               GO  TO  UPD2-020
           END-IF
           GO  TO  UPD2-010.
      *
       UPD2-020.
           PERFORM  ZC2-RTN      THRU  ZC2-EX.
           IF  ZERO-SW  =  0
               GO  TO  UPD2-010
           END-IF
      ***  •i–¼ƒ}ƒXƒ^@‚q‚d‚`‚c
           MOVE  JMST3-03     TO  HI-MHCD HI-HCD.
      *           READ  HI2-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD2-010
           END-IF
           IF  W-SEN  =  0
               IF  HI-BC3   <  30  OR   >  39
                   GO  TO  UPD2-010
               END-IF
           END-IF
           IF  W-SEN  =  1
               IF  HI-BC3   <  20  OR   >  29
                   GO  TO  UPD2-010
               END-IF
           END-IF
           IF  W-SEN  =  2
               IF  HI-BC3   >  19
                   GO  TO  UPD2-010
               END-IF
           END-IF
      *
           MOVE  SPACE     TO  YZAI-R.
           INITIALIZE  YZAI-R.
           MOVE  9         TO  YZAI-01.
           MOVE  JMST3-03  TO  YZAI-02.
           MOVE  JMST3-09  TO  YZAI-03.
           PERFORM  MOV-RTN       THRU  MOV-EX.
           MOVE  HI-BC3    TO  YZAI-91.
           MOVE  W-ZCHK    TO  YZAI-92.
           PERFORM  WRI-RTN       THRU  WRI-EX.
           GO  TO  UPD2-010.
       UPD2-EX.
           EXIT.
      ******************************
      ***   UPD3  RTN            ***
      ******************************
       UPD3-RTN.
      *
           INITIALIZE                 JNYZ-KEY.
           MOVE  ZERO             TO  W-MCD.
           MOVE  W-FH1            TO  W-MCD1.
           MOVE  W-MCD            TO  JNYZ-01.
      *
      *           START  JNYZ   KEY  NOT <  JNYZ-KEY   INVALID
      *///////////////
           CALL "DB_Start" USING
            JNYZ_PNAME1 "JNYZ-KEY" " NOT < " JNYZ-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  UPD3-EX
           END-IF.
       UPD3-010.
      ***  “üŒÉ—\’èŽc‚e@‚q‚d‚`‚c
      *           READ  JNYZ   NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JNYZ_PNAME1 BY REFERENCE JNYZ-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD3-EX
           END-IF
      *
           MOVE  JNYZ-01          TO  W-HCD.
           IF  W-HCD1    >  W-TH
               GO  TO  UPD3-EX
           END-IF
           IF  W-HCD1    >= W-FH1 AND   <=  W-TH1
               GO  TO  UPD3-020
           END-IF
           IF  W-HCD1    >= W-FH2 AND   <=  W-TH2
               GO  TO  UPD3-020
           END-IF
           IF  W-HCD1    >= W-FH3 AND   <=  W-TH3
               GO  TO  UPD3-020
           END-IF
           IF  W-HCD1    >= W-FH4 AND   <=  W-TH4
               GO  TO  UPD3-020
           END-IF
           IF  W-HCD1    >= W-FH5 AND   <=  W-TH5
               GO  TO  UPD3-020
           END-IF
           IF  W-HCD1    >= W-FH6 AND   <=  W-TH6
               GO  TO  UPD3-020
           END-IF
           GO  TO  UPD3-010.
      *
       UPD3-020.
           PERFORM  ZC3-RTN      THRU  ZC3-EX.
           IF  ZERO-SW  =  0
               GO  TO  UPD3-010
           END-IF
      ***  •i–¼ƒ}ƒXƒ^@‚q‚d‚`‚c
           MOVE  JNYZ-01      TO  HI-MHCD HI-HCD.
      *           READ  HI2-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD3-010
           END-IF
           IF  W-SEN  =  0
               IF  HI-BC3   <  30  OR   >  39
                   GO  TO  UPD3-010
               END-IF
           END-IF
           IF  W-SEN  =  1
               IF  HI-BC3   <  20  OR   >  29
                   GO  TO  UPD3-010
               END-IF
           END-IF
           IF  W-SEN  =  2
               IF  HI-BC3   >  19
                   GO  TO  UPD3-010
               END-IF
           END-IF
      *
           MOVE  SPACE     TO  YZAI-R.
           INITIALIZE  YZAI-R.
           MOVE  9         TO  YZAI-01.
           MOVE  JNYZ-01   TO  YZAI-02.
           MOVE  JNYZ-02   TO  YZAI-03.
           PERFORM  MOV-RTN       THRU  MOV-EX.
           MOVE  HI-BC3    TO  YZAI-91.
           MOVE  W-ZCHK    TO  YZAI-92.
           IF  W-ZCHK     =  4
               MOVE  1         TO  YZAI-93
           END-IF
           PERFORM  WRI-RTN       THRU  WRI-EX.
           GO  TO  UPD3-010.
       UPD3-EX.
           EXIT.
      *
      **************************
      ***  Ì § ² Ù  CLOSE    ***
      **************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-YZAI_IDLST JT-YZAI_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           IF  W-ZCHK     =  3  OR  4
               CALL "DB_F_Close" USING
                BY REFERENCE JMST3_IDLST JMST3_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JNYZ_IDLST JNYZ_PNAME1
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       END-EX.
           EXIT.
      ************************************
      ***  ‚i‚s|‚x‚y‚`‚h@‚v‚q‚h‚s‚d  ***
      ************************************
       WRI-RTN.
      *           WRITE    YZAI-R.
      *//////////////
           CALL "DB_Insert" USING
            JT-YZAI_PNAME1 JT-YZAI_LNAME YZAI-R RETURNING RET.
           IF  ERR-STAT  NOT = "00"
               MOVE  "W"          TO  ERR-M
               MOVE  "WK0128"     TO  ERR-F
               PERFORM  ERR-RTN   THRU  ERR-EX
           END-IF.
       WRI-EX.
           EXIT.
      ********************************
      *    ÝŒÉ@‚y‚d‚q‚n@ƒ`ƒFƒbƒN  *
      ********************************
       ZC1-RTN.
           MOVE  ZERO  TO  W-SUD.
           MOVE  0     TO  ZERO-SW.
           MOVE  1     TO  I.
       ZC1-010.
           IF  I  >  10
               GO  TO  ZC1-EX
           END-IF
           COMPUTE  W-SU(I)  =  NJZAI-0411(I)  -  NJZAI-0511(I)
                             +  NJZAI-0611(I)  +  NJZAI-0711(I)
                             -  NJZAI-0811(I)  +  NJZAI-1111(I).
           IF  W-ZCHK    NOT =  1
               COMPUTE  W-SU(I)  =  W-SU(I)    -  NJZAI-0911(I)
           END-IF
           IF  (ZERO-SW   = 0)  AND  (W-SU(I)  NOT =  ZERO)
               MOVE   1     TO  ZERO-SW
           END-IF
           ADD  1     TO  I.
           GO  TO  ZC1-010.
       ZC1-EX.
           EXIT.
      ****************************************
      *    —a‚èEŽæ‚æ‚¯@‚y‚d‚q‚n@ƒ`ƒFƒbƒN  *
      ****************************************
       ZC2-RTN.
           MOVE  ZERO  TO  W-SUD.
           MOVE  0     TO  ZERO-SW.
           MOVE  1     TO  I.
       ZC2-010.
           IF  I  >  10
               GO  TO  ZC2-EX
           END-IF
           COMPUTE  W-SU(I)  =  JMST3-1111(I)  -  JMST3-1211(I)
                             -  JMST3-141 (I)  -  JMST3-151 (I).
           COMPUTE  W-SU(I)  =  -1             *  W-SU(I).
           IF  (ZERO-SW   = 0)  AND  (W-SU(I)  NOT =  ZERO)
               MOVE   1     TO  ZERO-SW
           END-IF
           ADD  1     TO  I.
           GO  TO  ZC2-010.
       ZC2-EX.
           EXIT.
      ****************************************
      *    “üŒÉ—\’èŽc@‚y‚d‚q‚n@ƒ`ƒFƒbƒN    *
      ****************************************
       ZC3-RTN.
           MOVE  9        TO  NJZAI-01.
           MOVE  JNYZ-01  TO  NJZAI-02.
           MOVE  JNYZ-02  TO  NJZAI-03.
      *           READ  NJZAI    WITH  UNLOCK   INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ZERO      TO   NJZAI-0711(01)  NJZAI-0711(02)
                                    NJZAI-0711(03)  NJZAI-0711(04)
                                    NJZAI-0711(05)  NJZAI-0711(06)
                                    NJZAI-0711(07)  NJZAI-0711(08)
                                    NJZAI-0711(09)  NJZAI-0711(10)
           END-IF
           MOVE  ZERO  TO  W-SUD.
           MOVE  0     TO  ZERO-SW.
           MOVE  1     TO  I.
       ZC3-010.
           IF  I  >  10
               GO  TO  ZC3-EX
           END-IF
           COMPUTE     W-SU(I)  =    JNYZ-0311(I)  -  NJZAI-0711(I).
           IF  (ZERO-SW   = 0)  AND  (W-SU(I)  NOT =  ZERO)
               MOVE   1     TO  ZERO-SW
           END-IF
           ADD  1     TO  I.
           GO  TO  ZC3-010.
       ZC3-EX.
           EXIT.
      *****************************
      *    ”—Ê@‚l‚n‚u‚d         *
      *****************************
       MOV-RTN.
           MOVE  1     TO  I.
       MOV-010.
           IF  I  >  10
               GO  TO  MOV-EX
           END-IF
           MOVE   W-SU(I)     TO  YZAI-0411(I).
           ADD  1     TO  I.
           GO  TO  MOV-010.
       MOV-EX.
           EXIT.
      ***
       COPY  LPMSG.
      ***

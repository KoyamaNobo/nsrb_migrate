       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         HKT850.
      **************************************************************
      *    PROGRAM         :  担当別　売上･粗利前年対比　問合せ    *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  SCHK85                               *
      *    COMPILE TYPE    :  COBOL                                *
      *    JS-SIGN         :  通常=0 , 参考=1                      *
      **************************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       77  ATAI               PIC S9(009)V9(003) VALUE 1.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0512".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-TNC          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-TNCD.
             03  W-TNC1       PIC  9(001).
             03  W-TNC2       PIC  9(001).
           02  CNT.
             03  CNT1         PIC  9(002).
             03  CNT2         PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-NG.
             03  W-N          PIC  9(004).
             03  W-ND    REDEFINES W-N.
               04  W-N1       PIC  9(002).
               04  W-N2       PIC  9(002).
             03  W-G          PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-L            PIC  9(002).
           02  W-MD.
             03  W-NU         PIC S9(009).
             03  W-KEI        PIC S9(009).
             03  W-OU         PIC S9(009).
             03  W-NA         PIC S9(009).
             03  W-OA         PIC S9(009).
             03  W-RNU        PIC S9(009).
             03  W-RKEI       PIC S9(009).
             03  W-ROU        PIC S9(009).
             03  W-RNA        PIC S9(009).
             03  W-ROA        PIC S9(009).
             03  W-UKR        PIC S9(003)V9(01).
             03  W-UZR        PIC S9(003)V9(01).
             03  W-AZR        PIC S9(003)V9(01).
             03  W-RUKR       PIC S9(003)V9(01).
             03  W-RUZR       PIC S9(003)V9(01).
             03  W-RAZR       PIC S9(003)V9(01).
           02  W-HD.
             03  WH-NU         PIC S9(009).
             03  WH-KEI        PIC S9(009).
             03  WH-OU         PIC S9(009).
             03  WH-NA         PIC S9(009).
             03  WH-OA         PIC S9(009).
             03  WH-UKR        PIC S9(003)V9(01).
             03  WH-UZR        PIC S9(003)V9(01).
             03  WH-AZR        PIC S9(003)V9(01).
           02  W-TD.
             03  WT-NU         PIC S9(009).
             03  WT-KEI        PIC S9(009).
             03  WT-OU         PIC S9(009).
             03  WT-NA         PIC S9(009).
             03  WT-OA         PIC S9(009).
             03  WT-UKR        PIC S9(003)V9(01).
             03  WT-UZR        PIC S9(003)V9(01).
             03  WT-AZR        PIC S9(003)V9(01).
       01  ERR-STAT           PIC  X(002).
           COPY  LSTAT.
      *
           COPY LIBFDD.
           COPY LIHKBM.
      *FD  HTHKF
       01  HTHKF_HKT850.
           02  HTHKF_PNAME1   PIC  X(005) VALUE "HTHKF".
           02  F              PIC  X(001).
           02  HTHKF_LNAME    PIC  X(012) VALUE "HTHKF_HKT850".
           02  F              PIC  X(001).
           02  HTHKF_KEY1     PIC  X(100) VALUE SPACE.
           02  HTHKF_SORT     PIC  X(100) VALUE SPACE.
           02  HTHKF_IDLST    PIC  X(100) VALUE SPACE.
           02  HTHKF_RES      USAGE  POINTER.
       01  HTHK-R.
           02  HTHK-KEY.
             03  HTHK-NEN     PIC  9(004).
             03  HTHK-TNC     PIC  9(001).
           02  HTHK-ATKG.
             03  HTHK-TKGD  OCCURS  12.
               04  HTHK-KG.
                 05  HTHK-KEI PIC S9(010).
                 05  HTHK-ZIT PIC S9(010).
           02  F              PIC  X(011).
       77  F                  PIC  X(001).
      *FD  THY-M
       01  THY-M_HKT850.
           02  THY-M_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  THY-M_LNAME    PIC  X(012) VALUE "THY-M_HKT850".
           02  F              PIC  X(001).
           02  THY-M_KEY1     PIC  X(100) VALUE SPACE.
           02  THY-M_SORT     PIC  X(100) VALUE SPACE.
           02  THY-M_IDLST    PIC  X(100) VALUE SPACE.
           02  THY-M_RES      USAGE  POINTER.
       01  TH-R.
           02  TH-KEY         PIC  9(004).
           02  TH-IKC         PIC  9(001).
           02  TH-UD.
             03  TH-UK.
               04  TH-OUK.
                 05  TH-OU    OCCURS  12  PIC  9(009).
               04  TH-NUK.
                 05  TH-NU    OCCURS  12  PIC  9(009).
             03  TH-U     REDEFINES TH-UK.
               04  TH-UKD   OCCURS  24  PIC S9(009).
             03  TH-TU.
               04  TH-AOTU    PIC S9(010).
               04  TH-ANTU    PIC S9(010).
           02  TH-AD.
             03  TH-AK.
               04  TH-OAK.
                 05  TH-OA    OCCURS  12  PIC  9(009).
               04  TH-NAK.
                 05  TH-NA    OCCURS  12  PIC  9(009).
             03  TH-A  REDEFINES TH-AK.
               04  TH-AKD   OCCURS  24  PIC S9(009).
             03  TH-TA.
               04  TH-AOTA    PIC S9(010).
               04  TH-ANTA    PIC S9(010).
           02  TH-NG.
             03  TH-N         PIC  9(004).
             03  TH-G         PIC  9(002).
           02  TH-TC.
             03  TH-TC1       PIC  9(001).
             03  TH-TC2       PIC  9(001).
           02  TH-BC          PIC  9(001).
           02  F              PIC  X(025).
           02  TH-SEN         PIC  9(001).
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
           02  A-TNC   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-TNNA  PIC  N(014).
           02  D-NEN   PIC  9(002).
           02  D-MEI.
             03  FILLER  PIC ----,---,--- .
             03  FILLER  PIC ----.- .
             03  FILLER  PIC ----.- .
             03  FILLER  PIC ----.- .
             03  FILLER  PIC ----.- .
             03  FILLER  PIC ----,---,--- .
             03  FILLER  PIC ----.- .
             03  FILLER  PIC ----.- .
           02  D-HT.
             03  FILLER  PIC ----,---,--- .
             03  FILLER  PIC ----.- .
             03  FILLER  PIC ----.- .
             03  FILLER  PIC ----,---,--- .
             03  FILLER  PIC ----.- .
           02  D-AT.
             03  FILLER  PIC ----,---,--- .
             03  FILLER  PIC ----.- .
             03  FILLER  PIC ----.- .
             03  FILLER  PIC ----,---,--- .
             03  FILLER  PIC ----.- .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                   "***  ﾃﾞｰﾀ ﾅｼ  ***".
             03  E-ME2   PIC  X(017) VALUE
                   "***  ﾗｲﾝ ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY  LIBSCR.
       PROCEDURE           DIVISION.
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
            "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TNC" "9" "3" "10" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TNC" BY REFERENCE W-TNC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "67" "1" "A-TNC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "174" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNNA" "N" "3" "13" "28" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNNA" BY REFERENCE HKB-TNNA "28" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NEN" "9" "5" "4" "2" "D-TNNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NEN" BY REFERENCE W-N2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEI" " " "W-L" "0" "60" "D-NEN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MEI" "----,---,---" "W-L" "9" "12" " " "D-MEI"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MEI" BY REFERENCE W-NU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MEI" "----.-" "W-L" "22" "6" "01D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MEI" BY REFERENCE W-UKR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MEI" "----.-" "W-L" "29" "6" "02D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-MEI" BY REFERENCE W-RUKR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-MEI" "----.-" "W-L" "36" "6" "03D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-MEI" BY REFERENCE W-UZR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-MEI" "----.-" "W-L" "43" "6" "04D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-MEI" BY REFERENCE W-RUZR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-MEI" "----,---,---" "W-L" "51" "12" "05D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06D-MEI" BY REFERENCE W-NA "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-MEI" "----.-" "W-L" "64" "6" "06D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07D-MEI" BY REFERENCE W-AZR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-MEI" "----.-" "W-L" "71" "6" "07D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08D-MEI" BY REFERENCE W-RAZR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HT" " " "W-L" "0" "42" "D-MEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-HT" "----,---,---" "W-L" "9" "12" " " "D-HT"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-HT" BY REFERENCE WH-NU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-HT" "----.-" "W-L" "22" "6" "01D-HT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-HT" BY REFERENCE WH-UKR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-HT" "----.-" "W-L" "36" "6" "02D-HT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-HT" BY REFERENCE WH-UZR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-HT" "----,---,---" "W-L" "51" "12" "03D-HT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-HT" BY REFERENCE WH-NA "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-HT" "----.-" "W-L" "64" "6" "04D-HT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-HT" BY REFERENCE WH-AZR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-AT" " " "22" "0" "42" "D-HT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-AT" "----,---,---" "22" "9" "12" " " "D-AT"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-AT" BY REFERENCE WT-NU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-AT" "----.-" "22" "22" "6" "01D-AT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-AT" BY REFERENCE WT-UKR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-AT" "----.-" "22" "36" "6" "02D-AT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-AT" BY REFERENCE WT-UZR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-AT" "----,---,---" "22" "51" "12" "03D-AT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-AT" BY REFERENCE WT-NA "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-AT" "----.-" "22" "64" "6" "04D-AT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-AT" BY REFERENCE WT-AZR "4" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "94" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "94" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
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
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE ZERO TO W-NG.
           MOVE D-NHNG TO W-NGS.
           IF  W-N2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-N
           END-IF
           IF  W-N2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-N
           END-IF
           SUBTRACT 1 FROM W-G.
           IF  W-G = ZERO
               MOVE 12 TO W-G
               SUBTRACT 1 FROM W-N
           END-IF
           IF  W-G < 4
               SUBTRACT 1 FROM W-N
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HTHKF_PNAME1 "SHARED" BY REFERENCE HTHKF_IDLST "1"
            "HTHK-KEY" BY REFERENCE HTHK-KEY.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0512ID.
           MOVE WK0512ID TO THY-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" THY-M_PNAME1 " " BY REFERENCE THY-M_IDLST "0".
       M-15.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHK85" RETURNING RESU.
           CALL "SD_Output" USING "D-NEN" D-NEN "p" RETURNING RESU.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-TNC "A-TNC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-TNC > 7
               GO TO M-20
           END-IF.
       M-22.
           MOVE SPACE TO HKB-KEY.
           MOVE 04 TO HKB-NO.
           MOVE ZERO TO W-TNCD.
           MOVE W-TNC TO W-TNC1.
           MOVE W-TNCD TO HKB-TNC.
      *           START HKBM KEY NOT < HKB-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HKBM_PNAME1 "HKB-KEY" " NOT < " HKB-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF
      *           READ HKBM NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HKBM_PNAME1 BY REFERENCE HKB-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF
           IF  HKB-NO NOT = 4
               GO TO M-20
           END-IF
           IF  HKB-TNC > (W-TNC * 10 + 9)
               GO TO M-20
           END-IF
           CALL "SD_Output" USING "D-TNNA" D-TNNA "p" RETURNING RESU.
      *
           MOVE W-N TO HTHK-NEN.
           MOVE W-TNC TO HTHK-TNC.
      *           READ HTHKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HTHKF_PNAME1 BY REFERENCE HTHK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO HTHK-ATKG
           END-IF
      *
           CALL "DB_F_Close" USING
            BY REFERENCE THY-M_IDLST THY-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" THY-M_PNAME1 " " BY REFERENCE THY-M_IDLST "0".
      *
           MOVE ZERO TO W-MD W-HD W-TD.
       M-30.
      *           READ THY-M AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" THY-M_PNAME1 BY REFERENCE TH-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  TH-TC1 NOT = W-TNC
               GO TO M-30
           END-IF
           MOVE ZERO TO CNT.
           MOVE 7 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-35.
           ADD 1 TO CNT1.
           IF  CNT1 = 7 OR 13
               GO TO M-45
           END-IF.
       M-40.
           MOVE TH-NU(CNT1) TO W-NU.
           MOVE HTHK-KEI(CNT1) TO W-KEI.
           MOVE TH-OU(CNT1) TO W-OU.
           MOVE TH-NA(CNT1) TO W-NA.
           MOVE TH-OA(CNT1) TO W-OA.
           ADD TH-NU(CNT1) TO W-RNU.
           ADD HTHK-KEI(CNT1) TO W-RKEI.
           ADD TH-OU(CNT1) TO W-ROU.
           ADD TH-NA(CNT1) TO W-RNA.
           ADD TH-OA(CNT1) TO W-ROA.
           MOVE ZERO TO W-UKR W-UZR W-RUKR W-RUZR W-AZR W-RAZR.
           IF  W-NU NOT = ZERO
               IF  W-KEI NOT = ZERO
                   COMPUTE W-UKR ROUNDED = W-NU * 100 / W-KEI * ATAI
               END-IF
           END-IF
           IF  W-NU NOT = ZERO
               IF  W-OU NOT = ZERO
                   COMPUTE W-UZR ROUNDED = W-NU * 100 / W-OU * ATAI
               END-IF
           END-IF
           IF  W-RNU NOT = ZERO
               IF  W-RKEI NOT = ZERO
                   COMPUTE W-RUKR ROUNDED = W-RNU * 100 / W-RKEI * ATAI
               END-IF
           END-IF
           IF  W-RNU NOT = ZERO
               IF  W-ROU NOT = ZERO
                   COMPUTE W-RUZR ROUNDED = W-RNU * 100 / W-ROU * ATAI
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  W-NA NOT = ZERO
                   IF  W-OA NOT = ZERO
                       COMPUTE W-AZR ROUNDED = W-NA * 100 / W-OA * ATAI
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  W-RNA NOT = ZERO
                   IF  W-ROA NOT = ZERO
                   COMPUTE W-RAZR ROUNDED = W-RNA * 100 / W-ROA * ATAI
                   END-IF
               END-IF
           END-IF
      *
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L > 22
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           CALL "SD_Output" USING "D-MEI" D-MEI "p" RETURNING RESU.
           ADD TH-NU(CNT1) TO WH-NU.
           ADD HTHK-KEI(CNT1) TO WH-KEI.
           ADD TH-OU(CNT1) TO WH-OU.
           ADD TH-NA(CNT1) TO WH-NA.
           ADD TH-OA(CNT1) TO WH-OA.
           GO TO M-35.
       M-45.
           MOVE ZERO TO WH-UKR WH-UZR WH-AZR.
           IF  WH-NU NOT = ZERO
               IF  WH-KEI NOT = ZERO
                   COMPUTE WH-UKR ROUNDED = WH-NU * 100 / WH-KEI * ATAI
               END-IF
           END-IF
           IF  WH-NU NOT = ZERO
               IF  WH-OU NOT = ZERO
                   COMPUTE WH-UZR ROUNDED = WH-NU * 100 / WH-OU * ATAI
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  WH-NA NOT = ZERO
                   IF  WH-OA NOT = ZERO
                   COMPUTE WH-AZR ROUNDED = WH-NA * 100 / WH-OA * ATAI
                   END-IF
               END-IF
           END-IF
      *
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L > 22
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           CALL "SD_Output" USING "D-HT" D-HT "p" RETURNING RESU.
           ADD WH-NU TO WT-NU.
           ADD WH-KEI TO WT-KEI.
           ADD WH-OU TO WT-OU.
           ADD WH-NA TO WT-NA.
           ADD WH-OA TO WT-OA.
           MOVE ZERO TO W-HD.
           IF  CNT1 = 7
               GO TO M-40
           END-IF
      *
           MOVE ZERO TO WT-UKR WT-UZR WT-AZR.
           IF  WT-NU NOT = ZERO
               IF  WT-KEI NOT = ZERO
                   COMPUTE WT-UKR ROUNDED = WT-NU * 100 / WT-KEI * ATAI
               END-IF
           END-IF
           IF  WT-NU NOT = ZERO
               IF  WT-OU NOT = ZERO
                   COMPUTE WT-UZR ROUNDED = WT-NU * 100 / WT-OU * ATAI
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  WT-NA NOT = ZERO
                   IF  WT-OA NOT = ZERO
                   COMPUTE WT-AZR ROUNDED = WT-NA * 100 / WT-OA * ATAI
                   END-IF
               END-IF
           END-IF
      *
           CALL "SD_Output" USING "D-AT" D-AT "p" RETURNING RESU.
       M-50.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT = ADV
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB
               GO TO M-50
           END-IF
           ADD 1 TO W-TNC.
           IF  W-TNC = 8
               MOVE 0 TO W-TNC
           END-IF
           CALL "SD_Output" USING "A-TNC" A-TNC "p" RETURNING RESU.
           GO TO M-22.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HTHKF_IDLST HTHKF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE THY-M_IDLST THY-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

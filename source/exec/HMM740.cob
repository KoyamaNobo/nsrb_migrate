       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      HMM740.
      *********************************************************
      *    PROGRAM         :  óöï®êUë÷íPâøèCê≥ÇeÅ@çÏê¨Å@Å@Å@  *
      *    PROGRAM         :  (T-HIM Å® HFTSF)          Å@Å@  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *    DATA WRITTN     :  00/06/01                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-NC           PIC  9(001).
           02  W-SM           PIC  N(004).
           COPY LSTAT.
      *
      *FD  HI-M
       01  HI-M_HMM740.
           02  HI-M_PNAME1    PIC  X(006) VALUE "T-HIM1".
           02  F              PIC  X(001).
           02  HI-M_PNAME2    PIC  X(006) VALUE "T-HIM2".
           02  F              PIC  X(001).
           02  HI-M_LNAME     PIC  X(011) VALUE "HI-M_HMM740".
           02  F              PIC  X(001).
           02  HI-M_KEY1      PIC  X(100) VALUE SPACE.
           02  HI-M_KEY2      PIC  X(100) VALUE SPACE.
           02  HI-M_SORT      PIC  X(100) VALUE SPACE.
           02  HI-M_IDLST     PIC  X(100) VALUE SPACE.
           02  HI-M_RES       USAGE  POINTER.
       01  HI-R.
           02  HI-KEY2.
             03  HI-MHCD      PIC  9(006).
             03  HI-MHCDD REDEFINES HI-MHCD.
               04  HI-MHCD1   PIC  9(004).
               04  HI-MHCD2   PIC  9(002).
             03  HI-HCD       PIC  9(006).
             03  HI-KEY   REDEFINES HI-HCD.
               04  HI-HCD1    PIC  9(004).
               04  HI-HCD2    PIC  9(002).
           02  HI-NAME        PIC  N(024).
           02  HI-BC.
             03  HI-BCD12.
               04  HI-BCD1    PIC  9(003).
               04  HI-BCW1 REDEFINES HI-BCD1.
                 05  HI-BC1   PIC  9(002).
                 05  HI-BC21  PIC  9(001).
               04  HI-BC22    PIC  9(001).
             03  HI-BCW12 REDEFINES HI-BCD12.
               04  F          PIC  9(002).
               04  HI-BC2     PIC  9(002).
             03  HI-BC3       PIC  9(002).
           02  HI-ASSD.
             03  HI-SSD   OCCURS  4.
               04  HI-SS      PIC  9(010).
           02  HI-ASKD  REDEFINES HI-ASSD.
             03  HI-SKD   OCCURS  4.
               04  HI-SK    OCCURS 10.
                 05  HI-S     PIC  9(001).
           02  HI-AHSD  REDEFINES HI-ASSD.
             03  HI-HSD.
               04  HI-SS1     PIC  9(010).
               04  HI-SD1   REDEFINES HI-SS1.
                 05  HI-S1    OCCURS  10  PIC  9(001).
               04  HI-SS2     PIC  9(010).
               04  HI-SD2    REDEFINES HI-SS2.
                 05  HI-S2    OCCURS  10  PIC  9(001).
               04  HI-SS3     PIC  9(010).
               04  HI-SD3    REDEFINES HI-SS3.
                 05  HI-S3    OCCURS  10  PIC  9(001).
               04  HI-SS4     PIC  9(010).
               04  HI-SD4    REDEFINES HI-SS4.
                 05  HI-S4    OCCURS  10  PIC  9(001).
           02  HI-SB          PIC  9(005).
           02  HI-FT          PIC  9(005).
           02  F              PIC  X(019).
           02  HI-KT          PIC  9(005).
           02  HI-TCD         PIC  9(004).
           02  HI-ISU         PIC  9(003).
           02  HI-KRC         PIC  9(001).
           02  HI-SCC         PIC  9(001).
           02  HI-BMC         PIC  9(002).
           02  HI-BMNO        PIC  9(001).
           02  HI-YG          PIC  9(005).
           02  HI-HKB         PIC  9(001).
           02  HI-HPV         PIC  9(001).
           02  HI-BC4         PIC  9(001).
           02  HI-SSC         PIC  9(001).
           02  F              PIC  X(005).
           02  HI-YG2         PIC  9(005).
           02  HI-SMS         PIC  N(016).
           02  HI-UNG         PIC  9(006).
           02  HI-NNG         PIC  9(006).
           02  HI-OL          PIC  X(001).
           02  HI-CS          PIC  N(010).
           02  HI-RNG.
             03  F            PIC  X(005).
             03  HI-DELC      PIC  9(001).
           02  HI-DNG         PIC  9(006).
           02  HI-SNG         PIC  9(004).
           02  HI-SNGD    REDEFINES HI-SNG.
             03  HI-SNEN      PIC  9(002).
             03  HI-SGET      PIC  9(002).
           02  HI-ENG         PIC  9(004).
           02  HI-ENGD    REDEFINES HI-ENG.
             03  HI-ENEN      PIC  9(002).
             03  HI-EGET      PIC  9(002).
       77  F                  PIC  X(001).
      *FD  HFTSF
       01  HFTSF_HMM740.
           02  HFTSF_PNAME1   PIC  X(005) VALUE "HFTSF".
           02  F              PIC  X(001).
           02  HFTSF_LNAME    PIC  X(012) VALUE "HFTSF_HMM740".
           02  F              PIC  X(001).
           02  HFTSF_KEY1     PIC  X(100) VALUE SPACE.
           02  HFTSF_KEY2     PIC  X(100) VALUE SPACE.
           02  HFTSF_SORT     PIC  X(100) VALUE SPACE.
           02  HFTSF_IDLST    PIC  X(100) VALUE SPACE.
           02  HFTSF_RES      USAGE  POINTER.
       01  HFTS-R.
           02  HFTS-KEY.
             03  HFTS-NC      PIC  9(001).
             03  HFTS-HCD     PIC  9(006).
             03  HFTS-HCDD  REDEFINES HFTS-HCD.
               04  HFTS-HCD1  PIC  9(004).
               04  HFTS-HCD2  PIC  9(002).
           02  HFTS-OLD.
             03  HFTS-FTO     PIC  9(005).
             03  HFTS-ZRGO    PIC  9(005).
             03  HFTS-SKGO    PIC  9(005).
             03  HFTS-GKGO    PIC  9(005).
             03  HFTS-KNGO    PIC  9(004).
           02  HFTS-NEW.
             03  HFTS-FT      PIC  9(005).
             03  HFTS-ZRG     PIC  9(005).
             03  HFTS-SKG     PIC  9(005).
             03  HFTS-GKG     PIC  9(005).
             03  HFTS-KNG     PIC  9(004).
           02 HFTS-BC.
             03  HFTS-BC1     PIC  9(002).
             03  HFTS-BC2.
               04  HFTS-BC21  PIC  9(001).
               04  HFTS-BC22  PIC  9(001).
             03  HFTS-BC3     PIC  9(002).
           02  F              PIC  X(003).
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
           02  FILLER  PIC  N(022) VALUE
                "ÅñÅñÅñÅ@Å@óöï®êUë÷íPâøèCê≥ÇeÅ@çÏê¨Å@Å@ÅñÅñÅñ".
           02  FILLER  PIC  N(017) VALUE
                "ÅiÅ@ÇsÅ|ÇgÇhÇlÅ@Å®Å@ÇgÇeÇsÇrÇeÅ@Åj".
           02  FILLER  PIC  X(022) VALUE
                "ämîF  OK=1 NO=9   ÿ¿∞›".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SM    PIC  N(004).
       01  C-ERR.
           02  FILLER.
             03  E-ME10  PIC  X(025) VALUE
                  "***  HFTSF WRITE ¥◊∞  ***".
             03  E-ME11  PIC  X(026) VALUE
                  "***  HFTSF DELETE ¥◊∞  ***".
             03  E-KEY   PIC  X(007).
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
            "C-MID" " " "0" "0" "100" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "1" "12" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "5" "17" "34" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "X" "20" "36" "22" "02C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "53" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SM" "N" "12" "30" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING
            "D-SM" BY REFERENCE W-SM "8" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "58" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "58" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME10" "X" "24" "15" "25" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME11" "X" "24" "15" "26" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-KEY" "X" "24" "50" "7" "E-ME11" " " RETURNING RESU.
       CALL "SD_From" USING
            "E-KEY" BY REFERENCE HFTS-KEY "7" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE "ï]âøë÷Ç¶" TO W-SM.
           CALL "SD_Output" USING "D-SM" D-SM "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" HFTSF_PNAME1 " " BY REFERENCE HFTSF_IDLST "1"
            "HFTS-KEY" BY REFERENCE HFTS-KEY.
           PERFORM S-05 THRU S-15.
           IF  COMPLETION_CODE = 255
               CALL "DB_F_Close" USING
                BY REFERENCE HFTSF_IDLST HFTSF_PNAME1
               GO TO M-95
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 " " BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-15.
      *           READ HI-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HI-YG = ZERO
               GO TO M-15
           END-IF
           IF  HI-FT = HI-YG
               GO TO M-15
           END-IF.
       M-20.
           MOVE ZERO TO HFTS-R.
           MOVE SPACE TO HFTS-KEY.
           MOVE 1 TO HFTS-NC.
           MOVE HI-HCD TO HFTS-HCD.
           MOVE HI-FT TO HFTS-FTO.
           MOVE HI-YG TO HFTS-FT.
           MOVE HI-BC TO HFTS-BC.
      *           WRITE HFTS-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HFTSF_PNAME1 HFTSF_LNAME HFTS-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-25
           END-IF
           GO TO M-15.
       M-25.
           IF  ERR-STAT NOT = "24"
               GO TO M-90
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE HFTSF_IDLST HFTSF_PNAME1.
           MOVE "HFTSF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" HFTSF_PNAME1 " " BY REFERENCE HFTSF_IDLST "1"
            "HFTS-KEY" BY REFERENCE HFTS-KEY.
           GO TO M-20.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE HFTSF_IDLST HFTSF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO HFTS-KEY.
           MOVE 1 TO HFTS-NC.
      *           START HFTSF KEY NOT < HFTS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HFTSF_PNAME1 "HFTS-KEY" " NOT < " HFTS-KEY RETURNING RET.
           IF  RET = 1
               GO TO S-15
           END-IF.
       S-10.
      *           READ HFTSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HFTSF_PNAME1 BY REFERENCE HFTS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-15
           END-IF
           IF  HFTS-NC NOT = 1
               GO TO S-15
           END-IF
      *           DELETE HFTSF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HFTSF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-15
           END-IF
           GO TO S-10.
       S-15.
           EXIT.

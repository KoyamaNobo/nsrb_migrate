       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      HMM750.
      *********************************************************
      *    PROGRAM         :  ¨UΦPΏC³XVi]ΏΦ¦j*
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
       01  HI-M_HMM760.
           02  HI-M_PNAME1    PIC  X(006) VALUE "T-HIM1".
           02  F              PIC  X(001).
           02  HI-M_LNAME     PIC  X(011) VALUE "HI-M_HMM760".
           02  F              PIC  X(001).
           02  HI-M_KEY1      PIC  X(100) VALUE SPACE.
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
       01  HFTSF_HMM760.
           02  HFTSF_PNAME1   PIC  X(005) VALUE "HFTSF".
           02  F              PIC  X(001).
           02  HFTSF_LNAME    PIC  X(012) VALUE "HFTSF_HMM760".
           02  F              PIC  X(001).
           02  HFTSF_KEY1     PIC  X(100) VALUE SPACE.
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
                "@@¨@UΦPΏ@XV@@".
           02  FILLER  PIC  X(022) VALUE
                "mF  OK=1 NO=9   Ψΐ°έ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SM    PIC  N(004).
       01  C-ERR.
           02  FILLER.
             03  E-ME3   PIC  X(016) VALUE
                  "***  HIM ΕΌ  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  DATA ΕΌ  ***".
             03  E-ME5   PIC  X(023) VALUE
                  "***  ΠΊ³Όέ Γή°ΐ ±Ψ  ***".
             03  E-ME9   PIC  X(018) VALUE
                  "***  DATA ΄Χ°  ***".
             03  E-ME10  PIC  X(025) VALUE
                  "***  HIM REWRITE ΄Χ°  ***".
             03  E-HCD   PIC  9(006).
           COPY LSSEM.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "66" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "12" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "20" "36" "22" "01C-MID" " " RETURNING RESU.
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
            "C-ERR" " " "0" "0" "105" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "105" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "16" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "23" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "18" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "25" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HCD" "9" "24" "50" "6" "E-ME10" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HCD" BY REFERENCE HI-HCD "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE "]ΏΦ¦" TO W-SM.
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
            "INPUT" HFTSF_PNAME1 " " BY REFERENCE HFTSF_IDLST "1"
            "HFTS-KEY" BY REFERENCE HFTS-KEY.
           CALL "DB_F_Open" USING
            "I-O" HI-M_PNAME1 " " BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
      *
           MOVE SPACE TO HFTS-KEY.
           MOVE 1 TO HFTS-NC.
      *           START HFTSF KEY NOT < HFTS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HFTSF_PNAME1 "HFTS-KEY" " NOT < " HFTS-KEY RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-15.
      *           READ HFTSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HFTSF_PNAME1 BY REFERENCE HFTS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  1 NOT = HFTS-NC
               GO TO M-90
           END-IF
      *
           MOVE HFTS-HCD TO HI-KEY.
      *           READ HI-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE HFTS-FT TO HI-FT.
      *           REWRITE HI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HI-M_PNAME1 HI-M_LNAME HI-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-15.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE HFTSF_IDLST HFTSF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

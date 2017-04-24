       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD180.
      ************************************************************
      *    PROGRAM         :  発送明細ファイル　単価・締日セット *
      *    PRINTER TYPE    :  ****                               *
      *    SCREEN          :  ******                             *
      *    COMPILE TYPE    :  COBOL                              *
      *    JS-SIGN         :  ワークマン=1 , ナフコ=2            *
      ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-FILE             PIC  X(013).
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-NGP.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPD  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
             03  F            PIC  9(002).
           02  W-NGD          PIC  9(006).
           02  W-ENGD         PIC  9(006).
           02  W-DMM          PIC  9(001).
           02  W-SNGP         PIC  9(008).
           02  W-MNGP         PIC  9(008).
           02  W-L            PIC  9(002).
           02  W-INV          PIC  9(001).
           02  W-NA           PIC  N(007).
           02  W-TAN          PIC  9(005).
           02  W-DTW1         PIC  9(003).
           02  W-DTW2         PIC  9(001).
           02  W-SDTD         PIC  9(008).
           02  W-SDTM  REDEFINES W-SDTD.
             03  W-SDTNG.
               04  W-SDTN     PIC  9(004).
               04  W-SDTG     PIC  9(002).
             03  W-SDTP       PIC  9(002).
           02  W-SDATE        PIC  9(008).
           02  W-FTD          PIC  9(005).
           02  W-SNO          PIC  9(006).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LITSKF.
           COPY LITHTM.
           COPY LIHIM.
           COPY LIHUHM.
           COPY LIHSMS.
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　発送明細Ｆ　単価・締日セット　　＊＊＊".
           02  FILLER  PIC  X(024) VALUE
                "発送日 　'  年   月   日".
           02  FILLER  PIC  X(024) VALUE
                "請求日 　'  年   月   日".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-NEN   PIC  9(002).
           02  A-GET   PIC  9(002).
           02  A-PEY   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MID   PIC  N(007).
           02  D-INVM  PIC  X(034) VALUE
                "単価なし 指図№   ｺｰﾄﾞ  品      名".
           02  D-INV.
             03  01D-INV  PIC  X(007).
             03  02D-INV  PIC  9(006).
             03  03D-INV  PIC  N(024).
           02  D-INVC.
             03  FILLER   PIC  X(050) VALUE
                  "                                                  ".
             03  FILLER   PIC  X(013) VALUE "             ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  ﾃﾞｰﾀ ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  ﾄｸｲｻｷ ﾅｼ  ***".
             03  E-ME3   PIC  X(017) VALUE
                  "***  ﾋﾝﾒｲ ﾅｼ  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  HUHM ﾅｼ  ***".
             03  E-ME5   PIC  X(021) VALUE
                  "***  ﾌﾘｶｴﾀﾝｶ ｴﾗｰ  ***".
             03  E-ME10  PIC  X(021) VALUE
                  "***  REWRITE ｴﾗｰ  ***".
             03  E-KEY   PIC  X(007).
             03  E-TCD   PIC  9(004).
             03  E-HCD   PIC  9(006).
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
             03  E-STAT  PIC  X(002).
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
            "C-MID" " " "0" "0" "118" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "15" "48" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "5" "27" "24" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "7" "27" "24" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "22" "35" "22" "03C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "7" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "W-L" "37" "2" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "W-L" "42" "2" "A-NEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PEY" "9" "W-L" "47" "2" "A-GET" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "52" "1" "A-PEY" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "172" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID" "N" "1" "1" "14" " " "C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-MID" BY REFERENCE W-NA "14" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-INVM" "X" "10" "1" "34" "D-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-INV" " " "W-L" "0" "61" "D-INVM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-INV" "X" "W-L" "10" "7" " " "D-INV"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-INV" BY REFERENCE HSMS-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-INV" "9" "W-L" "18" "6" "01D-INV" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-INV" BY REFERENCE HSMS-09 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-INV" "N" "W-L" "25" "48" "02D-INV" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-INV" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-INVC" " " "W-L" "0" "63" "D-INV" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-INVC" "X" "W-L" "10" "50" " " "D-INVC" RETURNING RESU.
       CALL "SD_Init" USING 
           "02D-INVC" "X" "W-L" "60" "13" "01D-INVC" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "224" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "224" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "17" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "21" "E-ME4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "21" "E-ME5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "38" "7" "E-ME10" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE HSMS-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TCD" "9" "24" "46" "4" "E-KEY" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE T-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HCD" "9" "24" "51" "6" "E-TCD" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-HCD" BY REFERENCE HSMS-09 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-HCD" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME78" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" " " "24" "0" "80" "E-ME99" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-CL" "X" "24" "1" "40" " " "E-CL"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-CL" "X" "24" "41" "40" "01E-CL" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-CL" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN = 1
               MOVE "【ワークマン】" TO W-NA
           ELSE
               IF  JS-SIGN = 2
                   MOVE "　【ナフコ】　" TO W-NA
               ELSE
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "DB_Close"
                   STOP RUN
               END-IF
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-MID" D-MID "p" RETURNING RESU.
      *
           COPY LIBCPR.
           MOVE ZERO TO W-NGP.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-NGD.
           ADD 1 TO W-GET.
           IF  W-GET = 13
               ADD 1 TO W-NEN
               MOVE 1 TO W-GET
           END-IF
           MOVE W-NG TO W-ENGD.
      *
           CALL "DB_F_Open" USING
            "I-O" HSMSF_PNAME1 "SHARED" BY REFERENCE HSMSF_IDLST "1"
            "HSMS-KEY" BY REFERENCE HSMS-KEY.
      *
           PERFORM SET-RTN THRU SET-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
      *
           IF  JS-SIGN = 1
               MOVE 9850 TO T-KEY
           ELSE
               IF  JS-SIGN = 2
                   MOVE 5000 TO T-KEY
               END-IF
           END-IF
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE HSMSF_IDLST HSMSF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE T-M_IDLST T-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TSKF_IDLST TSKF_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-NGP = 99999999
               MOVE ZERO TO W-NGP
           END-IF
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-PEY" A-PEY "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE HSMSF_IDLST HSMSF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE T-M_IDLST T-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TSKF_IDLST TSKF_PNAME1
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-PEY "A-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-20
           END-IF
           MOVE W-NGP TO W-SNGP.
           PERFORM SKD-RTN THRU SKD-EX.
           MOVE 7 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE W-SDTD TO W-NGP.
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-PEY" A-PEY "p" RETURNING RESU.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 5 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE W-SNGP TO W-NGP
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-30
           END-IF
           IF  W-NG NOT = W-NGD AND W-ENGD
               GO TO M-30
           END-IF.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-PEY "A-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-30
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-35
           END-IF
           MOVE W-NGP TO W-MNGP.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-35
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-40
           END-IF
           IF  W-DMM = 9
               MOVE 5 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE W-SNGP TO W-NGP
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-40
           END-IF
      *
           CALL "DB_F_Close" USING
            BY REFERENCE HSMSF_IDLST HSMSF_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" HSMSF_PNAME1 "SHARED" BY REFERENCE HSMSF_IDLST "1"
            "HSMS-KEY" BY REFERENCE HSMS-KEY.
       M-45.
      *           READ HSMSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSMSF_PNAME1 BY REFERENCE HSMS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE T-M_IDLST T-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TSKF_IDLST TSKF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HSMSF_IDLST HSMSF_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  HSMS-02 = 7
               GO TO M-45
           END-IF
           IF  HSMS-061 NOT = T-TCD
               GO TO M-45
           END-IF
           IF  HSMS-03 NOT = 0
               GO TO M-45
           END-IF
           IF  HSMS-05 NOT = W-SNGP
               GO TO M-45
           END-IF
           IF  HSMS-23 = 1
               GO TO M-45
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HUH-M_PNAME1 "SHARED" BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
           MOVE 0 TO W-INV.
       M-50.
           MOVE HSMS-01 TO W-SNO.
           IF  HSMS-02 = 7
               GO TO M-75
           END-IF.
       M-55.
           IF  HSMS-02 = 7
               GO TO M-70
           END-IF
           MOVE 0 TO W-TAN.
           IF  HSMS-25 = 1
               GO TO M-65
           END-IF
           MOVE HSMS-061 TO THT-TCD.
           MOVE HSMS-09 TO THT-HCD.
           MOVE HSMS-10 TO THT-SIZ.
      *           READ THTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO THT-T
           END-IF
           MOVE THT-T TO W-TAN.
           IF  W-TAN NOT = ZERO
               GO TO M-60
           END-IF
           MOVE HSMS-061 TO THT-TCD.
           MOVE HSMS-09 TO THT-HCD.
           MOVE 9 TO THT-SIZ.
      *           READ THTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO THT-T
           END-IF
           MOVE THT-T TO W-TAN.
       M-60.
           IF  W-TAN NOT = ZERO
               GO TO M-65
           END-IF
           MOVE HSMS-09 TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "＊＊　品名なし　＊＊" TO HI-NAME
           END-IF
           IF  W-INV = 0
               MOVE 1 TO W-INV
               MOVE 10 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               CALL "SD_Output" USING "D-INVM" D-INVM "p" RETURNING RESU
           END-IF
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L > 20
               PERFORM CLR-RTN THRU CLR-EX
           END-IF
           CALL "SD_Output" USING "D-INV" D-INV "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
       M-65.
           PERFORM HUH-RTN THRU HUH-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF
           MOVE W-MNGP TO HSMS-18.
           IF  HSMS-25 NOT = 1
               MOVE W-TAN TO HSMS-17
           END-IF.
       M-70.
           MOVE 1 TO HSMS-26.
      *           REWRITE HSMS-R1 INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HSMSF_PNAME1 HSMSF_LNAME HSMS-R1 RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-75.
      *           READ HSMSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSMSF_PNAME1 BY REFERENCE HSMS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HSMS-01 = W-SNO
               GO TO M-55
           END-IF
           IF  HSMS-061 NOT = T-TCD
               GO TO M-75
           END-IF
           IF  HSMS-03 NOT = 0
               GO TO M-75
           END-IF
           IF  HSMS-05 NOT = W-SNGP
               GO TO M-75
           END-IF
           IF  HSMS-23 = 1
               GO TO M-75
           END-IF
           GO TO M-50.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-M_IDLST HUH-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HSMSF_IDLST HSMSF_PNAME1.
           IF  W-INV = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       SET-RTN.
      *           READ HSMSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSMSF_PNAME1 BY REFERENCE HSMS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HSMSF_IDLST HSMSF_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SET-EX
           END-IF
           IF  HSMS-061 NOT = 5000 AND 9850
               GO TO SET-RTN
           END-IF
           MOVE 99999999 TO W-NGP.
       SET-010.
           IF  JS-SIGN = 1
               IF  HSMS-061 = 9850
                   IF  HSMS-23 = 0
                       IF  HSMS-05 <= W-NGP
                           MOVE HSMS-05 TO W-NGP
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 2
               IF  HSMS-061 = 5000
                   IF  HSMS-23 = 0
                       IF  HSMS-05 <= W-NGP
                           MOVE HSMS-05 TO W-NGP
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF HSMS-26 = 0
               GO TO SET-020
           END-IF
           MOVE 0 TO HSMS-26.
      *           REWRITE HSMS-R1 INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HSMSF_PNAME1 HSMSF_LNAME HSMS-R1 RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HSMSF_IDLST HSMSF_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SET-EX
           END-IF.
       SET-020.
      *           READ HSMSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSMSF_PNAME1 BY REFERENCE HSMS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO SET-EX
           END-IF
           IF  HSMS-061 NOT = 5000 AND 9850
               GO TO SET-020
           END-IF
           GO TO SET-010.
       SET-EX.
           EXIT.
       CLR-RTN.
           MOVE 10 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       CLR-010.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L < 21
               CALL "SD_Output" USING "D-INVC" D-INVC "p" RETURNING RESU
               GO TO CLR-010
           END-IF
           MOVE 11 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       CLR-EX.
           EXIT.
       SKD-RTN.
           MOVE T-KEY TO TSK-KEY.
      *           READ TSKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO TSK-R
           END-IF
           MOVE TSK-ZNGP(4) TO W-SDATE.
           IF  TSK-ZNGP(5) NOT = ZERO
               MOVE TSK-ZNGP(5) TO W-SDATE
           END-IF
      *
           MOVE ZERO TO W-SDTD.
           MOVE W-NGP TO W-SDTD.
       SKD-010.
           MOVE T-SS TO W-SDTP.
           IF  W-SDTP = 00 OR 99
               MOVE ZERO TO W-SDTD
               GO TO SKD-EX
           END-IF
           IF  W-SDTP = 30 OR 31
               IF  W-SDTG = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
                   MOVE 31 TO W-SDTP
               ELSE
                   IF  W-SDTG = 4 OR 6 OR 9 OR 11
                       MOVE 30 TO W-SDTP
                   ELSE
                       DIVIDE 4 INTO W-SDTN GIVING W-DTW1
                                                 REMAINDER W-DTW2
                       IF  W-DTW2 = 0
                           MOVE 29 TO W-SDTP
                       ELSE
                           MOVE 28 TO W-SDTP
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  W-NGP > W-SDTD
               IF  W-GET NOT = W-SDTG
                   MOVE ZERO TO W-SDTD
                   GO TO SKD-EX
               ELSE
                   ADD 1 TO W-SDTG
                   GO TO SKD-020
               END-IF
           END-IF
           IF  W-SDATE NOT = ZERO
               IF  W-SDATE >= W-SDTD
                   ADD 1 TO W-SDTG
                   GO TO SKD-020
               END-IF
           END-IF
           GO TO SKD-EX.
       SKD-020.
           IF  W-SDTG = 13
               MOVE 1 TO W-SDTG
               ADD 1 TO W-SDTN
           END-IF
           GO TO SKD-010.
       SKD-EX.
           EXIT.
       HUH-RTN.
           MOVE HSMS-09 TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO HUH-EX
           END-IF
           MOVE HSMS-09 TO HUH-KEY.
      *           READ HUH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO HUH-EX
           END-IF
           IF (HUH-ZK NOT = ZERO) AND (HUH-ZS NOT = ZERO)
               COMPUTE W-FTD = HUH-ZK / HUH-ZS
               IF  HI-FT NOT = W-FTD
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "E-ME5" E-ME5 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-KEY" E-KEY "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-HCD" E-HCD "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
                   GO TO HUH-EX
               END-IF
           END-IF
           IF (HUH-NK NOT = ZERO) AND (HUH-NS NOT = ZERO)
               COMPUTE W-FTD = HUH-NK / HUH-NS
               IF  HI-FT NOT = W-FTD
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "E-ME5" E-ME5 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-KEY" E-KEY "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-HCD" E-HCD "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
                   GO TO HUH-EX
               END-IF
           END-IF
           IF (HUH-UG NOT = ZERO) AND (HUH-SS NOT = ZERO)
               COMPUTE W-FTD = HUH-UG / HUH-SS
               IF  HI-FT NOT = W-FTD
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "E-ME5" E-ME5 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-KEY" E-KEY "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-HCD" E-HCD "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
                   GO TO HUH-EX
               END-IF
           END-IF
           IF (HUH-YK NOT = ZERO) AND (HUH-YS NOT = ZERO)
               COMPUTE W-FTD = HUH-YK / HUH-YS
               IF  HI-FT NOT = W-FTD
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "E-ME5" E-ME5 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-KEY" E-KEY "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-HCD" E-HCD "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               END-IF
           END-IF.
       HUH-EX.
           EXIT.

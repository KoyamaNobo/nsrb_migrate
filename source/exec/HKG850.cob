       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG850.
      ******************************************************************
      *    PROGRAM         :  請求書　取り消し                         *
      *    PRINTER TYPE    :  JIPS                                     *
      *    SCREEN          :  ******                                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-END              PIC  9(001) VALUE 0.
       01  W-DATA.
           02  W-SEN          PIC  9(001).
           02  W-DATE.
             03  W-NEN        PIC  9(004).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-TCD          PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  W-DC           PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LITSKF.
           COPY LISKDF.
           COPY LITM.
           COPY LITUKF.
      *FD  SM-F
       01  SM-F_HKG850.
           02  SM-F_PNAME1    PIC  X(003) VALUE "SMF".
           02  F              PIC  X(001).
           02  SM-F_LNAME     PIC  X(011) VALUE "SM-F_HKG850".
           02  F              PIC  X(001).
           02  SM-F_KEY1      PIC  X(100) VALUE SPACE.
           02  SM-F_SORT      PIC  X(100) VALUE SPACE.
           02  SM-F_IDLST     PIC  X(100) VALUE SPACE.
           02  SM-F_RES       USAGE  POINTER.
       01  SM-R.
           02  SM-TCD         PIC  X(004).
           02  SM-DATE        PIC  9(008).
           02  SM-SZZ         PIC S9(009).
           02  SM-SZZZ        PIC S9(007).
           02  SM-SUK         PIC S9(009).
           02  SM-SUKZ        PIC S9(007).
           02  SM-STS         PIC S9(007).
           02  SM-STSZ        PIC S9(005).
           02  SM-SNK         PIC S9(009).
           02  SM-SNKZ        PIC S9(007).
           02  F              PIC  X(010).
           02  SM-DNO         PIC  9(006).
           02  SM-SP          PIC  9(002).
           02  SM-SK          PIC  9(001).
           02  SM-CHK         PIC  9(001).
           02  F              PIC  X(006).
           02  SM-CCD         PIC  9(003).
           02  SM-PC          PIC  9(001).
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
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　請求書　取り消し　　＊＊＊".
           02  FILLER  PIC  X(042) VALUE
                  "一括 = 1  ,  得意先別 = 2  .....  ".
           02  FILLER  PIC  X(019) VALUE
                "    年   月   日 分".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  A-TCD   PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MID1.
             03  FILLER.
               04  FILLER  PIC  X(011) VALUE "ｺｰﾄﾞ       ".
               04  FILLER  PIC  N(004) VALUE "得意先名".
               04  FILLER  PIC  X(052) VALUE
                "                                                    ".
             03  FILLER  PIC  X(009) VALUE "終了=ｆ･9".
             03  FILLER  PIC  X(019) VALUE
                  "    年   月   日 分".
           02  D-TNA   PIC  N(026).
           02  D-DATE.
             03  01D-DATE  PIC  9(004).
             03  02D-DATE  PIC  9(002).
             03  03D-DATE  PIC  9(002).
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(026) VALUE
                  "***  TSKF REWRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(026) VALUE
                  "***  SKDF REWRITE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(016) VALUE
                  "***  SMF ﾅｼ  ***".
             03  E-ME5   PIC  X(017) VALUE
                  "***  TUKF ﾅｼ  ***".
             03  E-ME6   PIC  X(025) VALUE
                  "***  TUKF DELETE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(026) VALUE
                  "***  TUKF REWRITE ｴﾗｰ  ***".
             03  E-SKD   PIC  X(020).
             03  E-TSK   PIC  9(004).
             03  E-TUK   PIC  X(014).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
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
            "C-MID" " " "0" "0" "119" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "22" "36" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "4" "23" "42" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "10" "30" "19" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "22" "29" "22" "03C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "6" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "4" "56" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "7" "11" "4" "A-SEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "46" "1" "A-TCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "159" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID1" " " "0" "0" "99" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MID1" " " "7" "0" "71" " " "D-MID1"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-MID1" "X" "7" "6" "11" " " "01D-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
          "0201D-MID1" "N" "7" "17" "8" "0101D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
         "0301D-MID1" "X" "7" "26" "52" "0201D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MID1" "X" "8" "11" "9" "01D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MID1" "X" "10" "30" "19" "02D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "7" "26" "52" "D-MID1" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATE" " " "10" "0" "8" "D-TNA" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DATE" "9" "10" "30" "4" " " "D-DATE"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DATE" BY REFERENCE W-NEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DATE" "9" "10" "37" "2" "01D-DATE" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DATE" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-DATE" "9" "10" "42" "2" "02D-DATE" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-DATE" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "283" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "283" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" "E-STAT" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "26" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "26" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "16" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "17" "E-ME4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "25" "E-ME5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "26" "E-ME6" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-SKD" "X" "24" "45" "20" "E-ME7" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-SKD" BY REFERENCE SKD-KEY "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TSK" "9" "24" "45" "4" "E-SKD" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-TSK" BY REFERENCE TSK-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TUK" "X" "24" "45" "14" "E-TSK" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-TUK" BY REFERENCE TUK-KEY "14" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-TUK" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" " " "24" "0" "80" "E-ME99" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-CL" "X" "0" "1" "40" " " "E-CL"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-CL" "X" "0" "41" "40" "01E-CL" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SEN < 1 OR > 2
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-15
           END-IF
      *
           IF  W-SEN = 1
               PERFORM IKS-RTN THRU IKS-EX
           ELSE
               PERFORM TBS-RTN THRU TBS-EX
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *=================================================================
       IKS-RTN.
           CALL "DB_F_Open" USING
            "INPUT" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
       IKS-020.
      *           READ TSKF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TSKF_PNAME1 BY REFERENCE TSK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO IKS-040
           END-IF
           IF  TSK-ZNGP(4) > W-DATE
               MOVE TSK-ZNGP(4) TO W-DATE
           END-IF
           IF  TSK-ZNGP(5) > W-DATE
               MOVE TSK-ZNGP(5) TO W-DATE
           END-IF
           GO TO IKS-020.
       IKS-040.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           IF  W-DATE = ZERO
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO IKS-EX
           END-IF
      *
           CALL "SD_Output" USING "D-DATE" D-DATE "p" RETURNING RESU.
       IKS-060.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO IKS-060
           END-IF
           IF  W-DMM = 9
               GO TO IKS-EX
           END-IF
           IF  W-DMM NOT = 1
               GO TO IKS-060
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
       IKS-160.
      *           READ TSKF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TSKF_PNAME1 BY REFERENCE TSK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO IKS-220
           END-IF
           IF  TSK-ZNGP(4) = ZERO
               GO TO IKS-160
           END-IF
           IF  W-DATE NOT = TSK-ZNGP(4) AND TSK-ZNGP(5)
               GO TO IKS-160
           END-IF
      *
           IF  TSK-ZNGP(5) = ZERO
               IF  TSK-ZNGP(4) = W-DATE
                   MOVE ZERO TO TSK-ZSD(4)
               END-IF
           END-IF
           IF  TSK-ZNGP(5) = W-DATE
               MOVE ZERO TO TSK-ZSD(5)
           END-IF
      *
      *           REWRITE TSK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TSKF_PNAME1 TSKF_LNAME TSK-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TSKF_IDLST TSKF_PNAME1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TSK" E-TSK "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO IKS-EX
           END-IF
           GO TO IKS-160.
       IKS-220.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
       IKS-240.
      *           READ SKDF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO IKS-260
           END-IF
           IF  SKD-SKD NOT = W-DATE
               GO TO IKS-240
           END-IF
           MOVE ZERO TO SKD-SNO.
      *           REWRITE SKD-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SKDF_PNAME1 SKDF_LNAME SKD-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SKD" E-SKD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO IKS-260
           END-IF
           GO TO IKS-240.
       IKS-260.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" SM-F_PNAME1 " " BY REFERENCE SM-F_IDLST "0".
       IKS-280.
      *           READ SM-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" SM-F_PNAME1 BY REFERENCE SM-R " " RETURNING RET.
           IF  RET = 1
               GO TO IKS-300
           END-IF
           IF  SM-PC NOT = 0
               GO TO IKS-280
           END-IF
           IF  SM-DATE NOT = W-DATE
               GO TO IKS-280
           END-IF
           MOVE 2 TO SM-PC.
      *           REWRITE SM-R.
      *///////////////
           CALL "DB_Update" USING
            SM-F_PNAME1 SM-F_LNAME SM-R RETURNING RET.
           GO TO IKS-280.
       IKS-300.
           CALL "DB_F_Close" USING BY REFERENCE SM-F_IDLST SM-F_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" TUKF_PNAME1 "SHARED" BY REFERENCE TUKF_IDLST "2"
            "TUK-KEY" BY REFERENCE TUK-KEY "TUK-KEY2" BY REFERENCE
            TUK-KEY2.
       IKS-320.
      *           READ TUKF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TUKF_PNAME1 BY REFERENCE TUK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO IKS-980
           END-IF
           IF (TUK-DC = 4) AND (TUK-DATE = W-DATE)
               GO TO IKS-360
           END-IF
           IF (TUK-DC NOT = 4) AND (TUK-SKD = W-DATE)
               GO TO IKS-340
           END-IF
           GO TO IKS-320.
       IKS-340.
           MOVE ZERO TO TUK-SKD.
      *           REWRITE TUK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TUKF_PNAME1 TUKF_LNAME TUK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TUK" E-TUK "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO IKS-980
           END-IF
           GO TO IKS-320.
       IKS-360.
      *           DELETE TUKF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TUKF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TUK" E-TUK "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO IKS-980
           END-IF
           GO TO IKS-320.
       IKS-980.
           CALL "DB_F_Close" USING BY REFERENCE TUKF_IDLST TUKF_PNAME1.
       IKS-EX.
           EXIT.
      *-----------------------------------------------------------------
       TBS-RTN.
           CALL "DB_F_Open" USING
            "I-O" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
           CALL "DB_F_Open" USING
            "I-O" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "I-O" TUKF_PNAME1 "SHARED" BY REFERENCE TUKF_IDLST "2"
            "TUK-KEY" BY REFERENCE TUK-KEY "TUK-KEY2" BY REFERENCE
            TUK-KEY2.
       TBS-020.
           CALL "SD_Output" USING "D-MID1" D-MID1 "p" RETURNING RESU.
       TBS-040.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO TBS-900
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO TBS-040
           END-IF
      *
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO TBS-040
           END-IF
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
      *
           MOVE W-TCD TO TSK-KEY.
      *           READ TSKF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO TBS-040
           END-IF
           IF  ZERO = TSK-ZNGP(4) AND TSK-ZNGP(5)
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO TBS-040
           END-IF
           IF  TSK-ZNGP(5) = ZERO
               MOVE TSK-ZNGP(4) TO W-DATE
           ELSE
               MOVE TSK-ZNGP(5) TO W-DATE
           END-IF
           CALL "SD_Output" USING "D-DATE" D-DATE "p" RETURNING RESU.
       TBS-060.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO TBS-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO TBS-060
           END-IF
           IF  W-DMM = 9
               GO TO TBS-020
           END-IF
           IF  W-DMM NOT = 1
               GO TO TBS-060
           END-IF
      *
           IF  TSK-ZNGP(5) = ZERO
               IF  TSK-ZNGP(4) = W-DATE
                   MOVE ZERO TO TSK-ZSD(4)
               END-IF
           END-IF
           IF  TSK-ZNGP(5) = W-DATE
               MOVE ZERO TO TSK-ZSD(5)
           END-IF
      *
      *           REWRITE TSK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TSKF_PNAME1 TSKF_LNAME TSK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TSK" E-TSK "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TBS-900
           END-IF.
       TBS-080.
           MOVE SPACE TO SKD-KEY.
           MOVE W-TCD TO SKD-TCD.
      *           START SKDF KEY NOT < SKD-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SKDF_PNAME1 "SKD-KEY" " NOT < " SKD-KEY RETURNING RET.
           IF  RET = 1
               GO TO TBS-120
           END-IF.
       TBS-100.
      *           READ SKDF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO TBS-120
           END-IF
           IF  SKD-TCD NOT = W-TCD
               GO TO TBS-120
           END-IF
           IF  SKD-SKD NOT = W-DATE
               GO TO TBS-100
           END-IF
           MOVE ZERO TO SKD-SNO.
      *           REWRITE SKD-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SKDF_PNAME1 SKDF_LNAME SKD-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TBS-900
           END-IF
           GO TO TBS-100.
       TBS-120.
           CALL "DB_F_Open" USING
            "I-O" SM-F_PNAME1 " " BY REFERENCE SM-F_IDLST "0".
       TBS-140.
      *           READ SM-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" SM-F_PNAME1 BY REFERENCE SM-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO TBS-160
           END-IF
           IF  SM-PC NOT = 0
               GO TO TBS-140
           END-IF
           IF  SM-TCD NOT = W-TCD
               GO TO TBS-140
           END-IF
           IF  SM-DATE NOT = W-DATE
               GO TO TBS-140
           END-IF
      *
           MOVE 2 TO SM-PC.
      *           REWRITE SM-R.
      *///////////////
           CALL "DB_Update" USING
            SM-F_PNAME1 SM-F_LNAME SM-R RETURNING RET.
           CALL "DB_F_Close" USING BY REFERENCE SM-F_IDLST SM-F_PNAME1.
       TBS-160.
           MOVE 0 TO W-DC.
           MOVE SPACE TO TUK-KEY.
           MOVE W-TCD TO TUK-TCD.
      *           START TUKF KEY NOT < TUK-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TUKF_PNAME1 "TUK-KEY" " NOT < " TUK-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO TBS-900
           END-IF.
       TBS-180.
      *           READ TUKF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TUKF_PNAME1 BY REFERENCE TUK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO TBS-240
           END-IF
           IF  TUK-TCD NOT = W-TCD
               GO TO TBS-240
           END-IF
           IF (TUK-DC = 4) AND (TUK-DATE = W-DATE)
               GO TO TBS-220
           END-IF
           IF (TUK-DC NOT = 4) AND (TUK-SKD = W-DATE)
               GO TO TBS-200
           END-IF
           GO TO TBS-180.
       TBS-200.
           MOVE ZERO TO TUK-SKD.
      *           REWRITE TUK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TUKF_PNAME1 TUKF_LNAME TUK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TUK" E-TUK "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TBS-900
           END-IF
           GO TO TBS-180.
       TBS-220.
      *           DELETE TUKF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TUKF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TUK" E-TUK "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TBS-900
           END-IF
           MOVE 1 TO W-DC.
           GO TO TBS-180.
       TBS-240.
           IF W-DC = 0
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           GO TO TBS-020.
       TBS-900.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TUKF_IDLST TUKF_PNAME1.
       TBS-EX.
           EXIT.

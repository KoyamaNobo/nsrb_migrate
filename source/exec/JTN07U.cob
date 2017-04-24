       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JTN07U.
      *********************************************************
      *    PROGRAM         :  トラスコ他指図変換              *
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
       77  W-END              PIC  9(001) VALUE 0.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-DATE         PIC  9(008).
           02  W-ONO          PIC  9(006).
           02  W-SNO          PIC  9(006).
           02  W-D.
             03  W-OSC.
               04  W-TCD      PIC  9(004).
               04  W-CCD      PIC  9(003).
             03  W-TPC        PIC  9(004).
             03  W-SOK        PIC  9(001).
             03  W-UNS        PIC  9(001).
             03  W-ISU        PIC  9(003).
             03  W-THT        PIC  N(009).
             03  W-TTE        PIC  N(019).
           02  W-NO           PIC  9(002).
           02  W-OEB          PIC  9(002).
           02  W-GNO          PIC  9(001).
           02  W-DC           PIC  9(001).
           02  WRI-SW         PIC  9(001).
           02  W-TEK.
             03  F            PIC  N(001) VALUE "№".
             03  W-TK1        PIC  N(002).
             03  F            PIC  N(001) VALUE SPACE.
             03  W-TK2        PIC  N(019).
             03  W-TK2D  REDEFINES W-TK2.
               04  W-TK21     PIC  N(004).
               04  W-TK22     PIC  N(015).
           02  W-TEKI         PIC  N(016).
           02  W-TEKID REDEFINES W-TEKI.
             03  W-TKD   OCCURS  16.
               04  W-TK       PIC  N(001).
           02  W-TTEA         PIC  N(019).
           02  W-TTED  REDEFINES W-TTEA.
             03  W-TED   OCCURS  19.
               04  W-TE       PIC  N(001).
           02  CNT1           PIC  9(002).
           02  CNT2           PIC  9(002).
      *
           COPY   LIBFDD.
           COPY   L-TDIF.
           COPY   LJMSTD.
           COPY   L-JSTR.
           COPY   L-JCON.
           COPY   LOKJF.
           COPY   LTDNKN.
      *FD  WTNAF
       01  WTNAF_JTN07U.
           02  WTNAF_PNAME1   PIC  X(005) VALUE "WTNAF".
           02  F              PIC  X(001).
           02  WTNAF_LNAME    PIC  X(012) VALUE "WTNAF_JTN07U".
           02  F              PIC  X(001).
           02  WTNAF_KEY1     PIC  X(100) VALUE SPACE.
           02  WTNAF_SORT     PIC  X(100) VALUE SPACE.
           02  WTNAF_IDLST    PIC  X(100) VALUE SPACE.
           02  WTNAF_RES      USAGE  POINTER.
       01  WTNA-R.
           02  WTNA-KEY.
             03  WTNA-TENC    PIC  9(004).
           02  WTNA-TNA       PIC  N(026).
           02  WTNA-OSN       PIC  9(001).
           02  F              PIC  X(007).
       77  F                  PIC  X(001).
      *FD  TSHW
       01  TSHW_JTN07U.
           02  TSHW_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TSHW_LNAME     PIC  X(011) VALUE "TSHW_JTN07U".
           02  F              PIC  X(001).
           02  TSHW_KEY1      PIC  X(100) VALUE SPACE.
           02  TSHW_SORT      PIC  X(100) VALUE SPACE.
           02  TSHW_IDLST     PIC  X(100) VALUE SPACE.
           02  TSHW_RES       USAGE  POINTER.
       01  TSHW-R.
           02  TSHW-DATE      PIC  9(006).
           02  TSHW-TCD       PIC  9(004).
           02  TSHW-CCD       PIC  9(003).
           02  TSHW-TPC       PIC  9(004).
           02  TSHW-SOK       PIC  9(001).
           02  TSHW-UNS       PIC  9(001).
           02  TSHW-HNO       PIC  X(010).
           02  TSHW-JNOD.
             03  TSHW-JNO     PIC  9(006).
             03  TSHW-JGN     PIC  9(001).
           02  TSHW-ISU       PIC  9(003).
           02  TSHW-HCD       PIC  9(006).
           02  TSHW-SKB       PIC  9(001).
           02  TSHW-ASU.
             03  TSHW-SUD   OCCURS  10.
               04  TSHW-SU    PIC S9(004).
           02  TSHW-SUT       PIC S9(005).
           02  TSHW-TEKI      PIC  N(028).
           02  TSHW-TED   REDEFINES TSHW-TEKI.
             03  TSHW-THT     PIC  N(009).
             03  TSHW-TTE     PIC  N(019).
           02  F              PIC  X(109).
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　トラスコ他　出荷指図書　作成　　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2.
               04  FILLER  PIC  X(018) VALUE
                    "***  DATA ｴﾗｰ  ***".
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  9(003).
               04  FILLER  PIC  9(001).
               04  FILLER  PIC  9(001).
             03  E-ME3.
               04  FILLER  PIC  X(017) VALUE
                    "***  JCON ﾅｼ  ***".
               04  FILLER  PIC  X(002).
             03  E-ME11.
               04  FILLER  PIC  X(026) VALUE
                    "***  JCON REWRITE ｴﾗｰ  ***".
               04  FILLER  PIC  X(002).
             03  E-ME12.
               04  FILLER  PIC  X(024) VALUE
                    "***  OKJF WRITE ｴﾗｰ  ***".
               04  FILLER  PIC  X(006).
             03  E-ME13.
               04  FILLER  PIC  X(024) VALUE
                    "***  JSTR WRITE ｴﾗｰ  ***".
               04  FILLER  PIC  X(007).
             03  E-ME14.
               04  FILLER  PIC  X(027) VALUE
                    "***  JT-DNKN WRITE ｴﾗｰ  ***".
               04  FILLER  PIC  X(015).
             03  E-ME17.
               04  FILLER  PIC  X(017) VALUE
                    "***  TDIF ﾅｼ  ***".
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  9(003).
               04  FILLER  PIC  9(001).
               04  FILLER  PIC  9(001).
             03  E-ME18.
               04  FILLER  PIC  X(026) VALUE
                    "***  TDIF REWRITE ｴﾗｰ  ***".
               04  FILLER  PIC  X(007).
           COPY LIBSCR.
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
            "C-MID" " " "0" "0" "48" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "15" "48" " " "C-MID" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "253" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "253" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" " " "24" "0" "27" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME2" "X" "24" "15" "18" " " "E-ME2" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME2" "9" "24" "45" "4" "01E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME2" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03E-ME2" "9" "24" "50" "3" "02E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03E-ME2" BY REFERENCE W-CCD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04E-ME2" "9" "24" "54" "1" "03E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04E-ME2" BY REFERENCE W-SOK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05E-ME2" "9" "24" "56" "1" "04E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "05E-ME2" BY REFERENCE W-UNS "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" " " "24" "0" "19" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME3" "X" "24" "15" "17" " " "E-ME3" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME3" "X" "24" "35" "2" "01E-ME3" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME3" BY REFERENCE JCON1-KEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" " " "24" "0" "28" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME11" "X" "24" "15" "26" " " "E-ME11" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME11" "X" "24" "45" "2" "01E-ME11" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME11" BY REFERENCE JCON1-KEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" " " "24" "0" "30" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME12" "X" "24" "15" "24" " " "E-ME12" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME12" "X" "24" "45" "6" "01E-ME12" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME12" BY REFERENCE OKJF-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" " " "24" "0" "31" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME13" "X" "24" "15" "24" " " "E-ME13" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME13" "X" "24" "45" "7" "01E-ME13" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME13" BY REFERENCE JSTR-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME14" " " "24" "0" "42" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME14" "X" "24" "15" "27" " " "E-ME14" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME14" "X" "24" "45" "15" "01E-ME14" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME14" BY REFERENCE DNKN-KEY "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME17" " " "24" "0" "26" "E-ME14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME17" "X" "24" "15" "17" " " "E-ME17" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME17" "9" "24" "45" "4" "01E-ME17" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME17" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03E-ME17" "9" "24" "50" "3" "02E-ME17" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03E-ME17" BY REFERENCE W-CCD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04E-ME17" "9" "24" "54" "1" "03E-ME17" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04E-ME17" BY REFERENCE W-SOK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05E-ME17" "9" "24" "56" "1" "04E-ME17" " " RETURNING RESU.
       CALL "SD_From" USING 
            "05E-ME17" BY REFERENCE W-UNS "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME18" " " "24" "0" "33" "E-ME17" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME18" "X" "24" "15" "26" " " "E-ME18" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME18" "X" "24" "45" "7" "01E-ME18" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME18" BY REFERENCE TDI-KEY "7" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-010.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO TSHW_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TSHW_PNAME1 " " BY REFERENCE TSHW_IDLST "0".
      *           READ TSHW AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TSHW_PNAME1 BY REFERENCE TSHW-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TSHW_IDLST TSHW_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  200
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           CALL "DB_F_Open" USING
            "I-O" TDIF_PNAME1 " " BY REFERENCE TDIF_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "I-O" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
           CALL "DB_F_Open" USING
            "I-O" JT-DNKN_PNAME1 "SHARED" BY REFERENCE JT-DNKN_IDLST "1"
            "DNKN-KEY" BY REFERENCE DNKN-KEY.
           CALL "DB_F_Open" USING
            "INPUT" WTNAF_PNAME1 "SHARED" BY REFERENCE WTNAF_IDLST "1"
            "WTNA-KEY" BY REFERENCE WTNA-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JMSTD_PNAME1 "SHARED" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2"
            BY REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE
            JMSTD-KEY3.
      *
           MOVE    "12"        TO    JCON1-KEY.
      *           READ     JCON       UNLOCK     INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF
           MOVE    "14"        TO    JCON1-KEY.
      *           READ     JCON       UNLOCK     INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF
      *
           MOVE    ZERO      TO   W-NGP.
           MOVE    TSHW-DATE TO   W-NGPS.
           COPY LIBCPR.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       M-100.
           MOVE      TSHW-TCD TO       W-TCD.
           MOVE      TSHW-CCD TO       W-CCD.
           MOVE      TSHW-SOK TO       W-SOK.
           MOVE      TSHW-UNS TO       W-UNS.
           MOVE      TSHW-THT TO       W-THT.
           MOVE      ZERO     TO       W-OEB W-NO.
           IF  W-UNS        =   9
               GO  TO  M-160
           END-IF
           MOVE      1        TO       JCON1-01.
           MOVE      4        TO       JCON1-02.
      *           READ     JCON               INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF
           IF  JCON1-04  =  199999
               MOVE     100000    TO    W-ONO
           ELSE
               COMPUTE  W-ONO  =   JCON1-04  +  1
           END-IF
           MOVE    W-ONO   TO    JCON1-04.
      *           REWRITE  JCON1-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF.
       M-120.
           INITIALIZE                OKJF-R.
           MOVE     W-ONO      TO    OKJF-KEY.
           MOVE     W-UNS      TO    OKJF-02.
           MOVE     W-NGPS     TO    OKJF-03.
           MOVE     W-SOK      TO    OKJF-04.
           MOVE     W-OSC      TO    OKJF-05.
           MOVE     W-THT      TO    OKJF-06.
           MOVE     ZERO       TO    OKJF-07   OKJF-08   OKJF-10.
           MOVE     1          TO    OKJF-09.
           MOVE     1          TO    OKJF-13.
      *           WRITE    OKJF-R     INVALID
      *//////////////
           CALL "DB_Insert" USING
            OKJF_PNAME1 OKJF_LNAME OKJF-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-140
           END-IF
           GO  TO M-160.
       M-140.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
           GO TO M-120.
       M-160.
           MOVE     TSHW-TPC   TO    W-TPC.
           MOVE     TSHW-ISU   TO    W-ISU.
           MOVE     TSHW-THT   TO    W-THT.
           MOVE     TSHW-TTE   TO    W-TTE.
      *
           MOVE     1          TO    JCON1-01.
           MOVE     2          TO    JCON1-02.
      *           READ     JCON       INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF.
       M-180.
           ADD     1           TO  W-NO.
           IF  W-NO            >   88
               GO  TO  M-100
           END-IF
      *
           ADD     1           TO  JCON1-04.
           IF  JCON1-04        =   200000
               MOVE   100001       TO    JCON1-04
           END-IF
      *
           MOVE     JCON1-04   TO    JSTR-01.
           MOVE     ZERO       TO    JSTR-02.
      *           START    JSTR  KEY   NOT <  JSTR-KEY   INVALID
      *///////////////
           CALL "DB_Start" USING
            JSTR_PNAME1 "JSTR-KEY" " NOT < " JSTR-KEY RETURNING RET.
           IF  RET = 1
               GO  TO   M-200
           END-IF.
      *           READ     JSTR       NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO   M-200
           END-IF
           IF  JSTR-01     NOT =   JCON1-04
               GO  TO  M-200
           ELSE
               GO  TO   M-180
           END-IF.
       M-200.
           MOVE  JCON1-04  TO  W-SNO.
      *           REWRITE  JCON1-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF
      *
           MOVE     ZERO       TO    W-GNO.
       M-220.
           ADD      1          TO    W-GNO
           IF  W-GNO           >     6
               GO  TO  M-160
           END-IF
           IF  W-TCD      NOT  =     9850
               GO  TO  M-240
           END-IF
           IF  W-TPC           =     ZERO
               GO  TO  M-240
           END-IF
           MOVE     W-TPC      TO    WTNA-KEY.
      *           READ     WTNAF      UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" WTNAF_PNAME1 BY REFERENCE WTNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    WTNA-TNA
           END-IF.
       M-240.
           IF  TSHW-JNOD       =     ZERO
               MOVE     0          TO    JMSTD-01
               GO  TO  M-250
           END-IF
           MOVE     TSHW-JNOD  TO    JMSTD-KEY1.
      *           READ     JMSTD      UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JMSTD_PNAME1 BY REFERENCE JMSTD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     0          TO    JMSTD-01
               GO  TO  M-250
           END-IF.
       M-250.
           MOVE     SPACE      TO    JSTR-R.
           INITIALIZE                JSTR-R.
           MOVE     W-SNO      TO    JSTR-01.
           MOVE     W-GNO      TO    JSTR-02.
           MOVE     0          TO    JSTR-03.
           MOVE     W-NGP      TO    JSTR-04.
           MOVE     TSHW-SU(01) TO    JSTR-111(01).
           MOVE     TSHW-SU(02) TO    JSTR-111(02).
           MOVE     TSHW-SU(03) TO    JSTR-111(03).
           MOVE     TSHW-SU(04) TO    JSTR-111(04).
           MOVE     TSHW-SU(05) TO    JSTR-111(05).
           MOVE     TSHW-SU(06) TO    JSTR-111(06).
           MOVE     TSHW-SU(07) TO    JSTR-111(07).
           MOVE     TSHW-SU(08) TO    JSTR-111(08).
           MOVE     TSHW-SU(09) TO    JSTR-111(09).
           MOVE     TSHW-SU(10) TO    JSTR-111(10).
           COMPUTE  JSTR-112  =  TSHW-SU(01)  +  TSHW-SU(02)
                              +  TSHW-SU(03)  +  TSHW-SU(04)
                              +  TSHW-SU(05)  +  TSHW-SU(06)
                              +  TSHW-SU(07)  +  TSHW-SU(08)
                              +  TSHW-SU(09)  +  TSHW-SU(10).
           MOVE     W-TCD      TO    JSTR-061.
           MOVE     W-CCD      TO    JSTR-062.
           MOVE     W-SOK      TO    JSTR-07.
           MOVE     TSHW-JNO   TO    JSTR-081.
           MOVE     TSHW-JGN   TO    JSTR-082.
           MOVE     TSHW-HCD   TO    JSTR-09.
           MOVE     TSHW-SKB   TO    JSTR-10.
           MOVE     JMSTD-01   TO    JSTR-13.
           MOVE     TSHW-UNS   TO    JSTR-14.
           MOVE     0          TO    JSTR-14A.
           IF  W-UNS        =   9
               MOVE     ZERO       TO    JSTR-14B  JSTR-14C
           ELSE
               MOVE     W-ONO      TO    JSTR-14B
               COMPUTE  W-OEB   =  W-NO       -   1
               MOVE     W-OEB      TO    JSTR-14C
           END-IF
           MOVE     TSHW-HNO   TO    JSTR-20.
           MOVE     TSHW-THT   TO    JSTR-14D.
           MOVE     W-NO       TO    W-TK1.
           PERFORM  TEK-RTN    THRU  TEK-EX.
           MOVE     W-TEK      TO    JSTR-15.
           MOVE     ZERO       TO    JSTR-15A.
           MOVE     1          TO    JSTR-16.
           MOVE     4          TO    JSTR-30.
           IF  JSTR-14      =   9
               MOVE     1          TO    JSTR-4012
               MOVE     9          TO    JSTR-17
           ELSE
               MOVE     0          TO    JSTR-4012
               MOVE     0          TO    JSTR-17
           END-IF
           MOVE     0          TO    JSTR-158.
           MOVE     STN-NO2    TO    JSTR-4011.
      *           WRITE    JSTR-R     INVALID
      *//////////////
           CALL "DB_Insert" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-260
           END-IF
           GO  TO M-280.
       M-260.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           GO TO M-240.
       M-280.
           MOVE     SPACE      TO    DNKN-R.
           INITIALIZE                DNKN-R.
           MOVE     W-SOK      TO    DNKN-01.
           MOVE     TSHW-HCD   TO    DNKN-02.
           MOVE     3          TO    DNKN-03.
           MOVE     W-SNO      TO    DNKN-041.
           MOVE     W-GNO      TO    DNKN-042.
      *           WRITE    DNKN-R     INVALID
      *//////////////
           CALL "DB_Insert" USING
            JT-DNKN_PNAME1 JT-DNKN_LNAME DNKN-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-300
           END-IF
           GO  TO M-400.
       M-300.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-DNKN_IDLST JT-DNKN_PNAME1.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" JT-DNKN_PNAME1 "SHARED" BY REFERENCE JT-DNKN_IDLST "1"
            "DNKN-KEY" BY REFERENCE DNKN-KEY.
           GO TO M-280.
       M-400.
      *           READ TSHW AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TSHW_PNAME1 BY REFERENCE TSHW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-900
           END-IF
           IF (TSHW-TCD  =  W-TCD)  AND  (TSHW-CCD  =  W-CCD) AND
              (TSHW-TPC  =  W-TPC)  AND  (TSHW-SOK  =  W-SOK) AND
              (TSHW-UNS  =  W-UNS)  AND
              (TSHW-ISU  =  W-ISU)  AND  (TSHW-THT  =  W-THT) AND
              (TSHW-TTE  =  W-TTE)
               GO  TO  M-220
           END-IF
           IF (TSHW-TCD  =  W-TCD)  AND  (TSHW-CCD  =  W-CCD) AND
                                         (TSHW-SOK  =  W-SOK) AND
              (TSHW-UNS  =  W-UNS)
               GO  TO  M-160
           END-IF
      *
           PERFORM TDR-RTN THRU TDR-EX.
           IF  W-DC = 1
               GO  TO  M-100
           END-IF
           CALL "SD_Output" USING "E-ME78" E-ME78 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           GO TO M-960.
       M-900.
           PERFORM TDR-RTN THRU TDR-EX.
           IF  W-DC NOT = 1
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-960.
           CALL "DB_F_Close" USING BY REFERENCE TSHW_IDLST TSHW_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TDIF_IDLST TDIF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-DNKN_IDLST JT-DNKN_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WTNAF_IDLST WTNAF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       TEK-RTN.
           MOVE     W-NO       TO    W-TK1.
           IF  W-TCD      NOT  =     9850
               MOVE     TSHW-TTE   TO    W-TK2
               GO  TO  TEK-EX
           END-IF
           IF  W-TPC           =     ZERO  OR  9998  OR  9999
               MOVE     TSHW-TTE   TO    W-TK2
               GO  TO  TEK-EX
           END-IF
           MOVE     W-TPC      TO    W-TK21.
           MOVE     WTNA-TNA   TO    W-TK22.
           IF  TSHW-TTE        =     SPACE
               GO  TO  TEK-EX
           END-IF
           MOVE     W-TK22     TO    W-TEKI.
           MOVE     17         TO    CNT1.
       TEK-010.
           SUBTRACT   1      FROM    CNT1.
           IF  CNT1            =     ZERO
               GO  TO  TEK-020
           END-IF
           IF  W-TK(CNT1)      =     SPACE
               GO  TO  TEK-010
           END-IF
           ADD        1        TO    CNT1.
       TEK-020.
           MOVE     TSHW-TTE   TO    W-TTEA.
           MOVE     ZERO       TO    CNT2.
       TEK-030.
           ADD        1        TO    CNT1  CNT2.
           IF  CNT1            <     17
               MOVE     W-TE(CNT2) TO    W-TK(CNT1)
               GO  TO  TEK-030
           END-IF
           MOVE     W-TEKI     TO    W-TK22.
       TEK-EX.
           EXIT.
       TDR-RTN.
           MOVE 0     TO W-DC.
      *           SELECT TDIF  WHERE  TDI-DATE = W-NGPS
      *                          AND  TDI-TCD = W-TCD AND  TDI-CCD = W-CCD
      *                          AND  TDI-SOK = W-SOK AND  TDI-UNS = W-UNS
      *                          AND  TDI-PRC = 2 AND  TDI-UPC = 0.
      *///////////////
           CALL "DB_Select" USING
            TDIF_PNAME1 "WHERE" 
            "TDI-DATE" "=" W-TCD "AND"
            "TDI-CCD" "=" W-CCD "AND"
            "TDI-SOK" "=" W-SOK "AND"
            "TDI-UNS" "=" W-UNS "AND"
            "TDI-PRC" "=" "2" "AND"
            "TDI-UPC" "=" "0" RETURNING RET.
       TDR-010.
      *           READ TDIF AT END 
      *///////////////
           CALL "DB_Read" USING
            "AT END" TDIF_PNAME1 BY REFERENCE TDI-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_Scratch" USING TDIF_PNAME1
               GO TO TDR-EX
           END-IF
           MOVE 1 TO TDI-UPC.
      *           REWRITE  TDI-R     INVALID
      *///////////////
           CALL "DB_Update" USING
            TDIF_PNAME1 TDIF_LNAME TDI-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME18" E-ME18 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF
           GO TO TDR-010.
       TDR-EX.
           EXIT.

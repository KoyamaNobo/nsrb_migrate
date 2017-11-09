       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS46U.
      *********************************************************
      *    PROGRAM         :  ワークマンＥＯＳ自動指図        *
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
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-OKC          PIC  9(001).
           02  W-DATE         PIC  9(008).
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
           02  W-CCD          PIC  9(003).
           02  W-TENC         PIC  9(004).
           02  W-ISU          PIC  9(003).
           02  W-ONO          PIC  9(006).
           02  W-SNO          PIC  9(006).
           02  W-OEB          PIC  9(002).
           02  W-NO           PIC  9(002).
           02  W-GNO          PIC  9(001).
           02  W-DC           PIC  9(001).
           02  WRI-SW         PIC  9(001).
           02  W-TEKI.
             03  F            PIC  N(001) VALUE "№".
             03  W-TK1        PIC  N(002).
             03  F            PIC  N(001) VALUE SPACE.
             03  W-TK2        PIC  N(004).
             03  W-TK3        PIC  N(015).
           02  W-15    REDEFINES W-TEKI  PIC  N(023).
      *
           COPY     LIBFDD.
           COPY     LITDNW.
           COPY     L-JSTR.
           COPY     L-JCON.
           COPY     LOKJF.
           COPY     LTDNKN.
           COPY     LNJZAI.
      *FD  WTNAF
       01  WTNAF_JHS46U.
           02  WTNAF_PNAME1   PIC  X(005) VALUE "WTNAF".
           02  F              PIC  X(001).
           02  WTNAF_LNAME    PIC  X(012) VALUE "WTNAF_JHS46U".
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
      *FD  SHWW
       01  SHWW_JHS46U.
           02  SHWW_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SHWW_LNAME     PIC  X(011) VALUE "SHWW_JHS46U".
           02  F              PIC  X(001).
           02  SHWW_KEY1      PIC  X(100) VALUE SPACE.
           02  SHWW_SORT      PIC  X(100) VALUE SPACE.
           02  SHWW_IDLST     PIC  X(100) VALUE SPACE.
           02  SHWW_RES       USAGE  POINTER.
       01  SHWW-R.
           02  SHWW-CCD       PIC  9(003).
           02  SHWW-TENC      PIC  9(004).
           02  SHWW-ISU       PIC  9(003).
           02  SHWW-HCD       PIC  9(006).
           02  SHWW-SIZ       PIC  9(001).
           02  SHWW-ASU.
             03  SHWW-SUD   OCCURS  10.
               04  SHWW-SU    PIC S9(004).
           02  F              PIC  X(007).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　ワークマン　出荷指図書　作成　　＊＊＊".
           02  FILLER  PIC  X(024) VALUE
                "指図日   '  年   月   日".
           02  FILLER  PIC  X(22)
                   VALUE "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  ACP-AREA.
           02  ACP-NGP.
               03  A-NEN      PIC  9(02).
               03  A-GET      PIC  9(02).
               03  A-PEY      PIC  9(02).
           02  ACP-OKC     PIC  9(01).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2.
               04  FILLER  PIC  X(018) VALUE
                    "***  DATA ｴﾗｰ  ***".
               04  FILLER  PIC  9(003).
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  9(003).
             03  E-ME3.
               04  FILLER  PIC  X(017) VALUE
                    "***  JCON ﾅｼ  ***".
               04  FILLER  PIC  X(002).
             03  E-ME4.
               04  FILLER  PIC  X(018) VALUE
                    "***  WTNAF ﾅｼ  ***".
               04  FILLER  PIC  X(004).
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
             03  E-ME15.
               04  FILLER  PIC  X(027) VALUE
                    "***  NJZAI REWRITE ｴﾗｰ  ***".
               04  FILLER  PIC  X(008).
             03  E-ME16.
               04  FILLER  PIC  X(025) VALUE
                    "***  NJZAI WRITE ｴﾗｰ  ***".
               04  FILLER  PIC  X(008).
             03  E-ME18.
               04  FILLER  PIC  X(027) VALUE
                    "***  TDNWF REWRITE ｴﾗｰ  ***".
               04  FILLER  PIC  X(020).
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
            "C-MID" " " "0" "0" "94" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "1" "15" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "X" "12" "25" "24" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "X" "22" "43" "22" "02C-MID" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-NGP" " " "12" "0" "6" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "A-NEN" "9" "12" "35" "2" " " "ACP-NGP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-NEN" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-GET" "9" "12" "40" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-PEY" "9" "12" "45" "2" "A-GET" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-OKC" "9" "22" "60" "1" "ACP-NGP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-OKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "332" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "332" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" " " "24" "0" "28" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME2" "X" "24" "15" "18" " " "E-ME2" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME2" "9" "24" "40" "3" "01E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING
            "02E-ME2" BY REFERENCE W-CCD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03E-ME2" "9" "24" "44" "4" "02E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING
            "03E-ME2" BY REFERENCE W-TENC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04E-ME2" "9" "24" "49" "3" "03E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING
            "04E-ME2" BY REFERENCE W-ISU "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" " " "24" "0" "19" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME3" "X" "24" "15" "17" " " "E-ME3" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME3" "X" "24" "35" "2" "01E-ME3" " " RETURNING RESU.
       CALL "SD_From" USING
            "02E-ME3" BY REFERENCE JCON1-KEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME4" " " "24" "0" "22" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME4" "X" "24" "15" "18" " " "E-ME4" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME4" "X" "24" "35" "4" "01E-ME4" " " RETURNING RESU.
       CALL "SD_From" USING
            "02E-ME4" BY REFERENCE WTNA-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME11" " " "24" "0" "28" "E-ME4" " " RETURNING RESU.
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
            "E-ME15" " " "24" "0" "35" "E-ME14" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME15" "X" "24" "15" "27" " " "E-ME15" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME15" "X" "24" "45" "8" "01E-ME15" " " RETURNING RESU.
       CALL "SD_From" USING
            "02E-ME15" BY REFERENCE NJZAI-KEY "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME16" " " "24" "0" "33" "E-ME15" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME16" "X" "24" "15" "25" " " "E-ME16" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME16" "X" "24" "45" "8" "01E-ME16" " " RETURNING RESU.
       CALL "SD_From" USING
            "02E-ME16" BY REFERENCE NJZAI-KEY "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME18" " " "24" "0" "47" "E-ME16" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME18" "X" "24" "15" "27" " " "E-ME18" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME18" "X" "24" "45" "20" "01E-ME18" " " RETURNING RESU.
       CALL "SD_From" USING
            "02E-ME18" BY REFERENCE TDNW1-KEY "20" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-010.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-NGP.
           ACCEPT W-NGPS FROM  DATE.
           COPY LIBCPR.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NGP TO W-DATE.
           CALL "SD_Output" USING "ACP-NGP" ACP-NGP "p" RETURNING RESU.
      *
           PERFORM ACP-RTN THRU ACP-EX.
           IF  ESTAT      =  "P9"
               GO  TO  M-980
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SHWW_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SHWW_PNAME1 " " BY REFERENCE SHWW_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" WTNAF_PNAME1 "SHARED" BY REFERENCE WTNAF_IDLST "1"
            "WTNA-KEY" BY REFERENCE WTNA-KEY.
           CALL "DB_F_Open" USING
            "I-O" TDNWF_PNAME1 " " BY REFERENCE TDNWF_IDLST "1"
            "TDNW1-KEY" BY REFERENCE TDNW1-KEY.
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
            "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
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
      *           READ SHWW AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SHWW_PNAME1 BY REFERENCE SHWW-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF.
       M-100.
           MOVE      SHWW-CCD  TO      W-CCD.
           MOVE      1         TO      JCON1-01.
           MOVE      4         TO      JCON1-02.
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
           END-IF
           MOVE ZERO TO W-OEB W-NO.
       M-120.
           INITIALIZE                OKJF-R.
           MOVE     W-ONO      TO    OKJF-KEY.
           MOVE     1          TO    OKJF-02.
           MOVE     W-NGPS     TO    OKJF-03.
           MOVE     6          TO    OKJF-04.
           MOVE     W-CCD      TO    OKJF-05.
           ADD      9850000    TO    OKJF-05.
           MOVE     SPACE      TO    OKJF-06.
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
           MOVE      SHWW-TENC TO      W-TENC.
           MOVE      SHWW-ISU  TO      W-ISU.
           MOVE     W-TENC     TO    WTNA-KEY.
      *           READ     WTNAF      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" WTNAF_PNAME1 BY REFERENCE WTNA-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               MOVE SPACE TO WTNA-TNA
           END-IF
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
           ADD      1          TO    W-NO.
           IF  W-NO            >    88
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
           END-IF
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
           ADD      1          TO    W-GNO.
           IF  W-GNO           >     6
               GO  TO  M-160
           END-IF.
       M-240.
           MOVE     SPACE      TO    JSTR-R.
           INITIALIZE                JSTR-R.
           MOVE     W-SNO      TO    JSTR-01.
           MOVE     W-GNO      TO    JSTR-02.
           MOVE     0          TO    JSTR-03.
           MOVE     W-NGP      TO    JSTR-04.
           MOVE     SHWW-SU(01) TO    JSTR-111(01).
           MOVE     SHWW-SU(02) TO    JSTR-111(02).
           MOVE     SHWW-SU(03) TO    JSTR-111(03).
           MOVE     SHWW-SU(04) TO    JSTR-111(04).
           MOVE     SHWW-SU(05) TO    JSTR-111(05).
           MOVE     SHWW-SU(06) TO    JSTR-111(06).
           MOVE     SHWW-SU(07) TO    JSTR-111(07).
           MOVE     SHWW-SU(08) TO    JSTR-111(08).
           MOVE     SHWW-SU(09) TO    JSTR-111(09).
           MOVE     SHWW-SU(10) TO    JSTR-111(10).
           COMPUTE  JSTR-112  =  SHWW-SU(01)  +  SHWW-SU(02)
                              +  SHWW-SU(03)  +  SHWW-SU(04)
                              +  SHWW-SU(05)  +  SHWW-SU(06)
                              +  SHWW-SU(07)  +  SHWW-SU(08)
                              +  SHWW-SU(09)  +  SHWW-SU(10).
           MOVE     9850       TO    JSTR-061.
           MOVE     W-CCD      TO    JSTR-062.
           MOVE     6          TO    JSTR-07.
           MOVE     ZERO       TO    JSTR-08.
           MOVE     SHWW-HCD   TO    JSTR-09.
           MOVE     SHWW-SIZ   TO    JSTR-10.
           MOVE     0          TO    JSTR-13.
           MOVE     1          TO    JSTR-14.
           MOVE     0          TO    JSTR-14A.
           MOVE     W-ONO      TO    JSTR-14B.
           COMPUTE  W-OEB   =  W-NO       -  1.
           MOVE     W-OEB      TO    JSTR-14C.
           MOVE     SPACE      TO    JSTR-14D.
           MOVE     SPACE      TO    JSTR-15.
           MOVE     SPACE      TO    W-TK1   W-TK2   W-TK3.
           MOVE     W-NO       TO    W-TK1.
           MOVE     W-TENC     TO    W-TK2.
           MOVE     WTNA-TNA   TO    W-TK3.
           MOVE     W-15       TO    JSTR-15.
           MOVE     SPACE      TO    JSTR-20.
           MOVE     ZERO       TO    JSTR-15A.
           MOVE     1          TO    JSTR-16.
           MOVE     1          TO    JSTR-30.
           MOVE     0          TO    JSTR-4012.
           MOVE     0          TO    JSTR-17.
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
           MOVE     6          TO    DNKN-01.
           MOVE     SHWW-HCD   TO    DNKN-02.
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
           GO  TO M-320.
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
       M-320.
           IF  SHWW-HCD        >     999899
               GO  TO  M-400
           END-IF
           MOVE     6          TO    NJZAI-01.
           MOVE     SHWW-HCD   TO    NJZAI-02.
           MOVE     SHWW-SIZ   TO    NJZAI-03.
      *           READ     NJZAI      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-340
           END-IF
           PERFORM  NZS-RTN     THRU  NZS-EX.
      *           REWRITE  NJZAI-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           GO  TO  M-360.
       M-340.
           MOVE     SPACE      TO    NJZAI-R.
           INITIALIZE                NJZAI-R.
           MOVE     6          TO    NJZAI-01.
           MOVE     SHWW-HCD   TO    NJZAI-02.
           MOVE     SHWW-SIZ   TO    NJZAI-03.
           PERFORM  NZS-RTN     THRU  NZS-EX.
           PERFORM  NJW-RTN     THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  M-340
           END-IF.
       M-360.
           MOVE     9          TO    NJZAI-01.
           MOVE     SHWW-HCD   TO    NJZAI-02.
           MOVE     SHWW-SIZ   TO    NJZAI-03.
      *           READ     NJZAI      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-380
           END-IF
           PERFORM  NZS-RTN     THRU  NZS-EX.
      *           REWRITE  NJZAI-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           GO  TO  M-400.
       M-380.
           MOVE     SPACE      TO    NJZAI-R.
           INITIALIZE                NJZAI-R.
           MOVE     9          TO    NJZAI-01.
           MOVE     SHWW-HCD   TO    NJZAI-02.
           MOVE     SHWW-SIZ   TO    NJZAI-03.
           PERFORM  NZS-RTN     THRU  NZS-EX.
           PERFORM  NJW-RTN     THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  M-380
           END-IF.
       M-400.
      *           READ SHWW AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SHWW_PNAME1 BY REFERENCE SHWW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-900
           END-IF
           IF (SHWW-CCD  =  W-CCD)  AND
              (SHWW-TENC  =  W-TENC)  AND  (SHWW-ISU  =  W-ISU)
               GO  TO  M-220
           END-IF
           IF (SHWW-CCD  =  W-CCD)  AND  (SHWW-TENC  =  W-TENC)
               GO  TO  M-160
           END-IF
      *
           PERFORM TDR-RTN THRU TDR-EX.
           IF  W-DC NOT = 2
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF
           IF  SHWW-CCD  =  W-CCD
               GO  TO  M-160
           END-IF
           GO  TO  M-100.
       M-900.
           PERFORM TDR-RTN THRU TDR-EX.
           IF  W-DC NOT = 2
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-960.
           CALL "DB_F_Close" USING
            BY REFERENCE SHWW_IDLST SHWW_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WTNAF_IDLST WTNAF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE OKJF_IDLST OKJF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-DNKN_IDLST JT-DNKN_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "P9"
               GO  TO  ACP-EX
           END-IF
           IF  ESTAT  NOT =  "00" AND "01" AND "06"
               GO  TO  ACP-RTN
           END-IF
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
       ACP-010.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACP-RTN
           END-IF
           IF  ESTAT  NOT =  "00" AND "01" AND "06"
               GO  TO  ACP-010
           END-IF
           CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU.
           IF  (W-NEN2 = ZERO) AND  (W-GET = ZERO)
               GO  TO  ACP-020
           END-IF
           IF  (W-GET <  1)  OR  (W-GET >  12)
               GO  TO  ACP-010
           END-IF.
       ACP-020.
           CALL "SD_Accept" USING BY REFERENCE A-PEY "A-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACP-010
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-020
           END-IF
           CALL "SD_Output" USING "A-PEY" A-PEY "p" RETURNING RESU.
           IF  (W-PEY =  ZERO)  AND  (W-GET = ZERO)
               MOVE W-DATE TO W-NGP
               CALL "SD_Output" USING
                "ACP-NGP" ACP-NGP "p" RETURNING RESU
               GO TO ACP-030
           END-IF
           IF  (W-PEY <  1)  OR  (W-PEY >  31)
               GO  TO  ACP-020
           END-IF
           MOVE  ZERO      TO  W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-NGP <  W-DATE
               GO  TO  ACP-RTN
           END-IF.
       ACP-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACP-020
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-030
           END-IF
           IF  W-OKC      =  9
               GO  TO  ACP-RTN
           END-IF
           IF  W-OKC  NOT =  1
               GO  TO  ACP-030
           END-IF.
       ACP-EX.
           EXIT.
       NZS-RTN.
           ADD      SHWW-SU(01) TO    NJZAI-0911(01).
           ADD      SHWW-SU(02) TO    NJZAI-0911(02).
           ADD      SHWW-SU(03) TO    NJZAI-0911(03).
           ADD      SHWW-SU(04) TO    NJZAI-0911(04).
           ADD      SHWW-SU(05) TO    NJZAI-0911(05).
           ADD      SHWW-SU(06) TO    NJZAI-0911(06).
           ADD      SHWW-SU(07) TO    NJZAI-0911(07).
           ADD      SHWW-SU(08) TO    NJZAI-0911(08).
           ADD      SHWW-SU(09) TO    NJZAI-0911(09).
           ADD      SHWW-SU(10) TO    NJZAI-0911(10).
       NZS-EX.
           EXIT.
       NJW-RTN.
           MOVE     0          TO    WRI-SW.
      *           WRITE  NJZAI-R    INVALID
      *//////////////
           CALL "DB_Insert" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME16" E-ME16 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  NJW-010
           END-IF
           GO  TO NJW-EX.
       NJW-010.
           IF  ERR-STAT NOT = "24"
               MOVE 2 TO WRI-SW
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO NJW-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           MOVE 1 TO WRI-SW.
       NJW-EX.
           EXIT.
       TDR-RTN.
           MOVE 0     TO W-DC.
           MOVE ZERO  TO TDNW1-KEY.
           MOVE 07    TO TDNW1-SCD.
           MOVE W-TENC TO TDNW1-TCD.
      *           START TDNWF KEY NOT < TDNW1-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TDNWF_PNAME1 "TDNW1-KEY" " NOT < " TDNW1-KEY RETURNING RET.
           IF  RET = 1
               GO TO TDR-EX
           END-IF.
       TDR-010.
      *           READ TDNWF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNWF_PNAME1 BY REFERENCE TDNW-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO TDR-EX
           END-IF
           IF  W-TENC NOT =  TDNW1-TCD
               GO TO TDR-EX
           END-IF
           IF  TDNW1-HC   =  1
               GO TO TDR-010
           END-IF
           MOVE 1 TO TDNW1-HC.
      *           REWRITE  TDNW-R1    INVALID
      *///////////////
           CALL "DB_Update" USING
            TDNWF_PNAME1 TDNWF_LNAME TDNW-R1 RETURNING RET.
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
           IF  W-DC = 1
               MOVE 2 TO W-DC
           END-IF
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF
           GO TO TDR-010.
       TDR-EX.
           EXIT.

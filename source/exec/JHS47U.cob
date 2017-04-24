       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS47U.
      *********************************************************
      *    PROGRAM         :  ナフコＥＯＳ自動指図            *
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
           02  W-SNGP         PIC  9(008).
           02  W-SNGP2        PIC  9(008).
           02  W-NNGP         PIC  9(008).
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
           02  W-HJC          PIC  9(002).
           02  W-CCD          PIC  9(003).
           02  W-NHB          PIC  9(001).
           02  W-NHBD         PIC  9(001).
           02  W-DNO          PIC  9(007).
           02  W-ONO          PIC  9(006).
           02  W-SNO          PIC  9(006).
           02  W-OEB          PIC  9(002).
           02  W-GNO          PIC  9(001).
           02  W-DC           PIC  9(001).
           02  WRI-SW         PIC  9(001).
           02  W-TEK          PIC  N(023).
           02  W-TEKD  REDEFINES W-TEK.
             03  F            PIC  N(007).
             03  W-TEKI       PIC  N(005).
             03  F            PIC  N(011).
           02  W-DUR.
             03  W-DURD       PIC  X(010).
             03  F            PIC  X(016).
      *
           COPY     LIBFDD.
           COPY     LITDNN.
           COPY     L-JSTR.
           COPY     L-JCON.
           COPY     LOKJF.
           COPY     LTDNKN.
           COPY     LNJZAI.
      *FD  SHNW
       01  SHNW_JHS47U.
           02  SHNW_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SHNW_LNAME     PIC  X(011) VALUE "SHNW_JHS47U".
           02  F              PIC  X(001).
           02  SHNW_KEY1      PIC  X(100) VALUE SPACE.
           02  SHNW_SORT      PIC  X(100) VALUE SPACE.
           02  SHNW_IDLST     PIC  X(100) VALUE SPACE.
           02  SHNW_RES       USAGE  POINTER.
       01  SHNW-R.
           02  SHNW-HJC       PIC  9(002).
           02  SHNW-CCD       PIC  9(003).
           02  SHNW-DNO       PIC  9(007).
           02  SHNW-HCD       PIC  9(006).
           02  SHNW-SIZ       PIC  9(001).
           02  SHNW-ASU.
             03  SHNW-SUD   OCCURS  10.
               04  SHNW-SU    PIC S9(004).
           02  SHNW-ISU       PIC  9(003).
           02  SHNW-NHB       PIC  9(001).
           02  SHNW-NFN       PIC  9(001).
       77  F                  PIC  X(001).
      *
      *****   ナフコ数量ファイル 45/5   ********************************
      *FD  NSURYOF
       01  NSURYOF_JHS47U.
           02  NSURYOF_PNAME1 PIC  X(007) VALUE "NSURYOF".
           02  F              PIC  X(001).
           02  NSURYOF_LNAME  PIC  X(014) VALUE "NSURYOF_JHS47U".
           02  F              PIC  X(001).
           02  NSURYOF_KEY1   PIC  X(100) VALUE SPACE.
           02  NSURYOF_SORT   PIC  X(100) VALUE SPACE.
           02  NSURYOF_IDLST  PIC  X(100) VALUE SPACE.
           02  NSURYOF_RES    USAGE  POINTER.
       01  NSURYO-R.
           02  NSURYO-01      PIC 9(06).
           02  NSURYO-02      PIC 9(06).
           02  NSURYO-03      PIC 9(03).
           02  NSURYO-04      PIC 9(08).
           02  NSURYO-05      PIC 9(05).
           02  NSURYO-051     PIC 9(01).
           02  NSURYO-06      PIC 9(08).
           02  NSURYO-07      PIC 9(04).
           02  NSURYO-08      PIC 9(02).
           02  NSURYO-09      PIC 9(02).
       77  F                  PIC X(01).
      *****   ナフコ箱数ファイル 36/7   ********************************
      *FD  NHAKOF
       01  NHAKOF_JHS47U.
           02  NHAKOF_PNAME1  PIC  X(006) VALUE "NHAKOF".
           02  F              PIC  X(001).
           02  NHAKOF_LNAME   PIC  X(013) VALUE "NHAKOF_JHS47U".
           02  F              PIC  X(001).
           02  NHAKOF_KEY1    PIC  X(100) VALUE SPACE.
           02  NHAKOF_SORT    PIC  X(100) VALUE SPACE.
           02  NHAKOF_IDLST   PIC  X(100) VALUE SPACE.
           02  NHAKOF_RES     USAGE  POINTER.
       01  NHAKO-R.
           02  NHAKO-01       PIC 9(06).
           02  NHAKO-02       PIC 9(06).
           02  NHAKO-03       PIC 9(03).
           02  NHAKO-04       PIC X(01).
           02  NHAKO-05       PIC 9(06).
           02  NHAKO-06       PIC 9(08).
           02  NHAKO-07       PIC 9(04).
           02  NHAKO-08       PIC 9(02).
       77  F                  PIC X(01).
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　ナフコ　出荷指図書　作成　　＊＊＊".
           02  FILLER  PIC  X(047) VALUE 
                "指図日１ '  年   月   日  ,  ２ '  年   月   日". 
           02  FILLER  PIC  X(024) VALUE
                "納入日   '  年   月   日".
           02  FILLER  PIC  X(22)
                   VALUE "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  ACP-AREA.
           02  ACP-NGP.
               03  A-NEN      PIC  9(02).
               03  A-GET      PIC  9(02).
               03  A-PEY      PIC  9(02).
           02  ACP-NGP2. 
               03  A-NEN2     PIC  9(02).
               03  A-GET2     PIC  9(02).
               03  A-PEY2     PIC  9(02).
           02  ACP-NNGP.
               03  A-NNEN     PIC  9(02).
               03  A-NGET     PIC  9(02).
               03  A-NPEY     PIC  9(02).
           02  ACP-OKC     PIC  9(01).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2.
               04  FILLER  PIC  X(018) VALUE
                    "***  DATA ｴﾗｰ  ***".
               04  FILLER  PIC  9(003).
             03  E-ME3.
               04  FILLER  PIC  X(017) VALUE
                    "***  JCON ﾅｼ  ***".
               04  FILLER  PIC  X(002).
             03  E-ME7   PIC  X(025) VALUE
                  "***  NHAKOF WRITEｴﾗｰ  ***".
             03  E-ME8   PIC  X(026) VALUE
                  "***  NSURYOF WRITEｴﾗｰ  ***".
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
             03  E-ME17.
               04  FILLER  PIC  X(018) VALUE
                    "***  TDNNF ﾅｼ  ***".
               04  FILLER  PIC  9(003).
               04  FILLER  PIC  9(007).
             03  E-ME18.
               04  FILLER  PIC  X(027) VALUE
                    "***  TDNNF REWRITE ｴﾗｰ  ***".
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
            "C-MID" " " "0" "0" "137" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "15" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "12" "25" "47" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "14" "25" "24" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "22" "43" "22" "03C-MID" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "19" " " " " RETURNING RESU.
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
            "ACP-NGP2" " " "12" "0" "6" "ACP-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN2" "9" "12" "58" "2" " " "ACP-NGP2" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN2" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET2" "9" "12" "63" "2" "A-NEN2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET2" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PEY2" "9" "12" "68" "2" "A-GET2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PEY2" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-NNGP" " " "14" "0" "6" "ACP-NGP2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NNEN" "9" "14" "35" "2" " " "ACP-NNGP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NNEN" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NGET" "9" "14" "40" "2" "A-NNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NGET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NPEY" "9" "14" "45" "2" "A-NGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NPEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "22" "60" "1" "ACP-NNGP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "382" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "382" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" " " "24" "0" "21" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME2" "X" "24" "15" "18" " " "E-ME2" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME2" "9" "24" "45" "3" "01E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING
            "02E-ME2" BY REFERENCE W-CCD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" " " "24" "0" "19" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME3" "X" "24" "15" "17" " " "E-ME3" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME3" "X" "24" "35" "2" "01E-ME3" " " RETURNING RESU.
       CALL "SD_From" USING
            "02E-ME3" BY REFERENCE JCON1-KEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME7" "X" "24" "15" "25" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME8" "X" "24" "15" "26" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME11" " " "24" "0" "28" "E-ME8" " " RETURNING RESU.
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
            "E-ME17" " " "24" "0" "28" "E-ME16" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME17" "X" "24" "15" "18" " " "E-ME17" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME17" "9" "24" "45" "3" "01E-ME17" " " RETURNING RESU.
       CALL "SD_From" USING
            "02E-ME17" BY REFERENCE W-CCD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03E-ME17" "9" "24" "49" "7" "02E-ME17" " " RETURNING RESU.
       CALL "SD_From" USING
            "03E-ME17" BY REFERENCE W-DNO "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME18" " " "24" "0" "47" "E-ME17" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME18" "X" "24" "15" "27" " " "E-ME18" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME18" "X" "24" "45" "20" "01E-ME18" " " RETURNING RESU.
       CALL "SD_From" USING
            "02E-ME18" BY REFERENCE TDNN1-KEY "20" "0" RETURNING RESU.
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
      *
           PERFORM ACP-RTN THRU ACP-EX.
           IF  ESTAT      =  "P9"
               GO  TO  M-980
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SHNW_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" SHNW_PNAME1 " " BY REFERENCE SHNW_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" TDNNF_PNAME1 "SHARED" BY REFERENCE TDNNF_IDLST "1"
            "TDNN1-KEY" BY REFERENCE TDNN1-KEY.
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
           CALL "DB_F_Open" USING
            "EXTEND" NSURYOF_PNAME1 " " BY REFERENCE NSURYOF_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" NHAKOF_PNAME1 " " BY REFERENCE NHAKOF_IDLST "0".
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
      *           READ SHNW AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SHNW_PNAME1 BY REFERENCE SHNW-R " " RETURNING RET.
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
           MOVE      SHNW-HJC TO       W-HJC.
           MOVE      SHNW-DNO TO       W-DNO.
           MOVE      SHNW-CCD TO       W-CCD.
           MOVE      SHNW-NHB TO       W-NHB.
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
           END-IF
           MOVE ZERO TO W-OEB.
           IF  SHNW-NFN  =  2
               MOVE  W-SNGP     TO  W-NGP 
           ELSE 
               MOVE  W-SNGP2    TO  W-NGP
           END-IF.
       M-120.
           INITIALIZE                OKJF-R.
           MOVE     W-ONO      TO    OKJF-KEY.
           MOVE     1          TO    OKJF-02.
           MOVE     W-NGPS     TO    OKJF-03.
           MOVE     6          TO    OKJF-04.
           MOVE     W-CCD      TO    OKJF-05.
           ADD      5000000    TO    OKJF-05.
           MOVE     W-DNO      TO    OKJF-06.
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
           ADD      1          TO    W-GNO
           IF  W-GNO           >     6   ADD    1      TO  W-OEB
               GO  TO  M-160
           END-IF.
       M-240.
           MOVE     SPACE      TO    JSTR-R.
           INITIALIZE                JSTR-R.
           MOVE     W-SNO      TO    JSTR-01.
           MOVE     W-GNO      TO    JSTR-02.
           MOVE     0          TO    JSTR-03.
           MOVE     W-NGP      TO    JSTR-04.
           MOVE     SHNW-SU(01) TO    JSTR-111(01).
           MOVE     SHNW-SU(02) TO    JSTR-111(02).
           MOVE     SHNW-SU(03) TO    JSTR-111(03).
           MOVE     SHNW-SU(04) TO    JSTR-111(04).
           MOVE     SHNW-SU(05) TO    JSTR-111(05).
           MOVE     SHNW-SU(06) TO    JSTR-111(06).
           MOVE     SHNW-SU(07) TO    JSTR-111(07).
           MOVE     SHNW-SU(08) TO    JSTR-111(08).
           MOVE     SHNW-SU(09) TO    JSTR-111(09).
           MOVE     SHNW-SU(10) TO    JSTR-111(10).
           COMPUTE  JSTR-112  =  SHNW-SU(01)  +  SHNW-SU(02)
                              +  SHNW-SU(03)  +  SHNW-SU(04)
                              +  SHNW-SU(05)  +  SHNW-SU(06)
                              +  SHNW-SU(07)  +  SHNW-SU(08)
                              +  SHNW-SU(09)  +  SHNW-SU(10).
           MOVE     5000       TO    JSTR-061.
           MOVE     W-CCD      TO    JSTR-062.
           MOVE     6          TO    JSTR-07.
           MOVE     ZERO       TO    JSTR-08.
           MOVE     SHNW-HCD   TO    JSTR-09.
           MOVE     SHNW-SIZ   TO    JSTR-10.
           MOVE     0          TO    JSTR-13.
           MOVE     1          TO    JSTR-14.
           MOVE     1          TO    JSTR-14A.
           MOVE     W-ONO      TO    JSTR-14B.
           MOVE     W-OEB      TO    JSTR-14C.
           MOVE     SPACE      TO    JSTR-14D.
           MOVE     W-DNO      TO    JSTR-14D.
           MOVE     SPACE      TO    JSTR-15.
           IF  W-CCD = 115
               MOVE "ワーク館分" TO W-TEKI
               MOVE W-TEK TO JSTR-15
           END-IF
           IF  W-CCD = 196
               MOVE SPACE TO W-TEK
               IF  W-NHB = 1
                   MOVE "ハード館分" TO W-TEKI
                   MOVE W-TEK TO JSTR-15
               ELSE
                   IF  W-NHB = 2
                       MOVE "生活館分　" TO W-TEKI
                       MOVE W-TEK TO JSTR-15
                   END-IF
               END-IF
           END-IF
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
           MOVE     SHNW-HCD   TO    DNKN-02.
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
           IF  SHNW-HCD        >     999899
               GO  TO  M-400
           END-IF
           MOVE     6          TO    NJZAI-01.
           MOVE     SHNW-HCD   TO    NJZAI-02.
           MOVE     SHNW-SIZ   TO    NJZAI-03.
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
           MOVE     SHNW-HCD   TO    NJZAI-02.
           MOVE     SHNW-SIZ   TO    NJZAI-03.
           PERFORM  NZS-RTN     THRU  NZS-EX.
           PERFORM  NJW-RTN     THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  M-340
           END-IF.
       M-360.
           MOVE     9          TO    NJZAI-01.
           MOVE     SHNW-HCD   TO    NJZAI-02.
           MOVE     SHNW-SIZ   TO    NJZAI-03.
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
           MOVE     SHNW-HCD   TO    NJZAI-02.
           MOVE     SHNW-SIZ   TO    NJZAI-03.
           PERFORM  NZS-RTN     THRU  NZS-EX.
           PERFORM  NJW-RTN     THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  M-380
           END-IF.
       M-400.
      *           READ SHNW AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SHNW_PNAME1 BY REFERENCE SHNW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-900
           END-IF
           IF (SHNW-HJC  =  W-HJC)  AND  (SHNW-CCD  =  W-CCD) AND
              (SHNW-NHB  =  W-NHB)
               GO  TO  M-220
           END-IF
      *
           PERFORM TDR-RTN THRU TDR-EX.
           IF  W-END = 9
               GO TO M-960
           END-IF
           IF  W-DC = 2
               GO  TO  M-100
           END-IF
           CALL "SD_Output" USING "E-ME78" E-ME78 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           GO TO M-960.
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
            BY REFERENCE NSURYOF_IDLST NSURYOF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NHAKOF_IDLST NHAKOF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SHNW_IDLST SHNW_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
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
               GO TO ACP-025
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
       ACP-025.
           MOVE  W-NGP     TO  W-SNGP.
       ACP-026. 
           CALL "SD_Accept" USING BY REFERENCE A-NEN2 "A-NEN2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09" 
               MOVE  W-SNGP     TO  W-NGP
               GO  TO  ACP-020
           END-IF
           IF  ESTAT  NOT =  "00" AND "01" AND "06"
               GO  TO  ACP-026
           END-IF
           CALL "SD_Output" USING "A-NEN2" A-NEN2 "p" RETURNING RESU.
       ACP-027. 
           CALL "SD_Accept" USING BY REFERENCE A-GET2 "A-GET2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACP-026
           END-IF
           IF  ESTAT  NOT =  "00" AND "01" AND "06"
               GO  TO  ACP-027
           END-IF
           CALL "SD_Output" USING "A-GET2" A-GET2 "p" RETURNING RESU.
           IF  (W-NEN2 = ZERO) AND  (W-GET = ZERO)
               GO  TO  ACP-028
           END-IF
           IF  (W-GET <  1)  OR  (W-GET >  12)
               GO  TO  ACP-027
           END-IF.
       ACP-028. 
           CALL "SD_Accept" USING BY REFERENCE A-PEY2 "A-PEY2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACP-027
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-028
           END-IF
           CALL "SD_Output" USING "A-PEY2" A-PEY2 "p" RETURNING RESU.
           IF  (W-PEY =  ZERO)  AND  (W-GET = ZERO) 
               MOVE W-DATE TO W-NGP 
               CALL "SD_Output" USING
                "ACP-NGP2" ACP-NGP2 "p" RETURNING RESU
               GO TO ACP-029
           END-IF
           IF  (W-PEY <  1)  OR  (W-PEY >  31)
               GO  TO  ACP-028
           END-IF
           MOVE  ZERO      TO  W-NEN1. 
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1 
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2 
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-NGP <  W-DATE 
               GO  TO  ACP-026
           END-IF.
       ACP-029. 
           MOVE  W-NGP     TO  W-SNGP2. 
           IF  W-SNGP    >  W-SNGP2 
               GO  TO  ACP-026
           END-IF
      *
           ADD   1         TO  W-PEY.
           IF  W-PEY =  29  OR  30
               IF  W-GET   =  2
                   ADD   1         TO  W-GET
                   MOVE  1         TO  W-PEY
           END-IF
           IF  W-PEY =  31
               IF  W-GET   =  4 OR  6 OR  9 OR 11
                   ADD   1         TO  W-GET
                   MOVE  1         TO  W-PEY
           END-IF
           IF  W-PEY =  32
               IF  W-GET   =  1 OR  3 OR  5 OR  7 OR  8 OR 10 OR 12
                   ADD   1         TO  W-GET
                   MOVE  1         TO  W-PEY
                   IF  W-GET   =  13
                       ADD   1         TO  W-NEN2
                       MOVE  1         TO  W-GET
                       MOVE  5         TO  W-PEY
                   END-IF
               END-IF
           END-IF
           MOVE  W-NGP     TO  W-NNGP.
           CALL "SD_Output" USING
            "ACP-NNGP" ACP-NNGP "p" RETURNING RESU.
           GO  TO  ACP-090.
      *
       ACP-030.
           CALL "SD_Accept" USING BY REFERENCE A-NNEN "A-NNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               MOVE  W-SNGP2    TO  W-NGP
               GO  TO  ACP-028
           END-IF
           IF  ESTAT  NOT =  "00" AND "01" AND "06"
               GO  TO  ACP-030
           END-IF
           CALL "SD_Output" USING "A-NNEN" A-NNEN "p" RETURNING RESU.
       ACP-040.
           CALL "SD_Accept" USING BY REFERENCE A-NGET "A-NGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACP-030
           END-IF
           IF  ESTAT  NOT =  "00" AND "01" AND "06"
               GO  TO  ACP-040
           END-IF
           CALL "SD_Output" USING "A-NGET" A-NGET "p" RETURNING RESU.
           IF  (W-NEN2 = ZERO) AND  (W-GET = ZERO)
               GO  TO  ACP-050
           END-IF
           IF  (W-GET <  1)  OR  (W-GET >  12)
               GO  TO  ACP-040
           END-IF.
       ACP-050.
           CALL "SD_Accept" USING BY REFERENCE A-NPEY "A-NPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACP-040
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-050
           END-IF
           CALL "SD_Output" USING "A-NPEY" A-NPEY "p" RETURNING RESU.
           IF  (W-PEY <  1)  OR  (W-PEY >  31)
               GO  TO  ACP-050
           END-IF
           MOVE  ZERO      TO  W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE  W-NGP     TO  W-NNGP.
           IF  W-SNGP NOT <  W-NNGP
               GO  TO  ACP-030
           END-IF.
      *
       ACP-090.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACP-050
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-090
           END-IF
           IF  W-OKC      =  9
               GO  TO  ACP-RTN
           END-IF
           IF  W-OKC  NOT =  1
               GO  TO  ACP-090
           END-IF.
       ACP-EX.
           EXIT.
       NZS-RTN.
           ADD      SHNW-SU(01) TO    NJZAI-0911(01).
           ADD      SHNW-SU(02) TO    NJZAI-0911(02).
           ADD      SHNW-SU(03) TO    NJZAI-0911(03).
           ADD      SHNW-SU(04) TO    NJZAI-0911(04).
           ADD      SHNW-SU(05) TO    NJZAI-0911(05).
           ADD      SHNW-SU(06) TO    NJZAI-0911(06).
           ADD      SHNW-SU(07) TO    NJZAI-0911(07).
           ADD      SHNW-SU(08) TO    NJZAI-0911(08).
           ADD      SHNW-SU(09) TO    NJZAI-0911(09).
           ADD      SHNW-SU(10) TO    NJZAI-0911(10).
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
           MOVE SPACE TO TDNN1-KEY.
           MOVE W-HJC TO TDNN1-SCD.
           IF  W-CCD = 981
               MOVE 081 TO W-CCD
           END-IF
           MOVE W-CCD TO TDNN1-TCD.
      *           START TDNNF KEY NOT < TDNN1-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TDNNF_PNAME1 "TDNN1-KEY" " NOT < " TDNN1-KEY RETURNING RET.
           IF  RET = 1
               GO TO TDR-EX
           END-IF.
       TDR-010.
      *           READ TDNNF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO TDR-EX
           END-IF
           IF  TDNN1-HC   =  1
               GO TO TDR-010
           END-IF
           IF  TDNN1-DGN = ZERO
               IF  TDNN1-TCD NOT = 196
                   MOVE 0 TO W-NHBD
               ELSE
                   MOVE TDNN1-DUR TO W-DUR
                   IF  W-DURD = "ﾉｳﾋﾝﾊﾞｼｮ:0"
                       MOVE 1 TO W-NHBD
                   ELSE
                       IF  W-DURD = "ﾉｳﾋﾝﾊﾞｼｮ:1" OR "ﾉｳﾋﾝﾊﾞｼｮ:2"
                           MOVE 2 TO W-NHBD
                       ELSE
                           MOVE 0 TO W-NHBD
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  TDNN1-DGN = ZERO
               IF  TDNN1-TCD = 081
                   IF  TDNN1-NHB = 1 
                       MOVE 1 TO W-NHBD
                   END-IF
               END-IF
           END-IF
           IF (W-HJC  NOT =  TDNN1-SCD)  OR   (W-CCD  NOT =  TDNN1-TCD)
                                         OR   (W-NHB  NOT =  W-NHBD)
               IF  TDNN1-TCD = 081
                   GO TO TDR-010
               ELSE
                   GO TO TDR-EX
               END-IF
           END-IF
           MOVE 1 TO TDNN1-HC.
      *           REWRITE  TDNN-R1    INVALID
      *///////////////
           CALL "DB_Update" USING
            TDNNF_PNAME1 TDNNF_LNAME TDNN-R1 RETURNING RET.
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
      *
           IF TDNN1-DGN NOT = ZERO
               GO TO TDR-030.
       TDR-020.
           MOVE ZERO TO NHAKO-R.
           MOVE SPACE TO NHAKO-04.
           MOVE 191708 TO NHAKO-01 NHAKO-02.
           MOVE TDNN1-TCD TO NHAKO-03.
           MOVE TDNN1-NHB TO NHAKO-04.
           MOVE 1 TO NHAKO-05.
           MOVE W-NNGP TO NHAKO-06.
      *           WRITE NHAKO-R.
      *//////////////
           CALL "DB_Insert" USING
            NHAKOF_PNAME1 NHAKOF_LNAME NHAKO-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO TDR-010
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME7" E-ME7 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               MOVE 9 TO W-END
               GO TO TDR-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NHAKOF_IDLST NHAKOF_PNAME1.
           MOVE "NHAKOF       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" NHAKOF_PNAME1 " " BY REFERENCE NHAKOF_IDLST "0".
           GO TO TDR-020.
       TDR-030.
           MOVE ZERO TO NSURYO-R.
           MOVE 191708 TO NSURYO-01 NSURYO-02.
           MOVE TDNN2-TCD TO NSURYO-03.
           MOVE TDNN2-DNOD TO NSURYO-04.
           MOVE TDNN2-TSU TO NSURYO-05.
           IF  TDNN2-SU NOT = TDNN2-TSU
               MOVE 04 TO NSURYO-08
           END-IF
           MOVE W-NNGP TO NSURYO-06.
      *           WRITE NSURYO-R.
      *//////////////
           CALL "DB_Insert" USING
            NSURYOF_PNAME1 NSURYOF_LNAME NSURYO-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO TDR-010
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME8" E-ME8 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               MOVE 9 TO W-END
               GO TO TDR-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NSURYOF_IDLST NSURYOF_PNAME1.
           MOVE "NSURYOF      " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" NSURYOF_PNAME1 " " BY REFERENCE NSURYOF_IDLST "0".
           GO TO TDR-030.
       TDR-EX.
           EXIT.

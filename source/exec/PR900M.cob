       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PR900M.
      **************************************************************
      *****     財務仕訳ヒストリ　取引先・摘要他　修正入力     *****
      **************************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM100.
       OBJECT-COMPUTER.    NEAC-SYSTEM100.
       INPUT-OUTPUT        SECTION.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT            PIC X(02).
       01  W-DATA.
           02  W-DATE          PIC 9(08) VALUE ZERO.
           02  W-NGP           PIC 9(08).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NEN         PIC 9(04).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1      PIC 9(02).
               04  W-NEN2      PIC 9(02).
             03  W-GET         PIC 9(02).
             03  W-PEY         PIC 9(02).
           02  W-NGPL  REDEFINES W-NGP.
             03  F             PIC 9(02).
             03  W-NGPS        PIC 9(06).
           02  W-DNO           PIC 9(06).
           02  W-TCD           PIC 9(05).
           02  W-NAMEN         PIC N(10).
           02  W-ATKD.
             03  W-TKD  OCCURS  6.
               04  W-TKC       PIC 9(03).
               04  W-TKM       PIC N(20).
               04  W-KRCD      PIC 9(04).
               04  W-KSCD      PIC 9(04).
               04  W-TAX       PIC X(01).
           02  W-DMM           PIC 9(01).
           02  W-L             PIC 9(02).
           02  W-G             PIC 9(01).
           02  W-GNO           PIC 9(01).
           02  W-KRCDM         PIC 9(04).
           02  W-KSCDM         PIC 9(04).
      *
       COPY  LIBFDD.
       COPY  FCTL.
       COPY  TKLIB.
       COPY  LTKI.
      *
       01  SDH_PR900M.
           02  SDH_PNAME1   PIC  X(009)  VALUE "SIWAKE-H1".
           02  F            PIC  X(001).
           02  SDH_PNAME2   PIC  X(009)  VALUE "SIWAKE-H2".
           02  F            PIC  X(001).
           02  SDH_PNAME3   PIC  X(009)  VALUE "SIWAKE-H3".
           02  F            PIC  X(001).
           02  SDH_LNAME    PIC  X(003)  VALUE "SDH".
           02  F            PIC  X(001).
           02  SDH_KEY1     PIC  X(100)  VALUE SPACE.
           02  SDH_KEY2     PIC  X(100)  VALUE SPACE.
           02  SDH_SORT     PIC  X(100)  VALUE SPACE.
           02  SDH_IDLST    PIC  X(100)  VALUE SPACE.
           02  SDH_RES      USAGE  POINTER.
       COPY  SIWAKH.
       77  F                PIC  X(001).
      *
       77  ESTAT            PIC  X(002).
       77  RESU             PIC  9(001).
       77  RESP             PIC  9(001).
       77  RET              PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER    PIC  9(003).
       77  USER_ID          PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE  PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL      PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER.
             03  FILLER  PIC N(24) VALUE
                  "＊＊＊　　仕訳ヒストリ　取引先・摘要他　修正入力".
             03  FILLER  PIC N(05) VALUE "　　＊＊＊".
           02  FILLER.
             03  FILLER  PIC N(02) VALUE "日付".
             03  FILLER  PIC N(03) VALUE "伝票№".
             03  FILLER  PIC N(02) VALUE "終了".
             03  FILLER  PIC X(04) VALUE "=PF9".
           02  FILLER    PIC N(03) VALUE "取引先".
           02  FILLER.
             03  FILLER  PIC N(03) VALUE "税区分".
           02  FILLER.
             03  FILLER  PIC N(03) VALUE "摘要①".
           02  FILLER.
             03  FILLER  PIC N(01) VALUE "②".
           02  FILLER.
             03  FILLER  PIC N(01) VALUE "③".
           02  FILLER.
             03  FILLER  PIC N(01) VALUE "④".
           02  FILLER.
             03  FILLER  PIC N(01) VALUE "⑤".
           02  FILLER.
             03  FILLER  PIC N(01) VALUE "⑥".
           02  FILLER    PIC X(22) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-NGP     PIC 9(06).
             03  A-DNO     PIC 9(06).
           02  FILLER.
             03  A-TCD     PIC 9(05).
             03  A-NAMEN   PIC N(10).
           02  FILLER.
             03  A-TKC     PIC 9(03).
             03  A-TKM     PIC N(20).
             03  A-TAX     PIC X(01).
           02  A-DMM       PIC 9(01).
       01  C-DSP.
           02  FILLER.
             03  D-TNA     PIC N(10).
             03  D-TNAS    PIC N(10) VALUE
                  "　　　　　　　　　　".
           02  FILLER.
             03  D-TKCS    PIC X(03) VALUE "   ".
             03  D-TKM     PIC N(20).
       01  C-ERR.
           02  FILLER.
             03  E-STAT    PIC X(02).
             03  E-ME01    PIC X(23) VALUE
                  "***  ﾄﾘﾋｷｻｷﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME02    PIC X(21) VALUE
                  "***  ﾃｷﾖｳﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME03    PIC X(17) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME04    PIC X(18) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME05    PIC X(21) VALUE
                  "***  PROGRAM ｴﾗｰ  ***".
             03  E-ME06    PIC X(23) VALUE
                  "***  ｺﾝﾄﾛｰﾙﾌｧｲﾙ ﾅｼ  ***".
             03  E-ME10    PIC X(21) VALUE
                  "***  REWRITE ｴﾗｰ  ***".
             03  E-KEY     PIC X(17).
             03  E-ME98    PIC X(05) VALUE X"1B4A05".
             03  E-ME99    PIC X(05) VALUE X"1B4A05".
             03  E-CL      PIC X(50) VALUE
                  "                                                  ".
           COPY LIBSCR.
       PROCEDURE       DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "126" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" " " "1" "0" "58" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-MID" "N" "1" "12" "48" " " "01C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
           "0201C-MID" "N" "1" "60" "10" "0101C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" " " "4" "0" "18" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID" "N" "4" "11" "4" " " "02C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID" "N" "4" "25" "6" "0102C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0302C-MID" "N" "4" "46" "4" "0202C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0402C-MID" "X" "4" "50" "4" "0302C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "6" "11" "6" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" " " "7" "0" "6" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0104C-MID" "N" "7" "65" "6" " " "04C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" " " "8" "0" "6" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0105C-MID" "N" "8" "13" "6" " " "05C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" " " "9" "0" "2" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0106C-MID" "N" "9" "17" "2" " " "06C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" " " "10" "0" "2" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0107C-MID" "N" "10" "17" "2" " " "07C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" " " "11" "0" "2" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0108C-MID" "N" "11" "17" "2" " " "08C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" " " "12" "0" "2" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0109C-MID" "N" "12" "17" "2" " " "09C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" " " "13" "0" "2" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0110C-MID" "N" "13" "17" "2" " " "10C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-MID" "X" "20" "29" "22" "10C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "82" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "4" "0" "12" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-NGP" "9" "4" "16" "6" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-NGP" BY REFERENCE W-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DNO" "9" "4" "32" "6" "A-NGP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DNO" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-ACP" " " "6" "0" "25" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-TCD" "9" "6" "18" "5" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-TCD" BY REFERENCE W-TCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-NAMEN" "N" "6" "25" "20" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-NAMEN" BY REFERENCE W-NAMEN "20" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03C-ACP" " " "W-L" "0" "44" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-TKC" "9" "W-L" "20" "3" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-TKC" BY REFERENCE W-TKC(1) "3" "1"
            BY REFERENCE W-G 52 RETURNING RESU.
       CALL "SD_Init" USING
            "A-TKM" "N" "W-L" "25" "40" "A-TKC" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-TKM" BY REFERENCE W-TKM(1) "40" "1"
            BY REFERENCE W-G 52 RETURNING RESU.
       CALL "SD_Init" USING
            "A-TAX" "X" "W-L" "68" "1" "A-TKM" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-TAX" BY REFERENCE W-TAX(1) "1" "1"
            BY REFERENCE W-G 52 RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "46" "1" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "83" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "6" "0" "40" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "D-TNA" "N" "6" "25" "20" " " "01C-ACP" RETURNING RESU.
       CALL "SD_From" USING
            "D-TNA" BY REFERENCE W-NAMEN "20" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-TNAS" "N" "6" "25" "20" "D-TNA" " " RETURNING RESU.
       CALL "SD_Init" USING
            "02C-ACP" " " "W-L" "0" "43" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-TKCS" "X" "W-L" "20" "3" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "D-TKM" "N" "W-L" "25" "40" "D-TKCS" " " RETURNING RESU.
       CALL "SD_From" USING
            "D-TKM" BY REFERENCE W-TKM(1) "40" "1"
            BY REFERENCE W-G 52 RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "223" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "223" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME01" "X" "24" "15" "23" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME02" "X" "24" "15" "21" "E-ME01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME03" "X" "24" "15" "17" "E-ME02" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME04" "X" "24" "15" "18" "E-ME03" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME05" "X" "24" "15" "21" "E-ME04" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME06" "X" "24" "15" "23" "E-ME05" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME10" "X" "24" "15" "21" "E-ME06" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-KEY" "X" "24" "50" "17" "E-ME10" " " RETURNING RESU.
       CALL "SD_From" USING
            "E-KEY" BY REFERENCE SH-KEY1 "17" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-KEY" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p"
                                         RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE "TAX   " TO FCTL-KEY4.
      *           READ FCTL-F UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               CALL "SD_Output" USING "E-ME06" E-ME06 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN.
           MOVE TAX-CODE TO W-KRCDM.
           MOVE TAX-CODE1 TO W-KSCDM.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           COPY LIBCPR.
           CALL "DB_F_Open" USING
            "INPUT" TK_PNAME1 "SHARED" BY REFERENCE TK_IDLST "1"
            "TK-KEY" BY REFERENCE TK-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TKI_PNAME1 "SHARED" BY REFERENCE TKI_IDLST "1"
            "TKI-KEY" BY REFERENCE TKI-KEY.
           CALL "DB_F_Open" USING
            "I-O" SDH_PNAME1 "SHARED" BY REFERENCE SDH_IDLST "3"
            "SH-KEY1" BY REFERENCE SH-KEY1 "SH-KEY2" BY REFERENCE
            SH-KEY2 "SH-KEY3" BY REFERENCE SH-KEY3.
       M-100.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p"
                                         RETURNING RESU.
       M-120.
           CALL "SD_Accept" USING BY REFERENCE A-NGP "A-NGP" "9" "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU.
           IF  ESTAT = "P9"
               GO TO M-900
           END-IF.
           IF  ESTAT = "09"
               GO TO M-100
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-120
           END-IF.
           IF  W-NGPS = ZERO
               MOVE W-DATE TO W-NGP
               CALL "SD_Output" USING "A-NGP" A-NGP "p"
                                         RETURNING RESU
           END-IF.
           IF  W-GET < 1 OR > 12
               GO TO M-120
           END-IF.
           IF  W-PEY < 1 OR > 31
               GO TO M-120
           END-IF.
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF.
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
           MOVE SPACE TO SH-KEY1.
           MOVE W-NGP TO HTRDATE.
      *           START SDH KEY NOT < SH-KEY1 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDH_PNAME1 "SH-KEY1" " NOT < " SH-KEY1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME03" E-ME03 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-120
           END-IF
      *           READ SDH NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME03" E-ME03 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-120
           END-IF.
           IF  W-NGP NOT = HTRDATE
               CALL "SD_Output" USING "E-ME03" E-ME03 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-120
           END-IF.
       M-140.
           CALL "SD_Accept" USING BY REFERENCE A-DNO "A-DNO" "9" "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-CL" C-CL "p"
                                         RETURNING RESU.
           IF  ESTAT = "P9"
               GO TO M-900
           END-IF.
           IF  ESTAT = "09"
               GO TO M-120
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-140
           END-IF.
           MOVE SPACE TO SH-KEY1.
           MOVE W-NGP TO HTRDATE.
           MOVE W-DNO TO HJUNLNO.
      *           START SDH KEY NOT < SH-KEY1 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDH_PNAME1 "SH-KEY1" " NOT < " SH-KEY1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME03" E-ME03 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-140
           END-IF
      *           READ SDH NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME03" E-ME03 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-140
           END-IF.
           IF  W-DNO NOT = HJUNLNO
               CALL "SD_Output" USING "E-ME03" E-ME03 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-140
           END-IF.
           MOVE 999 TO W-TKC(1) W-TKC(2) W-TKC(3)
                       W-TKC(4) W-TKC(5) W-TKC(6).
           MOVE ALL "－" TO W-TKM(1) W-TKM(2) W-TKM(3)
                              W-TKM(4) W-TKM(5) W-TKM(6).
           MOVE ZERO TO W-KRCD(1) W-KRCD(2) W-KRCD(3)
                        W-KRCD(4) W-KRCD(5) W-KRCD(6)
                        W-KSCD(1) W-KSCD(2) W-KSCD(3)
                        W-KSCD(4) W-KSCD(5) W-KSCD(6).
           MOVE SPACE TO W-TAX(1) W-TAX(2) W-TAX(3)
                         W-TAX(4) W-TAX(5) W-TAX(6).
           MOVE ZERO TO W-GNO.
           MOVE HCUSTCD TO W-TCD.
           MOVE HNAMEN TO W-NAMEN.
       M-160.
           IF  W-GNO > 5
               CALL "SD_Output" USING "E-ME04" E-ME04 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-KEY" E-KEY "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-900
           END-IF.
           ADD 1 TO W-GNO.
           IF  W-GNO NOT = HLINENO
               CALL "SD_Output" USING "E-ME04" E-ME04 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-KEY" E-KEY "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-900
           END-IF.
           MOVE HTEKICD TO W-TKC(W-GNO).
           MOVE HTEKIYO TO W-TKM(W-GNO).
           IF  HDR-CR = 1
               MOVE HACCNTCD TO W-KRCD(W-GNO)
           ELSE
               MOVE HACCNTCD TO W-KSCD(W-GNO)
           END-IF.
           MOVE HETAX   TO W-TAX(W-GNO).
       M-180.
      *           READ SDH NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-200
           END-IF.
           IF  W-NGP NOT = HTRDATE
               GO TO M-200
           END-IF.
           IF  W-DNO NOT = HJUNLNO
               GO TO M-200
           END-IF.
           IF  W-GNO NOT = HLINENO
               GO TO M-160
           END-IF.
           IF  HDR-CR = 1
               MOVE HACCNTCD TO W-KRCD(W-GNO)
           ELSE
               MOVE HACCNTCD TO W-KSCD(W-GNO)
           END-IF.
           GO TO M-180.
       M-200.
           CALL "SD_Output" USING "A-TCD" A-TCD "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p"
                                         RETURNING RESU.
           MOVE ZERO TO W-G.
           MOVE 7 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L
                          RETURNING RESU.
       M-220.
           ADD 1 TO W-G.
           IF  W-G > 5
               GO TO M-240
           END-IF.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L
                          RETURNING RESU.
           IF  W-TKC(W-G) = 999
               CALL "SD_Output" USING "D-TKCS" D-TKCS "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "A-TKM" A-TKM "p"
                                         RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-TKC" A-TKC "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "A-TKM" A-TKM "p"
                                         RETURNING RESU
           END-IF
           CALL "SD_Output" USING "A-TAX" A-TAX "p"
                                         RETURNING RESU.
           GO TO M-220.
       M-240.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "5"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU.
           IF  ESTAT = "P0"
               MOVE 99999 TO W-TCD
               CALL "SD_Output" USING "A-TCD" A-TCD "p"
                                         RETURNING RESU
               GO TO M-260
           END-IF.
           IF  ESTAT = "09"
               GO TO M-140
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-240
           END-IF.
           IF  W-TCD = 99999
               GO TO M-260
           END-IF.
           IF  W-TCD = ZERO
               MOVE SPACE TO W-NAMEN
               CALL "SD_Output" USING "D-TNAS" D-TNAS "p"
                                         RETURNING RESU
               GO TO M-300
           END-IF.
           MOVE W-TCD TO TK-KEY.
      *           READ TK WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TK_PNAME1 BY REFERENCE TK-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME01" E-ME01 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-240
           END-IF.
           MOVE TK-NAMEN TO W-NAMEN.
           CALL "SD_Output" USING "A-TCD" A-TCD "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p"
                                         RETURNING RESU.
           GO TO M-300.
       M-260.
           CALL "SD_Accept" USING BY REFERENCE A-NAMEN "A-NAMEN" "N"
                "20" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-240
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-260
           END-IF.
           CALL "SD_Output" USING "A-TCD" A-TCD "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p"
                                         RETURNING RESU.
       M-300.
           MOVE ZERO TO W-G.
           MOVE 7 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L
                          RETURNING RESU.
       M-320.
           ADD 1 TO W-G W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L
                          RETURNING RESU.
           IF  W-G > 5
               GO TO M-500
           END-IF.
           IF  W-TKC(W-G) = 999
               GO TO M-320
           END-IF.
       M-340.
           CALL "SD_Accept" USING BY REFERENCE A-TKC "A-TKC" "9"
                "3" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-460
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-340
           END-IF.
           IF  W-TKC(W-G) = ZERO
               GO TO M-360
           END-IF.
           MOVE W-TKC(W-G) TO TKI-KEY.
      *           READ TKI WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TKI_PNAME1 BY REFERENCE TKI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME02" E-ME02 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p"
                                         RETURNING RESU
               GO TO M-340
           END-IF.
           MOVE TKI-02 TO W-TKM(W-G).
           CALL "SD_Output" USING "D-TKM" D-TKM "p"
                                         RETURNING RESU.
           GO TO M-380.
       M-360.
           CALL "SD_Accept" USING BY REFERENCE A-TKM "A-TKM" "N"
                "40" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-340
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-360
           END-IF.
           CALL "SD_Output" USING "D-TKM" D-TKM "p"
                                         RETURNING RESU.
       M-380.
           IF  W-KRCD(W-G) = W-KRCDM OR W-KSCDM
               GO TO M-400
           END-IF.
           IF  W-KSCD(W-G) = W-KRCDM OR W-KSCDM
               GO TO M-400
           END-IF.
           GO TO M-320.
       M-400.
           CALL "SD_Accept" USING BY REFERENCE A-TAX "A-TAX" "X"
                "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-360
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-400
           END-IF.
           IF  W-TAX(W-G) NOT = " " AND "1" AND "3" AND "5" AND "7"
               GO TO M-400
           END-IF.
           GO TO M-320.
       M-460.
           SUBTRACT 1 FROM W-G W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L
                          RETURNING RESU.
           IF  W-G = ZERO
               GO TO M-240.
           IF  W-TKC(W-G) = 999
               GO TO M-460.
           GO TO M-340.
       M-500.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO M-460
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-500
           END-IF.
           IF  W-DMM = 9
               GO TO M-120
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-500
           END-IF.
           MOVE W-NGP TO W-DATE.
           MOVE SPACE TO SH-KEY1.
           MOVE W-NGP TO HTRDATE.
           MOVE W-DNO TO HJUNLNO.
      *           START SDH KEY NOT < SH-KEY1 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDH_PNAME1 "SH-KEY1" " NOT < " SH-KEY1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME05" E-ME05 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-900
           END-IF
      *           READ SDH NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SDH_PNAME1 BY REFERENCE SH-REC " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME05" E-ME05 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-900
           END-IF.
           IF (W-NGP NOT = HTRDATE) OR (W-DNO NOT = HJUNLNO)
               CALL "SD_Output" USING "E-ME05" E-ME05 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-900
           END-IF.
       M-520.
           IF  W-TCD > 09999 AND < 30000
               MOVE 99999 TO HCUSTCD
           ELSE
               MOVE W-TCD TO HCUSTCD
           END-IF.
           MOVE W-NAMEN TO HNAMEN.
           MOVE W-TKM(HLINENO) TO HTEKIYO.
           MOVE W-TAX(HLINENO) TO HETAX.
      *           REWRITE  SH-REC       INVALID
      *///////////////
           CALL "DB_Update" USING
            SDH_PNAME1 SDH_LNAME SH-REC RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-STAT" E-STAT "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME10" E-ME10 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-KEY" E-KEY "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-900
           END-IF.
      *           READ SDH NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SDH_PNAME1 BY REFERENCE SH-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-100
           END-IF.
           IF (W-NGP NOT = HTRDATE) OR (W-DNO NOT = HJUNLNO)
               GO TO M-100
           END-IF.
           GO TO M-520.
       M-900.
           CALL "DB_F_Close" USING BY REFERENCE TK_IDLST TK_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TKI_IDLST TKI_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

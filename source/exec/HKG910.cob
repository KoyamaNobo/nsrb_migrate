       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG910.
      *********************************************************
      *    PROGRAM         :  全体累積ファイル　クリア        *
      *    PRINTER TYPE    :  *****                           *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/29                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERR-STAT           PIC  X(002).
       01  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-NGPD.
             03  W-NGD        PIC  9(006).
             03  W-NGDD  REDEFINES W-NGD.
               04  W-NEND     PIC  9(004).
               04  W-NENDL REDEFINES W-NEND.
                 05  W-NEND1  PIC  9(002).
                 05  W-NEND2  PIC  9(002).
               04  W-GETD     PIC  9(002).
             03  W-NGDL  REDEFINES W-NGD.
               04  F          PIC  9(002).
               04  W-NGDS     PIC  9(004).
             03  W-PEYD       PIC  9(002).
           02  W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-DTW1         PIC  9(003).
           02  W-DTW2         PIC  9(001).
           02  W-SDTD         PIC  9(008).
           02  W-SDTM  REDEFINES W-SDTD.
             03  W-SDTN       PIC  9(004).
             03  W-SDTG       PIC  9(002).
             03  W-SDTP       PIC  9(002).
           COPY LDAIW.
      *
           COPY LIBFDD.
           COPY LSNYUR.
           COPY LITTM.
           COPY LIHKBM.
           COPY LITUKF.
           COPY LIKKBM.
           COPY LITM.
           COPY LITSKF.
           COPY LISKDF.
      *FD  TUKOF
       01  TUKOF_HKG910.
           02  TUKOF_PNAME1   PIC  X(005) VALUE "TUKOF".
           02  F              PIC  X(001).
           02  TUKOF_LNAME    PIC  X(012) VALUE "TUKOF_HKG910".
           02  F              PIC  X(001).
           02  TUKOF_KEY1     PIC  X(100) VALUE SPACE.
           02  TUKOF_SORT     PIC  X(100) VALUE SPACE.
           02  TUKOF_IDLST    PIC  X(100) VALUE SPACE.
           02  TUKOF_RES      USAGE  POINTER.
       01  TUKO-R.
           02  TUKO-KEY.
             03  TUKO-TCD     PIC  9(004).
             03  TUKO-DAI     PIC  X(010).
           02  F              PIC  X(004).
           02  TUKO-DATE      PIC  9(008).
           02  F              PIC  X(038).
       77  F                  PIC  X(001).
      *FD  TUKOYR
       01  TUKOYR_HKG910.
           02  TUKOYR_PNAME1  PIC  X(006) VALUE "TUKOYR".
           02  F              PIC  X(001).
           02  TUKOYR_LNAME   PIC  X(013) VALUE "TUKOYR_HKG910".
           02  F              PIC  X(001).
           02  TUKOYR_KEY1    PIC  X(100) VALUE SPACE.
           02  TUKOYR_SORT    PIC  X(100) VALUE SPACE.
           02  TUKOYR_IDLST   PIC  X(100) VALUE SPACE.
           02  TUKOYR_RES     USAGE  POINTER.
       01  TUKOY-R.
           02  TUKOY-KEY.
             03  TUKOY-TCD    PIC  9(004).
             03  TUKOY-DAI    PIC  X(010).
           02  F              PIC  X(004).
           02  TUKOY-DATE     PIC  9(008).
           02  F              PIC  X(038).
       77  F                  PIC  X(001).
      *FD  TSKFYR
       01  TSKFYR_HKG910.
           02  TSKFYR_PNAME1  PIC  X(006) VALUE "TSKFYR".
           02  F              PIC  X(001).
           02  TSKFYR_LNAME   PIC  X(013) VALUE "TSKFYR_HKG910".
           02  F              PIC  X(001).
           02  TSKFYR_KEY1    PIC  X(100) VALUE SPACE.
           02  TSKFYR_SORT    PIC  X(100) VALUE SPACE.
           02  TSKFYR_IDLST   PIC  X(100) VALUE SPACE.
           02  TSKFYR_RES     USAGE  POINTER.
       01  TSKY-R.
           02  F              PIC  X(250).
           02  TSKY-NG        PIC  9(006).
       77  F                  PIC  X(001).
      *FD  SKDFYR
       01  SKDFYR_HKG910.
           02  SKDFYR_PNAME1  PIC  X(006) VALUE "SKDFYR".
           02  F              PIC  X(001).
           02  SKDFYR_LNAME   PIC  X(013) VALUE "SKDFYR_HKG910".
           02  F              PIC  X(001).
           02  SKDFYR_KEY1    PIC  X(100) VALUE SPACE.
           02  SKDFYR_SORT    PIC  X(100) VALUE SPACE.
           02  SKDFYR_IDLST   PIC  X(100) VALUE SPACE.
           02  SKDFYR_RES     USAGE  POINTER.
       01  SKDY-R.
           02  F              PIC  X(180).
           02  SKDY-NG        PIC  9(006).
           02  F              PIC  X(006).
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　履物・工品　更新・クリア　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATEM ﾅｼ  ***".
             03  E-ME3   PIC  X(027) VALUE
                  "***  DATEM REWRITE ｴﾗｰ  ***".
             03  E-ME5   PIC  X(028) VALUE
                  "***   TUKOF DELETE ｴﾗｰ   ***".
             03  E-ME6   PIC  X(027) VALUE
                  "***   TUKOF WRITE ｴﾗｰ   ***".
             03  E-ME7   PIC  X(027) VALUE
                  "***   TUKF DELETE ｴﾗｰ   ***".
             03  E-ME8   PIC  X(017) VALUE
                  "***  KKBM ﾅｼ  ***".
             03  E-ME9   PIC  X(026) VALUE
                  "***  KKBM REWRITE ｴﾗｰ  ***".
             03  E-ME11  PIC  X(026) VALUE
                  "***  TT-M REWRITE ｴﾗ-  ***".
             03  E-ME12  PIC  X(028) VALUE
                  "***   TSKF REWRITE ｴﾗｰ   ***".
             03  E-ME13  PIC  X(027) VALUE
                  "***   TSKF DELETE ｴﾗｰ   ***".
             03  E-ME14  PIC  X(026) VALUE
                  "***   SKDF WRITE ｴﾗｰ   ***".
             03  E-ME15  PIC  X(027) VALUE
                  "***   SKDF DELETE ｴﾗｰ   ***".
             03  E-ME21  PIC  X(017) VALUE
                  "***  HKBM ﾅｼ  ***".
             03  E-ME22  PIC  X(026) VALUE
                  "***  HKBM REWRITE ｴﾗｰ  ***".
             03  E-ME23  PIC  X(024) VALUE
                  "***  HKBM WRITE ｴﾗｰ  ***".
             03  E-ME28  PIC  X(026) VALUE
                  "***   TUKF WRITE ｴﾗｰ   ***".
             03  E-ME29  PIC  X(028) VALUE
                  "***   TUKOYR WRITE ｴﾗｰ   ***".
             03  E-ME30  PIC  X(028) VALUE
                  "***   SKDFYR WRITE ｴﾗｰ   ***".
             03  E-ME31  PIC  X(028) VALUE
                  "***   TSKFYR WRITE ｴﾗｰ   ***".
             03  E-ME50  PIC  X(026) VALUE
                  "***   ﾀﾞｲﾁｮｳNO ｵｰﾊﾞｰ   ***".
             03  E-TTM   PIC  X(004).
             03  E-TUK   PIC  X(014).
             03  E-TUKO  PIC  X(014).
             03  E-TSK   PIC  X(004).
             03  E-SKD   PIC  X(020).
           COPY LSSEM.
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "308" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "563" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "563" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "27" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "28" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "27" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "27" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "17" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "26" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "26" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "28" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "27" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME14" "X" "24" "15" "26" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME15" "X" "24" "15" "27" "E-ME14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME21" "X" "24" "15" "17" "E-ME15" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME22" "X" "24" "15" "26" "E-ME21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME23" "X" "24" "15" "24" "E-ME22" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME28" "X" "24" "15" "26" "E-ME23" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME29" "X" "24" "15" "28" "E-ME28" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME30" "X" "24" "15" "28" "E-ME29" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME31" "X" "24" "15" "28" "E-ME30" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME50" "X" "24" "15" "26" "E-ME31" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TTM" "X" "24" "50" "4" "E-ME50" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TTM" BY REFERENCE TT-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TUK" "X" "24" "50" "14" "E-TTM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TUK" BY REFERENCE TUK-KEY "14" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TUKO" "X" "24" "50" "14" "E-TUK" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TUKO" BY REFERENCE TUKO-KEY "14" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TSK" "X" "24" "50" "4" "E-TUKO" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TSK" BY REFERENCE TSK-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-SKD" "X" "24" "50" "20" "E-TSK" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-SKD" BY REFERENCE SKD-KEY "20" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           COPY LIBCPR.
           MOVE ZERO TO W-NGPD.
           MOVE D-NHNG TO W-NGDS.
           IF  W-NEND2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEND
           END-IF
           IF  W-NEND2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEND
           END-IF
           MOVE 1 TO W-PEYD.
      *
           PERFORM DEL-RTN THRU DEL-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
      *
           MOVE W-NGPD TO W-NGP.
           ADD 1 TO W-GET.
           IF  W-GET = 13
               ADD 1 TO W-NEN
               MOVE 1 TO W-GET
           END-IF
           CALL "DB_F_Open" USING
            "I-O" TT-M_PNAME1 " " BY REFERENCE TT-M_IDLST "1"
            "TT-KEY" BY REFERENCE TT-KEY.
           CALL "DB_F_Open" USING
            "I-O" TUKF_PNAME1 "SHARED" BY REFERENCE TUKF_IDLST "2"
            "TUK-KEY" BY REFERENCE TUK-KEY "TUK-KEY2" BY REFERENCE
            TUK-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
       M-10.
      *           READ TT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TT-M_PNAME1 BY REFERENCE TT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           MOVE TT-TUZ TO TT-TZZ.
           MOVE TT-TUZZ TO TT-TZZZ.
           MOVE ZERO TO TT-TUA TT-TUAZ TT-TNB TT-TNBZ
                        TT-TNK TT-TNKZ TT-TUG.
      *           REWRITE TT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TT-M_PNAME1 TT-M_LNAME TT-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TTM" E-TTM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  ZERO = TT-TZZ AND TT-TZZZ
               GO TO M-10
           END-IF
           PERFORM HKB-RTN THRU HKB-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-15
           END-IF
           PERFORM TUK-RTN THRU TUK-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-15
           END-IF
           GO TO M-10.
       M-15.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TUKF_IDLST TUKF_PNAME1.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
      *
           MOVE ZERO TO W-PEY.
           CALL "DB_F_Open" USING
            "I-O" TSKF_PNAME1 " " BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
           CALL "DB_F_Open" USING
            "I-O" SKDF_PNAME1 " " BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" TSKFYR_PNAME1 " " BY REFERENCE TSKFYR_IDLST "0".
       M-20.
      *           READ TSKF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TSKF_PNAME1 BY REFERENCE TSK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF.
       M-25.
           INITIALIZE TSKY-R.
           MOVE TSK-R TO TSKY-R.
           MOVE W-NGD TO TSKY-NG.
      *           WRITE TSKY-R.
      *//////////////
           CALL "DB_Insert" USING
            TSKFYR_PNAME1 TSKFYR_LNAME TSKY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-30
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-TSK" E-TSK "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME31" E-ME31 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL " " USING BY REFERENCE T-M_IDLST
               CALL "DB_F_Close" USING
                BY REFERENCE TSKF_IDLST TSKF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE SKDF_IDLST SKDF_PNAME1
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TSKFYR_IDLST TSKFYR_PNAME1.
           MOVE "TSKFYR       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" TSKFYR_PNAME1 " " BY REFERENCE TSKFYR_IDLST "0".
           GO TO M-25.
       M-30.
           IF  TSK-ZNGP(4) NOT = ZERO
               IF  TSK-ZNGP(5) = ZERO
                   MOVE TSK-ZSD(2) TO TSK-ZSD(1)
                   MOVE TSK-ZSD(3) TO TSK-ZSD(2)
                   MOVE TSK-ZSD(4) TO TSK-ZSD(3)
               ELSE
                   MOVE TSK-ZSD(3) TO TSK-ZSD(1)
                   MOVE TSK-ZSD(4) TO TSK-ZSD(2)
                   MOVE TSK-ZSD(5) TO TSK-ZSD(3)
               END-IF
           END-IF
           IF  TSK-ZNGP(4) = ZERO
               MOVE TSK-ZSD(2) TO TSK-ZSD(1)
               MOVE TSK-ZSD(3) TO TSK-ZSD(2)
               MOVE ZERO TO TSK-ZSD(3)
           END-IF
           MOVE ZERO TO TSK-ZSD(4) TSK-ZSD(5).
           IF  ZERO = TSK-ZSD(1) AND TSK-ZSD(2) AND TSK-ZSD(3)
               GO TO M-35
           END-IF
           MOVE TSK-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO T-SS T-DCC
           END-IF
           MOVE T-TNC TO TSK-TNC.
           MOVE T-DCC TO TSK-DCC.
           PERFORM TSR-RTN THRU TSR-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
           IF  ZERO = TSK-HTS(3) AND TSK-SZS(3)
               GO TO M-20
           END-IF
      *
           PERFORM SKD-RTN THRU SKD-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
           GO TO M-20.
       M-35.
           PERFORM TSD-RTN THRU TSD-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
           GO TO M-20.
       M-40.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TSKFYR_IDLST TSKFYR_PNAME1.
      *
           CALL "DB_F_Open" USING
            "OUTPUT" NYUR-F_PNAME1 " " BY REFERENCE NYUR-F_IDLST "0".
           CALL "DB_F_Close" USING
            BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" M-DATE_PNAME1 "SHARED" BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
           MOVE "01" TO DATE-KEY.
      *           READ M-DATE INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" M-DATE_PNAME1 BY REFERENCE DATE-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE M-DATE_IDLST M-DATE_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE W-NGDS TO D-ENG.
           ADD 1 TO W-GETD.
           IF  W-GETD = 13
               MOVE 1 TO W-GETD
               ADD 1 TO W-NEND
           END-IF
           MOVE W-NGDS TO D-NHNG D-NKNG D-NANG.
           MOVE ZERO TO D-HSD D-HND D-KUD D-KKD D-KSD D-KRD.
           MOVE 0 TO DATE-HFC.
      *           REWRITE DATE-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            M-DATE_PNAME1 M-DATE_LNAME DATE-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           MOVE SPACE TO KKB-KEY.
           MOVE 90 TO KKB-NO.
      *           READ KKB-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               GO TO M-95
           END-IF
           MOVE 0 TO KKB-SC15.
      *           REWRITE KKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KKB-M_PNAME1 KKB-M_LNAME KKB-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       DEL-RTN.
           MOVE W-NGPD TO W-NGP.
           SUBTRACT 1 FROM W-NEN.
           MOVE 99 TO W-PEY.
      *
           CALL "DB_F_Open" USING
            "I-O" TUKOF_PNAME1 "SHARED" BY REFERENCE TUKOF_IDLST "1"
            "TUKO-KEY" BY REFERENCE TUKO-KEY.
       DEL-010.
      *           READ TUKOF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TUKOF_PNAME1 BY REFERENCE TUKO-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DEL-020
           END-IF
           IF  TUKO-DATE > W-NGP
               GO TO DEL-010
           END-IF
      *           DELETE TUKOF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TUKOF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TUKO" E-TUKO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO DEL-020
           END-IF
           GO TO DEL-010.
       DEL-020.
           IF  COMPLETION_CODE = 255
               CALL "DB_F_Close" USING
                BY REFERENCE TUKOF_IDLST TUKOF_PNAME1
               GO TO DEL-EX
           END-IF
      *
           MOVE W-NGPD TO W-NGP.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEN
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEN
           END-IF
           CALL "DB_F_Open" USING
            "I-O" TUKF_PNAME1 "SHARED" BY REFERENCE TUKF_IDLST "2"
            "TUK-KEY" BY REFERENCE TUK-KEY "TUK-KEY2" BY REFERENCE
            TUK-KEY2.
           CALL "DB_F_Open" USING
            "EXTEND" TUKOYR_PNAME1 " " BY REFERENCE TUKOYR_IDLST "0".
       DEL-110.
      *           READ TUKF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TUKF_PNAME1 BY REFERENCE TUK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DEL-140
           END-IF
           IF  TUK-DATE > W-NGP
               GO TO DEL-110
           END-IF
      *
           INITIALIZE TUKO-R.
           MOVE TUK-R TO TUKO-R.
      *           WRITE TUKO-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TUKOYR_PNAME1 TUKOYR_LNAME TUKO-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TUK" E-TUK "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO DEL-140
           END-IF.
       DEL-120.
           INITIALIZE TUKOY-R.
           MOVE TUK-R TO TUKOY-R.
      *           WRITE TUKOY-R.
      *//////////////
           CALL "DB_Insert" USING
            TUKOYR_PNAME1 TUKOYR_LNAME TUKOY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO DEL-130
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-TUK" E-TUK "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME29" E-ME29 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO DEL-140
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TUKOYR_IDLST TUKOYR_PNAME1.
           MOVE "TUKOYR       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" TUKOYR_PNAME1 " " BY REFERENCE TUKOYR_IDLST "0".
           GO TO DEL-120.
       DEL-130.
      *           DELETE TUKF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TUKF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TUK" E-TUK "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO DEL-140
           END-IF
           GO TO DEL-110.
       DEL-140.
           CALL "DB_F_Close" USING BY REFERENCE TUKF_IDLST TUKF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TUKOF_IDLST TUKOF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TUKOYR_IDLST TUKOYR_PNAME1.
           IF  COMPLETION_CODE = 255
               GO TO DEL-EX
           END-IF
      *
           MOVE W-NGPD TO W-NGP.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEN
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEN
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEN
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEN
           END-IF
           MOVE 99 TO W-PEY.
      *
           CALL "DB_F_Open" USING
            "I-O" SKDF_PNAME1 " " BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" SKDFYR_PNAME1 " " BY REFERENCE SKDFYR_IDLST "0".
       DEL-210.
      *           READ SKDF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DEL-260
           END-IF
           IF  SKD-NGP > W-NGP
               GO TO DEL-210
           END-IF.
       DEL-220.
           INITIALIZE SKDY-R.
           MOVE SKD-R TO SKDY-R.
           MOVE W-NGD TO SKDY-NG.
      *           WRITE SKDY-R.
      *//////////////
           CALL "DB_Insert" USING
            SKDFYR_PNAME1 SKDFYR_LNAME SKDY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO DEL-240
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-SKD" E-SKD "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME30" E-ME30 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO DEL-140
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE SKDFYR_IDLST SKDFYR_PNAME1.
           MOVE "SKDFYR       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" SKDFYR_PNAME1 " " BY REFERENCE SKDFYR_IDLST "0".
           GO TO DEL-220.
       DEL-240.
      *           DELETE SKDF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING SKDF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SKD" E-SKD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO DEL-260
           END-IF
           GO TO DEL-210.
       DEL-260.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SKDFYR_IDLST SKDFYR_PNAME1.
       DEL-EX.
           EXIT.
       HKB-RTN.
           CALL "DB_F_Open" USING
            "I-O" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
      *           READ HKBM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO HKB-010
           END-IF
           MOVE HKB-DAI TO W-DAI.
           GO TO HKB-EX.
       HKB-010.
           INITIALIZE HKB-R.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
      *           WRITE HKB-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME23" E-ME23 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
           END-IF
           INITIALIZE W-DAI.
       HKB-EX.
           EXIT.
       TUK-RTN.
           COPY LDAIP.
           IF  W-DAI1 = "999"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME50" E-ME50 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           MOVE W-DAI TO HKB-DAI.
      *           REWRITE HKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME22" E-ME22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TUK-EX
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
       TUK-010.
           INITIALIZE TUK-R.
           MOVE TT-KEY TO TUK-TCD TUK-TCD2.
           MOVE W-DAI TO TUK-DAI.
           MOVE W-NGP TO TUK-DATE.
           MOVE 0 TO TUK-DC.
           MOVE TT-TZZ TO TUK-KIN.
           MOVE TT-TZZZ TO TUK-SHZ.
           MOVE TUK-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO T-TNC T-DCC
           END-IF
           MOVE T-TNC TO TUK-TNC.
           MOVE T-DCC TO TUK-DCC.
           MOVE TT-BC TO TUK-BMC.
      *           WRITE TUK-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TUKF_PNAME1 TUKF_LNAME TUK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME28" E-ME28 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TUK-020
           END-IF
           GO TO TUK-EX.
       TUK-020.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TUK-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE TUKF_IDLST TUKF_PNAME1.
           MOVE "TUKF         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TUKF_PNAME1 "SHARED" BY REFERENCE TUKF_IDLST "2"
            "TUK-KEY" BY REFERENCE TUK-KEY "TUK-KEY2" BY REFERENCE
            TUK-KEY2.
           GO TO TUK-010.
       TUK-EX.
           EXIT.
       TSD-RTN.
      *           DELETE TSKF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TSKF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TSK" E-TSK "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       TSD-EX.
           EXIT.
       TSR-RTN.
      *           REWRITE TSK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TSKF_PNAME1 TSKF_LNAME TSK-R RETURNING RET.
           IF RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TSK" E-TSK "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       TSR-EX.
           EXIT.
       SKD-RTN.
           MOVE ZERO TO W-SDTD.
           IF  T-SS = ZERO OR 99
               GO TO SKD-010
           END-IF
           MOVE W-NGP TO W-SDTD.
           MOVE T-SS TO W-SDTP.
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
           END-IF.
       SKD-010.
           MOVE ZERO TO SKD-R.
           MOVE SPACE TO SKD-BI.
           MOVE TSK-TCD TO SKD-TCD.
           MOVE W-NGP TO SKD-DATE.
           MOVE 5 TO SKD-DTC.
           MOVE TSK-HTS(3) TO SKD-KIN.
           MOVE TSK-SZS(3) TO SKD-SHZ.
           MOVE W-SDTD TO SKD-SKD.
           MOVE TSK-TNC TO SKD-TNC.
           MOVE TSK-DCC TO SKD-DCC.
           MOVE TSK-BMC TO SKD-BMC.
      *           WRITE SKD-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            SKDF_PNAME1 SKDF_LNAME SKD-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SKD-020
           END-IF
           GO TO SKD-EX.
       SKD-020.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SKD-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           MOVE "SKDF         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" SKDF_PNAME1 " " BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
           GO TO SKD-010.
       SKD-EX.
           EXIT.

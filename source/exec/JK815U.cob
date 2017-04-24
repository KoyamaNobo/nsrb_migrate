       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG240.
      *********************************************************
      *    PROGRAM         :  運送得意先別出荷個数ワーク　作成①
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID21        PIC  X(006) VALUE "WK0256".
           02  W-FID22        PIC  X(003).
       01  W-FID2.
           02  W-FID11        PIC  X(006) VALUE "WK0128".
           02  W-FID12        PIC  X(003).
       01  W-DATA.
           02  W-NG           PIC  9(006).
           02  W-NGL   REDEFINES W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-DATE         PIC  9(006).
           02  W-NGP   REDEFINES W-DATE.
             03  W-NEND       PIC  9(002).
             03  W-GETD       PIC  9(002).
             03  W-PEYD       PIC  9(002).
           02  W-SNG          PIC  9(006).
           02  W-ENG          PIC  9(006).
           02  W-SKC          PIC  9(001).
           02  W-USC          PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-BMC          PIC  9(001).
           02  W-BMCD         PIC  9(001).
           02  W-OKJ          PIC  9(006).
           02  W-SUD.
             03  W-SU22       PIC S9(006).
             03  W-SU23       PIC S9(006).
             03  W-SU24       PIC S9(006).
             03  W-ASU        PIC S9(006).
           02  W-KSUD.
             03  W-KS22       PIC S9(005)V9(2).
             03  W-KS22D REDEFINES W-KS22.
               04  W-KS22F    PIC S9(005).
               04  W-KS22R    PIC S9(002).
             03  W-KS23       PIC S9(005)V9(2).
             03  W-KS23D REDEFINES W-KS23.
               04  W-KS23F    PIC S9(005).
               04  W-KS23R    PIC S9(002).
             03  W-KS24       PIC S9(005)V9(2).
             03  W-KS24D REDEFINES W-KS24.
               04  W-KS24F    PIC S9(005).
               04  W-KS24R    PIC S9(002).
             03  W-KSU        PIC S9(005).
             03  W-TKSU       PIC S9(005).
           02  W-DMM          PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-DC           PIC  9(001).
           02  CNT            PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY L-JCON.
           COPY LITM.
           COPY LIHIM2.
      *FD  JSTRRF
       01  JSTRRF_JK815U.
           02  JSTRRF_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  JSTRRF_LNAME   PIC  X(013) VALUE "JSTRRF_JK815U".
           02  F              PIC  X(001).
           02  JSTRRF_KEY1    PIC  X(100) VALUE SPACE.
           02  JSTRRF_SORT    PIC  X(100) VALUE SPACE.
           02  JSTRRF_IDLST   PIC  X(100) VALUE SPACE.
           02  JSTRRF_RES     USAGE  POINTER.
       01  JSTRR-R.
           02  JSTRR-KEY.
             03  JSTRR-01     PIC 9(6).
             03  JSTRR-02     PIC 9(1).
           02  JSTRR-03       PIC 9(1).
           02  JSTRR-04.
             03  JSTRR-0412.
               04  JSTRR-041  PIC 9(4).
               04  JSTRR-042  PIC 9(2).
             03  JSTRR-043    PIC 9(2).
           02  JSTRR-05.
             03  JSTRR-0512.
               04  JSTRR-051  PIC 9(4).
               04  JSTRR-052  PIC 9(2).
             03  JSTRR-053    PIC 9(2).
           02  JSTRR-06.
             03  JSTRR-061    PIC 9(4).
             03  JSTRR-062    PIC 9(3).
           02  JSTRR-07       PIC 9(1).
           02  JSTRR-08.
             03  JSTRR-081    PIC 9(6).
             03  JSTRR-082    PIC 9(1).
           02  JSTRR-09       PIC 9(6).
           02  JSTRR-10       PIC 9(1).
           02  JSTRR-11.
             03  JSTRR-111    OCCURS  10.
               04  JSTRR-1111 PIC S9(4).
             03  JSTRR-112    PIC S9(5).
           02  JSTRR-12.
             03  JSTRR-121    OCCURS  10.
               04  JSTRR-1211 PIC S9(4).
             03  JSTRR-122    PIC S9(5).
           02  JSTRR-13       PIC 9(1).
           02  JSTRR-14       PIC 9(1).
           02  JSTRR-14A      PIC 9(3).
           02  JSTRR-14B      PIC 9(6).
           02  JSTRR-14C      PIC 9(2).
           02  JSTRR-14D      PIC N(9).
           02  JSTRR-15       PIC N(23).
           02  JSTRR-20       PIC X(10).
           02  JSTRR-15A      PIC S9(03).
           02  F              PIC X(01).
           02  JSTRR-40.
             03  JSTRR-401.
               04  JSTRR-4011 PIC X(03).
               04  JSTRR-4012 PIC 9(01).
             03  JSTRR-402.
               04  JSTRR-4021 PIC X(03).
               04  JSTRR-4022 PIC 9(01).
               04  JSTRR-4023 PIC 9(01).
           02  FILLER         PIC X(16).
           02  JSTRR-19       PIC X(01).
           02  JSTRR-158      PIC 9(01).
           02  JSTRR-16       PIC 9(01).
           02  JSTRR-17       PIC 9(01).
       77  F                  PIC X(01).
      *FD  EXLF
       01  EXLF_JK815U.
           02  EXLF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  EXLF_LNAME     PIC  X(011) VALUE "EXLF_JK815U".
           02  F              PIC  X(001).
           02  EXLF_KEY1      PIC  X(100) VALUE SPACE.
           02  EXLF_SORT      PIC  X(100) VALUE SPACE.
           02  EXLF_IDLST     PIC  X(100) VALUE SPACE.
           02  EXLF_RES       USAGE  POINTER.
       01  EXL-R.
           02  EXL-NG         PIC  9(006).
           02  EXL-SKC        PIC  9(001).
           02  EXL-SKN        PIC  N(006).
           02  EXL-USC        PIC  9(001).
           02  EXL-USN        PIC  N(006).
           02  EXL-TCD        PIC  9(004).
           02  EXL-TNA        PIC  N(026).
           02  EXL-KS22       PIC S9(005).
           02  EXL-KS23       PIC S9(005).
           02  EXL-KS24       PIC S9(005).
           02  EXL-KSU        PIC S9(005).
           02  EXL-TNC        PIC  9(002).
           02  F              PIC  X(018).
       77  F                  PIC  X(001).
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
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　運送得意先別　個数ワーク作成　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(021) VALUE
                  "［　'  年   月 分　］".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-NG.
             03  A-NEN   PIC  9(002).
             03  A-GET   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME01.
               04  FILLER  PIC  X(015) VALUE
                    "***  TM ﾅｼ  ***".
               04  FILLER  PIC  9(004).
             03  E-ME02.
               04  FILLER  PIC  X(016) VALUE
                    "***  HIM ﾅｼ  ***".
               04  FILLER  PIC  9(006).
             03  E-ME03.
               04  FILLER  PIC  X(016) VALUE
                    "***  ? ? ?   ***".
               04  FILLER  PIC  9(006).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "379" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "15" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "15" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "15" "48" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "15" "48" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "15" "48" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "15" "48" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "15" "48" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "15" "28" "21" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "35" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NG" " " "15" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "15" "33" "2" " " "A-NG" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "15" "38" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "52" "1" "A-NG" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "123" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "123" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME01" " " "24" "0" "19" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME01" "X" "24" "15" "15" " " "E-ME01" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME01" "9" "24" "34" "4" "01E-ME01" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME01" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME02" " " "24" "0" "22" "E-ME01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME02" "X" "24" "15" "16" " " "E-ME02" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME02" "9" "24" "35" "6" "01E-ME02" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME02" BY REFERENCE JSTRR-09 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME03" " " "24" "0" "22" "E-ME02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME03" "X" "24" "15" "16" " " "E-ME03" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME03" "9" "24" "35" "6" "01E-ME03" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME03" BY REFERENCE W-OKJ "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME03" " " RETURNING RESU.
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
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           ACCEPT W-DATE FROM DATE.
           MOVE W-NEND TO W-NEN2.
           MOVE W-GETD TO W-GET.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-ENG.
           SUBTRACT 1 FROM W-NEN.
           MOVE W-NG TO W-SNG.
           ADD 1 TO W-NEN.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEN
           END-IF
           CALL "SD_Output" USING "A-NG" A-NG "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
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
           END-IF
           IF  W-NG < W-SNG OR > W-ENG
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0256ID.
           MOVE WK0256ID TO JSTRRF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSTRRF_PNAME1 " " BY REFERENCE JSTRRF_IDLST "0".
       M-25.
      *           READ JSTRRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSTRRF_PNAME1 BY REFERENCE JSTRR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JSTRRF_IDLST JSTRRF_PNAME1
               GO TO M-95
           END-IF
           IF  JSTRR-0512 NOT = W-NG
               GO TO M-25
           END-IF
           IF  JSTRR-061 = 5000
               GO TO M-25
           END-IF
           IF  JSTRR-15A = ZERO
               GO TO M-25
           END-IF
           IF  JSTRR-14 = 9
               GO TO M-25
           END-IF
      *
           MOVE W-FID2 TO WK0128ID.
           MOVE WK0128ID TO EXLF_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" EXLF_PNAME1 " " BY REFERENCE EXLF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
       M-30.
           MOVE ZERO TO W-SUD W-KSUD W-DC CHK.
           MOVE JSTRR-07 TO W-SKC.
           MOVE JSTRR-14 TO W-USC.
           MOVE JSTRR-061 TO W-TCD.
           MOVE JSTRR-14B TO W-OKJ.
           MOVE JSTRR-15A TO W-KSU.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME01" E-ME01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-90
           END-IF
           IF  JSTRR-14B < 100000
               MOVE 1 TO CHK
               MOVE JSTRR-15A TO W-KS24 W-KSU
               GO TO M-45
           END-IF.
       M-35.
           IF  JSTRR-122 = ZERO
               GO TO M-40
           END-IF
           MOVE JSTRR-09 TO HI-MHCD HI-HCD.
      *           READ HI2-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME02" E-ME02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-90
           END-IF
           IF  HI-ISU = ZERO
               GO TO M-40
           END-IF
           ADD JSTRR-122 TO W-ASU.
           IF  HI-BC3 = 10
               ADD JSTRR-122 TO W-SU22
           ELSE
               IF  HI-BC3 = 20
                   ADD JSTRR-122 TO W-SU23
               ELSE
                   ADD JSTRR-122 TO W-SU24
               END-IF
           END-IF
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF.
       M-40.
      *           READ JSTRRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSTRRF_PNAME1 BY REFERENCE JSTRR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  JSTRR-0512 NOT = W-NG
               GO TO M-40
           END-IF
           IF  JSTRR-061 = 5000
               GO TO M-40
           END-IF
           IF  JSTRR-15A = ZERO
               GO TO M-40
           END-IF
           IF  JSTRR-14 = 9
               GO TO M-40
           END-IF
           IF  JSTRR-09 > 999899
               GO TO M-40
           END-IF
           IF (JSTRR-07 = W-SKC) AND (JSTRR-14 = W-USC) AND
              (JSTRR-061 = W-TCD) AND (JSTRR-14B = W-OKJ)
               GO TO M-35
           END-IF
           IF  W-KSU = ZERO
               GO TO M-30
           END-IF
           IF  W-ASU = ZERO
               GO TO M-30
           END-IF
           PERFORM S-05 THRU S-30.
       M-45.
           PERFORM S-40 THRU S-45.
           IF  CHK = 0
               GO TO M-30
           END-IF.
       M-50.
      *           READ JSTRRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSTRRF_PNAME1 BY REFERENCE JSTRR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JSTRR-0512 NOT = W-NG
               GO TO M-50
           END-IF
           IF  JSTRR-061 = 5000
               GO TO M-50
           END-IF
           IF  JSTRR-15A = ZERO
               GO TO M-50
           END-IF
           IF  JSTRR-14 = 9
               GO TO M-50
           END-IF
           IF  JSTRR-09 > 999899
               GO TO M-50
           END-IF
           IF (JSTRR-07 = W-SKC) AND (JSTRR-14 = W-USC) AND
              (JSTRR-061 = W-TCD) AND (JSTRR-14B = W-OKJ)
               GO TO M-50
           END-IF
           GO TO M-30.
       M-90.
           IF  W-DC = 1
               IF  W-KSU NOT = ZERO
                   IF  W-ASU NOT = ZERO
                       PERFORM S-05 THRU S-30
                       PERFORM S-40 THRU S-45
                   END-IF
               END-IF
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE JSTRRF_IDLST JSTRRF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE EXLF_IDLST EXLF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           IF  W-SU22 NOT = ZERO
               COMPUTE W-KS22 ROUNDED = W-SU22 * W-KSU / W-ASU
           END-IF
           IF  W-SU23 NOT = ZERO
               COMPUTE W-KS23 ROUNDED = W-SU23 * W-KSU / W-ASU
           END-IF
           IF  W-SU24 NOT = ZERO
               COMPUTE W-KS24 ROUNDED = W-SU24 * W-KSU / W-ASU
           END-IF
           COMPUTE W-TKSU = W-KS22F + W-KS23F + W-KS24F.
           IF  W-TKSU = W-KSU
               GO TO S-30
           END-IF
           MOVE 0 TO CNT.
           IF  W-TKSU > W-KSU
               GO TO S-20
           END-IF.
       S-10.
           IF  W-KS22R = W-KS23R AND W-KS24R
               IF  W-KS22F NOT = ZERO
                   ADD 1 TO W-KS22F
                   GO TO S-15
               ELSE
                   IF  W-KS23F NOT = ZERO
                       ADD 1 TO W-KS23F
                       GO TO S-15
                   ELSE
                       IF  W-KS24F NOT = ZERO
                           ADD 1 TO W-KS24F
                           GO TO S-15
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  W-KS22R = W-KS23R AND W-KS24R
               IF  W-KS22R NOT = ZERO
                   ADD 1 TO W-KS22F
                   GO TO S-15
               ELSE
                   IF  W-KS23R NOT = ZERO
                       ADD 1 TO W-KS23F
                       GO TO S-15
                   ELSE
                       IF  W-KS24R NOT = ZERO
                           ADD 1 TO W-KS24F
                           GO TO S-15
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  W-KS22R > W-KS23R
               IF  W-KS22R >= W-KS24R
                   ADD 1 TO W-KS22F
                   GO TO S-15
               END-IF
           END-IF
           IF  W-KS22R > W-KS24R
               IF  W-KS22R >= W-KS23R
                   ADD 1 TO W-KS22F
                   GO TO S-15
               END-IF
           END-IF
           IF  W-KS23R > W-KS22R
               IF  W-KS23R >= W-KS24R
                   ADD 1 TO W-KS23F
                   GO TO S-15
               END-IF
           END-IF
           IF  W-KS23R > W-KS24R
               IF  W-KS23R >= W-KS22R
                   ADD 1 TO W-KS23F
                   GO TO S-15
               END-IF
           END-IF
           IF  W-KS24R > W-KS22R
               IF  W-KS24R >= W-KS23R
                   ADD 1 TO W-KS24F
                   GO TO S-15
               END-IF
           END-IF
           IF  W-KS24R > W-KS23R
               IF  W-KS24R >= W-KS22R
                   ADD 1 TO W-KS24F
                   GO TO S-15
               END-IF
           END-IF.
       S-15.
           COMPUTE W-TKSU = W-KS22F + W-KS23F + W-KS24F.
           IF  W-TKSU = W-KSU
               GO TO S-30
           END-IF
           ADD 1 TO CNT.
           IF  CNT > 2
               CALL "SD_Output" USING
                "E-ME03" E-ME03 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO S-10.
       S-20.
           IF  W-KS22R = W-KS23R AND W-KS24R
               IF  W-KS22F NOT = ZERO
                   SUBTRACT 1 FROM W-KS22F
                   GO TO S-25
               ELSE
                   IF  W-KS23F NOT = ZERO
                       SUBTRACT 1 FROM W-KS23F
                       GO TO S-25
                   ELSE
                       IF  W-KS24F NOT = ZERO
                           SUBTRACT 1 FROM W-KS24F
                           GO TO S-25
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  W-KS22R = W-KS23R AND W-KS24R
               IF  W-KS22R NOT = ZERO
                   SUBTRACT 1 FROM W-KS22F
                   GO TO S-25
               ELSE
                   IF  W-KS23R NOT = ZERO
                       SUBTRACT 1 FROM W-KS23F
                       GO TO S-25
                   ELSE
                       IF  W-KS24R NOT = ZERO
                           SUBTRACT 1 FROM W-KS24F
                           GO TO S-25
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  W-KS22R > W-KS23R
               IF  W-KS22R >= W-KS24R
                   SUBTRACT 1 FROM W-KS22F
                   GO TO S-25
               END-IF
           END-IF
           IF  W-KS22R > W-KS24R
               IF  W-KS22R >= W-KS23R
                   SUBTRACT 1 FROM W-KS22F
                   GO TO S-25
               END-IF
           END-IF
           IF  W-KS23R > W-KS22R
               IF  W-KS23R >= W-KS24R
                   SUBTRACT 1 FROM W-KS23F
                   GO TO S-25
               END-IF
           END-IF
           IF  W-KS23R > W-KS24R
               IF  W-KS23R >= W-KS22R
                   SUBTRACT 1 FROM W-KS23F
                   GO TO S-25
               END-IF
           END-IF
           IF  W-KS24R > W-KS22R
               IF  W-KS24R >= W-KS23R
                   SUBTRACT 1 FROM W-KS24F
                   GO TO S-25
               END-IF
           END-IF
           IF  W-KS24R > W-KS23R
               IF  W-KS24R >= W-KS22R
                   SUBTRACT 1 FROM W-KS24F
                   GO TO S-25
               END-IF
           END-IF.
       S-25.
           COMPUTE W-TKSU = W-KS22F + W-KS23F + W-KS24F.
           IF  W-TKSU = W-KSU
               GO TO S-30
           END-IF
           ADD 1 TO CNT.
           IF  CNT > 2
               CALL "SD_Output" USING
                "E-ME03" E-ME03 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO S-20.
       S-30.
           EXIT.
       S-40.
           INITIALIZE EXL-R.
           MOVE W-NG TO EXL-NG.
           MOVE W-SKC TO EXL-SKC.
           MOVE 3 TO JCON3-01.
           MOVE W-SKC TO JCON3-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO JCON3-03
           END-IF
           MOVE JCON3-03 TO EXL-SKN.
           MOVE W-USC TO EXL-USC.
           MOVE 2 TO JCON2-01.
           MOVE W-USC TO JCON2-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO JCON2-03
           END-IF
           MOVE JCON2-03 TO EXL-USN.
           MOVE W-TCD TO EXL-TCD.
           MOVE T-NAME TO EXL-TNA.
           MOVE W-KS22F TO EXL-KS22.
           MOVE W-KS23F TO EXL-KS23.
           MOVE W-KS24F TO EXL-KS24.
           MOVE W-KSU TO EXL-KSU.
           MOVE T-TNC TO EXL-TNC.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXLF_PNAME1 JCON_LNAME EXL-R RETURNING RET.
       S-45.
           EXIT.

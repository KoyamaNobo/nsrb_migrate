       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG960.
      ************************************************************
      *    PROGRAM         :  履物月次　マスター更新・クリア　　 *
      *    PRINTER TYPE    :  ****                               *
      *    SCREEN          :  ******                             *
      *        変更　　　  :  62/05/22                           *
      *    COMPILE TYPE    :  COBOL                              *
      ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       77  I                  PIC  9(002).
       01  W-DATA.
           02  W-NGD          PIC  9(006).
           02  W-NGDD  REDEFINES W-NGD.
             03  W-NEND       PIC  9(004).
             03  W-NENDL REDEFINES W-NEND.
               04  W-NEND1    PIC  9(002).
               04  W-NEND2    PIC  9(002).
             03  W-GETD       PIC  9(002).
           02  W-NGDL  REDEFINES W-NGD.
             03  F            PIC  9(002).
             03  W-NGDS       PIC  9(004).
           02  W-D.
             03  W-HCD        PIC  9(006).
             03  W-ZSU        PIC S9(006).
             03  W-ZKIN       PIC S9(009).
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-GET        PIC  9(002).
           02  W-ASS          PIC S9(006).
           02  CNT            PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LDAIW.
           COPY LWMSG.
      *
           COPY LIBFDD.
           COPY LIHHTF.
           COPY LIHIM.
           COPY LIHUHM.
      *FD  UTR-F
       01  UTR-F_HMG960.
           02  UTR-F_PNAME1   PIC  X(004) VALUE "UTRF".
           02  F              PIC  X(001).
           02  UTR-F_LNAME    PIC  X(013) VALUE "UTR-F_HMG960".
           02  F              PIC  X(001).
           02  UTR-F_KEY1     PIC  X(100) VALUE SPACE.
           02  UTR-F_SORT     PIC  X(100) VALUE SPACE.
           02  UTR-F_IDLST    PIC  X(100) VALUE SPACE.
           02  UTR-F_RES      USAGE  POINTER.
       01  UTR-R              PIC  X(128).
       77  F                  PIC  X(001).
      *FD  TAZ-M
       01  TAZ-M_HMG960.
           02  TAZ-M_PNAME1   PIC  X(004) VALUE "TAZM".
           02  F              PIC  X(001).
           02  TAZ-M_LNAME    PIC  X(012) VALUE "TAZ-M_HMG960".
           02  F              PIC  X(001).
           02  TAZ-M_KEY1     PIC  X(100) VALUE SPACE.
           02  TAZ-M_SORT     PIC  X(100) VALUE SPACE.
           02  TAZ-M_IDLST    PIC  X(100) VALUE SPACE.
           02  TAZ-M_RES      USAGE  POINTER.
       01  TAZ-R.
           02  TAZ-KEY.
             03  TAZ-TCD      PIC  9(004).
             03  TAZ-HCD      PIC  9(006).
           02  TAZ-AZS        PIC S9(005).
           02  TAZ-AAS        PIC S9(005).
           02  TAZ-SZS        PIC S9(005).
           02  TAZ-SAS        PIC S9(005).
           02  F              PIC  X(006).
           02  TAZ-NG         PIC  9(006).
       77  F                  PIC  X(001).
      *FD  SNTRF
       01  SNTRF_HMG960.
           02  SNTRF_PNAME1   PIC  X(005) VALUE "SNTRF".
           02  F              PIC  X(001).
           02  SNTRF_LNAME    PIC  X(012) VALUE "SNTRF_HMG960".
           02  F              PIC  X(001).
           02  SNTRF_KEY1     PIC  X(100) VALUE SPACE.
           02  SNTRF_SORT     PIC  X(100) VALUE SPACE.
           02  SNTRF_IDLST    PIC  X(100) VALUE SPACE.
           02  SNTRF_RES      USAGE  POINTER.
       01  SNTR-R             PIC  X(128).
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
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　履物月次　マスター更新・クリア　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-DSP.
           02  D-ME.
             03  FILLER.
               04  FILLER  PIC  X(001) VALUE "(".
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  X(001) VALUE ")".
               04  FILLER  PIC  N(010) VALUE
                    "在庫数量　　在庫金額".
             03  FILLER.
               04  FILLER  PIC  N(004)       VALUE "ＨＨＴＦ".
               04  FILLER  PIC ----,--9 .
               04  FILLER  PIC ----,---,--9 .
             03  FILLER.
               04  FILLER  PIC  N(004)       VALUE "ＨＵＨＭ".
               04  FILLER  PIC ----,--9 .
               04  FILLER  PIC ----,---,--9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME3   PIC  X(027) VALUE
                  "***  TAZ-M REWRITE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(027) VALUE
                  "***  TAZ-M DELETE  ｴﾗｰ  ***".
             03  E-ME5   PIC  X(026) VALUE
                  "***  HUHM REWRITE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(018) VALUE
                  "***  ｻﾞｲｺ ｴﾗｰ  ***".
             03  E-ME7   PIC  X(017) VALUE
                  "***  HUHM ﾅｼ  ***".
             03  E-ME8   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME11  PIC  X(026) VALUE
                  "***  HHTF REWRITE ｴﾗ-  ***".
             03  E-HHTF  PIC  X(007).
             03  E-HUHM  PIC  X(006).
             03  E-TAZM  PIC  X(017).
           COPY LSSEM.
           COPY LIBSCR.
           COPY LSMSG.
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
            "C-MID" " " "0" "0" "350" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "50" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "50" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "50" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "50" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "50" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "50" "06C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "84" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ME" " " "0" "0" "84" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-ME" " " "13" "0" "28" " " "D-ME" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-ME" "X" "13" "21" "1" " " "01D-ME" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-ME" "9" "13" "22" "6" "0101D-ME" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-ME" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0301D-ME" "X" "13" "28" "1" "0201D-ME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0401D-ME" "N" "13" "30" "20" "0301D-ME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-ME" " " "14" "0" "28" "01D-ME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-ME" "N" "14" "21" "8" " " "02D-ME" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-ME" "----,--9" "14" "30" "8" "0102D-ME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0202D-ME" BY REFERENCE W-ZSU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-ME" "----,---,--9" "14" "38" "12" "0202D-ME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0302D-ME" BY REFERENCE W-ZKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-ME" " " "15" "0" "28" "02D-ME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103D-ME" "N" "15" "21" "8" " " "03D-ME" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203D-ME" "----,--9" "15" "30" "8" "0103D-ME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0203D-ME" BY REFERENCE HUH-YS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0303D-ME" "----,---,--9" "15" "38" "12" "0203D-ME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0303D-ME" BY REFERENCE HUH-YK "9" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "187" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "187" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "27" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "27" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "26" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "18" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "17" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "16" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "26" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HHTF" "X" "24" "50" "7" "E-ME11" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HHTF" BY REFERENCE HHT-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HUHM" "X" "24" "50" "6" "E-HHTF" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HUHM" BY REFERENCE HUH-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TAZM" "X" "24" "50" "17" "E-HUHM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TAZM" BY REFERENCE TAZ-KEY "10" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           COPY LIBCPR.
           MOVE ZERO TO W-NGD.
           MOVE D-NHNG TO W-NGDS.
           IF  W-NEND2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEND
           END-IF
           IF  W-NEND2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEND
           END-IF
           MOVE W-NGD TO W-NG.
           ADD 1 TO W-GETD.
           IF  W-GETD = 13
               ADD 1 TO W-NEND
               MOVE 1 TO W-GETD
           END-IF
           CALL "DB_F_Open" USING
            "I-O" HHTF_PNAME1 " " BY REFERENCE HHTF_IDLST "2"
            "HHT-KEY" BY REFERENCE HHT-KEY "HHT-KEY2" BY REFERENCE
            HHT-KEY2.
           CALL "DB_F_Open" USING
            "I-O" TAZ-M_PNAME1 " " BY REFERENCE TAZ-M_IDLST "1"
            "TAZ-KEY" BY REFERENCE TAZ-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HUH-M_PNAME1 " " BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "OUTPUT" SNTRF_PNAME1 " " BY REFERENCE SNTRF_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" UTR-F_PNAME1 " " BY REFERENCE UTR-F_IDLST "0".
       M-15.
      *           READ HHTF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF.
       M-20.
           MOVE ZERO TO W-D W-ASS.
           MOVE HHT-HCD TO W-HCD.
       M-25.
           MOVE ZERO TO CNT.
       M-30.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-35
           END-IF
           IF (HHT-SIZ = 4) AND (CNT = 10)
               COMPUTE HHT-ZSU(CNT) = HHT-ZSU(CNT) + W-ASS -
                                      HHT-USU(CNT)
           ELSE
               ADD HHT-ASS(CNT) TO W-ASS
               COMPUTE HHT-ZSU(CNT) = HHT-ZSU(CNT) + HHT-NSU(CNT) -
                                      HHT-USU(CNT) - HHT-ASS(CNT)
           END-IF
           MOVE ZERO TO HHT-NSU(CNT) HHT-USU(CNT) HHT-ASS(CNT).
           IF  W-GET = 04 OR 10
               MOVE HHT-ZSU(CNT) TO HHT-TZS(CNT)
               IF (HHT-SIZ = 4) AND (CNT = 10)
                   MOVE HHT-ZSU(CNT) TO HHT-TSU(CNT)
               END-IF
           END-IF
      *----教育のみ０９／０７で棚卸を行う-------------------------------
           IF  W-NG = 201507
               MOVE HHT-ZSU(CNT) TO HHT-TZS(CNT)
               IF  HHT-BC3 NOT = 30
                   MOVE HHT-ZSU(CNT) TO HHT-TSU(CNT)
               ELSE
                   IF (HHT-SIZ = 4) AND (CNT = 10)
                       MOVE HHT-ZSU(CNT) TO HHT-TSU(CNT)
                   END-IF
               END-IF
           END-IF
      *-----------------------------------------------------------------
           ADD HHT-ZSU(CNT) TO W-ZSU.
           GO TO M-30.
       M-35.
      *           REWRITE HHT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HHTF_PNAME1 HHTF_LNAME HHT-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HHTF" E-HHTF "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *
      *           READ HHTF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           IF  HHT-HCD = W-HCD
               GO TO M-25
           END-IF
      *
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HHTF" E-HHTF "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO HI-FT
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           COMPUTE W-ZKIN = HI-FT * W-ZSU.
      *
           MOVE W-HCD TO HUH-KEY.
      *           READ HUH-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HUHM" E-HUHM "p" RETURNING RESU
               GO TO M-20
           END-IF
           IF (HUH-YS NOT = W-ZSU) OR (HUH-YK NOT = W-ZKIN)
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-ME" D-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-20.
       M-40.
           IF (HUH-YS NOT = W-ZSU) OR (HUH-YK NOT = W-ZKIN)
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-ME" D-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-50.
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-M_IDLST HUH-M_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" HUH-M_PNAME1 " " BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
       M-55.
      *           READ HUH-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HUH-M_PNAME1 BY REFERENCE HUH-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-70
           END-IF
           MOVE W-NGD TO HUH-NG.
           MOVE HUH-YS TO HUH-ZS.
           MOVE HUH-YK TO HUH-ZK.
           MOVE ZERO TO HUH-NS HUH-NK HUH-SS HUH-SK HUH-UG.
           MOVE HUH-KEY TO HI-KEY W-HCD.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HUHM" E-HUHM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-60
           END-IF
           COMPUTE HUH-YK = HUH-YS * HI-FT.
       M-60.
      *           REWRITE HUH-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HUH-M_PNAME1 HUH-M_LNAME HUH-R RETURNING RET.
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
                "E-HUHM" E-HUHM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-55.
       M-70.
      *           READ TAZ-M NEXT RECORD AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TAZ-M_PNAME1 BY REFERENCE TAZ-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  ZERO = TAZ-AAS AND TAZ-SAS
               GO TO M-75
           END-IF
           MOVE TAZ-AAS TO TAZ-AZS.
           MOVE TAZ-SAS TO TAZ-SZS.
           MOVE W-NGD TO TAZ-NG.
      *           REWRITE TAZ-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TAZ-M_PNAME1 TAZ-M_LNAME TAZ-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TAZM" E-TAZM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-70.
       M-75.
      *           DELETE TAZ-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TAZ-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TAZM" E-TAZM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-70
           END-IF
           GO TO M-70.
       M-95.
           PERFORM END-RTN  THRU  END-EX.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-M_IDLST HUH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TAZ-M_IDLST TAZ-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UTR-F_IDLST UTR-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       END-EX.
           EXIT.
       COPY   LPMSG.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD600.
       DATE-WRITTEN. 1999-02-02.
      *********************************************************
      *    PROGRAM         :  履物在庫表ワーク　作成　　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0 = 在庫明細表                  *
      *                    :  1 = 品名別棚卸明細表            *
      *                    :  2 = 倉庫別   〃                 *
      *                    :  3 = 品名別   〃     －分        *
      *    JS-SIGN         :  5 = 親子在庫明細表              *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0128".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0256".
           02  W-FID22        PIC  X(003).
       01  W-FID3.
           02  W-FID31        PIC  X(006) VALUE "WK0064".
           02  W-FID32        PIC  X(003).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-C            PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-AC           PIC  9(001).
           02  W-ZCD.
             03  W-ZC    OCCURS   4  PIC  9(001).
           02  W-ZSD.
             03  W-ZS    OCCURS  10  PIC S9(006).
           02  W-D.
             03  W-SOC        PIC  9(001).
             03  W-HCD        PIC  9(006).
             03  W-ASUD.
               04  W-ASU   OCCURS  4.
                 05  W-SUD   OCCURS  10.
                   06  W-SU   PIC S9(006).
             03  W-ZST        PIC S9(006).
             03  W-AS         PIC S9(006).
             03  W-GZS        PIC S9(006).
             03  W-BC1        PIC  9(002).
             03  W-BC2        PIC  9(002).
             03  W-BC3        PIC  9(002).
             03  W-MHCD       PIC  9(006).
             03  W-CHK        PIC  9(001).
             03  W-UNC        PIC  9(001).
           02  W-HCDD         PIC  9(006).
           02  W-INV          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIHIM.
           COPY LIHHTF.
      *FD  HTIW-F
       01  HTIW-F_HMD600.
           02  HTIW-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HTIW-F_LNAME   PIC  X(013) VALUE "HTIW-F_HMD600".
           02  F              PIC  X(001).
           02  HTIW-F_KEY1    PIC  X(100) VALUE SPACE.
           02  HTIW-F_SORT    PIC  X(100) VALUE SPACE.
           02  HTIW-F_IDLST   PIC  X(100) VALUE SPACE.
           02  HTIW-F_RES     USAGE  POINTER.
       01  HTIW-R.
           02  F              PIC  X(007).
           02  HTIW-SOC       PIC  9(001).
           02  HTIW-HCD       PIC  9(006).
           02  HTIW-SIZ       PIC  9(001).
           02  HTIW-SUD.
             03  HTIW-SU      PIC S9(006)  OCCURS  10.
           02  HTIW-BC.
             03  HTIW-BC1     PIC  9(002).
             03  HTIW-BC2     PIC  9(002).
             03  HTIW-BC3     PIC  9(002).
           02  F              PIC  X(175).
       77  F                  PIC  X(001).
      *FD  HZW-F
       01  HZW-F_HMD600.
           02  HZW-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HZW-F_LNAME    PIC  X(012) VALUE "HZW-F_HMD600".
           02  F              PIC  X(001).
           02  HZW-F_KEY1     PIC  X(100) VALUE SPACE.
           02  HZW-F_SORT     PIC  X(100) VALUE SPACE.
           02  HZW-F_IDLST    PIC  X(100) VALUE SPACE.
           02  HZW-F_RES      USAGE  POINTER.
       01  HZW-R.
           02  HZW-KEY.
             03  HZW-HCD      PIC  9(006).
             03  HZW-SIZ      PIC  9(001).
           02  HZW-AZS.
             03  HZW-ZSD   OCCURS  10.
               04  HZW-ZS     PIC S9(006).
           02  HZW-TSU.
             03  HZW-ZST      PIC S9(006).
             03  HZW-AS       PIC S9(006).
             03  HZW-GZS      PIC S9(006).
             03  HZW-TC       PIC  9(001).
           02  HZW-BC1        PIC  9(002).
           02  HZW-BC2        PIC  9(002).
           02  HZW-BC3        PIC  9(002).
           02  HZW-BMC        PIC  9(002).
           02  HZW-BMNO       PIC  9(001).
           02  HZW-NO         PIC  9(001).
           02  HZW-SOC        PIC  9(001).
           02  HZW-MHCD       PIC  9(006).
           02  HZW-CHK        PIC  9(001).
           02  HZW-FT         PIC  9(005).
           02  F              PIC  X(019).
       77  F                  PIC  X(001).
      *FD  CODEF
       01  CODEF_HMD600.
           02  CODEF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  CODEF_LNAME    PIC  X(012) VALUE "CODEF_HMD600".
           02  F              PIC  X(001).
           02  CODEF_KEY1     PIC  X(100) VALUE SPACE.
           02  CODEF_SORT     PIC  X(100) VALUE SPACE.
           02  CODEF_IDLST    PIC  X(100) VALUE SPACE.
           02  CODEF_RES      USAGE  POINTER.
       01  CODE-R.
           02  CODE-MHCD      PIC  9(006).
           02  CODE-HCD       PIC  9(006).
           02  CODE-CHK       PIC  9(001).
           02  CODE-BC1       PIC  9(002).
           02  F              PIC  X(049).
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
           02  FILLER  PIC N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-DSP.
           02  D-MID1  PIC N(021) VALUE
                "＊＊＊　　履物在庫表ワーク　作成　　＊＊＊".
           02  D-MID2  PIC N(021) VALUE
                "＊＊＊　　倉別棚卸表ワーク　作成　　＊＊＊".
           02  D-MID3  PIC X(047) VALUE
                "棚卸マイナスリスト  印字   しない=0 , する=1   ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME2.
               04  CFILLER  PIC  X(016) VALUE
                    "***  HIM ﾅｼ  ***".
               04  02E-ME2  PIC  9(006).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
           "C-MID" " " "0" "0" "252" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "7" "10" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "8" "10" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "9" "10" "42" "05C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "131" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-MID1" "N" "6" "10" "42" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "D-MID2" "N" "6" "10" "42" "D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-MID3" "X" "15" "10" "47" "D-MID2" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "15" "56" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "100" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "10" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" " " "24" "0" "22" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01E-ME2" "X" "24" "15" "16" " " "E-ME2" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-ME2" "9" "24" "33" "6" "01E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING
           "02E-ME2" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 0 AND 1 AND 2 AND 3 AND 5
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN NOT = 2
               CALL "SD_Output" USING
                "D-MID1" D-MID1 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "D-MID2" D-MID2 "p" RETURNING RESU
           END-IF
           IF  JS-SIGN NOT = 3
               GO TO M-15
           END-IF
           CALL "SD_Output" USING "D-MID3" D-MID3 "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF.
       M-15.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12.
           MOVE W-FID1 TO WK0128ID.
           MOVE WK0128ID TO HZW-F_PNAME1.
           IF  JS-SIGN NOT = 2
               CALL "DB_F_Open" USING
                "INPUT" HHTF_PNAME1 "SHARED" BY REFERENCE HHTF_IDLST "2"
                "HHT-KEY" BY REFERENCE HHT-KEY "HHT-KEY2" BY REFERENCE
                HHT-KEY2
           ELSE
               MOVE STN-NO2 TO W-FID22
               MOVE W-FID2 TO WK0256ID
               MOVE WK0256ID TO HTIW-F_PNAME1
               CALL "DB_F_Open" USING
                "INPUT" HTIW-F_PNAME1 " " BY REFERENCE HTIW-F_IDLST "0"
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "OUTPUT" HZW-F_PNAME1 " " BY REFERENCE HZW-F_IDLST "0".
           IF  JS-SIGN = 2
               GO TO M-50
           END-IF
           MOVE ZERO TO W-HCDD.
       M-20.
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  HHT-HCD > 999899
               GO TO M-20
           END-IF
           IF  JS-SIGN NOT = 5
               GO TO M-25
           END-IF
           IF  W-HCDD = HHT-HCD
               GO TO M-20
           END-IF
           PERFORM S-55 THRU S-70.
           MOVE HHT-HCD TO W-HCDD.
           IF  W-INV = 0
               GO TO M-20
           END-IF.
       M-25.
           MOVE ZERO TO W-D W-ZCD.
           MOVE HHT-HCD TO W-HCD.
           MOVE HHT-BC1 TO W-BC1.
           MOVE HHT-BC2 TO W-BC2.
           MOVE HHT-BC3 TO W-BC3.
           IF  JS-SIGN = 5
               MOVE CODE-MHCD TO W-MHCD
               MOVE CODE-CHK TO W-CHK
           END-IF.
       M-30.
           PERFORM S-05 THRU S-30.
       M-35.
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF
           IF  HHT-HCD > 999899
               GO TO M-35
           END-IF
           IF  HHT-HCD = W-HCD
               GO TO M-30
           END-IF
           IF  JS-SIGN NOT = 5
               GO TO M-40
           END-IF
           PERFORM S-55 THRU S-70.
           IF  W-INV = 0
               GO TO M-35
           END-IF.
       M-40.
           PERFORM S-35 THRU S-50.
           GO TO M-25.
       M-45.
           PERFORM S-35 THRU S-50.
           GO TO M-95.
      *****    倉別棚卸    *********************************************
       M-50.
      *           READ HTIW-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HTIW-F_PNAME1 BY REFERENCE HTIW-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF.
       M-55.
           MOVE ZERO TO W-D W-ZCD.
           MOVE HTIW-SOC TO W-SOC.
           MOVE HTIW-HCD TO W-HCD.
           MOVE HTIW-BC1 TO W-BC1.
           MOVE HTIW-BC2 TO W-BC2.
           MOVE HTIW-BC3 TO W-BC3.
       M-60.
           PERFORM S-05 THRU S-30.
       M-65.
      *           READ HTIW-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HTIW-F_PNAME1 BY REFERENCE HTIW-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-70
           END-IF
           IF (HTIW-SOC = W-SOC) AND (HTIW-HCD = W-HCD)
               GO TO M-60
           END-IF
           PERFORM S-35 THRU S-50.
           GO TO M-55.
       M-70.
           PERFORM S-35 THRU S-50.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           IF  JS-SIGN NOT = 2
               CALL "DB_F_Close" USING
                BY REFERENCE HHTF_IDLST HHTF_PNAME1
           ELSE
               CALL "DB_F_Close" USING
                BY REFERENCE HTIW-F_IDLST HTIW-F_PNAME1
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE HZW-F_IDLST HZW-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE ZERO TO W-ZSD.
           MOVE ZERO TO CNT CHK.
       S-10.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO S-25
           END-IF
           IF  JS-SIGN = 1 OR 3
               GO TO S-15
           END-IF
           IF  JS-SIGN = 2
               MOVE HTIW-SU(CNT) TO W-ZS(CNT)
               GO TO S-20
           END-IF
           IF  HHT-SIZ = 4
               IF  CNT = 10
                   COMPUTE W-AS = W-AS + HHT-ZSU(10) + HHT-ASS(10)
                                                     - HHT-USU(10)
                   GO TO S-10
               END-IF
           END-IF
           COMPUTE W-ZS(CNT) = HHT-ZSU(CNT) + HHT-NSU(CNT)
                             - HHT-USU(CNT) - HHT-ASS(CNT).
           COMPUTE W-AS = W-AS + HHT-ASS(CNT).
           GO TO S-20.
       S-15.
           IF  HHT-SIZ = 4
               IF  CNT = 10
                   MOVE HHT-TSU(10) TO W-AS
                   GO TO S-10
               END-IF
           END-IF
           MOVE HHT-TSU(CNT) TO W-ZS(CNT).
           IF  JS-SIGN = 3
               IF  W-UNC = 0
                   IF  W-ZS(CNT) < ZERO
                       MOVE 1 TO W-UNC
                   END-IF
               END-IF
           END-IF.
       S-20.
           IF  CHK = 0
               IF  W-ZS(CNT) NOT = ZERO
                   MOVE 1 TO CHK
               END-IF
           END-IF
           ADD W-ZS(CNT) TO W-ZST.
           GO TO S-10.
       S-25.
           IF  CHK NOT = 0
               IF  JS-SIGN = 2
                   MOVE CHK TO W-ZC(HTIW-SIZ)
                   MOVE W-ZSD TO W-ASU(HTIW-SIZ)
               ELSE
                   MOVE CHK TO W-ZC(HHT-SIZ)
                   MOVE W-ZSD TO W-ASU(HHT-SIZ)
               END-IF
           END-IF.
       S-30.
           EXIT.
       S-35.
           IF  JS-SIGN = 3
               IF  W-UNC = 0
                   GO TO S-50
               END-IF
           END-IF
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO HI-BMC HI-BMNO
           END-IF
      *
           COMPUTE W-AS = -1 * W-AS.
           COMPUTE W-GZS = W-ZST - W-AS.
      *
           MOVE ZERO TO CHK W-C.
       S-40.
           ADD 1 TO W-C.
           IF  W-C = 5
               GO TO S-45
           END-IF
           IF  W-ZC(W-C) = 0
               GO TO S-40
           END-IF
      *
           MOVE ZERO TO HZW-R.
           MOVE W-HCD TO HZW-HCD.
           MOVE W-C TO HZW-SIZ.
           MOVE W-ASU(W-C) TO HZW-AZS.
           MOVE W-BC1 TO HZW-BC1.
           MOVE W-BC2 TO HZW-BC2.
           MOVE W-BC3 TO HZW-BC3.
           MOVE HI-BMC TO HZW-BMC.
           MOVE HI-BMNO TO HZW-BMNO.
           MOVE HI-FT TO HZW-FT.
           COMPUTE HZW-NO = W-C - 1.
           IF  HZW-NO = 0
               MOVE 4 TO HZW-NO
           END-IF
           IF (W-C = 1) OR ((W-C = 4) AND (W-ZC(1) = 0)) OR
              ((W-C = 3) AND (W-ZC(1) = 0) AND (W-ZC(4) = 0)) OR
              ((W-C = 2) AND
                  (W-ZC(1) = 0) AND (W-ZC(4) = 0) AND (W-ZC(3) = 0))
               MOVE 1 TO HZW-TC
               MOVE W-ZST TO HZW-ZST
               MOVE W-AS TO HZW-AS
               MOVE W-GZS TO HZW-GZS
           END-IF
           IF  JS-SIGN = 2
               MOVE W-SOC TO HZW-SOC
           END-IF
           IF  JS-SIGN = 5
               MOVE W-MHCD TO HZW-MHCD
               MOVE W-CHK TO HZW-CHK
           END-IF
      *           WRITE HZW-R.
      *///////////////
           CALL "DB_Insert" USING
            HZW-F_PNAME1 HZW-F_LNAME HZW-R RETURNING RET.
           MOVE 1 TO CHK.
           GO TO S-40.
       S-45.
           IF  CHK = 1
               GO TO S-50
           END-IF
           IF  ZERO = W-ZST AND W-AS AND W-GZS
               GO TO S-50
           END-IF
      *
           MOVE ZERO TO HZW-R.
           MOVE W-HCD TO HZW-HCD.
           MOVE W-BC1 TO HZW-BC1.
           MOVE W-BC2 TO HZW-BC2.
           MOVE W-BC3 TO HZW-BC3.
           MOVE HI-BMC TO HZW-BMC.
           MOVE HI-BMNO TO HZW-BMNO.
           MOVE HI-FT TO HZW-FT.
           MOVE 9 TO HZW-NO.
           MOVE 1 TO HZW-TC
           MOVE W-ZST TO HZW-ZST
           MOVE W-AS TO HZW-AS
           MOVE W-GZS TO HZW-GZS.
           IF  JS-SIGN = 2
               MOVE W-SOC TO HZW-SOC
           END-IF
           IF  JS-SIGN = 5
               MOVE W-MHCD TO HZW-MHCD
               MOVE W-CHK TO HZW-CHK
           END-IF
      *           WRITE HZW-R.
      *///////////////
           CALL "DB_Insert" USING
            HZW-F_PNAME1 HZW-F_LNAME HZW-R RETURNING RET.
       S-50.
           EXIT.
       S-55.
           MOVE 0 TO W-INV.
           MOVE STN-NO2 TO W-FID32.
           MOVE W-FID3 TO WK0064ID.
           MOVE WK0064ID TO CODEF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 " " BY REFERENCE CODEF_IDLST "0".
       S-60.
      *           READ CODEF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" CODEF_PNAME1 BY REFERENCE CODE-R " " RETURNING RET.
           IF  RET = 1
               GO TO S-65
           END-IF
           IF  HHT-HCD > CODE-HCD
               GO TO S-60
           END-IF
           IF  HHT-HCD < CODE-HCD
               GO TO S-65
           END-IF
           MOVE 1 TO W-INV.
       S-65.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
       S-70.
           EXIT.

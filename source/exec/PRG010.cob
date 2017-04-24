       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PRG010.
      *********************************************************
      *    PROGRAM         :  残高　明細表　　                *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA                        DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013) VALUE SPACE.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  ERR-STAT           PIC  X(002).
       01  W-20K              PIC  X(005) VALUE X"1A24212474".
       01  HEAD1.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(029) VALUE SPACE.
           02  H-MID          PIC  N(018).
           02  F              PIC  X(015) VALUE SPACE.
           02  F              PIC  X(002) VALUE "P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(032) VALUE SPACE.
           02  F              PIC  X(001) VALUE "(".
           02  H-DATE         PIC  N(012).
           02  F              PIC  X(001) VALUE ")".
           02  F              PIC  X(024) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(014) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(010) VALUE
                "取　　引　　先　　名".
           02  F              PIC  X(034) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
       01  W-P.
           02  P-15K          PIC  X(005).
           02  F              PIC  X(014).
           02  P-KEY          PIC  9(004).
           02  F              PIC  X(002).
           02  P-NAME         PIC  N(026).
           02  F              PIC  X(002).
           02  P-KIN          PIC --,---,---,--9.
           02  P-20K          PIC  X(005).
       01  W-MID.
           02  W-MID1         PIC  N(018) VALUE
                "　【　　売掛金残高　明細表　　】　　".
           02  W-MID2         PIC  N(018) VALUE
                "【　　消費税未収残高　明細表　　】　".
           02  W-MID3         PIC  N(018) VALUE
                "　【　　買掛金残高　明細表　　】　　".
           02  W-MID4         PIC  N(018) VALUE
                "【　　保有受取手形残高　明細表　　】".
           02  W-MID5         PIC  N(018) VALUE
                "　【　　受取手形残高　明細表　　】　".
           02  W-MID6         PIC  N(018) VALUE
                "　【　　割引手形残高　明細表　　】　".
           02  W-MID7         PIC  N(018) VALUE
                "　【　　支払手形残高　明細表　　】　".
           02  W-DATEM        PIC  N(012).
           02  W-NGPM.
             03  W-NENM       PIC  N(002).
             03  F            PIC  N(001) VALUE "年".
             03  W-GETM       PIC  N(002).
             03  F            PIC  N(001) VALUE "月".
             03  W-PEYM       PIC  N(002).
             03  F            PIC  N(004) VALUE "日　現在".
       01  W-DATA.
           02  W-NGP.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-NGL   REDEFINES W-NG.
               04  F          PIC  9(002).
               04  W-NGS      PIC  9(004).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-NGPD.
             03  W-NEND       PIC  9(002).
             03  W-GETD       PIC  Z(002).
             03  W-PEYD       PIC  Z(002).
           02  W-TKIN         PIC S9(010).
           02  W-L            PIC  9(002).
           02  W-DCD.
             03  W-DC         PIC  9(001) OCCURS  10.
           02  W-DCM          PIC  N(001).
           02  W-NO           PIC  9(002).
           02  W-PAGE         PIC  9(002).
           02  W-TPC          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-CC           PIC  9(001).
           02  CNT            PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-MIDD         PIC  N(018).
           02  W-NAME         PIC  N(026).
           02  W-NAMED REDEFINES W-NAME.
             03  W-NAME1      PIC  N(008).
             03  F            PIC  N(001).
             03  W-NAME2      PIC  N(008).
             03  F            PIC  N(009).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LITM.
           COPY LISM.
           COPY LIBANK.
           COPY LICAL.
           COPY LSPF.
      *       FD  WZD-F
       01  WZD-F_PRG010.
           02  WZD-F_PNAME1          PIC  X(009)  VALUE SPACE.
           02  F                     PIC  X(001).
           02  WZD-F_LNAME           PIC  X(012)  VALUE "WZD-F_PRG010".
           02  F                     PIC  X(001).
           02  WZD-F_KEY1            PIC  X(100)  VALUE SPACE.
           02  WZD-F_KEY2            PIC  X(100)  VALUE SPACE.
           02  WZD-F_SORT            PIC  X(100)  VALUE SPACE.
           02  WZD-F_IDLST           PIC  X(100)  VALUE SPACE.
           02  WZD-F_RES             USAGE  POINTER.
       01  WZD-R.
           02  WZD-NO         PIC  9(002).
           02  WZD-KEY        PIC  9(004).
           02  WZD-KIN        PIC  9(010).
           02  WZD-NG         PIC  9(004).
           02  F              PIC  X(044).
       77  F                       PIC  X(001).
      *       FD  ZD-F
       01  ZD-F_PRG010.
           02  ZD-F_PNAME1          PIC  X(003)  VALUE "ZDF".
           02  F                    PIC  X(001).
           02  ZD-F_LNAME           PIC  X(011)  VALUE "ZD-F_PRG010".
           02  F                    PIC  X(001).
           02  ZD-F_KEY1            PIC  X(100)  VALUE SPACE.
           02  ZD-F_KEY2            PIC  X(100)  VALUE SPACE.
           02  ZD-F_SORT            PIC  X(100)  VALUE SPACE.
           02  ZD-F_IDLST           PIC  X(100)  VALUE SPACE.
           02  ZD-F_RES             USAGE  POINTER.
       01  ZD-R               PIC  X(021).
       77  F                       PIC  X(001).
      *       FD  ZDYR
       01  ZDYR_PRG010.
           02  ZDYR_PNAME1          PIC  X(004)  VALUE "ZDYR".
           02  F                    PIC  X(001).
           02  ZDYR_LNAME           PIC  X(011)  VALUE "ZDYR_PRG010".
           02  F                    PIC  X(001).
           02  ZDYR_KEY1            PIC  X(100)  VALUE SPACE.
           02  ZDYR_KEY2            PIC  X(100)  VALUE SPACE.
           02  ZDYR_SORT            PIC  X(100)  VALUE SPACE.
           02  ZDYR_IDLST           PIC  X(100)  VALUE SPACE.
           02  ZDYR_RES             USAGE  POINTER.
       01  ZDY-R              PIC  X(021).
       77  F                       PIC  X(001).
      *
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL   PIC  X(12)  VALUE  "CLEAR SCREEN".
       01  C-MID.
           02  FILLER   PIC  N(016) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER   PIC  N(016) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊　　残高　明細表　　＊＊＊".
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(032) VALUE
                "（　０９ X １１ 白紙  セット　）".
           02  FILLER  PIC  X(032) VALUE
                "<  TEST PRINT  ｽﾙ=9 ｼﾅｲ=1...   >".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-DSP.
           02  D-DM.
             03  D-DM1  PIC  N(006) VALUE
                  "売　掛　金　" .
             03  D-DM2  PIC  N(006) VALUE
                  "消費税未収　" .
             03  D-DM3  PIC  N(006) VALUE
                  "買　掛　金　" .
             03  D-DM4  PIC  N(006) VALUE
                  "保有受取手形" .
             03  D-DM5  PIC  N(006) VALUE
                  "受取手形　　" .
             03  D-DM6  PIC  N(006) VALUE
                  "割引手形　　" .
             03  D-DM7  PIC  N(006) VALUE
                  "支払手形　　" .
           02  D-DMN.
             03  D-DMN1  PIC  N(006) VALUE
                  "売　掛　金　".
             03  D-DMN2  PIC  N(006) VALUE
                  "消費税未収　".
             03  D-DMN3  PIC  N(006) VALUE
                  "買　掛　金　".
             03  D-DMN4  PIC  N(006) VALUE
                  "保有受取手形".
             03  D-DMN5  PIC  N(006) VALUE
                  "受取手形　　".
             03  D-DMN6  PIC  N(006) VALUE
                  "割引手形　　".
             03  D-DMN7  PIC  N(006) VALUE
                  "支払手形　　".
           02  D-DCM    PIC  N(001).
           02  D-CM.
             03  FILLER  PIC  X(015) VALUE
                  "               ".
             03  FILLER  PIC  X(015) VALUE
                  "               ".
             03  FILLER  PIC  X(015) VALUE
                  "               ".
             03  FILLER  PIC  X(015) VALUE
                  "               ".
             03  FILLER  PIC  X(015) VALUE
                  "               ".
             03  FILLER  PIC  X(015) VALUE
                  "               ".
             03  FILLER  PIC  X(015) VALUE
                  "               ".
             03  FILLER.
               04  FILLER  PIC  N(010) VALUE
                    "残高データ　クリア　".
               04  FILLER  PIC  X(016) VALUE
                    "ｽﾙ=1  ｼﾅｲ=9 ... ".
       01  C-ACP.
           02  A-TPC     PIC  9(001).
           02  A-CC      PIC  9(001).
           02  A-DMM     PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1  PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2  PIC  X(018) VALUE
                  "***  CALNM ﾅｼ  ***".
             03  E-ME3  PIC  X(024) VALUE
                  "***  ZDYR WRITE ｴﾗｰ  ***".
           COPY LSSEM.
           COPY LIBSCR.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999-FPG010" RETURNING RESP.
      *       01  C-CLEAR.
       CALL "SD_Init" USING
            "C-CLEAR" " " "0" "0" "12" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "C-CL" "X" "1" "0" "12" " " "C-CLEAR"  RETURNING RESU.
      *   01  C-MID.
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "310" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "32" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "32" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "32" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "32" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "32" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "32" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "32" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "13" "10" "32" "07C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "15" "10" "32" "08C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "20" "15" "22" "09C-MID" " "
            RETURNING RESU.
      *   01  C-DSP.
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "311" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DM" " " "0" "0" "84" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DM1" "RN" "3" "55" "12" " " "D-DM" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DM2" "RN" "4" "55" "12" "D-DM1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DM3" "RN" "5" "55" "12" "D-DM2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DM4" "RN" "6" "55" "12" "D-DM3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DM5" "RN" "7" "55" "12" "D-DM4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DM6" "RN" "8" "55" "12" "D-DM5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DM7" "RN" "9" "55" "12" "D-DM6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DMN" " " "0" "0" "84" "D-DM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DMN1" "N" "3" "55" "12" " " "D-DMN" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DMN2" "N" "4" "55" "12" "D-DMN1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DMN3" "N" "5" "55" "12" "D-DMN2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DMN4" "N" "6" "55" "12" "D-DMN3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DMN5" "N" "7" "55" "12" "D-DMN4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DMN6" "N" "8" "55" "12" "D-DMN5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DMN7" "N" "9" "55" "12" "D-DMN6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DCM" "N" "W-L" "52" "2" "D-DMN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-DCM" BY REFERENCE W-DCM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CM" " " "0" "0" "141" "D-DCM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-CM" "X" "3" "52" "15" " " "D-CM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-CM" "X" "4" "52" "15" "01D-CM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-CM" "X" "5" "52" "15" "02D-CM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-CM" "X" "6" "52" "15" "03D-CM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-CM" "X" "7" "52" "15" "04D-CM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-CM" "X" "8" "52" "15" "05D-CM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-CM" "X" "9" "52" "15" "06D-CM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-CM" " " "15" "0" "36" "07D-CM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0108D-CM" "N" "15" "8" "20" " " "08D-CM" RETURNING RESU.
       CALL "SD_Init" USING 
            "0208D-CM" "X" "15" "28" "16" "0108D-CM" " "
            RETURNING RESU.
      *  01  C-ACP.
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "3" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "A-TPC" "9" "15" "39" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING
            "A-TPC" BY REFERENCE W-TPC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-CC" "9" "15" "43" "1" "A-TPC" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-CC" BY REFERENCE W-CC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "32" "1" "A-CC" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *  01  C-ERR.
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "59" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "59" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" "X" "24" "15" "24" "E-ME2" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "D-DMN" D-DMN "p"
                                         RETURNING RESU.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO WZD-F_PNAME1.
           PERFORM S-55 THRU S-75.
           CALL "DB_F_Open" USING
            "INPUT" WZD-F_PNAME1 " " BY REFERENCE WZD-F_IDLST "0".
      *           READ WZD-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" WZD-F_PNAME1 BY REFERENCE WZD-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE WZD-F_IDLST WZD-F_PNAME1
               GO TO M-95
           END-IF.
           MOVE ZERO TO W-NG.
           MOVE WZD-NG TO W-NGS.
           COPY LIBCPR.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF.
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" CALNM_PNAME1 "SHARED" BY REFERENCE CALNM_IDLST "1"
            "CL-KEY" BY REFERENCE CL-KEY.
           MOVE ZERO TO CL-KEY.
           MOVE W-NG TO CL-NG.
      *           START CALNM KEY NOT < CL-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CALNM_PNAME1 "CL-KEY" " NOT < " CL-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
      *
               CALL "DB_F_Close" USING
                BY REFERENCE WZD-F_IDLST WZD-F_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE CALNM_IDLST CALNM_PNAME1
               GO TO M-95
           END-IF.
       M-10.
      *           READ CALNM NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CALNM_PNAME1 BY REFERENCE CALN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF.
           IF  W-NG = CL-NG
               MOVE CL-PEY TO W-PEY
               GO TO M-10
           END-IF.
       M-15.
           CALL "DB_F_Close" USING
            BY REFERENCE CALNM_IDLST CALNM_PNAME1.
           MOVE W-NEN2 TO W-NEND.
           MOVE W-GET TO W-GETD.
           MOVE W-PEY TO W-PEYD.
           MOVE W-NEND TO W-NENM.
           MOVE W-GETD TO W-GETM.
           MOVE W-PEYD TO W-PEYM.
           MOVE W-NGPM TO W-DATEM.
           MOVE W-DATEM TO H-DATE.
           MOVE ALL "Ｎ" TO H-MID.
           MOVE ZERO TO W-TKIN W-PAGE CHK.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME.
           MOVE 9999 TO P-KEY.
           MOVE ALL "Ｎ" TO P-NAME.
           MOVE 999999999 TO P-KIN.
       M-20.
           CALL "SD_Accept" USING
                 BY REFERENCE A-TPC "A-TPC" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE WZD-F_IDLST WZD-F_PNAME1
               IF  CHK = 5
                   CALL "PR_Close" RETURNING RESP
               END-IF
           END-IF.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
           IF  W-TPC = 1
               GO TO M-25
           END-IF.
           IF  W-TPC NOT = 9
               GO TO M-20
           END-IF.
           IF  CHK = 0
               MOVE 5 TO CHK
               CALL "PR_Open" RETURNING RESP
               PERFORM S-10 THRU S-15
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO M-20.
       M-25.
           CALL "SD_Accept" USING
                 BY REFERENCE A-DMM "A-DMM" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF.
           IF  W-DMM = 9
               CALL "DB_F_Close" USING
                BY REFERENCE WZD-F_IDLST WZD-F_PNAME1
               IF  CHK = 5
                   CALL "PR_Close" RETURNING RESP
               END-IF
           END-IF.
           IF  W-DMM = 9
               GO TO M-95
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-25
           END-IF.
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
       M-30.
           MOVE ZERO TO W-PAGE W-TKIN.
           MOVE WZD-NO TO W-NO.
           MOVE SPACE TO W-MIDD.
           IF  W-NO = 11
      *
               CALL "SD_Output" USING "D-DM1" D-DM1 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN2" D-DMN2 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN3" D-DMN3 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN4" D-DMN4 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN5" D-DMN5 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN6" D-DMN6 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN7" D-DMN7 "p"
                                             RETURNING RESU
      *
               MOVE W-MID1 TO W-MIDD
           END-IF.
           IF  W-NO = 12
      *
               CALL "SD_Output" USING "D-DMN1" D-DMN1 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DM2" D-DM2 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN3" D-DMN3 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN4" D-DMN4 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN5" D-DMN5 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN6" D-DMN6 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN7" D-DMN7 "p"
                                             RETURNING RESU
               MOVE W-MID2 TO W-MIDD
           END-IF.
           IF  W-NO = 21
               CALL "SD_Output" USING "D-DMN1" D-DMN1 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN2" D-DMN2 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DM3" D-DM3 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN4" D-DMN4 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN5" D-DMN5 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN6" D-DMN6 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN7" D-DMN7 "p"
                                             RETURNING RESU
      *
               MOVE W-MID3 TO W-MIDD
           END-IF.
           IF  W-NO = 31
      *
               CALL "SD_Output" USING "D-DMN1" D-DMN1 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN2" D-DMN2 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN3" D-DMN3 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DM4" D-DM4 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN5" D-DMN5 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN6" D-DMN6 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN7" D-DMN7 "p"
                                             RETURNING RESU
      *
               MOVE W-MID4 TO W-MIDD
           END-IF.
           IF  W-NO = 32
      *
               CALL "SD_Output" USING "D-DMN1" D-DMN1 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN2" D-DMN2 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN3" D-DMN3 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN4" D-DMN4 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DM5" D-DM5 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN6" D-DMN6 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN7" D-DMN7 "p"
                                             RETURNING RESU
      *
               MOVE W-MID5 TO W-MIDD
           END-IF.
           IF  W-NO = 33
      *
               CALL "SD_Output" USING "D-DMN1" D-DMN1 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN2" D-DMN2 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN3" D-DMN3 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN4" D-DMN4 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN5" D-DMN5 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DM6" D-DM6 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN7" D-DMN7 "p"
                                             RETURNING RESU
               MOVE W-MID6 TO W-MIDD
           END-IF.
           IF  W-NO = 34
               CALL "SD_Output" USING "D-DMN1" D-DMN1 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN2" D-DMN2 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN3" D-DMN3 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN4" D-DMN4 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN5" D-DMN5 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DMN6" D-DMN6 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "D-DM7" D-DM7 "p"
                                             RETURNING RESU
      *
               MOVE W-MID7 TO W-MIDD
           END-IF.
           MOVE W-MIDD TO H-MID.
           IF  CHK = 0
               MOVE 5 TO CHK
               CALL "PR_Open" RETURNING RESP
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF.
       M-35.
           PERFORM S-20 THRU S-40.
       M-40.
      *           READ WZD-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" WZD-F_PNAME1 BY REFERENCE WZD-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF.
           IF  W-NO = WZD-NO
               GO TO M-35
           END-IF.
           PERFORM S-45 THRU S-50.
           GO TO M-30.
       M-50.
           PERFORM S-45 THRU S-50.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WZD-F_IDLST WZD-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
      *
           CALL "SD_Output" USING "D-CM" D-CM "p"
                                         RETURNING RESU.
      *
       M-60.
           CALL "SD_Accept" USING
                 BY REFERENCE A-CC "A-CC" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-60
           END-IF.
           IF  W-CC NOT = 1 AND 9
               GO TO M-60
           END-IF.
       M-65.
           CALL "SD_Accept" USING
                 BY REFERENCE A-DMM "A-DMM" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-60
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-65
           END-IF.
           IF  W-DMM = 9
               GO TO M-60
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-65
           END-IF.
           IF  W-CC = 9
               GO TO M-95
           END-IF.
           IF  W-CC NOT = 1
               GO TO M-65
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" ZD-F_PNAME1 " " BY REFERENCE ZD-F_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" ZDYR_PNAME1 " " BY REFERENCE ZDYR_IDLST "0".
       M-70.
      *           READ ZD-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" ZD-F_PNAME1 BY REFERENCE ZD-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF.
       M-75.
           MOVE ZERO TO ZDY-R.
           MOVE ZD-R TO ZDY-R.
      *           WRITE ZDY-R.
      *//////////////////////
           CALL "DB_Insert" USING
            ZDYR_PNAME1 ZDYR_LNAME ZDY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-70
           END-IF.
      *
           CALL "SD_Output" USING "E-STAT" E-STAT "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "E-ME3" E-ME3 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU.
      *
           IF  ERR-STAT NOT = "34"
      *
               CALL "SD_Output" USING "E-ME78" E-ME78 "p"
                                          RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
      *
               CALL "DB_F_Close" USING
                BY REFERENCE ZD-F_IDLST ZD-F_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE ZDYR_IDLST ZDYR_PNAME1
               GO TO M-95
           END-IF.
      *
           CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU.
      *
           CALL "DB_F_Close" USING BY REFERENCE ZDYR_IDLST ZDYR_PNAME1.
           MOVE "ZDYR         " TO W-FILE.
      *
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "EXTEND" ZDYR_PNAME1 " " BY REFERENCE ZDYR_IDLST "0".
           GO TO M-75.
       M-80.
           CALL "DB_F_Close" USING BY REFERENCE ZD-F_IDLST ZD-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE ZDYR_IDLST ZDYR_PNAME1.
      *
           CALL "DB_F_Open" USING
            "OUTPUT" ZD-F_PNAME1 " " BY REFERENCE ZD-F_IDLST "0".
           CALL "DB_F_Close" USING BY REFERENCE ZD-F_IDLST ZD-F_PNAME1.
       M-95.
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
      *
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_LineFeed" USING "5" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-NAME.
           IF  W-NO = 21 OR 34
               GO TO S-25
           END-IF.
           IF  W-NO = 33
               GO TO S-30
           END-IF.
           MOVE WZD-KEY TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　" TO T-NAME
           END-IF.
           MOVE T-NAME TO W-NAME.
           GO TO S-35.
       S-25.
           MOVE WZD-KEY TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　" TO S-NAME
           END-IF.
           MOVE S-NAME TO W-NAME.
           GO TO S-35.
       S-30.
           MOVE WZD-KEY TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター" TO B-BNA
               MOVE "なし　＊＊　　　" TO B-SNA
           END-IF.
           MOVE B-BNA TO W-NAME1.
           MOVE B-SNA TO W-NAME2.
       S-35.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME.
           MOVE WZD-KEY TO P-KEY.
           MOVE W-NAME TO P-NAME.
           MOVE WZD-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD WZD-KIN TO W-TKIN.
       S-40.
           EXIT.
       S-45.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME.
           MOVE "　　　　【　　合　　計　　】　" TO P-NAME.
           MOVE W-TKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-50.
           EXIT.
       S-55.
           MOVE ZERO TO W-DCD.
           CALL "DB_F_Open" USING
            "INPUT" WZD-F_PNAME1 " " BY REFERENCE WZD-F_IDLST "0".
       S-60.
      *           READ WZD-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" WZD-F_PNAME1 BY REFERENCE WZD-R " " RETURNING RET.
           IF  RET = 1
               GO TO S-65.
           IF  WZD-NO = 11
               IF  W-DC(01) = 0
                   MOVE 1 TO W-DC(01)
               END-IF
           END-IF.
           IF  WZD-NO = 12
               IF  W-DC(02) = 0
                   MOVE 1 TO W-DC(02)
               END-IF
           END-IF.
           IF  WZD-NO = 21
               IF  W-DC(03) = 0
                   MOVE 1 TO W-DC(03)
               END-IF
           END-IF.
           IF  WZD-NO = 31
               IF  W-DC(04) = 0
                   MOVE 1 TO W-DC(04)
               END-IF
           END-IF.
           IF  WZD-NO = 32
               IF  W-DC(05) = 0
                   MOVE 1 TO W-DC(05)
               END-IF
           END-IF.
           IF  WZD-NO = 33
               IF  W-DC(06) = 0
                   MOVE 1 TO W-DC(06)
               END-IF
           END-IF.
           IF  WZD-NO = 34
               IF  W-DC(07) = 0
                   MOVE 1 TO W-DC(07)
               END-IF
           END-IF.
           GO TO S-60.
       S-65.
           CALL "DB_F_Close" USING
            BY REFERENCE WZD-F_IDLST WZD-F_PNAME1.
           MOVE ZERO TO CNT.
           MOVE 2 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       S-70.
           ADD 1 TO CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT > 7
               GO TO S-75
           END-IF.
           IF  W-DC(CNT) = 0
               MOVE "×" TO W-DCM
           ELSE
               MOVE "○" TO W-DCM
           END-IF.
           CALL "SD_Output" USING "D-DCM" D-DCM "p"
                                             RETURNING RESU.
           GO TO S-70.
       S-75.
           EXIT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMT110.
      *********************************************************
      *    PROGRAM         :  在庫明細問合せ　　　　　　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHT11                          *
      *        変更　　　  :  62/05/11                        *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=全体,1=－分,                  *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  W-D.
           02  W-AZCD.
             03  W-ZCD   OCCURS  4.
               04  W-ZC       PIC  9(001).
           02  W-AZSUD.
             03  W-AZSU   OCCURS  4.
               04  W-ZSUD  OCCURS  10.
                 05  W-ZSU    PIC S9(006).
             03  W-TSU        PIC S9(006).
             03  W-ASU        PIC S9(006).
             03  W-SSU        PIC S9(006).
           02  W-SUD.
             03  W-SU    OCCURS  10  PIC S9(006).
           02  W-KEY          PIC  9(006).
           02  W-HCD          PIC  9(006).
           02  W-SIZ          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-SC           PIC  9(002).
           02  W-L            PIC  9(002).
           02  CHK            PIC  9(001).
           02  CNT            PIC  9(001).
       01  W-DATE.
           02  W-SG           PIC  9(002).
           02  W-SP           PIC  9(002).
           02  W-UG           PIC  9(002).
           02  W-UP           PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
      *FD  HHTF
       01  HHTF_HMT110.
           02  HHTF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HHTF_LNAME     PIC  X(011) VALUE "HHTF_HMT110".
           02  F              PIC  X(001).
           02  HHTF_KEY1      PIC  X(100) VALUE SPACE.
           02  HHTF_SORT      PIC  X(100) VALUE SPACE.
           02  HHTF_IDLST     PIC  X(100) VALUE SPACE.
           02  HHTF_RES       USAGE  POINTER.
       01  HHT-R.
           02  HHT-KEY2.
             03  HHT-MHCD     PIC  9(006).
             03  HHT-KEY.
               04  HHT-HCD    PIC  9(006).
               04  HHT-SIZ    PIC  9(001).
           02  HHT-AZSU.                                                前月残数
             03  HHT-ZSUD  OCCURS  10.
               04  HHT-ZSU    PIC S9(006) COMP-3.
           02  HHT-ANSU.                                                入庫数
             03  HHT-NSUD  OCCURS  10.
               04  HHT-NSU    PIC S9(006) COMP-3.
           02  HHT-AUSU.                                                出庫数
             03  HHT-USUD  OCCURS  10.
               04  HHT-USU    PIC S9(006) COMP-3.
           02  HHT-AASS.                                                預り出荷
             03  HHT-ASSD  OCCURS  10.
               04  HHT-ASS    PIC S9(004) COMP-3.
           02  HHT-ATZS.                                                棚卸帳簿
             03  HHT-TSZD  OCCURS  10.
               04  HHT-TZS    PIC S9(006) COMP-3.
           02  HHT-ATSU.                                                棚卸数
             03  HHT-TSUD  OCCURS  10.
               04  HHT-TSU    PIC S9(006) COMP-3.
           02  HHT-BC1        PIC  9(002).                              分類CD1
           02  HHT-BC2        PIC  9(002).                              分類CD2
           02  HHT-BC3        PIC  9(002).                              分類CD3
           02  HHT-BMNO       PIC  9(001).
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-KEY   PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-DATE.
             03  01D-DATE  PIC Z9.
             03  02D-DATE  PIC Z9.
             03  03D-DATE  PIC Z9.
             03  04D-DATE  PIC Z9.
           02  FILLER.
             03  D-M1.
               04  01D-M1  PIC  9(006).
               04  02D-M1  PIC  N(024).
               04  03D-M1  PIC ------9.
               04  04D-M1  PIC ------9.
               04  05D-M1  PIC ------9.
             03  D-M2.
               04  01D-M2  PIC  9(001).
               04  02D-M2  PIC ------9.
               04  03D-M2  PIC ------9.
               04  04D-M2  PIC ------9.
               04  05D-M2  PIC ------9.
               04  06D-M2  PIC ------9.
               04  07D-M2  PIC ------9.
               04  08D-M2  PIC ------9.
               04  09D-M2  PIC ------9.
               04  10D-M2  PIC ------9.
               04  11D-M2  PIC ------9.
             03  D-C1    PIC  X(007) VALUE "       ".
             03  D-C2    PIC  X(007) VALUE "       ".
           02  D-NM    PIC  X(037) VALUE
                "(  次ﾍﾟｰｼﾞ=ﾘﾀｰﾝ  ｺｰﾄﾞ=BSKIP   ﾘﾀｰﾝ  )".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-KEY" "9" "22" "9" "6" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-KEY" BY REFERENCE W-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "51" "1" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "205" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-DATE" " " "1" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "01D-DATE" "Z9" "1" "52" "2" " " "D-DATE" RETURNING RESU.
       CALL "SD_From" USING
        "01D-DATE" BY REFERENCE W-SG "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-DATE" "Z9" "1" "56" "2" "01D-DATE" " " RETURNING RESU.
       CALL "SD_From" USING
        "02D-DATE" BY REFERENCE W-SP "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-DATE" "Z9" "1" "67" "2" "02D-DATE" " " RETURNING RESU.
       CALL "SD_From" USING
        "03D-DATE" BY REFERENCE W-UG "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "04D-DATE" "Z9" "1" "71" "2" "03D-DATE" " " RETURNING RESU.
       CALL "SD_From" USING
        "04D-DATE" BY REFERENCE W-UP "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "01C-DSP" " " "W-L" "0" "75" "D-DATE" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-M1" " " "W-L" "0" "75" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "01D-M1" "9" "W-L" "1" "6" " " "D-M1" RETURNING RESU.
       CALL "SD_From" USING
        "01D-M1" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-M1" "N" "W-L" "8" "48" "01D-M1" " " RETURNING RESU.
       CALL "SD_From" USING
        "02D-M1" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
        "03D-M1" "------9" "W-L" "57" "7" "02D-M1" " " RETURNING RESU.
       CALL "SD_From" USING
        "03D-M1" BY REFERENCE W-TSU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
        "04D-M1" "------9" "W-L" "65" "7" "03D-M1" " " RETURNING RESU.
       CALL "SD_From" USING
        "04D-M1" BY REFERENCE W-ASU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
        "05D-M1" "------9" "W-L" "73" "7" "04D-M1" " " RETURNING RESU.
       CALL "SD_From" USING
        "05D-M1" BY REFERENCE W-SSU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-M2" " " "W-L" "0" "71" "D-M1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-M2" "9" "W-L" "9" "1" " " "D-M2" RETURNING RESU.
       CALL "SD_From" USING
        "01D-M2" BY REFERENCE W-SIZ "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
        "02D-M2" "------9" "W-L" "10" "7" "01D-M2" " " RETURNING RESU.
       CALL "SD_From" USING
        "02D-M2" BY REFERENCE W-SU(1) "6" "1" "1" 6 RETURNING RESU.
       CALL "SD_Init" USING
        "03D-M2" "------9" "W-L" "17" "7" "02D-M2" " " RETURNING RESU.
       CALL "SD_From" USING
        "03D-M2" BY REFERENCE W-SU(1) "6" "1" "2" 6 RETURNING RESU.
       CALL "SD_Init" USING
        "04D-M2" "------9" "W-L" "24" "7" "03D-M2" " " RETURNING RESU.
       CALL "SD_From" USING
        "04D-M2" BY REFERENCE W-SU(1) "6" "1" "3" 6 RETURNING RESU.
       CALL "SD_Init" USING
        "05D-M2" "------9" "W-L" "31" "7" "04D-M2" " " RETURNING RESU.
       CALL "SD_From" USING
        "05D-M2" BY REFERENCE W-SU(1) "6" "1" "4" 6 RETURNING RESU.
       CALL "SD_Init" USING
        "06D-M2" "------9" "W-L" "38" "7" "05D-M2" " " RETURNING RESU.
       CALL "SD_From" USING
        "06D-M2" BY REFERENCE W-SU(1) "6" "1" "5" 6 RETURNING RESU.
       CALL "SD_Init" USING
        "07D-M2" "------9" "W-L" "45" "7" "06D-M2" " " RETURNING RESU.
       CALL "SD_From" USING
        "07D-M2" BY REFERENCE W-SU(1) "6" "1" "6" 6 RETURNING RESU.
       CALL "SD_Init" USING
        "08D-M2" "------9" "W-L" "52" "7" "07D-M2" " " RETURNING RESU.
       CALL "SD_From" USING
        "08D-M2" BY REFERENCE W-SU(1) "6" "1" "7" 6 RETURNING RESU.
       CALL "SD_Init" USING
        "09D-M2" "------9" "W-L" "59" "7" "08D-M2" " " RETURNING RESU.
       CALL "SD_From" USING
        "09D-M2" BY REFERENCE W-SU(1) "6" "1" "8" 6 RETURNING RESU.
       CALL "SD_Init" USING
        "10D-M2" "------9" "W-L" "66" "7" "09D-M2" " " RETURNING RESU.
       CALL "SD_From" USING
        "10D-M2" BY REFERENCE W-SU(1) "6" "1" "9" 6 RETURNING RESU.
       CALL "SD_Init" USING
        "11D-M2" "------9" "W-L" "73" "7" "10D-M2" " " RETURNING RESU.
       CALL "SD_From" USING
        "11D-M2" BY REFERENCE W-SU(1) "6" "1" "10" 6 RETURNING RESU.
       CALL "SD_Init" USING
           "D-C1" "X" "W-L" "66" "7" "D-M2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-C2" "X" "W-L" "73" "7" "D-C1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-NM" "X" "23" "22" "37" "01C-DSP" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "77" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "77" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           COPY LIBCPR.
           MOVE D-HNG TO W-SG.
           MOVE D-HNP TO W-SP.
           MOVE D-HSG TO W-UG.
           MOVE D-HSP TO W-UP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO HHTF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HHTF_PNAME1 " " BY REFERENCE HHTF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-10.
           CALL "SD_Screen_Output" USING "SCHT11" RETURNING RESU.
           CALL "SD_Output" USING "D-DATE" D-DATE "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HHTF_PNAME1 " " BY REFERENCE HHTF_IDLST "0".
       M-15.
      *           READ HHTF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HHTF_PNAME1 BY REFERENCE HHT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-10
           END-IF
           IF  W-KEY > HHT-HCD
               GO TO M-15
           END-IF
           MOVE 6 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-20.
           MOVE HHT-HCD TO W-HCD.
           MOVE ZERO TO W-AZCD W-AZSUD.
       M-25.
           MOVE ZERO TO W-SC.
       M-30.
           ADD 1 TO W-SC.
           IF  W-SC = 11
               GO TO M-35
           END-IF
           IF  HHT-SIZ = 4
               IF  W-SC = 10
                   COMPUTE W-ASU = W-ASU + HHT-ZSU(10) + HHT-ASS(10)
                                                       - HHT-USU(10)
                   GO TO M-35
               END-IF
           END-IF
           COMPUTE W-ZSU(HHT-SIZ,W-SC) = HHT-ZSU(W-SC) + HHT-NSU(W-SC)
                                       - HHT-USU(W-SC) - HHT-ASS(W-SC).
           COMPUTE W-ASU = W-ASU + HHT-ASS(W-SC).
           IF  W-ZSU(HHT-SIZ,W-SC) NOT = ZERO
               ADD W-ZSU(HHT-SIZ,W-SC) TO W-TSU
               IF  W-ZC(HHT-SIZ) = 0
                   MOVE 1 TO W-ZC(HHT-SIZ)
               END-IF
           END-IF
           GO TO M-30.
       M-35.
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HHTF_IDLST HHTF_PNAME1
               CALL "DB_F_Open" USING
                "INPUT" HHTF_PNAME1 " " BY REFERENCE HHTF_IDLST "0"
               GO TO M-35
           END-IF
           IF  W-HCD = HHT-HCD
               GO TO M-25
           END-IF
           IF  ZERO = W-AZCD AND W-ASU
               GO TO M-20
           END-IF
      *
           COMPUTE W-ASU = W-ASU * -1.
           COMPUTE W-SSU = W-TSU - W-ASU.
           IF  JS-SIGN = 1
               IF  -1 < W-SSU AND < W-TSU
                   GO TO M-20
               END-IF
           END-IF
      *
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE ZERO TO CHK.
           IF  W-L = 22
               PERFORM S-05 THRU S-10
           END-IF
           IF  CHK = 9
               GO TO M-10
           END-IF
      *
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　ＨＩＭ　なし　＊＊　" TO HI-NAME
           END-IF
      *
           CALL "SD_Output" USING "D-M1" D-M1 "p" RETURNING RESU.
      *
           MOVE ZERO TO CNT.
       M-40.
           ADD 1 TO CNT.
           IF  CNT = 5
               GO TO M-20
           END-IF
           COMPUTE W-SIZ = CNT + 1.
           IF  W-SIZ = 5
               MOVE 1 TO W-SIZ
           END-IF
           IF  W-ZC(W-SIZ) = 0
               GO TO M-40
           END-IF
           MOVE ZERO TO W-SUD.
           MOVE W-AZSU(W-SIZ) TO W-SUD.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE ZERO TO CHK.
           IF  W-L = 22
               PERFORM S-05 THRU S-10
           END-IF
           IF  CHK = 9
               GO TO M-10
           END-IF
           CALL "SD_Output" USING "D-M2" D-M2 "p" RETURNING RESU.
           IF  W-SIZ = 4
               CALL "SD_Output" USING "D-C1" D-C1 "p" RETURNING RESU
           END-IF
           IF  W-SIZ = 3 OR 4
               CALL "SD_Output" USING "D-C2" D-C2 "p" RETURNING RESU
           END-IF
           GO TO M-40.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 9 TO CHK
               GO TO S-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-05
           END-IF
           CALL "SD_Screen_Output" USING "SCHT11" RETURNING RESU.
           CALL "SD_Output" USING "D-DATE" D-DATE "p" RETURNING RESU.
           MOVE 7 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       S-10.
           EXIT.

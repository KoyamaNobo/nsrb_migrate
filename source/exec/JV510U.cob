       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JV510U.
      ************************************************************
      *    PROGRAM         :  玉島棚卸変換用ワーク作成           *
      *    PRINTER TYPE    :  JIPS                               *
      *    SCREEN          :  ______                             *
      *    COMPILE TYPE    :  COBOL                              *
      ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0128".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  W-DMM          PIC  X(01).
           02  W-SC           PIC  9(001).
           02  CNT            PIC  9(02).
           02  W-SCD          PIC  9(001).
           02  W-ASID.
             03  W-ASI   OCCURS   5.
               04  W-SID   OCCURS  10.
                 05  W-SI     PIC  X(004).
           02  W-MSI.
             03  F            PIC  X(040) VALUE
                  "        SS  S   M   L   LL  XL  XXL     ".
             03  F            PIC  X(040) VALUE
                  "                            28.029.030.0".
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.0    ".
             03  F            PIC  X(040) VALUE
                  "24.024.525.025.526.026.527.027.5        ".
           02  W-CHK          PIC  9(05).
           COPY LSTAT.
      *
           COPY LIHIM.
           COPY LICODE.
      *FD  WTANA
       01  WTANA_JV510U.
           02  WTANA_PNAME1   PIC  X(009) VALUE "TANAOROSI".
           02  F              PIC  X(001).
           02  WTANA_LNAME    PIC  X(012) VALUE "WTANA_JV510U".
           02  F              PIC  X(001).
           02  WTANA_KEY1     PIC  X(100) VALUE SPACE.
           02  WTANA_SORT     PIC  X(100) VALUE SPACE.
           02  WTANA_IDLST    PIC  X(100) VALUE SPACE.
           02  WTANA_RES      USAGE  POINTER.
       01  WTANA-R.
           02  WTANA-HHT      PIC X(6).                                 HHT
           02  WTANA-KBN      PIC 9(1).                                 検品C
           02  WTANA-DATE     PIC 9(8).                                 日付
           02  WTANA-SOK      PIC 9(1).                                 倉庫C
           02  WTANA-BAS      PIC 9(6).                                 場所NO
           02  WTANA-JAN      PIC X(16).                                JAN
           02  WTANA-JAND  REDEFINES WTANA-JAN.
             03  WTANA-JAN1   PIC X(13).
             03  WTANA-JAN2   PIC X(3).
           02  WTANA-NAME     PIC N(24).                                品名
           02  WTANA-HCD      PIC 9(6).                                 品名C
           02  WTANA-SIZ      PIC X(4).                                 ｻｲｽﾞ
           02  WTANA-SU       PIC S9(4).                                数量
           02  WTANA-NGP      PIC 9(8).                                 D日付
           02  WTANA-TIME     PIC 9(6).                                 D時刻
       77  F                  PIC X(1).
      *FD  TANAO                                                        ﾆﾌﾀﾞﾄﾗﾝ
       01  TANAO_JV510U.
           02  TANAO_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TANAO_LNAME    PIC  X(012) VALUE "TANAO_JV510U".
           02  F              PIC  X(001).
           02  TANAO_KEY1     PIC  X(100) VALUE SPACE.
           02  TANAO_SORT     PIC  X(100) VALUE SPACE.
           02  TANAO_IDLST    PIC  X(100) VALUE SPACE.
           02  TANAO_RES      USAGE  POINTER.
       01  TANAO-R.
           02  TANAO-HHT      PIC X(6).                                 HHT
           02  TANAO-SOK      PIC 9(1).                                 倉庫C
           02  TANAO-BAS      PIC 9(6).                                 場所NO
           02  TANAO-HCD      PIC 9(6).                                 品名C
           02  TANAO-SC       PIC 9(1).                                 ｻｲｽﾞ
           02  TANAO-ASU.                                               数量
             03  TANAO-SUD   OCCURS  10.                                数量
               04  TANAO-SU   PIC S9(6).                                数量
           02  TANAO-GC       PIC 9(1).                                 行C
           02  TANAO-ISU      PIC 9(3).                                 入数
           02  TANAO-BC       PIC 9(06).
           02  F              PIC X(38).
       77  F                  PIC X(01).
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
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　棚卸変換用ワーク作成　　＊＊＊".
           02  FILLER.
               03  FILLER  PIC  N(003) VALUE  "確認（".
               03  FILLER  PIC  X(009) VALUE  "OK=1,NO=9".
               03  FILLER  PIC  N(001) VALUE  "）".
               03  FILLER  PIC  X(009) VALUE  "---> ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME3   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME8   PIC  X(017) VALUE
                  "***  ｻｲｽﾞ ﾅｼ  ***".
             03  E-ME9   PIC  X(017) VALUE
                  "***  ｲﾘｽｳ ﾅｼ  ***".
             03  E-HCD   PIC  9(006).
             03  E-SIZ   PIC  X(004).
             03  E-CHK   PIC  9(005).
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
           "C-MID" " " "0" "0" "66" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "1" "16" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" " " "23" "0" "26" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "23" "41" "6" " " "02C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "X" "23" "47" "9" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "23" "56" "2" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "X" "23" "58" "9" "05C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "62" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "100" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "100" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" "X" "24" "15" "16" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME8" "X" "24" "15" "17" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME9" "X" "24" "15" "17" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-HCD" "9" "24" "45" "6" "E-ME9" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-HCD" BY REFERENCE WTANA-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-SIZ" "X" "24" "53" "4" "E-HCD" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-SIZ" BY REFERENCE WTANA-SIZ "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-CHK" "9" "24" "75" "5" "E-SIZ" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-CHK" BY REFERENCE W-CHK "5" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           MOVE W-MSI TO W-ASID.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID22.
           MOVE W-FID2 TO WK0128ID.
           MOVE WK0128ID TO TANAO_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" WTANA_PNAME1 " " BY REFERENCE WTANA_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "OUTPUT" TANAO_PNAME1 " " BY REFERENCE TANAO_IDLST "0".
      *           READ WTANA AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" WTANA_PNAME1 BY REFERENCE WTANA-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-15.
           ADD 1 TO W-CHK.
           CALL "SD_Output" USING "E-CHK" E-CHK "p" RETURNING RESU.
           MOVE WTANA-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-30
           END-IF
           IF  WTANA-JAN2 NOT = SPACE
               IF  HI-ISU = ZERO
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "E-ME9" E-ME9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-HCD" E-HCD "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-SIZ" E-SIZ "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-30
               END-IF
           END-IF
           IF  WTANA-SIZ = SPACE
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SIZ" E-SIZ "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-30
           END-IF
           MOVE 0 TO W-SC.
       M-20.
           ADD 1 TO W-SC.
           IF  W-SC = 6
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SIZ" E-SIZ "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-30
           END-IF
           IF  W-SC > 1
               COMPUTE W-SCD = W-SC - 1
           ELSE
               MOVE W-SC TO W-SCD
           END-IF
           MOVE ZERO TO CNT.
       M-25.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-20
           END-IF
           IF  WTANA-SIZ NOT = W-SI(W-SC,CNT)
               GO TO M-25
           END-IF
           IF  HI-S(W-SCD,CNT) = 0
               GO TO M-25
           END-IF
      *
           INITIALIZE TANAO-R.
           MOVE WTANA-HHT TO TANAO-HHT.
           IF  WTANA-SOK = 1
               MOVE 6 TO TANAO-SOK
           END-IF
           MOVE WTANA-BAS TO TANAO-BAS.
           MOVE WTANA-HCD TO TANAO-HCD.
           MOVE W-SCD TO TANAO-SC.
           IF  WTANA-JAN2 NOT = SPACE
               COMPUTE TANAO-SU(CNT) = WTANA-SU / HI-ISU
               IF  W-SCD = 1
                   MOVE 4 TO TANAO-GC
               ELSE
                   IF  W-SCD = 2
                       MOVE 1 TO TANAO-GC
                   ELSE
                       IF  W-SCD = 3
                           MOVE 2 TO TANAO-GC
                       ELSE
                           IF  W-SCD = 4
                               MOVE 3 TO TANAO-GC
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  WTANA-JAN2 = SPACE
               MOVE WTANA-SU TO TANAO-SU(CNT)
               IF  W-SCD = 1
                   MOVE 8 TO TANAO-GC
               ELSE
                   IF  W-SCD = 2
                       MOVE 5 TO TANAO-GC
                   ELSE
                       IF  W-SCD = 3
                           MOVE 6 TO TANAO-GC
                       ELSE
                           IF  W-SCD = 4
                               MOVE 7 TO TANAO-GC
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE HI-ISU TO TANAO-ISU.
           MOVE HI-BC TO TANAO-BC.
      *           WRITE TANAO-R.
      *//////////////
           CALL "DB_Insert" USING
            TANAO_PNAME1 TANAO_LNAME TANAO-R RETURNING RET.
       M-30.
      *           READ WTANA AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" WTANA_PNAME1 BY REFERENCE WTANA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           GO TO M-15.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE WTANA_IDLST WTANA_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TANAO_IDLST TANAO_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

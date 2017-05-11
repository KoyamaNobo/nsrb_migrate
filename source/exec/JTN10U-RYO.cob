       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JTN10U.
      *================================================================*
      *            福山通運  荷札変換ファイル  生成　　　　　　        *
      *               JS-SIGN  0:藤田 , 1:早島                         *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA                           DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-SEN          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-CHK          PIC  9(001).
           02  W-NAME         PIC  N(026).
           02  W-NAMD  REDEFINES W-NAME.
             03  W-NAU        PIC  N(020).
             03  W-NAS        PIC  N(006).
           02  W-NGP          PIC  9(006).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-SYSD         PIC  9(006).
           02  W-YMD   REDEFINES W-SYSD.
             03  W-YY         PIC  9(002).
             03  W-MM         PIC  9(002).
             03  W-DD         PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY  LOKJF-RYO.
           COPY  LITCM.
      *FD  FUKUF
       01  FUKUF_JTN10U.
           02  FUKUF_PNAME1 PIC  X(006) VALUE "FUKUF1".
           02  F            PIC  X(001).
           02  FUKUF_LNAME  PIC  X(012) VALUE "FUKUF_JTN10U".
           02  F            PIC  X(001).
           02  FUKUF_KEY1   PIC  X(100) VALUE SPACE.
           02  FUKUF_SORT   PIC  X(100) VALUE SPACE.
           02  FUKUF_IDLST  PIC  X(100) VALUE SPACE.
           02  FUKUF_RES    USAGE  POINTER.
       01  FUKU-R.
           02  FUKU-X1      PIC X(15).
           02  FUKU-TEL     PIC X(17).
           02  FUKU-JSU     PIC N(20).
           02  FUKU-JSS     PIC N(20).
           02  FUKU-N1      PIC N(20).
           02  FUKU-NAU     PIC N(20).
           02  FUKU-NASD    PIC N(20).
           02  FUKU-NASW  REDEFINES FUKU-NASD.
             03  FUKU-NAS   PIC N(06).
             03  FUKU-N2    PIC N(14).
           02  FUKU-UNO     PIC X(08).
           02  FUKU-X2      PIC X(05).
           02  FUKU-X3      PIC X(03).
           02  FUKU-NR      PIC X(12).
           02  FUKU-KSU     PIC 9(02).
           02  FUKU-91      PIC 9(03).
           02  FUKU-92      PIC 9(04).
           02  FUKU-F1.
             03  FUKU-N3    PIC N(15).
           02  FUKU-F1.
             03  FUKU-N4    PIC N(15).
           02  FUKU-HSI     PIC N(09).
           02  FUKU-NSP     PIC N(06).
           02  FUKU-X5      PIC X(30).
           02  FUKU-X6      PIC X(30).
           02  FUKU-95      PIC 9(08).
           02  FUKU-NO.
             03  FUKU-ONO   PIC X(06).
             03  FUKU-X7    PIC X(10).
           02  FUKU-X8      PIC X(30).
           02  FUKU-1       PIC 9(01).
           02  FUKU-96      PIC 9(04).
           02  FUKU-NGP1    PIC 9(08).
           02  FUKU-NGP2    PIC 9(08).
       77  F                PIC X(01).
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
           02  FILLER  PIC  N(021) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                  "＊＊＊　　　福山通運　荷札変換　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(030) VALUE
                "未抽出分=1 , 日付指定=5   ﾘﾀｰﾝ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-DSP.
           02  D-MID0  PIC  N(007) VALUE
                  "｛　藤　田　｝".
           02  D-MID1  PIC  N(007) VALUE
                  "｛　早　島　｝".
           02  D-CHK.
             03  FILLER  PIC  N(012) VALUE
                    "（　前回データ未処理　）".
             03  FILLER  PIC  X(038) VALUE
                  "前回データ　消さない=0 , 消す=5   ﾘﾀｰﾝ".
           02  D-DATE.
             03  FILLER  PIC  X(018) VALUE
                  "'  年   月   日 分".
           02  D-DATS.
             03  FILLER  PIC  X(018) VALUE
                  "                  ".
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  A-DMM   PIC  9(001).
           02  A-CHK   PIC  9(001).
           02  A-NGP.
             03  A-NEN   PIC  9(002).
             03  A-GET   PIC  9(002).
             03  A-PEY   PIC  9(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(026) VALUE
                  "***  OKJF REWRITE ｴﾗｰ  ***".
             03  E-KEY   PIC  9(006).
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
            "C-MID" " " "0" "0" "346" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "42" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "16" "16" "30" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "22" "21" "22" "08C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "126" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID0" "N" "5" "24" "14" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID1" "N" "5" "24" "14" "D-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CHK" " " "0" "0" "62" "D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-CHK" "N" "12" "19" "24" " " "D-CHK" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-CHK" "X" "14" "12" "38" "01D-CHK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATE" " " "0" "0" "18" "D-CHK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DATE" "X" "18" "28" "18" " " "D-DATE" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATS" " " "0" "0" "18" "D-DATE" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DATS" "X" "18" "28" "18" " " "D-DATS" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "16" "41" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "38" "1" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CHK" "9" "14" "45" "1" "A-DMM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CHK" BY REFERENCE W-CHK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NGP" " " "18" "0" "6" "A-CHK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "18" "29" "2" " " "A-NGP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "18" "34" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PEY" "9" "18" "39" "2" "A-GET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "49" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "49" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "26" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "9" "24" "45" "6" "E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE OKJF-01 "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 0 AND 1
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Output" USING
                "D-MID0" D-MID0 "p" RETURNING RESU
           ELSE
               IF  JS-SIGN = 0
                   CALL "SD_Output" USING
                    "D-MID1" D-MID1 "p" RETURNING RESU
               END-IF
           END-IF
           MOVE ZERO TO W-DATA.
           PERFORM S-05 THRU S-20.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
           ACCEPT W-SYSD FROM DATE.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SEN = 1
               CALL "SD_Output" USING "D-DATS" D-DATS "p" RETURNING RESU
               GO TO M-30
           END-IF
           IF  W-SEN NOT = 5
               GO TO M-10
           END-IF
           MOVE W-SYSD TO W-NGP.
           CALL "SD_Output" USING "D-DATE" D-DATE "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NGP" A-NGP "p" RETURNING RESU.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  W-NEN > 00 AND < 14
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  W-GET > 12
               GO TO M-20
           END-IF
           IF  W-GET = ZERO
               IF  W-NEN NOT = ZERO
                   GO TO M-20
               END-IF
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-PEY "A-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-PEY > 31
               GO TO M-20
           END-IF
           IF  W-PEY = ZERO
               IF  W-GET NOT = ZERO
                   GO TO M-25
               ELSE
                   MOVE W-SYSD TO W-NGP
                   CALL "SD_Output" USING
                    "A-NGP" A-NGP "p" RETURNING RESU
               END-IF
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-SEN = 5
                   GO TO M-25
               ELSE
                   GO TO M-10
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-30
           END-IF
      *
           CALL "DB_F_Open" USING "I-O SEQUENTIAL" OKJF_PNAME1 
            "SHARED" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
       M-40.
      *           READ OKJF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" OKJF_PNAME1 BY REFERENCE OKJF-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE OKJF_IDLST OKJF_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  OKJF-10 NOT = 1
               GO TO M-40
           END-IF
           IF  JS-SIGN = 0
               IF  OKJF-04 NOT = 6
                   GO TO M-40
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  OKJF-04 NOT = 4
                   GO TO M-40
               END-IF
           END-IF
           IF  OKJF-02 NOT = 1
               GO TO M-40
           END-IF
           IF  W-SEN = 1
               IF  OKJF-08 = 1
                   GO TO M-40
               END-IF
           END-IF
           IF  W-SEN = 5
               IF  OKJF-03 NOT = W-NGP
                   GO TO M-40
               END-IF
           END-IF
           IF  OKJF-07 = ZERO
               GO TO M-40
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" FUKUF_PNAME1 " " BY REFERENCE FUKUF_IDLST "0".
       M-45.
           MOVE OKJF-05 TO TC-KEY.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-R
               INITIALIZE TC-R
           END-IF
           INITIALIZE FUKU-R.
           MOVE SPACE TO FUKU-X1
                         FUKU-TEL
                         FUKU-X2
                         FUKU-X3
                         FUKU-NR
                         FUKU-UNO
                         FUKU-X5
                         FUKU-X6
                         FUKU-ONO
                         FUKU-X7
                         FUKU-X8.
           MOVE SPACE TO FUKU-JSU
                         FUKU-JSS
                         FUKU-N1
                         FUKU-NAU
                         FUKU-NASD
                         FUKU-HSI
                         FUKU-NSP
                         FUKU-N3
                         FUKU-N4.
           MOVE ZERO  TO FUKU-KSU
                         FUKU-91
                         FUKU-92
                         FUKU-95
                         FUKU-96
                         FUKU-NGP1
                         FUKU-NGP2.
           MOVE 1 TO FUKU-1.
           MOVE TC-TEL TO FUKU-TEL.
           MOVE TC-JSU TO FUKU-JSU.
           MOVE TC-JSS TO FUKU-JSS.
           MOVE TC-NAME TO W-NAME.
           MOVE W-NAU TO FUKU-NAU.
           MOVE W-NAS TO FUKU-NAS.
           MOVE TC-UNO TO FUKU-UNO.
           IF  JS-SIGN = 0
               IF  OKJF-05 > 4999999 AND < 5001000
                   MOVE "0865230155" TO FUKU-NR
               ELSE
                   MOVE "0862432456" TO FUKU-NR
               END-IF
           END-IF
           IF  JS-SIGN = 1
               MOVE "0862432467" TO FUKU-NR
           END-IF
           MOVE OKJF-07 TO FUKU-KSU.
           MOVE OKJF-06 TO FUKU-HSI.
           MOVE OKJF-01 TO FUKU-ONO.
           MOVE OKJF-03 TO FUKU-NGP1.
           MOVE W-SYSD TO FUKU-NGP2.
           ADD 20000000 TO FUKU-NGP1 FUKU-NGP2.
      *           WRITE FUKU-R.
      *//////////////
           CALL "DB_Insert" USING
            FUKUF_PNAME1 FUKUF_LNAME FUKU-R RETURNING RET.
      *
           IF  OKJF-08 = 1
               GO TO M-50
           END-IF
           MOVE 1 TO OKJF-08.
      *           REWRITE OKJF-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            OKJF_PNAME1 OKJF_LNAME OKJF-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-50.
      *           READ OKJF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" OKJF_PNAME1 BY REFERENCE OKJF-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  OKJF-10 NOT = 1
               GO TO M-50
           END-IF
           IF  JS-SIGN = 0
               IF  OKJF-04 NOT = 6
                   GO TO M-50
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  OKJF-04 NOT = 4
                   GO TO M-50
               END-IF
           END-IF
           IF  OKJF-02 NOT = 1
               GO TO M-50
           END-IF
           IF  W-SEN = 1
               IF  OKJF-08 = 1
                   GO TO M-50
               END-IF
           END-IF
           IF  W-SEN = 5
               IF  OKJF-03 NOT = W-NGP
                   GO TO M-50
               END-IF
           END-IF
           IF  OKJF-07 = ZERO
               GO TO M-50
           END-IF
           GO TO M-45.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE FUKUF_IDLST FUKUF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           CALL "DB_F_Open" USING
            "INPUT" FUKUF_PNAME1 " " BY REFERENCE FUKUF_IDLST "0".
       S-10.
      *           READ FUKUF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" FUKUF_PNAME1 BY REFERENCE FUKU-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE FUKUF_IDLST FUKUF_PNAME1
               GO TO S-20
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE FUKUF_IDLST FUKUF_PNAME1.
           CALL "SD_Output" USING "D-CHK" D-CHK "p" RETURNING RESU.
       S-15.
           CALL "SD_Accept" USING BY REFERENCE A-CHK "A-CHK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO S-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-15
           END-IF
           IF  W-CHK NOT = 0 AND 5
               GO TO S-15
           END-IF
           IF  W-CHK = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
           END-IF.
       S-20.
           EXIT.

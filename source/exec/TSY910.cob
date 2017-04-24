       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSY910.
      ******************************************************
      *****     受取手形・支払手形マスター　クリア     *****
      ******************************************************
       AUTHOR.     F-KOTAKE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-NGP.
             03  W-NEN      PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1   PIC  9(002).
               04  W-NEN2   PIC  9(002).
             03  W-GET      PIC  9(002).
             03  W-PEY      PIC  9(002).
           02  W-MNGP.
             03  W-MNEN     PIC  9(004).
             03  W-MGET     PIC  9(002).
             03  W-MPEY     PIC  9(002).
           02  W-DMM        PIC  9(001).
       01  ERR-STAT         PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIUKET.
           COPY LISHIT.
           COPY LITM.
           COPY LISM.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                  "＊＊＊　　　受手・支手マスター　クリア　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(032) VALUE
                "［　　平成  年  月  日　迄　　］".
           02  FILLER  PIC  X(028) VALUE
                "<  確認  OK=1 NO=9   ﾘﾀｰﾝ  >".
       01  C-ACP.
           02  FILLER.
             03  A-NEN   PIC  9(002).
             03  A-GET   PIC  9(002).
             03  A-PEY   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-D1    PIC  N(017) VALUE
                    "【　　ＴＭ　手形区分　クリア　　】".
             03  D-D2    PIC  N(017) VALUE
                    "【　　ＳＭ　手形区分　クリア　　】".
             03  D-D3    PIC  N(017) VALUE
                    "【　　　受取手形　　クリア　　　】".
             03  D-D4    PIC  N(017) VALUE
                    "【　　　支払手形　　クリア　　　】".
             03  D-D5    PIC  N(017) VALUE
                    "【　　ＴＭ　　手形区分　登録　　】".
             03  D-D6    PIC  N(017) VALUE
                    "【　　ＳＭ　　手形区分　登録　　】".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(026) VALUE
                  "***  UKETM DELETE ｴﾗｰ  ***".
             03  E-ME2   PIC  X(026) VALUE
                  "***  SHITM DELETE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(026) VALUE
                  "***  TM REWRITE ｴﾗｰ 1  ***".
             03  E-ME4   PIC  X(026) VALUE
                  "***  SM REWRITE ｴﾗｰ 1  ***".
             03  E-ME5   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME6   PIC  X(015) VALUE
                  "***  SM ﾅｼ  ***".
             03  E-ME7   PIC  X(026) VALUE
                  "***  TM REWRITE ｴﾗｰ 2  ***".
             03  E-ME8   PIC  X(026) VALUE
                  "***  SM REWRITE ｴﾗｰ 2  ***".
             03  E-UKEY  PIC  X(004).
             03  E-SKEY  PIC  X(004).
             03  E-TCD   PIC  X(004).
             03  E-SCD   PIC  X(004).
             03  E-STAT  PIC  X(002).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "410" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING 
            "08C-MID" "X" "14" "19" "32" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "21" "28" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "14" "0" "6" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "14" "29" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "14" "33" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PEY" "9" "14" "37" "2" "A-GET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "41" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "204" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "18" "0" "204" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-D1" "N" "18" "18" "34" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-D2" "N" "18" "18" "34" "D-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-D3" "N" "18" "18" "34" "D-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-D4" "N" "18" "18" "34" "D-D3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-D5" "N" "18" "18" "34" "D-D4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-D6" "N" "18" "18" "34" "D-D5" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "214" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "214" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "26" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "26" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "26" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "26" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "15" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "15" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "26" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "26" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-UKEY" "X" "24" "60" "4" "E-ME8" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-UKEY" BY REFERENCE UT-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-SKEY" "X" "24" "60" "4" "E-UKEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-SKEY" BY REFERENCE ST-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TCD" "X" "24" "50" "4" "E-SKEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE T-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-SCD" "X" "24" "50" "4" "E-TCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-SCD" BY REFERENCE S-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-SCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
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
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-PEY "A-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-25
           END-IF.
      *
           PERFORM S-05 THRU S-20.
           COPY LIBCPR.
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-NEN
           ELSE
               IF  W-NEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-NEN
               END-IF
           END-IF
           CALL "DB_F_Open" USING
            "I-O" UKET-M_PNAME1 " " BY REFERENCE UKET-M_IDLST "1"
            "UT-KEY" BY REFERENCE UT-KEY.
           CALL "DB_F_Open" USING
            "I-O" SHIT-M_PNAME1 " " BY REFERENCE SHIT-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
           CALL "SD_Output" USING "D-D3" D-D3 "p" RETURNING RESU.
       M-30.
      *           READ UKET-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" UKET-M_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "D-D4" D-D4 "p" RETURNING RESU
               GO TO M-35
           END-IF
           MOVE ZERO TO W-MNGP.
           MOVE UT-SNM TO W-MNEN.
           MOVE UT-OKG TO W-MGET.
           MOVE UT-OKP TO W-MPEY.
           IF  W-NGP < W-MNGP
               GO TO M-30
           END-IF
           IF  UT-SKC NOT = 50 AND 19 AND 90
               GO TO M-30
           END-IF
      *           DELETE UKET-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING UKET-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-UKEY" E-UKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-30.
       M-35.
      *           READ SHIT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SHIT-M_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           MOVE ZERO TO W-MNGP.
           MOVE ST-SNM TO W-MNEN.
           MOVE ST-MKG TO W-MGET.
           MOVE ST-MKP TO W-MPEY.
           IF  W-NGP < W-MNGP
               GO TO M-35
           END-IF
           IF  ST-SKC NOT = 50 AND 80 AND 90
               GO TO M-35
           END-IF
      *           DELETE SHIT-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING SHIT-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SKEY" E-SKEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-35.
       M-40.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-M_IDLST UKET-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SHIT-M_IDLST SHIT-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" UKET-M_PNAME1 " " BY REFERENCE UKET-M_IDLST "1"
            "UT-KEY" BY REFERENCE UT-KEY.
           CALL "SD_Output" USING "D-D5" D-D5 "p" RETURNING RESU.
       M-50.
      *           READ UKET-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" UKET-M_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           MOVE UT-TCD TO T-KEY.
      *           READ T-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-50
           END-IF
           IF  T-TGC = 1
               GO TO M-50
           END-IF
           MOVE 1 TO T-TGC.
      *           REWRITE T-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            T-M_PNAME1 T-M_LNAME T-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-50.
       M-55.
           CALL "DB_F_Open" USING
            "INPUT" SHIT-M_PNAME1 " " BY REFERENCE SHIT-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
           CALL "SD_Output" USING "D-D6" D-D6 "p" RETURNING RESU.
       M-60.
      *           READ SHIT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SHIT-M_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           MOVE ST-TCD TO S-KEY.
      *           READ S-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-60
           END-IF
           IF  S-TGC = 1
               GO TO M-60
           END-IF
           MOVE 1 TO S-TGC.
      *           REWRITE S-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            S-M_PNAME1 S-M_LNAME S-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SCD" E-SCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-60.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-M_IDLST UKET-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SHIT-M_IDLST SHIT-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           CALL "DB_F_Open" USING
            "I-O" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "I-O" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "SD_Output" USING "D-D1" D-D1 "p" RETURNING RESU.
       S-10.
      *           READ T-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" T-M_PNAME1 BY REFERENCE T-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "D-D2" D-D2 "p" RETURNING RESU
               GO TO S-15
           END-IF
           MOVE 0 TO T-TGC.
      *           REWRITE T-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            T-M_PNAME1 T-M_LNAME T-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO S-10.
       S-15.
      *           READ S-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" S-M_PNAME1 BY REFERENCE S-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-20
           END-IF
           MOVE 0 TO S-TGC.
      *           REWRITE S-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            S-M_PNAME1 S-M_LNAME S-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-SCD" E-SCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO S-15.
       S-20.
           EXIT.

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JTN95U.
      *********************************************************
      *    倉別在庫Ｍ　変換  （親コード修正時）               *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-MSG              PIC  X(030).
       77  W-SPACE            PIC  X(065) VALUE SPACE.
       01  W-DATA.
           02  W-AHCD.
             03  W-HCDD  OCCURS  18.
               04  W-FHCD     PIC  9(006).
               04  W-RHCD     PIC  9(006).
           02  CNT            PIC  9(002).
           02  W-C            PIC  9(002).
           02  W-L            PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-DE           PIC  9(001).
           02  W-S            PIC  9(001).
           02  W-DMM          PIC  9(001).
       01  W-RD.
           02  W-04.                                                    ｾﾞﾝｸﾘｺｼ
             03  W-041   OCCURS  10.                                    ｻｲｽﾞﾍﾞﾂ
               04  W-0411     PIC S9(006).
           02  W-05.                                                    ﾄｳﾆｭｳｺ
             03  W-051   OCCURS  10.                                    ｻｲｽﾞﾍﾞﾂ
               04  W-0511     PIC S9(006).
           02  W-06.                                                    ﾄｳｼｭｯｺ
             03  W-061   OCCURS  10.                                    ｻｲｽﾞﾍﾞﾂ
               04  W-0611     PIC S9(006).
           02  W-07.
             03  W-071   OCCURS  10.                                    ｻｲｽﾞﾍﾞﾂ
               04  W-0711     PIC S9(006).
           02  W-08.
             03  W-081   OCCURS  10.                                    ｻｲｽﾞﾍﾞﾂ
               04  W-0811     PIC S9(006).
           02  W-09.                                                    ｻｼｽﾞﾐｶｸﾃ
             03  W-091   OCCURS  10.                                    ｻｲｽﾞﾍﾞﾂ
               04  W-0911     PIC S9(006).
           02  W-10.                                                    ｻﾞｲｺﾁｮｳｾ
             03  W-101   OCCURS  10.                                    ｻｲｽﾞﾍﾞﾂ
               04  W-1011     PIC S9(006).
           02  W-11.                                                    ｼﾒｺﾞﾆｭｳｺ
             03  W-111   OCCURS  10.                                    ｻｲｽﾞﾍﾞﾂ
               04  W-1111     PIC S9(006).
           COPY LSTAT.
      *
           COPY LIHIM.
           COPY L-JCON.
           COPY LNJZAI.
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
           02  FILLER  PIC  N(028) VALUE
           "＊＊＊　　倉別在庫Ｍ　変換　（親コード修正時）　　＊＊＊".
           02  FILLER  PIC  X(027) VALUE
                "元ｺｰﾄﾞ → 先ｺｰﾄﾞ 品　　　名".
           02  FILLER  PIC  X(010) VALUE "確認=ｆ･10".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-FHCD  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-SPACE PIC  X(065).
             03  D-RHCD  PIC  9(006).
             03  D-NAME  PIC  N(024).
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(030).
             03  E-HCD   PIC  9(006).
             03  E-KEY   PIC  X(008).
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
            "C-MID" " " "0" "0" "115" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "15" "56" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "3" "7" "27" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "22" "7" "10" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "23" "42" "22" "03C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FHCD" "9" "W-L" "7" "6" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FHCD" BY REFERENCE W-FHCD(1) "6" "1" BY REFERENCE CNT 12
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "59" "1" "A-FHCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "119" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "W-L" "0" "119" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SPACE" "X" "W-L" "7" "65" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SPACE" BY REFERENCE W-SPACE "65" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-RHCD" "9" "W-L" "17" "6" "D-SPACE" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-RHCD" BY REFERENCE W-RHCD(1) "6" "1" BY REFERENCE CNT 12
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "W-L" "24" "48" "D-RHCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "44" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "44" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME" "X" "24" "15" "30" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-ME" BY REFERENCE W-MSG "30" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HCD" "9" "24" "46" "6" "E-ME" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HCD" BY REFERENCE HI-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "53" "8" "E-HCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE NJZAI-KEY "8" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           MOVE ZERO TO W-AHCD CNT CHK.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-10.
           ADD 1 TO CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT = 19
               GO TO M-25
           END-IF
           IF  CHK = 1
               MOVE ZERO TO W-FHCD(CNT) W-RHCD(CNT)
               CALL "SD_Output" USING
                "D-SPACE" D-SPACE "p" RETURNING RESU
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-FHCD "A-FHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               IF  CNT = 1
                   GO TO M-95
               END-IF
           END-IF
           IF  ESTAT = ADV
               IF  CNT NOT = 1
                   MOVE 1 TO CHK
                   MOVE ZERO TO W-FHCD(CNT) W-RHCD(CNT)
                   CALL "SD_Output" USING
                    "D-SPACE" D-SPACE "p" RETURNING RESU
                   GO TO M-10
               END-IF
           END-IF
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
      *
           MOVE W-FHCD(CNT) TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           IF  HI-MHCD = HI-KEY
               GO TO M-15
           END-IF
           MOVE HI-MHCD TO W-RHCD(CNT).
           MOVE W-RHCD(CNT) TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  親ｺｰﾄﾞ ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-15
           END-IF
           CALL "SD_Output" USING "D-RHCD" D-RHCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           GO TO M-10.
       M-20.
           IF  CNT NOT = 1
               SUBTRACT 1 FROM CNT W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
           END-IF
           IF  W-RHCD(CNT) = ZERO
               GO TO M-20
           END-IF
           GO TO M-15.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 0 TO CHK
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-DMM = 9
               MOVE 0 TO CHK
               GO TO M-20
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-25
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
      *
           MOVE SPACE TO JCON1-KEY.
           MOVE 3 TO JCON3-01.
      *           START JCON KEY NOT < JCON1-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JCON_PNAME1 "JCON1-KEY" " NOT < " JCON1-KEY RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  JCON ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE 0 TO W-DE.
       M-30.
           IF  W-DE = 9
               GO TO M-90
           END-IF
      *           READ JCON NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JCON_PNAME1 BY REFERENCE JCON-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               MOVE 9 TO JCON3-02 W-DE
               GO TO M-35
           END-IF
           IF  JCON3-01 NOT = 3
               MOVE 9 TO JCON3-02 W-DE
           END-IF.
       M-35.
           MOVE ZERO TO CNT.
       M-40.
           ADD 1 TO CNT.
           IF  CNT = 19
               GO TO M-30
           END-IF
           IF  W-FHCD(CNT) = ZERO
               GO TO M-30
           END-IF
           MOVE W-FHCD(CNT) TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO HI-S(1,1) HI-S(2,1) HI-S(3,1) HI-S(4,1)
           END-IF
           MOVE 0 TO W-S.
       M-45.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO M-40
           END-IF
           IF  HI-SS(W-S) = ZERO
               GO TO M-45
           END-IF
           MOVE JCON3-02 TO NJZAI-01.
           MOVE W-FHCD(CNT) TO NJZAI-02.
           MOVE W-S TO NJZAI-03.
      *           READ NJZAI INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF
           PERFORM S-05 THRU S-15.
      *
           MOVE JCON3-02 TO NJZAI-01.
           MOVE W-RHCD(CNT) TO NJZAI-02.
           MOVE W-S TO NJZAI-03.
      *           READ NJZAI INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           PERFORM S-20 THRU S-30.
      *           REWRITE NJZAI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  REWRITE ｴﾗｰ  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-55.
       M-50.
           INITIALIZE NJZAI-R.
           MOVE JCON3-02 TO NJZAI-01.
           MOVE W-RHCD(CNT) TO NJZAI-02.
           MOVE W-S TO NJZAI-03.
           PERFORM S-20 THRU S-30.
      *           WRITE NJZAI-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  WRITE ｴﾗｰ  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-55.
           MOVE JCON3-02 TO NJZAI-01.
           MOVE W-FHCD(CNT) TO NJZAI-02.
           MOVE W-S TO NJZAI-03.
      *           READ NJZAI INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  NJZAI ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *           DELETE NJZAI INVALID KEY
      *///////////////
           CALL "DB_Delete" USING NJZAI_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  DELETE ｴﾗｰ  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-45.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE ZERO TO W-RD W-C.
       S-10.
           ADD 1 TO W-C.
           IF  W-C < 11
               MOVE NJZAI-0411(W-C) TO W-0411(W-C)
               MOVE NJZAI-0511(W-C) TO W-0511(W-C)
               MOVE NJZAI-0611(W-C) TO W-0611(W-C)
               MOVE NJZAI-0711(W-C) TO W-0711(W-C)
               MOVE NJZAI-0811(W-C) TO W-0811(W-C)
               MOVE NJZAI-0911(W-C) TO W-0911(W-C)
               MOVE NJZAI-1011(W-C) TO W-1011(W-C)
               MOVE NJZAI-1111(W-C) TO W-1111(W-C)
               GO TO S-10
           END-IF.
       S-15.
           EXIT.
       S-20.
           MOVE ZERO TO W-C.
       S-25.
           ADD 1 TO W-C.
           IF  W-C < 11
               ADD W-0411(W-C) TO NJZAI-0411(W-C)
               ADD W-0511(W-C) TO NJZAI-0511(W-C)
               ADD W-0611(W-C) TO NJZAI-0611(W-C)
               ADD W-0711(W-C) TO NJZAI-0711(W-C)
               ADD W-0811(W-C) TO NJZAI-0811(W-C)
               ADD W-0911(W-C) TO NJZAI-0911(W-C)
               ADD W-1011(W-C) TO NJZAI-1011(W-C)
               ADD W-1111(W-C) TO NJZAI-1111(W-C)
               GO TO S-25
           END-IF.
       S-30.
           EXIT.

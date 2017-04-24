       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSG310.
      **************************************************
      *****     期　日　別　支　払　手　形　帳     *****
      *****            ( FDL : FTG310 )            *****
      **************************************************
       AUTHOR. T-FUJII.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  F              PIC  X(035).
           02  H-NEN          PIC Z9.
           02  F              PIC  X(003).
           02  H-GET          PIC Z9.
           02  F              PIC  X(003).
           02  H-PEY          PIC Z9.
           02  F              PIC  X(038).
       01  HEAD2.
           02  H-15K          PIC  X(005).
           02  H-3K           PIC  X(008).
           02  F              PIC  X(001).
           02  H-BKM          PIC  N(004).
           02  H-BKN          PIC  N(008).
           02  F              PIC  X(036).
           02  H-MAN1         PIC Z9.
           02  F              PIC  X(003).
           02  H-MAN2         PIC Z9.
           02  F              PIC  X(003).
           02  H-MAN3         PIC Z9.
           02  F              PIC  X(003).
           02  H-4K           PIC  X(008).
           02  H-20K          PIC  X(005).
       01  W-P.
           02  P-15K          PIC  X(005).
           02  F              PIC  X(001).
           02  P-FRI1         PIC Z9.
           02  P-FRI2         PIC Z9.
           02  P-FRI3         PIC Z9.
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(010).
           02  P-KEY          PIC  9(004).
           02  P-KIN          PIC  Z(011).
           02  P-RKIN         PIC  Z(011).
           02  P-20K          PIC  X(005).
       01  W-DATA.
           02  W-HNGP.
             03  W-HNEN       PIC  9(002).
             03  W-HGP.
               04  W-HGET     PIC  9(002).
               04  W-HPEY     PIC  9(002).
           02  W-MNGP.
             03  W-MNEN       PIC  9(002).
             03  W-MGET       PIC  9(002).
             03  W-MPEY       PIC  9(002).
           02  W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GP         PIC  9(004).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-SNGP.
             03  W-SNEN       PIC  9(004).
             03  W-SNENL REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-SNGPL REDEFINES W-SNGP.
             03  F            PIC  9(002).
             03  W-SNGPS      PIC  9(006).
           02  W-FNGP.
             03  W-FNEN       PIC  9(002).
             03  W-FGET       PIC  9(002).
             03  W-FPEY       PIC  9(002).
           02  W-TKIN         PIC  9(010).
           02  W-RKIN         PIC  9(010).
           02  W-FRI          PIC  9(006).
           02  CHK            PIC  9(001).
           02  W-CK.
             03  W-15K        PIC  X(005) VALUE X"1A24212078".
             03  W-20K        PIC  X(005) VALUE X"1A24212474".
             03  W-3K         PIC  X(008) VALUE X"1A26212068222176".
             03  W-4K         PIC  X(008) VALUE X"1A26212068212078".
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LISM.
           COPY LIBANK.
           COPY LSSHIT.
           COPY LSPF.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　期日別　支払手形　明細表　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999-FTG310" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "336" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "48" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "48" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "48" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "48" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "48" "06C-MID" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           MOVE ZERO TO W-NGP W-HNGP.
           MOVE DATE-04R TO W-NGPS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           COMPUTE W-HNEN = W-NEN - DATE-YC1.
           MOVE W-GP TO W-HGP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO SHIT-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SHIT-F_PNAME1 " " BY REFERENCE SHIT-F_IDLST "0".
       M-10.
      *           READ SHIT-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SHIT-F_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SHIT-F_IDLST SHIT-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ST-SKC NOT = ZERO
               GO TO M-10
           END-IF
           MOVE ZERO TO W-SNGP.
           MOVE ST-MKD TO W-SNGPS.
           IF  W-SNEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-SNEN
           ELSE
               IF  W-SNEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-SNEN
               END-IF
           END-IF
           IF  W-NGP NOT < W-SNGP
               GO TO M-10
           END-IF
           MOVE SPACE TO HEAD1.
           MOVE W-HNEN TO H-NEN.
           MOVE W-HGET TO H-GET.
           MOVE W-HPEY TO H-PEY.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO CHK.
       M-15.
           MOVE ST-MKD TO W-MNGP.
           MOVE ZERO TO W-RKIN.
       M-30.
           MOVE SPACE TO HEAD2.
           MOVE W-15K TO H-15K.
           MOVE W-20K TO H-20K.
           MOVE W-3K TO H-3K.
           MOVE W-4K TO H-4K.
           MOVE W-MNEN TO H-MAN1.
           MOVE W-MGET TO H-MAN2.
           MOVE W-MPEY TO H-MAN3.
           MOVE "銀行名　" TO H-BKM.
           MOVE ST-BCD TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "ＢＡＮＫＭ　無し" TO B-BNA
           END-IF
           MOVE B-BNA TO H-BKN.
           IF  CHK = 5
               MOVE ZERO TO CHK
               PERFORM S-05 THRU S-15
           ELSE
               PERFORM S-10 THRU S-15
           END-IF
           MOVE ZERO TO W-TKIN W-FRI.
       M-40.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           IF  ST-FDD NOT = W-FRI
               MOVE ST-FDD TO W-FRI W-FNGP
               MOVE W-FNEN TO P-FRI1
               MOVE W-FGET TO P-FRI2
               MOVE W-FPEY TO P-FRI3
           END-IF
           MOVE ST-TCD TO P-TCD.
           MOVE ST-TCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　　仕入先　無し　　＊＊" TO S-NAME
           END-IF
           MOVE S-NAME TO P-TNA.
           MOVE ST-KEY TO P-KEY.
           MOVE ST-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               MOVE W-FNEN TO P-FRI1
               MOVE W-FGET TO P-FRI2
               MOVE W-FPEY TO P-FRI3
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD ST-KIN TO W-TKIN W-RKIN.
       M-45.
      *           READ SHIT-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SHIT-F_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  ST-SKC NOT = ZERO
               GO TO M-45
           END-IF
           MOVE ZERO TO W-SNGP.
           MOVE ST-MKD TO W-SNGPS.
           IF  W-SNEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-SNEN
           ELSE
               IF  W-SNEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-SNEN
               END-IF
           END-IF
           IF  W-NGP NOT < W-SNGP
               GO TO M-45
           END-IF
           IF  ST-MKD NOT = W-MNGP
               GO TO M-55
           END-IF
           IF  B-KEY = ST-BCD
               GO TO M-40
           END-IF.
       M-50.
           PERFORM S-20 THRU S-25.
           MOVE 5 TO CHK.
           GO TO M-30.
       M-55.
           PERFORM S-30 THRU S-35.
           MOVE 5 TO CHK.
           GO TO M-15.
       M-90.
           PERFORM S-30 THRU S-35.
       M-95.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SHIT-F_IDLST SHIT-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE "　　　　　　　　　　　　【　ＴＯＴＡＬ　】" TO P-TNA.
           MOVE W-TKIN TO P-KIN.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           EXIT.
       S-30.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE "　　　　　　　　　　　　【　ＴＯＴＡＬ　】" TO P-TNA.
           MOVE W-TKIN TO P-KIN.
           MOVE W-RKIN TO P-RKIN.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-35.
           EXIT.

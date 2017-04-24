       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSG120.
      **********************************************
      *****     支　払　手　形　明　細　表     *****
      *****          ( FDL : FTG110 )          *****
      **********************************************
       AUTHOR. S-NAKAO.
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
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  X(021) VALUE SPACE.
           02  F              PIC  N(014) VALUE
                 "支　払　手　形　明　細　表　".
           02  F              PIC  X(022) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
       01  HEAD2.
           02  F              PIC  X(063).
           02  H-NEN          PIC Z9.
           02  F              PIC  X(003).
           02  H-GET          PIC Z9.
           02  F              PIC  X(003).
           02  H-PEY          PIC Z9.
           02  F              PIC  X(010).
           02  H-PAGE         PIC Z9.
           02  F              PIC  X(001).
       01  W-P.
           02  P-15K          PIC  X(005).
           02  F              PIC  X(001).
           02  P-FRI.
             03  P-FRI1       PIC Z9.
             03  P-FRI2       PIC Z9.
             03  P-FRI3       PIC Z9.
           02  P-NO           PIC  X(004).
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  P-X1           PIC  X(001).
           02  P-NAME         PIC  N(026).
           02  P-X2           PIC  X(001).
           02  P-KBN          PIC  N(002).
           02  P-MAN.
             03  P-MAN1       PIC Z9.
             03  P-MAN2       PIC Z9.
             03  P-MAN3       PIC Z9.
           02  P-KIN          PIC  Z(010).
           02  P-BKN          PIC  N(008).
           02  P-20K          PIC  X(005).
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
           02  W-FRI          PIC  9(006).
           02  W-FRID  REDEFINES W-FRI.
             03  W-FRI1       PIC  9(002).
             03  W-FRI2       PIC  9(002).
             03  W-FRI3       PIC  9(002).
           02  W-MAN.
             03  W-MAN1       PIC  9(002).
             03  W-MAN2       PIC  9(002).
             03  W-MAN3       PIC  9(002).
           02  W-C            PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-WNG.
             03  W-WNEN       PIC  9(002).
             03  W-WGET       PIC  9(002).
           02  CNT            PIC  9(003).
           02  W-SKIN         PIC  9(011).
           02  W-AKIN         PIC  9(011).
           02  W-PAGE         PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LISM.
           COPY LSSHIT.
           COPY LIBANK.
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
           02  FILLER  PIC  N(020) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                 "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                 "＊＊＊　　　支払手形　明細表　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                 "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(030) VALUE
                "<   H.  年  月分　打ち出し   >".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-NEN   PIC  9(002).
             03  A-GET   PIC  9(002).
           02  A-DMM   PIC  9(001).
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999-FTG110" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "332" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "40" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "40" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "40" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "40" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "40" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "40" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "14" "15" "30" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "19" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "14" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "14" "21" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-WNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "14" "25" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-WGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "36" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           MOVE ZERO TO W-NGP.
           MOVE DATE-04R TO W-NGPS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           COMPUTE W-WNEN = W-NEN - DATE-YC1.
           MOVE W-WNEN TO H-NEN.
           MOVE W-GET TO H-GET.
           MOVE W-PEY TO H-PEY.
      *
           MOVE ZERO TO W-NGP.
           MOVE D-NTNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           COMPUTE W-WNEN = W-NEN - DATE-YC1.
           MOVE W-GET TO W-WGET.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-15.
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-WGET < 1 OR > 12
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
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO SHIT-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SHIT-F_PNAME1 " " BY REFERENCE SHIT-F_IDLST "0".
       M-30.
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
           IF  ST-FNG NOT = W-WNG
               GO TO M-30
           END-IF
           IF  ST-SKC = 90
               GO TO M-30
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO W-AKIN W-PAGE W-FRI.
           PERFORM S-10 THRU S-15.
       M-35.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME P-KBN P-BKN.
           IF  ST-FDD NOT = W-FRI
               MOVE ZERO TO W-SKIN CNT
               MOVE ST-FDD TO W-FRI
               MOVE W-FRI1 TO P-FRI1
               MOVE W-FRI2 TO P-FRI2
               MOVE W-FRI3 TO P-FRI3
           END-IF
           MOVE ST-KEY TO P-NO.
           MOVE ST-TCD TO P-TCD.
           MOVE ST-TCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  "　＊＊　　仕入先　無し　　＊＊　　" TO S-NAME
           END-IF
           MOVE S-NAME TO P-NAME.
           IF  ST-TSC = "20"
               MOVE  "小手" TO P-KBN
           END-IF
           IF  ST-TSC = "21"
               MOVE  "約手" TO P-KBN
           END-IF
           IF  ST-TSC = "22"
               MOVE  "為手" TO P-KBN
           END-IF
           MOVE ST-MKD TO W-MAN.
           MOVE W-MAN1 TO P-MAN1.
           MOVE W-MAN2 TO P-MAN2.
           MOVE W-MAN3 TO P-MAN3.
           MOVE ST-KIN TO P-KIN.
           MOVE ST-BCD TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  "ＢＡＮＫＭ　無し" TO B-BNA
               MOVE SPACE TO B-SNA
           END-IF
           MOVE B-BNA TO P-BKN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               MOVE W-FRI1 TO P-FRI1
               MOVE W-FRI2 TO P-FRI2
               MOVE W-FRI3 TO P-FRI3
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME P-KBN P-BKN.
           MOVE B-SNA TO P-BKN.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD ST-KIN TO W-SKIN W-AKIN.
           ADD 1 TO CNT.
       M-50.
      *           READ SHIT-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SHIT-F_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  ST-FNG NOT = W-WNG
               GO TO M-50
           END-IF
           IF  ST-SKC = 90
               GO TO M-50
           END-IF
           IF  ST-FDD NOT = W-FRI
               PERFORM S-20 THRU S-30
           END-IF
           GO TO M-35.
       M-90.
           PERFORM S-20 THRU S-30.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME P-KBN P-BKN.
           MOVE  "　　　　　　　　　【　総　合　計　】　" TO P-NAME.
           MOVE W-AKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SHIT-F_IDLST SHIT-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
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
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
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
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-NAME P-KBN P-BKN.
           IF  CNT = 1
               GO TO S-25
           END-IF
           MOVE  "　　　　　　　　　　　　　（　小　計　）" TO P-NAME.
           MOVE W-SKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               MOVE W-FRI1 TO P-FRI1
               MOVE W-FRI2 TO P-FRI2
               MOVE W-FRI3 TO P-FRI3
           END-IF.
       S-25.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-30.
           EXIT.

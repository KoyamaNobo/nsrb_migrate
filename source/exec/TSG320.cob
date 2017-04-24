       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSG320.
      **************************************************
      *****     支払手形　期日別　相手科目別表     *****
      *****            ( FDL : FTG320 )            *****
      **************************************************
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
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  N(011) VALUE
                 "支払手形　相手科目別表".
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  H-NEN          PIC Z9.
           02  F              PIC  N(001) VALUE  "年".
           02  H-GET          PIC Z9.
           02  F              PIC  N(001) VALUE  "月".
           02  H-PEY          PIC Z9.
           02  F              PIC  N(003) VALUE  "日現在".
       01  HEAD2.
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE  "期日".
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  N(003) VALUE  "総金額".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE  "　材　料".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE  "仕入商品".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE  "　設　備".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE  "外注工賃".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE  "製造経費".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE  "営業経費".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE  "　その他".
       01  W-P.
           02  F              PIC  X(001).
           02  P-D1.
             03  P-MAN1       PIC Z9.
             03  P-MAN2       PIC Z9.
             03  P-MAN3       PIC Z9.
             03  F            PIC  X(008).
           02  P-D2    REDEFINES P-D1.
             03  F            PIC  X(007).
             03  P-TM         PIC  N(003).
             03  F            PIC  X(001).
           02  P-D3    REDEFINES P-D1.
             03  F            PIC  X(001).
             03  P-F          PIC  X(001).
             03  P-TG         PIC  N(005).
             03  P-R          PIC  X(001).
             03  F            PIC  X(001).
           02  P-KIN          PIC  Z(010).
           02  P-S1           PIC  Z(009).
           02  P-S2           PIC  Z(009).
           02  P-S3           PIC  Z(009).
           02  P-S4           PIC  Z(009).
           02  P-S5           PIC  Z(009).
           02  P-S6           PIC  Z(009).
           02  P-S7           PIC  Z(009).
       01  W-D.
           02  W-KIN          PIC  9(010).
           02  W-S1           PIC  9(009).
           02  W-S2           PIC  9(009).
           02  W-S3           PIC  9(009).
           02  W-S4           PIC  9(009).
           02  W-S5           PIC  9(009).
           02  W-S6           PIC  9(009).
           02  W-S7           PIC  9(009).
       01  W-TD.
           02  WT-KIN         PIC  9(010).
           02  WT-S1          PIC  9(009).
           02  WT-S2          PIC  9(009).
           02  WT-S3          PIC  9(009).
           02  WT-S4          PIC  9(009).
           02  WT-S5          PIC  9(009).
           02  WT-S6          PIC  9(009).
           02  WT-S7          PIC  9(009).
       01  W-AD.
           02  WA-KIN         PIC  9(010).
           02  WA-S1          PIC  9(009).
           02  WA-S2          PIC  9(009).
           02  WA-S3          PIC  9(009).
           02  WA-S4          PIC  9(009).
           02  WA-S5          PIC  9(009).
           02  WA-S6          PIC  9(009).
           02  WA-S7          PIC  9(009).
       01  W-DATA.
           02  W-MAN          PIC  9(006).
           02  W-MAND  REDEFINES W-MAN.
             03  W-MNG        PIC  9(004).
             03  W-MNGD  REDEFINES W-MNG.
               04  W-MN       PIC  9(002).
               04  W-MG       PIC  9(002).
             03  W-MP         PIC  9(002).
           02  W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GP.
               04  W-GET      PIC  9(002).
               04  W-PEY      PIC  9(002).
           02  W-NGPD  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
             03  F            PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  CHK            PIC  9(001).
           02  W-WNGP.
             03  W-WNEN       PIC  9(002).
             03  W-WGP.
               04  W-WGET     PIC  9(002).
               04  W-WPEY     PIC  9(002).
           02  W-FNG.
             03  W-FNEN       PIC  9(002).
             03  W-FGET       PIC  9(002).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
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
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊　　　支払手形　相手科目別表　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(032) VALUE
                "当月振出し  平成  年  月分  作成".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC Z9 .
             03  FILLER  PIC Z9 .
       01  C-ACP.
           02  A-DMM   PIC  9(001).
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999-FTG320" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "376" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "46" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "17" "32" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "22" "22" "08C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "4" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NG" " " "12" "0" "4" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NG" "Z9" "12" "33" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-FNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NG" "Z9" "12" "37" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-FGET "2" "0" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "39" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           COPY LIBCPR.
           MOVE ZERO TO W-NGP W-WNGP W-FNG.
           MOVE DATE-04R TO W-NGPS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           COMPUTE W-WNEN = W-NEN - DATE-YC1.
           MOVE W-GP TO W-WGP.
           MOVE ZERO TO W-NGP.
           MOVE D-NTNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           COMPUTE W-FNEN = W-NEN - DATE-YC1.
           MOVE W-GET TO W-FGET.
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO SHIT-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SHIT-F_PNAME1 " " BY REFERENCE SHIT-F_IDLST "0".
           MOVE ZERO TO W-D.
       M-15.
      *           READ SHIT-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SHIT-F_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF
           IF  ST-SKC NOT = ZERO
               GO TO M-15
           END-IF
           IF (ST-SNF = W-NEN) AND (ST-FDG = W-GET)
               PERFORM S-55 THRU S-60
           END-IF
           GO TO M-15.
       M-20.
           CALL "DB_F_Close" USING
            BY REFERENCE SHIT-F_IDLST SHIT-F_PNAME1.
           CALL "PR_Open" RETURNING RESP.
           MOVE W-WNEN TO H-NEN.
           MOVE W-WGET TO H-GET.
           MOVE W-WPEY TO H-PEY.
           PERFORM S-10 THRU S-15.
      *
           MOVE SPACE TO W-P.
           MOVE "(" TO P-F.
           MOVE  "当月振出し" TO P-TG.
           MOVE ")" TO P-R.
           MOVE W-KIN TO P-KIN.
           MOVE W-S1 TO P-S1.
           MOVE W-S2 TO P-S2.
           MOVE W-S3 TO P-S3.
           MOVE W-S4 TO P-S4.
           MOVE W-S5 TO P-S5.
           MOVE W-S6 TO P-S6.
           MOVE W-S7 TO P-S7.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           CALL "DB_F_Open" USING
            "INPUT" SHIT-F_PNAME1 " " BY REFERENCE SHIT-F_IDLST "0".
       M-30.
      *           READ SHIT-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SHIT-F_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  ST-SKC NOT = ZERO
               GO TO M-30
           END-IF
           MOVE ZERO TO W-AD.
       M-35.
           MOVE ZERO TO W-TD CHK.
           MOVE ST-MKD TO W-MAN.
       M-40.
           MOVE ZERO TO W-D.
           MOVE ST-MKD TO W-MAN.
       M-45.
           PERFORM S-55 THRU S-60.
       M-50.
      *           READ SHIT-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SHIT-F_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  ST-SKC NOT = ZERO
               GO TO M-50
           END-IF
           IF  ST-MNG NOT = W-MNG
               GO TO M-60
           END-IF
           IF  ST-MKD = W-MAN
               GO TO M-45
           END-IF.
       M-55.
           PERFORM S-20 THRU S-25.
           GO TO M-40.
       M-60.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-40.
           GO TO M-35.
       M-90.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-40.
           PERFORM S-45 THRU S-50.
       M-95.
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
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-P.
           IF  CHK = ZERO
               MOVE W-MN TO P-MAN1
               MOVE W-MG TO P-MAN2
           END-IF
           MOVE W-MP TO P-MAN3.
           MOVE W-KIN TO P-KIN.
           MOVE W-S1 TO P-S1.
           MOVE W-S2 TO P-S2.
           MOVE W-S3 TO P-S3.
           MOVE W-S4 TO P-S4.
           MOVE W-S5 TO P-S5.
           MOVE W-S6 TO P-S6.
           MOVE W-S7 TO P-S7.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               MOVE W-MN TO P-MAN1
               MOVE W-MG TO P-MAN2
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD W-KIN TO WT-KIN.
           ADD W-S1 TO WT-S1.
           ADD W-S2 TO WT-S2.
           ADD W-S3 TO WT-S3.
           ADD W-S4 TO WT-S4.
           ADD W-S5 TO WT-S5.
           ADD W-S6 TO WT-S6.
           ADD W-S7 TO WT-S7.
           ADD 1 TO CHK.
       S-25.
           EXIT.
       S-30.
           IF  CHK < 2
               GO TO S-35
           END-IF
           MOVE SPACE TO W-P.
           MOVE  "　合計" TO P-TM.
           MOVE WT-KIN TO P-KIN.
           MOVE WT-S1 TO P-S1.
           MOVE WT-S2 TO P-S2.
           MOVE WT-S3 TO P-S3.
           MOVE WT-S4 TO P-S4.
           MOVE WT-S5 TO P-S5.
           MOVE WT-S6 TO P-S6.
           MOVE WT-S7 TO P-S7.
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
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-35.
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
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WT-KIN TO WA-KIN.
           ADD WT-S1 TO WA-S1.
           ADD WT-S2 TO WA-S2.
           ADD WT-S3 TO WA-S3.
           ADD WT-S4 TO WA-S4.
           ADD WT-S5 TO WA-S5.
           ADD WT-S6 TO WA-S6.
           ADD WT-S7 TO WA-S7.
       S-40.
           EXIT.
       S-45.
           MOVE SPACE TO W-P.
           MOVE  "総合計" TO P-TM.
           MOVE WA-KIN TO P-KIN.
           MOVE WA-S1 TO P-S1.
           MOVE WA-S2 TO P-S2.
           MOVE WA-S3 TO P-S3.
           MOVE WA-S4 TO P-S4.
           MOVE WA-S5 TO P-S5.
           MOVE WA-S6 TO P-S6.
           MOVE WA-S7 TO P-S7.
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
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-50.
           EXIT.
       S-55.
           ADD ST-KIN TO W-KIN.
           ADD ST-ZR TO W-S1.
           ADD ST-SS TO W-S2.
           ADD ST-SB TO W-S3.
           ADD ST-GC TO W-S4.
           ADD ST-SZ TO W-S5.
           ADD ST-EG TO W-S6.
           ADD ST-ST TO W-S7.
       S-60.
           EXIT.

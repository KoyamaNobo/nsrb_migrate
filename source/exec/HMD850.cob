       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD850.
       AUTHOR. T-FUJII.
       DATE-WRITTEN. 1974-07-17.
      *********************************************************
      *    PROGRAM         :  担当者別売上粗利集計表          *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *        変更　　　  :  96/07/08                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(038) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "＊＊＊　　担当者別　売上粗利集計表　　＊＊＊".
           02  F              PIC  X(008) VALUE SPACE.
           02  H-GET          PIC  Z(002).
           02  F              PIC  N(001) VALUE "月".
           02  F              PIC  X(002) VALUE " 1".
           02  F              PIC  N(001) VALUE "日".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(001) VALUE "～".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-PEY          PIC  Z(002).
           02  F              PIC  N(001) VALUE "日".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  X(009) VALUE "I-----   ".
           02  F              PIC  N(005) VALUE "カジュアル".
           02  F              PIC  X(014) VALUE "   -----I  :  ".
           02  F              PIC  X(011) VALUE "I--------  ".
           02  F              PIC  N(003) VALUE "ワーク".
           02  F              PIC  X(016) VALUE "  --------I  :  ".
           02  F              PIC  X(011) VALUE "I-------   ".
           02  F              PIC  N(003) VALUE "教　育".
           02  F              PIC  X(016) VALUE "   -------I  :  ".
           02  F              PIC  X(011) VALUE "I-------   ".
           02  F              PIC  N(003) VALUE "合　計".
           02  F              PIC  X(011) VALUE "   -------I".
       01  HEAD3.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "担当".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上粗利".
           02  F              PIC  X(003) VALUE "  :".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上粗利".
           02  F              PIC  X(003) VALUE "  :".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上粗利".
           02  F              PIC  X(003) VALUE "  :".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上粗利".
       01  W-P.
           02  P-TM           PIC  N(004).
           02  P-TMD   REDEFINES P-TM.
             03  F            PIC  X(002).
             03  P-TNC        PIC  9(002).
             03  F            PIC  X(004).
           02  P-CU           PIC ---,---,---,--9.
           02  F              PIC  X(001).
           02  P-CAR          PIC ----,---,--9.
           02  F              PIC  X(002).
           02  P-X1           PIC  X(001).
           02  F              PIC  X(002).
           02  P-WU           PIC ---,---,---,--9.
           02  F              PIC  X(001).
           02  P-WAR          PIC ----,---,--9.
           02  F              PIC  X(002).
           02  P-X2           PIC  X(001).
           02  F              PIC  X(002).
           02  P-KU           PIC ---,---,---,--9.
           02  F              PIC  X(001).
           02  P-KAR          PIC ----,---,--9.
           02  F              PIC  X(002).
           02  P-X3           PIC  X(001).
           02  F              PIC  X(002).
           02  P-GU           PIC ---,---,---,--9.
           02  F              PIC  X(001).
           02  P-GAR          PIC ----,---,--9.
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
             03  W-NGD.
               04  W-NEND     PIC  9(004).
               04  W-NENDL REDEFINES W-NEND.
                 05  W-NEND1  PIC  9(002).
                 05  W-NEND2  PIC  9(002).
               04  W-GETD     PIC  9(002).
             03  W-PEYD       PIC  9(002).
           02  W-NGPDL REDEFINES W-NGPD.
             03  F            PIC  9(002).
             03  W-NGPDS      PIC  9(006).
           02  W-TNC.
             03  W-TNC1       PIC  9(001).
             03  W-TNC2       PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-UKIN         PIC S9(008).
           02  W-GKIN         PIC S9(008).
           02  W-D.
             03  W-CU         PIC S9(010).
             03  W-CUG        PIC S9(010).
             03  W-CAR        PIC S9(009).
             03  W-WU         PIC S9(010).
             03  W-WUG        PIC S9(010).
             03  W-WAR        PIC S9(009).
             03  W-KU         PIC S9(010).
             03  W-KUG        PIC S9(010).
             03  W-KAR        PIC S9(009).
             03  W-GU         PIC S9(010).
             03  W-GUG        PIC S9(010).
             03  W-GAR        PIC S9(009).
       01  WS-D.
           02  WS-CU          PIC S9(010).
           02  WS-CAR         PIC S9(009).
           02  WS-WU          PIC S9(010).
           02  WS-WAR         PIC S9(009).
           02  WS-KU          PIC S9(010).
           02  WS-KAR         PIC S9(009).
           02  WS-GU          PIC S9(010).
           02  WS-GAR         PIC S9(009).
       01  WA-D.
           02  WA-CU          PIC S9(010).
           02  WA-CAR         PIC S9(009).
           02  WA-WU          PIC S9(010).
           02  WA-WAR         PIC S9(009).
           02  WA-KU          PIC S9(010).
           02  WA-KAR         PIC S9(009).
           02  WA-GU          PIC S9(010).
           02  WA-GAR         PIC S9(009).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LSSNTW.
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　担当者別　売上粗利集計表　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(036) VALUE
                "'  年   月 01日  ～  '  年   月   日".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-PEY   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NGP.
             03  01D-NGP  PIC  9(002).
             03  02D-NGP  PIC  9(002).
             03  03D-NGP  PIC  9(002).
             03  04D-NGP  PIC  9(002).
             03  05D-NGP  PIC  9(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "366" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "44" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "44" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "44" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "44" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "44" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "44" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "44" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "15" "13" "36" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "20" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "3" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PEY" "9" "15" "45" "2" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "37" "1" "A-PEY" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "10" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NGP" " " "15" "0" "10" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NGP" "9" "15" "14" "2" " " "D-NGP"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NGP" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NGP" "9" "15" "19" "2" "01D-NGP" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NGP" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-NGP" "9" "15" "35" "2" "02D-NGP" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NGP" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-NGP" "9" "15" "40" "2" "03D-NGP" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NGP" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-NGP" "9" "15" "45" "2" "04D-NGP" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "05D-NGP" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "27" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "27" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           MOVE ZERO TO W-NGP W-NGPD.
           MOVE D-NHNG TO W-NGS.
           MOVE D-HSD TO W-NGPDS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-NEND2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEND
           END-IF
           IF  W-NEND2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEND
           END-IF
           MOVE ZERO TO W-PEY.
           IF  W-NG = W-NGD
               MOVE W-PEYD TO W-PEY
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NGP" D-NGP "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-PEY "A-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-15
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SNTRF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SNTRF_PNAME1 " " BY REFERENCE SNTRF_IDLST "0".
       M-20.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-20
           END-IF
           MOVE ZERO TO W-GKIN.
           IF (SNTR-SNC = 0) OR (SNTR-DC NOT = 2)
               COMPUTE W-GKIN = SNTR-SU * SNTR-FT
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-GKIN = W-GKIN * -1
               COMPUTE W-UKIN = SNTR-KIN * -1
           ELSE
               MOVE SNTR-KIN TO W-UKIN
           END-IF
           IF  ZERO = W-UKIN AND W-GKIN
               GO TO M-20
           END-IF
           IF  SNTR-NGP > W-NGP
               GO TO M-20
           END-IF
      *
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           MOVE W-GET TO H-GET.
           MOVE W-PEY TO H-PEY.
           MOVE ZERO TO WA-D W-PAGE.
           PERFORM S-10 THRU S-15.
       M-25.
           MOVE SNTR-TNC1 TO W-TNC1.
           MOVE ZERO TO WS-D.
       M-30.
           MOVE SNTR-TNC2 TO W-TNC2.
           MOVE ZERO TO W-D.
       M-35.
           ADD W-UKIN TO W-GU.
           ADD W-GKIN TO W-GUG.
           IF  SNTR-BC3 = 10
               ADD W-UKIN TO W-CU
               ADD W-GKIN TO W-CUG
           ELSE
               IF  SNTR-BC3 = 20
                   ADD W-UKIN TO W-WU
                   ADD W-GKIN TO W-WUG
               ELSE
                   ADD W-UKIN TO W-KU
                   ADD W-GKIN TO W-KUG
               END-IF
           END-IF.
       M-40.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-40
           END-IF
           MOVE ZERO TO W-GKIN.
           IF (SNTR-SNC = 0) OR (SNTR-DC NOT = 2)
               COMPUTE W-GKIN = SNTR-SU * SNTR-FT
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-GKIN = W-GKIN * -1
               COMPUTE W-UKIN = SNTR-KIN * -1
           ELSE
               MOVE SNTR-KIN TO W-UKIN
           END-IF
           IF  ZERO = W-UKIN AND W-GKIN
               GO TO M-40
           END-IF
           IF  SNTR-NGP > W-NGP
               GO TO M-40
           END-IF
           IF  SNTR-TNC1 NOT = W-TNC1
               GO TO M-45
           END-IF
           IF  SNTR-TNC2 = W-TNC2
               GO TO M-35
           END-IF
      *
           PERFORM S-20 THRU S-25.
           GO TO M-30.
       M-45.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-35.
           GO TO M-25.
       M-90.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           IF  ZERO = W-GU AND W-GUG AND W-CU AND W-CUG AND
                     W-WU AND W-WUG AND W-KU AND W-KUG
               GO TO S-25
           END-IF
           COMPUTE W-CAR = W-CU - W-CUG.
           COMPUTE W-WAR = W-WU - W-WUG.
           COMPUTE W-KAR = W-KU - W-KUG.
           COMPUTE W-GAR = W-GU - W-GUG.
      *
           MOVE SPACE TO W-P.
           MOVE ":" TO P-X1 P-X2 P-X3.
           MOVE W-TNC TO P-TNC.
           IF (W-CU NOT = ZERO) OR (W-CUG NOT = ZERO)
               MOVE W-CU TO P-CU
               MOVE W-CAR TO P-CAR
           END-IF
           IF (W-WU NOT = ZERO) OR (W-WUG NOT = ZERO)
               MOVE W-WU TO P-WU
               MOVE W-WAR TO P-WAR
           END-IF
           IF (W-KU NOT = ZERO) OR (W-KUG NOT = ZERO)
               MOVE W-KU TO P-KU
               MOVE W-KAR TO P-KAR
           END-IF
           MOVE W-GU TO P-GU.
           MOVE W-GAR TO P-GAR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-CU TO WS-CU.
           ADD W-CAR TO WS-CAR.
           ADD W-WU TO WS-WU.
           ADD W-WAR TO WS-WAR.
           ADD W-KU TO WS-KU.
           ADD W-KAR TO WS-KAR.
           ADD W-GU TO WS-GU.
           ADD W-GAR TO WS-GAR.
       S-25.
           EXIT.
       S-30.
           MOVE SPACE TO W-P.
           MOVE ":" TO P-X1 P-X2 P-X3.
           MOVE "［小計］" TO P-TM.
           IF (WS-CU NOT = ZERO)
               MOVE WS-CU TO P-CU
               MOVE WS-CAR TO P-CAR
           END-IF
           IF (WS-WU NOT = ZERO)
               MOVE WS-WU TO P-WU
               MOVE WS-WAR TO P-WAR
           END-IF
           IF (WS-KU NOT = ZERO)
               MOVE WS-KU TO P-KU
               MOVE WS-KAR TO P-KAR
           END-IF
           MOVE WS-GU TO P-GU.
           MOVE WS-GAR TO P-GAR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-P.
           MOVE ":" TO P-X1 P-X2 P-X3.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WS-CU TO WA-CU.
           ADD WS-CAR TO WA-CAR.
           ADD WS-WU TO  WA-WU.
           ADD WS-WAR TO WA-WAR.
           ADD WS-KU TO WA-KU.
           ADD WS-KAR TO WA-KAR.
           ADD WS-GU TO WA-GU.
           ADD WS-GAR TO WA-GAR.
       S-35.
           EXIT.
       S-40.
           MOVE SPACE TO W-P.
           MOVE ":" TO P-X1 P-X2 P-X3.
           MOVE "【合計】" TO P-TM.
           MOVE WA-CU TO P-CU.
           MOVE WA-CAR TO P-CAR.
           MOVE WA-WU TO P-WU.
           MOVE WA-WAR TO P-WAR.
           MOVE WA-KU TO P-KU.
           MOVE WA-KAR TO P-KAR.
           MOVE WA-GU TO P-GU.
           MOVE WA-GAR TO P-GAR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-45.
           EXIT.

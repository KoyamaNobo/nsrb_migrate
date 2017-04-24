       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HMY450.
       AUTHOR.        S-NAKAO.
      *********************************************************
      *    PROGRAM         :  得意先別　月別　販売実績表      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/20                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
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
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-SN           PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-SG           PIC Z9.
           02  F              PIC  X(006) VALUE "月 ～ ".
           02  H-EN           PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-EG           PIC Z9.
           02  F              PIC  N(018) VALUE
                "月　得意先月別　販売実績表　　＊＊＊".
           02  F              PIC  X(020) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  H-ND.
             03  H-X1         PIC  X(001).
             03  H-TCD        PIC  9(004).
             03  H-X2         PIC  X(001).
             03  F            PIC  X(001).
             03  H-NAME       PIC  N(026).
             03  F            PIC  X(001).
           02  H-TD    REDEFINES H-ND.
             03  H-TM         PIC  N(009).
             03  F            PIC  X(042).
           02  F              PIC  X(076).
       01  HEAD3.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(019) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　年　月".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  N(008) VALUE "　売上消費税金額".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上原価".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　売上粗利益".
           02  F              PIC  N(004) VALUE "　利益率".
           02  F              PIC  X(001) VALUE "%".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(006) VALUE "売上回収金額".
           02  F              PIC  N(008) VALUE "　消費税回収金額".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(006) VALUE "売上売掛残高".
           02  F              PIC  N(008) VALUE "　消費税売掛残高".
           02  F              PIC  X(005) VALUE X"1A24212474".
       01  W-P.
           02  F              PIC  X(018).
           02  P-NGD.
             03  F            PIC  X(002).
             03  P-NG         PIC 99/99.
             03  F            PIC  X(001).
           02  P-TM    REDEFINES P-NGD   PIC  N(004).
           02  P-UA           PIC --,---,---,--9.
           02  P-UAZ          PIC ----,---,--9.
           02  P-UG           PIC --,---,---,--9.
           02  P-AR           PIC -----,---,--9.
           02  P-RR           PIC ----9.9.
           02  P-ND.
             03  P-NK         PIC --,---,---,--9.
             03  P-NKZ        PIC ----,---,--9.
           02  P-ZK    REDEFINES P-ND.
             03  P-ZM         PIC  X(018).
             03  F            PIC  X(008).
           02  P-UZ           PIC ----,---,--9.
           02  P-UZZ          PIC ----,---,--9.
       01  W-DATA.
           02  W-TCD          PIC  9(004).
           02  W-UA           PIC S9(010).
           02  W-UAZ          PIC S9(009).
           02  W-AR           PIC S9(010).
           02  W-RR           PIC -999V9.
           02  W-DC           PIC  9(002).
           02  W-PC           PIC  9(002).
           02  W-PCD          PIC  9(002).
           02  CNT            PIC  9(002).
           02  CHK            PIC  9(002).
           02  W-ZC           PIC  9(001).
           02  W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SND   REDEFINES W-SNEN.
               04  W-SN1      PIC  9(002).
               04  W-SN2      PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-ENG.
             03  W-ENEN       PIC  9(004).
             03  W-END   REDEFINES W-ENEN.
               04  W-EN1      PIC  9(002).
               04  W-EN2      PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-ENGL  REDEFINES W-ENG.
             03  F            PIC  9(002).
             03  W-ENGS       PIC  9(004).
           02  W-NG           PIC  9(006).
           02  W-NGD   REDEFINES W-NG.
             03  W-NEN        PIC  9(004).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-GC           PIC  9(002).
           02  W-GCD          PIC  9(002).
           02  W-PAGE         PIC  9(003) VALUE ZERO.
           02  W-STCD         PIC  9(004).
           02  W-ETCD         PIC  9(004) VALUE 9999.
           02  W-DMM          PIC  9(001).
       01  W-TD.
           02  WT-UA          PIC S9(010).
           02  WT-UAZ         PIC S9(009).
           02  WT-UG          PIC S9(010).
           02  WT-NK          PIC S9(010).
           02  WT-NKZ         PIC S9(009).
       01  W-RDA.
           02  W-RD    OCCURS  12.
             03  WR-DC        PIC  9(001).
             03  WR-ZZ        PIC S9(009).
             03  WR-ZZZ       PIC S9(008).
             03  WR-UA        PIC S9(010).
             03  WR-UAZ       PIC S9(009).
             03  WR-UG        PIC S9(010).
             03  WR-NK        PIC S9(010).
             03  WR-NKZ       PIC S9(009).
             03  WR-UZ        PIC S9(009).
             03  WR-UZZ       PIC S9(008).
       01  W-ATA.
           02  W-AT    OCCURS  12.
             03  WA-DC        PIC  9(001).
             03  WA-ZZ        PIC S9(009).
             03  WA-ZZZ       PIC S9(008).
             03  WA-UA        PIC S9(010).
             03  WA-UAZ       PIC S9(009).
             03  WA-UG        PIC S9(010).
             03  WA-NK        PIC S9(010).
             03  WA-NKZ       PIC S9(009).
             03  WA-UZ        PIC S9(009).
             03  WA-UZZ       PIC S9(008).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LITM.
           COPY LSPF.
       01  TM-YF_HMY450.
           02  TM-YF_PNAME1  PIC  X(009)  VALUE SPACE.
           02  F             PIC  X(001).
           02  TM-YF_LNAME   PIC  X(012)  VALUE "TM-YF_HMY450".
           02  F             PIC  X(001).
           02  TM-YF_KEY1    PIC  X(100)  VALUE SPACE.
           02  TM-YF_KEY2    PIC  X(100)  VALUE SPACE.
           02  TM-YF_SORT    PIC  X(100)  VALUE SPACE.
           02  TM-YF_IDLST   PIC  X(100)  VALUE SPACE.
           02  TM-YF_RES     USAGE  POINTER.
       01  TM-R.
           02  TM-TCD         PIC  9(004).
           02  TM-ZZ          PIC S9(009).
           02  TM-ZZZ         PIC S9(007).
           02  TM-UZ          PIC S9(009).
           02  TM-UZZ         PIC S9(007).
           02  TM-UA          PIC S9(009).
           02  TM-UAZ         PIC S9(007).
           02  TM-UN          PIC S9(008).
           02  TM-UNZ         PIC S9(006).
           02  TM-NK          PIC S9(009).
           02  TM-NKZ         PIC S9(007).
           02  TM-UG          PIC S9(009).
           02  F              PIC  X(031).
           02  TM-NG.
             03  TM-NEN       PIC  9(004).
             03  TM-GET       PIC  9(002).
       77  F                  PIC  X(001).
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
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　得意先月別　販売実績表　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(024) VALUE
                  "'  年  月 より '  年  月".
           02  FILLER  PIC  X(018) VALUE
                  "ｺｰﾄﾞ  0000 ～ 9999".
           02  FILLER  PIC  X(022) VALUE
                  "確認　OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
               03  A-STCD    PIC  9(004).
               03  A-ETCD    PIC  9(004).
           02  A-DMM    PIC  9(001).
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC 99.
             03  FILLER  PIC Z9.
             03  FILLER  PIC 99.
             03  FILLER  PIC Z9.
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC X(18) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME2     PIC X(17) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98    PIC X(5)  VALUE X"1B4A05".
             03  E-ME99    PIC X(5)  VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
       CALL "SD_Init" USING
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "358" " " " " RETURNING RESU.
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
            "08C-MID" "X" "14" "22" "24" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "16" "25" "18" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "20" "23" "22" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "16" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
            "A-STCD" "9" "16" "31" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ETCD" "9" "16" "39" "4" "A-STCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "40" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "14" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "99" "14" "23" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-SN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "Z9" "14" "27" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NG" "99" "14" "38" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE W-EN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NG" "Z9" "14" "42" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NG" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "45" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "45" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO W-SNG W-ENG.
           MOVE D-SPNG TO W-SNGS.
           MOVE D-EPNG TO W-ENGS.
           IF  W-SN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF.
           IF  W-SN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
           IF  W-EN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN.
           IF  W-EN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" 
                                              RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
           IF  W-STCD > W-ETCD
               GO TO M-15
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF.
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-30
           END-IF.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO TM-YF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TM-YF_PNAME1 " " BY REFERENCE TM-YF_IDLST "0".
       M-35.
      *           READ TM-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TM-YF_PNAME1 BY REFERENCE TM-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_F_Close" USING
                BY REFERENCE TM-YF_IDLST TM-YF_PNAME1
               GO TO M-95
           END-IF.
           IF  TM-NG < W-SNG OR > W-ENG
               GO TO M-35
           END-IF.
           IF  TM-TCD < W-STCD OR > W-ETCD
               GO TO M-35
           END-IF.
           COMPUTE W-GCD = W-SGET - 1.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE W-SN2 TO H-SN.
           MOVE W-SGET TO H-SG.
           MOVE W-EN2 TO H-EN.
           MOVE W-EGET TO H-EG.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO W-ATA.
       M-40.
           MOVE TM-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　得意先マスター　無し　＊＊"  TO T-NAME
           END-IF.
           MOVE ZERO TO W-RDA W-ZC W-DC.
       M-45.
           COMPUTE W-UA = TM-UA - TM-UN.
           COMPUTE W-UAZ = TM-UAZ - TM-UNZ.
           IF  ZERO NOT = TM-ZZ OR TM-ZZZ OR TM-UZ OR TM-UZZ
                      OR W-UA OR W-UAZ OR TM-NK OR TM-NKZ OR TM-UG
               MOVE 5 TO W-ZC
           END-IF.
           MOVE TM-GET TO W-GC.
           IF  W-GCD >=  W-GC
               ADD 12 TO W-GC
           END-IF.
           SUBTRACT W-GCD FROM W-GC.
           IF  W-GC < 1 OR > 12
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                              RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                              RETURNING RESU
               GO TO M-90
           END-IF.
           ADD 1 TO W-DC.
           MOVE ZERO TO W-RD(W-GC).
           MOVE 1 TO WR-DC(W-GC).
           MOVE TM-ZZ TO WR-ZZ(W-GC).
           MOVE TM-ZZZ TO WR-ZZZ(W-GC).
           MOVE TM-UZ TO WR-UZ(W-GC).
           MOVE TM-UZZ TO WR-UZZ(W-GC).
           MOVE W-UA TO WR-UA(W-GC).
           MOVE W-UAZ TO WR-UAZ(W-GC).
           MOVE TM-NK TO WR-NK(W-GC).
           MOVE TM-NKZ TO WR-NKZ(W-GC).
           MOVE TM-UG TO WR-UG(W-GC).
       M-50.
      *           READ TM-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TM-YF_PNAME1 BY REFERENCE TM-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF.
           IF  TM-NG < W-SNG OR > W-ENG
               GO TO M-50
           END-IF.
           IF  TM-TCD < W-STCD OR > W-ETCD
               GO TO M-50
           END-IF.
           IF  W-TCD = TM-TCD
               GO TO M-45
           END-IF.
           IF  W-ZC NOT = ZERO
               PERFORM S-20 THRU S-60
           END-IF.
           GO TO M-40.
       M-85.
           IF  W-ZC NOT = ZERO
               PERFORM S-20 THRU S-60
           END-IF.
           MOVE W-ATA TO W-RDA.
           PERFORM S-20 THRU S-60.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TM-YF_IDLST TM-YF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE 2 TO W-PC.
       S-15.
           EXIT.
       S-20.
           MOVE ZERO TO CNT CHK W-TD.
           MOVE W-SNG TO W-NG.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEN
           END-IF.
       S-25.
           ADD 1 TO CNT W-GET.
           IF  CNT = 13
               GO TO S-45
           END-IF.
           IF  W-GET = 13
               MOVE 1 TO W-GET
               ADD 1 TO W-NEN
           END-IF.
           IF  W-NG < W-SNG OR > W-ENG
               GO TO S-25
           END-IF.
           IF  WR-DC(CNT) = ZERO
               GO TO S-25
           END-IF.
           IF  CHK NOT = ZERO
               GO TO S-30
           END-IF.
           MOVE 5 TO CHK.
           IF  W-PC > 58
               PERFORM S-05 THRU S-15
           END-IF.
           COMPUTE W-PCD = 58 - W-PC.
           IF  W-DC > W-PCD
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO HEAD2.
           IF  WR-DC(CNT) = 1
               MOVE "(" TO H-X1
               MOVE ")" TO H-X2
               MOVE W-TCD TO H-TCD
               MOVE T-NAME TO H-NAME
           END-IF.
           IF  WR-DC(CNT) = 3
               MOVE "【　総　合　計　】" TO H-TM
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD 3 TO W-PC.
           IF  W-SNG NOT = W-NG
               GO TO S-30
           END-IF.
           IF  ZERO = WR-ZZ(CNT) AND WR-ZZZ(CNT)
               GO TO S-30
           END-IF.
           MOVE SPACE TO W-P.
           MOVE "---  前期繰越  ---" TO P-ZM.
           MOVE WR-ZZ(CNT) TO P-UZ.
           MOVE WR-ZZZ(CNT) TO P-UZZ.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD 1 TO W-PC.
       S-30.
           COMPUTE W-AR = WR-UA(CNT) - WR-UG(CNT).
           MOVE ZERO TO W-RR.
           IF  W-AR = ZERO
               GO TO S-40
           END-IF.
           IF  WR-UA(CNT) = ZERO
               GO TO S-35
           END-IF.
           IF  W-AR > ZERO
               IF  WR-UA(CNT) < ZERO
                   GO TO S-35
               END-IF
           END-IF.
           IF  WR-UA(CNT) > ZERO
               IF  W-AR < ZERO
                   GO TO S-35
               END-IF
           END-IF.
           IF  ZERO > WR-UA(CNT) AND W-AR
               COMPUTE W-RR ROUNDED = (W-AR * -100) / WR-UA(CNT)
           ELSE
               COMPUTE W-RR ROUNDED = (W-AR * 100) / WR-UA(CNT)
           END-IF.
           GO TO S-40.
       S-35.
           IF  W-AR > ZERO
               MOVE 100 TO W-RR
           ELSE
               MOVE -100 TO W-RR
           END-IF.
       S-40.
           MOVE SPACE TO W-P.
           MOVE W-NGS TO P-NG.
           MOVE WR-UA(CNT) TO P-UA.
           MOVE WR-UAZ(CNT) TO P-UAZ.
           MOVE WR-UG(CNT) TO P-UG.
           MOVE W-AR TO P-AR.
           MOVE W-RR TO P-RR.
           MOVE WR-NK(CNT) TO P-NK.
           MOVE WR-NKZ(CNT) TO P-NKZ.
           MOVE WR-UZ(CNT) TO P-UZ.
           MOVE WR-UZZ(CNT) TO P-UZZ.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD 1 TO W-PC.
           ADD WR-UA(CNT) TO WT-UA.
           ADD WR-UAZ(CNT) TO WT-UAZ.
           ADD WR-NK(CNT) TO WT-NK.
           ADD WR-NKZ(CNT) TO WT-NKZ.
           ADD WR-UG(CNT) TO WT-UG.
           IF  WR-DC(CNT) = 1
               MOVE 3 TO WA-DC(CNT)
               ADD WR-ZZ(CNT) TO WA-ZZ(CNT)
               ADD WR-ZZZ(CNT) TO WA-ZZZ(CNT)
               ADD WR-UA(CNT) TO WA-UA(CNT)
               ADD WR-UAZ(CNT) TO WA-UAZ(CNT)
               ADD WR-NK(CNT) TO WA-NK(CNT)
               ADD WR-NKZ(CNT) TO WA-NKZ(CNT)
               ADD WR-UZ(CNT) TO WA-UZ(CNT)
               ADD WR-UZZ(CNT) TO WA-UZZ(CNT)
               ADD WR-UG(CNT) TO WA-UG(CNT)
           END-IF.
           GO TO S-25.
       S-45.
           COMPUTE W-AR = WT-UA - WT-UG.
           MOVE ZERO TO W-RR.
           IF  W-AR = ZERO
               GO TO S-55
           END-IF.
           IF  WT-UA = ZERO
               GO TO S-50
           END-IF.
           IF  W-AR > ZERO
               IF  WT-UA < ZERO
                   GO TO S-50
               END-IF
           END-IF.
           IF  WT-UA > ZERO
               IF  W-AR < ZERO
                   GO TO S-50
               END-IF
           END-IF.
           IF  ZERO > WT-UA AND W-AR
               COMPUTE W-RR ROUNDED = (W-AR * -100) / WT-UA
           ELSE
               COMPUTE W-RR ROUNDED = (W-AR * 100) / WT-UA
           END-IF.
           GO TO S-55.
       S-50.
           IF  W-AR > ZERO
               MOVE 100 TO W-RR
           ELSE
               MOVE -100 TO W-RR
           END-IF.
       S-55.
           MOVE SPACE TO W-P.
           MOVE "［合計］" TO P-TM.
           MOVE WT-UA TO P-UA.
           MOVE WT-UAZ TO P-UAZ.
           MOVE WT-UG TO P-UG.
           MOVE W-AR TO P-AR.
           MOVE W-RR TO P-RR.
           MOVE WT-NK TO P-NK.
           MOVE WT-NKZ TO P-NKZ.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD 2 TO W-PC.
       S-60.
           EXIT.

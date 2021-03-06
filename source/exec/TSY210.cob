       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSY210.
      ****************************************************
      *****     x@₯@θ@`@ @iζψζΚj     *****
      *****         ( FDL : FTY210,FTY220 )          *****
      ****************************************************
       AUTHOR. S-NAKAO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  PRN999             PIC  X(006) VALUE "PRN999".
       77  FTY220             PIC  X(006) VALUE "FTY220".
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0128".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0064".
           02  W-FID22        PIC  X(003).
       01  HEAD1.
           02  H-20K          PIC  X(005).
           02  F              PIC  X(035).
           02  H-NEN          PIC Z9.
           02  F              PIC  X(003).
           02  H-GET          PIC Z9.
           02  F              PIC  X(003).
           02  H-PEY          PIC Z9.
           02  F              PIC  X(031).
           02  H-NOM          PIC  N(001).
           02  F              PIC  X(001).
           02  H-NO           PIC  9(003).
           02  F              PIC  X(001).
       01  HEAD2.
           02  H-15K          PIC  X(005).
           02  F              PIC  X(008).
           02  H-TCD          PIC  9(004).
           02  F              PIC  X(010).
           02  H-ADR          PIC  N(008).
           02  F              PIC  X(012).
           02  H-NAME         PIC  N(024).
           02  F              PIC  X(004).
       01  W-P1.
           02  P-15K1         PIC  X(005).
           02  F              PIC  X(001).
           02  P-UTD.
             03  P-UNEN       PIC Z9.
             03  P-UGET       PIC Z9.
             03  P-UPEY       PIC Z9.
           02  P-KEY          PIC  9(004).
           02  P-KBN          PIC  N(002).
           02  P-MKD.
             03  P-MNEN       PIC Z9.
             03  P-MGET       PIC Z9.
             03  P-MPEY       PIC Z9.
           02  P-KIN          PIC ZZZZZZZZZZ9.
           02  P-OK           PIC  X(001).
           02  P-BKD1.
             03  P-BKN1       PIC  N(008).
             03  P-BCD        PIC  9(004).
             03  P-F1    REDEFINES P-BCD  PIC  X(004).
             03  P-F2         PIC  X(001).
           02  P-BKD2   REDEFINES P-BKD1.
             03  P-F3         PIC  X(004).
             03  P-BKN2       PIC  N(008).
             03  P-F4         PIC  X(001).
           02  F              PIC  X(039).
           02  P-20K1         PIC  X(005).
       01  W-P2.
           02  P-15K2         PIC  X(005).
           02  F              PIC  X(020).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(003).
           02  P-GKIN         PIC ZZZZ,ZZZ,ZZZ,ZZ9.
           02  F              PIC  X(003).
           02  P-NO           PIC  9(003).
           02  F              PIC  X(001).
           02  P-20K2         PIC  X(005).
       01  W-DATA.
           02  W-TCDD.
             03  W-STCD       PIC  9(004).
             03  W-ETCD       PIC  9(004) VALUE 9999.
           02  W-DMM          PIC  9(001).
           02  W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-TCD          PIC  9(004).
           02  W-GKIN         PIC  9(011).
           02  CHK            PIC  9(001).
           02  W-NO           PIC  9(003).
           02  W-JUP          PIC  N(024).
           02  W-JUPD  REDEFINES W-JUP.
             03  ADR1  OCCURS  8  PIC  N(001).
             03  F            PIC  N(016).
           02  W-ADR          PIC  N(008).
           02  W-ADRD  REDEFINES W-ADR.
             03  ADR2  OCCURS  8  PIC  N(001).
           02  CNT            PIC  9(002).
       01  W-CK.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LISM.
           COPY LIBANK.
           COPY LSSHIT.
           COPY LSPF.
      *FD  SI-F
       01  SI-F_TSY210.
           02  SI-F_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SI-F_LNAME     PIC  X(011) VALUE "SI-F_TSY210".
           02  F              PIC  X(001).
           02  SI-F_KEY1      PIC  X(100) VALUE SPACE.
           02  SI-F_SORT      PIC  X(100) VALUE SPACE.
           02  SI-F_IDLST     PIC  X(100) VALUE SPACE.
           02  SI-F_RES       USAGE  POINTER.
       01  SI-R.
           02  SI-TCD         PIC  9(004).
           02  SI-KIN         PIC  9(011).
           02  SI-NO          PIC  9(003).
           02  F              PIC  X(046).
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
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(024) VALUE
                  "".
           02  FILLER  PIC  N(024) VALUE
                  "".
           02  FILLER  PIC  N(024) VALUE
                  "@@@@@@@@@@@@@@@@".
           02  FILLER  PIC  N(022) VALUE
                  "@@@dόζΚ@x₯θ` @@@".
           02  FILLER  PIC  N(024) VALUE
                  "@@@@@@@@@@@@@@@@".
           02  FILLER  PIC  N(024) VALUE
                  "".
           02  FILLER  PIC  N(024) VALUE
                  "".
           02  FILLER  PIC  X(034) VALUE
                "dόζΊ°Δή 0000 ζθ 9999 άΕμ\".
           02  FILLER  PIC  X(022) VALUE
                "mF  OK=1 NO=9   Ψΐ°έ".
       01  C-ACP.
           02  FILLER.
             03  A-STCD  PIC  9(004).
             03  A-ETCD  PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ΕΌ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999-FTY210" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "388" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "48" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "48" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "48" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "48" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "14" "16" "34" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "22" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "14" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STCD" "9" "14" "27" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ETCD" "9" "14" "37" "4" "A-STCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "39" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "27" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "27" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
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
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4"
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
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-STCD > W-ETCD
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0128ID.
           MOVE WK0128ID TO SHIT-F_PNAME1.
           MOVE W-FID2 TO WK0064ID.
           MOVE WK0064ID TO SI-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SHIT-F_PNAME1 " " BY REFERENCE SHIT-F_IDLST "0".
       M-30.
      *           READ SHIT-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SHIT-F_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SHIT-F_IDLST SHIT-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ST-TCD > W-ETCD
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SHIT-F_IDLST SHIT-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ST-TCD < W-STCD
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
           CALL "DB_F_Open" USING
            "OUTPUT" SI-F_PNAME1 " " BY REFERENCE SI-F_IDLST "0".
           MOVE ZERO TO W-NGP.
           MOVE DATE-04R TO W-NGPS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           SUBTRACT DATE-YC1 FROM W-NEN.
           MOVE SPACE TO HEAD1.
           MOVE W-20K TO H-20K.
           MOVE W-NEN2 TO H-NEN.
           MOVE W-GET TO H-GET.
           MOVE W-PEY TO H-PEY.
           MOVE   "" TO H-NOM.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO CHK W-NO.
       M-35.
           ADD 1 TO W-NO.
           MOVE W-NO TO H-NO.
           MOVE ST-TCD TO W-TCD.
           MOVE ZERO TO W-GKIN.
           MOVE ST-TCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   "@@@dόζ@³΅@@" TO S-NAME
               MOVE SPACE TO S-JSU
           END-IF
           MOVE SPACE TO HEAD2.
           MOVE W-15K TO H-15K.
           MOVE ST-TCD TO H-TCD.
           MOVE S-NAME TO H-NAME.
           MOVE ALL   "@" TO W-JUP.
           MOVE S-JSU TO W-JUP
           MOVE ALL   "@" TO W-ADR.
           MOVE ZERO TO CNT.
       M-40.
           ADD 1 TO CNT.
           IF  CNT NOT = 9
               IF  ADR1(CNT) NOT =   "@"
                   MOVE ADR1(CNT) TO ADR2(CNT)
                   GO TO M-40
               END-IF
           END-IF
           MOVE W-ADR TO H-ADR.
           IF  CHK = ZERO
               MOVE 5 TO CHK
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF.
       M-50.
           PERFORM S-20 THRU S-35.
       M-55.
      *           READ SHIT-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SHIT-F_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-60
           END-IF
           IF  ST-TCD > W-ETCD
               GO TO M-60
           END-IF
           IF  ST-SKC = 90
               GO TO M-55
           END-IF
           IF  W-TCD = ST-TCD
               GO TO M-50
           END-IF
           PERFORM S-40 THRU S-45.
           GO TO M-35.
       M-60.
           PERFORM S-40 THRU S-45.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SHIT-F_IDLST SHIT-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SI-F_IDLST SI-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Open" USING
            "INPUT" SI-F_PNAME1 " " BY REFERENCE SI-F_IDLST "0".
      *           READ SI-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SI-F_PNAME1 BY REFERENCE SI-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SI-F_IDLST SI-F_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE S-M_IDLST S-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
      *           CALL "CBLASGN" USING SP-F PRN999 FTY220
           CALL "PR_Open_Assign" USING "999-FTY220" RETURNING RET.
           CALL "PR_Open" RETURNING RESP.
           MOVE SPACE TO HEAD1.
           MOVE W-20K TO H-20K.
           MOVE W-NEN2 TO H-NEN.
           MOVE W-GET TO H-GET.
           MOVE W-PEY TO H-PEY.
           PERFORM S-55 THRU S-60.
           MOVE ZERO TO W-GKIN.
       M-65.
           MOVE SI-TCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   "@@@dόζ@³΅@@" TO S-NAME
           END-IF
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15K2.
           MOVE W-20K TO P-20K2.
           MOVE ALL   "@" TO P-NAME.
           MOVE SI-TCD TO P-TCD.
           MOVE S-NAME TO P-NAME.
           MOVE SI-KIN TO P-GKIN.
           MOVE SI-NO TO P-NO.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-50 THRU S-60
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD SI-KIN TO W-GKIN.
      *           READ SI-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SI-F_PNAME1 BY REFERENCE SI-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-70
           END-IF
           GO TO M-65.
       M-70.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-50 THRU S-60
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15K2.
           MOVE W-20K TO P-20K2.
           MOVE   "@@@@@@@@@@y@@@v@@z@@@@@"
                                                              TO P-NAME.
           MOVE W-GKIN TO P-GKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-50 THRU S-60
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SI-F_IDLST SI-F_PNAME1.
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
           MOVE SPACE TO W-P1.
           MOVE W-15K TO P-15K1.
           MOVE W-20K TO P-20K1.
           MOVE SPACE TO P-KBN P-BKN1 P-BKN2.
           MOVE ST-FDN TO P-UNEN.
           MOVE ST-FDG TO P-UGET.
           MOVE ST-FDP TO P-UPEY.
           MOVE ST-KEY TO P-KEY.
           IF  ST-TSC = 20
               MOVE   "¬θ" TO P-KBN
           END-IF
           IF  ST-TSC = 21
               MOVE   "ρθ" TO P-KBN
           END-IF
           IF  ST-TSC = 22
               MOVE   "Χθ" TO P-KBN
           END-IF
           MOVE ST-MKN TO P-MNEN.
           MOVE ST-MKG TO P-MGET.
           MOVE ST-MKP TO P-MPEY.
           MOVE ST-KIN TO P-KIN.
           MOVE ST-BCD TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   "a`mjl@³΅" TO B-BNA
           END-IF
           IF  ST-SKC = 00
               MOVE B-BNA TO P-BKN1
               MOVE SPACE TO P-F1 P-F2
               ADD ST-KIN TO W-GKIN
           END-IF
           IF  ST-SKC = 50
               MOVE "*" TO P-OK
               MOVE B-BNA TO P-BKN2
               MOVE SPACE TO P-F3 P-F4
           END-IF
           IF  ST-SKC = 80
               MOVE "-" TO P-OK
               MOVE B-BNA TO P-BKN2
               MOVE SPACE TO P-F3 P-F4
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-35.
           EXIT.
       S-40.
           MOVE SPACE TO W-P1.
           MOVE W-15K TO P-15K1.
           MOVE W-20K TO P-20K1.
           MOVE SPACE TO P-KBN P-BKN1 P-BKN2.
           MOVE   "@y@c@@z" TO P-BKN1.
           MOVE SPACE TO P-F1 P-F2.
           MOVE W-GKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO SI-R.
           MOVE W-TCD TO SI-TCD.
           MOVE W-GKIN TO SI-KIN.
           MOVE W-NO TO SI-NO.
      *           WRITE SI-R.
      *//////////////
           CALL "DB_Insert" USING
            SI-F_PNAME1 SI-F_LNAME SI-R RETURNING RET.
       S-45.
           EXIT.
       S-50.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-55.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "5" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-60.
           EXIT.

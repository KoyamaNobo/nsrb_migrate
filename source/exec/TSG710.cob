       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSG710.
      ********************************************
      *****     ������`�����݁@�U�֓`�[     *****
      *****         ( FDL = FTG710 )         *****
      ********************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  N(008) VALUE  "�@�U�@�ց@�`�@�[".
           02  F              PIC  X(033) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
       01  HEAD2.
           02  F              PIC  X(040) VALUE SPACE.
           02  F              PIC  N(002) VALUE  "����".
           02  F              PIC  X(002) VALUE SPACE.
           02  H-NEN          PIC  N(002).
           02  F              PIC  N(001) VALUE  "�N".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-GET          PIC  N(002).
           02  F              PIC  N(001) VALUE  "��".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-PEY          PIC  N(002).
           02  F              PIC  N(001) VALUE  "��".
           02  F              PIC  X(019) VALUE SPACE.
           02  F              PIC  N(001) VALUE  "��".
           02  F              PIC  X(007) VALUE SPACE.
           02  H-PAGE         PIC Z9.
           02  F              PIC  X(002) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(014) VALUE SPACE.
           02  F              PIC  N(005) VALUE  "�ȁ@�ځ@��".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(001) VALUE  "��".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(001) VALUE  "�z".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(001) VALUE  "�E".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(001) VALUE  "�v".
           02  F              PIC  X(011) VALUE SPACE.
           02  F              PIC  N(005) VALUE  "�ȁ@�ځ@��".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(001) VALUE  "��".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(001) VALUE  "�z".
           02  F              PIC  X(007) VALUE SPACE.
       01  HEAD9.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(043) VALUE SPACE.
           02  F              PIC  N(016) VALUE
                 "���@�i�@�S�@���@���@���@��@�Ё@".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  X(005) VALUE X"1A24212474".
       01  W-PD.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(092) VALUE SPACE.
           02  P-IM           PIC  N(004).
           02  F              PIC  X(005) VALUE X"1A24212474".
       01  W-P1.
           02  P1-15K         PIC  X(005).
           02  F              PIC  X(013).
           02  P-NA1          PIC  N(008).
           02  F              PIC  X(012).
           02  P-NA2          PIC  N(026).
           02  F              PIC  X(022).
           02  P1-20K         PIC  X(005).
       01  W-P2.
           02  P2-15K         PIC  X(005).
           02  F              PIC  X(013).
           02  P-KMK1         PIC  N(008).
           02  F              PIC  X(001).
           02  P-KIN1         PIC ZZZZZZZZZ9.
           02  F              PIC  X(001).
           02  P-D1           PIC  N(008).
           02  F              PIC  X(004).
           02  P-GET          PIC Z9.
           02  P-VER          PIC  X(001).
           02  P-PEY          PIC Z9.
           02  F              PIC  X(001).
           02  P-D2           PIC  N(004).
           02  F              PIC  X(004).
           02  P-KMK2         PIC  N(008).
           02  F              PIC  X(001).
           02  P-KIN2         PIC ZZZZZZZZZ9.
           02  F              PIC  X(006).
           02  P2-20K         PIC  X(005).
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
           02  W-NGPD.
             03  W-WNG.
               04  W-WNEN     PIC  9(002).
               04  W-WGET     PIC  9(002).
             03  W-PEYD       PIC  9(002).
           02  W-HNGP.
             03  W-HNEN       PIC Z9.
             03  W-HGET       PIC Z9.
             03  W-HPEY       PIC Z9.
           02  W-YBK          PIC  9(004).
           02  W-KIN          PIC  9(010).
           02  W-TCD          PIC  9(004).
           02  W-PAGE         PIC  9(002).
           02  W-TPC          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  CHK            PIC  9(001).
           02  CNT            PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIBANK.
           COPY LSUKET.
      *FD  SP-F
       77  SP-R               PIC  X(170).
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
                 "����������������������������������������������".
           02  FILLER  PIC  N(023) VALUE
                 "����������������������������������������������".
           02  FILLER  PIC  N(023) VALUE
                 "�������@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@������".
           02  FILLER  PIC  N(023) VALUE
                 "�������@�@�@�U�֓`�[�@�i���茈�ρj�@�@�@������".
           02  FILLER  PIC  N(023) VALUE
                 "�������@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@������".
           02  FILLER  PIC  N(023) VALUE
                 "����������������������������������������������".
           02  FILLER  PIC  N(023) VALUE
                 "����������������������������������������������".
           02  FILLER  PIC  X(022) VALUE
                "�y  H.  �N   �� ��  �z".
           02  FILLER  PIC  X(036) VALUE
                "[   ý� �����  ��=9 �Ų=1   ����   ]".
           02  FILLER  PIC  X(022) VALUE
                "�m�F  OK=1 NO=9   ����".
       01  C-ACP.
           02  A-TPC   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  BANKM ż  ***".
             03  E-ME2   PIC  X(015) VALUE
                  "***  TM ż  ***".
             03  E-YBK   PIC  9(004).
             03  E-TCD   PIC  9(004).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999-FTG710" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "402" " " " " RETURNING RESU.
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
            "08C-MID" "X" "12" "22" "22" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "14" "15" "36" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "20" "22" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TPC" "9" "14" "42" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TPC" BY REFERENCE W-TPC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "39" "1" "A-TPC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "4" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NG" " " "12" "0" "4" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NG" "9" "12" "28" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-WNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NG" "9" "12" "33" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-WGET "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "51" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "51" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "15" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-YBK" "9" "24" "40" "4" "E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-YBK" BY REFERENCE UT-SBC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TCD" "9" "24" "40" "4" "E-YBK" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE UT-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-TCD" " " RETURNING RESU.
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
           MOVE ZERO TO W-NG W-WNG.
           MOVE D-NTNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           COMPUTE W-WNEN = W-NEN - DATE-YC1.
           MOVE W-GET TO W-WGET.
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
           CALL "PR_Open" RETURNING RESP.
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P2-15K.
           MOVE W-20K TO P2-20K.
           MOVE ALL  "�m" TO P-KMK1 P-KMK2 P-D1 P-D2.
           MOVE 999999999 TO P-KIN1 P-KIN2.
           MOVE 99 TO H-NEN H-GET H-PEY H-PAGE P-GET P-PEY.
           MOVE "/" TO P-VER.
           MOVE ZERO TO CNT W-PAGE.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-TPC "A-TPC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-TPC = 1
               GO TO M-15
           END-IF
           IF  W-TPC NOT = 9
               GO TO M-10
           END-IF
           IF  CNT = ZERO
               PERFORM S-10 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD 1 TO CNT.
           IF  CNT = 6
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               MOVE ZERO TO CNT
           END-IF
           GO TO M-10.
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
               CALL "PR_Close" RETURNING RESP
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-15
           END-IF
           IF  CNT NOT = ZERO
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               MOVE ZERO TO CNT
           END-IF
           MOVE ZERO TO CHK.
           MOVE W-WNEN TO W-HNEN.
           MOVE W-WGET TO W-HGET.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO UKET-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           CALL "DB_F_Open" USING
            "INPUT" UKET-F_PNAME1 " " BY REFERENCE UKET-F_IDLST "0".
       M-25.
      *           READ UKET-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" UKET-F_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  UT-SKC NOT = 32
               GO TO M-25
           END-IF
           IF (UT-OKN NOT = W-NEN2) OR (UT-OKG NOT = W-GET)
               GO TO M-25
           END-IF.
       M-30.
           MOVE ZERO TO W-PAGE.
           MOVE UT-OKP TO W-PEY W-HPEY.
           MOVE UT-SBC TO W-YBK.
           MOVE W-YBK TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-YBK" E-YBK "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  CHK = ZERO
               MOVE 5 TO CHK
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE ZERO TO W-KIN.
       M-35.
           MOVE UT-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE W-TCD TO T-NAME
           END-IF
           PERFORM S-20 THRU S-30.
       M-45.
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P2-15K.
           MOVE W-20K TO P2-20K.
           MOVE SPACE TO P-KMK1 P-KMK2 P-D1 P-D2.
           MOVE  "���@���@��@�`�@" TO P-KMK1.
           MOVE  "��@��@��@�`�@" TO P-KMK2.
           MOVE UT-KIN TO P-KIN1 P-KIN2.
           MOVE  "���@��@���@�ρ@" TO P-D1.
           MOVE UT-MKG TO P-GET.
           MOVE "/" TO P-VER.
           MOVE UT-MKP TO P-PEY.
           IF  UT-TSC = 11
               MOVE  "�񑩎�`" TO P-D2
           END-IF
           IF  UT-TSC = 12
               MOVE  "�ב֎�`" TO P-D2
           END-IF
           IF  UT-TSC = 13
               MOVE "�ł񂳂�" TO P-D2
           END-IF
           IF  CNT NOT = 6
               GO TO M-50
           END-IF
           MOVE SPACE TO SP-R.
           MOVE HEAD9 TO SP-R.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           PERFORM S-05 THRU S-15.
           PERFORM S-20 THRU S-30.
       M-50.
           PERFORM S-45 THRU S-50.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD UT-KIN TO W-KIN.
           ADD 1 TO CNT.
       M-55.
      *           READ UKET-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" UKET-F_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  UT-SKC NOT = 32
               GO TO M-55
           END-IF
           IF (UT-OKN NOT = W-NEN2) OR (UT-OKG NOT = W-GET)
               GO TO M-55
           END-IF
           IF  W-PEY NOT = UT-OKP
               GO TO M-60
           END-IF
           IF  UT-SBC NOT = W-YBK
               GO TO M-60
           END-IF
           IF  UT-TCD = W-TCD
               GO TO M-45
           END-IF
           GO TO M-35.
       M-60.
           PERFORM S-35 THRU S-40.
           GO TO M-30.
       M-90.
           PERFORM S-35 THRU S-40.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-F_IDLST UKET-F_PNAME1.
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
           MOVE W-HNEN TO H-NEN.
           MOVE W-HGET TO H-GET.
           MOVE W-HPEY TO H-PEY.
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE  "���F��@" TO P-IM.
           MOVE W-PD TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO CNT.
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-P1.
           MOVE W-15K TO P1-15K.
           MOVE W-20K TO P1-20K.
           MOVE SPACE TO P-NA1 P-NA2.
           MOVE B-BNA TO P-NA1.
           MOVE T-NAME TO P-NA2.
           IF  CNT < 5
               GO TO S-25
           END-IF
           PERFORM S-45 THRU S-50.
           MOVE SPACE TO SP-R.
           MOVE HEAD9 TO SP-R.
           IF  CNT = 6
               CALL "PR_LineFeed" USING "3" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           ELSE
               CALL "PR_LineFeed" USING "5" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
           MOVE SPACE TO SP-R.
           PERFORM S-05 THRU S-15.
       S-25.
           PERFORM S-45 THRU S-50.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD 1 TO CNT.
       S-30.
           EXIT.
       S-35.
           IF  CNT = 6
               MOVE SPACE TO W-P2
               MOVE W-15K TO P2-15K
               MOVE W-20K TO P2-20K
               MOVE SPACE TO P-KMK1 P-KMK2 P-D1 P-D2
               MOVE W-KIN TO P-KIN1 P-KIN2
               MOVE SPACE TO SP-R
               MOVE W-P2 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD9 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               GO TO S-40
           END-IF
           PERFORM S-45 THRU S-50.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD 1 TO CNT.
           GO TO S-35.
       S-40.
           EXIT.
       S-45.
           MOVE SPACE TO P-IM.
           IF  CNT = 1
               MOVE  "���@��@" TO P-IM
           END-IF
           IF  CNT = 3
               MOVE  "�L����@" TO P-IM
           END-IF
           IF  CNT = 5
               MOVE  "���͈�@" TO P-IM
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-PD TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-50.
           EXIT.

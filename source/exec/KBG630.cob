       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG630.
      *********************************************************
      *    PROGRAM         :  ���i�d���W�v�\�@�@�@�@�@�@�@    *
      *    JS-SIGN         :  0:��\ , 1:�d�������� �@�@�@    *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0064".
           02  W-FID12        PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  N(018) VALUE
                "�������@�@���i�d���@���v�\�@�@������".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(007) VALUE " ����  ".
           02  F              PIC  N(007) VALUE "���@�@�i�@�@��".
           02  F              PIC  X(024) VALUE SPACE.
           02  F              PIC  N(004) VALUE "�@���@��".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(010) VALUE
                "�P�@���@�@�@�@���@�z".
       01  W-P.
           02  P-15K          PIC  X(005).
           02  P-JCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-JNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-SU           PIC -----,--9.
           02  P-T            PIC --,---,--9.99.
           02  P-KIN          PIC --,---,---,--9.
           02  P-20K          PIC  X(005).
       01  W-D.
           02  W-BKC          PIC  9(002).
           02  W-JCD          PIC  9(006).
           02  W-SU           PIC S9(006).
           02  W-KIN          PIC S9(009).
           02  W-SSU          PIC S9(006).
           02  W-SKIN         PIC S9(009).
           02  W-ASU          PIC S9(006).
           02  W-AKIN         PIC S9(009).
           02  W-T            PIC S9(006)V9(02).
           02  W-PAGE         PIC  9(002).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIJM.
           COPY LSPF.
      *FD  SS-F
       01  SS-F_KBG630.
           02  SS-F_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SS-F_LNAME     PIC  X(011) VALUE "SS-F_KBG630".
           02  F              PIC  X(001).
           02  SS-F_KEY1      PIC  X(100) VALUE SPACE.
           02  SS-F_SORT      PIC  X(100) VALUE SPACE.
           02  SS-F_IDLST     PIC  X(100) VALUE SPACE.
           02  SS-F_RES       USAGE  POINTER.
       01  SS-R.
           02  SS-JCD         PIC  9(006).
           02  SS-SCD         PIC  9(004).
           02  SS-SU          PIC S9(006).
           02  SS-KIN         PIC S9(009).
           02  SS-SC          PIC  9(001).
           02  SS-SJCD        PIC  9(006).
           02  SS-NG          PIC  9(006).
           02  SS-NGL   REDEFINES SS-NG.
             03  F            PIC  9(002).
             03  SS-NGS       PIC  9(004).
           02  SS-BKC         PIC  9(002).
           02  SS-BKNO        PIC  9(002).
           02  F              PIC  X(022).
       77  F                  PIC  X(001).
      *
      *FD  EXL-F
       01  EXL-F_KBG630.
           02  EXL-F_PNAME1   PIC  X(009) VALUE "WK0128000".
           02  F              PIC  X(001).
           02  EXL-F_LNAME    PIC  X(012) VALUE "EXL-F_KBG630".
           02  F              PIC  X(001).
           02  EXL-F_KEY1     PIC  X(100) VALUE SPACE.
           02  EXL-F_SORT     PIC  X(100) VALUE SPACE.
           02  EXL-F_IDLST    PIC  X(100) VALUE SPACE.
           02  EXL-F_RES      USAGE  POINTER.
       01  EXL-R.
           02  EXL-JCD        PIC  9(006).
           02  EXL-JNA        PIC  N(024).
           02  EXL-SU         PIC S9(007).
           02  EXL-T          PIC S9(007)V9(2).
           02  EXL-KIN        PIC S9(012).
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
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(018) VALUE
                "������������������������������������".
           02  FILLER  PIC  N(018) VALUE
                "������������������������������������".
           02  FILLER  PIC  N(018) VALUE
                "�������@�@�@�@�@�@�@�@�@�@�@�@������".
           02  FILLER  PIC  N(018) VALUE
                "�������@�@���i�d���@���v�\�@�@������".
           02  FILLER  PIC  N(018) VALUE
                "�������@�@�@�@�@�@�@�@�@�@�@�@������".
           02  FILLER  PIC  N(018) VALUE
                "������������������������������������".
           02  FILLER  PIC  N(018) VALUE
                "������������������������������������".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ż  ***".
             03  E-ME98  PIC  X(075) VALUE X"1B4A05".
             03  E-ME99  PIC  X(075) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "252" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "36" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "36" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "36" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "36" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "36" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "36" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "36" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "167" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "167" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "75" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "75" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           COPY LIBCPR.
           MOVE DATE-05R TO H-DATE.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12.
           MOVE W-FID1 TO WK0064ID.
           MOVE WK0064ID TO SS-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SS-F_PNAME1 " " BY REFERENCE SS-F_IDLST "0".
       M-10.
      *           READ SS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SS-F_PNAME1 BY REFERENCE SS-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SS-F_IDLST SS-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           IF  JS-SIGN = 1
               CALL "DB_F_Open" USING
                "OUTPUT" EXL-F_PNAME1 " " BY REFERENCE EXL-F_IDLST "0"
           ELSE
               CALL "PR_Open" RETURNING RESP
           END-IF
           MOVE ZERO TO W-ASU W-AKIN W-PAGE.
           IF  JS-SIGN = 0
               PERFORM S-10 THRU S-15
           END-IF.
       M-15.
           MOVE ZERO TO W-SSU W-SKIN.
           MOVE SS-BKC TO W-BKC.
       M-20.
           MOVE ZERO TO W-SU W-KIN.
           MOVE SS-JCD TO J-KEY W-JCD.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "�@�������@�}�X�^�[�@�Ȃ��@�������@�@" TO J-NAME
           END-IF.
       M-25.
           PERFORM S-20 THRU S-25.
       M-30.
      *           READ SS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SS-F_PNAME1 BY REFERENCE SS-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  SS-BKC NOT = W-BKC
               GO TO M-40
           END-IF
           IF  W-JCD = SS-JCD
               GO TO M-25
           END-IF.
       M-35.
           PERFORM S-30 THRU S-45.
           GO TO M-20.
       M-40.
           PERFORM S-30 THRU S-45.
           PERFORM S-50 THRU S-65.
           GO TO M-15.
       M-85.
           PERFORM S-30 THRU S-45.
           PERFORM S-50 THRU S-65.
           PERFORM S-70 THRU S-80.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SS-F_IDLST SS-F_PNAME1.
           IF  JS-SIGN = 1
               CALL "DB_F_Close" USING
                BY REFERENCE EXL-F_IDLST EXL-F_PNAME1
           ELSE
               CALL "PR_Close" RETURNING RESP
           END-IF.
       M-95.
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
       S-15.
           EXIT.
       S-20.
           ADD SS-KIN TO W-KIN.
           IF  SS-KIN NOT = ZERO
               ADD SS-SU TO W-SU
           END-IF.
       S-25.
           EXIT.
       S-30.
           IF  ZERO = W-SU AND W-KIN
               GO TO S-45
           END-IF
           IF  JS-SIGN = 1
               GO TO S-35
           END-IF
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-JNA.
           MOVE W-JCD TO P-JCD.
           MOVE J-NAME TO P-JNA.
           IF  ZERO NOT = W-SU
               IF  ZERO NOT = W-KIN
                   COMPUTE W-T = W-KIN / W-SU
                   MOVE W-SU TO P-SU
                   MOVE W-T TO P-T
               END-IF
           END-IF
           MOVE W-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-10
           END-IF
      *
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO S-40.
       S-35.
           MOVE SPACE TO EXL-R.
           MOVE SPACE TO EXL-JNA.
           MOVE W-JCD TO EXL-JCD.
           MOVE J-NAME TO EXL-JNA.
           IF  ZERO NOT = W-SU
               IF  ZERO NOT = W-KIN
                   COMPUTE W-T = W-KIN / W-SU
                   MOVE W-SU TO EXL-SU
                   MOVE W-T TO EXL-T
               END-IF
           END-IF
           MOVE W-KIN TO EXL-KIN.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET.
       S-40.
           ADD W-SU TO W-SSU.
           ADD W-KIN TO W-SKIN.
       S-45.
           EXIT.
       S-50.
           IF  JS-SIGN = 1
               GO TO S-55
           END-IF
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-JNA.
           MOVE "�@�@�@�@�@�m�@�r�t�a�@�s�n�s�`�k�@�n�@�@" TO P-JNA.
           MOVE W-SSU TO P-SU.
           MOVE W-SKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO S-60.
       S-55.
           MOVE SPACE TO EXL-R.
           MOVE SPACE TO EXL-JNA.
           MOVE "�@�@�@�@�@�m�@�r�t�a�@�s�n�s�`�k�@�n�@�@" TO EXL-JNA.
           MOVE W-SSU TO EXL-SU.
           MOVE W-SKIN TO EXL-KIN.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET.
       S-60.
           ADD W-SSU TO W-ASU.
           ADD W-SKIN TO W-AKIN.
       S-65.
           EXIT.
       S-70.
           IF  JS-SIGN = 1
               GO TO S-75
           END-IF
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-JNA.
           MOVE "�@�@�@�y�@�`�k�k�@�s�n�s�`�k�@�z�@�@�@�@" TO P-JNA.
           MOVE W-ASU TO P-SU.
           MOVE W-AKIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO S-80.
       S-75.
           MOVE SPACE TO EXL-R.
           MOVE SPACE TO EXL-JNA.
           MOVE "�@�@�@�y�@�`�k�k�@�s�n�s�`�k�@�z�@�@�@�@" TO EXL-JNA.
           MOVE W-ASU TO EXL-SU.
           MOVE W-AKIN TO EXL-KIN.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET.
       S-80.
           EXIT.

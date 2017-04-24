       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG240.
      *********************************************************
      *    PROGRAM         :  �������ޕʁ@�̔����ѕ\ �@�@�@�@ *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=��\ , 1=�d�w�b�d�k , 2=��\�Q*
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0128".
           02  W-FID12        PIC  X(003).
       01  HEAD1.
           02  20K            PIC  X(005) VALUE X"1A24212474".
           02  H-SAN          PIC  N(001) VALUE SPACE.
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  N(019) VALUE
                "�������@�@�́@���@���@�с@�\�@�@������".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(039) VALUE SPACE.
           02  F              PIC  N(004) VALUE "�̔�����".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "������z".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(003) VALUE "���P��".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "�̔�����".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(014) VALUE
                "���P���@�@�@�̔����v�@���v��".
           02  F              PIC  X(001) VALUE "%".
       01  W-P.
           02  P-TM           PIC  N(018).
           02  P-MD    REDEFINES P-TM.
             03  P-M0         PIC  N(003).
             03  F            PIC  X(004).
             03  P-M1         PIC  N(003).
             03  F            PIC  X(004).
             03  P-M2         PIC  N(008).
           02  P-SS           PIC -------,--9.
           02  P-UKN          PIC -------,---,--9.
           02  P-ST1          PIC -----,--9.
           02  P-UG           PIC -------,---,--9.
           02  P-ST2          PIC -----,--9.
           02  P-UR           PIC --,---,---,--9.
           02  P-RR           PIC ------9.9.
       01  W-DATA.
           02  W-SCD.
             03  W-CC         PIC  9(002).
             03  W-NC         PIC  9(002).
             03  W-SC         PIC  9(002).
           02  W-D.
             03  W-ST1        PIC S9(005).
             03  W-ST2        PIC S9(005).
             03  W-UR         PIC S9(009).
             03  W-RR         PIC S9(003)V9(01).
             03  W-SS         PIC S9(007).
             03  W-UKN        PIC S9(010).
             03  W-UG         PIC S9(010).
           02  WT-D.
             03  WT-SS        PIC S9(007).
             03  WT-UKN       PIC S9(010).
             03  WT-UG        PIC S9(010).
           02  WS-D.
             03  WS-SS        PIC S9(007).
             03  WS-UKN       PIC S9(010).
             03  WS-UG        PIC S9(010).
           02  WA-D.
             03  WA-SS        PIC S9(007).
             03  WA-UKN       PIC S9(010).
             03  WA-UG        PIC S9(010).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  CNT            PIC  9(001).
           02  W-BRN3         PIC  N(003).
           02  W-BMN          PIC  N(003).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIHKBM.
           COPY LSPF.
      *FD  HC-F
       01  HC-F_HMG240.
           02  HC-F_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HC-F_LNAME     PIC  X(011) VALUE "HC-F_HMG240".
           02  F              PIC  X(001).
           02  HC-F_KEY1      PIC  X(100) VALUE SPACE.
           02  HC-F_SORT      PIC  X(100) VALUE SPACE.
           02  HC-F_IDLST     PIC  X(100) VALUE SPACE.
           02  HC-F_RES       USAGE  POINTER.
       01  HC-R.
           02  HC-BC.
             03  HC-BC1       PIC  9(002).
             03  HC-BC2.
               04  HC-BC21    PIC  9(001).
               04  HC-BC22    PIC  9(001).
             03  HC-BC3       PIC  9(002).
           02  HC-BMC         PIC  9(002).
           02  HC-BMNO        PIC  9(001).
           02  HC-ZKS         PIC S9(007).
           02  HC-ZKK         PIC S9(010).
           02  HC-NS          PIC S9(007).
           02  HC-SKN         PIC S9(010).
           02  HC-SS          PIC S9(007).
           02  HC-UKN         PIC S9(010).
           02  HC-YKS         PIC S9(007).
           02  HC-YKK         PIC S9(010).
           02  HC-UG          PIC S9(010).
           02  F              PIC  X(041).
       77  F                  PIC  X(001).
      *FD  EXLF
       01  EXLF_HMG240.
           02  EXLF_PNAME1    PIC  X(009) VALUE "WK0256000".
           02  F              PIC  X(001).
           02  EXLF_LNAME     PIC  X(011) VALUE "EXLF_HMG240".
           02  F              PIC  X(001).
           02  EXLF_KEY1      PIC  X(100) VALUE SPACE.
           02  EXLF_SORT      PIC  X(100) VALUE SPACE.
           02  EXLF_IDLST     PIC  X(100) VALUE SPACE.
           02  EXLF_RES       USAGE  POINTER.
       01  EXL-R.
           02  EXL-M0         PIC  N(003).
           02  EXL-M1         PIC  N(003).
           02  EXL-M2         PIC  N(008).
           02  EXL-SS         PIC S9(006).
           02  EXL-UKN        PIC S9(009).
           02  EXL-ST1        PIC S9(006).
           02  EXL-UG         PIC S9(009).
           02  EXL-ST2        PIC S9(006).
           02  EXL-UR         PIC S9(009).
           02  EXL-RR         PIC S9(005)V9.
           02  F              PIC  X(177).
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(019) VALUE
                "��������������������������������������".
           02  FILLER  PIC  N(019) VALUE
                "��������������������������������������".
           02  FILLER  PIC  N(019) VALUE
                "�������@�@�@�@�@�@�@�@�@�@�@�@�@������".
           02  FILLER  PIC  N(019) VALUE
                "�������@�@�́@���@���@�с@�\�@�@������".
           02  FILLER  PIC  N(019) VALUE
                "�������@�@�@�@�@�@�@�@�@�@�@�@�@������".
           02  FILLER  PIC  N(019) VALUE
                "��������������������������������������".
           02  FILLER  PIC  N(019) VALUE
                "��������������������������������������".
       01  C-ERR.
           02  FILLER.
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "266" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "13" "38" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "13" "38" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "13" "38" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "13" "38" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "13" "38" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "13" "38" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "13" "38" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "10" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 2
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP RUN
           END-IF
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12.
           MOVE W-FID1 TO WK0128ID.
           MOVE WK0128ID TO HC-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HC-F_PNAME1 " " BY REFERENCE HC-F_IDLST "0".
       M-10.
      *           READ HC-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HC-F_PNAME1 BY REFERENCE HC-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HC-F_IDLST HC-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ZERO = HC-ZKS AND HC-ZKK AND HC-NS AND HC-SKN AND
                     HC-SS AND HC-UKN AND HC-YKS AND HC-YKK AND HC-UG
               GO TO M-10
           END-IF
      *
           MOVE ZERO TO WA-D.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           IF  JS-SIGN = 1
               CALL "DB_F_Open" USING
                "OUTPUT" EXLF_PNAME1 " " BY REFERENCE EXLF_IDLST "0"
               GO TO M-15
           END-IF
           IF  JS-SIGN = 2
               MOVE "�Q" TO H-SAN
           END-IF
           CALL "PR_Open" RETURNING RESP.
      *
           MOVE DATE-02R TO H-DATE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-15.
           MOVE HC-BC3 TO W-CC.
           MOVE ZERO TO WS-D CHK.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "14" TO HKB-NO.
           MOVE W-CC TO HKB-BR3.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BRN3
           END-IF
           MOVE HKB-BRN3 TO W-BRN3.
       M-20.
           MOVE HC-BMC TO W-NC.
           MOVE ZERO TO WT-D CHK2 CNT.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "16" TO HKB-NO.
           MOVE W-NC TO HKB-BMC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BMN
           END-IF
           MOVE HKB-BMN TO W-BMN.
       M-25.
           IF  JS-SIGN = 1
               INITIALIZE EXL-R
           ELSE
               MOVE SPACE TO W-P
           END-IF
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               IF  JS-SIGN = 1
                   MOVE W-BRN3 TO EXL-M0
               ELSE
                   MOVE W-BRN3 TO P-M0
               END-IF
           END-IF
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               IF  JS-SIGN = 1
                   MOVE W-BMN TO EXL-M1
               ELSE
                   MOVE W-BMN TO P-M1
               END-IF
           END-IF
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "11" TO HKB-NO.
           MOVE HC-BC1 TO HKB-BR1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BRN1
           END-IF
           IF  JS-SIGN = 1
               MOVE HKB-BRN1 TO EXL-M2
           ELSE
               MOVE HKB-BRN1 TO P-M2
           END-IF
      *
           MOVE ZERO TO W-D.
           MOVE HC-SS TO W-SS.
           MOVE HC-UKN TO W-UKN.
           MOVE HC-UG TO W-UG.
      *
           PERFORM S-05 THRU S-20.
      *
           ADD HC-SS TO WT-SS.
           ADD HC-UKN TO WT-UKN.
           ADD HC-UG TO WT-UG.
           IF  CNT < 2
               ADD 1 TO CNT
           END-IF.
       M-30.
      *           READ HC-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HC-F_PNAME1 BY REFERENCE HC-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  ZERO = HC-ZKS AND HC-ZKK AND HC-NS AND HC-SKN AND
                     HC-SS AND HC-UKN AND HC-YKS AND HC-YKK AND HC-UG
               GO TO M-30
           END-IF
           IF  W-CC NOT = HC-BC3
               GO TO M-35
           END-IF
           IF  W-NC = HC-BMC
               GO TO M-25
           END-IF
           PERFORM S-25 THRU S-35.
           GO TO M-20.
       M-35.
           PERFORM S-25 THRU S-35.
           PERFORM S-40 THRU S-45.
           GO TO M-15.
       M-80.
           PERFORM S-25 THRU S-35.
           PERFORM S-40 THRU S-45.
      *
           IF  JS-SIGN = 1
               INITIALIZE EXL-R
               MOVE "�y�@���@�v�@�z�@" TO EXL-M2
           ELSE
               MOVE SPACE TO W-P
               MOVE "�@�@�y�@���@�v�@�z�@�@�@�@" TO P-TM
           END-IF
           MOVE ZERO TO W-D.
           MOVE WA-SS TO W-SS.
           MOVE WA-UKN TO W-UKN.
           MOVE WA-UG TO W-UG.
           PERFORM S-05 THRU S-20.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HC-F_IDLST HC-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           IF  JS-SIGN = 1
               CALL "DB_F_Close" USING
                BY REFERENCE EXLF_IDLST EXLF_PNAME1
           ELSE
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           IF  JS-SIGN = 1
               MOVE W-SS TO EXL-SS
               MOVE W-UKN TO EXL-UKN
               MOVE W-UG TO EXL-UG
           ELSE
               MOVE W-SS TO P-SS
               MOVE W-UKN TO P-UKN
               MOVE W-UG TO P-UG
           END-IF
           IF  W-SS = ZERO
               GO TO S-10
           END-IF
           IF  W-UKN NOT = ZERO
               COMPUTE W-ST1 ROUNDED = W-UKN / W-SS
               IF  JS-SIGN = 1
                   MOVE W-ST1 TO EXL-ST1
               ELSE
                   MOVE W-ST1 TO P-ST1
               END-IF
           END-IF
           IF  W-UG NOT = ZERO
               COMPUTE W-ST2 ROUNDED = W-UG / W-SS
               IF  JS-SIGN = 1
                   MOVE W-ST2 TO EXL-ST2
               ELSE
                   MOVE W-ST2 TO P-ST2
               END-IF
           END-IF.
       S-10.
           COMPUTE W-UR = W-UKN - W-UG.
           IF  JS-SIGN = 1
               MOVE W-UR TO EXL-UR
           ELSE
               MOVE W-UR TO P-UR
           END-IF
           IF  W-UKN = ZERO
               IF  W-UR NOT = ZERO
                   MOVE 100 TO W-RR
                   GO TO S-15
               END-IF
           END-IF
           IF  W-UR NOT = ZERO
               IF  W-UKN NOT = ZERO
                   COMPUTE W-RR ROUNDED = (W-UR * 100) / W-UKN
               END-IF
           END-IF.
       S-15.
           IF  W-UR > ZERO
               IF  W-RR < ZERO
                   COMPUTE W-RR = -1 * W-RR
               END-IF
           END-IF
           IF  W-UR < ZERO
               IF  W-RR > ZERO
                   COMPUTE W-RR = -1 * W-RR
               END-IF
           END-IF
           IF  JS-SIGN = 1
               MOVE W-RR TO EXL-RR
      *               WRITE EXL-R
      *//////////////
               CALL "DB_Insert" USING
                EXLF_PNAME1 EXLF_LNAME EXL-R RETURNING RET
               GO TO S-20
           END-IF
           MOVE W-RR TO P-RR.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-20.
           EXIT.
       S-25.
           IF  CNT NOT = 2
               GO TO S-30
           END-IF
           IF  JS-SIGN = 1
               INITIALIZE EXL-R
               MOVE "�@�@�@�i�@�v�@�j" TO EXL-M2
           ELSE
               MOVE SPACE TO W-P
               MOVE "�@�@�@�@�@�@�@�@�i�@�v�@�j" TO P-TM
           END-IF
           MOVE ZERO TO W-D.
           MOVE WT-SS TO W-SS.
           MOVE WT-UKN TO W-UKN.
           MOVE WT-UG TO W-UG.
      *
           PERFORM S-05 THRU S-20.
       S-30.
           IF  JS-SIGN NOT = 1
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
      *
           ADD WT-SS TO WS-SS.
           ADD WT-UKN TO WS-UKN.
           ADD WT-UG TO WS-UG.
       S-35.
           EXIT.
       S-40.
           IF  JS-SIGN = 1
               INITIALIZE EXL-R
               MOVE "�@�m�@���@�v�@�n" TO EXL-M2
           ELSE
               MOVE SPACE TO W-P
               MOVE "�@�@�@�@�m�@���@�v�@�n�@�@" TO P-TM
           END-IF
           MOVE ZERO TO W-D.
           MOVE WS-SS TO W-SS.
           MOVE WS-UKN TO W-UKN.
           MOVE WS-UG TO W-UG.
      *
           PERFORM S-05 THRU S-20.
           IF  JS-SIGN NOT = 1
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
      *
           ADD WS-SS TO WA-SS.
           ADD WS-UKN TO WA-UKN.
           ADD WS-UG TO WA-UG.
       S-45.
           EXIT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG320.
      *********************************************************
      *    PROGRAM         :  �S�����Ӑ�ʁ@�������ו\        *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        �ύX�@�@�@  :  62/05/29                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
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
           02  20K            PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  N(020) VALUE
                "�������@�@���Ӑ�ʁ@�������ו\�@�@������".
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  N(002) VALUE "�S��".
           02  F              PIC  X(021) VALUE
                " ���ށ@���@�Ӂ@��@��".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  N(003) VALUE "������".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(007) VALUE "�����z�@��@��".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "���@��".
           02  F              PIC  X(001) VALUE SPACE.
       01  W-P.
           02  F              PIC  X(001).
           02  P-TC           PIC  9(002).
           02  F              PIC  X(002).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-NA           PIC  N(026).
           02  F              PIC  X(002).
           02  P-DATE         PIC 99/99/99.
           02  P-KIN          PIC -----,---,--9.
           02  F              PIC  X(002).
           02  P-NC           PIC  N(006).
           02  F              PIC  X(001).
           02  P-TD           PIC 99/99/99.
       01  W-DATA.
           02  W-TC.
             03  W-TC1        PIC  9(001).
             03  W-TC2        PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-NKIN         PIC S9(009).
           02  W-TKIN         PIC S9(009).
           02  W-SKIN         PIC S9(009).
           02  W-AKIN         PIC S9(009).
           02  W-C            PIC  9(001).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-SETC.
             03  W-STC        PIC  9(002).
             03  W-ETC        PIC  9(002) VALUE 99.
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHKBM.
           COPY LSPF.
      *FD  NYUR-F
       01  NYUR-F_HKG320.
           02  NYUR-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  NYUR-F_LNAME   PIC  X(013) VALUE "NYUR-F_HKG320".
           02  F              PIC  X(001).
           02  NYUR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  NYUR-F_SORT    PIC  X(100) VALUE SPACE.
           02  NYUR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  NYUR-F_RES     USAGE  POINTER.
       01  NYUR-R.
           02  F              PIC  9(002).
           02  N-DATE         PIC  9(006).
           02  N-TCD          PIC  9(004).
           02  N-KIN          PIC S9(008).
           02  N-NC.
             03  N-NC1        PIC  9(001).
             03  N-NC2        PIC  9(001).
           02  F              PIC  9(003).
           02  N-TD           PIC  9(006).
           02  F              PIC  9(002).
           02  N-SD           PIC  9(004).
           02  N-BC           PIC  9(001).
           02  N-TC.
             03  N-TC1        PIC  9(001).
             03  N-TC2        PIC  9(001).
           02  F              PIC  X(088).
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "��������������������������������������������".
           02  FILLER  PIC  N(022) VALUE
                "��������������������������������������������".
           02  FILLER  PIC  N(022) VALUE
                "�������@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@������".
           02  FILLER  PIC  N(022) VALUE
                "�������@�@�S�����Ӑ�ʁ@�������ו\�@�@������".
           02  FILLER  PIC  N(022) VALUE
                "�������@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@������".
           02  FILLER  PIC  N(022) VALUE
                "��������������������������������������������".
           02  FILLER  PIC  N(022) VALUE
                "��������������������������������������������".
           02  FILLER  PIC  X(020) VALUE
                "�S���Һ���  00 �` 99".
           02  FILLER  PIC  X(022) VALUE
                "�m�F  OK=1 NO=9   ����".
       01  C-ACP.
           02  FILLER.
             03  A-STC   PIC  9(002).
             03  A-ETC   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1.
               04  FILLER  PIC  X(027) VALUE
                    "***  HKBM ż (       )  ***".
               04  02E-ME1 PIC  X(007).
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
           "C-MID" " " "0" "0" "350" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "15" "22" "20" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "22" "21" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "15" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-STC" "9" "15" "34" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-STC" BY REFERENCE W-STC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-ETC" "9" "15" "40" "2" "A-STC" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-ETC" BY REFERENCE W-ETC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "22" "38" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "44" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "44" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" " " "24" "0" "34" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "01E-ME1" "X" "24" "15" "27" " " "E-ME1" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-ME1" "X" "24" "29" "7" "01E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING
           "02E-ME1" BY REFERENCE HKB-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-STC "A-STC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-ETC "A-ETC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-STC > W-ETC
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
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
      *
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO NYUR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NYUR-F_PNAME1 " " BY REFERENCE NYUR-F_IDLST "0".
       M-25.
      *           READ NYUR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NYUR-F_PNAME1 BY REFERENCE NYUR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1
               GO TO M-95
           END-IF
           IF  N-TC < W-STC
               GO TO M-25
           END-IF
           IF  N-TC > W-ETC
               CALL "DB_F_Close" USING
                BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO W-AKIN.
       M-30.
           MOVE ZERO TO W-SKIN.
           MOVE N-TC1 TO W-TC1.
       M-35.
           MOVE ZERO TO W-TKIN W-C.
           MOVE N-TC2 TO W-TC2.
       M-40.
           MOVE ZERO TO W-NKIN CHK.
           MOVE N-TCD TO W-TCD.
       M-45.
           PERFORM S-20 THRU S-35.
       M-50.
      *           READ NYUR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NYUR-F_PNAME1 BY REFERENCE NYUR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  N-TC > W-ETC
               GO TO M-90
           END-IF
           IF  N-TC1 NOT = W-TC1
               GO TO M-65
           END-IF
           IF  N-TC2 NOT = W-TC2
               GO TO M-60
           END-IF
           IF  N-TCD NOT = W-TCD
               GO TO M-55
           END-IF
           GO TO M-45.
       M-55.
           PERFORM S-40 THRU S-45.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO M-40.
       M-60.
           PERFORM S-40 THRU S-45.
           PERFORM S-50 THRU S-55.
           PERFORM S-05 THRU S-15.
           GO TO M-35.
       M-65.
           PERFORM S-40 THRU S-45.
           PERFORM S-50 THRU S-55.
           PERFORM S-60 THRU S-65.
           PERFORM S-05 THRU S-15.
           GO TO M-30.
       M-90.
           PERFORM S-40 THRU S-45.
           PERFORM S-50 THRU S-55.
           PERFORM S-60 THRU S-65.
           PERFORM S-70 THRU S-75.
      *
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE ZERO TO CHK1 W-C.
       S-15.
           EXIT.
       S-20.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO W-P.
           IF  CHK1 NOT = ZERO
               GO TO S-30
           END-IF
           IF  W-C = ZERO
               MOVE W-TC TO P-TC
           END-IF
           MOVE N-TCD TO P-TCD.
           MOVE N-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "�@�����@�}�X�^�[�@�Ȃ��@�����@�@" TO T-NAME
           END-IF
           MOVE T-NAME TO P-NA.
           MOVE 5 TO CHK1 W-C.
       S-30.
           MOVE N-DATE TO P-DATE.
           MOVE N-KIN TO P-KIN.
           MOVE SPACE TO HKB-KEY.
           MOVE "31" TO HKB-NO.
           MOVE N-NC1 TO HKB-NKC1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE SPACE TO HKB-NKNA
           END-IF
           MOVE HKB-NKNA TO P-NC.
           IF  N-NC2 > 7
               MOVE "���@��@�Ł@" TO P-NC
           END-IF
           IF  N-TD NOT = ZERO
               MOVE N-TD TO P-TD
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD N-KIN TO W-NKIN.
           IF  CHK2 = 3
               MOVE 5 TO CHK2
           END-IF
           IF  CHK2 = ZERO
               MOVE 3 TO CHK2
           END-IF.
       S-35.
           EXIT.
       S-40.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           ADD W-NKIN TO W-TKIN.
           IF  CHK2 = 3
               GO TO S-45
           END-IF
           MOVE SPACE TO W-P.
           IF  W-C = ZERO
               MOVE W-TC TO P-TC
           END-IF
           MOVE "�@�@�@�@�@�@�@�@�@�@�@�@�i�@�v�@�j�@�@�@" TO P-NA.
           MOVE W-NKIN TO P-KIN.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE 5 TO W-C.
       S-45.
           EXIT.
       S-50.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO W-P.
           IF  W-C = ZERO
               MOVE W-TC TO P-TC
           END-IF
           MOVE "�@�@�@�@�@�@�@�@���@�@���@�v�@�@���@" TO P-NA.
           MOVE W-TKIN TO P-KIN.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD W-TKIN TO W-SKIN.
           MOVE 5 TO W-C.
       S-55.
           EXIT.
       S-60.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO W-P.
           MOVE "�@�@�@�@�@�m�@�@���@���@�v�@�@�n�@�@�@" TO P-NA.
           MOVE W-SKIN TO P-KIN.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD W-SKIN TO W-AKIN.
       S-65.
           EXIT.
       S-70.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO W-P.
           MOVE "�@�y�@�@���@���@�v�@�@�z�@�@�@�@�@�@�@" TO P-NA.
           MOVE W-AKIN TO P-KIN.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-75.
           EXIT.

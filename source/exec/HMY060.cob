       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY060.
      **************************************************************
      *    PROGRAM         :  �����N�ԃT�C�Y�ʍ�\�@���ԓ��t����   *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  ******                               *
      *        �ύX�@�@�@  :  95/08/08                             *
      *    COMPILE TYPE    :  COBOL                                *
      *    JS-SIGN         :  0=���t���� , 1=�挎��                *
      *    W-JS2           :  0=�o�וԕi , 1=�o��     , 2=�ԕi     *
      **************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM7200.
       OBJECT-COMPUTER. SYSTEM7200.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERR-STAT           PIC  X(002).
       01  JS-SIGN            PIC  9(001).
       01  W-JS2              PIC  9(001).
       01  W-DATA.
           02  W-SNG          PIC  9(006).
           02  W-SNGD  REDEFINES W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SND   REDEFINES W-SNEN.
               04  W-SN1      PIC  9(002).
               04  W-SN2      PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-ENG          PIC  9(006).
           02  W-ENGD  REDEFINES W-ENG.
             03  W-ENEN       PIC  9(004).
             03  W-END   REDEFINES W-ENEN.
               04  W-EN1      PIC  9(002).
               04  W-EN2      PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-ENGL  REDEFINES W-ENG.
             03  F            PIC  9(002).
             03  W-ENGS       PIC  9(004).
           02  W-SYM.
             03  W-SYY        PIC  9(004).
             03  W-SYD   REDEFINES W-SYY.
               04  W-SY1      PIC  9(002).
               04  W-SY2      PIC  9(002).
             03  W-SMM        PIC  9(002).
           02  W-SYML  REDEFINES W-SYM.
             03  F            PIC  9(002).
             03  W-SYMS       PIC  9(004).
           02  W-EYM.
             03  W-EYY        PIC  9(004).
             03  W-EYD   REDEFINES W-EYY.
               04  W-EY1      PIC  9(002).
               04  W-EY2      PIC  9(002).
             03  W-EMM        PIC  9(002).
           02  W-EYML  REDEFINES W-EYM.
             03  F            PIC  9(002).
             03  W-EYMS       PIC  9(004).
           02  W-SEN          PIC  9(001).
           02  W-DMM          PIC  9(001).
           COPY LSTAT.
           COPY LIBFDD.
       01  SNTRF_HMY060.
           02  SNTRF_PNAME1  PIC  X(005)  VALUE "SNTRF".
           02  F             PIC  X(001).
           02  SNTRF_LNAME   PIC  X(012)  VALUE "SNTRF_HMY060".
           02  F             PIC  X(001).
           02  SNTRF_KEY1    PIC  X(100)  VALUE SPACE.
           02  SNTRF_KEY2    PIC  X(100)  VALUE SPACE.
           02  SNTRF_SORT    PIC  X(100)  VALUE SPACE.
           02  SNTRF_IDLST   PIC  X(100)  VALUE SPACE.
           02  SNTRF_RES     USAGE  POINTER.
       01  SNTR-R             PIC  X(128).
       77  F                  PIC  X(001).
       01  STRANYR_HMY060.
           02  STRANYR_PNAME1  PIC  X(011)  VALUE "STRANYR-RDB".
           02  F               PIC  X(001).
           02  STRANYR_LNAME   PIC  X(014)  VALUE "STRANYR_HMY060".
           02  F               PIC  X(001).
           02  STRANYR_KEY1    PIC  X(100)  VALUE SPACE.
           02  STRANYR_KEY2    PIC  X(100)  VALUE SPACE.
           02  STRANYR_SORT    PIC  X(100)  VALUE SPACE.
           02  STRANYR_IDLST   PIC  X(100)  VALUE SPACE.
           02  STRANYR_RES     USAGE  POINTER.
       01  STRANY-R.
           02  STR-DNO        PIC  9(006).
           02  STR-GNO        PIC  9(001).
           02  STR-DATE.
             03  STR-NG       PIC  9(006).
             03  STR-PEY      PIC  9(002).
           02  STR-TCD        PIC  9(004).
           02  STR-HCD        PIC  9(006).
           02  STR-SC         PIC  9(001).
           02  STR-SUD.
             03  STR-SU    OCCURS  10  PIC S9(004)  COMP-3.
           02  STR-TSU        PIC S9(005).
           02  F              PIC  X(014).
           02  STR-DC         PIC  9(001).
           02  F              PIC  X(012).
           02  STR-BC3        PIC  9(002).
           02  F              PIC  X(037).
           02  STR-UNC        PIC  9(001).
       77  F                  PIC  X(001).
       01  NSSWF_HMY060.
           02  NSSWF_PNAME1  PIC  X(006)  VALUE "WK0064".
           02  F             PIC  X(001).
           02  NSSWF_LNAME   PIC  X(012)  VALUE "NSSWF_HMY060".
           02  F             PIC  X(001).
           02  NSSWF_KEY1    PIC  X(100)  VALUE SPACE.
           02  NSSWF_KEY2    PIC  X(100)  VALUE SPACE.
           02  NSSWF_SORT    PIC  X(100)  VALUE SPACE.
           02  NSSWF_IDLST   PIC  X(100)  VALUE SPACE.
           02  NSSWF_RES     USAGE  POINTER.
       01  NSSW-R.
           02  NSS-HCD        PIC  9(006).
           02  NSS-SC         PIC  9(001).
           02  NSS-SUD.
             03  NSS-SUDA  OCCURS  10.
               04  NSS-SU     PIC S9(006)  COMP-3.
           02  NSS-TSU        PIC S9(008)  COMP-3.
           02  NSS-TCD        PIC  9(004).
           02  NSS-NG         PIC  9(006).
           02  NSS-BC3        PIC  9(002).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(020) VALUE
                "����������������������������������������".
           02  FILLER  PIC  N(020) VALUE
                "����������������������������������������".
           02  FILLER  PIC  N(020) VALUE
                "�������@�@�@�@�@�@�@�@�@�@�@�@�@�@������".
           02  FILLER  PIC  N(020) VALUE
                "�������@�@�����N�ԃT�C�Y�ʍ�\�@�@������".
           02  FILLER  PIC  N(025) VALUE
                "�������@�@�@�@�@�@�@�@�@�@�@�@�@�@������".
           02  FILLER  PIC  N(020) VALUE
                "����������������������������������������".
           02  FILLER  PIC  N(020) VALUE
                "����������������������������������������".
           02  FILLER  PIC  X(049) VALUE
                "�S�� = 0  ,  ��ʁE���[�N = 1  ,  ���� = 2  ...  ".
           02  FILLER  PIC  X(040) VALUE
                "�c�`�s�`����    '  �N   �� �` '  �N   ��".
           02  FILLER  PIC  X(040) VALUE
                "�� �\ �� ��     '  �N   �� �` '  �N   ��".
           02  FILLER  PIC  X(022) VALUE
                "�m�F  OK=1 NO=9   ����".
       01  C-ACP.
           02  A-SEN    PIC  9(001).
           02  A-PNG.
             03  A-SNEN    PIC  9(002).
             03  A-SGET    PIC  9(002).
             03  A-ENEN    PIC  9(002).
             03  A-EGET    PIC  9(002).
           02  A-DMM     PIC  9(001).
       01  C-DSP.
           02  D-DNG.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(018) VALUE
                  "***  DATEM ż  ***".
             03  E-ME2     PIC  X(027) VALUE
                  "***  DATEM REWRITE �װ  ***".
             03  E-ME3     PIC  X(017) VALUE
                  "***  DATA ż  ***".
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "441" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "40" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "40" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "40" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "50" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "40" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "40" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "12" "10" "49" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "15" "10" "40" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "17" "10" "40" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "11C-MID" "X" "20" "24" "22" "10C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SEN" "9" "12" "58" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-PNG" " " "17" "0" "8" "A-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SNEN" "9" "17" "27" "2" " " "A-PNG" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SNEN" BY REFERENCE W-SN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SGET" "9" "17" "32" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ENEN" "9" "17" "41" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-ENEN" BY REFERENCE W-EN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EGET" "9" "17" "46" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "41" "1" "A-PNG" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-DNG" " " "15" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-DNG" "9" "15" "27" "2" " " "D-DNG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DNG" BY REFERENCE W-SY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-DNG" "9" "15" "32" "2" "01D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DNG" BY REFERENCE W-SMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-DNG" "9" "15" "41" "2" "02D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-DNG" BY REFERENCE W-EY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-DNG" "9" "15" "46" "2" "03D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-DNG" BY REFERENCE W-EMM "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "72" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "72" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "27" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" "X" "24" "15" "17" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP RUN
           END-IF.
           ACCEPT W-JS2 FROM ARGUMENT-VALUE.
           IF  W-JS2 > 2
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP RUN
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                              RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-NHNG TO W-EYMS.
           IF  W-EY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-EYY
           END-IF.
           IF  W-EY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-EYY
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" SNTRF_PNAME1 " " BY REFERENCE SNTRF_IDLST "0".
      *           READ SNTRF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               SUBTRACT 1 FROM W-EMM
           END-IF.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
           IF  W-EMM = ZERO
               SUBTRACT 1 FROM W-EYY
               MOVE 12 TO W-EMM
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" STRANYR_PNAME1 "SHARED" BY REFERENCE
            STRANYR_IDLST "0".
      *           READ STRANYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" STRANYR_PNAME1 BY REFERENCE STRANY-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE STRANYR_IDLST STRANYR_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "E-ME3" E-ME3 "p" 
                                              RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                              RETURNING RESU
               GO TO M-95
           END-IF.
           MOVE STR-NG TO W-SYM.
           CALL "DB_F_Close" USING
            BY REFERENCE STRANYR_IDLST STRANYR_PNAME1.
           MOVE W-EYM TO W-SNG W-ENG.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "D-DNG" D-DNG "p" 
                                              RETURNING RESU
               CALL "SD_Output" USING "A-PNG" A-PNG "p" 
                                              RETURNING RESU
               GO TO M-40
           END-IF.
           IF  W-EGET < 4
               COMPUTE W-ENEN = W-ENEN - 1
           END-IF.
           MOVE 4 TO W-EGET.
           COMPUTE W-SNEN = W-ENEN - 1.
           MOVE 5 TO W-SGET.
           IF  W-SYM > W-SNG
               MOVE W-SYM TO W-SNG
           END-IF.
           CALL "SD_Output" USING "D-DNG" D-DNG "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "A-PNG" A-PNG "p" 
                                              RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
           IF  W-SEN > 2
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
           MOVE ZERO TO W-SN1.
           IF  W-SN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF.
           IF  W-SN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
           IF  W-SNEN < W-SYY OR > W-EYY
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
           IF  W-SNG < W-SYM OR > W-EYM
               GO TO M-20
           END-IF.
           IF  W-SGET < 1 OR > 12
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF.
           MOVE ZERO TO W-EN1.
           IF  W-EN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF.
           IF  W-EN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
           IF  W-ENEN < W-SNEN OR > W-EYY
               GO TO M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF.
           IF  W-ENG < W-SNG
               GO TO M-30
           END-IF.
           IF  W-ENG < W-SYM OR > W-EYM
               GO TO M-30
           END-IF.
           IF  W-EGET < 1 OR > 12
               GO TO M-30
           END-IF.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-30
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF.
           IF  W-DMM = 9
               GO TO M-10
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-35
           END-IF.
       M-40.
           CALL "DB_F_Open" USING
            "INPUT" STRANYR_PNAME1 "SHARED" BY REFERENCE
            STRANYR_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" NSSWF_PNAME1 " " BY REFERENCE NSSWF_IDLST "0".
       M-45.
      *           READ STRANYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" STRANYR_PNAME1 BY REFERENCE STRANY-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_Scratch" USING STRANYR_PNAME1
               GO TO M-80
           END-IF.
           IF  W-JS2 = 0
               IF  STR-DC = 2 OR 3 OR 6 OR 8 OR 9
                   GO TO M-45
               END-IF
           END-IF.
           IF  W-JS2 = 1
               IF  STR-DC = 1 OR 2 OR 3 OR 6 OR 8 OR 9
                   GO TO M-45
               END-IF
           END-IF.
           IF  W-JS2 = 2
               IF  STR-DC NOT = 1
                   GO TO M-45
               END-IF
           END-IF.
           IF  W-SEN = 1
               IF  STR-BC3 = 30
                   GO TO M-45
               END-IF
           END-IF.
           IF  W-SEN = 2
               IF  STR-BC3 NOT = 30
                   GO TO M-45
               END-IF
           END-IF.
           IF  ZERO = STR-SU(01) AND STR-SU(02) AND STR-SU(03) AND
                     STR-SU(04) AND STR-SU(05) AND STR-SU(06) AND
                     STR-SU(07) AND STR-SU(08) AND STR-SU(09) AND
                     STR-SU(10)
               GO TO M-45.
           MOVE ZERO TO NSSW-R.
           MOVE STR-HCD TO NSS-HCD.
           MOVE STR-SC TO NSS-SC.
           MOVE STR-TCD TO NSS-TCD.
           MOVE STR-NG TO NSS-NG.
           MOVE STR-BC3 TO NSS-BC3.
           IF  STR-DC = 0 OR 4 OR 7
               MOVE STR-SU(01) TO NSS-SU(01)
               MOVE STR-SU(02) TO NSS-SU(02)
               MOVE STR-SU(03) TO NSS-SU(03)
               MOVE STR-SU(04) TO NSS-SU(04)
               MOVE STR-SU(05) TO NSS-SU(05)
               MOVE STR-SU(06) TO NSS-SU(06)
               MOVE STR-SU(07) TO NSS-SU(07)
               MOVE STR-SU(08) TO NSS-SU(08)
               MOVE STR-SU(09) TO NSS-SU(09)
               MOVE STR-SU(10) TO NSS-SU(10)
           ELSE
               COMPUTE NSS-SU(01) = -1 * STR-SU(01)
               COMPUTE NSS-SU(02) = -1 * STR-SU(02)
               COMPUTE NSS-SU(03) = -1 * STR-SU(03)
               COMPUTE NSS-SU(04) = -1 * STR-SU(04)
               COMPUTE NSS-SU(05) = -1 * STR-SU(05)
               COMPUTE NSS-SU(06) = -1 * STR-SU(06)
               COMPUTE NSS-SU(07) = -1 * STR-SU(07)
               COMPUTE NSS-SU(08) = -1 * STR-SU(08)
               COMPUTE NSS-SU(09) = -1 * STR-SU(09)
               COMPUTE NSS-SU(10) = -1 * STR-SU(10)
           END-IF.
           COMPUTE NSS-TSU = NSS-SU(01) + NSS-SU(02) + NSS-SU(03) +
                             NSS-SU(04) + NSS-SU(05) + NSS-SU(06) +
                             NSS-SU(07) + NSS-SU(08) + NSS-SU(09) +
                             NSS-SU(10).
      *           WRITE NSSW-R.
      *//////////////////////
           CALL "DB_Insert" USING
            NSSWF_PNAME1 NSSWF_LNAME NSSW-R RETURNING RET.
           GO TO M-45.
       M-80.
           CALL "DB_F_Close" USING
            BY REFERENCE STRANYR_IDLST STRANYR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NSSWF_IDLST NSSWF_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" M-DATE_PNAME1 "SHARED" BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
       M-85.
           MOVE "01" TO DATE-KEY.
      *           READ M-DATE INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" M-DATE_PNAME1 BY REFERENCE DATE-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                         RETURNING RESU
               GO TO M-95
           END-IF.
           MOVE W-SNGS TO D-SSNG.
           MOVE W-ENGS TO D-ESNG.
      *           REWRITE DATE-R INVALID KEY
      *//////////////////////
           CALL "DB_Update" USING
            M-DATE_PNAME1 M-DATE_LNAME DATE-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" 
                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                         RETURNING RESU
           END-IF.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
       M-95.
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSG410.
      ****************************************************
      *****     �ۗL�E����`�c���t�@�C���@�쐬     *****
      ****************************************************
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
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-KIN.
             03  W-HKIN       PIC  9(010).
             03  W-UKIN       PIC  9(010).
           02  W-TCD          PIC  9(004).
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-EC           PIC  9(001).
           02  W-FILE         PIC  X(013).
      *
           COPY LIBFDD.
           COPY LSUKET.
      *FD  ZD-F
       01  ZD-F_TSG410.
           02  ZD-F_PNAME1    PIC  X(003)  VALUE "ZDF".
           02  F              PIC  X(001).
           02  ZD-F_LNAME     PIC  X(011)  VALUE "ZD-F_TSG410".
           02  F              PIC  X(001).
           02  ZD-F_KEY1      PIC  X(100)  VALUE SPACE.
           02  ZD-F_KEY2      PIC  X(100)  VALUE SPACE.
           02  ZD-F_SORT      PIC  X(100)  VALUE SPACE.
           02  ZD-F_IDLST     PIC  X(100)  VALUE SPACE.
           02  ZD-F_RES       USAGE  POINTER.
       01  ZD-R.
           02  ZD-NO          PIC  9(002).
           02  ZD-KEY         PIC  9(004).
           02  ZD-KIN         PIC S9(010).
           02  ZD-NG          PIC  9(004).
           02  F              PIC  X(001).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(024) VALUE
                 "������������������������������������������������".
           02  FILLER  PIC  N(024) VALUE
                 "������������������������������������������������".
           02  FILLER  PIC  N(024) VALUE
                 "�������@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@������".
           02  FILLER  PIC  N(024) VALUE
                 "�������@�@�ۗL�E���c���t�@�C���@�쐬�@�@������".
           02  FILLER  PIC  N(024) VALUE
                 "�������@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@������".
           02  FILLER  PIC  N(024) VALUE
                 "������������������������������������������������".
           02  FILLER  PIC  N(024) VALUE
                 "������������������������������������������������".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ż  ***".
             03  E-ME2   PIC  X(023) VALUE
                  "***  ZDF WRITE �װ  ***".
             03  E-ME71.
               04  FILLER  PIC  X(013).
               04  FILLER  PIC  N(021) VALUE
                     "�I�[�o�[�t���[�A�̈���g�����A�e�m�b�{�ĊJ".
             03  E-ME78  PIC  N(002) VALUE  "�A��".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "336" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "48" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "48" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "48" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "48" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "48" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "191" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "191" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "23" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME71" " " "24" "0" "55" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME71" "X" "24" "1" "13" " " "E-ME71" RETURNING RESU.
       CALL "SD_From" USING 
            "01E-ME71" BY REFERENCE W-FILE "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME71" "N" "24" "15" "42" "01E-ME71" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME71" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME78" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" " " "24" "0" "80" "E-ME99" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-CL" "X" "24" "1" "40" " " "E-CL" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-CL" "X" "24" "41" "40" "01E-CL" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           INITIALIZE W-DATA.
           COPY LIBCPR.
           MOVE D-NTNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO UKET-F_PNAME1.
           CALL "DB_F_Open" USING
            "EXTEND" ZD-F_PNAME1 " " BY REFERENCE ZD-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" UKET-F_PNAME1 " " BY REFERENCE UKET-F_IDLST "0".
       M-10.
      *           READ UKET-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" UKET-F_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  UT-SKC = 19 OR 50 OR 60 OR 90
               GO TO M-10
           END-IF.
       M-15.
           MOVE UT-TCD TO W-TCD.
           MOVE ZERO TO W-KIN.
       M-20.
           ADD UT-KIN TO W-UKIN.
           IF  UT-SKC = ZERO
               ADD UT-KIN TO W-HKIN
           END-IF.
       M-25.
      *           READ UKET-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" UKET-F_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  UT-SKC = 19 OR 50 OR 60 OR 90
               GO TO M-25
           END-IF
           IF  UT-TCD = W-TCD
               GO TO M-20
           END-IF
           PERFORM S-05 THRU S-20.
           IF  W-EC = 9
               GO TO M-95
           END-IF
           GO TO M-15.
       M-50.
           PERFORM S-05 THRU S-20.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE ZD-F_IDLST ZD-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-F_IDLST UKET-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           IF  W-HKIN = ZERO
               GO TO S-10
           END-IF
           MOVE ZERO TO ZD-R.
           MOVE 31 TO ZD-NO.
           MOVE W-TCD TO ZD-KEY.
           MOVE W-HKIN TO ZD-KIN.
           MOVE W-NGS TO ZD-NG.
      *           WRITE ZD-R.
      *//////////////
           CALL "DB_Insert" USING
            ZD-F_PNAME1 ZD-F_LNAME ZD-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               MOVE 1 TO W-EC
               GO TO S-15
           END-IF.
       S-10.
           IF  W-UKIN = ZERO
               GO TO S-20
           END-IF
           MOVE ZERO TO ZD-R.
           MOVE 32 TO ZD-NO.
           MOVE W-TCD TO ZD-KEY.
           MOVE W-UKIN TO ZD-KIN.
           MOVE W-NGS TO ZD-NG.
      *           WRITE ZD-R.
      *//////////////
           CALL "DB_Insert" USING
            ZD-F_PNAME1 ZD-F_LNAME ZD-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               MOVE 2 TO W-EC
               GO TO S-15
           END-IF
           GO TO S-20.
       S-15.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-EC
               GO TO S-20
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE ZD-F_IDLST ZD-F_PNAME1.
           MOVE "ZDF          " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" ZD-F_PNAME1 " " BY REFERENCE ZD-F_IDLST "0".
           IF  W-EC = 1
               GO TO S-05
           END-IF
           GO TO S-10.
       S-20.
           EXIT.

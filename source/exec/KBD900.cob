       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBD900.
      *********************************************************
      *    PROGRAM         :  �w���x���C�����́@�@�@�@�@�@    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCBD90                          *
      *        �ύX�@�@�@  :  62/06/05                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(021) VALUE
                "�������@�@�w���@�x���C���@���X�g�@�@������".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "���@�@�t".
           02  F              PIC  X(008) VALUE "   ���� ".
           02  F              PIC  N(010) VALUE
                "�d�@�@���@�@��@�@��".
           02  F              PIC  X(022) VALUE SPACE.
           02  F              PIC  N(006) VALUE "�`�[���@�s�@".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "�x���敪".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "���@�@�z".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "�@�����".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "���@�@�v".
       01  W-P.
           02  P-DATE         PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-SCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-SNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-DNO          PIC  9(006).
           02  P-V            PIC  X(001).
           02  P-GNO          PIC  9(001).
           02  F              PIC  X(005).
           02  P-SC           PIC  9(001).
           02  P-KIN          PIC --,---,---,--9.
           02  P-SHZ          PIC ---,---,--9.
           02  P-KINT         PIC --,---,---,--9.
       01  W-R.
           02  WR-DC          PIC  9(002).
           02  WR-DCD  REDEFINES WR-DC.
             03  WR-DC1       PIC  9(001).
             03  WR-DC2       PIC  9(001).
           02  WR-DATE        PIC  9(008).
           02  WR-SCD         PIC  9(004).
           02  WR-JCD         PIC  9(006).
           02  WR-SU          PIC S9(007)V9(02).
           02  WR-T           PIC S9(006)V9(02).
           02  WR-KIN         PIC S9(008).
           02  WR-SHZ         PIC S9(007).
           02  WR-CD          PIC  9(006).
           02  WR-CDD  REDEFINES WR-CD.
             03  WR-CD1       PIC  9(002).
             03  WR-CD2       PIC  9(002).
             03  WR-CD3       PIC  9(002).
           02  WR-SJCD        PIC  9(006).
           02  WR-NNO         PIC  9(006).
           02  WR-FC          PIC  9(001).
           02  WR-YC          PIC  9(001).
           02  WR-TC          PIC  9(001).
           02  WR-SEC         PIC  9(001).
           02  WR-SC          PIC  9(001).
           02  WR-BSC         PIC  9(001).
           02  F              PIC  X(018).
           02  WR-KEY.
             03  WR-DNO       PIC  9(006).
             03  WR-GNO       PIC  9(001).
           02  WR-PCNT        PIC  9(001).
       01  W-ARD.
           02  W-RD    OCCURS   6  PIC  X(102).
       01  W-DATA.
           02  W-DATE         PIC  9(008).
           02  W-DATED REDEFINES W-DATE.
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
           02  W-NGPL  REDEFINES W-DATE.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-SCD          PIC  9(004).
           02  W-SC           PIC  9(001).
           02  W-DNO          PIC  9(006).
           02  W-L            PIC  9(002).
           02  W-LD           PIC  9(002).
           02  W-KINT         PIC S9(008).
           02  W-GD.
             03  W-GKIN       PIC S9(009).
             03  W-GSHZ       PIC S9(007).
             03  W-GKINT      PIC S9(009).
           02  W-DMM          PIC  9(001).
           02  CNT            PIC  9(001).
           02  CNTD           PIC  9(001).
           02  W-ACT          PIC  9(001) VALUE ZERO.
           02  W-EC           PIC  9(001) VALUE ZERO.
           02  W-LIST         PIC  9(001).
           02  W-PC           PIC  9(001) VALUE ZERO.
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-BNG          PIC  9(004).
           02  W-SCNAD.
             03  F            PIC  N(015) VALUE
                  "���@���@�@�U�@���@�@���؎�@�@".
             03  F            PIC  N(015) VALUE
                  "��@�`�@�@���|���E�@���̑����E".
           02  W-SCND  REDEFINES W-SCNAD.
             03  W-SCN   OCCURS   6  PIC  N(005).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKBNO.
           COPY LISM.
           COPY LSPF.
      *FD  JSS-F
       01  JSS-F_KBD900.
           02  JSS-F_PNAME1   PIC  X(004) VALUE "JSSF".
           02  F              PIC  X(001).
           02  JSS-F_LNAME    PIC  X(012) VALUE "JSS-F_KBD900".
           02  F              PIC  X(001).
           02  JSS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  JSS-F_KEY2     PIC  X(100) VALUE SPACE.
           02  JSS-F_SORT     PIC  X(100) VALUE SPACE.
           02  JSS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  JSS-F_RES      USAGE  POINTER.
       01  JSS-R.
           02  JS-DC          PIC  9(002).
           02  JS-DATE        PIC  9(008).
           02  JS-SCD         PIC  9(004).
           02  JS-JCD         PIC  9(006).
           02  JS-SU          PIC S9(007)V9(02).
           02  JS-T           PIC S9(006)V9(02).
           02  JS-KIN         PIC S9(008).
           02  JS-SHZ         PIC S9(007).
           02  JS-CD          PIC  9(006).
           02  JS-SJCD        PIC  9(006).
           02  JS-NNO         PIC  9(006).
           02  JS-FC          PIC  9(001).
           02  JS-YC          PIC  9(001).
           02  JS-TC          PIC  9(001).
           02  JS-SEC         PIC  9(001).
           02  JS-SC          PIC  9(001).
           02  JS-BSC         PIC  9(001).
           02  F              PIC  X(018).
           02  JS-KEY.
             03  JS-DNO       PIC  X(006).
             03  JS-GNO       PIC  9(001).
           02  JS-PCNT        PIC  9(001).
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
           02  FILLER  PIC  N(023) VALUE
                "����������������������������������������������".
           02  FILLER  PIC  N(023) VALUE
                "����������������������������������������������".
           02  FILLER  PIC  N(023) VALUE
                "�������@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@������".
           02  FILLER  PIC  N(023) VALUE
                "�������@�@�w���@�x���C���@���̓��X�g�@�@������".
           02  FILLER  PIC  N(023) VALUE
                "�������@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@������".
           02  FILLER  PIC  N(023) VALUE
                "����������������������������������������������".
           02  FILLER  PIC  N(023) VALUE
                "����������������������������������������������".
           02  FILLER  PIC  X(040) VALUE
                "�S��=1  ����\��=5  ��\���Ȃ�=9    ����".
           02  FILLER  PIC  X(022) VALUE
                "�m�F  OK=1 NO=9   ����".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  A-DNO   PIC  9(006).
           02  A-DATE  PIC  9(006).
           02  A-SCD   PIC  9(004).
           02  FILLER.
             03  A-SC    PIC  9(001).
             03  A-KIN   PIC S9(008).
             03  A-SHZ   PIC S9(007).
           02  A-LIST  PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SNA   PIC  N(024).
           02  D-DATA.
             03  D-SCN   PIC  N(005).
             03  D-KIN   PIC ZZZZZZZ9- .
             03  D-SHZ   PIC ZZZZZZ9- .
             03  D-KINT  PIC ZZZZZZZ9- .
           02  D-TD.
             03  FILLER  PIC ZZZZZZZZ9- .
             03  FILLER  PIC ZZZZZZZ9- .
             03  FILLER  PIC ZZZZZZZZ9- .
       01  C-SPC.
           02  FILLER.
             03  FILLER  PIC  X(012) VALUE "            ".
             03  FILLER  PIC  X(009) VALUE "         ".
             03  FILLER  PIC  X(008) VALUE "        ".
             03  FILLER  PIC  X(009) VALUE "         ".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(015) VALUE
                  "***  SM ż  ***".
             03  E-ME3   PIC  X(021) VALUE
                  "***  PROGRAM �װ  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  ���� �װ  ***".
             03  E-ME5   PIC  X(018) VALUE
                  "***  KBNOM ż  ***".
             03  E-ME6   PIC  X(027) VALUE
                  "***  KBNOM REWRITE �װ  ***".
             03  E-ME7   PIC  X(017) VALUE
                  "***  DATA ż  ***".
             03  E-ME8   PIC  X(024) VALUE
                  "***  JSSF WRITE �װ  ***".
             03  E-ME9   PIC  X(025) VALUE
                  "***  JSSF DELETE �װ  ***".
             03  E-ME10  PIC  X(026) VALUE
                  "***  JSSF REWRITE �װ  ***".
             03  E-ME11  PIC  X(021) VALUE
                  "***  ��� ����߮�  ***".
             03  E-ME72  PIC  N(013) VALUE
                  "�����X�V��A���͂��ĉ�����".
             03  E-ME78  PIC  N(002) VALUE "�A��".
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
            "C-MID" " " "0" "0" "384" " " " " RETURNING RESU.
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
            "08C-MID" "X" "14" "13" "40" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "22" "25" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "35" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "42" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DNO" "9" "5" "45" "6" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DNO" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DATE" "9" "6" "10" "6" "A-DNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DATE" BY REFERENCE W-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SCD" "9" "7" "10" "4" "A-DATE" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SCD" BY REFERENCE W-SCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-ACP" " " "W-L" "0" "16" "A-SCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SC" "9" "W-L" "5" "1" " " "05C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SC" BY REFERENCE W-SC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KIN" "S9" "W-L" "20" "8" "A-SC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KIN" BY REFERENCE WR-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHZ" "S9" "W-L" "31" "7" "A-KIN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHZ" BY REFERENCE WR-SHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-LIST" "9" "14" "48" "1" "05C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-LIST" BY REFERENCE W-LIST "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "42" "1" "A-LIST" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "113" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SNA" "N" "7" "24" "48" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SNA" BY REFERENCE S-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATA" " " "W-L" "0" "36" "D-SNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SCN" "N" "W-L" "7" "10" " " "D-DATA" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SCN" BY REFERENCE W-SCN(1) "10" "1" BY REFERENCE W-SC 10
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN" "ZZZZZZZ9-" "W-L" "20" "9" "D-SCN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-KIN" BY REFERENCE WR-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHZ" "ZZZZZZ9-" "W-L" "31" "8" "D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-SHZ" BY REFERENCE WR-SHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KINT" "ZZZZZZZ9-" "W-L" "42" "9" "D-SHZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-KINT" BY REFERENCE W-KINT "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TD" " " "16" "0" "29" "D-DATA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TD" "ZZZZZZZZ9-" "16" "19" "10" " " "D-TD"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-TD" BY REFERENCE W-GKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TD" "ZZZZZZZ9-" "16" "30" "9" "01D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-TD" BY REFERENCE W-GSHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-TD" "ZZZZZZZZ9-" "16" "41" "10" "02D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-TD" BY REFERENCE W-GKINT "9" "0" RETURNING RESU.
      *C-SPC
       CALL "SD_Init" USING 
            "C-SPC" " " "0" "0" "38" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-SPC" " " "W-L" "0" "38" " " "C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-SPC" "X" "W-L" "5" "12" " " "01C-SPC"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201C-SPC" "X" "W-L" "20" "9" "0101C-SPC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0301C-SPC" "X" "W-L" "31" "8" "0201C-SPC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0401C-SPC" "X" "W-L" "42" "9" "0301C-SPC" " "
            RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "334" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "334" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "21" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "18" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "18" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "27" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "17" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "24" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "25" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "26" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "21" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME72" "N" "24" "15" "26" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME72" " " RETURNING RESU.
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
       M-020.
           COPY LIBCPR.
           MOVE D-NBNG TO W-BNG.
           MOVE DATE-05R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" KBNO-M_PNAME1 " " BY REFERENCE KBNO-M_IDLST "1"
            "BNO-KEY" BY REFERENCE BNO-KEY.
           MOVE SPACE TO BNO-KEY.
           MOVE "02" TO BNO-KEYD.
      *           READ KBNO-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KBNO-M_PNAME1 BY REFERENCE KBNO-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KBNO-M_IDLST KBNO-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "I-O" JSS-F_PNAME1 "SHARED" BY REFERENCE JSS-F_IDLST "1"
            "JS-KEY" BY REFERENCE JS-KEY.
           CALL "SD_Screen_Output" USING "SCBD90" RETURNING RESU.
       M-040.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-ACT = 9
               GO TO M-700
           END-IF
           IF  W-ACT < 1 OR > 3
               GO TO M-040
           END-IF
           IF  W-ACT = 1
               GO TO M-160
           END-IF.
       M-100.
           CALL "SD_Screen_Output" USING "SCBD90" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
       M-120.
           CALL "SD_Accept" USING BY REFERENCE A-DNO "A-DNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-120
           END-IF
           MOVE ZERO TO JS-KEY.
           MOVE W-DNO TO JS-DNO.
      *           START JSS-F KEY NOT < JS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JSS-F_PNAME1 "JS-KEY" " NOT < " JS-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-120
           END-IF
      *           READ JSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSS-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-120
           END-IF
           IF  JS-DNO NOT = W-DNO
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-120
           END-IF
           IF  JS-DC NOT = 30
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-120
           END-IF
           PERFORM S-40 THRU S-65.
           IF  W-ACT = 3
               GO TO M-480
           END-IF.
       M-160.
           MOVE ZERO TO W-R.
           IF  W-ACT = 1
               CALL "SD_Screen_Output" USING "SCBD90" RETURNING RESU
               CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-DATE "A-DATE" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 1
                   GO TO M-040
               ELSE
                   GO TO M-120
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-160
           END-IF
           IF (W-GET < 1 OR > 12) OR (W-PEY < 1 OR > 31)
               GO TO M-160
           END-IF
           IF  W-NGS NOT = W-BNG
               GO TO M-160
           END-IF
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       M-180.
           IF  W-ACT = 1
               CALL "SD_Screen_Output" USING "SCBD90" RETURNING RESU
               MOVE ZERO TO W-ARD W-SCD
               COMPUTE W-DNO = BNO-DNO1 + 1
               CALL "SD_Output" USING
                "A-ACT" A-ACT "p" RETURNING RESU
               CALL "SD_Output" USING
                "A-DNO" A-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "A-DATE" A-DATE "p" RETURNING RESU
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-SCD "A-SCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-160
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-180
           END-IF
           MOVE W-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-180
           END-IF
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
           MOVE 9 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE ZERO TO CNT.
       M-200.
           ADD 1 TO CNT.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT = 7
               MOVE W-L TO W-LD
               GO TO M-420
           END-IF
           MOVE W-RD(CNT) TO W-R.
       M-220.
           CALL "SD_Accept" USING BY REFERENCE A-SC "A-SC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-360
           END-IF
           IF  ESTAT = ADV
               MOVE CNT TO CNTD
               MOVE W-L TO W-LD
               GO TO M-400
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-220
           END-IF
           IF  W-SC < 1 OR > 6
               GO TO M-220
           END-IF
           MOVE W-SC TO WR-SC.
           CALL "SD_Output" USING "D-SCN" D-SCN "p" RETURNING RESU.
       M-240.
           CALL "SD_Accept" USING BY REFERENCE A-KIN "A-KIN" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-220
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-240
           END-IF
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
       M-260.
           CALL "SD_Accept" USING BY REFERENCE A-SHZ "A-SHZ" "S9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-240
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-260
           END-IF
           COMPUTE W-KINT = WR-KIN + WR-SHZ.
           CALL "SD_Output" USING "D-SHZ" D-SHZ "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KINT" D-KINT "p" RETURNING RESU.
           IF  ZERO = WR-KIN AND WR-SHZ
               GO TO M-260
           END-IF.
       M-300.
           MOVE 30 TO WR-DC.
           MOVE W-DATE TO WR-DATE.
           MOVE W-SCD TO WR-SCD.
           MOVE W-DNO TO WR-DNO.
           MOVE CNT TO WR-GNO.
           MOVE W-R TO W-RD(CNT).
           GO TO M-200.
       M-360.
           IF  CNT = 1
               GO TO M-180
           END-IF
           SUBTRACT 1 FROM CNT.
           SUBTRACT 1 FROM W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE W-RD(CNT) TO W-R.
           GO TO M-260.
       M-400.
           CALL "SD_Output" USING "C-SPC" C-SPC "p" RETURNING RESU.
           MOVE ZERO TO W-R.
           MOVE W-R TO W-RD(CNTD)
           ADD 1 TO CNTD.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNTD NOT = 7
               GO TO M-400
           END-IF.
       M-420.
           MOVE ZERO TO CNTD W-GD.
       M-440.
           ADD 1 TO CNTD.
           IF  CNTD NOT = 7
               MOVE W-RD(CNTD) TO W-R
               ADD WR-KIN TO W-GKIN
               ADD WR-SHZ TO W-GSHZ
               COMPUTE W-KINT = WR-KIN + WR-SHZ
               ADD W-KINT TO W-GKINT
               GO TO M-440
           END-IF
           CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU.
           MOVE W-LD TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-480.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3
                   GO TO M-120
               ELSE
                   GO TO M-360
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-480
           END-IF
           IF  W-DMM = 9
               MOVE ZERO TO W-R
               IF  W-ACT = 1
                   GO TO M-180
               ELSE
                   GO TO M-100
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-480
           END-IF.
      ****************   �t�@�C���@�o�^   *****************
       M-500.
           IF  W-ACT NOT = 1
               GO TO M-560
           END-IF
           MOVE ZERO TO CNT.
       M-520.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           ADD 1 TO CNT.
           IF  CNT = 7
               GO TO M-680
           END-IF
           MOVE W-RD(CNT) TO W-R.
           IF  WR-DATE = ZERO
               GO TO M-680
           END-IF
           MOVE ZERO TO JSS-R.
           MOVE W-R TO JSS-R.
      *           WRITE JSS-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            JSS-F_PNAME1 JSS-F_LNAME JSS-R RETURNING RET.
           IF  RET = 1
               GO TO M-530
           END-IF
           GO TO M-540.
       M-530.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME8" E-ME8 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT = "24"
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME72" E-ME72 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           MOVE 5 TO W-EC.
           GO TO M-680.
       M-540.
           IF  CNT NOT = 1
               GO TO M-520
           END-IF
           MOVE W-DNO TO BNO-DNO1.
      *           REWRITE KBNO-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KBNO-M_PNAME1 KBNO-M_LNAME KBNO-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 5 TO W-EC
               GO TO M-680
           END-IF
           GO TO M-520.
      ****************   �t�@�C���@�C��   *****************
       M-560.
           IF  W-ACT NOT = 2
               GO TO M-660
           END-IF
           MOVE ZERO TO CNT.
       M-580.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           ADD 1 TO CNT.
           IF  CNT = 7
               GO TO M-680
           END-IF
           MOVE W-RD(CNT) TO W-R.
           IF  WR-DATE = ZERO
               GO TO M-620
           END-IF
           MOVE WR-KEY TO JS-KEY.
      *           READ JSS-F INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JSS-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-600
           END-IF
           MOVE W-R TO JSS-R.
           MOVE 0 TO JS-PCNT.
      *           REWRITE JSS-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JSS-F_PNAME1 JSS-F_LNAME JSS-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 5 TO W-EC
               GO TO M-680
           END-IF
           GO TO M-580.
       M-600.
           MOVE ZERO TO JSS-R.
           MOVE W-R TO JSS-R.
      *           WRITE JSS-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            JSS-F_PNAME1 JSS-F_LNAME JSS-R RETURNING RET.
           IF  RET = 1
               GO TO M-610
           END-IF
           GO TO M-580.
       M-610.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME8" E-ME8 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT = "24"
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME72" E-ME72 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           MOVE 5 TO W-EC.
           GO TO M-680.
       M-620.
           MOVE W-DNO TO JS-DNO.
           MOVE CNT TO JS-GNO.
      *           START JSS-F KEY NOT < JS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JSS-F_PNAME1 "JS-KEY" " NOT < " JS-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-680
           END-IF
           PERFORM S-80 THRU S-85.
           GO TO M-680.
      ****************   �t�@�C���@�폜   *****************
       M-660.
           MOVE ZERO TO JS-KEY.
           MOVE W-DNO TO JS-DNO.
      *           START JSS-F KEY NOT < JS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JSS-F_PNAME1 "JS-KEY" " NOT < " JS-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-680
           END-IF
           PERFORM S-80 THRU S-85.
      *******************************************************
       M-680.
           IF  W-EC = 5
               GO TO M-700
           END-IF
           MOVE ZERO TO W-R
           IF  W-ACT = 1
               GO TO M-180
           END-IF
           GO TO M-100.
      *******************    ��@�@�\    ***********************
       M-700.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-720.
           CALL "SD_Accept" USING BY REFERENCE A-LIST "A-LIST" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-720
           END-IF
           IF  W-LIST NOT = 1 AND 5 AND 9
               GO TO M-720
           END-IF.
       M-740.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-720
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-740
           END-IF
           IF  W-DMM = 9
               GO TO M-700
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-740
           END-IF
           IF  W-LIST = 9
               GO TO M-980
           END-IF
      *
           CALL "DB_F_Close" USING
            BY REFERENCE JSS-F_IDLST JSS-F_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" JSS-F_PNAME1 "SHARED" BY REFERENCE JSS-F_IDLST "1"
            "JS-KEY" BY REFERENCE JS-KEY.
       M-760.
      *           READ JSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSS-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-980
           END-IF
           IF  JS-DC NOT = 30
               GO TO M-760
           END-IF
           IF  W-LIST = 5
               IF  JS-PCNT NOT = 0
                   GO TO M-760
               END-IF
           END-IF
           MOVE 1 TO W-PC.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
       M-780.
           MOVE JS-DNO TO W-DNO.
           MOVE ZERO TO W-GD CNT.
           MOVE JS-DATE TO W-DATE.
           MOVE JS-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE "�@�����@�d����}�X�^�[�@�Ȃ��@����" TO S-NAME
           END-IF.
       M-800.
           COMPUTE W-KINT = JS-KIN + JS-SHZ.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-SNA.
           ADD 1 TO CNT.
           IF  CNT = 1
               MOVE W-NGPS TO P-DATE
               MOVE S-KEY TO P-SCD
               MOVE S-NAME TO P-SNA
           END-IF
           MOVE JS-DNO TO P-DNO.
           MOVE "-" TO P-V.
           MOVE JS-GNO TO P-GNO.
           MOVE JS-SC TO P-SC.
           MOVE JS-KIN TO P-KIN.
           MOVE JS-SHZ TO P-SHZ.
           MOVE W-KINT TO P-KINT.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE W-NGPS TO P-DATE
               MOVE S-KEY TO P-SCD
               MOVE S-NAME TO P-SNA
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD JS-KIN TO W-GKIN.
           ADD JS-SHZ TO W-GSHZ.
           ADD W-KINT TO W-GKINT.
           ADD 1 TO JS-PCNT.
      *           REWRITE JSS-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JSS-F_PNAME1 JSS-F_LNAME JSS-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-840.
      *           READ JSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSS-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-860
           END-IF
           IF  JS-DC NOT = 30
               GO TO M-840
           END-IF
           IF  W-LIST = 5
               IF  JS-PCNT NOT = 0
                   GO TO M-840
               END-IF
           END-IF
           IF  JS-DNO = W-DNO
               GO TO M-800
           END-IF
           IF  CNT NOT = 1
               PERFORM S-30 THRU S-35
           END-IF
           GO TO M-780.
       M-860.
           IF  CNT NOT = 1
               PERFORM S-30 THRU S-35
           END-IF.
      *******************   �d�@�m�@�c   ***********************
       M-980.
           CALL "DB_F_Close" USING
            BY REFERENCE KBNO-M_IDLST KBNO-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSS-F_IDLST JSS-F_PNAME1.
           IF  W-PC NOT = ZERO
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *******************    ���@�o�@��   ***********************
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
      ******************   �s�n�s�`�k�@��\   ***********************
       S-30.
           MOVE SPACE TO W-P.
           MOVE "�@�@�@�@�@�@�������@�s�n�s�`�k�@�������@" TO P-SNA.
           MOVE W-GKIN TO P-KIN.
           MOVE W-GSHZ TO P-SHZ.
           MOVE W-GKINT TO P-KINT.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-35.
           EXIT.
      **********   �v�n�q�j�@�������݁@�E�@��ʕ\��   ***********
       S-40.
           CALL "SD_Screen_Output" USING "SCBD90" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DNO" A-DNO "p" RETURNING RESU.
           MOVE ZERO TO W-ARD CNT W-GD.
           MOVE 9 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       S-45.
           ADD 1 TO CNT.
           IF  CNT = 7
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-65
           END-IF
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE ZERO TO W-R.
           MOVE JSS-R TO W-R.
           ADD WR-KIN TO W-GKIN.
           ADD WR-SHZ TO W-GSHZ.
           COMPUTE W-KINT = WR-KIN + WR-SHZ.
           ADD W-KINT TO W-GKINT.
           MOVE W-R TO W-RD(CNT).
           IF  CNT NOT = 1
               GO TO S-55
           END-IF
           MOVE WR-DATE TO W-DATE.
           MOVE WR-SCD TO W-SCD.
           MOVE W-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE "�@�����@�d����}�X�^�[�@�Ȃ��@����" TO S-NAME
           END-IF
           CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SCD" A-SCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
       S-55.
           MOVE WR-SC TO W-SC.
           CALL "SD_Output" USING "A-SC" A-SC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-DATA" D-DATA "p" RETURNING RESU.
       S-60.
      *           READ JSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSS-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU
               GO TO S-65
           END-IF
           IF  JS-DNO = W-DNO
               GO TO S-45
           END-IF
           CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU.
       S-65.
           EXIT.
      *****************   ��@��   ********************
       S-80.
      *           READ JSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSS-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-85
           END-IF
           IF  JS-DNO NOT = W-DNO
               GO TO S-85
           END-IF
      *           DELETE JSS-F INVALID KEY
      *///////////////
           CALL "DB_Delete" USING JSS-F_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 5 TO W-EC
               GO TO S-85
           END-IF
           GO TO S-80.
       S-85.
           EXIT.

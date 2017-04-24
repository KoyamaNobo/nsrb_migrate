       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG830.
      *******************************************************************
      *    PROGRAM         :  請求書発行予定　問合せ　                  *
      *    PRINTER TYPE    :  JIPS                                      *
      *    SCREEN          :  ******                                    *
      *    JS-SIGN         :  0=予定 , 1=月末ﾁｪｯｸ                       *
      *******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-FILE             PIC  X(013).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-DATE         PIC  9(008).
           02  W-NGPD.
             03  W-NEND       PIC  9(004).
             03  W-GETD       PIC  9(002).
             03  W-PEYD       PIC  9(002).
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-END          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
      *FD  NGPF
       01  NGPF_HKG830.
           02  NGPF_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F             PIC  X(001).
           02  NGPF_LNAME    PIC  X(011)  VALUE "NGPF_HKG830".
           02  F             PIC  X(001).
           02  NGPF_KEY1     PIC  X(100)  VALUE SPACE.
           02  NGPF_KEY2     PIC  X(100)  VALUE SPACE.
           02  NGPF_SORT     PIC  X(100)  VALUE SPACE.
           02  NGPF_IDLST    PIC  X(100)  VALUE SPACE.
           02  NGPF_RES      USAGE  POINTER.
       01  NGP-R.
           02  NGP-DATE       PIC  9(008).
           02  NGP-NGP   REDEFINES NGP-DATE.
             03  NGP-NG.
               04  NGP-NEN    PIC  9(004).
               04  NGP-GET    PIC  9(002).
             03  NGP-PEY      PIC  9(002).
           02  NGP-TCD        PIC  9(004).
           02  F              PIC  X(052).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　請求書　発行予定　　＊＊＊".
           02  FILLER.
             03  FILLER  PIC  N(005) VALUE "発行予定日".
             03  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER  PIC  N(007) VALUE "得　意　先　名".
           02  FILLER.
             03  FILLER  PIC  N(001) VALUE "年".
             03  FILLER  PIC  N(001) VALUE "月".
             03  FILLER  PIC  N(001) VALUE "日".
           02  FILLER  PIC  X(039) VALUE
                "NEXT=ﾘﾀｰﾝ  日付入力=ｆ･10  終了=ｆ･9   ".
       01  C-ACP.
           02  FILLER.
             03  A-NEN   PIC  9(004).
             03  A-GET   PIC  9(002).
             03  A-PEY   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-NGP.
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  N(001) VALUE "年".
               04  FILLER  PIC Z9.
               04  FILLER  PIC  N(001) VALUE "月".
               04  FILLER  PIC Z9.
               04  FILLER  PIC  N(001) VALUE "日".
             03  D-DATA.
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  N(026).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
           COPY LSSEM.
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
            "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "117" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "20" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" " " "3" "0" "28" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID" "N" "3" "7" "10" " " "02C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID" "X" "3" "21" "4" "0102C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302C-MID" "N" "3" "26" "14" "0202C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" " " "4" "0" "6" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103C-MID" "N" "4" "9" "2" " " "03C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203C-MID" "N" "4" "13" "2" "0103C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0303C-MID" "N" "4" "17" "2" "0203C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "23" "35" "39" "03C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "4" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "4" "5" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEND "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "4" "11" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GETD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PEY" "9" "4" "15" "2" "A-GET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PEY" BY REFERENCE W-PEYD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "73" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "70" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "W-L" "0" "70" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NGP" " " "W-L" "0" "14" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NGP" "9" "W-L" "5" "4" " " "D-NGP" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NGP" BY REFERENCE NGP-NEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NGP" "N" "W-L" "9" "2" "01D-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-NGP" "Z9" "W-L" "11" "2" "02D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NGP" BY REFERENCE NGP-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-NGP" "N" "W-L" "13" "2" "03D-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-NGP" "Z9" "W-L" "15" "2" "04D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "05D-NGP" BY REFERENCE NGP-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-NGP" "N" "W-L" "17" "2" "05D-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATA" " " "W-L" "0" "56" "D-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DATA" "9" "W-L" "21" "4" " " "D-DATA" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DATA" BY REFERENCE NGP-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DATA" "N" "W-L" "26" "52" "01D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DATA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "17" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "17" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
      *
           COPY LSSEM_P.
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
       M-07.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           IF  JS-SIGN = 0
               GO TO M-10
           END-IF
           COPY LIBCPR.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       M-10.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO NGPF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NGPF_PNAME1 " " BY REFERENCE NGPF_IDLST "0".
      *           READ NGPF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" NGPF_PNAME1 BY REFERENCE NGP-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           IF  JS-SIGN = 1
               IF  NGP-NG NOT = W-NG
                   CALL "DB_F_Close" USING
                    BY REFERENCE NGPF_IDLST NGPF_PNAME1
                   GO TO M-95
               END-IF
           END-IF.
           GO TO M-20.
       M-15.
           CALL "DB_F_Close" USING BY REFERENCE NGPF_IDLST NGPF_PNAME1.
           IF  JS-SIGN = 0
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-95.
       M-20.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-25.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 22
               GO TO M-35
           END-IF
           MOVE NGP-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "　＊得意先なし＊" TO T-NAME
           END-IF
           IF  NGP-NGP NOT = W-DATE
               MOVE NGP-NGP TO W-DATE
               CALL "SD_Output" USING "D-NGP" D-NGP "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-DATA" D-DATA "p" RETURNING RESU.
       M-30.
      *           READ NGPF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" NGPF_PNAME1 BY REFERENCE NGP-R " " RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               GO TO M-35
           END-IF
           GO TO M-25.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT = HTB
               IF  W-END = 1
                   CALL "DB_F_Close" USING
                    BY REFERENCE NGPF_IDLST NGPF_PNAME1
                   CALL "DB_F_Close" USING
                    BY REFERENCE T-M_IDLST T-M_PNAME1
                   GO TO M-07
               ELSE
                   CALL "SD_Output" USING
                    "C-CLEAR" C-CLEAR "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "C-MID" C-MID "p" RETURNING RESU
                   MOVE 3 TO W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
                   MOVE ZERO TO W-DATE
                   GO TO M-25
               END-IF
           END-IF
           IF  ESTAT NOT = ADV
               GO TO M-35
           END-IF.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-40
           END-IF.
       M-45.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-40
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-45
           END-IF.
       M-50.
           CALL "SD_Accept" USING BY REFERENCE A-PEY "A-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-45
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-50
           END-IF
      *
           CALL "DB_F_Close" USING BY REFERENCE NGPF_IDLST NGPF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NGPF_PNAME1 " " BY REFERENCE NGPF_IDLST "0".
       M-55.
      *           READ NGPF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" NGPF_PNAME1 BY REFERENCE NGP-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-40
           END-IF
           IF  NGP-DATE < W-DATE
               GO TO M-55
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE ZERO TO W-DATE.
           GO TO M-25.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE NGPF_IDLST NGPF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

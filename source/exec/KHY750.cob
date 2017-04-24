       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHY750.
      *********************************************************
      *    PROGRAM         :  工品年間販売計画・実績問合せ    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCKY75                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-PEY.
             03  W-BP         PIC  9(002).
             03  W-HP         PIC  9(002).
           02  W-DNG.
             03  W-DNEN       PIC  9(004).
             03  W-DGET       PIC  9(002).
           02  W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SGET       PIC  9(002).
           02  W-ENG.
             03  W-ENEN       PIC  9(004).
             03  W-EGET       PIC  9(002).
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGD   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-UK           PIC S9(008).
           02  W-MEI.
             03  W-AKIN.
               04  W-MKIN   OCCURS   4.
                 05  W-KIND  OCCURS  12.
                   06  W-KIN  PIC S9(009).
             03  WK-AKIN.
               04  WK-KIND  OCCURS   4.
                 05  WK-KIN   PIC S9(009).
             03  WS-AKIN.
               04  WS-KIND  OCCURS   4.
                 05  WS-KIN   PIC S9(009).
             03  WT-AKIN.
               04  WT-KIND  OCCURS   4.
                 05  WT-KIN   PIC S9(009).
           02  W-INV.
             03  WB-C         PIC  9(001).
             03  WH-C         PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-C            PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKHT2.
      *FD  KBHKF
       01  KBHKF_KHY750.
           02  KBHKF_PNAME1   PIC  X(005) VALUE "KBHKF".
           02  F              PIC  X(001).
           02  KBHKF_LNAME    PIC  X(012) VALUE "KBHKF_KHY750".
           02  F              PIC  X(001).
           02  KBHKF_KEY1     PIC  X(100) VALUE SPACE.
           02  KBHKF_KEY2     PIC  X(100) VALUE SPACE.
           02  KBHKF_SORT     PIC  X(100) VALUE SPACE.
           02  KBHKF_IDLST    PIC  X(100) VALUE SPACE.
           02  KBHKF_RES      USAGE  POINTER.
       01  KBHK-R.
           02  KBHK-KEY.
             03  KBHK-NEN     PIC  9(004).
             03  KBHK-BMN     PIC  9(002).
           02  KBHK-AKIN.
             03  KBHK-KIND  OCCURS  12.
               04  KBHK-KIN   PIC S9(009).
           02  F              PIC  X(014).
       77  F                  PIC  X(001).
      *FD  KHTMYR
       01  KHTMYR_KHY750.
           02  KHTMYR_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  KHTMYR_LNAME   PIC  X(013) VALUE "KHTMYR_KHY750".
           02  F              PIC  X(001).
           02  KHTMYR_KEY1    PIC  X(100) VALUE SPACE.
           02  KHTMYR_SORT    PIC  X(100) VALUE SPACE.
           02  KHTMYR_IDLST   PIC  X(100) VALUE SPACE.
           02  KHTMYR_RES     USAGE  POINTER.
       01  KHTY-R.
           02  KHTY-NG.
             03  KHTY-NEN     PIC  9(004).
             03  KHTY-GET     PIC  9(002).
           02  KHTY-KEYD.                                               ｺｰﾄﾞ
             03  KHTY-YC      PIC  9(002).                              ﾖｳﾄｸﾌﾞﾝ
             03  KHTY-NC      PIC  9(001).
             03  KHTY-KEY.
               04  KHTY-KEY1  PIC  X(002).
               04  KHTY-KEY2  PIC  9(003).
           02  KHTY-UKIN      PIC S9(008).                              ｳﾘｱｹﾞｶﾞｸ
           02  KHTY-NKIN      PIC S9(007).                              ﾈﾋﾞｷｶﾞｸ
           02  F              PIC  X(035).
       77  F                  PIC  X(001).
      *FD  URIR-F
       01  URIR-F_KHY750.
           02  URIR-F_PNAME1  PIC  X(005) VALUE "URIRF".
           02  F              PIC  X(001).
           02  URIR-F_LNAME   PIC  X(013) VALUE "URIR-F_KHY750".
           02  F              PIC  X(001).
           02  URIR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  URIR-F_SORT    PIC  X(100) VALUE SPACE.
           02  URIR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  URIR-F_RES     USAGE  POINTER.
       01  URIR-R.
           02  F              PIC  X(007).
           02  UR-PEY         PIC  9(002).
           02  F              PIC  X(033).
           02  UR-YC          PIC  9(002).
           02  F              PIC  X(084).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　工品販売計画・実績　問合せ　　＊＊＊".
       01  C-ACP.
           02  A-NEN   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-PEY.
             03  01D-PEY  PIC  Z(002).
             03  02D-PEY  PIC  Z(002).
           02  D-MEI.
             03  01D-MEI  PIC ----,---,--- .
             03  02D-MEI  PIC ----,---,--- .
             03  03D-MEI  PIC ----,---,--- .
             03  04D-MEI  PIC ----,---,--- .
           02  D-KAMI.           .
             03  01D-KAMI PIC ----,---,--- .
             03  02D-KAMI PIC ----,---,--- .
             03  03D-KAMI PIC ----,---,--- .
             03  04D-KAMI PIC ----,---,--- .
           02  D-SIMO.           .
             03  01D-SIMO PIC ----,---,--- .
             03  02D-SIMO PIC ----,---,--- .
             03  03D-SIMO PIC ----,---,--- .
             03  04D-SIMO PIC ----,---,--- .
           02  D-KEI.           .
             03  01D-KEI  PIC ----,---,--- .
             03  02D-KEI  PIC ----,---,--- .
             03  03D-KEI  PIC ----,---,--- .
             03  04D-KEI  PIC ----,---,--- .
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
            "C-MID" " " "0" "0" "46" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "20" "46" " " "C-MID"  RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "3" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "1" "9" "2" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "73" "1" "A-NEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "196" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PEY" " " "W-L" "0" "4" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-PEY" "Z" "W-L" "41" "2" " " "D-PEY"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-PEY" BY REFERENCE W-BP "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-PEY" "Z" "W-L" "77" "2" "01D-PEY" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "02D-PEY" BY REFERENCE W-HP "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEI" " " "W-L" "0" "48" "D-PEY" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MEI" "----,---,---" "W-L" "15" "12" " " "D-MEI"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MEI" BY REFERENCE W-KIN(1,1) "9" "2" "1" 108 
            BY REFERENCE CNT 9 RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MEI" "----,---,---" "W-L" "28" "12" "01D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MEI" BY REFERENCE W-KIN(1,1) "9" "2" "2" 108 
            BY REFERENCE CNT 9 RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MEI" "----,---,---" "W-L" "51" "12" "02D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-MEI" BY REFERENCE W-KIN(1,1) "9" "2" "3" 108 
            BY REFERENCE CNT 9 RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-MEI" "----,---,---" "W-L" "64" "12" "03D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-MEI" BY REFERENCE W-KIN(1,1) "9" "2" "4" 108 
            BY REFERENCE CNT 9 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KAMI" " " "11" "0" "48" "D-MEI" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-KAMI" "----,---,---" "11" "15" "12" " " "D-KAMI"
            RETURNING RESU.
       CALL "SD_From" USING 
         "01D-KAMI" BY REFERENCE WK-KIN(1) "9" "1" "1" 9 RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-KAMI" "----,---,---" "11" "28" "12" "01D-KAMI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
         "02D-KAMI" BY REFERENCE WK-KIN(1) "9" "1" "2" 9 RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-KAMI" "----,---,---" "11" "51" "12" "02D-KAMI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
         "03D-KAMI" BY REFERENCE WK-KIN(1) "9" "1" "3" 9 RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-KAMI" "----,---,---" "11" "64" "12" "03D-KAMI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
         "04D-KAMI" BY REFERENCE WK-KIN(1) "9" "1" "4" 9 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SIMO" " " "18" "0" "48" "D-KAMI" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SIMO" "----,---,---" "18" "15" "12" " " "D-SIMO"
            RETURNING RESU.
       CALL "SD_From" USING 
         "01D-SIMO" BY REFERENCE WS-KIN(1) "9" "1" "1" 9 RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SIMO" "----,---,---" "18" "28" "12" "01D-SIMO" " "
            RETURNING RESU.
       CALL "SD_From" USING 
         "02D-SIMO" BY REFERENCE WS-KIN(1) "9" "1" "2" 9 RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SIMO" "----,---,---" "18" "51" "12" "02D-SIMO" " "
            RETURNING RESU.
       CALL "SD_From" USING 
         "03D-SIMO" BY REFERENCE WS-KIN(1) "9" "1" "3" 9 RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-SIMO" "----,---,---" "18" "64" "12" "03D-SIMO" " "
            RETURNING RESU.
       CALL "SD_From" USING 
         "04D-SIMO" BY REFERENCE WS-KIN(1) "9" "1" "4" 9 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KEI" " " "19" "0" "48" "D-SIMO" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-KEI" "----,---,---" "19" "15" "12" " " "D-KEI"
            RETURNING RESU.
       CALL "SD_From" USING 
          "01D-KEI" BY REFERENCE WT-KIN(1) "9" "1" "1" 9 RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-KEI" "----,---,---" "19" "28" "12" "01D-KEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
          "02D-KEI" BY REFERENCE WT-KIN(1) "9" "1" "2" 9 RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-KEI" "----,---,---" "19" "51" "12" "02D-KEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
          "03D-KEI" BY REFERENCE WT-KIN(1) "9" "1" "3" 9 RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-KEI" "----,---,---" "19" "64" "12" "03D-KEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
          "04D-KEI" BY REFERENCE WT-KIN(1) "9" "1" "4" 9 RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "17" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "17" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCKY75" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-NKNG TO W-NGS.
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-DNG.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
      *
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NEN TO W-SNEN.
           MOVE 5 TO W-SGET.
           COMPUTE W-ENEN = W-NEN + 1.
           MOVE 4 TO W-EGET.
      *
           PERFORM KKS-RTN THRU KKS-EX.
           PERFORM ZSS-RTN THRU ZSS-EX.
           PERFORM TOT-RTN THRU TOT-EX.
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCKY75" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
      *
           MOVE ZERO TO CNT.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-25.
           ADD 1 TO CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT = 13
               GO TO M-30
           END-IF
           IF  CNT = 7
               CALL "SD_Output" USING "D-KAMI" D-KAMI "p" RETURNING RESU
               ADD 1 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-MEI" D-MEI "p" RETURNING RESU.
           GO TO M-25.
       M-30.
           IF  W-DNG >= W-SNG AND <= W-ENG
               IF  W-DGET > 4 AND < 11
                   MOVE W-DGET TO W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
                   CALL "SD_Output" USING
                    "D-PEY" D-PEY "p" RETURNING RESU
               ELSE
                   IF  W-DGET > 10
                       COMPUTE W-L = W-DGET + 1
                       CALL "SD_Output" USING
                        "D-PEY" D-PEY "p" RETURNING RESU
                   ELSE
                       COMPUTE W-L = W-DGET + 13
                       CALL "SD_Output" USING
                        "D-PEY" D-PEY "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF
           CALL "SD_Output" USING "D-SIMO" D-SIMO "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KEI" D-KEI "p" RETURNING RESU.
       M-60.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB
               GO TO M-60
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       KKS-RTN.
           CALL "DB_F_Open" USING
            "INPUT" KBHKF_PNAME1 "SHARED" BY REFERENCE KBHKF_IDLST "1"
            "KBHK-KEY" BY REFERENCE KBHK-KEY.
           MOVE ZERO TO W-MEI.
           MOVE W-NEN TO KBHK-NEN.
           MOVE 32 TO KBHK-BMN.
      *           READ KBHKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KBHKF_PNAME1 BY REFERENCE KBHK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO KKS-010
           END-IF
           MOVE KBHK-AKIN TO W-MKIN(1).
       KKS-010.
           MOVE W-NEN TO KBHK-NEN.
           MOVE 33 TO KBHK-BMN.
      *           READ KBHKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KBHKF_PNAME1 BY REFERENCE KBHK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO KKS-020
           END-IF
           MOVE KBHK-AKIN TO W-MKIN(3).
       KKS-020.
           CALL "DB_F_Close" USING
            BY REFERENCE KBHKF_IDLST KBHKF_PNAME1.
       KKS-EX.
           EXIT.
       ZSS-RTN.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO KHTMYR_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KHTMYR_PNAME1 " " BY REFERENCE KHTMYR_IDLST "0".
       ZSS-010.
      *           READ KHTMYR AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KHTMYR_PNAME1 BY REFERENCE KHTY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO ZSS-020
           END-IF
           IF  KHTY-NG < W-SNG
               GO TO ZSS-010
           END-IF
           IF  KHTY-NG > W-ENG
               GO TO ZSS-020
           END-IF
           IF  KHTY-YC = ZERO
               GO TO ZSS-010
           END-IF
           COMPUTE W-UK = KHTY-UKIN - KHTY-NKIN.
           IF  W-UK = ZERO
               GO TO ZSS-010
           END-IF
           IF  KHTY-GET > 4
               COMPUTE CNT = KHTY-GET - 4
           ELSE
               COMPUTE CNT = KHTY-GET + 8
           END-IF
           IF  KHTY-YC = 10 OR 11
               ADD W-UK TO W-KIN(2,CNT)
           ELSE
               ADD W-UK TO W-KIN(4,CNT)
           END-IF
           GO TO ZSS-010.
       ZSS-020.
           CALL "DB_F_Close" USING
            BY REFERENCE KHTMYR_IDLST KHTMYR_PNAME1.
           IF  W-DNG < W-SNG OR > W-ENG
               GO TO ZSS-EX
           END-IF
           IF  W-DGET > 4
               COMPUTE CNT = W-DGET - 4
           ELSE
               COMPUTE CNT = W-DGET + 8
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" KHT-M_PNAME1 "SHARED" BY REFERENCE KHT-M_IDLST "1"
            "KHT-KEYD" BY REFERENCE KHT-KEYD.
       ZSS-030.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO ZSS-040
           END-IF
           IF  KHT-YC = ZERO
               GO TO ZSS-030
           END-IF
           COMPUTE W-UK = KHT-UKIN - KHT-NKIN.
           IF  W-UK = ZERO
               GO TO ZSS-030
           END-IF
           IF  KHT-YC = 10 OR 11
               ADD W-UK TO W-KIN(2,CNT)
           ELSE
               ADD W-UK TO W-KIN(4,CNT)
           END-IF
           GO TO ZSS-030.
       ZSS-040.
           CALL "DB_F_Close" USING
            BY REFERENCE KHT-M_IDLST KHT-M_PNAME1.
           MOVE ZERO TO W-PEY.
           CALL "DB_F_Open" USING
            "INPUT" URIR-F_PNAME1 " " BY REFERENCE URIR-F_IDLST "0".
       ZSS-050.
      *           READ URIR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URIR-F_PNAME1 BY REFERENCE URIR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO ZSS-060
           END-IF
           IF  UR-YC = 10 OR 11
               IF  UR-PEY > W-BP
                   MOVE UR-PEY TO W-BP
               END-IF
           END-IF
           IF (UR-YC NOT = 00) AND (UR-YC NOT = 10)
                               AND (UR-YC NOT = 11)
               IF  UR-PEY > W-HP
                   MOVE UR-PEY TO W-HP
               END-IF
           END-IF
           GO TO ZSS-050.
       ZSS-060.
           CALL "DB_F_Close" USING
            BY REFERENCE URIR-F_IDLST URIR-F_PNAME1.
       ZSS-EX.
           EXIT.
       TOT-RTN.
           MOVE ZERO TO CNT.
       TOT-010.
           ADD 1 TO CNT.
           IF  CNT = 13
               GO TO TOT-020
           END-IF
           IF  CNT < 7
               ADD W-KIN(1,CNT) TO WK-KIN(1)
               ADD W-KIN(2,CNT) TO WK-KIN(2)
               ADD W-KIN(3,CNT) TO WK-KIN(3)
               ADD W-KIN(4,CNT) TO WK-KIN(4)
           ELSE
               ADD W-KIN(1,CNT) TO WS-KIN(1)
               ADD W-KIN(2,CNT) TO WS-KIN(2)
               ADD W-KIN(3,CNT) TO WS-KIN(3)
               ADD W-KIN(4,CNT) TO WS-KIN(4)
           END-IF
           GO TO TOT-010.
       TOT-020.
           COMPUTE WT-KIN(1) = WK-KIN(1) + WS-KIN(1).
           COMPUTE WT-KIN(2) = WK-KIN(2) + WS-KIN(2).
           COMPUTE WT-KIN(3) = WK-KIN(3) + WS-KIN(3).
           COMPUTE WT-KIN(4) = WK-KIN(4) + WS-KIN(4).
       TOT-EX.
           EXIT.

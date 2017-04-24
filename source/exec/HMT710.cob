       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMT710.
      *********************************************************
      *    PROGRAM         :  履物分類販売計画　入力　　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHT71                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=入力 , 1=月次更新             *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-END              PIC  9(001) VALUE 0.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-MDD.
           02  W-MD    OCCURS 38.
             03  WM-TM        PIC  X(030).
             03  WM-MD    REDEFINES WM-TM.
               04  WM-M0      PIC  N(003).
               04  F          PIC  X(002).
               04  WM-M1      PIC  N(003).
               04  F          PIC  X(002).
               04  WM-M2      PIC  N(007).
             03  WM-HKK       PIC S9(010).
             03  WM-HGK       PIC S9(010).
             03  WM-TSR       PIC S9(003)V9(02).
             03  WM-BC.
               04  WM-BC3     PIC  9(002).
               04  WM-BMC     PIC  9(002).
               04  WM-BMNO    PIC  9(001).
               04  WM-BC1     PIC  9(002).
       01  W-DATA.
           02  W-L            PIC  9(002).
           02  W-C            PIC  9(002).
           02  W-CD           PIC  9(002).
           02  W-NGD          PIC  9(006).
           02  W-SNG          PIC  9(006).
           02  W-ENG          PIC  9(006).
           02  W-NGP.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS.
               04  W-NGS      PIC  9(004).
               04  F          PIC  9(002).
           02  W-DMM          PIC  9(001).
      *
           02  W-BC.
             03  W-BC3        PIC  9(002).
             03  W-BMC        PIC  9(002).
             03  W-BMNO       PIC  9(001).
             03  W-BC1        PIC  9(002).
           02  W-HKK          PIC S9(010).
           02  WA-D.
             03  WA-HKK       PIC S9(010).
             03  WA-HGK       PIC S9(010).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  W-PC           PIC  9(001).
           02  W-BTC          PIC  9(001) VALUE 0.
           02  W-BRN3         PIC  N(003).
           02  W-BMN          PIC  N(003).
           02  W-TSR          PIC S9(003)V9(02).
           02  W-TM           PIC  X(030) VALUE
                "　　　　【　合　計　】　　　　".
           02  W-NGPD.
             03  F            PIC  9(002).
             03  W-GPD        PIC  9(004).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHKBM.
      *FD  HBHKF
       01  HBHKF_HMT710.
           02  HBHKF_PNAME1   PIC  X(005) VALUE "HBHKF".
           02  F              PIC  X(001).
           02  HBHKF_LNAME    PIC  X(012) VALUE "HBHKF_HMT710".
           02  F              PIC  X(001).
           02  HBHKF_KEY1     PIC  X(100) VALUE SPACE.
           02  HBHKF_KEY2     PIC  X(100) VALUE SPACE.
           02  HBHKF_KEY3     PIC  X(100) VALUE SPACE.
           02  HBHKF_KEY4     PIC  X(100) VALUE SPACE.
           02  HBHKF_KEY5     PIC  X(100) VALUE SPACE.
           02  HBHKF_SORT     PIC  X(100) VALUE SPACE.
           02  HBHKF_IDLST    PIC  X(100) VALUE SPACE.
           02  HBHKF_RES      USAGE  POINTER.
       01  HBHK-R.
           02  HBHK-KEY.
             03  HBHK-NG      PIC  9(006).
             03  HBHK-BC.
               04  HBHK-BC3   PIC  9(002).
               04  HBHK-BMNO  PIC  9(001).
               04  HBHK-BC1   PIC  9(002).
           02  HBHK-BMN       PIC  9(002).
           02  HBHK-KKIN      PIC S9(010).
           02  HBHK-JKIN      PIC S9(010).
           02  F              PIC  X(009).
       77  F                  PIC  X(001).
      *FD  HBUHF
       01  HBUHF_HMT710.
           02  HBUHF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HBUHF_LNAME    PIC  X(012) VALUE "HBUHF_HMT710".
           02  F              PIC  X(001).
           02  HBUHF_KEY1     PIC  X(100) VALUE SPACE.
           02  HBUHF_SORT     PIC  X(100) VALUE SPACE.
           02  HBUHF_IDLST    PIC  X(100) VALUE SPACE.
           02  HBUHF_RES      USAGE  POINTER.
       01  HBUH-R.
           02  HBUH-KEY.
             03  HBUH-BC1     PIC  9(002).
             03  HBUH-BC2.
               04  HBUH-BC21  PIC  9(001).
               04  HBUH-BC22  PIC  9(001).
             03  HBUH-BC3     PIC  9(002).
             03  HBUH-BMC     PIC  9(002).
             03  HBUH-BMNO    PIC  9(001).
           02  HBUH-ZS        PIC S9(007).
           02  HBUH-ZK        PIC S9(010).
           02  HBUH-NS        PIC S9(007).
           02  HBUH-NK        PIC S9(010).
           02  HBUH-SS        PIC S9(007).
           02  HBUH-SK        PIC S9(010).
           02  HBUH-YS        PIC S9(007).
           02  HBUH-YK        PIC S9(010).
           02  HBUH-UG        PIC S9(010).
           02  F              PIC  X(041).
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　履物分類販売計画　実績更新　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ACP.
           02  FILLER.
             03  A-NEN2  PIC  9(002).
             03  A-GET   PIC  9(002).
           02  A-HKK    PIC S9(009).
           02  A-DMM    PIC  9(001).
       01  C-DSP.
           02  D-NGP.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  Z(002).
             03  FILLER  PIC  Z(002).
           02  FILLER.
             03  D-MD.
               04  FILLER  PIC  X(030).
               04  FILLER  PIC ZZZZZZZZ9- .
               04  FILLER  PIC ZZZ,ZZZ,ZZ9- .
               04  FILLER  PIC ZZ9.99- .
             03  D-MIDE.
               04  FILLER  PIC  X(030).
               04  FILLER  PIC ZZZZZZZZ9- .
               04  FILLER  PIC ZZZ,ZZZ,ZZ9- .
               04  FILLER  PIC ZZ9.99- .
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(026) VALUE
                  "***  ﾌﾞﾝﾙｲ ｶﾞ 38ｦ ｺｴﾀ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME4   PIC  X(025) VALUE
                  "***  HBHKF WRITE ｴﾗｰ  ***".
             03  E-ME5   PIC  X(027) VALUE
                  "***  HBHKF REWRITE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(026) VALUE
                  "***  HBHKF DELETE ｴﾗｰ  ***".
             03  E-KEY   PIC  X(011).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "14" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "1" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN2" "9" "1" "60" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN2" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "1" "64" "2" "A-NEN2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HKK" "S9" "W-L" "41" "9" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HKK" BY REFERENCE WM-HKK(1) "10" "1" BY REFERENCE W-C 62
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "67" "1" "A-HKK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "124" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NGP" " " "1" "0" "6" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NGP" "9" "1" "60" "2" " " "D-NGP" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NGP" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NGP" "Z" "1" "64" "2" "01D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NGP" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-NGP" "Z" "1" "68" "2" "02D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NGP" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "W-L" "0" "118" "D-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MD" " " "W-L" "0" "59" " " "02C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MD" "X" "W-L" "8" "30" " " "D-MD" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MD" BY REFERENCE WM-TM(1) "30" "1" BY REFERENCE W-C 62
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MD" "ZZZZZZZZ9-" "W-L" "41" "10" "01D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MD" BY REFERENCE WM-HKK(1) "10" "1" BY REFERENCE W-C 62
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MD" " " "W-L" "52" "12" "02D-MD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-MD" BY REFERENCE WM-HGK(1) "10" "1" BY REFERENCE W-C 62
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-MD" " " "W-L" "65" "7" "03D-MD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-MD" BY REFERENCE WM-TSR(1) "5" "1" BY REFERENCE W-C 62
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MIDE" " " "W-L" "0" "59" "D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MIDE" "X" "W-L" "8" "30" " " "D-MIDE" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MIDE" BY REFERENCE W-TM "30" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "02D-MIDE" " " "W-L" "41" "10" "01D-MIDE" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MIDE" BY REFERENCE WA-HKK "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "03D-MIDE" " " "W-L" "52" "12" "02D-MIDE" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-MIDE" BY REFERENCE WA-HGK "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-MIDE" " " "W-L" "65" "7" "03D-MIDE" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-MIDE" BY REFERENCE W-TSR "5" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "162" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "162" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "26" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "25" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "27" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "26" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "50" "11" "E-ME6" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE HBHK-KEY "11" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-KEY" " " RETURNING RESU.
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
               CALL "DB_Close"
               STOP RUN
           END-IF
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU
           ELSE
               CALL "SD_Screen_Output" USING "SCHT71" RETURNING RESU
           END-IF
           MOVE ZERO TO W-NGP.
           IF  D-HSD NOT = ZERO
               MOVE D-HSD TO W-NGPS
           ELSE
               MOVE D-NHNG TO W-NGS
           END-IF
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-NGD.
           IF  JS-SIGN = 1
               GO TO M-17
           END-IF
           CALL "SD_Output" USING "D-NGP" D-NGP "p" RETURNING RESU.
           IF  W-GET > 3
               ADD 1 TO W-NEN
           END-IF
           MOVE 4 TO W-GET.
           MOVE W-NG TO W-ENG.
           MOVE W-NGD TO W-NG.
           IF  W-GET < 6
               SUBTRACT 1 FROM W-NEN
           END-IF
           MOVE 5 TO W-GET.
           MOVE W-NG TO W-SNG.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-NEN2 "A-NEN2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-15
           END-IF
           IF  W-NG < W-SNG OR > W-ENG
               GO TO M-10
           END-IF.
       M-17.
           MOVE ZERO TO W-C.
       M-20.
           ADD 1 TO W-C.
           IF  W-C NOT = 39
               INITIALIZE W-MD(W-C)
               GO TO M-20
           END-IF
      *
           IF  W-NG >= W-NGD
               PERFORM SET1-RTN THRU SET1-EX
           ELSE
               PERFORM SET2-RTN THRU SET2-EX
           END-IF
           IF  W-END = 1
               GO TO M-95
           END-IF
           MOVE W-C TO W-CD.
           IF  JS-SIGN = 1
               GO TO M-67
           END-IF
           MOVE 1 TO W-PC.
       M-30.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHT71" RETURNING RESU.
           CALL "SD_Output" USING "D-NGP" D-NGP "p" RETURNING RESU.
           MOVE 2 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-PC = 1
               MOVE ZERO TO W-C
           ELSE
               MOVE 20 TO W-C
           END-IF.
       M-35.
           ADD 1 TO W-C W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF (W-CD < W-C) OR (W-C = 39)
               GO TO M-40
           END-IF
           IF  W-PC = 1
               IF  W-C = 21
                   GO TO M-45
               END-IF
           END-IF
      *
           MOVE ZERO TO WM-TSR(W-C).
           IF  WM-HKK(W-C) NOT = ZERO
               COMPUTE WM-TSR(W-C) ROUNDED =
                                  (WM-HGK(W-C) / WM-HKK(W-C)) * 100
           END-IF
           CALL "SD_Output" USING "D-MD" D-MD "p" RETURNING RESU.
           GO TO M-35.
       M-40.
           MOVE ZERO TO W-TSR.
           IF  WA-HKK NOT = ZERO
               COMPUTE W-TSR ROUNDED = (WA-HGK / WA-HKK) * 100
           END-IF
           CALL "SD_Output" USING "D-MIDE" D-MIDE "p" RETURNING RESU.
       M-45.
           IF  W-BTC = 2
               GO TO M-55
           END-IF
           IF  W-BTC = 1
               MOVE 21 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE 19 TO W-C
               GO TO M-50
           END-IF
           MOVE 2 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-PC = 1
               MOVE ZERO TO W-C
           ELSE
               MOVE 20 TO W-C
           END-IF.
       M-50.
           ADD 1 TO W-C W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF (W-CD < W-C) OR (W-C = 39)
               GO TO M-60
           END-IF
           IF  W-PC = 1
               IF  W-C = 21
                   MOVE 2 TO W-PC
                   IF  W-CD >= W-C
                       GO TO M-30
                   ELSE
                       GO TO M-65
                   END-IF
               END-IF
           END-IF.
       M-55.
           MOVE 0 TO W-BTC.
           SUBTRACT WM-HKK(W-C) FROM WA-HKK.
           MOVE WM-HKK(W-C) TO W-HKK.
           CALL "SD_Accept" USING BY REFERENCE A-HKK "A-HKK" "S9" "9"
            BY REFERENCE ESTAT RETURNING RESU.
           ADD WM-HKK(W-C) TO WA-HKK.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               SUBTRACT 1 FROM W-C W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               IF  W-C = 0
                   GO TO M-45
               ELSE
                   IF  W-C NOT = 20
                       GO TO M-55
                   ELSE
                       MOVE 1 TO W-PC W-BTC
                       IF  W-CD >= W-C
                           GO TO M-30
                       ELSE
                           GO TO M-65
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-55
           END-IF
           MOVE ZERO TO WM-TSR(W-C).
           IF  WM-HKK(W-C) NOT = ZERO
               COMPUTE WM-TSR(W-C) ROUNDED =
                                  (WM-HGK(W-C) / WM-HKK(W-C)) * 100
           END-IF
           CALL "SD_Output" USING "D-MD" D-MD "p" RETURNING RESU.
           GO TO M-50.
       M-60.
           MOVE ZERO TO W-TSR.
           IF  WA-HKK NOT = ZERO
               COMPUTE W-TSR ROUNDED = (WA-HGK / WA-HKK) * 100
           END-IF
           CALL "SD_Output" USING "D-MIDE" D-MIDE "p" RETURNING RESU.
           IF  W-CD >= W-C
               GO TO M-30
           END-IF.
       M-65.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               SUBTRACT 1 FROM W-C W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE 2 TO W-BTC
               GO TO M-45
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-65
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-65
           END-IF.
       M-67.
           ACCEPT W-NGPD FROM DATE.
           CALL "DB_F_Open" USING
            "I-O" HBHKF_PNAME1 " " BY REFERENCE HBHKF_IDLST "1"
            "HBHK-KEY" BY REFERENCE HBHK-KEY.
           IF  W-NG >= W-NGD
               PERFORM DEL-RTN THRU DEL-EX
           END-IF
           IF  W-END = 1
               GO TO M-80
           END-IF
           MOVE ZERO TO W-C.
       M-70.
           ADD 1 TO W-C.
           IF  W-CD < W-C
               GO TO M-80
           END-IF
           IF  WM-BC(W-C) = ZERO
               GO TO M-70
           END-IF
           IF  W-NG >= W-NGD
               GO TO M-75
           END-IF
           MOVE W-NG TO HBHK-NG.
           MOVE WM-BC3(W-C) TO HBHK-BC3.
           MOVE WM-BMNO(W-C) TO HBHK-BMNO.
           MOVE WM-BC1(W-C) TO HBHK-BC1.
      *           READ HBHKF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HBHKF_PNAME1 BY REFERENCE HBHK-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-80
           END-IF
           MOVE WM-HKK(W-C) TO HBHK-KKIN.
           MOVE WM-HGK(W-C) TO HBHK-JKIN.
      *           REWRITE HBHK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HBHKF_PNAME1 HBHKF_LNAME HBHK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-80
           END-IF
           GO TO M-70.
       M-75.
           INITIALIZE HBHK-R.
           MOVE W-NG TO HBHK-NG.
           MOVE WM-BC3(W-C) TO HBHK-BC3.
           MOVE WM-BMNO(W-C) TO HBHK-BMNO.
           MOVE WM-BC1(W-C) TO HBHK-BC1.
           MOVE WM-BMC(W-C) TO HBHK-BMN.
           MOVE WM-HKK(W-C) TO HBHK-KKIN.
           MOVE WM-HGK(W-C) TO HBHK-JKIN.
      *           WRITE HBHK-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HBHKF_PNAME1 HBHKF_LNAME HBHK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-80
           END-IF
           GO TO M-70.
       M-80.
           CALL "DB_F_Close" USING
            BY REFERENCE HBHKF_IDLST HBHKF_PNAME1.
           IF  JS-SIGN = 0
               GO TO M-10
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       SET1-RTN.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO HBUHF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HBUHF_PNAME1 " " BY REFERENCE HBUHF_IDLST "0".
      *
      *           READ HBUHF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HBUHF_PNAME1 BY REFERENCE HBUH-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HBUHF_IDLST HBUHF_PNAME1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SET1-EX
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HBHKF_PNAME1 " " BY REFERENCE HBHKF_IDLST "1"
            "HBHK-KEY" BY REFERENCE HBHK-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
      *
           MOVE ZERO TO WA-D W-C.
       SET1-010.
           MOVE HBUH-BC3 TO W-BC3.
           MOVE ZERO TO CHK.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "14" TO HKB-NO.
           MOVE W-BC3 TO HKB-BR3.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BRN3
           END-IF
           MOVE HKB-BRN3 TO W-BRN3.
       SET1-020.
           MOVE HBUH-BMC TO W-BMC.
           MOVE HBUH-BMNO TO W-BMNO.
           MOVE ZERO TO CHK2.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "16" TO HKB-NO.
           MOVE W-BMC TO HKB-BMC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BMN
           END-IF
           MOVE HKB-BMN TO W-BMN.
       SET1-030.
           ADD 1 TO W-C.
           IF  W-C > 38
               CALL "DB_F_Close" USING
                BY REFERENCE HBUHF_IDLST HBUHF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HBHKF_IDLST HBHKF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SET1-EX
           END-IF
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               MOVE W-BRN3 TO WM-M0(W-C)
           END-IF
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               MOVE W-BMN TO WM-M1(W-C)
           END-IF
      *
           MOVE HBUH-BC1 TO W-BC1.
           MOVE SPACE TO HKB-KEY.
           MOVE "11" TO HKB-NO.
           MOVE HBUH-BC1 TO HKB-BR1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BRN1
           END-IF
           MOVE HKB-BRN1 TO WM-M2(W-C).
       SET1-035.
           MOVE HBUH-BC3 TO WM-BC3(W-C).
           MOVE HBUH-BMC TO WM-BMC(W-C).
           MOVE HBUH-BMNO TO WM-BMNO(W-C).
           MOVE HBUH-BC1 TO WM-BC1(W-C).
           IF  W-NG = W-NGD
               MOVE HBUH-SK TO WM-HGK(W-C)
               ADD HBUH-SK TO WA-HGK
           END-IF
      *
           MOVE W-NG TO HBHK-NG.
           MOVE HBUH-BC3 TO HBHK-BC3.
           MOVE HBUH-BMC TO HBHK-BMN.
           MOVE HBUH-BMNO TO HBHK-BMNO.
           MOVE HBUH-BC1 TO HBHK-BC1.
      *           READ HBHKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HBHKF_PNAME1 BY REFERENCE HBHK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO SET1-040
           END-IF
           MOVE HBHK-KKIN TO WM-HKK(W-C).
      *
           ADD HBHK-KKIN TO WA-HKK.
       SET1-040.
      *           READ HBUHF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HBUHF_PNAME1 BY REFERENCE HBUH-R " " RETURNING RET.
           IF  RET = 1
               GO TO SET1-050
           END-IF
           IF  W-BC3 NOT = HBUH-BC3
               GO TO SET1-010
           END-IF
           IF  W-BMC = HBUH-BMC
               GO TO SET1-030
           END-IF
           GO TO SET1-020.
       SET1-050.
           CALL "DB_F_Close" USING
            BY REFERENCE HBUHF_IDLST HBUHF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HBHKF_IDLST HBHKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
       SET1-EX.
           EXIT.
       SET2-RTN.
           CALL "DB_F_Open" USING
            "INPUT" HBHKF_PNAME1 " " BY REFERENCE HBHKF_IDLST  "1"
            "HBHK-KEY" BY REFERENCE HBHK-KEY.
           MOVE SPACE TO HBHK-KEY.
           MOVE W-NG TO HBHK-NG.
      *           START HBHKF KEY NOT < HBHK-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HBHKF_PNAME1 "HBHK-KEY" " NOT < " HBHK-KEY RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HBHKF_IDLST HBHKF_PNAME1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SET2-EX
           END-IF
      *           READ HBHKF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HBHKF_PNAME1 BY REFERENCE HBHK-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HBHKF_IDLST HBHKF_PNAME1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SET2-EX
           END-IF
           IF  W-NG NOT = HBHK-NG
               CALL "DB_F_Close" USING
                BY REFERENCE HBHKF_IDLST HBHKF_PNAME1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SET2-EX
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
      *
           MOVE ZERO TO WA-D W-C.
       SET2-010.
           MOVE HBHK-BC3 TO W-BC3.
           MOVE ZERO TO CHK.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "14" TO HKB-NO.
           MOVE W-BC3 TO HKB-BR3.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BRN3
           END-IF
           MOVE HKB-BRN3 TO W-BRN3.
       SET2-020.
           MOVE HBHK-BMN TO W-BMC.
           MOVE HBHK-BMNO TO W-BMNO.
           MOVE ZERO TO CHK2.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "16" TO HKB-NO.
           MOVE W-BMC TO HKB-BMC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BMN
           END-IF
           MOVE HKB-BMN TO W-BMN.
       SET2-030.
           ADD 1 TO W-C.
           IF  W-C > 38
               CALL "DB_F_Close" USING
                BY REFERENCE HBHKF_IDLST HBHKF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SET2-EX
           END-IF
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               MOVE W-BRN3 TO WM-M0(W-C)
           END-IF
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               MOVE W-BMN TO WM-M1(W-C)
           END-IF
      *
           MOVE HBHK-BC1 TO W-BC1.
           MOVE SPACE TO HKB-KEY.
           MOVE "11" TO HKB-NO.
           MOVE HBHK-BC1 TO HKB-BR1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BRN1
           END-IF
           MOVE HKB-BRN1 TO WM-M2(W-C).
      *
           MOVE HBHK-BC3 TO WM-BC3(W-C).
           MOVE HBHK-BMN TO WM-BMC(W-C).
           MOVE HBHK-BMNO TO WM-BMNO(W-C).
           MOVE HBHK-BC1 TO WM-BC1(W-C).
           MOVE HBHK-KKIN TO WM-HKK(W-C).
           MOVE HBHK-JKIN TO WM-HGK(W-C)
           ADD HBHK-KKIN TO WA-HKK.
           ADD HBHK-JKIN TO WA-HGK.
       SET2-040.
      *           READ HBHKF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HBHKF_PNAME1 BY REFERENCE HBHK-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO SET2-050
           END-IF
           IF  W-NG NOT = HBHK-NG
               GO TO SET2-050
           END-IF
           IF  W-BC3 NOT = HBHK-BC3
               GO TO SET2-010
           END-IF
           IF  W-BMC = HBHK-BMN
               GO TO SET2-030
           END-IF
           GO TO SET2-020.
       SET2-050.
           CALL "DB_F_Close" USING
            BY REFERENCE HBHKF_IDLST HBHKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
       SET2-EX.
           EXIT.
       DEL-RTN.
           MOVE SPACE TO HBHK-KEY.
           MOVE W-NG TO HBHK-NG.
      *           START HBHKF KEY NOT < HBHK-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HBHKF_PNAME1 "HBHK-KEY" " NOT < " HBHK-KEY RETURNING RET.
           IF  RET = 1
               GO TO DEL-EX.
       DEL-010.
      *           READ HBHKF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HBHKF_PNAME1 BY REFERENCE HBHK-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO DEL-EX
           END-IF
           IF  W-NG NOT = HBHK-NG
               GO TO DEL-EX
           END-IF
      *
      *           DELETE HBHKF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HBHKF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO DEL-EX
           END-IF
           GO TO DEL-010.
       DEL-EX.
           EXIT.

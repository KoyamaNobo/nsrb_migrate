       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMT730.
      *********************************************************
      *    PROGRAM         :  履物分類販売計画・実績対比問合せ*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-END              PIC  9(001) VALUE 0.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-MDD.
           02  W-MD    OCCURS 60.
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
       01  W-DATA.
           02  W-L            PIC  9(002).
           02  W-C            PIC  9(002).
           02  W-CD           PIC  9(002).
           02  W-NGD          PIC  9(006).
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
             03  W-NGPS       PIC  9(006).
           02  W-DMM          PIC  9(001).
      *
           02  W-BC.
             03  W-BC3        PIC  9(002).
             03  W-BMC        PIC  9(002).
             03  W-BMNO       PIC  9(001).
             03  W-BC1        PIC  9(002).
           02  WS-D.
             03  WS-HKK       PIC S9(010).
             03  WS-HGK       PIC S9(010).
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
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHKBM.
      *FD  HBHKF
       01  HBHKF_HMT730.
           02  HBHKF_PNAME1   PIC  X(005) VALUE "HBHKF".
           02  F              PIC  X(001).
           02  HBHKF_LNAME    PIC  X(012) VALUE "HBHKF_HMT730".
           02  F              PIC  X(001).
           02  HBHKF_KEY1     PIC  X(100) VALUE SPACE.
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
           02  HBHK-BMC       PIC  9(002).
           02  HBHK-KKIN      PIC S9(010).
           02  HBHK-JKIN      PIC S9(010).
           02  F              PIC  X(002).
           02  F              PIC  X(007).
       77  F                  PIC  X(001).
      *FD  HBUHF
       01  HBUHF_HMT730.
           02  HBUHF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HBUHF_LNAME    PIC  X(012) VALUE "HBUHF_HMT730".
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  FILLER.
             03  A-NEN2  PIC  9(002).
             03  A-GET   PIC  9(002).
           02  A-DMM    PIC  9(001).
       01  C-DSP.
           02  D-MID.
             03  FILLER  PIC  X(009) VALUE "         ".
             03  FILLER  PIC  X(049) VALUE
                  "年月=ｆ・1 , 前進=ｆ･7 , 後退=ｆ･8 , 終了=ｆ･9   ".
           02  D-NGP.
             03  D-NEN2  PIC  9(002).
             03  D-GET   PIC  Z(002).
             03  D-PEY   PIC  Z(002).
             03  D-PEYM  PIC  N(001) VALUE "末".
           02  FILLER.
             03  D-MD.
               04  01D-MD  PIC  X(030).
               04  02D-MD  PIC ----,---,--9 .
               04  03D-MD  PIC ----,---,--9 .
               04  04D-MD  PIC ---9.99 .
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(026) VALUE
                  "***  ﾌﾞﾝﾙｲ ｶﾞ 60ｦ ｺｴﾀ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
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
           "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
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
           "A-DMM" "9" "23" "71" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "127" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID" " " "23" "0" "58" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MID" "X" "23" "8" "9" " " "D-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MID" "X" "23" "23" "49" "01D-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NGP" " " "1" "0" "8" "D-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NEN2" "9" "1" "60" "2" " " "D-NGP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NEN2" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GET" "Z" "1" "64" "2" "D-NEN2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PEY" "Z" "1" "68" "2" "D-GET" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PEYM" "N" "1" "68" "2" "D-PEY" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-DSP" " " "W-L" "0" "61" "D-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MD" " " "W-L" "0" "61" " " "03C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MD" "X" "W-L" "8" "30" " " "D-MD" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MD" BY REFERENCE WM-TM(1) "30" "1" BY REFERENCE W-C 55
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MD" "----,---,--9" "W-L" "39" "12" "01D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MD" BY REFERENCE WM-HKK(1) "10" "1" BY REFERENCE W-C 55
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MD" "----,---,--9" "W-L" "52" "12" "02D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-MD" BY REFERENCE WM-HGK(1) "10" "1" BY REFERENCE W-C 55
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-MD" "---9.99" "W-L" "65" "7" "03D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-MD" BY REFERENCE WM-TSR(1) "5" "1" BY REFERENCE W-C 55
            RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "73" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "73" " " "C-ERR" RETURNING RESU.
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
           "E-ME98" "X" "24" "75" "5" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHT71" RETURNING RESU.
           CALL "SD_Output" USING "D-MID" D-MID "p" RETURNING RESU.
           MOVE ZERO TO W-NGP.
           MOVE D-HSD TO W-NGPS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-NGD.
           CALL "SD_Output" USING "D-NGP" D-NGP "p" RETURNING RESU.
           PERFORM INI-RTN THRU INI-EX.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO HBUHF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HBUHF_PNAME1 " " BY REFERENCE HBUHF_IDLST "0".
      *           READ HBUHF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HBUHF_PNAME1 BY REFERENCE HBUH-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HBUHF_IDLST HBUHF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
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
       M-10.
           MOVE HBUH-BC3 TO W-BC3.
           MOVE ZERO TO CHK WS-D.
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
       M-15.
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
       M-20.
           ADD 1 TO W-C.
           IF  W-C > 60
               CALL "DB_F_Close" USING
                BY REFERENCE HBUHF_IDLST HBUHF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HBHKF_IDLST HBHKF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
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
      *
           MOVE HBUH-SK TO WM-HGK(W-C).
      *
           ADD HBUH-SK TO WS-HGK.
      *
           MOVE HBUH-BC3 TO HBHK-BC3.
           MOVE HBUH-BMC TO HBHK-BMC.
           MOVE HBUH-BMNO TO HBHK-BMNO.
           MOVE HBUH-BC1 TO HBHK-BC1.
           MOVE W-NG TO HBHK-NG.
      *           READ HBHKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HBHKF_PNAME1 BY REFERENCE HBHK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           MOVE HBHK-KKIN TO WM-HKK(W-C).
      *
           ADD HBHK-KKIN TO WS-HKK.
      *
           MOVE ZERO TO WM-TSR(W-C).
           IF  WM-HKK(W-C) NOT = ZERO
               COMPUTE WM-TSR(W-C) ROUNDED =
                                  (WM-HGK(W-C) / WM-HKK(W-C)) * 100
           END-IF.
       M-25.
      *           READ HBUHF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HBUHF_PNAME1 BY REFERENCE HBUH-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           IF  W-BC3 NOT = HBUH-BC3
               GO TO M-30
           END-IF
           IF  W-BMC = HBUH-BMC
               GO TO M-20
           END-IF
           GO TO M-15.
       M-30.
           ADD 1 TO W-C.
           IF  W-C > 60
               CALL "DB_F_Close" USING
                BY REFERENCE HBUHF_IDLST HBUHF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HBHKF_IDLST HBHKF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           PERFORM SUB-RTN THRU SUB-EX.
           GO TO M-10.
       M-35.
           ADD 1 TO W-C.
           IF  W-C > 60
               CALL "DB_F_Close" USING
                BY REFERENCE HBUHF_IDLST HBUHF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HBHKF_IDLST HBHKF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           PERFORM SUB-RTN THRU SUB-EX.
      *
           ADD 1 TO W-C.
           IF  W-C > 60
               CALL "DB_F_Close" USING
                BY REFERENCE HBUHF_IDLST HBUHF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HBHKF_IDLST HBHKF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE "        【　合　計　】        " TO WM-TM(W-C).
           MOVE WA-HGK TO WM-HGK(W-C).
           MOVE WA-HKK TO WM-HKK(W-C).
           MOVE ZERO TO WM-TSR(W-C).
           IF  WM-HKK(W-C) NOT = ZERO
               COMPUTE WM-TSR(W-C) ROUNDED =
                                  (WM-HGK(W-C) / WM-HKK(W-C)) * 100
           END-IF
      *
           CALL "DB_F_Close" USING
            BY REFERENCE HBUHF_IDLST HBUHF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HBHKF_IDLST HBHKF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HKBM_IDLST HKBM_PNAME1.
       M-40.
           MOVE W-C TO W-CD.
           MOVE 1 TO W-PC.
       M-45.
           PERFORM DSP-RTN THRU DSP-EX.
       M-50.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF1
               GO TO M-55
           END-IF
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  W-PC = 1
               IF  ESTAT = PF8
                   GO TO M-50
               ELSE
                   IF  ESTAT = PF7
                       MOVE 2 TO W-PC
                       GO TO M-45
                   END-IF
               END-IF
           END-IF
           IF  W-PC = 2
               IF  ESTAT = PF7
                   MOVE 3 TO W-PC
                   GO TO M-45
               ELSE
                   IF  ESTAT = PF8
                       MOVE 1 TO W-PC
                       GO TO M-45
                   END-IF
               END-IF
           END-IF
           IF  W-PC = 3
               IF  ESTAT = PF7
                   GO TO M-50
               ELSE
                   IF  ESTAT = PF8
                       MOVE 2 TO W-PC
                       GO TO M-45
                   END-IF
               END-IF
           END-IF
           GO TO M-50.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-NEN2 "A-NEN2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-55
           END-IF
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       M-60.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-55
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-60
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-60
           END-IF
           IF  W-NG = W-NGD
               GO TO M-05
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HBHKF_PNAME1 " " BY REFERENCE HBHKF_IDLST "1"
            "HBHK-KEY" BY REFERENCE HBHK-KEY.
           MOVE SPACE TO HBHK-KEY.
           MOVE W-NG TO HBHK-NG.
      *           START HBHKF KEY NOT < HBHK-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HBHKF_PNAME1 "HBHK-KEY" "NOT <" HBHK-KEY RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HBHKF_IDLST HBHKF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-55
           END-IF
      *           READ HBHKF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HBHKF_PNAME1 BY REFERENCE HBHK-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HBHKF_IDLST HBHKF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-55
           END-IF
           IF  HBHK-NG NOT = W-NG
               CALL "DB_F_Close" USING
                BY REFERENCE HBHKF_IDLST HBHKF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-55
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           PERFORM INI-RTN THRU INI-EX.
           MOVE ZERO TO WA-D W-C.
       M-65.
           MOVE HBHK-BC3 TO W-BC3.
           MOVE ZERO TO CHK WS-D.
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
       M-70.
           MOVE HBHK-BMC TO W-BMC.
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
       M-75.
           ADD 1 TO W-C.
           IF  W-C > 60
               CALL "DB_F_Close" USING
                BY REFERENCE HBHKF_IDLST HBHKF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
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
           MOVE HBHK-KKIN TO WM-HKK(W-C).
           MOVE HBHK-JKIN TO WM-HGK(W-C).
      *
           ADD HBHK-KKIN TO WS-HKK.
           ADD HBHK-JKIN TO WS-HGK.
      *
           MOVE ZERO TO WM-TSR(W-C).
           IF  WM-HKK(W-C) NOT = ZERO
               COMPUTE WM-TSR(W-C) ROUNDED =
                                  (WM-HGK(W-C) / WM-HKK(W-C)) * 100
           END-IF
      *
      *           READ HBHKF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HBHKF_PNAME1 BY REFERENCE HBHK-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  W-NG NOT = HBHK-NG
               GO TO M-85
           END-IF
           IF  W-BC3 NOT = HBHK-BC3
               GO TO M-80
           END-IF
           IF  W-BMC = HBHK-BMC
               GO TO M-75
           END-IF
           GO TO M-70.
       M-80.
           ADD 1 TO W-C.
           IF  W-C > 60
               CALL "DB_F_Close" USING
                BY REFERENCE HBHKF_IDLST HBHKF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           PERFORM SUB-RTN THRU SUB-EX.
           GO TO M-65.
       M-85.
           ADD 1 TO W-C.
           IF  W-C > 60
               CALL "DB_F_Close" USING
                BY REFERENCE HBHKF_IDLST HBHKF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           PERFORM SUB-RTN THRU SUB-EX.
      *
           ADD 1 TO W-C.
           IF  W-C > 60
               CALL "DB_F_Close" USING
                BY REFERENCE HBHKF_IDLST HBHKF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE "        【　合　計　】        " TO WM-TM(W-C).
           MOVE WA-HGK TO WM-HGK(W-C).
           MOVE WA-HKK TO WM-HKK(W-C).
           MOVE ZERO TO WM-TSR(W-C).
           IF  WM-HKK(W-C) NOT = ZERO
               COMPUTE WM-TSR(W-C) ROUNDED =
                                  (WM-HGK(W-C) / WM-HKK(W-C)) * 100
           END-IF
      *
           CALL "DB_F_Close" USING
            BY REFERENCE HBHKF_IDLST HBHKF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           GO TO M-40.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       INI-RTN.
           MOVE ZERO TO W-C.
       INI-010.
           ADD 1 TO W-C.
           IF  W-C NOT = 61
               INITIALIZE W-MD(W-C)
               GO TO INI-010
           END-IF.
       INI-EX.
           EXIT.
       SUB-RTN.
           MOVE "            ［  小  計  ］    " TO WM-TM(W-C).
           MOVE WS-HGK TO WM-HGK(W-C).
           MOVE WS-HKK TO WM-HKK(W-C).
           MOVE ZERO TO WM-TSR(W-C).
           IF  WM-HKK(W-C) NOT = ZERO
               COMPUTE WM-TSR(W-C) ROUNDED =
                                  (WM-HGK(W-C) / WM-HKK(W-C)) * 100
           END-IF
      *
           ADD WS-HGK TO WA-HGK.
           ADD WS-HKK TO WA-HKK.
       SUB-EX.
           EXIT.
       DSP-RTN.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHT71" RETURNING RESU.
           CALL "SD_Output" USING "D-MID" D-MID "p" RETURNING RESU.
           IF  W-NG = W-NGD
               CALL "SD_Output" USING
                "D-NEN2" D-NEN2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-GET" D-GET "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-PEY" D-PEY "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "D-NEN2" D-NEN2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-GET" D-GET "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-PEYM" D-PEYM "p" RETURNING RESU
           END-IF
           MOVE 2 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-PC = 1
               MOVE ZERO TO W-C
           ELSE
               IF  W-PC = 2
                   MOVE 20 TO W-C
               ELSE
                   MOVE 40 TO W-C
               END-IF
           END-IF.
       DSP-010.
           ADD 1 TO W-C W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-CD < W-C
               GO TO DSP-EX
           END-IF
           IF  W-PC = 1
               IF  W-C = 21
                   GO TO DSP-EX
               END-IF
           END-IF
           IF  W-PC = 2
               IF  W-C = 41
                   GO TO DSP-EX
               END-IF
           END-IF
      *
           CALL "SD_Output" USING "D-MD" D-MD "p" RETURNING RESU.
           GO TO DSP-010.
       DSP-EX.
           EXIT.

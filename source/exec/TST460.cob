       IDENTIFICATION DIVISION.
       PROGRAM-ID. TST460.
      **********************************************
      *****     Š„ˆøŽèŒ`@ŒˆÏ—\’è@ˆê——       *****
      *****       ( SCREEN : SCTT46 )          *****
      **********************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE    SECTION.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-MNG.
             03  W-MNEN       PIC  9(004).
             03  W-MNENL REDEFINES W-MNEN.
               04  W-MNEN1    PIC  9(002).
               04  W-MNEN2    PIC  9(002).
             03  W-MGET       PIC  9(002).
           02  W-MNGL  REDEFINES W-MNG.
             03  F            PIC  9(002).
             03  W-MNGS       PIC  9(004).
           02  W-WNG.
             03  W-WNEN       PIC  9(002).
             03  W-WGET       PIC  9(002).
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-FNG.
             03  W-FNEN       PIC  9(004).
             03  W-FNENL REDEFINES W-FNEN.
               04  W-FNEN1    PIC  9(002).
               04  W-FNEN2    PIC  9(002).
             03  W-FGET       PIC  9(002).
           02  W-FNGL  REDEFINES W-FNG.
             03  F            PIC  9(002).
             03  W-FNGS       PIC  9(004).
           02  W-PD           PIC  9(002).
           02  W-ALD.
             03  F            PIC  X(030) VALUE
                  "040506070809101112131415161718".
             03  F            PIC  X(032) VALUE
                  "04050607080910111213141516171819".
           02  W-AL    REDEFINES W-ALD.
             03  W-LD         PIC  9(002)  OCCURS 31.
           02  W-LC.
             03  W-L1         PIC  9(002).
             03  W-L2         PIC  9(002).
             03  W-C1         PIC  9(002).
             03  W-C2         PIC  9(002).
             03  W-C          PIC  9(002).
           02  W-PEYD.
             03  W-HOF        PIC  X(001).
             03  W-PEY        PIC  Z(002).
             03  W-HOR        PIC  X(001).
           02  W-KIN          PIC  9(010).
           02  W-TKIN         PIC  9(010).
           02  W-BNKC         PIC  9(004).
           02  W-COD          PIC  9(004)  OCCURS  5.
           02  W-NAME         PIC  N(008)  OCCURS  5.
           02  CNT            PIC  9.
           02  W-L            PIC  9(002).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LICAL.
           COPY LSUKET.
           COPY LIBANK.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  FILLER.
             03  A-NEN   PIC  9(002).
             03  A-GET   PIC  9(002).
           02  A-BNKC  PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-COD   PIC  9(004).
             03  D-NAME  PIC  N(008).
           02  D-BNKN  PIC  N(008).
           02  D-PEY.
             03  FILLER  PIC  X(004).
           02  D-KIN.
             03  FILLER  PIC -----,---,--- .
           02  D-TKIN.
             03  FILLER  PIC -----,---,--- .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA Å¼  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  CALNM Å¼  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  BANKM Å¼  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-DATE  PIC  9(008).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-ACP" " " "0" "0" "9" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "1" "0" "4" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "1" "25" "2" " " "01C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-WNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "1" "29" "2" "A-NEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-WGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BNKC" "9" "4" "18" "4" "01C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BNKC" BY REFERENCE W-BNKC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "21" "66" "1" "A-BNKC" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "66" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "W-L" "0" "20" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-COD" "9" "W-L" "10" "4" " " "01C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-COD" BY REFERENCE W-COD(1) "4" "1" BY REFERENCE CNT 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "W-L" "15" "16" "D-COD" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE W-NAME(1) "16" "1" BY REFERENCE CNT 16
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BNKN" "N" "4" "31" "16" "01C-DSP" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-BNKN" BY REFERENCE B-BNA "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PEY" " " "W-L1" "0" "4" "D-BNKN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-PEY" "X" "W-L1" "W-C1" "4" " " "D-PEY" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-PEY" BY REFERENCE W-PEYD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN" " " "W-L2" "0" "13" "D-PEY" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-KIN" "-----,---,---" "W-L2" "W-C2" "13" " " "D-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-KIN" BY REFERENCE W-KIN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TKIN" " " "18" "0" "13" "D-KIN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TKIN" "-----,---,---" "18" "57" "13" " " "D-TKIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-TKIN" BY REFERENCE W-TKIN "10" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "121" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "121" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "18" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-DATE" "9" "24" "40" "8" "E-ME99" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-DATE" BY REFERENCE CL-DATE "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-DATE" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           MOVE ZERO TO W-MNG W-WNG.
           MOVE D-NTNG TO W-MNGS.
           IF  W-MNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-MNEN
           END-IF
           IF  W-MNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-MNEN
           END-IF
           COMPUTE W-WNEN = W-MNEN - DATE-YC1.
           MOVE W-MGET TO W-WGET.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO UKET-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" CALNM_PNAME1 "SHARED" BY REFERENCE CALNM_IDLST "1"
            "CL-KEY" BY REFERENCE CL-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           MOVE ZERO TO W-COD(01) W-COD(02) W-COD(03) W-COD(04)
                        W-COD(05).
           MOVE SPACE TO W-NAME(01) W-NAME(02) W-NAME(03) W-NAME(04)
                         W-NAME(05).
       M-07.
      *           READ BANK-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" BANK-M_PNAME1 BY REFERENCE BANK-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-08
           END-IF
           IF  B-YBC = 0
               GO TO M-07
           END-IF
           ADD 1 TO CNT.
           IF  CNT = 6
               GO TO M-08
           END-IF
           MOVE B-KEY TO W-COD(CNT).
           MOVE B-BNA TO W-NAME(CNT).
           GO TO M-07.
       M-08.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       M-10.
           CALL "SD_Screen_Output" USING "SCTT46" RETURNING RESU.
           MOVE ZERO TO CNT.
           MOVE 19 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-11.
           ADD 1 TO CNT.
           IF  CNT = 6
               GO TO M-12
           END-IF
           IF  W-COD(CNT) = ZERO
               GO TO M-12
           END-IF
           CALL "SD_Output" USING "D-COD" D-COD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           GO TO M-11.
       M-12.
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-WGET < 1 OR > 12
               GO TO M-20
           END-IF
           MOVE ZERO TO W-NG.
           MOVE W-WNG TO W-NGS.
           IF  W-NEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-NEN
           ELSE
               IF  W-NEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-NEN
               END-IF
           END-IF
           IF  W-NG < W-MNG
               GO TO M-20
           END-IF
      *
           MOVE SPACE TO CL-KEY.
           MOVE W-NG TO CL-NG.
           MOVE 1 TO CL-PEY.
      *           READ CALNM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" CALNM_PNAME1 BY REFERENCE CALN-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DATE" E-DATE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           PERFORM S-05 THRU S-10.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-BNKC "A-BNKC" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           MOVE SPACE TO B-KEY.
           MOVE W-BNKC TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
              GO TO M-25
           END-IF
           CALL "SD_Output" USING "D-BNKN" D-BNKN "p" RETURNING RESU.
           IF  B-YBC = 0
               GO TO M-25
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" UKET-F_PNAME1 " " BY REFERENCE UKET-F_IDLST "0".
       M-30.
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
               CALL "DB_F_Close" USING
                BY REFERENCE UKET-F_IDLST UKET-F_PNAME1
               GO TO M-10
           END-IF
           IF  UT-SKC NOT = 32
               GO TO M-30
           END-IF
           IF  UT-SBC NOT = W-BNKC
               GO TO M-30
           END-IF
           MOVE ZERO TO W-FNG.
           MOVE UT-OKN TO W-FNEN2.
           MOVE UT-OKG TO W-FGET.
           IF  W-FNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-FNEN
           END-IF
           IF  W-FNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-FNEN
           END-IF
           IF  W-FNG > W-NG
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-10
           END-IF
           IF  W-FNG < W-NG
               GO TO M-30
           END-IF
           MOVE ZERO TO W-TKIN.
       M-35.
           MOVE UT-OKP TO W-PD.
           MOVE ZERO TO W-KIN.
       M-40.
           ADD UT-KIN TO W-KIN W-TKIN.
       M-45.
      *           READ UKET-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" UKET-F_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  UT-SKC NOT = 32
               GO TO M-45
           END-IF
           IF  UT-SBC NOT = W-BNKC
               GO TO M-45
           END-IF
           MOVE ZERO TO W-FNG.
           MOVE UT-OKN TO W-FNEN2.
           MOVE UT-OKG TO W-FGET.
           IF  W-FNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-FNEN
           END-IF
           IF  W-FNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-FNEN
           END-IF
           IF  W-FNG > W-NG
               GO TO M-50
           END-IF
           IF  UT-OKP = W-PD
               GO TO M-40
           END-IF
           PERFORM S-15 THRU S-20.
           GO TO M-35.
       M-50.
           PERFORM S-15 THRU S-20.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-F_IDLST UKET-F_PNAME1.
           CALL "SD_Output" USING "D-TKIN" D-TKIN "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = "PF9"
               MOVE 1 TO W-PD
               GO TO M-60
           END-IF
           GO TO M-95.
       M-60.
           MOVE ZERO TO W-KIN.
           PERFORM S-15 THRU S-20.
           ADD 1 TO W-PD.
           IF  W-PD < 32
               GO TO M-60
           END-IF
           MOVE ZERO TO W-TKIN.
           CALL "SD_Output" USING "D-TKIN" D-TKIN "p" RETURNING RESU.
           GO TO M-25.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CALNM_IDLST CALNM_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO W-PEYD.
           MOVE CL-PEY TO W-PEY.
           MOVE W-PEY TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           IF  CL-SJ = 1
               MOVE "(" TO W-HOF
               MOVE ")" TO W-HOR
           END-IF
           IF  W-L1 > 10 AND < 21
               SUBTRACT 10 FROM W-L1
               CALL "SD_Arg_Match_Line" USING
                "W-L1" "2" W-L1 RETURNING RESU
           END-IF
           IF  W-L1 > 20 AND < 32
               SUBTRACT 20 FROM W-L1
               CALL "SD_Arg_Match_Line" USING
                "W-L1" "2" W-L1 RETURNING RESU
           END-IF
           ADD 6 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE CL-PEY TO W-PD.
           IF  W-PD < 11
               MOVE 10 TO W-C1
               CALL "SD_Arg_Match_Col" USING
                "W-C1" "2" W-C1 RETURNING RESU
           END-IF
           IF  W-PD > 10 AND < 21
               MOVE 31 TO W-C1
               CALL "SD_Arg_Match_Col" USING
                "W-C1" "2" W-C1 RETURNING RESU
           END-IF
           IF  W-PD > 20 AND < 32
               MOVE 52 TO W-C1
               CALL "SD_Arg_Match_Col" USING
                "W-C1" "2" W-C1 RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-PEY" D-PEY "p" RETURNING RESU.
      *           READ CALNM NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CALNM_PNAME1 BY REFERENCE CALN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO S-10
           END-IF
           IF  CL-NG = W-NG
               GO TO S-05
           END-IF.
       S-10.
           EXIT.
       S-15.
           MOVE W-PD TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-L2 > 10 AND < 21
               SUBTRACT 10 FROM W-L2
               CALL "SD_Arg_Match_Line" USING
                "W-L2" "2" W-L2 RETURNING RESU
           END-IF
           IF  W-L2 > 20 AND < 32
               SUBTRACT 20 FROM W-L2
               CALL "SD_Arg_Match_Line" USING
                "W-L2" "2" W-L2 RETURNING RESU
           END-IF
           ADD 6 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-PD < 11
               MOVE 15 TO W-C2
               CALL "SD_Arg_Match_Col" USING
                "W-C2" "2" W-C2 RETURNING RESU
           END-IF
           IF  W-PD > 10 AND < 21
               MOVE 36 TO W-C2
               CALL "SD_Arg_Match_Col" USING
                "W-C2" "2" W-C2 RETURNING RESU
           END-IF
           IF  W-PD > 20 AND < 32
               MOVE 57 TO W-C2
               CALL "SD_Arg_Match_Col" USING
                "W-C2" "2" W-C2 RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
       S-20.
           EXIT.

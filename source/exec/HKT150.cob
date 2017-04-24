       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKT150.
      ********************************************************
      *****     得意先別　売掛残高　問合せ（入金チェック）****
      ********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-TCD          PIC  9(004).
           02  W-SHC          PIC  X(011).
           02  W-SHC23 REDEFINES W-SHC.
             03  W-SHC23F     PIC  X(004).
             03  F            PIC  X(001).
             03  W-SHC23R     PIC  X(006).
           02  W-SHC32 REDEFINES W-SHC.
             03  W-SHC32F     PIC  X(006).
             03  F            PIC  X(001).
             03  W-SHC32R     PIC  X(004).
           02  W-SGT          PIC  N(002).
           02  W-STT          PIC  N(002).
           02  W-GPD.
             03  W-GETD       PIC  9(002).
             03  W-PEYD       PIC  9(002).
           02  W-L            PIC  9(002).
           02  W-D.
             03  W-UKIN       PIC S9(009).
             03  W-USHZ       PIC S9(007).
             03  W-NKIN       PIC S9(009).
             03  W-NSHZ       PIC S9(007).
             03  W-ZKIN       PIC S9(009).
             03  W-ZSHZ       PIC S9(007).
             03  W-ZKIND      PIC S9(009).
             03  W-ZSHZD      PIC S9(007).
           02  W-DNO          PIC  X(006).
           02  W-CHK          PIC  X(001).
           02  W-ZSM          PIC  N(004).
           02  W-DMM          PIC  9(001).
           02  W-NC           PIC  9(001).
           02  W-DC           PIC  9(001).
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
             03  W-NGS        PIC  9(004).
             03  F            PIC  9(002).
           02  W-HKNG.
             03  W-HSNG.
               04  W-HSN      PIC  9(004).
               04  W-HSG      PIC  9(002).
             03  W-HENG.
               04  W-HEN      PIC  9(004).
               04  W-HEG      PIC  9(002).
             03  W-KSNG.
               04  W-KSN      PIC  9(004).
               04  W-KSG      PIC  9(002).
             03  W-KENG.
               04  W-KEN      PIC  9(004).
               04  W-KEG      PIC  9(002).
           02  W-SPACE        PIC  X(078).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITUKW.
           COPY LITM.
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
           02  A-TCD   PIC  9(004).
           02  A-GP    PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NAME  PIC  N(026).
      *
           02  D-SHJC  PIC  X(080).
           02  D-SHJ.
             03  D-SS.
               04  FILLER  PIC  N(002) VALUE "締日".
               04  02D-SS  PIC  Z(002).
               04  FILLER  PIC  X(001) VALUE ",".
             03  D-SHD.
               04  FILLER  PIC  X(005) VALUE "入金S".
               04  02D-SHD PIC  Z(003).
               04  FILLER  PIC  X(001) VALUE ",".
             03  D-SSI.
               04  FILLER  PIC  X(005) VALUE "手形S".
               04  02D-SSI PIC  Z(003).
               04  FILLER  PIC  X(001) VALUE ",".
             03  D-SHC.
               04  01D-SHC PIC  X(011).
               04  FILLER  PIC  X(001) VALUE ",".
             03  D-SG.
               04  FILLER  PIC  N(003) VALUE "現金引".
               04  02D-SG  PIC  N(002).
               04  03D-SG  PIC 9.9 .
               04  FILLER  PIC  X(001) VALUE ",".
             03  D-ST.
               04  FILLER  PIC  N(003) VALUE "手数料".
               04  02D-ST  PIC  N(002).
               04  03D-ST  PIC 9.9 .
               04  FILLER  PIC  X(001) VALUE ",".
             03  D-SKR.
               04  FILLER  PIC  N(003) VALUE "送金料".
               04  02D-SKR PIC  Z(004).
      *
           02  FILLER.
             03  D-DATA.
               04  01D-DATA  PIC Z9 .
               04  02D-DATA  PIC Z9 .
               04  03D-DATA  PIC  X(006).
               04  04D-DATA  PIC  X(001).
               04  05D-DATA  PIC ----,---,--- .
               04  06D-DATA  PIC -----,--- .
               04  07D-DATA  PIC ----,---,--- .
               04  08D-DATA  PIC ----,--- .
               04  09D-DATA  PIC ----,---,--- .
               04  10D-DATA  PIC -----,--- .
             03  D-ZSM   PIC  N(004).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "**  DATA ﾅｼ  **".
             03  E-ME2   PIC  X(020) VALUE
                  "**  得意先　なし  **".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-TCD" "9" "2" "8" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-GP" "9" "6" "1" "4" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-GP" BY REFERENCE W-GPD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "71" "1" "A-GP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "288" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "2" "13" "52" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHJC" "X" "3" "1" "80" "D-NAME" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SHJC" BY REFERENCE W-SPACE "80" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHJ" " " "3" "0" "75" "D-SHJC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SS" " " "3" "13" "7" " " "D-SHJ" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SS" "N" "3" "3" "4" " " "D-SS" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SS" "Z" "3" "7" "2" "01D-SS" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SS" BY REFERENCE T-SS "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SS" "X" "3" "9" "1" "02D-SS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHD" " " "3" "13" "9" "D-SS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SHD" "X" "3" "10" "5" " " "D-SHD" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SHD" "Z" "3" "15" "3" "01D-SHD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SHD" BY REFERENCE T-NKY "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SHD" "X" "3" "18" "1" "02D-SHD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SSI" " " "3" "13" "9" "D-SHD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SSI" "X" "3" "19" "5" " " "D-SSI" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SSI" "Z" "3" "24" "3" "01D-SSI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SSI" BY REFERENCE T-SSI "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SSI" "X" "3" "27" "1" "02D-SSI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHC" " " "3" "13" "12" "D-SSI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SHC" "X" "3" "28" "11" " " "D-SHC" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-SHC" BY REFERENCE W-SHC "11" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SHC" "X" "3" "39" "1" "01D-SHC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SG" " " "3" "13" "14" "D-SHC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SG" "N" "3" "40" "6" " " "D-SG" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SG" "N" "3" "47" "4" "01D-SG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SG" BY REFERENCE W-SGT "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SG" "9.9" "3" "51" "3" "02D-SG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-SG" BY REFERENCE T-SGR "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-SG" "X" "3" "54" "1" "03D-SG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ST" " " "3" "13" "14" "D-SG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-ST" "N" "3" "55" "6" " " "D-ST" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-ST" "N" "3" "62" "4" "01D-ST" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-ST" BY REFERENCE W-STT "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-ST" "9.9" "3" "66" "3" "02D-ST" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-ST" BY REFERENCE T-STR "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-ST" "X" "3" "69" "1" "03D-ST" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SKR" " " "3" "13" "10" "D-ST" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SKR" "N" "3" "70" "6" " " "D-SKR" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SKR" "Z" "3" "76" "4" "01D-SKR" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SKR" BY REFERENCE T-SKR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-DSP" " " "W-L" "0" "81" "D-SHJ" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATA" " " "W-L" "13" "73" " " "04C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DATA" "Z9" "W-L" "1" "2" " " "D-DATA" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DATA" BY REFERENCE TUK-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DATA" "Z9" "W-L" "3" "2" "01D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DATA" BY REFERENCE TUK-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-DATA" "X" "W-L" "6" "6" "02D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-DATA" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-DATA" "X" "W-L" "12" "1" "03D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-DATA" BY REFERENCE W-CHK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-DATA" "----,---,---" "W-L" "14" "12" "04D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-DATA" BY REFERENCE W-UKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-DATA" "-----,---" "W-L" "27" "9" "05D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06D-DATA" BY REFERENCE W-USHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-DATA" "----,---,---" "W-L" "37" "12" "06D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07D-DATA" BY REFERENCE W-NKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-DATA" "----,---" "W-L" "50" "8" "07D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08D-DATA" BY REFERENCE W-NSHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "09D-DATA" "----,---,---" "W-L" "59" "12" "08D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "09D-DATA" BY REFERENCE W-ZKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "10D-DATA" "-----,---" "W-L" "72" "9" "09D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "10D-DATA" BY REFERENCE W-ZSHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZSM" "N" "W-L" "50" "8" "D-DATA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-ZSM" BY REFERENCE W-ZSM "8" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "95" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "95" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "15" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "20" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           INITIALIZE W-DATA.
           COPY LIBCPR.
      *
           MOVE ZERO TO W-NGP.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-HENG.
           ADD 1 TO W-GET.
           IF  W-GET = 13
               MOVE 1 TO W-GET
               ADD 1 TO W-NEN
           END-IF
           SUBTRACT 1 FROM W-NEN.
           MOVE W-NG TO W-HSNG.
      *
           MOVE ZERO TO W-NGP.
           MOVE D-NKNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-KENG.
           ADD 1 TO W-GET.
           IF  W-GET = 13
               MOVE 1 TO W-GET
               ADD 1 TO W-NEN
           END-IF
           SUBTRACT 1 FROM W-NEN.
           MOVE W-NG TO W-KSNG.
      *
           CALL "DB_F_Open" USING
            "INPUT" TUKF_PNAME1 "SHARED" BY REFERENCE TUKF_IDLST "1"
            "TUK-KEY" BY REFERENCE TUK-KEY.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
       M-10.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHK15" RETURNING RESU.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           PERFORM S-05 THRU S-25.
      *
           MOVE SPACE TO TUK-KEY.
           MOVE W-TCD TO TUK-TCD.
      *           START TUKF KEY NOT < TUK-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TUKF_PNAME1 "TUK-KEY" " NOT < " TUK-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF.
       M-20.
      *           READ TUKF NEXT RECORD WITH UNLOCK AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TUKF_PNAME1 BY REFERENCE TUK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  W-TCD NOT = TUK-TCD
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  ZERO = TUK-KIN AND TUK-SHZ
               GO TO M-20
           END-IF
           MOVE ZERO TO W-D.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-GP "A-GP" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-GETD > 12
               GO TO M-25
           END-IF
           IF  W-PEYD > 31
               GO TO M-25
           END-IF
      *
           MOVE ZERO TO W-NGP.
           MOVE W-GETD TO W-GET.
           MOVE W-PEYD TO W-PEY.
           IF  W-GPD = ZERO
               IF  T-BC = 0
                   MOVE W-HSNG TO W-NG
                   GO TO M-30
               ELSE
                   MOVE W-KSNG TO W-NG
                   GO TO M-30
               END-IF
           END-IF
           IF  T-BC = 0
               MOVE W-HEN TO W-NEN
               IF  W-HEG < W-GETD
                   SUBTRACT 1 FROM W-NEN
               END-IF
           END-IF
           IF  T-BC NOT = 0
               MOVE W-KEN TO W-NEN
               IF  W-KEG < W-GETD
                   SUBTRACT 1 FROM W-NEN
               END-IF
           END-IF.
       M-30.
           IF  TUK-NG < W-NG
               GO TO M-35
           END-IF
           GO TO M-40.
       M-35.
      *           READ TUKF NEXT RECORD WITH UNLOCK AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TUKF_PNAME1 BY REFERENCE TUK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  W-TCD NOT = TUK-TCD
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  ZERO = TUK-KIN AND TUK-SHZ
               GO TO M-35
           END-IF
           GO TO M-30.
       M-40.
           MOVE SPACE TO W-CHK W-DNO.
           IF  TUK-DC = 0
               MOVE "（前残）" TO W-ZSM
               MOVE ZERO TO W-UKIN W-USHZ W-NKIN W-NSHZ
               MOVE TUK-KIN TO W-ZKIN
               MOVE TUK-SHZ TO W-ZSHZ
           END-IF
           IF  TUK-DC = 1
               MOVE ZERO TO W-NKIN W-NSHZ
               ADD TUK-KIN TO W-ZKIN
               ADD TUK-SHZ TO W-ZSHZ
               IF (TUK-NGP >= W-NGP) OR (W-DC = 1)
                   MOVE TUK-DNO TO W-DNO
                   MOVE TUK-KIN TO W-UKIN
                   MOVE TUK-SHZ TO W-USHZ
               END-IF
           END-IF
           IF  TUK-DC = 2
               MOVE ZERO TO W-NKIN W-NSHZ
               SUBTRACT TUK-KIN FROM W-ZKIN
               SUBTRACT TUK-SHZ FROM W-ZSHZ
               IF (TUK-NGP >= W-NGP) OR (W-DC = 1)
                   MOVE TUK-DNO TO W-DNO
                   MOVE "*" TO W-CHK
                   COMPUTE W-UKIN = TUK-KIN * -1
                   COMPUTE W-USHZ = TUK-SHZ * -1
               END-IF
           END-IF
           IF  TUK-DC = 3
               MOVE ZERO TO W-UKIN W-USHZ
               SUBTRACT TUK-KIN FROM W-ZKIN
               SUBTRACT TUK-SHZ FROM W-ZSHZ
               IF (TUK-NGP >= W-NGP) OR (W-DC = 1)
                   MOVE TUK-DNO TO W-DNO
                   MOVE TUK-KIN TO W-NKIN
                   MOVE TUK-SHZ TO W-NSHZ
               END-IF
           END-IF
           IF  TUK-DC = 4
               MOVE TUK-DNO TO W-DNO
               MOVE "＜請求＞" TO W-ZSM
               MOVE ZERO TO W-UKIN W-USHZ W-NKIN W-NSHZ
               MOVE W-ZKIN TO W-ZKIND
               MOVE W-ZSHZ TO W-ZSHZD
               MOVE TUK-KIN TO W-ZKIN
               MOVE TUK-SHZ TO W-ZSHZ
           END-IF
           IF  W-DC = 0
               IF  TUK-NGP < W-NGP
                   GO TO M-47
               END-IF
           END-IF
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF.
       M-45.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 23
               MOVE 1 TO W-NC
               GO TO M-55
           END-IF
           CALL "SD_Output" USING "D-DATA" D-DATA "p" RETURNING RESU.
           IF  TUK-DC = 0 OR 4
               CALL "SD_Output" USING "D-ZSM" D-ZSM "p" RETURNING RESU
           END-IF.
       M-47.
           IF TUK-DC = 4
               MOVE W-ZKIND TO W-ZKIN
               MOVE W-ZSHZD TO W-ZSHZ
           END-IF.
       M-50.
      *           READ TUKF NEXT RECORD WITH UNLOCK AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TUKF_PNAME1 BY REFERENCE TUK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 2 TO W-NC
               GO TO M-55
           END-IF
           IF  W-TCD NOT = TUK-TCD
               MOVE 2 TO W-NC
               GO TO M-55
           END-IF
           IF  ZERO = TUK-KIN AND TUK-SHZ
               GO TO M-50
           END-IF
           GO TO M-40.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
                GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
                GO TO M-55
           END-IF
           IF  W-NC = 2
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 0
               GO TO M-55
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHK15" RETURNING RESU.
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           PERFORM S-05 THRU S-25.
           GO TO M-45.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE TUKF_IDLST TUKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SHJC" D-SHJC "p" RETURNING RESU.
           IF  T-SS NOT = ZERO
               CALL "SD_Output" USING "D-SS" D-SS "p" RETURNING RESU
           END-IF
           IF  T-NKY NOT = ZERO
               CALL "SD_Output" USING "D-SHD" D-SHD "p" RETURNING RESU
           END-IF
           IF  T-SSI NOT = ZERO
               CALL "SD_Output" USING "D-SSI" D-SSI "p" RETURNING RESU
           END-IF
           IF  T-SHC1 = ZERO
               GO TO S-10
           END-IF
           MOVE SPACE TO W-SHC
           IF  T-SHC1 = 1
               MOVE "現金" TO W-SHC23F
               IF  T-SHC2 = 2
                   MOVE "小切手" TO W-SHC23R
               ELSE
                   IF  T-SHC2 = 3
                       MOVE "手形" TO W-SHC23R
                   END-IF
               END-IF
           END-IF
           IF  T-SHC1 = 2
               MOVE "小切手" TO W-SHC32F
               IF  T-SHC2 = 1
                   MOVE "現金" TO W-SHC32R
               ELSE
                   IF  T-SHC2 = 3
                       MOVE "手形" TO W-SHC32R
                   END-IF
               END-IF
           END-IF
           IF  T-SHC1 = 3
               MOVE "手形" TO W-SHC23F
               IF  T-SHC2 = 1
                   MOVE "現金" TO W-SHC23R
               ELSE
                   IF  T-SHC2 = 2
                       MOVE "小切手" TO W-SHC23R
                   END-IF
               END-IF
           END-IF
           CALL "SD_Output" USING "D-SHC" D-SHC "p" RETURNING RESU.
       S-10.
           IF  T-SGT = 0
               GO TO S-15
           END-IF
           MOVE SPACE TO W-SGT.
           IF  T-SGT = 1
               MOVE "全体" TO W-SGT
           END-IF
           IF  T-SGT = 2
               MOVE "商品" TO W-SGT
           END-IF
           CALL "SD_Output" USING "D-SG" D-SG "p" RETURNING RESU.
       S-15.
           IF  T-STT = 0
               GO TO S-20
           END-IF
           MOVE SPACE TO W-STT.
           IF  T-STT = 1
               MOVE "全体" TO W-STT
           END-IF
           IF  T-STT = 2
               MOVE "商品" TO W-STT
           END-IF
           CALL "SD_Output" USING "D-ST" D-ST "p" RETURNING RESU.
       S-20.
           IF  T-SKR NOT = ZERO
               CALL "SD_Output" USING "D-SKR" D-SKR "p" RETURNING RESU
           END-IF
           MOVE 0 TO W-DC.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       S-25.
           EXIT.

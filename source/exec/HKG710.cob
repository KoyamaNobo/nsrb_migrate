       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG710.
       DATE-WRITTEN. 1974-05-15.
      *********************************************************
      *    PROGRAM         :  得意先元帳ワーク　作成        　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/20                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0064".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0128".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-FNG.
             03  W-FNEN       PIC  9(004).
             03  W-FGET       PIC  9(002).
           02  W-RNG.
             03  W-RNEN       PIC  9(004).
             03  W-RGET       PIC  9(002).
           02  W-NGP.
             03  W-NG         PIC  9(006).
             03  W-NGD   REDEFINES W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL REDEFINES W-NEN.
                 05  W-NEN1  PIC  9(002).
                 05  W-NEN2  PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-NGL  REDEFINES W-NG.
               04  F          PIC  9(002).
               04  W-NGS      PIC  9(004).
             03  W-PEY        PIC  9(002).
           02  W-SNGP.
             03  W-SNG.
               04  W-SNEN     PIC  9(004).
               04  W-SGET     PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-ENGP.
             03  W-ENG.
               04  W-ENEN     PIC  9(004).
               04  W-EGET     PIC  9(002).
             03  W-EPEY       PIC  9(002).
           02  W-SEC          PIC  9(001).
           02  W-TCD          PIC  9(004).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LICAL.
           COPY LITM.
      *FD  TUKF
       01  TUKF_HKG710.
           02  TUKF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TUKF_LNAME     PIC  X(011) VALUE "TUKF_HKG710".
           02  F              PIC  X(001).
           02  TUKF_KEY1      PIC  X(100) VALUE SPACE.
           02  TUKF_SORT      PIC  X(100) VALUE SPACE.
           02  TUKF_IDLST     PIC  X(100) VALUE SPACE.
           02  TUKF_RES       USAGE  POINTER.
       01  TUK-R.
           02  TUK-KEY.
             03  TUK-TCD      PIC  9(004).
             03  TUK-DAI      PIC  X(010).
           02  TUK-KEY2.
             03  TUK-TCD2     PIC  9(004).
             03  TUK-DATE     PIC  9(008).
             03  TUK-NGP   REDEFINES TUK-DATE.
               04  TUK-NG.
                 05  TUK-NEN  PIC  9(004).
                 05  TUK-GET  PIC  9(002).
               04  TUK-PEY    PIC  9(002).
             03  TUK-DC       PIC  9(001).
             03  TUK-DNO      PIC  X(006).
             03  TUK-GNO      PIC  9(001).
           02  TUK-KIN        PIC S9(009).
           02  TUK-SHZ        PIC S9(007).
           02  TUK-SKD        PIC  9(008).
           02  TUK-DCC        PIC  9(001).
           02  TUK-TNC        PIC  9(002).
           02  TUK-BMC        PIC  9(001).
           02  F              PIC  X(066).
       77  F                  PIC  X(001).
      *FD  TMS-F
       01  TMS-F_HKG710.
           02  TMS-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TMS-F_LNAME    PIC  X(012) VALUE "TMS-F_HKG710".
           02  F              PIC  X(001).
           02  TMS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  TMS-F_SORT     PIC  X(100) VALUE SPACE.
           02  TMS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  TMS-F_RES      USAGE  POINTER.
       01  TMS-R.
           02  TMS-TCD        PIC  9(004).
           02  TMS-DATE       PIC  9(008).
           02  TMS-KIN        PIC S9(009).
           02  TMS-SHZ        PIC S9(007).
           02  TMS-DC         PIC  9(001).
           02  TMS-BC         PIC  9(001).
           02  TMS-TNC        PIC  9(002).
           02  TMS-DCC        PIC  9(001).
           02  F              PIC  X(031).
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
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　得意先元帳ワーク　作成　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(021) VALUE
                "【　'  年   月 分　】".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-NEN   PIC  9(002).
             03  A-GET   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME9.
               04  FILLER  PIC  X(027) VALUE
                    "***  CALNM ﾅｼ (      )  ***".
               04  02E-ME9 PIC  X(006).
             03  E-KEY   PIC  9(004).
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "337" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "42" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "14" "20" "21" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "20" "21" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "14" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-NEN" "9" "14" "25" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-NEN" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-GET" "9" "14" "30" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "38" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "62" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "62" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "15" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME9" " " "24" "0" "33" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01E-ME9" "X" "24" "15" "27" " " "E-ME9" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-ME9" "X" "24" "30" "6" "01E-ME9" " " RETURNING RESU.
       CALL "SD_From" USING
           "02E-ME9" BY REFERENCE W-NG "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "9" "24" "35" "4" "E-ME9" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE T-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-KEY" " " RETURNING RESU.
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
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
      *
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU.
           MOVE W-NG TO W-RNG W-FNG.
           SUBTRACT 1 FROM W-FGET.
           IF  W-FGET = ZERO
               SUBTRACT 1 FROM W-FNEN
               MOVE 12 TO W-FGET
           END-IF
           SUBTRACT 1 FROM W-FGET.
           IF  W-FGET = ZERO
               SUBTRACT 1 FROM W-FNEN
               MOVE 12 TO W-FGET
           END-IF
           SUBTRACT 1 FROM W-FGET.
           IF  W-FGET = ZERO
               SUBTRACT 1 FROM W-FNEN
               MOVE 12 TO W-FGET
           END-IF
           GO TO M-20.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
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
           END-IF
           IF  W-NEN < W-FNEN OR > W-RNEN
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-15
           END-IF
           IF  W-NG < W-FNG OR > W-RNG
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
           MOVE W-NG TO W-SNG W-ENG.
           SUBTRACT 1 FROM W-SGET.
           IF  W-SGET = ZERO
               MOVE 12 TO W-SGET
               SUBTRACT 1 FROM W-SNEN
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" CALNM_PNAME1 "SHARED" BY REFERENCE CALNM_IDLST "1"
            "CL-KEY" BY REFERENCE CL-KEY.
       M-25.
           MOVE ZERO TO CL-KEY.
           IF W-SEC = 0
               MOVE W-SNG TO CL-NG
           ELSE
               MOVE W-ENG TO CL-NG
           END-IF
      *           START CALNM KEY NOT < CL-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CALNM_PNAME1 "CL-KEY" "NOT <" CL-KEY RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE CALNM_IDLST CALNM_PNAME1
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *           READ CALNM NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CALNM_PNAME1 BY REFERENCE CALN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE CALNM_IDLST CALNM_PNAME1
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  W-SEC = 0
               IF  W-SNG NOT = CL-NG
                   CALL "DB_F_Close" USING
                    BY REFERENCE CALNM_IDLST CALNM_PNAME1
                   CALL "SD_Output" USING
                    "E-ME9" E-ME9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-95
               END-IF
           END-IF
           IF  W-SEC NOT = 0
               IF  W-ENG NOT = CL-NG
                   CALL "DB_F_Close" USING
                    BY REFERENCE CALNM_IDLST CALNM_PNAME1
                   CALL "SD_Output" USING
                    "E-ME9" E-ME9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-95
               END-IF
           END-IF.
       M-30.
           IF  W-SEC = 0
               MOVE CL-KEY TO W-SNGP
           ELSE
               MOVE CL-KEY TO W-ENGP
           END-IF
      *           READ CALNM NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CALNM_PNAME1 BY REFERENCE CALN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           IF  W-SEC = 0
               IF  W-SNG = CL-NG
                   GO TO M-30
               END-IF
           END-IF
           IF  W-SEC NOT = 0
               IF  W-ENG = CL-NG
                   GO TO M-30
               END-IF
           END-IF.
       M-35.
           IF  W-SEC = 0
               MOVE 1 TO W-SEC
               GO TO M-25
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE CALNM_IDLST CALNM_PNAME1.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0064ID.
           MOVE W-FID2 TO WK0128ID.
           MOVE WK0064ID TO TMS-F_PNAME1.
           MOVE WK0128ID TO TUKF_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" TMS-F_PNAME1 " " BY REFERENCE TMS-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TUKF_PNAME1 " " BY REFERENCE TUKF_IDLST "0".
       M-40.
      *           READ TUKF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TUKF_PNAME1 BY REFERENCE TUK-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  TUK-NG > W-NG
               GO TO M-90
           END-IF
           IF  TUK-NG < W-NG
               GO TO M-40
           END-IF
           IF  TUK-TCD = W-TCD
               GO TO M-45
           END-IF
           MOVE TUK-TCD TO W-TCD.
           MOVE TUK-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO T-DCC T-TNC T-BC
           END-IF
      *
           IF  T-SS = 99
               GO TO M-40
           END-IF.
       M-45.
           MOVE ZERO TO TMS-R.
           MOVE TUK-TCD TO TMS-TCD.
           IF  TUK-DC = 0
               MOVE W-SNGP TO TMS-DATE
           END-IF
           IF  TUK-DC = 1 OR 2
               IF  TUK-PEY > 25
                   MOVE W-ENGP TO TMS-DATE
               ELSE
                   IF  TUK-PEY > 20
                       MOVE 25 TO W-PEY
                       MOVE W-NGP TO TMS-DATE
                   ELSE
                       IF  TUK-PEY > 15
                           MOVE 20 TO W-PEY
                           MOVE W-NGP TO TMS-DATE
                       ELSE
                           IF  TUK-PEY > 10
                               MOVE 15 TO W-PEY
                               MOVE W-NGP TO TMS-DATE
                           ELSE
                               IF  TUK-PEY > 05
                                   MOVE 10 TO W-PEY
                                   MOVE W-NGP TO TMS-DATE
                               ELSE
                                   MOVE 05 TO W-PEY
                                   MOVE W-NGP TO TMS-DATE
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  TUK-DC = 3 OR 4
               MOVE TUK-DATE TO TMS-DATE
           END-IF
           IF  TUK-DC = 2
               COMPUTE TMS-KIN = -1 * TUK-KIN
               COMPUTE TMS-SHZ = -1 * TUK-SHZ
           ELSE
               MOVE TUK-KIN TO TMS-KIN
               MOVE TUK-SHZ TO TMS-SHZ
           END-IF
           IF  TUK-DC = 0
               MOVE 0 TO TMS-DC
           END-IF
           IF  TUK-DC = 1 OR 2
               MOVE 1 TO TMS-DC
           END-IF
           IF  TUK-DC = 3
               MOVE 2 TO TMS-DC
           END-IF
           IF  TUK-DC = 4
               MOVE 3 TO TMS-DC
           END-IF
           MOVE T-DCC TO TMS-DCC.
           MOVE T-TNC TO TMS-TNC.
           MOVE T-BC TO TMS-BC.
      *           WRITE TMS-R.
      *///////////////
           CALL "DB_Insert" USING
            TMS-F_PNAME1 TMS-F_LNAME TMS-R RETURNING RET.
           GO TO M-40.
      *
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE TUKF_IDLST TUKF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TMS-F_IDLST TMS-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
      *
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

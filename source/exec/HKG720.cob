       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG720.
       DATE-WRITTEN. 1974-05-15.
      *********************************************************
      *    PROGRAM         :  得意先元帳ワーク　作成        　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  11/09/21                        *
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
           02  W-BM           PIC  9(001).
           02  W-STCD         PIC  9(004).
           02  W-ETCD         PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  W-FNG.
             03  W-FNEN       PIC  9(004).
             03  W-FND   REDEFINES W-FNEN.
               04  W-FN1      PIC  9(002).
               04  W-FN2      PIC  9(002).
             03  W-FGET       PIC  9(002).
           02  W-FNGD  REDEFINES W-FNG.
             03  F            PIC  9(002).
             03  W-FNGS       PIC  9(004).
           02  W-RNG.
             03  W-RNEN       PIC  9(004).
             03  W-RND   REDEFINES W-RNEN.
               04  W-RN1      PIC  9(002).
               04  W-RN2      PIC  9(002).
             03  W-RGET       PIC  9(002).
           02  W-RNGD  REDEFINES W-RNG.
             03  F            PIC  9(002).
             03  W-RNGS       PIC  9(004).
           02  W-NGP.
             03  W-NG         PIC  9(006).
             03  W-NGD   REDEFINES W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-NGL  REDEFINES W-NG.
               04  F          PIC  9(002).
               04  W-NGS      PIC  9(004).
             03  W-PEY        PIC  9(002).
           02  W-SNGP.
             03  W-SNG.
               04  W-SNEN     PIC  9(004).
               04  W-SND   REDEFINES W-SNEN.
                 05  W-SN1    PIC  9(002).
                 05  W-SN2    PIC  9(002).
               04  W-SGET     PIC  9(002).
             03  W-SNGD  REDEFINES W-SNG.
               04  F          PIC  9(002).
               04  W-SNGS     PIC  9(004).
             03  W-SPEY       PIC  9(002).
           02  W-ENGP.
             03  W-ENG.
               04  W-ENEN     PIC  9(004).
               04  W-END   REDEFINES W-ENEN.
                 05  W-EN1    PIC  9(002).
                 05  W-EN2    PIC  9(002).
               04  W-EGET     PIC  9(002).
             03  W-ENGD  REDEFINES W-ENG.
               04  F          PIC  9(002).
               04  W-ENGS     PIC  9(004).
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
       01  TUKF_HKG720.
           02  TUKF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TUKF_LNAME     PIC  X(011) VALUE "TUKF_HKG720".
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
       01  TMS-F_HKG720.
           02  TMS-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TMS-F_LNAME    PIC  X(012) VALUE "TMS-F_HKG720".
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
           02  TMS-NGP.
             03  TMS-NG       PIC  9(006).
             03  F            PIC  9(002).
           02  TMS-SNG        PIC  9(004).
           02  TMS-ENG        PIC  9(004).
           02  TMS-BM         PIC  9(001).
           02  TMS-STCD       PIC  9(004).
           02  TMS-ETCD       PIC  9(004).
           02  F              PIC  X(006).
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
           02  FILLER  PIC  X(039) VALUE
                  "データ期間   '  年   月　～　'  年   月".
           02  FILLER  PIC  X(039) VALUE
                  " 抽出期間    '  年   月　～　'  年   月".
           02  FILLER  PIC  X(042) VALUE
                  "全体 = 0  ,  履物 = 1  ,  工品他 = 2   [ ]".
           02  FILLER  PIC  X(024) VALUE
                  "得意先ｺｰﾄﾞ  0000 ～ 9999".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-DSP.
           02  D-DNG.
             03  01D-DNG  PIC  9(002).
             03  02D-DNG  PIC  Z(002).
             03  03D-DNG  PIC  9(002).
             03  04D-DNG  PIC  Z(002).
       01  C-ACP.
           02  FILLER.
             03  A-SNEN  PIC  9(002).
             03  A-SGET  PIC  9(002).
             03  A-ENEN  PIC  9(002).
             03  A-EGET  PIC  9(002).
           02  A-BM    PIC  9(001).
           02  FILLER.
             03  A-STCD  PIC  9(004).
             03  A-ETCD  PIC  9(004).
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
           "C-MID" " " "0" "0" "460" " " " " RETURNING RESU.
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
           "08C-MID" "X" "11" "17" "39" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "13" "17" "39" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "10C-MID" "X" "16" "17" "42" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "11C-MID" "X" "18" "17" "24" "10C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "12C-MID" "X" "22" "30" "22" "11C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-DNG" " " "11" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "01D-DNG" "9" "11" "31" "2" " " "D-DNG" RETURNING RESU.
       CALL "SD_From" USING
           "01D-DNG" BY REFERENCE W-FN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-DNG" "Z" "11" "36" "2" "01D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING
           "02D-DNG" BY REFERENCE W-FGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-DNG" "9" "11" "47" "2" "02D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING
           "03D-DNG" BY REFERENCE W-RN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "04D-DNG" "Z" "11" "52" "2" "03D-DNG" " " RETURNING RESU.
       CALL "SD_From" USING
           "04D-DNG" BY REFERENCE W-RGET "2" "0" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "18" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "13" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNEN" "9" "13" "31" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNEN" BY REFERENCE W-SN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SGET" "9" "13" "36" "2" "A-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENEN" "9" "13" "47" "2" "A-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENEN" BY REFERENCE W-EN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EGET" "9" "13" "52" "2" "A-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BM" "9" "16" "57" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BM" BY REFERENCE W-BM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "18" "0" "8" "A-BM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STCD" "9" "18" "29" "4" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ETCD" "9" "18" "37" "4" "A-STCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "47" "1" "03C-ACP" " " RETURNING RESU.
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
           MOVE 9999 TO W-ETCD.
           COPY LIBCPR.
      *
           MOVE D-SNG TO W-FNGS.
           MOVE D-NHNG TO W-RNGS W-SNGS W-ENGS.
           MOVE 20 TO W-FN1 W-RN1 W-SN1 W-EN1.
           CALL "SD_Output" USING "D-DNG" D-DNG "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SNEN" A-SNEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SGET" A-SGET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ENEN" A-ENEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-EGET" A-EGET "p" RETURNING RESU.
           PERFORM ACP-RTN THRU ACP-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
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
      *
           MOVE W-SNG TO W-FNG W-NG.
           MOVE W-ENG TO W-RNG.
       M-25.
           MOVE W-NG TO W-SNG W-ENG.
           SUBTRACT 1 FROM W-SGET.
           IF W-SGET = ZERO
               MOVE 12 TO W-SGET
               SUBTRACT 1 FROM W-SNEN
           END-IF
           MOVE 0 TO W-SEC.
      *
           CALL "DB_F_Open" USING
            "INPUT" CALNM_PNAME1 "SHARED" BY REFERENCE CALNM_IDLST "1"
            "CL-KEY" BY REFERENCE CL-KEY.
       M-30.
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
       M-35.
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
               GO TO M-40
           END-IF
           IF  W-SEC = 0
               IF  W-SNG = CL-NG
                   GO TO M-35
               END-IF
           END-IF
           IF  W-SEC NOT = 0
               IF  W-ENG = CL-NG
                   GO TO M-35
               END-IF
           END-IF.
       M-40.
           IF  W-SEC = 0
               MOVE 1 TO W-SEC
               GO TO M-30
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE CALNM_IDLST CALNM_PNAME1.
      *
           MOVE ZERO TO W-TCD.
           CALL "DB_F_Open" USING
            "INPUT" TUKF_PNAME1 " " BY REFERENCE TUKF_IDLST "0".
       M-45.
      *           READ TUKF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TUKF_PNAME1 BY REFERENCE TUK-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           IF  TUK-TCD < W-STCD OR > W-ETCD
               GO TO M-45
           END-IF
           IF  W-BM = 1
               IF  TUK-BMC NOT = 0
                   GO TO M-45
               END-IF
           END-IF
           IF  W-BM = 2
               IF  TUK-BMC = 0
                   GO TO M-45
               END-IF
           END-IF
           IF  TUK-NG > W-NG
               GO TO M-55
           END-IF
           IF  TUK-NG < W-NG
               GO TO M-45
           END-IF
           IF  TUK-TCD = W-TCD
               GO TO M-50
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
           IF  T-SS = 99
               GO TO M-45
           END-IF.
       M-50.
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
           MOVE W-NG TO TMS-NG.
           MOVE W-FNGS TO TMS-SNG.
           MOVE W-RNGS TO TMS-ENG.
           MOVE W-BM TO TMS-BM.
           MOVE W-STCD TO TMS-STCD.
           MOVE W-ETCD TO TMS-ETCD.
      *           WRITE TMS-R.
      *///////////////
           CALL "DB_Insert" USING
            TMS-F_PNAME1 TMS-F_LNAME TMS-R RETURNING RET.
           GO TO M-45.
       M-55.
           CALL "DB_F_Close" USING BY REFERENCE TUKF_IDLST TUKF_PNAME1.
           ADD 1 TO W-GET.
           IF  W-GET = 13
               MOVE 1 TO W-GET
               ADD 1 TO W-NEN
           END-IF
           IF  W-RNG NOT < W-NG
               GO TO M-25
           END-IF.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE TMS-F_IDLST TMS-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-SNEN "A-SNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-RTN
           END-IF
           IF  W-SNEN < W-FNEN
               GO TO ACP-RTN
           END-IF.
       ACP-020.
           CALL "SD_Accept" USING BY REFERENCE A-SGET "A-SGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-020
           END-IF
           IF  W-SGET < 1 OR > 12
               GO TO ACP-020
           END-IF
           IF  W-SNG < W-FNG
               GO TO ACP-020
           END-IF
           MOVE W-SNG TO W-NG.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
           ELSE
               ADD 1 TO W-NEN
           END-IF.
       ACP-040.
           CALL "SD_Accept" USING BY REFERENCE A-ENEN "A-ENEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-040
           END-IF
           IF  W-ENEN > W-RNEN
               GO TO ACP-040
           END-IF.
       ACP-060.
           CALL "SD_Accept" USING BY REFERENCE A-EGET "A-EGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-060
           END-IF
           IF  W-EGET < 1 OR > 12
               GO TO ACP-060
           END-IF
           IF  W-ENG > W-RNG
               GO TO ACP-060
           END-IF
           IF  W-ENG > W-NG
               GO TO ACP-060
           END-IF.
       ACP-080.
           CALL "SD_Accept" USING BY REFERENCE A-BM "A-BM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-060
           END-IF
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-080
           END-IF
           IF  W-BM > 2
               GO TO ACP-080
           END-IF.
       ACP-100.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-080
           END-IF
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-100
           END-IF.
       ACP-120.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-100
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-100
           END-IF
           IF  W-STCD > W-ETCD
               GO TO ACP-120
           END-IF.
       ACP-140.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-140
           END-IF
           IF  W-DMM = 9
               GO TO ACP-RTN
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-140
           END-IF.
       ACP-EX.
           EXIT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.  KHT210.
      *********************************************************
      *    PROGRAM         :  工品預り　受払問合せ　　　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCKT21                          *
      *        変更　　　  :  62/04/03                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-PAGE             PIC  9(002)  VALUE ZERO.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  N(023) VALUE
                "＊＊＊　　工品日付別　預り受払明細表　　＊＊＊".
           02  F              PIC  X(015) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(015) VALUE " ｺｰﾄﾞ  品　　名".
           02  F              PIC  X(019) VALUE SPACE.
           02  F              PIC  X(032) VALUE
                "単価　 日付　　預り数　　 預り額".
           02  F              PIC  X(040) VALUE
                "   出荷数 　　出荷額　　 残数 　　残金額".
       01  W-P.
           02  P-HCD          PIC  X(005).
           02  F              PIC  X(002).
           02  P-HNA          PIC  X(020).
           02  P-T            PIC ----,--9.99.
           02  F              PIC  X(002).
           02  P-GP           PIC 99/99.
           02  P-AUS          PIC ------,--9.
           02  P-AUK          PIC ---,---,--9.
           02  P-KM           PIC  X(020).
           02  P-AH    REDEFINES P-KM.
             03  P-AHS        PIC -----,--9.
             03  P-AHK        PIC ---,---,--9.
           02  P-ASU          PIC -----,--9.
           02  P-AKI          PIC ---,---,--9.
       01  W-DD.
           02  W-AUS          PIC S9(006).
           02  W-AUK          PIC S9(008).
           02  W-AHS          PIC S9(006).
           02  W-AHK          PIC S9(008).
       01  WT-D.
           02  WT-AUS         PIC S9(006).
           02  WT-AUK         PIC S9(008).
           02  WT-AHS         PIC S9(006).
           02  WT-AHK         PIC S9(008).
           02  WT-ASU         PIC S9(006).
           02  WT-AKI         PIC S9(008).
       01  W-D.
           02  W-DATE.
             03  W-NEN        PIC  9(004).
             03  W-GP         PIC  9(004).
             03  W-GPD   REDEFINES W-GP.
               04  W-GET      PIC  9(002).
               04  W-PEY      PIC  9(002).
           02  W-HCD          PIC  X(005).
           02  W-SEHCD.
             03  W-SHCD       PIC  X(005).
             03  W-EHCD       PIC  X(005).
           02  W-DMM          PIC  9(001).
           02  W-EC           PIC  9(001).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  W-NGP          PIC  9(006).
           02  W-YMD   REDEFINES W-NGP.
             03  W-ND         PIC  9(002).
             03  W-GD         PIC  9(002).
             03  W-PD         PIC  9(002).
           02  W-LC.
             03  W-L          PIC  9(002).
             03  W-C1         PIC  9(002).
             03  W-C2         PIC  9(002).
             03  W-C3         PIC  9(002).
             03  W-C4         PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKHM.
           COPY LIKHT1.
           COPY LSPF.
      *FD  URIR-F
       01  URIR-F_KHT210.
           02  URIR-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  URIR-F_LNAME   PIC  X(013) VALUE "URIR-F_KHT210".
           02  F              PIC  X(001).
           02  URIR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  URIR-F_SORT    PIC  X(100) VALUE SPACE.
           02  URIR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  URIR-F_RES     USAGE  POINTER.
       01  URIR-R.
           02  UR-DC          PIC  9(001).
           02  UR-DATE        PIC  9(008).
           02  UR-TCD         PIC  9(004).
           02  UR-HCD         PIC  X(005).
           02  UR-SU          PIC S9(006)V9(02).
           02  F              PIC  X(016).
           02  UR-YC          PIC  9(002).
           02  F              PIC  X(084).
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　工品日付別預り受払表　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ACP.
           02  A-DMM1  PIC  X(001).
           02  A-HCD   PIC  X(005).
           02  A-DMM2  PIC  X(001).
           02  FILLER.
             03  A-SHCD  PIC  X(005).
             03  A-EHCD  PIC  X(005).
           02  A-DMM3  PIC  X(001).
       01  C-DSP.
           02  D-TPM   PIC  X(038) VALUE
                "<  問合せ=0 打出し=5 終り=9   ﾘﾀｰﾝ   >".
           02  D-PM.
             03  FILLER  PIC  X(036) VALUE
                  "[   ｺｰﾄﾞ      より     まで打出し  ]".
             03  FILLER  PIC  X(028) VALUE
                  "(  確認  OK=1 NO=9   ﾘﾀｰﾝ  )".
           02  D-GP.
             03  01D-GP  PIC  Z(002).
             03  02D-GP  PIC  Z(002).
           02  D-HNA   PIC  X(020).
           02  D-DD.
             03  01D-DD  PIC  Z(002) .
             03  02D-DD  PIC ----,--- .
             03  03D-DD  PIC ----,--- .
             03  04D-DD  PIC ----,--- .
           02  D-TD.
             03  01D-TD  PIC ----,--- .
             03  02D-TD  PIC ----,--- .
             03  03D-TD  PIC ----,--- .
           02  D-NM    PIC  X(038) VALUE
                "<   ｺｰﾄﾞINPUT = 0  NEXTｺｰﾄﾞ = 5      >".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(030) VALUE
                  "***  KHM ﾅｼ  ***              ".
             03  E-ME2   PIC  X(030) VALUE
                  "***  KHM ｸﾌﾞﾝ ｴﾗｰ  ***        ".
             03  E-ME3   PIC  X(030) VALUE
                  "***  DATA ﾅｼ  ***             ".
             03  E-ME4   PIC  X(030) VALUE
                  "***  DATEM ﾅｼ  ***            ".
             03  E-ME5   PIC  X(030) VALUE
                  "***  KHTM ﾅｼ  ***             ".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-KEY   PIC  X(005).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "308" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "44" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "44" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "44" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "44" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "44" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "44" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "44" "06C-MID" " "  RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "18" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM1" "X" "12" "42" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM1" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "X" "3" "14" "5" "A-DMM1" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM2" "X" "21" "55" "1" "A-HCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM2" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "15" "0" "10" "A-DMM2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHCD" "X" "15" "24" "5" " " "04C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHCD" BY REFERENCE W-SHCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EHCD" "X" "15" "33" "5" "A-SHCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EHCD" BY REFERENCE W-EHCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM3" "X" "17" "39" "1" "04C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM3" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "214" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TPM" "X" "12" "13" "38" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PM" " " "0" "0" "64" "D-TPM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-PM" "X" "15" "15" "36" " " "D-PM"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-PM" "X" "17" "19" "28" "01D-PM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GP" " " "2" "0" "4" "D-PM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-GP" "Z" "2" "61" "2" " " "D-GP"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-GP" BY REFERENCE W-GD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-GP" "Z" "2" "65" "2" "01D-GP" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "02D-GP" BY REFERENCE W-PD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNA" "X" "3" "25" "20" "D-GP" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-HNA" BY REFERENCE KH-NAME "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DD" " " "W-L" "0" "26" "D-HNA" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DD" "Z" "W-L" "W-C1" "2" " " "D-DD"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DD" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DD" "----,---" "W-L" "W-C2" "8" "01D-DD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DD" BY REFERENCE W-AUS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-DD" "----,---" "W-L" "W-C3" "8" "02D-DD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-DD" BY REFERENCE W-AHS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-DD" "----,---" "W-L" "W-C4" "8" "03D-DD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-DD" BY REFERENCE WT-ASU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TD" " " "19" "0" "24" "D-DD" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TD" "----,---" "19" "43" "8" " " "D-TD"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-TD" BY REFERENCE WT-AUS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TD" "----,---" "19" "52" "8" "01D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-TD" BY REFERENCE WT-AHS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-TD" "----,---" "19" "61" "8" "02D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-TD" BY REFERENCE WT-ASU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NM" "X" "21" "22" "38" "D-TD" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "215" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "215" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "30" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "30" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "30" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "30" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "30" "E-ME4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "50" "5" "E-ME99" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE KH-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-KEY" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO URIR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" M-DATE_PNAME1 "SHARED" BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
           MOVE ZERO TO W-NGP.
       M-040.
           MOVE "01" TO DATE-KEY.
      *           READ M-DATE WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" M-DATE_PNAME1 BY REFERENCE DATE-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-040
           END-IF
           MOVE D-KUD TO W-NGP.
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
       M-060.
           CALL "SD_Output" USING "D-TPM" D-TPM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM1 "A-DMM1" "X" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM = 5
               GO TO M-600
           END-IF
           IF  W-DMM NOT = ZERO
               GO TO M-060
           END-IF.
       M-200.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KHT-M_PNAME1 "SHARED" BY REFERENCE KHT-M_IDLST "1"
            "KHT-KEY" BY REFERENCE KHT-KEY.
           CALL "DB_F_Open" USING
            "INPUT" URIR-F_PNAME1 " " BY REFERENCE URIR-F_IDLST "0".
       M-220.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCKT21" RETURNING RESU.
           CALL "SD_Output" USING "D-GP" D-GP "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE URIR-F_IDLST URIR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" URIR-F_PNAME1 " " BY REFERENCE URIR-F_IDLST "0".
       M-240.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "X" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               CALL "DB_F_Close" USING
                BY REFERENCE KH-M_IDLST KH-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE KHT-M_IDLST KHT-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE URIR-F_IDLST URIR-F_PNAME1
               GO TO M-020
           END-IF
           IF  ESTAT = C2 OR PF9
               CALL "DB_F_Close" USING
                BY REFERENCE KH-M_IDLST KH-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE KHT-M_IDLST KHT-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE URIR-F_IDLST URIR-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-240
           END-IF
           MOVE W-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               GO TO M-240
           END-IF
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           IF  KH-YC = 10 OR 11
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               GO TO M-240
           END-IF
           MOVE W-HCD TO KHT-KEY.
      *           READ KHT-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KHT-M_PNAME1 BY REFERENCE KHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-240
           END-IF
           MOVE 0 TO W-EC.
       M-260.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE 9 TO W-C1.
           CALL "SD_Arg_Match_Col" USING "W-C1" "2" W-C1 RETURNING RESU.
           MOVE 12 TO W-C2.
           CALL "SD_Arg_Match_Col" USING "W-C2" "2" W-C2 RETURNING RESU.
           MOVE 21 TO W-C3.
           CALL "SD_Arg_Match_Col" USING "W-C3" "2" W-C3 RETURNING RESU.
           MOVE 30 TO W-C4.
           CALL "SD_Arg_Match_Col" USING "W-C4" "2" W-C4 RETURNING RESU.
           MOVE ZERO TO WT-D.
           IF  KHT-AZS = ZERO
               GO TO M-280
           END-IF
           PERFORM S-15 THRU S-20.
           GO TO M-300.
       M-280.
           IF  KHT-AC = ZERO
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-240
           END-IF.
       M-300.
      *           READ URIR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URIR-F_PNAME1 BY REFERENCE URIR-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 5 TO W-EC
               GO TO M-420
           END-IF.
       M-320.
           IF  W-EC = 5
               GO TO M-420
           END-IF
           IF  W-HCD > UR-HCD
               GO TO M-300
           END-IF
           IF  W-HCD NOT = UR-HCD
               GO TO M-420
           END-IF.
       M-340.
           MOVE UR-DATE TO W-DATE.
           MOVE ZERO TO W-DD.
       M-360.
           IF  UR-DC = 3
               ADD UR-SU TO W-AUS
           END-IF
           IF  UR-DC = 4
               ADD UR-SU TO W-AHS
           END-IF.
       M-380.
      *           READ URIR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URIR-F_PNAME1 BY REFERENCE URIR-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 5 TO W-EC
               GO TO M-400
           END-IF
           IF  W-EC = 5
               GO TO M-420
           END-IF
           IF  W-HCD < UR-HCD
               GO TO M-400
           END-IF
           IF  W-DATE = UR-DATE
               GO TO M-360
           END-IF
           PERFORM S-05 THRU S-10.
           GO TO M-340.
       M-400.
           PERFORM S-05 THRU S-10.
       M-420.
           CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM2 "A-DMM2" "X" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-420
           END-IF
           IF  W-DMM = ZERO
               GO TO M-220
           END-IF
           IF  W-DMM NOT = 5
               GO TO M-420
           END-IF
           CALL "SD_Screen_Output" USING "SCKT21" RETURNING RESU.
           CALL "SD_Output" USING "D-GP" D-GP "p" RETURNING RESU.
       M-440.
      *           READ KH-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-220
           END-IF
           IF  KH-YC = 10 OR 11
               GO TO M-440
           END-IF
           MOVE KH-KEY TO KHT-KEY.
      *           READ KHT-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KHT-M_PNAME1 BY REFERENCE KHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-240
           END-IF
           IF  KHT-AZS NOT = ZERO
               GO TO M-460
           END-IF
           IF  KHT-AC = ZERO
               GO TO M-440
           END-IF.
       M-460.
           MOVE KH-KEY TO W-HCD.
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE 9 TO W-C1.
           CALL "SD_Arg_Match_Col" USING "W-C1" "2" W-C1 RETURNING RESU.
           MOVE 12 TO W-C2.
           CALL "SD_Arg_Match_Col" USING "W-C2" "2" W-C2 RETURNING RESU.
           MOVE 21 TO W-C3.
           CALL "SD_Arg_Match_Col" USING "W-C3" "2" W-C3 RETURNING RESU.
           MOVE 30 TO W-C4.
           CALL "SD_Arg_Match_Col" USING "W-C4" "2" W-C4 RETURNING RESU.
           MOVE ZERO TO WT-D.
           IF  KHT-AZS NOT = ZERO
               PERFORM S-15 THRU S-20
           END-IF
           IF  W-EC = 5
               GO TO M-420
           END-IF
           GO TO M-320.
       M-600.
           CALL "SD_Output" USING "D-PM" D-PM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD "A-SHCD" "X" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-600
           END-IF.
       M-620.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD "A-EHCD" "X" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-600
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-620
           END-IF
           IF  W-SHCD > W-EHCD
               GO TO M-600
           END-IF.
       M-640.
           CALL "SD_Accept" USING BY REFERENCE A-DMM3 "A-DMM3" "X" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-620
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-640
           END-IF
           IF  W-DMM = 9
               GO TO M-600
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-640
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           MOVE W-SHCD TO KH-KEY.
      *           START KH-M KEY NOT < KH-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            KH-M_PNAME1 "KH-KEY" " NOT < " KH-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KH-M_IDLST KH-M_PNAME1
               GO TO M-600
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" KHT-M_PNAME1 "SHARED" BY REFERENCE KHT-M_IDLST "1"
            "KHT-KEY" BY REFERENCE KHT-KEY.
           CALL "DB_F_Open" USING
            "INPUT" URIR-F_PNAME1 " " BY REFERENCE URIR-F_IDLST "0".
           CALL "PR_Open" RETURNING RESP.
           MOVE W-NGP TO H-DATE.
           PERFORM S-45 THRU S-50.
           MOVE ZERO TO CHK.
       M-660.
      *           READ URIR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URIR-F_PNAME1 BY REFERENCE URIR-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 5 TO CHK2
               GO TO M-680
           END-IF
           IF  UR-HCD < W-SHCD
               GO TO M-660
           END-IF.
       M-680.
      *           READ KH-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KH-M_IDLST KH-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE KHT-M_IDLST KHT-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE URIR-F_IDLST URIR-F_PNAME1
               CALL "PR_Close" RETURNING RESP
               GO TO M-020
           END-IF
           IF  W-EHCD < KH-HCD
               CALL "DB_F_Close" USING
                BY REFERENCE KH-M_IDLST KH-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE KHT-M_IDLST KHT-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE URIR-F_IDLST URIR-F_PNAME1
               CALL "PR_Close" RETURNING RESP
               GO TO M-020
           END-IF
           IF  KH-YC = 10 OR 11
               GO TO M-680
           END-IF
           MOVE KH-KEY TO KHT-KEY.
      *           READ KHT-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KHT-M_PNAME1 BY REFERENCE KHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KH-M_IDLST KH-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE KHT-M_IDLST KHT-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE URIR-F_IDLST URIR-F_PNAME1
               CALL "PR_Close" RETURNING RESP
               GO TO M-020
           END-IF
           MOVE ZERO TO WT-D CHK1.
           IF  KHT-AZS NOT = ZERO
               PERFORM S-80 THRU S-85
           END-IF
           IF  CHK2 = 5
               GO TO M-680
           END-IF
           IF  KH-HCD = UR-HCD
               GO TO M-700
           END-IF
           IF  KHT-AZS = ZERO
               GO TO M-680
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO M-680.
       M-700.
           MOVE UR-DATE TO W-DATE.
           MOVE ZERO TO W-DD.
       M-720.
           IF  UR-DC = 3
               ADD UR-SU TO W-AUS
           END-IF
           IF  UR-DC = 4
               ADD UR-SU TO W-AHS
           END-IF.
       M-740.
      *           READ URIR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URIR-F_PNAME1 BY REFERENCE URIR-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 5 TO CHK2
               GO TO M-760
           END-IF
           IF  KH-HCD < UR-HCD
               GO TO M-760
           END-IF
           IF  UR-DATE = W-DATE
               GO TO M-720
           END-IF
           PERFORM S-55 THRU S-65.
           GO TO M-700.
       M-760.
           PERFORM S-55 THRU S-65.
           PERFORM S-70 THRU S-75.
           GO TO M-680.
       S-05.
           COMPUTE WT-ASU = WT-ASU + W-AUS - W-AHS.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 20
               MOVE 6 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE 40 TO W-C1
               CALL "SD_Arg_Match_Col" USING
                "W-C1" "2" W-C1 RETURNING RESU
               MOVE 43 TO W-C2
               CALL "SD_Arg_Match_Col" USING
                "W-C2" "2" W-C2 RETURNING RESU
               MOVE 52 TO W-C3
               CALL "SD_Arg_Match_Col" USING
                "W-C3" "2" W-C3 RETURNING RESU
               MOVE 61 TO W-C4
               CALL "SD_Arg_Match_Col" USING
                "W-C4" "2" W-C4 RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-DD" D-DD "p" RETURNING RESU.
           ADD W-AUS TO WT-AUS.
           ADD W-AHS TO WT-AHS.
       S-10.
           EXIT.
       S-15.
           MOVE ZERO TO W-PEY W-AUS W-AHS.
           MOVE KHT-AZS TO WT-ASU.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           CALL "SD_Output" USING "D-DD" D-DD "p" RETURNING RESU.
       S-20.
           EXIT.
       S-40.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-45.
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
       S-50.
           EXIT.
       S-55.
           COMPUTE WT-ASU = WT-ASU + W-AUS - W-AHS.
           COMPUTE W-AUK = W-AUS * KH-T1.
           COMPUTE W-AHK = W-AHS * KH-T1.
           COMPUTE WT-AKI = WT-ASU * KH-T1.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-40 THRU S-50
           END-IF
           MOVE SPACE TO SP-R W-P.
           IF  CHK1 = ZERO
               MOVE 5 TO CHK1
               MOVE KH-HCD TO P-HCD
               MOVE KH-NAME TO P-HNA
               MOVE KH-T1 TO P-T
           END-IF.
       S-60.
           MOVE W-GP TO P-GP.
           MOVE W-AUS TO P-AUS.
           MOVE W-AUK TO P-AUK.
           MOVE W-AHS TO P-AHS.
           MOVE W-AHK TO P-AHK.
           MOVE WT-ASU TO P-ASU.
           MOVE WT-AKI TO P-AKI.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD W-AUS TO WT-AUS.
           ADD W-AHS TO WT-AHS.
       S-65.
           EXIT.
       S-70.
           COMPUTE WT-AUK = WT-AUS * KH-T1.
           COMPUTE WT-AHK = WT-AHS * KH-T1.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-40 THRU S-50
           END-IF
           MOVE SPACE TO SP-R W-P.
           MOVE "＊＊ ＴＯＴＡＬ ＊＊" TO P-HNA.
           MOVE WT-AUS TO P-AUS.
           MOVE WT-AUK TO P-AUK.
           MOVE WT-AHS TO P-AHS.
           MOVE WT-AHK TO P-AHK.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-75.
           EXIT.
       S-80.
           MOVE SPACE TO SP-R W-P.
           MOVE KH-HCD TO P-HCD.
           MOVE KH-NAME TO P-HNA.
           MOVE KH-T1 TO P-T.
           MOVE "  <   前月繰越   >  " TO P-AH.
           MOVE KHT-AZS TO P-ASU.
           COMPUTE WT-AKI = KHT-AZS * KH-T1.
           MOVE WT-AKI TO P-AKI.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE KHT-AZS TO WT-ASU.
           MOVE 5 TO CHK1.
       S-85.
           EXIT.

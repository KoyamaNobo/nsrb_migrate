       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD990.
      *************************************************************
      *    PROGRAM         :  śŚż[NěŹiGNZĎˇpj*
      *    PRINTER TYPE    :  *****                               *
      *    SCREEN          :  ******                              *
      *    COMPILE TYPE    :  COBOL                               *
      *    JS-SIGN         :  0=­să  1=­sO                  *
      *************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM100.
       OBJECT-COMPUTER. SYSTEM100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  JS-SIGN            PIC  9(001).
       77  W-CC               PIC  9(001).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-FIDI.
           02  W-FID21        PIC  X(006) VALUE "WK0256".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  W-TCD          PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-DNO          PIC  9(006).
           02  W-GNO          PIC  9(001).
           02  W-C            PIC  9(002).
           02  W-SNGP.
             03  W-SNEN       PIC  9(004).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-MKIN         PIC S9(009).
           02  W-KIN          PIC S9(009).
           COPY LSTAT.
      *
           COPY LITM.
           COPY LITSKF.
           COPY LISKDF.
      *FD  SKDW
       01  SKDW_HMD992.
           02  SKDW_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SKDW_LNAME     PIC  X(011) VALUE "SKDW_HMD992".
           02  F              PIC  X(001).
           02  SKDW_KEY1      PIC  X(100) VALUE SPACE.
           02  SKDW_SORT      PIC  X(100) VALUE SPACE.
           02  SKDW_IDLST     PIC  X(100) VALUE SPACE.
           02  SKDW_RES       USAGE  POINTER.
       01  SKDW-R.
           02  SKDW-KEY.
             03  SKDW-TCD     PIC  9(004).
             03  F            PIC  9(008).
             03  SKDW-DTC     PIC  9(001).
             03  SKDW-DNO     PIC  9(006).
             03  SKDW-GNO     PIC  9(001).
           02  F              PIC  9(006).
           02  SKDW-SU        PIC S9(006)V9(02).
           02  SKDW-T         PIC S9(006)V9(02).
           02  SKDW-KIN       PIC S9(009).
           02  SKDW-DC        PIC  9(001).
           02  SKDW-CSC       PIC  9(001).
           02  SKDW-SKD       PIC  9(008).
           02  F              PIC  X(125).
           02  SKDW-SNO       PIC  9(006).
           02  F              PIC  X(064).
       77  F                  PIC  X(001).
      *FD  STRAN3
       01  STRAN3_HMD992.
           02  STRAN3_PNAME1  PIC  X(007) VALUE "STRAN-3".
           02  F              PIC  X(001).
           02  STRAN3_LNAME   PIC  X(013) VALUE "STRAN3_HMD992".
           02  F              PIC  X(001).
           02  STRAN3_KEY1    PIC  X(100) VALUE SPACE.
           02  STRAN3_SORT    PIC  X(100) VALUE SPACE.
           02  STRAN3_IDLST   PIC  X(100) VALUE SPACE.
           02  STRAN3_RES     USAGE  POINTER.
       01  STRAN3-R.
           02  ST3-KEY.
             03  ST3-DNO      PIC  9(006).
             03  ST3-GNO      PIC  9(001).
             03  ST3-DATE.
               04  ST3-NEN    PIC  9(004).
               04  ST3-GET    PIC  9(002).
               04  ST3-PEY    PIC  9(002).
             03  ST3-TCD      PIC  9(004).
           02  ST3-D1.
             03  ST3-HCD      PIC  9(006).
             03  ST3-SIZ      PIC  9(001).
             03  ST3-ASU.
               04 ST3-SUD   OCCURS  10.
                 05  ST3-SU   PIC S9(004)  COMP-3.
             03  ST3-SUT      PIC S9(005).
             03  ST3-T        PIC S9(005).
             03  ST3-KIN      PIC S9(008).
             03  ST3-CSC      PIC  9(001).
             03  ST3-DC       PIC  9(001).
             03  F            PIC  X(005).
             03  ST3-CCD      PIC  9(003).
             03  ST3-BC1      PIC  9(002).
             03  F            PIC  X(028).
             03  ST3-SNGP     PIC  9(008).
             03  F            PIC  X(005).
           02  ST3-D2    REDEFINES ST3-D1.
             03  ST3-BI       PIC  N(024).
             03  F            PIC  X(036).
             03  ST3-SHZ      PIC S9(007).
             03  F            PIC  X(017).
           02  ST3-SNC        PIC  9(001).
       77  F                  PIC  X(001).
      *FD  SNTR-F
       01  SNTR-F_HMD992.
           02  SNTR-F_PNAME1  PIC  X(005) VALUE "SNTRF".
           02  F              PIC  X(001).
           02  SNTR-F_LNAME   PIC  X(013) VALUE "SNTR-F_HMD992".
           02  F              PIC  X(001).
           02  SNTR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  SNTR-F_SORT    PIC  X(100) VALUE SPACE.
           02  SNTR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  SNTR-F_RES     USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DNO       PIC  9(006).
           02  SNTR-GNO       PIC  9(001).
           02  SNTR-DATE.
             03  SNTR-NEN     PIC  9(004).
             03  SNTR-GET     PIC  9(002).
             03  SNTR-PEY     PIC  9(002).
           02  SNTR-TCD       PIC  9(004).
           02  SNTR-D1.
             03  SNTR-HCD     PIC  9(006).
             03  SNTR-SIZ     PIC  9(001).
             03  SNTR-ASU.
               04 SNTR-SUD   OCCURS  10.
                 05  SNTR-SU  PIC S9(004)  COMP-3.
             03  SNTR-SUT     PIC S9(005).
             03  SNTR-T       PIC S9(005).
             03  SNTR-KIN     PIC S9(008).
             03  SNTR-CSC     PIC  9(001).
             03  SNTR-DC      PIC  9(001).
             03  F            PIC  X(005).
             03  SNTR-CCD     PIC  9(003).
             03  SNTR-BC1     PIC  9(002).
             03  F            PIC  X(028).
             03  SNTR-SNGP    PIC  9(008).
             03  F            PIC  X(005).
           02  SNTR-D2    REDEFINES SNTR-D1.
             03  SNTR-BI      PIC  N(024).
             03  F            PIC  X(036).
             03  SNTR-SHZ     PIC S9(007).
             03  F            PIC  X(017).
           02  SNTR-SNC       PIC  9(001).
       77  F                  PIC  X(001).
      *FD  STWF
       01  STWF_HMD992.
           02  STWF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  STWF_LNAME     PIC  X(011) VALUE "STWF_HMD992".
           02  F              PIC  X(001).
           02  STWF_KEY1      PIC  X(100) VALUE SPACE.
           02  STWF_SORT      PIC  X(100) VALUE SPACE.
           02  STWF_IDLST     PIC  X(100) VALUE SPACE.
           02  STWF_RES       USAGE  POINTER.
       01  STW-R              PIC  X(128).
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
           02  FILLER  PIC  N(020) VALUE
                "".
           02  FILLER  PIC  N(020) VALUE
                "".
           02  FILLER  PIC  N(020) VALUE
                "@@@@@@@@@@@@@@".
           02  FILLER  PIC  N(020) VALUE
                "@@śŚż[N@ěŹ@@".
           02  FILLER  PIC  N(020) VALUE
                "@@@@@@@@@@@@@@".
           02  FILLER  PIC  N(020) VALUE
                "".
           02  FILLER  PIC  N(020) VALUE
                "".
           02  FILLER  PIC  N(013) VALUE
                "m@tbs[@Zbg@n".
           02  FILLER  PIC  X(026) VALUE
                "žÓćş°ÄŢ      ".
           02  FILLER  PIC  X(022) VALUE
                "    N      ú  ­s".
           02  FILLER  PIC  X(022) VALUE
                "mF  OK=1 NO=9   ŘŔ°Ý".
       01  C-ACP.
           02  A-TCD   PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-CM    PIC  N(008) VALUE
                "m`FbNpn@".
           02  D-TNA   PIC  N(026).
           02  D-SNGP.
             03  01D-SNGP  PIC  9(004).
             03  02D-SNGP  PIC  9(002).
             03  03D-SNGP  PIC  9(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA Ĺź  ***".
             03  E-ME2   PIC  X(019) VALUE
                  "***  Ä¸˛ťˇ ´×°  ***".
             03  E-ME3   PIC  X(022) VALUE
                  "***  žÓć@Čľ  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  TSKF Ĺź  ***".
             03  E-ME5   PIC  X(023) VALUE
                  "***  ž˛ˇ­łźŽ ĐĘŻşł  ***".
             03  E-ME6   PIC  X(017) VALUE
                  "***  Ň˛ť˛ Ĺź  ***".
             03  E-ME7   PIC  X(019) VALUE
                  "***  ˇÝśŢ¸ ´×°  ***".
             03  E-DNO   PIC  9(006).
             03  E-KIN.
               04  01E-KIN  PIC  -(010).
               04  02E-KIN  PIC  -(010).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "376" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "40" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "40" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "40" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "40" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "40" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "40" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "40" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "bN" "13" "17" "26" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "16" "11" "26" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "18" "11" "22" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-MID" "X" "20" "19" "22" "10C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "5" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "16" "23" "4" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "36" "1" "A-TCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "76" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CM" "N" "7" "22" "16" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "16" "29" "52" "D-CM" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SNGP" " " "18" "0" "8" "D-TNA" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SNGP" "9" "18" "11" "4" " " "D-SNGP"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-SNGP" BY REFERENCE W-SNEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SNGP" "9" "18" "18" "2" "01D-SNGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SNGP" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SNGP" "9" "18" "23" "2" "02D-SNGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-SNGP" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "220" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "220" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "19" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "22" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "23" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "17" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "19" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-DNO" "9" "24" "40" "6" "E-ME7" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-DNO" BY REFERENCE SKD-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KIN" " " "24" "0" "20" "E-DNO" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-KIN" "----------" "24" "35" "10" " " "E-KIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01E-KIN" BY REFERENCE W-MKIN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-KIN" "----------" "24" "45" "10" "01E-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02E-KIN" BY REFERENCE W-KIN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "D-CM" D-CM "p" RETURNING RESU
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
      *
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-10
           END-IF
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           IF  JS-SIGN = 0
               PERFORM S-05 THRU S-20
               IF  W-CC = 1
                   GO TO M-10
               END-IF
           END-IF
      *
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO STWF_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" STWF_PNAME1 " " BY REFERENCE STWF_IDLST "0".
           IF  JS-SIGN = 1
               MOVE STN-NO2 TO W-FID22
               MOVE W-FIDI TO WK0256ID
               MOVE WK0256ID TO SKDW_PNAME1
               CALL "DB_F_Open" USING
                "INPUT" SKDW_PNAME1 " " BY REFERENCE SKDW_IDLST "0"
               GO TO M-50
           END-IF
           CALL "DB_F_Open" USING
            "OUTPUT" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
           MOVE SPACE TO SKD-KEY.
           MOVE W-TCD TO SKD-TCD.
      *           START SKDF KEY NOT < SKD-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SKDF_PNAME1 "SKD-KEY" " NOT < " SKD-KEY RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               DISPLAY E-ME1 E-ME99
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-15.
      *           READ SKDF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  SKD-TCD NOT = W-TCD
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  SKD-SKD NOT = W-SNGP
               GO TO M-15
           END-IF
           IF  SKD-SNO = ZERO
               GO TO M-15
           END-IF
           IF  SKD-DTC = 3
               ADD SKD-KIN TO W-MKIN
           END-IF
           IF  SKD-DTC > 1
               GO TO M-15
           END-IF
           IF  SKD-DC = 4
               GO TO M-15
           END-IF.
       M-20.
           MOVE SKD-DNO TO W-DNO.
           MOVE SKD-GNO TO W-GNO.
           PERFORM S-25 THRU S-60.
       M-30.
      *           READ SKDF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SKD-TCD NOT = W-TCD
               GO TO M-90
           END-IF
           IF  SKD-SKD NOT = W-SNGP
               GO TO M-30
           END-IF
           IF  SKD-SNO = ZERO
               GO TO M-30
           END-IF
           IF  SKD-DTC = 3
               ADD SKD-KIN TO W-MKIN
           END-IF
           IF  SKD-DTC > 1
               GO TO M-30
           END-IF
           IF  SKD-DC = 4
               GO TO M-30
           END-IF
           IF  SKD-DNO = W-DNO
               GO TO M-30
           END-IF
           GO TO M-20.
       M-50.
      *           READ SKDW AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SKDW_PNAME1 BY REFERENCE SKDW-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  SKDW-TCD < W-TCD
               GO TO M-50
           END-IF
           IF  SKDW-TCD > W-TCD
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  SKDW-DTC > 1
               GO TO M-50
           END-IF
           IF  SKDW-DC = 4
               GO TO M-50
           END-IF
           MOVE SKDW-SKD TO W-SNGP.
       M-55.
           MOVE SKDW-DNO TO W-DNO.
           MOVE SKDW-GNO TO W-GNO.
           PERFORM S-25 THRU S-60.
       M-60.
      *           READ SKDW AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SKDW_PNAME1 BY REFERENCE SKDW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SKDW-TCD NOT = W-TCD
               GO TO M-90
           END-IF
           IF  SKDW-DTC > 1
               GO TO M-60
           END-IF
           IF  SKDW-DC = 4
               GO TO M-60
           END-IF
           IF  SKDW-DNO = W-DNO
               GO TO M-60
           END-IF
           GO TO M-55.
       M-90.
           IF  JS-SIGN = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SKDW_IDLST SKDW_PNAME1
           ELSE
               CALL "DB_F_Close" USING
                BY REFERENCE SKDF_IDLST SKDF_PNAME1
               IF  COMPLETION_CODE NOT = 255
                   IF  W-MKIN NOT = W-KIN
                       CALL "SD_Output" USING
                        "E-ME7" E-ME7 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-KIN" E-KIN "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME99" E-ME99 "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE STWF_IDLST STWF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE 0 TO W-CC.
           CALL "DB_F_Open" USING
            "INPUT" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
      *
           MOVE W-TCD TO TSK-KEY.
      *           READ TSKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-CC
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO S-15
           END-IF
           IF  TSK-ZNGP(5) NOT = ZERO
               MOVE TSK-ZNGP(5) TO W-SNGP
               MOVE TSK-HTS(5) TO W-MKIN
               SUBTRACT TSK-HTS(4) FROM W-MKIN
           ELSE
               IF  TSK-ZNGP(4) NOT = ZERO
                   MOVE TSK-ZNGP(4) TO W-SNGP
                   MOVE TSK-HTS(4) TO W-MKIN
                   SUBTRACT TSK-HTS(3) FROM W-MKIN
               ELSE
                   IF  TSK-ZNGP(3) NOT = ZERO
                       MOVE 1 TO W-C
                       MOVE TSK-ZNGP(3) TO W-SNGP
                       MOVE TSK-HTS(3) TO W-MKIN
                       SUBTRACT TSK-HTS(2) FROM W-MKIN
                   END-IF
               END-IF
           END-IF
           IF  W-SNGP = ZERO
               MOVE 1 TO W-CC
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO S-15
           END-IF
           CALL "SD_Output" USING "D-SNGP" D-SNGP "p" RETURNING RESU.
       S-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 1 TO W-CC
               GO TO S-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-10
           END-IF
           IF  W-DMM = 9
               MOVE 1 TO W-CC
               GO TO S-15
           END-IF
           IF  W-DMM NOT = 1
               GO TO S-10
           END-IF.
       S-15.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
       S-20.
           EXIT.
       S-25.
           MOVE 0 TO CHK.
           CALL "DB_F_Open" USING
            "INPUT" STRAN3_PNAME1 "SHARED" BY REFERENCE
            STRAN3_IDLST "0".
      *           SELECT STRAN3 WHERE ST3-DNO = W-DNO.
      *///////////////
           CALL "DB_Select" USING
            STRAN3_PNAME1 "WHERE" 
            "ST3-DNO" "=" W-DNO RETURNING RET.
       S-27.
      *           READ STRAN3 NEXT RECORD WITH UNLOCK AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" STRAN3_PNAME1 BY REFERENCE STRAN3-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "DB_Scratch" USING STRAN3_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE STRAN3_IDLST STRAN3_PNAME1
               GO TO S-40
           END-IF
           IF  ST3-TCD NOT = W-TCD
               GO TO S-27
           END-IF
           MOVE 1 TO CHK.
       S-30.
           IF  W-TCD NOT = ST3-TCD
               CALL "DB_Scratch" USING STRAN3_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE STRAN3_IDLST STRAN3_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DNO" E-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-60
           END-IF
           IF  ST3-GNO < 7
               MOVE W-SNGP TO ST3-SNGP
           END-IF
           MOVE ZERO TO STW-R.
           MOVE STRAN3-R TO STW-R.
      *           WRITE STW-R.
      *//////////////
           CALL "DB_Insert" USING
            STWF_PNAME1 STWF_LNAME STW-R RETURNING RET.
           IF  ST3-GNO < 7
               IF  ST3-SNC = 1 OR 3
                   COMPUTE W-KIN = (ST3-KIN * -1) + W-KIN
               ELSE
                   IF  ST3-DC = 1 OR 2 OR 5
                       COMPUTE W-KIN = (ST3-KIN * -1) + W-KIN
                   ELSE
                       IF  ST3-DC NOT = 8
                           ADD ST3-KIN TO W-KIN
                       END-IF
                   END-IF
               END-IF
           END-IF.
       S-32.
      *           READ STRAN3 NEXT RECORD WITH UNLOCK AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" STRAN3_PNAME1 BY REFERENCE STRAN3-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO S-35
           END-IF
           IF  W-DNO NOT = ST3-DNO
               GO TO S-35
           END-IF
           IF  ST3-TCD NOT = W-TCD
               GO TO S-32
           END-IF
           GO TO S-30.
       S-35.
           CALL "DB_Scratch" USING STRAN3_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE STRAN3_IDLST STRAN3_PNAME1.
           GO TO S-60.
       S-40.
           CALL "DB_F_Open" USING
            "INPUT" SNTR-F_PNAME1 "SHARED" BY REFERENCE 
            SNTR-F_IDLST "0".
           IF W-C = 1
               GO TO S-55
           END-IF.
       S-45.
      *           READ SNTR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-55
           END-IF
           IF  W-DNO NOT = SNTR-DNO
               GO TO S-45
           END-IF
           IF  W-TCD NOT = SNTR-TCD
               GO TO S-45
           END-IF
           MOVE 1 TO CHK.
       S-50.
           IF  W-TCD NOT = SNTR-TCD
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DNO" E-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-60
           END-IF
           IF  SNTR-GNO < 7
               MOVE W-SNGP TO SNTR-SNGP
           END-IF
           MOVE ZERO TO STW-R.
           MOVE SNTR-R TO STW-R.
      *           WRITE STW-R.
      *//////////////
           CALL "DB_Insert" USING
            STWF_PNAME1 STWF_LNAME STW-R RETURNING RET.
           IF  SNTR-GNO < 7
               IF  SNTR-SNC = 1 OR 3
                   COMPUTE W-KIN = (SNTR-KIN * -1) + W-KIN
               ELSE
                   IF  SNTR-DC = 1 OR 2 OR 5
                       COMPUTE W-KIN = (SNTR-KIN * -1) + W-KIN
                   ELSE
                       IF  SNTR-DC NOT = 8
                           ADD SNTR-KIN TO W-KIN
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *           READ SNTR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-55
           END-IF
           IF  W-DNO = SNTR-DNO
               GO TO S-50
           END-IF.
       S-55.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1.
           IF  CHK = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DNO" E-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       S-60.
           EXIT.

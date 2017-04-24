       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBT610.
      **************************************************************
      *    PROGRAM         :  履物製品仕入　納期別発注入庫残問合せ *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  SCBT61                               *
      *    COMPILE TYPE    :  COBOL                                *
      **************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0768ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0768".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-ND.
             03  W-NNEN       PIC  9(004).
             03  W-NNENL REDEFINES W-NNEN.
               04  W-NNEN1    PIC  9(002).
               04  W-NNEN2    PIC  9(002).
             03  W-NGET       PIC  9(002).
             03  W-NPEY       PIC  9(002).
           02  W-NDL   REDEFINES W-ND.
             03  F            PIC  9(002).
             03  W-NDS        PIC  9(006).
           02  W-HCD          PIC  9(006).
           02  W-NO           PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-L1           PIC  9(002).
           02  W-L2           PIC  9(002).
           02  W-C            PIC  9(002).
           02  W-G            PIC  9(001).
           02  W-S            PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-ASUD.
             03  W-GYO   OCCURS   6.
               04  W-ASU   OCCURS   4.
                 05  W-SUD   OCCURS  10.
                   06  W-SU   PIC S9(005).
               04  W-SUT      PIC S9(006).
               04  W-HD       PIC  9(006).
           02  W-RNO          PIC  9(008).
           02  W-AGSD.
             03  W-AGSU  OCCURS   4.
               04  W-GSUD  OCCURS  10.
                 05  W-GSU    PIC S9(005).
           02  W-TSU          PIC S9(006).
       01  W-EM               PIC  X(030) VALUE SPACE.
       01  W-FILE             PIC  X(013) VALUE SPACE.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LISM.
           COPY LIHIM.
      *FD  HSHWF
       01  HSHWF_KBT610.
           02  HSHWF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HSHWF_LNAME    PIC  X(012) VALUE "HSHWF_KBT610".
           02  F              PIC  X(001).
           02  HSHWF_KEY1     PIC  X(100) VALUE SPACE.
           02  HSHWF_SORT     PIC  X(100) VALUE SPACE.
           02  HSHWF_IDLST    PIC  X(100) VALUE SPACE.
           02  HSHWF_RES      USAGE  POINTER.
       01  HSHW-R.
           02  HSHW-KEY3.
             03  HSHW-SCD     PIC  9(004).
             03  HSHW-KEY2.
               04  HSHW-HCD   PIC  9(006).
               04  HSHW-RNO.
                 05  HSHW-RSN PIC  9(002).
                 05  HSHW-RNG PIC  9(004).
                 05  HSHW-RND PIC  9(002).
           02  HSHW-HDD       PIC  9(008).
           02  HSHW-HDDL  REDEFINES HSHW-HDD.
             03  F            PIC  9(002).
             03  HSHW-HNGPS   PIC  9(006).
           02  HSHW-AHSUD.
             03  HSHW-HSUD  OCCURS   4.                                  ｻｲｽﾞ
               04  HSHW-AHSU  OCCURS  10.
                 05  HSHW-HSU PIC S9(004).                               数量
           02  HSHW-ANSUD.
             03  HSHW-NSUD  OCCURS   4.                                  ｻｲｽﾞ
               04  HSHW-ANSU  OCCURS  10.
                 05  HSHW-NSU PIC S9(004).                               数量
           02  HSHW-AISUD.
             03  HSHW-ISUD  OCCURS   4.                                  ｻｲｽﾞ
               04  HSHW-AISU  OCCURS  10.
                 05  HSHW-ISU PIC S9(004).                               数量
           02  HSHW-T         PIC  9(005).
           02  HSHW-NDD       PIC  9(008).
           02  HSHW-NDDL  REDEFINES HSHW-NDD.
             03  F            PIC  9(002).
             03  HSHW-NNGPS   PIC  9(006).
             03  HSHW-NDS   REDEFINES HSHW-NNGPS.
               04  HSHW-NNEN  PIC  9(002).
               04  HSHW-NGET  PIC  9(002).
               04  HSHW-NPEY  PIC  9(002).
           02  HSHW-ENGP      PIC  9(006).
           02  F              PIC  X(243).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  FILLER.
             03  A-NNEN  PIC  9(002).
             03  A-NGET  PIC  9(002).
             03  A-NPEY  PIC  9(002).
           02  A-NO    PIC  9(001).
           02  A-NDS   PIC  9(006).
           02  A-HCD   PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MEI.
             03  FILLER.
               04  FILLER  PIC  9(001).
               04  FILLER  PIC  9(002).
               04  FILLER  PIC  X(001)   VALUE "/".
               04  FILLER  PIC  9(002).
               04  FILLER  PIC  X(001)   VALUE "/".
               04  FILLER  PIC  9(002).
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  N(024).
               04  FILLER  PIC  9(002).
               04  FILLER  PIC  X(001)   VALUE "-".
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  X(001)   VALUE "-".
               04  FILLER  PIC  9(002).
             03  FILLER.
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  N(024).
               04  FILLER  PIC ----,--9 .
           02  D-SSD.
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(006).
             03  FILLER  PIC ------9 .
             03  FILLER  PIC  9(006).
           02  D-GSM.
             03  FILLER  PIC  N(002) VALUE "納期".
             03  FILLER  PIC  X(006) VALUE "品ｺｰﾄﾞ".
             03  FILLER  PIC  X(006) VALUE "      ".
           02  D-GSC.
             03  FILLER  PIC  X(004) VALUE "    ".
             03  FILLER  PIC  X(006) VALUE "      ".
             03  FILLER  PIC  X(006) VALUE "      ".
             03  FILLER  PIC  X(006) VALUE "      ".
           02  D-GSD.
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(006).
             03  FILLER  PIC ------9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(030).
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
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "20" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "3" "0" "6" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NNEN" "9" "3" "3" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NNEN" BY REFERENCE W-NNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NGET" "9" "3" "6" "2" "A-NNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NGET" BY REFERENCE W-NGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NPEY" "9" "3" "9" "2" "A-NGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NPEY" BY REFERENCE W-NPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NO" "9" "18" "1" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NO" BY REFERENCE W-NO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NDS" "9" "20" "1" "6" "A-NO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NDS" BY REFERENCE W-NDS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "22" "1" "6" "A-NDS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "76" "1" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "239" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEI" " " "0" "0" "133" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MEI" " " "W-L1" "0" "71" " " "D-MEI" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-MEI" "9" "W-L1" "1" "1" " " "01D-MEI" RETURNING RESU.
       CALL "SD_From" USING 
            "0101D-MEI" BY REFERENCE W-G "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-MEI" "9" "W-L1" "3" "2" "0101D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-MEI" BY REFERENCE HSHW-NNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0301D-MEI" "X" "W-L1" "5" "1" "0201D-MEI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0401D-MEI" "9" "W-L1" "6" "2" "0301D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0401D-MEI" BY REFERENCE HSHW-NGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0501D-MEI" "X" "W-L1" "8" "1" "0401D-MEI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0601D-MEI" "9" "W-L1" "9" "2" "0501D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0601D-MEI" BY REFERENCE HSHW-NPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0701D-MEI" "9" "W-L1" "12" "4" "0601D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0701D-MEI" BY REFERENCE HSHW-SCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0801D-MEI" "N" "W-L1" "17" "48" "0701D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0801D-MEI" BY REFERENCE S-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0901D-MEI" "9" "W-L1" "66" "2" "0801D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0901D-MEI" BY REFERENCE HSHW-RSN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "1001D-MEI" "X" "W-L1" "68" "1" "0901D-MEI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "1101D-MEI" "9" "W-L1" "69" "4" "1001D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1101D-MEI" BY REFERENCE HSHW-RNG "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "1201D-MEI" "X" "W-L1" "73" "1" "1101D-MEI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "1301D-MEI" "9" "W-L1" "74" "2" "1201D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1301D-MEI" BY REFERENCE HSHW-RND "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MEI" " " "W-L2" "0" "62" "01D-MEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-MEI" "9" "W-L2" "17" "6" " " "02D-MEI"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0102D-MEI" BY REFERENCE HSHW-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-MEI" "N" "W-L2" "24" "48" "0102D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0202D-MEI" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-MEI" "----,--9" "W-L2" "73" "8" "0202D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0302D-MEI" BY REFERENCE W-SUT(1) "6" "1" 
            BY REFERENCE W-G 212 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SSD" " " "0" "0" "37" "D-MEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SSD" "------" "19" "W-C" "6" " " "D-SSD"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-SSD" BY REFERENCE W-SU(1,1,1) "5" "3" BY REFERENCE W-G
            212 "1" 50 BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SSD" "------" "20" "W-C" "6" "01D-SSD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SSD" BY REFERENCE W-SU(1,1,1) "5" "3" BY REFERENCE W-G
            212 "2" 50 BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SSD" "------" "21" "W-C" "6" "02D-SSD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-SSD" BY REFERENCE W-SU(1,1,1) "5" "3" BY REFERENCE W-G
            212 "3" 50 BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-SSD" "------" "22" "W-C" "6" "03D-SSD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-SSD" BY REFERENCE W-SU(1,1,1) "5" "3" BY REFERENCE W-G
            212 "4" 50 BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-SSD" "------9" "22" "70" "7" "04D-SSD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-SSD" BY REFERENCE W-SUT(1) "6" "1" BY REFERENCE W-G 212
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-SSD" "9" "20" "71" "6" "05D-SSD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "06D-SSD" BY REFERENCE W-HD(1) "6" "1" BY REFERENCE W-G 212
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GSM" " " "0" "0" "16" "D-SSD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-GSM" "N" "19" "2" "4" " " "D-GSM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-GSM" "X" "21" "1" "6" "01D-GSM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-GSM" "X" "20" "71" "6" "02D-GSM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GSC" " " "0" "0" "22" "D-GSM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-GSC" "X" "19" "2" "4" " " "D-GSC" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-GSC" "X" "20" "1" "6" "01D-GSC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-GSC" "X" "21" "1" "6" "02D-GSC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-GSC" "X" "22" "1" "6" "03D-GSC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GSD" " " "0" "0" "31" "D-GSC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-GSD" "------" "19" "W-C" "6" " " "D-GSD"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-GSD" BY REFERENCE W-GSU(1,1) "5" "2" "1" 50 
            BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-GSD" "------" "20" "W-C" "6" "01D-GSD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-GSD" BY REFERENCE W-GSU(1,1) "5" "2" "2" 50 
            BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-GSD" "------" "21" "W-C" "6" "02D-GSD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-GSD" BY REFERENCE W-GSU(1,1) "5" "2" "3" 50 
            BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-GSD" "------" "22" "W-C" "6" "03D-GSD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-GSD" BY REFERENCE W-GSU(1,1) "5" "2" "4" 50 
            BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-GSD" "------9" "22" "70" "7" "04D-GSD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-GSD" BY REFERENCE W-TSU "6" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "30" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "30" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME" "X" "24" "15" "30" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-ME" BY REFERENCE W-EM "30" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           CALL "SD_Screen_Output" USING "SCBT61" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0768ID.
           MOVE WK0768ID TO HSHWF_PNAME1.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-NNEN "A-NNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO M-10
           END-IF
           MOVE ZERO TO W-NNEN1.
           IF  W-NNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NNEN
           END-IF
           IF  W-NNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NNEN
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-NGET "A-NGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-NGET = ZERO
               IF  W-NNEN2 = ZERO
                   MOVE ZERO TO W-NNEN
               END-IF
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-NPEY "A-NPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HSHWF_PNAME1 " " BY REFERENCE HSHWF_IDLST "0".
       M-25.
      *           READ HSHWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HSHWF_PNAME1 BY REFERENCE HSHW-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HSHWF_IDLST HSHWF_PNAME1
               MOVE "***  データ　なし  ***        " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-10
           END-IF
           IF  HSHW-ENGP NOT = ZERO
               GO TO M-25
           END-IF
           IF  HSHW-NDD < W-ND
               GO TO M-25
           END-IF.
       M-30.
           MOVE ZERO TO W-ASUD W-G.
           MOVE 1 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 2 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
       M-35.
           ADD 1 TO W-G.
           IF  W-G = 7
               MOVE HSHW-RNO TO W-RNO
               GO TO M-45
           END-IF
           PERFORM SET-RTN THRU SET-EX.
           ADD 2 TO W-L1 W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           MOVE HSHW-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE "　＊＊＊　仕入先　なし　＊＊＊" TO S-NAME
           END-IF
           MOVE HSHW-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "　＊＊＊　品名　なし　＊＊＊　" TO HI-NAME
           END-IF
           CALL "SD_Output" USING "D-MEI" D-MEI "p" RETURNING RESU.
       M-40.
      *           READ HSHWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HSHWF_PNAME1 BY REFERENCE HSHW-R " " RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO W-RNO
               MOVE "ＥＮＤ　ＤＡＴＡ              " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-45
           END-IF
           IF  HSHW-ENGP NOT = ZERO
               GO TO M-40
           END-IF
           GO TO M-35.
       M-45.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE HSHWF_IDLST HSHWF_PNAME1
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               GO TO M-55
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-45
           END-IF
           IF  W-DMM = 9
               CALL "SD_Screen_Output" USING "SCBT61" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HSHWF_IDLST HSHWF_PNAME1
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-45
           END-IF
           CALL "SD_Screen_Output" USING "SCBT61" RETURNING RESU.
           IF  W-RNO = ZERO
               CALL "DB_F_Close" USING
                BY REFERENCE HSHWF_IDLST HSHWF_PNAME1
               GO TO M-10
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE HSHWF_IDLST HSHWF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HSHWF_PNAME1 " " BY REFERENCE HSHWF_IDLST "0".
       M-50.
      *           READ HSHWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HSHWF_PNAME1 BY REFERENCE HSHW-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HSHWF_IDLST HSHWF_PNAME1
               MOVE ZERO TO W-RNO
               MOVE "ＤＡＴＡ　エラー              " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-10
           END-IF
           IF  W-RNO NOT = HSHW-RNO
               GO TO M-50
           END-IF
           GO TO M-30.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-NO "A-NO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE HSHWF_IDLST HSHWF_PNAME1
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               GO TO M-45
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-55
           END-IF
           IF  W-NO = 9
               CALL "SD_Output" USING "D-GSM" D-GSM "p" RETURNING RESU
               GO TO M-65
           END-IF
           IF  W-NO < 1 OR > 6
               GO TO M-55
           END-IF
      *
           CALL "SD_Output" USING "D-GSC" D-GSC "p" RETURNING RESU.
           MOVE W-NO TO W-G.
           MOVE 4 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           MOVE ZERO TO CNT.
       M-60.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               ADD 6 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
               CALL "SD_Output" USING "D-SSD" D-SSD "p" RETURNING RESU
               GO TO M-60
           END-IF
           GO TO M-55.
       M-65.
           CALL "SD_Accept" USING BY REFERENCE A-NDS "A-NDS" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE HSHWF_IDLST HSHWF_PNAME1
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               GO TO M-55
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-65
           END-IF.
       M-70.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE HSHWF_IDLST HSHWF_PNAME1
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               GO TO M-65
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-70
           END-IF
           IF  W-HCD = ZERO
               GO TO M-75
           END-IF
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　品名　なし　＊＊      " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-70
           END-IF.
       M-75.
           PERFORM KEI-RTN THRU KEI-EX.
           GO TO M-55.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       SET-RTN.
           MOVE HSHW-HNGPS TO W-HD(W-G).
           MOVE ZERO TO W-S.
       SET-020.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO SET-EX
           END-IF
           MOVE ZERO TO CNT.
       SET-040.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO SET-020
           END-IF
           COMPUTE W-SU(W-G,W-S,CNT) = HSHW-HSU(W-S,CNT) -
                                 HSHW-NSU(W-S,CNT) - HSHW-ISU(W-S,CNT).
           ADD W-SU(W-G,W-S,CNT) TO W-SUT(W-G).
           MOVE HSHW-HNGPS TO W-HD(W-G).
           GO TO SET-040.
       SET-EX.
           EXIT.
       KEI-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE HSHWF_IDLST HSHWF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HSHWF_PNAME1 " " BY REFERENCE HSHWF_IDLST "0".
           MOVE ZERO TO W-AGSD.
       KEI-020.
      *           READ HSHWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HSHWF_PNAME1 BY REFERENCE HSHW-R " " RETURNING RET.
           IF  RET = 1
               GO TO KEI-080
           END-IF
           IF  W-NDS NOT = ZERO
               IF  HSHW-NNGPS < W-NDS
                   GO TO KEI-020
               END-IF
           END-IF
           IF  W-NDS NOT = ZERO
               IF  HSHW-NNGPS > W-NDS
                   GO TO KEI-080
               END-IF
           END-IF
           IF  W-HCD NOT = ZERO
               IF  HSHW-HCD NOT = W-HCD
                   GO TO KEI-020
               END-IF
           END-IF
           IF  HSHW-ENGP NOT = ZERO
               GO TO KEI-020
           END-IF
      *
           MOVE ZERO TO W-S.
       KEI-040.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO KEI-020
           END-IF
           MOVE ZERO TO CNT.
       KEI-060.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               COMPUTE W-GSU(W-S,CNT) = W-GSU(W-S,CNT)
                             + HSHW-HSU(W-S,CNT) - HSHW-NSU(W-S,CNT)
                                                 - HSHW-ISU(W-S,CNT)
               GO TO KEI-060
           END-IF
           GO TO KEI-040.
       KEI-080.
           MOVE ZERO TO W-S W-TSU.
       KEI-100.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO KEI-140
           END-IF
           MOVE ZERO TO CNT.
       KEI-120.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               ADD W-GSU(W-S,CNT) TO W-TSU
               GO TO KEI-120
           END-IF
           GO TO KEI-100.
       KEI-140.
           MOVE 4 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           MOVE ZERO TO CNT.
       KEI-160.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               ADD 6 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
               CALL "SD_Output" USING "D-GSD" D-GSD "p" RETURNING RESU
               GO TO KEI-160
           END-IF.
       KEI-EX.
           EXIT.

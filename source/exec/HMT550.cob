       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMT550.
      *********************************************************
      *    PROGRAM         :  入庫伝票　問合せ              　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHT55                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-MSG              PIC  X(040).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-SGPD         PIC  9(004).
           02  W-EGPD         PIC  9(004).
           02  W-SNGP.
             03  W-SNEN       PIC  9(004).
             03  W-SGP.
               04  W-SGET     PIC  9(002).
               04  W-SPEY     PIC  9(002).
           02  W-SNGPD REDEFINES W-SNGP.
             03  W-SNG        PIC  9(006).
             03  F            PIC  9(002).
           02  W-ENGP.
             03  W-ENEN       PIC  9(004).
             03  W-EGP.
               04  W-EGET     PIC  9(002).
               04  W-EPEY     PIC  9(002).
           02  W-ENGPD REDEFINES W-ENGP.
             03  W-ENG        PIC  9(006).
             03  F            PIC  9(002).
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NEND  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGD   REDEFINES W-NG.
             03  F            PIC  X(002).
             03  W-NGS        PIC  9(004).
           02  W-ADNG.
             03  W-DNGD  OCCURS  9.
               04  W-DNG.
                 05  W-DNEN   PIC  9(004).
                 05  W-DGET   PIC  9(002).
           02  W-HCD          PIC  9(006).
           02  W-DC           PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-GN           PIC  9(002).
           02  DCNT           PIC  9(002).
           02  CNT            PIC  9(002).
           02  W-NAME         PIC  N(024).
           02  W-NM     REDEFINES W-NAME.
             03  W-NM1        PIC  N(016).
             03  W-NM2        PIC  N(008).
           02  W-NAD    REDEFINES W-NAME.
             03  W-NA         PIC  N(001)  OCCURS  24.
           02  W-NAMEW        PIC  N(024).
           02  W-NADW   REDEFINES W-NAMEW.
             03  W-NAW        PIC  N(001)  OCCURS  24.
           02  W-DSP.
             03  W-ASD.
               04  W-AS    OCCURS  10.
                 05  W-S      PIC S9(004).
             03  W-SUT        PIC S9(005).
             03  W-T          PIC S9(005).
             03  W-KIN        PIC S9(008).
           02  W-DMM          PIC  9(001).
           02  W-DCN          PIC  N(002).
           02  W-AGD.
             03  W-GD    OCCURS  14.
               04  W-SIZ      PIC  9(001).
               04  W-ASUD.
                 05  W-ASU   OCCURS  10.
                   06  W-SU   PIC S9(004).
               04  W-SKC      PIC  9(001).
               04  W-SKN      PIC  N(006).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY L-JCON.
      *FD  WNK-F
       01  WNK-F_HMT550.
           02  WNK-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  WNK-F_LNAME    PIC  X(012) VALUE "WNK-F_HMT550".
           02  F              PIC  X(001).
           02  WNK-F_KEY1     PIC  X(100) VALUE SPACE.
           02  WNK-F_SORT     PIC  X(100) VALUE SPACE.
           02  WNK-F_IDLST    PIC  X(100) VALUE SPACE.
           02  WNK-F_RES      USAGE  POINTER.
       01  WNK-R.
           02  WNK-DNO        PIC  9(006).
           02  WNK-GNO        PIC  9(001).
           02  WNK-NGP.
             03  WNK-NG.
               04  WNK-NEN    PIC  9(004).
               04  WNK-GET    PIC  9(002).
             03  WNK-PEY      PIC  9(002).
           02  WNK-HCD        PIC  9(006).
           02  WNK-SIZ        PIC  9(001).
           02  WNK-ASUD.
             03  WNK-ASU   OCCURS  10.
               04  WNK-SU     PIC S9(004).
           02  WNK-SUT        PIC S9(005).
           02  F              PIC  X(008).
           02  WNK-KIN        PIC S9(008).
           02  WNK-NRC        PIC  9(001).
           02  WNK-SSC        PIC  9(001).
           02  F              PIC  X(001).
           02  WNK-SKC        PIC  9(001).
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  FILLER.
             03  A-SGP   PIC  9(004).
             03  A-EGP   PIC  9(004).
             03  A-HCD   PIC  9(006).
           02  A-GN    PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-HNA   PIC  N(024).
             03  D-HNAC.
               04  FILLER  PIC  X(006) VALUE "      ".
               04  FILLER  PIC  X(048) VALUE
                    "                                                ".
           02  D-MD.
             03  D-MD1.
               04  01D-MD1 PIC Z9 .
               04  02D-MD1 PIC Z9 .
               04  03D-MD1 PIC  N(002).
             03  D-MD2.
               04  01D-MD2 PIC  9(006).
               04  02D-MD2 PIC  N(016).
             03  D-MD3.
               04  01D-MD3 PIC  -(006).
               04  02D-MD3 PIC  -(006).
               04  03D-MD3 PIC  -(009).
               04  04D-MD3 PIC  9(006).
           02  D-SD.
             03  D-SD1.
               04  01D-SD1 PIC  Z(001).
               04  02D-SD1 PIC  -(005).
               04  03D-SD1 PIC  -(005).
               04  04D-SD1 PIC  -(005).
               04  05D-SD1 PIC  -(005).
               04  06D-SD1 PIC  -(005).
               04  07D-SD1 PIC  -(005).
               04  08D-SD1 PIC  -(005).
               04  09D-SD1 PIC  -(005).
               04  10D-SD1 PIC  -(005).
               04  11D-SD1 PIC  -(005).
             03  FILLER.
               04  02D-SD  PIC  9(001).
               04  03D-SD  PIC  N(006).
       01  C-ERR.
           02  FILLER  .
             03  E-ME    PIC  X(040).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "17" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "2" "0" "14" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-SGP" "9" "2" "6" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SGP" BY REFERENCE W-SGPD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EGP" "9" "2" "12" "4" "A-SGP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EGP" BY REFERENCE W-EGPD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-HCD" "9" "2" "22" "6" "A-EGP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-GN" "9" "22" "1" "2" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-GN" BY REFERENCE W-GN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "78" "1" "A-GN" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "239" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "2" "0" "102" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNA" "N" "2" "29" "48" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNAC" " " "2" "0" "54" "D-HNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-HNAC" "X" "2" "22" "6" " " "D-HNAC" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-HNAC" "X" "2" "29" "48" "01D-HNAC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MD" " " "W-L" "0" "73" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MD1" " " "W-L" "0" "8" " " "D-MD" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MD1" "Z9" "W-L" "4" "2" " " "D-MD1" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MD1" BY REFERENCE WNK-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MD1" "Z9" "W-L" "6" "2" "01D-MD1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MD1" BY REFERENCE WNK-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MD1" "N" "W-L" "9" "4" "02D-MD1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-MD1" BY REFERENCE W-DCN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MD2" " " "W-L" "0" "38" "D-MD1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MD2" "9" "W-L" "14" "6" " " "D-MD2" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MD2" BY REFERENCE WNK-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MD2" "N" "W-L" "21" "32" "01D-MD2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MD2" BY REFERENCE W-NM1 "32" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MD3" " " "W-L" "0" "27" "D-MD2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MD3" "------" "W-L" "53" "6" " " "D-MD3"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MD3" BY REFERENCE W-SUT "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MD3" "------" "W-L" "59" "6" "01D-MD3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MD3" BY REFERENCE W-T "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MD3" "---------" "W-L" "65" "9" "02D-MD3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-MD3" BY REFERENCE W-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-MD3" "9" "W-L" "75" "6" "03D-MD3" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-MD3" BY REFERENCE WNK-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SD" " " "0" "0" "64" "D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SD1" " " "22" "0" "51" " " "D-SD" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SD1" "Z" "22" "4" "1" " " "D-SD1" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-SD1" BY REFERENCE W-SIZ(1) "1" "1" BY REFERENCE W-GN 54
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SD1" "-----" "22" "5" "5" "01D-SD1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SD1" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 54 "01" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SD1" "-----" "22" "10" "5" "02D-SD1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-SD1" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 54 "02" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-SD1" "-----" "22" "15" "5" "03D-SD1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-SD1" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 54 "03" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-SD1" "-----" "22" "20" "5" "04D-SD1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-SD1" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 54 "04" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-SD1" "-----" "22" "25" "5" "05D-SD1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06D-SD1" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 54 "05" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-SD1" "-----" "22" "30" "5" "06D-SD1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07D-SD1" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 54 "06" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-SD1" "-----" "22" "35" "5" "07D-SD1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08D-SD1" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 54 "07" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "09D-SD1" "-----" "22" "40" "5" "08D-SD1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "09D-SD1" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 54 "08" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "10D-SD1" "-----" "22" "45" "5" "09D-SD1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "10D-SD1" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 54 "09" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "11D-SD1" "-----" "22" "50" "5" "10D-SD1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "11D-SD1" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE
            W-GN 54 "10" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SD" " " "21" "0" "13" "D-SD1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-SD" "9" "21" "61" "1" " " "02D-SD" RETURNING RESU.
       CALL "SD_From" USING 
            "0102D-SD" BY REFERENCE W-SKC(1) "1" "1" BY REFERENCE
            W-GN 54 RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-SD" "N" "21" "63" "12" "0102D-SD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0202D-SD" BY REFERENCE W-SKN(1) "12" "1" BY REFERENCE
            W-GN 54 RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "100" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "100" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME" "X" "24" "15" "40" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-ME" BY REFERENCE W-MSG "40" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME" " " RETURNING RESU.
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
           CALL "SD_Screen_Output" USING "SCHT55" RETURNING RESU.
           MOVE ZERO TO W-NG W-ADNG.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-DNG(2) W-DNG(3) W-DNG(4) W-DNG(5) W-DNG(6)
                        W-DNG(7) W-DNG(8) W-DNG(9).
           IF  W-DGET(9) = ZERO
               SUBTRACT 1 FROM W-DNEN(9)
               MOVE 12 TO W-DGET(9)
           END-IF
           MOVE W-DNG(9) TO W-DNG(8).
           SUBTRACT 1 FROM W-DGET(8).
           IF  W-DGET(8) = ZERO
               SUBTRACT 1 FROM W-DNEN(8)
               MOVE 12 TO W-DGET(8)
           END-IF
           MOVE W-DNG(8) TO W-DNG(7).
           SUBTRACT 1 FROM W-DGET(7).
           IF  W-DGET(7) = ZERO
               SUBTRACT 1 FROM W-DNEN(7)
               MOVE 12 TO W-DGET(7)
           END-IF
           MOVE W-DNG(7) TO W-DNG(6).
           SUBTRACT 1 FROM W-DGET(6).
           IF  W-DGET(6) = ZERO
               SUBTRACT 1 FROM W-DNEN(6)
               MOVE 12 TO W-DGET(6)
           END-IF
           MOVE W-DNG(6) TO W-DNG(5).
           SUBTRACT 1 FROM W-DGET(5).
           IF  W-DGET(5) = ZERO
               SUBTRACT 1 FROM W-DNEN(5)
               MOVE 12 TO W-DGET(5)
           END-IF
           MOVE W-DNG(5) TO W-DNG(4).
           SUBTRACT 1 FROM W-DGET(4).
           IF  W-DGET(4) = ZERO
               SUBTRACT 1 FROM W-DNEN(4)
               MOVE 12 TO W-DGET(4)
           END-IF
           MOVE W-DNG(4) TO W-DNG(3).
           SUBTRACT 1 FROM W-DGET(3).
           IF  W-DGET(3) = ZERO
               SUBTRACT 1 FROM W-DNEN(3)
               MOVE 12 TO W-DGET(3)
           END-IF
           MOVE W-DNG(3) TO W-DNG(2).
           SUBTRACT 1 FROM W-DGET(2).
           IF  W-DGET(2) = ZERO
               SUBTRACT 1 FROM W-DNEN(2)
               MOVE 12 TO W-DGET(2)
           END-IF
           MOVE W-DNG(2) TO W-DNG(1).
           SUBTRACT 1 FROM W-DGET(1).
           IF  W-DGET(1) = ZERO
               SUBTRACT 1 FROM W-DNEN(1)
               MOVE 12 TO W-DGET(1)
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO WNK-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON3-KEY" BY REFERENCE JCON3-KEY.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SGP "A-SGP" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           MOVE W-SGPD TO W-SGP.
           IF  W-SGP = ZERO
               MOVE W-DNG(1) TO W-SNG
               GO TO M-20
           END-IF
           IF  W-SGET < 1 OR > 12
               GO TO M-15
           END-IF
           IF  W-SPEY < 1 OR > 31
               GO TO M-15
           END-IF
           IF  W-SGET NOT = W-DGET(1) AND W-DGET(2) AND W-DGET(3)
                       AND W-DGET(4) AND W-DGET(5) AND W-DGET(6)
                       AND W-DGET(7) AND W-DGET(8) AND W-DGET(9)
               GO TO M-15
           END-IF
           IF  W-SGET = W-DGET(1)
               MOVE W-DNEN(1) TO W-SNEN
           END-IF
           IF  W-SGET = W-DGET(2)
               MOVE W-DNEN(2) TO W-SNEN
           END-IF
           IF  W-SGET = W-DGET(3)
               MOVE W-DNEN(3) TO W-SNEN
           END-IF
           IF  W-SGET = W-DGET(4)
               MOVE W-DNEN(4) TO W-SNEN
           END-IF
           IF  W-SGET = W-DGET(5)
               MOVE W-DNEN(5) TO W-SNEN
           END-IF
           IF  W-SGET = W-DGET(6)
               MOVE W-DNEN(6) TO W-SNEN
           END-IF
           IF  W-SGET = W-DGET(7)
               MOVE W-DNEN(7) TO W-SNEN
           END-IF
           IF  W-SGET = W-DGET(8)
               MOVE W-DNEN(8) TO W-SNEN
           END-IF
           IF  W-SGET = W-DGET(9)
               MOVE W-DNEN(9) TO W-SNEN
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-EGP "A-EGP" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           MOVE W-EGPD TO W-EGP.
           IF  W-EGP = 9999
               MOVE W-DNG(9) TO W-ENG
               GO TO M-25
           END-IF
           IF  W-EGET < 1 OR > 12
               GO TO M-20
           END-IF
           IF  W-EPEY < 1 OR > 31
               GO TO M-20
           END-IF
           IF  W-EGET NOT = W-DGET(1) AND W-DGET(2) AND W-DGET(3)
                       AND W-DGET(4) AND W-DGET(5) AND W-DGET(6)
                       AND W-DGET(7) AND W-DGET(8) AND W-DGET(9)
               GO TO M-20
           END-IF
           IF  W-EGET = W-DGET(1)
               MOVE W-DNEN(1) TO W-ENEN
           END-IF
           IF  W-EGET = W-DGET(2)
               MOVE W-DNEN(2) TO W-ENEN
           END-IF
           IF  W-EGET = W-DGET(3)
               MOVE W-DNEN(3) TO W-ENEN
           END-IF
           IF  W-EGET = W-DGET(4)
               MOVE W-DNEN(4) TO W-ENEN
           END-IF
           IF  W-EGET = W-DGET(5)
               MOVE W-DNEN(5) TO W-ENEN
           END-IF
           IF  W-EGET = W-DGET(6)
               MOVE W-DNEN(6) TO W-ENEN
           END-IF
           IF  W-EGET = W-DGET(7)
               MOVE W-DNEN(7) TO W-ENEN
           END-IF
           IF  W-EGET = W-DGET(8)
               MOVE W-DNEN(8) TO W-ENEN
           END-IF
           IF  W-EGET = W-DGET(9)
               MOVE W-DNEN(9) TO W-ENEN
           END-IF
           IF  W-SNGP > W-ENGP
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
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
      *
           IF  W-HCD = ZERO
               CALL "SD_Output" USING
                "D-HNAC" D-HNAC "p" RETURNING RESU
               GO TO M-30
           END-IF
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  ﾋﾝﾒｲ ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
       M-30.
           CALL "DB_F_Open" USING
            "INPUT" WNK-F_PNAME1 " " BY REFERENCE WNK-F_IDLST "0".
       M-35.
      *           READ WNK-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" WNK-F_PNAME1 BY REFERENCE WNK-R " " RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-DC
               GO TO M-50
           END-IF
           IF  WNK-NGP < W-SNGP
               GO TO M-35
           END-IF
           IF  W-HCD NOT = ZERO
               IF  WNK-HCD NOT = W-HCD
                   GO TO M-35
               END-IF
           END-IF
           IF  WNK-NG > W-ENG
               GO TO M-50
           END-IF
      *
           MOVE ZERO TO W-GN.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           PERFORM CLR-RTN THRU CLR-EX.
       M-40.
           ADD 1 TO W-L W-GN.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 18
               MOVE 0 TO W-DC
               GO TO M-50
           END-IF
           PERFORM MEI-RTN THRU MEI-EX.
       M-45.
      *           READ WNK-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" WNK-F_PNAME1 BY REFERENCE WNK-R " " RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-DC
               GO TO M-50
           END-IF
           IF  W-HCD NOT = ZERO
               IF  WNK-HCD NOT = W-HCD
                   GO TO M-45
               END-IF
           END-IF
           IF  WNK-NG > W-ENG
               MOVE 9 TO W-DC
               GO TO M-50
           END-IF
           GO TO M-40.
       M-50.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE WNK-F_IDLST WNK-F_PNAME1
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-55
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-50
           END-IF
           IF  W-DMM = 9
               GO TO M-60
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-50
           END-IF
           IF  W-DC = 9
               GO TO M-60
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHT55" RETURNING RESU.
           CALL "SD_Output" USING "A-SGP" A-SGP "p" RETURNING RESU.
           CALL "SD_Output" USING "A-EGP" A-EGP "p" RETURNING RESU.
           IF  W-HCD = ZERO
               CALL "SD_Output" USING
                "D-HNAC" D-HNAC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU
               CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU
           END-IF
           MOVE ZERO TO W-GN.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           PERFORM CLR-RTN THRU CLR-EX.
           GO TO M-40.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-GN "A-GN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE WNK-F_IDLST WNK-F_PNAME1
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-50
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-55
           END-IF
           IF  W-GN < 1 OR > 14
               GO TO M-55
           END-IF
           CALL "SD_Output" USING "D-SD" D-SD "p" RETURNING RESU.
           GO TO M-55.
       M-60.
           CALL "DB_F_Close" USING
            BY REFERENCE WNK-F_IDLST WNK-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHT55" RETURNING RESU.
           CALL "SD_Output" USING "A-SGP" A-SGP "p" RETURNING RESU.
           CALL "SD_Output" USING "A-EGP" A-EGP "p" RETURNING RESU.
           GO TO M-25.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       CLR-RTN.
           MOVE ZERO TO W-AGD CNT.
       CLR-010.
           ADD 1 TO CNT.
           IF  CNT NOT = 15
               MOVE SPACE TO W-SKN(CNT)
               GO TO CLR-010
           END-IF.
       CLR-EX.
           EXIT.
       MEI-RTN.
           MOVE SPACE TO W-DCN.
           MOVE ZERO TO W-DSP.
           IF  WNK-NRC = 5
               COMPUTE W-SUT = -1 * WNK-SUT
               COMPUTE W-KIN = -1 * WNK-KIN
               COMPUTE W-S(01) = -1 * WNK-SU(01)
               COMPUTE W-S(02) = -1 * WNK-SU(02)
               COMPUTE W-S(03) = -1 * WNK-SU(03)
               COMPUTE W-S(04) = -1 * WNK-SU(04)
               COMPUTE W-S(05) = -1 * WNK-SU(05)
               COMPUTE W-S(06) = -1 * WNK-SU(06)
               COMPUTE W-S(07) = -1 * WNK-SU(07)
               COMPUTE W-S(08) = -1 * WNK-SU(08)
               COMPUTE W-S(09) = -1 * WNK-SU(09)
               COMPUTE W-S(10) = -1 * WNK-SU(10)
           ELSE
               MOVE WNK-SUT TO W-SUT
               MOVE WNK-KIN TO W-KIN
               MOVE WNK-SU(01) TO W-S(01)
               MOVE WNK-SU(02) TO W-S(02)
               MOVE WNK-SU(03) TO W-S(03)
               MOVE WNK-SU(04) TO W-S(04)
               MOVE WNK-SU(05) TO W-S(05)
               MOVE WNK-SU(06) TO W-S(06)
               MOVE WNK-SU(07) TO W-S(07)
               MOVE WNK-SU(08) TO W-S(08)
               MOVE WNK-SU(09) TO W-S(09)
               MOVE WNK-SU(10) TO W-S(10)
           END-IF
           IF  W-KIN NOT = ZERO
               IF  W-SUT NOT = ZERO
                   COMPUTE W-T = W-KIN / W-SUT
               END-IF
           END-IF
           IF  WNK-SSC = 0
               MOVE "仕入" TO W-DCN
           END-IF
           IF  WNK-SSC = 1
               MOVE "加硫" TO W-DCN
           END-IF
           IF  WNK-SSC = 2
               MOVE "非加" TO W-DCN
           END-IF
           IF  WNK-SSC = 3
               MOVE "　他" TO W-DCN
           END-IF
           IF  WNK-SSC = 4
               MOVE "外作" TO W-DCN
           END-IF
           IF  WNK-SSC = 5
               MOVE "底付" TO W-DCN
           END-IF
           IF  WNK-SSC = 6
               MOVE "外注" TO W-DCN
           END-IF
           IF  WNK-NRC = 4
               MOVE "移動" TO W-DCN
           END-IF
           IF  WNK-NRC = 5
               MOVE "格外" TO W-DCN
           END-IF
           CALL "SD_Output" USING "D-MD1" D-MD1 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-MD3" D-MD3 "p" RETURNING RESU.
           MOVE 3 TO JCON3-01.
           MOVE WNK-SKC TO JCON3-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "倉庫なし　　" TO JCON3-03
           END-IF.
       MEI-010.
           IF  W-HCD NOT = ZERO
               GO TO MEI-020
           END-IF
           MOVE WNK-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "品名なし" TO HI-NAME
           END-IF
           MOVE HI-NAME TO W-NAME.
           IF  W-NM2 NOT = SPACE
               PERFORM HNA-RTN THRU HNA-EX
           END-IF
           CALL "SD_Output" USING "D-MD2" D-MD2 "p" RETURNING RESU.
       MEI-020.
           MOVE WNK-SIZ TO W-SIZ(W-GN).
           MOVE W-ASD TO W-ASUD(W-GN).
           MOVE WNK-SKC TO W-SKC(W-GN).
           MOVE JCON3-03 TO W-SKN(W-GN).
       MEI-EX.
           EXIT.
       HNA-RTN.
           MOVE SPACE TO W-NAMEW.
           MOVE W-NAME TO W-NAMEW.
           MOVE SPACE TO W-NAME.
           MOVE ZERO TO DCNT CNT.
       HNA-010.
           ADD 1 TO DCNT.
           IF  DCNT = 25
               GO TO HNA-EX
           END-IF
           IF  W-NAW(DCNT) NOT = SPACE
               ADD 1 TO CNT
               MOVE W-NAW(DCNT) TO W-NA(CNT)
           END-IF
           GO TO HNA-010.
       HNA-EX.
           EXIT.

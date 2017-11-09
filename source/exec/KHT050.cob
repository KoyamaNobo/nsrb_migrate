       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHT050.
      *********************************************************
      *    PROGRAM         :  工品売上・値引伝票　問合せ    　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCKT05 , SCKT06                 *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  品名別=0 , 得意先別=1           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-MSG              PIC  X(040).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-DATE         PIC  9(006).
           02  W-SDATE        PIC  9(006).
           02  W-EDATE        PIC  9(006).
           02  W-SNGP.
             03  W-SNEN       PIC  9(002).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-SNGPD REDEFINES W-SNGP.
             03  W-SNG        PIC  9(004).
             03  F            PIC  9(002).
           02  W-ENGP.
             03  W-ENEN       PIC  9(002).
             03  W-EGET       PIC  9(002).
             03  W-EPEY       PIC  9(002).
           02  W-ENGPD REDEFINES W-ENGP.
             03  W-ENG        PIC  9(004).
             03  F            PIC  9(002).
           02  W-NG.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-HCD          PIC  X(005).
           02  W-STCD         PIC  9(004).
           02  W-ETCD         PIC  9(004).
           02  W-TCD          PIC  9(004).
           02  W-SHCD         PIC  X(005).
           02  W-EHCD         PIC  X(005).
           02  W-L1           PIC  9(002).
           02  W-L2           PIC  9(002).
           02  CNT            PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-INV          PIC  9(001).
           02  W-DCN          PIC  N(002).
           02  W-HNA          PIC  N(024).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIKHM.
      *FD  URI-F
       01  URI-F_KHT050.
           02  URI-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  URI-F_LNAME    PIC  X(012) VALUE "URI-F_KHT050".
           02  F              PIC  X(001).
           02  URI-F_KEY1     PIC  X(100) VALUE SPACE.
           02  URI-F_SORT     PIC  X(100) VALUE SPACE.
           02  URI-F_IDLST    PIC  X(100) VALUE SPACE.
           02  URI-F_RES      USAGE  POINTER.
       01  URI-R.
           02  URI-DC         PIC  9(001).
           02  URI-NGP        PIC  9(008).
           02  URI-NGPD  REDEFINES URI-NGP.
             03  F            PIC  9(002).
             03  URI-NGPS     PIC  9(006).
           02  URI-TCD        PIC  9(004).
           02  URI-HCD        PIC  X(005).
           02  URI-SU         PIC S9(006)V9(02).
           02  URI-T          PIC S9(006)V9(02).
           02  URI-KIN        PIC S9(008).
           02  F              PIC  X(006).
           02  URI-NNO        PIC  X(006).
           02  F              PIC  X(012).
           02  URI-DNO        PIC  9(006).
           02  URI-GNO        PIC  9(001).
           02  URI-JCD        PIC  9(006).
           02  F              PIC  X(008).
           02  URI-TEK        PIC  N(008).
           02  F              PIC  X(025).
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
             03  A-SNGP  PIC  9(006).
             03  A-ENGP  PIC  9(006).
           02  FILLER.
             03  A-HCD   PIC  X(005).
             03  A-STCD  PIC  9(004).
             03  A-ETCD  PIC  9(004).
             03  A-TCD   PIC  9(004).
             03  A-SHCD  PIC  X(005).
             03  A-EHCD  PIC  X(005).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-HNA   PIC  N(024).
             03  D-TNA   PIC  N(025).
           02  D-MD1.
             03  FILLER.
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  N(002).
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  N(026).
             03  FILLER.
               04  FILLER  PIC  N(008).
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  X(001) VALUE  "-".
               04  FILLER  PIC  9(001).
           02  D-MD2.
             03  FILLER.
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  N(002).
               04  FILLER  PIC  N(024).
             03  FILLER.
               04  FILLER  PIC  N(008).
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  X(001) VALUE  "-".
               04  FILLER  PIC  9(001).
           02  FILLER.
             03  D-HCD   PIC  X(005).
             03  D-JCD   PIC  9(006).
           02  FILLER.
             03  D-NNO   PIC  X(006).
             03  D-SU    PIC ----,--9.99 .
             03  D-T     PIC ----,--9.99 .
             03  D-KIN   PIC ---,---,--9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(040).
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "40" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ACP" " " "1" "0" "12" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Init" USING
            "A-SNGP" "9" "1" "67" "6" " " "01C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING
            "A-SNGP" BY REFERENCE W-SNGP "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ENGP" "9" "1" "75" "6" "A-SNGP" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-ENGP" BY REFERENCE W-ENGP "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-ACP" " " "2" "0" "27" "01C-ACP" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "A-HCD" "X" "2" "7" "5" " " "02C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING
            "A-HCD" BY REFERENCE W-HCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-STCD" "9" "2" "71" "4" "A-HCD" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-ETCD" "9" "2" "77" "4" "A-STCD" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-TCD" "9" "2" "8" "4" "A-ETCD" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SHCD" "X" "2" "69" "5" "A-TCD" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-SHCD" BY REFERENCE W-SHCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-EHCD" "X" "2" "76" "5" "A-SHCD" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-EHCD" BY REFERENCE W-EHCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "23" "70" "1" "02C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "320" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-DSP" " " "2" "0" "98" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING
            "D-HNA" "N" "2" "13" "48" " " "01C-DSP"  RETURNING RESU.
       CALL "SD_From" USING
            "D-HNA" BY REFERENCE W-HNA "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-TNA" "N" "2" "13" "50" "D-HNA" " "  RETURNING RESU.
       CALL "SD_From" USING
            "D-TNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-MD1" " " "0" "0" "90" "01C-DSP" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01D-MD1" " " "W-L1" "0" "66" " " "D-MD1"  RETURNING RESU.
       CALL "SD_Init" USING
            "0101D-MD1" "9" "W-L1" "1" "6" " " "01D-MD1" RETURNING RESU.
       CALL "SD_From" USING
            "0101D-MD1" BY REFERENCE URI-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0201D-MD1" "N" "W-L1" "8" "4" "0101D-MD1" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0201D-MD1" BY REFERENCE W-DCN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0301D-MD1" "9" "W-L1" "13" "4" "0201D-MD1" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0301D-MD1" BY REFERENCE URI-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0401D-MD1" "N" "W-L1" "18" "52" "0301D-MD1" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0401D-MD1" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-MD1" " " "W-L2" "0" "24" "01D-MD1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "0102D-MD1" "N" "W-L2" "11" "16" " " "02D-MD1"
             RETURNING RESU.
       CALL "SD_From" USING
            "0102D-MD1" BY REFERENCE URI-TEK "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0202D-MD1" "9" "W-L2" "36" "6" "0102D-MD1" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0202D-MD1" BY REFERENCE URI-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0302D-MD1" "X" "W-L2" "42" "1" "0202D-MD1" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0402D-MD1" "9" "W-L2" "43" "1" "0302D-MD1" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0402D-MD1" BY REFERENCE URI-GNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-MD2" " " "0" "0" "82" "D-MD1" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01D-MD2" " " "W-L1" "0" "58" " " "D-MD2"  RETURNING RESU.
       CALL "SD_Init" USING
            "0101D-MD2" "9" "W-L1" "1" "6" " " "01D-MD2"
            RETURNING RESU.
       CALL "SD_From" USING
            "0101D-MD2" BY REFERENCE URI-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0201D-MD2" "N" "W-L1" "8" "4" "0101D-MD2" " "
             RETURNING RESU.
       CALL "SD_From" USING
            "0201D-MD2" BY REFERENCE W-DCN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0301D-MD2" "N" "W-L1" "19" "48" "0201D-MD2" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0301D-MD2" BY REFERENCE W-HNA "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-MD2" " " "W-L2" "0" "24" "01D-MD2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "0102D-MD2" "N" "W-L2" "11" "16" " " "02D-MD2"
            RETURNING RESU.
       CALL "SD_From" USING
            "0102D-MD2" BY REFERENCE URI-TEK "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0202D-MD2" "9" "W-L2" "36" "6" "0102D-MD2" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0202D-MD2" BY REFERENCE URI-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0302D-MD2" "X" "W-L2" "42" "1" "0202D-MD2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0402D-MD2" "9" "W-L2" "43" "1" "0302D-MD2" " "
             RETURNING RESU.
       CALL "SD_From" USING
            "0402D-MD2" BY REFERENCE URI-GNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04C-DSP" " " "W-L1" "0" "11" "D-MD2" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "D-HCD" "X" "W-L1" "13" "5" " " "04C-DSP"  RETURNING RESU.
       CALL "SD_From" USING
            "D-HCD" BY REFERENCE URI-HCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-JCD" "9" "W-L1" "74" "6" "D-HCD" " "  RETURNING RESU.
       CALL "SD_From" USING
            "D-JCD" BY REFERENCE URI-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "05C-DSP" " " "W-L2" "0" "39" "04C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NNO" "X" "W-L2" "29" "6" " " "05C-DSP"  RETURNING RESU.
       CALL "SD_From" USING
            "D-NNO" BY REFERENCE URI-NNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-SU" "----,--9.99" "W-L2" "45" "11" "D-NNO" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "D-SU" BY REFERENCE URI-SU "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-T" "----,--9.99" "W-L2" "57" "11" "D-SU" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "D-T" BY REFERENCE URI-T "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-KIN" "---,---,--9" "W-L2" "70" "11" "D-T" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "D-KIN" BY REFERENCE URI-KIN "8" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "100" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "100" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME" "X" "24" "15" "40" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_From" USING
            "E-ME" BY REFERENCE W-MSG "40" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-CL" "X" "24" "10" "50" "E-ME99" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 0 AND 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Screen_Output" USING "SCKT05" RETURNING RESU
           ELSE
               CALL "SD_Screen_Output" USING "SCKT06" RETURNING RESU
           END-IF
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-NKNG TO W-SNG W-ENG.
           MOVE 31 TO W-EPEY.
           MOVE 1 TO W-SPEY.
           ADD 1 TO W-SGET.
           IF  W-SGET = 13
               MOVE 1 TO W-SGET
           ELSE
               SUBTRACT 1 FROM W-SNEN
           END-IF
           MOVE W-SNGP TO W-SDATE.
           MOVE W-ENGP TO W-EDATE.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO URI-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SNGP "A-SNGP" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SNGP = ZERO
               MOVE W-SDATE TO W-SNGP
               GO TO M-15
           END-IF
           IF  W-SGET < 1 OR > 12
               GO TO M-10
           END-IF
           IF  W-SPEY < 1 OR > 31
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-ENGP "A-ENGP" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-ENGP = 999999
               MOVE W-EDATE TO W-ENGP
               GO TO M-17
           END-IF
           IF  W-EGET < 1 OR > 12
               GO TO M-15
           END-IF
           IF  W-EPEY < 1 OR > 31
               GO TO M-15
           END-IF.
       M-17.
           IF  W-SNGP > W-ENGP
               GO TO M-15
           END-IF
           IF  JS-SIGN = 1
               GO TO M-25
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "X" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
      *
           MOVE W-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  ﾋﾝﾒｲ ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               GO TO M-20
           END-IF
           MOVE SPACE TO W-HNA.
           IF  KH-KNA NOT = SPACE
               MOVE KH-KNA TO W-HNA
           ELSE
               MOVE KH-NAME TO W-HNA
           END-IF
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           PERFORM SEA-RTN THRU SEA-EX.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           GO TO M-30.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
      *
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  ﾄｸｲｻｷ ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           IF  T-BC = 0
               GO TO M-25
           END-IF
           PERFORM SEA-RTN THRU SEA-EX.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF.
       M-30.
           MOVE 0 TO W-DMM.
           MOVE 3 TO W-L1.
           CALL "SD_Arg_Match_Line" USING "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 4 TO W-L2.
           CALL "SD_Arg_Match_Line" USING "W-L2" "2" W-L2 RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" URI-F_PNAME1 "EXCLUSIVE" BY REFERENCE
            URI-F_IDLST "0".
      *           SELECT KNH-F WHERE KNH-HCD = W-HCD
      *//////////////////////add koyama 20170303
           IF  JS-SIGN = 0
               CALL "DB_Select" USING
                URI-F_PNAME1 "WHERE"
                "URI-NGPS" ">" W-SNGP "AND" "URI-NGPS" "<" W-ENGP "AND"
                "URI-HCD" "=" W-HCD "AND"
                "URI-TCD" ">" W-STCD "AND" "URI-TCD" "<"  W-ETCD
                RETURNING RET
           END-IF
           IF  JS-SIGN = 1
               CALL "DB_Select" USING
                URI-F_PNAME1 "WHERE"
                "URI-NGPS" ">" W-SNGP "AND" "URI-NGPS" "<" W-ENGP "AND"
                "URI-TCD" "=" W-TCD "AND"
                "URI-HCD" ">" W-SHCD "AND" "URI-TCD" "<"  W-EHCD
                RETURNING RET
           END-IF.
       M-35.
      *           READ URI-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" URI-F_PNAME1 BY REFERENCE URI-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE URI-F_IDLST URI-F_PNAME1
               MOVE SPACE TO W-MSG
               MOVE "***  DATA ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               GO TO M-70
           END-IF.
      *           IF  URI-NGPS < W-SNGP OR > W-ENGP
      *               GO TO M-35
      *           END-IF
      *           IF  JS-SIGN = 0
      *               IF  URI-HCD < W-HCD
      *                   GO TO M-35
      *               END-IF
      *           END-IF
      *           IF  JS-SIGN = 0
      *               IF  URI-HCD > W-HCD
      *                   CALL "DB_F_Close" USING
      *                    BY REFERENCE URI-F_IDLST URI-F_PNAME1
      *                   MOVE SPACE TO W-MSG
      *                   MOVE "***  DATA ﾅｼ  ***" TO W-MSG
      *                   CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
      *                   GO TO M-70
      *               END-IF
      *           END-IF
      *           IF  JS-SIGN = 0
      *               IF  URI-TCD < W-STCD OR > W-ETCD
      *                   GO TO M-35
      *               END-IF
      *           END-IF
      *           IF  JS-SIGN = 1
      *               IF  URI-TCD < W-TCD
      *                   GO TO M-35
      *               END-IF
      *           END-IF
      *           IF  JS-SIGN = 1
      *               IF  URI-TCD > W-TCD
      *                   CALL "DB_F_Close" USING
      *                    BY REFERENCE URI-F_IDLST URI-F_PNAME1
      *                   MOVE SPACE TO W-MSG
      *                   MOVE "***  DATA ﾅｼ  ***" TO W-MSG
      *                   CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
      *                   GO TO M-70
      *               END-IF
      *           END-IF
      *           IF  JS-SIGN = 1
      *               IF  URI-HCD < W-SHCD OR > W-EHCD
      *                   GO TO M-35
      *               END-IF
      *           END-IF.
      *//////////////////////add koyama 20170317
           IF  JS-SIGN = 0
               CALL "DB_Select" USING
                URI-F_PNAME1 "WHERE"
                "URI-NGPS" ">" W-SNGP "AND" "URI-NGPS" "<" W-ENGP "AND"
                "URI-HCD" "=" W-HCD "AND"
                "URI-TCD" ">" W-STCD "AND" "URI-TCD" "<"  W-ETCD
                RETURNING RET
           END-IF
           IF  JS-SIGN = 1
               CALL "DB_Select" USING
                URI-F_PNAME1 "WHERE"
                "URI-NGPS" ">" W-SNGP "AND" "URI-NGPS" "<" W-ENGP "AND"
                "URI-TCD" "=" W-TCD "AND"
                "URI-HCD" ">" W-SHCD "AND" "URI-TCD" "<"  W-EHCD
                RETURNING RET
           END-IF.
       M-40.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE URI-F_IDLST URI-F_PNAME1
               GO TO M-95
           END-IF
           IF  W-DMM = 9
               CALL "DB_F_Close" USING
                BY REFERENCE URI-F_IDLST URI-F_PNAME1
               GO TO M-17
           END-IF.
       M-50.
      *           READ URI-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" URI-F_PNAME1 BY REFERENCE URI-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE URI-F_IDLST URI-F_PNAME1
               MOVE SPACE TO W-MSG
               MOVE "***  END DATA  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               GO TO M-70
           END-IF
      *           IF  URI-NGPS < W-SNGP OR > W-ENGP
      *               GO TO M-50
      *           END-IF
      *           IF  JS-SIGN = 0
      *               IF  URI-HCD > W-HCD
      *                   CALL "DB_F_Close" USING
      *                    BY REFERENCE URI-F_IDLST URI-F_PNAME1
      *                   MOVE SPACE TO W-MSG
      *                   MOVE "***  END DATA  ***" TO W-MSG
      *                   CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
      *                   GO TO M-70
      *               END-IF
      *           END-IF
      *           IF  JS-SIGN = 0
      *               IF  URI-TCD < W-STCD OR > W-ETCD
      *                   GO TO M-50
      *               END-IF
      *           END-IF
      *           IF  JS-SIGN = 1
      *               IF  URI-TCD > W-TCD
      *                   CALL "DB_F_Close" USING
      *                    BY REFERENCE URI-F_IDLST URI-F_PNAME1
      *                   MOVE SPACE TO W-MSG
      *                   MOVE "***  END DATA  ***" TO W-MSG
      *                   CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
      *                   GO TO M-70
      *               END-IF
      *           END-IF
      *           IF  JS-SIGN = 1
      *               IF  URI-HCD < W-SHCD OR > W-EHCD
      *                   GO TO M-50
      *               END-IF
      *           END-IF
           GO TO M-40.
       M-70.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-70
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Screen_Output" USING "SCKT05" RETURNING RESU
           ELSE
               CALL "SD_Screen_Output" USING "SCKT06" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "A-SNGP" A-SNGP "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ENGP" A-ENGP "p" RETURNING RESU.
           MOVE 3 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 4 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU
               CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU
               CALL "SD_Output" USING "A-STCD" A-STCD "p" RETURNING RESU
               CALL "SD_Output" USING "A-ETCD" A-ETCD "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU
               CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU
               CALL "SD_Output" USING "A-SHCD" A-SHCD "p" RETURNING RESU
               CALL "SD_Output" USING "A-EHCD" A-EHCD "p" RETURNING RESU
           END-IF
           GO TO M-17.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       SEA-RTN.
           IF  JS-SIGN = 1
               GO TO SEA-110
           END-IF.
       SEA-010.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO SEA-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO SEA-010
           END-IF.
       SEA-020.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO SEA-010
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO SEA-020
           END-IF
           IF  W-STCD > W-ETCD
               GO TO SEA-020
           END-IF
           GO TO SEA-EX.
       SEA-110.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD "A-SHCD" "X" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO SEA-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO SEA-110
           END-IF.
       SEA-120.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD "A-EHCD" "X" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO SEA-110
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO SEA-120
           END-IF
           IF  W-SHCD > W-EHCD
               GO TO SEA-120
           END-IF.
       SEA-EX.
           EXIT.
       MEI-RTN.
           ADD 2 TO W-L1 W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-L1 = 23
               GO TO MEI-100
           END-IF
           MOVE SPACE TO W-DCN.
           IF  URI-DC = 1
               MOVE "返品" TO W-DCN
           ELSE
               IF  URI-DC = 1
                   MOVE "不返" TO W-DCN
               ELSE
                   IF  URI-DC = 3
                       MOVE "預売" TO W-DCN
                   ELSE
                       IF  URI-DC = 4
                           MOVE "預出" TO W-DCN
                       ELSE
                           IF  URI-DC = 5
                               MOVE "消税" TO W-DCN
                           ELSE
                               IF  URI-DC = 8
                                   MOVE "値引" TO W-DCN
                               ELSE
                                   IF  URI-DC = 9
                                       MOVE "値税" TO W-DCN
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  URI-DC = 8 OR 9
               COMPUTE URI-T = -1 * URI-T
               COMPUTE URI-KIN = -1 * URI-KIN
           END-IF
           IF  JS-SIGN = 1
               GO TO MEI-020
           END-IF
           MOVE URI-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "＊＊＊　得意先なし　＊＊＊" TO T-NAME
           END-IF
           CALL "SD_Output" USING "D-MD1" D-MD1 "p" RETURNING RESU.
           GO TO MEI-050.
       MEI-020.
           MOVE URI-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO KH-NAME KH-KNA
               MOVE "***  ﾋﾝﾒｲ ﾅｼ  ***" TO KH-NAME
           END-IF
           IF  URI-HCD = ZERO
               MOVE SPACE TO KH-NAME KH-KNA
           ELSE
               CALL "SD_Output" USING "D-HCD" D-HCD "p" RETURNING RESU
           END-IF
           MOVE SPACE TO W-HNA.
           IF  KH-KNA NOT = SPACE
               MOVE KH-KNA TO W-HNA
           ELSE
               MOVE KH-NAME TO W-HNA
           END-IF
           CALL "SD_Output" USING "D-MD2" D-MD2 "p" RETURNING RESU.
       MEI-050.
           IF  URI-JCD NOT = ZERO
               CALL "SD_Output" USING "D-JCD" D-JCD "p" RETURNING RESU
           END-IF
           IF  URI-NNO NOT = ZERO
               CALL "SD_Output" USING "D-NNO" D-NNO "p" RETURNING RESU
           END-IF
           IF  URI-SU NOT = ZERO
               CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU
           END-IF
           IF  URI-T NOT = ZERO
               CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU
           END-IF
           IF  URI-DC NOT = 4
               CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU
           END-IF
           GO TO MEI-EX.
       MEI-100.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO MEI-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO MEI-100
           END-IF
           IF  W-DMM NOT = 1 AND 9
               GO TO MEI-100
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Screen_Output" USING "SCKT05" RETURNING RESU
           ELSE
               CALL "SD_Screen_Output" USING "SCKT06" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "A-SNGP" A-SNGP "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ENGP" A-ENGP "p" RETURNING RESU.
           MOVE 3 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 4 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-DMM = 9
               GO TO MEI-EX
           END-IF
           IF  JS-SIGN = 0
               CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU
               CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU
               CALL "SD_Output" USING "A-STCD" A-STCD "p" RETURNING RESU
               CALL "SD_Output" USING "A-ETCD" A-ETCD "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU
               CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU
               CALL "SD_Output" USING "A-SHCD" A-SHCD "p" RETURNING RESU
               CALL "SD_Output" USING "A-EHCD" A-EHCD "p" RETURNING RESU
           END-IF
           GO TO MEI-RTN.
       MEI-EX.
           EXIT.
